;;; efrit-api-stream.el --- Streaming Messages API transport over curl -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; `url-retrieve' gives neither a usable byte-level filter nor a clean
;; way to abort a request in flight.  This transport runs curl as a
;; subprocess (what plz.el does underneath) and:
;;
;; - parses the Anthropic SSE stream *incrementally* in the process
;;   filter, calling ON-TEXT with each assistant text delta so the
;;   agent buffer can render as tokens arrive;
;; - reassembles the final message -- text blocks, tool_use blocks
;;   with their JSON-accumulated input, stop_reason, usage -- into the
;;   same hash-table shape `efrit-api-parse-response' produces, so
;;   `efrit-loop-handle-response' and everything downstream are
;;   unchanged;
;; - is cancellable: `efrit-api-stream-cancel' signals the process;
;;   remaining tool_use deltas are dropped and the callback gets an
;;   `interrupted' error;
;; - treats a timeout as degraded success when partial content exists
;;   (minuet's rule): the caller receives what arrived with
;;   stop_reason "end_turn" and a :partial marker, rather than nothing.
;;
;; `efrit-api-request-transforms' and `efrit-api-extra-body' apply
;; here exactly as in the url-retrieve path.  The curl program is
;; `efrit-api-stream-curl-program'; tests point it at a script that
;; emits a canned stream.
;;
;; Credentials never appear on the command line: headers are passed
;; through a temp config file read with `curl --config', which is
;; deleted when the process exits.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'json)
(require 'efrit-common)
(require 'efrit-api)
(require 'efrit-log)

(defgroup efrit-api-stream nil
  "Streaming transport."
  :group 'efrit
  :prefix "efrit-api-stream-")

(defcustom efrit-api-stream-curl-program "curl"
  "Program used for streaming requests.  Must accept curl's arguments."
  :type 'string
  :group 'efrit-api-stream)

(defcustom efrit-api-stream-timeout 300
  "Seconds without the request completing before it is killed.
Partial content received by then is delivered as a degraded success."
  :type 'integer
  :group 'efrit-api-stream)

(defcustom efrit-api-streaming (and (executable-find "curl") t)
  "When non-nil, the agentic loop streams responses via curl.
When nil (or curl is missing) the url-retrieve path is used."
  :type 'boolean
  :group 'efrit-api-stream)

;;; Request state

(cl-defstruct (efrit-api-stream (:constructor efrit-api-stream--make))
  process config-file
  callback on-text
  (buffer "")            ; unparsed tail of the SSE stream
  (blocks nil)           ; list of (INDEX . HASH) in reverse
  (json-acc nil)         ; alist INDEX -> accumulated input_json string
  message                ; hash from message_start (usage, model, id)
  stop-reason usage
  (status-line nil) (http-status nil)
  (finished nil) (partial nil) (cancelled nil)
  (error-body "")
  (context nil))          ; (URL MODEL TRANSPORT PURPOSE) for error messages

(defvar efrit-api-stream--active nil
  "List of in-flight `efrit-api-stream' structs.")

;;; SSE parsing

(defun efrit-api-stream--handle-event (st event data)
  "Fold one SSE EVENT with parsed DATA (hash) into ST."
  (pcase event
    ("message_start"
     (let ((msg (gethash "message" data)))
       (setf (efrit-api-stream-message st) msg
             (efrit-api-stream-usage st) (gethash "usage" msg))))
    ("content_block_start"
     (let* ((idx (gethash "index" data))
            (blk (copy-hash-table (gethash "content_block" data))))
       (when (equal (gethash "type" blk) "tool_use")
         ;; input arrives as JSON deltas; start empty
         (push (cons idx "") (efrit-api-stream-json-acc st)))
       (when (equal (gethash "type" blk) "text")
         (puthash "text" (or (gethash "text" blk) "") blk))
       (push (cons idx blk) (efrit-api-stream-blocks st))))
    ("content_block_delta"
     (let* ((idx (gethash "index" data))
            (delta (gethash "delta" data))
            (blk (cdr (assoc idx (efrit-api-stream-blocks st)))))
       (when blk
         (pcase (gethash "type" delta)
           ("text_delta"
            (let ((text (gethash "text" delta)))
              (puthash "text" (concat (gethash "text" blk) text) blk)
              (when-let* ((fn (efrit-api-stream-on-text st)))
                (condition-case err (funcall fn text)
                  (error (efrit-log 'warn "on-text callback: %s" (error-message-string err)))))))
           ("input_json_delta"
            (let ((cell (assoc idx (efrit-api-stream-json-acc st))))
              (setcdr cell (concat (cdr cell) (gethash "partial_json" delta)))))
           ("thinking_delta"
            (puthash "thinking" (concat (or (gethash "thinking" blk) "")
                                        (gethash "thinking" delta))
                     blk))))))
    ("content_block_stop"
     (let* ((idx (gethash "index" data))
            (blk (cdr (assoc idx (efrit-api-stream-blocks st))))
            (acc (assoc idx (efrit-api-stream-json-acc st))))
       (when (and blk acc)
         (puthash "input"
                  (condition-case nil
                      (if (string-empty-p (cdr acc))
                          (make-hash-table :test 'equal)
                        (json-parse-string (cdr acc) :object-type 'hash-table
                                           :array-type 'array))
                    (error
                     (efrit-log 'warn "tool_use %s: unparsable input JSON, passing empty"
                                (gethash "name" blk))
                     (make-hash-table :test 'equal)))
                  blk))))
    ("message_delta"
     (when-let* ((d (gethash "delta" data)))
       (when-let* ((sr (gethash "stop_reason" d)))
         (setf (efrit-api-stream-stop-reason st) sr)))
     (when-let* ((u (gethash "usage" data)))
       ;; message_delta carries output_tokens; merge into start usage
       (let ((usage (or (efrit-api-stream-usage st) (make-hash-table :test 'equal))))
         (maphash (lambda (k v) (puthash k v usage)) u)
         (setf (efrit-api-stream-usage st) usage))))
    ("message_stop" (setf (efrit-api-stream-finished st) t))
    ("error"
     (setf (efrit-api-stream-error-body st)
           (json-encode data)))
    (_ nil)))

(defun efrit-api-stream--consume (st chunk)
  "Append CHUNK to ST's buffer and dispatch every complete SSE event."
  (setf (efrit-api-stream-buffer st) (concat (efrit-api-stream-buffer st) chunk))
  (let ((buf (efrit-api-stream-buffer st)) (pos 0) event data-lines)
    ;; Events are separated by a blank line
    (while (string-match "\\(?:\r?\n\\)\\{2\\}" buf pos)
      (let ((block (substring buf pos (match-beginning 0))))
        (setq pos (match-end 0) event nil data-lines nil)
        (dolist (line (split-string block "\r?\n" t))
          (cond ((string-prefix-p "event:" line)
                 (setq event (string-trim (substring line 6))))
                ((string-prefix-p "data:" line)
                 (push (string-trim (substring line 5)) data-lines))
                (t nil)))                              ; ": comment" keepalive
        (when data-lines
          (let ((payload (string-join (nreverse data-lines) "\n")))
            (unless (equal payload "[DONE]")
              (condition-case err
                  (let ((obj (json-parse-string payload :object-type 'hash-table
                                                :array-type 'array)))
                    (efrit-api-stream--handle-event
                     st (or event (gethash "type" obj)) obj))
                (error
                 (efrit-log 'warn "SSE: bad JSON payload (%s): %s"
                            (error-message-string err)
                            (truncate-string-to-width payload 120 nil nil t)))))))))
    (setf (efrit-api-stream-buffer st) (substring buf pos))))

;;; Assembling the response

(defun efrit-api-stream-response (st)
  "Build the non-streaming response hash for ST from what has arrived."
  (let ((resp (if (efrit-api-stream-message st)
                  (copy-hash-table (efrit-api-stream-message st))
                (make-hash-table :test 'equal)))
        (blocks (sort (copy-sequence (efrit-api-stream-blocks st))
                      (lambda (a b) (< (car a) (car b))))))
    ;; tool_use blocks that never got content_block_stop: finalize input
    (dolist (b blocks)
      (let ((blk (cdr b)))
        (when (and (equal (gethash "type" blk) "tool_use")
                   (not (gethash "input" blk)))
          (puthash "input" (make-hash-table :test 'equal) blk))))
    (puthash "content" (vconcat (mapcar #'cdr blocks)) resp)
    (puthash "stop_reason" (or (efrit-api-stream-stop-reason st)
                               ;; partial: pretend a clean end so the loop
                               ;; renders what we have
                               (if (cl-some (lambda (b) (equal (gethash "type" (cdr b)) "tool_use"))
                                            blocks)
                                   "tool_use" "end_turn"))
             resp)
    (when (efrit-api-stream-usage st)
      (puthash "usage" (efrit-api-stream-usage st) resp))
    (when (efrit-api-stream-partial st)
      (puthash "efrit_partial" t resp))
    resp))

;;; Process plumbing

(defun efrit-api-stream--filter (st _proc chunk)
  (condition-case err
      (efrit-api-stream--consume st chunk)
    (error (efrit-log 'error "stream filter: %s" (error-message-string err)))))

(defun efrit-api-stream--cleanup (st)
  (setq efrit-api-stream--active (delq st efrit-api-stream--active))
  (when-let* ((f (efrit-api-stream-config-file st)))
    (ignore-errors (delete-file f))))

(defun efrit-api-stream--sentinel (st proc _event)
  (unless (process-live-p proc)
    (let* ((exit (process-exit-status proc))
           (cb (efrit-api-stream-callback st))
           (have-content (efrit-api-stream-blocks st)))
      ;; Flush anything left without a trailing blank line.  If the
      ;; process failed before any SSE arrived, what's left is the
      ;; HTTP error body (curl --fail-with-body writes it to stdout).
      (unless (string-empty-p (efrit-api-stream-buffer st))
        (if (and (/= exit 0) (null (efrit-api-stream-message st))
                 (string-empty-p (efrit-api-stream-error-body st)))
            (setf (efrit-api-stream-error-body st)
                  (string-trim (efrit-api-stream-buffer st))
                  (efrit-api-stream-buffer st) "")
          (efrit-api-stream--consume st "\n\n")))
      (efrit-api-stream--cleanup st)
      (cond
       ((efrit-api-stream-cancelled st)
        (funcall cb nil "interrupted"))
       ;; API-level error event in the stream, or non-2xx with a body
       ((not (string-empty-p (efrit-api-stream-error-body st)))
        (funcall cb nil (efrit-api-stream--fail st (efrit-api-stream--format-error
                                                    (efrit-api-stream-error-body st)))))
       ((efrit-api-stream-finished st)
        (funcall cb (efrit-api-stream-response st) nil))
       ;; curl 28 = operation timed out; anything else non-zero is transport
       ((and (/= exit 0) have-content)
        (setf (efrit-api-stream-partial st) t)
        (efrit-log 'warn "stream ended early (curl exit %d); delivering partial content" exit)
        (funcall cb (efrit-api-stream-response st) nil))
       ((/= exit 0)
        (funcall cb nil (efrit-api-stream--fail
                         st (format "curl exit %d%s%s" exit
                                    (pcase exit (28 (format " (no response within %ds)" efrit-api-stream-timeout))
                                           (6 " (could not resolve host)")
                                           (7 " (connection refused)") (35 " (TLS handshake failed)")
                                           (56 " (connection reset)") (_ ""))
                                    (if (efrit-api-stream-http-status st)
                                        (format ", HTTP %s" (efrit-api-stream-http-status st))
                                      "")))))
       ;; exit 0 but no message_stop: server closed early
       (have-content
        (setf (efrit-api-stream-partial st) t)
        (funcall cb (efrit-api-stream-response st) nil))
       (t (funcall cb nil (efrit-api-stream--fail
                           st (if (string-empty-p (efrit-api-stream-buffer st))
                                  "Empty response from endpoint"
                                (efrit-api-stream--format-error (efrit-api-stream-buffer st))))))))))

(defun efrit-api-stream--fail (st text)
  "TEXT prefixed with the request context recorded in ST."
  (apply #'efrit-api-describe-failure text (efrit-api-stream-context st)))

(defun efrit-api-stream--format-error (body)
  "Turn an error BODY (JSON or text) into efrit's \"API Error (type): msg\" form."
  (condition-case nil
      (let* ((obj (json-parse-string body :object-type 'hash-table))
             (err (or (gethash "error" obj) obj)))
        (format "API Error (%s): %s"
                (or (gethash "type" err) "unknown")
                (or (gethash "message" err) body)))
    (error (format "API Error: %s" (string-trim body)))))

(defun efrit-api-stream--write-config (headers)
  "Write HEADERS to a private temp file in curl config syntax; return its path."
  (let ((file (make-temp-file "efrit-curl-" nil ".cfg")))
    (with-temp-file file
      (dolist (h headers)
        (insert (format "header = %s\n"
                        (prin1-to-string (format "%s: %s" (car h) (cdr h)))))))
    (set-file-modes file #o600)
    file))

(defun efrit-api-stream-request (request-data callback &optional on-text)
  "Stream REQUEST-DATA to the Messages API.
CALLBACK is (lambda (RESPONSE ERROR)) with the assembled response
hash (as from the non-streaming API) or an error string.  ON-TEXT, if
given, is called with each assistant text delta as it arrives.
Returns the `efrit-api-stream' handle, for `efrit-api-stream-cancel'."
  (let* ((api-key (efrit-common-get-api-key))
         (req (efrit-api-apply-transforms
               (efrit-common-get-api-url)
               (efrit-api-build-headers api-key)
               (efrit-api-apply-extra-body
                (append request-data '(("stream" . t))))))
         (body-file (make-temp-file "efrit-body-" nil ".json"))
         (config (efrit-api-stream--write-config
                  (cons '("accept" . "text/event-stream") (plist-get req :headers))))
         (st (efrit-api-stream--make :callback callback :on-text on-text
                                     :config-file config
                                     ;; captured now; the sentinel runs
                                     ;; outside the caller's bindings
                                     :context (list (plist-get req :url)
                                                    (alist-get "model" request-data nil nil #'equal)
                                                    "curl (streaming)"
                                                    efrit-api-request-purpose))))
    (with-temp-file body-file
      (set-buffer-multibyte nil)
      (insert (efrit-api-encode-request (plist-get req :body))))
    (set-file-modes body-file #o600)
    (let ((proc
           (make-process
            :name "efrit-stream"
            :buffer nil
            :command (list efrit-api-stream-curl-program
                           "--silent" "--show-error" "--no-buffer"
                           "--fail-with-body"
                           "--max-time" (number-to-string efrit-api-stream-timeout)
                           "--config" config
                           "--data-binary" (concat "@" body-file)
                           "--request" "POST"
                           (plist-get req :url))
            :connection-type 'pipe
            :coding 'utf-8-unix
            :noquery t
            :filter (lambda (p c) (efrit-api-stream--filter st p c))
            :sentinel (lambda (p e)
                        (ignore-errors (delete-file body-file))
                        (efrit-api-stream--sentinel st p e)))))
      (setf (efrit-api-stream-process st) proc)
      (push st efrit-api-stream--active)
      st)))

(defun efrit-api-stream-cancel (&optional st)
  "Cancel stream ST, or every active stream when nil.
The callback receives error \"interrupted\"."
  (dolist (s (if st (list st) (copy-sequence efrit-api-stream--active)))
    (setf (efrit-api-stream-cancelled s) t)
    (when-let* ((p (efrit-api-stream-process s)))
      (when (process-live-p p)
        (interrupt-process p)
        (run-at-time 1 nil (lambda () (when (process-live-p p) (delete-process p))))))))

(provide 'efrit-api-stream)

;;; efrit-api-stream.el ends here
