;;; efrit-api.el --- Shared HTTP/API client layer for Efrit -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; Unified HTTP/API client layer for all Efrit modules.
;; This module consolidates:
;; - Header construction (with customization support)
;; - JSON encoding with Unicode escaping
;; - Async and sync API request functions
;; - Response parsing utilities
;;
;; The loop engine and the executor use this layer
;; instead of duplicating HTTP logic.

;;; Code:

(require 'json)
(require 'url)
(defvar url-show-status)
(require 'cl-lib)
(require 'efrit-common)
(require 'efrit-log)
(declare-function efrit-response-usage "efrit-chat-response")
(declare-function efrit-response-error "efrit-chat-response")
(declare-function efrit-response-stop-reason "efrit-chat-response")
(declare-function efrit-error-message "efrit-chat-response")

(declare-function efrit-log "efrit-log")
(declare-function efrit-log-debug "efrit-log")
(declare-function efrit-log-error "efrit-log")

;;; Header Customization

(defcustom efrit-api-custom-headers nil
  "Alist of custom headers to add to API requests.
Each element should be a cons cell of (HEADER-NAME . HEADER-VALUE).
Custom headers override default headers with the same name.
Example: \\='((\"authorization\" . \"Bearer your-token\")
             (\"custom-header\" . \"custom-value\"))"
  :type '(alist :key-type string :value-type string)
  :group 'efrit)

(defcustom efrit-api-excluded-headers nil
  "List of default header names to exclude from API requests.
Use this to remove headers that may conflict with proxy configurations.
Example: \\='(\"anthropic-version\" \"anthropic-beta\")"
  :type '(repeat string)
  :group 'efrit)

(defcustom efrit-api-request-transforms nil
  "Functions that may rewrite each outgoing request before it is sent.

Each is called with one argument, a plist (:url URL :headers ALIST
:body ALIST), and returns a plist of the same shape or nil to leave
the request unchanged.  They run in order, after the built-in header
handling, so they can retarget the endpoint, rename or delete body
fields for a gateway, or swap the auth header for a freshly minted
token.  (minuet's :transform idea.)

Example -- a gateway that wants the model in the path and no
anthropic-version header:

  (add-hook \='efrit-api-request-transforms
            (lambda (req)
              (let* ((body (plist-get req :body))
                     (model (cdr (assoc \"model\" body))))
                (list :url (format \"https://gw.example.com/models/%s/messages\" model)
                      :headers (assoc-delete-all \"anthropic-version\"
                                                 (copy-alist (plist-get req :headers)))
                      :body body))))"
  :type '(repeat function)
  :group 'efrit)

(defcustom efrit-api-extra-body nil
  "Alist merged into the top level of every request body.
Keys are strings as the API spells them; an entry here overrides the
value efrit would otherwise send.  Use it for parameters efrit has no
option for yet, e.g.

  \='((\"service_tier\" . \"auto\")
    (\"thinking\" . ((\"type\" . \"enabled\") (\"budget_tokens\" . 4096))))

Set a value to `:delete' to remove a key efrit sends by default."
  :type '(alist :key-type string :value-type sexp)
  :group 'efrit)

;;; Request context for error messages
;;
;; "HTTP error: curl exit 7" tells the user nothing about which host
;; efrit was talking to or on whose behalf.  Every request site binds
;; `efrit-api-request-purpose'; every failure is wrapped by
;; `efrit-api-describe-failure', which prefixes the purpose, the
;; endpoint (scheme://host/path -- never the query string or key),
;; the model and the transport.

(defvar efrit-api-request-purpose nil
  "Why the request in flight is being made, for error messages.
A short phrase such as \"the model's next turn\", \"reviewing the
proposed tool calls\", \"listing models\".  Bound dynamically by the
caller around the request; nil reads as \"an API request\".")

(defun efrit-api-display-url (url)
  "URL reduced to scheme://host[:port]/path for display.
The query string and fragment are dropped: they can carry keys."
  (condition-case nil
      (let* ((u (url-generic-parse-url url))
             (path (car (url-path-and-query u))))
        (format "%s://%s%s%s" (url-type u) (url-host u)
                (if-let* ((p (url-port-if-non-default u))) (format ":%d" p) "")
                (or path "")))
    (error "<unparsable url>")))

(defun efrit-api-describe-request (&optional url model transport purpose)
  "One line naming a request: PURPOSE, URL, MODEL, TRANSPORT.
Defaults: the messages endpoint, `efrit-default-model', the configured
transport, `efrit-api-request-purpose'."
  (format "%s → %s (model %s, via %s)"
          (or purpose efrit-api-request-purpose "an API request")
          (efrit-api-display-url (or url (efrit-common-get-api-url)))
          (or model (and (boundp 'efrit-default-model) efrit-default-model) "?")
          (or transport
              (if (bound-and-true-p efrit-api-streaming) "curl (streaming)" "url-retrieve"))))

(defun efrit-api-describe-failure (error-text &rest args)
  "ERROR-TEXT prefixed with the request context; ARGS as for `efrit-api-describe-request'.
The result is what the user sees in the agent buffer, so it says what
was attempted before it says what went wrong."
  (format "%s failed.\n%s"
          (apply #'efrit-api-describe-request args)
          error-text))

(defun efrit-api-apply-extra-body (body)
  "Return BODY (an alist) with `efrit-api-extra-body' merged in."
  (if (null efrit-api-extra-body)
      body
    (let ((out (copy-alist body)))
      (dolist (pair efrit-api-extra-body)
        (setq out (assoc-delete-all (car pair) out))
        (unless (eq (cdr pair) :delete)
          (setq out (append out (list (cons (car pair) (cdr pair)))))))
      out)))

(defun efrit-api-apply-transforms (url headers body)
  "Run `efrit-api-request-transforms' over URL, HEADERS and BODY.
Returns a plist (:url :headers :body).  A transform that signals is
logged and skipped, so one bad hook cannot take every request down."
  (let ((req (list :url url :headers headers :body body)))
    (dolist (fn efrit-api-request-transforms)
      (condition-case err
          (when-let* ((new (funcall fn req)))
            (setq req (list :url (or (plist-get new :url) (plist-get req :url))
                            :headers (or (plist-get new :headers) (plist-get req :headers))
                            :body (or (plist-get new :body) (plist-get req :body)))))
        (error
         (efrit-log 'warn "efrit-api-request-transforms: %S signalled: %s"
                    fn (error-message-string err)))))
    req))

(defun efrit-api-build-headers (api-key)
  "Build HTTP headers for API requests with API-KEY.
Applies header customization from `efrit-api-custom-headers' and
`efrit-api-excluded-headers'."
  ;; efrit-common-build-headers validates the key and picks the auth
  ;; scheme (x-api-key vs. Authorization: Bearer for proxies).
  (let ((default-headers (efrit-common-build-headers api-key)))
    ;; Remove excluded headers
    (when efrit-api-excluded-headers
      (setq default-headers
            (cl-remove-if (lambda (header)
                            (member (car header) efrit-api-excluded-headers))
                          default-headers)))
    ;; Add custom headers (custom headers override defaults)
    (append efrit-api-custom-headers default-headers)))

;;; JSON Encoding

(defvar efrit-api--last-body nil
  "The bytes of the last request body, kept for `efrit-api-explain-invalid-json'.")

(defun efrit-api-encode-request (data)
  "Encode DATA as JSON with proper Unicode escaping for HTTP transmission.
Returns a UTF-8 encoded string suitable for url-request-data."
  (let* ((json-string (json-encode data))
         (escaped-json (efrit-common-escape-json-unicode json-string)))
    (setq efrit-api--last-body (encode-coding-string escaped-json 'utf-8))))

(defun efrit-api--invalid-json-p (message)
  "Non-nil if the API's error MESSAGE says the request body was not JSON."
  (and (stringp message) (string-match-p "Invalid JSON\\|invalid json\\|JSON parse" message)))

(defun efrit-api-save-last-body ()
  "Write the last request body to a file in `efrit-data-directory'; return its path.
The file is what left this Emacs, byte for byte, so it can be checked
with another parser: jq . FILE, or python3 -m json.tool FILE."
  (interactive)
  (unless efrit-api--last-body (user-error "No request has been sent yet"))
  (let ((file (expand-file-name (format-time-string "request-body-%Y%m%d-%H%M%S.json")
                                (if (boundp 'efrit-data-directory)
                                    (symbol-value 'efrit-data-directory)
                                  temporary-file-directory))))
    (make-directory (file-name-directory file) t)
    (with-file-modes #o600
      (with-temp-file file
        (set-buffer-multibyte nil)
        (insert efrit-api--last-body)))
    (when (called-interactively-p 'any) (message "Request body saved to %s" file))
    file))

(defun efrit-api-explain-invalid-json ()
  "What is wrong with the last request body, as far as Emacs can tell.
Parses the bytes that were sent with `json-parse-string' and reports
the result, the size, and any escape or control character a stricter
parser could object to.  Returns the report string."
  (let* ((body (or efrit-api--last-body (user-error "No request has been sent yet")))
         (text (decode-coding-string body 'utf-8))
         (parse (condition-case e (and (json-parse-string text) "parses") (error (format "%S" e))))
         (non-ascii (cl-count-if (lambda (c) (> c 127)) text))
         (controls (cl-count-if (lambda (c) (and (< c 32) (not (memq c '(9 10 13))))) text))
         (odd (let ((n 0) (i 0))
                (while (string-match "\\\\u[0-9A-Fa-f]\\{4\\}[0-9A-Fa-f]" text i)
                  (setq n (1+ n) i (match-end 0)))
                n))
         (lone (let ((n 0) (i 0))
                 (while (string-match "\\\\u[dD][89abAB][0-9A-Fa-f]\\{2\\}\\(?:[^\\\\]\\|\\\\[^u]\\|\\\\u[^dD]\\|\\\\u[dD][0-7]\\)" text i)
                   (setq n (1+ n) i (match-end 0)))
                 n))
         (report (format "last request body: %d bytes, %s by Emacs; non-ASCII %d, raw controls %d, 5+ digit escapes %d, lone high surrogates %d; saved to %s"
                         (length body) parse non-ascii controls odd lone (efrit-api-save-last-body))))
    (efrit-log 'warn "%s" report)
    report))

;;; Response Parsing

(defun efrit-api-parse-response ()
  "Parse JSON response from current buffer.
Returns the parsed hash-table/alist, or signals an error.
Must be called with point in a url-retrieve response buffer."
  (goto-char (point-min))
  (when (search-forward-regexp "^$" nil t)
    (let* ((json-object-type 'hash-table)
           (json-array-type 'vector)
           (json-key-type 'string)
           (coding-system-for-read 'utf-8)
           (raw-response (decode-coding-region (point) (point-max) 'utf-8 t)))
      (let ((response (condition-case parse-err
                          (json-read-from-string raw-response)
                        (error
                         (error "Failed to parse API response: %s"
                                (error-message-string parse-err))))))
        ;; Check if the API returned an error object
        (if-let* ((error-obj (gethash "error" response)))
            (let ((error-type (gethash "type" error-obj))
                  (error-msg (gethash "message" error-obj)))
              ;; the body we sent is the evidence; keep it and say where
              (when (efrit-api--invalid-json-p error-msg)
                (setq error-msg (format "%s -- %s" error-msg
                                        (condition-case nil (efrit-api-explain-invalid-json)
                                          (error "could not inspect the last body")))))
              (error "API Error (%s): %s" error-type error-msg))
          response)))))

(defun efrit-api--error-from-body ()
  "Extract the API error from the body of the current response buffer.
Returns \"API Error (type): message\" if the body contains a JSON
error object, nil otherwise.  Must be called in a url-retrieve
response buffer."
  (ignore-errors
    (goto-char (point-min))
    (when (search-forward-regexp "^$" nil t)
      (let ((body (json-parse-string
                   (decode-coding-region (point) (point-max) 'utf-8 t)
                   :object-type 'hash-table)))
        (when-let* ((error-obj (gethash "error" body)))
          (let ((msg (or (gethash "message" error-obj) "unknown error")))
            (when (efrit-api--invalid-json-p msg)
              (setq msg (format "%s -- %s" msg
                                (condition-case nil (efrit-api-explain-invalid-json)
                                  (error "could not inspect the last body")))))
            (format "API Error (%s): %s" (or (gethash "type" error-obj) "unknown") msg)))))))

(defun efrit-api-extract-content (response)
  "Extract content array from API RESPONSE hash-table."
  (gethash "content" response))

(defun efrit-api-extract-stop-reason (response)
  "Extract stop_reason from API RESPONSE hash-table."
  (gethash "stop_reason" response))

;;; Async Request

(defun efrit-api--log-request (request-data transport)
  "Log the outgoing request: purpose, model, size, TRANSPORT.  Returns `float-time'."
  (let* ((messages (alist-get "messages" request-data nil nil #'equal))
         (tools (alist-get "tools" request-data nil nil #'equal)))
    (efrit-log 'info "api → %s: %s, %d message(s), %d tool(s), via %s"
               (alist-get "model" request-data nil nil #'equal)
               (or efrit-api-request-purpose "request")
               (length messages) (length tools) transport))
  (float-time))

(defun efrit-api--log-response (response started &optional purpose)
  "Log RESPONSE: stop reason, tokens, elapsed since STARTED, for PURPOSE."
  (let* ((usage (and response (efrit-response-usage response)))
         (err (and response (efrit-response-error response))))
    (if err
        (efrit-log 'warn "api ← error after %.1fs: %s (%s)"
                   (- (float-time) started) (efrit-error-message err)
                   (or purpose efrit-api-request-purpose "request"))
      (efrit-log 'info "api ← %s in %.1fs: in=%s out=%s cache_read=%s cache_write=%s (%s)"
                 (or (and response (efrit-response-stop-reason response)) "?")
                 (- (float-time) started)
                 (and usage (gethash "input_tokens" usage))
                 (and usage (gethash "output_tokens" usage))
                 (and usage (gethash "cache_read_input_tokens" usage))
                 (and usage (gethash "cache_creation_input_tokens" usage))
                 (or purpose efrit-api-request-purpose "request")))))

(defcustom efrit-api-inline-system-on-refusal t
  "When non-nil, a request refused before generation is retried once
with its system prompt folded into the first user message.
Some routes refuse any request that carries a `system' field (the
stop reason is refusal, no output, in about a second) while the same
text as part of the user message is answered.  The retry is logged;
the response of the retry carries the key `efrit-inlined-system' so a
caller can tell."
  :type 'boolean
  :group 'efrit)

(defun efrit-api--refused-p (response)
  "Non-nil if RESPONSE is a pre-generation refusal."
  (and (hash-table-p response)
       (equal (gethash "stop_reason" response) "refusal")))

(defun efrit-api-inline-system (request-data)
  "REQUEST-DATA with its system prompt moved into the first user message.
Returns nil when there is no system prompt to move."
  (let ((system (alist-get "system" request-data nil nil #'equal)))
    (when system
      (let* ((text (cond ((stringp system) system)
                         ((vectorp system)
                          (mapconcat (lambda (b) (or (alist-get "text" b nil nil #'equal) "")) system "\n"))
                         (t (format "%s" system))))
             (messages (append (alist-get "messages" request-data nil nil #'equal) nil))
             (first (car messages))
             (content (alist-get "content" first nil nil #'equal))
             (new-first
              (if (stringp content)
                  `(("role" . "user") ("content" . ,(concat text "\n\n---\n\n" content)))
                ;; a block vector: prepend a text block
                `(("role" . "user")
                  ("content" . ,(vconcat (vector `(("type" . "text") ("text" . ,text)))
                                         content))))))
        (append (cl-remove-if (lambda (c) (member (car c) '("system" "messages"))) request-data)
                `(("messages" . ,(vconcat (cons new-first (cdr messages))))))))))

(defun efrit-api-request-async (request-data callback &optional error-callback)
  "Send REQUEST-DATA to Claude API asynchronously.
Calls CALLBACK with (RESPONSE) on success.
Calls ERROR-CALLBACK with (ERROR-MESSAGE) on failure, or signals error if nil.
A refusal of a request carrying a system prompt is retried once with
the prompt inlined, as `efrit-api-request-sync' does."
  (let ((purpose efrit-api-request-purpose))
    (efrit-api--request-async-1
     request-data
     (lambda (response)
       (if (and efrit-api-inline-system-on-refusal
                (efrit-api--refused-p response)
                (alist-get "system" request-data nil nil #'equal))
           (let ((efrit-api-request-purpose purpose))
             (efrit-log 'warn "api: refused with a system prompt; retrying with it inlined (%s)"
                        (or purpose "request"))
             (efrit-api--request-async-1
              (efrit-api-inline-system request-data)
              (lambda (r2)
                (when (hash-table-p r2) (puthash "efrit-inlined-system" t r2))
                (funcall callback r2))
              error-callback))
         (funcall callback response)))
     error-callback)))

(defun efrit-api--request-async-1 (request-data callback &optional error-callback)
  "One asynchronous request; see `efrit-api-request-async'."
  (condition-case err
      (let* ((api-key (efrit-common-get-api-key))
             (req (efrit-api-apply-transforms
                   (efrit-common-get-api-url)
                   (efrit-api-build-headers api-key)
                   (efrit-api-apply-extra-body request-data)))
             (url-request-method "POST")
             (url-request-extra-headers (plist-get req :headers))
             (url-request-data (efrit-api-encode-request (plist-get req :body)))
             ;; captured now: the callback runs later, outside the
             ;; caller's dynamic bindings
             (describe-args (list (plist-get req :url)
                                  (alist-get "model" request-data nil nil #'equal)
                                  "url-retrieve"
                                  efrit-api-request-purpose))
             (purpose efrit-api-request-purpose)
             (started (efrit-api--log-request request-data "url-retrieve")))
        (url-retrieve
         (plist-get req :url)
         (lambda (status)
           ;; The callback can change the current buffer (tools may call
           ;; pop-to-buffer etc.), so capture the HTTP response buffer now
           ;; lest the cleanup kill whatever buffer the callback left current.
           (let ((response-buffer (current-buffer)))
             (unwind-protect
                 (condition-case url-err
                     (progn
                       (when-let* ((http-err (plist-get status :error)))
                         ;; The body usually carries the API's JSON error
                         ;; object, which is far more useful than
                         ;; url-retrieve's "(error http 400)".
                         (error "%s" (or (efrit-api--error-from-body)
                                         (format "HTTP error: %s" http-err))))
                       (let ((response (efrit-api-parse-response)))
                         (efrit-api--log-response response started purpose)
                         (funcall callback response)))
                   (error
                    (let ((msg (apply #'efrit-api-describe-failure
                                      (error-message-string url-err) describe-args)))
                      (efrit-log 'warn "api ← failed after %.1fs: %s" (- (float-time) started) msg)
                      (if error-callback
                          (funcall error-callback msg)
                        (error "%s" msg)))))
               (when (buffer-live-p response-buffer)
                 (kill-buffer response-buffer)))))
         nil t t))
    (error
     ;; Before the request left: no key, bad URL, encoding failure
     (if error-callback
         (funcall error-callback
                  (efrit-api-describe-failure (error-message-string err)
                                              nil (alist-get "model" request-data nil nil #'equal)
                                              "url-retrieve"))
       (signal (car err) (cdr err))))))

;;; Sync Request

(defun efrit-api-request-sync (request-data &optional timeout)
  "Send REQUEST-DATA to Claude API synchronously.
TIMEOUT is optional timeout in seconds (default 60).
Returns the parsed response hash-table, or signals an error.
A refusal of a request carrying a system prompt is retried once with
the prompt inlined (`efrit-api-inline-system-on-refusal')."
  (let ((response (efrit-api--request-sync-1 request-data timeout)))
    (if (and efrit-api-inline-system-on-refusal
             (efrit-api--refused-p response)
             (alist-get "system" request-data nil nil #'equal))
        (let ((inlined (efrit-api-inline-system request-data)))
          (efrit-log 'warn "api: refused with a system prompt; retrying with it inlined (%s)"
                     (or efrit-api-request-purpose "request"))
          (let ((r2 (efrit-api--request-sync-1 inlined timeout)))
            (when (hash-table-p r2) (puthash "efrit-inlined-system" t r2))
            r2))
      response)))

(defcustom efrit-api-sync-status t
  "When non-nil, a synchronous request shows its progress in the echo area.
A spinner, the purpose of the request and the seconds elapsed, redrawn
while the request is in flight.  The Contacting host message of
url.el is suppressed either way: it named the endpoint and then sat
there for the whole request."
  :type 'boolean
  :group 'efrit)

(defconst efrit-api--sync-spinner-frames ["⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"]
  "Text spinner frames for the synchronous request status.")

(defun efrit-api--sync-status (purpose started timeout frame)
  "The echo-area line for a synchronous request: spinner, PURPOSE, elapsed."
  (let ((elapsed (- (float-time) started)))
    (format "%s %s… %ds%s  (C-g stops the wait)"
            (aref efrit-api--sync-spinner-frames (mod frame (length efrit-api--sync-spinner-frames)))
            (or purpose "waiting for the API")
            (round elapsed)
            (if (and timeout (> timeout 0)) (format "/%ds" timeout) ""))))

(defun efrit-api--retrieve-and-wait (url timeout purpose started)
  "Fetch URL with `url-retrieve', waiting up to TIMEOUT seconds.
Returns the response buffer, or nil on timeout.  Unlike
`url-retrieve-synchronously' the wait lets timers and redisplay run,
so the mode-line spinner turns and the status line is redrawn (with
PURPOSE and the seconds since STARTED) instead of Emacs looking hung.
The process is deleted on timeout or C-g so no callback fires later."
  (let* ((done nil)
         (result nil)
         (buffer (url-retrieve url (lambda (_status) (setq done t result (current-buffer)))
                               nil t t))
         (deadline (and timeout (> timeout 0) (+ (float-time) timeout)))
         (frame 0)
         (message-log-max nil))
    (unwind-protect
        (progn
          (while (and (not done) (or (null deadline) (< (float-time) deadline)))
            (when efrit-api-sync-status
              (message "%s" (efrit-api--sync-status purpose started timeout frame))
              (cl-incf frame))
            (accept-process-output nil 0.2)
            (sit-for 0.05 t))
          (when efrit-api-sync-status (message nil))
          (and done result))
      (unless done
        ;; timeout or quit: stop the transfer, and its callback with it
        (when (and buffer (buffer-live-p buffer))
          (when-let* ((proc (get-buffer-process buffer)))
            (set-process-query-on-exit-flag proc nil)
            (set-process-sentinel proc nil)
            (set-process-filter proc nil)
            (delete-process proc))
          (let ((kill-buffer-query-functions nil))
            (kill-buffer buffer)))))))

(defun efrit-api--request-sync-1 (request-data &optional timeout)
  "One synchronous request; see `efrit-api-request-sync'."
  (let* ((api-key (efrit-common-get-api-key))
         (req (efrit-api-apply-transforms
               (efrit-common-get-api-url)
               (efrit-api-build-headers api-key)
               (efrit-api-apply-extra-body request-data)))
         (url-request-method "POST")
         (url-request-extra-headers (plist-get req :headers))
         (url-request-data (efrit-api-encode-request (plist-get req :body)))
         (url-show-status nil)
         (started (efrit-api--log-request request-data "url-retrieve (sync)"))
         (response-buffer (efrit-api--retrieve-and-wait
                           (plist-get req :url) (or timeout 60)
                           efrit-api-request-purpose started)))
    (unless response-buffer
      (error "%s" (efrit-api-describe-failure
                   (format "No response within %ds (timeout or connection error)" (or timeout 60))
                   (plist-get req :url)
                   (alist-get "model" request-data nil nil #'equal)
                   "url-retrieve (sync)")))
    (with-current-buffer response-buffer
      (unwind-protect
          (let ((response (efrit-api-parse-response)))
            (efrit-api--log-response response started)
            response)
        (kill-buffer)))))

;;; Prompt Caching
;;
;; Every request re-sends the same ~55k chars of tools schema + system
;; prompt, plus the growing conversation prefix, at full input price.
;; Anthropic prompt caching (5-minute TTL, writes 1.25x base, reads
;; 0.1x) makes iterations 2+ of an agentic loop nearly free for that
;; prefix (ef-tgt).  Request builders mark up to three breakpoints:
;; the last tool, the system prompt, and the last content block of the
;; last message (the API allows at most 4).  Marks are added at
;; request-build time on copies -- never on stored conversation
;; history, where they would accumulate past the 4-breakpoint limit.

(defcustom efrit-api-prompt-caching t
  "When non-nil, add cache_control breakpoints to Claude API requests.
Cuts input cost of multi-iteration sessions by roughly 70-90% and
improves time-to-first-token on long conversations.  Disable if a
proxy in `efrit-api-custom-headers' rejects cache_control fields."
  :type 'boolean
  :group 'efrit)

(defconst efrit-api--cache-control '(("type" . "ephemeral"))
  "The cache_control value marking an ephemeral cache breakpoint.")

(defun efrit-api--cache-marked-block (block)
  "Return a copy of content/tool BLOCK with a cache_control breakpoint.
BLOCK may be an alist (request builders) or a hash table (blocks
echoed back from parsed API responses).  Returns nil if BLOCK's shape
is not recognized, so callers can skip marking rather than corrupt
the request."
  (cond
   ((hash-table-p block)
    (let ((copy (copy-hash-table block)))
      (puthash "cache_control" efrit-api--cache-control copy)
      copy))
   ((consp (car-safe block))
    ;; Alist: cons a new pair onto the front; the shared tail is
    ;; never mutated.
    (cons (cons "cache_control" efrit-api--cache-control) block))))

(defun efrit-api-cacheable-system (system-prompt)
  "Wrap SYSTEM-PROMPT string for prompt caching.
Returns a one-element content-block vector carrying a cache_control
breakpoint, or SYSTEM-PROMPT unchanged when caching is disabled or
SYSTEM-PROMPT is not a string."
  (if (and efrit-api-prompt-caching (stringp system-prompt))
      (vector `(("type" . "text")
                ("text" . ,system-prompt)
                ("cache_control" . ,efrit-api--cache-control)))
    system-prompt))

(defun efrit-api-cacheable-tools (tools)
  "Return TOOLS vector with a cache_control breakpoint on the last entry.
Returns TOOLS unchanged when caching is disabled or TOOLS is empty.
Never mutates TOOLS (the schema is a shared constant)."
  (if (and efrit-api-prompt-caching (vectorp tools) (> (length tools) 0))
      (let* ((idx (1- (length tools)))
             (marked (efrit-api--cache-marked-block (aref tools idx))))
        (if marked
            (let ((copy (copy-sequence tools)))
              (aset copy idx marked)
              copy)
          tools))
    tools))

(defun efrit-api--block-get (obj key)
  "Get string KEY from OBJ, an alist (string or symbol keys) or hash table."
  (cond ((hash-table-p obj) (gethash key obj))
        ((listp obj) (or (cdr (assoc key obj))
                         (cdr (assq (intern key) obj))))))

(defun efrit-api--message-with-content (msg content)
  "Return a copy of message MSG with its content field replaced by CONTENT."
  (cond
   ((hash-table-p msg)
    (let ((copy (copy-hash-table msg)))
      (puthash "content" content copy)
      copy))
   ((listp msg)
    (mapcar (lambda (pair)
              (if (and (consp pair) (member (car pair) '("content" content)))
                  (cons (car pair) content)
                pair))
            msg))))

(defun efrit-api--message-with-cache-mark (msg)
  "Return a copy of MSG whose final content block has a cache breakpoint.
String content is converted to an equivalent one-block vector.
Returns nil when MSG's shape is not recognized."
  (let ((content (efrit-api--block-get msg "content")))
    (cond
     ((stringp content)
      (efrit-api--message-with-content
       msg (vector `(("type" . "text")
                     ("text" . ,content)
                     ("cache_control" . ,efrit-api--cache-control)))))
     ((and (vectorp content) (> (length content) 0))
      (let* ((idx (1- (length content)))
             (marked (efrit-api--cache-marked-block (aref content idx))))
        (when marked
          (let ((copy (copy-sequence content)))
            (aset copy idx marked)
            (efrit-api--message-with-content msg copy))))))))

(defun efrit-api-cacheable-messages (messages)
  "Return MESSAGES with a cache breakpoint on the final content block.
This caches the growing conversation prefix across loop iterations.
Non-destructive: only the last message (and its last block) is
copied, so stored history never accumulates stale breakpoints.
Returns MESSAGES unchanged when caching is disabled, MESSAGES is not
a non-empty vector, or the last message's shape is unrecognized."
  (if (and efrit-api-prompt-caching (vectorp messages)
           (> (length messages) 0))
      (let* ((idx (1- (length messages)))
             (marked (efrit-api--message-with-cache-mark (aref messages idx))))
        (if marked
            (let ((copy (copy-sequence messages)))
              (aset copy idx marked)
              copy)
          messages))
    messages))

;;; Tool Result Building

(defun efrit-api-build-tool-result (tool-id result &optional is-error)
  "Build a tool_result content block for TOOL-ID with RESULT.
When IS-ERROR is non-nil, marks the result as an error.

RESULT can be:
- A string: returned as text content
- An alist with an `image' key: returned as an image content block
- Anything else: converted to string via `format'

Returns an alist in the format required by the Anthropic API."
  (let ((content
         (cond
          ;; Check for image response format
          ((and (listp result)
                (alist-get 'image result))
           ;; Return as array containing the image block
           (vector (alist-get 'image result)))
          ;; String result - use as-is
          ((stringp result) result)
          ;; Everything else - convert to string
          (t (format "%s" result)))))
    `((type . "tool_result")
      (tool_use_id . ,tool-id)
      (content . ,content)
      ,@(when is-error '((is_error . t))))))

;;; Backward Compatibility Aliases
;; Note: These must be defined BEFORE the new variables to avoid warnings,
;; but we define them here for clarity. The warnings are harmless.

(provide 'efrit-api)

;;; efrit-api.el ends here
