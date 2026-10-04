;;; efrit-ask.el --- One-shot side requests to the model -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.9.2
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, ai

;;; Commentary:

;; A side question to the model that is not a turn of any session: a
;; commit message from a diff, a rewrite of a region, three candidate
;; phrasings, a title.  Nothing lands in the agent buffer or the REPL
;; history.
;;
;; `efrit-ask-once' wraps `efrit-api-request-async' with the three
;; things every caller got wrong on its own (after copilot-chat's
;; one-shot design, 2026-09-28):
;;
;; - the reply callback fires exactly once, whatever the transport
;;   does (a retry with the system prompt inlined, a watchdog racing
;;   the response);
;; - a newer ask with the same KEY supersedes an older one: the older
;;   reply is dropped when it arrives, so a user who asks twice sees
;;   the answer to the second question, not to the first, later;
;; - the request is issued from a hidden buffer that outlives the
;;   caller's, so the callback is not lost when the caller's buffer
;;   is killed while the model thinks.
;;
;; The callback receives the answer text (the text blocks joined) or,
;; on failure, nil and a message.  `efrit-ask-candidates' asks for N
;; variants in one request, separated by a marker the model is told
;; to emit, and splits them.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'efrit-log)
(require 'efrit-api)
(require 'efrit-chat-response)
(require 'efrit-config)

(defgroup efrit-ask nil
  "One-shot side requests."
  :group 'efrit)

(defcustom efrit-ask-model nil
  "Model for side requests; nil means `efrit-default-model'."
  :type '(choice (const :tag "The default model" nil) string)
  :group 'efrit-ask)

(defcustom efrit-ask-max-tokens 4096
  "Default max_tokens for a side request.
Room for a thinking model to think before it answers (1200 was eaten
by thinking on the prompt suggester, 2026-09-26)."
  :type 'integer
  :group 'efrit-ask)

(defconst efrit-ask-candidate-separator "<endCompletion>"
  "The marker the model puts between candidates in `efrit-ask-candidates'.")

(defvar efrit-ask--versions (make-hash-table :test 'equal)
  "KEY -> the version number of the latest ask with that key.")

(defvar efrit-ask--buffer-name " *efrit-ask*"
  "The hidden buffer side requests are issued from.")

(cl-defstruct (efrit-ask (:constructor efrit-ask--make))
  key version purpose (done nil) cancelled)

(defun efrit-ask--buffer ()
  "The always-live buffer requests are issued from."
  (or (get-buffer efrit-ask--buffer-name)
      (with-current-buffer (get-buffer-create efrit-ask--buffer-name)
        (setq buffer-undo-list t)
        (current-buffer))))

(defun efrit-ask-response-text (response)
  "The text blocks of RESPONSE joined, or \"\"."
  (let ((content (efrit-response-content response)) (texts nil))
    (when content
      (dotimes (i (length content))
        (let ((item (aref content i)))
          (when (and (hash-table-p item) (equal (gethash "type" item) "text"))
            (push (gethash "text" item) texts)))))
    (string-join (nreverse texts) "")))

(defun efrit-ask--describe-empty (response)
  "Why RESPONSE carried no text, for the user."
  (let* ((content (efrit-response-content response))
         (types (and content (mapcar (lambda (b) (and (hash-table-p b) (gethash "type" b)))
                                     (append content nil))))
         (stop (efrit-response-stop-reason response)))
    (if (equal stop "max_tokens")
        (format "the answer was cut at max_tokens before any text (blocks: %s)"
                (mapconcat (lambda (x) (format "%s" x)) types ", "))
      (format "the model returned no text (stop_reason %s, blocks: %s)"
              stop (mapconcat (lambda (x) (format "%s" x)) types ", ")))))

(cl-defun efrit-ask-once (prompt callback &key system purpose key max-tokens model)
  "Ask the model PROMPT once, off any session; CALLBACK gets the answer.
CALLBACK is called exactly once with (TEXT nil) on success or
\(nil MESSAGE) on failure.  SYSTEM is an optional system prompt.
PURPOSE names the request in the log.  KEY (a string; default
PURPOSE) groups asks that supersede each other: when a newer ask with
the same KEY is issued before this one answers, this one's reply is
dropped and CALLBACK gets (nil \"superseded\").  Returns an `efrit-ask'
handle; `efrit-ask-cancel' abandons it.

The request is sent from a hidden buffer, so the caller's buffer may
die meanwhile."
  (let* ((key (or key purpose "efrit-ask"))
         (version (1+ (gethash key efrit-ask--versions 0)))
         (ask (efrit-ask--make :key key :version version :purpose purpose))
         (finish (lambda (text message)
                   ;; once, and only if still current
                   (unless (efrit-ask-done ask)
                     (setf (efrit-ask-done ask) t)
                     (cond
                      ((efrit-ask-cancelled ask)
                       (efrit-log 'debug "ask %s: reply after cancel dropped" key))
                      ((/= version (gethash key efrit-ask--versions 0))
                       (efrit-log 'debug "ask %s: reply v%d dropped, v%d is current"
                                  key version (gethash key efrit-ask--versions 0))
                       (funcall callback nil "superseded"))
                      (t (funcall callback text message)))))))
    (puthash key version efrit-ask--versions)
    (let ((request `(("model" . ,(or model efrit-ask-model efrit-default-model))
                     ("max_tokens" . ,(or max-tokens efrit-ask-max-tokens))
                     ,@(when system `(("system" . ,(efrit-api-cacheable-system system))))
                     ("messages" . [(("role" . "user") ("content" . ,prompt))]))))
      (with-current-buffer (efrit-ask--buffer)
        (let ((efrit-api-request-purpose (or purpose "a side request")))
          (condition-case err
              (efrit-api-request-async
               request
               (lambda (response)
                 (cond
                  ((null response) (funcall finish nil "no response"))
                  ((efrit-response-error response)
                   (funcall finish nil (efrit-error-message (efrit-response-error response))))
                  (t (let ((text (efrit-ask-response-text response)))
                       (if (string-empty-p (string-trim text))
                           (funcall finish nil (efrit-ask--describe-empty response))
                         (funcall finish text nil))))))
               (lambda (message) (funcall finish nil (format "%s" message))))
            (error (funcall finish nil (error-message-string err)))))))
    ask))

(defun efrit-ask-cancel (ask)
  "Abandon ASK: its reply, if it comes, is dropped without a callback."
  (when (efrit-ask-p ask)
    (setf (efrit-ask-cancelled ask) t)
    t))

(defun efrit-ask-strip-fence (text)
  "TEXT without a code fence wrapped around the whole of it."
  (let ((s (string-trim text)))
    (if (string-match "\\````[^\n]*\n\\(\\(?:.\\|\n\\)*?\\)\n?```\\'" s)
        (string-trim (match-string 1 s))
      s)))

(defun efrit-ask-split-candidates (text)
  "The candidates in TEXT, split on `efrit-ask-candidate-separator', trimmed, non-empty."
  (cl-remove-if #'string-empty-p
                (mapcar (lambda (c) (efrit-ask-strip-fence (string-trim c)))
                        (split-string text (regexp-quote efrit-ask-candidate-separator)))))

(cl-defun efrit-ask-candidates (prompt count callback &key system purpose key max-tokens)
  "Ask for COUNT candidate answers to PROMPT in one request.
CALLBACK gets (CANDIDATES nil), a list of strings in the model's
order (fewer than COUNT if it gave fewer), or (nil MESSAGE).  Other
arguments as `efrit-ask-once'."
  (efrit-ask-once
   (format "%s\n\nGive %d different candidates.  Put exactly the marker %s on its own line between candidates, nothing before the first and nothing after the last.  No numbering, no commentary."
           prompt count efrit-ask-candidate-separator)
   (lambda (text message)
     (if text
         (funcall callback (efrit-ask-split-candidates text) nil)
       (funcall callback nil message)))
   :system system :purpose purpose :key key :max-tokens max-tokens))

(provide 'efrit-ask)

;;; efrit-ask.el ends here
