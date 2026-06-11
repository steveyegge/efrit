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
;; Both efrit-chat-api.el and efrit-executor.el should use this layer
;; instead of duplicating HTTP logic.

;;; Code:

(require 'json)
(require 'url)
(require 'cl-lib)
(require 'efrit-common)

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

(defun efrit-api-build-headers (api-key)
  "Build HTTP headers for API requests with API-KEY.
Applies header customization from `efrit-api-custom-headers' and
`efrit-api-excluded-headers'."
  (efrit-common--validate-api-key api-key)
  (let ((default-headers `(("x-api-key" . ,api-key)
                           ("anthropic-version" . ,efrit-common-api-version)
                           ("anthropic-beta" . "max-tokens-3-5-sonnet-2024-07-15")
                           ("content-type" . "application/json"))))
    ;; Remove excluded headers
    (when efrit-api-excluded-headers
      (setq default-headers
            (cl-remove-if (lambda (header)
                            (member (car header) efrit-api-excluded-headers))
                          default-headers)))
    ;; Add custom headers (custom headers override defaults)
    (append efrit-api-custom-headers default-headers)))

;;; JSON Encoding

(defun efrit-api-encode-request (data)
  "Encode DATA as JSON with proper Unicode escaping for HTTP transmission.
Returns a UTF-8 encoded string suitable for url-request-data."
  (let* ((json-string (json-encode data))
         (escaped-json (efrit-common-escape-json-unicode json-string)))
    (encode-coding-string escaped-json 'utf-8)))

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
          (format "API Error (%s): %s"
                  (or (gethash "type" error-obj) "unknown")
                  (or (gethash "message" error-obj) "unknown error")))))))

(defun efrit-api-extract-content (response)
  "Extract content array from API RESPONSE hash-table."
  (gethash "content" response))

(defun efrit-api-extract-stop-reason (response)
  "Extract stop_reason from API RESPONSE hash-table."
  (gethash "stop_reason" response))

;;; Async Request

(defun efrit-api-request-async (request-data callback &optional error-callback)
  "Send REQUEST-DATA to Claude API asynchronously.
Calls CALLBACK with (RESPONSE) on success.
Calls ERROR-CALLBACK with (ERROR-MESSAGE) on failure, or signals error if nil."
  (condition-case err
      (let* ((api-key (efrit-common-get-api-key))
             (url-request-method "POST")
             (url-request-extra-headers (efrit-api-build-headers api-key))
             (url-request-data (efrit-api-encode-request request-data)))
        (url-retrieve
         (efrit-common-get-api-url)
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
                         (funcall callback response)))
                   (error
                    (if error-callback
                        (funcall error-callback (error-message-string url-err))
                      (error "%s" (error-message-string url-err)))))
               (when (buffer-live-p response-buffer)
                 (kill-buffer response-buffer)))))
         nil t t))
    (error
     (if error-callback
         (funcall error-callback (error-message-string err))
       (signal (car err) (cdr err))))))

;;; Sync Request

(defun efrit-api-request-sync (request-data &optional timeout)
  "Send REQUEST-DATA to Claude API synchronously.
TIMEOUT is optional timeout in seconds (default 60).
Returns the parsed response hash-table, or signals an error."
  (let* ((api-key (efrit-common-get-api-key))
         (url-request-method "POST")
         (url-request-extra-headers (efrit-api-build-headers api-key))
         (url-request-data (efrit-api-encode-request request-data))
         (response-buffer (url-retrieve-synchronously
                           (efrit-common-get-api-url)
                           nil t (or timeout 60))))
    (unless response-buffer
      (error "Failed to get response from API (timeout or connection error)"))
    (with-current-buffer response-buffer
      (unwind-protect
          (efrit-api-parse-response)
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
