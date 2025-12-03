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
      (condition-case parse-err
          (let ((response (json-read-from-string raw-response)))
            ;; Check if the API returned an error object
            (if-let* ((error-obj (gethash "error" response)))
                (let ((error-type (gethash "type" error-obj))
                      (error-msg (gethash "message" error-obj)))
                  (error "API Error (%s): %s" error-type error-msg))
              response))
        (error
         (error "Failed to parse API response: %s" (error-message-string parse-err)))))))

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
           (unwind-protect
               (condition-case url-err
                   (progn
                     (when (plist-get status :error)
                       (error "HTTP error: %s" (plist-get status :error)))
                     (let ((response (efrit-api-parse-response)))
                       (funcall callback response)))
                 (error
                  (if error-callback
                      (funcall error-callback (error-message-string url-err))
                    (error "%s" (error-message-string url-err)))))
             (when (buffer-live-p (current-buffer))
               (kill-buffer (current-buffer)))))
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
