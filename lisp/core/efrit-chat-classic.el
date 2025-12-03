;;; efrit-chat-classic.el --- Classic multi-turn chat API for Efrit -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai
;; URL: https://github.com/steveyegge/efrit

;;; Commentary:
;; Classic multi-turn chat mode API implementation.
;; Handles request building, response parsing, tool execution loops,
;; and multi-turn conversation management.

;;; Code:

(require 'json)
(require 'url)
(require 'cl-lib)
(require 'efrit-chat-common)
(require 'efrit-chat-buffer)
(require 'efrit-api)
(require 'efrit-tools)
(require 'efrit-chat-transparency)
(require 'efrit-do-schema)
(require 'efrit-tool-edit-buffer)

;; Forward declarations
(declare-function efrit-log-debug "efrit-log")
(declare-function efrit-log-error "efrit-log")
(declare-function efrit-tools-system-prompt "efrit-tools")
(declare-function efrit-unified-context-add-message "efrit-session")
(declare-function efrit-chat--on-message-sent "efrit-chat-persistence")
(declare-function efrit-do "efrit-do" (command))
(declare-function efrit-do--get-current-tools-schema "efrit-do-schema")

;; Variables from efrit-common
(defvar efrit-api-url)

;;; Classic Mode State Variables

(defvar-local efrit--message-history nil
  "History of message exchanges with the API.")

(defvar-local efrit--current-conversation nil
  "Current multi-turn conversation state, if active.")

(defvar-local efrit--last-user-message nil
  "Last message sent by the user, for retry purposes.")

;;; API Request Functions

(defun efrit--send-api-request (messages)
  "Send MESSAGES to the Claude API and handle the response.
In batch mode (non-interactive), uses synchronous request with timeout.
In interactive mode, uses async request with callbacks."
  (let* ((api-key (efrit--get-api-key))
         (url-request-method "POST")
         (url-request-extra-headers (efrit-api-build-headers api-key))
         (system-prompt (when efrit-enable-tools (efrit-tools-system-prompt)))
         (model (or efrit-model efrit-default-model))
         (request-data
          `(("model" . ,model)
            ("max_tokens" . ,efrit-max-tokens)
            ("temperature" . ,efrit-temperature)
            ,@(when system-prompt
                `(("system" . ,system-prompt)))
            ("messages" . ,(vconcat
                            (mapcar (lambda (msg)
                                      `(("role" . ,(alist-get 'role msg))
                                        ("content" . ,(alist-get 'content msg))))
                                    messages)))
            ,@(when efrit-enable-tools
                `(("tools" . ,(efrit-do--get-current-tools-schema))))))
         (url-request-data (efrit-api-encode-request request-data)))
    ;; In batch mode, use synchronous request (async callbacks don't work)
    ;; In interactive mode, use async request
    (if noninteractive
        ;; Batch mode: synchronous request with immediate response handling
        (condition-case err
            (let ((response-buffer (url-retrieve-synchronously
                                    (or efrit-api-url (efrit-common-get-api-url)) t t 30)))
              (if response-buffer
                  (unwind-protect
                      (with-current-buffer response-buffer
                        (efrit--handle-api-response nil))
                    (when (buffer-live-p response-buffer)
                      (kill-buffer response-buffer)))
                (with-current-buffer (efrit--setup-buffer)
                  (setq buffer-read-only nil)
                  (let ((inhibit-read-only t))
                    (setq-local efrit--response-in-progress nil)
                    (efrit--display-message "Connection timeout: Could not reach API" 'system)
                    (efrit--insert-prompt)))))
          (error
           (with-current-buffer (efrit--setup-buffer)
             (setq buffer-read-only nil)
             (let ((inhibit-read-only t))
               (setq-local efrit--response-in-progress nil)
               (efrit--display-message 
                (format "API connection error: %s" (error-message-string err))
                'system)
               (efrit--insert-prompt)))))
      ;; Interactive mode: async request with callback
      (url-retrieve (or efrit-api-url (efrit-common-get-api-url))
                    'efrit--handle-api-response nil t t))))

;;; Response Parsing

(defun efrit--parse-api-response ()
  "Parse JSON response from current buffer and return content, or signal API error.
Returns the content hash-table from the API response.
Throws an error if the response contains an API error object."
  (goto-char (point-min))
  (when (search-forward-regexp "^$" nil t)
    (let* ((json-object-type 'hash-table)
           (json-array-type 'vector)
           (json-key-type 'string)
           (coding-system-for-read 'utf-8)
           (raw-response (decode-coding-region (point) (point-max) 'utf-8 t)))
      (condition-case parse-err
          (let ((response (json-read-from-string raw-response)))
            (if-let* ((error-obj (gethash "error" response)))
                (let ((error-type (gethash "type" error-obj))
                      (error-msg (gethash "message" error-obj)))
                  (efrit-log-error "API error response: type=%s message=%s" error-type error-msg)
                  (error "API Error (%s): %s" error-type error-msg))
              (gethash "content" response)))
        (error
         (efrit-log-error "Failed to parse API response JSON: %s" parse-err)
         (efrit-log-debug "Raw response was: %s" (substring raw-response 0 (min 500 (length raw-response))))
         (error "Failed to parse API response: %s" (error-message-string parse-err)))))))

;;; HTTP Status Processing

(defun efrit--process-http-status (status)
  "Process HTTP status and return non-nil if there's an error.
If there's an error, handle it and clean up the buffer."
  (when (plist-get status :error)
    (efrit--handle-http-error (plist-get status :error))
    (when (buffer-live-p (current-buffer))
      (kill-buffer (current-buffer)))
    t))

(defun efrit--handle-http-error (error-details)
  "Handle HTTP error, providing classification and guidance.
ERROR-DETAILS contains the error information."
  (require 'efrit-log)
  (efrit-log-error "[efrit-chat] API Error: %s" error-details)
  
  (let ((error-info (efrit--classify-error error-details)))
    (let ((error-type (nth 0 error-info))
          (error-description (nth 1 error-info))
          (recommendation (nth 2 error-info)))
      
      (efrit-log-error "[efrit-chat] Error type: %s" error-type)
      
      (with-current-buffer (efrit--setup-buffer)
        (setq buffer-read-only nil)
        (let ((inhibit-read-only t))
          ;; Remove "thinking" indicator if present
          (when efrit--response-in-progress
            (save-excursion
              (goto-char (point-max))
              (when (search-backward "System: Thinking..." nil t)
                (let ((start (match-beginning 0)))
                  (when (search-forward "Thinking..." nil t)
                    (delete-region start (point)))))))

          (setq-local efrit--response-in-progress nil)

          (efrit--display-message
           (concat
            (format "⚠️ %s (%s)\n" error-description error-type)
            (format "\nDetails: %s\n" error-details)
            (format "\nWhat to do: %s\n" recommendation)
            "\nOptions: M-x efrit-retry-last-message (retry)  |  M-x efrit-chat-clear (start fresh)")
           'system)

          (efrit--insert-prompt))))))

;;; Tool Extraction and Execution

(defun efrit--extract-content-and-tools (content)
  "Extract text content and execute tool calls from CONTENT array.
Returns (list message-text tool-results should-delegate) where tool-results
is a list of tool_result blocks for sending back to Claude."
  (let ((message-text "")
        (tool-results '())
        (should-delegate nil))
    (efrit-log-debug "Processing content with %d items" (length content))
    (when content
      (dotimes (i (length content))
        (let* ((item (aref content i))
               (type (gethash "type" item)))

          ;; Handle thinking content (from extended thinking models)
          (when (string= type "thinking")
            (let ((thinking-text (gethash "thinking" item)))
              (when thinking-text
                (efrit-log-debug "Processing thinking block: %d chars" (length thinking-text))
                (efrit-transparency--display-thinking thinking-text))))

          ;; Handle text content
          (when (string= type "text")
            (let ((text (gethash "text" item)))
              (when text
                (setq message-text (concat message-text text)))))

          ;; Handle tool calls
          (when (string= type "tool_use")
            (let* ((tool-name (gethash "name" item))
                   (tool-id (gethash "id" item))
                   (input (gethash "input" item))
                   (circuit-breaker-msg (efrit-chat--circuit-breaker-check)))

              ;; Display the tool call for transparency
              (efrit-transparency--display-tool-call tool-name input)

              ;; Execute tool call (or return circuit breaker error)
              (let ((result (if circuit-breaker-msg
                                circuit-breaker-msg
                              ;; Try built-in tool first
                              (or (efrit-chat--execute-builtin-tool tool-name input)
                                  ;; Delegate to dispatcher
                                  (efrit-chat--delegate-to-dispatcher tool-name tool-id input)))))
                
                ;; Record for circuit breaker
                (let ((is-error (and (stringp result)
                                     (or (string-match-p "^Error:" result)
                                         (string-match-p "^Circuit breaker:" result)))))
                  (efrit-chat--record-tool-call (not is-error)))

                ;; Display tool result
                (efrit-transparency--display-tool-result tool-name result)

                ;; Build tool_result block
                (push (efrit-api-build-tool-result tool-id result) tool-results)))))))

    (list message-text (nreverse tool-results) should-delegate)))

;;; Response Handling

(defun efrit--handle-api-response (status)
  "Handle the API response with STATUS."
  (unless (efrit--process-http-status status)
    (condition-case api-err
        (let ((content (efrit--parse-api-response)))
          (if content
              (with-current-buffer (efrit--setup-buffer)
                (let* ((result (efrit--extract-content-and-tools content))
                       (message-text (nth 0 result))
                       (tool-results (nth 1 result))
                       (should-delegate (nth 2 result)))
                  (cond
                   ;; If we have tool results, send them back to Claude
                   ((and tool-results (> (length tool-results) 0))
                    (efrit-log-debug "Tool results collected: %d, sending back to Claude"
                                     (length tool-results))
                    (push `((role . "assistant")
                            (content . ,content))
                          efrit--message-history)
                    (push `((role . "user")
                            (content . ,(vconcat tool-results)))
                          efrit--message-history)
                    (efrit--send-api-request (reverse efrit--message-history)))

                   ;; Delegate to efrit-do for multi-step completion
                   (should-delegate
                    (efrit-log-debug "Delegating to efrit-do")
                    (efrit--delegate-to-do))

                   ;; Normal response handling (no tools, just text)
                   (t
                    (efrit--update-ui-with-response message-text)))))
            (efrit--handle-parse-error)))
      (error
       (let ((error-msg (error-message-string api-err)))
         (if (string-match-p "^API Error" error-msg)
             (efrit--handle-http-error error-msg)
           (efrit--handle-parse-error)))))))

(defun efrit--handle-parse-error ()
  "Handle parsing errors by displaying error message and resetting UI."
  (with-current-buffer (efrit--setup-buffer)
    (setq buffer-read-only nil)
    (let ((inhibit-read-only t))
      (when efrit--response-in-progress
        (save-excursion
          (goto-char (point-max))
          (when (search-backward "System: Thinking..." nil t)
            (let ((start (match-beginning 0)))
              (when (search-forward "Thinking..." nil t)
                (delete-region start (point)))))))

      (setq-local efrit--response-in-progress nil)
      (efrit--display-message "Error parsing API response" 'system)
      (efrit--insert-prompt))))

(defun efrit--update-ui-with-response (message-text)
  "Update the chat UI with MESSAGE-TEXT response."
  (with-current-buffer (efrit--setup-buffer)
    (setq buffer-read-only nil)
    (let ((inhibit-read-only t))
      ;; Remove "thinking" indicator
      (when efrit--response-in-progress
        (save-excursion
          (goto-char (point-max))
          (when (search-backward "System: Thinking..." nil t)
            (let ((start (match-beginning 0)))
              (when (search-forward "Thinking..." nil t)
                (delete-region start (point)))))))

      (setq-local efrit--response-in-progress nil)

      ;; Display the response
      (efrit-transparency--display-incremental message-text)

      ;; Add to conversation history
      (push `((role . "assistant")
              (content . ,message-text))
            efrit--message-history)
      
      ;; Also add to unified context
      (require 'efrit-session)
      (efrit-unified-context-add-message 'assistant message-text 'chat)
      
      ;; Auto-save session
      (require 'efrit-chat-persistence)
      (efrit-chat--on-message-sent)

      ;; Insert prompt for next user input
      (efrit--insert-prompt))))

;;; Delegation

(defun efrit--delegate-to-do ()
  "Delegate current request to efrit-do for multi-step completion."
  (let ((original-request (when efrit--message-history
                            (let ((first-msg (car (last efrit--message-history))))
                              (when (eq (alist-get 'role first-msg) 'user)
                                (alist-get 'content first-msg))))))
    (when original-request
      (with-current-buffer (efrit--setup-buffer)
        (setq buffer-read-only nil)
        (let ((inhibit-read-only t))
          (when efrit--response-in-progress
            (save-excursion
              (goto-char (point-max))
              (when (search-backward "System: Thinking..." nil t)
                (let ((start (match-beginning 0)))
                  (when (search-forward "Thinking..." nil t)
                    (delete-region start (point)))))))

          (setq-local efrit--response-in-progress nil)

          (efrit--display-message
           "[Delegating to efrit-do for multi-step task completion...]"
           'system)

          (efrit--insert-prompt)

          (run-at-time 0.1 nil
                       (lambda ()
                         (require 'efrit-do)
                         (message "Executing via efrit-do: %s" original-request)
                         (efrit-do original-request))))))))

;;; Deprecated Functions

(defun efrit--detect-incomplete-task (_content _message-text)
  "Detect if CONTENT represents an incomplete multi-step task.
Returns non-nil if the task appears incomplete and needs delegation.

DEPRECATED: This function violated the Pure Executor principle by using
pattern matching to make decisions. Multi-turn completion should be
handled by asking Claude explicitly via efrit-multi-turn.el, not by
parsing response text. Currently disabled - always returns nil."
  nil)

(provide 'efrit-chat-classic)

;;; efrit-chat-classic.el ends here
