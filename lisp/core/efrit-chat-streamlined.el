;;; efrit-chat-streamlined.el --- Streamlined chat API for Efrit -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai
;; URL: https://github.com/steveyegge/efrit

;;; Commentary:
;; Streamlined single/multi-turn chat mode API implementation.
;; Uses a simplified conversation model with turn limits and
;; an enhanced system prompt for action-oriented responses.

;;; Code:

(require 'json)
(require 'url)
(require 'cl-lib)
(require 'efrit-chat-common)
(require 'efrit-api)
(require 'efrit-chat-buffer)
(require 'efrit-do-schema)
(require 'efrit-spinner)

;; Forward declarations
(declare-function efrit-log-debug "efrit-log")
(declare-function efrit-tools-system-prompt "efrit-tools")
(declare-function efrit-tools-get-context "efrit-tools")
(declare-function efrit-do--get-current-tools-schema "efrit-do-schema")
(declare-function efrit-do--execute-tool "efrit-do-dispatch")

;; Variables from efrit-common
(defvar efrit-api-url)

;;; Streamlined Mode Variables

(defvar efrit-streamlined--current-messages nil
  "Messages for the current conversation turn.")

(defvar efrit-streamlined--turn-count 0
  "Current turn count for the conversation.")

;;; Customization

(defcustom efrit-max-turns 2
  "Maximum number of turns in a conversation for streamlined interface."
  :type 'integer
  :group 'efrit)

;;; System Prompt

(defun efrit-streamlined--system-prompt ()
  "Generate enhanced system prompt for streamlined chat experience."
  (concat
   (if (fboundp 'efrit-tools-system-prompt)
       (efrit-tools-system-prompt)
     "You are Efrit, an Emacs assistant.")
   "\n\n### Response Guidelines\n"
   "Determine whether the user's request is primarily an ACTION (do something) or INFORMATION (explain/show something):\n\n"

   "**For ACTION requests** (write, create, make, add, fix, etc.):\n"
   "- Execute the requested action using Elisp code\n"
   "- Provide minimal chat output (e.g., \"Done.\" or \"Created haiku in *vim-haiku* buffer.\")\n"
   "- Complete simple tasks in a single turn\n"
   "- Don't break straightforward operations into multiple steps\n\n"

   "**For INFORMATION requests** (how, what, explain, show, etc.):\n"
   "- Provide comprehensive, well-formatted responses\n"
   "- Include examples, explanations, and details as appropriate\n"
   "- Structure information clearly with headers and formatting\n\n"

   "### Single vs Multi-turn\n"
   "- Complete simple requests in ONE turn whenever possible\n"
   "- Only use multiple turns for genuinely complex multi-step operations\n"
   "- Don't artificially break down straightforward tasks\n"))

;;; Logging

(defun efrit-streamlined--log-to-work (message)
  "Log MESSAGE to the work buffer for debugging."
  (when (fboundp 'efrit-log-debug)
    (efrit-log-debug "[streamlined] %s" message)))

;;; Display

(defun efrit-streamlined--display-response (text)
  "Display TEXT response in the minibuffer or appropriate location."
  (message "Efrit: %s" (if (> (length text) 100)
                           (concat (substring text 0 100) "...")
                         text)))

;;; API Request Functions

(defun efrit-streamlined--send-request (messages)
  "Send MESSAGES with enhanced streamlined prompt."
  (efrit-streamlined--log-to-work
   (format "Sending request with %d messages" (length messages)))

  ;; Start spinner
  (with-current-buffer (efrit--setup-buffer)
    (efrit-spinner-start))

  (let* ((api-key (efrit--get-api-key))
         (url-request-method "POST")
         (url-request-extra-headers (efrit-api-build-headers api-key))
         (system-prompt (efrit-streamlined--system-prompt))
         (cleaned-messages (mapcar (lambda (msg)
                                     (let ((content (alist-get 'content msg)))
                                       `(("role" . ,(alist-get 'role msg))
                                         ("content" . ,(if (stringp content)
                                                           (substring-no-properties content)
                                                         content)))))
                                   messages))
         (model (or efrit-model (require 'efrit-config) efrit-default-model))
         (request-data
          `(("model" . ,model)
            ("max_tokens" . ,efrit-max-tokens)
            ("temperature" . ,efrit-temperature)
            ("system" . ,system-prompt)
            ("messages" . ,(vconcat cleaned-messages))
            ,@(when efrit-enable-tools
                `(("tools" . ,(efrit-do--get-current-tools-schema))))))
         (url-request-data (efrit-api-encode-request request-data)))

    (efrit-streamlined--log-to-work
     (format "Request: %s characters, %s tools enabled"
             (length url-request-data)
             (if efrit-enable-tools "tools" "no tools")))

    (url-retrieve (or efrit-api-url (efrit-common-get-api-url))
                  'efrit-streamlined--handle-response nil t t)))

;;; Response Handling

(defun efrit-streamlined--handle-response (_status)
  "Handle API response with work buffer logging."
  ;; Stop the spinner
  (with-current-buffer (efrit--setup-buffer)
    (efrit-spinner-stop))
  
  (efrit-streamlined--log-to-work "Received response")

  (goto-char (point-min))
  (search-forward "\n\n")
  (let* ((response-text (buffer-substring-no-properties (point) (point-max)))
         (response-data (condition-case err
                            (json-read-from-string response-text)
                          (error
                           (efrit-streamlined--log-to-work
                            (format "JSON parse error: %s" err))
                           nil))))

    (if (not response-data)
        (efrit-streamlined--log-to-work "Failed to parse API response")

      ;; Check for API errors
      (if-let* ((error-obj (alist-get 'error response-data)))
          (let ((error-type (alist-get 'type error-obj))
                (error-message (alist-get 'message error-obj)))
            (efrit-streamlined--log-to-work
             (format "API ERROR: %s - %s" error-type error-message))
            (message "Efrit: API Error - %s" error-message)
            (efrit-streamlined--display-response
             (format "API Error (%s): %s" error-type error-message)))

        ;; Extract content
        (let* ((content-array (alist-get 'content response-data))
               (text-content "")
               (tool-uses (efrit-streamlined--extract-tool-uses response-data))
               (has-text-content nil))

          ;; Collect all text content
          (when (and content-array (vectorp content-array))
            (dotimes (i (length content-array))
              (let* ((content-item (aref content-array i))
                     (type (alist-get 'type content-item)))
                (when (string-equal type "text")
                  (when-let* ((text (alist-get 'text content-item)))
                    (setq text-content (concat text-content text))
                    (setq has-text-content t))))))

          (efrit-streamlined--log-to-work
           (format "Response: %s chars text, %d tools"
                   (length text-content)
                   (length tool-uses)))

          ;; Execute tools and collect results
          (let ((tool-results '()))
            (when tool-uses
              (setq tool-results (efrit-streamlined--execute-tools tool-uses)))

            ;; Check if Claude wants to continue
            (let* ((at-turn-limit (>= efrit-streamlined--turn-count efrit-max-turns))
                   (claude-signals-done (or (string-match-p
                                             "\\bdone\\b\\|\\bcomplete\\b\\|\\bfinished\\b\\|\\bfinal\\b"
                                             (downcase (or text-content "")))
                                            (and tool-uses (not has-text-content))))
                   (should-continue (and tool-uses
                                         (not claude-signals-done)
                                         (not at-turn-limit))))

              (efrit-streamlined--log-to-work
               (format "Turn %d/%d: %s"
                       efrit-streamlined--turn-count efrit-max-turns
                       (if should-continue "continuing" "finishing")))

              (when at-turn-limit
                (message "Efrit: Turn limit reached (%d)" efrit-max-turns))

              (if should-continue
                  (progn
                    (setq efrit-streamlined--turn-count (1+ efrit-streamlined--turn-count))
                    (efrit-streamlined--log-to-work "Continuing conversation with tool results")
                    (efrit-streamlined--continue-with-results tool-results content-array))
                (progn
                  (message "Efrit: Complete")
                  (when has-text-content
                    (efrit-streamlined--display-response text-content)))))))))))

;;; Tool Extraction

(defun efrit-streamlined--extract-tool-uses (response-data)
  "Extract tool_use blocks from RESPONSE-DATA."
  (condition-case err
      (let* ((content-array (alist-get 'content response-data))
             (tool-uses '()))
        (when (and content-array (vectorp content-array))
          (dotimes (i (length content-array))
            (let* ((content-item (aref content-array i))
                   (type (alist-get 'type content-item)))
              (when (and type (string-equal type "tool_use"))
                (push content-item tool-uses)))))
        (nreverse tool-uses))
    (error
     (efrit-streamlined--log-to-work
      (format "Tool extraction error: %s" (error-message-string err)))
     '())))

;;; Tool Execution

(defun efrit-streamlined--execute-tools (tool-uses)
  "Execute TOOL-USES and return list of tool_result blocks for continuation."
  (efrit-streamlined--log-to-work
   (format "Executing %d tools" (length tool-uses)))

  (let ((results '()))
    (dolist (tool-use tool-uses)
      (let* ((tool-name (alist-get 'name tool-use))
             (tool-input (alist-get 'input tool-use))
             (tool-id (alist-get 'id tool-use))
             (circuit-breaker-msg (efrit-chat--circuit-breaker-check)))

        (efrit-streamlined--log-to-work
         (format "Tool: %s (id: %s)" tool-name tool-id))

        ;; Convert alist input to hash-table for shared helper
        (let* ((input-hash (make-hash-table :test 'equal))
               (_ (dolist (pair tool-input)
                    (puthash (symbol-name (car pair)) (cdr pair) input-hash)))
               (result-content
                (if circuit-breaker-msg
                    (progn
                      (efrit-streamlined--log-to-work circuit-breaker-msg)
                      circuit-breaker-msg)
                  ;; Try built-in tool first
                  (or (efrit-chat--execute-builtin-tool tool-name input-hash)
                      ;; Delegate to dispatcher
                      (efrit-chat--delegate-to-dispatcher tool-name tool-id input-hash)))))

          ;; Record tool call for circuit breaker
          (let ((is-error (and (stringp result-content)
                               (or (string-match-p "^Error:" result-content)
                                   (string-match-p "^Circuit breaker:" result-content)))))
            (efrit-chat--record-tool-call (not is-error)))

          ;; Build proper tool_result block using helper
          (push (efrit-api-build-tool-result tool-id result-content) results))))

    (nreverse results)))

;;; Conversation Continuation

(defun efrit-streamlined--continue-with-results (tool-results assistant-content)
  "Continue conversation with TOOL-RESULTS from executed tools.
TOOL-RESULTS is a list of tool_result blocks from `efrit-api-build-tool-result'.
ASSISTANT-CONTENT is the original content array from the assistant response."
  (let ((updated-messages efrit-streamlined--current-messages))

    ;; Add assistant's response WITH tool_use blocks
    (when assistant-content
      (setq updated-messages
            (append updated-messages
                    (list `((role . "assistant")
                            (content . ,assistant-content))))))

    ;; Add user message with tool_result blocks
    (when (and tool-results (> (length tool-results) 0))
      (setq updated-messages
            (append updated-messages
                    (list `((role . "user")
                            (content . ,(vconcat tool-results)))))))

    ;; Continue conversation with results
    (when (> (length tool-results) 0)
      (setq efrit-streamlined--current-messages updated-messages)
      (efrit-streamlined--send-request updated-messages))))

(provide 'efrit-chat-streamlined)

;;; efrit-chat-streamlined.el ends here
