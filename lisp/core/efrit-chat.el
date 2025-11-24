;;; efrit-chat.el --- Conversational assistant for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.3.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai
;; URL: https://github.com/steveyegge/efrit

;;; Commentary:
;; Unified chat interface for Efrit, providing both:
;; - Classic interactive chat mode (efrit-chat)
;; - Streamlined single-turn mode (efrit-streamlined-send)
;;
;; This consolidates functionality from efrit-chat.el and efrit-chat-streamlined.el
;; into a single module with two complementary interfaces.

;;; Code:

(require 'json)
(require 'url)
(require 'cl-lib)
(require 'auth-source)
(require 'efrit-tools)
(require 'efrit-session)

;; Declare functions from other modules to avoid warnings
(declare-function efrit-common-get-api-key "efrit-common")
(declare-function efrit-do "efrit-do" (command))
(declare-function efrit-tools-system-prompt "efrit-tools")
(declare-function efrit-tools-get-context "efrit-tools")
(declare-function efrit-common-get-api-url "efrit-common")

;;; Customization

(defgroup efrit nil
  "Efrit conversational assistant for Emacs."
  :group 'tools
  :prefix "efrit-")

(defcustom efrit-buffer-name "*efrit-chat*"
  "Name of the Efrit conversation buffer."
  :type 'string
  :group 'efrit)

;; Use centralized model configuration
(require 'efrit-config)
(defvaralias 'efrit-model 'efrit-default-model)

(defcustom efrit-max-tokens 8192
  "Maximum number of tokens in the response.
Claude 3.5 Sonnet supports up to 8192 tokens with beta headers,
or 4096 without. This setting uses the higher limit."
  :type 'integer
  :group 'efrit)

(defcustom efrit-temperature 0.1
  "Temperature setting for response generation (0.0-1.0)."
  :type 'float
  :group 'efrit)

(defcustom efrit-show-timestamps nil
  "Whether to show timestamps in chat messages.
When non-nil, each message will be prefixed with a timestamp."
  :type 'boolean
  :group 'efrit)

;; Use centralized API URL - legacy variable kept for compatibility
(defcustom efrit-api-url nil
  "Legacy API URL setting. Use efrit-api-base-url in efrit-common instead.
When nil, uses the centralized configuration."
  :type '(choice (const :tag "Use centralized config" nil)
                 (string :tag "Legacy URL override"))
  :group 'efrit)

(defcustom efrit-enable-tools t
  "Whether to enable tools for the assistant."
  :type 'boolean
  :group 'efrit)

(defcustom efrit-max-retries 3
  "Maximum number of retry attempts when tool execution fails in chat mode."
  :type 'integer
  :group 'efrit)

(defcustom efrit-retry-on-errors t
  "Whether to automatically retry failed tool executions in chat mode."
  :type 'boolean
  :group 'efrit)

(defcustom efrit-custom-headers nil
  "Alist of custom headers to add to API requests.
Each element should be a cons cell of (HEADER-NAME . HEADER-VALUE).
Example: \\='((\"authorization\" . \"Bearer your-token\")
             (\"custom-header\" . \"custom-value\"))"
  :type '(alist :key-type string :value-type string)
  :group 'efrit)

(defcustom efrit-excluded-headers nil
  "List of default header names to exclude from API requests.
Example: \\='(\"anthropic-version\" \"anthropic-beta\")"
  :type '(repeat string)
  :group 'efrit)

;;; Streamlined Mode Customization

(defcustom efrit-work-buffer-name "*efrit-work*"
  "Name of the buffer for detailed work/thinking (streamlined mode)."
  :type 'string
  :group 'efrit)

(defcustom efrit-show-work-buffer nil
  "Whether to automatically show the work buffer during operations."
  :type 'boolean
  :group 'efrit)

(defcustom efrit-work-buffer-max-size 100000
  "Maximum size of work buffer before truncation (0 = no limit)."
  :type 'integer
  :group 'efrit)

(defcustom efrit-max-turns 2
  "Maximum number of turns in a conversation for streamlined interface."
  :type 'integer
  :group 'efrit)

;;; Internal variables - Classic Chat Mode

(defvar-local efrit--message-history nil
  "History of message exchanges with the API.")

(defvar-local efrit--conversation-marker nil
  "Marker for the end of the conversation area.")

(defvar-local efrit--input-marker nil
  "Marker for the start of the input area.")

(defvar-local efrit--response-in-progress nil
  "Flag indicating whether a response is in progress.")

(defvar-local efrit--current-conversation nil
  "Current multi-turn conversation state, if active.")

(defvar-local efrit--last-user-message nil
  "Last message sent by the user, for retry purposes.")

;;; Internal variables - Streamlined Mode

(defvar efrit-streamlined--current-messages nil
  "Messages for the current conversation turn.")

(defvar efrit-streamlined--turn-count 0
  "Current turn count for the conversation.")

;;; Helper Functions

(defun efrit--safe-error-message (err)
  "Safely get error message from ERR, returning \\='Unknown error\\=' if nil."
  (cond
   ((null err) "Unknown error (nil)")
   ((stringp err) err)
   ((and (listp err) (car err))
    (condition-case nil
        (error-message-string err)
      (error "Unknown error (failed to extract message)")))
   (t "Unknown error (invalid format)")))

(defun efrit--sanitize-chat-text (text)
  "Remove technical artifacts from TEXT for clean chat display."
  (let ((cleaned text))
    ;; Remove any remaining [Result: ...] tags
    (setq cleaned (replace-regexp-in-string "\\[Result:[^]]*\\]" "" cleaned))
    ;; Remove empty lines and excessive whitespace
    (setq cleaned (replace-regexp-in-string "\n\n+" "\n\n" cleaned))
    (string-trim cleaned)))

(defun efrit--build-tool-result (tool-id result)
  "Build a tool_result content block for TOOL-ID with RESULT.
Returns an alist in the format required by the Anthropic API:
  ((type . \"tool_result\")
   (tool_use_id . TOOL-ID)
   (content . RESULT-STRING))"
  `((type . "tool_result")
    (tool_use_id . ,tool-id)
    (content . ,(format "%s" result))))

;;; Faces

(defface efrit-user-face
  '((t :inherit font-lock-keyword-face))
  "Face for user messages in Efrit buffer."
  :group 'efrit)

(defface efrit-assistant-face
  '((t :inherit font-lock-doc-face))
  "Face for assistant responses in Efrit buffer."
  :group 'efrit)

(defface efrit-system-face
  '((t :inherit font-lock-comment-face))
  "Face for system messages in Efrit buffer."
  :group 'efrit)

(defface efrit-prompt-face
  '((t :inherit minibuffer-prompt :weight bold))
  "Face for the input prompt."
  :group 'efrit)

;;; Utility functions

(defun efrit--get-api-key ()
  "Get the Anthropic API key from .authinfo file."
  (require 'efrit-common)
  (efrit-common-get-api-key))

;;; Buffer management - Classic Chat Mode

(defun efrit--setup-buffer ()
  "Create and set up the Efrit buffer."
  (let ((buffer (get-buffer-create efrit-buffer-name)))
    (with-current-buffer buffer
      (unless (eq major-mode 'efrit-mode)
        (efrit-mode))
      ;; Always ensure buffer is editable
      (setq buffer-read-only nil)
      ;; Ensure we have markers for conversation and input areas
      (when (not efrit--conversation-marker)
        (setq-local efrit--conversation-marker (make-marker)))
      (when (not efrit--input-marker)
        (setq-local efrit--input-marker (make-marker))))
    buffer))

(defun efrit--insert-prompt ()
  "Insert a prompt in the buffer for user input."
  (with-current-buffer (efrit--setup-buffer)
    ;; Always ensure buffer is editable
    (setq buffer-read-only nil)
    (let ((inhibit-read-only t))
      ;; Go to end of buffer
      (goto-char (point-max))

      ;; Add spacing before input area (single newline since message already has one)
      (unless (bobp) (insert "\n"))

      ;; Set the marker for beginning of input area
      (set-marker efrit--input-marker (point))

      ;; Insert the prompt
      (let ((prompt-start (point)))
        (insert "> ")
        (add-text-properties prompt-start (point) '(face efrit-prompt-face)))

      ;; Ensure buffer is left editable
      (setq buffer-read-only nil))))

(defun efrit--display-message (message role)
  "Display MESSAGE in the Efrit buffer with ROLE (user, assistant, or system)."
  (with-current-buffer (efrit--setup-buffer)
    ;; Always ensure buffer is editable
    (setq buffer-read-only nil)
    (let ((inhibit-read-only t)
          (face (cond
                ((eq role 'user) 'efrit-user-face)
                ((eq role 'assistant) 'efrit-assistant-face)
                ((eq role 'system) 'efrit-system-face)
                (t nil)))
          (prefix (cond
                  ((eq role 'user) "You: ")
                  ((eq role 'assistant) "Assistant: ")
                  ((eq role 'system) "System: ")
                  (t "")))
          (timestamp (when efrit-show-timestamps
                      (format-time-string "[%H:%M:%S] "))))
      ;; All messages go to the conversation area (before input area)
      (if (and efrit--conversation-marker (marker-position efrit--conversation-marker))
          (goto-char (marker-position efrit--conversation-marker))
        ;; Fallback: go to input marker or end of buffer
        (if (and efrit--input-marker (marker-position efrit--input-marker))
            (goto-char (marker-position efrit--input-marker))
          (goto-char (point-max))))

      ;; Add spacing before message (single newline to separate from previous content)
      (unless (bobp) (insert "\n"))

      ;; Insert the message with appropriate prefix and optional timestamp
      (let ((start (point)))
        (when timestamp (insert timestamp))
        (insert prefix message "\n")  ; Add newline after message
        (when face
          (add-text-properties start (- (point) 1) `(face ,face))))  ; Don't apply face to newline

      ;; Update conversation marker to end of this message
      (set-marker efrit--conversation-marker (point))

      ;; Ensure buffer is editable
      (setq buffer-read-only nil))))

;;; Work Buffer Management - Streamlined Mode

(defun efrit-streamlined--get-work-buffer ()
  "Get or create the work buffer for detailed operations."
  (let ((buffer (get-buffer-create efrit-work-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'efrit-work-mode)
        (efrit-work-mode)))
    buffer))

(defun efrit-streamlined--log-to-work (message)
  "Log MESSAGE to the work buffer with timestamp."
  (let ((work-buffer (efrit-streamlined--get-work-buffer)))
    (with-current-buffer work-buffer
      (goto-char (point-max))
      (insert (format "[%s] %s\n"
                      (format-time-string "%H:%M:%S")
                      message))

      ;; Truncate buffer if it gets too large
      (when (and (> efrit-work-buffer-max-size 0)
                 (> (buffer-size) efrit-work-buffer-max-size))
        (goto-char (point-min))
        (forward-line (/ (count-lines (point-min) (point-max)) 2))
        (delete-region (point-min) (point))
        (goto-char (point-min))
        (insert (format "[%s] === Work buffer truncated (size limit: %d) ===\n"
                        (format-time-string "%H:%M:%S")
                        efrit-work-buffer-max-size))
        (goto-char (point-max)))

      (when efrit-show-work-buffer
        (display-buffer work-buffer)))))

(define-derived-mode efrit-work-mode fundamental-mode "Efrit-Work"
  "Major mode for the Efrit work buffer."
  (setq buffer-read-only nil)
  (font-lock-mode 1))

;;; Header customization

(defun efrit--build-headers (api-key)
  "Build HTTP headers for API requests, respecting customization options."
  (let ((default-headers `(("x-api-key" . ,api-key)
                          ("anthropic-version" . "2023-06-01")
                          ("anthropic-beta" . "max-tokens-3-5-sonnet-2024-07-15")
                          ("content-type" . "application/json"))))
    ;; Remove excluded headers
    (when efrit-excluded-headers
      (setq default-headers
            (cl-remove-if (lambda (header)
                           (member (car header) efrit-excluded-headers))
                         default-headers)))
    ;; Add custom headers (custom headers override defaults)
    (append efrit-custom-headers default-headers)))

;;; System Prompts

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

;;; API functions - Classic Chat Mode

(defun efrit--send-api-request (messages)
  "Send MESSAGES to the Claude API and handle the response."
  (let* ((api-key (efrit--get-api-key))
         (url-request-method "POST")
         (url-request-extra-headers (efrit--build-headers api-key))
         (system-prompt (when efrit-enable-tools (efrit-tools-system-prompt)))
         (request-data
         `(("model" . ,efrit-model)
         ("max_tokens" . ,efrit-max-tokens)
         ("temperature" . ,efrit-temperature)
         ,@(when system-prompt
         `(("system" . ,system-prompt)))
         ("messages" . ,(vconcat
         ;; Add the conversation history
         (mapcar (lambda (msg)
               `(("role" . ,(alist-get 'role msg))
                     ("content" . ,(alist-get 'content msg))))
                 messages)))
             ,@(when efrit-enable-tools
                 '(("tools" . [
                                  ;; Primary tool: Elisp evaluation
                                  (("name" . "eval_sexp")
                                  ("description" . "Evaluate a Lisp expression and return the result. This is the primary tool for interacting with Emacs.")
                                  ("input_schema" . (("type" . "object")
                                                      ("properties" . (("expr" . (("type" . "string")
                                                                                  ("description" . "The Elisp expression to evaluate")))))
                                                      ("required" . ["expr"]))))

                                  ;; Context gathering
                                  (("name" . "get_context")
                                  ("description" . "Get comprehensive context information about the Emacs environment")
                                  ("input_schema" . (("type" . "object")
                                                      ("properties" . (("request" . (("type" . "string")
                                                                                     ("description" . "Optional context request")))))
                                                      ("required" . []))))

                                  ;; Path resolution (useful helper)
                                  (("name" . "resolve_path")
                                  ("description" . "Resolve a path from natural language description")
                                  ("input_schema" . (("type" . "object")
                                                      ("properties" . (("path_description" . (("type" . "string")
                                                                                             ("description" . "Natural language path description")))))
                                                      ("required" . ["path_description"]))))

                                  ])))
                                  ))
         (json-string (json-encode request-data))
         ;; Convert unicode characters to JSON escape sequences to prevent multibyte HTTP errors
         (escaped-json (replace-regexp-in-string
                        "[^\x00-\x7F]"
                        (lambda (char)
                          (format "\\\\u%04X" (string-to-char char)))
                        json-string))
         (url-request-data (encode-coding-string escaped-json 'utf-8)))
    ;; Send request
    (url-retrieve (or efrit-api-url (efrit-common-get-api-url)) 'efrit--handle-api-response nil t t)))

(defun efrit--process-http-status (status)
  "Process HTTP status and return non-nil if there's an error.
If there's an error, handle it and clean up the buffer."
  (when (plist-get status :error)
    (efrit--handle-http-error (plist-get status :error))
    ;; Clean up response buffer
    (when (buffer-live-p (current-buffer))
      (kill-buffer (current-buffer)))
    t)) ; Return t to indicate error was handled

(defun efrit--handle-http-error (error-details)
  "Handle HTTP error with ERROR-DETAILS."
  (require 'efrit-log)
  (efrit-log-error "[efrit-chat] API Error: %s" error-details)
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

      ;; Clear in-progress flag
      (setq-local efrit--response-in-progress nil)

      ;; Display error with retry hint
      (efrit--display-message
       (format "API Error: %s\n(Use M-x efrit-retry-last-message to retry, or M-x efrit-show-errors for all errors)" error-details) 'system)

      ;; Insert prompt for next message
      (efrit--insert-prompt))))

(defun efrit--parse-api-response ()
  "Parse JSON response from current buffer and return content.
Returns the content hash-table from the API response, or nil if parsing fails."
  (goto-char (point-min))
  (when (search-forward-regexp "^$" nil t)
    (let* ((json-object-type 'hash-table)
           (json-array-type 'vector)
           (json-key-type 'string)
           ;; Ensure proper UTF-8 decoding
           (coding-system-for-read 'utf-8)
           (raw-response (decode-coding-region (point) (point-max) 'utf-8 t))
           (response (json-read-from-string raw-response))
           (content (gethash "content" response)))
      content)))

(defun efrit--detect-incomplete-task (_content _message-text)
  "Detect if CONTENT represents an incomplete multi-step task.
Returns non-nil if the task appears incomplete and needs delegation.

DEPRECATED: This function violated the Pure Executor principle by using
pattern matching to make decisions. Multi-turn completion should be
handled by asking Claude explicitly via efrit-multi-turn.el, not by
parsing response text. Currently disabled - always returns nil."
  ;; Always return nil - let Claude make decisions explicitly
  nil)

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


          ;; Handle text content
          (when (string= type "text")
            (let ((text (gethash "text" item)))
              (when text

                (setq message-text (concat message-text text)))))

          ;; Handle tool calls
          (when (string= type "tool_use")
            (let* ((tool-name (gethash "name" item))
                   (tool-id (gethash "id" item))  ; Captured for use in tool_result messages (ef-5af)
                   (input (gethash "input" item)))

              ;; Execute tool call
              (let ((result (condition-case tool-err
                                (cond
                                 ;; Handle eval_sexp tool call
                                 ((string= tool-name "eval_sexp")
                                 (let ((expr (gethash "expr" input)))
                                 (efrit-log-debug "Executing elisp: %s" expr)
                                 (if expr
                                   (let ((result (efrit-tools-eval-sexp expr)))
                                           (efrit-log-debug "Elisp result: %s" result)
                                           result)
                                       "Error: No expression provided")))
                                 ;; Handle get_context tool call
                                 ((string= tool-name "get_context")
                                 (efrit-tools-get-context))
                                 ;; Unknown tool
                                 (t
                                  (format "Error: Unknown tool '%s'" tool-name)))
                              (error
                               (format "Error executing tool %s: %s"
                                      tool-name (if tool-err
                                                   (efrit--safe-error-message tool-err)
                                                 "Unknown error"))))))

                ;; Collect ALL tool results for sending back to Claude
                (push (efrit--build-tool-result tool-id result) tool-results)

                ;; In chat mode, don't display tool results inline
                ;; Only show errors (buffer objects and nil results are suppressed)
                (when (string-match-p "^Error:" result)
                  (setq message-text (concat message-text "\n" result))))))))

      ;; Check if we should delegate to efrit-do
      (when (efrit--detect-incomplete-task content message-text)
        (efrit-log-debug "Detected incomplete task - will delegate")
        (setq should-delegate t)))

    ;; Return message text, tool results, and delegation flag
    (list message-text (nreverse tool-results) should-delegate)))

(defun efrit--update-ui-with-response (message-text)
  "Update the UI with MESSAGE-TEXT response."
  (with-current-buffer (efrit--setup-buffer)
    (setq buffer-read-only nil)
    (let ((inhibit-read-only t))
      ;; Remove "thinking" indicator (but only if it's actually at the end)
      (when efrit--response-in-progress
        (save-excursion
          (goto-char (point-max))
          (when (search-backward "System: Thinking..." nil t)
            (let ((start (match-beginning 0)))
              ;; Only delete if this is at the very end of the buffer to avoid deleting user content
              (when (and (search-forward "Thinking..." nil t)
                        (= (point) (point-max)))
                (delete-region start (point-max)))))))

      ;; Clear progress flag
      (setq-local efrit--response-in-progress nil)

      ;; Process tool calls and display response text
      (when (and message-text (not (string-empty-p message-text)))
        (if efrit-enable-tools
            ;; Tools already executed in efrit--extract-content-and-tools, just display the result
            (condition-case-unless-debug process-err
                (let ((highlighted-text (efrit--sanitize-chat-text message-text)))
                  ;; Display the message with tool results
                  (efrit--display-message highlighted-text 'assistant)

                  ;; Add to conversation history
                  (push `((role . "assistant")
                         (content . ,message-text))
                        efrit--message-history)
                  ;; Also add to unified context
                  (require 'efrit-session)
                  (efrit-unified-context-add-message 'assistant message-text 'chat))
              (error
               ;; Fallback to displaying the raw message if processing fails
               (message "Error processing response: %s"
                       (efrit--safe-error-message process-err))
               (efrit--display-message
                (concat message-text
                       "\n\n[Error processing response: "
                       (efrit--safe-error-message process-err) "]")
                'assistant)

               ;; Add original message to history
               (push `((role . "assistant")
                      (content . ,message-text))
                     efrit--message-history)
               ;; Also add to unified context
               (require 'efrit-session)
               (efrit-unified-context-add-message 'assistant message-text 'chat)))

          ;; Just display the text directly if tools are disabled
          (progn
            (efrit--display-message message-text 'assistant)

            ;; Add to conversation history
            (push `((role . "assistant")
                   (content . ,message-text))
                  efrit--message-history)
            ;; Also add to unified context
            (require 'efrit-session)
            (efrit-unified-context-add-message 'assistant message-text 'chat))))

      ;; Don't auto-continue in chat mode - insert prompt for next user input
      (efrit--insert-prompt))))

(defun efrit--handle-parse-error ()
  "Handle parsing errors by displaying error message and resetting UI."
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

      ;; Clear progress flag
      (setq-local efrit--response-in-progress nil)

      ;; Display error message
      (efrit--display-message
       "Error parsing API response" 'system)

      ;; Insert prompt for next message
      (efrit--insert-prompt))))

(defun efrit--handle-api-response (status)
  "Handle the API response with STATUS."


  (unless (efrit--process-http-status status)
    ;; Only proceed if there was no HTTP error
    (condition-case _api-err
        (let ((content (efrit--parse-api-response)))
          (if content
              ;; Execute tool processing in the chat buffer context
              (with-current-buffer (efrit--setup-buffer)
                (let* ((result (efrit--extract-content-and-tools content))
                       (message-text (nth 0 result))
                       (tool-results (nth 1 result))
                       (should-delegate (nth 2 result)))
                  (cond
                   ;; If we have tool results, send them back to Claude
                   ((and tool-results (> (length tool-results) 0))
                    (efrit-log-debug "Tool results collected: %d, sending back to Claude" (length tool-results))
                    ;; Add assistant's message with tool_use to history
                    (push `((role . "assistant")
                            (content . ,content))
                          efrit--message-history)
                    ;; Build user message with tool_result blocks
                    (push `((role . "user")
                            (content . ,(vconcat tool-results)))
                          efrit--message-history)
                    ;; Continue conversation - response will come back to this handler
                    (efrit--send-api-request (reverse efrit--message-history)))

                   ;; Delegate to efrit-do for multi-step completion
                   (should-delegate
                    (efrit-log-debug "Delegating to efrit-do")
                    (efrit--delegate-to-do))

                   ;; Normal response handling (no tools, just text)
                   (t
                    (efrit--update-ui-with-response message-text)))))
            ;; Handle case where content is nil
            (efrit--handle-parse-error)))
      ;; Handle any errors during parsing
      (error (efrit--handle-parse-error)))
    ;; Note: Let url-retrieve handle its own buffer cleanup
    ;; Manual cleanup here causes format-message(nil) errors
    ))

(defun efrit--delegate-to-do ()
  "Delegate current request to efrit-do for multi-step completion."
  (let ((original-request (when efrit--message-history
                            (let ((first-msg (car (last efrit--message-history))))
                              (when (eq (alist-get 'role first-msg) 'user)
                                (alist-get 'content first-msg))))))
    (when original-request
      ;; Update chat UI to indicate delegation
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

          ;; Clear in-progress flag
          (setq-local efrit--response-in-progress nil)

          ;; Show delegation message
          (efrit--display-message
           "[Delegating to efrit-do for multi-step task completion...]"
           'system)

          ;; Insert prompt and let user know
          (efrit--insert-prompt)

          ;; Execute via efrit-do asynchronously
          (run-at-time 0.1 nil
                       (lambda ()
                         (require 'efrit-do)
                         (message "Executing via efrit-do: %s" original-request)
                         (efrit-do original-request))))))))

;;; API functions - Streamlined Mode

(defun efrit-streamlined--send-request (messages)
  "Send MESSAGES with enhanced streamlined prompt."
  (efrit-streamlined--log-to-work
   (format "Sending request with %d messages" (length messages)))

  ;; Use enhanced system prompt
  (let* ((api-key (efrit--get-api-key))
         (url-request-method "POST")
         (url-request-extra-headers (efrit--build-headers api-key))
         (system-prompt (efrit-streamlined--system-prompt))
         (cleaned-messages (mapcar (lambda (msg)
                                     `(("role" . ,(alist-get 'role msg))
                                       ("content" . ,(substring-no-properties (alist-get 'content msg)))))
                                   messages))
         (request-data
          `(("model" . ,efrit-model)
            ("max_tokens" . ,efrit-max-tokens)
            ("temperature" . ,efrit-temperature)
            ("system" . ,system-prompt)
            ("messages" . ,(vconcat cleaned-messages))
            ,@(when efrit-enable-tools
                '(("tools" . [
                              (("name" . "eval_sexp")
                               ("description" . "Evaluate Elisp expression")
                               ("input_schema" . (("type" . "object")
                                                  ("properties" . (("expr" . (("type" . "string")
                                                                              ("description" . "Elisp expression")))))
                                                  ("required" . ["expr"]))))
                              (("name" . "get_context")
                               ("description" . "Get Emacs environment context")
                               ("input_schema" . (("type" . "object")
                                                  ("properties" . (("request" . (("type" . "string")
                                                                                 ("description" . "Context request")))))
                                                  ("required" . []))))
                              ])))
            ))
         (json-string (json-encode request-data))
         ;; Convert unicode characters to JSON escape sequences to prevent multibyte HTTP errors
         (escaped-json (replace-regexp-in-string
                        "[^\x00-\x7F]"
                        (lambda (char)
                          (format "\\\\u%04X" (string-to-char char)))
                        json-string))
         (url-request-data (encode-coding-string escaped-json 'utf-8)))

    ;; Log request details to work buffer
    (efrit-streamlined--log-to-work
     (format "Request: %s characters, %s tools enabled"
             (length url-request-data)
             (if efrit-enable-tools "tools" "no tools")))

    ;; Send request
    (url-retrieve (or efrit-api-url (efrit-common-get-api-url)) 'efrit-streamlined--handle-response nil t t)))

(defun efrit-streamlined--handle-response (_status)
  "Handle API response with work buffer logging."
  (efrit-streamlined--log-to-work "Received response")

  ;; Parse response and handle it
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

          ;; Log to work buffer
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
            (claude-signals-done (or (string-match-p "\\bdone\\b\\|\\bcomplete\\b\\|\\bfinished\\b\\|\\bfinal\\b"
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
                  ;; Continue conversation with tool results
                  (progn
                    (setq efrit-streamlined--turn-count (1+ efrit-streamlined--turn-count))
                    (efrit-streamlined--log-to-work "Continuing conversation with tool results")
                    (efrit-streamlined--continue-with-results tool-results text-content))

                ;; Final response - display it
                (progn
                  (message "Efrit: Complete")
                  (when has-text-content
                    (efrit-streamlined--display-response text-content)))))))))))

(defun efrit-streamlined--continue-with-results (tool-results text-content)
  "Continue conversation with TOOL-RESULTS from executed tools.
TOOL-RESULTS should be a list of tool_result blocks built with efrit--build-tool-result."
  (let ((updated-messages efrit-streamlined--current-messages))

    ;; Add assistant's text with tool_use if any (Claude's original response)
    ;; Note: In streamlined mode, the full content with tool_use was already added
    ;; So we just need to add the tool results as a user message

    ;; Build user message with tool_result blocks
    (when (and tool-results (> (length tool-results) 0))
      (push `((role . "user")
              (content . ,(vconcat tool-results)))
            updated-messages))

    ;; Continue conversation with results
    (when (> (length tool-results) 0)
      (setq efrit-streamlined--current-messages updated-messages)
      (efrit-streamlined--send-request updated-messages))))

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

(defun efrit-streamlined--execute-tools (tool-uses)
  "Execute TOOL-USES and return list of tool_result blocks for continuation."
  (efrit-streamlined--log-to-work
   (format "Executing %d tools" (length tool-uses)))

  (let ((results '()))
    (dolist (tool-use tool-uses)
      (let* ((tool-name (alist-get 'name tool-use))
             (tool-input (alist-get 'input tool-use))
             (tool-id (alist-get 'id tool-use)))

        (efrit-streamlined--log-to-work
         (format "Tool: %s (id: %s)" tool-name tool-id))

        (let ((result-content
               (cond
                ((string-equal tool-name "eval_sexp")
                 (let ((expr (alist-get 'expr tool-input)))
                   (if expr
                       (progn
                         (efrit-streamlined--log-to-work (format "Evaluating: %s" expr))
                         (condition-case err
                             (let ((result (eval (read expr))))
                               (let ((result-str (if (stringp result)
                                                     (substring-no-properties result)
                                                   (prin1-to-string result))))
                                 (efrit-streamlined--log-to-work
                                  (format "Result: %s" (substring result-str 0 (min 200 (length result-str)))))
                                 result-str))
                           (error
                            (let ((error-msg (error-message-string err)))
                              (efrit-streamlined--log-to-work
                               (format "Error: %s" error-msg))
                              (format "Error: %s" error-msg)))))
                     "Error: No expression provided")))

                ((string-equal tool-name "get_context")
                 (efrit-streamlined--log-to-work "Getting context...")
                 (let ((context (if (fboundp 'efrit-tools-get-context)
                                    (efrit-tools-get-context)
                                  "Context not available - efrit-tools not loaded")))
                   (efrit-streamlined--log-to-work
                    (format "Context: %d chars" (length context)))
                   context))

                (t
                 (efrit-streamlined--log-to-work
                  (format "Unknown tool: %s" tool-name))
                 (format "Error: Unknown tool %s" tool-name)))))

          ;; Build proper tool_result block using helper
          (push (efrit--build-tool-result tool-id result-content) results))))

    (reverse results)))

(defun efrit-streamlined--display-response (content)
  "Display CONTENT in chat buffer."
  (let* ((buffer-name efrit-buffer-name)
         (chat-buffer (get-buffer-create buffer-name)))
    (with-current-buffer chat-buffer
      (unless (derived-mode-p 'efrit-chat-mode)
        (efrit-streamlined--setup-chat-mode))

      (goto-char (point-max))
      (insert (format "\nA: %s\n\n" content))
      (goto-char (point-max)))))

(define-derived-mode efrit-chat-mode fundamental-mode "Efrit-Chat"
  "Major mode for Efrit chat."
  (setq buffer-read-only nil))

(defun efrit-streamlined--setup-chat-mode ()
  "Setup basic chat mode for the buffer."
  (efrit-chat-mode)
  (goto-char (point-max))
  (when (= (point-min) (point-max))
    (insert "Efrit Chat (Streamlined)\n")
    (insert "========================\n\n")))

;;; User Interface Commands - Classic Chat Mode

;;;###autoload
(defun efrit-send-message (message)
  "Send MESSAGE to the Claude API."
  (interactive "sMessage: ")
  (efrit-log-section "SEND MESSAGE")
  (efrit-log-debug "User message: %s" message)
  (with-current-buffer (efrit--setup-buffer)
    (setq buffer-read-only nil)
    (let ((inhibit-read-only t))
      ;; Check if a response is already in progress
      (when efrit--response-in-progress
        (user-error "Please wait for the current response to complete"))

      ;; Display the user message only if not already displayed
      (unless (save-excursion
                (goto-char (point-max))
                (forward-line -1)
                (and (looking-at "You: ")
                     (string-match-p (regexp-quote message) (thing-at-point 'line))))
        (efrit--display-message message 'user))

      ;; Don't use multi-turn in chat mode - users control the conversation
      (setq-local efrit--current-conversation nil)

      ;; Store message for retry on error
      (setq-local efrit--last-user-message message)

      ;; Add message to history (at beginning, as we're using push)
      (push `((role . "user")
             (content . ,message))
            efrit--message-history)

      ;; Also add to unified context
      (require 'efrit-session)
      (efrit-unified-context-add-message 'user message 'chat)

      ;; Show thinking indicator
      (efrit--display-message "Thinking..." 'system)

      ;; Set in-progress flag - will be cleared by response handler
      (setq-local efrit--response-in-progress t)

      ;; Send the API request (async - flag cleared by response handler)
      (condition-case err
          (efrit--send-api-request (reverse efrit--message-history))
        (error
         ;; If url-retrieve fails immediately, clear flag and show error
         (setq-local efrit--response-in-progress nil)
         (efrit--display-message
          (format "Failed to send request: %s" (error-message-string err))
          'system)
         (efrit--insert-prompt))))))

;;;###autoload
(defun efrit-send-buffer-message ()
  "Send the current input message from the buffer."
  (interactive)
  (with-current-buffer (efrit--setup-buffer)
    (setq buffer-read-only nil)
    (let ((inhibit-read-only t))
      (if (and efrit--input-marker
               (marker-position efrit--input-marker))
          (let* ((raw-input (buffer-substring-no-properties
                            efrit--input-marker (point-max)))
                 (message (string-trim
                          (replace-regexp-in-string "^>\\s-*" "" raw-input))))
            (if (string-empty-p message)
                (message "No input to send")
              ;; Replace the input area with properly formatted user message
              (delete-region efrit--input-marker (point-max))
              (goto-char efrit--input-marker)
              (insert (format "You: %s\n" message))
              ;; Update conversation marker to track conversation end
              (set-marker efrit--conversation-marker (point))
              ;; Send message
              (efrit-send-message message)))
        (progn
          (message "Debug: input marker not set up properly. Marker: %S, Position: %S"
                   efrit--input-marker
                   (when efrit--input-marker (marker-position efrit--input-marker)))
          (efrit--insert-prompt))))))

;;;###autoload
(defun efrit-insert-newline ()
  "Insert a newline in the message without sending."
  (interactive)
  (with-current-buffer (efrit--setup-buffer)
    (setq buffer-read-only nil)
    (insert "\n")))

;;;###autoload
(defun efrit-retry-last-message ()
  "Retry the last user message that was sent.
Useful when the previous API call failed."
  (interactive)
  (with-current-buffer (efrit--setup-buffer)
    (if efrit--last-user-message
        (progn
          (message "Retrying: %s" efrit--last-user-message)
          (efrit-send-message efrit--last-user-message))
      (message "No previous message to retry"))))

;;;###autoload
(defun efrit-chat-debug ()
  "Start efrit chat with full debugging (shows all technical details)."
  (interactive)
  (switch-to-buffer (efrit--setup-buffer))
  (setq buffer-read-only nil)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq-local efrit--message-history nil)
    (setq-local efrit--response-in-progress nil)
    (setq-local efrit--conversation-marker nil)
    (setq-local efrit--input-marker nil)
    (setq-local efrit--current-conversation nil))
  (setq-local efrit--conversation-marker (make-marker))
  (set-marker efrit--conversation-marker (point-min))
  (efrit--display-message
   (format "Efrit initialized. Enter your message below and press Enter to send.\nUse Shift+Enter for newlines. Using model: %s"
           efrit-model)
   'assistant)
  (efrit--insert-prompt))

;;; User Interface Commands - Streamlined Mode

;;;###autoload
(defun efrit-streamlined-send (message)
  "Send MESSAGE using streamlined chat experience."
  (interactive "sMessage: ")
  (let ((messages (list `((role . "user") (content . ,message)))))
    (setq efrit-streamlined--current-messages messages)
    (setq efrit-streamlined--turn-count 0)
    (efrit-streamlined--log-to-work (format "User message: %s" message))
    (message "Efrit: Processing request...")
    (efrit-streamlined--send-request messages)))

;;; Mode definition

;; efrit-mode-map is defined in efrit.el to be always available
(defvar efrit-mode-map)

(define-derived-mode efrit-mode text-mode "Efrit"
  "Major mode for interacting with the Efrit conversational assistant."
  ;; Initialize buffer-local variables
  (setq-local efrit--message-history nil)
  (setq-local efrit--conversation-marker nil)
  (setq-local efrit--input-marker nil)
  (setq-local efrit--response-in-progress nil)
  ;; Enable line wrapping
  (visual-line-mode 1))

;;; Main Chat Interface

;;;###autoload
(defun efrit-chat ()
  "Start efrit chat session - interactive buffer like ChatGPT."
  (interactive)
  (switch-to-buffer (efrit--setup-buffer))
  (setq buffer-read-only nil)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq-local efrit--message-history nil)
    (setq-local efrit--response-in-progress nil)
    (setq-local efrit--conversation-marker nil)
    (setq-local efrit--input-marker nil)
    (setq-local efrit--current-conversation nil))

  (setq-local efrit--conversation-marker (make-marker))
  (set-marker efrit--conversation-marker (point-min))

  (efrit--display-message
   (format "Efrit Chat Ready - Using model: %s" efrit-model)
   'assistant)

  (efrit--insert-prompt))

;;;###autoload
(defun efrit-chat-clear ()
  "Clear the conversation history and start fresh in the current chat buffer."
  (interactive)
  (if (not (eq major-mode 'efrit-mode))
      (user-error "Not in an Efrit chat buffer")
    (when (or (not efrit--message-history)
              (yes-or-no-p "Clear conversation history and start fresh? "))
      (setq buffer-read-only nil)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq-local efrit--message-history nil)
        (setq-local efrit--response-in-progress nil)
        (setq-local efrit--conversation-marker nil)
        (setq-local efrit--input-marker nil)
        (setq-local efrit--current-conversation nil))

      (setq-local efrit--conversation-marker (make-marker))
      (set-marker efrit--conversation-marker (point-min))

      (efrit--display-message
       (format "Conversation cleared - Using model: %s" efrit-model)
       'assistant)

      (efrit--insert-prompt)
      (message "Efrit chat conversation cleared"))))

(provide 'efrit-chat)

;;; efrit-chat.el ends here
