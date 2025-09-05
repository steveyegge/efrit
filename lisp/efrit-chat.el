;;; efrit-chat.el --- Conversational assistant for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.2.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai
;; URL: https://github.com/stevey/efrit

;;; Commentary:
;; This file provides a simple, clean implementation of a conversational
;; assistant using Anthropic's Claude API.  The implementation maintains conversation
;; state across multiple turns and properly handles buffer editing.

;;; Code:

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

(require 'json)
(require 'url)
(require 'cl-lib)
(require 'auth-source)
(require 'efrit-tools)
(require 'efrit-multi-turn)
(require 'efrit-debug)

;; Declare functions from other modules to avoid warnings
(declare-function efrit-common-get-api-key "efrit-common")
(declare-function efrit-do "efrit-do" (command))

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

(defcustom efrit-api-url "https://api.anthropic.com/v1/messages"
  "URL for the Anthropic API endpoint."
  :type 'string
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

;;; Internal variables

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

;;; Helper Functions

(defun efrit--sanitize-chat-text (text)
  "Remove technical artifacts from TEXT for clean chat display."
  (let ((cleaned text))
    ;; Remove any remaining [Result: ...] tags
    (setq cleaned (replace-regexp-in-string "\\[Result:[^]]*\\]" "" cleaned))
    ;; Remove empty lines and excessive whitespace
    (setq cleaned (replace-regexp-in-string "\n\n+" "\n\n" cleaned))
    (string-trim cleaned)))

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



;;; Buffer management

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
      
      ;; Add spacing before input area
      (unless (bobp) (insert "\n\n"))
      
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
                  (t ""))))
      ;; All messages go to the conversation area (before input area)
      (if (and efrit--conversation-marker (marker-position efrit--conversation-marker))
          (goto-char (marker-position efrit--conversation-marker))
        ;; Fallback: go to input marker or end of buffer
        (if (and efrit--input-marker (marker-position efrit--input-marker))
            (goto-char (marker-position efrit--input-marker))
          (goto-char (point-max))))
      
      ;; Add spacing before message
      (unless (bobp) (insert "\n\n"))
      
      ;; Insert the message with appropriate prefix
      (let ((start (point)))
        (insert prefix message)
        (when face
          (add-text-properties start (point) `(face ,face))))
      
      ;; Update conversation marker to end of this message
      (set-marker efrit--conversation-marker (point))
      
      ;; Ensure buffer is editable
      (setq buffer-read-only nil))))

;;; API functions

(defun efrit--send-api-request (messages)
  "Send MESSAGES to the Claude API and handle the response."
  (let* ((api-key (efrit--get-api-key))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("x-api-key" . ,api-key)
            ("anthropic-version" . "2023-06-01")
            ("anthropic-beta" . "max-tokens-3-5-sonnet-2024-07-15")
            ("content-type" . "application/json")))
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
         (url-request-data
          (encode-coding-string (json-encode request-data) 'utf-8)))
    ;; Send request
    (url-retrieve efrit-api-url 'efrit--handle-api-response nil t t)))

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

      ;; Display error
      (efrit--display-message
       (format "API Error: %s" error-details) 'system)

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

(defun efrit--detect-incomplete-task (content message-text)
  "Detect if CONTENT represents an incomplete multi-step task.
Returns non-nil if the task appears incomplete and needs delegation."
  (when (and content message-text)
    (let ((tool-count 0)
          (mentions-multiple-items nil))
      
      ;; Count tool calls
      (dotimes (i (length content))
        (let* ((item (aref content i))
               (type (gethash "type" item)))
          (when (string= type "tool_use")
            (cl-incf tool-count))))
      
      ;; Check if text mentions creating multiple items
      (when (or (string-match-p "\\(three\\|multiple\\|several\\) \\(buffers?\\|poems?\\|files?\\)" message-text)
                (string-match-p "create.*and.*and" message-text)
                (string-match-p "I'll create.*separate" message-text))
        (setq mentions-multiple-items t))
      
      ;; Delegate if:
      ;; 1. Text mentions multiple items but only 1 or 0 tools were called
      ;; 2. Text says "Let me" or "I'll" with words suggesting multiple steps
      (or (and mentions-multiple-items (< tool-count 2))
          (and (string-match-p "\\(Let me\\|I'll\\|I will\\).*\\(create\\|write\\|start\\).*\\(multiple\\|several\\|three\\|buffers\\)" message-text)
               (< tool-count 2))))))

(defun efrit--extract-content-and-tools (content)
  "Extract text content and execute tool calls from CONTENT array.
Returns the processed message text with tool results."
  (let ((message-text "")
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
                   (input (gethash "input" item)))

              ;; Execute tool call and add result to message text
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

                ;; In chat mode, don't display tool results inline
                ;; Only show them if they're buffer objects or errors
                (when (or (string-match-p "^#<buffer" result)
                          (string-match-p "^Error:" result))
                  (setq message-text (concat message-text "\n" result))))))))
      
      ;; Check if we should delegate to efrit-do
      (when (efrit--detect-incomplete-task content message-text)
        (efrit-log-debug "Detected incomplete task - will delegate")
        (setq should-delegate t)))

    ;; Return both the message text and delegation flag
    (cons message-text should-delegate)))

(defun efrit--update-ui-with-response (message-text)
  "Update the UI with MESSAGE-TEXT response."
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
                        efrit--message-history))
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
                     efrit--message-history)))

          ;; Just display the text directly if tools are disabled
          (progn
            (efrit--display-message message-text 'assistant)

            ;; Add to conversation history
            (push `((role . "assistant")
                   (content . ,message-text))
                  efrit--message-history))))

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
                       (message-text (car result))
                       (should-delegate (cdr result)))
                  (if should-delegate
                      ;; Delegate to efrit-do for multi-step completion
                      (progn
                        (efrit-log-debug "Delegating to efrit-do")
                        (efrit--delegate-to-do))
                    ;; Normal response handling
                    (efrit--update-ui-with-response message-text))))
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

;;; User Interface Commands

;;;###autoload
(defun efrit-send-message (message)
  "Send MESSAGE to the Claude API."
  (interactive "sMessage: ")
  (efrit-log-section "SEND MESSAGE")
  (efrit-log-debug "User message: %s" message)
  (with-current-buffer (efrit--setup-buffer)
    (setq buffer-read-only nil)
    (let ((inhibit-read-only t))
      ;; Display the user message
      (efrit--display-message message 'user)

      ;; Don't use multi-turn in chat mode - users control the conversation
      (setq-local efrit--current-conversation nil)

      ;; Set in-progress flag
      (setq-local efrit--response-in-progress t)

      ;; Add message to history (at beginning, as we're using push)
      (push `((role . "user")
             (content . ,message))
            efrit--message-history)

      ;; Show thinking indicator (use system role to avoid creating prompt)
      (efrit--display-message "Thinking..." 'system)

      ;; Send the API request with reversed message history (newest first->oldest last)
      (efrit--send-api-request (reverse efrit--message-history)))))

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
              ;; Delete the input area
              (delete-region efrit--input-marker (point-max))
              ;; Send the message
              (efrit-send-message message)))
        ;; Debug: marker not set up properly
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
(defun efrit-chat-debug ()
  "Start efrit chat with full debugging (shows all technical details)."
  (interactive)
  ;; Switch to the buffer
  (switch-to-buffer (efrit--setup-buffer))

  ;; Make sure buffer is editable
  (setq buffer-read-only nil)

  ;; Clear buffer and reset state
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq-local efrit--message-history nil)
    (setq-local efrit--response-in-progress nil)
    (setq-local efrit--conversation-marker nil)
    (setq-local efrit--input-marker nil)
    (setq-local efrit--current-conversation nil))

  ;; Initialize conversation marker at the beginning
  (setq-local efrit--conversation-marker (make-marker))
  (set-marker efrit--conversation-marker (point-min))

  ;; Show welcome message
  (efrit--display-message
   (format "Efrit initialized. Enter your message below and press Enter to send.\nUse Shift+Enter for newlines. Using model: %s"
           efrit-model)
   'assistant)

  ;; Insert prompt
  (efrit--insert-prompt))

;;; Mode definition

(defvar efrit-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Key bindings
    (define-key map (kbd "RET") 'efrit-send-buffer-message)
    (define-key map (kbd "S-<return>") 'efrit-insert-newline)
    (define-key map (kbd "C-c C-c") 'efrit-send-buffer-message)
    map)
  "Keymap for Efrit mode.")

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
  ;; Use the original chat interface but with clean display
  (switch-to-buffer (efrit--setup-buffer))
  (setq buffer-read-only nil)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq-local efrit--message-history nil)
    (setq-local efrit--response-in-progress nil)
    (setq-local efrit--conversation-marker nil)
    (setq-local efrit--input-marker nil)
    (setq-local efrit--current-conversation nil))
  
  ;; Initialize conversation marker
  (setq-local efrit--conversation-marker (make-marker))
  (set-marker efrit--conversation-marker (point-min))
  
  ;; Show welcome message
  (efrit--display-message
   (format "Efrit Chat Ready - Using model: %s" efrit-model)
   'assistant)
  
  (efrit--insert-prompt))



(provide 'efrit-chat)

;;; efrit-chat.el ends here
