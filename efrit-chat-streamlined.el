;;; efrit-chat-streamlined.el --- Streamlined chat experience -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.3.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai
;; URL: https://github.com/stevey/efrit

;;; Commentary:
;; This provides a streamlined chat experience with:
;; - Separate work buffer for detailed thinking/execution
;; - Clean chat buffer with minimal output for actions
;; - Appropriate response lengths based on request type
;; - Single-turn completion for simple requests

;;; Code:

(require 'efrit-tools)
(require 'json)
(require 'url)
(require 'cl-lib)

;; Declare external variables from efrit-chat
(defvar efrit-model)
(defvar efrit-max-tokens)
(defvar efrit-temperature)
(defvar efrit-enable-tools)
(defvar efrit-api-url)

;;; Customization

(defgroup efrit nil
  "Efrit conversational assistant for Emacs."
  :group 'tools
  :prefix "efrit-")

(defcustom efrit-model "claude-3-5-sonnet-20241022"
  "Claude model to use for conversations."
  :type 'string
  :group 'efrit)

(defcustom efrit-max-tokens 8192
  "Maximum number of tokens in the response."
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

(defcustom efrit-work-buffer-name "*efrit-work*"
  "Name of the buffer for detailed work/thinking."
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

;;; System Prompt Enhancement

(defun efrit-streamlined--system-prompt ()
  "Generate enhanced system prompt for streamlined chat experience."
  (concat 
   (efrit-tools-system-prompt)
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

;;; Work Buffer Management

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
      (let ((max-size (or (and (boundp 'efrit-work-buffer-max-size) 
                               efrit-work-buffer-max-size) 
                          100000))) ; Default fallback
        (when (and (> max-size 0)
                   (> (buffer-size) max-size))
          (goto-char (point-min))
          (forward-line (/ (count-lines (point-min) (point-max)) 2))
          (delete-region (point-min) (point))
          (goto-char (point-min))
          (insert (format "[%s] === Work buffer truncated (size limit: %d) ===\n"
                          (format-time-string "%H:%M:%S")
                          max-size))
          (goto-char (point-max))))
      
      (when (and (boundp 'efrit-show-work-buffer) efrit-show-work-buffer)
        (display-buffer work-buffer)))))

;;; Work Buffer Mode

(define-derived-mode efrit-work-mode fundamental-mode "Efrit-Work"
  "Major mode for the Efrit work buffer."
  (setq buffer-read-only nil)
  (font-lock-mode 1))

;;; Enhanced API Request Function

(defun efrit-streamlined--send-request (messages)
  "Send MESSAGES with enhanced streamlined prompt."
  (efrit-streamlined--log-to-work 
   (format "Sending request with %d messages" (length messages)))
  
  ;; Use enhanced system prompt
  (let* ((api-key (efrit--get-api-key))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("x-api-key" . ,api-key)
            ("anthropic-version" . "2023-06-01")
            ("anthropic-beta" . "max-tokens-3-5-sonnet-2024-07-15")
            ("content-type" . "application/json")))
         (system-prompt (efrit-streamlined--system-prompt))
         (request-data
          `(("model" . ,efrit-model)
            ("max_tokens" . ,efrit-max-tokens)
            ("temperature" . ,efrit-temperature)
            ("system" . ,system-prompt)
            ("messages" . ,(vconcat
                           (mapcar (lambda (msg)
                                   `(("role" . ,(alist-get 'role msg))
                                     ("content" . ,(alist-get 'content msg))))
                                 messages)))
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
         (url-request-data
          (encode-coding-string (json-encode request-data) 'utf-8)))
    
    ;; Log request details to work buffer
    (efrit-streamlined--log-to-work 
     (format "Request: %s characters, %s tools enabled" 
             (length url-request-data)
             (if efrit-enable-tools "tools" "no tools")))
    
    ;; Send request
    (url-retrieve efrit-api-url 'efrit-streamlined--handle-response nil t t)))

;;; Response Handler

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
      
      ;; Extract content
      (let* ((content-array (alist-get 'content response-data))
             (first-content (when (and content-array (> (length content-array) 0))
                             (aref content-array 0)))
             (text-content (when first-content (alist-get 'text first-content)))
             (tool-uses (efrit-streamlined--extract-tool-uses response-data)))
        
        ;; Log to work buffer
        (efrit-streamlined--log-to-work 
         (format "Response: %s chars text, %d tools" 
                 (if text-content (length text-content) 0)
                 (length tool-uses)))
        
        ;; Execute tools
        (when tool-uses
          (efrit-streamlined--execute-tools tool-uses))
        
        ;; Display response (Claude decides appropriate length)
        (when text-content
          (efrit-streamlined--display-response text-content))))))

;;; Tool Execution

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
  "Execute TOOL-USES and log results to work buffer."
  (efrit-streamlined--log-to-work 
   (format "Executing %d tools" (length tool-uses)))
  
  (dolist (tool-use tool-uses)
    (let* ((tool-name (alist-get 'name tool-use))
           (tool-input (alist-get 'input tool-use)))
      
      (efrit-streamlined--log-to-work 
       (format "Tool: %s" tool-name))
      
      (cond
       ((string-equal tool-name "eval_sexp")
        (let ((expr (alist-get 'expr tool-input)))
          (when expr
            (efrit-streamlined--log-to-work (format "Evaluating: %s" expr))
            (condition-case err
                (let ((result (eval (read expr))))
                  (efrit-streamlined--log-to-work 
                   (format "Result: %s" (prin1-to-string result))))
              (error
               (efrit-streamlined--log-to-work 
                (format "Error: %s" (error-message-string err))))))))
       
       ((string-equal tool-name "get_context")
        (efrit-streamlined--log-to-work "Getting context...")
        (let ((context (efrit-tools-get-context)))
          (efrit-streamlined--log-to-work 
           (format "Context: %d chars" (length context)))))
       
       (t
        (efrit-streamlined--log-to-work 
         (format "Unknown tool: %s" tool-name)))))))

;;; Response Display

(defun efrit-streamlined--display-response (content)
  "Display CONTENT in chat buffer (Claude has decided appropriate length)."
  (let* ((buffer-name (or (and (boundp 'efrit-buffer-name) efrit-buffer-name) "*efrit*"))
         (chat-buffer (get-buffer-create buffer-name)))
    (with-current-buffer chat-buffer
      ;; Ensure we're in chat mode
      (unless (derived-mode-p 'efrit-chat-mode)
        (efrit-streamlined--setup-chat-mode))
      
      ;; Add response to conversation
      (goto-char (point-max))
      (insert (format "\nA: %s\n\n" content))
      (goto-char (point-max)))))

;;; Chat Mode Setup

(defvar efrit-buffer-name "*efrit*"
  "Name of the Efrit chat buffer.")

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

;;; Public Interface

(defun efrit-streamlined-send (message)
  "Send MESSAGE using streamlined chat experience."
  (interactive "sMessage: ")
  (let ((messages (list `((role . "user") (content . ,message)))))
    (efrit-streamlined--log-to-work (format "User message: %s" message))
    (efrit-streamlined--send-request messages)))

(provide 'efrit-chat-streamlined)
;;; efrit-chat-streamlined.el ends here
