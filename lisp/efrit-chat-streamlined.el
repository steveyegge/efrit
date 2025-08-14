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

(require 'json)
(require 'url)
(require 'cl-lib)

;; Forward declarations to avoid byte-compile warnings
(declare-function efrit-tools-system-prompt "efrit-tools")
(declare-function efrit-tools-get-context "efrit-tools")
(declare-function efrit--get-api-key "efrit")

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

(defvar efrit-streamlined--current-messages nil
  "Messages for the current conversation turn.")

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
            
            ;; Check if Claude wants to continue (has tool use without final text)
            (let ((should-continue (and tool-uses 
                                       (or (not has-text-content)
                                           (string-match-p "\\.\\.\\.$\\|continue\\|more\\|next" text-content)))))
              
              (if should-continue
                  ;; Continue conversation with tool results
                  (progn
                    (efrit-streamlined--log-to-work "Continuing conversation with tool results")
                    (efrit-streamlined--continue-with-results tool-results text-content))
                
                ;; Final response - display it
                (when has-text-content
                  (efrit-streamlined--display-response text-content))))))))))

(defun efrit-streamlined--continue-with-results (tool-results text-content)
  "Continue conversation with TOOL-RESULTS from executed tools."
  ;; Build tool result messages
  (let ((result-messages '()))
    (dolist (result tool-results)
      (when result
        (push `((role . "user") (content . ,result)) result-messages)))
    
    ;; Add assistant's text if any
    (when (and text-content (not (string-empty-p (string-trim text-content))))
      (push `((role . "assistant") (content . ,text-content)) result-messages))
    
    ;; Continue conversation with results
    (when result-messages
      (let ((updated-messages (append efrit-streamlined--current-messages 
                                      (reverse result-messages))))
        (setq efrit-streamlined--current-messages updated-messages)
        (efrit-streamlined--send-request updated-messages)))))

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
  "Execute TOOL-USES and return list of result strings for continuation."
  (efrit-streamlined--log-to-work 
   (format "Executing %d tools" (length tool-uses)))
  
  (let ((results '()))
    (dolist (tool-use tool-uses)
      (let* ((tool-name (alist-get 'name tool-use))
             (tool-input (alist-get 'input tool-use))
             (tool-id (alist-get 'id tool-use)))
        
        (efrit-streamlined--log-to-work 
         (format "Tool: %s (id: %s)" tool-name tool-id))
        
        (cond
         ((string-equal tool-name "eval_sexp")
          (let ((expr (alist-get 'expr tool-input)))
            (when expr
              (efrit-streamlined--log-to-work (format "Evaluating: %s" expr))
              (condition-case err
                  (let ((result (eval (read expr))))
                    (let ((result-str (prin1-to-string result)))
                      (efrit-streamlined--log-to-work 
                       (format "Result: %s" result-str))
                      ;; Add tool result message for Claude
                      (push (format "Tool result for %s: %s" tool-id result-str) results)))
                (error
                 (let ((error-msg (error-message-string err)))
                   (efrit-streamlined--log-to-work 
                    (format "Error: %s" error-msg))
                   ;; Add error result for Claude
                   (push (format "Tool error for %s: %s" tool-id error-msg) results)))))))
         
         ((string-equal tool-name "get_context")
         (efrit-streamlined--log-to-work "Getting context...")
         (let ((context (if (fboundp 'efrit-tools-get-context)
                          (efrit-tools-get-context)
                       "Context not available - efrit-tools not loaded")))
         (efrit-streamlined--log-to-work 
          (format "Context: %d chars" (length context)))
             ;; Add context result for Claude
             (push (format "Tool result for %s: %s" tool-id (substring context 0 (min 500 (length context)))) results)))
         
         (t
          (efrit-streamlined--log-to-work 
           (format "Unknown tool: %s" tool-name))
          ;; Add error for unknown tool
          (push (format "Tool error for %s: Unknown tool %s" tool-id tool-name) results)))))
    
    (reverse results)))

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
    (setq efrit-streamlined--current-messages messages)
    (efrit-streamlined--log-to-work (format "User message: %s" message))
    (efrit-streamlined--send-request messages)))

(provide 'efrit-chat-streamlined)
;;; efrit-chat-streamlined.el ends here
