;;; efrit-agent.el --- Autonomous problem-solving agent -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (json "1.4"))
;; Keywords: tools, convenience, ai, agent
;; URL: https://github.com/stevey/efrit

;;; Commentary:
;; Autonomous problem-solving mode for Efrit. Takes high-level goals
;; and works until completion using self-directed AI consultation.

;;; Code:

(require 'json)
(require 'url)
(require 'auth-source)

;; Conditional requires
(declare-function efrit-tools-eval-sexp "efrit-tools")

;; Conditional requires
(declare-function efrit-streamlined-send "efrit-chat-streamlined")

;;; Customization

(defgroup efrit-agent nil
  "Autonomous problem-solving agent for Efrit."
  :group 'efrit
  :prefix "efrit-agent-")

(defcustom efrit-agent-max-iterations 50
  "Maximum iterations before forcing user consultation."
  :type 'integer
  :group 'efrit-agent)

(defcustom efrit-agent-api-url "https://api.anthropic.com/v1/messages"
  "API URL for Claude requests."
  :type 'string
  :group 'efrit-agent)

(defcustom efrit-agent-model "claude-4-sonnet-20250514"
  "Model to use for agent requests. Updated to latest Claude 4 Sonnet."
  :type 'string
  :group 'efrit-agent)

(defcustom efrit-agent-max-tokens 4000
  "Maximum tokens for agent responses."
  :type 'integer
  :group 'efrit-agent)

;; Use unified logging system
(require 'efrit-tools)
(require 'efrit-log)

;; Declare functions from other modules
(declare-function efrit-common-get-api-key "efrit-common")

;; Compatibility aliases
(defvaralias 'efrit-agent-debug 'efrit-log-level
  "Enable debug logging for agent operations (now uses efrit-log-level).")
(defvaralias 'efrit-agent-log-buffer 'efrit-log-buffer
  "Buffer name for agent debug logs (now uses efrit-log-buffer).")

;;; Session Management

(defvar efrit-agent--current-session nil
  "Current agent session data.")

(defun efrit-agent--create-session (goal &optional context session-id)
  "Create a new agent session for GOAL."
  (let ((session (make-hash-table :test 'equal)))
    (puthash "id" (or session-id (format "agent_%d" (floor (float-time)))) session)
    (puthash "goal" goal session)
    (puthash "context" (or context "") session)
    (puthash "status" "planning" session)
    (puthash "todos" (list) session)
    (puthash "actions" (list) session)
    (puthash "iteration" 0 session)
    (puthash "created_at" (current-time) session)
    session))

(defun efrit-agent--session-complete-p (session)
  "Check if SESSION is complete."
  (equal (gethash "status" session) "complete"))

(defun efrit-agent--update-session (session action result reflection)
  "Update SESSION with ACTION, RESULT, and REFLECTION."
  (let ((actions (gethash "actions" session)))
    (push (list :action action :result result :reflection reflection :timestamp (current-time)) actions)
    (puthash "actions" actions session)
    (puthash "iteration" (1+ (gethash "iteration" session)) session)))

(defun efrit-agent--session-summary (session)
  "Generate a summary of SESSION for display."
  (format "Session %s: %s (iteration %d, status: %s)"
          (gethash "id" session)
          (gethash "goal" session)
          (gethash "iteration" session)
          (gethash "status" session)))

;;; TODO Management

(defun efrit-agent--update-todos (session todos)
  "Update the TODO list for SESSION."
  (puthash "todos" todos session))

(defun efrit-agent--format-todos (todos)
  "Format TODOS for prompt display."
  (if (null todos)
      "(none)"
    (mapconcat (lambda (todo)
                 (format "- [%s] %s"
                         (plist-get todo :status)
                         (plist-get todo :content)))
               todos "\n")))

(defun efrit-agent--format-actions (actions)
  "Format ACTIONS for prompt display."
  (if (null actions)
      "(none)"
    (mapconcat (lambda (action)
                 (let ((output (or (plist-get (plist-get action :result) :output) "no-output")))
                   (format "- %s: %s"
                           (plist-get (plist-get action :action) :type)
                           (if (stringp output) 
                               (substring output 0 (min 100 (length output)))
                             (prin1-to-string output)))))
               (reverse (last actions 3)) "\n")))

;;; Logging and Debug Utilities

(defun efrit-agent--log (level format-string &rest args)
  "Log message with LEVEL and FORMAT-STRING with ARGS."
  (apply #'efrit-log level format-string (append args '("agent"))))

(defun efrit-agent--log-response (response-text)
  "Log raw API response for debugging."
  (efrit-log 'debug "RAW API RESPONSE:\n%s" 
            (substring response-text 0 (min 2000 (length response-text)))))

(defun efrit-agent--show-debug-buffer ()
  "Show the debug buffer in a window."
  (interactive)
  (when-let* ((buffer (get-buffer efrit-log-buffer)))
    (display-buffer buffer '(display-buffer-below-selected (window-height . 15)))))

;;; LLM Consultation

(defun efrit-agent--get-api-key ()
  "Get the Anthropic API key from .authinfo file."
  (efrit-agent--log 'debug "Looking for API key in .authinfo")
  (condition-case err
      (progn
        (require 'efrit-common)
        (efrit-agent--log 'debug "Found API key entry")
        (efrit-common-get-api-key))
    (error 
     (efrit-agent--log 'error "No API key found: %s" (error-message-string err))
     nil)))

(defun efrit-agent--build-system-prompt ()
  "Build system prompt for agent mode."
  "You are Efrit, an autonomous Emacs agent. Work systematically to achieve user goals.

For package upgrades:
1. Check if package exists and current version
2. List available packages/repositories
3. Update package lists if needed  
4. Upgrade specific package
5. Verify upgrade success

Use the eval_sexp tool to execute Elisp commands. Common patterns:
- (package-list-packages) - List all packages
- (package-refresh-contents) - Update package lists
- (package-upgrade-all) - Upgrade all packages
- (package-install 'package-name) - Install/upgrade specific package

You can also use M-x commands via: (call-interactively 'command-name)

IMPORTANT: Respond with either:
1. JSON containing: {\"status\": \"executing|complete|stuck\", \"next_action\": {\"type\": \"eval\", \"content\": \"(elisp-here)\"}, \"rationale\": \"explanation\"}
2. Or plain elisp code to execute directly

Work step-by-step until the goal is achieved.")

(defun efrit-agent--build-user-message (session)
  "Build user message for LLM consultation based on SESSION."
  (let ((goal (gethash "goal" session))
        (context (gethash "context" session))
        (todos (gethash "todos" session))
        (actions (gethash "actions" session))
        (iteration (gethash "iteration" session)))
    (format "GOAL: %s\nCONTEXT: %s\nITERATION: %d\n\nCURRENT TODOS:\n%s\n\nACTIONS TAKEN:\n%s\n\nWhat is your next action?"
            goal context iteration
            (efrit-agent--format-todos todos)
            (efrit-agent--format-actions actions))))

(defun efrit-agent--validate-http-response (response-buffer)
  "Validate HTTP response and extract body. Returns (success . body-text)."
  (with-current-buffer response-buffer
    (goto-char (point-min))
    (efrit-agent--log 'debug "=== HTTP RESPONSE HEADERS ===")
    (let ((header-end (save-excursion (search-forward-regexp "^$" nil t))))
      (when header-end
        (efrit-agent--log 'debug (buffer-substring-no-properties (point-min) header-end))
        
        ;; Check HTTP status
        (goto-char (point-min))
        (if (looking-at "HTTP/[0-9.]+ \\([0-9]+\\)")
            (let ((status-code (string-to-number (match-string 1))))
              (efrit-agent--log 'debug "HTTP status code: %d" status-code)
              (if (= status-code 200)
                  (progn
                    (goto-char header-end)
                    (let ((body-text (buffer-substring-no-properties (point) (point-max))))
                      (efrit-agent--log 'debug "Response body length: %d characters" (length body-text))
                      (cons t body-text)))
                (efrit-agent--log 'error "HTTP error %d" status-code)
                (cons nil (format "HTTP error %d" status-code))))
          (efrit-agent--log 'error "Invalid HTTP response format")
          (cons nil "Invalid HTTP response format"))))))

(defun efrit-agent--parse-api-response (response-buffer)
  "Parse Claude API response from RESPONSE-BUFFER and extract content."
  (efrit-agent--log 'debug "Parsing API response")
  (let ((validation (efrit-agent--validate-http-response response-buffer)))
    (if (car validation)
        (let ((raw-response (cdr validation)))
          (efrit-agent--log-response raw-response)
          (condition-case err
              (let* ((json-object-type 'hash-table)
                     (json-array-type 'vector)
                     (json-key-type 'string)
                     (response (json-read-from-string raw-response)))
                
                (efrit-agent--log 'debug "JSON parsed successfully")
                
                ;; Check for API errors first
                (if-let* ((error-obj (gethash "error" response)))
                    (progn
                      (efrit-agent--log 'error "API error: %s - %s" 
                                       (gethash "type" error-obj "unknown")
                                       (gethash "message" error-obj "no message"))
                      (kill-buffer response-buffer)
                      nil)
                  
                  ;; Extract content
                  (let ((content (gethash "content" response)))
                    (efrit-agent--log 'debug "Content array has %d items" (if content (length content) 0))
                    (kill-buffer response-buffer)
                    content)))
            
            (json-error
             (efrit-agent--log 'error "JSON parse error: %s" (error-message-string err))
             (efrit-agent--log 'error "Raw response: %s" (substring raw-response 0 (min 500 (length raw-response))))
             (kill-buffer response-buffer)
             nil)
            
            (error
             (efrit-agent--log 'error "Parse error: %s" (error-message-string err))
             (kill-buffer response-buffer)
             nil)))
      
      ;; HTTP validation failed
      (efrit-agent--log 'error "HTTP validation failed: %s" (cdr validation))
      (kill-buffer response-buffer)
      nil)))

(defun efrit-agent--consult-llm-callback (session callback-fn)
  "Callback function to handle LLM response for SESSION."
  (lambda (status)
    (efrit-agent--log 'debug "Callback invoked with status: %s" status)
    (condition-case err
        (let ((content (efrit-agent--parse-api-response (current-buffer))))
          (if content
              (progn
                (efrit-agent--log 'info "Successfully parsed API response")
                (funcall callback-fn session content nil))
            (efrit-agent--log 'error "Failed to parse API response")
            (funcall callback-fn session nil "Failed to parse API response")))
      (error
       (efrit-agent--log 'error "Callback error: %s" (error-message-string err))
       (funcall callback-fn session nil (format "Callback error: %s" (error-message-string err)))))))

(defun efrit-agent--consult-llm-async (session prompt callback-fn)
  "Send PROMPT to LLM backend asynchronously and call CALLBACK-FN with result."
  (efrit-agent--log 'info "Starting API request to Claude")
  (condition-case api-err
      (let* ((api-key (efrit-agent--get-api-key)))
        (if (not api-key)
            (progn
              (efrit-agent--log 'error "No API key available")
              (funcall callback-fn session nil "No API key available. Check your .authinfo file."))
          
          (let* ((url-request-method "POST")
                 (url-request-extra-headers
                  `(("x-api-key" . ,api-key)
                    ("anthropic-version" . "2023-06-01")
                    ("content-type" . "application/json")))
                 (request-data
                  `(("model" . ,efrit-agent-model)
                    ("max_tokens" . ,efrit-agent-max-tokens)
                    ("temperature" . 0.0)
                    ("messages" . [(("role" . "user")
                                   ("content" . ,prompt))])
                    ("system" . ,(efrit-agent--build-system-prompt))
                    ("tools" . [(("name" . "eval_sexp")
                                ("description" . "Evaluate a Lisp expression and return the result")
                                ("input_schema" . (("type" . "object")
                                                  ("properties" . (("expr" . (("type" . "string")
                                                                              ("description" . "The Elisp expression to evaluate")))))
                                                  ("required" . ["expr"]))))
                               (("name" . "get_context") 
                                ("description" . "Get current Emacs context and environment")
                                ("input_schema" . (("type" . "object")
                                                  ("properties" . ()))))])))
                 (url-request-data
                  (encode-coding-string (json-encode request-data) 'utf-8)))
            
            (efrit-agent--log 'debug "Request payload size: %d bytes" (length url-request-data))
            (efrit-agent--log 'debug "Model: %s, Max tokens: %d" efrit-agent-model efrit-agent-max-tokens)
            (efrit-agent--log 'info "Sending request to %s" efrit-agent-api-url)
            
            (url-retrieve efrit-agent-api-url 
                          (efrit-agent--consult-llm-callback session callback-fn)))))
    (error
     (efrit-agent--log 'error "API setup error: %s" (error-message-string api-err))
     (funcall callback-fn session nil (format "API setup error: %s" (error-message-string api-err))))))

(defun efrit-agent--extract-text-from-content (content)
  "Extract text content from Claude's CONTENT array."
  (efrit-agent--log 'debug "Extracting text from content array")
  (let ((text ""))
    (when content
      (dotimes (i (length content))
        (let* ((item (aref content i))
               (type (gethash "type" item)))
          (efrit-agent--log 'debug "Processing content item %d: type=%s" i type)
          (when (string= type "text")
            (when-let* ((item-text (gethash "text" item)))
              (efrit-agent--log 'debug "Found text content: %s" 
                               (substring item-text 0 (min 100 (length item-text))))
              (setq text (concat text item-text)))))))
    (efrit-agent--log 'debug "Total extracted text length: %d" (length text))
    text))

(defun efrit-agent--parse-signal (content)
  "Parse Claude's CONTENT array into signal plist."
  (efrit-agent--log 'debug "Parsing signal from content")
  (condition-case err
      (let ((text (efrit-agent--extract-text-from-content content)))
        (efrit-agent--log 'debug "Checking if response is JSON format")
        (if (string-match-p "^[[:space:]]*{" text)
            ;; Try to parse as JSON
            (progn
              (efrit-agent--log 'debug "Attempting JSON parse of response")
              (let* ((json-object-type 'hash-table)
                     (json-array-type 'list)
                     (json-key-type 'string)
                     (parsed (json-read-from-string text)))
                (efrit-agent--log 'info "Successfully parsed JSON response")
                (let ((status (intern (or (gethash "status" parsed) "executing")))
                      (next-action (gethash "next_action" parsed))
                      (rationale (gethash "rationale" parsed)))
                  (efrit-agent--log 'debug "Parsed status: %s" status)
                  (efrit-agent--log 'debug "Next action type: %s" (and next-action (plist-get next-action :type)))
                  (list :status status
                        :todos (gethash "todos" parsed)
                        :next_action next-action
                        :rationale rationale))))
          ;; If not JSON, create a simple action to execute the text as elisp
          (progn
            (efrit-agent--log 'info "Response is not JSON, treating as elisp code")
            (list :status 'executing
                  :next_action `(:type eval :content ,text)
                  :rationale "Executing response as elisp"))))
    (error
     (efrit-agent--log 'error "Signal parse error: %s" (error-message-string err))
     (list :status 'error :rationale (format "Parse error: %s" (error-message-string err))))))

;;; Action Execution

(defun efrit-agent--execute-action (action)
  "Execute ACTION and return result hash."
  (let ((action-type (plist-get action :type))
        (content (plist-get action :content))
        (result (make-hash-table :test 'equal)))
    
    (efrit-agent--log 'info "Executing action: %s" action-type)
    (efrit-agent--log 'debug "Action content: %s" (substring content 0 (min 200 (length content))))
    
    (puthash "timestamp" (current-time) result)
    (puthash "action_type" action-type result)
    (condition-case err
        (pcase action-type
          ('eval
           (efrit-agent--log 'debug "Loading efrit-tools for eval")
           (require 'efrit-tools)
           (efrit-agent--log 'debug "Evaluating elisp: %s" content)
           (let ((eval-result (efrit-tools-eval-sexp content)))
             (efrit-agent--log 'info "Eval successful, result: %s" (prin1-to-string eval-result))
             (puthash "success" t result)
             (puthash "output" (prin1-to-string eval-result) result)))
          ('shell
           (efrit-agent--log 'debug "Executing shell command: %s" content)
           (let ((shell-result (shell-command-to-string content)))
             (efrit-agent--log 'info "Shell command completed")
             (puthash "success" t result)
             (puthash "output" shell-result result)))
          ('user_input
           (efrit-agent--log 'debug "Requesting user input: %s" content)
           (let ((user-response (read-string (format "Efrit needs input: %s " content))))
             (efrit-agent--log 'info "User provided input: %s" user-response)
             (puthash "success" t result)
             (puthash "output" user-response result)))
          (_
           (efrit-agent--log 'error "Unknown action type: %s" action-type)
           (puthash "success" nil result)
           (puthash "error" (format "Unknown action type: %s" action-type) result)))
      (error
       (efrit-agent--log 'error "Action execution failed: %s" (error-message-string err))
       (puthash "success" nil result)
       (puthash "error" (error-message-string err) result)))
    
    (efrit-agent--log 'debug "Action execution result: success=%s" (gethash "success" result))
    result))

;;; Main Agent Loop

(defun efrit-agent--process-response (session content error-msg)
  "Process LLM CONTENT for SESSION and continue or complete."
  (efrit-agent--log 'info "Processing agent response")
  (if error-msg
      (progn
        (efrit-agent--log 'error "Response processing failed: %s" error-msg)
        (message "🔥 Agent error: %s" error-msg)
        (efrit-agent--show-debug-buffer))
    (let ((signal (efrit-agent--parse-signal content)))
      
      ;; Update session status  
      (puthash "status" (symbol-name (plist-get signal :status)) session)
      (puthash "iteration" (1+ (gethash "iteration" session)) session)
      
      ;; Handle signal status
      (pcase (plist-get signal :status)
        ('stuck
         (message "Agent stuck, requesting user input")
         (let ((user-input (read-string "Efrit is stuck. Help: ")))
           (puthash "context" (concat (gethash "context" session) "\n\nUser input: " user-input) session)
           (efrit-agent--continue-loop session)))
        ('complete
         (message "Agent completed goal: %s" (gethash "goal" session))
         (message "%s" (efrit-agent--session-summary session)))
        ('error
         (message "Agent error: %s" (plist-get signal :rationale)))
        (_
         ;; Execute next action
         (let ((next-action (plist-get signal :next_action)))
           (when next-action
             (efrit-agent--log 'info "Executing next action: %s" (plist-get next-action :type))
         (message "🔧 Agent executing: %s" (plist-get next-action :type))
             (let ((result (efrit-agent--execute-action next-action)))
               (efrit-agent--update-session session next-action result signal)
               (efrit-agent--continue-loop session)))))))))

(defun efrit-agent--continue-loop (session)
  "Continue the agent loop for SESSION."
  (when (and (not (efrit-agent--session-complete-p session))
             (< (gethash "iteration" session) efrit-agent-max-iterations))
    (let ((prompt (efrit-agent--build-user-message session)))
      (efrit-agent--consult-llm-async session prompt #'efrit-agent--process-response))))

;;;###autoload
(defun efrit-agent-solve (goal &optional context session-id)
  "Autonomous problem-solving mode for GOAL."
  (interactive "sGoal: ")
  (efrit-agent--log 'info "Starting new agent session with goal: %s" goal)
  (let ((session (efrit-agent--create-session goal context session-id)))
    (setq efrit-agent--current-session session)
    (efrit-agent--log 'debug "Session created with ID: %s" (gethash "id" session))
    (message "🚀 Efrit Agent starting: %s" goal)
    (when efrit-agent-debug
      (message "Debug logging enabled. Use M-x efrit-agent--show-debug-buffer to view logs.")
      (efrit-agent--show-debug-buffer))
    (efrit-agent--continue-loop session)
    session))

;;;###autoload
(defun efrit-agent-run (goal &optional context)
  "Run agent to achieve GOAL with optional CONTEXT."
  (interactive "sGoal: ")
  (efrit-agent-solve goal context))

(provide 'efrit-agent)
;;; efrit-agent.el ends here
