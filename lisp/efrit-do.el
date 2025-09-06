;;; efrit-do.el --- Execute natural language commands in Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.2.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai
;; URL: https://github.com/stevey/efrit

;;; Commentary:
;; This file provides a simple command interface for executing natural language
;; commands in Emacs using the Efrit assistant.  It allows users to issue
;; commands like "open Dired to my downloads folder" and have them executed
;; immediately.

;;; Code:

;; Declare external functions from efrit-chat (optional dependency)
(declare-function efrit--setup-buffer "efrit-chat")
(declare-function efrit--display-message "efrit-chat")
(declare-function efrit--insert-prompt "efrit-chat")

;; Declare external functions from efrit-async
(declare-function efrit-async-execute-command "efrit-async")

(require 'efrit-tools)
(require 'efrit-config)
(require 'efrit-common)
(require 'cl-lib)
(require 'seq)
(require 'ring)
(require 'url)
(require 'json)

;;; Customization

(defgroup efrit-do nil
  "Natural language command execution for Efrit."
  :group 'efrit
  :prefix "efrit-do-")

(defcustom efrit-do-buffer-name "*efrit-do*"
  "Name of the buffer for command execution results."
  :type 'string
  :group 'efrit-do)

(defcustom efrit-do-show-results t
  "Whether to show command results in a buffer."
  :type 'boolean
  :group 'efrit-do)

(defcustom efrit-do-show-errors-only t
  "When non-nil, only show the *efrit-do* buffer for errors.
When nil, show buffer for all results (controlled by `efrit-do-show-results')."
  :type 'boolean
  :group 'efrit-do)

(defcustom efrit-do-history-max 50
  "Maximum number of commands to keep in history."
  :type 'integer
  :group 'efrit-do)

(defcustom efrit-do-debug nil
  "Whether to show debug information during command execution."
  :type 'boolean
  :group 'efrit-do)

(defcustom efrit-do-context-size 10
  "Size of the context ring."
  :type 'integer
  :group 'efrit-do)

(defcustom efrit-do-context-file nil
  "File to persist context data.
If nil, uses the default location in the efrit data directory."
  :type '(choice (const :tag "Default location" nil)
                 (file :tag "Custom file"))
  :group 'efrit-do)

(defcustom efrit-do-max-retries 3
  "Maximum number of retry attempts when commands fail."
  :type 'integer
  :group 'efrit-do)

(defcustom efrit-do-retry-on-errors t
  "Whether to automatically retry failed commands by sending errors back to Claude."
  :type 'boolean
  :group 'efrit-do)

(defcustom efrit-model "claude-sonnet-4-20250514"
  "Claude model to use for efrit-do commands."
  :type 'string
  :group 'efrit-do)

(defcustom efrit-max-tokens 8192
  "Maximum number of tokens in the response for efrit-do."
  :type 'integer
  :group 'efrit-do)

(defcustom efrit-api-url "https://api.anthropic.com/v1/messages"
  "URL for the Anthropic API endpoint used by efrit-do."
  :type 'string
  :group 'efrit-do)

;;; Internal variables

(defvar efrit-do-history nil
  "History of executed commands.")

(defvar efrit-do--last-result nil
  "Result of the last executed command.")

(defvar efrit-do--current-todos nil
  "Current TODO list for the active command session.")

(defvar efrit-do--todo-counter 0
  "Counter for generating unique TODO IDs.")

(defconst efrit-do--tools-schema
  [(("name" . "eval_sexp")
    ("description" . "Evaluate a Lisp expression and return the result. This is the primary tool for interacting with Emacs.")
    ("input_schema" . (("type" . "object")
                      ("properties" . (("expr" . (("type" . "string")
                                                  ("description" . "The Elisp expression to evaluate")))))
                      ("required" . ["expr"]))))
   (("name" . "shell_exec")
    ("description" . "Execute a shell command and return the result.")
    ("input_schema" . (("type" . "object")
                      ("properties" . (("command" . (("type" . "string")
                                                     ("description" . "The shell command to execute")))))
                      ("required" . ["command"]))))
   (("name" . "todo_add")
    ("description" . "Add a new TODO item to track progress.")
    ("input_schema" . (("type" . "object")
                      ("properties" . (("content" . (("type" . "string")
                                                     ("description" . "The TODO item description")))
                                      ("priority" . (("type" . "string")
                                                    ("enum" . ["low" "medium" "high"])
                                                    ("description" . "Priority level")))))
                      ("required" . ["content"]))))
   (("name" . "todo_update")
    ("description" . "Update the status of a TODO item.")
    ("input_schema" . (("type" . "object")
                      ("properties" . (("id" . (("type" . "string")
                                                ("description" . "The TODO item ID")))
                                      ("status" . (("type" . "string")
                                                  ("enum" . ["todo" "in-progress" "completed"])
                                                  ("description" . "New status")))))
                      ("required" . ["id" "status"]))))
   (("name" . "todo_show")
    ("description" . "Show all current TODO items.")
    ("input_schema" . (("type" . "object")
                      ("properties" . ()))))
    (("name" . "buffer_create")
     ("description" . "Create a new buffer with content and optional mode. Use this for reports, lists, and formatted output.")
     ("input_schema" . (("type" . "object")
                       ("properties" . (("name" . (("type" . "string")
                                                   ("description" . "Buffer name (e.g. '*efrit-report: Files*')")))
                                       ("content" . (("type" . "string")
                                                     ("description" . "Buffer content")))
                                       ("mode" . (("type" . "string")
                                                 ("description" . "Optional major mode (e.g. 'markdown-mode', 'org-mode')")))))
                       ("required" . ["name" "content"]))))
    (("name" . "format_file_list")
     ("description" . "Format content as a markdown file list with bullet points.")
     ("input_schema" . (("type" . "object")
                       ("properties" . (("content" . (("type" . "string")
                                                      ("description" . "Raw content to format as file list")))))
                       ("required" . ["content"]))))
    (("name" . "format_todo_list")
     ("description" . "Format TODO list with optional sorting.")
     ("input_schema" . (("type" . "object")
                       ("properties" . (("sort_by" . (("type" . "string")
                                                      ("enum" . ["status" "priority"])
                                                      ("description" . "Optional sorting criteria")))))
                       ("required" . []))))
    (("name" . "display_in_buffer")
     ("description" . "Display content in a specific buffer.")
     ("input_schema" . (("type" . "object")
                       ("properties" . (("buffer_name" . (("type" . "string")
                                                          ("description" . "Buffer name")))
                                       ("content" . (("type" . "string")
                                                     ("description" . "Content to display")))
                                       ("window_height" . (("type" . "number")
                                                          ("description" . "Optional window height")))))
                       ("required" . ["buffer_name" "content"]))))
   (("name" . "session_complete")
    ("description" . "Mark the current session as complete with a final result message.")
    ("input_schema" . (("type" . "object")
                      ("properties" . (("message" . (("type" . "string")
                                                     ("description" . "Completion message summarizing what was accomplished")))))
                      ("required" . ["message"]))))
   (("name" . "suggest_execution_mode")
    ("description" . "Suggest whether a command should run synchronously or asynchronously based on its characteristics.")
    ("input_schema" . (("type" . "object")
                      ("properties" . (("mode" . (("type" . "string")
                                                  ("enum" . ["sync" "async"])
                                                  ("description" . "Suggested execution mode")))
                                      ("reason" . (("type" . "string")
                                                   ("description" . "Brief explanation for the suggestion")))))
                      ("required" . ["mode"]))))]
  "Schema definition for all available tools in efrit-do mode.")

;;; Context system

(cl-defstruct (efrit-do-context-item
            (:constructor efrit-do-context-item-create)
            (:type vector))
  "Context item structure."
  timestamp
  command
  result
  buffer
  directory
  window-config
  metadata)

(defvar efrit-do--context-ring nil
  "Ring buffer for context items.")

(defvar efrit-do--context-hooks nil
  "Hooks run when context is captured.")

;;; Session protocol instructions

(defun efrit-do--session-protocol-instructions ()
  "Return detailed instructions for Claude about the session protocol."
  (concat
   "SESSION PROTOCOL:\n"
   "You are continuing a multi-step session. Your goal is to complete the task incrementally.\n\n"
   
   "1. WORK LOG INTERPRETATION:\n"
   "   - The work log shows your previous steps: [[\"result1\", \"code1\"], [\"result2\", \"code2\"], ...]\n"
   "   - Each entry contains [result, code] from a previous tool execution\n"
   "   - Use this to understand what you've already done and what remains\n\n"
   
   "2. CONTINUATION DECISION:\n"
   "   - Analyze the work log and original command to determine next steps\n"
   "   - If the task is incomplete, execute the next logical step\n"
   "   - If the task is complete, use the session_complete tool with final result\n\n"
   
   "3. TWO-PHASE EXECUTION MODEL:\n"
   "   Phase 1 - Context Gathering (if needed):\n"
   "   - Use eval_sexp to gather information about Emacs state\n"
   "   - Examples: (buffer-list), (point), (buffer-substring-no-properties ...)\n"
   "   - Results help you make informed decisions\n\n"
   "   Phase 2 - Action Execution:\n"
   "   - Based on Phase 1 results (if any) and work log, execute the action\n"
   "   - Examples: create buffers, modify text, run commands\n"
   "   - Each execution adds to the work log for next continuation\n\n"
   
   "4. MULTI-STEP EXAMPLE:\n"
   "   Command: 'fetch weather and format it'\n"
   "   Step 1: shell_exec to fetch weather data\n"
   "   Step 2: buffer_create to display formatted result\n"
   "   Step 3: session_complete with success message\n\n"
   
   "5. IMPORTANT RULES:\n"
   "   - Execute ONE meaningful action per continuation\n"
   "   - Each step should make progress toward the goal\n"
   "   - Use session_complete when task is fully done\n"
   "   - Keep responses minimal - focus on execution\n"
   "   - The work log provides your memory across steps\n"))

;;; Context ring management

(defun efrit-do--ensure-context-ring ()
  "Ensure the context ring is initialized."
  (unless efrit-do--context-ring
    (setq efrit-do--context-ring (make-ring efrit-do-context-size))))

(defun efrit-do--capture-context (command result)
  "Capture current context after executing COMMAND with RESULT."
  (efrit-do--ensure-context-ring)
  (let ((item (efrit-do-context-item-create
               :timestamp (current-time)
               :command command
               :result (when result
                        ;; Truncate result to prevent context accumulation
                        (truncate-string-to-width result 2000 nil nil t))
               :buffer (buffer-name)
               :directory default-directory
               :window-config (current-window-configuration)
               :metadata (list :point (point)
                              :mark (mark)
                              :major-mode major-mode))))
    (ring-insert efrit-do--context-ring item)
    (run-hook-with-args 'efrit-do--context-hooks item)))

(defun efrit-do--get-context-items (&optional n)
  "Get N most recent context items (default all)."
  (efrit-do--ensure-context-ring)
  (let ((ring efrit-do--context-ring)
        (count (or n (ring-length efrit-do--context-ring)))
        items)
    (dotimes (i (min count (ring-length ring)))
      (push (ring-ref ring i) items))
    items))

(defun efrit-do--context-to-string (item)
  "Convert context ITEM to string representation."
  (format "[%s] %s -> %s (in %s)"
          (format-time-string "%H:%M:%S" (efrit-do-context-item-timestamp item))
          (efrit-do-context-item-command item)
          (truncate-string-to-width (or (efrit-do-context-item-result item) "") 50 nil nil t)
          (efrit-do-context-item-buffer item)))

(defun efrit-do--clear-context ()
  "Clear the context ring."
  (when efrit-do--context-ring
    (setq efrit-do--context-ring (make-ring efrit-do-context-size))))

;;; TODO Management

(cl-defstruct (efrit-do-todo-item
                (:constructor efrit-do-todo-item-create)
                (:type vector))
  "TODO item structure."
  id
  content
  status    ; 'todo, 'in-progress, 'completed
  priority  ; 'low, 'medium, 'high
  created-at
  completed-at)

(defun efrit-do--generate-todo-id ()
  "Generate a unique TODO ID."
  (setq efrit-do--todo-counter (1+ efrit-do--todo-counter))
  (format "efrit-todo-%d" efrit-do--todo-counter))

(defun efrit-do--add-todo (content &optional priority)
  "Add a new TODO item with CONTENT and optional PRIORITY."
  (unless (and content (stringp content) (not (string= "" (string-trim content))))
    (error "TODO content must be a non-empty string"))
  (unless (member priority '(low medium high nil))
    (error "TODO priority must be one of: low, medium, high"))
  (let ((todo (efrit-do-todo-item-create
               :id (efrit-do--generate-todo-id)
               :content content
               :status 'todo
               :priority (or priority 'medium)
               :created-at (current-time)
               :completed-at nil)))
    (push todo efrit-do--current-todos)
    (efrit-log 'debug "Added TODO: %s (priority: %s)" content (or priority 'medium))
    todo))

(defun efrit-do--update-todo-status (id new-status)
  "Update TODO with ID to NEW-STATUS."
  (unless (and (stringp id) (not (string= "" id)))
    (error "TODO ID must be a non-empty string"))
  (unless (member new-status '(todo in-progress completed))
    (error "TODO status must be one of: todo, in-progress, completed"))
  (when-let* ((todo (seq-find (lambda (item) 
                               (string= (efrit-do-todo-item-id item) id))
                             efrit-do--current-todos)))
    (setf (efrit-do-todo-item-status todo) new-status)
    (when (eq new-status 'completed)
      (setf (efrit-do-todo-item-completed-at todo) (current-time)))
    (efrit-log 'debug "Updated TODO %s to status: %s" id new-status)
    todo))

(defun efrit-do--format-todos-for-display ()
  "Format current TODOs for user display in raw order."
  (if (null efrit-do--current-todos)
      "No current TODOs"
    (mapconcat (lambda (todo)
                 (format "%s [%s] %s (%s)"
                         (pcase (efrit-do-todo-item-status todo)
                           ('todo "☐")
                           ('in-progress "⟳")
                           ('completed "☑"))
                         (upcase (symbol-name (efrit-do-todo-item-priority todo)))
                         (efrit-do-todo-item-content todo)
                         (efrit-do-todo-item-id todo)))
               efrit-do--current-todos "\n")))

(defun efrit-do--format-todos-for-prompt ()
  "Format current TODOs for AI prompt context."
  (if (null efrit-do--current-todos)
      ""
    (concat "\n\nCURRENT TODOs:\n" (efrit-do--format-todos-for-display) "\n")))

(defun efrit-do--clear-todos ()
  "Clear all current TODOs."
  (setq efrit-do--current-todos nil)
  (setq efrit-do--todo-counter 0))

;;; Context persistence

(defun efrit-do--save-context ()
  "Save context ring to file."
  (when efrit-do--context-ring
    (let ((file (or efrit-do-context-file 
                   (efrit-config-context-file "efrit-do-context.el")))
          (items (mapcar (lambda (item)
                          ;; Create a copy without window-config for serialization
                          (efrit-do-context-item-create
                           :timestamp (efrit-do-context-item-timestamp item)
                           :command (efrit-do-context-item-command item)
                           :result (when (efrit-do-context-item-result item)
                                     ;; Truncate result to prevent context accumulation
                                     (truncate-string-to-width 
                                      (efrit-do-context-item-result item) 2000 nil nil t))
                           :buffer (efrit-do-context-item-buffer item)
                           :directory (efrit-do-context-item-directory item)
                           :window-config nil  ; Skip window config
                           :metadata (efrit-do-context-item-metadata item)))
                        (efrit-do--get-context-items))))
      ;; Ensure directory exists
      (make-directory (file-name-directory file) t)
      (condition-case err
          (with-temp-file file
            (insert ";;; Efrit-do context data\n")
            (insert (format ";; Saved: %s\n" (current-time-string)))
            (insert (format "(setq efrit-do--saved-context-data\n'%S)\n" items)))
        (error
         (when efrit-do-debug
           (message "Error saving context: %s" (error-message-string err))))))))

(defun efrit-do--load-context ()
  "Load context ring from file."
  (let ((file (or efrit-do-context-file 
                 (efrit-config-context-file "efrit-do-context.el"))))
    (when (file-readable-p file)
      (condition-case err
          (progn
            (load-file file)
            (efrit-do--ensure-context-ring)
            (when (boundp 'efrit-do--saved-context-data)
              (dolist (item (reverse efrit-do--saved-context-data))
                (when (vectorp item)
                  (ring-insert efrit-do--context-ring item)))
              (makunbound 'efrit-do--saved-context-data)))
        (error
         (when efrit-do-debug
           (message "Error loading context: %s" (error-message-string err))))))))

;;; Helper functions for improved error handling

(defun efrit-do--build-error-context ()
  "Build rich contextual information for Claude when fixing errors.
Returns a string with current Emacs state, buffer info, and recent history."
  (let* ((current-buffer-name (buffer-name))
         (current-mode (symbol-name major-mode))
         (current-point (point))
         (buffer-size (buffer-size))
         (current-dir default-directory)
         (visible-buffers (mapcar #'buffer-name 
                                 (mapcar #'window-buffer (window-list))))
         (recent-context (efrit-do--get-context-items 2))
         (context-parts '()))
    
    ;; Current buffer information
    (push (format "CURRENT BUFFER: %s (mode: %s, point: %d/%d)" 
                  current-buffer-name current-mode current-point buffer-size)
          context-parts)
    
    ;; Current directory
    (push (format "CURRENT DIRECTORY: %s" current-dir) context-parts)
    
    ;; Visible buffers
    (push (format "VISIBLE BUFFERS: %s" 
                  (string-join visible-buffers ", "))
          context-parts)
    
    ;; Buffer content around point (if buffer has content)
    (when (> buffer-size 0)
      (let* ((start-pos (max (point-min) (- current-point 200)))
             (end-pos (min (point-max) (+ current-point 200)))
             (content-snippet (buffer-substring-no-properties start-pos end-pos))
             (lines (split-string content-snippet "\n" t)) ; Remove empty lines
             (truncated-lines (if (> (length lines) 10)
                                 (append (seq-take lines 4)
                                        '("...")
                                        (nthcdr (- (length lines) 4) lines))
                               lines)))
        (push (format "BUFFER CONTENT AROUND POINT:\n%s" 
                      (string-join truncated-lines "\n"))
              context-parts)))
    
    ;; Recent command history
    (when recent-context
      (push "RECENT COMMANDS:" context-parts)
      (dolist (item recent-context)
        (push (format "  %s -> %s" 
                      (efrit-do-context-item-command item)
                      (truncate-string-to-width 
                       (or (efrit-do-context-item-result item) "no result") 
                       60 nil nil t))
              context-parts)))
    
    ;; Window configuration info
    (push (format "WINDOW LAYOUT: %d windows" (length (window-list)))
          context-parts)
    
    ;; Join all context parts
    (string-join (reverse context-parts) "\n")))

(defun efrit-do--extract-error-info (result)
  "Extract error information from RESULT string.
Returns (error-p . error-msg) where error-p is t if errors found."
  (when (stringp result)
    (cond
     ;; Syntax errors
     ((string-match "\\[Syntax Error in \\([^:]+\\): \\(.+\\)\\]" result)
      (cons t (format "Syntax error in %s: %s" 
                      (match-string 1 result) 
                      (match-string 2 result))))
     ;; Runtime errors
     ((string-match "\\[Error executing \\([^:]+\\): \\(.+\\)\\]" result)
      (cons t (format "Runtime error in %s: %s" 
                      (match-string 1 result) 
                      (match-string 2 result))))
     ;; API errors
     ((string-match "API Error" result)
      (cons t result))
     ;; General errors
     ((string-match "Error:" result)
      (cons t result))
     ;; No error detected
     (t (cons nil nil)))))

(defun efrit-do--extract-executed-code (result)
  "Extract the executed code from RESULT string.
Returns the code string that was executed, or nil if not found."
  (when (stringp result)
    (cond
     ;; Extract from syntax error message
     ((string-match "\\[Syntax Error in \\([^:]+\\):" result)
      (match-string 1 result))
     ;; Extract from runtime error message
     ((string-match "\\[Error executing \\([^:]+\\):" result)
      (match-string 1 result))
     ;; Extract from successful execution message
     ((string-match "\\[Executed: \\([^]]+\\)\\]" result)
      (match-string 1 result))
     ;; No code found
     (t nil))))

(defun efrit-do--validate-elisp (code-string)
  "Check if CODE-STRING is valid elisp syntax.
Returns (valid-p . error-msg) where valid-p is t/nil and error-msg 
describes the syntax error if validation fails."
  (when (and code-string (stringp code-string))
    (condition-case err
        (progn 
          ;; Try to read the string as elisp - this catches syntax errors
          (ignore (read-from-string code-string))
          (cons t nil))
      (error 
       (cons nil (error-message-string err))))))

(defun efrit-do--extract-response-text (response-buffer)
  "Extract response text from RESPONSE-BUFFER with proper cleanup.
Returns the response body as a string, or nil if extraction fails.
The RESPONSE-BUFFER is automatically killed after extraction."
  (unwind-protect
      (when (buffer-live-p response-buffer)
        (with-current-buffer response-buffer
          (goto-char (point-min))
          (when (search-forward-regexp "^$" nil t)
            (buffer-substring-no-properties (point) (point-max)))))
    (when (buffer-live-p response-buffer)
      (kill-buffer response-buffer))))

(defun efrit-do--sanitize-elisp-string (str)
  "Sanitize potentially over-escaped elisp string from JSON.
This handles cases where JSON escaping has been applied multiple times."
  (when str
    ;; Try to detect and fix over-escaped strings
    (let ((cleaned str))
      ;; If we see patterns like \\\\b, it's likely over-escaped
      (when (string-match-p "\\\\\\\\\\\\b\\|\\\\\\\\\\\\(" cleaned)
        (setq cleaned (replace-regexp-in-string "\\\\\\\\\\\\b" "\\\\b" cleaned))
        (setq cleaned (replace-regexp-in-string "\\\\\\\\\\\\(" "\\\\(" cleaned))
        (setq cleaned (replace-regexp-in-string "\\\\\\\\\\\\)" "\\\\)" cleaned))
        (setq cleaned (replace-regexp-in-string "\\\\\\\\\\\\|" "\\\\|" cleaned)))
      cleaned)))

;;; Tool execution - Helper functions

(defun efrit-do--handle-eval-sexp (input-str)
  "Handle elisp evaluation with validation."
  (let ((validation (efrit-do--validate-elisp input-str)))
    (if (car validation)
        ;; Valid elisp - proceed with execution
        (condition-case eval-err
            (let ((eval-result (efrit-tools-eval-sexp input-str)))
              (format "\n[Executed: %s]\n[Result: %s]" input-str eval-result))
          (error
           (format "\n[Error executing %s: %s]" 
                   input-str (error-message-string eval-err))))
      ;; Invalid elisp - report syntax error
      (format "\n[Syntax Error in %s: %s]" 
              input-str (cdr validation)))))

(defun efrit-do--handle-shell-exec (input-str)
  "Handle shell command execution."
  (condition-case shell-err
      (let ((shell-result (shell-command-to-string input-str)))
        (format "\n[Executed: %s]\n[Result: %s]" input-str shell-result))
    (error
     (format "\n[Error executing shell command %s: %s]" 
             input-str (error-message-string shell-err)))))

(defun efrit-do--handle-todo-add (tool-input)
  "Handle TODO item addition."
  (let* ((content (if (hash-table-p tool-input)
                     (gethash "content" tool-input)
                   tool-input))
         (priority (when (hash-table-p tool-input)
                    (intern (or (gethash "priority" tool-input) "medium"))))
         (todo (efrit-do--add-todo content priority)))
    (format "\n[Added TODO: %s (%s)]" content (efrit-do-todo-item-id todo))))

(defun efrit-do--handle-todo-update (tool-input input-str)
  "Handle TODO status update."
  (let* ((id (if (hash-table-p tool-input)
                (gethash "id" tool-input)
              input-str))
         (status (when (hash-table-p tool-input)
                  (intern (gethash "status" tool-input)))))
    (if (efrit-do--update-todo-status id status)
        (format "\n[Updated TODO %s to %s]" id status)
      (format "\n[Error: TODO %s not found]" id))))

(defun efrit-do--handle-todo-show ()
  "Handle TODO list display."
  (format "\n[Current TODOs:]\n%s" (efrit-do--format-todos-for-display)))

(defun efrit-do--handle-buffer-create (tool-input input-str)
  "Handle buffer creation."
  (let* ((name (if (hash-table-p tool-input)
                  (gethash "name" tool-input)
                ""))
         (content (if (hash-table-p tool-input)
                     (gethash "content" tool-input)
                   input-str))
         (mode (when (hash-table-p tool-input)
                 (let ((mode-str (gethash "mode" tool-input)))
                   (when mode-str (intern mode-str))))))
    (condition-case err
        (let ((result (efrit-tools-create-buffer name content mode)))
          (format "\n[%s]" result))
      (error
       (format "\n[Error creating buffer: %s]" (error-message-string err))))))

(defun efrit-do--handle-format-file-list (input-str)
  "Handle file list formatting."
  (condition-case err
      (let ((result (efrit-tools-format-file-list input-str)))
        (format "\n[Formatted file list]\n%s" result))
    (error
     (format "\n[Error formatting file list: %s]" (error-message-string err)))))

(defun efrit-do--handle-format-todo-list (tool-input)
  "Handle TODO list formatting."
  (let ((sort-by (when (hash-table-p tool-input)
                   (let ((sort-str (gethash "sort_by" tool-input)))
                     (when sort-str (intern sort-str))))))
    (condition-case err
        (let ((result (efrit-tools-format-todo-list efrit-do--current-todos sort-by)))
          (format "\n[Formatted TODO list]\n%s" result))
      (error
       (format "\n[Error formatting TODO list: %s]" (error-message-string err))))))

(defun efrit-do--handle-session-complete (tool-input)
  "Handle session_complete tool with TOOL-INPUT hash table.
This signals that a multi-step session is complete."
  (let ((message (gethash "message" tool-input)))
    ;; Return a special marker that the async handler can detect
    (format "\n[SESSION-COMPLETE: %s]" (or message "Task completed"))))

(defun efrit-do--handle-display-in-buffer (tool-input input-str)
  "Handle display in buffer."
  (let* ((buffer-name (if (hash-table-p tool-input)
                         (gethash "buffer_name" tool-input)
                       "*efrit-display*"))
         (content (if (hash-table-p tool-input)
                     (gethash "content" tool-input)
                   input-str))
         (height (when (hash-table-p tool-input)
                   (gethash "window_height" tool-input))))
    (condition-case err
        (let ((result (efrit-tools-display-in-buffer buffer-name content height)))
          (format "\n[%s]" result))
      (error
       (format "\n[Error displaying in buffer: %s]" (error-message-string err))))))

(defun efrit-do--execute-tool (tool-item)
  "Execute a tool specified by TOOL-ITEM hash table.
TOOL-ITEM should contain \\='name\\=' and \\='input\\=' keys.
Returns a formatted string with execution results or empty string on failure."
  (let* ((tool-name (gethash "name" tool-item))
         (tool-input (gethash "input" tool-item))
         (input-str (cond
                     ((stringp tool-input) tool-input)
                     ((hash-table-p tool-input) 
                      (seq-some (lambda (key) (gethash key tool-input))
                                '("expr" "expression" "code" "command")))
                     (t (format "%S" tool-input)))))
    
    ;; Sanitize potentially over-escaped strings
    (when input-str
      (setq input-str (efrit-do--sanitize-elisp-string input-str)))
    
    (efrit-log 'debug "Tool use: %s with input: %S (extracted: %S)" 
               tool-name tool-input input-str)
    
    ;; Dispatch to appropriate handler
    (cond
     ((and (string= tool-name "eval_sexp") input-str)
      (efrit-do--handle-eval-sexp input-str))
     ((and (string= tool-name "shell_exec") input-str)
      (efrit-do--handle-shell-exec input-str))
     ((string= tool-name "todo_add")
      (efrit-do--handle-todo-add tool-input))
     ((string= tool-name "todo_update")
      (efrit-do--handle-todo-update tool-input input-str))
     ((string= tool-name "todo_show")
      (efrit-do--handle-todo-show))
     ((string= tool-name "buffer_create")
      (efrit-do--handle-buffer-create tool-input input-str))
     ((string= tool-name "format_file_list")
      (efrit-do--handle-format-file-list input-str))
     ((string= tool-name "format_todo_list")
      (efrit-do--handle-format-todo-list tool-input))
     ((string= tool-name "display_in_buffer")
      (efrit-do--handle-display-in-buffer tool-input input-str))
     ((string= tool-name "session_complete")
      (efrit-do--handle-session-complete tool-input))
     (t 
      (efrit-log 'warn "Unknown tool: %s with input: %S" tool-name tool-input)
      (format "\n[Unknown tool: %s]" tool-name)))))
;;; Command execution

(defun efrit-do--command-examples ()
  "Return examples section for command system prompt."
  (concat
   "Examples:\n\n"
   
   "User: show me untracked files in ~/.emacs.d/\n"
   "Assistant: I'll find untracked files and create a report.\n"
   "Tool call: shell_exec with command: \"cd ~/.emacs.d && find . -name '*.el' -not -path './.git/*'\"\n"
   "Tool call: format_file_list with content: \"[shell output]\"\n"
   "Tool call: buffer_create with name: \"*efrit-report: Untracked Files*\", content: \"[formatted list]\", mode: \"markdown-mode\"\n\n"
   
   "User: open dired to my downloads folder\n"
   "Assistant: I'll open dired for your downloads folder.\n"
   "Tool call: eval_sexp with expr: \"(dired (expand-file-name \\\"~/Downloads/\\\"))\"\n\n"
   
   "User: split window and show scratch buffer\n"
   "Assistant: I'll split the window and show the scratch buffer.\n"
   "Tool call: eval_sexp with expr: \"(progn (split-window-horizontally) (other-window 1) (switch-to-buffer \\\"*scratch*\\\"))\"\n\n"
   
   "User: save all buffers\n"
   "Assistant: I'll save all modified buffers.\n"
   "Tool call: eval_sexp with expr: \"(save-some-buffers t)\"\n\n"
   
   "User: wrap the text to 2500 columns\n"
   "Assistant: I'll wrap the text to 2500 columns.\n"
   "Tool call: eval_sexp with expr: \"(let ((fill-column 2500)) (fill-region (point-min) (point-max)))\"\n\n"))

(defun efrit-do--command-formatting-tools ()
  "Return formatting tools documentation for command system prompt."
  (concat
   "FORMATTING AND DISPLAY TOOLS:\n"
   "- buffer_create: Create dedicated buffers for reports, lists, analysis (specify name, content, mode)\n"
   "- format_file_list: Format raw text as markdown file lists with bullet points\n"
   "- format_todo_list: Format TODOs with optional sorting ('status', 'priority', or none)\n"
   "- display_in_buffer: Display content in specific buffers with custom window height\n\n"))

(defun efrit-do--command-common-tasks ()
  "Return common tasks section for command system prompt."
  (concat
   "Common tasks:\n"
   "- Font scaling: (global-text-scale-adjust 2) or (text-scale-adjust 2)\n"
   "- Buffer switching: (switch-to-buffer \"*Messages*\")\n"
   "- Window operations: (split-window-horizontally), (other-window 1)\n"
   "- Text wrapping: (let ((fill-column N)) (fill-region (point-min) (point-max)))\n"
   "- Sorting lines: (sort-lines nil (point-min) (point-max))\n"
   "- Case changes: (upcase-region (point-min) (point-max))\n\n"))

(defun efrit-do--command-system-prompt (&optional retry-count error-msg previous-code session-id work-log)
  "Generate system prompt for command execution with optional context.
Uses previous command context if available. If RETRY-COUNT is provided,
include retry-specific instructions with ERROR-MSG and PREVIOUS-CODE.
If SESSION-ID is provided, include session continuation protocol with WORK-LOG."
  (let ((context-info (when efrit-do--last-result
                        (let ((recent-items (efrit-do--get-context-items 1)))
                          (when recent-items
                            (let ((item (car recent-items)))
                              (format "\n\nPREVIOUS CONTEXT:\nLast command: %s\nLast result: %s\n\n"
                                      (efrit-do-context-item-command item)
                                      (efrit-do-context-item-result item)))))))
        (retry-info (when retry-count
                      (let ((rich-context (condition-case err
                                              (efrit-do--build-error-context)
                                            (error 
                                             (format "Error building context: %s" 
                                                     (error-message-string err))))))
                        (format "\n\nRETRY ATTEMPT %d/%d:\nPrevious code that failed: %s\nError encountered: %s\n\nCURRENT EMACS STATE:\n%s\n\nPlease analyze the error and current state to provide a corrected solution.\n\n"
                                retry-count efrit-do-max-retries 
                                (or previous-code "Unknown") 
                                (or error-msg "Unknown error")
                                rich-context))))
        (session-info (when session-id
                       (format "\n\nSESSION MODE ACTIVE:\nSession ID: %s\nWork Log: %s\n\n%s\n\n"
                              session-id
                              (or work-log "[]")
                              (efrit-do--session-protocol-instructions)))))
    (concat "You are Efrit, an AI assistant that executes natural language commands in Emacs.\n\n"
          
          (if session-id
              "IMPORTANT: You are in SESSION MODE. Follow the session protocol for multi-step execution.\n\n"
            "IMPORTANT: You are in COMMAND MODE. This means:\n")
          
          "- Generate valid Elisp code to accomplish the user's request\n"
          "- Use the eval_sexp tool for Emacs operations and shell_exec tool for shell commands\n"
          "- CRITICAL: When user asks to 'show', 'list', 'display' or create any kind of report, you MUST use buffer_create tool to create a dedicated buffer\n"
          "- FOR REPORTS/LISTS: ALWAYS use buffer_create to make dedicated buffers with appropriate formatting\n"
          "- FOR FILE LISTS: Use format_file_list to format paths as markdown lists\n"  
          "- USE TODO MANAGEMENT: For complex tasks, break them into smaller TODO items using todo_add, todo_update, and todo_show\n"
          "- TRACK PROGRESS: Mark TODOs as 'in-progress' when starting work, 'completed' when done\n"
          "- DO NOT explain what you're doing unless asked\n"
          "- DO NOT ask for clarification - make reasonable assumptions\n"
          "- ONLY use documented Emacs functions - NEVER invent function names\n"
          "- If unsure about a function, use simpler approaches or multiple steps\n"
          "- For file paths, always use expand-file-name to handle ~ expansion\n"
          "- Be concise in responses\n"
          "- If user says 'that didn't work' or similar, examine the previous command/result to debug\n\n"
          
          "BUFFER OPERATIONS GUIDANCE:\n"
          "- When user says 'the text' or 'the buffer', operate on entire buffer (point-min) to (point-max)\n"
          "- Use temporary bindings (let) for settings when possible - preserve user's original settings\n"
          "- Only operate on current paragraph/region if explicitly specified\n"
          "- For buffer-wide operations, prefer whole-buffer functions\n\n"
          
          (efrit-do--command-common-tasks)
          (efrit-do--command-formatting-tools)
          (efrit-do--command-examples)
          
          session-info
          
          "Remember: Generate safe, valid Elisp and execute immediately."
          (or context-info "")
          (or retry-info "")
          (efrit-do--format-todos-for-prompt))))

(defun efrit-do--format-result (command result)
  "Format COMMAND and RESULT for display."
  (with-temp-buffer
    (insert (format "Command: %s\n" command))
    (insert (make-string 60 ?-) "\n")
    (insert result)
    (buffer-string)))



(defun efrit-do--display-result (command result &optional error-p)
  "Display COMMAND and RESULT in the results buffer.
If ERROR-P is non-nil, this indicates an error result.
When `efrit-do-show-errors-only' is non-nil, only show buffer for errors."
  (when efrit-do-show-results
    ;; Always use standard results buffer - no smart detection
    (with-current-buffer (get-buffer-create efrit-do-buffer-name)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (unless (bobp)
          (insert "\n\n"))
        (insert (efrit-do--format-result command result)))
      ;; Only display buffer if not in errors-only mode, or if this is an error
      (when (or (not efrit-do-show-errors-only) error-p)
        (display-buffer (current-buffer) 
                        '(display-buffer-reuse-window
                          display-buffer-below-selected
                          (window-height . 10)))))))

(defun efrit-do--execute-command (command &optional retry-count error-msg previous-code)
  "Execute natural language COMMAND and return the result.
Uses improved error handling. If RETRY-COUNT is provided, this is a retry 
attempt with ERROR-MSG and PREVIOUS-CODE from the failed attempt."
  (condition-case api-err
      (let* ((api-key (efrit-common-get-api-key))
             (url-request-method "POST")
             (url-request-extra-headers
              `(("x-api-key" . ,api-key)
                ("anthropic-version" . "2023-06-01")
                ("content-type" . "application/json")))
             (system-prompt (efrit-do--command-system-prompt retry-count error-msg previous-code))
             (request-data
              `(("model" . ,efrit-model)
                ("max_tokens" . ,efrit-max-tokens)
                ("temperature" . 0.0)
                ("messages" . [(("role" . "user")
                               ("content" . ,command))])
                ("system" . ,system-prompt)
                ("tools" . ,efrit-do--tools-schema)))
             (json-string (json-encode request-data))
             ;; Convert unicode characters to JSON escape sequences to prevent multibyte HTTP errors
              (escaped-json (efrit-common-escape-json-unicode json-string))
              (url-request-data (encode-coding-string escaped-json 'utf-8)))
        
        (if-let* ((response-buffer (url-retrieve-synchronously efrit-api-url))
                  (response-text (efrit-do--extract-response-text response-buffer)))
            (efrit-do--process-api-response response-text)
          (error "Failed to get response from API")))
    (error
     (format "API error: %s" (error-message-string api-err)))))

(defun efrit-do--process-api-response (response-text)
  "Process API RESPONSE-TEXT and execute any tools."
  (condition-case json-err
      (let* ((json-object-type 'hash-table)
             (json-array-type 'vector)
             (json-key-type 'string)
             (response (json-read-from-string response-text))
             (message-text ""))
        
        (efrit-log 'debug "Raw API Response: %s" response-text)
        (efrit-log 'debug "Parsed response: %S" response)
        
        ;; Check for API errors first
        (if-let* ((error-obj (gethash "error" response)))
            (let ((error-type (gethash "type" error-obj))
                  (error-message (gethash "message" error-obj)))
              (format "API Error (%s): %s" error-type error-message))
          
          ;; Process successful response
          (let ((content (gethash "content" response)))
            (when efrit-do-debug
              (message "API Response content: %S" content))
            
            (when content
              (dotimes (i (length content))
                (let* ((item (aref content i))
                       (type (gethash "type" item)))
                  (cond
                   ;; Handle text content
                   ((string= type "text")
                    (when-let* ((text (gethash "text" item)))
                      (setq message-text (concat message-text text))))
                   
                   ;; Handle tool use
                   ((string= type "tool_use")
                    (setq message-text 
                          (concat message-text 
                                  (efrit-do--execute-tool item))))))))
            
            (or message-text "Command executed"))))
    (error
     (format "JSON parsing error: %s" (error-message-string json-err)))))

(defun efrit-do--execute-with-retry (command)
  "Execute COMMAND with retry logic if enabled.
Returns the final result after all retry attempts."
  (let ((attempt 0)
        (max-attempts (if efrit-do-retry-on-errors (1+ efrit-do-max-retries) 1))
        result error-info last-error last-code final-result)
    
    (while (and (< attempt max-attempts) (not final-result))
      (setq attempt (1+ attempt))
      
      (when (> attempt 1)
        (message "Retry attempt %d/%d..." (1- attempt) efrit-do-max-retries))
      
      (condition-case err
          (progn
            (setq result (if (= attempt 1)
                            (efrit-do--execute-command command)
                          (efrit-do--execute-command command attempt last-error last-code)))
            
            (if result
                ;; Check if result contains errors
                (progn
                  (setq error-info (efrit-do--extract-error-info result))
                  (if (and (car error-info) efrit-do-retry-on-errors (< attempt max-attempts))
                      ;; Error detected and retries available
                      (progn
                        (setq last-error (cdr error-info))
                        (setq last-code (efrit-do--extract-executed-code result))
                        (efrit-log 'debug "Error detected: %s. Will retry..." last-error))
                    ;; Success or no more retries
                    (setq final-result result)))
              ;; No result - treat as error
              (let ((no-result-error "No result returned from command execution"))
                (if (and efrit-do-retry-on-errors (< attempt max-attempts))
                    (setq last-error no-result-error)
                  (setq final-result no-result-error)))))
        (error
         ;; Handle API or system errors
         (let ((error-msg (format "Error: %s" (error-message-string err))))
           (if (and efrit-do-retry-on-errors (< attempt max-attempts))
               (setq last-error error-msg)
             (setq final-result error-msg))))))
    
    (cons final-result attempt)))

(defun efrit-do--process-result (command result attempt)
  "Process the final RESULT from executing COMMAND after ATTEMPT attempts."
  (if result
      (let* ((error-info (efrit-do--extract-error-info result))
             (is-error (car error-info)))
        (setq efrit-do--last-result result)
        (efrit-do--capture-context command result)
        (efrit-do--display-result command result is-error)
        
        (if is-error
            (progn
              (message "Command failed after %d attempt%s" 
                      attempt (if (> attempt 1) "s" ""))
              (user-error "%s" (cdr error-info)))
          (message "Command executed successfully%s" 
                  (if (> attempt 1) 
                      (format " (after %d attempts)" attempt) 
                      ""))))
    ;; Should never reach here, but handle just in case
    (let ((fallback-error "Failed to execute command"))
      (setq efrit-do--last-result fallback-error)
      (efrit-do--display-result command fallback-error t)
      (user-error "%s" fallback-error))))

;;;###autoload
(defun efrit-do-async (command)
  "Execute natural language COMMAND in Emacs asynchronously.
The command is sent to Claude using async infrastructure, providing
non-blocking execution with progress feedback. Results are displayed
when the command completes."
  (interactive
   (list (read-string "Command (async): " nil 'efrit-do-history)))
  
  ;; Add to history
  (add-to-history 'efrit-do-history command efrit-do-history-max)
  
  ;; Execute asynchronously
  (require 'efrit-async)
  (message "Executing asynchronously: %s..." command)
  (efrit-async-execute-command 
   command
   (lambda (result)
     (let* ((error-info (efrit-do--extract-error-info result))
            (is-error (car error-info)))
       ;; Store result and update context
       (setq efrit-do--last-result result)
       (efrit-do--capture-context command result)
       (efrit-do--display-result command result is-error)
       
       ;; Show completion message
       (if is-error
           (message "Async command failed: %s" (cdr error-info))
         (message "Async command completed successfully"))))))

;;;###autoload
(defun efrit-do (command)
  "Execute natural language COMMAND in Emacs.
The command is sent to Claude, which translates it into Elisp
and executes it immediately. Results are displayed in a dedicated
buffer if `efrit-do-show-results' is non-nil.

If retry is enabled and errors occur, automatically retry by sending 
error details back to Claude for correction."
  (interactive
   (list (read-string "Command: " nil 'efrit-do-history)))
  
  ;; Add to history
  (add-to-history 'efrit-do-history command efrit-do-history-max)
  
  ;; Execute with retry logic
  (message "Executing: %s..." command)
  (let* ((result-and-attempt (efrit-do--execute-with-retry command))
         (final-result (car result-and-attempt))
         (attempt (cdr result-and-attempt)))
    (efrit-do--process-result command final-result attempt)))

;;;###autoload
(defun efrit-do-repeat ()
  "Repeat the last efrit-do command."
  (interactive)
  (if efrit-do-history
      (efrit-do (car efrit-do-history))
    (message "No previous command to repeat")))

;;;###autoload
(defun efrit-do-clear-results ()
  "Clear the efrit-do results buffer."
  (interactive)
  (if-let* ((buffer (get-buffer efrit-do-buffer-name)))
      (progn
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (erase-buffer)))
        (message "Results buffer cleared"))
    (message "No results buffer to clear")))

;;; Context system user commands

;;;###autoload
(defun efrit-do-show-context ()
  "Show recent context items."
  (interactive)
  (let ((items (efrit-do--get-context-items)))
    (if items
        (with-output-to-temp-buffer "*efrit-do-context*"
          (princ "Recent efrit-do context:\n\n")
          (dolist (item items)
            (princ (efrit-do--context-to-string item))
            (princ "\n")))
      (message "No context items found"))))

;;;###autoload
(defun efrit-do-clear-context ()
  "Clear the context ring."
  (interactive)
  (efrit-do--clear-context)
  (message "Context cleared"))

;;;###autoload
(defun efrit-do-clear-history ()
  "Clear efrit-do command history and context."
  (interactive)
  (setq efrit-do-history nil)
  (efrit-do--clear-context)
  (setq efrit-do--last-result nil)
  (message "History and context cleared"))

;;;###autoload
(defun efrit-do-show-todos ()
  "Show current TODO items."
  (interactive)
  (with-output-to-temp-buffer "*efrit-do-todos*"
    (princ "Current efrit-do TODOs:\n\n")
    (princ (efrit-do--format-todos-for-display))
    (princ "\n\nUse efrit-do-clear-todos to clear all TODOs.")))

;;;###autoload
(defun efrit-do-clear-todos ()
  "Clear all TODO items."
  (interactive)
  (efrit-do--clear-todos)
  (message "All TODOs cleared"))

;;;###autoload
(defun efrit-do-clear-all ()
  "Clear all efrit-do state.
This includes: history, context, results buffer, TODOs, and conversations."
  (interactive)
  (setq efrit-do-history nil)
  (efrit-do--clear-context)
  (efrit-do--clear-todos)
  (setq efrit-do--last-result nil)
  
  ;; Clear results buffer if it exists
  (when-let* ((buffer (get-buffer efrit-do-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer))))
  
  ;; Clear multi-turn conversations if available
  (when (boundp 'efrit--multi-turn-conversations)
    (clrhash efrit--multi-turn-conversations))
  
  (message "All efrit-do state cleared"))

;;;###autoload
(defun efrit-do-reset ()
  "Interactive reset with options for different levels of clearing."
  (interactive)
  (let ((choice (read-char-choice 
                 "Reset efrit-do: (h)istory, (c)ontext, (r)esults, (t)odos, (a)ll state, or (q)uit? "
                 '(?h ?c ?r ?t ?a ?q))))
    (cond
     ((eq choice ?h) (setq efrit-do-history nil)
                     (setq efrit-do--last-result nil)
                     (message "Command history cleared"))
     ((eq choice ?c) (efrit-do-clear-context))
     ((eq choice ?r) (efrit-do-clear-results))
     ((eq choice ?t) (efrit-do-clear-todos))
     ((eq choice ?a) (efrit-do-clear-all))
     ((eq choice ?q) (message "Reset cancelled")))))

;;;###autoload
(defun efrit-do-to-chat (&optional n)
  "Convert recent efrit-do context to efrit-chat session.
Include last N commands (default 5)."
  (interactive "P")
  (require 'efrit-chat)
  (let* ((count (or n 5))
         (items (efrit-do--get-context-items count))
         (buffer (efrit--setup-buffer)))
    
    (if (not items)
        (message "No efrit-do context to convert")
      
      (with-current-buffer buffer
        (setq buffer-read-only nil)
        (let ((inhibit-read-only t))
          ;; Clear existing content and history
          (erase-buffer)
          (setq-local efrit--message-history nil)
          
          ;; Add context summary
          (efrit--display-message 
           (format "Chat session from %d recent efrit-do commands:" (length items))
           'system)
          
          ;; Convert each context item to conversation
          (dolist (item items) ; Show oldest first (items already in oldest-first order)
            (let ((command (efrit-do-context-item-command item))
                  (result (efrit-do-context-item-result item))
                  (timestamp (efrit-do-context-item-timestamp item)))
              
              ;; Display user command
              (efrit--display-message 
               (format "[%s] %s" 
                       (format-time-string "%H:%M:%S" timestamp)
                       command)
               'user)
              
              ;; Display result (truncated if too long)
              (efrit--display-message 
               (truncate-string-to-width result 500 nil nil t)
               'assistant)))
          
          ;; Build history in correct order for efrit-chat (newest first)
          ;; Process items in reverse order so newest ends up first
          (dolist (item items)
            (let ((command (efrit-do-context-item-command item))
                  (result (efrit-do-context-item-result item)))
              (push `((role . "assistant") (content . ,result)) efrit--message-history)
              (push `((role . "user") (content . ,command)) efrit--message-history)))
          
          ;; Insert prompt for new input
          (efrit--insert-prompt)))
      
      ;; Switch to the chat buffer
      (switch-to-buffer buffer)
      (message "Converted %d efrit-do commands to chat session" (length items)))))

;;; Initialization

(defun efrit-do--initialize ()
  "Initialize the efrit-do context system."
  (efrit-do--load-context)
  (add-hook 'kill-emacs-hook #'efrit-do--save-context))

(defun efrit-do--uninitialize ()
  "Uninitialize the efrit-do context system."
  (efrit-do--save-context)
  (remove-hook 'kill-emacs-hook #'efrit-do--save-context))

(add-hook 'after-init-hook #'efrit-do--initialize)

(provide 'efrit-do)

;;; efrit-do.el ends here
