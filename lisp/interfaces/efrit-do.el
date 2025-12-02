;;; efrit-do.el --- Execute natural language commands in Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.4.1
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
(declare-function efrit--build-headers "efrit-chat")

;; Declare external functions from efrit-executor
(declare-function efrit-execute "efrit-executor")
(declare-function efrit-execute-async "efrit-executor")

;; Declare external functions from efrit-tool-confirm-action
(declare-function efrit-tool-confirm-action "efrit-tool-confirm-action")

;; Declare external functions from efrit-tool-checkpoint
(declare-function efrit-tool-checkpoint "efrit-tool-checkpoint")
(declare-function efrit-tool-restore-checkpoint "efrit-tool-checkpoint")
(declare-function efrit-tool-list-checkpoints "efrit-tool-checkpoint")
(declare-function efrit-tool-delete-checkpoint "efrit-tool-checkpoint")

;; Declare external functions from efrit-tool-show-diff-preview
(declare-function efrit-tool-show-diff-preview "efrit-tool-show-diff-preview")

;; Declare external functions from efrit-tool-web-search
(declare-function efrit-tool-web-search "efrit-tool-web-search")

;; Declare external functions from efrit-tool-fetch-url
(declare-function efrit-tool-fetch-url "efrit-tool-fetch-url")

;; Declare external functions from Phase 1/2 tools
(declare-function efrit-tool-project-files "efrit-tool-project-files")
(declare-function efrit-tool-search-content "efrit-tool-search-content")
(declare-function efrit-tool-read-file "efrit-tool-read-file")
(declare-function efrit-tool-file-info "efrit-tool-file-info")
(declare-function efrit-tool-vcs-status "efrit-tool-vcs-status")
(declare-function efrit-tool-vcs-diff "efrit-tool-vcs-diff")
(declare-function efrit-tool-vcs-log "efrit-tool-vcs-log")
(declare-function efrit-tool-vcs-blame "efrit-tool-vcs-blame")
(declare-function efrit-tool-elisp-docs "efrit-tool-elisp-docs")

;; Declare external functions from efrit-tool-utils for project root management
(declare-function efrit-tool--get-project-root "efrit-tool-utils")
(declare-function efrit-set-project-root "efrit-tool-utils")
(declare-function efrit-clear-project-root "efrit-tool-utils")
(declare-function efrit-tool--format-agent-instructions-for-prompt "efrit-tool-utils")
(defvar efrit-project-root)

(require 'efrit-tools)
(require 'efrit-config)
(require 'efrit-common)
(require 'efrit-session)
(require 'efrit-chat)
(require 'efrit-progress)
(require 'efrit-tool-utils)
(require 'efrit-do-circuit-breaker)
(require 'efrit-do-schema)
(require 'efrit-do-handlers)
(require 'efrit-do-async-loop)
(require 'efrit-do-queue)
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

(defcustom efrit-do-auto-shrink-todo-buffers t
  "When non-nil, TODO display buffers automatically shrink to fit content.
This affects the TODO display buffer."
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

;; Circuit breaker customizations moved to efrit-do-circuit-breaker.el

(defcustom efrit-do-show-tool-execution t
  "Whether to show feedback when tools are executed.
When non-nil, displays messages like \\='Executing tool eval_sexp...\\=' during
command execution.  This provides visibility into what operations are
being performed."
  :type 'boolean
  :group 'efrit-do)

(defcustom efrit-do-max-buffer-lines 1000
  "Maximum number of lines to keep in the results buffer.
When the buffer exceeds this limit, older results are automatically
truncated to keep only the most recent results as specified by
`efrit-do-keep-results'.  Set to 0 to disable automatic truncation."
  :type 'integer
  :group 'efrit-do)

(defcustom efrit-do-keep-results 10
  "Number of recent command results to keep when truncating the buffer.
When the results buffer exceeds `efrit-do-max-buffer-lines', it is
truncated to keep this many recent results."
  :type 'integer
  :group 'efrit-do)

;; Context configuration moved to efrit-context.el

(defcustom efrit-do-max-retries 3
  "Maximum number of retry attempts when commands fail."
  :type 'integer
  :group 'efrit-do)

(defcustom efrit-do-retry-on-errors t
  "Whether to automatically retry failed commands by sending errors back to Claude."
  :type 'boolean
  :group 'efrit-do)

;; Use centralized model configuration from efrit-config
(defvar efrit-model)  ;; Forward declaration for efrit-chat's alias

(defcustom efrit-api-channel nil
  "API channel to use. Can be \\='ai-efrit or nil for default."
  :type '(choice (const :tag "Default" nil)
                 (const :tag "AI-Efrit" "ai-efrit"))
  :group 'efrit-do)

;; efrit-max-tokens is now aliased in efrit-config.el

;; efrit-api-url is defined in efrit-common.el (legacy, deprecated)

;;; Internal variables

(defvar efrit-do-history nil
  "History of executed commands.")

(defvar efrit-do--last-result nil
  "Result of the last executed command.")

(defvar efrit-do--current-todos nil
  "Current TODO list for the active command session.")

(defvar efrit-do--todo-counter 0
  "Counter for generating unique TODO IDs.")

(defvar efrit-do--force-complete nil
  "When t, forces session completion on next API response.")

;; Circuit breaker state and error loop detection state moved to efrit-do-circuit-breaker.el
;; Tool schemas (JSON definitions for Claude) moved to efrit-do-schema.el

;;; Tool Dispatch Table
;; Maps tool names to (HANDLER-FN . ARG-TYPE) where ARG-TYPE is:
;;   :input-str - handler takes extracted input string
;;   :tool-input - handler takes raw tool-input hash table
;;   :both - handler takes (tool-input, input-str)
;;   :none - handler takes no arguments
(defconst efrit-do--tool-dispatch-table
  '(;; Core execution tools
    ("eval_sexp"          . (efrit-do--handle-eval-sexp . :input-str))
    ("shell_exec"         . (efrit-do--handle-shell-exec . :input-str))
    ;; TODO management tool
    ("todo_write" . (efrit-do--handle-todo-write . :tool-input))
    ;; Buffer and display tools
    ("buffer_create"      . (efrit-do--handle-buffer-create . :both))
    ("format_file_list"   . (efrit-do--handle-format-file-list . :input-str))
    ("format_todo_list"   . (efrit-do--handle-format-todo-list . :tool-input))
    ("display_in_buffer"  . (efrit-do--handle-display-in-buffer . :both))
    ;; Session tools
    ("session_complete"   . (efrit-do--handle-session-complete . :tool-input))
    ("glob_files"         . (efrit-do--handle-glob-files . :tool-input))
    ("request_user_input" . (efrit-do--handle-request-user-input . :tool-input))
    ("confirm_action"     . (efrit-do--handle-confirm-action . :tool-input))
    ;; Checkpoint tools (Phase 3)
    ("checkpoint"         . (efrit-do--handle-checkpoint . :tool-input))
    ("restore_checkpoint" . (efrit-do--handle-restore-checkpoint . :tool-input))
    ("list_checkpoints"   . (efrit-do--handle-list-checkpoints . :none))
    ("delete_checkpoint"  . (efrit-do--handle-delete-checkpoint . :tool-input))
    ("show_diff_preview"  . (efrit-do--handle-show-diff-preview . :tool-input))
    ;; External knowledge tools (Phase 4)
    ("web_search"         . (efrit-do--handle-web-search . :tool-input))
    ("fetch_url"          . (efrit-do--handle-fetch-url . :tool-input))
    ;; Codebase exploration tools (Phase 1/2)
    ("project_files"      . (efrit-do--handle-project-files . :tool-input))
    ("search_content"     . (efrit-do--handle-search-content . :tool-input))
    ("read_file"          . (efrit-do--handle-read-file . :tool-input))
    ("edit_file"          . (efrit-do--handle-edit-file . :tool-input))
    ("create_file"        . (efrit-do--handle-create-file . :tool-input))
    ("file_info"          . (efrit-do--handle-file-info . :tool-input))
    ("vcs_status"         . (efrit-do--handle-vcs-status . :tool-input))
    ("vcs_diff"           . (efrit-do--handle-vcs-diff . :tool-input))
    ("vcs_log"            . (efrit-do--handle-vcs-log . :tool-input))
    ("vcs_blame"          . (efrit-do--handle-vcs-blame . :tool-input))
    ("elisp_docs"         . (efrit-do--handle-elisp-docs . :tool-input))
    ("set_project_root"   . (efrit-do--handle-set-project-root . :tool-input))
    ("get_diagnostics"    . (efrit-do--handle-get-diagnostics . :tool-input)))
  "Dispatch table mapping tool names to handlers and argument types.")

;; Budget hints, tool schemas -> efrit-do-schema.el
;; Circuit breaker, error loop detection -> efrit-do-circuit-breaker.el

;;; Context system - delegates to efrit-context.el

;;; Session protocol instructions

(defun efrit-do--session-protocol-instructions ()
  "Return detailed instructions for Claude about the session protocol."
  (concat
   "SESSION PROTOCOL:\n"
   "You are continuing a multi-step session. Your goal is to complete the task incrementally.\n\n"

   "SESSION COMPLETION:\n"
   "- When code executes successfully, call session_complete\n"
   "- If the work log shows the task was accomplished, call session_complete\n"
   "- Don't re-execute code that already succeeded\n\n"

   "TASK MANAGEMENT WITH todo_write:\n"
   "Use todo_write PROACTIVELY to give the user visibility into complex work.\n\n"
   "ALWAYS use todo_write when:\n"
   "- User explicitly lists multiple items (numbered, comma-separated, bulleted)\n"
   "- Task involves iteration ('fix all X', 'update each Y', 'for every Z')\n"
   "- Task has investigation + implementation phases\n"
   "- Work will span multiple API turns\n"
   "- You need to track what's done vs remaining\n\n"
   "PREFER todo_write for visibility over batching for efficiency when:\n"
   "- The user can see progress on long-running tasks\n"
   "- Breaking work into steps helps verify each step succeeded\n"
   "- The task involves different types of operations (read, modify, verify)\n\n"
   "OK to batch without todo_write when:\n"
   "- All operations are the same type and can be combined in one expression\n"
   "- Total work is trivially simple (< 3 seconds of API time)\n"
   "- Operations are pure read-only with no side effects\n\n"
   "WORKFLOW:\n"
   "- Each update replaces the ENTIRE list - always include all tasks\n"
   "- Keep exactly ONE task as in_progress at a time\n"
   "- Mark tasks completed immediately after finishing\n"
   "- When all tasks are completed, call session_complete\n\n"

   "WORK LOG:\n"
   "- The work log shows previous steps: [[\"result1\", \"code1\"], ...]\n"
   "- Use this to understand what's done and what remains\n\n"

   "EXECUTION:\n"
   "- BATCH MULTIPLE TOOL CALLS when they are independent (e.g., reading several files)\n"
   "- Use eval_sexp for Emacs operations, shell_exec for shell commands\n"
   "- Keep responses minimal - focus on execution\n"
   "- Each API continuation has a cost - be efficient by combining independent operations\n\n"

   "ERROR HANDLING:\n"
   "- If code fails, analyze the error and try a different approach\n"
   "- Use (describe-function 'name) to learn correct usage\n"
   "- Don't retry the same failing pattern more than twice\n"))

;;; Context ring management

(defun efrit-do--ensure-context-ring ()
  "Ensure the context system is initialized."
  (efrit-context-init))

(defun efrit-do--capture-context (command result)
  "Capture current context after executing COMMAND with RESULT."
  (efrit-do--ensure-context-ring)
  (let ((metadata (list :point (point)
                       :mark (mark)
                       :major-mode major-mode)))
    (efrit-context-ring-add command result metadata)))

(defun efrit-do--get-context-items (&optional n)
  "Get N most recent context items (default all)."
  (efrit-do--ensure-context-ring)
  (efrit-context-ring-get-recent n))

(defun efrit-do--context-to-string (item)
  "Convert context ITEM to string representation."
  (efrit-context-item-to-string item))

(defun efrit-do--clear-context ()
  "Clear the context ring."
  (efrit-context-ring-clear))

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

(defun efrit-do--find-todo (id)
  "Find TODO item by ID."
  (seq-find (lambda (item) 
              (string= (efrit-do-todo-item-id item) id))
           efrit-do--current-todos))

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
  "Clear all current TODOs and reset circuit breaker for new session."
  (setq efrit-do--current-todos nil)
  (setq efrit-do--todo-counter 0)
  ;; Reset circuit breaker for new session
  (efrit-do--circuit-breaker-reset))

;;; Context persistence

(defun efrit-do--save-context ()
  "Save context ring to file."
  (efrit-context-ring-persist))

(defun efrit-do--load-context ()
  "Load context ring from file."
  (efrit-context-ring-restore))

;;; Helper functions for improved error handling
;; Note: efrit-do--validate-elisp moved to efrit-do-handlers.el

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
                      (efrit-context-item-command item)
                      (truncate-string-to-width 
                       (or (efrit-context-item-result item) "no result") 
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


;;; Tool handlers moved to efrit-do-handlers.el

(defun efrit-do--dispatch-tool (tool-name tool-input input-str)
  "Dispatch TOOL-NAME to its handler with appropriate arguments.
TOOL-INPUT is the raw hash table, INPUT-STR is the extracted string.
Uses `efrit-do--tool-dispatch-table' for lookup."
  (if-let* ((entry (assoc tool-name efrit-do--tool-dispatch-table))
            (handler-info (cdr entry))
            (handler (car handler-info))
            (arg-type (cdr handler-info)))
      (pcase arg-type
        (:input-str
         (if input-str
             (funcall handler input-str)
           (format "\n[Error: %s requires input parameter]" tool-name)))
        (:tool-input
         (funcall handler tool-input))
        (:both
         (funcall handler tool-input input-str))
        (:none
         (funcall handler))
        (_
         (efrit-log 'error "Invalid arg-type %s for tool %s" arg-type tool-name)
         (format "\n[Internal error: invalid dispatch for %s]" tool-name)))
    ;; Unknown tool
    (progn
      (efrit-log 'warn "Unknown tool: %s with input: %S" tool-name tool-input)
      (format "\n[Unknown tool: %s]" tool-name))))

(defun efrit-do--execute-tool (tool-item)
  "Execute a tool specified by TOOL-ITEM hash table.
TOOL-ITEM should contain \\='name\\=' and \\='input\\=' keys.
Returns a formatted string with execution results or empty string on failure.
Applies circuit breaker limits to prevent infinite loops."
  (if (null tool-item)
      "\n[Error: Tool input cannot be nil]"
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

      ;; Show user-visible feedback for tool execution if enabled
      (when efrit-do-show-tool-execution
        (message "Efrit: Executing tool '%s'..." tool-name))

      ;; CIRCUIT BREAKER: Check limits before executing
      (let ((breaker-check (efrit-do--circuit-breaker-check-limits tool-name tool-input)))
        (if (not (car breaker-check))
            ;; Circuit breaker blocked execution
            (progn
              (efrit-log 'error "Circuit breaker blocked tool: %s" tool-name)
              (when (fboundp 'efrit-session-track-error)
                (efrit-session-track-error (format "Circuit breaker: %s" (cdr breaker-check))))
              (cdr breaker-check))

          ;; Circuit breaker allows execution - record the call
          (efrit-do--circuit-breaker-record-call tool-name tool-input)

          ;; If there's a warning message, prepend it to the result
          (let* ((warning (cdr breaker-check))
                 (result (efrit-do--dispatch-tool tool-name tool-input input-str))
                 ;; Check result for error loops and inject warnings if needed
                 (loop-check (efrit-do--error-loop-check-result result))
                 (final-result (cdr loop-check))
                 (return-value (if warning
                                   (concat "\n" warning "\n" final-result)
                                 final-result)))
            ;; Log tool call to active session's work_log (if session exists)
            (when-let* ((session (efrit-session-active)))
              (efrit-session-add-work session
                                      return-value
                                      (format "(%s %S)" tool-name tool-input)
                                      nil  ; no todo-snapshot
                                      tool-name))
            return-value))))))

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
   "Tool call: eval_sexp with expr: \"(let ((fill-column 2500)) (fill-region (point-min) (point-max)))\"\n\n"
   
   "User: fix warnings in *Warnings* buffer\n"
   "Assistant: I'll fix the warnings systematically.\n"
   "Tool call: eval_sexp with expr: \"(with-current-buffer \\\"*Warnings*\\\" (buffer-string))\"\n"
   "[Response: Warning text showing 3 issues...]\n"
   "Tool call: todo_write with todos: [{content: \"Fix lexical-binding in file1.el\", status: \"in_progress\", activeForm: \"Fixing lexical-binding in file1.el\"}, {content: \"Fix lexical-binding in file2.el\", status: \"pending\", activeForm: \"Fixing lexical-binding in file2.el\"}, {content: \"Verify all warnings fixed\", status: \"pending\", activeForm: \"Verifying all warnings fixed\"}]\n"
   "[Then work through each task, updating todo_write to mark completed and set next in_progress...]\n\n"

   "--- MORE TODO EXAMPLES (when to use todo_write PROACTIVELY) ---\n\n"

   "User: update all the docstrings in utils.el\n"
   "Assistant: I'll update the docstrings systematically.\n"
   "Tool call: todo_write with todos: [{content: \"Read utils.el and identify functions\", status: \"in_progress\", activeForm: \"Reading utils.el\"}, {content: \"Update docstrings for each function\", status: \"pending\", activeForm: \"Updating docstrings\"}, {content: \"Verify all docstrings complete\", status: \"pending\", activeForm: \"Verifying docstrings\"}]\n"
   "[Calls eval_sexp to read file, then updates todo_write with specific function tasks...]\n\n"

   "User: install these packages: magit, projectile, company\n"
   "Assistant: I'll install each package.\n"
   "Tool call: todo_write with todos: [{content: \"Install magit\", status: \"in_progress\", activeForm: \"Installing magit\"}, {content: \"Install projectile\", status: \"pending\", activeForm: \"Installing projectile\"}, {content: \"Install company\", status: \"pending\", activeForm: \"Installing company\"}]\n"
   "[Works through each, updating status after each install...]\n\n"

   "User: refactor this function to use cl-loop\n"
   "Assistant: I'll refactor the function.\n"
   "Tool call: eval_sexp [This is a SIMPLE task - single operation, no todo_write needed]\n\n"))

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

(defun efrit-do--classify-task-complexity (command)
  "Classify COMMAND as simple or complex based on content analysis.
Returns \\='simple for single-action tasks, \\='complex for multi-step workflows."
  (let ((simple-patterns '("open" "find" "goto" "show" "display" "list" 
                           "create buffer" "insert" "delete" "replace"
                           "navigate" "search" "close" "save"))
        (complex-patterns '("fix all" "organize" "process each" "download"
                           "batch" "multiple" "series" "workflow" "pipeline"
                           "for each" "all.*and.*" "scan.*then.*")))
    (cond
     ;; Check for complex indicators
     ((cl-some (lambda (pattern) 
                 (string-match-p pattern (downcase command))) 
               complex-patterns)
      'complex)
     ;; Check for simple indicators  
     ((cl-some (lambda (pattern)
                 (string-match-p pattern (downcase command)))
               simple-patterns)
      'simple)
     ;; Default to simple for ambiguous cases
     (t 'simple))))

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
                                      (efrit-context-item-command item)
                                      (efrit-context-item-result item)))))))
        (retry-info (when retry-count
                      (let ((rich-context (condition-case err
                                              (efrit-do--build-error-context)
                                            (error 
                                             (format "Error building context: %s" 
                                                     (error-message-string err))))))
                        (format "\n\nRETRY ATTEMPT %d/%d:\nPrevious code that failed: %s\nError encountered: %s\n\nCURRENT EMACS STATE:\n%s\n\nERROR ADAPTATION REQUIRED:\n1. ANALYZE the error - what type is expected vs provided?\n2. If 'Wrong type argument', the function returns a different type than you assumed\n3. Use (describe-function 'name) to learn the ACTUAL return value format\n4. DO NOT retry similar code - try a fundamentally different approach\n5. If the same error pattern occurred before, you MUST read documentation first\n\n"
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
            (concat "IMPORTANT: You are in COMMAND MODE - INITIAL EXECUTION.\n\n"

                    "SESSION COMPLETION:\n"
                    "- After code executes successfully, call session_complete\n"
                    "- Don't re-execute code that already worked\n"
                    "- For pure questions (no Emacs operations needed), just answer and call session_complete\n\n"))
          
          "CRITICAL CONTEXT RULES:\n"
          (format "- Project root: %s%s\n"
                  (efrit-tool--get-project-root)
                  (if efrit-project-root " (explicitly set)" " (auto-detected)"))
          "- You are operating INSIDE Emacs - all operations should use Elisp unless explicitly requesting shell commands\n"
          "- When user says 'open' files, use find-file to open in Emacs buffers, NOT shell commands\n"
          "- 'Display', 'show', 'list' means create Emacs buffers, NOT terminal output\n"
          "- 'Edit', 'modify', 'change' means buffer operations, NOT external editors\n"
          "- SIMPLE TASKS (1-2 tool calls): Use eval_sexp directly\n"
          "- COMPLEX TASKS (3+ steps OR user lists multiple items): Use todo_write FIRST\n"
          "- PROACTIVE RULE: When user explicitly lists items (numbered, comma-separated), ALWAYS use todo_write\n"
          "- TASK CLASSIFICATION: Most 'open X files' requests are SIMPLE - use eval_sexp with directory-files-recursively\n"
          "- If project_files or search_content returns results from the wrong directory, use set_project_root first\n\n"
          
          "TOOL SELECTION GUIDE:\n"
          "- eval_sexp: PRIMARY TOOL for Emacs operations (open files, edit buffers, navigate, define functions, etc.)\n"
          "- shell_exec: ONLY when explicitly asking for shell/terminal operations\n"
          "- buffer_create: ONLY for read-only reports, lists, and formatted output display\n"
          "  * NEVER use buffer_create for code that needs to be evaluated/executed\n"
          "  * For code generation: use eval_sexp with (with-current-buffer... (insert...)) then evaluate\n"
          "- todo_write: PROACTIVELY use for multi-step tasks - call FIRST with plan, update as you progress\n\n"

          "CODE GENERATION vs DISPLAY:\n"
          "- When user asks to WRITE CODE or DEFINE FUNCTIONS: Use eval_sexp to insert into buffer AND evaluate\n"
          "- When user asks to SHOW/DISPLAY RESULTS: Use buffer_create for formatted output\n"
          "- Example: 'write fibonacci function' -> Use eval_sexp to (defun fib ...)\n"
          "- Example: 'show me all buffers' -> Use buffer_create with (buffer-list) results\n\n"

          "NAMING CONVENTIONS:\n"
          "- When user specifies a function name exactly, use that EXACT name\n"
          "- When creating code for a file like 'foo-bar.el', use 'foo-bar-' prefix for functions\n"
          "- Example: 'efrit-utils.el with word-count' -> name it 'efrit-utils-word-count' or 'efrit-common-count-words'\n"
          "- Follow Emacs Lisp conventions: package-prefix-descriptive-name\n"
          "- NEVER ignore naming guidance from the user or implied by file location\n\n"

          "EXECUTION RULES:\n"
          "- Generate valid Elisp code to accomplish the user's request\n"
          "- When user asks to 'show', 'list', 'display' - use buffer_create for formatted output\n"
          "- FOR FILE LISTS: Use format_file_list to format paths as markdown lists\n"
          "- For complex tasks (3+ steps): Use todo_write FIRST to show plan, then execute\n"
          "- IMPORTANT: Call todo_write BEFORE you start work, not after\n"
          "- Mark tasks in_progress when starting, completed immediately when done\n"
          "- DO NOT explain what you're doing unless asked\n"
          "- DO NOT ask for clarification - make reasonable assumptions\n"
          "- ONLY use documented Emacs functions - NEVER invent function names\n"
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
          
          ;; Include project-specific agent instructions (AGENTS.md/CLAUDE.md)
          (efrit-tool--format-agent-instructions-for-prompt)
          
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

(defun efrit-do--truncate-results-buffer ()
  "Truncate results buffer to keep only recent results within size limits.
Keeps the last `efrit-do-keep-results' command results when the buffer
exceeds `efrit-do-max-buffer-lines' lines."
  (when (and (> efrit-do-max-buffer-lines 0)
             (> (count-lines (point-min) (point-max)) efrit-do-max-buffer-lines))
    (save-excursion
      (goto-char (point-min))
      ;; Find boundaries of results by looking for "Command: " markers
      (let ((result-positions nil))
        ;; Collect positions of all result boundaries
        (while (re-search-forward "^Command: " nil t)
          (push (line-beginning-position) result-positions))
        (setq result-positions (nreverse result-positions))

        ;; Keep only the last N results
        (when (> (length result-positions) efrit-do-keep-results)
          (let* ((delete-up-to (nth (- (length result-positions)
                                      efrit-do-keep-results)
                                   result-positions))
                 (inhibit-read-only t))
            (delete-region (point-min) delete-up-to)
            (goto-char (point-min))
            (insert (format "[... %d older result%s truncated ...]\n\n"
                           (- (length result-positions) efrit-do-keep-results)
                           (if (> (- (length result-positions) efrit-do-keep-results) 1)
                               "s" "")))))))))

(defun efrit-do--display-result (command result &optional error-p)
  "Display COMMAND and RESULT in the results buffer.
If ERROR-P is non-nil, this indicates an error result.
When `efrit-do-show-errors-only' is non-nil, only show buffer for errors."
  (when error-p
    (require 'efrit-log)
    (efrit-log-error "[efrit-do] Command failed: %s\nResult: %s" command result))
  (when efrit-do-show-results
    ;; Always use standard results buffer - no smart detection
    (with-current-buffer (get-buffer-create efrit-do-buffer-name)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (unless (bobp)
          (insert "\n\n"))
        (insert (efrit-do--format-result command result))
        ;; Auto-truncate if buffer is too large
        (efrit-do--truncate-results-buffer))
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
             (url-request-extra-headers (efrit--build-headers api-key))
             (system-prompt (efrit-do--command-system-prompt retry-count error-msg previous-code))
             (request-data
              `(("model" . ,efrit-default-model)
                ("max_tokens" . ,efrit-max-tokens)
                ("temperature" . 0.0)
                ("messages" . [(("role" . "user")
                               ("content" . ,command))])
                ("system" . ,system-prompt)
                ("tools" . ,(efrit-do--get-current-tools-schema))))
             (json-string (json-encode request-data))
             ;; Convert unicode characters to JSON escape sequences to prevent multibyte HTTP errors
              (escaped-json (efrit-common-escape-json-unicode json-string))
              (url-request-data (encode-coding-string escaped-json 'utf-8)))
        
        (if-let* ((api-url (or efrit-api-url (efrit-common-get-api-url)))
                  (response-buffer (with-local-quit
                                     (url-retrieve-synchronously api-url)))
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

(defun efrit-do--start-progress-timer (start-time _command)
  "Start a timer to show progress feedback during command execution.
START-TIME is when the command started.
_COMMAND is a short description (currently unused)."
  (run-at-time 1 1
               (lambda ()
                 (let ((elapsed (float-time (time-since start-time))))
                   (message "Efrit: Executing (%.1fs)..." elapsed)))))

(defun efrit-do--process-result (command result attempt)
  "Process the final RESULT from executing COMMAND after ATTEMPT attempts.
Returns RESULT for programmatic use."
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
                      ""))
          ;; Return the result for programmatic use
          result))
    ;; Should never reach here, but handle just in case
    (let ((fallback-error "Failed to execute command"))
      (setq efrit-do--last-result fallback-error)
      (efrit-do--display-result command fallback-error t)
      (user-error "%s" fallback-error))))

;;;###autoload
(defun efrit-do-async-legacy (command)
  "Execute natural language COMMAND using legacy async executor.

DEPRECATED: This function is deprecated and should not be used.
Use `efrit-do' instead, which provides proper async execution with
progress buffer, queueing, and interruption support, or use
`efrit-do-sync' for synchronous blocking execution.

This function uses the older `efrit-execute-async' path which lacks
the full agentic loop infrastructure. It is kept for backward
compatibility only."
  (interactive
   (list (read-string "Command (legacy async): " nil 'efrit-do-history)))

  ;; Add to history
  (add-to-history 'efrit-do-history command efrit-do-history-max)

  ;; Track command execution
  (efrit-session-track-command command)

  ;; Reset circuit breaker for new command session
  (efrit-do--circuit-breaker-reset)

  ;; Execute asynchronously
  (require 'efrit-executor)
  (message "Executing asynchronously: %s..." command)
  (efrit-execute-async
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

;;; Async Infrastructure Integration

(defun efrit-do--create-session-for-command (command)
  "Create and initialize an Efrit session for COMMAND.
Returns the session object, ready for async loop execution."
  (let* ((session-id (format "efrit-do-%s" (format-time-string "%Y%m%d%H%M%S")))
         (session (efrit-session-create session-id command)))
    ;; Set as active session
    (efrit-session-set-active session)
    ;; Reset circuit breaker for new command session
    (efrit-do--circuit-breaker-reset)
    ;; Reset TODO list for new session
    (setq efrit-do--current-todos nil)
    (setq efrit-do--todo-counter 0)
    session))

(defun efrit-do--on-async-complete (session stop-reason)
  "Handle completion of async efrit-do SESSION with STOP-REASON.
Displays final results and processes queued commands."
  (let* ((session-id (efrit-session-id session))
         (command (efrit-session-command session))
         ;; Build a result summary from session state
         (work-log (efrit-session-work-log session))
         (result (if work-log
                     (format "Completed %d steps" (length work-log))
                   "Completed")))
    ;; Store result
    (setq efrit-do--last-result result)
    (efrit-do--capture-context command result)
    
    ;; Display result based on stop reason
    (let ((is-error (member stop-reason '("api-error" "interrupted"))))
      (when is-error
        (setq result (format "Session ended: %s" stop-reason)))
      (efrit-do--display-result command result is-error))
    
    ;; Show completion message
    (message "Efrit: %s (session %s)"
             (pcase stop-reason
               ("end_turn" "completed")
               ("interrupted" "interrupted by user")
               ("api-error" "failed with API error")
               ("iteration-limit-exceeded" "hit iteration limit")
               (_ stop-reason))
             session-id)
    
    ;; Process next queued command if available (on normal completion)
    (when (string= stop-reason "end_turn")
      (when-let* ((next-cmd (efrit-do-queue-pop-command)))
        (message "Efrit: starting queued command...")
        ;; Use run-at-time to avoid deep recursion
        (run-at-time 0.1 nil #'efrit-do next-cmd)))))

;;;###autoload
(defun efrit-do (command)
  "Execute natural language COMMAND in Emacs asynchronously (recommended).

This is the PRIMARY interface for Efrit command execution. It provides
non-blocking execution with a real-time progress buffer showing:
- Claude's reasoning and decision-making
- Tool invocations and results
- Session status and progress

FEATURES:
- Progress buffer with real-time updates (automatic display)
- Interruption support with \\[keyboard-quit] (C-g)
- Command queuing if a session is already running
- Non-blocking execution keeps Emacs responsive
- Session state tracking and restoration

USAGE:
```
M-x efrit-do RET
> create a buffer with today's date
```

If a command is already running, this command is queued for later
execution. Use `efrit-do-show-queue' to view queued commands.

For silent execution without showing the progress buffer, use
`efrit-do-silently' instead. For synchronous/blocking execution,
use `efrit-do-sync'.

Returns the session object for programmatic use."
  (interactive
   (list (read-string "Command: " nil 'efrit-do-history)))
  
  ;; Check if there's already an active session
  (if (efrit-session-active)
      ;; Queue this command for later
      (progn
        (if (efrit-do-queue-add-command command)
            (message "Efrit: command queued (position %d)"
                     (efrit-do-queue-size))
          (message "Efrit: queue full, command not added"))
        nil)
    
    ;; No active session - start async execution
    ;; Add to history
    (add-to-history 'efrit-do-history command efrit-do-history-max)
    
    ;; Track command execution
    (efrit-session-track-command command)
    
    ;; Create session and start async loop
    (let ((session (efrit-do--create-session-for-command command)))
      (message "Efrit: starting async execution...")
      (efrit-do-async-loop session nil #'efrit-do--on-async-complete)
      session)))

;;;###autoload
(defun efrit-do-sync (command)
  "Execute natural language COMMAND in Emacs synchronously (blocking).

This is the synchronous/blocking execution path. Most users should
prefer `efrit-do' for non-blocking async execution with real-time
progress visibility, command queueing, and proper interruption handling.

Use `efrit-do-sync' only when you need blocking behavior, such as:
- Scripts where waiting for completion is required
- Scripting contexts that cannot handle async execution
- One-off commands where blocking is acceptable

The command is sent to Claude, which translates it into Elisp
and executes it immediately. Results are displayed in a dedicated
buffer if `efrit-do-show-results' is non-nil.

Progress feedback shows elapsed time and tool executions.
Use \\[keyboard-quit] (C-g) to cancel execution during API calls.

Returns the result string for programmatic use."
  (interactive
   (list (read-string "Command (sync): " nil 'efrit-do-history)))

  ;; Add to history
  (add-to-history 'efrit-do-history command efrit-do-history-max)

  ;; Track command execution
  (efrit-session-track-command command)

  ;; Reset circuit breaker for new command session
  (efrit-do--circuit-breaker-reset)

  ;; Reset TODO list for new session
  (setq efrit-do--current-todos nil)
  (setq efrit-do--todo-counter 0)

  ;; Use efrit-execute which has proper agentic loop with continuation
  (require 'efrit-executor)
  (let* ((start-time (current-time))
         (progress-timer (efrit-do--start-progress-timer start-time command))
         result)
    (unwind-protect
        (progn
          (message "Executing: %s..." command)
          (setq result (efrit-execute command))
          ;; Process and display result
          (when result
            (let* ((error-info (efrit-do--extract-error-info result))
                   (is-error (car error-info)))
              (efrit-do--capture-context command result)
              (efrit-do--display-result command result is-error)
              (setq efrit-do--last-result result)))
          result)
      ;; Always cancel the timer when done
      (when progress-timer
        (cancel-timer progress-timer)))))

;;;###autoload
(defun efrit-do-silently (command)
  "Execute COMMAND asynchronously in background without progress buffer.

Like `efrit-do', but without automatically displaying the progress buffer.
Progress is visible only in:
- Modeline status indicator
- Minibuffer messages

WHEN TO USE:
- Long-running commands where you want to continue editing
- Commands where you don't need to see detailed progress
- Background tasks that shouldn't interrupt your workflow

Show progress buffer manually with \\[efrit-do-show-progress].
Use \\[keyboard-quit] (C-g) to interrupt execution.

If a command is running, this command is queued for later execution.
Use `efrit-do-show-queue' to view queued commands.

Returns the session object for programmatic use."
  (interactive
   (list (read-string "Command (silent): " nil 'efrit-do-history)))
  
  ;; Check if there's already an active session
  (if (efrit-session-active)
      ;; Queue this command for later
      (progn
        (if (efrit-do-queue-add-command command)
            (message "Efrit: command queued (position %d)"
                     (efrit-do-queue-size))
          (message "Efrit: queue full, command not added"))
        nil)
    
    ;; No active session - start async execution
    ;; Add to history
    (add-to-history 'efrit-do-history command efrit-do-history-max)
    
    ;; Track command execution
    (efrit-session-track-command command)
    
    ;; Create session and start async loop WITHOUT showing progress buffer
    (let ((session (efrit-do--create-session-for-command command))
          (original-show efrit-do-async-show-progress-buffer))
      (unwind-protect
          (progn
            (setq efrit-do-async-show-progress-buffer nil)
            (message "Efrit: starting async execution (background)...")
            (efrit-do-async-loop session nil #'efrit-do--on-async-complete))
        (setq efrit-do-async-show-progress-buffer original-show))
      session)))

;;;###autoload
(defun efrit-do-show-progress (&optional session-id)
  "Show the progress buffer for SESSION-ID or the current session.
If called interactively without SESSION-ID, uses the active session.
If SESSION-ID is provided, shows the progress buffer for that session."
  (interactive)
  (let* ((session-id (or session-id
                        (when (efrit-session-active)
                          (efrit-session-id (efrit-session-active)))))
         (buffer (when session-id
                   (efrit-progress-get-buffer session-id))))
    (cond
     (buffer
      (display-buffer buffer)
      (message "Showing progress buffer for session %s" session-id))
     (session-id
      (message "Progress buffer not found for session %s" session-id))
     (t
      (message "No active session. Use M-x efrit-do to start a command.")))))

;;;###autoload
(defun efrit-do-show-queue ()
  "Show the current command queue.
Displays queued commands with their positions and status."
  (interactive)
  (efrit-session-show-queue))

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

;;;###autoload
(defun efrit-do-truncate-old-results ()
  "Truncate old results, keeping only recent ones.
Keeps the last `efrit-do-keep-results' command results in the buffer."
  (interactive)
  (if-let* ((buffer (get-buffer efrit-do-buffer-name)))
      (with-current-buffer buffer
        (let ((before-lines (count-lines (point-min) (point-max))))
          (efrit-do--truncate-results-buffer)
          (let ((after-lines (count-lines (point-min) (point-max))))
            (if (< after-lines before-lines)
                (message "Truncated results buffer (%d lines -> %d lines)"
                        before-lines after-lines)
              (message "Results buffer already within limits")))))
    (message "No results buffer to truncate")))

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
  "Show current TODO items in a shrink-to-fit buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*efrit-do-todos*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Current efrit-do TODOs\n")
        (insert "=====================\n\n")
        
        (if (null efrit-do--current-todos)
            (insert "No current TODOs.\n")
          ;; Show statistics
          (let ((total (length efrit-do--current-todos))
                (completed (seq-count (lambda (todo)
                                       (eq (efrit-do-todo-item-status todo) 'completed))
                                     efrit-do--current-todos))
                (in-progress (seq-count (lambda (todo)
                                         (eq (efrit-do-todo-item-status todo) 'in-progress))
                                       efrit-do--current-todos)))
            (insert (format "Total: %d | Completed: %d | In Progress: %d | Pending: %d\n\n"
                           total completed in-progress (- total completed in-progress))))
          
          ;; Show TODO list
          (insert (efrit-do--format-todos-for-display)))
        
        (insert "\n\nCommands:\n")
        (insert "  M-x efrit-do-clear-todos - Clear all TODOs\n")
        (insert "  M-x efrit-progress-show  - Show progress buffer\n")
        
        (goto-char (point-min))
        (special-mode)))
    
    ;; Display buffer with optional shrink-to-fit
    (let ((window (display-buffer buffer
                                 (if efrit-do-auto-shrink-todo-buffers
                                     '((display-buffer-reuse-window
                                        display-buffer-below-selected)
                                       (window-height . fit-window-to-buffer)
                                       (window-parameters . ((no-delete-other-windows . t))))
                                   '(display-buffer-reuse-window
                                     display-buffer-below-selected)))))
      (when (and window efrit-do-auto-shrink-todo-buffers)
        (fit-window-to-buffer window nil nil 15 nil)))))

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
            (let ((command (efrit-context-item-command item))
                  (result (efrit-context-item-result item))
                  (timestamp (efrit-context-item-timestamp item)))
              
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
            (let ((command (efrit-context-item-command item))
                  (result (efrit-context-item-result item)))
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
