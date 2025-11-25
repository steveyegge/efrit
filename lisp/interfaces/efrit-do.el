;;; efrit-do.el --- Execute natural language commands in Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.3.0
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
(declare-function efrit-execute-async "efrit-executor")

(require 'efrit-tools)
(require 'efrit-config)
(require 'efrit-common)
(require 'efrit-session)
(require 'efrit-chat)
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

(defcustom efrit-do-max-tool-calls-per-session 30
  "Maximum number of tool calls allowed in a single session.
When this limit is reached, the circuit breaker trips and the session is
terminated.  This prevents infinite loops from burning through API tokens."
  :type 'integer
  :group 'efrit-do)

(defcustom efrit-do-max-same-tool-calls 15
  "Maximum number of consecutive calls to the same tool.
When this limit is reached, the circuit breaker trips and terminates the session.
Set higher (15) to allow legitimate multi-step coding tasks that require
many eval_sexp calls in sequence (e.g., writing multiple functions)."
  :type 'integer
  :group 'efrit-do)

(defcustom efrit-do-max-identical-tool-calls 3
  "Maximum number of consecutive identical tool calls (same tool + same input).
This catches true infinite loops where the same operation is repeated.
Lower than `efrit-do-max-same-tool-calls' since identical calls are more
likely to indicate a loop than varied calls to the same tool."
  :type 'integer
  :group 'efrit-do)

(defcustom efrit-do-same-tool-warning-threshold 10
  "Issue a warning after this many consecutive calls to the same tool.
The warning is sent to Claude before the hard limit at `efrit-do-max-same-tool-calls'
to give it a chance to change approach."
  :type 'integer
  :group 'efrit-do)

(defcustom efrit-do-circuit-breaker-enabled t
  "Whether circuit breaker protection is enabled.
When non-nil, enforces hard limits on tool calls to prevent infinite loops.
Recommended to keep enabled for safety."
  :type 'boolean
  :group 'efrit-do)

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

(defcustom efrit-max-tokens 8192
  "Maximum number of tokens in the response for efrit-do."
  :type 'integer
  :group 'efrit-do)

;; Use centralized API URL - legacy variable kept for compatibility
(defcustom efrit-api-url nil
  "Legacy API URL setting. Use efrit-api-base-url in efrit-common instead.
When nil, uses the centralized configuration."
  :type '(choice (const :tag "Use centralized config" nil)
                 (string :tag "Legacy URL override"))
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

(defvar efrit-do--workflow-state 'initial
  "Current state in the TODO workflow.
Possible states:
  - initial: No analysis done yet
  - analyzed: Analysis complete, ready for TODO creation
  - todos-created: TODOs exist, ready for execution
  - executing: Working through TODOs")

(defvar efrit-do--last-tool-called nil
  "Track the last tool that was called.")

(defvar efrit-do--tool-call-count 0
  "Count consecutive calls to the same tool.")

(defvar efrit-do--last-tool-input nil
  "Track the last tool input hash for detecting identical calls.")

(defvar efrit-do--identical-call-count 0
  "Count consecutive identical tool calls (same tool + same input).")

(defvar efrit-do--force-complete nil
  "When t, forces session completion on next API response.")

(defvar efrit-do--todo-awaiting-completion nil
  "TODO ID that executed code but hasn't been marked completed yet.")

;;; Circuit Breaker State

(defvar efrit-do--session-tool-count 0
  "Total number of tool calls in the current session.
Reset when a new session starts.  Used by circuit breaker to prevent
runaway loops.")

(defvar efrit-do--circuit-breaker-tripped nil
  "When non-nil, circuit breaker has tripped and session is terminated.
Stores reason for tripping as a string.")

(defconst efrit-do--tools-schema
  [(("name" . "eval_sexp")
  ("description" . "PRIMARY TOOL: Execute Elisp code directly. Use for simple tasks like opening files, buffer operations, single commands. For complex multi-step tasks, use todo_analyze first.

EXAMPLES:
- Open files: (find-file \"/path/to/file.txt\")
- Open all images: (dolist (f (directory-files-recursively \"~/Pictures\" \"\\\\.\\\\(?:png\\\\|jpe?g\\\\)$\")) (find-file f))
- Create buffer: (switch-to-buffer (get-buffer-create \"*My Buffer*\"))
- Edit text: (with-current-buffer \"file.txt\" (goto-char (point-min)) (insert \"Hello\"))
- Navigate: (goto-line 50) or (goto-char (point-max))
- Search: (re-search-forward \"pattern\" nil t)")
  ("input_schema" . (("type" . "object")
  ("properties" . (("expr" . (("type" . "string")
  ("description" . "The Elisp expression to evaluate")))))
  ("required" . ["expr"]))))
   (("name" . "shell_exec")
   ("description" . "Execute a shell command and return the result. ONLY use when user explicitly requests shell/terminal operations or external tools.

EXAMPLES:
- File system info: \"ls -la ~/Documents\"
- Process management: \"ps aux | grep emacs\"
- System commands: \"git status\" or \"make build\"
- External tools: \"curl -s https://api.example.com\"

DO NOT USE for Emacs operations like opening files - use eval_sexp instead.")
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
   (("name" . "todo_analyze")
   ("description" . "COMPLEX TASKS ONLY: Break down multi-step workflows into TODO items. Use eval_sexp for simple single-action tasks like opening files.

USE WHEN:
- Multiple dependent steps (e.g., \"fix all warnings in buffer\" requires: scan warnings, create fix for each, apply fixes)
- Conditional logic (e.g., \"organize files by type\" requires: check each file, categorize, move to appropriate folders)
- Async operations (e.g., \"download and process multiple files\")

DO NOT USE for simple tasks:
- Opening files: Use eval_sexp directly
- Single buffer operations: Use eval_sexp directly  
- Basic navigation/editing: Use eval_sexp directly")
   ("input_schema" . (("type" . "object")
   ("properties" . (("command" . (("type" . "string")
   ("description" . "The command to analyze")))
   ("context" . (("type" . "string")
   ("description" . "Additional context about the current state")))))
   ("required" . ["command"]))))
   (("name" . "todo_status")
    ("description" . "Get summary of TODO list: total, pending, in-progress, completed.")
    ("input_schema" . (("type" . "object")
                      ("properties" . ()))))
   (("name" . "todo_next")
    ("description" . "Get the next pending TODO item to work on.")
    ("input_schema" . (("type" . "object")
                      ("properties" . ()))))
   (("name" . "todo_execute_next")
   ("description" . "Execute the next pending TODO by marking it in-progress and providing clear task instructions.")
   ("input_schema" . (("type" . "object")
   ("properties" . ()))))
   (("name" . "todo_get_instructions")
     ("description" . "Get execution instructions for the next pending TODO. Marks the TODO as in-progress and returns detailed guidance for completing the task. This is an INFORMATION tool - it provides instructions but does NOT execute code.")
     ("input_schema" . (("type" . "object")
                       ("properties" . ()))))
    (("name" . "todo_complete_check")
    ("description" . "Check if all TODOs are completed. Returns true if all done, false if work remains.")
    ("input_schema" . (("type" . "object")
                      ("properties" . ()))))
   (("name" . "suggest_execution_mode")
   ("description" . "Suggest whether a command should run synchronously or asynchronously based on its characteristics.")
   ("input_schema" . (("type" . "object")
   ("properties" . (("mode" . (("type" . "string")
   ("enum" . ["sync" "async"])
   ("description" . "Suggested execution mode")))
   ("reason" . (("type" . "string")
   ("description" . "Brief explanation for the suggestion")))))
   ("required" . ["mode"]))))
    (("name" . "glob_files")
     ("description" . "Get list of files matching a pattern. INFORMATIONAL ONLY - does not open files. Use results with eval_sexp to perform actions.

EXAMPLES:
- Find images: pattern=\"~/Pictures\" extension=\"png,jpg,jpeg\"  
- Find code files: pattern=\"~/project/src\" extension=\"py,js,ts\"
- Find all files: pattern=\"~/Documents\" extension=\"*\"")
     ("input_schema" . (("type" . "object")
                       ("properties" . (("pattern" . (("type" . "string")
                                                      ("description" . "Directory path to search (supports ~ expansion)")))
                                       ("extension" . (("type" . "string")
                                                      ("description" . "File extensions to match (comma-separated, or * for all)")))
                                       ("recursive" . (("type" . "boolean")
                                                      ("description" . "Whether to search subdirectories (default: true)")))))
                       ("required" . ["pattern" "extension"]))))]
                      "Schema definition for all available tools in efrit-do mode.")

(defun efrit-do--get-tools-for-state ()
  "Return tool schema filtered by current workflow state.

This implements schema-based loop prevention by restricting which tools Claude
can call based on the current workflow state:

- initial: Allow planning tools (todo_analyze, todo_add) + always-available
- todos-created/executing: Block planning, restrict query tools, promote
  execution
- After todo_get_instructions called: Remove it from schema to force
  eval_sexp

The goal is to prevent infinite loops of todo_status, todo_get_instructions
without actual code execution."
  (let* ((always-available-tools
          ;; These tools are safe in any state
          '("glob_files" "buffer_create" "format_file_list"
            "format_todo_list" "display_in_buffer" "session_complete"
            "suggest_execution_mode"))
         (planning-tools
          ;; For initial planning and analysis
          '("todo_analyze" "todo_add"))
         (execution-tools
          ;; For actually doing work
          '("eval_sexp" "shell_exec" "todo_update" "todo_complete_check"))
         (query-tools
          ;; Information tools - use sparingly
          '("todo_status" "todo_next"))
         (dangerous-tools
          ;; Loop-prone tools - strict limits
          '("todo_get_instructions" "todo_execute_next"))
         (allowed-tool-names
          (cond
           ;; Initial state: Allow planning + always-available
           ((eq efrit-do--workflow-state 'initial)
            (append planning-tools always-available-tools query-tools dangerous-tools))

           ;; After todo_get_instructions called: Block it to force execution
           ((and (or (eq efrit-do--workflow-state 'todos-created)
                     (eq efrit-do--workflow-state 'executing))
                 (>= efrit-do--tool-call-count 1)
                 (memq efrit-do--last-tool-called '(todo_get_instructions todo_execute_next)))
            (efrit-log 'info "Schema filter: Blocking todo_get_instructions after %d calls, forcing eval_sexp"
                      efrit-do--tool-call-count)
            (append execution-tools query-tools always-available-tools))

           ;; TODOs exist: Allow limited query + execution
           ((or (eq efrit-do--workflow-state 'todos-created)
                (eq efrit-do--workflow-state 'executing))
            (append execution-tools query-tools dangerous-tools always-available-tools))

           ;; Analyzed but no TODOs: Allow adding TODOs + execution
           ((eq efrit-do--workflow-state 'analyzed)
            (append planning-tools execution-tools query-tools always-available-tools))

           ;; Default: Return everything (safety fallback)
           (t
            (efrit-log 'warn "Schema filter: Unknown state %s, returning all tools"
                      efrit-do--workflow-state)
            (mapcar (lambda (tool) (alist-get "name" tool nil nil #'string=))
                   efrit-do--tools-schema)))))

    ;; Filter the full schema to only include allowed tools
    (seq-filter (lambda (tool)
                  (member (alist-get "name" tool nil nil #'string=)
                         allowed-tool-names))
               efrit-do--tools-schema)))

(defun efrit-do--get-current-tools-schema ()
  "Return tool schema for current workflow state.
This is the main entry point for dynamic tool filtering."
  (efrit-do--get-tools-for-state))

;;; Circuit Breaker Implementation

(defun efrit-do--circuit-breaker-reset ()
  "Reset circuit breaker state for a new session."
  (setq efrit-do--session-tool-count 0)
  (setq efrit-do--circuit-breaker-tripped nil)
  (setq efrit-do--last-tool-called nil)
  (setq efrit-do--tool-call-count 0)
  (setq efrit-do--last-tool-input nil)
  (setq efrit-do--identical-call-count 0))

(defun efrit-do--hash-tool-input (tool-input)
  "Create a hash string for TOOL-INPUT to detect identical calls."
  (secure-hash 'md5 (format "%S" tool-input)))

(defun efrit-do--circuit-breaker-check-limits (tool-name &optional tool-input)
  "Check circuit breaker limits before executing TOOL-NAME with TOOL-INPUT.
Returns (ALLOWED-P . MESSAGE) where ALLOWED-P is t if execution should proceed.
MESSAGE may contain a warning even when ALLOWED-P is t.
Uses efrit--safe-execute for error handling."
  (if (not efrit-do-circuit-breaker-enabled)
      (cons t nil) ; Circuit breaker disabled
    (let* ((input-hash (when tool-input (efrit-do--hash-tool-input tool-input)))
           (is-same-tool (string= tool-name efrit-do--last-tool-called))
           (is-identical-call (and is-same-tool
                                   input-hash
                                   (equal input-hash efrit-do--last-tool-input)))
           (result
            (efrit--safe-execute
             (lambda ()
               (cond
                ;; Already tripped - block all further calls
                (efrit-do--circuit-breaker-tripped
                 (cons nil (format "Circuit breaker active: %s"
                                   efrit-do--circuit-breaker-tripped)))

                ;; Check session-wide limit
                ((>= efrit-do--session-tool-count efrit-do-max-tool-calls-per-session)
                 (setq efrit-do--circuit-breaker-tripped
                       (format "Session limit reached: %d/%d tool calls. Last tool: %s (consecutive: %d)"
                               efrit-do--session-tool-count
                               efrit-do-max-tool-calls-per-session
                               (or efrit-do--last-tool-called "none")
                               efrit-do--tool-call-count))
                 (cons nil efrit-do--circuit-breaker-tripped))

                ;; Check identical call limit (same tool + same input) - this catches true loops
                ((and is-identical-call
                      (>= efrit-do--identical-call-count efrit-do-max-identical-tool-calls))
                 (setq efrit-do--circuit-breaker-tripped
                       (format "Identical tool call detected: '%s' called %d times with same input (limit: %d). This appears to be an infinite loop."
                               tool-name
                               (1+ efrit-do--identical-call-count)
                               efrit-do-max-identical-tool-calls))
                 (efrit-log 'warn "Circuit breaker tripped: %s" efrit-do--circuit-breaker-tripped)
                 (cons nil efrit-do--circuit-breaker-tripped))

                ;; Check same-tool hard limit
                ((and is-same-tool
                      (>= efrit-do--tool-call-count efrit-do-max-same-tool-calls))
                 (setq efrit-do--circuit-breaker-tripped
                       (format "Same tool '%s' called %d times consecutively (limit: %d)"
                               tool-name
                               (1+ efrit-do--tool-call-count)
                               efrit-do-max-same-tool-calls))
                 (efrit-log 'warn "Circuit breaker tripped: %s" efrit-do--circuit-breaker-tripped)
                 (cons nil efrit-do--circuit-breaker-tripped))

                ;; Check same-tool warning threshold - allow but warn
                ((and is-same-tool
                      (>= efrit-do--tool-call-count efrit-do-same-tool-warning-threshold))
                 (cons t (format "[WARNING: '%s' called %d times consecutively. Consider varying your approach. Hard limit at %d calls.]"
                                 tool-name
                                 (1+ efrit-do--tool-call-count)
                                 efrit-do-max-same-tool-calls)))

                ;; All checks passed
                (t (cons t nil))))
             "circuit breaker check"
             "Check efrit-do-circuit-breaker-enabled and tool call limits")))
      (if (car result)
          (cdr result) ; Return the check result
        ;; Safe-execute caught an error - treat as breaker trip
        (progn
          (setq efrit-do--circuit-breaker-tripped (cdr result))
          (cons nil (format "Circuit breaker error: %s" (cdr result))))))))

(defun efrit-do--circuit-breaker-record-call (tool-name &optional tool-input)
  "Record a tool call for TOOL-NAME with TOOL-INPUT in circuit breaker tracking.
Updates counters and session tracking. Uses efrit--safe-execute for safety."
  (when efrit-do-circuit-breaker-enabled
    (efrit--safe-execute
     (lambda ()
       (let ((input-hash (when tool-input (efrit-do--hash-tool-input tool-input))))
         ;; Update session-wide counter
         (cl-incf efrit-do--session-tool-count)

         ;; Update same-tool counter
         (if (string= tool-name efrit-do--last-tool-called)
             (cl-incf efrit-do--tool-call-count)
           (setq efrit-do--tool-call-count 1))

         ;; Update identical-call counter (same tool + same input)
         (if (and (string= tool-name efrit-do--last-tool-called)
                  input-hash
                  (equal input-hash efrit-do--last-tool-input))
             (cl-incf efrit-do--identical-call-count)
           (setq efrit-do--identical-call-count 1))

         ;; Update last tool and input
         (setq efrit-do--last-tool-called tool-name)
         (setq efrit-do--last-tool-input input-hash)

         ;; Track in session
         (when (fboundp 'efrit-session-track-tool-used)
           (efrit-session-track-tool-used tool-name))

         ;; Log the call
         (efrit-log 'debug "Circuit breaker: tool=%s count=%d/%d same=%d/%d identical=%d/%d"
                    tool-name
                    efrit-do--session-tool-count
                    efrit-do-max-tool-calls-per-session
                    efrit-do--tool-call-count
                    efrit-do-max-same-tool-calls
                    efrit-do--identical-call-count
                    efrit-do-max-identical-tool-calls)))
     "recording tool call"
     "Check circuit breaker state and session tracking")))

;;; Context system - delegates to efrit-context.el

;;; Session protocol instructions

(defun efrit-do--session-protocol-instructions ()
  "Return detailed instructions for Claude about the session protocol."
  (concat
   "SESSION PROTOCOL:\n"
   "You are continuing a multi-step session. Your goal is to complete the task incrementally.\n\n"
   
   "0. CHOOSE YOUR APPROACH:\n"
   "   - SIMPLE tasks (open files, single commands): Use eval_sexp directly\n"  
   "   - COMPLEX tasks (multi-step workflows): Use todo_analyze to break down\n"
   "   - For CONTINUING sessions: The system will provide work log context\n"
   "   - NEVER call todo_status first on a new task - it will fail!\n"
    "   - CRITICAL: Never call the same tool twice in a row!\n\n"
    
    "LOOP PREVENTION PROTOCOL:\n"
    "1. CIRCUIT BREAKER PROTECTION:\n"
    "   - Session limit: Maximum 30 tool calls per session\n"
    "   - Same tool limit: Max 15 consecutive calls to same tool (warning at 10)\n"
    "   - Identical call limit: Max 3 calls with SAME tool AND SAME input (true loop detection)\n"
    "   - If you see circuit breaker warnings, CHANGE YOUR APPROACH immediately\n"
    "   - Circuit breaker trips = session terminated, must start new session\n"
    "   - Multi-step coding tasks (writing multiple functions) are fine up to 15 calls\n\n"
    "2. If you see [CRITICAL LOOP] or [ERROR] messages:\n"
    "   - Read the error message carefully\n"
    "   - Follow the IMMEDIATE ACTION REQUIRED instructions\n"
    "   - Never repeat the exact same tool call (same input)\n\n"
    "3. If you're making no progress after 3 attempts:\n"
    "   - Call session_complete with an explanation\n\n"
    "4. Trust the error messages - they prevent infinite loops\n\n"
   
   "1. WORK LOG INTERPRETATION:\n"
   "   - The work log shows your previous steps: [[\"result1\", \"code1\"], [\"result2\", \"code2\"], ...]\n"
   "   - Each entry contains [result, code] from a previous tool execution\n"
   "   - Use this to understand what you've already done and what remains\n\n"
   
   "2. TODO-BASED WORKFLOW (IMPORTANT - PREVENTS LOOPS!):\n"
   "   - ALWAYS check todo_status at the start of each continuation\n"
   "   - Use todo_complete_check to see if all work is done\n"
   "   - If all TODOs completed, use session_complete immediately\n"
   "   - Otherwise: mark current TODO complete, get next with todo_next\n"
   "   - This prevents infinite loops by tracking progress!\n\n"
   
   "3. CONTINUATION DECISION:\n"
   "   - First check TODOs - if all complete, session is done\n"
   "   - Analyze the work log and original command to determine next steps\n"
   "   - If the task is incomplete, execute the next logical step\n"
   "   - If the task is complete, use the session_complete tool with final result\n\n"
   
   "4. TWO-PHASE EXECUTION MODEL:\n"
   "   Phase 1 - Context Gathering (if needed):\n"
   "   - Use eval_sexp to gather information about Emacs state\n"
   "   - Examples: (buffer-list), (point), (buffer-substring-no-properties ...)\n"
   "   - Results help you make informed decisions\n\n"
   "   Phase 2 - Action Execution:\n"
   "   - Based on Phase 1 results (if any) and work log, execute the action\n"
   "   - Examples: create buffers, modify text, run commands\n"
   "   - Each execution adds to the work log for next continuation\n\n"
   
   "5. TODO-DRIVEN EXAMPLE:\n"
   "   Command: 'fix all warnings in buffer'\n"
   "   Initial: todo_analyze, then todo_add for each warning found\n"
   "   Loop: todo_status → todo_get_instructions → eval_sexp/shell_exec → todo_update\n"
   "   End: todo_complete_check → session_complete\n\n"
   
   "6. IMPORTANT RULES:\n"
   "   - ALWAYS use TODOs for multi-step tasks to prevent loops\n"
   "   - PREFER todo_get_instructions over todo_execute_next (clearer intent)\n"
   "   - todo_get_instructions provides instructions but does NOT execute - you must use eval_sexp/shell_exec\n"
   "   - Execute ONE meaningful action per continuation\n"
   "   - Check TODO completion before continuing session\n"
   "   - Use session_complete when all TODOs are done\n"
   "   - Keep responses minimal - focus on execution\n"
   "   - The work log provides your memory across steps\n\n"
   
   "7. ANTI-LOOP PROTECTIONS:\n"
   "   - DO NOT alternate between todo_status and todo_analyze\n"
   "   - DO NOT call todo_analyze multiple times without creating TODOs\n"
   "   - After todo_status shows pending TODOs: call todo_get_instructions (preferred) or todo_execute_next\n"
   "   - After todo_get_instructions: DO the actual work with eval_sexp/shell_exec, then todo_update to mark complete\n"
   "   - Focus on EXECUTION not endless analysis\n\n"

   "8. ERROR ADAPTATION PROTOCOL (CRITICAL!):\n"
   "   When code fails with an error:\n"
   "   a) ANALYZE the error message - understand WHY it failed\n"
   "      - Type errors mean wrong data type (check return values with describe-function)\n"
   "      - 'Wrong type argument' means you're passing X when it expects Y\n"
   "      - Void function/variable means the name is wrong or not loaded\n"
   "   b) If same error occurs 2+ times, TRY A DIFFERENT APPROACH:\n"
   "      - Use (describe-function 'function-name) to read actual documentation\n"
   "      - Use (symbol-function 'function-name) to inspect implementation\n"
   "      - Try simpler alternatives (e.g., message instead of complex formatting)\n"
   "   c) NEVER retry the same code pattern more than twice\n"
   "      - If (foo (bar x)) fails, don't try (foo (bar (baz x)))\n"
   "      - Instead, read documentation or try completely different approach\n"
   "   d) When in doubt, use (describe-function 'name) to learn correct usage\n"
   "   e) Example: If garbage-collect returns unexpected structure:\n"
   "      - DON'T keep guessing the structure\n"
   "      - DO: (describe-function 'garbage-collect) to learn the actual return format\n"))

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
  (setq efrit-do--workflow-state 'initial)
  (setq efrit-do--todo-awaiting-completion nil)
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
              ;; Check if there's a TODO in-progress that needs completion
              (let ((in-progress-todo (seq-find (lambda (todo)
                                                (eq (efrit-do-todo-item-status todo) 'in-progress))
                                              efrit-do--current-todos)))
                (when in-progress-todo
                  (setq efrit-do--todo-awaiting-completion (efrit-do-todo-item-id in-progress-todo))))
              
              (format "\n[Executed: %s]\n[Result: %s]%s" 
                      input-str 
                      eval-result
                      (if efrit-do--todo-awaiting-completion
                          (format "\n⚠️ NEXT REQUIRED: Call todo_update with id=\"%s\" status=\"completed\""
                                  efrit-do--todo-awaiting-completion)
                        "")))
          (error
           (format "\n[Error executing %s: %s]" 
                   input-str (error-message-string eval-err))))
      ;; Invalid elisp - report syntax error
      (format "\n[Syntax Error in %s: %s]" 
              input-str (cdr validation)))))

(defcustom efrit-do-shell-security-enabled t
  "When non-nil, enforce security restrictions on shell commands.
Recommended to keep enabled for safety."
  :type 'boolean
  :group 'efrit)

(defvar efrit-do-allowed-shell-commands
  '("ls" "pwd" "date" "whoami" "uname" "df" "ps" "top"
    "cat" "head" "tail" "wc" "grep" "find" "which"
    "echo" "printf" "basename" "dirname" "realpath"
    "git" "make" "emacs" "python" "node" "npm"
    "curl" "wget")
  "List of shell commands considered safe for AI execution.")

(defvar efrit-do-forbidden-shell-patterns
  '("\\brm\\b" "rmdir" "mkdir" "touch" "\\bmv\\b" "\\bcp\\b"
    "chmod" "chown" "sudo" "su" "passwd"
    "kill" "killall" "pkill" "shutdown" "reboot"
    "\\bdd\\b" "fdisk" "mkfs" "mount" "umount"
    "export" "unset" "source" "\\berl\\b"
    "[^a-zA-Z0-9_]>[^a-zA-Z0-9_]" "[^a-zA-Z0-9_]>>[^a-zA-Z0-9_]" 
    "[^a-zA-Z0-9_]<[^a-zA-Z0-9_]" "[^a-zA-Z0-9_]|[^a-zA-Z0-9_]"
    "[^a-zA-Z0-9_]&[^a-zA-Z0-9_]" "&&" "||" "[^a-zA-Z0-9_];[^a-zA-Z0-9_]"
    "\\$(" "`" "\\${")
  "Patterns that are forbidden in shell commands.")

(defun efrit-do--validate-shell-command (command)
  "Validate that COMMAND is safe for execution.
Returns (SAFE-P . ERROR-MESSAGE) where SAFE-P is t if safe."
  (cl-block validate-shell
    (if (not efrit-do-shell-security-enabled)
        (cons t nil) ; Security disabled, allow all
      
      (let ((command-clean (string-trim command)))
        
        ;; Check for empty command
        (when (string-empty-p command-clean)
          (cl-return-from validate-shell 
            (cons nil "Empty shell command not allowed")))
        
        ;; Extract the main command (first word)
        (let ((main-command (car (split-string command-clean))))
          
          ;; Check if main command is in allowed list
          (unless (member main-command efrit-do-allowed-shell-commands)
            (cl-return-from validate-shell
              (cons nil (format "Shell command '%s' not in allowed whitelist" main-command))))
          
          ;; Check for forbidden patterns
          (dolist (pattern efrit-do-forbidden-shell-patterns)
            (when (string-match-p pattern command-clean)
              (cl-return-from validate-shell
                (cons nil (format "Shell command contains forbidden pattern: %s" pattern)))))
          
          ;; Additional safety checks
          (when (> (length command-clean) 200)
            (cl-return-from validate-shell
              (cons nil "Shell command too long (>200 chars)")))
          
          ;; Command appears safe
          (cons t nil))))))

(defun efrit-do--handle-shell-exec (input-str)
  "Handle shell command execution with security validation."
  (let ((validation (efrit-do--validate-shell-command input-str)))
    (if (car validation)
        ;; Command is safe, execute it
        (condition-case shell-err
            (let* ((start-time (current-time))
                   (shell-result (with-timeout (10) ; 10 second timeout
                                   (shell-command-to-string input-str)))
                   (end-time (current-time))
                   (duration (float-time (time-subtract end-time start-time))))
              (format "\n[🔧 Executed: %s]\n[⏱️ Duration: %.2fs]\n[📤 Result: %s]" 
                      input-str duration 
                      (if (> (length shell-result) 1000)
                          (concat (substring shell-result 0 1000) "\n... (output truncated)")
                        shell-result)))
          (timeout (format "\n[❌ Shell command timed out after 10 seconds: %s]" input-str))
          (error (format "\n[❌ Error executing shell command '%s': %s]" 
                         input-str (error-message-string shell-err))))
      ;; Command failed validation
      (format "\n[🚫 SECURITY: Shell command blocked - %s]\n[Command: %s]" 
              (cdr validation) input-str))))

(defun efrit-do--handle-todo-add (tool-input)
  "Handle TODO item addition."
  ;; Reset tool tracking when adding TODOs (valid transition)
  (unless (eq efrit-do--last-tool-called 'todo_add)
    (setq efrit-do--tool-call-count 0))
  (setq efrit-do--last-tool-called 'todo_add)
  
  (let* ((content (if (hash-table-p tool-input)
                     (gethash "content" tool-input)
                   tool-input))
         (priority (when (hash-table-p tool-input)
                    (intern (or (gethash "priority" tool-input) "medium"))))
         (todo (efrit-do--add-todo content priority)))
    ;; Track TODO creation
    (efrit-session-track-todo-created content)
    ;; Update workflow state
    (setq efrit-do--workflow-state 'todos-created)
    ;; Update progress display
    (when (fboundp 'efrit-progress-show-todos)
      (efrit-progress-show-todos))
    ;; Update TODO buffer if visible
    (let ((todo-buffer (get-buffer "*efrit-do-todos*")))
      (when (and todo-buffer (get-buffer-window todo-buffer))
        (efrit-do-show-todos)))
    (format "\n[Added TODO: %s (%s)]" content (efrit-do-todo-item-id todo))))

(defun efrit-do--handle-todo-update (tool-input input-str)
  "Handle TODO status update."
  (let* ((id (if (hash-table-p tool-input)
                (gethash "id" tool-input)
              input-str))
         (status (when (hash-table-p tool-input)
                  (intern (gethash "status" tool-input)))))
    (if (efrit-do--update-todo-status id status)
        (progn
          ;; Clear awaiting completion when TODO is updated
          (when (string= id efrit-do--todo-awaiting-completion)
            (setq efrit-do--todo-awaiting-completion nil))
          
          ;; Track TODO completion
          (when (eq status 'completed)
            (let ((todo-item (efrit-do--find-todo id)))
              (when todo-item
                (efrit-session-track-todo-completed (efrit-do-todo-item-content todo-item)))))
          ;; Update progress display
          (when (fboundp 'efrit-progress-update-todo)
            (efrit-progress-update-todo id status))
          (when (fboundp 'efrit-progress-show-todos)
            (efrit-progress-show-todos))
          ;; Update TODO buffer if visible
          (let ((buffer (get-buffer "*efrit-do-todos*")))
            (when (and buffer (get-buffer-window buffer))
              (efrit-do-show-todos)))
          (format "\n[Updated TODO %s to %s]" id status))
      (format "\n[Error: TODO %s not found]" id))))

(defun efrit-do--handle-todo-show ()
  "Handle TODO list display."
  (format "\n[Current TODOs:]\n%s" (efrit-do--format-todos-for-display)))

(defun efrit-do--handle-todo-analyze (tool-input)
  "Handle TODO analysis for a command."
  ;; Track tool calls
  (if (eq efrit-do--last-tool-called 'todo_analyze)
      (cl-incf efrit-do--tool-call-count)
    (setq efrit-do--tool-call-count 1))
  (setq efrit-do--last-tool-called 'todo_analyze)
  
  ;; Hard stop after 2 calls - return guidance instead of error
  (if (> efrit-do--tool-call-count 2)
  (format "🚨 CRITICAL LOOP: todo_analyze called %d times!

🎯 IMMEDIATE NEXT ACTION:
- For opening images: Call eval_sexp with: (find-file \"~/Documents/image1.png\")
- For fixing warnings: Call eval_sexp with: (with-current-buffer \"*Warnings*\" (buffer-string))
- For general tasks: Call eval_sexp with elisp code to examine the situation

DO NOT call todo_analyze again!"
              efrit-do--tool-call-count)
    ;; Continue with normal processing
    (let ((command (if (hash-table-p tool-input)
                       (gethash "command" tool-input)
                     tool-input)))
      ;; Check workflow state
      (cond
     ;; Already analyzed - don't allow re-analysis
     ((eq efrit-do--workflow-state 'analyzed)
      "\n[ERROR: Already analyzed! You MUST now:\n1. Call eval_sexp to examine the warnings\n2. Call todo_add for each warning\nDO NOT call todo_analyze again!]")
     
     ;; Already have TODOs - absolutely forbidden
     ((or efrit-do--current-todos (eq efrit-do--workflow-state 'todos-created))
      (format "\n[ERROR: Analysis forbidden - %d TODOs already exist!\nCall todo_status to see them or todo_next to work on them.]" 
              (length efrit-do--current-todos)))
     
     ;; Valid initial analysis
     (t
      (setq efrit-do--workflow-state 'analyzed)
      ;; Provide specific guidance based on command type
      (cond
       ((string-match-p "\\(fix\\|resolve\\).*\\(warning\\|error\\)" command)
       ;; Actually analyze warnings and create TODOs automatically
         (condition-case err
             (progn
               ;; Try to get warnings from the buffer
               (let ((warnings-content 
                      (if (get-buffer "*Warnings*")
                          (with-current-buffer "*Warnings*"
                            (buffer-string))
                        "No *Warnings* buffer found")))
                 ;; Parse warnings and create TODOs
                 (let ((todo-count 0))
                   (when (and warnings-content (not (string= warnings-content "No *Warnings* buffer found")))
                     ;; Split into lines and create a TODO for each warning
                     (dolist (line (split-string warnings-content "\n" t))
                       (when (string-match-p "Warning\\|Error" line)
                         (efrit-do--add-todo (format "Fix: %s" (string-trim line)) 'medium)
                         (cl-incf todo-count))))
                   ;; Add verification TODO
                   (when (> todo-count 0)
                     (efrit-do--add-todo "Verify all warnings are resolved" 'low)
                     (cl-incf todo-count))
                   ;; Update workflow state
                   (setq efrit-do--workflow-state 'todos-created)
                   ;; Return success message
                   (format "\n[Analysis Complete: Created %d TODOs for '%s'
✓ Automatically created TODO items for each warning
✓ Added verification TODO
NEXT ACTION: Call todo_get_instructions to start working on the first TODO]" 
                          todo-count command))))
           (error 
            ;; Fallback to manual instructions if auto-analysis fails
            (format "\n[Auto-analysis failed: %s
FALLBACK - Manual steps required:
1. Use eval_sexp with: (with-current-buffer \"*Warnings*\" (buffer-string))
2. Create TODOs manually with todo_add for each warning
3. Then call todo_get_instructions to start working]" (error-message-string err)))))
       
       ((string-match-p "\\(create\\|make\\|build\\)" command)
        (format "\n[Analysis: Creation task - '%s'
Break this down into steps and use todo_add for each step]" command))
       
       (t
        (format "\n[Analysis: '%s'
1. Break this into discrete steps
2. Use todo_add to create a TODO for each step
3. Work through the list systematically]" command))))))))

(defun efrit-do--handle-todo-status ()
  "Return TODO list status summary."
  ;; Track tool calls
  (if (eq efrit-do--last-tool-called 'todo_status)
      (cl-incf efrit-do--tool-call-count)
    (setq efrit-do--tool-call-count 1))
  (setq efrit-do--last-tool-called 'todo_status)
  
  ;; Prevent loops with completion guidance after 2 calls
  (if (>= efrit-do--tool-call-count 2)
      (let ((total (length efrit-do--current-todos))
            (completed (seq-count (lambda (todo)
                                   (eq (efrit-do-todo-item-status todo) 'completed))
                                 efrit-do--current-todos)))
        (if (= total completed)
            "\n[🎉 TASK COMPLETE: All TODOs finished! Stop calling todo_status. Your work is done.]"
          (format "\n[🚨 LOOP PREVENTION: You've called todo_status %d times! 
TODOs remain: %d total, %d completed. 
REQUIRED ACTION: Call todo_get_instructions (if TODOs pending) or eval_sexp (to execute code).
STOP calling todo_status repeatedly!]" 
                  efrit-do--tool-call-count total completed)))
    ;; Continue with normal processing
    (let ((total (length efrit-do--current-todos))
        (pending (seq-count (lambda (todo) 
                             (eq (efrit-do-todo-item-status todo) 'todo))
                           efrit-do--current-todos))
        (in-progress (seq-count (lambda (todo)
                                 (eq (efrit-do-todo-item-status todo) 'in-progress))
                               efrit-do--current-todos))
        (completed (seq-count (lambda (todo)
                               (eq (efrit-do-todo-item-status todo) 'completed))
                             efrit-do--current-todos)))
    (cond
     ;; No TODOs and wrong state
     ((and (= total 0) (not (eq efrit-do--workflow-state 'initial)))
      "\n[ERROR: No TODOs but workflow already started!\nCall todo_add to create TODOs based on your analysis.]")
     
     ;; No TODOs - must analyze first
     ((= total 0)
      "\n[TODO Status: EMPTY\nNEXT ACTION: Call todo_analyze with your command.\nDO NOT call todo_status again!]")
     
     ;; Have TODOs
     (t 
      (setq efrit-do--workflow-state 'todos-created)
      (format "\n[TODO Status: %d total, %d pending, %d in-progress, %d completed\nNEXT ACTION: Call todo_get_instructions to start working on the next TODO.]"
              total pending in-progress completed))))))

(defun efrit-do--handle-todo-next ()
  "Get next pending TODO."
  (let ((next-todo (seq-find (lambda (todo)
                              (eq (efrit-do-todo-item-status todo) 'todo))
                            efrit-do--current-todos)))
    (if next-todo
        (format "\n[Next TODO: %s (ID: %s)]" 
                (efrit-do-todo-item-content next-todo)
                (efrit-do-todo-item-id next-todo))
      "\n[No pending TODOs]")))

(defun efrit-do--handle-todo-execute-next ()
  "Execute the next pending TODO by marking it in-progress and providing details."
  ;; Track tool calls to prevent loops
  (if (eq efrit-do--last-tool-called 'todo_get_instructions)
      (cl-incf efrit-do--tool-call-count)
    (setq efrit-do--tool-call-count 1))
  (setq efrit-do--last-tool-called 'todo_get_instructions)
  
  ;; Force different behavior after 2 calls
  (if (>= efrit-do--tool-call-count 2)
      "\n[🚨 EXECUTION REQUIRED: You've called todo_get_instructions multiple times. Stop asking for instructions and START EXECUTING CODE with eval_sexp. The task is clear from previous instructions.]"
    
    (let ((next-todo (seq-find (lambda (todo)
                                (eq (efrit-do-todo-item-status todo) 'todo))
                              efrit-do--current-todos)))
    (if next-todo
        (let ((todo-id (efrit-do-todo-item-id next-todo))
              (todo-content (efrit-do-todo-item-content next-todo)))
          ;; Mark as in-progress
          (efrit-do--update-todo-status todo-id 'in-progress)
          ;; Return directive to execute the task
          (format "\n[EXECUTING TODO %s: %s]
🎯 TASK STARTED - You must now complete this specific task:

\"%s\"

🚨 MANDATORY WORKFLOW:
1. Call eval_sexp to execute Elisp code for this task
2. IMMEDIATELY call todo_update with id=\"%s\" status=\"completed\"
3. STOP calling todo_get_instructions

⚠️ CRITICAL: You MUST call todo_update after each eval_sexp success or the system will loop!"
                  todo-id todo-content todo-content todo-id))
      "\n[No pending TODOs to execute]"))))

(defun efrit-do--handle-todo-complete-check ()
  "Check if all TODOs are completed."
  (let ((incomplete (seq-find (lambda (todo)
                               (not (eq (efrit-do-todo-item-status todo) 'completed)))
                             efrit-do--current-todos)))
    (if incomplete
        "\n[TODOs incomplete - work remains]"
      "\n[All TODOs completed!]")))

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

(defun efrit-do--handle-glob-files (tool-input)
  "Handle glob_files tool to list files matching pattern."
  (let* ((pattern (if (hash-table-p tool-input)
                     (gethash "pattern" tool-input)
                   "~/"))
         (extension (if (hash-table-p tool-input)
                       (gethash "extension" tool-input)
                     "*"))
         (recursive (if (hash-table-p tool-input)
                       (gethash "recursive" tool-input)
                     t)))
    (condition-case err
        (let* ((expanded-pattern (expand-file-name pattern))
               (extensions (if (string= extension "*")
                              '("")
                            (split-string extension ",")))
               (all-files '()))
          (dolist (ext extensions)
            (let* ((ext-clean (string-trim ext))
                   (search-pattern (if recursive
                                      (format "%s/**/*.%s" expanded-pattern ext-clean)
                                    (format "%s/*.%s" expanded-pattern ext-clean)))
                   (files (if (string= ext-clean "")
                             (if recursive
                                 (directory-files-recursively expanded-pattern ".*")
                               (directory-files expanded-pattern t "^[^.]"))
                           (file-expand-wildcards search-pattern))))
              (setq all-files (append all-files files))))
          (let ((file-count (length all-files))
                (preview (if (> (length all-files) 10)
                            (append (seq-take all-files 8) 
                                   (list (format "... and %d more files" (- (length all-files) 8))))
                          all-files)))
            (format "\n[Found %d files in %s:\n%s]"
                    file-count
                    pattern
                    (mapconcat #'identity preview "\n"))))
      (error
       (format "\n[Error finding files: %s]" (error-message-string err))))))

(defun efrit-do--execute-tool (tool-item)
  "Execute a tool specified by TOOL-ITEM hash table.
TOOL-ITEM should contain \\='name\\=' and \\='input\\=' keys.
Returns a formatted string with execution results or empty string on failure.
Applies circuit breaker limits to prevent infinite loops."
  ;; Validate tool-item is not nil
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
        (let ((warning (cdr breaker-check))
              (result
               ;; Dispatch to appropriate handler
               (cond
                ((string= tool-name "eval_sexp")
                 (if input-str
                     (efrit-do--handle-eval-sexp input-str)
                   "\n[Error: eval_sexp requires 'code' parameter]"))
                ((string= tool-name "shell_exec")
                 (if input-str
                     (efrit-do--handle-shell-exec input-str)
                   "\n[Error: shell_exec requires 'command' parameter]"))
                ((string= tool-name "todo_add")
                 (efrit-do--handle-todo-add tool-input))
                ((string= tool-name "todo_update")
                 (efrit-do--handle-todo-update tool-input input-str))
                ((string= tool-name "todo_show")
                 (efrit-do--handle-todo-show))
                ((string= tool-name "todo_analyze")
                 (efrit-do--handle-todo-analyze tool-input))
                ((string= tool-name "todo_status")
                 (efrit-do--handle-todo-status))
                ((string= tool-name "todo_next")
                 (efrit-do--handle-todo-next))
                ((string= tool-name "todo_execute_next")
                 (efrit-do--handle-todo-execute-next))
                ((string= tool-name "todo_get_instructions")
                 (efrit-do--handle-todo-execute-next))  ; Same handler, clearer name
                ((string= tool-name "todo_complete_check")
                 (efrit-do--handle-todo-complete-check))
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
                ((string= tool-name "glob_files")
                 (efrit-do--handle-glob-files tool-input))
                (t
                 (efrit-log 'warn "Unknown tool: %s with input: %S" tool-name tool-input)
                 (format "\n[Unknown tool: %s]" tool-name)))))

          ;; Prepend warning if present
          (if warning
              (concat "\n" warning "\n" result)
            result)))))))

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
   "Tool call: todo_analyze with command: \"fix warnings in *Warnings* buffer\"\n"
   "[Response: Analysis with steps...]\n"
   "Tool call: eval_sexp with expr: \"(with-current-buffer \\\"*Warnings*\\\" (buffer-string))\"\n"
   "[Response: Warning text...]\n"
   "Tool call: todo_add with content: \"Fix lexical-binding in file1.el\"\n"
   "Tool call: todo_add with content: \"Fix lexical-binding in file2.el\"\n"
   "Tool call: todo_add with content: \"Verify all warnings fixed\"\n"
   "[Then work through TODOs...]\n\n"))

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
            "IMPORTANT: You are in COMMAND MODE - INITIAL EXECUTION.\n\n")
          
          "CRITICAL CONTEXT RULES:\n"
          "- You are operating INSIDE Emacs - all operations should use Elisp unless explicitly requesting shell commands\n"
          "- When user says 'open' files, use find-file to open in Emacs buffers, NOT shell commands\n"
          "- 'Display', 'show', 'list' means create Emacs buffers, NOT terminal output\n"
          "- 'Edit', 'modify', 'change' means buffer operations, NOT external editors\n"
          "- SIMPLE TASKS (≤2 elisp forms): Use eval_sexp directly - NO todo_analyze needed\n"
          "- COMPLEX TASKS (multi-step workflows): Use todo_analyze to break down\n"
          "- TASK CLASSIFICATION: Most 'open X files' requests are SIMPLE - use eval_sexp with directory-files-recursively\n\n"
          
          "TOOL SELECTION GUIDE:\n"
          "- eval_sexp: PRIMARY TOOL for Emacs operations (open files, edit buffers, navigate, define functions, etc.)\n"
          "- shell_exec: ONLY when explicitly asking for shell/terminal operations\n"
          "- buffer_create: ONLY for read-only reports, lists, and formatted output display\n"
          "  * NEVER use buffer_create for code that needs to be evaluated/executed\n"
          "  * For code generation: use eval_sexp with (with-current-buffer... (insert...)) then evaluate\n"
          "- todo_analyze: ONLY for genuinely complex multi-step workflows\n\n"

          "CODE GENERATION vs DISPLAY:\n"
          "- When user asks to WRITE CODE or DEFINE FUNCTIONS: Use eval_sexp to insert into buffer AND evaluate\n"
          "- When user asks to SHOW/DISPLAY RESULTS: Use buffer_create for formatted output\n"
          "- Example: 'write fibonacci function' -> Use eval_sexp to (defun fib ...)\n"
          "- Example: 'show me all buffers' -> Use buffer_create with (buffer-list) results\n\n"
          
          "EXECUTION RULES:\n"
          "- Generate valid Elisp code to accomplish the user's request\n"
          "- CRITICAL: When user asks to 'show', 'list', 'display' or create any kind of report, you MUST use buffer_create tool to create a dedicated buffer\n"
          "- FOR REPORTS/LISTS: ALWAYS use buffer_create to make dedicated buffers with appropriate formatting\n"
          "- FOR FILE LISTS: Use format_file_list to format paths as markdown lists\n"  
          "- USE TODO MANAGEMENT: For ANY multi-step task, ALWAYS create TODOs first\n"
          "- TODO WORKFLOW: 1) Call todo_analyze ONCE, 2) IMMEDIATELY follow its instructions to scan/add TODOs\n"
          "- CRITICAL: After todo_analyze, take ACTION - don't analyze again!\n"
          "- TRACK PROGRESS: Mark TODOs 'in-progress' when starting, 'completed' when done\n"
          "- PREVENT LOOPS: In sessions, ALWAYS check todo_complete_check before continuing\n"
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

;;;###autoload
(defun efrit-do (command)
  "Execute natural language COMMAND in Emacs.
The command is sent to Claude, which translates it into Elisp
and executes it immediately. Results are displayed in a dedicated
buffer if `efrit-do-show-results' is non-nil.

Progress feedback shows elapsed time and tool executions.
Use \\[keyboard-quit] (C-g) to cancel execution during API calls.

If retry is enabled and errors occur, automatically retry by sending
error details back to Claude for correction."
  (interactive
   (list (read-string "Command: " nil 'efrit-do-history)))

  ;; Add to history
  (add-to-history 'efrit-do-history command efrit-do-history-max)

  ;; Track command execution
  (efrit-session-track-command command)

  ;; Reset circuit breaker for new command session
  (efrit-do--circuit-breaker-reset)

  ;; Execute with retry logic and progress feedback
  (let* ((start-time (current-time))
         (progress-timer (efrit-do--start-progress-timer start-time command)))
    (unwind-protect
        (progn
          (message "Executing: %s..." command)
          (let* ((result-and-attempt (efrit-do--execute-with-retry command))
                 (final-result (car result-and-attempt))
                 (attempt (cdr result-and-attempt)))
            (efrit-do--process-result command final-result attempt)))
      ;; Always cancel the timer when done
      (when progress-timer
        (cancel-timer progress-timer)))))

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
