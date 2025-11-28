;;; efrit-do.el --- Execute natural language commands in Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.3.1
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
(defvar efrit-project-root)

(require 'efrit-tools)
(require 'efrit-config)
(require 'efrit-common)
(require 'efrit-session)
(require 'efrit-chat)
(require 'efrit-progress)
(require 'efrit-tool-utils)
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
  "Maximum consecutive calls to the same tool before circuit breaker trips.
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
  "Warn after this many consecutive calls to the same tool.
Warning is sent before the hard limit at `efrit-do-max-same-tool-calls'."
  :type 'integer
  :group 'efrit-do)

(defcustom efrit-do-circuit-breaker-enabled t
  "Whether circuit breaker protection is enabled.
When non-nil, enforces hard limits on tool calls to prevent infinite loops.
Recommended to keep enabled for safety."
  :type 'boolean
  :group 'efrit-do)

(defcustom efrit-do-max-same-error-occurrences 3
  "Maximum times the same error can occur before triggering loop detection.
When this limit is reached, a warning message is injected telling Claude
to try a different approach.  Error signatures are tracked by hashing
the error message."
  :type 'integer
  :group 'efrit-do)

(defcustom efrit-do-error-loop-auto-complete 5
  "Auto-complete session after this many same-error occurrences.
When the same error occurs this many times, the session is automatically
terminated with an error summary.  Set to nil to disable auto-completion."
  :type '(choice integer (const nil))
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

;;; Circuit Breaker State

(defvar efrit-do--session-tool-count 0
  "Total number of tool calls in the current session.
Reset when a new session starts.  Used by circuit breaker to prevent
runaway loops.")

(defvar efrit-do--circuit-breaker-tripped nil
  "When non-nil, circuit breaker has tripped and session is terminated.
Stores reason for tripping as a string.")

;;; Error Loop Detection State

(defvar efrit-do--last-error-hash nil
  "Hash of the most recent error message for loop detection.")

(defvar efrit-do--same-error-count 0
  "Count of consecutive occurrences of the same error.")

(defvar efrit-do--error-history nil
  "List of recent error signatures for pattern analysis.
Each entry is (error-hash . error-message).")

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
    ("file_info"          . (efrit-do--handle-file-info . :tool-input))
    ("vcs_status"         . (efrit-do--handle-vcs-status . :tool-input))
    ("vcs_diff"           . (efrit-do--handle-vcs-diff . :tool-input))
    ("vcs_log"            . (efrit-do--handle-vcs-log . :tool-input))
    ("vcs_blame"          . (efrit-do--handle-vcs-blame . :tool-input))
    ("elisp_docs"         . (efrit-do--handle-elisp-docs . :tool-input))
    ("set_project_root"   . (efrit-do--handle-set-project-root . :tool-input)))
  "Dispatch table mapping tool names to handlers and argument types.")

;;; Budget Hints for Tool Schemas
;;
;; These functions modify tool descriptions dynamically based on remaining
;; context budget, giving Claude guidance on how much data tools can return.

(defvar efrit-do--budget-hints-alist
  '(("project_files" . "Budget allows ~%d files.")
    ("search_content" . "Budget allows ~%d matches with context.")
    ("read_file" . "Budget allows ~%d KB of file content.")
    ("vcs_log" . "Budget allows ~%d commits.")
    ("vcs_diff" . "Budget allows ~%d KB of diff output.")
    ("vcs_blame" . "Budget allows ~%d lines of blame output."))
  "Alist mapping tool names to budget hint templates.
Template uses %d for the calculated limit based on remaining budget.")

(defvar efrit-do--budget-item-sizes
  '(("project_files" . 100)      ; ~100 tokens per file entry
    ("search_content" . 200)     ; ~200 tokens per match with context
    ("read_file" . 285)          ; ~285 tokens per KB (1KB / 3.5 chars/token)
    ("vcs_log" . 150)            ; ~150 tokens per commit entry
    ("vcs_diff" . 285)           ; ~285 tokens per KB
    ("vcs_blame" . 50))          ; ~50 tokens per blame line
  "Alist mapping tool names to estimated tokens per item.
Used to calculate budget limits from remaining token budget.")

(defun efrit-do--budget-hint-for-tool (tool-name remaining-budget)
  "Generate budget hint for TOOL-NAME given REMAINING-BUDGET tokens.
Returns a hint string or nil if no hint applies."
  (when-let* ((template (alist-get tool-name efrit-do--budget-hints-alist nil nil #'string=))
              (item-size (alist-get tool-name efrit-do--budget-item-sizes nil nil #'string=))
              (limit (max 1 (floor (/ (float remaining-budget) item-size)))))
    (format template limit)))

(defun efrit-do--inject-budget-hints (schema budget)
  "Return copy of SCHEMA with budget hints injected into descriptions.
BUDGET is an efrit-budget struct from efrit-budget.el.
If BUDGET is nil, returns schema unchanged."
  (if (or (null budget)
          (not (fboundp 'efrit-budget-remaining)))
      schema
    (let ((remaining (efrit-budget-remaining budget)))
      (if (or (null remaining) (< remaining 5000))
          schema ; Don't modify if budget critically low or unknown
        (vconcat
         (mapcar
          (lambda (tool)
            (let* ((name (alist-get "name" tool nil nil #'string=))
                   (description (alist-get "description" tool nil nil #'string=))
                   (hint (efrit-do--budget-hint-for-tool name remaining)))
              (if hint
                  ;; Create a copy with modified description
                  (let ((new-tool (copy-tree tool)))
                    (setcdr (assoc "description" new-tool #'string=)
                            (concat description "\n\n[" hint "]"))
                    new-tool)
                tool)))
          schema))))))

(defun efrit-do--budget-warning-prompt (budget)
  "Generate budget warning string if BUDGET is low.
Returns nil if no warning needed, otherwise a warning string
suitable for prepending to system prompt."
  (when (and budget (fboundp 'efrit-budget-format-warning))
    (efrit-budget-format-warning budget)))

(defconst efrit-do--tools-schema
  [(("name" . "eval_sexp")
  ("description" . "PRIMARY TOOL: Execute Elisp code directly. Use for simple tasks like opening files, buffer operations, single commands. For complex multi-step tasks (3+ steps), use todo_write to track progress.

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
   (("name" . "todo_write")
    ("description" . "PROACTIVE TASK TRACKING - Use this tool to give the user visibility into your progress.

YOU MUST USE THIS TOOL WHEN:
1. Task requires 3+ distinct steps or operations
2. User gives you multiple things to do (numbered list, comma-separated, etc.)
3. You need to explore/investigate before implementing
4. Task involves iterating over multiple files, functions, or items
5. Task could take multiple API turns to complete

RECOGNIZE THESE PATTERNS:
- 'Fix all the X in Y' → Multiple fixes = use todo_write
- 'Update A, B, and C' → Multiple items = use todo_write
- 'Refactor the X system' → Large change = use todo_write
- 'Find and fix' → Investigation + fixing = use todo_write
- 'Add X to all Y files' → Iteration = use todo_write

DO NOT USE for:
- Single eval_sexp operations (open file, navigate, simple edit)
- Pure questions that need no Emacs operations
- Tasks completable in 1-2 tool calls

Each TODO item requires:
- content: Imperative form (e.g., 'Fix the bug in auth.el')
- status: pending | in_progress | completed
- activeForm: Present continuous (e.g., 'Fixing the bug in auth.el')

WORKFLOW:
1. Call todo_write with ALL planned tasks at start (first task in_progress)
2. After each task completes, call todo_write to mark it completed and set next in_progress
3. When all done, call session_complete

IMPORTANT: Only ONE task in_progress at a time. Mark complete IMMEDIATELY after finishing.")
    ("input_schema" . (("type" . "object")
                      ("properties" . (("todos" . (("type" . "array")
                                                   ("description" . "The complete TODO list")
                                                   ("items" . (("type" . "object")
                                                              ("properties" . (("content" . (("type" . "string")
                                                                                             ("description" . "Task in imperative form (e.g., 'Fix the bug')")))
                                                                              ("status" . (("type" . "string")
                                                                                          ("enum" . ["pending" "in_progress" "completed"])
                                                                                          ("description" . "Task status")))
                                                                              ("activeForm" . (("type" . "string")
                                                                                              ("description" . "Task in present continuous (e.g., 'Fixing the bug')")))))
                                                              ("required" . ["content" "status" "activeForm"])))))))
                      ("required" . ["todos"]))))
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
                       ("required" . ["pattern" "extension"]))))
    (("name" . "request_user_input")
     ("description" . "Pause execution and ask the user a question. Use this when you need clarification, confirmation, or a choice from the user before proceeding. The session will pause until the user responds.

EXAMPLES:
- Clarification: \"Which file do you want me to modify: config.el or init.el?\"
- Confirmation: \"This will delete 15 files. Proceed?\" with options [\"Yes\", \"No\"]
- Choice: \"How should I format the output?\" with options [\"Table\", \"List\", \"JSON\"]

IMPORTANT: After calling this tool, the session pauses. You will receive the user's response in the next API call continuation.")
     ("input_schema" . (("type" . "object")
                       ("properties" . (("question" . (("type" . "string")
                                                       ("description" . "The question to ask the user")))
                                       ("options" . (("type" . "array")
                                                    ("items" . (("type" . "string")))
                                                    ("description" . "Optional list of choices. If provided, user picks one. If omitted, user provides free-form input.")))))
                       ("required" . ["question"]))))
   (("name" . "confirm_action")
    ("description" . "Request explicit user confirmation before a destructive or important operation. Use this INSTEAD of request_user_input when confirming a specific action.

SEVERITY LEVELS:
- info: Simple y/n prompt for low-risk confirmations
- warning: Yellow highlighting, shows details list, uses completing-read
- danger: Red highlighting, requires typing 'yes' explicitly (not just y/n)

EXAMPLES:
- Delete files: action=\"Delete 15 files permanently\" severity=\"danger\" details=[\"file1.txt\",...]
- Modify config: action=\"Update production config\" severity=\"warning\"
- Create directory: action=\"Create src/utils/ directory\" severity=\"info\"

The session PAUSES until user responds. On timeout, treated as rejection.")
    ("input_schema" . (("type" . "object")
                      ("properties" . (("action" . (("type" . "string")
                                                    ("description" . "Short description of what will happen")))
                                      ("details" . (("type" . "array")
                                                   ("items" . (("type" . "string")))
                                                   ("description" . "Optional list of specific items affected (files, settings, etc.)")))
                                      ("severity" . (("type" . "string")
                                                    ("enum" . ["info" "warning" "danger"])
                                                    ("description" . "Severity level: info (y/n), warning (highlighting), danger (type 'yes')")))
                                      ("options" . (("type" . "array")
                                                   ("items" . (("type" . "string")))
                                                   ("description" . "Custom choices (default: ['Yes', 'No'])")))
                                      ("timeout_seconds" . (("type" . "number")
                                                           ("description" . "How long to wait before auto-rejecting (default: 300)")))))
                      ("required" . ["action"]))))
   ;; Checkpoint tools - Phase 3: Workflow Enhancement
   (("name" . "checkpoint")
    ("description" . "Create a restore point before risky operations. Uses git stash internally.

EXAMPLES:
- Before refactoring: checkpoint description=\"Before refactoring auth module\"
- Before bulk changes: checkpoint description=\"Before renaming all variables\"

Returns checkpoint_id which can be used with restore_checkpoint to undo changes.")
    ("input_schema" . (("type" . "object")
                      ("properties" . (("description" . (("type" . "string")
                                                         ("description" . "What operation we're about to do (required)")))))
                      ("required" . ["description"]))))
   (("name" . "restore_checkpoint")
    ("description" . "Restore from a previous checkpoint created by the checkpoint tool.

EXAMPLES:
- Undo all changes: restore_checkpoint checkpoint_id=\"efrit-20251125-123456-abc123\"
- Keep checkpoint after restore: restore_checkpoint checkpoint_id=\"...\" keep_checkpoint=true

If the refactoring went wrong, this undoes all changes back to the checkpoint.")
    ("input_schema" . (("type" . "object")
                      ("properties" . (("checkpoint_id" . (("type" . "string")
                                                           ("description" . "ID of checkpoint to restore (required)")))
                                      ("keep_checkpoint" . (("type" . "boolean")
                                                           ("description" . "If true, don't delete checkpoint after restore (default: false)")))))
                      ("required" . ["checkpoint_id"]))))
   (("name" . "list_checkpoints")
    ("description" . "List all available checkpoints. Shows checkpoint IDs, descriptions, and creation times.")
    ("input_schema" . (("type" . "object")
                      ("properties" . ()))))
   (("name" . "delete_checkpoint")
    ("description" . "Delete a checkpoint without restoring it. Use when you're satisfied with changes and don't need the safety net.")
    ("input_schema" . (("type" . "object")
                      ("properties" . (("checkpoint_id" . (("type" . "string")
                                                           ("description" . "ID of checkpoint to delete (required)")))))
                      ("required" . ["checkpoint_id"]))))
   ;; Diff preview tool - Phase 3: Workflow Enhancement
   (("name" . "show_diff_preview")
    ("description" . "Show the user proposed changes in a diff view before applying them. Use this when you want to preview multiple file changes and let the user approve, reject, or selectively apply them.

EXAMPLES:
- Refactoring: Show all files that will change before renaming a function
- Code generation: Preview new files before creating them
- Bulk edits: Show diff of all affected files before making changes

The user sees a unified diff view and can:
- Approve all changes
- Reject all changes
- In selective mode: Choose which changes to apply

IMPORTANT: Session PAUSES until user responds.")
    ("input_schema" . (("type" . "object")
                      ("properties" . (("changes" . (("type" . "array")
                                                     ("items" . (("type" . "object")
                                                                 ("properties" . (("file" . (("type" . "string")))
                                                                                 ("old_content" . (("type" . "string")))
                                                                                 ("new_content" . (("type" . "string")))))))
                                                     ("description" . "List of changes. Each has: file (path), old_content (current, or null for new file), new_content (proposed, or null for deletion)")))
                                      ("description" . (("type" . "string")
                                                        ("description" . "What these changes accomplish")))
                                      ("apply_mode" . (("type" . "string")
                                                      ("enum" . ["all_or_nothing" "selective"])
                                                      ("description" . "all_or_nothing (default): approve/reject all. selective: user picks which changes")))))
                      ("required" . ["changes"]))))
   ;; Web search tool - Phase 4: External Knowledge
   (("name" . "web_search")
    ("description" . "Search the web for documentation, solutions, and examples. Use this when you need to look up information, find how to do something in Emacs, or research a problem.

EXAMPLES:
- Documentation: query=\"emacs company-mode configuration\"
- How-to: query=\"how to parse json in elisp\"
- Site-specific: query=\"magit stage hunks\" site=\"emacs.stackexchange.com\"

PRIVACY: Search queries are sent to an external search engine.
Do not include sensitive user data in queries.

RATE LIMITED: Max 10 searches per session by default.
REQUIRES USER CONSENT on first use in session.")
    ("input_schema" . (("type" . "object")
                      ("properties" . (("query" . (("type" . "string")
                                                   ("description" . "Search terms (required)")))
                                      ("site" . (("type" . "string")
                                                 ("description" . "Optional site restriction (e.g., 'emacs.stackexchange.com')")))
                                      ("max_results" . (("type" . "number")
                                                        ("description" . "Maximum results to return (default: 5)")))
                                      ("type" . (("type" . "string")
                                                ("enum" . ["general" "docs" "code"])
                                                ("description" . "Search type hint (default: general)")))))
                      ("required" . ["query"]))))
   (("name" . "fetch_url")
    ("description" . "Retrieve content from a specific URL. Use this to fetch documentation pages, README files, or other web content found via web_search.

EXAMPLES:
- Fetch docs: url=\"https://www.gnu.org/software/emacs/manual/html_node/elisp/index.html\"
- GitHub README: url=\"https://raw.githubusercontent.com/user/repo/main/README.md\" format=\"text\"
- Extract section: url=\"https://emacs.stackexchange.com/q/12345\" selector=\".answer\"

SECURITY: By default, only fetches from allowed domains (gnu.org, github.com, stackexchange.com, etc.).
Configure via efrit-fetch-url-security-level and efrit-fetch-url-allowed-domains.")
    ("input_schema" . (("type" . "object")
                      ("properties" . (("url" . (("type" . "string")
                                                 ("description" . "URL to fetch (required)")))
                                      ("selector" . (("type" . "string")
                                                     ("description" . "Optional CSS selector to extract specific content (#id, .class, or tag)")))
                                      ("format" . (("type" . "string")
                                                  ("enum" . ["text" "markdown" "html"])
                                                  ("description" . "Output format (default: markdown)")))
                                      ("max_length" . (("type" . "number")
                                                       ("description" . "Max content length in chars (default: 50000)")))))
                      ("required" . ["url"]))))
   ;; Phase 1/2: Codebase Exploration Tools
   (("name" . "project_files")
    ("description" . "List files in the project directory. Uses git ls-files for git repos (fast, respects .gitignore), falls back to directory traversal otherwise.

EXAMPLES:
- List all: project_files (no args, shows project root)
- Filter by glob: project_files pattern=\"*.el\"
- Specific dir: project_files path=\"lisp/\"
- Include hidden: project_files include_hidden=true

Returns: File paths with metadata (size, mtime, relative paths).")
    ("input_schema" . (("type" . "object")
                      ("properties" . (("path" . (("type" . "string")
                                                  ("description" . "Directory to scan (default: project root)")))
                                      ("pattern" . (("type" . "string")
                                                    ("description" . "Glob pattern filter (e.g., \"*.el\")")))
                                      ("max_depth" . (("type" . "number")
                                                      ("description" . "Max directory depth (default: 5)")))
                                      ("include_hidden" . (("type" . "boolean")
                                                           ("description" . "Include dotfiles (default: false)")))
                                      ("max_files" . (("type" . "number")
                                                      ("description" . "Max files to return (default: 500)")))
                                      ("offset" . (("type" . "number")
                                                   ("description" . "Pagination offset (default: 0)")))))
                      ("required" . []))))
   (("name" . "search_content")
    ("description" . "Search for content across the codebase. Uses ripgrep when available for best performance.

EXAMPLES:
- Simple search: search_content pattern=\"defun my-function\"
- Regex search: search_content pattern=\"defun.*test\" is_regex=true
- Filter files: search_content pattern=\"TODO\" file_pattern=\"*.el\"
- Case sensitive: search_content pattern=\"MyClass\" case_sensitive=true

Returns: Matches with file path, line number, column, content, and context lines.")
    ("input_schema" . (("type" . "object")
                      ("properties" . (("pattern" . (("type" . "string")
                                                     ("description" . "Search pattern (required)")))
                                      ("is_regex" . (("type" . "boolean")
                                                     ("description" . "Treat pattern as regex (default: false)")))
                                      ("path" . (("type" . "string")
                                                 ("description" . "Search scope directory (default: project root)")))
                                      ("file_pattern" . (("type" . "string")
                                                         ("description" . "Glob filter for files (e.g., \"*.el\")")))
                                      ("context_lines" . (("type" . "number")
                                                          ("description" . "Lines before/after match (default: 2)")))
                                      ("max_results" . (("type" . "number")
                                                        ("description" . "Max results (default: 50)")))
                                      ("case_sensitive" . (("type" . "boolean")
                                                           ("description" . "Case sensitive search (default: false)")))
                                      ("offset" . (("type" . "number")
                                                   ("description" . "Pagination offset (default: 0)")))))
                      ("required" . ["pattern"]))))
   (("name" . "read_file")
    ("description" . "Read a file and return its contents with metadata. Supports line ranges for large files.

EXAMPLES:
- Full file: read_file path=\"lisp/efrit.el\"
- Line range: read_file path=\"init.el\" start_line=100 end_line=150
- With encoding: read_file path=\"data.txt\" encoding=\"latin-1\"

Returns: File content, encoding, size, line counts. Binary files return metadata only.")
    ("input_schema" . (("type" . "object")
                      ("properties" . (("path" . (("type" . "string")
                                                  ("description" . "File path to read (required)")))
                                      ("start_line" . (("type" . "number")
                                                       ("description" . "Start line (1-indexed, optional)")))
                                      ("end_line" . (("type" . "number")
                                                     ("description" . "End line (inclusive, optional)")))
                                      ("encoding" . (("type" . "string")
                                                     ("description" . "Encoding override (optional)")))
                                      ("max_size" . (("type" . "number")
                                                     ("description" . "Max bytes to read (default: 100000)")))))
                      ("required" . ["path"]))))
   (("name" . "file_info")
    ("description" . "Get metadata about files without reading contents. Useful for checking existence, size, type before reading.

EXAMPLES:
- Single file: file_info paths=\"config.el\"
- Multiple files: file_info paths=[\"a.el\", \"b.el\", \"c.el\"]

Returns: exists, size, mtime, permissions, is_binary, is_directory, first_line peek.")
    ("input_schema" . (("type" . "object")
                      ("properties" . (("paths" . (("type" . "array")
                                                   ("items" . (("type" . "string")))
                                                   ("description" . "File paths to query (required)")))))
                      ("required" . ["paths"]))))
   (("name" . "vcs_status")
    ("description" . "Get the current git repository status.

Returns: current branch, upstream tracking info, staged/unstaged/untracked files, stash count, recent commits, and special states (rebasing, merging, etc.).")
    ("input_schema" . (("type" . "object")
                      ("properties" . (("path" . (("type" . "string")
                                                  ("description" . "Repository path (default: project root)")))))
                      ("required" . []))))
   (("name" . "vcs_diff")
    ("description" . "Get diff output for repository changes.

EXAMPLES:
- Unstaged changes: vcs_diff
- Staged changes: vcs_diff staged=true
- Against commit: vcs_diff commit=\"HEAD~3\"
- Specific file: vcs_diff path=\"lisp/efrit.el\"

Returns: Unified diff output with file statistics (insertions/deletions per file).")
    ("input_schema" . (("type" . "object")
                      ("properties" . (("path" . (("type" . "string")
                                                  ("description" . "File or directory to diff (default: all)")))
                                      ("staged" . (("type" . "boolean")
                                                   ("description" . "Show staged changes only (default: false)")))
                                      ("commit" . (("type" . "string")
                                                   ("description" . "Diff against specific commit")))
                                      ("context_lines" . (("type" . "number")
                                                          ("description" . "Lines of context (default: 3)")))))
                      ("required" . []))))
   (("name" . "vcs_log")
    ("description" . "Get commit history.

EXAMPLES:
- Recent commits: vcs_log
- File history: vcs_log path=\"lisp/efrit.el\"
- By author: vcs_log author=\"steve\"
- Search messages: vcs_log grep=\"fix bug\"

Returns: Commit list with hash, author, date, message.")
    ("input_schema" . (("type" . "object")
                      ("properties" . (("path" . (("type" . "string")
                                                  ("description" . "File or directory for filtered history")))
                                      ("count" . (("type" . "number")
                                                  ("description" . "Number of commits (default: 10)")))
                                      ("since" . (("type" . "string")
                                                  ("description" . "Date filter (e.g., '1 week ago')")))
                                      ("author" . (("type" . "string")
                                                   ("description" . "Author filter")))
                                      ("grep" . (("type" . "string")
                                                 ("description" . "Commit message search")))))
                      ("required" . []))))
   (("name" . "vcs_blame")
    ("description" . "Get line-by-line code attribution via git blame.

EXAMPLES:
- Full file: vcs_blame path=\"lisp/efrit.el\"
- Line range: vcs_blame path=\"efrit.el\" start_line=100 end_line=150

Returns: Per-line commit info (hash, author, date, message) with content.")
    ("input_schema" . (("type" . "object")
                      ("properties" . (("path" . (("type" . "string")
                                                  ("description" . "File to blame (required)")))
                                      ("start_line" . (("type" . "number")
                                                       ("description" . "Range start (1-indexed)")))
                                      ("end_line" . (("type" . "number")
                                                     ("description" . "Range end (inclusive)")))))
                      ("required" . ["path"]))))
   (("name" . "elisp_docs")
    ("description" . "Look up Emacs Lisp documentation. Returns structured data without opening help buffers.

EXAMPLES:
- Single symbol: elisp_docs symbol=\"defun\"
- Multiple: elisp_docs symbol=[\"defun\", \"defvar\", \"defmacro\"]
- With source: elisp_docs symbol=\"find-file\" include_source=true
- Related symbols: elisp_docs symbol=\"buffer\" related=true

Returns: Symbol type, docstring, signature (for functions), source location, related symbols.")
    ("input_schema" . (("type" . "object")
                      ("properties" . (("symbol" . (("type" . "string")
                                                    ("description" . "Symbol name or list of names (required)")))
                                      ("type" . (("type" . "string")
                                                 ("enum" . ["auto" "function" "variable" "face"])
                                                 ("description" . "Symbol type (default: auto-detect)")))
                                      ("include_source" . (("type" . "boolean")
                                                           ("description" . "Include source file/line (default: false)")))
                                      ("related" . (("type" . "boolean")
                                                    ("description" . "Include related symbols (default: false)")))))
                      ("required" . ["symbol"]))))
   (("name" . "set_project_root")
    ("description" . "Set the project root directory explicitly. CALL THIS FIRST if you need to work with files but the project context is unclear or default-directory is wrong (e.g., in daemon mode).

EXAMPLES:
- Set project: set_project_root path=\"~/src/myproject\"
- Clear and auto-detect: set_project_root path=\"\"

This affects all file operations (project_files, search_content, read_file, etc.) which resolve relative paths against the project root.

Returns: The normalized project root path, or error if path doesn't exist.")
    ("input_schema" . (("type" . "object")
                      ("properties" . (("path" . (("type" . "string")
                                                  ("description" . "Absolute path to project root, or empty string to clear and auto-detect")))))
                      ("required" . ["path"]))))]
                      "Schema definition for all available tools in efrit-do mode.")

(defun efrit-do--get-current-tools-schema (&optional budget)
  "Return full tool schema, optionally with budget hints.
If BUDGET is provided (an efrit-budget struct), inject budget hints
into tool descriptions."
  (if budget
      (efrit-do--inject-budget-hints efrit-do--tools-schema budget)
    efrit-do--tools-schema))

;;; Circuit Breaker Implementation

(defun efrit-do--circuit-breaker-reset ()
  "Reset circuit breaker state for a new session."
  (setq efrit-do--session-tool-count 0)
  (setq efrit-do--circuit-breaker-tripped nil)
  (setq efrit-do--last-tool-called nil)
  (setq efrit-do--tool-call-count 0)
  (setq efrit-do--last-tool-input nil)
  (setq efrit-do--identical-call-count 0)
  ;; Reset error loop detection state
  (efrit-do--error-loop-reset))

(defun efrit-do--hash-tool-input (tool-input)
  "Create a hash string for TOOL-INPUT to detect identical calls.
For hash tables, sorts keys to ensure consistent ordering."
  (let ((normalized (efrit-do--normalize-for-hash tool-input)))
    (secure-hash 'md5 (format "%S" normalized))))

(defun efrit-do--normalize-for-hash (obj)
  "Normalize OBJ for consistent hashing, handling hash table key order.
Converts hash tables to sorted alists for order-independent comparison."
  (cond
   ((hash-table-p obj)
    ;; Convert to alist with sorted keys for consistent ordering
    (let ((pairs '()))
      (maphash (lambda (k v)
                 (push (cons k (efrit-do--normalize-for-hash v)) pairs))
               obj)
      (sort pairs (lambda (a b)
                    (string< (format "%S" (car a))
                            (format "%S" (car b)))))))
   ((vectorp obj)
    (vconcat (mapcar #'efrit-do--normalize-for-hash obj)))
   ((listp obj)
    (mapcar #'efrit-do--normalize-for-hash obj))
   (t obj)))

;;; Error Loop Detection Functions

(defun efrit-do--hash-error-message (error-msg)
  "Create a hash string for ERROR-MSG to detect repeated errors.
Normalizes the error message to ignore minor variations like timestamps
or memory addresses that might differ between identical errors."
  (when error-msg
    (let ((normalized (replace-regexp-in-string
                       ;; Remove hex addresses like #<buffer 0x12345>
                       "#<[^>]+>"
                       "#<normalized>"
                       (replace-regexp-in-string
                        ;; Remove line numbers that might vary
                        "line [0-9]+"
                        "line N"
                        error-msg))))
      (secure-hash 'md5 normalized))))

(defun efrit-do--error-loop-reset ()
  "Reset error loop detection state for a new session."
  (setq efrit-do--last-error-hash nil)
  (setq efrit-do--same-error-count 0)
  (setq efrit-do--error-history nil))

(defun efrit-do--error-loop-record (error-msg)
  "Record ERROR-MSG for error loop detection.
Returns (WARNING-P . MESSAGE) where WARNING-P indicates an error loop detected."
  (when (and error-msg (stringp error-msg))
    (let* ((error-hash (efrit-do--hash-error-message error-msg))
           (is-same-error (and error-hash
                               efrit-do--last-error-hash
                               (equal error-hash efrit-do--last-error-hash))))
      ;; Update tracking state
      (if is-same-error
          (setq efrit-do--same-error-count (1+ efrit-do--same-error-count))
        (setq efrit-do--same-error-count 1))
      (setq efrit-do--last-error-hash error-hash)

      ;; Add to history (keep last 10)
      (push (cons error-hash (substring error-msg 0 (min 100 (length error-msg))))
            efrit-do--error-history)
      (when (> (length efrit-do--error-history) 10)
        (setq efrit-do--error-history (seq-take efrit-do--error-history 10)))

      ;; Check for error loop conditions
      (cond
       ;; Auto-complete threshold reached - trip the circuit breaker
       ((and efrit-do-error-loop-auto-complete
             (>= efrit-do--same-error-count efrit-do-error-loop-auto-complete))
        (setq efrit-do--circuit-breaker-tripped
              (format "Error loop detected: Same error occurred %d times. Error: %s"
                      efrit-do--same-error-count
                      (substring error-msg 0 (min 150 (length error-msg)))))
        (efrit-log 'error "Error loop auto-complete: %s" efrit-do--circuit-breaker-tripped)
        (cons 'auto-complete efrit-do--circuit-breaker-tripped))

       ;; Warning threshold reached - inject guidance
       ((>= efrit-do--same-error-count efrit-do-max-same-error-occurrences)
        (let ((warning (format "[ERROR LOOP DETECTED: This error has occurred %d times. The same approach keeps failing. You MUST try a COMPLETELY DIFFERENT approach. Consider:\n- Different function or API\n- Alternative algorithm\n- Simplified approach\n- Ask user for clarification\nDO NOT retry the same code pattern.]"
                               efrit-do--same-error-count)))
          (efrit-log 'warn "Error loop warning: error occurred %d times" efrit-do--same-error-count)
          (cons 'warning warning)))

       ;; No loop detected yet
       (t (cons nil nil))))))

(defun efrit-do--error-loop-check-result (result)
  "Check RESULT for errors and track for loop detection.
Returns (MODIFIED-P . NEW-RESULT) where MODIFIED-P indicates if result
was modified with an error loop warning."
  (let* ((error-info (efrit-do--extract-error-info result))
         (is-error (car error-info))
         (error-msg (cdr error-info)))
    (if (not is-error)
        ;; No error - reset the same-error counter (but keep last hash)
        (progn
          (setq efrit-do--same-error-count 0)
          (cons nil result))
      ;; Error detected - record and check for loop
      (let ((loop-result (efrit-do--error-loop-record error-msg)))
        (pcase (car loop-result)
          ('auto-complete
           ;; Session should be terminated
           (cons t (concat result "\n\n" (cdr loop-result))))
          ('warning
           ;; Inject warning into result
           (cons t (concat result "\n\n" (cdr loop-result))))
          (_
           ;; No loop detected
           (cons nil result)))))))

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

   "SESSION COMPLETION:\n"
   "- When code executes successfully, call session_complete\n"
   "- If the work log shows the task was accomplished, call session_complete\n"
   "- Don't re-execute code that already succeeded\n\n"

   "TASK MANAGEMENT WITH todo_write:\n"
   "- Use todo_write to track multi-step tasks (3+ steps)\n"
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
              (format "\n[Executed: %s]\n[Result: %s]"
                      input-str
                      eval-result))
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

(defun efrit-do--handle-todo-write (tool-input)
  "Handle todo_write tool - replaces entire TODO list.
TOOL-INPUT is a hash table with a `todos' array.
Each todo has content, status, and activeForm.

This is a Pure Executor implementation - it simply stores what
Claude sends without validation or state machine logic."
  (let* ((todos-array (when (hash-table-p tool-input)
                        (gethash "todos" tool-input)))
         (new-todos nil))
    ;; Convert the input array to our internal TODO item format
    (when (and todos-array (> (length todos-array) 0))
      (let ((counter 0))
        (seq-doseq (todo-data todos-array)
          (let* ((content (gethash "content" todo-data ""))
                 (status-str (gethash "status" todo-data "pending"))
                 ;; activeForm is stored but not yet used (for future UI display)
                 (_active-form (gethash "activeForm" todo-data content))
                 (status (intern status-str))
                 ;; Map status strings to internal symbols
                 (internal-status (cond
                                   ((eq status 'pending) 'todo)
                                   ((eq status 'in_progress) 'in-progress)
                                   ((eq status 'completed) 'completed)
                                   (t 'todo)))
                 (todo-id (format "todo-%d" (cl-incf counter)))
                 (todo (efrit-do-todo-item-create
                        :id todo-id
                        :content content
                        :status internal-status
                        :priority 'medium
                        :created-at (current-time)
                        :completed-at (when (eq internal-status 'completed)
                                        (current-time)))))
            ;; activeForm will be used by *efrit-todos* buffer (ef-kyl)
            (push todo new-todos)))))

    ;; Replace the entire TODO list (nreverse is destructive, so capture result)
    (setq efrit-do--current-todos (nreverse new-todos))
    (setq efrit-do--todo-counter (length efrit-do--current-todos))

    ;; Return a summary
    (let* ((total (length efrit-do--current-todos))
           (pending (seq-count (lambda (todo)
                                 (eq (efrit-do-todo-item-status todo) 'todo))
                               efrit-do--current-todos))
           (in-progress (seq-count (lambda (todo)
                                     (eq (efrit-do-todo-item-status todo) 'in-progress))
                                   efrit-do--current-todos))
           (completed (seq-count (lambda (todo)
                                   (eq (efrit-do-todo-item-status todo) 'completed))
                                 efrit-do--current-todos))
           ;; Find the current in-progress task
           (current-task (seq-find (lambda (todo)
                                     (eq (efrit-do-todo-item-status todo) 'in-progress))
                                   efrit-do--current-todos)))
      (efrit-log 'debug "todo_write: stored %d todos (%d pending, %d in-progress, %d completed)"
                 total pending in-progress completed)
      (format "\n[TODO list updated: %d total (%d pending, %d in-progress, %d completed)%s]"
              total pending in-progress completed
              (if current-task
                  (format "\nCurrent task: %s" (efrit-do-todo-item-content current-task))
                "")))))

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

(defcustom efrit-do-glob-files-max-results 1000
  "Maximum number of files to return from glob_files.
Prevents runaway searches from overwhelming the system."
  :type 'integer
  :group 'efrit-do)

(defun efrit-do--handle-glob-files (tool-input)
  "Handle glob_files tool to list files matching pattern.
Includes safety limits to prevent hanging on large directories."
  ;; Validate input - must be a hash table with required 'pattern' field
  (if (not (hash-table-p tool-input))
      "\n[Error: glob_files requires a hash table input with 'pattern' and 'extension' fields]"
    (let* ((pattern (gethash "pattern" tool-input))
           (extension (gethash "extension" tool-input))
           (recursive (gethash "recursive" tool-input t)))

      ;; Validate required fields
      (cond
       ((or (null pattern) (string-empty-p pattern))
        "\n[Error: glob_files requires 'pattern' field (directory path)]")

       ((or (null extension) (string-empty-p extension))
        "\n[Error: glob_files requires 'extension' field (e.g., 'el' or 'py,js' or '*')]")

       ;; Reject dangerous patterns that could hang Emacs
       ((and recursive
             (member (expand-file-name pattern)
                     (list (expand-file-name "~")
                           (expand-file-name "/")
                           (expand-file-name "~/"))))
        (format "\n[Error: Recursive search from '%s' not allowed - too broad. Please specify a more specific directory.]" pattern))

       (t
        (condition-case err
            (let* ((expanded-pattern (expand-file-name pattern))
                   (extensions (if (string= extension "*")
                                  '("")
                                (split-string extension ",")))
                   (all-files '())
                   (file-limit efrit-do-glob-files-max-results)
                   (limit-reached nil))

              ;; Verify directory exists
              (unless (file-directory-p expanded-pattern)
                (error "Directory not found: %s" expanded-pattern))

              (catch 'limit-reached
                (dolist (ext extensions)
                  (let* ((ext-clean (string-trim ext))
                         (files (if (string= ext-clean "")
                                   (if recursive
                                       (directory-files-recursively expanded-pattern ".*")
                                     (directory-files expanded-pattern t "^[^.]"))
                                 (if recursive
                                     (directory-files-recursively
                                      expanded-pattern
                                      (format "\\.%s$" (regexp-quote ext-clean)))
                                   (file-expand-wildcards
                                    (format "%s/*.%s" expanded-pattern ext-clean))))))
                    (setq all-files (append all-files files))
                    ;; Check limit
                    (when (> (length all-files) file-limit)
                      (setq limit-reached t)
                      (setq all-files (seq-take all-files file-limit))
                      (throw 'limit-reached t)))))

              (let ((file-count (length all-files))
                    (preview (if (> (length all-files) 10)
                                (append (seq-take all-files 8)
                                       (list (format "... and %d more files" (- (length all-files) 8))))
                              all-files)))
                (format "\n[Found %d files%s in %s:\n%s]"
                        file-count
                        (if limit-reached
                            (format " (limit %d reached)" file-limit)
                          "")
                        pattern
                        (mapconcat #'identity preview "\n"))))
          (error
           (format "\n[Error finding files: %s]" (error-message-string err)))))))))

(defun efrit-do--handle-request-user-input (tool-input)
  "Handle request_user_input tool to pause and ask user a question.
Sets the session to waiting-for-user state and emits a progress event."
  (require 'efrit-session)
  (require 'efrit-progress)
  (if (not (hash-table-p tool-input))
      "\n[Error: request_user_input requires a hash table input with 'question' field]"
    (let* ((question (gethash "question" tool-input))
           (options (gethash "options" tool-input))
           (session (efrit-session-active)))
      (cond
       ((or (null question) (string-empty-p question))
        "\n[Error: request_user_input requires 'question' field]")

       ((not session)
        "\n[Error: request_user_input requires an active session]")

       (t
        ;; Set pending question on session
        (efrit-session-set-pending-question
         session question
         (when options (append options nil)))  ; Convert vector to list

        ;; Emit progress event
        (efrit-progress-show-message
         (format "🤔 Question for user: %s" question)
         'claude)

        ;; Return a special marker that the executor can recognize
        (format "\n[WAITING-FOR-USER]\nQuestion: %s%s\n\nSession paused. User response required before continuing."
                question
                (if options
                    (format "\nOptions: %s" (mapconcat #'identity options ", "))
                  "")))))))

(defun efrit-do--handle-confirm-action (tool-input)
  "Handle confirm_action tool to get user confirmation for destructive operations.
Prompts user synchronously and returns confirmation result as JSON."
  (require 'efrit-tool-confirm-action)
  (if (not (hash-table-p tool-input))
      "\n[Error: confirm_action requires a hash table input with 'action' field]"
    (let* ((action (gethash "action" tool-input))
           (details (gethash "details" tool-input))
           (severity (gethash "severity" tool-input))
           (options (gethash "options" tool-input))
           (timeout (gethash "timeout_seconds" tool-input)))
      (cond
       ((or (null action) (string-empty-p action))
        "\n[Error: confirm_action requires 'action' field]")
       (t
        ;; Build args alist for the tool function
        (let* ((args `((action . ,action)
                       ,@(when details `((details . ,details)))
                       ,@(when severity `((severity . ,severity)))
                       ,@(when options `((options . ,options)))
                       ,@(when timeout `((timeout_seconds . ,timeout)))))
               (result (efrit-tool-confirm-action args)))
          ;; Return JSON-encoded result for Claude to process
          (format "\n[Confirmation Result]\n%s" (json-encode result))))))))

;;; Tool Handler Helpers - Reduce boilerplate in handler definitions
;;
;; Tool handlers follow a repetitive pattern:
;; 1. require the tool module
;; 2. validate hash-table input
;; 3. extract required/optional fields
;; 4. build alist
;; 5. call tool function
;; 6. format result as JSON
;;
;; These helper functions reduce boilerplate while keeping handlers readable.

(defun efrit-do--validate-hash-table (tool-input tool-name)
  "Validate TOOL-INPUT is a hash-table for TOOL-NAME.
Returns error string if invalid, nil if valid."
  (unless (hash-table-p tool-input)
    (format "\n[Error: %s requires a hash table input]" tool-name)))

(defun efrit-do--validate-required (tool-input tool-name field)
  "Validate required FIELD exists in TOOL-INPUT for TOOL-NAME.
Returns error string if invalid, nil if valid."
  (let ((val (gethash field tool-input)))
    (when (or (null val) (and (stringp val) (string-empty-p val)))
      (format "\n[Error: %s requires '%s' field]" tool-name field))))

(defun efrit-do--extract-fields (tool-input field-specs)
  "Extract fields from TOOL-INPUT according to FIELD-SPECS.
FIELD-SPECS is a list of (json-key . transform-fn) or just json-key strings.
Returns an alist with SYMBOL keys suitable for passing to tool functions.
Nil values and nil results from transforms are filtered out."
  (delq nil
        (mapcar (lambda (spec)
                  (let* ((key (if (consp spec) (car spec) spec))
                         (transform (if (consp spec) (cdr spec) #'identity))
                         (val (gethash key tool-input))
                         ;; Convert string keys to symbols for alist-get compatibility
                         (sym-key (if (stringp key) (intern key) key)))
                    (when val
                      (cons sym-key (funcall transform val)))))
                field-specs)))

(defun efrit-do--format-tool-result (result label)
  "Format tool RESULT with LABEL for return to Claude."
  (format "\n[%s]\n%s" label (json-encode result)))

(defun efrit-do--vector-to-list (val)
  "Convert VAL from vector to list if needed."
  (if (vectorp val) (append val nil) val))

;;; Checkpoint Tool Handlers - Phase 3: Workflow Enhancement

(defun efrit-do--handle-checkpoint (tool-input)
  "Handle checkpoint tool to create a restore point before risky operations."
  (require 'efrit-tool-checkpoint)
  (if (not (hash-table-p tool-input))
      "\n[Error: checkpoint requires a hash table input with 'description' field]"
    (let ((description (gethash "description" tool-input)))
      (if (or (null description) (string-empty-p description))
          "\n[Error: checkpoint requires 'description' field]"
        (let* ((args `((description . ,description)))
               (result (efrit-tool-checkpoint args)))
          (format "\n[Checkpoint Result]\n%s" (json-encode result)))))))

(defun efrit-do--handle-restore-checkpoint (tool-input)
  "Handle restore_checkpoint tool to restore from a previous checkpoint."
  (require 'efrit-tool-checkpoint)
  (if (not (hash-table-p tool-input))
      "\n[Error: restore_checkpoint requires a hash table input with 'checkpoint_id' field]"
    (let ((checkpoint-id (gethash "checkpoint_id" tool-input))
          (keep-checkpoint (gethash "keep_checkpoint" tool-input)))
      (if (or (null checkpoint-id) (string-empty-p checkpoint-id))
          "\n[Error: restore_checkpoint requires 'checkpoint_id' field]"
        (let* ((args `((checkpoint_id . ,checkpoint-id)
                       ,@(when keep-checkpoint `((keep_checkpoint . ,keep-checkpoint)))))
               (result (efrit-tool-restore-checkpoint args)))
          (format "\n[Restore Result]\n%s" (json-encode result)))))))

(defun efrit-do--handle-list-checkpoints ()
  "Handle list_checkpoints tool to list all available checkpoints."
  (require 'efrit-tool-checkpoint)
  (let ((result (efrit-tool-list-checkpoints nil)))
    (format "\n[Checkpoints]\n%s" (json-encode result))))

(defun efrit-do--handle-delete-checkpoint (tool-input)
  "Handle delete_checkpoint tool to delete a checkpoint without restoring."
  (require 'efrit-tool-checkpoint)
  (if (not (hash-table-p tool-input))
      "\n[Error: delete_checkpoint requires a hash table input with 'checkpoint_id' field]"
    (let ((checkpoint-id (gethash "checkpoint_id" tool-input)))
      (if (or (null checkpoint-id) (string-empty-p checkpoint-id))
          "\n[Error: delete_checkpoint requires 'checkpoint_id' field]"
        (let* ((args `((checkpoint_id . ,checkpoint-id)))
               (result (efrit-tool-delete-checkpoint args)))
          (format "\n[Delete Result]\n%s" (json-encode result)))))))

(defun efrit-do--handle-show-diff-preview (tool-input)
  "Handle show_diff_preview tool to show proposed changes in a diff view."
  (require 'efrit-tool-show-diff-preview)
  (if (not (hash-table-p tool-input))
      "\n[Error: show_diff_preview requires a hash table input with 'changes' field]"
    (let ((changes (gethash "changes" tool-input))
          (description (gethash "description" tool-input))
          (apply-mode (gethash "apply_mode" tool-input)))
      (if (or (null changes) (zerop (length changes)))
          "\n[Error: show_diff_preview requires non-empty 'changes' array]"
        ;; Convert hash tables in changes array to alists for the tool
        (let* ((changes-list
                (mapcar (lambda (change)
                          (if (hash-table-p change)
                              `((file . ,(gethash "file" change))
                                (old_content . ,(gethash "old_content" change))
                                (new_content . ,(gethash "new_content" change)))
                            change))
                        (append changes nil)))  ; Convert vector to list
               (args `((changes . ,changes-list)
                       ,@(when description `((description . ,description)))
                       ,@(when apply-mode `((apply_mode . ,apply-mode)))))
               (result (efrit-tool-show-diff-preview args)))
          (format "\n[Diff Preview Result]\n%s" (json-encode result)))))))

;;; Web Search Tool Handler - Phase 4: External Knowledge

(defun efrit-do--handle-web-search (tool-input)
  "Handle web_search tool to search the web for information."
  (require 'efrit-tool-web-search)
  (or (efrit-do--validate-hash-table tool-input "web_search")
      (efrit-do--validate-required tool-input "web_search" "query")
      (let* ((args (efrit-do--extract-fields
                    tool-input '("query" "site" "max_results" "type")))
             (result (efrit-tool-web-search args)))
        (efrit-do--format-tool-result result "Web Search Result"))))

(defun efrit-do--handle-fetch-url (tool-input)
  "Handle fetch_url tool to retrieve content from a URL."
  (require 'efrit-tool-fetch-url)
  (or (efrit-do--validate-hash-table tool-input "fetch_url")
      (efrit-do--validate-required tool-input "fetch_url" "url")
      (let* ((args (efrit-do--extract-fields
                    tool-input '("url" "selector" "format" "max_length")))
             (result (efrit-tool-fetch-url args)))
        (efrit-do--format-tool-result result "Fetch URL Result"))))

;;; Phase 1/2: Codebase Exploration Tool Handlers

(defun efrit-do--handle-project-files (tool-input)
  "Handle project_files tool to list files in the project."
  (require 'efrit-tool-project-files)
  (let* ((args (when (hash-table-p tool-input)
                 (efrit-do--extract-fields
                  tool-input '("path" "pattern" "max_depth" "include_hidden"
                               "max_files" "offset"))))
         (result (efrit-tool-project-files args)))
    (efrit-do--format-tool-result result "Project Files Result")))

(defun efrit-do--handle-search-content (tool-input)
  "Handle search_content tool to search for content in the codebase."
  (require 'efrit-tool-search-content)
  (or (efrit-do--validate-hash-table tool-input "search_content")
      (efrit-do--validate-required tool-input "search_content" "pattern")
      (let* ((args (efrit-do--extract-fields
                    tool-input '("pattern" "is_regex" "path" "file_pattern"
                                 "context_lines" "max_results" "case_sensitive" "offset")))
             (result (efrit-tool-search-content args)))
        (efrit-do--format-tool-result result "Search Content Result"))))

(defun efrit-do--handle-read-file (tool-input)
  "Handle read_file tool to read a file's contents."
  (require 'efrit-tool-read-file)
  (or (efrit-do--validate-hash-table tool-input "read_file")
      (efrit-do--validate-required tool-input "read_file" "path")
      (let* ((args (efrit-do--extract-fields
                    tool-input '("path" "start_line" "end_line" "encoding" "max_size")))
             (result (efrit-tool-read-file args)))
        (efrit-do--format-tool-result result "Read File Result"))))

(defun efrit-do--handle-file-info (tool-input)
  "Handle file_info tool to get metadata about files."
  (require 'efrit-tool-file-info)
  (or (efrit-do--validate-hash-table tool-input "file_info")
      (efrit-do--validate-required tool-input "file_info" "paths")
      (let* ((args (efrit-do--extract-fields
                    tool-input `(("paths" . ,#'efrit-do--vector-to-list))))
             (result (efrit-tool-file-info args)))
        (efrit-do--format-tool-result result "File Info Result"))))

(defun efrit-do--handle-vcs-status (tool-input)
  "Handle vcs_status tool to get git repository status."
  (require 'efrit-tool-vcs-status)
  (let* ((args (when (hash-table-p tool-input)
                 (efrit-do--extract-fields tool-input '("path"))))
         (result (efrit-tool-vcs-status args)))
    (efrit-do--format-tool-result result "VCS Status Result")))

(defun efrit-do--handle-vcs-diff (tool-input)
  "Handle vcs_diff tool to get git diff output."
  (require 'efrit-tool-vcs-diff)
  (let* ((args (when (hash-table-p tool-input)
                 (efrit-do--extract-fields
                  tool-input '("path" "staged" "commit" "context_lines"))))
         (result (efrit-tool-vcs-diff args)))
    (efrit-do--format-tool-result result "VCS Diff Result")))

(defun efrit-do--handle-vcs-log (tool-input)
  "Handle vcs_log tool to get commit history."
  (require 'efrit-tool-vcs-log)
  (let* ((args (when (hash-table-p tool-input)
                 (efrit-do--extract-fields
                  tool-input '("path" "count" "since" "author" "grep"))))
         (result (efrit-tool-vcs-log args)))
    (efrit-do--format-tool-result result "VCS Log Result")))

(defun efrit-do--handle-vcs-blame (tool-input)
  "Handle vcs_blame tool to get line-by-line code attribution."
  (require 'efrit-tool-vcs-blame)
  (or (efrit-do--validate-hash-table tool-input "vcs_blame")
      (efrit-do--validate-required tool-input "vcs_blame" "path")
      (let* ((args (efrit-do--extract-fields
                    tool-input '("path" "start_line" "end_line")))
             (result (efrit-tool-vcs-blame args)))
        (efrit-do--format-tool-result result "VCS Blame Result"))))

(defun efrit-do--handle-set-project-root (tool-input)
  "Handle set_project_root tool to set the project context."
  (require 'efrit-tool-utils)
  (if (not (hash-table-p tool-input))
      "\n[Error: set_project_root requires a hash table input with 'path' field]"
    (let ((path (gethash "path" tool-input)))
      (cond
       ;; Empty path means clear and auto-detect
       ((or (null path) (string-empty-p path))
        (efrit-clear-project-root)
        (format "\n[Project root cleared. Now using auto-detection: %s]"
                (efrit-tool--get-project-root)))
       ;; Set explicit path
       (t
        (let ((result (efrit-set-project-root path)))
          (if result
              (format "\n[Project root set to: %s]" result)
            (format "\n[Error: Path does not exist: %s]" path))))))))

(defun efrit-do--handle-elisp-docs (tool-input)
  "Handle elisp_docs tool to look up Emacs Lisp documentation."
  (require 'efrit-tool-elisp-docs)
  (or (efrit-do--validate-hash-table tool-input "elisp_docs")
      (efrit-do--validate-required tool-input "elisp_docs" "symbol")
      (let* ((args (efrit-do--extract-fields
                    tool-input `(("symbol" . ,#'efrit-do--vector-to-list)
                                 "type" "include_source" "related")))
             (result (efrit-tool-elisp-docs args)))
        (efrit-do--format-tool-result result "Elisp Docs Result"))))

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
          "- COMPLEX TASKS (3+ steps): Use todo_write FIRST to show your plan, then execute\n"
          "- PROACTIVE RULE: If in doubt, use todo_write - visibility helps the user\n"
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

This uses the proper agentic loop that continues the conversation
until Claude calls session_complete or returns with stop_reason end_turn.

Returns the result string for programmatic use."
  (interactive
   (list (read-string "Command: " nil 'efrit-do-history)))

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
