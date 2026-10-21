;;; efrit-do-schema.el --- Tool schemas for efrit-do -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.12.12
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai
;; URL: https://github.com/stevey/efrit

;;; Commentary:
;; This file contains the tool schemas (JSON schema definitions) for efrit-do.
;; These schemas are sent to Claude to describe available tools.
;; Extracted from efrit-do.el for maintainability.
;;
;; Tool Naming Convention:
;;
;; CANONICAL: object_verb pattern (e.g., buffer_create, file_read, vcs_diff)
;; ALIASES: verb_object pattern (e.g., create_buffer, read_file) also work
;;
;; Both naming patterns are accepted via the dispatch table in efrit-do-dispatch.el.
;; Use the canonical object_verb pattern for new tools.
;;
;; Current aliases (for chat-mode compatibility):
;;   create_buffer -> buffer_create
;;   edit_buffer   -> (uses efrit-tool-edit-buffer directly)
;;   read_buffer   -> (uses efrit-tool-read-buffer directly)
;;   buffer_info   -> (uses efrit-tool-buffer-info directly)
;;   get_context   -> (uses efrit-tools-get-context directly)

;;; Code:

(require 'cl-lib)
(require 'efrit-tool-registry)

;; Note: Tool dispatch table is in efrit-do.el (maps tool names to handlers)
;; This file only contains the JSON schemas sent to Claude.

;; Forward declarations for optional budget integration
(declare-function efrit-budget-remaining "efrit-budget")
(declare-function efrit-budget-format-warning "efrit-budget")

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

;;; Tool Schema Definitions

(defconst efrit-do--tools-schema
  [(("name" . "eval_sexp")
  ("description" . "PRIMARY TOOL: Execute Elisp code directly. Use for simple tasks like opening files, buffer operations, single commands. For complex multi-step tasks (3+ steps), use todo_write to track progress.

EXAMPLES:
- Open files: (find-file \"/path/to/file.txt\")
- Open all images: (dolist (f (directory-files-recursively \"~/Pictures\" \"\\\\.\\\\(?:png\\\\|jpe?g\\\\)$\")) (find-file f))
- Create buffer: (switch-to-buffer (get-buffer-create \"*My Buffer*\"))
- Edit text: (with-current-buffer \"file.txt\" (goto-char (point-min)) (insert \"Hello\"))
- Navigate: (goto-line 50) or (goto-char (point-max))
- Search: (re-search-forward \"pattern\" nil t)

NEVER call synchronous user-input functions (read-string, y-or-n-p, yes-or-no-p, completing-read, read-char, etc.) - they freeze all of Emacs and are rejected with an error. To ask the user something, use the request_user_input tool instead.")
  ("input_schema" . (("type" . "object")
  ("properties" . (("expr" . (("type" . "string")
  ("description" . "The Elisp expression to evaluate")))))
  ("required" . ["expr"]))))
   (("name" . "run_program")
    ("description" . "Run ONE external program with an argument array, no shell. Prefer this over shell_exec for any single command: the user is asked about the program name alone, so a grant on it covers every later call, and there is no quoting, redirection or substitution to get wrong. Output (stdout and stderr) and the exit code come back; do the filtering, counting and looping yourself or in eval_sexp. Use stdin to feed input instead of `echo X | prog'. Give a longer timeout for slow programs.

EXAMPLES:
- {\"program\": \"tmux\", \"args\": [\"-L\", \"t\", \"list-keys\", \"-T\", \"prefix\"]}
- {\"program\": \"git\", \"args\": [\"status\", \"--short\"]}
- {\"program\": \"python3\", \"args\": [\"-\"], \"stdin\": \"print(2+2)\"}

NOT for searching, listing or reading files: use search_content, project_files, file_info, read_file (no permission prompt). NOT for Emacs operations: use eval_sexp.")
    ("input_schema" . (("type" . "object")
                       ("properties" . (("program" . (("type" . "string")
                                                      ("description" . "Executable name (looked up in PATH) or path")))
                                       ("args" . (("type" . "array")
                                                  ("items" . (("type" . "string")))
                                                  ("description" . "Arguments, one array element each, unquoted")))
                                       ("stdin" . (("type" . "string")
                                                   ("description" . "Text written to the program's standard input")))
                                       ("timeout" . (("type" . "integer")
                                                     ("description" . "Seconds before the program is killed (default 10, max 120)")))))
                       ("required" . ["program"]))))
   (("name" . "shell_exec")
   ("description" . "Execute a shell command line and return the result. ONLY when the user asked for the shell, or the line needs the shell itself (a pipeline between two programs the model cannot replace, a glob the program must receive expanded). For one program, use run_program: it asks the user less. NOT for searching, listing or reading files: use search_content, project_files, file_info, read_file (no permission prompt; a shell line asks the user). Avoid redirection, substitution, loops and `eval': each makes the line one the user must read and approve by hand.

EXAMPLES:
- Pipelines between programs: \"ps aux | grep emacs\"
- Build systems that want a shell: \"make build\"

DO NOT USE for Emacs operations like opening files - use eval_sexp instead.")
     ("input_schema" . (("type" . "object")
                       ("properties" . (("command" . (("type" . "string")
                                                      ("description" . "The shell command to execute")))))
                       ("required" . ["command"]))))
    (("name" . "buffer_create")
     ("description" . "Create a read-only report buffer for output too long for the conversation. Not shown automatically: the user opens it from the tool row. Short results belong in your reply instead.")
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
   ;; suggest_execution_mode removed - no handler implemented
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

IMPORTANT: After calling this tool, the session pauses. You will receive the user's response in the next API call continuation.

RULES: (1) If the user answers with a question of their own, answer it in text BEFORE asking again; a repeated ask without an answer is refused. (2) Never put choices in a final message as prose (\"1. ... 2. ... Which one?\"): the user gets no menu. Choices always go through this tool, with the options list. (3) Options are things only the user can decide; do not offer a plan you have already chosen as three variants of itself. (4) For a set the user picks several from (which items to adopt, which files to include), pass multi=true: the user gets toggles and RET, and the answer lists the chosen options separated by \"; \". Never ask the user to type numbers.")
     ("input_schema" . (("type" . "object")
                       ("properties" . (("question" . (("type" . "string")
                                                       ("description" . "The question to ask the user")))
                                       ("options" . (("type" . "array")
                                                    ("items" . (("type" . "string")))
                                                    ("description" . "Optional list of choices. If provided, user picks one (or several with multi). If omitted, user provides free-form input.")))
                                       ("multi" . (("type" . "boolean")
                                                  ("description" . "The user may choose several options (toggles + RET); the answer lists them separated by \"; \"")))))
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

The session PAUSES until the user responds, however long that takes; there is no timeout. If the result is an error, the prompt itself failed; the action was never started.")
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
                                                   ("description" . "Custom choices (default: ['Yes', 'No'])")))))
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
- Edit a change in ediff before approving. Then the result's user_edits lists {index, file, new_content}: apply THAT new_content for those changes, not your proposal.

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
   ;; Semantic navigation through Emacs's own machinery (efrit-tool-navigate)
   (("name" . "xref_references")
    ("description" . "Where a symbol is used, through the xref backend of the file's buffer (eglot, lsp-mode, etags, the elisp backend: whatever the user runs). Results are file:line: summary. Give file and line so the backend resolves the symbol in scope. Read-only.")
    ("input_schema" . (("type" . "object")
                      ("properties" . (("symbol" . (("type" . "string") ("description" . "The identifier")))
                                      ("file" . (("type" . "string") ("description" . "File whose backend answers (default: the user's buffer)")))
                                      ("line" . (("type" . "integer") ("description" . "1-based line to place point at first")))
                                      ("column" . (("type" . "integer") ("description" . "0-based column")))))
                      ("required" . ["symbol"]))))
   (("name" . "xref_apropos")
    ("description" . "Definitions whose names match a pattern, through the xref backend (words for elisp and LSP, a regexp for etags). Read-only.")
    ("input_schema" . (("type" . "object")
                      ("properties" . (("pattern" . (("type" . "string")))
                                      ("file" . (("type" . "string") ("description" . "File whose backend answers (default: the user's buffer)")))))
                      ("required" . ["pattern"]))))
   (("name" . "imenu_symbols")
    ("description" . "The definitions of a file as its major mode's imenu sees them: name, kind, 1-based line. Cheap structure for any language. Read-only.")
    ("input_schema" . (("type" . "object")
                      ("properties" . (("file" . (("type" . "string") ("description" . "Default: the user's buffer")))))
                      ("required" . []))))
   (("name" . "treesit_info")
    ("description" . "The tree-sitter syntax node at a position (type, field, line range, text), its ancestors and named children; or the whole tree to depth 20 with whole_tree. Needs a *-ts-mode buffer. Read-only.")
    ("input_schema" . (("type" . "object")
                      ("properties" . (("file" . (("type" . "string")))
                                      ("line" . (("type" . "integer") ("description" . "1-based (default: point)")))
                                      ("column" . (("type" . "integer") ("description" . "0-based")))
                                      ("ancestors" . (("type" . "integer") ("description" . "How many parents to list (default 3)")))
                                      ("children" . (("type" . "boolean") ("description" . "List the named children (default true)")))
                                      ("whole_tree" . (("type" . "boolean") ("description" . "The whole tree instead")))))
                      ("required" . []))))
   (("name" . "surrounding_context")
    ("description" . "The definition(s) enclosing a line of a file, whole, from tree-sitter: the function at depth 1, its class or module too at depth 2. Cheaper than read_file for what is around a place. Needs a grammar for the file's language. Read-only.")
    ("input_schema" . (("type" . "object")
                      ("properties" . (("file" . (("type" . "string")))
                                      ("line" . (("type" . "integer") ("description" . "1-based")))
                                      ("depth" . (("type" . "integer") ("description" . "How many enclosing definitions (default 1, at most 3)")))))
                      ("required" . ["file" "line"]))))
   (("name" . "show_location")
    ("description" . "Show the user a place in a file: open it in a window (focus stays here) and highlight the range for a moment. Prefer start_text/end_text anchors (they survive line drift); line/end_line as a fallback. Use it to point at what you are talking about.")
    ("input_schema" . (("type" . "object")
                      ("properties" . (("file" . (("type" . "string")))
                                      ("start_text" . (("type" . "string") ("description" . "Exact text where the range starts")))
                                      ("end_text" . (("type" . "string") ("description" . "Exact text where it ends (searched after start_text)")))
                                      ("line" . (("type" . "integer") ("description" . "1-based, when no start_text")))
                                      ("end_line" . (("type" . "integer")))))
                      ("required" . ["file"]))))
   (("name" . "show_ui_element")
    ("description" . "Point at a piece of the user's screen: flash the mode line, header line or tab line of a window (by buffer name) for two seconds and return where it is in frame pixels. Use it instead of describing a location in words (\"the arc on row 2 next to the status word\"), and when a user's screenshot shows a window line, to confirm which one you mean before acting. A screenshot the user attaches with /shot comes with a LAYOUT block: window rectangles and the constructs of each line; read it before asking which element they meant.")
    ("input_schema" . (("type" . "object")
                      ("properties" . (("buffer" . (("type" . "string") ("description" . "Name of the buffer whose window line to flash")))
                                      ("element" . (("type" . "string") ("enum" . ["mode-line" "header-line" "tab-line"]) ("description" . "Default mode-line")))))
                      ("required" . ["buffer"]))))
   (("name" . "get_last_error")
    ("description" . "The user's recent command errors in this Emacs (message, condition, command, buffer, backtrace frame summary), newest first. Use this when the user asks why something just failed, before reproducing anything. Read-only.")
    ("input_schema" . (("type" . "object")
                      ("properties" . (("count" . (("type" . "integer") ("description" . "How many (default 1)")))))
                      ("required" . []))))
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
   (("name" . "load_project_file")
    ("description" . "Load one Emacs Lisp source file of THIS project into the running Emacs (the source, never a stale .elc). Use it after editing a .el file so the new definitions are live, and before run_ert for a test file. This is the in-process route: eval_sexp refuses `load', and a separate `emacs --batch' is refused too. Returns the features provided.")
    ("input_schema" . (("type" . "object")
                      ("properties" . (("path" . (("type" . "string") ("description" . "A .el file under the project root")))))
                      ("required" . ["path"]))))
   (("name" . "run_ert")
    ("description" . "Run ERT tests in the running Emacs and return counts plus the failing tests' messages. selector is a regexp on test names (default: every loaded test) or a list of names. Load the test file first with load_project_file. Prefer this over any emacs --batch: it runs here, now, with the user's libraries.")
    ("input_schema" . (("type" . "object")
                      ("properties" . (("selector" . (("type" . "string") ("description" . "Regexp on test names, or \"t\" for all")))))
                      ("required" . []))))
   (("name" . "reproduce_command")
    ("description" . "Reproduce an error the user reports from a key or command: runs the command (call-interactively, in the user's buffer) or a form, with the backtrace captured at the point of the signal, as toggle-debug-on-error would show it. Returns error, condition, data and the real frames. Use it FIRST when the user says 'X gives an error' and names or implies the command (look the key up with eval_sexp (key-binding (kbd \"<f3>\")) if needed); get_last_error is the recorded summary, this is the live reproduction. Exec class: reviewed like eval_sexp. Commands that prompt in the minibuffer cannot be driven; pass form with the arguments spelled out instead.")
    ("input_schema" . (("type" . "object")
                      ("properties" . (("command" . (("type" . "string") ("description" . "Interactive command name, e.g. tzz/transient/context")))
                                      ("form" . (("type" . "string") ("description" . "A Lisp form to evaluate instead of a command")))
                                      ("buffer" . (("type" . "string") ("description" . "Buffer to run in (default: the user's buffer)")))))
                      ("required" . []))))
   (("name" . "project_outline")
    ("description" . "Every top-level definition in the project's source files, by file, with line numbers (imenu). A few thousand tokens for a typical project; cached, so repeating it is free. Call it FIRST when a task spans files you have not read: it answers 'where is X' in one call, then read the two or three files whole. Read-only.")
    ("input_schema" . (("type" . "object")
                      ("properties" . (("path" . (("type" . "string") ("description" . "Directory (default: the project root)")))
                                      ("pattern" . (("type" . "string") ("description" . "Glob to keep only some files, e.g. *.el or lisp/interfaces/*")))
                                      ("max_files" . (("type" . "integer") ("description" . "Default 400")))))
                      ("required" . []))))
   (("name" . "symbol_info")
    ("description" . "Everything about one Emacs Lisp symbol in ONE call: kinds (function/command/macro/variable/user-option/face), signature, docstring, defining file and line, the definition's source text, the current value (in a buffer you name), default and standard values, advice, whether it is compiled, and (optionally) its callers. Read-only, not reviewed. Use this instead of search_content + read_file + eval_sexp probes of symbol-file/fboundp/describe-*.

EXAMPLES:
- symbol_info symbol=\"efrit-agent-spinner-mode-line-string\"
- symbol_info symbol=\"global-mode-string\" buffer=\"*scratch*\"
- symbol_info symbol=\"efrit-agent-buffers\" include_callers=true")
    ("input_schema" . (("type" . "object")
                      ("properties" . (("symbol" . (("type" . "string") ("description" . "The symbol's name")))
                                      ("buffer" . (("type" . "string") ("description" . "Buffer whose binding of a variable is read (default: the user's buffer)")))
                                      ("include_source" . (("type" . "boolean") ("description" . "Include the definition's text (default true)")))
                                      ("include_callers" . (("type" . "boolean") ("description" . "Walk xref for callers (default false; slower)")))))
                      ("required" . ["symbol"]))))
   (("name" . "live_state")
    ("description" . "Current values of named variables and the bound/fbound state of names, in one buffer. Names only: nothing is evaluated or called, so it is read-only and not reviewed. Use it for every 'what is X right now' instead of an eval_sexp of (list x y z).

EXAMPLE: live_state names=[\"global-mode-string\", \"header-line-format\", \"efrit-agent--thinking-label\"] buffer=\"*efrit-agent*\"")
    ("input_schema" . (("type" . "object")
                      ("properties" . (("names" . (("type" . "array") ("items" . (("type" . "string"))) ("description" . "Symbol names")))
                                      ("buffer" . (("type" . "string") ("description" . "Buffer whose bindings are read (default: the user's buffer)")))))
                      ("required" . ["names"]))))
   (("name" . "mode_line_report")
    ("description" . "Every window's mode line, header line and tab line as rendered text, taken apart into the constructs they are made of, each named by the variable or (:eval FUNCTION) it came from; plus global-mode-string and mode-line-misc-info. Answers 'what is that thing in the mode line / header' by name in one call. Read-only.

EXAMPLE: mode_line_report buffer=\"*efrit-agent*\"")
    ("input_schema" . (("type" . "object")
                      ("properties" . (("frame" . (("type" . "string") ("enum" . ["selected" "all"]) ("description" . "Windows of the selected frame (default) or all frames")))
                                      ("buffer" . (("type" . "string") ("description" . "Only windows showing this buffer")))))
                      ("required" . []))))
   (("name" . "buffer_list")
    ("description" . "The user's live buffers: name, file, remote (TRAMP host or false), size, major mode, modified, shown. Nothing is entered or read. Local buffers come first. RULE for remote buffers (the user's): consider local buffers first; then the NAMES of remote ones; only when a remote buffer is very likely the one that matters, ask the user (request_user_input) before reading it. Never sweep remote buffers with a search: each costs a round trip to the host and a grant prompt.

EXAMPLES:
- buffer_list pattern=\"report\\|adopt\"
- buffer_list  (everything; then buffer_scan or read_buffer on the ones that matter)")
    ("input_schema" . (("type" . "object")
                      ("properties" . (("pattern" . (("type" . "string") ("description" . "Regexp on buffer and file names")))
                                      ("include_hidden" . (("type" . "boolean") ("description" . "Also names starting with a space (default false)")))
                                      ("max" . (("type" . "integer") ("description" . "Default 200")))))
                      ("required" . []))))
   (("name" . "buffer_scan")
    ("description" . "Regexp matches with surrounding lines in one buffer, or in every live buffer whose name matches buffer_regexp. Read-only; each buffer passes the sandbox's buffer check. Use it instead of re-search-forward loops in eval_sexp: logs (*efrit-log*, *Messages*), the agent transcript, any buffer.

EXAMPLES:
- buffer_scan buffer=\"*efrit-log*\" pattern=\"sandbox: denied\" from_end=true max=5
- buffer_scan buffer_regexp=\"^\\\\*efrit\" pattern=\"kill.ring\" context=3")
    ("input_schema" . (("type" . "object")
                      ("properties" . (("pattern" . (("type" . "string") ("description" . "Emacs regexp")))
                                      ("buffer" . (("type" . "string") ("description" . "Buffer name (default: the user's buffer)")))
                                      ("buffer_regexp" . (("type" . "string") ("description" . "Scan every live buffer whose name matches, instead of buffer")))
                                      ("context" . (("type" . "integer") ("description" . "Lines of context each side (default 2)")))
                                      ("max" . (("type" . "integer") ("description" . "Most matches returned (default 200)")))
                                      ("from_end" . (("type" . "boolean") ("description" . "Newest first: search from the end")))
                                      ("include_remote" . (("type" . "boolean") ("description" . "Under buffer_regexp, also scan buffers of remote files (default false: they are listed as skipped)")))))
                      ("required" . ["pattern"]))))
   (("name" . "read_file")
    ("description" . "Read files and return their contents with metadata. READ WHOLE FILES, SEVERAL AT ONCE: pass paths=[...] for every file the task touches in ONE call; the result holds them all under a token budget (default 40k), and a file that does not fit comes back as its outline (definitions with line numbers) so you read a range of it next. Do not read a file in 60-line windows: nine range reads of one file cost nine round trips where one whole read costs one.

EXAMPLES:
- The files of a feature, whole: read_file paths=[\"lisp/a.el\", \"lisp/b.el\", \"test/test-a.el\"]
- One file, whole: read_file path=\"lisp/efrit.el\"
- A range of a big file (after its outline): read_file path=\"init.el\" start_line=100 end_line=150

Returns: per file, content, size, line count; or the outline when over budget. Binary files return metadata only.")
    ("input_schema" . (("type" . "object")
                      ("properties" . (("path" . (("type" . "string")
                                                  ("description" . "One file to read (or use paths)")))
                                      ("paths" . (("type" . "array") ("items" . (("type" . "string")))
                                                  ("description" . "Several files, whole, in one result")))
                                      ("budget" . (("type" . "integer")
                                                   ("description" . "Token budget for a paths read (default 40000)")))
                                      ("start_line" . (("type" . "number")
                                                       ("description" . "Start line (1-indexed, optional)")))
                                      ("end_line" . (("type" . "number")
                                                     ("description" . "End line (inclusive, optional)")))
                                      ("encoding" . (("type" . "string")
                                                     ("description" . "Encoding override (optional)")))
                                      ("max_size" . (("type" . "number")
                                                     ("description" . "Max bytes to read (default: 100000)")))))
                      ("required" . []))))
   (("name" . "undo_edit")
     ("description" . "Undo the most recent edit to a specific file. Complementary to checkpoint/restore (which work at session level).

   WHEN TO USE:
   - You edited a file and want to revert just that file
   - After running tests and finding that a specific file's change broke things
   - You want to keep other files' changes but undo one file

   EXAMPLES:
   - Simple undo: undo_edit path=\"lisp/config.el\"

   IMPORTANT:
   - Reverts only the most recent edit to this file (not all changes in session)
   - Each undo_edit removes one version, so you can call it multiple times
   - Only works for files edited during this session
   - Returns remaining undo versions available

   WORKFLOW:
   1. Edit file with edit_file
   2. Run tests
   3. If this file's change failed: undo_edit path=\"file.el\"
   4. Try a different approach
   5. Edit file again if needed")
     ("input_schema" . (("type" . "object")
                       ("properties" . (("path" . (("type" . "string")
                                                   ("description" . "File path to undo (required)")))))
                       ("required" . ["path"]))))
   (("name" . "edit_file")
     ("description" . "Make surgical edits to a file using find-and-replace. This is the PRIMARY tool for editing files - prefer this over eval_sexp for file modifications.

   EXAMPLES:
   - Simple replace: edit_file path=\"config.el\" old_str=\"old text\" new_str=\"new text\"
   - Replace all: edit_file path=\"code.el\" old_str=\"foo\" new_str=\"bar\" replace_all=true
   - Add context for uniqueness: Include surrounding lines in old_str if the text isn't unique

   IMPORTANT:
   - old_str must match EXACTLY (including whitespace and newlines)
   - By default, old_str must be unique in the file (use replace_all for multiple matches)
   - The file is saved automatically after editing
   - Returns a git-style diff showing the changes made
   - Automatically tracked for undo_edit - can revert edits if tests fail

   USE THIS TOOL INSTEAD OF eval_sexp for file edits - it's safer and shows diffs.")
    ("input_schema" . (("type" . "object")
                      ("properties" . (("path" . (("type" . "string")
                                                  ("description" . "Absolute path to the file to edit (required)")))
                                      ("old_str" . (("type" . "string")
                                                    ("description" . "Exact text to find and replace (required)")))
                                      ("new_str" . (("type" . "string")
                                                    ("description" . "Text to replace old_str with (required)")))
                                      ("replace_all" . (("type" . "boolean")
                                                        ("description" . "If true, replace all occurrences. Otherwise old_str must be unique (default: false)")))))
                      ("required" . ["path" "old_str" "new_str"]))))
   (("name" . "create_file")
    ("description" . "Create a new file with the given content. This is the PRIMARY tool for creating new files - prefer this over eval_sexp for file creation.

   EXAMPLES:
   - Create new file: create_file path=\"/path/to/new.el\" content=\"(provide 'new)\"
   - Overwrite existing: create_file path=\"config.el\" content=\"...\" overwrite=true

   IMPORTANT:
   - Path must be absolute
   - Parent directories are created automatically if needed
   - By default, will NOT overwrite existing files (use overwrite=true)
   - Returns a git-style diff showing the new content

   USE THIS TOOL INSTEAD OF eval_sexp for file creation - it's atomic and shows diffs.")
    ("input_schema" . (("type" . "object")
                      ("properties" . (("path" . (("type" . "string")
                                                  ("description" . "Absolute path to the file to create (required)")))
                                      ("content" . (("type" . "string")
                                                    ("description" . "Content to write to the file (required)")))
                                      ("overwrite" . (("type" . "boolean")
                                                      ("description" . "If true, overwrite existing file. Otherwise fails if file exists (default: false)")))))
                      ("required" . ["path" "content"]))))
   (("name" . "editor_state")
    ("description" . "Snapshot of where the user is in Emacs: current buffer, file, mode, point (line/column), active region, diagnostics on the current line, project root, other visible buffers. The same block is prepended automatically to each user message; call this to refresh it after your own edits moved point or changed buffers, or to inspect a specific buffer.")
    ("input_schema" . (("type" . "object")
                      ("properties" . (("buffer" . (("type" . "string")
                                                    ("description" . "Buffer name to inspect. Default: the buffer the user is working in (not the efrit buffer).")))))
                      ("required" . []))))
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
   (("name" . "emacs_apropos")
    ("description" . "Find the Emacs command, function or variable for a job when you do not know its name. Searches THIS Emacs (built-ins plus whatever the user has installed: magit, consult, org, tramp...), ranked like M-x apropos, and returns the first docstring line, the source feature, and current values of matching variables. Read-only; needs no grant.

Use it BEFORE reaching for shell_exec or writing Lisp from memory. Then call the command with eval_sexp.

EXAMPLES:
- emacs_apropos query=\"recent files\"            -> recentf-open-files, recentf-list (with its value)
- emacs_apropos query=\"revert buffer\"           -> revert-buffer-quick, revert-buffer
- emacs_apropos query=\"vc diff\"                 -> vc-diff, vc-diff-mergebase, vc-ediff
- emacs_apropos query=\"dired mark\" kind=\"all\"
- emacs_apropos query=\"^tramp-.*method\" kind=\"variable\"   (regexp)")
    ("input_schema" . (("type" . "object")
                      ("properties" . (("query" . (("type" . "string")
                                                   ("description" . "Words describing the job (two or more words match symbols containing at least two), or a regexp")))
                                      ("kind" . (("type" . "string")
                                                 ("enum" . ["command" "function" "variable" "all"])
                                                 ("description" . "What to search (default: command, i.e. things a user can M-x)")))
                                      ("include_values" . (("type" . "boolean")
                                                           ("description" . "Report current values of matching variables (default: true)")))
                                      ("include_internal" . (("type" . "boolean")
                                                             ("description" . "Include private symbols with -- in the name (default: false)")))
                                      ("max" . (("type" . "integer")
                                                ("description" . "Most results to return (default: 25)")))))
                      ("required" . ["query"]))))
   ;; set_project_root removed from the model-facing schema: it let the
    ;; model move its own sandbox.  M-x efrit-set-project-root remains.
    (("name" . "get_diagnostics")
    ("description" . "Get compiler/linter errors from IDE diagnostic systems. CRITICAL for the edit-compile-fix loop.

Collects diagnostics from:
- Flymake (built-in checker)
- Flycheck (popular third-party checker)
- LSP-mode (language server diagnostics)
- Compilation buffer (make/gcc/etc. errors)

EXAMPLES:
- Current buffer: get_diagnostics
- Specific file: get_diagnostics path=\"src/main.el\"
- Every open file of the project: get_diagnostics path=\"project\"
- Only errors: get_diagnostics severity=\"error\"
- From compilation: get_diagnostics sources=[\"compilation\"]

Returns: Array of diagnostics with source, severity, message, line, column.
Use after making edits to check what errors remain.")
    ("input_schema" . (("type" . "object")
                      ("properties" . (("path" . (("type" . "string")
                                                  ("description" . "File path to get diagnostics for (default: current buffer); \"project\" for every open file under the project root")))
                                      ("sources" . (("type" . "array")
                                                   ("items" . (("type" . "string")
                                                              ("enum" . ["flymake" "flycheck" "lsp" "compilation" "all"])))
                                                   ("description" . "Which diagnostic sources to query (default: all)")))
                                      ("severity" . (("type" . "string")
                                                    ("enum" . ["error" "warning" "info"])
                                                    ("description" . "Minimum severity to include (default: all)")))))
                      ("required" . []))))
   (("name" . "read_image")
    ("description" . "Read an image file for visual analysis. Use this to see and analyze image contents.

EXAMPLES:
- View screenshot: read_image path=\"~/Desktop/screenshot.png\"
- Analyze diagram: read_image path=\"/tmp/architecture.jpg\"
- Check image: read_image path=\"./assets/logo.webp\"

Supported formats: PNG, JPEG, GIF, WebP
Maximum size: 5MB (recommended under 1MB for best performance)

IMPORTANT: This is the ONLY way to actually SEE image contents. Do NOT use find-file or other methods to view images - they won't let you analyze the visual content.")
    ("input_schema" . (("type" . "object")
                      ("properties" . (("path" . (("type" . "string")
                                                  ("description" . "Path to the image file (required)")))))
                      ("required" . ["path"]))))
   (("name" . "format_file")
    ("description" . "Auto-format a file using the appropriate formatter. Use after making edits to ensure consistent code style.

EXAMPLES:
- Format elisp: format_file path=\"lisp/efrit.el\" (uses emacs-lisp-mode indentation)
- Format JS/TS: format_file path=\"src/app.ts\" (uses prettier)
- Format Go: format_file path=\"main.go\" (uses gofmt)
- Format Python: format_file path=\"script.py\" (uses black)

SUPPORTED FORMATTERS:
- Elisp: Built-in emacs-lisp-mode indentation
- JS/TS/JSON/CSS/HTML/YAML/MD: prettier
- Go: gofmt
- Rust: rustfmt
- Python: black
- Ruby: rubocop
- Shell: shfmt
- C/C++: clang-format

Returns a diff showing formatting changes, or a message if file was already formatted.")
    ("input_schema" . (("type" . "object")
                      ("properties" . (("path" . (("type" . "string")
                                                  ("description" . "File path to format (required)")))))
                      ("required" . ["path"]))))
   ;; Beads issue tracking tools
   (("name" . "beads_ready")
    ("description" . "Get ready work (unblocked issues) from beads issue tracker. No blockers, ready to work on immediately. Useful for finding tasks to work on next.")
    ("input_schema" . (("type" . "object")
                      ("properties" . (("limit" . (("type" . "integer")
                                                   ("description" . "Maximum number of issues to return (optional)")))
                                       ("priority" . (("type" . "integer")
                                                     ("description" . "Filter by priority level (optional, 0-4)")))
                                       ("assignee" . (("type" . "string")
                                                     ("description" . "Filter by assignee username (optional)"))))))))
   (("name" . "beads_create")
    ("description" . "Create a new issue in beads issue tracker. Returns the created issue ID.")
    ("input_schema" . (("type" . "object")
                      ("properties" . (("title" . (("type" . "string")
                                                   ("description" . "Issue title (required)")))
                                       ("type" . (("type" . "string")
                                                 ("description" . "Issue type: bug, feature, task, epic, chore (optional)")))
                                       ("priority" . (("type" . "integer")
                                                     ("description" . "Priority level 0-4: 0=Critical, 1=High, 2=Medium, 3=Low, 4=Backlog (optional)")))
                                       ("description" . (("type" . "string")
                                                        ("description" . "Detailed description (optional)")))
                                       ("acceptance" . (("type" . "string")
                                                       ("description" . "Acceptance criteria (optional)")))))
                      ("required" . ["title"]))))
   (("name" . "beads_update")
    ("description" . "Update an existing issue in beads issue tracker. Returns the updated issue details.")
    ("input_schema" . (("type" . "object")
                      ("properties" . (("id" . (("type" . "string")
                                               ("description" . "Issue ID (required)")))
                                       ("status" . (("type" . "string")
                                                   ("description" . "New status: open, in_progress, blocked, closed (optional)")))
                                       ("priority" . (("type" . "integer")
                                                     ("description" . "New priority level 0-4 (optional)")))
                                       ("assignee" . (("type" . "string")
                                                     ("description" . "Assignee username (optional)")))
                                       ("description" . (("type" . "string")
                                                        ("description" . "Updated description (optional)")))))
                      ("required" . ["id"]))))
   (("name" . "beads_close")
    ("description" . "Close an issue in beads issue tracker. Returns confirmation of closure.")
    ("input_schema" . (("type" . "object")
                      ("properties" . (("id" . (("type" . "string")
                                               ("description" . "Issue ID (required)")))
                                       ("reason" . (("type" . "string")
                                                   ("description" . "Reason for closure (optional)")))))
                      ("required" . ["id"]))))
   (("name" . "beads_list")
    ("description" . "List issues from beads issue tracker with optional filtering. Returns list of matching issues.")
    ("input_schema" . (("type" . "object")
                      ("properties" . (("status" . (("type" . "string")
                                                   ("description" . "Filter by status: open, in_progress, blocked, closed (optional)")))
                                       ("priority" . (("type" . "integer")
                                                     ("description" . "Filter by priority level (optional)")))
                                       ("type" . (("type" . "string")
                                                 ("description" . "Filter by issue type (optional)")))
                                       ("assignee" . (("type" . "string")
                                                     ("description" . "Filter by assignee (optional)")))
                                       ("limit" . (("type" . "integer")
                                                  ("description" . "Maximum issues to return (optional)"))))))))
   (("name" . "beads_show")
    ("description" . "Show detailed information about a specific issue in beads issue tracker. Returns full issue details including dependencies.")
    ("input_schema" . (("type" . "object")
                      ("properties" . (("id" . (("type" . "string")
                                               ("description" . "Issue ID (required)")))))
                      ("required" . ["id"]))))
   (("name" . "display_hint")
    ("description" . "Control how tool results are displayed in the agent buffer. Use this to provide Claude with fine-grained control over result visualization, including summaries, rendering hints, importance levels, and line annotations.
    
   EXAMPLES:
   - Highlight important output: display_hint tool_use_id=\"toolcall-123\" summary=\"3 files modified\" render_type=\"diff\" importance=\"warning\"
   - Collapse verbose output: display_hint tool_use_id=\"toolcall-456\" summary=\"Searched 150 files, found 3 matches\" auto_expand=false
   - Error emphasis: display_hint tool_use_id=\"toolcall-789\" summary=\"Build failed: missing dependency\" render_type=\"error\" importance=\"error\" auto_expand=true

   WHEN TO USE:
   - After tool execution, to provide a better user experience in the agent buffer
   - When tool output is verbose but important parts could be summarized
   - To highlight errors, warnings, or important successes
   - To control expansion state (show/hide details by default)
   - To add contextual annotations to specific lines of output")
    ("input_schema" . (("type" . "object")
                      ("properties" . (("tool_use_id" . (("type" . "string")
                                                        ("description" . "ID of the tool call to apply hint to (required)")))
                                      ("summary" . (("type" . "string")
                                                   ("description" . "Summary text to display when collapsed (required)")))
                                      ("render_type" . (("type" . "string")
                                                       ("enum" . ["text" "diff" "elisp" "json" "shell" "grep" "markdown" "error"])
                                                       ("description" . "How to render the output (default: text)")))
                                      ("auto_expand" . (("type" . "boolean")
                                                       ("description" . "Whether to auto-expand by default (default: false for verbose, true for errors)")))
                                      ("importance" . (("type" . "string")
                                                      ("enum" . ["normal" "success" "warning" "error"])
                                                      ("description" . "Importance level affects styling (default: normal)")))
                                      ("annotations" . (("type" . "array")
                                                       ("items" . (("type" . "object")
                                                                  ("properties" . (("line" . (("type" . "integer")
                                                                                             ("description" . "Line number to annotate (1-indexed)")))
                                                                                  ("note" . (("type" . "string")
                                                                                            ("description" . "Annotation text")))))
                                                                  ("required" . ["line" "note"])))
                                                       ("description" . "Optional line-by-line annotations")))))
                      ("required" . ["tool_use_id" "summary"]))))]
   "Schema definition for all available tools in efrit-do mode.")

(defun efrit-do--get-current-tools-schema (&optional budget)
  "Return full tool schema, optionally with budget hints.
If BUDGET is provided (an efrit-budget struct), inject budget hints
into tool descriptions.  Tools other packages registered through
`efrit-register-tool' (efrit-tool-registry.el) follow efrit's own."
  (let ((schema (if budget
                    (efrit-do--inject-budget-hints efrit-do--tools-schema budget)
                  efrit-do--tools-schema)))
    (if (bound-and-true-p efrit-tool-registry)
        (vconcat schema (efrit-tool-registry-schema))
      schema)))

(provide 'efrit-do-schema)
;;; efrit-do-schema.el ends here
