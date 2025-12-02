;;; efrit-do-schema.el --- Tool schemas for efrit-do -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai
;; URL: https://github.com/stevey/efrit

;;; Commentary:
;; This file contains the tool schemas (JSON schema definitions) for efrit-do.
;; These schemas are sent to Claude to describe available tools.
;; Extracted from efrit-do.el for maintainability.

;;; Code:

(require 'cl-lib)

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
                      ("required" . ["path"]))))
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
- Only errors: get_diagnostics severity=\"error\"
- From compilation: get_diagnostics sources=[\"compilation\"]

Returns: Array of diagnostics with source, severity, message, line, column.
Use after making edits to check what errors remain.")
    ("input_schema" . (("type" . "object")
                      ("properties" . (("path" . (("type" . "string")
                                                  ("description" . "File path to get diagnostics for (default: current buffer)")))
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
                      ("required" . ["path"]))))]
  "Schema definition for all available tools in efrit-do mode.")

(defun efrit-do--get-current-tools-schema (&optional budget)
  "Return full tool schema, optionally with budget hints.
If BUDGET is provided (an efrit-budget struct), inject budget hints
into tool descriptions."
  (if budget
      (efrit-do--inject-budget-hints efrit-do--tools-schema budget)
    efrit-do--tools-schema))

(provide 'efrit-do-schema)
;;; efrit-do-schema.el ends here
