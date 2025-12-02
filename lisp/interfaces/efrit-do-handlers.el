;;; efrit-do-handlers.el --- Tool handlers for efrit-do -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; This module contains all tool handler functions for efrit-do.
;; Each handler processes a specific tool call from Claude and returns
;; a formatted result string.
;;
;; Tool handlers follow a standard pattern:
;; 1. Validate input (hash-table, required fields)
;; 2. Extract fields from tool-input
;; 3. Call the underlying tool function
;; 4. Format and return the result
;;
;; The dispatch table in efrit-do.el maps tool names to these handlers.
;;
;; FIELD ACCESS PATTERNS:
;;
;; Two patterns are used for extracting fields from tool-input hash tables:
;;
;; 1. Input Structs (efrit-tool-inputs.el) - for complex tools:
;;    (let* ((input (efrit-foo-input-create tool-input))
;;           (field (efrit-foo-input-get-field input)))
;;      ...)
;;    Use when: Tool has complex validation, type coercion, default values,
;;    or multiple validation rules (e.g., todo_write, glob_files).
;;
;; 2. Extract-fields helper - for simple tools:
;;    (or (efrit-do--validate-hash-table tool-input "tool-name")
;;        (efrit-do--validate-required tool-input "tool-name" "field")
;;        (let* ((args (efrit-do--extract-fields tool-input '("f1" "f2")))
;;               (result (tool-fn args)))
;;          (efrit-do--format-tool-result result "Label")))
;;    Use when: Tool just needs to extract values and pass them through
;;    (e.g., web_search, vcs_diff, read_file).
;;
;; Choose the simpler pattern when possible; use structs for validation.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'efrit-tools)
(require 'efrit-tool-inputs)
(require 'efrit-session)
(require 'efrit-progress)
(require 'efrit-common)

;; Forward declarations
(declare-function efrit-tool-confirm-action "efrit-tool-confirm-action")
(declare-function efrit-tool-checkpoint "efrit-tool-checkpoint")
(declare-function efrit-tool-restore-checkpoint "efrit-tool-checkpoint")
(declare-function efrit-tool-list-checkpoints "efrit-tool-checkpoint")
(declare-function efrit-tool-delete-checkpoint "efrit-tool-checkpoint")
(declare-function efrit-tool-show-diff-preview "efrit-tool-show-diff-preview")
(declare-function efrit-tool-web-search "efrit-tool-web-search")
(declare-function efrit-tool-fetch-url "efrit-tool-fetch-url")
(declare-function efrit-tool-project-files "efrit-tool-project-files")
(declare-function efrit-tool-search-content "efrit-tool-search-content")
(declare-function efrit-tool-read-file "efrit-tool-read-file")
(declare-function efrit-tool-edit-file "efrit-tool-edit-file")
(declare-function efrit-tool-create-file "efrit-tool-create-file")
(declare-function efrit-tool-file-info "efrit-tool-file-info")
(declare-function efrit-tool-vcs-status "efrit-tool-vcs-status")
(declare-function efrit-tool-vcs-diff "efrit-tool-vcs-diff")
(declare-function efrit-tool-vcs-log "efrit-tool-vcs-log")
(declare-function efrit-tool-vcs-blame "efrit-tool-vcs-blame")
(declare-function efrit-tool-elisp-docs "efrit-tool-elisp-docs")
(declare-function efrit-tool--get-project-root "efrit-tool-utils")
(declare-function efrit-set-project-root "efrit-tool-utils")
(declare-function efrit-clear-project-root "efrit-tool-utils")
(declare-function efrit-session-active "efrit-session")
(declare-function efrit-session-set-pending-question "efrit-session")
(declare-function efrit-progress-show-message "efrit-progress")
(declare-function efrit-log "efrit-log")

;; Variables from efrit-do that handlers need
(defvar efrit-do--current-todos)
(defvar efrit-do--todo-counter)

;; Forward declare the TODO item struct accessors
(declare-function efrit-do-todo-item-create "efrit-do")
(declare-function efrit-do-todo-item-id "efrit-do")
(declare-function efrit-do-todo-item-content "efrit-do")
(declare-function efrit-do-todo-item-status "efrit-do")
(declare-function efrit-do-todo-item-priority "efrit-do")

;;; Customization

(defcustom efrit-do-shell-security-enabled t
  "When non-nil, enforce security restrictions on shell commands.
Recommended to keep enabled for safety."
  :type 'boolean
  :group 'efrit-do)

(defcustom efrit-do-glob-files-max-results 1000
  "Maximum number of files to return from glob_files.
Prevents runaway searches from overwhelming the system."
  :type 'integer
  :group 'efrit-do)

;;; Shell command security

(defcustom efrit-do-allowed-shell-commands
  '("ls" "pwd" "date" "whoami" "uname" "df" "ps" "top"
    "cat" "head" "tail" "wc" "grep" "find" "which"
    "echo" "printf" "basename" "dirname" "realpath"
    "git" "make" "emacs" "python" "python3" "pip" "pip3"
    "node" "npm" "npx" "pnpm" "yarn" "bun"
    "cargo" "rustc" "rustup"
    "go" "gofmt"
    "java" "javac" "mvn" "gradle"
    "ruby" "gem" "bundle" "rake"
    "php" "composer"
    "dotnet" "nuget"
    "zig" "cmake" "ninja"
    "docker" "docker-compose"
    "kubectl" "helm"
    "terraform" "ansible"
    "curl" "wget" "jq" "yq"
    "rg" "fd" "ag" "ack" "sed" "awk" "sort" "uniq" "tr" "cut"
    "diff" "patch" "file" "stat" "du" "tree"
    "tar" "gzip" "gunzip" "zip" "unzip"
    "ssh" "scp" "rsync"
    "test" "[")
  "List of shell commands considered safe for AI execution.
Use \"*\" as the sole element to allow ALL commands (disables command whitelist).
Example: (setq efrit-do-allowed-shell-commands \\='(\"*\"))"
  :type '(repeat string)
  :group 'efrit-do)

(defcustom efrit-do-forbidden-shell-patterns
  '("sudo" "su" "passwd"
    "shutdown" "reboot"
    "fdisk" "mkfs" "mount" "umount"
    "\\$(" "`" "\\${")
  "Patterns that are forbidden in shell commands.
Set to nil to disable pattern checking entirely.

By default, only truly dangerous system-level commands are blocked.
Pipes, redirects, and command chaining are allowed for development workflows."
  :type '(repeat string)
  :group 'efrit-do)

;;; Helper functions

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

          ;; Check if main command is in allowed list (unless "*" wildcard)
          (unless (or (member "*" efrit-do-allowed-shell-commands)
                      (member main-command efrit-do-allowed-shell-commands))
            (cl-return-from validate-shell
              (cons nil (format "Shell command '%s' not in allowed whitelist" main-command))))

          ;; Check for forbidden patterns (if any)
          (dolist (pattern efrit-do-forbidden-shell-patterns)
            (when (string-match-p pattern command-clean)
              (cl-return-from validate-shell
                (cons nil (format "Shell command contains forbidden pattern: %s" pattern)))))

          ;; Command appears safe
          (cons t nil))))))

(defun efrit-do--validate-hash-table (tool-input tool-name)
  "Validate TOOL-INPUT is a hash-table for TOOL-NAME.
Returns error string if invalid, nil if valid."
  (unless (hash-table-p tool-input)
    (efrit-format-error tool-name "requires a hash table input")))

(defun efrit-do--validate-required (tool-input tool-name field)
  "Validate required FIELD exists in TOOL-INPUT for TOOL-NAME.
Returns error string if invalid, nil if valid."
  (let ((val (gethash field tool-input)))
    (when (or (null val) (and (stringp val) (string-empty-p val)))
      (efrit-format-validation-error field (format "required by %s" tool-name)))))

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

;;; Core tool handlers

(defun efrit-do--handle-eval-sexp (input-str)
  "Handle eval_sexp tool to evaluate Emacs Lisp code.
INPUT-STR is the elisp expression to evaluate.
Validates syntax before execution and returns the result or error."
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

(defun efrit-do--handle-shell-exec (input-str)
  "Handle shell_exec tool to execute a shell command.
INPUT-STR is the shell command to execute.
Validates against security whitelist before execution.
Returns output or security error."
  (let ((validation (efrit-do--validate-shell-command input-str)))
    (if (car validation)
        ;; Command is safe, execute it
        (condition-case shell-err
            (let* ((start-time (current-time))
                   (shell-result (with-timeout (10) ; 10 second timeout
                                   (shell-command-to-string input-str)))
                   (end-time (current-time))
                   (duration (float-time (time-subtract end-time start-time))))
              (format "\n[Executed: %s]\n[Duration: %.2fs]\n[Result: %s]"
                      input-str duration
                      (if (> (length shell-result) 1000)
                          (concat (substring shell-result 0 1000) "\n... (output truncated)")
                        shell-result)))
          (timeout (format "\n[Shell command timed out after 10 seconds: %s]" input-str))
          (error (format "\n[Error executing shell command '%s': %s]"
                         input-str (error-message-string shell-err))))
      ;; Command failed validation
      (format "\n[SECURITY: Shell command blocked - %s]\n[Command: %s]"
              (cdr validation) input-str))))

(defun efrit-do--handle-todo-write (tool-input)
  "Handle todo_write tool - replaces entire TODO list.
TOOL-INPUT is a hash table with a `todos' array.
Each todo has content, status, and activeForm.

This is a Pure Executor implementation - it simply stores what
Claude sends without validation or state machine logic."
  (let* ((input (efrit-todo-write-input-create tool-input))
         (todos-array (efrit-todo-write-input-get-todos input))
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
  "Handle buffer_create tool to create or update a buffer with content.
TOOL-INPUT is a hash table with name, content, and optional mode fields.
INPUT-STR is fallback content if not specified in tool-input."
  (let* ((input (efrit-buffer-create-input-create tool-input))
         (name (efrit-buffer-create-input-get-name input))
         (content (or (efrit-buffer-create-input-get-content input) input-str))
         (mode (efrit-buffer-create-input-get-mode input)))
    (condition-case err
        (let ((result (efrit-tools-create-buffer name content mode)))
          (format "\n[%s]" result))
      (error
       (format "\n[Error creating buffer: %s]" (error-message-string err))))))

(defun efrit-do--handle-format-file-list (input-str)
  "Handle format_file_list tool to format a file listing for display.
INPUT-STR is a newline-separated list of file paths."
  (condition-case err
      (let ((result (efrit-tools-format-file-list input-str)))
        (format "\n[Formatted file list]\n%s" result))
    (error
     (format "\n[Error formatting file list: %s]" (error-message-string err)))))

(defun efrit-do--handle-format-todo-list (tool-input)
  "Handle format_todo_list tool to format current TODOs for display.
TOOL-INPUT is a hash table with optional sort-by field."
  (let* ((input (efrit-format-todo-list-input-create tool-input))
         (sort-by (efrit-format-todo-list-input-get-sort-by input)))
    (condition-case err
        (let ((result (efrit-tools-format-todo-list efrit-do--current-todos sort-by)))
          (format "\n[Formatted TODO list]\n%s" result))
      (error
       (format "\n[Error formatting TODO list: %s]" (error-message-string err))))))

(defun efrit-do--handle-session-complete (tool-input)
  "Handle session_complete tool with TOOL-INPUT hash table.
This signals that a multi-step session is complete."
  (let* ((input (efrit-session-complete-input-create tool-input))
         (message (efrit-session-complete-input-get-message input)))
    ;; Return a special marker that the async handler can detect
    (format "\n[SESSION-COMPLETE: %s]" (or message "Task completed"))))

(defun efrit-do--handle-display-in-buffer (tool-input input-str)
  "Handle display_in_buffer tool to show content in an Emacs buffer.
TOOL-INPUT has buffer_name, content, and optional window_height.
INPUT-STR is fallback content if not in tool-input."
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
  "Handle glob_files tool to list files matching pattern.
Includes safety limits to prevent hanging on large directories."
  ;; Validate input - must be a hash table with required 'pattern' field
   (let* ((input (efrit-glob-files-input-create tool-input))
          (validation (efrit-glob-files-input-is-valid input)))
     (if (not (car validation))
        (format "\n[Error: %s]" (cdr validation))
        (let* ((pattern (efrit-glob-files-input-get-pattern input))
               (extension (efrit-glob-files-input-get-extension input))
               (recursive (efrit-glob-files-input-get-recursive input)))

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
           (format "\n[Error finding files: %s]" (error-message-string err))))))))))

(defun efrit-do--handle-request-user-input (tool-input)
  "Handle request_user_input tool to pause and ask user a question.
Sets the session to waiting-for-user state and emits a progress event."
  (let* ((input (efrit-request-user-input-input-create tool-input))
         (validation (efrit-request-user-input-input-is-valid input)))
    (if (not (car validation))
        (format "\n[Error: %s]" (cdr validation))
      (let* ((question (efrit-request-user-input-input-get-question input))
             (options (efrit-request-user-input-input-get-options input))
             (session (efrit-session-active)))
        (if (not session)
            "\n[Error: request_user_input requires an active session]"
          ;; Set pending question on session
          (efrit-session-set-pending-question
           session question
           (when options options))

          ;; Emit progress event
          (efrit-progress-show-message
           (format "Question for user: %s" question)
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
  (or (efrit-do--validate-hash-table tool-input "confirm_action")
      (efrit-do--validate-required tool-input "confirm_action" "action")
      (let* ((args (efrit-do--extract-fields
                    tool-input '("action" "details" "severity"
                                 ("options" . efrit-do--vector-to-list)
                                 "timeout_seconds")))
             (result (efrit-tool-confirm-action args)))
        (efrit-do--format-tool-result result "Confirmation Result"))))

;;; Checkpoint Tool Handlers - Phase 3: Workflow Enhancement

(defun efrit-do--handle-checkpoint (tool-input)
  "Handle checkpoint tool to create a restore point before risky operations."
  (require 'efrit-tool-checkpoint)
  (or (efrit-do--validate-hash-table tool-input "checkpoint")
      (efrit-do--validate-required tool-input "checkpoint" "description")
      (let* ((args (efrit-do--extract-fields tool-input '("description")))
             (result (efrit-tool-checkpoint args)))
        (efrit-do--format-tool-result result "Checkpoint Result"))))

(defun efrit-do--handle-restore-checkpoint (tool-input)
  "Handle restore_checkpoint tool to restore from a previous checkpoint."
  (require 'efrit-tool-checkpoint)
  (or (efrit-do--validate-hash-table tool-input "restore_checkpoint")
      (efrit-do--validate-required tool-input "restore_checkpoint" "checkpoint_id")
      (let* ((args (efrit-do--extract-fields tool-input '("checkpoint_id" "keep_checkpoint")))
             (result (efrit-tool-restore-checkpoint args)))
        (efrit-do--format-tool-result result "Restore Result"))))

(defun efrit-do--handle-list-checkpoints ()
  "Handle list_checkpoints tool to list all available checkpoints.
Returns JSON-encoded list of checkpoint metadata."
  (require 'efrit-tool-checkpoint)
  (let ((result (efrit-tool-list-checkpoints nil)))
    (format "\n[Checkpoints]\n%s" (json-encode result))))

(defun efrit-do--handle-delete-checkpoint (tool-input)
  "Handle delete_checkpoint tool to delete a checkpoint without restoring."
  (require 'efrit-tool-checkpoint)
  (or (efrit-do--validate-hash-table tool-input "delete_checkpoint")
      (efrit-do--validate-required tool-input "delete_checkpoint" "checkpoint_id")
      (let* ((args (efrit-do--extract-fields tool-input '("checkpoint_id")))
             (result (efrit-tool-delete-checkpoint args)))
        (efrit-do--format-tool-result result "Delete Result"))))

(defun efrit-do--convert-changes-array (changes)
  "Convert CHANGES array from hash tables to alists for tool functions."
  (mapcar (lambda (change)
            (if (hash-table-p change)
                `((file . ,(gethash "file" change))
                  (old_content . ,(gethash "old_content" change))
                  (new_content . ,(gethash "new_content" change)))
              change))
          (efrit-do--vector-to-list changes)))

(defun efrit-do--handle-show-diff-preview (tool-input)
  "Handle show_diff_preview tool to show proposed changes in a diff view."
  (require 'efrit-tool-show-diff-preview)
  (or (efrit-do--validate-hash-table tool-input "show_diff_preview")
      (efrit-do--validate-required tool-input "show_diff_preview" "changes")
      (let ((changes (gethash "changes" tool-input)))
        (if (zerop (length changes))
            (efrit-format-validation-error "changes" "must be non-empty")
          (let* ((args (efrit-do--extract-fields
                        tool-input '("description" "apply_mode")))
                 (args-with-changes (cons `(changes . ,(efrit-do--convert-changes-array changes))
                                          args))
                 (result (efrit-tool-show-diff-preview args-with-changes)))
            (efrit-do--format-tool-result result "Diff Preview Result"))))))

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

(defun efrit-do--handle-edit-file (tool-input)
  "Handle edit_file tool to make surgical edits to a file."
  (require 'efrit-tool-edit-file)
  (or (efrit-do--validate-hash-table tool-input "edit_file")
      (efrit-do--validate-required tool-input "edit_file" "path")
      (efrit-do--validate-required tool-input "edit_file" "old_str")
      (efrit-do--validate-required tool-input "edit_file" "new_str")
      (let* ((args (efrit-do--extract-fields
                    tool-input '("path" "old_str" "new_str" "replace_all")))
             (result (efrit-tool-edit-file args)))
        (efrit-do--format-tool-result result "Edit File Result"))))

(defun efrit-do--handle-create-file (tool-input)
  "Handle create_file tool to atomically create a file with content."
  (require 'efrit-tool-create-file)
  (or (efrit-do--validate-hash-table tool-input "create_file")
      (efrit-do--validate-required tool-input "create_file" "path")
      (efrit-do--validate-required tool-input "create_file" "content")
      (let* ((args (efrit-do--extract-fields
                    tool-input '("path" "content" "overwrite")))
             (result (efrit-tool-create-file args)))
        (efrit-do--format-tool-result result "Create File Result"))))

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
  (or (efrit-do--validate-hash-table tool-input "set_project_root")
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
              (efrit-format-error "set_project_root"
                                  (format "Path does not exist: %s" path)))))))))

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

(provide 'efrit-do-handlers)

;;; efrit-do-handlers.el ends here
