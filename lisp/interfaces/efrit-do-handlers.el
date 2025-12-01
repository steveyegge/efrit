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

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'efrit-tools)

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

;;; Core tool handlers

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

(provide 'efrit-do-handlers)

;;; efrit-do-handlers.el ends here
