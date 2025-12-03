;;; efrit-do-dispatch.el --- Tool dispatch for efrit-do -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai
;; URL: https://github.com/stevey/efrit

;;; Commentary:
;; This file contains the tool dispatch table and execution runtime for efrit-do.
;; Extracted from efrit-do.el to allow other modules (efrit-chat-api.el,
;; efrit-executor.el) to access tool execution without depending on efrit-do.el.

;;; Code:

(require 'efrit-log)
(require 'efrit-do-circuit-breaker)

;; Forward declarations for handler functions (defined in efrit-do-handlers.el)
(declare-function efrit-do--handle-eval-sexp "efrit-do-handlers")
(declare-function efrit-do--handle-shell-exec "efrit-do-handlers")
(declare-function efrit-do--handle-todo-write "efrit-do-handlers")
(declare-function efrit-do--handle-buffer-create "efrit-do-handlers")
(declare-function efrit-do--handle-format-file-list "efrit-do-handlers")
(declare-function efrit-do--handle-format-todo-list "efrit-do-handlers")
(declare-function efrit-do--handle-display-in-buffer "efrit-do-handlers")
(declare-function efrit-do--handle-session-complete "efrit-do-handlers")
(declare-function efrit-do--handle-glob-files "efrit-do-handlers")
(declare-function efrit-do--handle-request-user-input "efrit-do-handlers")
(declare-function efrit-do--handle-confirm-action "efrit-do-handlers")
(declare-function efrit-do--handle-checkpoint "efrit-do-handlers")
(declare-function efrit-do--handle-restore-checkpoint "efrit-do-handlers")
(declare-function efrit-do--handle-list-checkpoints "efrit-do-handlers")
(declare-function efrit-do--handle-delete-checkpoint "efrit-do-handlers")
(declare-function efrit-do--handle-show-diff-preview "efrit-do-handlers")
(declare-function efrit-do--handle-web-search "efrit-do-handlers")
(declare-function efrit-do--handle-fetch-url "efrit-do-handlers")
(declare-function efrit-do--handle-project-files "efrit-do-handlers")
(declare-function efrit-do--handle-search-content "efrit-do-handlers")
(declare-function efrit-do--handle-read-file "efrit-do-handlers")
(declare-function efrit-do--handle-undo-edit "efrit-do-handlers")
(declare-function efrit-do--handle-edit-file "efrit-do-handlers")
(declare-function efrit-do--handle-create-file "efrit-do-handlers")
(declare-function efrit-do--handle-file-info "efrit-do-handlers")
(declare-function efrit-do--handle-vcs-status "efrit-do-handlers")
(declare-function efrit-do--handle-vcs-diff "efrit-do-handlers")
(declare-function efrit-do--handle-vcs-log "efrit-do-handlers")
(declare-function efrit-do--handle-vcs-blame "efrit-do-handlers")
(declare-function efrit-do--handle-elisp-docs "efrit-do-handlers")
(declare-function efrit-do--handle-set-project-root "efrit-do-handlers")
(declare-function efrit-do--handle-get-diagnostics "efrit-do-handlers")
(declare-function efrit-do--handle-read-image "efrit-do-handlers")
(declare-function efrit-do--handle-format-file "efrit-do-handlers")
(declare-function efrit-do--handle-beads-ready "efrit-do-handlers")
(declare-function efrit-do--handle-beads-create "efrit-do-handlers")
(declare-function efrit-do--handle-beads-update "efrit-do-handlers")
(declare-function efrit-do--handle-beads-close "efrit-do-handlers")
(declare-function efrit-do--handle-beads-list "efrit-do-handlers")
(declare-function efrit-do--handle-beads-show "efrit-do-handlers")
;; Alias handlers for chat-mode tool names
(declare-function efrit-do--handle-create-buffer-alias "efrit-do-handlers")
(declare-function efrit-do--handle-edit-buffer-alias "efrit-do-handlers")
(declare-function efrit-do--handle-read-buffer-alias "efrit-do-handlers")
(declare-function efrit-do--handle-buffer-info-alias "efrit-do-handlers")
(declare-function efrit-do--handle-get-context-alias "efrit-do-handlers")

;; Forward declarations for session tracking
(declare-function efrit-session-active "efrit-session")
(declare-function efrit-session-add-work "efrit-session")
(declare-function efrit-session-track-error "efrit-session")

;; External variables
(defvar efrit-do-show-tool-execution)

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
    ("undo_edit"          . (efrit-do--handle-undo-edit . :tool-input))
    ("edit_file"          . (efrit-do--handle-edit-file . :tool-input))
    ("create_file"        . (efrit-do--handle-create-file . :tool-input))
    ("file_info"          . (efrit-do--handle-file-info . :tool-input))
    ("vcs_status"         . (efrit-do--handle-vcs-status . :tool-input))
    ("vcs_diff"           . (efrit-do--handle-vcs-diff . :tool-input))
    ("vcs_log"            . (efrit-do--handle-vcs-log . :tool-input))
    ("vcs_blame"          . (efrit-do--handle-vcs-blame . :tool-input))
    ("elisp_docs"         . (efrit-do--handle-elisp-docs . :tool-input))
    ("set_project_root"   . (efrit-do--handle-set-project-root . :tool-input))
    ("get_diagnostics"    . (efrit-do--handle-get-diagnostics . :tool-input))
    ("read_image"         . (efrit-do--handle-read-image . :tool-input))
    ("format_file"        . (efrit-do--handle-format-file . :tool-input))
    ;; Issue tracking tools (beads)
    ("beads_ready"        . (efrit-do--handle-beads-ready . :tool-input))
    ("beads_create"       . (efrit-do--handle-beads-create . :tool-input))
    ("beads_update"       . (efrit-do--handle-beads-update . :tool-input))
    ("beads_close"        . (efrit-do--handle-beads-close . :tool-input))
    ("beads_list"         . (efrit-do--handle-beads-list . :tool-input))
    ("beads_show"         . (efrit-do--handle-beads-show . :tool-input))
    ;; Aliases for chat-mode tool names (maps to same handlers)
    ;; Canonical naming: verb_noun (buffer_create), aliases: noun_verb (create_buffer)
    ("create_buffer"      . (efrit-do--handle-create-buffer-alias . :tool-input))
    ("edit_buffer"        . (efrit-do--handle-edit-buffer-alias . :tool-input))
    ("read_buffer"        . (efrit-do--handle-read-buffer-alias . :tool-input))
    ("buffer_info"        . (efrit-do--handle-buffer-info-alias . :tool-input))
    ("get_context"        . (efrit-do--handle-get-context-alias . :tool-input)))
  "Dispatch table mapping tool names to handlers and argument types.")

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
      (when (bound-and-true-p efrit-do-show-tool-execution)
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

(provide 'efrit-do-dispatch)
;;; efrit-do-dispatch.el ends here
