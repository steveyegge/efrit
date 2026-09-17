;;; efrit-permissions.el --- Gate mutating tool calls -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; Until now every tool ran unconditionally; the only gates were
;; `confirm_action' and `show_diff_preview', which the *model* had to
;; choose to call.  This module puts a gate in front of tools that
;; change state, decided by efrit, before dispatch.
;;
;; Design (after agent-shell's permission model):
;;
;; - Each tool has a class: `read' (never asks), `write' (changes
;;   files or buffers), `exec' (runs arbitrary code or commands),
;;   `control' (session/UI plumbing, never asks).  See
;;   `efrit-permission-tool-classes'.
;;
;; - `efrit-permission-policy' says which classes need consent.  The
;;   default asks for `write' and `exec'.
;;
;; - `efrit-permission-responder-function', if set, is consulted
;;   first with a request alist and may decide programmatically
;;   (auto-allow reads in a scratch project, deny shell on prod hosts,
;;   ...).  Returning nil falls through to the interactive prompt.
;;
;; - The interactive prompt shows what is about to happen (the elisp,
;;   the command, the diff target) and offers: y allow once, n deny,
;;   ! allow this tool for the rest of the session, a allow everything
;;   for the rest of the session, ? show the full input.
;;
;; - A denial ends the turn.  Letting the model continue after a
;;   refusal it cannot see the reason for produces confused retries;
;;   ending the turn hands control back to the user, whose next input
;;   explains what they want instead.  The denial is recorded as a
;;   tool_result so the conversation stays well-formed.
;;
;; Every decision is written to the tool audit log, including for
;; eval_sexp and shell_exec, which previously bypassed it.
;;
;; Pure Executor: this is "basic validation / security filtering",
;; which ARCHITECTURE.md lists as allowed.  Nothing here interprets
;; the task; it asks the human.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'efrit-log)
(require 'efrit-events)
(require 'efrit-ui-helpers)

(declare-function efrit-tool-audit "efrit-tool-utils")
(declare-function efrit-tool--get-project-root "efrit-tool-utils")
(declare-function efrit-tool-unified-diff "efrit-tool-utils")

(defgroup efrit-permissions nil
  "Consent for mutating tool calls."
  :group 'efrit
  :prefix "efrit-permission-")

;;; Tool classification

(defcustom efrit-permission-tool-classes
  '(;; exec: arbitrary code or commands
    ("eval_sexp" . exec) ("shell_exec" . exec)
    ;; write: files and buffers
    ("edit_file" . write) ("create_file" . write) ("format_file" . write)
    ("undo_edit" . write) ("restore_checkpoint" . write)
    ("delete_checkpoint" . write) ("checkpoint" . write)
    ("buffer_create" . write) ("edit_buffer" . write) ("create_buffer" . write)
    ("beads_create" . write) ("beads_update" . write) ("beads_close" . write)
    ("set_project_root" . write)
    ;; control: efrit plumbing, never gated
    ("session_complete" . control) ("todo_write" . control)
    ("request_user_input" . control) ("confirm_action" . control)
    ("display_hint" . control) ("show_diff_preview" . control)
    ("display_in_buffer" . control) ("format_file_list" . control)
    ("format_todo_list" . control))
  "Alist mapping tool names to permission classes.
Classes are `read', `write', `exec', `control'.  Tools not listed are
treated as `read'.  Network tools (web_search, fetch_url) already
have their own consent inside the tool."
  :type '(alist :key-type string
                :value-type (choice (const read) (const write)
                                    (const exec) (const control)))
  :group 'efrit-permissions)

(defcustom efrit-permission-policy '(write exec)
  "Tool classes that require consent before running.
A list drawn from `write' and `exec'.  nil disables gating entirely
\(the pre-0.5 behaviour)."
  :type '(set (const write) (const exec))
  :group 'efrit-permissions)

(defcustom efrit-permission-responder-function nil
  "Function consulted before prompting the user, or nil.

Called with one argument, a REQUEST alist:

  (:tool NAME) (:class CLASS) (:input HASH-TABLE) (:summary STRING)
  (:project-root DIR) (:session-id ID-OR-NIL)

Return one of `allow', `deny', `allow-tool' (this tool for the rest
of the session), `allow-all' (everything for the rest of the
session), or nil to fall through to the interactive prompt.

Examples:

  ;; Trust everything inside a scratch tree:
  (setq efrit-permission-responder-function
        (lambda (req)
          (when (string-prefix-p \"/tmp/scratch/\"
                                 (alist-get :project-root req))
            \\='allow)))

  ;; Never let shell_exec run on a remote root:
  (setq efrit-permission-responder-function
        (lambda (req)
          (when (and (equal (alist-get :tool req) \"shell_exec\")
                     (file-remote-p (alist-get :project-root req)))
            \\='deny)))"
  :type '(choice (const nil) function)
  :group 'efrit-permissions)

(defcustom efrit-permission-summary-max-lines 12
  "Lines of the tool input shown in the consent prompt before truncation."
  :type 'integer
  :group 'efrit-permissions)

;;; Session grants

(defvar efrit-permission--session-grants (make-hash-table :test 'equal)
  "Session-id -> list of tool names granted for the session, or the symbol `all'.")

(defun efrit-permission--grant (session-id what)
  "Record WHAT (a tool name or `all') as granted for SESSION-ID."
  (let ((key (or session-id "global")))
    (if (eq what 'all)
        (puthash key 'all efrit-permission--session-grants)
      (let ((cur (gethash key efrit-permission--session-grants)))
        (unless (eq cur 'all)
          (puthash key (cons what cur) efrit-permission--session-grants))))))

(defun efrit-permission--granted-p (session-id tool)
  "Non-nil if TOOL was granted for SESSION-ID (or globally) earlier."
  (cl-some (lambda (key)
             (let ((g (gethash key efrit-permission--session-grants)))
               (or (eq g 'all) (member tool g))))
           (list (or session-id "global") "global")))

(defun efrit-permission-reset (&optional session-id)
  "Forget session grants for SESSION-ID, or all grants when nil."
  (interactive)
  (if session-id
      (remhash session-id efrit-permission--session-grants)
    (clrhash efrit-permission--session-grants))
  (when (called-interactively-p 'any)
    (message "Efrit: permission grants cleared")))

;;; Classification and summaries

(defun efrit-permission-tool-class (tool)
  "Return the permission class symbol for TOOL."
  (or (cdr (assoc tool efrit-permission-tool-classes)) 'read))

(defun efrit-permission-needed-p (tool)
  "Non-nil if TOOL's class is in `efrit-permission-policy'."
  (memq (efrit-permission-tool-class tool) efrit-permission-policy))

(defun efrit-permission--input-get (input key)
  (cond ((hash-table-p input) (gethash key input))
        ((stringp input) (and (equal key "expr") input))))

(defun efrit-permission--truncate-lines (text)
  "TEXT limited to `efrit-permission-summary-max-lines' lines."
  (let ((lines (split-string (or text "") "\n")))
    (if (> (length lines) efrit-permission-summary-max-lines)
        (concat (string-join (seq-take lines efrit-permission-summary-max-lines) "\n")
                (format "\n... (%d more lines)"
                        (- (length lines) efrit-permission-summary-max-lines)))
      text)))

(defun efrit-permission-summarize (tool input)
  "One-paragraph human summary of what TOOL would do with INPUT."
  (let ((g (lambda (k) (efrit-permission--input-get input k))))
    (efrit-permission--truncate-lines
     (pcase tool
       ("eval_sexp" (or (funcall g "expr") (funcall g "expression")
                        (funcall g "code") (format "%S" input)))
       ("shell_exec" (format "$ %s" (or (funcall g "command") input)))
       ("edit_file"
        (format "%s\n--- old_str\n%s\n+++ new_str\n%s"
                (funcall g "path") (funcall g "old_str") (funcall g "new_str")))
       ("create_file"
        (format "%s (%d chars)%s" (funcall g "path")
                (length (or (funcall g "content") ""))
                (if (eq (funcall g "overwrite") t) " OVERWRITE" "")))
       ("format_file" (format "format %s" (funcall g "path")))
       ("undo_edit" (format "undo last edit to %s" (funcall g "path")))
       ("set_project_root" (format "project root -> %s" (funcall g "path")))
       ((or "buffer_create" "create_buffer")
        (format "buffer %s (%d chars)" (funcall g "name")
                (length (or (funcall g "content") ""))))
       ("edit_buffer" (format "edit buffer %s" (funcall g "buffer")))
       ("restore_checkpoint" (format "git stash pop %s" (funcall g "checkpoint_id")))
       (_ (let ((parts nil))
            (when (hash-table-p input)
              (maphash (lambda (k v) (push (format "%s=%S" k v) parts)) input))
            (string-join (nreverse parts) " ")))))))

;;; The prompt

(defvar efrit-permission--last-request nil
  "The most recent request alist, for `efrit-permission-show-last'.")

(defvar efrit-permission-last-input nil
  "The tool input to actually run after the latest `efrit-permission-check'.
Equal to the input passed in unless the user chose [e]dit at the prompt.")

(defcustom efrit-permission-preview t
  "When non-nil, show a diff/content preview before asking about write tools."
  :type 'boolean
  :group 'efrit-permissions)

(defconst efrit-permission--editable-fields
  '(("eval_sexp" "expr" emacs-lisp-mode)
    ("shell_exec" "command" sh-mode)
    ("edit_file" "new_str" nil)
    ("create_file" "content" nil)
    ("buffer_create" "content" nil)
    ("create_buffer" "content" nil)
    ("edit_buffer" "content" nil))
  "Per tool: the input field the user may edit before approving, and a mode.")

(defun efrit-permission--preview-text (tool input)
  "Return (TEXT . MODE) previewing what TOOL would write, or nil."
  (let ((g (lambda (k) (efrit-permission--input-get input k))))
    (pcase tool
      ("edit_file"
       (when (fboundp 'efrit-tool-unified-diff)
         (let ((path (or (funcall g "path") "file")))
           (cons (efrit-tool-unified-diff (or (funcall g "old_str") "")
                                          (or (funcall g "new_str") "")
                                          path)
                 'diff-mode))))
      ("create_file"
       (let* ((path (or (funcall g "path") "file"))
              (content (or (funcall g "content") "")))
         (cons (concat (format "--- /dev/null\n+++ b/%s\n" (file-name-nondirectory path))
                       (mapconcat (lambda (l) (concat "+" l))
                                  (split-string content "\n") "\n"))
               'diff-mode)))
      ((or "buffer_create" "create_buffer" "edit_buffer")
       (cons (or (funcall g "content") "") nil))
      (_ nil))))

(defun efrit-permission--prompt (request)
  "Ask the user about REQUEST.
Returns allow/deny/allow-tool/allow-all, or (edit . NEW-INPUT) when
the user edited the tool's primary field before approving."
  (let* ((tool (alist-get :tool request))
         (input (alist-get :input request))
         (summary (alist-get :summary request))
         (root (alist-get :project-root request))
         (remote (and root (file-remote-p root)))
         (editable (assoc tool efrit-permission--editable-fields))
         (header (format "Efrit wants to run %s%s:\n%s\n"
                         tool
                         (if remote (format " on %s" remote) "")
                         summary))
         (choices (append '(?y ?n ?! ?a ??) (and editable '(?e))))
         (legend (concat "[y]es once  [n]o  [!] always this tool  [a]ll tools this session  [?] full input"
                         (and editable "  [e]dit first")))
         (preview-win nil))
    (setq efrit-permission--last-request request)
    (unwind-protect
        (progn
          ;; Show what would change before asking
          (when (and efrit-permission-preview (not noninteractive))
            (when-let* ((pv (efrit-permission--preview-text tool input)))
              (when (and (stringp (car pv)) (not (string-empty-p (car pv))))
                (setq preview-win
                      (efrit-show-preview "*efrit-permission-preview*" (car pv) (cdr pv))))))
          (catch 'decided
            (while t
              (let ((c (read-char-choice (concat header legend " ") choices)))
                (pcase c
                  (?y (throw 'decided 'allow))
                  (?n (throw 'decided 'deny))
                  (?! (throw 'decided 'allow-tool))
                  (?a (throw 'decided 'allow-all))
                  (?e
                   (let* ((field (nth 1 editable))
                          (mode (nth 2 editable))
                          (cur (or (efrit-permission--input-get input field) ""))
                          (new (condition-case nil
                                   (efrit-edit-in-buffer cur (format "%s %s" tool field) mode)
                                 (quit nil))))
                     (when (and new (not (equal new cur)))
                       (let ((copy (if (hash-table-p input) (copy-hash-table input)
                                     (make-hash-table :test 'equal))))
                         (puthash field new copy)
                         (throw 'decided (cons 'edit copy))))
                     ;; unchanged or cancelled: ask again
                     nil))
                  (?? (efrit-permission-show-last)))))))
      (when (window-live-p preview-win)
        (ignore-errors (quit-window nil preview-win))))))

(defun efrit-permission-show-last ()
  "Show the full input of the last permission request in a buffer."
  (interactive)
  (if (null efrit-permission--last-request)
      (message "No permission request yet")
    (let ((req efrit-permission--last-request))
      (with-current-buffer (get-buffer-create "*efrit-permission*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "Tool: %s  Class: %s\nProject root: %s\n\n"
                          (alist-get :tool req) (alist-get :class req)
                          (alist-get :project-root req)))
          (let ((input (alist-get :input req)))
            (if (hash-table-p input)
                (maphash (lambda (k v) (insert (format "%s:\n%s\n\n" k v))) input)
              (insert (format "%S" input)))))
        (special-mode)
        (display-buffer (current-buffer))))))

;;; Entry point used by the dispatcher

(defun efrit-permission-check (tool input &optional session-id)
  "Decide whether TOOL may run with INPUT for SESSION-ID.
Returns `allow' or `deny'.  Records session grants as a side effect
and writes an audit entry for every gated decision.  If the user
edited the input at the prompt, the edited version is available from
`efrit-permission-last-input' immediately after this returns `allow'."
  (setq efrit-permission-last-input input)
  (if (not (efrit-permission-needed-p tool))
      'allow
    (let* ((class (efrit-permission-tool-class tool))
           (root (ignore-errors (efrit-tool--get-project-root)))
           (request `((:tool . ,tool) (:class . ,class) (:input . ,input)
                      (:summary . ,(efrit-permission-summarize tool input))
                      (:project-root . ,root) (:session-id . ,session-id)))
           (decision
            (cond
             ((efrit-permission--granted-p session-id tool) 'allow)
             ((and efrit-permission-responder-function
                   (condition-case err
                       (funcall efrit-permission-responder-function request)
                     (error
                      (efrit-log 'warn "permission responder signalled: %s"
                                 (error-message-string err))
                      nil))))
             (t (condition-case nil
                    (efrit-permission--prompt request)
                  (quit 'deny))))))
      (pcase decision
        ('allow-tool (efrit-permission--grant session-id tool) (setq decision 'allow))
        ('allow-all (efrit-permission--grant session-id 'all) (setq decision 'allow))
        (`(edit . ,new-input)
         (setq efrit-permission-last-input new-input
               decision 'allow)))
      (when (fboundp 'efrit-tool-audit)
        (efrit-tool-audit tool (if (hash-table-p input) input (list :input input))
                          (if (eq decision 'allow) :permitted :denied)))
      (efrit-log 'info "permission %s: %s (%s)" decision tool class)
      (efrit-publish 'permission `((:session-id . ,session-id)
                                   (:tool . ,tool) (:decision . ,decision)))
      decision)))

(defconst efrit-permission-denied-result
  "Error permission denied: the user declined to allow this tool call. The turn ends here; do not retry. Wait for the user's next instruction."
  "Tool result recorded when the user denies a call.
Starts with \"Error \" so the loop's error detection sees it, and is
matched literally by `efrit-loop-execute-tools' to end the turn.")

(provide 'efrit-permissions)

;;; efrit-permissions.el ends here
