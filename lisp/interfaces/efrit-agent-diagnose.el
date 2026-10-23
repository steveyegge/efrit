;;; efrit-agent-diagnose.el --- What is wrong with this agent session, without touching it -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.12.12
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, ai

;;; Commentary:

;; `efrit-doctor' answers "is efrit set up"; this answers "why is THIS
;; session not working" (after M-x Machina's `i', 2026-10-14).
;; `efrit-agent-diagnose-collect' reads saved and live state and
;; returns rows (LABEL SEVERITY DETAIL), SEVERITY one of `ok', `info',
;; `warning', `blocked'.  It starts nothing, sends nothing, repairs
;; nothing.  `efrit-agent-diagnose' shows the rows in a popup; `w'
;; there copies a report without paths' private parts or error text.
;;
;; Checks: the project root and recorded worktree exist and are the
;; checkout they were; the branch matches; the session file is
;; readable and parses; the model and endpoint are configured (not
;; probed); the sandbox store of the project parses; the live status,
;; open prompt, queue and last API error; the latest Emacs error
;; recorded by get_last_error.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'efrit-config)
(require 'efrit-common)
(require 'efrit-pill)

(declare-function efrit-vcs-root "efrit-vcs")
(declare-function efrit-vcs-branch "efrit-vcs")
(declare-function efrit-repl-session-get "efrit-repl-session")
(declare-function efrit-repl-session-id "efrit-repl-session")
(declare-function efrit-repl-session-status "efrit-repl-session")
(declare-function efrit-repl-session-project-root "efrit-repl-session")
(declare-function efrit-repl-session-queue "efrit-repl-session")
(declare-function efrit-repl-session-meta-get "efrit-repl-session")
(declare-function efrit-repl-session-api-messages "efrit-repl-session")
(declare-function efrit-repl-session-buffer "efrit-repl-session")
(declare-function efrit-session-persist-load "efrit-session-persist")
(declare-function efrit-session-persist--get-file-path "efrit-session-persist")
(declare-function efrit-sandbox-store-file "efrit-sandbox-store")
(declare-function efrit-last-error-list "efrit-tool-last-error")
(declare-function efrit-show-popup "efrit-ui-helpers")
(declare-function efrit-agent-buffer-for "efrit-agent-core")
(defvar efrit-agent--repl-session)
(defvar efrit-agent--waiting)
(defvar efrit-agent--unread)
(defvar efrit-agent--thinking-label)
(defvar efrit-default-model)
(defvar efrit-api-url)
(defvar efrit-sandbox-enabled)

(defface efrit-diagnose-ok '((t :inherit success)) "An ok row." :group 'efrit)
(defface efrit-diagnose-warning '((t :inherit warning)) "A warning row." :group 'efrit)
(defface efrit-diagnose-blocked '((t :inherit error :weight bold)) "A blocking row." :group 'efrit)

(defun efrit-agent-diagnose--session (buffer-or-id)
  "(SESSION BUFFER LIVE-P) for BUFFER-OR-ID: a live buffer's session, else the saved one."
  (cond
   ((and (bufferp buffer-or-id) (buffer-live-p buffer-or-id))
    (list (buffer-local-value 'efrit-agent--repl-session buffer-or-id) buffer-or-id t))
   ((stringp buffer-or-id)
    (if-let* ((buf (and (fboundp 'efrit-agent-buffer-for) (efrit-agent-buffer-for buffer-or-id))))
        (list (buffer-local-value 'efrit-agent--repl-session buf) buf t)
      (require 'efrit-session-persist)
      (list (ignore-errors (efrit-session-persist-load buffer-or-id)) nil nil)))
   (t (list nil nil nil))))

(defun efrit-agent-diagnose-collect (buffer-or-id)
  "The rows (LABEL SEVERITY DETAIL) describing the session of BUFFER-OR-ID."
  (require 'efrit-vcs)
  (pcase-let* ((`(,session ,buffer ,live) (efrit-agent-diagnose--session buffer-or-id))
               (rows nil))
    (cl-flet ((row (label severity detail) (push (list label severity (format "%s" detail)) rows)))
      (if (null session)
          (row "Session" 'blocked (format "no live or saved session for %S" buffer-or-id))
        (let* ((id (efrit-repl-session-id session))
               (root (efrit-repl-session-project-root session))
               (worktree (efrit-repl-session-meta-get session :worktree))
               (branch (efrit-repl-session-meta-get session :branch))
               (file (progn (require 'efrit-session-persist) (efrit-session-persist--get-file-path id))))
          (row "Session" 'info id)
          (row "Buffer" (if live 'ok 'info) (if live (buffer-name buffer) "not open in this Emacs"))
          ;; where it lives
          (row "Project root" (cond ((null root) 'warning) ((file-directory-p root) 'ok) (t 'blocked))
               (cond ((null root) "none recorded") ((file-directory-p root) (abbreviate-file-name root))
                     (t (format "%s is missing" (abbreviate-file-name root)))))
          (when worktree
            (row "Recorded worktree" (if (file-directory-p worktree) 'ok 'blocked)
                 (if (file-directory-p worktree) (abbreviate-file-name worktree)
                   (format "%s is missing; M-x efrit-agent-relocate-session" (abbreviate-file-name worktree)))))
          (let* ((dir (or worktree root))
                 (vc-root (and dir (file-directory-p dir) (ignore-errors (efrit-vcs-root dir)))))
            (when (and dir (file-directory-p dir))
              (row "Version control" (if vc-root 'ok 'info)
                   (if vc-root (format "Git tree %s" (abbreviate-file-name vc-root)) "not a version-controlled tree"))
              (when (and vc-root worktree (not (equal (file-truename vc-root) (file-truename worktree))))
                (row "Worktree mismatch" 'blocked
                     (format "the recorded worktree is inside another checkout (%s)" (abbreviate-file-name vc-root))))
              (when vc-root
                (let ((actual (ignore-errors (efrit-vcs-branch vc-root))))
                  (row "Branch" (cond ((null branch) 'info) ((equal actual branch) 'ok) (t 'warning))
                       (cond ((null branch) (or actual "detached"))
                             ((equal actual branch) actual)
                             (t (format "%s now, %s recorded" (or actual "detached") branch))))))))
          ;; the saved record
          (row "Session file" (cond ((not (file-exists-p file)) 'info)
                                    ((ignore-errors (efrit-session-persist-load id)) 'ok)
                                    (t 'blocked))
               (cond ((not (file-exists-p file)) "not saved yet")
                     ((ignore-errors (efrit-session-persist-load id)) (abbreviate-file-name file))
                     (t (format "%s does not parse" (abbreviate-file-name file)))))
          (row "History" 'info (format "%d API message(s)" (length (efrit-repl-session-api-messages session))))
          ;; model and endpoint, static
          (row "Model" (if (and (stringp efrit-default-model) (not (string-empty-p efrit-default-model))) 'ok 'blocked)
               (or efrit-default-model "none configured"))
          (row "Endpoint" 'info (or efrit-api-url "the default API URL"))
          (row "API key" (if (ignore-errors (efrit-common-get-api-key)) 'ok 'blocked)
               (if (ignore-errors (efrit-common-get-api-key)) "found" "not found (auth-source / environment)"))
          ;; the sandbox store of the project
          (when (and root (file-directory-p root) (require 'efrit-sandbox-store nil t))
            (let ((store (efrit-sandbox-store-file root)))
              (when (file-exists-p store)
                (row "Sandbox store"
                     (if (ignore-errors (with-temp-buffer (insert-file-contents store) (json-parse-buffer) t)) 'ok 'blocked)
                     (abbreviate-file-name store)))))
          (row "Sandbox" 'info (if (bound-and-true-p efrit-sandbox-enabled) "on" "OFF: every tool runs unchecked"))
          ;; live state
          (when live
            (with-current-buffer buffer
              (row "Status" (pcase (efrit-repl-session-status session) ('failed 'warning) (_ 'info))
                   (efrit-repl-session-status session))
              (when (bound-and-true-p efrit-agent--thinking-label)
                (row "Request" 'info (format "in flight: %s" efrit-agent--thinking-label)))
              (when (bound-and-true-p efrit-agent--waiting)
                (row "Prompt open" 'warning (format "%s; answer it in the buffer" efrit-agent--waiting)))
              (when (bound-and-true-p efrit-agent--unread) (row "Unread" 'info "a reply you have not read"))
              (let ((queue (efrit-repl-session-queue session)))
                (when queue (row "Queue" 'info (format "%d input(s) waiting for the turn to end" (length queue)))))))
          ;; last Emacs error
          (when (and (require 'efrit-tool-last-error nil t) (fboundp 'efrit-last-error-list))
            (when-let* ((last (car (efrit-last-error-list))))
              (row "Last Emacs error" 'info
                   (truncate-string-to-width (format "%s" (or (plist-get last :message) last)) 100 nil nil "…")))))))
    (nreverse rows)))

(defun efrit-agent-diagnose--face (severity)
  (pcase severity
    ('ok 'efrit-diagnose-ok) ('warning 'efrit-diagnose-warning) ('blocked 'efrit-diagnose-blocked) (_ 'shadow)))

(defun efrit-agent-diagnose-text (rows &optional shareable)
  "ROWS as text.  With SHAREABLE, home directories and error text are elided."
  (mapconcat (lambda (r)
               (pcase-let ((`(,label ,severity ,detail) r))
                 (format "%-18s %s %s" label
                         (if shareable (format "%-8s" (upcase (symbol-name severity)))
                           (efrit-pill (format "%-7s" (symbol-name severity)) :state severity
                                       :strong (eq severity 'blocked)))
                         (if shareable
                             (replace-regexp-in-string (regexp-quote (expand-file-name "~")) "~"
                                                       (if (string-match-p "error" label) "(elided)" detail) t t)
                           detail))))
             rows "\n"))

(defvar-local efrit-agent-diagnose--rows nil)

(defun efrit-agent-diagnose-copy ()
  "Copy a shareable report of the rows shown."
  (interactive)
  (kill-new (substring-no-properties (efrit-agent-diagnose-text efrit-agent-diagnose--rows t)))
  (message "efrit: report copied (home paths shortened, error text elided)"))

;;;###autoload
(defun efrit-agent-diagnose (&optional buffer-or-id)
  "Show what is known about the agent session in BUFFER-OR-ID (default here).
Reads state only: starts nothing, sends nothing.  `w' copies a
shareable report, `g' refreshes, `q' closes."
  (interactive
   (list (cond ((derived-mode-p 'efrit-agent-mode) (current-buffer))
               ((and (derived-mode-p 'tabulated-list-mode) (bufferp (tabulated-list-get-id))) (tabulated-list-get-id))
               (t (progn (require 'efrit-session-persist)
                         (completing-read "Session: "
                                          (mapcar #'car (efrit-session-persist-list)) nil t))))))
  (require 'efrit-ui-helpers)
  (let* ((rows (efrit-agent-diagnose-collect buffer-or-id))
         (blocked (cl-count 'blocked rows :key #'cadr))
         (warnings (cl-count 'warning rows :key #'cadr)))
    (efrit-show-popup
     "*efrit diagnose*"
     (lambda ()
       (setq efrit-agent-diagnose--rows rows)
       (insert (propertize (format "efrit session diagnosis: %d blocking, %d warning%s\n"
                                   blocked warnings (if (= warnings 1) "" "s"))
                           'face 'bold)
               (propertize "w copies a shareable report · g refreshes · q closes\n\n" 'face 'shadow)
               (efrit-agent-diagnose-text rows) "\n")
       (local-set-key (kbd "w") #'efrit-agent-diagnose-copy)
       (local-set-key (kbd "g") (lambda () (interactive) (efrit-agent-diagnose buffer-or-id)))))
    rows))

(declare-function efrit-session-persist-list "efrit-session-persist")

(provide 'efrit-agent-diagnose)

;;; efrit-agent-diagnose.el ends here
