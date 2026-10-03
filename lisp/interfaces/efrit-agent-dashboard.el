;;; efrit-agent-dashboard.el --- Every agent buffer at a glance -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.8.5
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, ai, convenience

;;; Commentary:

;; With several agent buffers (efrit-agent-instances) you want one
;; list: `M-x efrit-agent-dashboard' is a `tabulated-list-mode' of
;; them: buffer, project, branch, dirty files, session status,
;; queued inputs, model, last activity.  RET visits, k kills the
;; buffer (cancelling its turn), s opens the project's VC status, g
;; refreshes; it refreshes itself on status events too (after
;; ai-code-interface's session dashboard, 2026-09-28).

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'tabulated-list)
(require 'efrit-agent-core)
(require 'efrit-events)

(declare-function efrit-repl-session-status "efrit-repl-session")
(declare-function efrit-repl-session-queue "efrit-repl-session")
(declare-function efrit-repl-session-last-activity "efrit-repl-session")
(declare-function efrit-repl-session-project-root "efrit-repl-session")
(declare-function efrit-vcs-branch "efrit-vcs")
(declare-function efrit-vcs-status-files "efrit-vcs")
(declare-function efrit-vcs-root "efrit-vcs")
(declare-function efrit-vcs-show-status "efrit-vcs")
(declare-function efrit-agent-cancel "efrit-agent")
(declare-function efrit-agent-display "efrit-agent-core")
(defvar efrit-agent--repl-session)
(defvar efrit-agent--instance)
(defvar efrit-default-model)

(defconst efrit-agent-dashboard-buffer "*efrit agents*")

(defun efrit-agent-dashboard--row (buf)
  "The dashboard row for agent BUF."
  (with-current-buffer buf
    (let* ((session efrit-agent--repl-session)
           (root (or (and session (efrit-repl-session-project-root session)) default-directory))
           (vc-root (ignore-errors (efrit-vcs-root root)))
           (branch (or (and vc-root (ignore-errors (efrit-vcs-branch vc-root))) ""))
           (dirty (and vc-root (ignore-errors (length (efrit-vcs-status-files vc-root)))))
           (status (if session (format "%s" (efrit-repl-session-status session)) "no session"))
           (queue (if session (length (efrit-repl-session-queue session)) 0))
           (last (and session (efrit-repl-session-last-activity session))))
      (list buf
            (vector (buffer-name buf)
                    (abbreviate-file-name (directory-file-name root))
                    branch
                    (if dirty (number-to-string dirty) "")
                    (propertize status 'face (pcase status
                                               ("working" 'warning) ("waiting" 'font-lock-keyword-face)
                                               ("failed" 'error) (_ 'default)))
                    (if (> queue 0) (number-to-string queue) "")
                    (or (bound-and-true-p efrit-default-model) "")
                    (if last (format-time-string "%H:%M:%S" last) ""))))))

(defun efrit-agent-dashboard--entries ()
  (mapcar #'efrit-agent-dashboard--row (efrit-agent-buffers)))

(defun efrit-agent-dashboard-visit ()
  "Visit the agent buffer at point."
  (interactive)
  (when-let* ((buf (tabulated-list-get-id)))
    (efrit-agent-display buf t)))

(defun efrit-agent-dashboard-kill ()
  "Kill the agent buffer at point, cancelling its turn."
  (interactive)
  (when-let* ((buf (tabulated-list-get-id)))
    (when (yes-or-no-p (format "Kill %s? " (buffer-name buf)))
      (with-current-buffer buf (ignore-errors (efrit-agent-cancel)))
      (kill-buffer buf)
      (revert-buffer))))

(defun efrit-agent-dashboard-vc-status ()
  "Open the VC status of the project at point (Magit when loaded)."
  (interactive)
  (when-let* ((buf (tabulated-list-get-id)))
    (require 'efrit-vcs)
    (efrit-vcs-show-status (with-current-buffer buf default-directory))))

(defvar efrit-agent-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'efrit-agent-dashboard-visit)
    (define-key map (kbd "k") #'efrit-agent-dashboard-kill)
    (define-key map (kbd "s") #'efrit-agent-dashboard-vc-status)
    map))

(define-derived-mode efrit-agent-dashboard-mode tabulated-list-mode "Efrit-Agents"
  "The agent buffers: RET visits, k kills, s VC status, g refreshes."
  (setq tabulated-list-format
        [("Buffer" 26 t) ("Project" 28 t) ("Branch" 14 t) ("Dirty" 5 t :right-align t)
         ("Status" 9 t) ("Queue" 5 t :right-align t) ("Model" 24 t) ("Last" 8 t)])
  (setq tabulated-list-padding 1)
  (setq tabulated-list-entries #'efrit-agent-dashboard--entries)
  (tabulated-list-init-header)
  (add-hook 'kill-buffer-hook #'efrit-agent-dashboard--unsubscribe nil t))

(defun efrit-agent-dashboard--on-status (_event)
  (when-let* ((buf (get-buffer efrit-agent-dashboard-buffer)))
    (when (get-buffer-window buf t)
      (with-current-buffer buf (revert-buffer)))))

(defun efrit-agent-dashboard--unsubscribe ()
  (efrit-unsubscribe 'status #'efrit-agent-dashboard--on-status))

;;;###autoload
(defun efrit-agent-dashboard ()
  "List every agent buffer with its project, branch, status and queue."
  (interactive)
  (require 'efrit-vcs)
  (let ((buf (get-buffer-create efrit-agent-dashboard-buffer)))
    (with-current-buffer buf
      (unless (derived-mode-p 'efrit-agent-dashboard-mode)
        (efrit-agent-dashboard-mode))
      (tabulated-list-print t))
    (efrit-subscribe 'status #'efrit-agent-dashboard--on-status)
    (pop-to-buffer buf)))

(provide 'efrit-agent-dashboard)

;;; efrit-agent-dashboard.el ends here
