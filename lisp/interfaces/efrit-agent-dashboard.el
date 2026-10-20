;;; efrit-agent-dashboard.el --- Every agent buffer at a glance -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.12.12
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, ai, convenience

;;; Commentary:

;; With several agent buffers (efrit-agent-instances) you want one
;; list: `M-x efrit-agent-dashboard' is a `tabulated-list-mode' of
;; them, grouped by FOLDER (a tag on the session, `F' sets it; nothing
;; moves on disk), with the state in colour and a counts line at the
;; top ("2 working · 1 waiting · 1 unread · 3 idle").  Columns: the
;; attention mark, buffer, folder, project (a worktree when the agent
;; has its own), branch, dirty files, status, queue, model, last
;; activity.  (After ai-code-interface's session dashboard,
;; 2026-09-28, and M-x Machina's board, 2026-10-14.)
;;
;; Keys: RET visits, ] and [ go to the next and previous agent that
;; needs you, z fills the frame with the agent at point (z again puts
;; the windows back), i diagnoses it, F sets its folder, n makes a
;; new agent in a fresh worktree, W relocates a saved session, e opens
;; eshell in its project, k kills it (cancelling its turn), s opens
;; the project's VC status, g refreshes.  It refreshes itself on
;; status and attention events.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'tabulated-list)
(require 'efrit-agent-core)
(require 'efrit-events)
(require 'efrit-agent-ring)
(require 'efrit-pill)
(defvar efrit-pill-e-alert-color)

(declare-function efrit-repl-session-status "efrit-repl-session")
(declare-function efrit-repl-session-queue "efrit-repl-session")
(declare-function efrit-repl-session-last-activity "efrit-repl-session")
(declare-function efrit-repl-session-project-root "efrit-repl-session")
(declare-function efrit-repl-session-meta-get "efrit-repl-session")
(declare-function efrit-repl-session-meta-put "efrit-repl-session")
(declare-function efrit-vcs-branch "efrit-vcs")
(declare-function efrit-vcs-status-files "efrit-vcs")
(declare-function efrit-vcs-root "efrit-vcs")
(declare-function efrit-vcs-show-status "efrit-vcs")
(declare-function efrit-agent-cancel "efrit-agent")
(declare-function efrit-agent-display "efrit-agent-core")
(declare-function efrit-agent-diagnose "efrit-agent-diagnose")
(declare-function efrit-agent-new-worktree-instance "efrit-agent-instances")
(declare-function efrit-agent-new-instance "efrit-agent-instances")
(declare-function efrit-agent-tab-line-label-default "efrit-agent-tab-line")
(declare-function efrit-agent-relocate-session "efrit-agent-instances")
(declare-function eshell "eshell")
(defvar efrit-agent--repl-session)
(defvar efrit-agent--instance)
(defvar efrit-default-model)

(defconst efrit-agent-dashboard-buffer "*efrit agents*")

(defface efrit-agent-dashboard-folder '((t :inherit font-lock-type-face :weight bold))
  "A folder heading row." :group 'efrit-agent-ring)

;;;; Folders

(defun efrit-agent-folder (buffer)
  "The folder tag of agent BUFFER's session, or \"\"."
  (let ((session (buffer-local-value 'efrit-agent--repl-session buffer)))
    (or (and session (fboundp 'efrit-repl-session-meta-get)
             (efrit-repl-session-meta-get session :folder))
        "")))

(defun efrit-agent-set-folder (buffer folder)
  "Tag agent BUFFER's session with FOLDER (\"Work/Billing\"); empty clears it.
Nothing moves on disk; the dashboard groups by it."
  (interactive
   (let ((buf (or (and (derived-mode-p 'efrit-agent-mode) (current-buffer))
                  (and (derived-mode-p 'tabulated-list-mode) (tabulated-list-get-id))
                  (user-error "No agent buffer here"))))
     (list buf (read-string "Folder (empty for none): " (efrit-agent-folder buf)))))
  (let ((session (buffer-local-value 'efrit-agent--repl-session buffer))
        (folder (string-trim (replace-regexp-in-string "/+" "/" folder) "/+" "/+")))
    (unless session (user-error "%s has no session" (buffer-name buffer)))
    (efrit-repl-session-meta-put session :folder (and (not (string-empty-p folder)) folder))
    (efrit-publish 'attention-changed (list (cons :buffer buffer)))
    folder))

;;;; Rows

(defun efrit-agent-dashboard--row (buf)
  "The dashboard row for agent BUF."
  (with-current-buffer buf
    (let* ((session efrit-agent--repl-session)
           (root (or (and session (efrit-repl-session-project-root session)) default-directory))
           (vc-root (ignore-errors (efrit-vcs-root root)))
           (branch (or (and vc-root (ignore-errors (efrit-vcs-branch vc-root))) ""))
           (dirty (and vc-root (ignore-errors (length (efrit-vcs-status-files vc-root)))))
           (status (cond ((buffer-local-value 'efrit-agent--waiting buf) "waiting")
                         (session (format "%s" (efrit-repl-session-status session)))
                         (t "no session")))
           (queue (if session (length (efrit-repl-session-queue session)) 0))
           (last (and session (efrit-repl-session-last-activity session)))
           (mark (efrit-agent-attention-dot buf ""))
           (folder (efrit-agent-folder buf)))
      (list buf
            (vector mark
                    (efrit-pill (efrit-agent-tab-line-label-default buf) :key (buffer-name buf))
                    (if (string-empty-p folder) "" (efrit-pill folder :key folder))
                    (abbreviate-file-name (directory-file-name root))
                    branch
                    (if dirty (number-to-string dirty) "")
                    (efrit-pill status :state (intern status))
                    (if (> queue 0) (number-to-string queue) "")
                    (or (bound-and-true-p efrit-default-model) "")
                    (if last (format-time-string "%H:%M:%S" last) ""))))))

(defun efrit-agent-dashboard--folder-row (folder count)
  "A heading row for FOLDER with COUNT agents; its id is the folder string."
  (list (concat "folder:" folder)
        (vector "" (if (string-empty-p folder)
                       (propertize "(no folder)" 'face 'efrit-agent-dashboard-folder)
                     (efrit-pill folder :key folder :strong t :scale 1.0))
                "" (propertize (format "%d agent%s" count (if (= 1 count) "" "s")) 'face 'shadow)
                "" "" "" "" "" "")))

(defun efrit-agent-dashboard--entries ()
  "Rows grouped by folder: a heading, then the folder's agents in ring order."
  (let* ((ring (efrit-agent-ring))
         (folders (sort (delete-dups (mapcar #'efrit-agent-folder ring)) #'string<))
         (rows nil))
    (if (equal folders '(""))
        (mapcar #'efrit-agent-dashboard--row ring)
      (dolist (folder folders)
        (let ((members (seq-filter (lambda (b) (equal folder (efrit-agent-folder b))) ring)))
          (push (efrit-agent-dashboard--folder-row folder (length members)) rows)
          (dolist (b members) (push (efrit-agent-dashboard--row b) rows))))
      (nreverse rows))))

(defun efrit-agent-dashboard--buffer-at-point ()
  (let ((id (tabulated-list-get-id)))
    (and (bufferp id) (buffer-live-p id) id)))

(defun efrit-agent-dashboard--require-buffer ()
  (or (efrit-agent-dashboard--buffer-at-point) (user-error "No agent on this line")))

;;;; Commands

(defun efrit-agent-dashboard-visit ()
  "Visit the agent buffer at point."
  (interactive)
  (efrit-agent-display (efrit-agent-dashboard--require-buffer) t))

(defun efrit-agent-dashboard-kill ()
  "Kill the agent buffer at point, cancelling its turn."
  (interactive)
  (let ((buf (efrit-agent-dashboard--require-buffer)))
    (when (yes-or-no-p (format "Kill %s? " (buffer-name buf)))
      (with-current-buffer buf (ignore-errors (efrit-agent-cancel)))
      (kill-buffer buf)
      (revert-buffer))))

(defun efrit-agent-dashboard-vc-status ()
  "Open the VC status of the project at point (Magit when loaded)."
  (interactive)
  (let ((buf (efrit-agent-dashboard--require-buffer)))
    (require 'efrit-vcs)
    (efrit-vcs-show-status (with-current-buffer buf default-directory))))

(defun efrit-agent-dashboard-diagnose ()
  "Diagnose the agent at point without touching it."
  (interactive)
  (require 'efrit-agent-diagnose)
  (efrit-agent-diagnose (efrit-agent-dashboard--require-buffer)))

(defun efrit-agent-dashboard-set-folder ()
  "Set the folder of the agent at point."
  (interactive)
  (call-interactively #'efrit-agent-set-folder)
  (revert-buffer))

(defun efrit-agent-dashboard-eshell ()
  "Open eshell in the project of the agent at point."
  (interactive)
  (let* ((buf (efrit-agent-dashboard--require-buffer))
         (default-directory (with-current-buffer buf default-directory)))
    (eshell t)))

(defun efrit-agent-dashboard-new (&optional worktree)
  "Make a new agent, asking for its directory (default: the project at point).
With WORKTREE (a prefix argument, or `N'), a fresh Git worktree of that
project instead."
  (interactive "P")
  (let* ((buf (efrit-agent-dashboard--buffer-at-point))
         (default-directory (if buf (with-current-buffer buf default-directory) default-directory)))
    (if worktree
        (call-interactively #'efrit-agent-new-worktree-instance)
      (call-interactively #'efrit-agent-new-instance))
    (when (buffer-live-p (get-buffer efrit-agent-dashboard-buffer))
      (with-current-buffer efrit-agent-dashboard-buffer (revert-buffer)))))

(defun efrit-agent-dashboard-new-worktree ()
  "Make a new agent in a fresh worktree of the project at point."
  (interactive)
  (efrit-agent-dashboard-new t))

(defun efrit-agent-dashboard-relocate ()
  "Point a saved session at a checkout that replaced its worktree."
  (interactive)
  (call-interactively #'efrit-agent-relocate-session))

(defun efrit-agent-dashboard-focus ()
  "Fill the frame with the agent at point; `efrit-agent-focus' again restores."
  (interactive)
  (efrit-agent-focus (efrit-agent-dashboard--require-buffer)))

;;;; Focus

(defun efrit-agent-focus (&optional buffer)
  "Show agent BUFFER (default the current or project's one) alone in this frame.
The window layout is remembered on the frame; calling this again,
from anywhere, puts it back (after the Claude desktop app and M-x
Machina's z, 2026-10-14)."
  (interactive)
  (if-let* ((saved (frame-parameter nil 'efrit-agent-focus-layout)))
      (progn (set-frame-parameter nil 'efrit-agent-focus-layout nil)
             (when (window-configuration-p saved)
               (set-window-configuration saved))
             (message "efrit: layout restored"))
    (let ((buf (or buffer
                   (and (derived-mode-p 'efrit-agent-mode) (current-buffer))
                   (car (efrit-agent-ring-attention-queue))
                   (car (efrit-agent-ring))
                   (user-error "No agent buffer to focus"))))
      (set-frame-parameter nil 'efrit-agent-focus-layout (current-window-configuration))
      (let ((ignore-window-parameters t))
        (delete-other-windows)
        (set-window-dedicated-p (selected-window) nil)
        (switch-to-buffer buf))
      (message "efrit: focus on %s; the same key restores your windows" (buffer-name buf)))))

;;;; Mode

(defvar efrit-agent-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    ;; a defvar'd map does not get `tabulated-list-mode-map' as parent
    ;; from `define-derived-mode' (that only happens for a map it makes
    ;; itself), so g, n, p, S were missing (tzz, 2026-10-15)
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "g") #'revert-buffer)
    (define-key map (kbd "RET") #'efrit-agent-dashboard-visit)
    (define-key map (kbd "k") #'efrit-agent-dashboard-kill)
    (define-key map (kbd "s") #'efrit-agent-dashboard-vc-status)
    (define-key map (kbd "i") #'efrit-agent-dashboard-diagnose)
    (define-key map (kbd "F") #'efrit-agent-dashboard-set-folder)
    (define-key map (kbd "e") #'efrit-agent-dashboard-eshell)
    (define-key map (kbd "n") #'efrit-agent-dashboard-new)
    (define-key map (kbd "N") #'efrit-agent-dashboard-new-worktree)
    (define-key map (kbd "W") #'efrit-agent-dashboard-relocate)
    (define-key map (kbd "z") #'efrit-agent-dashboard-focus)
    (define-key map (kbd "]") #'efrit-agent-next-attention)
    (define-key map (kbd "[") #'efrit-agent-previous-attention)
    (define-key map (kbd "u") #'efrit-agent-dashboard-toggle-unread)
    map))

(defun efrit-agent-dashboard-toggle-unread ()
  "Mark the agent at point unread, or read when it is unread."
  (interactive)
  (efrit-agent-toggle-unread (efrit-agent-dashboard--require-buffer))
  (revert-buffer))

(defun efrit-agent-dashboard--header ()
  (or (efrit-agent-ring-summary) "no agent buffers"))

(define-derived-mode efrit-agent-dashboard-mode tabulated-list-mode "Efrit-Agents"
  "The agent buffers by folder.  \\{efrit-agent-dashboard-mode-map}"
  (setq tabulated-list-format
        [("" 3 nil) ("Buffer" 24 t) ("Folder" 14 t) ("Project" 26 t) ("Branch" 14 t)
         ("Dirty" 5 t :right-align t) ("Status" 9 t) ("Queue" 5 t :right-align t)
         ("Model" 22 t) ("Last" 8 t)])
  (setq tabulated-list-padding 1)
  (setq tabulated-list-entries #'efrit-agent-dashboard--entries)
  (tabulated-list-init-header)
  (add-hook 'tabulated-list-revert-hook #'efrit-agent-dashboard--set-mode-line nil t)
  (add-hook 'kill-buffer-hook #'efrit-agent-dashboard--unsubscribe nil t))

(defun efrit-agent-dashboard--set-mode-line ()
  "The logo (red when any agent needs you) and the counts, in the mode line."
  (let* ((needs (efrit-agent-ring-attention-queue))
         (logo (efrit-agent-logo (and needs efrit-pill-e-alert-color))))
    (setq mode-line-process
          (list " " (if logo (efrit-agent-logo-clickable logo "efrit agents dashboard") "")
                " " (efrit-agent-dashboard--header)))))

(defun efrit-agent-dashboard--on-change (_event)
  (when-let* ((buf (get-buffer efrit-agent-dashboard-buffer)))
    (when (get-buffer-window buf t)
      (with-current-buffer buf (revert-buffer)))))

(defun efrit-agent-dashboard--unsubscribe ()
  (efrit-unsubscribe 'status #'efrit-agent-dashboard--on-change)
  (efrit-unsubscribe 'attention-changed #'efrit-agent-dashboard--on-change))

;;;###autoload
(defun efrit-agent-dashboard ()
  "List every agent buffer by folder with its state, project, branch and queue."
  (interactive)
  (require 'efrit-vcs)
  (require 'efrit-agent-tab-line)
  (let ((buf (get-buffer-create efrit-agent-dashboard-buffer)))
    (with-current-buffer buf
      (unless (derived-mode-p 'efrit-agent-dashboard-mode)
        (efrit-agent-dashboard-mode))
      (tabulated-list-print t)
      (efrit-agent-dashboard--set-mode-line))
    (efrit-subscribe 'status #'efrit-agent-dashboard--on-change)
    (efrit-subscribe 'attention-changed #'efrit-agent-dashboard--on-change)
    (pop-to-buffer buf)))

(provide 'efrit-agent-dashboard)

;;; efrit-agent-dashboard.el ends here
