;;; efrit-agent-instances.el --- Several agent buffers: naming, layout, toggling -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.12.12
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, ai, convenience

;;; Commentary:

;; One agent buffer per (project, instance).  The first instance of a
;; project is `*efrit[proj]*', later ones `*efrit[proj:2]*' or, once
;; renamed, `*efrit[proj:review]*' (after claude-code-ide's instance
;; naming, 2026-09-28).  `efrit-agent-buffer-name' stays the name of
;; the buffer used outside any project (and by the efrit-do path).
;;
;; Layout: each agent buffer is shown in a side window on
;; `efrit-agent-side' (right by default), in a slot per instance so a
;; project's buffers sit next to each other.  The windows are
;; dedicated and survive `delete-other-windows'.
;; `efrit-agent-toggle' hides every agent window of the current
;; project and remembers the set per (tab, project) in a frame
;; parameter, so each tab restores its own.
;;
;; `efrit-agent-instances-mode' turns this on; off, `efrit' keeps its
;; single bottom-window buffer.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'project)
(require 'efrit-agent-core)
(require 'efrit-repl-session)

(defvar efrit-agent--repl-session)
(declare-function efrit-agent-mode "efrit-agent")

(defgroup efrit-agent-instances nil
  "Several agent buffers, by project."
  :group 'efrit-agent)

(defcustom efrit-agent-side 'right
  "Which side of the frame agent windows live on when instances are on."
  :type '(choice (const right) (const left) (const bottom))
  :group 'efrit-agent-instances)

(defcustom efrit-agent-side-size 0.4
  "Width (or height for `bottom') of an agent side window, as a fraction."
  :type 'number
  :group 'efrit-agent-instances)

(defcustom efrit-agent-slots-per-project 16
  "Side-window slots reserved per project, so its buffers stay grouped."
  :type 'integer
  :group 'efrit-agent-instances)

(defvar efrit-agent--projects nil
  "Project roots in the order they got their slot block.")

;;;; Naming

(defun efrit-agent-project-root (&optional dir)
  "The project root for DIR (default `default-directory'), or nil outside any."
  (let ((default-directory (or dir default-directory)))
    (when-let* ((p (project-current nil)))
      (file-name-as-directory (expand-file-name (project-root p))))))

(defun efrit-agent--project-label (root)
  (if root (file-name-nondirectory (directory-file-name root)) "efrit"))

(defun efrit-agent-instance-buffer-name (root &optional name)
  "The buffer name for ROOT's instance NAME (nil: the first)."
  (if name
      (format "*efrit[%s:%s]*" (efrit-agent--project-label root) name)
    (format "*efrit[%s]*" (efrit-agent--project-label root))))

(defun efrit-agent-instances (&optional root)
  "The agent buffers of ROOT (nil: every project), first instance first."
  (sort (cl-remove-if-not
         (lambda (b)
           (let ((inst (buffer-local-value 'efrit-agent--instance b)))
             (and inst (or (null root) (equal (plist-get inst :project) root)))))
         (efrit-agent-buffers))
        (lambda (a b) (< (or (plist-get (buffer-local-value 'efrit-agent--instance a) :number) 0)
                         (or (plist-get (buffer-local-value 'efrit-agent--instance b) :number) 0)))))

(defun efrit-agent--free-number (root)
  "The lowest instance number ROOT does not use (1 is the unnamed first)."
  (let ((used (mapcar (lambda (b) (plist-get (buffer-local-value 'efrit-agent--instance b) :number))
                      (efrit-agent-instances root))))
    (cl-loop for n from 1 unless (memq n used) return n)))

;;;; Creating

(defun efrit-agent-instance-create (&optional root name)
  "A new agent buffer for ROOT (default the current project) named NAME.
Without NAME the first instance is unnamed, later ones are numbered.
Returns the buffer, in `efrit-agent-mode' with a fresh REPL session."
  (require 'efrit-agent)
  (require 'efrit-repl-session)
  (let* ((root (or root (efrit-agent-project-root)))
         (number (efrit-agent--free-number root))
         (name (or name (and (> number 1) (number-to-string number))))
         (buf (generate-new-buffer (efrit-agent-instance-buffer-name root name))))
    (with-current-buffer buf
      (setq default-directory (or root default-directory))
      (efrit-agent-mode)
      (setq efrit-agent--instance (list :project root :name name :number number))
      (efrit-agent--init-regions)
      (efrit-agent--setup-regions)
      (setq efrit-agent--repl-session (efrit-repl-session-create default-directory))
      (setf (efrit-repl-session-buffer efrit-agent--repl-session) buf))
    (unless (member root efrit-agent--projects)
      (setq efrit-agent--projects (append efrit-agent--projects (list root))))
    buf))

(defun efrit-agent--worktree-slug (name)
  (let ((slug (string-trim (replace-regexp-in-string "[^a-z0-9]+" "-" (downcase name)) "-+" "-+")))
    (if (string-empty-p slug) "agent" slug)))

;;;###autoload
(defun efrit-agent-new-worktree-instance (name &optional branch destination root)
  "A new agent buffer NAME on a fresh Git worktree of ROOT's repository.
BRANCH (default agent/NAME) is created from HEAD at DESTINATION
\(default a sibling directory ROOT-NAME).  Existing branches and
directories are refused.  The buffer's project is the worktree, so
its edits, VC and sandbox grants stay there; the session records the
worktree and branch and `efrit-resume' checks them.  Interactively the
repository is asked for first, with completion, defaulting to the
current buffer's.  Every prompt is answered before Git is touched."
  (interactive
   (let* ((here (efrit-vcs-root))
          ;; always ask, with completion, defaulting to this buffer's
          ;; repository when there is one (tzz, 2026-10-15: "whether
          ;; there's a git tree in the current buffer or not, prompt
          ;; with path completion")
          (chosen (read-directory-name "Repository to branch from: "
                                       (or here default-directory) here t))
          (root (or (efrit-vcs-root chosen)
                    (user-error "%s is not inside a Git tree" (abbreviate-file-name chosen))))
          (name (read-string "Agent name: "))
          (slug (efrit-agent--worktree-slug name))
          (branch (read-string "New branch: " (concat "agent/" slug)))
          (suggested (concat (directory-file-name root) "-" slug))
          (destination (read-directory-name "New worktree directory: "
                                            (file-name-directory suggested) suggested nil
                                            (file-name-nondirectory suggested))))
     (list name branch destination root)))
  (require 'efrit-vcs)
  (let* ((root (or root (efrit-vcs-root) (user-error "Not in a Git tree")))
         (name (string-trim name))
         (branch (or branch (concat "agent/" (efrit-agent--worktree-slug name))))
         (destination (or destination (concat (directory-file-name root) "-" (efrit-agent--worktree-slug name)))))
    (when (string-empty-p name) (user-error "The agent needs a name"))
    (let* ((worktree (condition-case err
                         (efrit-vcs-worktree-add branch destination root)
                       (efrit-vcs-error (user-error "%s" (cadr err)))))
           (buf (efrit-agent-instance-create worktree name)))
      (with-current-buffer buf
        (efrit-repl-session-meta-put efrit-agent--repl-session :worktree worktree)
        (efrit-repl-session-meta-put efrit-agent--repl-session :branch branch))
      (when (called-interactively-p 'interactive)
        (efrit-agent-display buf t)
        (message "efrit: %s works in %s on %s" (buffer-name buf) (abbreviate-file-name worktree) branch))
      buf)))

(defun efrit-agent-relocate-session (session-id directory)
  "Point saved SESSION-ID at DIRECTORY, a checkout that replaced its worktree.
For a session whose recorded worktree moved.  Only the saved record
changes: the conversation, Git and the directory are left alone.  The
directory must be a Git tree; you confirm the change."
  (interactive
   (let* ((sessions (progn (require 'efrit-session-persist) (efrit-session-persist-list)))
          (id (completing-read "Session: " (mapcar #'car sessions) nil t))
          (dir (read-directory-name "Checkout it now lives in: " nil nil t)))
     (list id dir)))
  (require 'efrit-session-persist)
  (require 'efrit-vcs)
  (let* ((session (or (efrit-session-persist-load session-id)
                      (user-error "No saved session %s" session-id)))
         (root (or (efrit-vcs-root directory)
                   (user-error "%s is not inside a Git tree" (abbreviate-file-name directory))))
         (branch (efrit-vcs-branch root))
         (old (efrit-repl-session-meta-get session :worktree)))
    (when (efrit-agent-buffer-for session-id)
      (user-error "Session %s is open in a buffer; relocate a session that is not running" session-id))
    (unless (yes-or-no-p (format "Move session %s from %s to %s (branch %s)? " session-id
                                 (if old (abbreviate-file-name old) "its project") (abbreviate-file-name root)
                                 (or branch "?")))
      (user-error "Not moved"))
    (efrit-repl-session-meta-put session :worktree root)
    (efrit-repl-session-meta-put session :branch branch)
    (setf (efrit-repl-session-project-root session) root)
    (efrit-session-persist-save session)
    (message "efrit: session %s now lives in %s" session-id (abbreviate-file-name root))
    root))

(declare-function efrit-session-persist-list "efrit-session-persist")
(declare-function efrit-session-persist-load "efrit-session-persist")
(declare-function efrit-session-persist-save "efrit-session-persist")
(declare-function efrit-vcs-branch "efrit-vcs")
(declare-function efrit-agent-buffer-for "efrit-agent-core")
(declare-function efrit-repl-session-meta-put "efrit-repl-session")
(declare-function efrit-repl-session-meta-get "efrit-repl-session")
(declare-function efrit-vcs-worktree-add "efrit-vcs")
(declare-function efrit-vcs-root "efrit-vcs")

(defun efrit-agent-instance-for-project (&optional root create)
  "The most recently used agent buffer of ROOT, or a new one when CREATE."
  (let* ((root (or root (efrit-agent-project-root)))
         (bufs (efrit-agent-instances root)))
    (or (cl-find-if (lambda (b) (memq b bufs)) (buffer-list))   ; buffer-list is MRU
        (and create (efrit-agent-instance-create root)))))

(defun efrit-agent-rename-instance (name)
  "Give this agent buffer the instance NAME: `*efrit[proj:NAME]*'."
  (interactive (list (read-string "Instance name: "
                                  (plist-get efrit-agent--instance :name))))
  (unless (derived-mode-p 'efrit-agent-mode) (user-error "Not an agent buffer"))
  (let ((root (plist-get efrit-agent--instance :project)))
    (when (string-empty-p (string-trim name)) (user-error "Empty name"))
    (setq efrit-agent--instance (plist-put (or efrit-agent--instance (list :project root :number 1))
                                           :name name))
    (rename-buffer (efrit-agent-instance-buffer-name root name) t)
    (force-mode-line-update)))

;;;###autoload
(defun efrit-agent-set-directory (directory)
  "Make DIRECTORY this agent's root: project root, working directory, buffer name.
Every turn runs there (tools, shell, the sandbox's project grants).
Set it when the agent came up in the wrong place (2026-10-16: an agent
rooted in a Google Drive folder reviewing ~/.tmux.conf; every grant
was saved against the wrong project).  Also `/cd DIR' in the input."
  (interactive (list (read-directory-name "Agent directory: " nil nil t)))
  (unless (derived-mode-p 'efrit-agent-mode) (user-error "Not an agent buffer"))
  (let ((root (file-name-as-directory (expand-file-name directory))))
    (unless (file-directory-p root) (user-error "Not a directory: %s" root))
    (setq default-directory root)
    (when efrit-agent--repl-session
      (setf (efrit-repl-session-project-root efrit-agent--repl-session) root))
    (let ((old (plist-get efrit-agent--instance :project)))
      (setq efrit-agent--instance (plist-put (or efrit-agent--instance (list :number 1)) :project root))
      (unless (member root efrit-agent--projects)
        (setq efrit-agent--projects (append efrit-agent--projects (list root))))
      (when (and old (not (equal old root)) (null (efrit-agent-instances old)))
        (setq efrit-agent--projects (delete old efrit-agent--projects))))
    (rename-buffer (efrit-agent-instance-buffer-name root (plist-get efrit-agent--instance :name)) t)
    (force-mode-line-update)
    (message "efrit: this agent now works in %s" (abbreviate-file-name root))
    root))

;;;; Layout: side windows in slots

(defun efrit-agent--slot (buffer)
  "The side-window slot of BUFFER: its project's block plus its number."
  (let* ((inst (buffer-local-value 'efrit-agent--instance buffer))
         (root (plist-get inst :project))
         (block (or (cl-position root efrit-agent--projects :test #'equal) 0)))
    (+ (* block efrit-agent-slots-per-project)
       (1- (or (plist-get inst :number) 1)))))

(defun efrit-agent-display-in-side-window (buffer &optional select)
  "Show BUFFER in its side-window slot; with SELECT, select it."
  (let ((win (or (get-buffer-window buffer)
                 (display-buffer-in-side-window
                  buffer
                  `((side . ,efrit-agent-side)
                    (slot . ,(efrit-agent--slot buffer))
                    (,(if (eq efrit-agent-side 'bottom) 'window-height 'window-width)
                     . ,efrit-agent-side-size)
                    (dedicated . t)
                    (window-parameters . ((no-delete-other-windows . t))))))))
    (when (window-live-p win)
      (set-window-dedicated-p win t)
      (when select
        (select-window win)
        (with-current-buffer buffer
          (when (and efrit-agent--input-start (marker-position efrit-agent--input-start))
            (goto-char (point-max))))))
    win))

;;;; Toggling, per tab

(defun efrit-agent--tab-key ()
  "The key of the current tab (or the frame when tabs are off) for hidden sets."
  (if (and (fboundp 'tab-bar-mode) (bound-and-true-p tab-bar-mode))
      (alist-get 'name (tab-bar--current-tab))
    "frame"))

(defun efrit-agent--hidden-set (root)
  (cdr (assoc (cons (efrit-agent--tab-key) root)
              (frame-parameter nil 'efrit-agent-hidden))))

(defun efrit-agent--set-hidden-set (root buffers)
  (let ((all (assoc-delete-all (cons (efrit-agent--tab-key) root)
                               (copy-alist (frame-parameter nil 'efrit-agent-hidden)))))
    (set-frame-parameter nil 'efrit-agent-hidden
                         (if buffers (cons (cons (cons (efrit-agent--tab-key) root) buffers) all) all))))

(defun efrit-agent-toggle (&optional root)
  "Hide the agent windows of ROOT's project in this tab, or bring them back.
With none shown and none remembered, open the project's instance."
  (interactive)
  (let* ((root (or root (efrit-agent-project-root)))
         (shown (cl-remove-if-not (lambda (b) (get-buffer-window b))
                                  (efrit-agent-instances root))))
    (cond
     (shown
      (efrit-agent--set-hidden-set root shown)
      (dolist (b shown)
        (dolist (w (get-buffer-window-list b nil nil))
          (when (window-live-p w) (delete-window w)))))
     (t
      (let ((remembered (cl-remove-if-not #'buffer-live-p (efrit-agent--hidden-set root))))
        (efrit-agent--set-hidden-set root nil)
        (if remembered
            (progn (dolist (b remembered) (efrit-agent-display-in-side-window b))
                   (efrit-agent-display-in-side-window (car remembered) t))
          (efrit-agent-display-in-side-window (efrit-agent-instance-for-project root t) t)))))))

;;;; Wiring

;;;###autoload
(define-minor-mode efrit-agent-instances-mode
  "Several agent buffers, one per project and instance, in side windows.
`efrit' and `efrit-agent-open' open the current project's instance;
with a prefix argument they make a new one.  `efrit-agent-toggle'
hides and restores a project's agent windows per tab.
`efrit-agent-display' shows an instance buffer in its side window."
  :global t
  :group 'efrit-agent-instances)

;;;###autoload
(defun efrit-agent-new-instance (directory &optional name)
  "A new agent buffer working in DIRECTORY; no repository needed.
Interactively asks for the directory only (default: the current
project or `default-directory'); the buffer is named after it,
`*efrit[dir]*', and `efrit-agent-rename-instance' renames it.  NAME
is for callers.  The buffer is shown and selected (tzz, 2026-10-15)."
  (interactive
   (let ((default (or (efrit-agent-project-root) default-directory)))
     (list (read-directory-name "New agent in directory: " default default t))))
  (let* ((root (file-name-as-directory (expand-file-name directory)))
         (buf (efrit-agent-instance-create root name)))
    (efrit-agent-display buf t)
    buf))

(defun efrit-agent-open-instance (&optional new)
  "Open the current project's agent buffer; with NEW, a new instance of it."
  (interactive "P")
  (let ((buf (if new
                 (efrit-agent-instance-create)
               (efrit-agent-instance-for-project nil t))))
    (efrit-agent-display buf t)
    buf))

(defun efrit-agent-switch-instance (buffer)
  "Switch to agent BUFFER, chosen among all instances by name."
  (interactive
   (let ((names (mapcar #'buffer-name (efrit-agent-buffers))))
     (unless names (user-error "No agent buffers"))
     (list (get-buffer (completing-read "Agent buffer: " names nil t)))))
  (efrit-agent-display buffer t))

(provide 'efrit-agent-instances)

;;; efrit-agent-instances.el ends here
