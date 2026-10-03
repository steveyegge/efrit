;;; efrit-agent-instances.el --- Several agent buffers: naming, layout, toggling -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.8.5
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
