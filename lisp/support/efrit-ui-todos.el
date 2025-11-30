;;; efrit-ui-todos.el --- TODO buffer and mode line for Efrit -*- lexical-binding: t -*-

;; Copyright (C) 2025 Steven Yegge

;; Author: Steven Yegge
;; Version: 0.4.0
;; Package-Requires: ((emacs "28.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Live TODO buffer display and mode line task indicator for Efrit.

;;; Code:

(require 'efrit-common)
(require 'efrit-ui-faces)
(require 'seq)

(declare-function efrit-do-todo-item-status "efrit-do")
(declare-function efrit-do-todo-item-content "efrit-do")

;;; ========================================================================
;;; Live TODO Buffer
;;; ========================================================================

(defconst efrit-todos-buffer-name "*efrit-todos*"
  "Name of the dedicated TODO display buffer.")

(defcustom efrit-todos-auto-show t
  "Whether to automatically show the TODO buffer when todos are updated."
  :type 'boolean
  :group 'efrit-todos)

(defcustom efrit-todos-window-height 10
  "Height of the TODO buffer window."
  :type 'integer
  :group 'efrit-todos)

;;; TODO Buffer Mode

(defvar efrit-todos-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'quit-window)
    (define-key map "g" 'efrit-todos-refresh)
    (define-key map "c" 'efrit-todos-clear-completed)
    map)
  "Keymap for efrit-todos-mode.")

(define-derived-mode efrit-todos-mode special-mode "Efrit-TODOs"
  "Major mode for viewing Efrit TODO list.
\\{efrit-todos-mode-map}"
  (setq-local truncate-lines t)
  (setq-local word-wrap nil)
  (hl-line-mode 1))

;;; TODO Buffer Functions

(defun efrit-todos--get-buffer ()
  "Get or create the TODO buffer."
  (let ((buffer (get-buffer-create efrit-todos-buffer-name)))
    (with-current-buffer buffer
      (unless (eq major-mode 'efrit-todos-mode)
        (efrit-todos-mode)))
    buffer))

(defun efrit-todos--render ()
  "Render the TODO list in the buffer."
  (require 'efrit-do)
  (let* ((buffer (efrit-todos--get-buffer))
         (todos (when (boundp 'efrit-do--current-todos)
                  (symbol-value 'efrit-do--current-todos))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)

        ;; Header
        (insert (propertize "Efrit Tasks" 'face 'efrit-todos-header) "\n")
        (insert (make-string 40 ?═) "\n\n")

        (if (null todos)
            (insert (propertize "No active tasks" 'face 'font-lock-comment-face) "\n")

          ;; Progress bar
          (let* ((total (length todos))
                 (completed (seq-count (lambda (todo)
                                        (eq (efrit-do-todo-item-status todo) 'completed))
                                      todos))
                 (progress-pct (if (> total 0) (/ (* 100.0 completed) total) 0))
                 (bar-width 20)
                 (filled (round (* bar-width (/ progress-pct 100.0))))
                 (empty (- bar-width filled)))
            (insert (format "Progress: %d/%d complete\n" completed total))
            (insert "[")
            (insert (propertize (make-string filled ?█) 'face 'efrit-todos-progress-bar))
            (insert (make-string empty ?░))
            (insert (format "] %.0f%%\n\n" progress-pct)))

          ;; Task list
          (dolist (todo todos)
            (let* ((status (efrit-do-todo-item-status todo))
                   (content (efrit-do-todo-item-content todo))
                   (icon (pcase status
                           ('todo "  ")       ; pending
                           ('pending "  ")    ; alias
                           ('in-progress "→ ")
                           ('completed "✓ ")))
                   (face (pcase status
                           ('todo 'efrit-todos-pending)
                           ('pending 'efrit-todos-pending)
                           ('in-progress 'efrit-todos-in-progress)
                           ('completed 'efrit-todos-completed)))
                   (status-str (pcase status
                                 ('todo "[PENDING]")
                                 ('pending "[PENDING]")
                                 ('in-progress "[IN PROGRESS]")
                                 ('completed "[COMPLETED]"))))
              (insert (propertize (concat icon status-str " " content "\n") 'face face)))))

        ;; Footer with key bindings
        (insert "\n" (make-string 40 ?─) "\n")
        (insert (propertize "q" 'face 'bold) " quit  ")
        (insert (propertize "g" 'face 'bold) " refresh  ")
        (insert (propertize "c" 'face 'bold) " clear completed\n")))))

(defun efrit-todos-refresh ()
  "Refresh the TODO buffer."
  (interactive)
  (efrit-todos--render)
  (when (called-interactively-p 'interactive)
    (message "TODO list refreshed")))

(defun efrit-todos-clear-completed ()
  "Clear completed TODOs from the list."
  (interactive)
  (require 'efrit-do)
  (when (bound-and-true-p efrit-do--current-todos)
    (setq efrit-do--current-todos
          (seq-remove (lambda (todo)
                       (eq (efrit-do-todo-item-status todo) 'completed))
                     efrit-do--current-todos))
    (efrit-todos--render)
    (message "Cleared completed TODOs")))

;;;###autoload
(defun efrit-todos-show ()
  "Show the TODO buffer."
  (interactive)
  (efrit-todos--render)
  (let ((buffer (efrit-todos--get-buffer)))
    (display-buffer buffer
                    `((display-buffer-reuse-window
                       display-buffer-at-bottom)
                      (window-height . ,efrit-todos-window-height)
                      (dedicated . t)))))

(defun efrit-todos-update ()
  "Update the TODO buffer if it exists and is visible.
Called automatically when todo_write is executed."
  (let ((buffer (get-buffer efrit-todos-buffer-name)))
    (when (and buffer (get-buffer-window buffer))
      (efrit-todos--render))))

;; Hook into todo_write to auto-update the buffer
(defun efrit-todos--after-todo-write-advice (&rest _args)
  "Advice function to update TODO buffer after todo_write."
  (efrit-todos-update)
  (when (and efrit-todos-auto-show
             (not (get-buffer-window efrit-todos-buffer-name)))
    ;; Auto-show buffer if enabled and not already visible
    (efrit-todos-show)))

;; This advice will be added when efrit-do is loaded
(with-eval-after-load 'efrit-do
  (advice-add 'efrit-do--handle-todo-write :after #'efrit-todos--after-todo-write-advice))

;;; ========================================================================
;;; Mode Line Task Indicator
;;; ========================================================================

(defcustom efrit-show-task-in-modeline t
  "Whether to show the current task in the mode line.
When non-nil and a task is in_progress, displays:
  [Efrit: Task description...]
When no tasks are in progress, displays:
  [Efrit: idle]"
  :type 'boolean
  :group 'efrit-todos)

(defcustom efrit-modeline-task-max-length 40
  "Maximum length of task description in mode line.
Longer descriptions will be truncated with ellipsis."
  :type 'integer
  :group 'efrit-todos)

(defvar efrit-modeline--current-task nil
  "Cache of current in-progress task for mode line display.")

(defun efrit-modeline--truncate (text max-length)
  "Truncate TEXT to MAX-LENGTH characters, adding ellipsis if needed.
Uses `efrit-common-truncate-string' with ellipsis counted in max length."
  (efrit-common-truncate-string text max-length t))

(defun efrit-modeline--get-current-task ()
  "Get the current in-progress task content, or nil if none."
  (when (bound-and-true-p efrit-do--current-todos)
    (let ((in-progress-task
           (seq-find (lambda (todo)
                       (eq (efrit-do-todo-item-status todo) 'in-progress))
                     efrit-do--current-todos)))
      (when in-progress-task
        (efrit-do-todo-item-content in-progress-task)))))

(defun efrit-modeline--update ()
  "Update the mode line task cache and force mode line redisplay."
  (setq efrit-modeline--current-task (efrit-modeline--get-current-task))
  (force-mode-line-update t))

(defun efrit-modeline-construct ()
  "Construct the mode line string for the current task.
Returns nil if `efrit-show-task-in-modeline' is nil."
  (when efrit-show-task-in-modeline
    (let ((task efrit-modeline--current-task))
      (if task
          (propertize
           (format "[Efrit: %s]"
                   (efrit-modeline--truncate task efrit-modeline-task-max-length))
           'face 'efrit-modeline-task
           'help-echo (format "Current task: %s" task))
        (propertize "[Efrit: idle]"
                    'face 'efrit-modeline-idle
                    'help-echo "No task in progress")))))

;;;###autoload
(defun efrit-modeline-mode-line-format ()
  "Return the mode line format element for efrit task display.
Add this to `mode-line-format' or `global-mode-string' to display."
  '(:eval (efrit-modeline-construct)))

;; Hook to update mode line when todos change
(defun efrit-modeline--after-todo-write-advice (&rest _args)
  "Update mode line after todo_write is called."
  (efrit-modeline--update))

;; Add advice when efrit-do is loaded
(with-eval-after-load 'efrit-do
  (advice-add 'efrit-do--handle-todo-write :after #'efrit-modeline--after-todo-write-advice))

;;;###autoload
(defun efrit-modeline-enable ()
  "Enable the Efrit task indicator in the mode line.
Adds the indicator to `global-mode-string'."
  (interactive)
  (unless (member '(:eval (efrit-modeline-construct)) global-mode-string)
    (setq global-mode-string
          (append global-mode-string
                  '((:eval (efrit-modeline-construct))))))
  (efrit-modeline--update)
  (message "Efrit mode line task indicator enabled"))

;;;###autoload
(defun efrit-modeline-disable ()
  "Disable the Efrit task indicator in the mode line."
  (interactive)
  (setq global-mode-string
        (delete '(:eval (efrit-modeline-construct)) global-mode-string))
  (force-mode-line-update t)
  (message "Efrit mode line task indicator disabled"))

(provide 'efrit-ui-todos)

;;; efrit-ui-todos.el ends here
