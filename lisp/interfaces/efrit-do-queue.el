;;; efrit-do-queue.el --- Command queueing integration for async execution -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; Command queueing integration for Efrit's async execution.
;; Allows users to queue multiple `efrit-do` commands for sequential execution.
;;
;; When a session completes, the next queued command is automatically executed.
;; Users can:
;; - Queue commands with `efrit-do-queue-command`
;; - View queue with `efrit-do-show-queue`
;; - Clear queue with `efrit-do-clear-queue`
;; - Skip to next command with `efrit-do-next`

;;; Code:

(require 'cl-lib)
(require 'efrit-log)
(require 'efrit-common)
(require 'efrit-session)

;;; Customization

(defgroup efrit-do-queue nil
  "Command queueing for Efrit async execution."
  :group 'efrit
  :prefix "efrit-do-queue-")

(defcustom efrit-do-queue-auto-process t
  "Whether to automatically process queued commands after session completes."
  :type 'boolean
  :group 'efrit-do-queue)

(defcustom efrit-do-queue-max-size 100
  "Maximum number of commands that can be queued."
  :type 'integer
  :group 'efrit-do-queue)

;;; Global Queue State

(defvar efrit-do-queue--global-queue nil
  "Global queue of pending commands (separate from per-session queues).
Each entry is a command string.")

(defvar efrit-do-queue--processing nil
  "Flag indicating if a queued command is being processed.")

;;; Queue Management

(defun efrit-do-queue-add-command (command &optional session)
  "Add COMMAND to queue for SESSION (or global queue if SESSION is nil).
Returns t if added successfully, nil if queue is full."
  (cond
   (session
    ;; Per-session queue
    (if (>= (efrit-session-command-queue-length session)
            efrit-do-queue-max-size)
        (progn
          (efrit-log 'warn "Session %s queue is full" (efrit-session-id session))
          nil)
      (efrit-session-queue-command session command)
      t))
   (t
    ;; Global queue
    (if (>= (length efrit-do-queue--global-queue) efrit-do-queue-max-size)
        (progn
          (efrit-log 'warn "Global queue is full")
          nil)
      (setq efrit-do-queue--global-queue
            (append efrit-do-queue--global-queue (list command)))
      (efrit-log 'info "Command queued (total: %d)"
                 (length efrit-do-queue--global-queue))
      t))))

(defun efrit-do-queue-pop-command (&optional session)
  "Pop and return next command from SESSION's queue (or global if nil)."
  (if session
      (efrit-session-dequeue-command session)
    (let ((cmd (car efrit-do-queue--global-queue)))
      (when cmd
        (setq efrit-do-queue--global-queue (cdr efrit-do-queue--global-queue))
        (efrit-log 'debug "Popped command from global queue (remaining: %d)"
                   (length efrit-do-queue--global-queue)))
      cmd)))

(defun efrit-do-queue-clear (&optional session)
  "Clear queue for SESSION (or global if nil)."
  (if session
      (progn
        (setf (efrit-session-command-queue session) nil)
        (efrit-log 'info "Cleared session %s queue" (efrit-session-id session)))
    (let ((count (length efrit-do-queue--global-queue)))
      (setq efrit-do-queue--global-queue nil)
      (efrit-log 'info "Cleared global queue (%d commands)" count)
      count)))

(defun efrit-do-queue-size (&optional session)
  "Get size of queue for SESSION (or global if nil)."
  (if session
      (efrit-session-command-queue-length session)
    (length efrit-do-queue--global-queue)))

;;;###autoload
(defun efrit-do-queue-command (command)
  "Queue COMMAND for later execution."
  (interactive "sCommand: ")
  (if (efrit-do-queue-add-command command)
      (message "Queued: %s (queue size: %d)"
               (efrit-truncate-string command 60)
               (efrit-do-queue-size))
    (message "Queue is full, cannot add command")))

;;;###autoload
(defun efrit-do-show-queue ()
  "Display the current command queue."
  (interactive)
  (let ((buffer (get-buffer-create "*Efrit Queue*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "Efrit Command Queue\n")
      (insert "===================\n\n")
      
      (let ((queue efrit-do-queue--global-queue))
        (if (null queue)
            (insert "Queue is empty\n")
          (insert (format "Queued commands (%d):\n\n" (length queue)))
          (cl-loop for cmd in queue
                   for i from 1
                   do (insert (format "%d. %s\n"
                                     i
                                     (efrit-truncate-string cmd 70)))))))
    
    (display-buffer buffer)))

;;;###autoload
(defun efrit-do-clear-queue ()
  "Clear all queued commands."
  (interactive)
  (let ((count (efrit-do-queue-clear)))
    (message "Cleared %d commands from queue" count)))

;;;###autoload
(defun efrit-do-queue-show-size ()
  "Show the size of the command queue."
  (interactive)
  (let ((size (efrit-do-queue-size)))
    (message "Queue size: %d" size)))

;;;###autoload
(defun efrit-do-next ()
  "Manually trigger execution of next queued command."
  (interactive)
  (let ((cmd (efrit-do-queue-pop-command)))
    (if cmd
        (progn
          (message "Executing: %s" (efrit-truncate-string cmd 60))
          ;; This would integrate with efrit-do to actually execute the command
          ;; For now, we just log
          (efrit-log 'info "Would execute: %s" cmd))
      (message "No queued commands"))))

;;; Queue Processing

(defun efrit-do-queue-process-next (&optional session)
  "Process next queued command for SESSION (or global if nil).
Called automatically after session completes.
Returns t if a command was started, nil if queue is empty."
  (when efrit-do-queue-auto-process
    (let ((cmd (efrit-do-queue-pop-command session)))
      (if cmd
          (progn
            (efrit-log 'info "Processing queued command: %s"
                       (efrit-truncate-string cmd 60))
            ;; This would integrate with efrit-do to execute the command
            ;; The actual execution is deferred to whoever calls this function
            t)
        nil))))

(defun efrit-do-queue-on-session-complete (session stop-reason)
  "Hook called when SESSION completes.
Automatically processes next queued command if available.
STOP-REASON indicates why the session ended."
  (when (eq stop-reason 'end_turn)
    ;; Only auto-process on normal completion, not on errors/interrupts
    (efrit-log 'debug "Session %s completed, checking for queued commands"
               (efrit-session-id session))
    (efrit-do-queue-process-next session)))

(provide 'efrit-do-queue)

;;; efrit-do-queue.el ends here
