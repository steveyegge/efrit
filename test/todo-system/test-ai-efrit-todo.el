;;; test-ai-efrit-todo.el --- Test TODO system with ai-efrit channel -*- lexical-binding: t -*-

;; Load Efrit
(add-to-list 'load-path (expand-file-name "lisp" default-directory))
(require 'efrit)

;; Configure for ai-efrit channel
(setq efrit-api-channel "ai-efrit")
(setq efrit-log-level 'debug)

;; Clear any existing TODOs
(efrit-do-clear-todos)

;; Create test warnings buffer
(with-current-buffer (get-buffer-create "*Warnings*")
  (erase-buffer)
  (insert "Warning: test1.el:1: Warning: file `test1.el' lacks lexical-binding directive
Warning: test2.el:1: Warning: file `test2.el' lacks lexical-binding directive
Warning: test3.el:1: Warning: file `test3.el' lacks lexical-binding directive")
  (goto-char (point-min)))

(message "Created *Warnings* buffer with 3 warnings")

;; Function to monitor the session
(defun test-monitor-session ()
  "Monitor the TODO progress."
  (when efrit-async--active-session
    (let* ((todos efrit-do--current-todos)
           (total (length todos))
           (completed (seq-count (lambda (todo)
                                  (eq (efrit-do-todo-item-status todo) 'completed))
                                todos)))
      (message "Session %s: %d/%d TODOs completed" 
               (efrit-session-id efrit-async--active-session)
               completed total)
      
      ;; Show current TODOs
      (when todos
        (message "Current TODOs:")
        (dolist (todo todos)
          (message "  %s [%s] %s" 
                   (pcase (efrit-do-todo-item-status todo)
                     ('todo "☐")
                     ('in-progress "⟳")
                     ('completed "☑"))
                   (efrit-do-todo-item-id todo)
                   (efrit-do-todo-item-content todo))))
      
      ;; Check if still active
      (if efrit-async--active-session
          (run-at-time 2 nil #'test-monitor-session)
        (message "Session complete!")))))

;; Execute the async command
(message "\n=== Starting ai-efrit test ===")
(message "Executing: fix all warnings in *Warnings* buffer")

;; Show relevant buffers
(delete-other-windows)
(switch-to-buffer "*Warnings*")
(split-window-vertically)
(other-window 1)
(switch-to-buffer "*scratch*")

;; Start monitoring
(run-at-time 2 nil #'test-monitor-session)

;; Execute the command
(efrit-do-async "fix all warnings in *Warnings* buffer")

(message "\nMonitoring session...")
(message "Watch for:")
(message "- *Efrit Progress* buffer for real-time updates")
(message "- M-x efrit-async-show-todos to see TODO list")
(message "- This should NOT loop infinitely!")
(message "\nIf it loops, use M-x efrit-async-cancel")