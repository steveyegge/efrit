;;; test-efrit-interactive.el --- Interactive test for TODO system -*- lexical-binding: t -*-

;; Instructions:
;; 1. Load this file: M-x load-file RET test-efrit-interactive.el RET
;; 2. It will create a *Warnings* buffer with test warnings
;; 3. Run: M-x test-efrit-todo-workflow RET
;; 4. Watch the progress!

(require 'efrit)

(defun test-setup-warnings ()
  "Create a test *Warnings* buffer."
  (with-current-buffer (get-buffer-create "*Warnings*")
    (erase-buffer)
    (insert "Warning (bytecomp): test-file-1.el:1:1: Warning: file `test-file-1.el' lacks lexical-binding directive
Warning (bytecomp): test-file-2.el:1:1: Warning: file `test-file-2.el' lacks lexical-binding directive  
Warning (bytecomp): test-file-3.el:1:1: Warning: file `test-file-3.el' lacks lexical-binding directive
Warning (bytecomp): helper.el:1:1: Warning: file `helper.el' lacks lexical-binding directive
Warning (bytecomp): utils.el:1:1: Warning: file `utils.el' lacks lexical-binding directive")
    (goto-char (point-min))
    (current-buffer)))

(defun test-efrit-todo-workflow ()
  "Test the TODO workflow with efrit-do-async."
  (interactive)
  
  ;; Clear previous state
  (efrit-do-clear-todos)
  (when (get-buffer "*Efrit Progress*")
    (kill-buffer "*Efrit Progress*"))
  
  ;; Setup warnings
  (let ((warnings-buffer (test-setup-warnings)))
    
    ;; Setup window layout
    (delete-other-windows)
    (switch-to-buffer warnings-buffer)
    (split-window-horizontally)
    (other-window 1)
    
    ;; Show progress buffer location
    (switch-to-buffer "*scratch*")
    (erase-buffer)
    (insert "=== Efrit TODO Test ===\n\n")
    (insert "1. Warnings buffer created with 5 warnings\n")
    (insert "2. Running: efrit-do-async \"fix all warnings in *Warnings* buffer\"\n")
    (insert "3. Watch for TODO creation and progress!\n\n")
    (insert "Commands to monitor:\n")
    (insert "- M-x efrit-progress-show       ; View progress\n")
    (insert "- M-x efrit-async-show-todos    ; View TODO list\n") 
    (insert "- M-x efrit-async-status        ; Check session status\n")
    (insert "- M-x efrit-async-cancel        ; Cancel if looping\n\n")
    
    ;; Start the async command
    (message "Starting efrit-do-async...")
    (efrit-do-async "fix all warnings in *Warnings* buffer")
    
    ;; Auto-show progress after a moment
    (run-at-time 1 nil (lambda ()
                        (when (get-buffer "*Efrit Progress*")
                          (efrit-progress-show))))
    
    ;; Monitor for TODOs
    (run-at-time 3 nil (lambda ()
                        (when efrit-do--current-todos
                          (message "TODOs created! Count: %d" 
                                   (length efrit-do--current-todos))
                          (efrit-async-show-todos))))))

(defun test-check-todo-state ()
  "Check current TODO state."
  (interactive)
  (if efrit-do--current-todos
      (let ((total (length efrit-do--current-todos))
            (completed (seq-count (lambda (todo)
                                   (eq (efrit-do-todo-item-status todo) 'completed))
                                 efrit-do--current-todos)))
        (message "TODOs: %d/%d completed. All done? %s" 
                 completed total 
                 (efrit-do--handle-todo-complete-check)))
    (message "No TODOs found")))

;; Key bindings for testing
(global-set-key (kbd "C-c t t") 'test-efrit-todo-workflow)
(global-set-key (kbd "C-c t c") 'test-check-todo-state)
(global-set-key (kbd "C-c t s") 'efrit-async-show-todos)
(global-set-key (kbd "C-c t p") 'efrit-progress-show)

(message "Test loaded! Commands available:")
(message "  C-c t t - Run test workflow")
(message "  C-c t c - Check TODO state") 
(message "  C-c t s - Show TODOs")
(message "  C-c t p - Show progress")
(message "Or: M-x test-efrit-todo-workflow")