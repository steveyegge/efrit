;;; test-todo-system.el --- Test TODO list integration -*- lexical-binding: t -*-

;; Test file to verify TODO list prevents loops

(defun create-test-warnings-buffer ()
  "Create a buffer with multiple warning messages."
  (with-current-buffer (get-buffer-create "*Test Warnings*")
    (erase-buffer)
    (insert ";;; file1.el --- Test file 1\n\n")
    (insert ";; This file is missing lexical-binding\n\n")
    (insert "(defun test1 () \"Test function 1\")\n\n")
    (insert ";;; file2.el --- Test file 2\n\n") 
    (insert ";; Also missing lexical-binding\n\n")
    (insert "(defun test2 () \"Test function 2\")\n\n")
    (insert ";;; file3.el --- Test file 3\n\n")
    (insert ";; No lexical binding here either\n\n")
    (insert "(defun test3 () \"Test function 3\")\n\n")
    (current-buffer)))

(defun test-todo-workflow ()
  "Test the TODO-based workflow."
  (interactive)
  ;; Clear any existing TODOs
  (efrit-do-clear-todos)
  
  ;; Create test warnings
  (let ((buffer (create-test-warnings-buffer)))
    (switch-to-buffer buffer)
    (message "Created test buffer with warnings. Use efrit-do-async to fix them!")))

(defun verify-todo-completion ()
  "Verify TODO completion prevents loops."
  (interactive)
  (if efrit-do--current-todos
      (let ((stats (efrit-do--handle-todo-status)))
        (message "TODO Status: %s" stats)
        (when (string-match "All TODOs completed" (efrit-do--handle-todo-complete-check))
          (message "SUCCESS: All TODOs completed - loop prevention working!")))
    (message "No TODOs found")))

;; Test commands to verify the system
(global-set-key (kbd "C-c t w") 'test-todo-workflow)
(global-set-key (kbd "C-c t v") 'verify-todo-completion)
(global-set-key (kbd "C-c t s") 'efrit-do-show-todos)
(global-set-key (kbd "C-c t p") 'efrit-progress-show)

(message "Test TODO system loaded. Keys:
C-c t w - Create test warnings buffer
C-c t v - Verify TODO completion
C-c t s - Show current TODOs
C-c t p - Show progress buffer

Try: M-x test-todo-workflow RET, then run efrit-do-async with 'fix all lexical-binding warnings'")