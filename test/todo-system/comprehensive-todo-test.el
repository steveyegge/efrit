;;; comprehensive-todo-test.el --- Comprehensive TODO system test -*- lexical-binding: t -*-

;; Load system
(add-to-list 'load-path "lisp")
(require 'efrit-do)

(defun create-warnings-buffer ()
  "Create a test *Warnings* buffer."
  (with-current-buffer (get-buffer-create "*Warnings*")
    (erase-buffer)
    (insert "file1.el:1: Warning: Package lacks a file-local 'lexical-binding' directive\n")
    (insert "file2.el:1: Warning: Package lacks a file-local 'lexical-binding' directive\n") 
    (insert "file3.el:1: Warning: Package lacks a file-local 'lexical-binding' directive\n")
    (current-buffer)))

(defun simulate-todo-workflow ()
  "Simulate the complete TODO workflow."
  (message "=== Comprehensive TODO System Test ===")
  
  ;; Step 1: Create warnings buffer
  (message "\n1. Creating *Warnings* buffer...")
  (create-warnings-buffer)
  
  ;; Step 2: Clear existing TODOs
  (message "\n2. Clearing existing TODOs...")
  (efrit-do--clear-todos)
  
  ;; Step 3: First todo_analyze call (should provide instructions)
  (message "\n3. First todo_analyze call...")
  (let ((result1 (efrit-do--handle-todo-analyze "fix all warnings in *Warnings* buffer")))
    (message "First call result:")
    (message "%s" result1))
  
  ;; Step 4: Simulate following instructions - add TODOs
  (message "\n4. Following instructions - adding TODOs...")
  (efrit-do--add-todo "Fix lexical-binding warning in file1.el" 'high)
  (efrit-do--add-todo "Fix lexical-binding warning in file2.el" 'high)
  (efrit-do--add-todo "Fix lexical-binding warning in file3.el" 'high)
  (efrit-do--add-todo "Verify all warnings resolved" 'medium)
  
  ;; Step 5: Show current TODO status
  (message "\n5. Current TODO status:")
  (message "%s" (efrit-do--handle-todo-status))
  
  ;; Step 6: Try todo_analyze again (should prevent loop)
  (message "\n6. Second todo_analyze call (testing loop prevention)...")
  (let ((result2 (efrit-do--handle-todo-analyze "fix all warnings in *Warnings* buffer")))
    (message "Second call result:")
    (message "%s" result2)
    (if (string-match "Already have" result2)
        (message "✓ LOOP PREVENTION WORKING!")
      (message "✗ Loop prevention failed")))
  
  ;; Step 7: Work through TODOs
  (message "\n7. Working through TODOs...")
  (let ((first-todo (car efrit-do--current-todos)))
    (when first-todo
      (let ((id (efrit-do-todo-item-id first-todo)))
        (message "Starting TODO: %s" (efrit-do-todo-item-content first-todo))
        (efrit-do--update-todo-status id 'in-progress)
        (message "Status: %s" (efrit-do--handle-todo-status))
        
        ;; Complete it
        (efrit-do--update-todo-status id 'completed)
        (message "Completed first TODO")
        (message "Status: %s" (efrit-do--handle-todo-status)))))
  
  ;; Step 8: Show next TODO
  (message "\n8. Next TODO:")
  (message "%s" (efrit-do--handle-todo-next))
  
  ;; Step 9: Check completion
  (message "\n9. Completion check:")
  (message "%s" (efrit-do--handle-todo-complete-check))
  
  ;; Step 10: Complete all TODOs
  (message "\n10. Completing all remaining TODOs...")
  (dolist (todo efrit-do--current-todos)
    (let ((id (efrit-do-todo-item-id todo))
          (status (efrit-do-todo-item-status todo)))
      (unless (eq status 'completed)
        (efrit-do--update-todo-status id 'completed)
        (message "Completed: %s" (efrit-do-todo-item-content todo)))))
  
  ;; Final status
  (message "\n11. Final completion check:")
  (message "%s" (efrit-do--handle-todo-complete-check))
  
  (message "\n=== Test Complete ==="))

;; Run the test
(simulate-todo-workflow)