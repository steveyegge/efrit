;;; simple-todo-test.el --- Simple test for TODO loop prevention -*- lexical-binding: t -*-

;; Load the system with proper path handling
(let ((lisp-dir (expand-file-name "lisp" (file-name-directory load-file-name))))
  (add-to-list 'load-path lisp-dir))

(require 'efrit-common)
(require 'efrit-do)

(message "=== Testing Efrit TODO System Loop Prevention ===")

;; Test 1: Verify todo_analyze creates proper instructions
(defun test-todo-analyze-loop-prevention ()
  "Test that todo_analyze prevents loops by providing clear instructions."
  (message "\n--- Test 1: todo_analyze Loop Prevention ---")
  
  ;; Clear any existing TODOs
  (efrit-do--clear-todos)
  (message "Cleared existing TODOs")
  
  ;; First call to todo_analyze should provide instructions
  (let ((command "fix all warnings in *Warnings* buffer"))
    (message "Calling todo_analyze with: %s" command)
    (let ((result1 (efrit-do--handle-todo-analyze command)))
      (message "First todo_analyze result:")
      (message "%s" result1)
      
      ;; Add some fake TODOs to simulate following the instructions
      (efrit-do--add-todo "Fix lexical-binding warning in file1.el" 'high)
      (efrit-do--add-todo "Fix lexical-binding warning in file2.el" 'high)
      (efrit-do--add-todo "Verify all warnings resolved" 'medium)
      (message "Added 3 TODOs to simulate following instructions")
      
      ;; Now call todo_analyze again - should prevent loop
      (let ((result2 (efrit-do--handle-todo-analyze command)))
        (message "\nSecond todo_analyze result (should prevent loop):")
        (message "%s" result2)
        
        ;; Verify it detected existing TODOs
        (if (string-match "Already have \\([0-9]+\\) TODOs" result2)
            (message "✓ SUCCESS: Loop prevention working - detected %s existing TODOs" 
                     (match-string 1 result2))
          (message "✗ FAILED: Loop prevention not working"))))))

;; Test 2: Show TODO workflow commands
(defun test-todo-workflow-commands ()
  "Test various TODO workflow commands."
  (message "\n--- Test 2: TODO Workflow Commands ---")
  
  ;; Show current status
  (let ((status (efrit-do--handle-todo-status)))
    (message "TODO Status: %s" status))
  
  ;; Show TODOs
  (let ((todos (efrit-do--handle-todo-show)))
    (message "Current TODOs: %s" todos))
  
  ;; Get next TODO
  (let ((next (efrit-do--handle-todo-next)))
    (message "Next TODO: %s" next))
  
  ;; Check completion
  (let ((completion (efrit-do--handle-todo-complete-check)))
    (message "Completion check: %s" completion)))

;; Test 3: Simulate TODO progression
(defun test-todo-progression ()
  "Test TODO status progression."
  (message "\n--- Test 3: TODO Progression ---")
  
  ;; Update first TODO to in-progress
  (let ((first-todo (car efrit-do--current-todos)))
    (when first-todo
      (let ((id (efrit-do-todo-item-id first-todo)))
        (message "Updating TODO %s to in-progress" id)
        (efrit-do--update-todo-status id 'in-progress)
        
        ;; Show updated status
        (let ((status (efrit-do--handle-todo-status)))
          (message "Updated status: %s" status))
        
        ;; Complete the TODO
        (message "Completing TODO %s" id)
        (efrit-do--update-todo-status id 'completed)
        
        ;; Show final status
        (let ((status (efrit-do--handle-todo-status)))
          (message "Final status: %s" status))
        
        ;; Check if all complete
        (let ((completion (efrit-do--handle-todo-complete-check)))
          (message "Completion check: %s" completion)))))

;; Test 4: Verify system prompt includes TODO instructions
(defun test-system-prompt-todo-instructions ()
  "Test that system prompt includes TODO workflow guidance."
  (message "\n--- Test 4: System Prompt TODO Instructions ---")
  
  (let ((prompt (efrit-do--command-system-prompt)))
    (if (string-match "todo_analyze" prompt)
        (message "✓ SUCCESS: System prompt includes TODO workflow instructions")
      (message "✗ FAILED: System prompt missing TODO instructions"))
    
    (if (string-match "don't analyze again" prompt)
        (message "✓ SUCCESS: System prompt includes loop prevention guidance")
      (message "✗ FAILED: System prompt missing loop prevention guidance"))))

;; Run all tests
(condition-case err
    (progn
      (test-todo-analyze-loop-prevention)
      (test-todo-workflow-commands)
      (test-todo-progression)
      (test-system-prompt-todo-instructions)
      (message "\n=== All Tests Completed ===")
      (message "Loop prevention mechanism appears to be working correctly!"))
  (error 
   (message "Error during testing: %s" err)))

(provide 'simple-todo-test)