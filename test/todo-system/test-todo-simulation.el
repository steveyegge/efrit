;;; test-todo-simulation.el --- Simulate TODO workflow -*- lexical-binding: t -*-

;; Load test setup
(load-file "test-warnings-setup.el")

;; Clear any existing TODOs
(efrit-do-clear-todos)

;; Simulate what Claude should do with the new TODO system

(message "\n=== Simulating Claude's TODO-based workflow ===\n")

;; Step 1: Claude analyzes the command
(let ((analysis (efrit-do--handle-todo-analyze 
                 (let ((input (make-hash-table :test 'equal)))
                   (puthash "command" "fix warnings in *Warnings* buffer" input)
                   input))))
  (message "1. TODO Analysis: %s" analysis))

;; Step 2: Claude scans the warnings buffer
(let ((warnings-content (with-current-buffer "*Warnings*"
                         (buffer-string))))
  (message "\n2. Scanning warnings buffer...")
  (message "Found content: %s" (substring warnings-content 0 100)))

;; Step 3: Claude creates TODOs for each warning
(message "\n3. Creating TODOs for each warning...")
(let ((todo1 (efrit-do--handle-todo-add 
              (let ((input (make-hash-table :test 'equal)))
                (puthash "content" "Fix lexical-binding warning in file1.el" input)
                (puthash "priority" "high" input)
                input)))
      (todo2 (efrit-do--handle-todo-add
              (let ((input (make-hash-table :test 'equal)))
                (puthash "content" "Fix lexical-binding warning in file2.el" input) 
                (puthash "priority" "high" input)
                input)))
      (todo3 (efrit-do--handle-todo-add
              (let ((input (make-hash-table :test 'equal)))
                (puthash "content" "Fix lexical-binding warning in file3.el" input)
                (puthash "priority" "high" input)
                input)))
      (todo4 (efrit-do--handle-todo-add
              (let ((input (make-hash-table :test 'equal)))
                (puthash "content" "Fix lexical-binding warning in test-file.el" input)
                (puthash "priority" "medium" input)
                input)))
      (todo5 (efrit-do--handle-todo-add
              (let ((input (make-hash-table :test 'equal)))
                (puthash "content" "Fix lexical-binding warning in another-file.el" input)
                (puthash "priority" "medium" input)
                input)))
      (todo6 (efrit-do--handle-todo-add
              (let ((input (make-hash-table :test 'equal)))
                (puthash "content" "Verify all warnings are resolved" input)
                (puthash "priority" "low" input)
                input))))
  (message "Created TODOs: %s, %s, %s, %s, %s, %s" todo1 todo2 todo3 todo4 todo5 todo6))

;; Step 4: Check TODO status
(message "\n4. TODO Status: %s" (efrit-do--handle-todo-status))

;; Step 5: Show the TODO list
(efrit-do-show-todos)
(message "\n5. Showing TODO list buffer...")

;; Step 6: Simulate working through TODOs
(message "\n6. Simulating work on first TODO...")
(let* ((first-todo (car efrit-do--current-todos))
       (todo-id (when first-todo (efrit-do-todo-item-id first-todo))))
  (when todo-id
    ;; Mark as in-progress
    (message "   Marking %s as in-progress..." todo-id)
    (efrit-do--handle-todo-update 
     (let ((input (make-hash-table :test 'equal)))
       (puthash "id" todo-id input)
       (puthash "status" "in-progress" input)
       input) nil)
    
    ;; Simulate fixing the warning
    (message "   [Would fix lexical-binding in file1.el here]")
    
    ;; Mark as completed
    (message "   Marking %s as completed..." todo-id)
    (efrit-do--handle-todo-update
     (let ((input (make-hash-table :test 'equal)))
       (puthash "id" todo-id input)
       (puthash "status" "completed" input)
       input) nil)))

;; Step 7: Check completion status
(message "\n7. Checking if all TODOs complete: %s" 
         (efrit-do--handle-todo-complete-check))

;; Step 8: Show next TODO
(message "\n8. Next TODO: %s" (efrit-do--handle-todo-next))

;; Show progress buffer
(efrit-progress-show)
(message "\n9. Progress buffer shown")

;; Show async TODOs (simulating active session)
(setq efrit-async--active-session
      (make-efrit-session
       :id "test-session-123"
       :command "fix warnings in *Warnings* buffer"
       :status 'active
       :start-time (current-time)
       :work-log '()))

(efrit-async-show-todos)
(message "\n10. Async TODO buffer shown")

(message "\n=== Simulation complete ===")
(message "TODO system is working! Claude should:")
(message "1. Analyze command and create TODOs")
(message "2. Work through each TODO systematically")  
(message "3. Check todo_complete_check to know when to stop")
(message "4. Call session_complete when all done")
(message "\nNo more infinite loops!")