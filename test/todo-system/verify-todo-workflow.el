;;; verify-todo-workflow.el --- Verify TODO workflow logic -*- lexical-binding: t -*-

(add-to-list 'load-path (expand-file-name "lisp" default-directory))
(require 'efrit)

(message "\n=== Verifying TODO Workflow Logic ===\n")

;; Test 1: Verify todo_analyze gives proper guidance
(message "Test 1: TODO Analysis")
(let ((result (efrit-do--handle-todo-analyze 
               (let ((ht (make-hash-table :test 'equal)))
                 (puthash "command" "fix all warnings in *Warnings* buffer" ht)
                 ht))))
  (message "Analysis result: %s" result))

;; Test 2: Verify session protocol includes TODO checking
(message "\n\nTest 2: Session Protocol Instructions")
(let ((protocol (efrit-do--session-protocol-instructions)))
  (if (string-match "IMMEDIATE SESSION CONTINUATION CHECK" protocol)
      (message "✓ Protocol includes immediate TODO checking")
    (message "✗ Protocol missing TODO checking!")))

;; Test 3: Simulate a session continuation with TODOs
(message "\n\nTest 3: Session Continuation Simulation")

;; Create some test TODOs
(efrit-do-clear-todos)
(efrit-do--add-todo "Fix warning in file1.el" 'high)
(efrit-do--add-todo "Fix warning in file2.el" 'high)
(efrit-do--add-todo "Verify all warnings fixed" 'low)

(message "Created %d test TODOs" (length efrit-do--current-todos))

;; Check what Claude would see
(message "\nWhat Claude sees on continuation:")
(message "1. todo_status: %s" (efrit-do--handle-todo-status))
(message "2. todo_complete_check: %s" (efrit-do--handle-todo-complete-check))
(message "3. todo_next: %s" (efrit-do--handle-todo-next))

;; Simulate completing all TODOs
(message "\n\nSimulating TODO completion:")
(dolist (todo efrit-do--current-todos)
  (let ((id (efrit-do-todo-item-id todo)))
    (efrit-do--update-todo-status id 'completed)
    (message "Completed: %s" (efrit-do-todo-item-content todo))))

(message "\nAfter completing all TODOs:")
(message "todo_complete_check: %s" (efrit-do--handle-todo-complete-check))

;; Test 4: Verify work log includes TODO state
(message "\n\nTest 4: Work Log with TODOs")
(let ((test-session (make-efrit-session
                     :id "test-123"
                     :command "fix warnings"
                     :status 'active
                     :start-time (current-time)
                     :work-log '())))
  (setq efrit-async--active-session test-session)
  
  ;; Update session with TODO state
  (efrit-async--update-session "test-123" "Test result" "Test code")
  
  (let ((work-log (efrit-session-work-log test-session)))
    (if (and work-log (= (length (car work-log)) 3))
        (message "✓ Work log includes TODO state")
      (message "✗ Work log missing TODO state!"))))

(message "\n\n=== Workflow Verification Complete ===")
(message "\nThe TODO system should prevent loops by:")
(message "1. Giving specific guidance instead of generic 'analyze' response")
(message "2. Forcing TODO status check at start of each continuation")
(message "3. Providing clear completion detection via todo_complete_check")
(message "4. Including TODO state in work log for context")

(message "\nTo test with real API:")
(message "1. Load test-efrit-interactive.el in Emacs")
(message "2. Run M-x test-efrit-todo-workflow")
(message "3. Watch if it creates TODOs and completes without looping!")