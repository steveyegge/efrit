;;; test-todo-execution-workflow.el --- Test the fixed TODO execution workflow  -*- lexical-binding: t; -*-

;;; Commentary:
;; Test that the new loop detection allows the proper TODO execution workflow:
;; todo_execute_next → eval_sexp/shell_exec → todo_update → repeat

;;; Code:

(require 'cl-lib)
(load-file "lisp/efrit-protocol.el")
(load-file "lisp/efrit-async.el")

(defun test-todo-execution-workflow ()
  "Test that proper TODO execution workflow is allowed."
  (let ((test-session (make-efrit-session :id "test-workflow-session")))
    (setq efrit-async--active-session test-session)
    
    ;; Simulate proper workflow: todo_execute_next → eval_sexp → todo_update → repeat
    (message "=== Testing Proper TODO Execution Workflow ===")
    
    ;; First todo_execute_next
    (let ((result1 (efrit-async--check-session-loops test-session "todo_execute_next")))
      (message "1st todo_execute_next: %s" (if result1 "BLOCKED" "ALLOWED"))
      (unless result1
        (push (cons "todo_execute_next" (current-time)) 
              (efrit-session-tool-history test-session))))
    
    ;; Claude executes the TODO with eval_sexp
    (let ((result2 (efrit-async--check-session-loops test-session "eval_sexp")))
      (message "eval_sexp after todo_execute_next: %s" (if result2 "BLOCKED" "ALLOWED"))
      (unless result2
        (push (cons "eval_sexp" (current-time)) 
              (efrit-session-tool-history test-session))))
    
    ;; Claude marks TODO as completed
    (let ((result3 (efrit-async--check-session-loops test-session "todo_update")))
      (message "todo_update after eval_sexp: %s" (if result3 "BLOCKED" "ALLOWED"))
      (unless result3
        (push (cons "todo_update" (current-time)) 
              (efrit-session-tool-history test-session))))
    
    ;; Second todo_execute_next (should be allowed - made progress)
    (let ((result4 (efrit-async--check-session-loops test-session "todo_execute_next")))
      (message "2nd todo_execute_next after progress: %s" (if result4 "BLOCKED" "ALLOWED"))
      (unless result4
        (push (cons "todo_execute_next" (current-time)) 
              (efrit-session-tool-history test-session))))
    
    ;; More execution
    (let ((result5 (efrit-async--check-session-loops test-session "shell_exec")))
      (message "shell_exec after 2nd todo_execute_next: %s" (if result5 "BLOCKED" "ALLOWED"))
      (unless result5
        (push (cons "shell_exec" (current-time)) 
              (efrit-session-tool-history test-session))))
    
    ;; Third todo_execute_next (should still be allowed - made progress)
    (let ((result6 (efrit-async--check-session-loops test-session "todo_execute_next")))
      (message "3rd todo_execute_next after more progress: %s" (if result6 "BLOCKED" "ALLOWED"))
      (unless result6
        (push (cons "todo_execute_next" (current-time)) 
              (efrit-session-tool-history test-session))))
    
    (message "History: %S" (mapcar #'car (efrit-session-tool-history test-session)))
    (message "=== Proper Workflow Test Complete ===")
    ))

(defun test-todo-execution-loop ()
  "Test that actual TODO execution loops are still blocked."
  (let ((test-session (make-efrit-session :id "test-loop-session")))
    (setq efrit-async--active-session test-session)
    
    (message "\n=== Testing TODO Execution Loop Detection ===")
    
    ;; First todo_execute_next
    (let ((result1 (efrit-async--check-session-loops test-session "todo_execute_next")))
      (message "1st todo_execute_next: %s" (if result1 "BLOCKED" "ALLOWED"))
      (unless result1
        (push (cons "todo_execute_next" (current-time)) 
              (efrit-session-tool-history test-session))))
    
    ;; Second todo_execute_next (no progress)
    (let ((result2 (efrit-async--check-session-loops test-session "todo_execute_next")))
      (message "2nd todo_execute_next (no progress): %s" (if result2 "BLOCKED" "ALLOWED"))
      (unless result2
        (push (cons "todo_execute_next" (current-time)) 
              (efrit-session-tool-history test-session))))
    
    ;; Third todo_execute_next (should be blocked - no progress made)
    (let ((result3 (efrit-async--check-session-loops test-session "todo_execute_next")))
      (message "3rd todo_execute_next (no progress): %s" (if result3 "BLOCKED" "ALLOWED"))
      (when result3
        (message "Loop detected correctly: %s" result3)))
    
    (message "History: %S" (mapcar #'car (efrit-session-tool-history test-session)))
    (message "=== Loop Detection Test Complete ===")
    ))

;; Run the tests
(test-todo-execution-workflow)
(test-todo-execution-loop)

(message "\n✓ TODO execution workflow tests completed!")

;;; test-todo-execution-workflow.el ends here
