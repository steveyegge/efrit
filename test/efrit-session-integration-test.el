;;; efrit-session-integration-test.el --- Session flow integration test -*- lexical-binding: t -*-

;; Test the complete session flow with real API calls

;;; Code:

(require 'efrit)
(require 'efrit-do)
(require 'efrit-async) 
(require 'efrit-unified)

(defvar efrit-test-results nil
  "Collect test results.")

(defun efrit-test-session-flow ()
  "Test complete session flow."
  (setq efrit-test-results nil)
  
  (message "\n=== Efrit Session Flow Integration Test ===")
  (message "This test will make real API calls and consume tokens.\n")
  
  ;; Test 1: Simple sync execution
  (message "Test 1: Sync command execution...")
  (condition-case err
      (let ((start-time (float-time)))
        (efrit-do "What is the capital of France?")
        (let ((elapsed (- (float-time) start-time)))
          (push (format "✓ Sync test passed (%.1fs)" elapsed) efrit-test-results)
          (message "✓ Sync test passed (%.1fs)" elapsed)))
    (error 
     (push (format "✗ Sync test failed: %s" err) efrit-test-results)
     (message "✗ Sync test failed: %s" err)))
  
  ;; Test 2: Multi-step session
  (message "\nTest 2: Multi-step session execution...")
  (let ((completed nil)
        (result nil)
        (start-time (float-time)))
    
    ;; Use efrit-async-execute-command directly
    (efrit-async-execute-command 
     "Create a buffer called *efrit-test*, write 'Session test successful!' in it, then tell me what you wrote"
     (lambda (res)
       (setq result res)
       (setq completed t)))
    
    ;; Wait for completion
    (while (and (not completed)
                (< (- (float-time) start-time) 30))
      (sleep-for 0.5))
    
    (if completed
        (let ((elapsed (- (float-time) start-time)))
          (push (format "✓ Multi-step test passed (%.1fs)" elapsed) efrit-test-results)
          (message "✓ Multi-step test passed (%.1fs)" elapsed)
          ;; Verify buffer was created
          (if (get-buffer "*efrit-test*")
              (message "  - Buffer created successfully")
            (message "  - Warning: Buffer not found")))
      (push "✗ Multi-step test timed out" efrit-test-results)
      (message "✗ Multi-step test timed out")))
  
  ;; Test 3: Queue functionality
  (message "\nTest 3: Queue processing...")
  (let ((queue-test-start (float-time))
        (commands-queued 0))
    
    ;; Clear any existing queue
    (setq efrit-async--session-queue nil)
    
    ;; Queue multiple commands
    (efrit-async-execute-command "What is 1 + 1?" 
                                (lambda (_) (message "  - Command 1 complete")))
    (setq commands-queued 1)
    
    (efrit-async-execute-command "What is 2 + 2?"
                                (lambda (_) (message "  - Command 2 complete")))
    (setq commands-queued 2)
    
    (let ((queue-size (length efrit-async--session-queue)))
      (if (> queue-size 0)
          (progn
            (push (format "✓ Queue test passed (%d queued)" queue-size) efrit-test-results)
            (message "✓ Queue test passed (%d commands queued)" queue-size))
        (push "✗ Queue test failed - no commands queued" efrit-test-results)
        (message "✗ Queue test failed - no commands queued"))))
  
  ;; Test 4: Unified interface
  (message "\nTest 4: Unified interface...")
  (condition-case err
      (progn
        (efrit-unified-do "What day is it today?")
        (push "✓ Unified interface test passed" efrit-test-results)
        (message "✓ Unified interface test passed"))
    (error
     (push (format "✗ Unified interface test failed: %s" err) efrit-test-results)
     (message "✗ Unified interface test failed: %s" err)))
  
  ;; Summary
  (message "\n=== Test Summary ===")
  (dolist (result (reverse efrit-test-results))
    (message result))
  
  (let ((passed (cl-count-if (lambda (r) (string-prefix-p "✓" r)) efrit-test-results))
        (total (length efrit-test-results)))
    (message "\nPassed: %d/%d tests" passed total)
    (when (= passed total)
      (message "\n🎉 All tests passed!"))))

;; Run the test
(efrit-test-session-flow)

(provide 'efrit-session-integration-test)
;;; efrit-session-integration-test.el ends here