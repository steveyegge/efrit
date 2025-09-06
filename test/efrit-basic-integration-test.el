;;; efrit-basic-integration-test.el --- Basic integration test -*- lexical-binding: t -*-

;; A simple integration test to verify the API connection works

;;; Code:

(require 'efrit)
(require 'efrit-do)

(defun efrit-basic-integration-test ()
  "Run a basic integration test."
  (message "\n=== Efrit Basic Integration Test ===\n")
  
  ;; Test 1: Simple sync command
  (message "Test 1: Simple sync command...")
  (condition-case err
      (progn
        (efrit-do "What is 2 + 2?")
        (message "✓ Sync command succeeded"))
    (error 
     (message "✗ Sync command failed: %s" (error-message-string err))))
  
  ;; Test 2: Check context was captured
  (message "\nTest 2: Context capture...")
  (let ((recent (efrit-context-ring-get-recent 1)))
    (if recent
        (let ((item (car recent)))
          (message "✓ Context captured: %s" 
                   (efrit-context-item-to-string item)))
      (message "✗ No context captured")))
  
  ;; Test 3: Async via unified interface
  (message "\nTest 3: Async command via unified interface...")
  (let ((start-time (float-time)))
    (condition-case err
        (progn
          (efrit-do-async "What is 5 + 5?")
          ;; Just check it started
          (message "✓ Async command started"))
      (error 
       (message "✗ Async command failed: %s" (error-message-string err)))))
  
  (message "\n=== Test Complete ===\n"))

;; Run the test
(efrit-basic-integration-test)

(provide 'efrit-basic-integration-test)
;;; efrit-basic-integration-test.el ends here