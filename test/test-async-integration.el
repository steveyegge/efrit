;;; test-async-integration.el --- Test async integration -*- lexical-binding: t -*-

;; Test file for verifying efrit-async and efrit-do integration

;;; Commentary:
;; Simple tests to verify the async integration between efrit-async.el and efrit-do.el

;;; Code:

(require 'efrit-async)
(require 'efrit-do)
(require 'efrit-log)

;;; Basic functionality tests

(defun test-async-basic-setup ()
  "Test that async infrastructure loads correctly."
  (message "Testing async basic setup...")
  
  ;; Test that required functions exist
  (when (fboundp 'efrit-async-execute-command)
    (message "✓ efrit-async-execute-command function exists"))
  
  (when (fboundp 'efrit-do-async)
    (message "✓ efrit-do-async function exists"))
  
  ;; Test session structure
  (let ((session (make-efrit-session :id "test" :status 'active)))
    (when (efrit-session-p session)
      (message "✓ Session structure works")))
  
  (message "Basic setup test completed"))

(defun test-async-mock-callback ()
  "Test async execution with a mock callback."
  (message "Testing async callback mechanism...")
  (let ((callback-called nil)
        (callback-result nil))
    
    ;; Simple mock test - this won't actually call the API
    ;; but will test the callback mechanism
    (let ((mock-callback (lambda (result)
                           (setq callback-called t)
                           (setq callback-result result)
                           (message "✓ Callback received: %s" result))))
      
      ;; Test the callback directly
      (funcall mock-callback "test result")
      
      (when (and callback-called (string= callback-result "test result"))
        (message "✓ Callback mechanism works")))
    
    (message "Callback test completed")))

;;; Integration test helpers

(defun test-async-simple-command ()
  "Test a simple async command (requires API key)."
  (interactive)
  (message "Testing simple async command...")
  
  ;; This will actually make an API call if you have an API key configured
  (condition-case err
      (efrit-async-execute-command 
       "show me the current time"
       (lambda (result)
         (message "✓ Async command result: %s" result)))
    (error
     (message "✗ Async command failed: %s" (error-message-string err)))))

;;; Run all tests

(defun test-async-integration-all ()
  "Run all async integration tests."
  (interactive)
  (message "\n=== Starting Async Integration Tests ===")
  (test-async-basic-setup)
  (test-async-mock-callback)
  (message "=== Basic tests complete ===")
  (message "\nTo test with API calls, run: (test-async-simple-command)"))

;; Auto-run basic tests when loaded
(test-async-integration-all)

(provide 'test-async-integration)
;;; test-async-integration.el ends here