;;; efrit-basic-integration-test.el --- Basic integration test -*- lexical-binding: t -*-

;; A simple integration test to verify the API connection works
;; and basic dashboard/session integration

;;; Code:

(require 'efrit)
(require 'efrit-do)
(require 'efrit-dashboard)
(require 'efrit-session-tracker)

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
  
  ;; Test 3: Session tracking integration
  (message "\nTest 3: Session tracking integration...")
  (condition-case err
      (progn
        (efrit-session-start)
        (let ((session-id efrit-session-id))
          (efrit-session-track-command "integration test")
          (let ((commands-count (efrit-session-get-metric 'commands-executed)))
            (if (>= commands-count 1)
                (message "✓ Session tracking works: %d commands tracked" commands-count)
              (message "✗ Session tracking failed"))))
        (efrit-session-end))
    (error 
     (message "✗ Session tracking failed: %s" (error-message-string err))))
  
  ;; Test 4: Dashboard basic functionality
  (message "\nTest 4: Dashboard basic functionality...")
  (condition-case err
      (progn
        (efrit-dashboard)
        (if (get-buffer "*efrit-dashboard*")
            (progn
              (with-current-buffer "*efrit-dashboard*"
                (if (string-match-p "SESSION STATE" (buffer-string))
                    (message "✓ Dashboard created and shows sections")
                  (message "✗ Dashboard content missing")))
              (kill-buffer "*efrit-dashboard*"))
          (message "✗ Dashboard buffer not created")))
    (error 
     (message "✗ Dashboard creation failed: %s" (error-message-string err))))
  
  ;; Test 5: Async via unified interface (if available)
  (message "\nTest 5: Async command via unified interface...")
  (condition-case err
      (progn
        (if (fboundp 'efrit-do-async)
            (progn
              (efrit-do-async "What is 5 + 5?")
              (message "✓ Async command started"))
          (message "⚠ efrit-do-async not available, skipping")))
    (error 
     (message "✗ Async command failed: %s" (error-message-string err))))
  
  (message "\n=== Test Complete ===\n"))

;; Run the test
(efrit-basic-integration-test)

(provide 'efrit-basic-integration-test)
;;; efrit-basic-integration-test.el ends here