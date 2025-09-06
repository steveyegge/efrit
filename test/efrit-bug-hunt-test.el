;;; efrit-bug-hunt-test.el --- Bug hunting test suite -*- lexical-binding: t -*-

;;; Commentary:
;; Comprehensive test to find bugs in the new session-based architecture

;;; Code:

(require 'efrit)

(defun efrit-test-log (message &rest args)
  "Log MESSAGE with ARGS to test buffer."
  (with-current-buffer (get-buffer-create "*efrit-bug-hunt*")
    (goto-char (point-max))
    (insert (apply #'format (concat "\n" message) args))
    (insert "\n")))

(defun efrit-bug-test-1-rapid-commands ()
  "Test rapid command execution to find race conditions."
  (efrit-test-log "=== Test 1: Rapid Command Execution ===")
  
  ;; Fire off multiple commands quickly
  (dotimes (i 5)
    (efrit-async-execute-command 
     (format "What is %d + %d?" i (1+ i))
     (lambda (result)
       (efrit-test-log "Command %d result: %s" i 
                       (efrit-common-truncate-string result 50)))))
  
  (efrit-test-log "Fired 5 rapid commands"))

(defun efrit-bug-test-2-large-context ()
  "Test with very large context to find memory issues."
  (efrit-test-log "=== Test 2: Large Context Handling ===")
  
  ;; Create a buffer with large content
  (with-temp-buffer
    (dotimes (i 1000)
      (insert (format "Line %d: %s\n" i (make-string 100 ?x))))
    
    (efrit-test-log "Created buffer with %d chars" (buffer-size))
    
    ;; Try to execute command with this large context
    (condition-case err
        (efrit-do "summarize this buffer in one sentence")
      (error (efrit-test-log "ERROR: %s" err)))))

(defun efrit-bug-test-3-unicode-handling ()
  "Test unicode and special character handling."
  (efrit-test-log "=== Test 3: Unicode Handling ===")
  
  ;; Test various unicode scenarios
  (let ((test-strings '("Hello 世界"
                       "Emoji test: 🎉 🤖 💻"
                       "Math symbols: ∀x∈ℝ ∃y"
                       "Quotes: "curly" 'quotes'")))
    (dolist (str test-strings)
      (efrit-test-log "Testing: %s" str)
      (condition-case err
          (efrit-do (format "echo back exactly: %s" str))
        (error (efrit-test-log "ERROR with %s: %s" str err))))))

(defun efrit-bug-test-4-error-recovery ()
  "Test error handling and recovery."
  (efrit-test-log "=== Test 4: Error Recovery ===")
  
  ;; Test invalid elisp
  (efrit-test-log "Testing invalid elisp...")
  (efrit-do "(this-function-does-not-exist-12345)")
  
  ;; Test command after error
  (sit-for 1)
  (efrit-test-log "Testing recovery after error...")
  (efrit-do "What is 2 + 2?"))

(defun efrit-bug-test-5-session-cleanup ()
  "Test session cleanup and memory management."
  (efrit-test-log "=== Test 5: Session Cleanup ===")
  
  (let ((initial-sessions (hash-table-count efrit-async--sessions)))
    (efrit-test-log "Initial session count: %d" initial-sessions)
    
    ;; Create multiple sessions
    (dotimes (i 3)
      (efrit-async-execute-command 
       (format "Session %d test" i)
       #'ignore))
    
    (sit-for 2)
    (efrit-test-log "Sessions after creation: %d" 
                    (hash-table-count efrit-async--sessions))
    
    ;; Force cleanup
    (efrit-performance-run-cleanup)
    (efrit-test-log "Sessions after cleanup: %d"
                    (hash-table-count efrit-async--sessions))))

(defun efrit-bug-test-6-cache-invalidation ()
  "Test cache behavior and invalidation."
  (efrit-test-log "=== Test 6: Cache Invalidation ===")
  
  ;; Clear cache first
  (efrit-performance-clear-cache)
  
  ;; Execute same command twice
  (let ((command "What is the capital of Japan?"))
    (efrit-test-log "First execution...")
    (efrit-do command)
    
    (efrit-test-log "Second execution (should hit cache)...")
    (let ((start-time (float-time)))
      (efrit-do command)
      (let ((elapsed (- (float-time) start-time)))
        (efrit-test-log "Second execution time: %.3fs" elapsed)
        (when (> elapsed 0.5)
          (efrit-test-log "WARNING: Cache might not be working!"))))))

(defun efrit-bug-test-7-queue-overflow ()
  "Test queue size limits and overflow handling."
  (efrit-test-log "=== Test 7: Queue Overflow ===")
  
  ;; Try to overflow the queue
  (let ((max-size efrit-async-max-session-queue-size))
    (efrit-test-log "Queue max size: %d" max-size)
    
    ;; Add more than max
    (dotimes (i (+ max-size 5))
      (efrit-async--add-to-queue (format "Queue test %d" i)))
    
    (efrit-test-log "Queue size after overflow attempt: %d"
                    (length efrit-async--session-queue))))

(defun efrit-bug-test-8-concurrent-unified ()
  "Test concurrent unified interface calls."
  (efrit-test-log "=== Test 8: Concurrent Unified Calls ===")
  
  ;; Multiple unified calls
  (efrit-unified-do "What is 1 + 1?")
  (efrit-unified-do "What is 2 + 2?")
  (efrit-unified-do "List three colors")
  
  (efrit-test-log "Fired 3 unified commands")
  
  ;; Check status
  (sit-for 1)
  (efrit-unified-status))

(defun efrit-bug-test-9-mode-line-cleanup ()
  "Test mode line updates and cleanup."
  (efrit-test-log "=== Test 9: Mode Line Cleanup ===")
  
  ;; Start async command
  (efrit-async-execute-command 
   "Count to 5 slowly"
   (lambda (_)
     (efrit-test-log "Mode line should be cleared now")))
  
  ;; Cancel it
  (sit-for 0.5)
  (when efrit-async--active-session
    (efrit-test-log "Cancelling active session...")
    (efrit-async-cancel)))

(defun efrit-bug-test-10-api-key-switching ()
  "Test API key retrieval for ai-efrit channel."
  (efrit-test-log "=== Test 10: API Key Channel Test ===")
  
  ;; Temporarily set channel
  (let ((efrit-api-channel "ai-efrit"))
    (condition-case err
        (let ((key (efrit-common-get-api-key)))
          (if (string-prefix-p "sk-ant-" key)
              (efrit-test-log "✓ Retrieved ai-efrit API key correctly")
            (efrit-test-log "✗ Wrong API key retrieved")))
      (error (efrit-test-log "ERROR getting API key: %s" err)))))

;; Run all tests
(defun efrit-run-bug-hunt ()
  "Run all bug hunting tests."
  (interactive)
  
  ;; Clear log buffer
  (with-current-buffer (get-buffer-create "*efrit-bug-hunt*")
    (erase-buffer))
  
  (efrit-test-log "Starting Efrit Bug Hunt - %s" (current-time-string))
  (efrit-test-log "Using API channel: %s" (or efrit-api-channel "default"))
  
  ;; Run tests with delays between them
  (efrit-bug-test-1-rapid-commands)
  (sit-for 2)
  
  (efrit-bug-test-2-large-context)
  (sit-for 2)
  
  (efrit-bug-test-3-unicode-handling)
  (sit-for 2)
  
  (efrit-bug-test-4-error-recovery)
  (sit-for 2)
  
  (efrit-bug-test-5-session-cleanup)
  (sit-for 2)
  
  (efrit-bug-test-6-cache-invalidation)
  (sit-for 2)
  
  (efrit-bug-test-7-queue-overflow)
  (sit-for 2)
  
  (efrit-bug-test-8-concurrent-unified)
  (sit-for 2)
  
  (efrit-bug-test-9-mode-line-cleanup)
  (sit-for 2)
  
  (efrit-bug-test-10-api-key-switching)
  
  (efrit-test-log "\n=== Bug Hunt Complete ===")
  (display-buffer "*efrit-bug-hunt*"))

;; Set API channel and run tests
(setq efrit-api-channel "ai-efrit")
(efrit-run-bug-hunt)

(provide 'efrit-bug-hunt-test)
;;; efrit-bug-hunt-test.el ends here