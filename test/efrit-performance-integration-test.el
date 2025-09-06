;;; efrit-performance-integration-test.el --- Test performance features -*- lexical-binding: t -*-

;;; Code:

(require 'efrit)
(require 'efrit-performance)

(defun efrit-perf-test-cache ()
  "Test caching functionality."
  (message "\n=== Performance Caching Test ===")
  
  ;; Clear cache first
  (efrit-performance-clear-cache)
  
  ;; Test 1: Execute command and measure time
  (message "\nTest 1: Initial command execution...")
  (let ((start-time (float-time)))
    (efrit-do "What is the capital of France?")
    (let ((first-time (- (float-time) start-time)))
      (message "First execution: %.2fs" first-time)
      
      ;; Test 2: Execute same command again (should hit cache)
      (message "\nTest 2: Cached execution...")
      (let ((cache-start (float-time)))
        (efrit-do "What is the capital of France?") 
        (let ((cache-time (- (float-time) cache-start)))
          (message "Cached execution: %.2fs" cache-time)
          (if (< cache-time 0.1)
              (message "✓ Cache hit successful!")
            (message "✗ Cache might not be working")))))))

(defun efrit-perf-test-stats ()
  "Test performance statistics."
  (message "\n=== Performance Statistics Test ===")
  
  ;; Run a few commands to gather stats
  (message "\nGenerating performance data...")
  (efrit-do "What is 1 + 1?")
  (sit-for 0.5)
  (efrit-do "What is 2 + 2?")
  (sit-for 0.5)
  
  ;; Show stats
  (message "\nPerformance statistics:")
  (efrit-performance-show-stats))

(defun efrit-perf-test-memory ()
  "Test memory management."
  (message "\n=== Memory Management Test ===")
  
  ;; Check initial session count
  (let ((initial-count (hash-table-count efrit-async--sessions)))
    (message "\nInitial sessions: %d" initial-count)
    
    ;; Create multiple sessions
    (dotimes (i 3)
      (efrit-async-execute-command 
       (format "Test command %d" i)
       (lambda (_) nil)))
    
    (sit-for 1)
    (message "Sessions after creation: %d" 
             (hash-table-count efrit-async--sessions))
    
    ;; Run cleanup
    (efrit-performance-run-cleanup)
    (message "Sessions after cleanup: %d"
             (hash-table-count efrit-async--sessions))))

;; Run the tests
(when (y-or-n-p "Run performance integration tests? (will consume tokens) ")
  (efrit-perf-test-cache)
  (efrit-perf-test-stats) 
  (efrit-perf-test-memory)
  (message "\n=== Performance Tests Complete ==="))

(provide 'efrit-performance-integration-test)
;;; efrit-performance-integration-test.el ends here