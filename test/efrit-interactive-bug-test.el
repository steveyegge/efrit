;;; efrit-interactive-bug-test.el --- Interactive bug testing -*- lexical-binding: t -*-

;;; Commentary:
;; Test Efrit interactively to find bugs in the new session architecture

;;; Code:

(require 'efrit)

;; Set AI-Efrit channel
(setq efrit-api-channel "ai-efrit")

(defun efrit-bug-test ()
  "Run interactive bug tests."
  (interactive)
  
  (with-current-buffer (get-buffer-create "*efrit-bug-test*")
    (erase-buffer)
    (insert "Efrit Bug Hunt - Interactive Test\n")
    (insert "=================================\n\n")
    (insert "API Channel: ai-efrit\n")
    (insert "Testing various scenarios...\n\n"))
  
  ;; Test 1: Basic sync command
  (efrit-test-scenario "Basic sync command" 
    (efrit-do "What is the capital of France?"))
  
  ;; Test 2: Async command
  (efrit-test-scenario "Async command"
    (efrit-do-async "List three programming languages"))
  
  ;; Test 3: Unicode handling
  (efrit-test-scenario "Unicode test"
    (efrit-do "Echo this exactly: Hello 世界 🎉"))
  
  ;; Test 4: Large buffer context
  (with-temp-buffer
    (insert (make-string 10000 ?x))
    (efrit-test-scenario "Large buffer context"
      (efrit-do "How many characters are in this buffer?")))
  
  ;; Test 5: Rapid commands (queue test)
  (efrit-test-scenario "Rapid async commands"
    (progn
      (efrit-do-async "Count to 3")
      (efrit-do-async "Name a color")
      (efrit-do-async "What day is it?")))
  
  ;; Test 6: Error handling
  (efrit-test-scenario "Error handling"
    (efrit-do "(invalid-elisp-function-12345)"))
  
  ;; Test 7: Unified interface
  (efrit-test-scenario "Unified interface"
    (efrit-unified-do "Fetch the current time"))
  
  ;; Test 8: Cache test
  (efrit-test-scenario "Cache test"
    (progn
      (efrit-performance-clear-cache)
      (message "First call...")
      (benchmark-run 1 (efrit-do "What is 2+2?"))
      (message "Second call (should be cached)...")  
      (benchmark-run 1 (efrit-do "What is 2+2?"))))
  
  ;; Display results
  (display-buffer "*efrit-bug-test*"))

(defun efrit-test-scenario (name code)
  "Run test scenario NAME with CODE."
  (with-current-buffer "*efrit-bug-test*"
    (goto-char (point-max))
    (insert (format "\n[TEST] %s\n" name)))
  
  (condition-case err
      (progn
        (message "Testing: %s" name)
        (eval code)
        (with-current-buffer "*efrit-bug-test*"
          (goto-char (point-max))
          (insert "✓ Completed\n")))
    (error
     (with-current-buffer "*efrit-bug-test*"
       (goto-char (point-max))
       (insert (format "✗ Error: %s\n" (error-message-string err)))))))

;; Interactive test commands
(defun efrit-test-queue-management ()
  "Test queue management specifically."
  (interactive)
  (message "Queue before: %d items" (length efrit-async--session-queue))
  
  ;; Add several commands
  (dotimes (i 5)
    (efrit-async--add-to-queue (format "Test command %d" i)))
  
  (message "Queue after adding: %d items" (length efrit-async--session-queue))
  
  ;; Show queue
  (efrit-async-show-queue))

(defun efrit-test-performance-stats ()
  "Test performance statistics."
  (interactive)
  (efrit-performance-show-stats))

(defun efrit-test-memory-leak ()
  "Test for memory leaks."
  (interactive)
  (let ((initial-mem (garbage-collect)))
    (dotimes (i 10)
      (with-temp-buffer
        (insert (format "Test buffer %d" i))
        (efrit-context-capture-state)))
    
    (let ((final-mem (garbage-collect)))
      (message "Memory test complete. Check for growth."))))

(provide 'efrit-interactive-bug-test)
;;; efrit-interactive-bug-test.el ends here