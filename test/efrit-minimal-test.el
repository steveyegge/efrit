;;; efrit-minimal-test.el --- Minimal integration test -*- lexical-binding: t -*-

;;; Code:

;; Load only necessary modules
(add-to-list 'load-path (expand-file-name "../lisp" 
                                         (file-name-directory load-file-name)))

(require 'efrit-common)
(require 'efrit-log)
(require 'efrit-protocol)
(require 'efrit-context)
(require 'efrit-do)

(message "\n=== Efrit Minimal Integration Test ===")
(message "Testing core functionality with real API calls...\n")

;; Test 1: Simple command
(message "Test 1: Basic sync command...")
(condition-case err
    (progn
      (efrit-do "What is the capital of Japan?")
      (message "✓ Basic command succeeded\n"))
  (error 
   (message "✗ Basic command failed: %s\n" err)))

;; Test 2: Multi-step session  
(message "Test 2: Multi-step session...")
(condition-case err
    (progn
      (efrit-do "First, tell me what 10 + 20 equals. Then multiply that result by 2.")
      (message "✓ Multi-step session succeeded\n"))
  (error
   (message "✗ Multi-step session failed: %s\n" err)))

;; Test 3: Context persistence
(message "Test 3: Context operations...")
(let ((initial-count (length (efrit-context-ring-get-recent))))
  (efrit-do "What color is the sky?")
  (let ((new-count (length (efrit-context-ring-get-recent))))
    (if (> new-count initial-count)
        (message "✓ Context capture succeeded (%d items)\n" new-count)
      (message "✗ Context capture failed\n"))))

;; Test 4: Error handling
(message "Test 4: Error handling...")
(condition-case err
    (progn  
      (efrit-do "(this-is-not-a-valid-elisp-function-12345)")
      (message "✓ Error was handled gracefully\n"))
  (error
   (message "✗ Unexpected error: %s\n" err)))

(message "=== Tests Complete ===")

(provide 'efrit-minimal-test)
;;; efrit-minimal-test.el ends here