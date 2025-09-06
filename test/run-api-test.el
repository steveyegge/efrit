;;; run-api-test.el --- Run API test -*- lexical-binding: t -*-

(add-to-list 'load-path (expand-file-name "../lisp" 
                                         (file-name-directory load-file-name)))

;; Load modules
(require 'efrit-config)
(require 'efrit-common) 
(require 'efrit-log)
(require 'efrit-protocol)
(require 'efrit-tools)
(require 'efrit-context)
(require 'efrit-performance)
(require 'efrit-multi-turn)
(require 'efrit-chat)
(require 'efrit-chat-streamlined)
(require 'efrit-do)
(require 'efrit-async)
(require 'efrit-unified)

;; Set AI-Efrit channel and debug mode
(setq efrit-api-channel "ai-efrit")
(setq efrit-debug-mode t)
(setq efrit-log-to-messages t)

(message "\n=== Efrit API Test (ai-efrit channel) ===")

;; Bug Test 1: API Key retrieval
(message "\nBug Test 1: API key retrieval...")
(let ((key (efrit-common-get-api-key)))
  (if (and key (string-prefix-p "sk-ant-api03-8THYk7pL" key))
      (message "✓ Correct ai-efrit API key retrieved")
    (message "✗ Wrong API key: %s" (if key (substring key 0 20) "nil"))))

;; Bug Test 2: Simple sync command
(message "\nBug Test 2: Sync command...")
(condition-case err
    (let ((result (efrit-do "What is 2 + 2?")))
      (message "✓ Sync command returned: %s" 
               (if (stringp result)
                   (substring result 0 (min 50 (length result)))
                 result)))
  (error 
   (message "✗ Sync command failed: %s" (error-message-string err))))

;; Bug Test 3: Model name check
(message "\nBug Test 3: Model name...")
(if (string= efrit-model "claude-3-5-sonnet-20241022")
    (message "✓ Using correct model: %s" efrit-model)
  (message "✗ Wrong model: %s" efrit-model))

;; Bug Test 4: Context capture
(message "\nBug Test 4: Context capture...")
(with-temp-buffer
  (insert "Test content")
  (let ((context (efrit-context-capture-state)))
    (if (and context
             (string= "Test content" (efrit-context-state-buffer-string context)))
        (message "✓ Context capture works")
      (message "✗ Context capture failed"))))

;; Bug Test 5: Performance cache
(message "\nBug Test 5: Cache functionality...")
(efrit-performance-clear-cache)
(let ((cmd "What is the speed of light?")
      (time1 nil)
      (time2 nil))
  ;; First call
  (let ((start (float-time)))
    (efrit-do cmd)
    (setq time1 (- (float-time) start)))
  
  ;; Second call (should be cached)
  (let ((start (float-time)))
    (efrit-do cmd)
    (setq time2 (- (float-time) start)))
  
  (message "First call: %.2fs, Second call: %.2fs" time1 time2)
  (if (< time2 (* time1 0.1))  ; Second should be <10% of first
      (message "✓ Cache appears to work")
    (message "✗ Cache might not be working")))

;; Bug Test 6: Error in tool execution
(message "\nBug Test 6: Error handling...")
(condition-case err
    (progn
      (efrit-do "(this-is-not-a-valid-function-xyz)")
      (message "✓ Error handled gracefully"))
  (error
   (message "✗ Unhandled error: %s" (error-message-string err))))

(message "\n=== API Tests Complete ===")

(provide 'run-api-test)
;;; run-api-test.el ends here