;;; efrit-simple-bug-test.el --- Simple bug test -*- lexical-binding: t -*-

;;; Code:

;; Load efrit modules manually in correct order
(add-to-list 'load-path (expand-file-name "../lisp" 
                                         (file-name-directory load-file-name)))

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
(require 'efrit-remote-queue)

;; Set AI-Efrit channel
(setq efrit-api-channel "ai-efrit")

(message "\n=== Efrit Bug Test (ai-efrit channel) ===")
(message "API Channel: %s" efrit-api-channel)

;; Test 1: Basic sync command
(message "\nTest 1: Basic sync command...")
(condition-case err
    (progn
      (efrit-do "What is 2 + 2?")
      (message "✓ Sync command works"))
  (error 
   (message "✗ Sync command failed: %s" (error-message-string err))))

;; Test 2: Queue management
(message "\nTest 2: Queue management...")  
(condition-case err
    (progn
      ;; Add to queue
      (efrit-async--add-to-queue "Test command 1")
      (efrit-async--add-to-queue "Test command 2")
      (message "Queue size: %d" (length efrit-async--session-queue))
      (if (= 2 (length efrit-async--session-queue))
          (message "✓ Queue management works")
        (message "✗ Queue size incorrect")))
  (error
   (message "✗ Queue test failed: %s" (error-message-string err))))

;; Test 3: Context capture
(message "\nTest 3: Context capture...")
(condition-case err
    (with-temp-buffer
      (insert "Test buffer content")
      (let ((context (efrit-context-capture-state)))
        (if (and context 
                 (string= "Test buffer content" 
                         (efrit-context-state-buffer-string context)))
            (message "✓ Context capture works") 
          (message "✗ Context capture incorrect"))))
  (error
   (message "✗ Context test failed: %s" (error-message-string err))))

;; Test 4: Performance cache
(message "\nTest 4: Performance cache...")
(condition-case err
    (progn
      (efrit-performance-clear-cache)
      (let* ((key "test-key")
             (value "test-value"))
        (efrit-performance-cache-put key value)
        (let ((retrieved (efrit-performance-get-cached key)))
          (if (string= value retrieved)
              (message "✓ Cache works")
            (message "✗ Cache retrieval failed")))))
  (error
   (message "✗ Cache test failed: %s" (error-message-string err))))

;; Test 5: API key retrieval
(message "\nTest 5: API key for ai-efrit...")
(condition-case err
    (let ((key (efrit-common-get-api-key)))
      (cond
       ((not key)
        (message "✗ No API key retrieved"))
       ((string-prefix-p "sk-ant-api03-8THYk7pL" key)
        (message "✓ Correct ai-efrit API key"))
       (t
        (message "✗ Wrong API key: %s..." (substring key 0 20)))))
  (error
   (message "✗ API key test failed: %s" (error-message-string err))))

;; Test 6: Unicode handling
(message "\nTest 6: Unicode handling...")
(condition-case err
    (let ((test-str "Hello 世界 🎉"))
      (efrit-do (format "Echo exactly: %s" test-str))
      (message "✓ Unicode handled"))
  (error
   (message "✗ Unicode test failed: %s" (error-message-string err))))

(message "\n=== Tests Complete ===")

(provide 'efrit-simple-bug-test)
;;; efrit-simple-bug-test.el ends here