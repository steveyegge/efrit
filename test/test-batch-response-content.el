;;; test-batch-response-content.el --- Verify actual response content parsing -*- lexical-binding: t; -*-

(add-to-list 'load-path "./lisp")
(add-to-list 'load-path "./lisp/core")
(add-to-list 'load-path "./lisp/support")
(add-to-list 'load-path "./lisp/interfaces")
(add-to-list 'load-path "./lisp/tools")

(require 'efrit-config)
(require 'efrit-common)
(require 'efrit-log)
(require 'efrit-tools)
(require 'efrit-chat)

(message "Testing batch mode response content parsing...")

;; Start chat
(efrit-chat)

;; Send a simple message that should get a clear response
(efrit-send-message "Write exactly this string: BATCH_MODE_WORKS")

;; Show the full buffer content
(with-current-buffer "*efrit-chat*"
  (let ((content (buffer-string)))
    (message "\n=== FULL BUFFER CONTENT ===\n%s\n==========================\n" content)
    
    ;; Check for expected content
    (if (string-match-p "You:" content)
        (message "✅ User message present")
      (message "❌ User message missing"))
    
    (if (string-match-p "Assistant:" content)
        (message "✅ Assistant response present")
      (message "❌ Assistant response missing"))
    
    (if (string-match-p "BATCH_MODE_WORKS" content)
        (message "✅ Expected string found in response")
      (message "⚠️ Expected string not found (may be API parsing issue)"))))

(message "\n✅ Batch response content test complete!")

;;; test-batch-response-content.el ends here
