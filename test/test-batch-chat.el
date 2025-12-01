;;; test-batch-chat.el --- Test efrit-chat in batch mode -*- lexical-binding: t; -*-

;; Add lisp directory to load path
(add-to-list 'load-path "./lisp")
(add-to-list 'load-path "./lisp/core")
(add-to-list 'load-path "./lisp/support")
(add-to-list 'load-path "./lisp/interfaces")
(add-to-list 'load-path "./lisp/tools")

;; Load just what we need for chat
(require 'efrit-config)
(require 'efrit-common)
(require 'efrit-log)
(require 'efrit-tools)
(require 'efrit-chat)

(message "✅ All chat dependencies loaded successfully!")

;; Verify we're in batch mode
(message "Running in batch mode: %s" noninteractive)

;; Start chat
(efrit-chat)
(message "✅ Chat initialized!")

;; Show initial buffer state
(with-current-buffer "*efrit-chat*"
  (message "Initial chat buffer:")
  (message "--- START ---")
  (message "%s" (buffer-string))
  (message "--- END ---"))

;; Test programmatic message sending
(message "\n🧪 Testing programmatic message in batch mode...")

;; Send a test message
(efrit-send-message "Say 'Batch mode works!'")

;; In batch mode with synchronous requests, the response should already be processed
(with-current-buffer "*efrit-chat*"
  (let ((final-content (buffer-string)))
    (message "Chat buffer after API call:")
    (message "--- START ---")
    (message "%s" final-content)
    (message "--- END ---")
    
    ;; Verify response was received
    (if (string-match-p "Batch mode works" final-content)
        (message "✅ SUCCESS: Response received and processed in batch mode!")
      (if (string-match-p "Assistant:" final-content)
          (message "⚠️ Response buffer has assistant block but may not be fully parsed")
        (message "❌ FAILURE: No response from API")))))

(message "\n✅ Batch mode test completed!")

;;; test-batch-chat.el ends here
