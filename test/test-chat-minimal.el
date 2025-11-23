;;; test-chat-minimal.el --- Minimal chat test -*- lexical-binding: t; -*-

;; Load only what's needed for efrit-chat

(add-to-list 'load-path "./lisp")

;; Load dependencies in order
(require 'efrit-config)
(require 'efrit-common)
(require 'efrit-log)
(require 'efrit-tools)
(require 'efrit-debug)
(require 'efrit-multi-turn)
(require 'efrit-chat)

(message "✅ All chat dependencies loaded successfully!")

;; Start chat
(efrit-chat)

(message "✅ Chat started! Try typing a message and pressing Enter.")

;; Show buffer content
(with-current-buffer "*efrit-chat*"
  (message "Initial chat buffer:")
  (message "--- START ---")
  (message "%s" (buffer-string))
  (message "--- END ---"))

;; Test programmatic message sending
(message "\n🧪 Testing programmatic message sending...")

;; Send a test message programmatically
(efrit-send-message "What is 5 + 3?")

;; Give it a moment to process
(sleep-for 3)

(with-current-buffer "*efrit-chat*"
  (message "Chat buffer after API call:")
  (message "--- START ---")
  (message "%s" (buffer-string))
  (message "--- END ---"))

(message "✅ Minimal chat test completed!")

;;; test-chat-minimal.el ends here
