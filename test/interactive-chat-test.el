;;; Interactive test for efrit chat functionality

;; Load efrit
(add-to-list 'load-path "./lisp")
(require 'efrit)

;; Start chat
(efrit-chat)

;; Add some instructions to the buffer
(with-current-buffer "*efrit-chat*"
  (goto-char (point-max))
  (insert "write a haiku about coding"))

(message "✅ Efrit chat is ready!")
(message "✅ Type 'write a haiku about coding' and press Enter")
(message "✅ Then type 'now reverse that haiku' and press Enter")
(message "")
(message "The chat buffer should show proper formatting with:")
(message "- Assistant: messages on separate lines")
(message "- You: messages properly formatted") 
(message "- No text disappearing between exchanges")
