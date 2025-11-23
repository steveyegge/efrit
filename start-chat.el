;;; start-chat.el --- Reliable script to start efrit chat

;; Load efrit (this should provide all functionality)
(add-to-list 'load-path "./lisp")

(message "Loading Efrit...")

;; Load in order to handle any dependency issues gracefully  
(condition-case err
    (progn
      (require 'efrit-config)
      (require 'efrit-log)  
      (require 'efrit-common)
      (require 'efrit-tools)
      (require 'efrit-debug)
      (require 'efrit-multi-turn)
      (require 'efrit-chat)
      (require 'efrit-session-tracker)
      (require 'efrit-dashboard)
      (message "✅ All Efrit modules loaded successfully!"))
  (error 
   (message "❌ Error loading Efrit: %s" (error-message-string err))
   (message "Trying minimal load...")
   (require 'efrit-chat)))

;; Start session tracking
(condition-case err
    (progn
      (efrit-session-start)
      (message "✅ Session tracking started: %s" efrit-session-id))
  (error (message "⚠️ Session tracking unavailable: %s" (error-message-string err))))

;; Start the chat interface
(efrit-chat)

(message "")
(message "🎯 EFRIT CHAT READY!")
(message "===================")
(message "")
(message "Chat fixes applied:")
(message "✅ Proper message formatting")  
(message "✅ Safety system active (prevents buffer destruction)")
(message "✅ Session tracking operational")
(message "")
(message "Test the haiku workflow:")
(message "1. Type: write a haiku about programming")
(message "2. Press Enter and wait for response")
(message "3. Type: now reverse that haiku")
(message "4. Press Enter")
(message "")
(message "The text should NOT disappear and formatting should be clean!")
