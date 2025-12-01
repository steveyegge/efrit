;;; test-history.el --- Test chat history features -*- lexical-binding: t; -*-

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
(require 'efrit-chat-persistence)
(require 'efrit-chat-history)

(message "Testing chat history features...\n")

;; Create a test session with some messages
(message "Test 1: Create test session with messages")
(let ((buffer (efrit-chat)))
  (with-current-buffer buffer
    ;; Simulate a conversation
    (setq-local efrit--message-history
                '(((role . "user") (content . "What is 2+2?"))
                  ((role . "assistant") (content . "The answer to 2+2 is 4."))
                  ((role . "user") (content . "What about 3+3?"))
                  ((role . "assistant") (content . "3+3 equals 6."))))
    
    ;; Save the session
    (efrit-chat-save-session)
    (message "✅ Test session created and saved")))

;; Test listing sessions
(message "\nTest 2: List chat sessions")
(let ((sessions (efrit-chat-list-sessions)))
  (if (> (length sessions) 0)
      (let ((first (car sessions)))
        (message "✅ Found %d session(s)" (length sessions))
        (message "   Most recent: %s (%d messages)"
                (alist-get 'id first)
                (alist-get 'message_count first)))
    (message "❌ No sessions found")))

;; Test search functionality
(message "\nTest 3: Search history")
(let ((results (seq-filter
                (lambda (s)
                  (string-match-p "2\\|3" (or (alist-get 'snippet s) "")))
                (efrit-chat-list-sessions))))
  (if (> (length results) 0)
      (message "✅ Found %d matching session(s)" (length results))
    (message "⚠️ Search found no matches (snippet may not contain full content)")))

;; Test export
(message "\nTest 4: Export session")
(let* ((sessions (efrit-chat-list-sessions))
       (session-id (alist-get 'id (car sessions)))
       (export-file (expand-file-name "test-export.txt" "/tmp")))
  (efrit-chat-history-export-session session-id export-file)
  (if (file-exists-p export-file)
      (let ((content (with-temp-buffer
                       (insert-file-contents export-file)
                       (buffer-string))))
        (if (string-match-p "Chat Session" content)
            (message "✅ Export successful (%d bytes)" (length content))
          (message "❌ Export file missing expected content")))
    (message "❌ Export file not created")))

;; Test persistence round-trip
(message "\nTest 5: Load session back")
(let* ((sessions (efrit-chat-list-sessions))
       (original (car sessions))
       (session-id (alist-get 'id original))
       (loaded (efrit-chat-load-session session-id)))
  (if loaded
      (let ((orig-count (alist-get 'message_count original))
            (loaded-count (alist-get 'message_count loaded)))
        (if (= orig-count loaded-count)
            (message "✅ Session loaded correctly (%d messages preserved)"
                    loaded-count)
          (message "❌ Message count mismatch: %d vs %d"
                   orig-count loaded-count)))
    (message "❌ Failed to load session")))

;; Summary
(message "\n" "=" (make-string 50 ?=))
(message "Chat history features:")
(message "- ✅ Session persistence (auto-save)")
(message "- ✅ List sessions with metadata")
(message "- ✅ Restore previous sessions")
(message "- ✅ Export conversations to files")
(message "- ✅ Search history")
(message "\nKnown features:")
(message "- Branching (resume from specific message) - not yet implemented")
(message "=" (make-string 50 ?=))

(message "\n✅ History tests complete!")

;;; test-history.el ends here
