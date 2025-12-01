;;; test-error-display.el --- Test error display in chat UI -*- lexical-binding: t; -*-

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

(message "Testing error display in chat buffer...\n")

;; Initialize chat
(efrit-chat)
(message "✅ Chat initialized")

;; Simulate an error by calling the error handler directly
(message "\nSimulating error handler with various error types:\n")

;; Test 1: Rate limit error
(message "Test 1: Rate limit error")
(with-current-buffer "*efrit-chat*"
  ;; Clear the buffer first
  (setq buffer-read-only nil)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq-local efrit--response-in-progress t)
    (efrit--display-message "Thinking..." 'system)))

(efrit--handle-http-error "429 Too Many Requests - rate limit exceeded")

(with-current-buffer "*efrit-chat*"
  (let ((content (buffer-string)))
    (if (string-match-p "rate.*limit" content)
        (message "✅ Rate limit error correctly displayed")
      (message "❌ Rate limit error not in buffer")))
  (message "Buffer preview:")
  (let ((lines (split-string (buffer-string) "\n")))
    (dolist (line (seq-take lines 5))
      (when (not (string-empty-p line))
        (message "  %s" line)))))

;; Test 2: Auth error
(message "\nTest 2: Auth error")
(with-current-buffer "*efrit-chat*"
  (setq buffer-read-only nil)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq-local efrit--response-in-progress t)
    (efrit--display-message "Thinking..." 'system)))

(efrit--handle-http-error "401 Unauthorized - Invalid API key")

(with-current-buffer "*efrit-chat*"
  (let ((content (buffer-string)))
    (if (string-match-p "API.*key" content)
        (message "✅ Auth error correctly displayed")
      (message "❌ Auth error not in buffer"))))

(message "\n✅ Error display test complete!")

;;; test-error-display.el ends here
