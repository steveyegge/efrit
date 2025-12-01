;;; test-error-handling.el --- Test error classification and display -*- lexical-binding: t; -*-

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

(message "Testing error classification...\n")

;; Load the error classification function by requiring chat-api
(require 'efrit-chat-api)

;; Test cases for error classification
(let ((test-cases '(
  ("429 Too Many Requests" "rate-limit")
  ("401 Unauthorized" "auth-error")
  ("403 Forbidden - quota exceeded" "permission-error")
  ("400 Bad Request" "bad-request")
  ("500 Internal Server Error" "server-error")
  ("Connection refused" "network-error")
  ("Invalid model name: claude-xyz" "bad-request")
  ("Unknown error 123" "unknown-error")
  )))
  
  (message "Error Classification Tests:")
  (message "==============================\n")
  
  (dolist (test test-cases)
    (let* ((error-msg (nth 0 test))
           (expected-type (nth 1 test))
           (result (efrit--classify-error error-msg))
           (actual-type (nth 0 result))
           (description (nth 1 result))
           (recommendation (nth 2 result)))
      
      (if (string= actual-type expected-type)
          (message "✅ \"%s\"\n   Type: %s\n   Recommendation: %s\n"
                   error-msg actual-type recommendation)
        (message "❌ \"%s\"\n   Expected: %s, Got: %s\n"
                 error-msg expected-type actual-type)))))

(message "\n✅ Error classification test complete!")

;;; test-error-handling.el ends here
