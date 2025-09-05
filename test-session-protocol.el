#!/usr/bin/env emacs --script

;; Test for session protocol implementation

(add-to-list 'load-path (expand-file-name "lisp" default-directory))

(require 'efrit-log)
(require 'efrit-common)
(require 'efrit-config)
(require 'efrit-tools)
(require 'efrit-async)
(require 'efrit-do)

;; Override API key check for testing
(defun efrit-config-get-api-key ()
  "Override for testing - returns dummy key."
  "test-api-key-123")

(message "=== Testing Session Protocol Implementation ===")
(message "")

;; Test 1: Verify system prompt includes session protocol
(message "Test 1: System prompt for session mode")
(let* ((prompt-normal (efrit-do--command-system-prompt))
       (prompt-session (efrit-do--command-system-prompt nil nil nil "test-session" "[['result1', 'code1']]")))
  (if (string-match "SESSION MODE ACTIVE" prompt-session)
      (message "✓ Session mode detected in system prompt")
    (message "✗ Session mode NOT detected in system prompt"))
  
  (if (string-match "SESSION PROTOCOL:" prompt-session)
      (message "✓ Session protocol instructions included")
    (message "✗ Session protocol instructions NOT included")))

(message "")
(message "Test 2: Tools schema includes session_complete")
(let ((tools efrit-do--tools-schema)
      (found-session-complete nil))
  (dotimes (i (length tools))
    (let* ((tool (aref tools i))
           (name (cdr (assoc "name" tool))))
      (when (string= name "session_complete")
        (setq found-session-complete t))))
  (if found-session-complete
      (message "✓ session_complete tool found in schema")
    (message "✗ session_complete tool NOT found in schema")))

(message "")
(message "Test 3: Session complete handler")
(let* ((tool-input (make-hash-table :test 'equal))
       (result nil))
  (puthash "message" "Test completed successfully" tool-input)
  (setq result (efrit-do--handle-session-complete tool-input))
  (if (string-match "SESSION-COMPLETE: Test completed successfully" result)
      (message "✓ Session complete handler works correctly")
    (message "✗ Session complete handler failed: %s" result)))

(message "")
(message "Test 4: Session object creation in async")
(let ((test-complete nil))
  ;; Mock the API request to avoid actual API calls
  (cl-letf (((symbol-function 'efrit-async--api-request)
             (lambda (data callback)
               (message "✓ API request would be made with session protocol")
               (setq test-complete t))))
    
    (efrit-async-execute-command "test command" 
                                (lambda (result) 
                                  (message "Callback called with: %s" result)))
    
    (if efrit-async--active-session
        (progn
          (message "✓ Active session created: %s" (efrit-session-id efrit-async--active-session))
          (message "  Command: %s" (efrit-session-command efrit-async--active-session))
          (message "  Status: %s" (efrit-session-status efrit-async--active-session)))
      (message "✗ No active session created"))))

(message "")
(message "=== All Session Protocol Tests Complete ===")
(kill-emacs 0)