#!/usr/bin/env emacs --script

;; Offline test of async infrastructure

(add-to-list 'load-path (expand-file-name "lisp" default-directory))

(require 'efrit-log)
(require 'efrit-common)
(require 'efrit-config)
(require 'efrit-tools)
(require 'efrit-async)
(require 'efrit-do)

(message "Testing async infrastructure without API calls...")

;; Test the session management
(let ((session (make-efrit-session 
                :id "test-session-123"
                :status 'active
                :start-time (current-time))))
  
  (setq efrit-async--active-session session)
  
  ;; Test progress display
  (efrit-async--show-progress "Testing progress display")
  (message "Current session ID: %s" (efrit-session-id efrit-async--active-session))
  
  ;; Test session update
  (efrit-async--update-session "test-session-123" "test result" "(message \"test\")")
  (message "Work log length: %d" (length (efrit-session-work-log efrit-async--active-session)))
  
  ;; Test session completion
  (efrit-async--complete-session "test-session-123" "final result")
  
  (if efrit-async--active-session
      (message "ERROR: Session should be nil after completion")
    (message "✓ Session properly cleared after completion")))

;; Test async status functions
(message "\n=== Testing Status Functions ===")
(efrit-async-status)

(message "\n=== Testing Log Functions ===")
;; Create a mock session for log testing
(let ((session (make-efrit-session 
                :id "log-test"
                :status 'active
                :start-time (current-time))))
  (setq efrit-async--active-session session)
  (efrit-async--update-session "log-test" "result 1" "(+ 1 1)")
  (efrit-async--update-session "log-test" "result 2" "(+ 2 2)")
  
  ;; Test compression
  (let ((compressed (efrit-session--compress-log session)))
    (message "Compressed log: %S" compressed))
  
  (setq efrit-async--active-session nil))

(message "\n✓ All offline tests completed successfully!")
(kill-emacs 0)