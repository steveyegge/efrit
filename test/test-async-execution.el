;;; test-async-execution.el --- Test async execution -*- lexical-binding: t -*-

(add-to-list 'load-path "../lisp")
(require 'efrit-async)

(message "\n=== Testing Efrit Async Execution ===")

;; Test 1: Basic async request
(message "\nTest 1: Basic async request")
(setq efrit-async--active-session nil)

(let ((response-received nil))
  (efrit-async--api-request 
   '((command . "test command"))
   (lambda (response)
     (setq response-received response)
     (message "✓ Callback invoked")
     (message "  Response: %S" response)))
  
  ;; Should not have response immediately
  (if response-received
      (error "Response should not be immediate!")
    (message "✓ Request is async (no immediate response)"))
  
  ;; Wait for response
  (message "  Waiting for async response...")
  (sit-for 1)
  
  ;; Should have response now
  (if response-received
      (message "✓ Response received after delay")
    (error "No response received!")))

;; Test 2: Response handling
(message "\nTest 2: Response handling")
(setq efrit-async--active-session nil)

(let ((test-response `((elisp . "(concat \"Hello \" \"async!\")")
                      (session . ((id . "handle-test")
                                (status . "complete"))))))
  (efrit-async--handle-response test-response)
  
  ;; Check session was created and completed
  (if efrit-async--active-session
      (error "Session should be cleared after complete status")
    (message "✓ Session cleared after complete")))

;; Test 3: Session flow simulation
(message "\nTest 3: Session flow simulation")
(setq efrit-async--active-session nil)

;; Simulate initial request
(message "  Starting async flow...")
(efrit-async--api-request
 '((command . "refactor function"))
 (lambda (response)
   (message "  Got response: %s" (alist-get 'status (alist-get 'session response)))
   (efrit-async--handle-response response)))

(sit-for 1)

;; Check final state
(if efrit-async--active-session
    (progn
      (message "✗ Session still active: %s" 
               (efrit-session-id efrit-async--active-session))
      (message "  Status: %s" (efrit-session-status efrit-async--active-session)))
  (message "✓ Session completed and cleared"))

;; Test 4: Error handling
(message "\nTest 4: Error handling")
(setq efrit-async--active-session 
      (make-efrit-session 
       :id "error-test"
       :start-time (current-time)
       :status 'active
       :buffer (current-buffer)))

(efrit-async--handle-error "Simulated error")
(if efrit-async--active-session
    (error "Session should be cleared on error")
  (message "✓ Session cleared on error"))

;; Test 5: Mode line integration
(message "\nTest 5: Mode line integration")
(if (member '(efrit-async-mode-line-string 
              (" " efrit-async-mode-line-string " "))
            mode-line-misc-info)
    (message "✓ Mode line integration configured")
  (error "Mode line not configured"))

(message "\n=== All async execution tests passed! ===\n")