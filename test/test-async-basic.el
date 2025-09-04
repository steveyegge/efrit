;;; test-async-basic.el --- Basic tests for efrit-async -*- lexical-binding: t -*-

(add-to-list 'load-path "../lisp")
(require 'efrit-async)

(message "\n=== Testing Efrit Async Basic Functionality ===")

;; Test 1: Session creation
(message "\nTest 1: Session creation")
(let ((session (make-efrit-session 
                :id "test-123"
                :start-time (current-time)
                :status 'active
                :buffer (current-buffer))))
  (message "✓ Created session: %s" (efrit-session-id session))
  (message "  Status: %s" (efrit-session-status session))
  (message "  Buffer: %s" (buffer-name (efrit-session-buffer session))))

;; Test 2: Session management
(message "\nTest 2: Session management")
(setq efrit-async--active-session nil)
(efrit-async--update-session "test-456" "result1" "(test elisp 1)")
(if efrit-async--active-session
    (progn
      (message "✓ Session created via update")
      (message "  ID: %s" (efrit-session-id efrit-async--active-session))
      (message "  Work log length: %d" 
               (length (efrit-session-work-log efrit-async--active-session))))
  (error "Session not created"))

;; Add another step
(efrit-async--update-session "test-456" "result2" "(test elisp 2)")
(message "✓ Added second step")
(message "  Work log length: %d" 
         (length (efrit-session-work-log efrit-async--active-session)))

;; Test 3: Work log compression
(message "\nTest 3: Work log compression")
(let ((compressed (efrit-session--compress-log efrit-async--active-session)))
  (message "✓ Compressed log:")
  (message "  Step count: %s" (alist-get 'step-count compressed))
  (message "  Last result: %s" (alist-get 'last-result compressed))
  (message "  Elapsed time: %.2fs" (alist-get 'elapsed-time compressed)))

;; Test 4: Session completion
(message "\nTest 4: Session completion")
(efrit-async--complete-session "test-456" "final result")
(if (null efrit-async--active-session)
    (message "✓ Session cleared after completion")
  (error "Session not cleared"))

;; Test 5: Queue management
(message "\nTest 5: Queue management")
(setq efrit-async--session-queue nil)
(push "command1" efrit-async--session-queue)
(push "command2" efrit-async--session-queue)
(message "✓ Queue has %d items" (length efrit-async--session-queue))
(let ((cmd (pop efrit-async--session-queue)))
  (message "✓ Popped: %s" cmd)
  (message "  Queue now has %d items" (length efrit-async--session-queue)))

;; Test 6: Progress display (visual test)
(message "\nTest 6: Progress display")
(setq efrit-async--active-session 
      (make-efrit-session 
       :id "progress-test"
       :start-time (current-time)
       :status 'active
       :buffer (current-buffer)))
(efrit-async--show-progress "Testing...")
(message "✓ Progress shown (check minibuffer and mode line)")
(sleep-for 1)
(efrit-async--show-progress "Still working...")
(sleep-for 1)
(efrit-async--complete-session "progress-test" "done")

(message "\n=== All basic async tests passed! ===\n")