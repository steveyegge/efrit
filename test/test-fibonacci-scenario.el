;;; test-fibonacci-scenario.el --- Test fibonacci multi-turn scenario -*- lexical-binding: t; -*-

;;; Commentary:
;; This is the REAL integration test that validates the tool use protocol.
;; The test:
;; 1. Sends "Write fibonacci in *scratch*"
;; 2. Verifies code appears in *scratch*
;; 3. Sends "Now evaluate fib(10)"
;; 4. Verifies result is 55
;;
;; This test BURNS TOKENS - it makes real API calls to Claude.
;; This is the test case that originally exposed the broken tool protocol.

;;; Code:

(add-to-list 'load-path (expand-file-name "./lisp"))
(add-to-list 'load-path (expand-file-name "./lisp/core"))
(add-to-list 'load-path (expand-file-name "./lisp/interfaces"))
(add-to-list 'load-path (expand-file-name "./lisp/support"))

;; Load dependencies
(require 'efrit-config)
(require 'efrit-common)
(require 'efrit-log)
(require 'efrit-session)
(require 'efrit-tools)
(require 'efrit-chat)

;; Test configuration
(defvar test-fib--timeout 120
  "Maximum seconds to wait for each API response.")

(defvar test-fib--poll-interval 1
  "Seconds between polling for response completion.")

(defun test-fib--wait-for-response ()
  "Wait for the current response to complete.
Returns t if response completed, nil if timeout."
  (let ((start-time (current-time))
        (completed nil))
    (while (and (not completed)
                (< (float-time (time-since start-time)) test-fib--timeout))
      (sleep-for test-fib--poll-interval)
      ;; Check if response is still in progress
      (with-current-buffer "*efrit-chat*"
        (unless efrit--response-in-progress
          (setq completed t))))
    completed))

(defun test-fib--scratch-contains-fib ()
  "Check if *scratch* contains a fibonacci function definition.
Returns t if fib function is defined and appears to be correct."
  (let ((scratch-content (with-current-buffer "*scratch*"
                           (buffer-string))))
    ;; Check for defun fib in some form
    (and (string-match-p "defun.*fib" scratch-content)
         ;; Also verify it has some recursion or base case logic
         (or (string-match-p "(fib " scratch-content)  ; recursive call
             (string-match-p "<=" scratch-content)      ; base case comparison
             (string-match-p "zerop" scratch-content)))))

(defun test-fib--fib-works ()
  "Check if fib(10) evaluates to 55.
Returns t if the function works correctly."
  (condition-case err
      (let ((result (eval (read "(fib 10)") t)))
        (eq result 55))
    (error
     (message "Error evaluating fib(10): %s" (error-message-string err))
     nil)))

(defun test-fib--clear-scratch ()
  "Clear *scratch* buffer and remove any fib function."
  (with-current-buffer "*scratch*"
    (erase-buffer)
    (insert ";; This buffer is for text that is not saved.\n")
    (insert ";; Test will write fibonacci here.\n\n"))
  ;; Remove any previously defined fib function
  (fmakunbound 'fib))

(defun test-fib--run ()
  "Run the fibonacci multi-turn scenario test.
This is a REAL test that burns tokens."
  (interactive)
  (message "")
  (message "========================================")
  (message "🧪 FIBONACCI SCENARIO TEST")
  (message "========================================")
  (message "⚠️  This test burns tokens!")
  (message "")

  ;; Setup
  (test-fib--clear-scratch)
  (message "✅ Step 0: Cleared *scratch* buffer")

  ;; Start fresh chat
  (efrit-chat)
  (message "✅ Step 1: Started efrit-chat")

  ;; Phase 1: Ask Claude to write fibonacci
  (message "")
  (message "📝 Phase 1: Requesting fibonacci function...")
  (efrit-send-message "Write a fibonacci function called 'fib' in the *scratch* buffer. Use (defun fib (n) ...) format. After writing, evaluate the buffer to define the function.")

  ;; Wait for response
  (if (test-fib--wait-for-response)
      (progn
        (message "✅ Phase 1: Got response")
        ;; Check if fib was written to scratch
        (if (test-fib--scratch-contains-fib)
            (message "✅ Phase 1: Fibonacci code found in *scratch*")
          (progn
            (message "❌ Phase 1: No fibonacci code in *scratch*")
            (message "   *scratch* contents:")
            (message "%s" (with-current-buffer "*scratch*" (buffer-string)))
            (error "Test failed: No fibonacci code written"))))
    (error "Test failed: Timeout waiting for Phase 1 response"))

  ;; Check if function is defined
  (if (fboundp 'fib)
      (message "✅ Phase 1: Function 'fib' is defined")
    (progn
      (message "⚠️  Phase 1: Function 'fib' not yet defined, will ask Claude to evaluate")
      ;; Function not defined, need to ask Claude to evaluate
      (efrit-send-message "The function is not yet defined. Please evaluate the buffer to define the fib function.")
      (unless (test-fib--wait-for-response)
        (error "Test failed: Timeout waiting for evaluation"))
      (if (fboundp 'fib)
          (message "✅ Phase 1: Function 'fib' now defined after evaluation request")
        (progn
          (message "❌ Phase 1: Function 'fib' still not defined")
          (error "Test failed: fib function not defined")))))

  ;; Phase 2: Ask Claude to evaluate fib(10)
  (message "")
  (message "🔢 Phase 2: Requesting fib(10) evaluation...")
  (efrit-send-message "Now evaluate (fib 10) and tell me the result.")

  ;; Wait for response
  (if (test-fib--wait-for-response)
      (progn
        (message "✅ Phase 2: Got response")
        ;; Verify fib(10) = 55 by checking ourselves
        (if (test-fib--fib-works)
            (message "✅ Phase 2: fib(10) = 55 - CORRECT!")
          (progn
            (message "❌ Phase 2: fib(10) does not return 55")
            (let ((actual (condition-case err
                             (eval (read "(fib 10)") t)
                           (error (format "Error: %s" (error-message-string err))))))
              (message "   Actual result: %s" actual))
            (error "Test failed: fib(10) != 55"))))
    (error "Test failed: Timeout waiting for Phase 2 response"))

  ;; Success
  (message "")
  (message "========================================")
  (message "🎉 TEST PASSED!")
  (message "========================================")
  (message "")
  (message "Chat buffer contents:")
  (message "--- START ---")
  (with-current-buffer "*efrit-chat*"
    (message "%s" (buffer-string)))
  (message "--- END ---"))

;; Run the test
(condition-case err
    (test-fib--run)
  (error
   (message "")
   (message "========================================")
   (message "❌ TEST FAILED: %s" (error-message-string err))
   (message "========================================")
   (message "")
   (message "Chat buffer at failure:")
   (message "--- START ---")
   (when (get-buffer "*efrit-chat*")
     (with-current-buffer "*efrit-chat*"
       (message "%s" (buffer-string))))
   (message "--- END ---")
   (message "")
   (message "*scratch* buffer at failure:")
   (message "--- START ---")
   (with-current-buffer "*scratch*"
     (message "%s" (buffer-string)))
   (message "--- END ---")
   (kill-emacs 1)))

(kill-emacs 0)

;;; test-fibonacci-scenario.el ends here
