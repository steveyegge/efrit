#!/usr/bin/env emacs --script
;;; test-chat-retry-integration.el --- Test enhanced efrit-chat retry functionality -*- lexical-binding: t; -*-

;; Test the integration of efrit-do's retry logic into efrit-chat

(setq debug-on-error t)
(setq max-lisp-eval-depth 3000)

;; Add the current directory to load path
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))

;; Load required modules
(require 'efrit-chat)
(require 'efrit-debug)

(defvar test-passed 0)
(defvar test-failed 0)

(defun test-assert (condition message)
  "Assert CONDITION is true, report MESSAGE on failure."
  (if condition
      (progn
        (message "✅ %s" message)
        (setq test-passed (1+ test-passed)))
    (progn
      (message "❌ %s" message)
      (setq test-failed (1+ test-failed)))))

(defun test-summary ()
  "Print test summary and exit."
  (message "\n=== Test Summary ===")
  (message "Total: %d, Passed: %d, Failed: %d" 
           (+ test-passed test-failed) test-passed test-failed)
  (if (> test-failed 0)
      (progn
        (message "❌ SOME TESTS FAILED!")
        (kill-emacs 1))
    (progn
      (message "🎉 ALL TESTS PASSED!")
      (kill-emacs 0))))

(message "=== Testing Enhanced Efrit-Chat Retry Integration ===")

;; Test 1: Basic retry configuration
(message "\n🔧 Testing retry configuration...")
(test-assert (numberp efrit-max-retries) "efrit-max-retries is a number")
(test-assert (= efrit-max-retries 3) "efrit-max-retries defaults to 3")
(test-assert (eq efrit-retry-on-errors t) "efrit-retry-on-errors defaults to t")

;; Test 2: Error extraction functions
(message "\n🔍 Testing error extraction functions...")
(let ((error-result "[Result: Error executing (undefined-function): Symbol's function definition is void: undefined-function]"))
  (let ((error-info (efrit--extract-error-info error-result)))
    (test-assert (car error-info) "Error correctly detected in tool result")
    (test-assert (string-match-p "Error" (cdr error-info)) "Error message extracted")))

(let ((no-error-result "[Result: 42]"))
  (let ((error-info (efrit--extract-error-info no-error-result)))
    (test-assert (not (car error-info)) "No error detected in successful result")))

;; Test 3: Context building function
(message "\n📋 Testing context building...")
(let ((context (efrit--build-error-context)))
  (test-assert (stringp context) "Context is a string")
  (test-assert (> (length context) 50) "Context contains meaningful information")
  (test-assert (string-match-p "CURRENT BUFFER" context) "Context includes buffer information"))

;; Test 4: System prompt generation with retry
(message "\n📝 Testing system prompt generation...")
(let ((basic-prompt (efrit--generate-system-prompt)))
  (test-assert (stringp basic-prompt) "Basic prompt is a string")
  (test-assert (> (length basic-prompt) 100) "Basic prompt has content"))

(let ((retry-prompt (efrit--generate-system-prompt 1 "Test error" "(+ 1 'invalid)")))
  (test-assert (stringp retry-prompt) "Retry prompt is a string")
  (test-assert (string-match-p "RETRY ATTEMPT 1" retry-prompt) "Retry prompt includes retry information")
  (test-assert (string-match-p "Test error" retry-prompt) "Retry prompt includes error message")
  (test-assert (string-match-p "CURRENT EMACS STATE" retry-prompt) "Retry prompt includes context"))

;; Test 5: Chat buffer setup with retry variables
(message "\n💬 Testing enhanced chat buffer setup...")
(with-current-buffer (efrit--setup-buffer)
  (test-assert (local-variable-p 'efrit--retry-count) "Retry count is buffer-local")
  (test-assert (= efrit--retry-count 0) "Retry count initializes to 0")
  (test-assert (boundp 'efrit--message-history) "Message history is set up")
  (test-assert (markerp efrit--conversation-marker) "Conversation marker is set up"))

;; Test 6: Tool execution with error detection
(message "\n🛠️ Testing tool execution error detection...")
;; Create mock tool results with errors
(let ((tool-results '("[Result: 42]" "[Result: Error: Division by zero]" "[Result: Success]")))
  (let ((has-errors (seq-some (lambda (result) 
                               (car (efrit--extract-error-info result))) 
                             tool-results)))
    (test-assert has-errors "Error detection works on tool results list")))

(let ((tool-results '("[Result: 42]" "[Result: Success]")))
  (let ((has-errors (seq-some (lambda (result) 
                               (car (efrit--extract-error-info result))) 
                             tool-results)))
    (test-assert (not has-errors) "No false positives in error detection")))

;; Test 7: Retry count management
(message "\n🔄 Testing retry count management...")
(with-current-buffer (efrit--setup-buffer)
  (setq-local efrit--retry-count 0)
  (test-assert (= efrit--retry-count 0) "Retry count starts at 0")
  
  ;; Simulate retry increment (would normally be done by efrit--handle-tool-retry)
  (setq-local efrit--retry-count (1+ efrit--retry-count))
  (test-assert (= efrit--retry-count 1) "Retry count increments correctly")
  
  ;; Reset should work
  (setq-local efrit--retry-count 0)
  (test-assert (= efrit--retry-count 0) "Retry count resets correctly"))

;; Test 8: Extract content and tools return format
(message "\n📦 Testing content extraction return format...")
;; Mock content array with text and tool use
(let* ((mock-content (vector 
                     (let ((text-item (make-hash-table :test 'equal)))
                       (puthash "type" "text" text-item)
                       (puthash "text" "Here's the result: " text-item)
                       text-item)
                     (let ((tool-item (make-hash-table :test 'equal)))
                       (puthash "type" "tool_use" tool-item)
                       (puthash "name" "eval_sexp" tool-item)
                       (let ((input (make-hash-table :test 'equal)))
                         (puthash "expr" "(+ 2 3)" input)
                         (puthash "input" input tool-item))
                       tool-item)))
       (result (efrit--extract-content-and-tools mock-content)))
  (test-assert (consp result) "Extract function returns a cons cell")
  (test-assert (stringp (car result)) "First element is message text")
  (test-assert (listp (cdr result)) "Second element is tool results list")
  (test-assert (string-match-p "Here's the result" (car result)) "Message text includes original text")
  (test-assert (string-match-p "Result:" (car result)) "Message text includes tool results"))

(message "\n🎯 Testing integration scenarios...")

;; Test 9: Chat initialization includes all retry components
(message "\n🚀 Testing full chat initialization...")
(let ((chat-buffer (efrit--setup-buffer)))
  (with-current-buffer chat-buffer
    (efrit-mode)  ; Ensure mode is set
    (test-assert (eq major-mode 'efrit-mode) "Chat buffer uses efrit-mode")
    (test-assert (local-variable-p 'efrit--retry-count) "Retry count is buffer-local in mode")
    (test-assert (= efrit--retry-count 0) "Retry count is initialized in mode")))

;; Test 10: API request function accepts retry parameters
(message "\n🌐 Testing API request retry parameters...")
(let* ((test-messages '(((role . "user") (content . "test"))))
       ;; We can't actually send API requests in tests, but we can check the function signature
       (api-function-exists (fboundp 'efrit--send-api-request)))
  (test-assert api-function-exists "API request function exists")
  ;; Check that function accepts optional parameters by examining its argument list
  (let ((func-args (help-function-arglist 'efrit--send-api-request)))
    (test-assert (member '&optional func-args) "API function accepts optional parameters")))

(message "\n🔧 Testing error handling integration...")

;; Test 11: Comprehensive error pattern matching
(let ((test-cases '(
                   ("[Result: Error executing (+ 1 'invalid): Wrong type argument: numberp, invalid]" . t)
                   ("[Result: 42]" . nil)
                   ("[Result: Success]" . nil)
                   ("[Result: Error: Division by zero]" . t)
                   ("API Error: Connection failed" . t)
                   ("Error: Something went wrong" . t)
                   ("Normal response text" . nil))))
  (dolist (test-case test-cases)
    (let* ((input (car test-case))
           (expected (cdr test-case))
           (actual (car (efrit--extract-error-info input))))
      (test-assert (eq actual expected) 
                  (format "Error detection for: %s" (substring input 0 (min 30 (length input))))))))

(message "\n🏁 Enhanced efrit-chat retry integration testing complete!")

(test-summary)
