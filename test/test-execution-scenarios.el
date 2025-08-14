;;; test-execution-scenarios.el --- Execution engine tests for complex efrit scenarios -*- lexical-binding: t -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 1.0.0
;; Keywords: ai, tools, testing, execution

;;; Commentary:
;;
;; Tests for the efrit elisp execution engine and local functionality.
;; These tests do NOT make API calls to Claude - they test the execution layer:
;; - Multi-buffer operations via direct elisp execution
;; - Complex elisp execution chains
;; - History and context management (local state)
;; - Error handling and recovery in elisp evaluation
;; - Multi-turn conversation state management (no API calls)

;;; Code:

(message "=== Starting Efrit Execution Engine Tests ===")
(message "NOTE: These tests validate elisp execution, NOT API integration")

;; Set up environment
(add-to-list 'load-path ".")

;; Load all modules
(require 'efrit-tools)
(require 'efrit-multi-turn)
(require 'efrit-chat)
(require 'efrit-do)

;; Test tracking
(defvar efrit-test-results nil
  "List of test results.")

(defun efrit-test-record (test-name success-p message)
  "Record a test result."
  (push (list test-name success-p message) efrit-test-results)
  (message "%s %s: %s" 
           (if success-p "✅" "❌") 
           test-name 
           message))

(defun efrit-test-summary ()
  "Print test summary."
  (let ((total (length efrit-test-results))
        (passed (length (cl-remove-if-not #'cadr efrit-test-results))))
    (message "\n=== Test Summary ===")
    (message "Total: %d, Passed: %d, Failed: %d" 
             total passed (- total passed))
    (when (< passed total)
      (message "\nFailures:")
      (dolist (result (cl-remove-if #'cadr efrit-test-results))
        (message "  ❌ %s: %s" (car result) (caddr result))))
    (= passed total)))

;;; Multi-Buffer Operation Tests (Execution Engine)

(message "\n🖥️ Testing Multi-Buffer Operations (Local Elisp Execution)...")

;; Test 1: Create and populate multiple buffers from single elisp command
;; NOTE: This tests efrit-tools-eval-sexp, not Claude API integration
(condition-case err
    (let* ((buffer1-name "*efrit-test-buffer1*")
           (buffer2-name "*efrit-test-buffer2*")
           (complex-command 
            (format "(progn 
                       (with-current-buffer (get-buffer-create \"%s\")
                         (erase-buffer)
                         (insert \"Content for buffer 1\\nLine 2\\nLine 3\"))
                       (with-current-buffer (get-buffer-create \"%s\")
                         (erase-buffer)
                         (insert \"Content for buffer 2\\nDifferent content\"))
                       (list (buffer-name (get-buffer \"%s\"))
                             (buffer-name (get-buffer \"%s\"))))"
                    buffer1-name buffer2-name buffer1-name buffer2-name))
           (result (efrit-tools-eval-sexp complex-command)))
      
      ;; Verify both buffers were created and populated
      (let ((buf1 (get-buffer buffer1-name))
            (buf2 (get-buffer buffer2-name)))
        (if (and buf1 buf2
                 (with-current-buffer buf1 
                   (string-match-p "Content for buffer 1" (buffer-string)))
                 (with-current-buffer buf2
                   (string-match-p "Content for buffer 2" (buffer-string))))
            (efrit-test-record "Multi-Buffer Creation" t 
                               (format "Created and populated two buffers: %s" result))
          (efrit-test-record "Multi-Buffer Creation" nil 
                             "Failed to create or populate buffers correctly"))
        
        ;; Clean up
        (when buf1 (kill-buffer buf1))
        (when buf2 (kill-buffer buf2))))
  (error 
   (efrit-test-record "Multi-Buffer Creation" nil 
                      (format "Error: %s" (error-message-string err)))))

;; Test 2: Window splitting and buffer display
(condition-case err
    (let* ((original-windows (length (window-list)))
           (split-command "(progn 
                             (delete-other-windows)
                             (split-window-horizontally)
                             (other-window 1)
                             (switch-to-buffer \"*scratch*\")
                             (other-window 1)
                             (switch-to-buffer \"*Messages*\")
                             (length (window-list)))")
           (result (efrit-tools-eval-sexp split-command))
           (new-windows (string-to-number result)))
      
      (if (= new-windows 2)
          (efrit-test-record "Window Operations" t 
                             (format "Successfully split windows: %d total" new-windows))
        (efrit-test-record "Window Operations" nil 
                           (format "Window split failed: %d windows" new-windows)))
      
      ;; Restore original window configuration
      (delete-other-windows))
  (error 
   (efrit-test-record "Window Operations" nil 
                      (format "Error: %s" (error-message-string err)))))

;;; Complex Elisp Execution Tests

(message "\n🔧 Testing Complex Elisp Execution...")

;; Test 3: Nested function calls with data transformation
(condition-case err
    (let* ((data-processing-command 
            "(let ((data '((\"Alice\" 25 \"Engineer\")
                          (\"Bob\" 30 \"Designer\")
                          (\"Charlie\" 35 \"Manager\"))))
               (mapcar (lambda (person)
                         (list (upcase (car person))
                               (* (cadr person) 12)  ; months
                               (concat (caddr person) \" Role\")))
                       data))")
           (result (efrit-tools-eval-sexp data-processing-command)))
      
      (if (string-match-p "ALICE.*300.*Engineer Role" result)
          (efrit-test-record "Data Processing" t 
                             "Complex data transformation successful")
        (efrit-test-record "Data Processing" nil 
                           (format "Data processing failed: %s" result))))
  (error 
   (efrit-test-record "Data Processing" nil 
                      (format "Error: %s" (error-message-string err)))))

;; Test 4: File system operations
(condition-case err
    (let* ((temp-file (make-temp-file "efrit-test"))
           (file-ops-command 
            (format "(progn
                       (with-temp-file \"%s\"
                         (insert \"Test content\\nLine 2\\nLine 3\"))
                       (with-temp-buffer
                         (insert-file-contents \"%s\")
                         (goto-char (point-min))
                         (forward-line 1)
                         (buffer-substring (point) (point-at-eol))))"
                    temp-file temp-file))
           (result (efrit-tools-eval-sexp file-ops-command)))
      
      (if (string-match-p "Line 2" result)
          (efrit-test-record "File Operations" t 
                             "File write/read operations successful")
        (efrit-test-record "File Operations" nil 
                           (format "File operations failed: %s" result)))
      
      ;; Clean up
      (when (file-exists-p temp-file)
        (delete-file temp-file)))
  (error 
   (efrit-test-record "File Operations" nil 
                      (format "Error: %s" (error-message-string err)))))

;;; History and Context Management Tests

(message "\n📚 Testing History and Context Management...")

;; Test 5: efrit-do history functionality
(condition-case err
    (let ((original-history efrit-do-history))
      ;; Clear history first
      (setq efrit-do-history nil)
      
      ;; Simulate some history
      (add-to-history 'efrit-do-history "test command 1")
      (add-to-history 'efrit-do-history "test command 2")
      (add-to-history 'efrit-do-history "test command 3")
      
      (if (and (= (length efrit-do-history) 3)
               (string= (car efrit-do-history) "test command 3"))
          (efrit-test-record "History Management" t 
                             "History tracking works correctly")
        (efrit-test-record "History Management" nil 
                           (format "History tracking failed: %s" efrit-do-history)))
      
      ;; Test history clearing
      (efrit-do-clear-history)
      (if (null efrit-do-history)
          (efrit-test-record "History Clearing" t 
                             "History clearing works correctly")
        (efrit-test-record "History Clearing" nil 
                           "History clearing failed"))
      
      ;; Restore original history
      (setq efrit-do-history original-history))
  (error 
   (efrit-test-record "History Management" nil 
                      (format "Error: %s" (error-message-string err)))))

;; Test 6: Context ring operations
(condition-case err
    (let ((original-ring efrit-do--context-ring))
      ;; Initialize fresh context ring
      (efrit-do--clear-context)
      
      ;; Add some context items
      (efrit-do--capture-context "command 1" "result 1")
      (efrit-do--capture-context "command 2" "result 2")
      
      (let ((items (efrit-do--get-context-items 2)))
        (if (= (length items) 2)
            (efrit-test-record "Context Ring" t 
                               "Context ring operations work correctly")
          (efrit-test-record "Context Ring" nil 
                             (format "Context ring failed: %d items" (length items)))))
      
      ;; Restore original ring
      (setq efrit-do--context-ring original-ring))
  (error 
   (efrit-test-record "Context Ring" nil 
                      (format "Error: %s" (error-message-string err)))))

;;; Multi-Turn Conversation Tests

(message "\n🔄 Testing Multi-Turn Conversations...")

;; Test 7: Conversation lifecycle
(condition-case err
    (let* ((conv (efrit--create-conversation "test multi-step request"))
           (conv-id (efrit-conversation-id conv)))
      
      ;; Test conversation creation
      (if (and conv (efrit-conversation-p conv))
          (efrit-test-record "Conversation Creation" t 
                             (format "Created conversation: %s" conv-id))
        (efrit-test-record "Conversation Creation" nil 
                           "Failed to create conversation"))
      
      ;; Test conversation advancement
      (efrit--advance-conversation-turn conv)
      (if (= (efrit-conversation-current-turn conv) 2)
          (efrit-test-record "Conversation Advancement" t 
                             "Turn advancement works correctly")
        (efrit-test-record "Conversation Advancement" nil 
                           "Turn advancement failed"))
      
      ;; Test history addition
      (efrit--add-to-conversation-history conv "test request" "test response")
      (let ((history (efrit-conversation-context-history conv)))
        (if (and history (= (length history) 1))
            (efrit-test-record "Conversation History" t 
                               "History tracking works correctly")
          (efrit-test-record "Conversation History" nil 
                             "History tracking failed")))
      
      ;; Clean up
      (remhash conv-id efrit--multi-turn-conversations))
  (error 
   (efrit-test-record "Multi-Turn Conversations" nil 
                      (format "Error: %s" (error-message-string err)))))

;;; Tool Extraction and Response Processing Tests

(message "\n🛠️ Testing Tool Extraction and Response Processing...")

;; Test 8: Complex tool extraction
(condition-case err
    (let* ((complex-response 
            "I'll help you with that. First, let me create a buffer:
<elisp>(with-current-buffer (get-buffer-create \"*test-output*\")
  (erase-buffer)
  (insert \"Hello World\\n\")
  (buffer-name))</elisp>

Now let me add more content:
<elisp>(with-current-buffer \"*test-output*\"
  (goto-char (point-max))
  (insert \"Additional line\\n\")
  (buffer-size))</elisp>

Done!")
           (extracted (efrit-tools-extract-tools-from-response complex-response)))
      
      (if (and extracted 
               (string-match-p "test-output" (car extracted))
               (string-match-p "28" (car extracted)))  ; Buffer size after adding content
          (efrit-test-record "Complex Tool Extraction" t 
                             "Multi-tool extraction successful")
        (efrit-test-record "Complex Tool Extraction" nil 
                           (format "Tool extraction failed: %s" (car extracted))))
      
      ;; Clean up test buffer
      (when-let* ((buf (get-buffer "*test-output*")))
        (kill-buffer buf)))
  (error 
   (efrit-test-record "Complex Tool Extraction" nil 
                      (format "Error: %s" (error-message-string err)))))

;;; Error Handling and Recovery Tests

(message "\n⚠️ Testing Error Handling and Recovery...")

;; Test 9: Graceful error handling
(condition-case err
    (let* ((bad-elisp "(/ 1 0)")  ; Division by zero
           (result (efrit-tools-eval-sexp bad-elisp)))
      
      (if (string-match-p "error\\|Error\\|ERROR" result)
          (efrit-test-record "Error Handling" t 
                             "Graceful error handling works")
        (efrit-test-record "Error Handling" nil 
                           (format "Error handling failed: %s" result))))
  (error 
   (efrit-test-record "Error Handling" nil 
                      (format "Error in error test: %s" (error-message-string err)))))

;; Test 10: Invalid elisp syntax
(condition-case err
    (let* ((invalid-elisp "(defun incomplete-function")  ; Unclosed parenthesis
           (result (efrit-tools-eval-sexp invalid-elisp)))
      
      (if (string-match-p "error\\|Error\\|ERROR\\|invalid\\|Invalid" result)
          (efrit-test-record "Syntax Error Handling" t 
                             "Syntax error handling works")
        (efrit-test-record "Syntax Error Handling" nil 
                           (format "Syntax error handling failed: %s" result))))
  (error 
   (efrit-test-record "Syntax Error Handling" nil 
                      (format "Error in syntax test: %s" (error-message-string err)))))

;;; Performance and Edge Cases

(message "\n🚀 Testing Performance and Edge Cases...")

;; Test 11: Large data processing
(condition-case err
    (let* ((large-data-command 
            "(let ((large-list (number-sequence 1 1000)))
               (length (mapcar (lambda (x) (* x x)) large-list)))")
           (result (efrit-tools-eval-sexp large-data-command)))
      
      (if (string= result "1000")
          (efrit-test-record "Large Data Processing" t 
                             "Large data processing successful")
        (efrit-test-record "Large Data Processing" nil 
                           (format "Large data processing failed: %s" result))))
  (error 
   (efrit-test-record "Large Data Processing" nil 
                      (format "Error: %s" (error-message-string err)))))

;; Test 12: Empty and edge case inputs
(condition-case err
    (let* ((edge-cases '("nil" 
                        "'()" 
                        "\"\""
                        "(list)"))
           (all-passed t))
      
      (dolist (case edge-cases)
        (let ((result (efrit-tools-eval-sexp case)))
          (unless result  ; Any result is acceptable for edge cases
            (setq all-passed nil))))
      
      (efrit-test-record "Edge Case Handling" all-passed 
                         (if all-passed 
                             "All edge cases handled gracefully"
                           "Some edge cases failed")))
  (error 
   (efrit-test-record "Edge Case Handling" nil 
                      (format "Error: %s" (error-message-string err)))))

;;; Syntax Validation Tests

(message "\n🔍 Testing Elisp Syntax Validation...")

;; Test 13: Valid elisp syntax
(condition-case err
    (let ((validation (efrit-do--validate-elisp "(+ 1 2)")))
      (if (car validation)
          (efrit-test-record "Valid Elisp Validation" t 
                             "Valid elisp correctly identified")
        (efrit-test-record "Valid Elisp Validation" nil 
                           (format "Valid elisp rejected: %s" (cdr validation)))))
  (error 
   (efrit-test-record "Valid Elisp Validation" nil 
                      (format "Error: %s" (error-message-string err)))))

;; Test 14: Invalid elisp syntax - missing paren
(condition-case err
    (let ((validation (efrit-do--validate-elisp "(+ 1 2")))
      (if (not (car validation))
          (efrit-test-record "Invalid Elisp Detection" t 
                             "Invalid elisp correctly caught")
        (efrit-test-record "Invalid Elisp Detection" nil 
                           "Invalid elisp not detected")))
  (error 
   (efrit-test-record "Invalid Elisp Detection" nil 
                      (format "Error: %s" (error-message-string err)))))

;; Test 15: Original wyvern buffer bug case  
(condition-case err
    (let ((validation (efrit-do--validate-elisp 
                       "(length (seq-filter (lambda (buf) (string-match-p \"wyvern\" (buffer-name buf) t)) (buffer-list)))")))
      (if (car validation)
          (efrit-test-record "Wyvern Buffer Bug Case" t 
                             "Original bug case passes validation (syntax is valid)")
        (efrit-test-record "Wyvern Buffer Bug Case" nil 
                           (format "Original bug case failed validation: %s" (cdr validation)))))
  (error 
   (efrit-test-record "Wyvern Buffer Bug Case" nil 
                      (format "Error: %s" (error-message-string err)))))

;; Test 16: Validation integration in execution flow
(condition-case err
    (let* ((tool-item (make-hash-table :test 'equal)))
      (puthash "name" "eval_sexp" tool-item)
      (puthash "input" "(+ 1 2" tool-item)  ; Missing closing paren
      (let ((result (efrit-do--execute-tool tool-item)))
        (if (string-match-p "Syntax Error" result)
            (efrit-test-record "Execution Flow Integration" t 
                               "Syntax errors caught in execution flow")
          (efrit-test-record "Execution Flow Integration" nil 
                             (format "Syntax error not caught: %s" result)))))
  (error 
   (efrit-test-record "Execution Flow Integration" nil 
                      (format "Error: %s" (error-message-string err)))))

;;; Final Summary

(message "\n" (make-string 50 ?=))
(let ((success (efrit-test-summary)))
  (if success
      (progn
        (message "\n🎉 ALL EXECUTION ENGINE TESTS PASSED!")
        (message "   ✅ Multi-buffer operations working")
        (message "   ✅ Complex elisp execution working") 
        (message "   ✅ History and context management working")
        (message "   ✅ Multi-turn conversation state working")
        (message "   ✅ Tool extraction working")
        (message "   ✅ Error handling working")
        (message "   ✅ Performance tests passing")
        (message "\n🚀 efrit execution engine is robust and ready!"))
    (progn
      (message "\n❌ SOME INTEGRATION TESTS FAILED!")
      (message "   Please review the failures above.")
      (kill-emacs 1))))

(provide 'test-execution-scenarios)

;;; test-execution-scenarios.el ends here
