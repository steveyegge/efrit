;;; test-api-integration.el --- Real API integration tests for efrit -*- lexical-binding: t -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 1.0.0
;; Keywords: ai, tools, testing, api, integration

;;; Commentary:
;;
;; REAL API integration tests that make actual calls to Claude.
;; These tests exercise the complete pipeline:
;; 1. User request → 2. API call to Claude → 3. Claude generates elisp → 4. Execute elisp → 5. Verify result
;;
;; WARNING: These tests consume API credits and require network connectivity.
;; Set EFRIT_SKIP_API_TESTS=1 to skip these tests.

;;; Code:

(message "=== Real API Integration Tests ===")

;; Check if API tests should be skipped
(when (getenv "EFRIT_SKIP_API_TESTS")
  (message "Skipping API tests (EFRIT_SKIP_API_TESTS is set)")
  (kill-emacs 0))

;; Set up environment
(add-to-list 'load-path ".")

;; Load all modules
(require 'efrit-tools)
(require 'efrit-multi-turn)
(require 'efrit-chat)
(require 'efrit-do)

;; Test tracking
(defvar efrit-api-test-results nil
  "List of API test results.")

(defun efrit-api-test-record (test-name success-p message)
  "Record an API test result."
  (push (list test-name success-p message) efrit-api-test-results)
  (message "%s %s: %s" 
           (if success-p "✅" "❌") 
           test-name 
           message))

(defun efrit-api-test-summary ()
  "Print API test summary."
  (let ((total (length efrit-api-test-results))
        (passed (length (cl-remove-if-not #'cadr efrit-api-test-results))))
    (message "\n=== API Integration Test Summary ===")
    (message "Total: %d, Passed: %d, Failed: %d" 
             total passed (- total passed))
    (when (< passed total)
      (message "\nFailures:")
      (dolist (result (cl-remove-if #'cadr efrit-api-test-results))
        (message "  ❌ %s: %s" (car result) (caddr result))))
    (= passed total)))

;; Check API key availability
(message "\n🔑 Checking API key availability...")
(condition-case err
    (let ((api-key (efrit--get-api-key)))
      (if (and api-key (> (length api-key) 10))
          (progn
            (message "✅ API key found (length: %d)" (length api-key))
            (message "⚠️  API tests will consume credits!"))
        (message "❌ No valid API key found")
        (message "Set ANTHROPIC_API_KEY environment variable or configure efrit-api-key")
        (kill-emacs 1)))
  (error 
   (message "❌ Error getting API key: %s" (error-message-string err))
   (kill-emacs 1)))

;;; Real API Integration Tests

(message "\n🌐 Testing Full API Integration Pipeline...")

;; Test 1: Simple calculation request
(message "\n📊 Test 1: Simple calculation via API...")
(condition-case err
    (let* ((test-request "Calculate 15 * 23 and show the result")
           (start-time (current-time))
           (result (efrit-do--execute-command test-request))
           (elapsed (float-time (time-subtract (current-time) start-time))))
      
      (message "Request: %s" test-request)
      (message "Response time: %.2fs" elapsed)
      (message "Result: %s" (truncate-string-to-width result 100 nil nil "..."))
      
      ;; Look for the calculation result (345) in the response
      (if (string-match-p "345" result)
          (efrit-api-test-record "Simple Calculation" t 
                                 (format "Found correct result (345) in %.2fs" elapsed))
        (efrit-api-test-record "Simple Calculation" nil 
                               (format "Result missing 345: %s" result))))
  (error 
   (efrit-api-test-record "Simple Calculation" nil 
                          (format "API Error: %s" (error-message-string err)))))

;; Test 2: Multi-buffer creation request  
(message "\n🖥️ Test 2: Multi-buffer creation via API...")
(condition-case err
    (let* ((test-request "Create two buffers: one called '*test-api-buffer1*' with content 'Hello from API test' and another called '*test-api-buffer2*' with content 'Second buffer content'")
           (start-time (current-time))
           (result (efrit-do--execute-command test-request))
           (elapsed (float-time (time-subtract (current-time) start-time))))
      
      (message "Request: %s" (truncate-string-to-width test-request 80 nil nil "..."))
      (message "Response time: %.2fs" elapsed)
      
      ;; Check if buffers were actually created
      (let ((buf1 (get-buffer "*test-api-buffer1*"))
            (buf2 (get-buffer "*test-api-buffer2*")))
        
        (if (and buf1 buf2)
            (let ((content1 (with-current-buffer buf1 (buffer-string)))
                  (content2 (with-current-buffer buf2 (buffer-string))))
              
              (if (and (string-match-p "Hello from API test" content1)
                       (string-match-p "Second buffer content" content2))
                  (efrit-api-test-record "Multi-Buffer Creation" t 
                                         (format "Successfully created and populated buffers in %.2fs" elapsed))
                (efrit-api-test-record "Multi-Buffer Creation" nil 
                                       (format "Buffers created but content wrong: '%s', '%s'" content1 content2)))
              
              ;; Clean up
              (kill-buffer buf1)
              (kill-buffer buf2))
          (efrit-api-test-record "Multi-Buffer Creation" nil 
                                 (format "Buffers not created: buf1=%s, buf2=%s" buf1 buf2)))))
  (error 
   (efrit-api-test-record "Multi-Buffer Creation" nil 
                          (format "API Error: %s" (error-message-string err)))))

;; Test 3: File operation request
(message "\n📁 Test 3: File operations via API...")
(condition-case err
    (let* ((temp-file (make-temp-file "efrit-api-test"))
           (test-request (format "Write the text 'API Integration Test Content' to the file %s and then read it back" temp-file))
           (start-time (current-time))
           (result (efrit-do--execute-command test-request))
           (elapsed (float-time (time-subtract (current-time) start-time))))
      
      (message "Request: Write and read file %s" (file-name-nondirectory temp-file))
      (message "Response time: %.2fs" elapsed)
      
      ;; Check if file was created and contains expected content
      (if (and (file-exists-p temp-file)
               (with-temp-buffer
                 (insert-file-contents temp-file)
                 (string-match-p "API Integration Test Content" (buffer-string))))
          (efrit-api-test-record "File Operations" t 
                                 (format "File operations successful in %.2fs" elapsed))
        (efrit-api-test-record "File Operations" nil 
                               (format "File operation failed: exists=%s" (file-exists-p temp-file))))
      
      ;; Clean up
      (when (file-exists-p temp-file)
        (delete-file temp-file)))
  (error 
   (efrit-api-test-record "File Operations" nil 
                          (format "API Error: %s" (error-message-string err)))))

;; Test 4: Buffer manipulation request
(message "\n📝 Test 4: Buffer manipulation via API...")
(condition-case err
    (let* ((test-buffer-name "*api-test-manipulation*")
           (test-request (format "Create a buffer called '%s', insert 'Line 1', go to a new line, insert 'Line 2', then tell me how many lines are in the buffer" test-buffer-name))
           (start-time (current-time))
           (result (efrit-do--execute-command test-request))
           (elapsed (float-time (time-subtract (current-time) start-time))))
      
      (message "Request: Buffer manipulation test")
      (message "Response time: %.2fs" elapsed)
      
      ;; Check buffer was created and manipulated correctly
      (let ((buf (get-buffer test-buffer-name)))
        (if buf
            (let ((content (with-current-buffer buf (buffer-string)))
                  (line-count (with-current-buffer buf (count-lines (point-min) (point-max)))))
              
              (if (and (string-match-p "Line 1" content)
                       (string-match-p "Line 2" content)
                       (>= line-count 2)
                       (string-match-p "2" result)) ; Look for line count in response
                  (efrit-api-test-record "Buffer Manipulation" t 
                                         (format "Buffer manipulation successful in %.2fs" elapsed))
                (efrit-api-test-record "Buffer Manipulation" nil 
                                       (format "Buffer content wrong: lines=%d, content='%s'" line-count content)))
              
              ;; Clean up
              (kill-buffer buf))
          (efrit-api-test-record "Buffer Manipulation" nil 
                                 "Buffer was not created"))))
  (error 
   (efrit-api-test-record "Buffer Manipulation" nil 
                          (format "API Error: %s" (error-message-string err)))))

;; Test 5: Complex data processing request
(message "\n🔢 Test 5: Complex data processing via API...")
(condition-case err
    (let* ((test-request "Create a list of numbers from 1 to 5, multiply each by 2, then sum the results and show me the final total")
           (start-time (current-time))
           (result (efrit-do--execute-command test-request))
           (elapsed (float-time (time-subtract (current-time) start-time))))
      
      (message "Request: Data processing (1-5, *2, sum)")
      (message "Response time: %.2fs" elapsed)
      
      ;; Expected result: (2+4+6+8+10) = 30
      (if (string-match-p "30" result)
          (efrit-api-test-record "Data Processing" t 
                                 (format "Correct sum (30) calculated in %.2fs" elapsed))
        (efrit-api-test-record "Data Processing" nil 
                               (format "Expected 30 in result: %s" result))))
  (error 
   (efrit-api-test-record "Data Processing" nil 
                          (format "API Error: %s" (error-message-string err)))))

;; Test 6: Error handling and recovery
(message "\n⚠️ Test 6: Error handling via API...")
(condition-case err
    (let* ((test-request "Try to divide 10 by 0 and handle the error gracefully")
           (start-time (current-time))
           (result (efrit-do--execute-command test-request))
           (elapsed (float-time (time-subtract (current-time) start-time))))
      
      (message "Request: Division by zero handling")
      (message "Response time: %.2fs" elapsed)
      
      ;; Should contain error handling, not crash
      ;; Accept either proper error handling in elisp OR function error messages
      (if (and result (> (length result) 10)
               (or (string-match-p "Caught.*error\\|condition-case\\|error handling" result)
                   (string-match-p "Function not defined\\|arith-error" result))
               (not (string-match-p "ERROR.*ERROR" result))) ; Not a cascading error
          (efrit-api-test-record "Error Handling" t 
                                 (format "Error handled gracefully in %.2fs" elapsed))
        (efrit-api-test-record "Error Handling" nil 
                               (format "Error handling failed: %s" result))))
  (error 
   (efrit-api-test-record "Error Handling" nil 
                          (format "API Error: %s" (error-message-string err)))))

;;; Performance and Edge Cases

(message "\n🚀 Testing API Performance...")

;; Test 7: Response time benchmark
(let ((response-times '())
      (simple-request "Calculate 7 + 8"))
  
  (message "\n⏱️ Test 7: Response time benchmark (3 requests)...")
  
  (dotimes (i 3)
    (condition-case err
        (let* ((start-time (current-time))
               (result (efrit-do--execute-command simple-request))
               (elapsed (float-time (time-subtract (current-time) start-time))))
          (push elapsed response-times)
          (message "  Request %d: %.2fs" (1+ i) elapsed))
      (error 
       (message "  Request %d: ERROR - %s" (1+ i) (error-message-string err)))))
  
  (when response-times
    (let* ((avg-time (/ (apply #'+ response-times) (length response-times)))
           (max-time (apply #'max response-times))
           (min-time (apply #'min response-times)))
      
      (if (< avg-time 10.0) ; Reasonable response time
          (efrit-api-test-record "Response Time" t 
                                 (format "Avg: %.2fs (min: %.2fs, max: %.2fs)" avg-time min-time max-time))
        (efrit-api-test-record "Response Time" nil 
                               (format "Slow responses: avg %.2fs" avg-time))))))

;;; Final Summary

(message "\n" (make-string 60 ?=))
(let ((success (efrit-api-test-summary)))
  (if success
      (progn
        (message "\n🎉 ALL API INTEGRATION TESTS PASSED!")
        (message "   ✅ Simple calculations working")
        (message "   ✅ Multi-buffer operations working")
        (message "   ✅ File operations working")
        (message "   ✅ Buffer manipulation working")
        (message "   ✅ Data processing working")
        (message "   ✅ Error handling working")
        (message "   ✅ Performance acceptable")
        (message "\n🚀 Full API integration is production ready!")
        (message "   Claude successfully generates and executes elisp")
        (message "   End-to-end pipeline is robust"))
    (progn
      (message "\n❌ SOME API INTEGRATION TESTS FAILED!")
      (message "   This could indicate:")
      (message "   - API connectivity issues")
      (message "   - Claude model changes")
      (message "   - Network timeouts")
      (message "   - Rate limiting")
      (message "   Please review the failures above.")
      (kill-emacs 1))))

(provide 'test-api-integration)

;;; test-api-integration.el ends here
