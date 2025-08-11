;;; test-retry-forced.el --- Force retry scenarios to test logic -*- lexical-binding: t -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>  
;; Keywords: test, retry, debugging
;; Version: 1.0.0

;;; Commentary:
;; This file provides utilities for testing retry logic by simulating
;; various error conditions. Useful for debugging and verifying retry
;; behavior without needing to trigger real API errors.

(add-to-list 'load-path ".")
(require 'efrit-tools)
(require 'efrit-multi-turn) 
(require 'efrit-chat)
(require 'efrit-do)

;; Configure for testing
(setq efrit-do-debug t)
(setq efrit-do-max-retries 2)
(setq efrit-do-retry-on-errors t)

(message "=== Testing Retry Logic with Forced Errors ===")

;; Test 1: Simulate a syntax error response and see how retry handles it
(defun test-retry-with-syntax-error ()
  "Test retry logic by simulating a syntax error."
  (message "\n🧪 Test 1: Simulating syntax error scenario...")
  
  ;; Create a mock result that looks like a syntax error
  (let ((mock-error-result "[Syntax Error in (bad-function arg1 arg2): void-function bad-function]"))
    
    ;; Test error detection
    (let ((error-info (efrit-do--extract-error-info mock-error-result))
          (code-info (efrit-do--extract-executed-code mock-error-result)))
      
      (message "  Error detected: %s" (car error-info))
      (message "  Error message: %s" (cdr error-info))
      (message "  Extracted code: %s" code-info)
      
      ;; Test retry prompt generation
      (let ((retry-prompt (efrit-do--command-system-prompt 1 (cdr error-info) code-info)))
        (message "  Retry prompt length: %d" (length retry-prompt))
        (message "  Contains RETRY ATTEMPT: %s" (if (string-match-p "RETRY ATTEMPT" retry-prompt) "YES" "NO"))
        (message "  Contains error info: %s" (if (string-match-p "bad-function" retry-prompt) "YES" "NO"))))))

;; Test 2: Test the retry loop logic with mock functions
(defun test-retry-loop-logic ()
  "Test the retry loop decision logic."
  (message "\n🧪 Test 2: Testing retry loop decision logic...")
  
  (let ((efrit-do-max-retries 3)
        (efrit-do-retry-on-errors t))
    
    ;; Test scenarios
    (message "  Max attempts with retry enabled: %d" (if efrit-do-retry-on-errors (1+ efrit-do-max-retries) 1))
    (message "  Max attempts with retry disabled: %d" (let ((efrit-do-retry-on-errors nil))
                                                          (if efrit-do-retry-on-errors (1+ efrit-do-max-retries) 1)))
    
    ;; Test error conditions
    (let ((mock-success "[Executed: (+ 1 2)]\n[Result: 3]")
          (mock-error "[Syntax Error in (bad-call): invalid syntax]"))
      
      (message "  Success case error detection: %s" (car (efrit-do--extract-error-info mock-success)))
      (message "  Error case error detection: %s" (car (efrit-do--extract-error-info mock-error))))))

;; Test 3: Test the context building for retries
(defun test-retry-context-building ()
  "Test context building for retry scenarios."
  (message "\n🧪 Test 3: Testing retry context building...")
  
  ;; Test the rich context function that was added in Session 3
  (if (fboundp 'efrit-do--build-error-context)
      (let ((context (efrit-do--build-error-context)))
        (message "  Error context length: %d characters" (length context))
        (message "  Contains buffer info: %s" (if (string-match-p "Current buffer" context) "YES" "NO"))
        (message "  Contains directory info: %s" (if (string-match-p "directory" context) "YES" "NO")))
    (message "  Error context function not found (Session 3 not implemented)")))

;; Run tests
(test-retry-with-syntax-error)
(test-retry-loop-logic)
(test-retry-context-building)

(message "\n✅ Forced retry tests complete")
(message "\nTo test with real API (if you have API key):")
(message "(efrit-do \"Some deliberately complex command that might fail\")")

;;; test-retry-forced.el ends here
