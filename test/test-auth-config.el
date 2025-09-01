;;; test-auth-config.el --- Tests for authHelper and base_url configuration -*- lexical-binding: t -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Paulo Casaretto <paulo.casaretto@shopify.com>
;; Version: 1.0.0
;; Keywords: ai, tools, testing, auth, config

;;; Commentary:
;;
;; Comprehensive tests for the authHelper and base_url configuration functionality.
;; These tests verify:
;; 1. Default auth-source behavior is preserved
;; 2. Custom authHelper functions work correctly
;; 3. Base URL configuration precedence is respected
;; 4. Environment variable integration works
;; 5. Error handling is robust
;; 6. Integration across multiple modules

;;; Code:

(message "=== Authentication and Base URL Configuration Tests ===")

;; Set up environment
(add-to-list 'load-path ".")
(add-to-list 'load-path "../lisp")  ; For when run from test directory

;; Load required modules
(require 'efrit-tools)
(require 'cl-lib)

;; Test tracking
(defvar test-auth-results nil
  "List of authentication test results.")

(defvar test-auth-original-settings nil
  "Original settings to restore after tests.")

(defvar test-auth-test-count 0
  "Count of tests run.")

(defvar test-auth-pass-count 0
  "Count of tests passed.")

;;; Test Infrastructure

(defun test-auth--record (test-name success-p message)
  "Record an authentication test result."
  (setq test-auth-test-count (1+ test-auth-test-count))
  (when success-p
    (setq test-auth-pass-count (1+ test-auth-pass-count)))
  (push (list test-name success-p message) test-auth-results)
  (message "%s %s: %s" 
           (if success-p "✅" "❌") 
           test-name 
           message))

(defun test-auth--setup ()
  "Set up test environment."
  (message "\n🔧 Setting up test environment...")
  ;; Save original settings
  (setq test-auth-original-settings
        (list (cons 'efrit-auth-helper efrit-auth-helper)
              (cons 'efrit-base-url efrit-base-url)))
  ;; Reset to defaults
  (setq efrit-auth-helper nil
        efrit-base-url nil)
  (message "✅ Test environment ready"))

(defun test-auth--cleanup ()
  "Clean up test environment."
  (message "\n🧹 Cleaning up test environment...")
  ;; Restore original settings
  (when test-auth-original-settings
    (setq efrit-auth-helper (cdr (assoc 'efrit-auth-helper test-auth-original-settings))
          efrit-base-url (cdr (assoc 'efrit-base-url test-auth-original-settings))))
  (message "✅ Test environment restored"))

(defun test-auth--summary ()
  "Print test summary and return success status."
  (message "\n=== Authentication Configuration Test Summary ===")
  (message "Total: %d, Passed: %d, Failed: %d" 
           test-auth-test-count 
           test-auth-pass-count 
           (- test-auth-test-count test-auth-pass-count))
  
  (when (< test-auth-pass-count test-auth-test-count)
    (message "\nFailures:")
    (dolist (result (cl-remove-if #'cadr test-auth-results))
      (message "  ❌ %s: %s" (car result) (caddr result))))
  
  (let ((success-p (= test-auth-pass-count test-auth-test-count)))
    (if success-p
        (message "\n🎉 All authentication configuration tests passed!")
      (message "\n❌ Some authentication configuration tests failed!"))
    success-p))

;;; Mock Infrastructure

(defun test-auth--create-simple-helper (api-key)
  "Create a simple authHelper that returns API-KEY."
  (lambda () api-key))

(defun test-auth--create-failing-helper (error-msg)
  "Create an authHelper that throws an error with ERROR-MSG."
  (lambda () (error error-msg)))

(defun test-auth--create-nil-helper ()
  "Create an authHelper that returns nil."
  (lambda () nil))

(defun test-auth--create-logging-helper (api-key)
  "Create an authHelper that logs and returns API-KEY."
  (lambda () 
    (message "Test auth helper called")
    api-key))

;;; Core Test Cases

(defun test-auth-default-api-url ()
  "Test default API URL construction."
  (let ((efrit-base-url nil))
    (condition-case err
        (let ((url (efrit--get-api-url "v1/messages")))
          (if (string= url "https://api.anthropic.com/v1/messages")
              (test-auth--record "Default API URL" t 
                                "Returns correct default URL")
            (test-auth--record "Default API URL" nil 
                              (format "Expected default URL, got: %s" url))))
      (error
       (test-auth--record "Default API URL" nil 
                         (format "Error: %s" (error-message-string err)))))))

(defun test-auth-custom-base-url ()
  "Test custom base URL setting."
  (let ((efrit-base-url "https://custom.anthropic.com"))
    (condition-case err
        (let ((url (efrit--get-api-url "v1/messages")))
          (if (string= url "https://custom.anthropic.com/v1/messages")
              (test-auth--record "Custom Base URL" t 
                                "Uses custom base URL correctly")
            (test-auth--record "Custom Base URL" nil 
                              (format "Expected custom URL, got: %s" url))))
      (error
       (test-auth--record "Custom Base URL" nil 
                         (format "Error: %s" (error-message-string err)))))))

(defun test-auth-different-endpoints ()
  "Test URL construction with different endpoints."
  (let ((efrit-base-url "https://test.anthropic.com"))
    (condition-case err
        (let ((urls (mapcar #'efrit--get-api-url 
                           '("v1/messages" "v2/chat" "health"))))
          (if (and (string= (nth 0 urls) "https://test.anthropic.com/v1/messages")
                   (string= (nth 1 urls) "https://test.anthropic.com/v2/chat")
                   (string= (nth 2 urls) "https://test.anthropic.com/health"))
              (test-auth--record "Different Endpoints" t 
                                "Constructs URLs correctly for different endpoints")
            (test-auth--record "Different Endpoints" nil 
                              (format "URL construction failed: %s" urls))))
      (error
       (test-auth--record "Different Endpoints" nil 
                         (format "Error: %s" (error-message-string err)))))))

(defun test-auth-simple-helper ()
  "Test simple custom authHelper."
  (let ((efrit-auth-helper (test-auth--create-simple-helper "test-key-123")))
    (condition-case err
        (let ((key (efrit--get-api-key)))
          (if (string= key "test-key-123")
              (test-auth--record "Simple AuthHelper" t 
                                "Returns custom API key correctly")
            (test-auth--record "Simple AuthHelper" nil 
                              (format "Expected test-key-123, got: %s" key))))
      (error
       (test-auth--record "Simple AuthHelper" nil 
                         (format "Error: %s" (error-message-string err)))))))

(defun test-auth-nil-helper ()
  "Test authHelper that returns nil."
  (let ((efrit-auth-helper (test-auth--create-nil-helper)))
    (condition-case err
        (let ((key (efrit--get-api-key)))
          (if (null key)
              (test-auth--record "Nil AuthHelper" t 
                                "Handles nil return correctly")
            (test-auth--record "Nil AuthHelper" nil 
                              (format "Expected nil, got: %s" key))))
      (error
       (test-auth--record "Nil AuthHelper" nil 
                         (format "Error: %s" (error-message-string err)))))))

(defun test-auth-failing-helper ()
  "Test authHelper that throws an error."
  (let ((efrit-auth-helper (test-auth--create-failing-helper "Test auth error")))
    (condition-case err
        (progn
          (efrit--get-api-key)
          (test-auth--record "Failing AuthHelper" nil 
                            "Should have thrown an error"))
      (error
       (if (string-match-p "Test auth error" (error-message-string err))
           (test-auth--record "Failing AuthHelper" t 
                             "Properly propagates auth helper errors")
         (test-auth--record "Failing AuthHelper" nil 
                           (format "Unexpected error: %s" (error-message-string err))))))))

(defun test-auth-helper-fallback ()
  "Test fallback to auth-source when authHelper is nil."
  (let ((efrit-auth-helper nil))
    (condition-case err
        (progn
          ;; This will likely fail in test environment, but should not crash
          (efrit--get-api-key)
          (test-auth--record "AuthHelper Fallback" t 
                            "Falls back to auth-source without crashing"))
      (error
       ;; auth-source errors are expected in test environment
       (if (string-match-p "auth\\|secret" (error-message-string err))
           (test-auth--record "AuthHelper Fallback" t 
                             "Properly falls back to auth-source")
         (test-auth--record "AuthHelper Fallback" nil 
                           (format "Unexpected fallback error: %s" (error-message-string err))))))))

(defun test-auth-integration ()
  "Test integration of authHelper and base_url together."
  (let ((efrit-auth-helper (test-auth--create-simple-helper "integration-key"))
        (efrit-base-url "https://integration.anthropic.com"))
    (condition-case err
        (let ((key (efrit--get-api-key))
              (url (efrit--get-api-url "v1/messages")))
          (if (and (string= key "integration-key")
                   (string= url "https://integration.anthropic.com/v1/messages"))
              (test-auth--record "Integration Test" t 
                                "AuthHelper and base_url work together")
            (test-auth--record "Integration Test" nil 
                              (format "Integration failed - key: %s, url: %s" key url))))
      (error
       (test-auth--record "Integration Test" nil 
                         (format "Error: %s" (error-message-string err)))))))

(defun test-auth-function-base-url ()
  "Test function-based base URL configuration."
  (let ((efrit-base-url (lambda () "https://function.anthropic.com")))
    (condition-case err
        (let ((url (efrit--get-api-url "v1/messages")))
          (if (string= url "https://function.anthropic.com/v1/messages")
              (test-auth--record "Function Base URL" t 
                                "Uses function-based base URL correctly")
            (test-auth--record "Function Base URL" nil 
                              (format "Expected function URL, got: %s" url))))
      (error
       (test-auth--record "Function Base URL" nil 
                         (format "Error: %s" (error-message-string err)))))))

(defun test-auth-dynamic-function-base-url ()
  "Test dynamic function-based base URL with environment fallback."
  (let ((efrit-base-url (lambda () (or (getenv "TEST_ANTHROPIC_URL") "https://default.anthropic.com"))))
    ;; Test without env var
    (setenv "TEST_ANTHROPIC_URL" nil)
    (condition-case err
        (let ((url (efrit--get-api-url "v1/messages")))
          (if (string= url "https://default.anthropic.com/v1/messages")
              (test-auth--record "Dynamic Function Base URL (no env)" t 
                                "Uses fallback when env var not set")
            (test-auth--record "Dynamic Function Base URL (no env)" nil 
                              (format "Expected default URL, got: %s" url))))
      (error
       (test-auth--record "Dynamic Function Base URL (no env)" nil 
                         (format "Error: %s" (error-message-string err)))))
    
    ;; Test with env var
    (setenv "TEST_ANTHROPIC_URL" "https://env-test.anthropic.com")
    (condition-case err
        (let ((url (efrit--get-api-url "v1/messages")))
          (if (string= url "https://env-test.anthropic.com/v1/messages")
              (test-auth--record "Dynamic Function Base URL (with env)" t 
                                "Uses env var when set")
            (test-auth--record "Dynamic Function Base URL (with env)" nil 
                              (format "Expected env URL, got: %s" url))))
      (error
       (test-auth--record "Dynamic Function Base URL (with env)" nil 
                         (format "Error: %s" (error-message-string err)))))
    ;; Cleanup
    (setenv "TEST_ANTHROPIC_URL" nil)))

(defun test-auth-trailing-slash-handling ()
  "Test base URL trailing slash handling."
  (let ((efrit-base-url "https://test.anthropic.com/"))  ; Note trailing slash
    (condition-case err
        (let ((url (efrit--get-api-url "v1/messages")))
          (if (string= url "https://test.anthropic.com//v1/messages")
              (test-auth--record "Trailing Slash" t 
                                "Preserves trailing slash behavior")
            (test-auth--record "Trailing Slash" nil 
                              (format "Unexpected trailing slash handling: %s" url))))
      (error
       (test-auth--record "Trailing Slash" nil 
                         (format "Error: %s" (error-message-string err)))))))

;;; Test Runner

(defun test-auth-run-all ()
  "Run all authentication and base URL configuration tests."
  (interactive)
  
  ;; Reset test state
  (setq test-auth-results nil
        test-auth-test-count 0
        test-auth-pass-count 0)
  
  ;; Set up test environment
  (test-auth--setup)
  
  ;; Run all test cases
  (message "\n🧪 Running Base URL Configuration Tests...")
  (test-auth-default-api-url)
  (test-auth-custom-base-url)
  (test-auth-different-endpoints)
  (test-auth-function-base-url)
  (test-auth-dynamic-function-base-url)
  (test-auth-trailing-slash-handling)
  
  (message "\n🔑 Running AuthHelper Tests...")
  (test-auth-simple-helper)
  (test-auth-nil-helper)
  (test-auth-failing-helper)
  (test-auth-helper-fallback)
  
  (message "\n🔗 Running Integration Tests...")
  (test-auth-integration)
  
  ;; Clean up and show results
  (test-auth--cleanup)
  (let ((success-p (test-auth--summary)))
    (unless success-p
      (kill-emacs 1))))

;; Run tests when loaded in batch mode
(when noninteractive
  (test-auth-run-all))

(provide 'test-auth-config)
;;; test-auth-config.el ends here