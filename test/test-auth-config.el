;;; test-auth-config.el --- Tests for authentication and configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Keywords: tests

;;; Commentary:
;; Test suite for authentication and base URL configuration features.
;; Adapted from PR #9 to work with current efrit architecture.

;;; Code:

(add-to-list 'load-path "../lisp")

(require 'ert)
(require 'efrit-common)

;; Test helper functions
(defun test-auth-config--with-env-var (var value body-fn)
  "Execute BODY-FN with environment variable VAR set to VALUE."
  (let ((old-value (getenv var)))
    (setenv var value)
    (unwind-protect
        (funcall body-fn)
      (if old-value
          (setenv var old-value)
        (setenv var nil)))))

(defmacro test-auth-config-with-env (var value &rest body)
  "Execute BODY with environment variable VAR set to VALUE."
  `(test-auth-config--with-env-var ,var ,value (lambda () ,@body)))

;; Authentication tests
(ert-deftest test-auth-config-env-var-symbol ()
  "Test API key retrieval from environment variable via symbol."
  (test-auth-config-with-env "TEST_API_KEY" "sk-test-key-1234567890abcdef"
    (let ((efrit-api-key 'TEST_API_KEY))
      (should (string= (efrit-common-get-api-key) "sk-test-key-1234567890abcdef")))))

(ert-deftest test-auth-config-env-var-anthropic ()
  "Test API key retrieval from ANTHROPIC_API_KEY environment variable."
  (test-auth-config-with-env "ANTHROPIC_API_KEY" "sk-fallback-key-1234567890abcdef"
    (let ((efrit-api-key nil))
      (should (string= (efrit-common-get-api-key) "sk-fallback-key-1234567890abcdef")))))

(ert-deftest test-auth-config-function ()
  "Test API key retrieval from function."
  (let ((efrit-api-key (lambda () "sk-function-key-1234567890abcdef")))
    (should (string= (efrit-common-get-api-key) "sk-function-key-1234567890abcdef"))))

(ert-deftest test-auth-config-invalid-key-format ()
  "Test that invalid API key formats are rejected."
  (let ((efrit-api-key "invalid-key"))
    (should-error (efrit-common-get-api-key) :type 'error)))

(ert-deftest test-auth-config-empty-key ()
  "Test that empty API keys are rejected."
  (let ((efrit-api-key ""))
    (should-error (efrit-common-get-api-key) :type 'error)))

;; Base URL configuration tests
(ert-deftest test-auth-config-base-url-string ()
  "Test static base URL configuration."
  (let ((efrit-api-base-url "https://custom-api.example.com"))
    (should (string= (efrit-common-get-base-url) "https://custom-api.example.com"))
    (should (string= (efrit-common-get-api-url) "https://custom-api.example.com/v1/messages"))))

(ert-deftest test-auth-config-base-url-function ()
  "Test dynamic base URL configuration."
  (let ((efrit-api-base-url (lambda () "https://dynamic-api.example.com")))
    (should (string= (efrit-common-get-base-url) "https://dynamic-api.example.com"))
    (should (string= (efrit-common-get-api-url) "https://dynamic-api.example.com/v1/messages"))))

(ert-deftest test-auth-config-base-url-default ()
  "Test default base URL configuration."
  (let ((efrit-api-base-url nil))
    (should (string= (efrit-common-get-base-url) "https://api.anthropic.com"))
    (should (string= (efrit-common-get-api-url) "https://api.anthropic.com/v1/messages"))))

;; Security tests
(ert-deftest test-auth-config-key-sanitization ()
  "Test that API keys are sanitized in logs."
  (let ((test-key "sk-test-very-long-key-1234567890abcdef"))
    (should (string= (efrit-common--sanitize-key-for-logging test-key) "sk-tes...cdef"))))

(ert-deftest test-auth-config-key-validation ()
  "Test API key validation function."
  (should (efrit-common--validate-api-key "sk-valid-key-1234567890abcdef"))
  (should-error (efrit-common--validate-api-key "invalid") :type 'error)
  (should-error (efrit-common--validate-api-key "sk-short") :type 'error)
  (should-error (efrit-common--validate-api-key "") :type 'error)
  (should-error (efrit-common--validate-api-key nil) :type 'error))

;; Integration test
(ert-deftest test-auth-config-full-integration ()
  "Test full authentication and URL configuration integration."
  (test-auth-config-with-env "CUSTOM_API_KEY" "sk-integration-key-1234567890abcdef"
    (let ((efrit-api-key 'CUSTOM_API_KEY)
          (efrit-api-base-url "https://enterprise.example.com"))
      (should (string= (efrit-common-get-api-key) "sk-integration-key-1234567890abcdef"))
      (should (string= (efrit-common-get-api-url) "https://enterprise.example.com/v1/messages")))))

(provide 'test-auth-config)
;;; test-auth-config.el ends here
