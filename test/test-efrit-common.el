;;; test-efrit-common.el --- Tests for efrit-common.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;;; Commentary:
;; Unit tests for common utility functions.

;;; Code:

(require 'ert)

;; Add load paths
(add-to-list 'load-path (expand-file-name "../lisp" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../lisp/core" (file-name-directory load-file-name)))

(require 'efrit-common)

;;; API Key Validation Tests

(ert-deftest test-common-validate-api-key-valid ()
  "Test validation of a properly formatted API key."
  (should (efrit-common--validate-api-key "sk-ant-api03-test-key-here-1234567890")))

(ert-deftest test-common-validate-api-key-too-short ()
  "Test that short API keys are rejected."
  (should-error (efrit-common--validate-api-key "sk-short")
                :type 'error))

(ert-deftest test-common-validate-api-key-wrong-prefix ()
  "Test that keys without sk- prefix are rejected."
  (should-error (efrit-common--validate-api-key "api-ant-1234567890123456789012345")
                :type 'error))

(ert-deftest test-common-validate-api-key-empty ()
  "Test that empty keys are rejected."
  (should-error (efrit-common--validate-api-key "")
                :type 'error))

(ert-deftest test-common-validate-api-key-nil ()
  "Test that nil keys are rejected."
  (should-error (efrit-common--validate-api-key nil)
                :type 'error))

;;; API Key Sanitization Tests

(ert-deftest test-common-sanitize-key-valid ()
  "Test sanitization of a valid API key."
  (let ((sanitized (efrit-common--sanitize-key-for-logging "sk-ant-api03-abcdefghijklmnopqrst")))
    (should (string-prefix-p "sk-ant" sanitized))
    (should (string-suffix-p "qrst" sanitized))
    (should (string-match-p "\\.\\.\\." sanitized))))

(ert-deftest test-common-sanitize-key-short ()
  "Test sanitization of short/invalid key."
  (should (equal (efrit-common--sanitize-key-for-logging "short")
                 "[INVALID-KEY]")))

(ert-deftest test-common-sanitize-key-nil ()
  "Test sanitization of nil."
  (should (equal (efrit-common--sanitize-key-for-logging nil)
                 "[INVALID-KEY]")))

;;; Filename Validation Tests

(ert-deftest test-common-validate-filename-valid ()
  "Test validation of valid filenames."
  (should (equal (efrit-common--validate-filename "file.txt") "file.txt"))
  (should (equal (efrit-common--validate-filename "my-file_123.el") "my-file_123.el")))

(ert-deftest test-common-validate-filename-special-chars ()
  "Test that special characters are rejected."
  (should-error (efrit-common--validate-filename "file<name>.txt") :type 'error)
  (should-error (efrit-common--validate-filename "file:name.txt") :type 'error)
  (should-error (efrit-common--validate-filename "file|name.txt") :type 'error)
  (should-error (efrit-common--validate-filename "file?name.txt") :type 'error)
  (should-error (efrit-common--validate-filename "file*name.txt") :type 'error))

(ert-deftest test-common-validate-filename-too-long ()
  "Test that overly long filenames are rejected."
  (let ((long-name (make-string 256 ?a)))
    (should-error (efrit-common--validate-filename long-name) :type 'error)))

;;; Path Validation Tests

(ert-deftest test-common-validate-path-within-allowed ()
  "Test that paths within allowed directories pass."
  (let ((allowed-paths (list (expand-file-name "~"))))
    (should (efrit-common--validate-path "~/.config" allowed-paths))))

(ert-deftest test-common-validate-path-outside-allowed ()
  "Test that paths outside allowed directories are rejected."
  (let ((allowed-paths (list "/home/user/safe")))
    (should-error (efrit-common--validate-path "/etc/passwd" allowed-paths)
                  :type 'error)))

;;; Safe File Expansion Tests

(ert-deftest test-common-safe-expand-valid ()
  "Test safe expansion of valid paths."
  (let* ((temp-dir (make-temp-file "efrit-test" t))
         (result (efrit-common-safe-expand-file-name "file.txt" temp-dir)))
    (should (string-prefix-p temp-dir result))
    (should (string-suffix-p "file.txt" result))
    (delete-directory temp-dir t)))

(ert-deftest test-common-safe-expand-traversal-blocked ()
  "Test that path traversal is blocked."
  (let ((temp-dir (make-temp-file "efrit-test" t)))
    (unwind-protect
        (should-error (efrit-common-safe-expand-file-name "../../../etc/passwd" temp-dir)
                      :type 'error)
      (delete-directory temp-dir t))))

;;; String Truncation Tests

(ert-deftest test-common-truncate-short-string ()
  "Test that short strings are not truncated."
  (should (equal (efrit-common-truncate-string "hello" 10) "hello")))

(ert-deftest test-common-truncate-long-string ()
  "Test truncation of long strings."
  (let ((result (efrit-common-truncate-string "hello world" 8)))
    (should (string-suffix-p "..." result))
    ;; Default behavior: 8 chars + "..."
    (should (= (length result) 11))))

(ert-deftest test-common-truncate-ellipsis-in-max ()
  "Test truncation with ellipsis included in max length."
  (let ((result (efrit-common-truncate-string "hello world" 8 t)))
    (should (string-suffix-p "..." result))
    ;; With ellipsis-in-max: total length should be 8
    (should (= (length result) 8))))

(ert-deftest test-common-truncate-exact-length ()
  "Test string at exact max length."
  (should (equal (efrit-common-truncate-string "hello" 5) "hello")))

;;; Unicode Escaping Tests

(ert-deftest test-common-escape-unicode-ascii ()
  "Test that ASCII strings are unchanged."
  (should (equal (efrit-common-escape-json-unicode "hello world") "hello world")))

(ert-deftest test-common-escape-unicode-non-ascii ()
  "Test escaping of non-ASCII characters."
  (let ((result (efrit-common-escape-json-unicode "hello \u00e9 world")))
    (should (string-match-p "\\\\u00E9" result))))

(ert-deftest test-common-escape-unicode-emoji ()
  "Test escaping of emoji."
  (let ((result (efrit-common-escape-json-unicode "test \u2714 pass")))
    (should (string-match-p "\\\\u2714" result))))

;;; URL and Header Building Tests

(ert-deftest test-common-get-base-url-default ()
  "Test default base URL."
  (let ((efrit-api-base-url "https://api.anthropic.com"))
    (should (equal (efrit-common-get-base-url) "https://api.anthropic.com"))))

(ert-deftest test-common-get-base-url-function ()
  "Test base URL from function."
  (let ((efrit-api-base-url (lambda () "https://custom.api.example.com")))
    (should (equal (efrit-common-get-base-url) "https://custom.api.example.com"))))

(ert-deftest test-common-get-api-url ()
  "Test full API URL construction."
  (let ((efrit-api-base-url "https://api.anthropic.com"))
    (should (equal (efrit-common-get-api-url) "https://api.anthropic.com/v1/messages"))))

(ert-deftest test-common-build-headers ()
  "Test header building with valid API key."
  (let* ((test-key "sk-ant-api03-valid-key-for-testing-123")
         (headers (efrit-common-build-headers test-key)))
    (should (equal (alist-get "Content-Type" headers nil nil #'equal) "application/json"))
    (should (equal (alist-get "x-api-key" headers nil nil #'equal) test-key))
    (should (alist-get "anthropic-version" headers nil nil #'equal))))

;;; Safe Error Message Tests

(ert-deftest test-common-safe-error-message-string ()
  "Test extracting message from string."
  (should (equal (efrit-common-safe-error-message "error message") "error message")))

(ert-deftest test-common-safe-error-message-cons ()
  "Test extracting message from error cons."
  (should (equal (efrit-common-safe-error-message '(error "the message")) "the message")))

;;; Safe Execute Tests

(ert-deftest test-common-safe-execute-success ()
  "Test safe execute with successful function."
  (let ((result (efrit--safe-execute (lambda () (+ 1 2)) "test addition")))
    (should (car result))
    (should (equal (cdr result) 3))))

(ert-deftest test-common-safe-execute-error ()
  "Test safe execute with failing function."
  (let ((result (efrit--safe-execute
                 (lambda () (error "intentional error"))
                 "test error handling")))
    (should-not (car result))
    (should (stringp (cdr result)))))

(ert-deftest test-common-safe-execute-file-error ()
  "Test safe execute with file error."
  (let ((result (efrit--safe-execute
                 (lambda () (signal 'file-error '("test" "no such file")))
                 "test file error")))
    (should-not (car result))
    (should (string-match-p "File error" (cdr result)))))

(provide 'test-efrit-common)
;;; test-efrit-common.el ends here
