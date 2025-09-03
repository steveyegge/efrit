;;; test-retry-logic.el --- Tests for efrit-do retry functionality -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Keywords: test

;;; Commentary:
;; Tests for the retry logic implementation in efrit-do.el

;;; Code:

;; Add the lisp directory to load path
(let ((test-dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "../lisp" test-dir)))
(require 'ert)
(require 'efrit-do)

;; Test helper functions

(defun test-retry--setup ()
  "Set up test environment for retry tests."
  (setq efrit-do-debug t)
  (setq efrit-do-retry-on-errors t)
  (setq efrit-do-max-retries 2)
  (setq efrit-do-show-results nil)) ; Don't show buffers during tests

(defun test-retry--cleanup ()
  "Clean up after retry tests."
  (setq efrit-do-debug nil)
  (setq efrit-do-retry-on-errors t)
  (setq efrit-do-max-retries 3)
  (setq efrit-do-show-results t))

;; Test error extraction functions

(ert-deftest test-extract-error-info-syntax-error ()
  "Test extraction of syntax error information."
  (test-retry--setup)
  (let ((result "[Syntax Error in (invalid-elisp: Invalid read syntax: \")\"]"))
    (let ((error-info (efrit-do--extract-error-info result)))
      (should (car error-info))
      (should (string-match "Syntax error" (cdr error-info)))))
  (test-retry--cleanup))

(ert-deftest test-extract-error-info-runtime-error ()
  "Test extraction of runtime error information."
  (test-retry--setup)
  (let ((result "[Error executing (nonexistent-function): Symbol's function definition is void: nonexistent-function]"))
    (let ((error-info (efrit-do--extract-error-info result)))
      (should (car error-info))
      (should (string-match "Runtime error" (cdr error-info)))))
  (test-retry--cleanup))

(ert-deftest test-extract-error-info-no-error ()
  "Test that no error is detected in successful results."
  (test-retry--setup)
  (let ((result "[Executed: (message \"test\")]\n[Result: test]"))
    (let ((error-info (efrit-do--extract-error-info result)))
      (should-not (car error-info))
      (should-not (cdr error-info))))
  (test-retry--cleanup))

(ert-deftest test-extract-executed-code-from-error ()
  "Test extraction of executed code from error messages."
  (test-retry--setup)
  (let ((result "[Syntax Error in (invalid-elisp: Invalid read syntax]"))
    (let ((code (efrit-do--extract-executed-code result)))
      (should (string= code "(invalid-elisp"))))
  (test-retry--cleanup))

(ert-deftest test-extract-executed-code-from-success ()
  "Test extraction of executed code from success messages."
  (test-retry--setup)
  (let ((result "[Executed: (message \"hello\")]\n[Result: hello]"))
    (let ((code (efrit-do--extract-executed-code result)))
      (should (string= code "(message \"hello\")"))))
  (test-retry--cleanup))

;; Test system prompt generation for retries

(ert-deftest test-retry-system-prompt-generation ()
  "Test that retry system prompts include error information."
  (test-retry--setup)
  (let ((prompt (efrit-do--command-system-prompt 2 "Syntax error: invalid" "(bad-code")))
    (should (string-match "RETRY ATTEMPT 2/" prompt))
    (should (string-match "Previous code that failed: (bad-code" prompt))
    (should (string-match "Error encountered: Syntax error: invalid" prompt)))
  (test-retry--cleanup))

(ert-deftest test-normal-system-prompt-generation ()
  "Test that normal system prompts don't include retry information."
  (test-retry--setup)
  (let ((prompt (efrit-do--command-system-prompt)))
    (should-not (string-match "RETRY ATTEMPT" prompt))
    (should-not (string-match "Previous code that failed" prompt)))
  (test-retry--cleanup))

;; Mock function tests (since we can't easily test full API calls)

(defvar test-retry--mock-responses nil
  "List of mock responses for testing retry logic.")

(defvar test-retry--call-count 0
  "Counter for mock API calls.")

(defun test-retry--mock-execute-command (command &optional retry-count error-msg previous-code)
  "Mock version of efrit-do--execute-command for testing."
  (setq test-retry--call-count (1+ test-retry--call-count))
  (when test-retry--mock-responses
    (pop test-retry--mock-responses)))

(ert-deftest test-retry-disabled-single-attempt ()
  "Test that when retry is disabled, only one attempt is made."
  (test-retry--setup)
  (setq efrit-do-retry-on-errors nil)
  (setq test-retry--mock-responses '("[Syntax Error in (bad: syntax error]"))
  (setq test-retry--call-count 0)
  
  ;; Mock the execute command function
  (let ((original-fn (symbol-function 'efrit-do--execute-command)))
    (unwind-protect
        (progn
          (fset 'efrit-do--execute-command #'test-retry--mock-execute-command)
          (should-error (efrit-do "test command")))
      (fset 'efrit-do--execute-command original-fn)))
  
  (should (= test-retry--call-count 1))
  (test-retry--cleanup))

(ert-deftest test-retry-enabled-multiple-attempts ()
  "Test that when retry is enabled, multiple attempts are made on errors."
  (test-retry--setup)
  (setq efrit-do-retry-on-errors t)
  (setq efrit-do-max-retries 2)
  (setq test-retry--mock-responses '("[Syntax Error in (bad1: error1]"
                                    "[Syntax Error in (bad2: error2]"
                                    "[Executed: (message \"success\")]\n[Result: success]"))
  (setq test-retry--call-count 0)
  
  ;; Mock the execute command function
  (let ((original-fn (symbol-function 'efrit-do--execute-command)))
    (unwind-protect
        (progn
          (fset 'efrit-do--execute-command #'test-retry--mock-execute-command)
          (efrit-do "test command"))
      (fset 'efrit-do--execute-command original-fn)))
  
  (should (= test-retry--call-count 3)) ; First attempt + 2 retries
  (test-retry--cleanup))

(ert-deftest test-retry-exhausted-all-attempts ()
  "Test behavior when all retry attempts are exhausted."
  (test-retry--setup)
  (setq efrit-do-retry-on-errors t)
  (setq efrit-do-max-retries 2)
  (setq test-retry--mock-responses '("[Syntax Error in (bad1: error1]"
                                    "[Syntax Error in (bad2: error2]"
                                    "[Syntax Error in (bad3: error3]"))
  (setq test-retry--call-count 0)
  
  ;; Mock the execute command function
  (let ((original-fn (symbol-function 'efrit-do--execute-command)))
    (unwind-protect
        (progn
          (fset 'efrit-do--execute-command #'test-retry--mock-execute-command)
          (should-error (efrit-do "test command")))
      (fset 'efrit-do--execute-command original-fn)))
  
  (should (= test-retry--call-count 3)) ; First attempt + 2 retries = 3 total
  (test-retry--cleanup))

(ert-deftest test-retry-success-on-first-attempt ()
  "Test that no retries occur when first attempt succeeds."
  (test-retry--setup)
  (setq efrit-do-retry-on-errors t)
  (setq test-retry--mock-responses '("[Executed: (message \"success\")]\n[Result: success]"))
  (setq test-retry--call-count 0)
  
  ;; Mock the execute command function
  (let ((original-fn (symbol-function 'efrit-do--execute-command)))
    (unwind-protect
        (progn
          (fset 'efrit-do--execute-command #'test-retry--mock-execute-command)
          (efrit-do "test command"))
      (fset 'efrit-do--execute-command original-fn)))
  
  (should (= test-retry--call-count 1)) ; Only one attempt needed
  (test-retry--cleanup))

;; Test integration scenarios

(ert-deftest test-retry-integration-with-context ()
  "Test that retry logic integrates properly with context system."
  (test-retry--setup)
  (efrit-do--clear-context) ; Start with clean context
  
  (setq test-retry--mock-responses '("[Executed: (message \"test\")]\n[Result: test]"))
  (setq test-retry--call-count 0)
  
  ;; Mock the execute command function
  (let ((original-fn (symbol-function 'efrit-do--execute-command)))
    (unwind-protect
        (progn
          (fset 'efrit-do--execute-command #'test-retry--mock-execute-command)
          (efrit-do "test command"))
      (fset 'efrit-do--execute-command original-fn)))
  
  ;; Verify context was captured
  (let ((items (efrit-do--get-context-items 1)))
    (should items)
    (should (string= (efrit-do-context-item-command (car items)) "test command")))
  
  (test-retry--cleanup))

;; Run all tests

(defun test-retry-run-all ()
  "Run all retry logic tests."
  (interactive)
  (ert-run-tests-batch "^test-.*retry.*"))

(provide 'test-retry-logic)

;;; test-retry-logic.el ends here
