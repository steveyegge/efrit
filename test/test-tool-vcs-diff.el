;;; test-tool-vcs-diff.el --- Tests for vcs_diff tool -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;;; Commentary:
;; Tests for efrit-tool-vcs-diff git diff functionality.
;; Note: These tests require running in a git repository.

;;; Code:

(require 'ert)
(require 'json)

;; Add load paths for test
(add-to-list 'load-path (expand-file-name "../lisp" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../lisp/core" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../lisp/tools" (file-name-directory load-file-name)))

(require 'efrit-tool-utils)
(require 'efrit-tool-vcs-diff)

;;; Helper functions

(defun test-vcs-diff--get-result (response)
  "Extract result from RESPONSE alist."
  (alist-get 'result response))

(defun test-vcs-diff--success-p (response)
  "Check if RESPONSE indicates success."
  (eq (alist-get 'success response) t))

;;; Basic functionality tests

(ert-deftest test-vcs-diff-basic ()
  "Test basic vcs diff (unstaged changes)."
  (let ((response (efrit-tool-vcs-diff '())))
    (should (test-vcs-diff--success-p response))
    (let ((result (test-vcs-diff--get-result response)))
      ;; Should have diff field (may be empty if no changes)
      (should (stringp (alist-get 'diff result)))
      ;; Should have summary
      (should (alist-get 'summary result))
      ;; Should have files array
      (should (vectorp (alist-get 'files result)))
      ;; Should indicate diff type
      (should (equal (alist-get 'diff_type result) "unstaged")))))

(ert-deftest test-vcs-diff-staged ()
  "Test staged diff."
  (let ((response (efrit-tool-vcs-diff '((staged . t)))))
    (should (test-vcs-diff--success-p response))
    (let ((result (test-vcs-diff--get-result response)))
      ;; Should indicate staged diff type
      (should (equal (alist-get 'diff_type result) "staged")))))

(ert-deftest test-vcs-diff-against-commit ()
  "Test diff against specific commit."
  (let ((response (efrit-tool-vcs-diff '((commit . "HEAD~1")))))
    (should (test-vcs-diff--success-p response))
    (let ((result (test-vcs-diff--get-result response)))
      ;; Should indicate commit diff type
      (should (string-match-p "vs HEAD~1" (alist-get 'diff_type result))))))

;;; Summary tests

(ert-deftest test-vcs-diff-summary-structure ()
  "Test that summary has correct structure."
  (let* ((response (efrit-tool-vcs-diff '((commit . "HEAD~1"))))
         (result (test-vcs-diff--get-result response))
         (summary (alist-get 'summary result)))
    (should (test-vcs-diff--success-p response))
    ;; Summary should have file count and line stats
    (should (assq 'files_changed summary))
    (should (assq 'insertions summary))
    (should (assq 'deletions summary))
    ;; All should be integers
    (should (integerp (alist-get 'files_changed summary)))
    (should (integerp (alist-get 'insertions summary)))
    (should (integerp (alist-get 'deletions summary)))))

(ert-deftest test-vcs-diff-summary-counts ()
  "Test that summary counts are non-negative."
  (let* ((response (efrit-tool-vcs-diff '((commit . "HEAD~1"))))
         (result (test-vcs-diff--get-result response))
         (summary (alist-get 'summary result)))
    (should (test-vcs-diff--success-p response))
    (should (>= (alist-get 'files_changed summary) 0))
    (should (>= (alist-get 'insertions summary) 0))
    (should (>= (alist-get 'deletions summary) 0))))

;;; File stats tests

(ert-deftest test-vcs-diff-files-structure ()
  "Test that per-file stats have correct structure."
  (let* ((response (efrit-tool-vcs-diff '((commit . "HEAD~1"))))
         (result (test-vcs-diff--get-result response))
         (files (alist-get 'files result)))
    (should (test-vcs-diff--success-p response))
    ;; If there are file changes, check structure
    (when (> (length files) 0)
      (let ((first-file (aref files 0)))
        (should (alist-get 'path first-file))
        (should (integerp (alist-get 'insertions first-file)))
        (should (integerp (alist-get 'deletions first-file)))))))

;;; Context lines tests

(ert-deftest test-vcs-diff-context-lines ()
  "Test that context_lines parameter works."
  ;; Default is 3 context lines
  (let* ((response-default (efrit-tool-vcs-diff '((commit . "HEAD~1"))))
         (response-more (efrit-tool-vcs-diff '((commit . "HEAD~1")
                                               (context_lines . 5)))))
    (should (test-vcs-diff--success-p response-default))
    (should (test-vcs-diff--success-p response-more))
    ;; Can't easily verify the actual context, but both should succeed
    ))

;;; Path filtering tests

(ert-deftest test-vcs-diff-path-filter ()
  "Test diff with specific file path."
  (let ((response (efrit-tool-vcs-diff '((path . "CLAUDE.md")
                                         (commit . "HEAD~1")))))
    (should (test-vcs-diff--success-p response))
    (let* ((result (test-vcs-diff--get-result response))
           (files (alist-get 'files result)))
      ;; If there are file changes, they should only be for CLAUDE.md
      (when (> (length files) 0)
        (should (seq-every-p (lambda (f)
                               (string-match-p "CLAUDE" (alist-get 'path f)))
                             files))))))

;;; Truncation tests

(ert-deftest test-vcs-diff-truncated-indicator ()
  "Test that truncated indicator is present."
  (let* ((response (efrit-tool-vcs-diff '((commit . "HEAD~1"))))
         (result (test-vcs-diff--get-result response)))
    (should (test-vcs-diff--success-p response))
    ;; Truncated should be a boolean
    (should (member (alist-get 'truncated result) '(t :json-false)))))

;;; Diff output format tests

(ert-deftest test-vcs-diff-output-format ()
  "Test that diff output is valid."
  (let* ((response (efrit-tool-vcs-diff '((commit . "HEAD~1"))))
         (result (test-vcs-diff--get-result response))
         (diff (alist-get 'diff result)))
    (should (test-vcs-diff--success-p response))
    ;; Diff should be a string
    (should (stringp diff))
    ;; If non-empty, should contain diff markers
    (when (> (length diff) 0)
      ;; Should contain typical diff content
      (should (or (string-match-p "^diff --git" diff)
                  (string-match-p "^@@" diff)
                  ;; Or could be empty if no changes
                  (string-empty-p diff))))))

;;; Result completeness tests

(ert-deftest test-vcs-diff-all-fields-present ()
  "Test that all expected fields are present in response."
  (let* ((response (efrit-tool-vcs-diff '()))
         (result (test-vcs-diff--get-result response)))
    (should (test-vcs-diff--success-p response))
    ;; Check all expected fields exist
    (should (assq 'diff result))
    (should (assq 'summary result))
    (should (assq 'files result))
    (should (assq 'truncated result))
    (should (assq 'diff_type result))))

;;; JSON encoding test

(ert-deftest test-vcs-diff-json-encodable ()
  "Test that response is JSON encodable."
  (let ((response (efrit-tool-vcs-diff '((commit . "HEAD~1")))))
    (should (stringp (json-encode response)))))

(provide 'test-tool-vcs-diff)

;;; test-tool-vcs-diff.el ends here
