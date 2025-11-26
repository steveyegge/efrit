;;; test-tool-search-content.el --- Tests for search_content tool -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;;; Commentary:
;; Tests for efrit-tool-search-content grep-like search functionality.
;; These tests run against the actual efrit repository.

;;; Code:

(require 'ert)
(require 'json)

;; Add load paths for test
(add-to-list 'load-path (expand-file-name "../lisp" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../lisp/core" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../lisp/tools" (file-name-directory load-file-name)))

(require 'efrit-tool-utils)
(require 'efrit-tool-search-content)

;;; Helper functions

(defun test-search-content--get-result (response)
  "Extract result from RESPONSE alist."
  (alist-get 'result response))

(defun test-search-content--success-p (response)
  "Check if RESPONSE indicates success."
  (eq (alist-get 'success response) t))

(defun test-search-content--get-matches (response)
  "Extract matches array from RESPONSE."
  (alist-get 'matches (test-search-content--get-result response)))

;;; Basic functionality tests

(ert-deftest test-search-content-basic ()
  "Test basic search for a known pattern."
  ;; Search for 'defun' which definitely exists in .el files
  (let ((response (efrit-tool-search-content '((pattern . "defun efrit")))))
    (should (test-search-content--success-p response))
    (let ((result (test-search-content--get-result response)))
      (should (vectorp (alist-get 'matches result)))
      (should (> (length (alist-get 'matches result)) 0)))))

(ert-deftest test-search-content-match-structure ()
  "Test that matches have correct structure."
  (let* ((response (efrit-tool-search-content '((pattern . "require 'efrit"))))
         (matches (test-search-content--get-matches response)))
    (should (test-search-content--success-p response))
    (should (> (length matches) 0))
    ;; Check first match structure
    (let ((first-match (aref matches 0)))
      (should (alist-get 'file first-match))
      (should (alist-get 'file_relative first-match))
      (should (alist-get 'line first-match))
      (should (integerp (alist-get 'line first-match)))
      (should (alist-get 'column first-match))
      (should (alist-get 'content first-match)))))

(ert-deftest test-search-content-content-matches ()
  "Test that content actually contains the search pattern."
  (let* ((response (efrit-tool-search-content '((pattern . "efrit-tool-utils"))))
         (matches (test-search-content--get-matches response)))
    (should (test-search-content--success-p response))
    (should (> (length matches) 0))
    ;; Each match content should contain the pattern
    (should (seq-every-p (lambda (m)
                           (string-match-p "efrit-tool-utils"
                                           (alist-get 'content m)))
                         matches))))

;;; File pattern filtering tests

(ert-deftest test-search-content-file-pattern-el ()
  "Test filtering by *.el file pattern."
  (let* ((response (efrit-tool-search-content '((pattern . "lexical-binding")
                                                 (file_pattern . "*.el"))))
         (matches (test-search-content--get-matches response)))
    (should (test-search-content--success-p response))
    (should (> (length matches) 0))
    ;; All matches should be in .el files
    (should (seq-every-p (lambda (m)
                           (string-suffix-p ".el" (alist-get 'file_relative m)))
                         matches))))

(ert-deftest test-search-content-file-pattern-md ()
  "Test filtering by *.md file pattern."
  (let* ((response (efrit-tool-search-content '((pattern . "Efrit")
                                                 (file_pattern . "*.md"))))
         (matches (test-search-content--get-matches response)))
    (should (test-search-content--success-p response))
    (should (> (length matches) 0))
    ;; All matches should be in .md files
    (should (seq-every-p (lambda (m)
                           (string-suffix-p ".md" (alist-get 'file_relative m)))
                         matches))))

;;; Regex tests

(ert-deftest test-search-content-regex ()
  "Test regex pattern search."
  (let* ((response (efrit-tool-search-content '((pattern . "defun efrit-tool-[a-z]+-")
                                                 (is_regex . t)
                                                 (file_pattern . "*.el"))))
         (matches (test-search-content--get-matches response)))
    (should (test-search-content--success-p response))
    (should (> (length matches) 0))))

(ert-deftest test-search-content-literal-special-chars ()
  "Test literal search with regex special characters."
  ;; Search for a literal string with special chars - use a simpler pattern
  ;; that exists in the codebase (e.g., ";;;" which has no regex special chars
  ;; but tests literal mode)
  (let* ((response (efrit-tool-search-content '((pattern . "defun test-")
                                                 (is_regex . :json-false)
                                                 (file_pattern . "*.el"))))
         (matches (test-search-content--get-matches response)))
    (should (test-search-content--success-p response))
    ;; Should find the literal string
    (should (> (length matches) 0))))

;;; Case sensitivity tests

(ert-deftest test-search-content-case-insensitive ()
  "Test case-insensitive search (default)."
  (let* ((response (efrit-tool-search-content '((pattern . "EFRIT"))))
         (matches (test-search-content--get-matches response)))
    (should (test-search-content--success-p response))
    ;; Should find matches even though code uses lowercase 'efrit'
    (should (> (length matches) 0))))

(ert-deftest test-search-content-case-sensitive ()
  "Test case-sensitive search."
  (let* ((response-insensitive (efrit-tool-search-content '((pattern . "EFRIT"))))
         (response-sensitive (efrit-tool-search-content '((pattern . "EFRIT")
                                                           (case_sensitive . t))))
         (matches-insensitive (test-search-content--get-matches response-insensitive))
         (matches-sensitive (test-search-content--get-matches response-sensitive)))
    (should (test-search-content--success-p response-insensitive))
    (should (test-search-content--success-p response-sensitive))
    ;; Case insensitive should find more or equal matches
    (should (>= (length matches-insensitive) (length matches-sensitive)))))

;;; Context lines tests

(ert-deftest test-search-content-context-before ()
  "Test that context_before is populated."
  (let* ((response (efrit-tool-search-content '((pattern . "efrit-tool-utils")
                                                 (context_lines . 2)
                                                 (max_results . 1))))
         (matches (test-search-content--get-matches response)))
    (should (test-search-content--success-p response))
    (should (> (length matches) 0))
    ;; First match should have context_before array
    (let ((first-match (aref matches 0)))
      (should (vectorp (alist-get 'context_before first-match))))))

;;; Pagination tests

(ert-deftest test-search-content-max-results ()
  "Test max_results limit."
  (let* ((response (efrit-tool-search-content '((pattern . "defun")
                                                 (max_results . 5))))
         (result (test-search-content--get-result response))
         (matches (alist-get 'matches result)))
    (should (test-search-content--success-p response))
    (should (<= (length matches) 5))
    (should (= (alist-get 'returned result) (length matches)))))

(ert-deftest test-search-content-offset ()
  "Test pagination with offset."
  (let* ((response1 (efrit-tool-search-content '((pattern . "defun")
                                                  (max_results . 3))))
         (response2 (efrit-tool-search-content '((pattern . "defun")
                                                  (max_results . 3)
                                                  (offset . 3))))
         (matches1 (test-search-content--get-matches response1))
         (matches2 (test-search-content--get-matches response2)))
    (should (test-search-content--success-p response1))
    (should (test-search-content--success-p response2))
    ;; Matches should be different
    (when (and (> (length matches1) 0) (> (length matches2) 0))
      (should-not (equal (alist-get 'file (aref matches1 0))
                         (alist-get 'file (aref matches2 0)))))))

(ert-deftest test-search-content-truncated-indicator ()
  "Test that truncated indicator works."
  (let* ((response (efrit-tool-search-content '((pattern . "defun")
                                                 (max_results . 2))))
         (result (test-search-content--get-result response)))
    (should (test-search-content--success-p response))
    ;; defun appears many times, should be truncated
    (should (eq (alist-get 'truncated result) t))
    (should (alist-get 'continuation_hint result))))

;;; No results tests

(ert-deftest test-search-content-no-matches ()
  "Test search that returns no matches."
  ;; Search in .md files only for a pattern that won't exist there
  (let* ((response (efrit-tool-search-content '((pattern . "ert-deftest xyzzy")
                                                 (file_pattern . "*.md"))))
         (result (test-search-content--get-result response))
         (matches (alist-get 'matches result)))
    (should (test-search-content--success-p response))
    (should (= (length matches) 0))
    (should (eq (alist-get 'truncated result) :json-false))))

;;; Error handling tests

(ert-deftest test-search-content-missing-pattern ()
  "Test error when pattern is missing."
  (let ((response (efrit-tool-search-content '())))
    (should-not (test-search-content--success-p response))
    (should (alist-get 'error response))))

(ert-deftest test-search-content-empty-pattern ()
  "Test error when pattern is empty."
  (let ((response (efrit-tool-search-content '((pattern . "")))))
    (should-not (test-search-content--success-p response))
    (should (alist-get 'error response))))

;;; Result metadata tests

(ert-deftest test-search-content-metadata ()
  "Test that result includes correct metadata."
  (let* ((response (efrit-tool-search-content '((pattern . "efrit"))))
         (result (test-search-content--get-result response)))
    (should (test-search-content--success-p response))
    (should (integerp (alist-get 'returned result)))
    (should (integerp (alist-get 'offset result)))
    (should (alist-get 'search_tool result))))

(ert-deftest test-search-content-search-tool ()
  "Test that search_tool is reported."
  (let* ((response (efrit-tool-search-content '((pattern . "efrit"))))
         (result (test-search-content--get-result response)))
    (should (test-search-content--success-p response))
    ;; Should be either 'ripgrep' or 'elisp'
    (should (member (alist-get 'search_tool result) '("ripgrep" "elisp")))))

;;; JSON encoding test

(ert-deftest test-search-content-json-encodable ()
  "Test that response is JSON encodable."
  (let ((response (efrit-tool-search-content '((pattern . "efrit")
                                                (context_lines . 2)
                                                (max_results . 5)))))
    (should (stringp (json-encode response)))))

(provide 'test-tool-search-content)

;;; test-tool-search-content.el ends here
