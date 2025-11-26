;;; test-tool-vcs-blame.el --- Tests for vcs_blame tool -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;;; Commentary:
;; Tests for efrit-tool-vcs-blame git blame functionality.
;; Note: These tests require running in a git repository.

;;; Code:

(require 'ert)
(require 'json)

;; Add load paths for test
(add-to-list 'load-path (expand-file-name "../lisp" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../lisp/core" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../lisp/tools" (file-name-directory load-file-name)))

(require 'efrit-tool-utils)
(require 'efrit-tool-vcs-blame)

;;; Helper functions

(defun test-vcs-blame--get-result (response)
  "Extract result from RESPONSE alist."
  (alist-get 'result response))

(defun test-vcs-blame--success-p (response)
  "Check if RESPONSE indicates success."
  (eq (alist-get 'success response) t))

(defun test-vcs-blame--get-lines (response)
  "Extract lines array from RESPONSE."
  (alist-get 'lines (test-vcs-blame--get-result response)))

;;; Basic functionality tests

(ert-deftest test-vcs-blame-basic ()
  "Test basic blame on a tracked file."
  ;; Use CLAUDE.md which should exist in this repo
  (let ((response (efrit-tool-vcs-blame '((path . "CLAUDE.md")
                                          (start_line . 1)
                                          (end_line . 3)))))
    (should (test-vcs-blame--success-p response))
    (let ((lines (test-vcs-blame--get-lines response)))
      (should (= (length lines) 3))
      ;; Each line should have expected fields
      (let ((first-line (aref lines 0)))
        (should (alist-get 'line_number first-line))
        (should (alist-get 'commit first-line))
        (should (alist-get 'content first-line))
        ;; First line in a commit group has full metadata
        (should (alist-get 'author first-line))
        (should (alist-get 'date first-line))))))

(ert-deftest test-vcs-blame-line-numbers ()
  "Test that line numbers are correct."
  (let ((response (efrit-tool-vcs-blame '((path . "CLAUDE.md")
                                          (start_line . 5)
                                          (end_line . 7)))))
    (should (test-vcs-blame--success-p response))
    (let ((lines (test-vcs-blame--get-lines response)))
      (should (= (length lines) 3))
      (should (= (alist-get 'line_number (aref lines 0)) 5))
      (should (= (alist-get 'line_number (aref lines 1)) 6))
      (should (= (alist-get 'line_number (aref lines 2)) 7)))))

(ert-deftest test-vcs-blame-commit-hash-format ()
  "Test that commit hashes have correct format."
  (let ((response (efrit-tool-vcs-blame '((path . "CLAUDE.md")
                                          (start_line . 1)
                                          (end_line . 1)))))
    (should (test-vcs-blame--success-p response))
    (let* ((lines (test-vcs-blame--get-lines response))
           (first-line (aref lines 0)))
      ;; Short hash should be 8 chars
      (should (= (length (alist-get 'commit first-line)) 8))
      ;; Full hash should be 40 chars
      (should (= (length (alist-get 'full_commit first-line)) 40))
      ;; Both should be hex
      (should (string-match-p "^[0-9a-f]+$" (alist-get 'commit first-line)))
      (should (string-match-p "^[0-9a-f]+$" (alist-get 'full_commit first-line))))))

(ert-deftest test-vcs-blame-date-format ()
  "Test that dates are in ISO 8601 format."
  (let ((response (efrit-tool-vcs-blame '((path . "CLAUDE.md")
                                          (start_line . 1)
                                          (end_line . 1)))))
    (should (test-vcs-blame--success-p response))
    (let* ((lines (test-vcs-blame--get-lines response))
           (first-line (aref lines 0))
           (date (alist-get 'date first-line)))
      ;; Should be ISO 8601 format
      (should (string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T" date)))))

;;; Error handling tests

(ert-deftest test-vcs-blame-missing-path ()
  "Test error when path is missing."
  (let ((response (efrit-tool-vcs-blame '())))
    (should-not (test-vcs-blame--success-p response))
    (should (alist-get 'error response))))

(ert-deftest test-vcs-blame-nonexistent-file ()
  "Test error when file doesn't exist."
  (let ((response (efrit-tool-vcs-blame '((path . "this-file-does-not-exist.txt")))))
    (should-not (test-vcs-blame--success-p response))
    (should (alist-get 'error response))))

(ert-deftest test-vcs-blame-invalid-line-range ()
  "Test error when start_line > end_line."
  (let ((response (efrit-tool-vcs-blame '((path . "CLAUDE.md")
                                          (start_line . 10)
                                          (end_line . 5)))))
    (should-not (test-vcs-blame--success-p response))
    (should (alist-get 'error response))))

(ert-deftest test-vcs-blame-negative-line ()
  "Test error when line number is negative."
  (let ((response (efrit-tool-vcs-blame '((path . "CLAUDE.md")
                                          (start_line . -1)))))
    (should-not (test-vcs-blame--success-p response))
    (should (alist-get 'error response))))

;;; Result metadata tests

(ert-deftest test-vcs-blame-result-metadata ()
  "Test that result includes correct metadata."
  (let ((response (efrit-tool-vcs-blame '((path . "CLAUDE.md")
                                          (start_line . 1)
                                          (end_line . 5)))))
    (should (test-vcs-blame--success-p response))
    (let ((result (test-vcs-blame--get-result response)))
      (should (equal (alist-get 'file result) "CLAUDE.md"))
      (should (= (alist-get 'line_count result) 5))
      (should (= (alist-get 'start_line result) 1))
      (should (= (alist-get 'end_line result) 5)))))

(ert-deftest test-vcs-blame-whole-file ()
  "Test blaming without line range."
  ;; Use a small file
  (let ((response (efrit-tool-vcs-blame '((path . "Makefile")))))
    (should (test-vcs-blame--success-p response))
    (let ((result (test-vcs-blame--get-result response)))
      ;; Should have lines
      (should (> (alist-get 'line_count result) 0))
      ;; No start/end_line in result since not specified
      (should-not (alist-get 'start_line result))
      (should-not (alist-get 'end_line result)))))

(ert-deftest test-vcs-blame-start-only ()
  "Test blaming with only start_line (to end of file)."
  (let ((response (efrit-tool-vcs-blame '((path . "CLAUDE.md")
                                          (start_line . 100)))))
    (should (test-vcs-blame--success-p response))
    (let ((result (test-vcs-blame--get-result response)))
      (should (= (alist-get 'start_line result) 100))
      ;; Should have some lines from line 100 onwards
      (should (> (alist-get 'line_count result) 0)))))

;;; JSON encoding test

(ert-deftest test-vcs-blame-json-encodable ()
  "Test that response is JSON encodable."
  (let ((response (efrit-tool-vcs-blame '((path . "CLAUDE.md")
                                          (start_line . 1)
                                          (end_line . 5)))))
    (should (stringp (json-encode response)))))

(provide 'test-tool-vcs-blame)

;;; test-tool-vcs-blame.el ends here
