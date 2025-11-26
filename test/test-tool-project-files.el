;;; test-tool-project-files.el --- Tests for project_files tool -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;;; Commentary:
;; Tests for efrit-tool-project-files project file listing functionality.
;; These tests run against the actual efrit repository.

;;; Code:

(require 'ert)
(require 'json)

;; Add load paths for test
(add-to-list 'load-path (expand-file-name "../lisp" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../lisp/core" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../lisp/tools" (file-name-directory load-file-name)))

(require 'efrit-tool-utils)
(require 'efrit-tool-project-files)

;;; Helper functions

(defun test-project-files--get-result (response)
  "Extract result from RESPONSE alist."
  (alist-get 'result response))

(defun test-project-files--success-p (response)
  "Check if RESPONSE indicates success."
  (eq (alist-get 'success response) t))

(defun test-project-files--get-files (response)
  "Extract files array from RESPONSE."
  (alist-get 'files (test-project-files--get-result response)))

;;; Basic functionality tests

(ert-deftest test-project-files-basic ()
  "Test basic project file listing."
  (let ((response (efrit-tool-project-files '())))
    (should (test-project-files--success-p response))
    (let ((result (test-project-files--get-result response)))
      ;; Should have expected fields
      (should (alist-get 'root result))
      (should (alist-get 'total_files result))
      (should (>= (alist-get 'total_files result) 1))
      (should (vectorp (alist-get 'files result))))))

(ert-deftest test-project-files-returns-files ()
  "Test that actual files are returned."
  (let* ((response (efrit-tool-project-files '()))
         (files (test-project-files--get-files response)))
    (should (test-project-files--success-p response))
    (should (> (length files) 0))
    ;; Check first file has expected structure
    (let ((first-file (aref files 0)))
      (should (alist-get 'path first-file))
      (should (alist-get 'path_relative first-file))
      (should (alist-get 'size first-file))
      (should (alist-get 'mtime first-file)))))

(ert-deftest test-project-files-file-metadata ()
  "Test that file metadata is correct."
  (let* ((response (efrit-tool-project-files '()))
         (files (test-project-files--get-files response)))
    (should (test-project-files--success-p response))
    ;; Find a known file
    (let ((known-file (seq-find
                       (lambda (f)
                         (string-suffix-p "CLAUDE.md" (alist-get 'path_relative f)))
                       files)))
      (when known-file
        ;; Size should be positive
        (should (> (alist-get 'size known-file) 0))
        ;; mtime should be ISO format
        (should (string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T"
                                (alist-get 'mtime known-file)))))))

;;; Pattern filtering tests

(ert-deftest test-project-files-pattern-el ()
  "Test filtering by *.el pattern."
  (let* ((response (efrit-tool-project-files '((pattern . "*.el"))))
         (files (test-project-files--get-files response)))
    (should (test-project-files--success-p response))
    (should (> (length files) 0))
    ;; All returned files should be .el files
    (seq-every-p (lambda (f)
                   (string-suffix-p ".el" (alist-get 'path_relative f)))
                 files)))

(ert-deftest test-project-files-pattern-md ()
  "Test filtering by *.md pattern."
  (let* ((response (efrit-tool-project-files '((pattern . "*.md"))))
         (files (test-project-files--get-files response)))
    (should (test-project-files--success-p response))
    (should (> (length files) 0))
    ;; All returned files should be .md files
    (should (seq-every-p (lambda (f)
                           (string-suffix-p ".md" (alist-get 'path_relative f)))
                         files))))

(ert-deftest test-project-files-pattern-no-match ()
  "Test pattern that matches nothing."
  (let* ((response (efrit-tool-project-files '((pattern . "*.xyz-nonexistent"))))
         (files (test-project-files--get-files response)))
    (should (test-project-files--success-p response))
    (should (= (length files) 0))))

;;; Pagination tests

(ert-deftest test-project-files-max-files ()
  "Test max_files limit."
  (let* ((response (efrit-tool-project-files '((max_files . 5))))
         (result (test-project-files--get-result response))
         (files (alist-get 'files result)))
    (should (test-project-files--success-p response))
    (should (<= (length files) 5))
    (should (= (alist-get 'returned result) (length files)))))

(ert-deftest test-project-files-offset ()
  "Test pagination with offset."
  (let* ((response1 (efrit-tool-project-files '((max_files . 3))))
         (response2 (efrit-tool-project-files '((max_files . 3) (offset . 3))))
         (files1 (test-project-files--get-files response1))
         (files2 (test-project-files--get-files response2)))
    (should (test-project-files--success-p response1))
    (should (test-project-files--success-p response2))
    ;; Files should be different
    (when (and (> (length files1) 0) (> (length files2) 0))
      (should-not (equal (alist-get 'path (aref files1 0))
                         (alist-get 'path (aref files2 0)))))))

(ert-deftest test-project-files-truncated-indicator ()
  "Test that truncated indicator works."
  (let* ((response (efrit-tool-project-files '((max_files . 2))))
         (result (test-project-files--get-result response)))
    (should (test-project-files--success-p response))
    ;; If total > returned, truncated should be true
    (when (> (alist-get 'total_files result) (alist-get 'returned result))
      (should (eq (alist-get 'truncated result) t))
      (should (alist-get 'continuation_hint result)))))

;;; Hidden files tests

(ert-deftest test-project-files-hidden-excluded ()
  "Test that hidden files are excluded by default."
  (let* ((response (efrit-tool-project-files '()))
         (files (test-project-files--get-files response)))
    (should (test-project-files--success-p response))
    ;; No files should start with . or contain /.
    (should (seq-every-p (lambda (f)
                           (let ((rel (alist-get 'path_relative f)))
                             (and (not (string-prefix-p "." rel))
                                  (not (string-match-p "/\\." rel)))))
                         files))))

(ert-deftest test-project-files-include-hidden ()
  "Test include_hidden option."
  (let* ((response-default (efrit-tool-project-files '()))
         (response-hidden (efrit-tool-project-files '((include_hidden . t))))
         (total-default (alist-get 'total_files (test-project-files--get-result response-default)))
         (total-hidden (alist-get 'total_files (test-project-files--get-result response-hidden))))
    (should (test-project-files--success-p response-default))
    (should (test-project-files--success-p response-hidden))
    ;; Hidden should include more or equal files
    (should (>= total-hidden total-default))))

;;; Result metadata tests

(ert-deftest test-project-files-metadata ()
  "Test that result includes correct metadata."
  (let* ((response (efrit-tool-project-files '()))
         (result (test-project-files--get-result response)))
    (should (test-project-files--success-p response))
    (should (stringp (alist-get 'root result)))
    (should (stringp (alist-get 'project_type result)))
    (should (integerp (alist-get 'total_files result)))
    (should (integerp (alist-get 'returned result)))
    (should (integerp (alist-get 'offset result)))))

(ert-deftest test-project-files-git-project ()
  "Test that git project is detected correctly."
  (let* ((response (efrit-tool-project-files '()))
         (result (test-project-files--get-result response)))
    (should (test-project-files--success-p response))
    ;; efrit is a git repo
    (should (equal (alist-get 'project_type result) "git"))
    (should (eq (alist-get 'gitignore_respected result) t))))

;;; Error handling tests

(ert-deftest test-project-files-nonexistent-path ()
  "Test error when path doesn't exist."
  (let ((response (efrit-tool-project-files '((path . "/nonexistent/path/xyz")))))
    (should-not (test-project-files--success-p response))
    (should (alist-get 'error response))))

;;; JSON encoding test

(ert-deftest test-project-files-json-encodable ()
  "Test that response is JSON encodable."
  (let ((response (efrit-tool-project-files '((max_files . 10)))))
    (should (stringp (json-encode response)))))

(provide 'test-tool-project-files)

;;; test-tool-project-files.el ends here
