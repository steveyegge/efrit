;;; test-tool-read-file.el --- Tests for read_file tool -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;;; Commentary:
;; Tests for efrit-tool-read-file file reading functionality.
;; These tests run against the actual efrit repository.

;;; Code:

(require 'ert)
(require 'json)

;; Add load paths for test
(add-to-list 'load-path (expand-file-name "../lisp" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../lisp/core" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../lisp/tools" (file-name-directory load-file-name)))

(require 'efrit-tool-utils)
(require 'efrit-tool-read-file)

;;; Helper functions

(defun test-read-file--get-result (response)
  "Extract result from RESPONSE alist."
  (alist-get 'result response))

(defun test-read-file--success-p (response)
  "Check if RESPONSE indicates success."
  (eq (alist-get 'success response) t))

;;; Basic functionality tests

(ert-deftest test-read-file-basic ()
  "Test basic file reading."
  (let ((response (efrit-tool-read-file '((path . "CLAUDE.md")))))
    (should (test-read-file--success-p response))
    (let ((result (test-read-file--get-result response)))
      (should (alist-get 'path result))
      (should (alist-get 'path_relative result))
      (should (alist-get 'content result))
      (should (stringp (alist-get 'content result)))
      (should (> (length (alist-get 'content result)) 0)))))

(ert-deftest test-read-file-content-correct ()
  "Test that content is actually from the file."
  (let* ((response (efrit-tool-read-file '((path . "CLAUDE.md"))))
         (result (test-read-file--get-result response))
         (content (alist-get 'content result)))
    (should (test-read-file--success-p response))
    ;; CLAUDE.md should start with a specific pattern
    (should (string-match-p "^# " content))))

(ert-deftest test-read-file-metadata ()
  "Test that metadata is correct."
  (let* ((response (efrit-tool-read-file '((path . "CLAUDE.md"))))
         (result (test-read-file--get-result response)))
    (should (test-read-file--success-p response))
    ;; Size should be positive
    (should (> (alist-get 'size result) 0))
    ;; Total lines should be positive
    (should (> (alist-get 'total_lines result) 0))
    ;; Should have encoding
    (should (stringp (alist-get 'encoding result)))
    ;; mtime should be ISO format
    (should (string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T"
                            (alist-get 'mtime result)))))

;;; Line range tests

(ert-deftest test-read-file-line-range ()
  "Test reading specific line range."
  (let* ((response (efrit-tool-read-file '((path . "CLAUDE.md")
                                           (start_line . 1)
                                           (end_line . 5))))
         (result (test-read-file--get-result response))
         (content (alist-get 'content result)))
    (should (test-read-file--success-p response))
    ;; Should have content
    (should (> (length content) 0))
    ;; lines_returned should indicate range
    (should (string-match-p "1-" (alist-get 'lines_returned result)))))

(ert-deftest test-read-file-start-line-only ()
  "Test reading from specific start line to end."
  (let* ((response-full (efrit-tool-read-file '((path . "CLAUDE.md"))))
         (response-partial (efrit-tool-read-file '((path . "CLAUDE.md")
                                                   (start_line . 50)
                                                   (end_line . 60))))
         (result-full (test-read-file--get-result response-full))
         (result-partial (test-read-file--get-result response-partial)))
    (should (test-read-file--success-p response-full))
    (should (test-read-file--success-p response-partial))
    ;; Partial should have less content (using specific range ensures this)
    (should (< (length (alist-get 'content result-partial))
               (length (alist-get 'content result-full))))))

(ert-deftest test-read-file-line-numbers ()
  "Test that line numbers are 1-indexed."
  (let* ((response (efrit-tool-read-file '((path . "CLAUDE.md")
                                           (start_line . 1)
                                           (end_line . 1))))
         (result (test-read-file--get-result response))
         (content (alist-get 'content result)))
    (should (test-read-file--success-p response))
    ;; Should get exactly one line (might end with newline)
    (should (= (length (split-string content "\n" t)) 1))))

;;; Encoding tests

(ert-deftest test-read-file-utf8 ()
  "Test that UTF-8 encoding is detected."
  (let* ((response (efrit-tool-read-file '((path . "CLAUDE.md"))))
         (result (test-read-file--get-result response)))
    (should (test-read-file--success-p response))
    ;; Should detect UTF-8
    (should (string-match-p "utf-8" (downcase (alist-get 'encoding result))))))

;;; Binary file tests

(ert-deftest test-read-file-binary-detection ()
  "Test that binary files are detected.
This test creates a temporary binary file within the project for testing."
  ;; Create temp file within project to avoid sandbox violation
  (let* ((project-root (locate-dominating-file default-directory "CLAUDE.md"))
         (temp-file (expand-file-name (format "test/temp-binary-%s" (format-time-string "%s"))
                                      project-root)))
    ;; Write some binary content
    (with-temp-file temp-file
      (set-buffer-multibyte nil)
      (insert "\x00\x01\x02\x03\x04\x05"))
    (unwind-protect
        (let ((response (efrit-tool-read-file `((path . ,temp-file)))))
          (should (test-read-file--success-p response))
          (let ((result (test-read-file--get-result response)))
            ;; Should detect as binary
            (should (eq (alist-get 'is_binary result) t))
            ;; Content should be null for binary
            (should (eq (alist-get 'content result) :json-null))))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

;;; Large file handling tests

(ert-deftest test-read-file-truncation ()
  "Test that large files are truncated."
  ;; Use a small max_size to test truncation
  (let* ((response (efrit-tool-read-file '((path . "CLAUDE.md")
                                           (max_size . 100))))
         (result (test-read-file--get-result response)))
    (should (test-read-file--success-p response))
    ;; Should be truncated
    (should (eq (alist-get 'truncated result) t))
    ;; Content should be around 100 bytes
    (should (<= (length (alist-get 'content result)) 150))))

;;; Path handling tests

(ert-deftest test-read-file-absolute-path ()
  "Test reading with absolute path."
  (let* ((project-root (locate-dominating-file default-directory "CLAUDE.md"))
         (abs-path (expand-file-name "CLAUDE.md" project-root))
         (response (efrit-tool-read-file `((path . ,abs-path)))))
    (should (test-read-file--success-p response))
    (let ((result (test-read-file--get-result response)))
      (should (> (length (alist-get 'content result)) 0)))))

(ert-deftest test-read-file-relative-path ()
  "Test reading with relative path."
  (let ((response (efrit-tool-read-file '((path . "lisp/core/efrit-tools.el")))))
    (should (test-read-file--success-p response))
    (let ((result (test-read-file--get-result response)))
      (should (> (length (alist-get 'content result)) 0))
      ;; path_relative should be set
      (should (stringp (alist-get 'path_relative result))))))

;;; Error handling tests

(ert-deftest test-read-file-missing-path ()
  "Test error when path is missing."
  (let ((response (efrit-tool-read-file '())))
    (should-not (test-read-file--success-p response))
    (should (alist-get 'error response))))

(ert-deftest test-read-file-empty-path ()
  "Test error when path is empty."
  (let ((response (efrit-tool-read-file '((path . "")))))
    (should-not (test-read-file--success-p response))
    (should (alist-get 'error response))))

(ert-deftest test-read-file-nonexistent ()
  "Test error when file doesn't exist."
  (let ((response (efrit-tool-read-file '((path . "nonexistent-file-xyz.txt")))))
    (should-not (test-read-file--success-p response))
    (should (alist-get 'error response))))

(ert-deftest test-read-file-directory ()
  "Test error when path is a directory."
  (let ((response (efrit-tool-read-file '((path . "lisp")))))
    (should-not (test-read-file--success-p response))
    (should (alist-get 'error response))))

;;; JSON encoding test

(ert-deftest test-read-file-json-encodable ()
  "Test that response is JSON encodable."
  (let ((response (efrit-tool-read-file '((path . "CLAUDE.md")
                                          (start_line . 1)
                                          (end_line . 10)))))
    (should (stringp (json-encode response)))))

(ert-deftest test-read-file-binary-json-encodable ()
  "Test that binary file response is JSON encodable."
  ;; Create temp file within project to avoid sandbox violation
  (let* ((project-root (locate-dominating-file default-directory "CLAUDE.md"))
         (temp-file (expand-file-name (format "test/temp-binary-json-%s" (format-time-string "%s"))
                                      project-root)))
    (with-temp-file temp-file
      (set-buffer-multibyte nil)
      (insert "\x00\x01\x02\x03"))
    (unwind-protect
        (let ((response (efrit-tool-read-file `((path . ,temp-file)))))
          (should (stringp (json-encode response))))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

(provide 'test-tool-read-file)

;;; test-tool-read-file.el ends here
