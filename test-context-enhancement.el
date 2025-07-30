;;; test-context-enhancement.el --- Tests for error context enhancement -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Keywords: test

;;; Commentary:
;; Tests for the error context enhancement implementation in efrit-do.el

;;; Code:

(require 'ert)
(require 'efrit-do)

;; Test helper functions

(defun test-context--setup ()
  "Set up test environment for context tests."
  (setq efrit-do-debug t)
  (setq efrit-do-show-results nil)) ; Don't show buffers during tests

(defun test-context--cleanup ()
  "Clean up after context tests."
  (setq efrit-do-debug nil)
  (setq efrit-do-show-results t))

(defun test-context--create-test-buffer (name content &optional mode)
  "Create a test buffer NAME with CONTENT and optional MODE."
  (with-current-buffer (get-buffer-create name)
    (erase-buffer)
    (insert content)
    (when mode
      (funcall mode))
    (goto-char (point-min))
    (current-buffer)))

;; Test context generation functions

(ert-deftest test-context-build-basic-info ()
  "Test that basic context information is captured correctly."
  (test-context--setup)
  (let ((test-buffer (test-context--create-test-buffer 
                     "*test-context*" 
                     "Line 1\nLine 2\nLine 3\nLine 4\nLine 5")))
    (with-current-buffer test-buffer
      (goto-char 15) ; Position in middle of content
      (let ((context (efrit-do--build-error-context)))
        (should (string-match "CURRENT BUFFER: \\*test-context\\*" context))
        (should (string-match "mode: fundamental-mode" context))
        (should (string-match "point: 15/" context))
        (should (string-match "CURRENT DIRECTORY:" context))
        (should (string-match "WINDOW LAYOUT:" context)))))
  (test-context--cleanup))

(ert-deftest test-context-buffer-content-snippet ()
  "Test that buffer content around point is captured correctly."
  (test-context--setup)
  (let ((test-content "Line 1\nLine 2\nThis is the target line\nLine 4\nLine 5"))
    (let ((test-buffer (test-context--create-test-buffer 
                       "*test-content*" 
                       test-content)))
      (with-current-buffer test-buffer
        (goto-char (string-match "target line" test-content))
        (let ((context (efrit-do--build-error-context)))
          (should (string-match "BUFFER CONTENT AROUND POINT:" context))
          (should (string-match "target line" context))))))
  (test-context--cleanup))

(ert-deftest test-context-visible-buffers ()
  "Test that visible buffers are captured correctly."
  (test-context--setup)
  (let ((test-buffer1 (test-context--create-test-buffer "*test1*" "content1"))
        (test-buffer2 (test-context--create-test-buffer "*test2*" "content2")))
    ;; Make sure buffers are visible in windows
    (switch-to-buffer test-buffer1)
    (split-window)
    (other-window 1)
    (switch-to-buffer test-buffer2)
    (other-window 1) ; Back to first buffer
    
    (let ((context (efrit-do--build-error-context)))
      (should (string-match "VISIBLE BUFFERS:" context))
      (should (string-match "\\*test1\\*" context))
      (should (string-match "\\*test2\\*" context))
      (should (string-match "WINDOW LAYOUT: 2 windows" context)))
    
    ;; Clean up windows
    (delete-other-windows))
  (test-context--cleanup))

(ert-deftest test-context-recent-commands ()
  "Test that recent command history is included in context."
  (test-context--setup)
  (efrit-do--clear-context) ; Start with clean context
  
  ;; Add some fake context items
  (efrit-do--capture-context "test command 1" "test result 1")
  (efrit-do--capture-context "test command 2" "test result 2")
  
  (let ((context (efrit-do--build-error-context)))
    (should (string-match "RECENT COMMANDS:" context))
    (should (string-match "test command" context))
    (should (string-match "test result" context)))
  
  (test-context--cleanup))

(ert-deftest test-context-empty-buffer ()
  "Test context generation with empty buffer."
  (test-context--setup)
  (let ((test-buffer (test-context--create-test-buffer "*empty*" "")))
    (with-current-buffer test-buffer
      (let ((context (efrit-do--build-error-context)))
        (should (string-match "CURRENT BUFFER: \\*empty\\*" context))
        (should (string-match "point: 1/0" context)) ; Empty buffer
        (should-not (string-match "BUFFER CONTENT AROUND POINT:" context)))))
  (test-context--cleanup))

(ert-deftest test-context-long-content-truncation ()
  "Test that long buffer content is properly truncated."
  (test-context--setup)
  ;; Create a buffer with enough lines around point to trigger truncation
  (let* ((lines-before (make-list 10 "Line before point"))
         (lines-after (make-list 10 "Line after point"))
         (all-lines (append lines-before '("TARGET LINE AT POINT") lines-after))
         (long-content (string-join all-lines "\n")))
    (let ((test-buffer (test-context--create-test-buffer "*long*" long-content)))
      (with-current-buffer test-buffer
        ;; Position at the beginning to get many lines in context window
        (goto-char (point-min))
        (let ((context (efrit-do--build-error-context)))
          (should (string-match "BUFFER CONTENT AROUND POINT:" context))
          ;; The content should include truncation since we have >10 lines
          (let ((content-section (substring context 
                                           (string-match "BUFFER CONTENT AROUND POINT:" context))))
            (should (string-match "\\.\\.\\." content-section))
            (should (string-match "Line before point" content-section)))))))
  (test-context--cleanup))

;; Test integration with retry system

(ert-deftest test-context-integration-with-retry-prompt ()
  "Test that context is properly integrated into retry system prompts."
  (test-context--setup)
  (let ((test-buffer (test-context--create-test-buffer 
                     "*retry-test*" 
                     "Test content for retry")))
    (with-current-buffer test-buffer
      (let ((prompt (efrit-do--command-system-prompt 2 "Test error" "(test-code)")))
        (should (string-match "RETRY ATTEMPT 2/" prompt))
        (should (string-match "Previous code that failed: (test-code)" prompt))
        (should (string-match "Error encountered: Test error" prompt))
        (should (string-match "CURRENT EMACS STATE:" prompt))
        (should (string-match "CURRENT BUFFER: \\*retry-test\\*" prompt))
        (should (string-match "Test content for retry" prompt)))))
  (test-context--cleanup))

(ert-deftest test-context-normal-prompt-no-retry-info ()
  "Test that normal prompts don't include retry or context information."
  (test-context--setup)
  (let ((prompt (efrit-do--command-system-prompt)))
    (should-not (string-match "RETRY ATTEMPT" prompt))
    (should-not (string-match "CURRENT EMACS STATE" prompt))
    (should-not (string-match "Previous code that failed" prompt)))
  (test-context--cleanup))

(ert-deftest test-context-error-handling ()
  "Test that context building handles errors gracefully."
  (test-context--setup)
  ;; Test with a corrupted context ring or other error conditions
  (let ((efrit-do--context-ring nil)) ; Simulate missing context ring
    (let ((context (efrit-do--build-error-context)))
      (should (stringp context)) ; Should still return a string
      (should (string-match "CURRENT BUFFER:" context)))) ; Basic info should still work
  (test-context--cleanup))

;; Test different buffer modes

(ert-deftest test-context-with-different-modes ()
  "Test context generation with different major modes."
  (test-context--setup)
  ;; Test with emacs-lisp-mode
  (let ((test-buffer (test-context--create-test-buffer 
                     "*lisp-test*" 
                     "(defun test-func () 'test)"
                     'emacs-lisp-mode)))
    (with-current-buffer test-buffer
      (let ((context (efrit-do--build-error-context)))
        (should (string-match "mode: emacs-lisp-mode" context))
        (should (string-match "defun test-func" context)))))
  (test-context--cleanup))

;; Run all tests

(defun test-context-run-all ()
  "Run all context enhancement tests."
  (interactive)
  (ert-run-tests-batch "^test-context.*"))

(provide 'test-context-enhancement)

;;; test-context-enhancement.el ends here
