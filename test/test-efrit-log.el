;;; test-efrit-log.el --- Tests for efrit-log.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;;; Commentary:
;; Unit tests for the unified logging system.

;;; Code:

(require 'ert)

;; Add load paths
(add-to-list 'load-path (expand-file-name "../lisp" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../lisp/core" (file-name-directory load-file-name)))

(require 'efrit-log)

;;; Level Enabled Tests

(ert-deftest test-log-level-enabled-debug ()
  "Test debug level enabling."
  (let ((efrit-log-level 'debug))
    (should (efrit-log--level-enabled-p 'debug))
    (should (efrit-log--level-enabled-p 'info))
    (should (efrit-log--level-enabled-p 'warn))
    (should (efrit-log--level-enabled-p 'error))))

(ert-deftest test-log-level-enabled-info ()
  "Test info level filtering."
  (let ((efrit-log-level 'info))
    (should-not (efrit-log--level-enabled-p 'debug))
    (should (efrit-log--level-enabled-p 'info))
    (should (efrit-log--level-enabled-p 'warn))
    (should (efrit-log--level-enabled-p 'error))))

(ert-deftest test-log-level-enabled-warn ()
  "Test warn level filtering."
  (let ((efrit-log-level 'warn))
    (should-not (efrit-log--level-enabled-p 'debug))
    (should-not (efrit-log--level-enabled-p 'info))
    (should (efrit-log--level-enabled-p 'warn))
    (should (efrit-log--level-enabled-p 'error))))

(ert-deftest test-log-level-enabled-error ()
  "Test error level filtering."
  (let ((efrit-log-level 'error))
    (should-not (efrit-log--level-enabled-p 'debug))
    (should-not (efrit-log--level-enabled-p 'info))
    (should-not (efrit-log--level-enabled-p 'warn))
    (should (efrit-log--level-enabled-p 'error))))

(ert-deftest test-log-level-enabled-none ()
  "Test that none level disables all logging."
  (let ((efrit-log-level 'none))
    (should-not (efrit-log--level-enabled-p 'debug))
    (should-not (efrit-log--level-enabled-p 'info))
    (should-not (efrit-log--level-enabled-p 'warn))
    (should-not (efrit-log--level-enabled-p 'error))))

;;; Level Echo Tests

(ert-deftest test-log-level-echo-warn ()
  "Test echo level filtering at warn."
  (let ((efrit-log-echo-level 'warn))
    (should-not (efrit-log--level-echo-p 'debug))
    (should-not (efrit-log--level-echo-p 'info))
    (should (efrit-log--level-echo-p 'warn))
    (should (efrit-log--level-echo-p 'error))))

;;; Logging Tests

(ert-deftest test-log-basic-message ()
  "Test basic logging to buffer."
  (let ((efrit-log-level 'debug)
        (efrit-log-echo-level 'none)  ; Don't echo to messages
        (efrit-log-buffer "*efrit-test-log*"))
    (when (get-buffer efrit-log-buffer)
      (kill-buffer efrit-log-buffer))
    (efrit-log 'info "Test message")
    (with-current-buffer efrit-log-buffer
      (should (string-match-p "INFO" (buffer-string)))
      (should (string-match-p "Test message" (buffer-string))))
    (kill-buffer efrit-log-buffer)))

(ert-deftest test-log-with-format-args ()
  "Test logging with format arguments."
  (let ((efrit-log-level 'debug)
        (efrit-log-echo-level 'none)
        (efrit-log-buffer "*efrit-test-log*"))
    (when (get-buffer efrit-log-buffer)
      (kill-buffer efrit-log-buffer))
    (efrit-log 'info "Count: %d, Name: %s" 42 "test")
    (with-current-buffer efrit-log-buffer
      (should (string-match-p "Count: 42" (buffer-string)))
      (should (string-match-p "Name: test" (buffer-string))))
    (kill-buffer efrit-log-buffer)))

(ert-deftest test-log-filtered-by-level ()
  "Test that messages below log level are filtered."
  (let ((efrit-log-level 'warn)
        (efrit-log-echo-level 'none)
        (efrit-log-buffer "*efrit-test-log*"))
    (when (get-buffer efrit-log-buffer)
      (kill-buffer efrit-log-buffer))
    (efrit-log 'debug "Debug message")
    (efrit-log 'info "Info message")
    (efrit-log 'warn "Warning message")
    (with-current-buffer efrit-log-buffer
      (should-not (string-match-p "Debug message" (buffer-string)))
      (should-not (string-match-p "Info message" (buffer-string)))
      (should (string-match-p "Warning message" (buffer-string))))
    (kill-buffer efrit-log-buffer)))

(ert-deftest test-log-buffer-line-limit ()
  "Test that buffer respects max lines limit."
  (let ((efrit-log-level 'debug)
        (efrit-log-echo-level 'none)
        (efrit-log-max-lines 10)
        (efrit-log-buffer "*efrit-test-log*"))
    (when (get-buffer efrit-log-buffer)
      (kill-buffer efrit-log-buffer))
    ;; Log 15 messages (more than limit)
    (dotimes (i 15)
      (efrit-log 'info "Message %d" i))
    (with-current-buffer efrit-log-buffer
      ;; Buffer should have been trimmed
      (should (<= (count-lines (point-min) (point-max)) 10)))
    (kill-buffer efrit-log-buffer)))

;;; Convenience Function Tests

(ert-deftest test-log-debug-function ()
  "Test efrit-log-debug convenience function."
  (let ((efrit-log-level 'debug)
        (efrit-log-echo-level 'none)
        (efrit-log-buffer "*efrit-test-log*"))
    (when (get-buffer efrit-log-buffer)
      (kill-buffer efrit-log-buffer))
    (efrit-log-debug "Debug test")
    (with-current-buffer efrit-log-buffer
      (should (string-match-p "DEBUG" (buffer-string))))
    (kill-buffer efrit-log-buffer)))

(ert-deftest test-log-error-function ()
  "Test efrit-log-error convenience function."
  (let ((efrit-log-level 'debug)
        (efrit-log-echo-level 'none)
        (efrit-log-buffer "*efrit-test-log*"))
    (when (get-buffer efrit-log-buffer)
      (kill-buffer efrit-log-buffer))
    (efrit-log-error "Error test")
    (with-current-buffer efrit-log-buffer
      (should (string-match-p "ERROR" (buffer-string))))
    (kill-buffer efrit-log-buffer)))

(ert-deftest test-log-section ()
  "Test efrit-log-section separator."
  (let ((efrit-log-level 'debug)
        (efrit-log-echo-level 'none)
        (efrit-log-buffer "*efrit-test-log*"))
    (when (get-buffer efrit-log-buffer)
      (kill-buffer efrit-log-buffer))
    (efrit-log-section "Test Section")
    (with-current-buffer efrit-log-buffer
      (should (string-match-p "=== Test Section ===" (buffer-string))))
    (kill-buffer efrit-log-buffer)))

;;; Edge Cases

(ert-deftest test-log-percent-in-message ()
  "Test that percent signs don't cause format errors."
  (let ((efrit-log-level 'debug)
        (efrit-log-echo-level 'none)
        (efrit-log-buffer "*efrit-test-log*"))
    (when (get-buffer efrit-log-buffer)
      (kill-buffer efrit-log-buffer))
    ;; This should not error
    (efrit-log 'info "Progress: 50%% complete")
    (efrit-log 'info "Literal percent: %s" "100%")
    (with-current-buffer efrit-log-buffer
      (should (string-match-p "50%" (buffer-string)))
      (should (string-match-p "100%" (buffer-string))))
    (kill-buffer efrit-log-buffer)))

(provide 'test-efrit-log)
;;; test-efrit-log.el ends here
