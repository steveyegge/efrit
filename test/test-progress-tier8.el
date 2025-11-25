;;; test-progress-tier8.el --- Tier 8: Progress streaming verification tests -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, testing

;;; Commentary:

;; Tier 8 automated tests for progress streaming functionality.
;; These tests verify:
;; - Progress file creation and format (JSONL)
;; - Event emission and structure
;; - Session lifecycle tracking
;; - Tool call progress reporting
;; - Buffer display functionality
;; - Injection file handling
;;
;; These are unit tests that don't require a running daemon.

;;; Code:

(require 'ert)
(require 'json)
(require 'efrit-progress)
(require 'efrit-config)

;;; Test helper functions

(defvar test-tier8--temp-dir nil
  "Temporary directory for test files.")

(defun test-tier8--setup ()
  "Setup test environment."
  (setq test-tier8--temp-dir (make-temp-file "efrit-progress-test" t))
  ;; Override the efrit data directory for testing
  (setq efrit-data-directory test-tier8--temp-dir)
  ;; Ensure sessions subdirectory exists
  (make-directory (expand-file-name "sessions" test-tier8--temp-dir) t))

(defun test-tier8--teardown ()
  "Cleanup test environment."
  (when (and test-tier8--temp-dir (file-directory-p test-tier8--temp-dir))
    (delete-directory test-tier8--temp-dir t))
  (setq test-tier8--temp-dir nil)
  ;; Reset progress state
  (setq efrit-progress--current-session nil)
  (setq efrit-progress--current-file nil)
  (setq efrit-progress--event-counter 0))

(defun test-tier8--read-progress-file (session-id)
  "Read all events from progress file for SESSION-ID."
  (let* ((session-dir (expand-file-name session-id
                                        (expand-file-name "sessions" efrit-data-directory)))
         (progress-file (expand-file-name "progress.jsonl" session-dir)))
    (when (file-exists-p progress-file)
      (with-temp-buffer
        (insert-file-contents progress-file)
        (let ((events '())
              (json-object-type 'hash-table)
              (json-array-type 'vector)
              (json-key-type 'string))
          (goto-char (point-min))
          (while (not (eobp))
            (let ((line (buffer-substring-no-properties
                        (point) (line-end-position))))
              (unless (string-empty-p line)
                (push (json-read-from-string line) events)))
            (forward-line 1))
          (nreverse events))))))

;;; Session lifecycle tests

(ert-deftest test-tier8-session-start-creates-file ()
  "Test that starting a session creates the progress file."
  (test-tier8--setup)
  (unwind-protect
      (let ((session-id "test-session-001"))
        (efrit-progress-start-session session-id "test command")
        (let ((events (test-tier8--read-progress-file session-id)))
          (should (= 1 (length events)))
          (let ((event (car events)))
            (should (equal "session-start" (gethash "type" event)))
            (should (equal session-id (gethash "session" event)))
            (should (equal "test command" (gethash "command" event))))))
    (test-tier8--teardown)))

(ert-deftest test-tier8-session-end-emits-event ()
  "Test that ending a session emits the correct event."
  (test-tier8--setup)
  (unwind-protect
      (let ((session-id "test-session-002"))
        (efrit-progress-start-session session-id "test command")
        (efrit-progress-end-session session-id t)
        (let ((events (test-tier8--read-progress-file session-id)))
          (should (= 2 (length events)))
          (let ((end-event (cadr events)))
            (should (equal "session-end" (gethash "type" end-event)))
            (should (eq t (gethash "success" end-event))))))
    (test-tier8--teardown)))

(ert-deftest test-tier8-session-end-failure-flag ()
  "Test that session end correctly records failure status."
  (test-tier8--setup)
  (unwind-protect
      (let ((session-id "test-session-003"))
        (efrit-progress-start-session session-id "test command")
        (efrit-progress-end-session session-id nil)
        (let ((events (test-tier8--read-progress-file session-id)))
          (let ((end-event (cadr events)))
            (should (eq :json-false (gethash "success" end-event))))))
    (test-tier8--teardown)))

;;; Event structure tests

(ert-deftest test-tier8-event-has-timestamp ()
  "Test that all events have timestamps."
  (test-tier8--setup)
  (unwind-protect
      (let ((session-id "test-session-004"))
        (efrit-progress-start-session session-id "test command")
        (let ((events (test-tier8--read-progress-file session-id)))
          (dolist (event events)
            (should (gethash "timestamp" event))
            ;; Should be ISO 8601 format
            (should (string-match-p "^[0-9]\\{4\\}-" (gethash "timestamp" event))))))
    (test-tier8--teardown)))

(ert-deftest test-tier8-event-has-sequence ()
  "Test that events have incrementing sequence numbers."
  (test-tier8--setup)
  (unwind-protect
      (let ((session-id "test-session-005"))
        (efrit-progress-start-session session-id "test command")
        (efrit-progress-show-message "message 1")
        (efrit-progress-show-message "message 2")
        (efrit-progress-end-session session-id t)
        (let* ((events (test-tier8--read-progress-file session-id))
               (seqs (mapcar (lambda (e) (gethash "seq" e)) events)))
          ;; Sequences should be incrementing
          (should (equal seqs '(1 2 3 4)))))
    (test-tier8--teardown)))

;;; Tool progress tests

(ert-deftest test-tier8-tool-start-event ()
  "Test that tool start events are correctly emitted."
  (test-tier8--setup)
  (unwind-protect
      (let ((session-id "test-session-006"))
        (efrit-progress-start-session session-id "test command")
        (efrit-progress-show-tool-start "eval_sexp" '(("code" . "(+ 1 2)")))
        (let ((events (test-tier8--read-progress-file session-id)))
          (let ((tool-event (cadr events)))
            (should (equal "tool-start" (gethash "type" tool-event)))
            (should (equal "eval_sexp" (gethash "tool" tool-event)))
            (should (gethash "input" tool-event)))))
    (test-tier8--teardown)))

(ert-deftest test-tier8-tool-result-event ()
  "Test that tool result events are correctly emitted."
  (test-tier8--setup)
  (unwind-protect
      (let ((session-id "test-session-007"))
        (efrit-progress-start-session session-id "test command")
        (efrit-progress-show-tool-start "eval_sexp" '(("code" . "(+ 1 2)")))
        (efrit-progress-show-tool-result "eval_sexp" "3" t)
        (let ((events (test-tier8--read-progress-file session-id)))
          (let ((result-event (caddr events)))
            (should (equal "tool-result" (gethash "type" result-event)))
            (should (equal "eval_sexp" (gethash "tool" result-event)))
            (should (eq t (gethash "success" result-event)))
            (should (string-match-p "3" (gethash "result" result-event))))))
    (test-tier8--teardown)))

(ert-deftest test-tier8-tool-result-failure ()
  "Test that tool result failure is correctly recorded."
  (test-tier8--setup)
  (unwind-protect
      (let ((session-id "test-session-008"))
        (efrit-progress-start-session session-id "test command")
        (efrit-progress-show-tool-start "eval_sexp" '(("code" . "(error)")))
        (efrit-progress-show-tool-result "eval_sexp" "Error: test error" nil)
        (let ((events (test-tier8--read-progress-file session-id)))
          (let ((result-event (caddr events)))
            (should (eq :json-false (gethash "success" result-event))))))
    (test-tier8--teardown)))

;;; Message display tests

(ert-deftest test-tier8-text-event ()
  "Test that text message events are correctly emitted."
  (test-tier8--setup)
  (unwind-protect
      (let ((session-id "test-session-009"))
        (efrit-progress-start-session session-id "test command")
        (efrit-progress-show-message "Hello from Claude" 'claude)
        (let ((events (test-tier8--read-progress-file session-id)))
          (let ((text-event (cadr events)))
            (should (equal "text" (gethash "type" text-event)))
            (should (equal "Hello from Claude" (gethash "message" text-event)))
            (should (equal "claude" (gethash "message_type" text-event))))))
    (test-tier8--teardown)))

(ert-deftest test-tier8-error-message ()
  "Test that error messages have correct type."
  (test-tier8--setup)
  (unwind-protect
      (let ((session-id "test-session-010"))
        (efrit-progress-start-session session-id "test command")
        (efrit-progress-show-message "Something went wrong" 'error)
        (let ((events (test-tier8--read-progress-file session-id)))
          (let ((text-event (cadr events)))
            (should (equal "error" (gethash "message_type" text-event))))))
    (test-tier8--teardown)))

;;; Loop detection tracking tests

(ert-deftest test-tier8-tool-repeat-count ()
  "Test that tool repeat count is tracked."
  (test-tier8--setup)
  (unwind-protect
      (let ((session-id "test-session-011"))
        (efrit-progress-start-session session-id "test command")
        (efrit-progress-show-tool-start "eval_sexp" '(("code" . "(+ 1 2)")))
        (efrit-progress-show-tool-start "eval_sexp" '(("code" . "(+ 1 2)")))
        (efrit-progress-show-tool-start "eval_sexp" '(("code" . "(+ 1 2)")))
        (let ((events (test-tier8--read-progress-file session-id)))
          (let ((third-tool (nth 3 events)))  ; 0=session-start, 1-3=tool-starts
            (should (= 3 (gethash "repeat_count" third-tool))))))
    (test-tier8--teardown)))

(ert-deftest test-tier8-tool-repeat-resets ()
  "Test that tool repeat count resets when tool changes."
  (test-tier8--setup)
  (unwind-protect
      (let ((session-id "test-session-012"))
        (efrit-progress-start-session session-id "test command")
        (efrit-progress-show-tool-start "eval_sexp" '(("code" . "(+ 1 2)")))
        (efrit-progress-show-tool-start "eval_sexp" '(("code" . "(+ 1 2)")))
        (efrit-progress-show-tool-start "shell_exec" '(("cmd" . "ls")))  ; Different tool
        (let ((events (test-tier8--read-progress-file session-id)))
          (let ((shell-event (nth 3 events)))
            (should (= 1 (gethash "repeat_count" shell-event))))))
    (test-tier8--teardown)))

;;; Status query tests

(ert-deftest test-tier8-get-status-returns-alist ()
  "Test that get-status returns an alist."
  (let ((status (efrit-progress-get-status)))
    (should (listp status))
    (should (assq 'session status))
    (should (assq 'progress-file status))
    (should (assq 'event-count status))))

(ert-deftest test-tier8-get-status-tracks-session ()
  "Test that get-status reflects current session."
  (test-tier8--setup)
  (unwind-protect
      (let ((session-id "test-session-013"))
        (efrit-progress-start-session session-id "test command")
        (let ((status (efrit-progress-get-status)))
          (should (equal session-id (alist-get 'session status)))
          (should (stringp (alist-get 'progress-file status))))
        (efrit-progress-end-session session-id t)
        (let ((status (efrit-progress-get-status)))
          (should (null (alist-get 'session status)))))
    (test-tier8--teardown)))

;;; Truncation tests

(ert-deftest test-tier8-truncate-short-string ()
  "Test that short strings are not truncated."
  (let ((result (efrit-progress--truncate "short" 100)))
    (should (equal "short" result))))

(ert-deftest test-tier8-truncate-long-string ()
  "Test that long strings are truncated with ellipsis."
  (let ((long-string (make-string 200 ?x)))
    (let ((result (efrit-progress--truncate long-string 50)))
      (should (= 50 (length result)))
      (should (string-suffix-p "..." result)))))

(ert-deftest test-tier8-truncate-nil ()
  "Test that nil input returns nil."
  (should (null (efrit-progress--truncate nil 50))))

;;; Progress enabled flag tests

(ert-deftest test-tier8-disabled-no-file ()
  "Test that no file is created when progress is disabled."
  (test-tier8--setup)
  (unwind-protect
      (let ((efrit-progress-enabled nil)
            (session-id "test-session-014"))
        (efrit-progress-start-session session-id "test command")
        (efrit-progress-show-message "This should not be written")
        ;; File should not exist
        (should-not (test-tier8--read-progress-file session-id)))
    (test-tier8--teardown)))

;;; JSONL format verification

(ert-deftest test-tier8-jsonl-format ()
  "Test that progress file is valid JSONL (one JSON object per line)."
  (test-tier8--setup)
  (unwind-protect
      (let ((session-id "test-session-015"))
        (efrit-progress-start-session session-id "test command")
        (efrit-progress-show-message "message 1")
        (efrit-progress-show-message "message 2")
        (efrit-progress-end-session session-id t)
        ;; Read raw file and verify format
        (let* ((session-dir (expand-file-name session-id
                                              (expand-file-name "sessions" efrit-data-directory)))
               (progress-file (expand-file-name "progress.jsonl" session-dir)))
          (with-temp-buffer
            (insert-file-contents progress-file)
            ;; Each line should be valid JSON
            (goto-char (point-min))
            (while (not (eobp))
              (let ((line (buffer-substring-no-properties
                          (point) (line-end-position))))
                (unless (string-empty-p line)
                  ;; Should not error when parsing
                  (should (json-read-from-string line))))
              (forward-line 1)))))
    (test-tier8--teardown)))

(provide 'test-progress-tier8)

;;; test-progress-tier8.el ends here
