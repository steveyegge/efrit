;;; efrit-integration-tests.el --- Integration tests for Efrit session flow -*- lexical-binding: t -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>

;;; Commentary:

;; Integration tests that hit the production Claude API to verify
;; the complete session flow works correctly. These tests will:
;; - Use real API tokens
;; - Make actual API calls
;; - Test multi-step session execution
;; - Verify async queue processing
;; - Test error handling and recovery

;;; Code:

(require 'ert)
(require 'efrit)
(require 'efrit-do)
(require 'efrit-async)
(require 'efrit-unified)
(require 'efrit-context)

;;; Test Configuration

(defvar efrit-integration-test-timeout 30
  "Timeout in seconds for async operations.")

(defvar efrit-integration-test-debug t
  "Enable debug output during tests.")

;;; Helper Functions

(defun efrit-test--wait-for-async (predicate &optional timeout)
  "Wait for PREDICATE to return non-nil, with optional TIMEOUT."
  (let ((timeout (or timeout efrit-integration-test-timeout))
        (start-time (float-time)))
    (while (and (not (funcall predicate))
                (< (- (float-time) start-time) timeout))
      (sleep-for 0.1))
    (funcall predicate)))

(defun efrit-test--with-clean-state (body)
  "Execute BODY with clean efrit state."
  (let ((efrit-async--active-session nil)
        (efrit-async--session-queue nil))
    (efrit-context-ring-clear)
    (funcall body)))

;;; Basic Session Flow Tests

(ert-deftest efrit-integration-test-simple-command ()
  "Test a simple synchronous command execution."
  (efrit-test--with-clean-state
   (lambda ()
     (let* ((result nil)
            (command "What is 2 + 2?"))
       (efrit-do command)
       ;; Check context was captured
       (let ((recent (efrit-context-ring-get-recent 1)))
         (should recent)
         (should (equal (efrit-context-item-command (car recent)) command))
         (should (string-match "4" (efrit-context-item-result (car recent)))))))))

(ert-deftest efrit-integration-test-multi-step-session ()
  "Test multi-step session execution flow."
  (efrit-test--with-clean-state
   (lambda ()
     (let* ((result nil)
            (completed nil)
            (command "Create a buffer called *test-session*, insert 'Hello World', then tell me what the buffer contains"))
       
       ;; Execute async command
       (efrit-async-execute-command 
        command
        (lambda (res)
          (setq result res)
          (setq completed t)))
       
       ;; Wait for completion
       (should (efrit-test--wait-for-async (lambda () completed)))
       
       ;; Verify results
       (should result)
       (should (get-buffer "*test-session*"))
       (with-current-buffer "*test-session*"
         (should (string-match "Hello World" (buffer-string))))
       
       ;; Check work log
       (should efrit-async--active-session)
       (let ((work-log (efrit-session-work-log efrit-async--active-session)))
         (should (> (length work-log) 1))
         (when efrit-integration-test-debug
           (message "Work log entries: %d" (length work-log))))))))

(ert-deftest efrit-integration-test-async-queue ()
  "Test async queue processing."
  (efrit-test--with-clean-state
   (lambda ()
     (let ((results '())
           (commands '("What is 5 + 5?" 
                      "What is 10 - 3?"
                      "What is 2 * 6?"))
           (completed 0))
       
       ;; Queue multiple commands
       (dolist (cmd commands)
         (efrit-async-execute-command
          cmd
          (lambda (result)
            (push result results)
            (cl-incf completed))))
       
       ;; Should have one active and two queued
       (should efrit-async--active-session)
       (should (= 2 (length efrit-async--session-queue)))
       
       ;; Wait for all to complete
       (should (efrit-test--wait-for-async 
                (lambda () (= completed 3))
                (* 3 efrit-integration-test-timeout)))
       
       ;; Verify all completed
       (should (= 3 (length results)))
       (should (null efrit-async--session-queue))))))

(ert-deftest efrit-integration-test-error-handling ()
  "Test error handling and recovery."
  (efrit-test--with-clean-state
   (lambda ()
     (let* ((result nil)
            (completed nil)
            ;; Command that should trigger an error
            (command "(this-function-does-not-exist-12345)"))
       
       (efrit-do-async 
        command
        (lambda (res)
          (setq result res)
          (setq completed t)))
       
       (should (efrit-test--wait-for-async (lambda () completed)))
       
       ;; Should get an error response
       (should result)
       (should (string-match-p "error\\|Error" result))))))

(ert-deftest efrit-integration-test-unified-mode-decision ()
  "Test Claude's mode decision in unified interface."
  (efrit-test--with-clean-state
   (lambda ()
     ;; Test a command that should be async
     (let* ((start-time (float-time))
            (command "Count slowly from 1 to 5, pausing briefly between each number"))
       
       ;; Let Claude decide the mode
       (efrit-unified-do command)
       
       ;; For a slow counting task, Claude might choose async
       ;; but we can't guarantee it, so just verify execution
       (let ((recent (efrit-context-ring-get-recent 1)))
         (should recent)
         (should (efrit-context-item-result (car recent))))))))

(ert-deftest efrit-integration-test-context-persistence ()
  "Test context persistence and restoration."
  (efrit-test--with-clean-state
   (lambda ()
     ;; Execute a command
     (efrit-do "What is the capital of France?")
     
     ;; Save context
     (efrit-context-ring-persist)
     
     ;; Clear and restore
     (efrit-context-ring-clear)
     (should (null (efrit-context-ring-get-recent)))
     
     (efrit-context-ring-restore)
     
     ;; Verify restored
     (let ((recent (efrit-context-ring-get-recent 1)))
       (should recent)
       (should (string-match-p "France" (efrit-context-item-command (car recent))))))))

(ert-deftest efrit-integration-test-session-complete-tool ()
  "Test that session_complete tool properly ends sessions."
  (efrit-test--with-clean-state
   (lambda ()
     (let* ((completed nil)
            (result nil)
            ;; Command that should use session_complete
            (command "Tell me a one-word answer: what color is the sky?"))
       
       (efrit-async-execute-command
        command
        (lambda (res)
          (setq result res)
          (setq completed t)))
       
       (should (efrit-test--wait-for-async (lambda () completed)))
       
       ;; Session should be complete and cleared
       (should result)
       (should (null efrit-async--active-session))))))

;;; Performance Tests

(ert-deftest efrit-integration-test-performance-baseline ()
  "Establish performance baseline for simple commands."
  (efrit-test--with-clean-state
   (lambda ()
     (let ((start-time (float-time))
           (timings '()))
       
       ;; Run several simple commands
       (dotimes (i 3)
         (let ((cmd-start (float-time)))
           (efrit-do "What is 1 + 1?")
           (push (- (float-time) cmd-start) timings)))
       
       (let ((avg-time (/ (apply #'+ timings) (length timings))))
         (when efrit-integration-test-debug
           (message "Average response time: %.2fs" avg-time))
         ;; Typical response should be under 5 seconds
         (should (< avg-time 5.0)))))))

;;; Test Runner

(defun efrit-run-integration-tests ()
  "Run all integration tests interactively."
  (interactive)
  (ert-run-tests-interactively "^efrit-integration-test-"))

(defun efrit-run-integration-tests-batch ()
  "Run all integration tests in batch mode."
  (ert-run-tests-batch "^efrit-integration-test-"))

(provide 'efrit-integration-tests)
;;; efrit-integration-tests.el ends here