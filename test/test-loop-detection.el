;;; test-loop-detection.el --- Tests for progress-based loop detection -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Efrit Contributors

;; This file is part of Efrit.

;;; Commentary:

;; Test suite for the new progress-based loop detection system.
;; Tests various scenarios to ensure it works correctly and doesn't
;; have false positives.

;;; Code:

(require 'ert)
(require 'efrit-loop-detection)
(require 'efrit-session)

(defun test-loop--make-session (status continuation-count tool-history)
  "Create a test session with given parameters."
  (make-efrit-session :status status
                      :continuation-count continuation-count
                      :tool-history tool-history
                      :last-progress-tick 0))

(defun test-loop--make-history-entry (tool-name progress-tick)
  "Create a tool history entry for testing."
  (list tool-name (current-time) progress-tick "input-hash" "result-hash"))

(ert-deftest test-loop-detection-no-session ()
  "Test that loop detection handles nil session gracefully."
  (should-not (efrit-loop-check nil "any_tool")))

(ert-deftest test-loop-detection-new-session ()
  "Test that new sessions with little history don't trigger detection."
  (let ((session (test-loop--make-session 'active 5 
                   (list (test-loop--make-history-entry "tool1" 0)
                         (test-loop--make-history-entry "tool2" 0)))))
    (should-not (efrit-loop-check session "tool3"))))

(ert-deftest test-loop-detection-safety-limit ()
  "Test that absolute safety limit triggers correctly."
  (let ((session (test-loop--make-session 'active 600 '())))
    (should (string-match "SAFETY LIMIT" 
                         (efrit-loop-check session "any_tool")))))

(ert-deftest test-loop-detection-progress-made ()
  "Test that sessions making progress don't trigger loop detection."
  (let* ((history (list
                   (test-loop--make-history-entry "tool1" 0)
                   (test-loop--make-history-entry "tool2" 1)
                   (test-loop--make-history-entry "tool3" 2)
                   (test-loop--make-history-entry "tool4" 3)
                   (test-loop--make-history-entry "tool5" 4)
                   (test-loop--make-history-entry "tool6" 5)
                   (test-loop--make-history-entry "tool7" 6)
                   (test-loop--make-history-entry "tool8" 7)
                   (test-loop--make-history-entry "tool9" 8)))
         (session (test-loop--make-session 'active 15 history)))
    (should-not (efrit-loop-check session "tool10"))))

(ert-deftest test-loop-detection-stalled-progress ()
  "Test that stalled progress triggers loop detection."
  (let* ((history (list
                   (test-loop--make-history-entry "todo_analyze" 5)
                   (test-loop--make-history-entry "todo_execute_next" 5)
                   (test-loop--make-history-entry "todo_analyze" 5)
                   (test-loop--make-history-entry "todo_execute_next" 5)
                   (test-loop--make-history-entry "todo_analyze" 5)
                   (test-loop--make-history-entry "todo_execute_next" 5)
                   (test-loop--make-history-entry "todo_analyze" 5)
                   (test-loop--make-history-entry "todo_execute_next" 5)
                   (test-loop--make-history-entry "todo_analyze" 5)
                   (test-loop--make-history-entry "todo_execute_next" 5)
                   (test-loop--make-history-entry "todo_analyze" 5)))
         (session (test-loop--make-session 'active 20 history)))
    (should (string-match "NO PROGRESS" 
                         (efrit-loop-check session "todo_analyze")))))

(ert-deftest test-loop-detection-no-pending-work ()
  "Test that no pending work prevents false loop detection."
  (let* ((history (list
                   (test-loop--make-history-entry "eval_sexp" 5)
                   (test-loop--make-history-entry "shell_exec" 5)
                   (test-loop--make-history-entry "eval_sexp" 5)
                   (test-loop--make-history-entry "shell_exec" 5)
                   (test-loop--make-history-entry "eval_sexp" 5)
                   (test-loop--make-history-entry "shell_exec" 5)
                   (test-loop--make-history-entry "eval_sexp" 5)
                   (test-loop--make-history-entry "shell_exec" 5)
                   (test-loop--make-history-entry "eval_sexp" 5)))
         (session (test-loop--make-session 'active 15 history)))
    ;; No TODO activity = no pending work = no loop detection
    (should-not (efrit-loop-check session "eval_sexp"))))

(ert-deftest test-loop-detection-inactive-session ()
  "Test that inactive sessions don't trigger loop detection."
  (let* ((history (list
                   (test-loop--make-history-entry "todo_analyze" 5)
                   (test-loop--make-history-entry "todo_analyze" 5)
                   (test-loop--make-history-entry "todo_analyze" 5)
                   (test-loop--make-history-entry "todo_analyze" 5)
                   (test-loop--make-history-entry "todo_analyze" 5)
                   (test-loop--make-history-entry "todo_analyze" 5)
                   (test-loop--make-history-entry "todo_analyze" 5)
                   (test-loop--make-history-entry "todo_analyze" 5)))
         (session (test-loop--make-session 'complete 15 history)))
    (should-not (efrit-loop-check session "todo_analyze"))))

(provide 'test-loop-detection)
;;; test-loop-detection.el ends here
