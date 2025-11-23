;;; test-circuit-breaker.el --- Tests for circuit breaker limits -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Efrit Contributors

;; This file is part of Efrit.

;;; Commentary:

;; Test suite for the circuit breaker system that prevents infinite loops
;; by enforcing hard limits on tool calls per session.

;;; Code:

(require 'ert)
(require 'efrit-do)

(ert-deftest test-circuit-breaker-reset ()
  "Test that circuit breaker resets correctly."
  ;; Simulate some tool calls
  (setq efrit-do--session-tool-count 10)
  (setq efrit-do--last-tool-called "eval_sexp")
  (setq efrit-do--tool-call-count 3)
  (setq efrit-do--circuit-breaker-tripped "Test trip")

  ;; Reset
  (efrit-do--circuit-breaker-reset)

  ;; Verify all counters cleared
  (should (= efrit-do--session-tool-count 0))
  (should (null efrit-do--last-tool-called))
  (should (= efrit-do--tool-call-count 0))
  (should (null efrit-do--circuit-breaker-tripped)))

(ert-deftest test-circuit-breaker-record-call ()
  "Test that circuit breaker records tool calls correctly."
  (efrit-do--circuit-breaker-reset)

  ;; Record first call
  (efrit-do--circuit-breaker-record-call "eval_sexp")
  (should (= efrit-do--session-tool-count 1))
  (should (string= efrit-do--last-tool-called "eval_sexp"))
  (should (= efrit-do--tool-call-count 1))

  ;; Record same tool again
  (efrit-do--circuit-breaker-record-call "eval_sexp")
  (should (= efrit-do--session-tool-count 2))
  (should (= efrit-do--tool-call-count 2))

  ;; Record different tool
  (efrit-do--circuit-breaker-record-call "shell_exec")
  (should (= efrit-do--session-tool-count 3))
  (should (string= efrit-do--last-tool-called "shell_exec"))
  (should (= efrit-do--tool-call-count 1))) ; Reset for new tool

(ert-deftest test-circuit-breaker-session-limit ()
  "Test that session limit is enforced."
  (efrit-do--circuit-breaker-reset)
  (let ((efrit-do-circuit-breaker-enabled t)
        (efrit-do-max-tool-calls-per-session 5))

    ;; Fill up to limit
    (dotimes (i 5)
      (efrit-do--circuit-breaker-record-call (format "tool%d" i)))

    ;; Next check should trip the breaker
    (let ((check-result (efrit-do--circuit-breaker-check-limits "tool6")))
      (should (not (car check-result))) ; Not allowed
      (should (string-match-p "CIRCUIT BREAKER" (cdr check-result)))
      (should efrit-do--circuit-breaker-tripped))))

(ert-deftest test-circuit-breaker-same-tool-limit ()
  "Test that same-tool consecutive limit triggers warning."
  (efrit-do--circuit-breaker-reset)
  (let ((efrit-do-circuit-breaker-enabled t)
        (efrit-do-max-same-tool-calls 3))

    ;; Call same tool 3 times
    (dotimes (i 3)
      (efrit-do--circuit-breaker-record-call "eval_sexp"))

    ;; 4th call should trigger warning but allow
    (let ((check-result (efrit-do--circuit-breaker-check-limits "eval_sexp")))
      (should (car check-result)) ; Still allowed
      (should (string-match-p "WARNING" (cdr check-result))))))

(ert-deftest test-circuit-breaker-disabled ()
  "Test that circuit breaker can be disabled."
  (efrit-do--circuit-breaker-reset)
  (let ((efrit-do-circuit-breaker-enabled nil))

    ;; Record many calls
    (dotimes (i 100)
      (efrit-do--circuit-breaker-record-call "eval_sexp"))

    ;; Should still allow
    (let ((check-result (efrit-do--circuit-breaker-check-limits "eval_sexp")))
      (should (car check-result))
      (should (null (cdr check-result))))))

(ert-deftest test-circuit-breaker-after-trip ()
  "Test that circuit breaker blocks all calls after trip."
  (efrit-do--circuit-breaker-reset)
  (let ((efrit-do-circuit-breaker-enabled t)
        (efrit-do-max-tool-calls-per-session 2))

    ;; Trip the breaker
    (efrit-do--circuit-breaker-record-call "tool1")
    (efrit-do--circuit-breaker-record-call "tool2")
    (efrit-do--circuit-breaker-check-limits "tool3") ; This trips it

    ;; Try different tool - should still be blocked
    (let ((check-result (efrit-do--circuit-breaker-check-limits "different_tool")))
      (should (not (car check-result)))
      (should (string-match-p "TRIPPED" (cdr check-result))))))

(ert-deftest test-circuit-breaker-normal-workflow ()
  "Test that normal workflows don't trip the breaker."
  (efrit-do--circuit-breaker-reset)
  (let ((efrit-do-circuit-breaker-enabled t)
        (efrit-do-max-tool-calls-per-session 30)
        (efrit-do-max-same-tool-calls 3))

    ;; Simulate a normal workflow: analyze -> add todos -> execute
    (efrit-do--circuit-breaker-record-call "todo_analyze")
    (should (car (efrit-do--circuit-breaker-check-limits "todo_add")))
    (efrit-do--circuit-breaker-record-call "todo_add")
    (should (car (efrit-do--circuit-breaker-check-limits "todo_add")))
    (efrit-do--circuit-breaker-record-call "todo_add")
    (should (car (efrit-do--circuit-breaker-check-limits "eval_sexp")))
    (efrit-do--circuit-breaker-record-call "eval_sexp")
    (should (car (efrit-do--circuit-breaker-check-limits "todo_update")))
    (efrit-do--circuit-breaker-record-call "todo_update")

    ;; All should succeed
    (should (< efrit-do--session-tool-count 10))
    (should (not efrit-do--circuit-breaker-tripped))))

(ert-deftest test-circuit-breaker-integration-with-execute-tool ()
  "Test circuit breaker integration with tool execution."
  (efrit-do--circuit-breaker-reset)
  (let ((efrit-do-circuit-breaker-enabled t)
        (efrit-do-max-tool-calls-per-session 2))

    ;; Create mock tool items
    (let ((tool1 (make-hash-table :test 'equal))
          (tool2 (make-hash-table :test 'equal))
          (tool3 (make-hash-table :test 'equal)))

      (puthash "name" "todo_show" tool1)
      (puthash "input" (make-hash-table) tool1)

      (puthash "name" "todo_show" tool2)
      (puthash "input" (make-hash-table) tool2)

      (puthash "name" "eval_sexp" tool3)
      (puthash "input" (make-hash-table) tool3)
      (puthash "expr" "(+ 1 1)" (gethash "input" tool3))

      ;; Execute two tools
      (efrit-do--execute-tool tool1)
      (efrit-do--execute-tool tool2)

      ;; Third should be blocked
      (let ((result (efrit-do--execute-tool tool3)))
        (should (string-match-p "CIRCUIT BREAKER" result))))))

(provide 'test-circuit-breaker)
;;; test-circuit-breaker.el ends here
