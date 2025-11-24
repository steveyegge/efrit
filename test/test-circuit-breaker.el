;;; test-circuit-breaker.el --- Tests for circuit breaker limits -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Efrit Contributors

;; This file is part of Efrit.

;;; Commentary:

;; Test suite for the circuit breaker system that prevents infinite loops
;; by enforcing hard limits on tool calls per session.
;;
;; The circuit breaker has three levels of protection:
;; 1. Session limit - max total tool calls per session (default: 30)
;; 2. Same tool limit - max consecutive calls to same tool (default: 15, warning at 10)
;; 3. Identical call limit - max calls with same tool AND same input (default: 3)

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
  (setq efrit-do--last-tool-input "abc123")
  (setq efrit-do--identical-call-count 2)

  ;; Reset
  (efrit-do--circuit-breaker-reset)

  ;; Verify all counters cleared
  (should (= efrit-do--session-tool-count 0))
  (should (null efrit-do--last-tool-called))
  (should (= efrit-do--tool-call-count 0))
  (should (null efrit-do--circuit-breaker-tripped))
  (should (null efrit-do--last-tool-input))
  (should (= efrit-do--identical-call-count 0)))

(ert-deftest test-circuit-breaker-record-call ()
  "Test that circuit breaker records tool calls correctly."
  (efrit-do--circuit-breaker-reset)
  (let ((efrit-do-circuit-breaker-enabled t))

    ;; Record first call
    (efrit-do--circuit-breaker-record-call "eval_sexp")
    (should (= efrit-do--session-tool-count 1))
    (should (string= efrit-do--last-tool-called "eval_sexp"))
    (should (= efrit-do--tool-call-count 1))

    ;; Record same tool again (different input - nil vs nil is same)
    (efrit-do--circuit-breaker-record-call "eval_sexp")
    (should (= efrit-do--session-tool-count 2))
    (should (= efrit-do--tool-call-count 2))

    ;; Record different tool
    (efrit-do--circuit-breaker-record-call "shell_exec")
    (should (= efrit-do--session-tool-count 3))
    (should (string= efrit-do--last-tool-called "shell_exec"))
    (should (= efrit-do--tool-call-count 1)))) ; Reset for new tool

(ert-deftest test-circuit-breaker-record-call-with-input ()
  "Test that circuit breaker tracks identical calls (same tool + same input)."
  (efrit-do--circuit-breaker-reset)
  (let ((efrit-do-circuit-breaker-enabled t)
        (input1 (make-hash-table :test 'equal))
        (input2 (make-hash-table :test 'equal)))
    (puthash "code" "(+ 1 1)" input1)
    (puthash "code" "(+ 2 2)" input2)

    ;; First call with input1
    (efrit-do--circuit-breaker-record-call "eval_sexp" input1)
    (should (= efrit-do--identical-call-count 1))

    ;; Same tool, same input
    (efrit-do--circuit-breaker-record-call "eval_sexp" input1)
    (should (= efrit-do--identical-call-count 2))
    (should (= efrit-do--tool-call-count 2))

    ;; Same tool, different input - resets identical count
    (efrit-do--circuit-breaker-record-call "eval_sexp" input2)
    (should (= efrit-do--identical-call-count 1))
    (should (= efrit-do--tool-call-count 3))))

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
      (should (string-match-p "Session limit" (cdr check-result)))
      (should efrit-do--circuit-breaker-tripped))))

(ert-deftest test-circuit-breaker-same-tool-warning ()
  "Test that same-tool consecutive limit triggers warning before hard limit."
  (efrit-do--circuit-breaker-reset)
  (let ((efrit-do-circuit-breaker-enabled t)
        (efrit-do-max-same-tool-calls 15)
        (efrit-do-same-tool-warning-threshold 10)
        (efrit-do-max-identical-tool-calls 100)) ; High so we don't hit it

    ;; Call same tool 10 times (reaching warning threshold)
    (dotimes (i 10)
      (let ((input (make-hash-table :test 'equal)))
        (puthash "code" (format "(+ %d %d)" i i) input)
        (efrit-do--circuit-breaker-record-call "eval_sexp" input)))

    ;; 11th call should trigger warning but still allow
    (let* ((input (make-hash-table :test 'equal))
           (_ (puthash "code" "(+ 10 10)" input))
           (check-result (efrit-do--circuit-breaker-check-limits "eval_sexp" input)))
      (should (car check-result)) ; Still allowed
      (should (string-match-p "WARNING" (cdr check-result)))
      (should (not efrit-do--circuit-breaker-tripped)))))

(ert-deftest test-circuit-breaker-same-tool-hard-limit ()
  "Test that same-tool hard limit trips the breaker."
  (efrit-do--circuit-breaker-reset)
  (let ((efrit-do-circuit-breaker-enabled t)
        (efrit-do-max-same-tool-calls 5)
        (efrit-do-same-tool-warning-threshold 3)
        (efrit-do-max-identical-tool-calls 100)) ; High so we don't hit it

    ;; Call same tool 5 times with different inputs
    (dotimes (i 5)
      (let ((input (make-hash-table :test 'equal)))
        (puthash "code" (format "(+ %d %d)" i i) input)
        (efrit-do--circuit-breaker-record-call "eval_sexp" input)))

    ;; 6th call should trip the breaker
    (let* ((input (make-hash-table :test 'equal))
           (_ (puthash "code" "(+ 5 5)" input))
           (check-result (efrit-do--circuit-breaker-check-limits "eval_sexp" input)))
      (should (not (car check-result))) ; Not allowed
      (should (string-match-p "Same tool" (cdr check-result)))
      (should efrit-do--circuit-breaker-tripped))))

(ert-deftest test-circuit-breaker-identical-call-limit ()
  "Test that identical calls (same tool + same input) trip the breaker quickly."
  (efrit-do--circuit-breaker-reset)
  (let ((efrit-do-circuit-breaker-enabled t)
        (efrit-do-max-same-tool-calls 100) ; High so we don't hit it
        (efrit-do-max-identical-tool-calls 3)
        (input (make-hash-table :test 'equal)))
    (puthash "code" "(infinite-loop)" input)

    ;; Call same tool with same input 3 times
    (dotimes (i 3)
      (efrit-do--circuit-breaker-record-call "eval_sexp" input))

    ;; 4th identical call should trip the breaker
    (let ((check-result (efrit-do--circuit-breaker-check-limits "eval_sexp" input)))
      (should (not (car check-result))) ; Not allowed
      (should (string-match-p "Identical tool call" (cdr check-result)))
      (should (string-match-p "infinite loop" (cdr check-result)))
      (should efrit-do--circuit-breaker-tripped))))

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
      (should (string-match-p "active" (cdr check-result))))))

(ert-deftest test-circuit-breaker-normal-workflow ()
  "Test that normal workflows don't trip the breaker."
  (efrit-do--circuit-breaker-reset)
  (let ((efrit-do-circuit-breaker-enabled t)
        (efrit-do-max-tool-calls-per-session 30)
        (efrit-do-max-same-tool-calls 15)
        (efrit-do-same-tool-warning-threshold 10)
        (efrit-do-max-identical-tool-calls 3))

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

(ert-deftest test-circuit-breaker-multi-step-coding ()
  "Test that multi-step coding tasks (many eval_sexp with different inputs) work."
  (efrit-do--circuit-breaker-reset)
  (let ((efrit-do-circuit-breaker-enabled t)
        (efrit-do-max-tool-calls-per-session 30)
        (efrit-do-max-same-tool-calls 15)
        (efrit-do-same-tool-warning-threshold 10)
        (efrit-do-max-identical-tool-calls 3))

    ;; Simulate writing 12 different functions (12 eval_sexp calls with different inputs)
    (dotimes (i 12)
      (let ((input (make-hash-table :test 'equal)))
        (puthash "code" (format "(defun func%d () %d)" i i) input)
        (should (car (efrit-do--circuit-breaker-check-limits "eval_sexp" input)))
        (efrit-do--circuit-breaker-record-call "eval_sexp" input)))

    ;; Should NOT have tripped
    (should (not efrit-do--circuit-breaker-tripped))
    (should (= efrit-do--tool-call-count 12))
    (should (= efrit-do--session-tool-count 12))))

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

      ;; Third should be blocked (session limit)
      (let ((result (efrit-do--execute-tool tool3)))
        (should (string-match-p "Session limit" result))))))

(provide 'test-circuit-breaker)
;;; test-circuit-breaker.el ends here
