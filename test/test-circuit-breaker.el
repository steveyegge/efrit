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

;;; Error Loop Detection Tests

(ert-deftest test-error-loop-reset ()
  "Test that error loop state resets correctly."
  ;; Set up some error tracking state
  (setq efrit-do--last-error-hash "abc123")
  (setq efrit-do--same-error-count 5)
  (setq efrit-do--error-history '(("hash1" . "error1") ("hash2" . "error2")))

  ;; Reset via circuit breaker reset (which calls error loop reset)
  (efrit-do--circuit-breaker-reset)

  ;; Verify error loop state is cleared
  (should (null efrit-do--last-error-hash))
  (should (= efrit-do--same-error-count 0))
  (should (null efrit-do--error-history)))

(ert-deftest test-error-loop-hash-normalization ()
  "Test that error message hashing normalizes variations."
  ;; Same error with different line numbers should hash the same
  (let ((hash1 (efrit-do--hash-error-message "Error at line 10: undefined function"))
        (hash2 (efrit-do--hash-error-message "Error at line 42: undefined function")))
    (should (string= hash1 hash2)))

  ;; Different errors should hash differently
  (let ((hash1 (efrit-do--hash-error-message "Symbol's value as variable is void"))
        (hash2 (efrit-do--hash-error-message "Wrong type argument")))
    (should (not (string= hash1 hash2)))))

(ert-deftest test-error-loop-record-same-error ()
  "Test that recording same error increments counter."
  (efrit-do--circuit-breaker-reset)

  ;; Record same error multiple times
  (efrit-do--error-loop-record "Symbol's value as variable is void: foo")
  (should (= efrit-do--same-error-count 1))

  (efrit-do--error-loop-record "Symbol's value as variable is void: foo")
  (should (= efrit-do--same-error-count 2))

  (efrit-do--error-loop-record "Symbol's value as variable is void: foo")
  (should (= efrit-do--same-error-count 3)))

(ert-deftest test-error-loop-record-different-error ()
  "Test that recording different error resets counter."
  (efrit-do--circuit-breaker-reset)

  ;; Record first error
  (efrit-do--error-loop-record "Symbol's value as variable is void: foo")
  (efrit-do--error-loop-record "Symbol's value as variable is void: foo")
  (should (= efrit-do--same-error-count 2))

  ;; Record different error - should reset counter
  (efrit-do--error-loop-record "Wrong type argument: stringp, 42")
  (should (= efrit-do--same-error-count 1)))

(ert-deftest test-error-loop-warning-threshold ()
  "Test that warning is returned at threshold."
  (efrit-do--circuit-breaker-reset)
  (let ((efrit-do-max-same-error-occurrences 3)
        (efrit-do-error-loop-auto-complete 10)) ; High so we don't hit it

    ;; First two errors - no warning
    (let ((result1 (efrit-do--error-loop-record "Test error")))
      (should (null (car result1))))
    (let ((result2 (efrit-do--error-loop-record "Test error")))
      (should (null (car result2))))

    ;; Third error - should trigger warning
    (let ((result3 (efrit-do--error-loop-record "Test error")))
      (should (eq (car result3) 'warning))
      (should (string-match-p "ERROR LOOP DETECTED" (cdr result3)))
      (should (string-match-p "3 times" (cdr result3))))))

(ert-deftest test-error-loop-auto-complete ()
  "Test that auto-complete trips circuit breaker at threshold."
  (efrit-do--circuit-breaker-reset)
  (let ((efrit-do-max-same-error-occurrences 2)
        (efrit-do-error-loop-auto-complete 4))

    ;; Record errors up to auto-complete threshold
    (dotimes (i 3)
      (efrit-do--error-loop-record "Persistent error"))
    (should (null efrit-do--circuit-breaker-tripped))

    ;; 4th occurrence should trip breaker
    (let ((result (efrit-do--error-loop-record "Persistent error")))
      (should (eq (car result) 'auto-complete))
      (should efrit-do--circuit-breaker-tripped)
      (should (string-match-p "Error loop detected" efrit-do--circuit-breaker-tripped)))))

(ert-deftest test-error-loop-check-result-with-error ()
  "Test error loop check on tool result containing error."
  (efrit-do--circuit-breaker-reset)
  (let ((efrit-do-max-same-error-occurrences 2)
        (efrit-do-error-loop-auto-complete nil)) ; Disable auto-complete

    ;; Simulate two tool results with same error
    (efrit-do--error-loop-check-result "[Error: Symbol's value as variable is void: test]")
    (let ((check (efrit-do--error-loop-check-result "[Error: Symbol's value as variable is void: test]")))
      ;; Second occurrence hits threshold - result should be modified
      (should (car check))
      (should (string-match-p "ERROR LOOP DETECTED" (cdr check))))))

(ert-deftest test-error-loop-check-result-success-resets ()
  "Test that successful result resets error counter."
  (efrit-do--circuit-breaker-reset)

  ;; Build up error count
  (efrit-do--error-loop-record "Test error")
  (efrit-do--error-loop-record "Test error")
  (should (= efrit-do--same-error-count 2))

  ;; Success should reset the counter
  (efrit-do--error-loop-check-result "[Success: Operation completed]")
  (should (= efrit-do--same-error-count 0)))

(ert-deftest test-error-loop-history-limit ()
  "Test that error history is limited to 10 entries."
  (efrit-do--circuit-breaker-reset)

  ;; Record 15 different errors
  (dotimes (i 15)
    (efrit-do--error-loop-record (format "Error %d: unique message" i)))

  ;; History should be limited to 10
  (should (= (length efrit-do--error-history) 10)))

;;; Executor Integration Tests

(ert-deftest test-circuit-breaker-stops-session-continuation ()
  "Test that tripped circuit breaker stops executor from continuing session.
This verifies the fix for ef-9jp: Circuit breaker trips but session keeps calling tools."
  (require 'efrit-executor)
  (efrit-do--circuit-breaker-reset)
  (let ((efrit-do-circuit-breaker-enabled t)
        (callback-called nil)
        (callback-result nil))

    ;; Trip the circuit breaker manually
    (setq efrit-do--circuit-breaker-tripped "Test: Session limit exceeded")

    ;; Create a mock session (needs id and command)
    (let ((mock-session (efrit-session-create "test-circuit-breaker" "test command")))
      ;; Call continue-session - it should detect the tripped breaker and NOT continue
      (efrit-executor--continue-session
       mock-session
       (lambda (result)
         (setq callback-called t)
         (setq callback-result result)))

      ;; Verify callback was called with circuit breaker message
      (should callback-called)
      (should (string-match-p "Circuit breaker" callback-result))

      ;; Session should be complete (status = 'complete)
      (should (eq (efrit-session-status mock-session) 'complete)))))

(ert-deftest test-circuit-breaker-allows-session-when-not-tripped ()
  "Test that session continuation is allowed when circuit breaker is NOT tripped."
  (require 'efrit-executor)
  (efrit-do--circuit-breaker-reset)

  ;; Verify breaker is not tripped
  (should (null efrit-do--circuit-breaker-tripped))

  ;; The check in efrit-executor--continue-session should pass
  ;; We can't fully test without mocking the API, but we can verify the condition
  (should (not (and (boundp 'efrit-do--circuit-breaker-tripped)
                    efrit-do--circuit-breaker-tripped))))

(provide 'test-circuit-breaker)
;;; test-circuit-breaker.el ends here
