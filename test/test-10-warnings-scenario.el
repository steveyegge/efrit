;;; test-10-warnings-scenario.el --- Test the old problematic scenario -*- lexical-binding: t; -*-

;;; Commentary:
;; Test that the new system can handle 10 warnings without false positives

;;; Code:

(require 'ert)
(require 'efrit-loop-detection)
(require 'efrit-session)

(defun test-10-warnings--make-session (status continuation-count tool-history)
  "Create a test session with given parameters."
  (make-efrit-session :status status
                      :continuation-count continuation-count
                      :tool-history tool-history
                      :last-progress-tick 0))

(defun test-10-warnings--make-history-entry (tool-name progress-tick)
  "Create a tool history entry for testing."
  (list tool-name (current-time) progress-tick "input-hash" "result-hash"))

(ert-deftest test-10-warnings-scenario ()
  "Test that processing 10 warnings doesn't trigger false loop detection.
This is the scenario that broke the old system."
  (let* ((history 
          ;; Simulate processing 10 warnings with legitimate todo_execute_next calls
          ;; Each warning gets processed with increasing progress
          (list
           (test-10-warnings--make-history-entry "todo_analyze" 0)
           (test-10-warnings--make-history-entry "todo_execute_next" 0)
           (test-10-warnings--make-history-entry "eval_sexp" 1)     ; progress made
           (test-10-warnings--make-history-entry "todo_update" 1)
           (test-10-warnings--make-history-entry "todo_execute_next" 1)
           (test-10-warnings--make-history-entry "eval_sexp" 2)     ; progress made
           (test-10-warnings--make-history-entry "todo_update" 2)
           (test-10-warnings--make-history-entry "todo_execute_next" 2)
           (test-10-warnings--make-history-entry "eval_sexp" 3)     ; progress made
           (test-10-warnings--make-history-entry "todo_update" 3)
           (test-10-warnings--make-history-entry "todo_execute_next" 3)
           (test-10-warnings--make-history-entry "eval_sexp" 4)     ; progress made
           (test-10-warnings--make-history-entry "todo_update" 4)
           (test-10-warnings--make-history-entry "todo_execute_next" 4)
           (test-10-warnings--make-history-entry "eval_sexp" 5)     ; progress made
           (test-10-warnings--make-history-entry "todo_update" 5)))
         (session (test-10-warnings--make-session 'active 20 history)))
    ;; This should NOT trigger loop detection because progress is being made
    (should-not (efrit-loop-check session "todo_execute_next"))
    ;; Even after many more todo_execute_next calls
    (should-not (efrit-loop-check session "todo_execute_next"))
    (should-not (efrit-loop-check session "todo_execute_next"))
    (should-not (efrit-loop-check session "todo_execute_next"))))

(ert-deftest test-old-system-would-have-failed ()
  "Test a scenario that would have incorrectly triggered the old system.
The old system counted 'todo_execute_next called 3 times' as a loop."
  (let* ((history 
          ;; This would have triggered the old system after 3 todo_execute_next calls
          ;; even though real work is being done
          (list
           (test-10-warnings--make-history-entry "todo_execute_next" 1)
           (test-10-warnings--make-history-entry "eval_sexp" 2)      ; real work!
           (test-10-warnings--make-history-entry "todo_execute_next" 2)
           (test-10-warnings--make-history-entry "eval_sexp" 3)      ; real work!
           (test-10-warnings--make-history-entry "todo_execute_next" 3)  ; 3rd call - old system would panic
           (test-10-warnings--make-history-entry "eval_sexp" 4)))    ; but we're still making progress!
         (session (test-10-warnings--make-session 'active 15 history)))
    ;; New system correctly sees progress and doesn't trigger
    (should-not (efrit-loop-check session "todo_execute_next"))))

(provide 'test-10-warnings-scenario)
;;; test-10-warnings-scenario.el ends here
