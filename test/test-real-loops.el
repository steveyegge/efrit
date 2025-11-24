;;; test-real-loops.el --- Test that real loops are still caught -*- lexical-binding: t; -*-

;;; Commentary:
;; Ensure the new system still catches actual infinite loops

;;; Code:

(require 'ert)
(require 'efrit-loop-detection)
(require 'efrit-session)

(defun test-real-loops--make-session (status continuation-count tool-history)
  "Create a test session with given parameters."
  (make-efrit-session :status status
                      :continuation-count continuation-count
                      :tool-history tool-history
                      :last-progress-tick 10))  ; Set a baseline progress

(defun test-real-loops--make-history-entry (tool-name progress-tick)
  "Create a tool history entry for testing."
  (list tool-name (current-time) progress-tick "input-hash" "result-hash"))

(ert-deftest test-real-infinite-loop-caught ()
  "Test that a real infinite loop (no progress) is correctly detected."
  (let* ((progress-tick 10)  ; Frozen progress
         (history 
          ;; 15 tool calls with no progress whatsoever - this is a real loop!
          (list
           (test-real-loops--make-history-entry "todo_analyze" progress-tick)
           (test-real-loops--make-history-entry "todo_execute_next" progress-tick)
           (test-real-loops--make-history-entry "todo_analyze" progress-tick)
           (test-real-loops--make-history-entry "todo_execute_next" progress-tick)
           (test-real-loops--make-history-entry "todo_analyze" progress-tick)
           (test-real-loops--make-history-entry "todo_execute_next" progress-tick)
           (test-real-loops--make-history-entry "todo_analyze" progress-tick)
           (test-real-loops--make-history-entry "todo_execute_next" progress-tick)
           (test-real-loops--make-history-entry "todo_analyze" progress-tick)
           (test-real-loops--make-history-entry "todo_execute_next" progress-tick)
           (test-real-loops--make-history-entry "todo_analyze" progress-tick)
           (test-real-loops--make-history-entry "todo_execute_next" progress-tick)
           (test-real-loops--make-history-entry "todo_analyze" progress-tick)
           (test-real-loops--make-history-entry "todo_execute_next" progress-tick)
           (test-real-loops--make-history-entry "todo_analyze" progress-tick)))
         (session (test-real-loops--make-session 'active 20 history)))
    ;; This SHOULD trigger loop detection because no progress is being made
    (should (string-match "NO PROGRESS" (efrit-loop-check session "todo_execute_next")))))

(ert-deftest test-subtle-progress-still-works ()
  "Test that even small amounts of progress prevent false loop detection."
  (let* ((history 
          ;; Similar to above, but with tiny amounts of progress
          (list
           (test-real-loops--make-history-entry "todo_analyze" 10)
           (test-real-loops--make-history-entry "todo_execute_next" 10)
           (test-real-loops--make-history-entry "todo_analyze" 10)
           (test-real-loops--make-history-entry "todo_execute_next" 10)
           (test-real-loops--make-history-entry "todo_analyze" 10)
           (test-real-loops--make-history-entry "todo_execute_next" 10)
           (test-real-loops--make-history-entry "todo_analyze" 11)  ; tiny progress!
           (test-real-loops--make-history-entry "todo_execute_next" 11)
           (test-real-loops--make-history-entry "todo_analyze" 11)
           (test-real-loops--make-history-entry "todo_execute_next" 11)
           (test-real-loops--make-history-entry "todo_analyze" 11)
           (test-real-loops--make-history-entry "todo_execute_next" 11)
           (test-real-loops--make-history-entry "todo_analyze" 11)
           (test-real-loops--make-history-entry "todo_execute_next" 11)
           (test-real-loops--make-history-entry "todo_analyze" 11)))
         (session (test-real-loops--make-session 'active 20 history)))
    ;; This should NOT trigger because there was some progress (10->11)
    (should-not (efrit-loop-check session "todo_execute_next"))))

(provide 'test-real-loops)
;;; test-real-loops.el ends here
