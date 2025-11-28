;;; test-efrit-agent.el --- Tests for efrit-agent buffer -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;;; Commentary:
;; Comprehensive tests for the efrit-agent buffer module.
;; Tests cover:
;; - Buffer creation and mode activation
;; - Header rendering with all status types
;; - Task section with various TODO states
;; - Activity section with tool calls
;; - Input section interaction
;; - Session lifecycle
;; - Status transitions

;;; Code:

(require 'ert)
(require 'efrit-agent)

;;; Test helpers

(defmacro with-efrit-agent-test-buffer (&rest body)
  "Execute BODY with a clean efrit-agent buffer."
  (declare (indent 0) (debug t))
  `(let ((efrit-agent-auto-show nil))  ; Don't try to display buffer
     (with-current-buffer (efrit-agent--get-buffer)
       (let ((inhibit-read-only t))
         (erase-buffer))
       (efrit-agent-mode)
       ;; Reset state
       (setq efrit-agent--session-id nil)
       (setq efrit-agent--command nil)
       (setq efrit-agent--status 'idle)
       (setq efrit-agent--start-time nil)
       (setq efrit-agent--todos nil)
       (setq efrit-agent--activities nil)
       (setq efrit-agent--pending-question nil)
       (setq efrit-agent--expanded-items (make-hash-table :test 'equal))
       ,@body)))

;;; Buffer creation tests

(ert-deftest test-efrit-agent-buffer-creation ()
  "Test that agent buffer can be created."
  (with-efrit-agent-test-buffer
    (should (eq major-mode 'efrit-agent-mode))
    (should (equal (buffer-name) "*efrit-agent*"))))

(ert-deftest test-efrit-agent-start-session ()
  "Test session initialization."
  (with-efrit-agent-test-buffer
    (efrit-agent-start-session "test-session-123" "Tell me a joke")
    (should (equal efrit-agent--session-id "test-session-123"))
    (should (equal efrit-agent--command "Tell me a joke"))
    (should (eq efrit-agent--status 'working))
    (should efrit-agent--start-time)))

;;; Status rendering tests

(ert-deftest test-efrit-agent-status-idle ()
  "Test idle status rendering."
  (with-efrit-agent-test-buffer
    (setq efrit-agent--status 'idle)
    (efrit-agent--render)
    (should (string-match-p "Idle" (buffer-string)))))

(ert-deftest test-efrit-agent-status-working ()
  "Test working status rendering."
  (with-efrit-agent-test-buffer
    (setq efrit-agent--status 'working)
    (setq efrit-agent--start-time (current-time))
    (efrit-agent--render)
    (should (string-match-p "Working" (buffer-string)))))

(ert-deftest test-efrit-agent-status-waiting ()
  "Test waiting status rendering."
  (with-efrit-agent-test-buffer
    (setq efrit-agent--status 'waiting)
    (setq efrit-agent--start-time (current-time))
    (efrit-agent--render)
    (should (string-match-p "Waiting" (buffer-string)))))

(ert-deftest test-efrit-agent-status-complete ()
  "Test complete status rendering."
  (with-efrit-agent-test-buffer
    (setq efrit-agent--status 'complete)
    (efrit-agent--render)
    (should (string-match-p "Complete" (buffer-string)))))

(ert-deftest test-efrit-agent-status-failed ()
  "Test failed status rendering."
  (with-efrit-agent-test-buffer
    (setq efrit-agent--status 'failed)
    (efrit-agent--render)
    (should (string-match-p "Failed" (buffer-string)))))

;;; Task section tests

(ert-deftest test-efrit-agent-no-tasks ()
  "Test rendering with no tasks."
  (with-efrit-agent-test-buffer
    (setq efrit-agent--todos nil)
    (efrit-agent--render)
    (should (string-match-p "No tasks yet" (buffer-string)))))

(ert-deftest test-efrit-agent-tasks-complete ()
  "Test rendering completed tasks."
  (with-efrit-agent-test-buffer
    (setq efrit-agent--todos
          (list '(:id "1" :content "Task one" :status completed)))
    (efrit-agent--render)
    (should (string-match-p "Task one" (buffer-string)))
    (should (string-match-p "1/1 complete" (buffer-string)))))

(ert-deftest test-efrit-agent-tasks-in-progress ()
  "Test rendering in-progress tasks."
  (with-efrit-agent-test-buffer
    (setq efrit-agent--todos
          (list '(:id "1" :content "Current task" :status in_progress)))
    (efrit-agent--render)
    (should (string-match-p "Current task" (buffer-string)))
    (should (string-match-p "<- current" (buffer-string)))))

(ert-deftest test-efrit-agent-tasks-mixed ()
  "Test rendering mixed task states."
  (with-efrit-agent-test-buffer
    (setq efrit-agent--todos
          (list '(:id "1" :content "Done task" :status completed)
                '(:id "2" :content "Current task" :status in_progress)
                '(:id "3" :content "Pending task" :status pending)))
    (efrit-agent--render)
    (should (string-match-p "Done task" (buffer-string)))
    (should (string-match-p "Current task" (buffer-string)))
    (should (string-match-p "Pending task" (buffer-string)))
    (should (string-match-p "1/3 complete" (buffer-string)))))

;;; Activity section tests

(ert-deftest test-efrit-agent-no-activity ()
  "Test rendering with no activity."
  (with-efrit-agent-test-buffer
    (setq efrit-agent--activities nil)
    (efrit-agent--render)
    (should (string-match-p "No activity yet" (buffer-string)))))

(ert-deftest test-efrit-agent-tool-activity ()
  "Test rendering tool call activity."
  (with-efrit-agent-test-buffer
    (setq efrit-agent--activities
          (list (list :id "tool-1"
                      :type 'tool
                      :tool "read_file"
                      :result "file contents"
                      :success t
                      :timestamp (current-time))))
    (efrit-agent--render)
    (should (string-match-p "read_file" (buffer-string)))
    (should (string-match-p "file contents" (buffer-string)))))

(ert-deftest test-efrit-agent-message-activity ()
  "Test rendering Claude message activity."
  (with-efrit-agent-test-buffer
    (setq efrit-agent--activities
          (list (list :id "msg-1"
                      :type 'message
                      :text "I'll help you with that"
                      :timestamp (current-time))))
    (efrit-agent--render)
    (should (string-match-p "I'll help you with that" (buffer-string)))))

(ert-deftest test-efrit-agent-error-activity ()
  "Test rendering error activity."
  (with-efrit-agent-test-buffer
    (setq efrit-agent--activities
          (list (list :id "err-1"
                      :type 'error
                      :text "Something went wrong"
                      :timestamp (current-time))))
    (efrit-agent--render)
    (should (string-match-p "Something went wrong" (buffer-string)))))

;;; Input section tests

(ert-deftest test-efrit-agent-input-hidden-when-not-waiting ()
  "Test that input section is hidden when not in waiting state."
  (with-efrit-agent-test-buffer
    (setq efrit-agent--status 'working)
    (efrit-agent--render)
    (should-not (string-match-p "Input" (buffer-string)))))

(ert-deftest test-efrit-agent-input-shown-when-waiting ()
  "Test that input section appears when waiting."
  (with-efrit-agent-test-buffer
    (setq efrit-agent--status 'waiting)
    (setq efrit-agent--start-time (current-time))
    (efrit-agent--render)
    (should (string-match-p "Input" (buffer-string)))))

(ert-deftest test-efrit-agent-input-shows-question ()
  "Test that pending question is displayed."
  (with-efrit-agent-test-buffer
    (setq efrit-agent--status 'waiting)
    (setq efrit-agent--start-time (current-time))
    (setq efrit-agent--pending-question
          '("Which approach?" nil "2025-01-01T00:00:00"))
    (efrit-agent--render)
    (should (string-match-p "Question:" (buffer-string)))
    (should (string-match-p "Which approach?" (buffer-string)))))

(ert-deftest test-efrit-agent-input-shows-options ()
  "Test that options are displayed when available."
  (with-efrit-agent-test-buffer
    (setq efrit-agent--status 'waiting)
    (setq efrit-agent--start-time (current-time))
    (setq efrit-agent--pending-question
          '("Which one?" ("Option A" "Option B" "Option C") "2025-01-01T00:00:00"))
    (efrit-agent--render)
    (should (string-match-p "Options:" (buffer-string)))
    (should (string-match-p "\\[1\\] Option A" (buffer-string)))
    (should (string-match-p "\\[2\\] Option B" (buffer-string)))
    (should (string-match-p "\\[3\\] Option C" (buffer-string)))))

;;; Status transition tests

(ert-deftest test-efrit-agent-status-transition-working-to-waiting ()
  "Test transition from working to waiting state."
  (with-efrit-agent-test-buffer
    (setq efrit-agent--status 'working)
    (setq efrit-agent--start-time (current-time))
    (efrit-agent--render)
    (should (string-match-p "Working" (buffer-string)))

    ;; Transition to waiting
    (setq efrit-agent--status 'waiting)
    (setq efrit-agent--pending-question '("Continue?" nil "ts"))
    (efrit-agent--render)
    (should (string-match-p "Waiting" (buffer-string)))
    (should (string-match-p "Input" (buffer-string)))))

(ert-deftest test-efrit-agent-status-transition-working-to-complete ()
  "Test transition from working to complete state."
  (with-efrit-agent-test-buffer
    (setq efrit-agent--status 'working)
    (setq efrit-agent--start-time (current-time))
    (efrit-agent--render)
    (should (string-match-p "Working" (buffer-string)))

    ;; Transition to complete
    (efrit-agent-end-session t)
    (should (string-match-p "Complete" (buffer-string)))))

(ert-deftest test-efrit-agent-status-transition-working-to-failed ()
  "Test transition from working to failed state."
  (with-efrit-agent-test-buffer
    (setq efrit-agent--status 'working)
    (setq efrit-agent--start-time (current-time))
    (efrit-agent--render)

    ;; Transition to failed
    (efrit-agent-end-session nil)
    (should (string-match-p "Failed" (buffer-string)))))

;;; API function tests

(ert-deftest test-efrit-agent-add-activity ()
  "Test adding activity entries."
  (with-efrit-agent-test-buffer
    (should (null efrit-agent--activities))
    (efrit-agent-add-activity
     (list :type 'tool :tool "test_tool" :timestamp (current-time)))
    (should (= 1 (length efrit-agent--activities)))))

(ert-deftest test-efrit-agent-update-todos ()
  "Test updating TODO list."
  (with-efrit-agent-test-buffer
    (should (null efrit-agent--todos))
    (efrit-agent-update-todos
     (list '(:id "1" :content "New task" :status pending)))
    (should (= 1 (length efrit-agent--todos)))))

(ert-deftest test-efrit-agent-set-status ()
  "Test setting session status."
  (with-efrit-agent-test-buffer
    (should (eq efrit-agent--status 'idle))
    (efrit-agent-set-status 'working)
    (should (eq efrit-agent--status 'working))))

;;; Display style tests

(ert-deftest test-efrit-agent-unicode-display ()
  "Test Unicode display characters."
  (let ((efrit-agent-display-style 'unicode))
    (should (equal "●" (efrit-agent--char 'status-working)))
    (should (equal "✓" (efrit-agent--char 'task-complete)))
    (should (equal "▶" (efrit-agent--char 'task-in-progress)))))

(ert-deftest test-efrit-agent-ascii-display ()
  "Test ASCII display characters."
  (let ((efrit-agent-display-style 'ascii))
    (should (equal "*" (efrit-agent--char 'status-working)))
    (should (equal "[x]" (efrit-agent--char 'task-complete)))
    (should (equal "->" (efrit-agent--char 'task-in-progress)))))

;;; Verbosity tests

(ert-deftest test-efrit-agent-cycle-verbosity ()
  "Test verbosity cycling."
  (with-efrit-agent-test-buffer
    (setq efrit-agent-verbosity 'minimal)
    (efrit-agent-cycle-verbosity)
    (should (eq efrit-agent-verbosity 'normal))
    (efrit-agent-cycle-verbosity)
    (should (eq efrit-agent-verbosity 'verbose))
    (efrit-agent-cycle-verbosity)
    (should (eq efrit-agent-verbosity 'minimal))))

;;; Expansion tests

(ert-deftest test-efrit-agent-expand-collapse ()
  "Test tool call expansion and collapse."
  (with-efrit-agent-test-buffer
    (setq efrit-agent--activities
          (list (list :id "tool-1"
                      :type 'tool
                      :tool "test_tool"
                      :input "{\"key\": \"value\"}"
                      :result "output"
                      :success t
                      :timestamp (current-time))))
    (efrit-agent--render)

    ;; Initially collapsed
    (should-not (gethash "tool-1" efrit-agent--expanded-items))

    ;; Expand
    (puthash "tool-1" t efrit-agent--expanded-items)
    (efrit-agent--render)
    (should (string-match-p "Input:" (buffer-string)))

    ;; Collapse
    (remhash "tool-1" efrit-agent--expanded-items)
    (efrit-agent--render)
    (should-not (string-match-p "Input:" (buffer-string)))))

;;; Elapsed time formatting tests

(ert-deftest test-efrit-agent-format-elapsed-seconds ()
  "Test elapsed time formatting for seconds."
  (with-efrit-agent-test-buffer
    (setq efrit-agent--start-time (time-subtract (current-time) 5))
    (let ((elapsed (efrit-agent--format-elapsed)))
      (should (string-match-p "\\`[0-9]+\\.[0-9]s\\'" elapsed)))))

(ert-deftest test-efrit-agent-format-elapsed-minutes ()
  "Test elapsed time formatting for minutes."
  (with-efrit-agent-test-buffer
    (setq efrit-agent--start-time (time-subtract (current-time) 90))
    (let ((elapsed (efrit-agent--format-elapsed)))
      (should (string-match-p "\\`[0-9]+:[0-9][0-9]\\'" elapsed)))))

(provide 'test-efrit-agent)

;;; test-efrit-agent.el ends here
