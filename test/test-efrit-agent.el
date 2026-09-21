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

;;; Inline diff display tests

(ert-deftest test-efrit-agent-diff-content-detection ()
  "Test detection of diff content in strings."
  ;; Git diff format
  (should (efrit-agent--diff-content-p "diff --git a/test.el b/test.el"))
  ;; Unified diff headers
  (should (efrit-agent--diff-content-p "--- a/test.el\n+++ b/test.el"))
  ;; Hunk headers
  (should (efrit-agent--diff-content-p "@@ -1,3 +1,4 @@\n context"))
  ;; Not a diff
  (should-not (efrit-agent--diff-content-p "This is regular text"))
  (should-not (efrit-agent--diff-content-p "function returns nil")))

(ert-deftest test-efrit-agent-diff-extraction-from-alist ()
  "Test extraction of diff content from vcs_diff style results."
  (let ((vcs-result '((diff . "--- a/test.el\n+++ b/test.el")
                      (summary . ((files_changed . 1))))))
    (should (equal (efrit-agent--extract-diff-from-result vcs-result)
                   "--- a/test.el\n+++ b/test.el"))))

(ert-deftest test-efrit-agent-diff-extraction-from-string ()
  "Test extraction of diff content from string results."
  (let ((diff-string "diff --git a/test.el b/test.el\n--- a/test.el"))
    (should (equal (efrit-agent--extract-diff-from-result diff-string)
                   diff-string))))

(ert-deftest test-efrit-agent-diff-line-faces ()
  "Test that diff lines get appropriate faces."
  (let ((added (efrit-agent--format-diff-line "+new line" ""))
        (removed (efrit-agent--format-diff-line "-old line" ""))
        (context (efrit-agent--format-diff-line " same line" ""))
        (hunk (efrit-agent--format-diff-line "@@ -1,3 +1,4 @@" "")))
    (should (eq (get-text-property 0 'face added) 'efrit-agent-diff-added))
    (should (eq (get-text-property 0 'face removed) 'efrit-agent-diff-removed))
    (should (eq (get-text-property 0 'face context) 'efrit-agent-diff-context))
    (should (eq (get-text-property 0 'face hunk) 'efrit-agent-diff-hunk-header))))

(ert-deftest test-efrit-agent-diff-formatting-integration ()
  "Test full diff formatting in tool expansion."
  (let ((efrit-agent-show-diff t)
        (efrit-agent-verbosity 'normal)  ; Ensure consistent verbosity
        (result '((diff . "diff --git a/test.el b/test.el\n--- a/test.el\n+++ b/test.el\n@@ -1 +1 @@\n-old\n+new"))))
    (let ((formatted (efrit-agent--format-tool-expansion nil result t)))
      ;; Should contain the diff content
      (should (string-match-p "diff --git" formatted))
      ;; Should have diff faces applied
      (let ((has-added-face nil))
        (dotimes (i (length formatted))
          (when (eq (get-text-property i 'face formatted) 'efrit-agent-diff-added)
            (setq has-added-face t)))
        (should has-added-face)))))

(ert-deftest test-efrit-agent-diff-disabled ()
  "Test that diff highlighting can be disabled."
  (let ((efrit-agent-show-diff nil)
        (efrit-agent-verbosity 'normal)  ; Ensure consistent verbosity
        (result '((diff . "diff --git a/test.el b/test.el\n+new"))))
    (let ((formatted (efrit-agent--format-tool-expansion nil result t)))
      ;; Should contain diff text but NOT have diff-specific faces
      (should (string-match-p "diff" formatted))
      (let ((has-diff-face nil))
        (dotimes (i (length formatted))
          (when (memq (get-text-property i 'face formatted)
                      '(efrit-agent-diff-added efrit-agent-diff-removed))
            (setq has-diff-face t)))
        (should-not has-diff-face)))))

(ert-deftest test-efrit-agent-prompt-cannot-be-deleted ()
  "Backspace at the start of the input, or a kill over the prompt, is refused.
The prompt text is read-only with front-sticky read-only, both at
buffer setup and after `efrit-agent--set-input-prompt' rewrites it."
  (require 'efrit-agent-input)
  (efrit)
  (with-current-buffer (efrit-agent--get-buffer)
    (unwind-protect
        (progn
          (goto-char (point-max))
          (insert "abc")
          (goto-char efrit-agent--input-start)
          (should-error (delete-char -1) :type 'text-read-only)
          ;; the field keeps line-beginning-position after the prompt,
          ;; so a kill over "the line" no longer reaches it; a region
          ;; that explicitly spans the prompt is still refused
          (goto-char (point-max))
          (should (= (line-beginning-position) efrit-agent--input-start))
          (should-error (delete-region (let ((inhibit-field-text-motion t)) (line-beginning-position))
                                       (point))
                        :type 'text-read-only)
          (should (equal (efrit-agent--get-input) "abc"))
          ;; a question rewrites the prompt; still protected
          (efrit-agent--set-input-prompt "Answer (or 1/2): ")
          (goto-char efrit-agent--input-start)
          (should-error (delete-char -1) :type 'text-read-only)
          (should (get-text-property (1- efrit-agent--input-start) 'efrit-agent-prompt))
          ;; typing at the marker still works and lands in the input
          (goto-char (point-max))
          (insert "d")
          (should (equal (efrit-agent--get-input) "abcd")))
      (efrit-agent--clear-input))))

(ert-deftest test-efrit-agent-input-is-a-field-like-comint ()
  "Transcript and prompt are the output field; C-a/kill-line stop at the prompt,
C-p still crosses into the transcript, C-c C-u kills the whole input."
  (require 'efrit-agent-input)
  (efrit)
  (with-current-buffer (efrit-agent--get-buffer)
    (unwind-protect
        (progn
          (efrit-agent--append-to-conversation "earlier output\n" nil)
          (should (eq (get-text-property (1- efrit-agent--input-start) 'field) 'output))
          (should (eq (get-text-property (point-min) 'field) 'output))
          (goto-char (point-max)) (insert "hello world")
          ;; plain beginning-of-line honours the field
          (beginning-of-line)
          (should (= (point) efrit-agent--input-start))
          ;; input-bol: after the prompt; again: the true line start
          (goto-char (point-max)) (efrit-agent-input-bol)
          (should (= (point) efrit-agent--input-start))
          (efrit-agent-input-bol)
          (should (= (point) (let ((inhibit-field-text-motion t)) (line-beginning-position))))
          ;; kill-line from input start kills the input and nothing else
          (goto-char efrit-agent--input-start) (kill-line)
          (should (equal (efrit-agent--get-input) ""))
          (should (get-text-property (1- efrit-agent--input-start) 'efrit-agent-prompt))
          ;; line motion crosses fields (inhibit-line-move-field-capture)
          (insert "xyz") (forward-line -1)
          (should (eq (get-text-property (point) 'field) 'output))
          ;; C-c C-u
          (goto-char (point-max)) (efrit-agent-input-kill)
          (should (equal (efrit-agent--get-input) ""))
          (should (equal (car kill-ring) "xyz")))
      (efrit-agent--clear-input))))

(ert-deftest test-efrit-agent-prompt-does-not-accumulate ()
  "Resetting the prompt replaces it; it used to append one \"> \" per reset."
  (require 'efrit-agent-input)
  (efrit)
  (with-current-buffer (efrit-agent--get-buffer)
    (dotimes (_ 3) (efrit-agent--reset-input-prompt))
    (efrit-agent--set-input-prompt "Answer: ")
    (efrit-agent--reset-input-prompt)
    (let ((line (buffer-substring-no-properties
                 (let ((inhibit-field-text-motion t))
                   (save-excursion (goto-char (point-max)) (line-beginning-position)))
                 (point-max))))
      (should (equal line "> ")))))

(ert-deftest test-efrit-agent-arrows-history-at-edges-motion-inside ()
  "Up on the first input line recalls history; inside a multi-line input it moves up."
  (require 'efrit-agent-input)
  (efrit)
  (with-current-buffer (efrit-agent--get-buffer)
    (unwind-protect
        (let ((efrit-agent--input-history (list "second cmd" "first cmd"))
              (efrit-agent--global-history nil)
              (efrit-agent--history-index -1)
              (efrit-agent--history-temp nil))
          (goto-char (point-max)) (insert "draft")
          ;; one-line input: up = previous history, saving the draft
          (efrit-agent-input-up)
          (should (equal (efrit-agent--get-input) "second cmd"))
          (efrit-agent-input-up)
          (should (equal (efrit-agent--get-input) "first cmd"))
          ;; down twice: back through history, then the draft returns
          (efrit-agent-input-down)
          (should (equal (efrit-agent--get-input) "second cmd"))
          (efrit-agent-input-down)
          (should (equal (efrit-agent--get-input) "draft"))
          ;; multi-line input: up from the second line is line motion
          (efrit-agent--clear-input)
          (goto-char (point-max)) (insert "line one\nline two")
          (efrit-agent-input-up)
          (should (equal (efrit-agent--get-input) "line one\nline two"))
          (should (efrit-agent--input-first-line-p)))
      (efrit-agent--clear-input))))

(ert-deftest test-efrit-agent-ret-sends-multiline-input-and-shift-selects ()
  "RET sends from any line of a multi-line input (S-RET made the lines);
S-<up> on the first line extends the selection instead of recalling history;
the header hint names the keys."
  (require 'efrit-agent-input)
  (efrit)
  (with-current-buffer (efrit-agent--get-buffer)
    (let ((sent nil) (efrit-agent--input-history (list "old")))
      (cl-letf (((symbol-function 'efrit-agent--repl-send) (lambda (input) (setq sent input))))
        (unwind-protect
            (progn
              (goto-char (point-max)) (insert "line one")
              (efrit-agent-input-newline) (insert "line two")
              (should (efrit-agent--in-input-region-p))
              ;; point is on the last line; RET sends the whole thing
              (efrit-agent-input-send-or-newline)
              (should (equal sent "line one\nline two"))
              (should (equal (efrit-agent--get-input) ""))
              ;; from the first line too
              (setq sent nil)
              (goto-char (point-max)) (insert "a") (efrit-agent-input-newline) (insert "b")
              (goto-char efrit-agent--input-start) (forward-char 1)
              (efrit-agent-input-send-or-newline)
              (should (equal sent "a\nb"))
              ;; shift-up on a one-line input: selection, not history
              (goto-char (point-max)) (insert "x") (efrit-agent-input-newline) (insert "keep me")
              (goto-char (point-max))
              (let ((this-command-keys-shift-translated t)
                    (shift-select-mode t)
                    (transient-mark-mode t)
                    (start (point)))
                ;; the command loop runs this for an (interactive "^") command
                (handle-shift-selection)
                (efrit-agent-input-up)
                (should (equal (efrit-agent--get-input) "x\nkeep me"))
                (should (region-active-p))
                (should (= (mark) start))
                (should (< (point) start)))
              (deactivate-mark)
              ;; the hint names both keys
              (let ((hint (efrit-agent-input-hint)))
                (should (string-match-p "RET sends" hint))
                (should (string-match-p "S-<return> newline" hint))))
          (efrit-agent--clear-input))))))

(ert-deftest test-efrit-agent-user-turn-keeps-prefix-and-text-faces ()
  "The user block background is layered under the prompt/text faces, not over them."
  (efrit)
  (with-current-buffer (efrit-agent--get-buffer)
    (efrit-agent--add-user-message "check limits")
    (goto-char (point-min)) (search-forward "check")
    (let ((face (get-text-property (point) 'face)))
      (should (memq 'efrit-agent-user-message (ensure-list face)))
      (should (memq 'efrit-agent-user-block (ensure-list face))))
    (goto-char (point-min)) (search-forward "❯")
    (should (memq 'efrit-agent-user-prefix (ensure-list (get-text-property (1- (point)) 'face))))))

(ert-deftest test-efrit-agent-review-rows-show-approve-and-reject ()
  "The reviewer's start and verdict events render as a tool-style row."
  (require 'efrit-review)
  (efrit)
  (with-current-buffer (efrit-agent--get-buffer)
    (efrit-publish 'review-start '((:session-id . "s") (:calls . 2) (:model . "m") (:prompt . "P")))
    (efrit-publish 'review-verdict '((:session-id . "s") (:verdict . approve)))
    (efrit-publish 'review-start '((:session-id . "s") (:calls . 1) (:model . "m") (:prompt . "P")))
    (efrit-publish 'review-verdict '((:session-id . "s") (:verdict . reject) (:reason . "wrong file")))
    (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
           ;; header rows only: the rejected one auto-expands and its
           ;; body mentions the reviewer too
           (rows (seq-filter (lambda (l) (string-match-p "[✓✗] review:" l)) (split-string text "\n"))))
      (should (= (length rows) 2))
      (should (string-match-p "✓ review: 2 tool calls · m · approved" (nth 0 rows)))
      (should (string-match-p "✗ review: 1 tool call · m · rejected · wrong file" (nth 1 rows))))
    ;; d expands in the transcript, inserts in the input
    (goto-char (point-min)) (search-forward "review")
    (should (eq (key-binding "d") 'efrit-agent-toggle-expand))
    (goto-char (point-max)) (efrit-agent--maybe-enable-input-mode)
    (should (eq (key-binding "d") 'self-insert-command))))

(ert-deftest test-efrit-agent-report-buffer-quits-and-opens-from-row ()
  (require 'efrit-tool-edit-buffer)
  (efrit)
  (unwind-protect
      (progn
        (efrit-tool-create-buffer '((name . "*efrit-report: T*") (content . "x\n") (mode . "text-mode")))
        (with-current-buffer "*efrit-report: T*"
          (should (eq (key-binding "q") 'quit-window))
          (should efrit-tool-report-buffer))
        (with-current-buffer (efrit-agent--get-buffer)
          (let ((h (make-hash-table :test 'equal)))
            (puthash "name" "*efrit-report: T*" h)
            (let ((id (efrit-agent-show-tool-start "buffer_create" h)))
              (efrit-agent-show-tool-result id "Created buffer '*efrit-report: T*' with 2 characters" t 0.0)))
          (let ((row (seq-find (lambda (l) (string-match-p "buffer_create" l))
                               (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n"))))
            (should (string-match-p "\\*efrit-report: T\\* (2 chars) · o opens" row)))
          ;; not shown until asked
          (should-not (get-buffer-window "*efrit-report: T*"))
          (goto-char (point-min)) (search-forward "buffer_create")
          (cl-letf (((symbol-function 'select-window) (lambda (w &rest _) w)))
            (efrit-agent-open-at-point))
          (should (get-buffer-window "*efrit-report: T*"))))
    (when (get-buffer "*efrit-report: T*")
      (ignore-errors (delete-window (get-buffer-window "*efrit-report: T*")))
      (kill-buffer "*efrit-report: T*"))))

(ert-deftest test-efrit-agent-display-reuses-window ()
  "Showing the agent buffer twice does not create a second window for it."
  (efrit)
  (let* ((buf (efrit-agent--get-buffer))
         (before (length (window-list))))
    (efrit-agent-display buf)
    (let ((after-first (length (window-list))))
      (efrit-agent-display buf)
      (efrit-agent-display buf t)
      (should (= (length (window-list)) after-first))
      (should (= 1 (length (get-buffer-window-list buf nil t))))
      (should (<= (- after-first before) 1)))))

(ert-deftest test-efrit-agent-quit-removes-the-split-eshell-style ()
  "The window efrit made for itself goes away on quit, in every path:
plain open, redisplay while visible, and reopen after switching away.
Batch Emacs may refuse to split a tiny frame; skip then."
  (set-frame-height nil 60)
  (delete-other-windows)
  (switch-to-buffer "*scratch*")
  (let ((buf (progn (efrit) (efrit-agent--get-buffer))))
    (skip-unless (> (length (window-list)) 1))
    (cl-flet ((quit-agent ()
                (select-window (get-buffer-window buf))
                (efrit-agent-quit)))
      ;; open + quit
      (quit-agent)
      (should (= 1 (length (window-list))))
      ;; open, show again while visible (a session does this), quit
      (efrit) (efrit-agent-display buf t) (efrit-agent--show-buffer)
      (quit-agent)
      (should (= 1 (length (window-list))))
      ;; the window is dedicated and carries a delete-window quit-restore
      (efrit)
      (let ((w (get-buffer-window buf)))
        (should (window-dedicated-p w))
        (should (eq 'window (car (window-parameter w 'quit-restore)))))
      (quit-agent)
      (should (= 1 (length (window-list))))
      ;; a side-by-side layout: quitting removes only efrit's split
      (split-window-right)
      (efrit)
      (should (= 3 (length (window-list))))
      (quit-agent)
      (should (= 2 (length (window-list))))
      (delete-other-windows)
      ;; killing the buffer (not quit-window) also removes the split
      (efrit)
      (should (= 2 (length (window-list))))
      (select-window (get-buffer-window buf))
      (kill-current-buffer)
      (should (= 1 (length (window-list))))
      ;; a window made by older code (plain split, not dedicated) is
      ;; upgraded when M-x efrit finds the buffer already in it
      (let* ((buf2 (progn (efrit) (efrit-agent--get-buffer)))
             (w (get-buffer-window buf2)))
        (set-window-dedicated-p w nil)
        (efrit)
        (should (window-dedicated-p w))
        (select-window w) (kill-current-buffer)
        (should (= 1 (length (window-list))))))))

(provide 'test-efrit-agent)

;;; test-efrit-agent.el ends here
