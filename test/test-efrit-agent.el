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

;;; Submitting while a turn runs

(defun efrit-agent--get-input-raw ()
  "The input text with trailing whitespace kept (the trimmed getter hides list edits)."
  (buffer-substring-no-properties efrit-agent--input-start (point-max)))

(defun test-efrit-agent--type (text)
  "Put TEXT into the input region."
  (efrit-agent--clear-input)
  (goto-char (point-max))
  (insert text))

(ert-deftest test-efrit-agent-busy-submit-queues-then-steers ()
  "RET on a busy session queues, M-RET steers, both show the line at once
with their own prefix; the queue drains in order when the turn ends,
turning the waiting mark into a sent one, and keeps the user's draft;
a failed turn holds the queue.  `efrit-submit' from Lisp returns nil
on a busy session without drawing anything."
  (with-efrit-agent-test-buffer
    (efrit-agent--init-regions)
    (efrit-agent--setup-regions)
    (let ((session (efrit-agent-repl-session))
          (sent nil) (events nil))
      (cl-letf (((symbol-function 'efrit-agent--repl-send)
                 (lambda (input &optional _api) (push input sent) t))
                ((symbol-function 'run-at-time)
                 (lambda (_s _r fn &rest args) (apply fn args) nil))
                ((symbol-function 'efrit-agent-display) #'ignore))
        (efrit-subscribe 'steer (lambda (e) (push (cons 'steer (alist-get :text e)) events)))
        (efrit-subscribe 'queued (lambda (e) (push (cons 'queued (alist-get :text e)) events)))
        (unwind-protect
            (progn
              (efrit-repl-loop-hold session)
              ;; RET: queued, drawn with the waiting prefix, input cleared
              (test-efrit-agent--type "second thing")
              (efrit-agent-input-send)
              (should (equal '("second thing") (efrit-repl-session-queue session)))
              (should (equal "" (efrit-agent--get-input)))
              (should (efrit-agent--find-user-message "second thing" 'queued))
              (should (equal '(queued . "second thing") (car events)))
              (should-not sent)
              ;; M-RET: steer event, drawn as a steer line
              (test-efrit-agent--type "and be brief")
              (efrit-agent-input-send-override)
              (should (equal '(steer . "and be brief") (car events)))
              (should (efrit-agent--find-user-message "and be brief" 'steer))
              (should-not sent)
              ;; Lisp callers get nil and nothing drawn
              (should-not (efrit-submit "from lisp"))
              (should-not (efrit-agent--find-user-message "from lisp" nil))
              ;; Another queued one, then a draft the user is typing
              (test-efrit-agent--type "third thing")
              (efrit-agent-input-send)
              (test-efrit-agent--type "draft in progress")
              ;; A failed turn holds the queue
              (efrit-repl-loop-release session)
              (efrit-agent--on-turn-complete session "api-error")
              (should-not sent)
              (should (= 2 (length (efrit-repl-session-queue session))))
              ;; A good turn sends the oldest, keeps the draft, remarks the line
              (efrit-agent--on-turn-complete session "end_turn")
              (should (equal '("second thing") sent))
              (should (equal '("third thing") (efrit-repl-session-queue session)))
              (should (equal "draft in progress" (efrit-agent--get-input)))
              (should-not (efrit-agent--find-user-message "second thing" 'queued))
              (should (efrit-agent--find-user-message "second thing" nil))
              ;; Resume sends the next by hand
              (efrit-agent-queue-resume)
              (should (equal '("third thing" "second thing") sent))
              (should-not (efrit-repl-session-queue session))
              ;; Undo history holds only the draft
              (should (equal (list (cons (marker-position efrit-agent--input-start) (point-max)))
                             buffer-undo-list)))
          (dolist (type '(steer queued))
            (dolist (fn (cdr (assq type efrit-events--subscribers)))
              (unless (symbolp fn) (efrit-unsubscribe type fn)))))))))

(ert-deftest test-efrit-agent-fold-is-invisible-and-isearch-opens ()
  "A collapsed tool body stays in the buffer as invisible text; toggling
flips visibility and the glyph in place; the isearch predicate skips,
opens, or accepts hidden matches per `search-invisible', and cleanup
folds back what it opened unless point is there."
  (with-efrit-agent-test-buffer
    (efrit-agent--init-regions)
    (efrit-agent--setup-regions)
    (let* ((efrit-agent-display-mode 'minimal)
           (id (efrit-agent--add-tool-call "eval_sexp" '(("expr" . "(+ 1 1)")))))
      (efrit-agent--update-tool-result id "the-hidden-answer" t 0.2)
      (let* ((region (efrit-agent--find-tool-region id))
             (body (efrit-agent--tool-body-bounds (car region) (cdr region))))
        (should body)
        (should (string-match-p "the-hidden-answer" (buffer-substring (car body) (cdr body))))
        (should (eq 'efrit-tool-body (get-text-property (car body) 'invisible)))
        (should-not (get-text-property (car region) 'efrit-tool-expanded))
        ;; Toggle in place: same region start, glyph flips, body visible
        (goto-char (car region))
        (should (efrit-agent--toggle-tool-expansion))
        (should (get-text-property (car region) 'efrit-tool-expanded))
        (should-not (get-text-property (car body) 'invisible))
        (should (string-match-p (regexp-quote (efrit-agent--char 'expand-expanded))
                                (buffer-substring (car region) (+ (car region) 6))))
        (efrit-agent--toggle-tool-expansion)
        (should (eq 'efrit-tool-body (get-text-property (car body) 'invisible)))
        ;; isearch predicate
        (let ((m (save-excursion (goto-char (car body)) (search-forward "hidden") (point))))
          (let ((search-invisible nil))
            (should-not (efrit-agent--isearch-filter (- m 6) m)))
          (let ((search-invisible t))
            (should (efrit-agent--isearch-filter (- m 6) m))
            (should (get-text-property (car body) 'invisible)))
          (let ((search-invisible 'open))
            (should (efrit-agent--isearch-filter (- m 6) m))
            (should-not (get-text-property (car body) 'invisible))
            (should (equal (list id) efrit-agent--isearch-opened)))
          ;; Cleanup with point elsewhere folds it back
          (goto-char (point-max))
          (efrit-agent--isearch-cleanup)
          (should (get-text-property (car body) 'invisible))
          (should-not efrit-agent--isearch-opened)
          ;; Cleanup with point on the match keeps it open
          (let ((search-invisible 'open)) (efrit-agent--isearch-filter (- m 6) m))
          (goto-char m)
          (efrit-agent--isearch-cleanup)
          (should-not (get-text-property (car body) 'invisible)))
        ;; The mode set the buffer up
        (should (eq isearch-filter-predicate #'efrit-agent--isearch-filter))
        (should (memq 'efrit-tool-body buffer-invisibility-spec))))))

(ert-deftest test-efrit-agent-copy-last-output-and-restart ()
  "Copy-last-output takes the newest answer wherever point is; restart
gives a fresh session in the same windows and drops the queue."
  (with-efrit-agent-test-buffer
    (efrit-agent--init-regions)
    (efrit-agent--setup-regions)
    (let ((session (efrit-agent-repl-session))
          (kill-ring nil))
      (efrit-agent--add-user-message "q1")
      (efrit-agent--add-claude-message "first answer")
      (efrit-agent--stream-end-message)
      (efrit-agent--add-user-message "q2")
      (efrit-agent--add-claude-message "second ")
      (efrit-agent--add-claude-message "answer")
      (efrit-agent--stream-end-message)
      (goto-char (point-min))
      (efrit-agent-copy-last-output)
      (should (equal "second answer" (car kill-ring)))
      (should (equal (efrit-repl-session-id session) (efrit-agent-session-id)))
      (efrit-agent-copy-session-id)
      (should (equal (efrit-repl-session-id session) (car kill-ring)))
      (efrit-repl-session-enqueue session "later")
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
        (efrit-agent-restart))
      (should-not (equal session efrit-agent--repl-session))
      (should-not (efrit-repl-session-queue efrit-agent--repl-session))
      (should-not (string-match-p "second answer" (buffer-string)))
      (should (efrit-agent--in-input-region-p)))))

(ert-deftest test-efrit-agent-streamed-markdown-renders-in-place ()
  "Chunks of Markdown streamed into the agent buffer come out rendered:
markup gone, faces on, the conversation-end marker after the message,
the user's draft in the input untouched, no undo entries for it."
  (with-efrit-agent-test-buffer
    (efrit-agent--init-regions)
    (efrit-agent--setup-regions)
    (goto-char (point-max))
    (insert "my draft")
    (let ((efrit-markdown-enabled t))
      (dolist (c '("## Plan\n\nFirst **do" "** this:\n\n```elisp\n(setq x" " 1)\n```\n\nThen *rest*"))
        (efrit-agent--add-claude-message c))
      (efrit-agent--stream-end-message))
    (let* ((end (marker-position efrit-agent--conversation-end))
           (text (buffer-substring-no-properties (point-min) end)))
      (should-not (string-match-p "[#*`]" text))
      (should (string-match-p "Plan\n\nFirst do this:\n\nelisp\n(setq x 1)\n\nThen rest" text))
      ;; buffer positions are 1-based: string-search + 1
      (cl-flet ((at (needle) (1+ (string-search needle (buffer-string)))))
        (should (memq 'efrit-markdown-bold (ensure-list (get-text-property (at "do this") 'face))))
        (should-not (memq 'efrit-markdown-bold (ensure-list (get-text-property (at "this:") 'face))))
        (should (get-text-property (at "(setq") 'efrit-markdown-frozen))
        ;; still tagged as the model's message, read-only
        (should (eq 'claude-message (get-text-property (at "Plan") 'efrit-type)))
        (should (get-text-property (at "Plan") 'read-only)))
      (should (equal "my draft" (efrit-agent--get-input)))
      (should (equal (list (cons (marker-position efrit-agent--input-start) (point-max)))
                     buffer-undo-list)))))

(ert-deftest test-efrit-agent-cancel-ends-a-waiting-turn ()
  "Cancel on a session waiting for an answer ends the turn as interrupted,
withdraws the question, and leaves the session idle; on a working
session it sets the interrupt flag for the loop."
  (with-efrit-agent-test-buffer
    (efrit-agent--init-regions)
    (efrit-agent--setup-regions)
    (let ((session (efrit-agent-repl-session)) (ended nil))
      (cl-letf (((symbol-function 'efrit-api-stream-cancel) #'ignore)
                ((symbol-function 'efrit-session-active) (lambda () nil))
                ((symbol-function 'efrit-agent--refresh-status-line) #'ignore))
        (efrit-subscribe 'turn-complete (lambda (e) (push (alist-get :stop-reason e) ended)))
        (unwind-protect
            (progn
              (efrit-repl-session-set-status session 'waiting)
              (setf (efrit-repl-session-pending-question session) '(:q "which?"))
              (setq efrit-agent--status 'waiting)
              (efrit-agent-cancel)
              (should (eq 'idle (efrit-repl-session-status session)))
              (should-not (efrit-repl-session-pending-question session))
              (should (member "interrupted" ended))
              ;; working: only the flag, the loop finishes on its own
              (efrit-repl-loop-hold session)
              (setq efrit-agent--status 'working)
              (efrit-agent-cancel)
              (should (efrit-repl-session-interrupt-requested session))
              (should (eq 'working (efrit-repl-session-status session))))
          (dolist (fn (cdr (assq 'turn-complete efrit-events--subscribers)))
            (unless (symbolp fn) (efrit-unsubscribe 'turn-complete fn))))))))

(ert-deftest test-efrit-agent-last-answer-bounds-span-rendered-chrome ()
  "The last answer's bounds cover the whole message even after the
Markdown pass inserted a code label and bullets inside it."
  (with-efrit-agent-test-buffer
    (efrit-agent--init-regions)
    (efrit-agent--setup-regions)
    (efrit-agent--add-user-message "q")
    (efrit-agent--add-claude-message "## Report\n\nA **bold** word.\n\n- a\n- b\n\n```elisp\n(defun ok () t)\n```\n\nSee x.")
    (efrit-agent--stream-end-message)
    (let* ((b (efrit-agent--last-claude-message-bounds))
           (text (buffer-substring-no-properties (car b) (cdr b))))
      (should (string-prefix-p "Report" text))
      (should (string-suffix-p "See x." (string-trim text)))
      (should (memq 'efrit-markdown-header (ensure-list (get-text-property (car b) 'face))))
      ;; the bullet and the code label carry the message's id
      (let ((id (get-text-property (car b) 'efrit-id)))
        (should (equal id (get-text-property (1+ (string-search "•" (buffer-string))) 'efrit-id)))
        (should (equal id (get-text-property (1+ (string-search "elisp\n(defun" (buffer-string))) 'efrit-id)))))))

(ert-deftest test-efrit-agent-user-line-during-stream-closes-the-message ()
  "A steer or queued line drawn while the model streams ends the message
first; the next chunk starts a new one below the line, and the line
keeps its text and kind (it used to land inside the message and get
rendered as Markdown)."
  (with-efrit-agent-test-buffer
    (efrit-agent--init-regions)
    (efrit-agent--setup-regions)
    (let ((session (efrit-agent-repl-session)))
      (efrit-repl-loop-hold session)
      (efrit-agent--add-claude-message "I will now")
      (efrit-agent-busy-submit-steer "Change *of* plan?")
      (efrit-agent--add-claude-message " compute **it**.")
      (efrit-agent--stream-end-message)
      (let ((text (buffer-substring-no-properties (point-min) (marker-position efrit-agent--conversation-end))))
        (should (string-search "I will now\n\n↳ Change *of* plan?\n\n compute it." text)))
      (should (efrit-agent--find-user-message "Change *of* plan?" 'steer))
      ;; the thinking indicator was not touched by the steer line
      (efrit-repl-loop-hold session)
      (efrit-agent--show-thinking "waiting")
      (efrit-agent-busy-submit-queue "later")
      (should efrit-agent--thinking-indicator)
      (efrit-agent--hide-thinking))))

(ert-deftest test-efrit-agent-lines-drawn-under-the-thinking-indicator-survive-it ()
  "A queued or steer line (or a note) drawn while the thinking indicator
shows goes above the indicator and is still there after the indicator
is hidden.  It used to land inside the indicator's span and be deleted
with it: every mark drawn mid-turn vanished (2026-09-25)."
  (with-efrit-agent-test-buffer
    (efrit-agent--init-regions)
    (efrit-agent--setup-regions)
    (let ((session (efrit-agent-repl-session)))
      (efrit-repl-loop-hold session)
      (efrit-agent--show-thinking "waiting for Claude...")
      (should efrit-agent--thinking-indicator)
      (efrit-agent-busy-submit-queue "later please")
      (efrit-agent-busy-submit-steer "and now this")
      (efrit-agent--append-to-conversation "  a note\n" '(efrit-type note))
      (should efrit-agent--thinking-indicator)
      ;; the indicator is still the last thing
      (should (< (car (efrit-agent--find-user-message "and now this" 'steer))
                 (marker-position (car efrit-agent--thinking-indicator))))
      (efrit-agent--hide-thinking)
      (should (efrit-agent--find-user-message "later please" 'queued))
      (should (efrit-agent--find-user-message "and now this" 'steer))
      (should (string-search "a note" (buffer-string)))
      ;; and the sent-mark rewrite still finds it afterwards
      (efrit-agent--unmark-queued-message "later please")
      (should (efrit-agent--find-user-message "later please" nil)))))

(ert-deftest test-efrit-agent-question-menu-does-not-open-after-the-answer ()
  "The question menu opens from a timer; an answer before the timer runs
cancels it, so no menu appears over an answered question."
  (with-efrit-agent-test-buffer
    (efrit-agent--init-regions)
    (efrit-agent--setup-regions)
    (let ((opened nil) (timers nil))
      (cl-letf (((symbol-function 'efrit-agent-question-menu-available-p) (lambda () t))
                ((symbol-function 'efrit-agent--open-question-menu)
                 (lambda (&rest _) (setq opened t)))
                ((symbol-function 'run-at-time)
                 (lambda (_s _r fn &rest args)
                   (let ((tm (timer-create)))
                     (timer-set-function tm fn args)
                     (push tm timers) tm)))
                ((symbol-function 'cancel-timer)
                 (lambda (tm) (setq timers (delq tm timers)))))
        (efrit-agent--add-question "Which colour?" '("red" "blue"))
        (should efrit-agent--question-menu-timer)
        ;; answer arrives before the timer fires
        (efrit-agent--on-question-answered '((:response . "blue")))
        (should-not efrit-agent--question-menu-timer)
        (should-not timers)
        (should-not opened)))))

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

;;; Spinner

(ert-deftest test-efrit-agent-thinking-start-runs-spinner ()
  "A thinking-start event starts the spinner timer: the label is set and a
repeating timer exists, so the in-buffer line and the mode-line glyph
animate.  thinking-stop clears both."
  (with-efrit-agent-test-buffer
    (efrit-agent--spinner-stop)
    (efrit-publish 'thinking-start '((:session-id . "s") (:label . "waiting for Claude...")))
    (unwind-protect
        (progn
          (should (equal efrit-agent--thinking-label "waiting for Claude..."))
          (should (timerp efrit-agent--spinner-timer))
          (should (memq efrit-agent--spinner-timer timer-list))
          (efrit-publish 'thinking-stop '((:session-id . "s")))
          (should-not efrit-agent--thinking-label)
          (should-not efrit-agent--spinner-timer))
      (efrit-agent--spinner-stop))))

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
      (cl-letf (((symbol-function 'efrit-agent--repl-send) (lambda (input &optional _api) (setq sent input))))
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

(ert-deftest test-efrit-agent-quote-region ()
  "A region of the transcript lands in the input as a `> ' quote when
idle; when busy it is queued with an optional trailer."
  (with-efrit-agent-test-buffer
    (efrit-agent--init-regions)
    (efrit-agent--setup-regions)
    (efrit-agent--add-user-message "earlier question")
    (let* ((session (efrit-agent-repl-session))
           (start (progn (goto-char (point-min)) (search-forward "earlier") (match-beginning 0)))
           (end (line-end-position)))
      (efrit-agent-quote-region start end)
      (should (equal "> earlier question" (efrit-agent--get-input)))
      (should (= (point) (point-max)))
      (efrit-agent--clear-input)
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "why?"))
                ((symbol-function 'efrit-agent-display) #'ignore))
        (efrit-repl-loop-hold session)
        (efrit-agent-quote-region start end)
        (should (equal '("> earlier question\n\nwhy?") (efrit-repl-session-queue session)))
        (should (efrit-agent--find-user-message "> earlier question\n\nwhy?" 'queued))
        ;; the quote line is drawn with a bar in place of the `> '
        (goto-char (point-min))
        (search-forward "why?")
        (search-backward "> earlier")
        (should (get-text-property (point) 'display))
        (efrit-repl-loop-release session)))))

(ert-deftest test-efrit-agent-narrow-to-turns ()
  "C-c C-z shows the last N turns; the input stays; C-c C-a widens."
  (with-efrit-agent-test-buffer
    (efrit-agent--init-regions)
    (efrit-agent--setup-regions)
    (dolist (q '("first" "second" "third"))
      (efrit-agent--add-user-message q))
    (should (= 3 (length (efrit-agent--turn-starts))))
    (efrit-agent-narrow-to-turns 1)
    (should (buffer-narrowed-p))
    (let ((visible (buffer-substring-no-properties (point-min) (point-max))))
      (should (string-match-p "third" visible))
      (should-not (string-match-p "second" visible))
      (should (>= (point-max) (marker-position efrit-agent--input-start))))
    (efrit-agent-narrow-to-turns 2)
    (should (string-match-p "second" (buffer-substring-no-properties (point-min) (point-max))))
    (efrit-agent-widen)
    (should-not (buffer-narrowed-p))))

(ert-deftest test-efrit-agent-input-list-edit ()
  "S-RET continues a list item, ends it when empty; TAB / S-TAB move an item."
  (with-efrit-agent-test-buffer
    (efrit-agent--init-regions)
    (efrit-agent--setup-regions)
    (test-efrit-agent--type "- one")
    (efrit-agent-input-newline)
    (should (equal "- one\n- " (efrit-agent--get-input-raw)))
    (insert "two")
    (efrit-agent-input-indent-item)
    (should (equal "- one\n  - two" (efrit-agent--get-input-raw)))
    (efrit-agent-input-dedent-item)
    (should (equal "- one\n- two" (efrit-agent--get-input-raw)))
    ;; empty item: the marker goes and the list ends
    (efrit-agent-input-newline)
    (efrit-agent-input-newline)
    (should (equal "- one\n- two\n\n" (efrit-agent--get-input-raw)))
    ;; numbered
    (test-efrit-agent--type "3. c")
    (efrit-agent-input-newline)
    (should (equal "3. c\n4. " (efrit-agent--get-input-raw)))
    ;; plain text: a plain newline, and TAB completes rather than indents
    (test-efrit-agent--type "plain")
    (efrit-agent-input-newline)
    (should (equal "plain\n" (efrit-agent--get-input-raw)))
    (efrit-agent--clear-input)))

(ert-deftest test-efrit-agent-keys-resolve-by-context ()
  "RET, TAB and the digits are menu-item filters: `key-binding' shows
the command that will run, and a key that does not apply falls
through to the next map (the major mode's RET, self-insert for digits)."
  (with-efrit-agent-test-buffer
    (efrit-agent--init-regions)
    (efrit-agent--setup-regions)
    ;; in the input: RET sends, a digit types itself, TAB completes
    (goto-char (point-max))
    (efrit-agent-input-mode 1)
    (should (eq (key-binding (kbd "RET")) #'efrit-agent-input-send))
    (should (eq (key-binding (kbd "1")) #'self-insert-command))
    (should (eq (key-binding (kbd "TAB")) #'completion-at-point))
    ;; on a list item TAB indents
    (insert "- item")
    (should (eq (key-binding (kbd "TAB")) #'efrit-agent-input-indent-item))
    (efrit-agent--clear-input)
    ;; a pending question with two options: 1 and 2 answer, 3 types
    (setq efrit-agent--status 'waiting
          efrit-agent--pending-question (list "Which?" '("red" "blue") "now"))
    (should (commandp (key-binding (kbd "1"))))
    (should-not (eq (key-binding (kbd "1")) #'self-insert-command))
    (should (eq (key-binding (kbd "3")) #'self-insert-command))
    (setq efrit-agent--status 'idle efrit-agent--pending-question nil)
    ;; in the conversation: the major mode's RET
    (efrit-agent-input-mode -1)
    (goto-char (point-min))
    (should (eq (key-binding (kbd "RET")) #'efrit-agent-toggle-expand))))

(ert-deftest test-efrit-agent-regenerate-rewinds-before-the-human-message-not-a-tool-result ()
  "The rewind mark is the last HUMAN message; tool_result user messages
after it are skipped, so no tool_use is left without its result."
  (let ((session (efrit-repl-session-create default-directory)))
    (efrit-repl-session-add-user-message session "q1")
    ;; assistant tool_use, then its tool_result (a user message), then the answer
    (setf (efrit-repl-session-api-messages session)
          (append (efrit-repl-session-api-messages session)
                  (list `((role . "assistant")
                          (content . ,(vector `((type . "tool_use") (id . "t1") (name . "read_file") (input . nil))))))))
    (efrit-repl-session-add-tool-result session "t1" "contents")
    (efrit-repl-session-add-assistant-message session "answer")
    (should (= 4 (length (efrit-repl-session-api-messages session))))
    (should (efrit-repl-session-tool-result-message-p (nth 2 (efrit-repl-session-api-messages session))))
    ;; the mark is before q1 (index 0), not before the tool result (index 2)
    (should (equal '(0) (efrit-agent--history-mark-before-last-user session)))))

(ert-deftest test-efrit-agent-regenerate-replaces-only-on-success ()
  "Regenerate rewinds the history to before the last user message and
resends it; the old exchange is deleted when the new turn ends well
and kept when it fails."
  (with-efrit-agent-test-buffer
    (efrit-agent--init-regions)
    (efrit-agent--setup-regions)
    (let* ((session (efrit-repl-session-create default-directory))
           (sent nil))
      (setq efrit-agent--repl-session session)
      (setf (efrit-repl-session-buffer session) (current-buffer))
      ;; a prior exchange in history and transcript
      (efrit-repl-session-add-user-message session "first q")
      (efrit-repl-session-add-assistant-message session "old answer")
      (efrit-agent--add-user-message "first q")
      (efrit-agent--append-to-conversation "old answer\n" '(efrit-type claude-message efrit-id "m1"))
      ;; the send stub does what the real one does to the history
      (cl-letf (((symbol-function 'efrit-agent--repl-send)
                 (lambda (input &optional _api)
                   (setq sent input)
                   (efrit-repl-session-add-user-message session input)
                   t)))
        (efrit-agent-regenerate))
      (should (equal "first q" sent))
      ;; history rewound to before the old pair, then the question re-added
      (should (= 1 (length (efrit-repl-session-api-messages session))))
      ;; transcript: old exchange still there plus the new user line
      (should (= 2 (length (efrit-agent--turn-starts))))
      (should (string-match-p "old answer" (buffer-string)))
      ;; a failed turn keeps it: both user lines stay
      (efrit-agent--on-turn-complete session "api-error")
      (should (string-match-p "old answer" (buffer-string)))
      (should-not efrit-agent--regenerate)
      (should (= 2 (length (efrit-agent--turn-starts))))
      ;; a fresh exchange, regenerated with success: only it is replaced
      (efrit-repl-session-add-assistant-message session "second answer")
      (efrit-agent--append-to-conversation "second answer\n" '(efrit-type claude-message efrit-id "m2"))
      (cl-letf (((symbol-function 'efrit-agent--repl-send)
                 (lambda (input &optional _api) (efrit-repl-session-add-user-message session input) t)))
        (efrit-agent-regenerate))
      (efrit-agent--on-turn-complete session "end_turn")
      (should-not (string-match-p "second answer" (buffer-string)))
      (should (string-match-p "old answer" (buffer-string)))
      (should (= 2 (length (efrit-agent--turn-starts)))))))

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
