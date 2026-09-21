;;; test-events.el --- Tests for efrit-events and efrit-usage -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'efrit-events)
(require 'efrit-usage)

(defmacro test-events--isolated (&rest body)
  "Run BODY with a private subscriber table and no idle timer."
  (declare (indent 0))
  `(let ((efrit-events--subscribers nil)
         (efrit-events--idle-timer nil)
         (efrit-idle-delay nil))
     ,@body))

(ert-deftest test-events-subscribe-publish-order-and-payload ()
  (test-events--isolated
    (let ((got nil))
      (efrit-subscribe 'tool-start (lambda (ev) (push (cons 'a (alist-get :tool ev)) got)))
      (efrit-subscribe 'tool-start (lambda (ev) (push (cons 'b (alist-get :tool ev)) got)))
      (efrit-subscribe t (lambda (ev) (push (cons 'all (alist-get :type ev)) got)))
      (let ((ev (efrit-publish 'tool-start '((:session-id . "s") (:tool . "read_file")))))
        (should (eq (alist-get :type ev) 'tool-start))
        (should (alist-get :time ev))
        (should (equal (nreverse got)
                       '((a . "read_file") (b . "read_file") (all . tool-start))))))))

(ert-deftest test-events-subscriber-error-is-isolated ()
  (test-events--isolated
    (let ((ran nil))
      (efrit-subscribe 'turn-complete (lambda (_) (error "bad subscriber")))
      (efrit-subscribe 'turn-complete (lambda (_) (setq ran t)))
      (efrit-publish 'turn-complete '((:session-id . "s")))
      (should ran))))

(ert-deftest test-events-unsubscribe-and-dedupe ()
  (test-events--isolated
    (let ((n 0) (fn nil))
      (setq fn (lambda (_) (cl-incf n)))
      (efrit-subscribe 'x fn)
      (efrit-subscribe 'x fn)                ; no duplicate
      (efrit-publish 'x)
      (should (= n 1))
      (efrit-unsubscribe 'x fn)
      (efrit-publish 'x)
      (should (= n 1)))))

(ert-deftest test-events-idle-fires-after-turn-complete ()
  (test-events--isolated
    (let ((efrit-idle-delay 0.05) (idle nil))
      (efrit-subscribe 'idle (lambda (ev) (setq idle ev)))
      (efrit-publish 'turn-complete '((:session-id . "s7")))
      (should (timerp efrit-events--idle-timer))
      (sleep-for 0.2)
      (should idle)
      (should (equal (alist-get :session-id idle) "s7"))
      (should (eq (alist-get :idle-event idle) 'turn-complete)))))

(ert-deftest test-events-idle-cancelled-by-activity ()
  (test-events--isolated
    (let ((efrit-idle-delay 0.1) (idle nil))
      (efrit-subscribe 'idle (lambda (_) (setq idle t)))
      (efrit-publish 'turn-complete '((:session-id . "s8")))
      (efrit-publish 'turn-start '((:session-id . "s8")))   ; user came back
      (sleep-for 0.25)
      (should-not idle))))

;;; usage

(defun test-events--usage (in out cr cw)
  (let ((h (make-hash-table :test 'equal)))
    (puthash "input_tokens" in h) (puthash "output_tokens" out h)
    (puthash "cache_read_input_tokens" cr h)
    (puthash "cache_creation_input_tokens" cw h)
    h))

(ert-deftest test-usage-accumulates-and-tracks-context ()
  (let ((efrit-usage--by-session (make-hash-table :test 'equal)))
    (efrit-usage--on-api-response
     `((:session-id . "u1") (:usage . ,(test-events--usage 1000 50 0 9000))))
    (efrit-usage--on-api-response
     `((:session-id . "u1") (:usage . ,(test-events--usage 200 80 9000 0))))
    (let ((u (efrit-usage-for "u1")))
      (should (= (efrit-usage-requests u) 2))
      (should (= (efrit-usage-input u) 1200))
      (should (= (efrit-usage-output u) 130))
      (should (= (efrit-usage-cache-read u) 9000))
      (should (= (efrit-usage-cache-write u) 9000))
      ;; live context = last request's total read
      (should (= (efrit-usage-context u) 9200)))
    (should-not (efrit-usage-for "other"))))

(ert-deftest test-usage-indicator ()
  (let ((efrit-usage--by-session (make-hash-table :test 'equal))
        (efrit-usage-context-window 100000))
    (should-not (efrit-usage-indicator "none"))
    (efrit-usage--on-api-response
     `((:session-id . "u2") (:usage . ,(test-events--usage 29000 0 0 0))))
    (let ((s (efrit-usage-indicator "u2")))
      (should (string-prefix-p "29k/100k" s))
      (should (eq (get-text-property 0 'face s) 'success))
      (should (string-match-p "29%" (get-text-property 0 'help-echo s))))
    (efrit-usage--on-api-response
     `((:session-id . "u2") (:usage . ,(test-events--usage 90000 0 0 0))))
    (should (eq (get-text-property 0 'face (efrit-usage-indicator "u2")) 'error))))

(ert-deftest test-usage-compact-number ()
  (should (string= (efrit-usage-compact-number 999) "999"))
  (should (string= (efrit-usage-compact-number 1500) "1.5k"))
  (should (string= (efrit-usage-compact-number 29000) "29k"))
  (should (string= (efrit-usage-compact-number 200000) "200k"))
  (should (string= (efrit-usage-compact-number 1200000) "1.2m")))

(ert-deftest test-usage-wired-to-bus ()
  "The subscriber is registered at load time on the real bus."
  (should (memq #'efrit-usage--on-api-response
                (cdr (assq 'api-response efrit-events--subscribers)))))

;;; The agent buffer is driven by the bus alone

(ert-deftest test-events-agent-buffer-renders-from-published-events ()
  "Every visible thing in the agent buffer arrives as an event: tool rows
paired by tool-id, text deltas, thinking, questions, status, TODOs.
No advice is installed on any efrit function."
  (require 'efrit-agent)
  (require 'efrit-agent-integration)
  (let ((efrit-agent-buffer-name "*efrit-agent-test-events*"))
    (unwind-protect
        (progn
          (efrit)
          (with-current-buffer efrit-agent-buffer-name
            (efrit-agent--add-user-message "do it")
            ;; REPL-path shapes: ids pair start and result even when two
            ;; calls of the same tool interleave
            (efrit-publish 'status '((:session-id . "s") (:status . working)))
            (should (eq efrit-agent--status 'working))
            (efrit-publish 'thinking-start '((:session-id . "s") (:label . "waiting for Claude...")))
            (should efrit-agent--thinking-indicator)
            (efrit-publish 'text-delta '((:session-id . "s") (:text . "Hello ")))
            (efrit-publish 'thinking-stop '((:session-id . "s")))
            (should-not efrit-agent--thinking-indicator)
            (efrit-publish 'text-delta '((:session-id . "s") (:text . "there.")))
            (efrit-publish 'text-end '((:session-id . "s")))
            (efrit-publish 'tool-start '((:session-id . "s") (:tool-id . "a") (:tool . "read_file")))
            (efrit-publish 'tool-start '((:session-id . "s") (:tool-id . "b") (:tool . "read_file")))
            (efrit-publish 'tool-result '((:session-id . "s") (:tool-id . "a") (:tool . "read_file")
                                          (:result . "first") (:success . t) (:elapsed . 0.5)))
            (efrit-publish 'tool-result '((:session-id . "s") (:tool-id . "b") (:tool . "read_file")
                                          (:result . "Error second") (:success . nil) (:elapsed . 0.1)))
            (let ((text (buffer-substring-no-properties (point-min) (point-max))))
              (should (string-match-p "Hello there\\." text))
              (should (string-match-p "✓ read_file.*0\\.5s" text))
              (should (string-match-p "✗ read_file.*0\\.1s" text)))
            ;; efrit-do path shapes: no id, paired by name
            (efrit-publish 'tool-start '((:session-id . "d") (:tool . "shell_exec")))
            (efrit-publish 'tool-result '((:session-id . "d") (:tool . "shell_exec") (:result . "ok") (:success . t)))
            (should (string-match-p "✓ shell_exec" (buffer-string)))
            (efrit-publish 'message '((:session-id . "d") (:text . "boom") (:kind . error)))
            (should (string-match-p "Error: boom" (buffer-string)))
            ;; a question flips status and shows options; the answer flips back
            (efrit-publish 'question '((:session-id . "s") (:question . "Which?") (:options . ("x" "y"))))
            (should (eq efrit-agent--status 'waiting))
            (should (string-match-p "Which\\?" (buffer-string)))
            (efrit-publish 'question-answered '((:session-id . "s") (:response . "x")))
            (should (eq efrit-agent--status 'working))
            (should-not efrit-agent--pending-question)
            (efrit-publish 'status '((:session-id . "s") (:status . idle)))
            (should (eq efrit-agent--status 'idle)))
          ;; no advice anywhere on the functions the old integration wrapped
          (dolist (f '(efrit-progress-show-tool-start efrit-progress-show-tool-result
                       efrit-progress-show-message efrit-progress-start-session
                       efrit-progress-end-session efrit-session-set-pending-question
                       efrit-session-respond-to-question efrit-do--handle-todo-write
                       efrit-todo-update-status))
            (when (fboundp f)
              (should-not (advice--p (advice--symbol-function f))))))
      (when (get-buffer efrit-agent-buffer-name) (kill-buffer efrit-agent-buffer-name)))))

(ert-deftest test-events-sources-publish ()
  "The progress layer, the session and the TODO store publish at the source."
  (require 'efrit-progress)
  (require 'efrit-session)
  (require 'efrit-todo)
  (let ((seen nil))
    (cl-flet ((rec (ev) (push (alist-get :type ev) seen)))
      (efrit-subscribe t #'rec)
      (unwind-protect
          (let ((efrit-progress-buffer-enabled nil))
            (efrit-progress-show-tool-start "read_file" nil)
            (efrit-progress-show-tool-result "read_file" "ok" t)
            (efrit-progress-show-message "hi" 'claude)
            (let ((s (efrit-session-create "sess-1" "cmd")))
              (efrit-session-set-pending-question s "Q?" '("a"))
              (efrit-session-respond-to-question s "a"))
            (let ((efrit-todo--current-todos nil))
              (efrit-todo-add "x")
              (efrit-todo-update-status (efrit-todo-item-id (car efrit-todo--current-todos)) 'completed))
            (dolist (ev '(tool-start tool-result message question question-answered todos-changed))
              (should (memq ev seen))))
        (efrit-unsubscribe t #'rec)))))

(provide 'test-events)
;;; test-events.el ends here
