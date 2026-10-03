;;; test-unattended.el --- prompts answered by policy while away -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'efrit-events)
(require 'efrit-unattended)
(require 'efrit-sandbox)
(require 'efrit-limits)
(require 'efrit-review)

(ert-deftest test-unattended-policy-answers ()
  "Sandbox: deny.  Limits: raise once, twice, then stop.  Preview: the default."
  (clrhash efrit-unattended--raises)
  (let ((efrit-unattended-max-raises 2))
    (should (equal (car (efrit-unattended-answer "read_file's sandbox request" nil)) nil))
    (should (string-match-p "denied" (cdr (efrit-unattended-answer "read_file's sandbox request" nil))))
    (should (eq 'once (car (efrit-unattended-answer "the max-iterations limit prompt" nil))))
    (should (eq 'once (car (efrit-unattended-answer "the max-iterations limit prompt" nil))))
    (should (null (car (efrit-unattended-answer "the max-iterations limit prompt" nil))))
    (should (string-match-p "turn stops" (cdr (efrit-unattended-answer "the max-iterations limit prompt" nil))))
    ;; another limit has its own count
    (should (eq 'once (car (efrit-unattended-answer "the session-timeout limit prompt" nil))))
    (should (equal 'reject (car (efrit-unattended-answer "the diff preview" 'reject))))))

(ert-deftest test-unattended-prompt-turn-is-answered-without-opening ()
  "With the mode on, `efrit-with-prompt-turn' never runs its body: the
policy's answer is returned, an event records it, a note is written,
and the turn's end gets a summary.  With the mode off a `prompt-open'
event fires so notifications can say efrit is waiting."
  (let* ((opened nil) (events nil) (notes nil)
         (listener (lambda (e) (push (alist-get :type e) events)))
         (note-listener (lambda (e) (push (alist-get :text e) notes)))
         (efrit-current-session-id "s-unatt")
         (efrit-prompt--owner nil))
    (efrit-subscribe 'prompt-open listener)
    (efrit-subscribe 'prompt-answered-unattended listener)
    (efrit-subscribe 'note note-listener)
    (unwind-protect
        (progn
          (efrit-unattended-mode 1)
          (efrit-publish 'turn-start '((:session-id . "s-unatt")))
          (should (null (efrit-with-prompt-turn "edit_file's sandbox request" nil
                          (setq opened t) 'session)))
          (should-not opened)
          (should (memq 'prompt-answered-unattended events))
          (should (cl-some (lambda (n) (string-match-p "denied" n)) notes))
          ;; the summary at the end of the turn
          (efrit-publish 'turn-complete '((:session-id . "s-unatt") (:stop-reason . "end_turn")))
          (should (cl-some (lambda (n) (string-match-p "1 prompt(s) answered by policy" n)) notes))
          ;; off: the body runs and prompt-open fires
          (efrit-unattended-mode -1)
          (setq events nil)
          (should (eq 'session (efrit-with-prompt-turn "edit_file's sandbox request" nil
                                 (setq opened t) 'session)))
          (should opened)
          (should (equal '(prompt-open) events)))
      (efrit-unattended-mode -1)
      (efrit-unsubscribe 'prompt-open listener)
      (efrit-unsubscribe 'prompt-answered-unattended listener)
      (efrit-unsubscribe 'note note-listener))))

(ert-deftest test-review-flags-a-shell-line-that-starts-emacs ()
  "`emacs --batch …' is not refused by the sandbox; the reviewer sees it
flagged and must find the agent's reason.  `emacsclient' is not flagged."
  (should (efrit-sandbox-shell-starts-emacs-p "cd x && /opt/homebrew/bin/emacs --batch -Q -l t.el"))
  (should (efrit-sandbox-shell-starts-emacs-p "emacs-29.1 -Q --batch"))
  (should-not (efrit-sandbox-shell-starts-emacs-p "emacsclient -e '(+ 1 2)'"))
  (should-not (efrit-sandbox-shell-starts-emacs-p "ls -la"))
  (let* ((input (make-hash-table :test 'equal))
         (use (let ((u (make-hash-table :test 'equal)))
                (puthash "command" "emacs --batch -Q -f batch-byte-compile a.el" input)
                (puthash "type" "tool_use" u) (puthash "id" "t1" u)
                (puthash "name" "shell_exec" u) (puthash "input" input u) u))
         (batch (efrit-review-describe-batch (vector use))))
    (should (string-match-p "\\[FLAG shell: starts another Emacs" batch))
    (should (string-match-p "must have said why" batch))
    (should (string-match-p "FLAG" efrit-review--system-prompt)))
  ;; the sandbox itself lets it through on an ordinary shell grant
  (let ((efrit-sandbox-enabled t) (efrit-project-root temporary-file-directory)
        (efrit-sandbox--session-grants (make-hash-table :test 'equal))
        (efrit-sandbox-request-function (lambda (_r) 'session)))
    (should (efrit-sandbox-check 'shell "emacs --batch -Q -l x.el" "shell_exec"))))

(provide 'test-unattended)
;;; test-unattended.el ends here
