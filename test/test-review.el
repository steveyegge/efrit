;;; test-review.el --- second-model review of proposed tool calls -*- lexical-binding: t; -*-

;;; Commentary:
;; Unit tests for efrit-review (what the reviewer sees, verdict
;; parsing, failure policy) and loop integration over the REPL mock
;; harness: an approved turn runs its tools, a rejected turn feeds the
;; rejection back and continues, repeated rejections hand the turn to
;; the user.  The reviewer's API call is stubbed at
;; `efrit-api-request-async'; the proposer's call at
;; `efrit-repl-loop--api-call', as in test-repl-loop.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'efrit-review)
(require 'efrit-repl-session)
(require 'efrit-repl-loop)
(require 'efrit-do)

(defvar efrit-project-root)

;;; Helpers (content blocks as the API delivers them: hash tables)

(defun test-review--text (text)
  (let ((ht (make-hash-table :test 'equal)))
    (puthash "type" "text" ht) (puthash "text" text ht) ht))

(defun test-review--tool-use (id name input-alist)
  (let ((ht (make-hash-table :test 'equal))
        (input (make-hash-table :test 'equal)))
    (dolist (pair input-alist) (puthash (car pair) (cdr pair) input))
    (puthash "type" "tool_use" ht) (puthash "id" id ht)
    (puthash "name" name ht) (puthash "input" input ht)
    ht))

(defun test-review--response (content stop-reason)
  (let ((r (make-hash-table :test 'equal)))
    (puthash "content" content r) (puthash "stop_reason" stop-reason r) r))

(defun test-review--reviewer-response (text)
  "A reviewer API response whose single text block is TEXT."
  (test-review--response (vector (test-review--text text)) "end_turn"))

(defmacro test-review--with-reviewer (verdict-texts &rest body)
  "Run BODY with the reviewer's API call stubbed.
VERDICT-TEXTS is a form giving a list of reviewer reply strings,
consumed in order; each stubbed call also records the request it
received in `test-review--requests'.  A string starting with
\"ERROR:\" makes the stub call the error callback instead."
  (declare (indent 1) (debug t))
  `(let ((test-review--queue ,verdict-texts)
         (test-review--requests nil))
     (cl-letf (((symbol-function 'efrit-api-request-async)
                (lambda (request callback &optional error-callback)
                  (push request test-review--requests)
                  (let ((text (pop test-review--queue)))
                    (cond
                     ((null text) (funcall error-callback "reviewer queue empty"))
                     ((string-prefix-p "ERROR:" text)
                      (funcall error-callback (substring text 6)))
                     (t (funcall callback (test-review--reviewer-response text))))))))
       (clrhash efrit-review--rejections)
       (unwind-protect (progn ,@body)
         (clrhash efrit-review--rejections)))))

(defvar test-review--queue nil)
(defvar test-review--requests nil)

;;; Applicability

(ert-deftest test-review-applies-only-to-mutating-calls ()
  (let ((efrit-review-enabled t)
        (efrit-review-classes '(write exec)))
    (should (efrit-review-applies-p
             (vector (test-review--tool-use "1" "edit_file" '(("path" . "a"))))))
    (should (efrit-review-applies-p
             (vector (test-review--tool-use "1" "shell_exec" '(("command" . "ls"))))))
    (should-not (efrit-review-applies-p
                 (vector (test-review--text "hi")
                         (test-review--tool-use "1" "read_file" '(("path" . "a"))))))
    (should-not (efrit-review-applies-p
                 (vector (test-review--tool-use "1" "session_complete" nil)))))
  ;; with the default classes, network calls are reviewed too: data
  ;; leaves the machine
  (let ((efrit-review-enabled t))
    (should (memq 'net efrit-review-classes))
    (should (efrit-review-applies-p
             (vector (test-review--tool-use "1" "fetch_url" '(("url" . "https://x")))))))
  ;; the skip reason names the read-only tools; nil when reviewed or no tools
  (let ((efrit-review-enabled t))
    (should (equal (efrit-review-skip-reason
                    (vector (test-review--tool-use "1" "project_files" nil)
                            (test-review--tool-use "2" "read_file" '(("path" . "a")))))
                   "read-only turn (project_files, read_file)"))
    (should-not (efrit-review-skip-reason (vector (test-review--tool-use "1" "edit_file" nil))))
    (should-not (efrit-review-skip-reason (vector (test-review--text "just words"))))
    ;; control-only turns (the session_complete after a reviewed turn) say nothing
    (should-not (efrit-review-skip-reason (vector (test-review--tool-use "1" "session_complete" nil))))
    (should-not (efrit-review-skip-reason (vector (test-review--tool-use "1" "todo_write" nil)
                                                  (test-review--tool-use "2" "session_complete" nil))))
    ;; a read alongside a control tool is still a read-only turn, named without the control tool
    (should (equal (efrit-review-skip-reason (vector (test-review--tool-use "1" "read_file" nil)
                                                     (test-review--tool-use "2" "todo_write" nil)))
                   "read-only turn (read_file)")))
  (let ((efrit-review-enabled nil))
    (should (equal (efrit-review-skip-reason (vector (test-review--tool-use "1" "edit_file" nil)))
                   "review off")))
  (let ((efrit-review-enabled nil))
    (should-not (efrit-review-applies-p
                 (vector (test-review--tool-use "1" "edit_file" '(("path" . "a"))))))))

;;; What the reviewer sees

(ert-deftest test-review-user-intent-skips-tool-results-and-context ()
  (let ((messages
         (list '((role . "user") (content . "<editor-context>\nBuffer: x\n</editor-context>\n\nrename foo to bar"))
               `((role . "assistant") (content . ,(vector (test-review--text "ok"))))
               `((role . "user") (content . ,(vector (let ((h (make-hash-table :test 'equal)))
                                                        (puthash "type" "tool_result" h)
                                                        (puthash "content" "done" h) h)))))))
    (should (equal (efrit-review-user-intent messages) "rename foo to bar"))
    (should (equal (efrit-review-user-intent (vconcat messages)) "rename foo to bar"))
    (should-not (efrit-review-user-intent nil))))

(ert-deftest test-review-batch-excludes-reads-and-outputs ()
  (let* ((efrit-review-classes '(write exec))
         (content (vector (test-review--text "I'll edit it.")
                          (test-review--tool-use "1" "read_file" '(("path" . "secret.txt")))
                          (test-review--tool-use "2" "edit_file" '(("path" . "a.el") ("new_string" . "x")))
                          (test-review--tool-use "3" "shell_exec" '(("command" . "make")))))
         (batch (efrit-review-describe-batch content)))
    (should (string-match-p "\\`1\\. edit_file " batch))
    (should (string-match-p "\n2\\. shell_exec " batch))
    (should-not (string-match-p "read_file" batch))
    (should-not (string-match-p "secret" batch))))

(ert-deftest test-review-long-input-is-cut ()
  (let* ((efrit-review-max-input-chars 50)
         (content (vector (test-review--tool-use
                           "1" "create_file"
                           `(("path" . "big") ("content" . ,(make-string 500 ?x))))))
         (batch (efrit-review-describe-batch content)))
    (should (< (length batch) 120))
    (should (string-match-p "more chars" batch))))

(ert-deftest test-review-request-shape ()
  "The review request carries no tools, the configured model, and no history."
  (let ((efrit-review-enabled t)
        (efrit-review-model "reviewer-model")
        (efrit-default-model "proposer-model")
        (content (vector (test-review--text "Editing.")
                         (test-review--tool-use "1" "edit_file" '(("path" . "a"))))))
    (test-review--with-reviewer '("{\"verdict\": \"approve\"}")
      (let ((got nil))
        (efrit-review-turn "s1" (list '((role . "user") (content . "fix a")))
                           content (lambda (v) (setq got v)))
        (should (equal got '(approve . nil)))
        (let ((req (car test-review--requests)))
          (should (equal (alist-get "model" req nil nil #'equal) "reviewer-model"))
          (should-not (assoc "tools" req))
          (let* ((msgs (alist-get "messages" req nil nil #'equal))
                 (text (alist-get "content" (aref msgs 0) nil nil #'equal)))
            (should (= (length msgs) 1))
            (should (string-match-p "USER REQUEST:\nfix a" text))
            (should (string-match-p "AGENT SAID THIS TURN:\nEditing\\." text))
            (should (string-match-p "1\\. edit_file" text))))))))

;;; Verdicts

(ert-deftest test-review-parse-verdict ()
  (should (equal (efrit-review-parse-verdict "{\"verdict\": \"approve\"}") '(approve . nil)))
  (should (equal (efrit-review-parse-verdict
                  "Sure.\n{\"verdict\": \"reject\", \"reason\": \"touches ~/.ssh\"}\n")
                 '(reject . "touches ~/.ssh")))
  (should (equal (car (efrit-review-parse-verdict "{\"verdict\": \"REJECT\"}")) 'reject))
  (should-not (efrit-review-parse-verdict "{\"verdict\": \"maybe\"}"))
  (should-not (efrit-review-parse-verdict "no json here"))
  (should-not (efrit-review-parse-verdict nil))
  ;; A second object after the verdict, or one embedded in an example,
  ;; must not swallow the first (the greedy match did: every review
  ;; came back "malformed", 2026-09-25)
  (should (equal (efrit-review-parse-verdict
                  "{\"verdict\": \"approve\"}\n\nFor the record: {\"note\": \"none\"}")
                 '(approve . nil)))
  (should (equal (efrit-review-parse-verdict
                  "{\"verdict\": \"reject\", \"reason\": \"the call {deletes} x\"}")
                 '(reject . "the call {deletes} x")))
  ;; A leading non-verdict object is skipped for a later verdict
  (should (equal (efrit-review-parse-verdict
                  "{\"thinking\": 1} {\"verdict\": \"approve\"}")
                 '(approve . nil))))

(ert-deftest test-review-failure-policy ()
  (let ((content (vector (test-review--tool-use "1" "edit_file" '(("path" . "a"))))))
    (let ((efrit-review-on-failure 'approve) (got nil))
      (test-review--with-reviewer '("ERROR:boom")
        (efrit-review-turn "s" nil content (lambda (v) (setq got v)))
        (should (eq (car got) 'approve))))
    (let ((efrit-review-on-failure 'reject) (got nil))
      (test-review--with-reviewer '("ERROR:boom")
        (efrit-review-turn "s" nil content (lambda (v) (setq got v)))
        (should (eq (car got) 'reject))
        (should (string-match-p "boom" (cdr got)))))
    ;; malformed reply follows the same policy
    (let ((efrit-review-on-failure 'reject) (got nil))
      (test-review--with-reviewer '("I think it's fine")
        (efrit-review-turn "s" nil content (lambda (v) (setq got v)))
        (should (eq (car got) 'reject))))))

(ert-deftest test-review-failure-policy-per-class ()
  "The default fails closed for exec and open for write; a batch with
both fails closed."
  (let ((efrit-review-on-failure '((exec . reject) (t . approve))))
    (should (eq 'approve (efrit-review-failure-policy '(write))))
    (should (eq 'reject (efrit-review-failure-policy '(exec))))
    (should (eq 'reject (efrit-review-failure-policy '(write exec))))
    (should (eq 'approve (efrit-review-failure-policy nil)))
    (let ((edit (vector (test-review--tool-use "1" "edit_file" '(("path" . "a")))))
          (shell (vector (test-review--tool-use "2" "shell_exec" '(("command" . "ls")))))
          (got nil))
      (test-review--with-reviewer '("ERROR:boom")
        (efrit-review-turn "s" nil edit (lambda (v) (setq got v)))
        (should (eq (car got) 'approve)))
      (test-review--with-reviewer '("ERROR:boom")
        (efrit-review-turn "s" nil shell (lambda (v) (setq got v)))
        (should (eq (car got) 'reject))
        (should (string-match-p "nothing was run" (cdr got))))))
  ;; a class with no entry and no t entry approves
  (let ((efrit-review-on-failure '((exec . reject))))
    (should (eq 'approve (efrit-review-failure-policy '(net))))))

(ert-deftest test-review-project-override-file ()
  "A project's settings.json can turn review off or change its classes;
the customization values apply when the file says nothing."
  (let* ((root (file-name-as-directory (make-temp-file "efrit-rev-" t)))
         (efrit-project-root root)
         (efrit-data-directory (expand-file-name "data" root))
         (efrit-settings--cache (make-hash-table :test 'equal))
         (efrit-review-enabled t)
         (efrit-review-classes '(write exec net)))
    (unwind-protect
        (progn
          (should (efrit-review-enabled-p))
          (should (equal (efrit-review-effective-classes) '(write exec net)))
          (efrit-review-set-project-override nil nil)
          (should-not (efrit-review-enabled-p))
          (should (equal (plist-get (efrit-review-project-override) :enabled) nil))
          (efrit-review-set-project-override 'unset '(write))
          (should (efrit-review-enabled-p))
          (should (equal (efrit-review-effective-classes) '(write)))
          ;; a shell call is not reviewable under the narrowed classes
          (should-not (efrit-review-applies-p
                       (vector (test-review--tool-use "1" "shell_exec" '(("command" . "ls"))))))
          ;; unset both removes the section
          (efrit-review-set-project-override 'unset nil)
          (should-not (efrit-settings-get root "review"))
          ;; a bad classes list in the file falls back to the option
          (efrit-settings-put root "review" (let ((h (make-hash-table :test 'equal)))
                                              (puthash "classes" '("write" "bogus") h) h))
          (should (equal (efrit-review-effective-classes) '(write exec net))))
      (delete-directory root t))))

(ert-deftest test-review-rejection-counter ()
  "Rejections are counted per proposal within a turn: the same batch
coming back climbs, a different batch starts at one, an approval or a
new turn clears everything (2026-10-01: an ancillary work-log edit
rejected in two turns ended the second turn at its first tool)."
  (clrhash efrit-review--rejections)
  (let ((log '("edit_file ~/work/log.org")) (code '("edit_file src/a.el")))
    (should (= 1 (efrit-review-note-verdict "s" 'reject log)))
    (should (= 2 (efrit-review-note-verdict "s" 'reject log)))
    ;; a different proposal: not a loop
    (should (= 1 (efrit-review-note-verdict "s" 'reject code)))
    (should (= 0 (efrit-review-note-verdict "s" 'approve code)))
    (should (= 1 (efrit-review-note-verdict "s" 'reject log)))
    ;; a new turn starts clean
    (efrit-review-forget-session "s")
    (should (= 1 (efrit-review-note-verdict "s" 'reject log)))
    ;; old callers without a signature still count repeats
    (should (= 1 (efrit-review-note-verdict "t" 'reject)))
    (should (= 2 (efrit-review-note-verdict "t" 'reject))))
  (clrhash efrit-review--rejections))

(ert-deftest test-review-batch-signature-names-tools-and-files ()
  (let* ((h1 (make-hash-table :test 'equal)) (h2 (make-hash-table :test 'equal)))
    (puthash "path" "~/work/log.org" h1)
    (puthash "command" "ls" h2)
    (let ((content (vector (let ((u (make-hash-table :test 'equal)))
                             (puthash "type" "tool_use" u) (puthash "id" "a" u)
                             (puthash "name" "edit_file" u) (puthash "input" h1 u) u)
                           (let ((u (make-hash-table :test 'equal)))
                             (puthash "type" "tool_use" u) (puthash "id" "b" u)
                             (puthash "name" "shell_exec" u) (puthash "input" h2 u) u))))
      (should (equal '("edit_file ~/work/log.org" "shell_exec ls")
                     (efrit-review-batch-signature content))))))

;;; Loop integration (REPL adapter, everything mocked)

(defmacro test-review--with-loop (responses tool-result &rest body)
  "Like test-repl-loop's harness: proposer responses and tool dispatch stubbed.
Records each dispatched tool name in `test-review--dispatched'."
  (declare (indent 2) (debug t))
  `(let ((test-responses ,responses)
         (test-review--dispatched nil))
     (cl-letf (((symbol-function 'efrit-repl-loop--api-call)
                (lambda (_session _messages callback)
                  (let ((response (pop test-responses)))
                    (if response
                        (funcall callback response nil)
                      (funcall callback nil "mock response queue empty")))))
               ((symbol-function 'efrit-do--execute-tool-string)
                (lambda (tool-item)
                  (push (gethash "name" tool-item) test-review--dispatched)
                  ,tool-result))
               ((symbol-function 'efrit-agent-set-status) #'ignore))
       (unwind-protect (progn ,@body)
         (clrhash efrit-repl-loop--active)))))

(defvar test-review--dispatched nil)

(ert-deftest test-review-loop-approved-turn-runs-tools ()
  (let ((efrit-review-enabled t)
        (session (efrit-repl-session-create))
        (turn-reason nil))
    (test-review--with-reviewer '("{\"verdict\": \"approve\"}")
      (test-review--with-loop
          (list (test-review--response
                 (vector (test-review--text "Editing.")
                         (test-review--tool-use "t1" "edit_file" '(("path" . "a.el"))))
                 "tool_use")
                (test-review--response
                 (vector (test-review--text "Done."))
                 "end_turn"))
          "edited"
        (efrit-repl-continue session "fix a.el"
                             (lambda (_s reason) (setq turn-reason reason)))
        (should (equal turn-reason "end_turn"))
        (should (equal test-review--dispatched '("edit_file")))
        (should (= (length test-review--requests) 1))))))

(ert-deftest test-review-loop-read-only-turn-is-not-reviewed ()
  (let ((efrit-review-enabled t)
        (session (efrit-repl-session-create))
        (turn-reason nil))
    (test-review--with-reviewer nil
      (test-review--with-loop
          (list (test-review--response
                 (vector (test-review--tool-use "t1" "read_file" '(("path" . "a.el"))))
                 "tool_use")
                (test-review--response (vector (test-review--text "Read it.")) "end_turn"))
          "contents"
        (efrit-repl-continue session "show a.el"
                             (lambda (_s reason) (setq turn-reason reason)))
        (should (equal turn-reason "end_turn"))
        (should (equal test-review--dispatched '("read_file")))
        (should (null test-review--requests))))))

(ert-deftest test-review-loop-rejected-turn-feeds-back-and-continues ()
  "A rejection: no tool runs, every tool_use gets an is_error result
carrying the reason, and the proposer's next response is processed."
  (let ((efrit-review-enabled t)
        (efrit-review-max-rejections 2)
        (session (efrit-repl-session-create))
        (turn-reason nil))
    (test-review--with-reviewer '("{\"verdict\": \"reject\", \"reason\": \"user asked for a.el, not b.el\"}"
                                  "{\"verdict\": \"approve\"}")
      (test-review--with-loop
          (list (test-review--response
                 (vector (test-review--tool-use "t1" "read_file" '(("path" . "a.el")))
                         (test-review--tool-use "t2" "edit_file" '(("path" . "b.el"))))
                 "tool_use")
                (test-review--response
                 (vector (test-review--tool-use "t3" "edit_file" '(("path" . "a.el"))))
                 "tool_use")
                (test-review--response (vector (test-review--text "Done.")) "end_turn"))
          "ok"
        (efrit-repl-continue session "fix a.el"
                             (lambda (_s reason) (setq turn-reason reason)))
        (should (equal turn-reason "end_turn"))
        ;; only the revised turn's tool ran
        (should (equal test-review--dispatched '("edit_file")))
        (let* ((messages (efrit-repl-session-api-messages session))
               ;; user, assistant(rejected), user(rejections), assistant, user(result), assistant
               (rejections (alist-get 'content (nth 2 messages))))
          (should (= (length messages) 6))
          (should (= (length rejections) 2))
          (let ((r1 (aref rejections 0)) (r2 (aref rejections 1)))
            (should (eq (alist-get 'is_error r1) t))
            (should (eq (alist-get 'is_error r2) t))
            ;; the read (not reviewable) is marked as not run; the edit carries the reason
            (should (string-match-p "not run" (alist-get 'content r1)))
            (should (string-match-p "user asked for a.el" (alist-get 'content r2)))
            (should (string-prefix-p efrit-review-rejected-prefix (alist-get 'content r2)))))))))

(ert-deftest test-review-loop-repeated-rejection-asks-the-user ()
  "The same proposal rejected `efrit-review-max-rejections' times pauses
the turn with a question to the user (like request_user_input); the
work in progress is kept, nothing is run, and the question carries the
reviewer's last reason (tzz, 2026-10-01: advise the intern, do not
fire them)."
  (let* ((efrit-review-enabled t)
         (efrit-review-max-rejections 2)
         (session (efrit-repl-session-create))
         (turn-reason nil)
         (asked nil)
         (listener (lambda (e) (push e asked))))
    (efrit-subscribe 'question listener)
    (unwind-protect
        (test-review--with-reviewer '("{\"verdict\": \"reject\", \"reason\": \"no\"}"
                                      "{\"verdict\": \"reject\", \"reason\": \"still no\"}")
          (test-review--with-loop
              (list (test-review--response
                     (vector (test-review--tool-use "t1" "edit_file" '(("path" . "b.el"))))
                     "tool_use")
                    (test-review--response
                     (vector (test-review--tool-use "t2" "edit_file" '(("path" . "b.el"))))
                     "tool_use")
                    (test-review--response (vector (test-review--text "unreached")) "end_turn"))
              "ok"
            (efrit-repl-continue session "fix a.el"
                                 (lambda (_s reason) (setq turn-reason reason)))
            (should (equal turn-reason "waiting-for-user"))
            (should (null test-review--dispatched))
            (should (eq (efrit-repl-session-status session) 'waiting))
            (let ((q (car asked)))
              (should (eq (alist-get :kind q) 'review))
              (should (string-match-p "still no" (alist-get :question q)))
              (should (member "Allow it this once" (alist-get :options q))))
            ;; the session remembers it, with the batch signature
            (let ((meta (nth 3 (efrit-repl-session-pending-question session))))
              (should (eq (plist-get meta :kind) 'review))
              (should (equal '("edit_file b.el") (plist-get meta :signature)))
              ;; "allow" lets that batch through unreviewed, once
              (should (string-match-p "allows the action"
                                      (efrit-review-answer-escalation (efrit-repl-session-id session) meta "Allow it this once")))
              (let ((content (vector (test-review--tool-use "t3" "edit_file" '(("path" . "b.el"))))))
                (should-not (efrit-review-applies-p content (efrit-repl-session-id session)))
                (should (efrit-review-applies-p content (efrit-repl-session-id session))))
              (should (string-match-p "drop that action" (efrit-review-answer-escalation "s" meta "Drop it and continue the task")))
              (should (equal "do it differently" (efrit-review-answer-escalation "s" meta "do it differently"))))))
      (efrit-unsubscribe 'question listener))))

(ert-deftest test-review-loop-different-rejections-do-not-escalate ()
  "Two rejections of different proposals in one turn are two pieces of
advice, not a loop: the turn goes on and the third, acceptable batch runs."
  (let ((efrit-review-enabled t)
        (efrit-review-max-rejections 2)
        (session (efrit-repl-session-create))
        (turn-reason nil))
    (test-review--with-reviewer '("{\"verdict\": \"reject\", \"reason\": \"not the log\"}"
                                  "{\"verdict\": \"reject\", \"reason\": \"not that either\"}"
                                  "{\"verdict\": \"approve\"}")
      (test-review--with-loop
          (list (test-review--response
                 (vector (test-review--tool-use "t1" "edit_file" '(("path" . "log.org"))))
                 "tool_use")
                (test-review--response
                 (vector (test-review--tool-use "t2" "edit_file" '(("path" . "notes.org"))))
                 "tool_use")
                (test-review--response
                 (vector (test-review--tool-use "t3" "edit_file" '(("path" . "a.el"))))
                 "tool_use")
                (test-review--response (vector (test-review--text "done")) "end_turn"))
          "ok"
        (efrit-repl-continue session "fix a.el"
                             (lambda (_s reason) (setq turn-reason reason)))
        (should (equal turn-reason "end_turn"))
        (should (equal '("edit_file") test-review--dispatched))))))

(ert-deftest test-review-eval-cannot-touch-reviewer ()
  "The proposer cannot switch its own reviewer off from eval_sexp."
  (require 'efrit-sandbox-eval)
  (should (efrit-sandbox-eval-inspect '(setq efrit-review-enabled nil)))
  (should (efrit-sandbox-eval-inspect '(efrit-review-forget-session "x"))))

(provide 'test-review)
;;; test-review.el ends here

(ert-deftest test-review-message-carries-the-standing-instructions ()
  "The reviewer sees the user's AGENTS.md/CLAUDE.md block before the
request, so an action those files call for on every turn is not
rejected as unasked (2026-10-01: the work-log edit)."
  (let ((dir (file-name-as-directory (make-temp-file "efrit-review-instr-" t))))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "CLAUDE.md" dir)
            (insert "# Rules\n\nOn every request, first append a line to ~/work/log.org.\n"))
          (let* ((efrit-project-root dir)
                 (msg (efrit-review--user-message "review this article" nil "1. edit_file {...log.org...}")))
            (should (string-match-p "USER'S STANDING INSTRUCTIONS:" msg))
            (should (string-match-p "append a line to ~/work/log.org" msg))
            (should (< (string-match "STANDING INSTRUCTIONS" msg) (string-match "USER REQUEST:" msg))))
          ;; cut when long, and absent when there is no file
          (let ((efrit-review-instructions-max-chars 40) (efrit-project-root dir))
            (should (string-match-p "more chars\\]" (efrit-review--user-message "x" nil "y"))))
          (let ((efrit-project-root (file-name-as-directory (make-temp-file "efrit-review-none-" t))))
            (should-not (string-match-p "STANDING INSTRUCTIONS" (efrit-review--user-message "x" nil "y")))))
      (delete-directory dir t))))
