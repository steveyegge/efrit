;;; test-repl-loop.el --- Tests for efrit-repl-loop -*- lexical-binding: t; -*-

;;; Commentary:
;; Mocked tests for the REPL agentic loop over the shared engine
;; (efrit-loop, ef-0t4).  The API call and tool dispatch are stubbed,
;; so these run without network access and verify the wiring: turn
;; lifecycle, message accumulation, waiting-for-user pause, and the
;; API error path.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'efrit-repl-session)
(require 'efrit-repl-loop)
(require 'efrit-do)

;;; Mock Helpers

(defun test-repl-loop--make-text (text)
  "Create a text content block with TEXT."
  (let ((ht (make-hash-table :test 'equal)))
    (puthash "type" "text" ht)
    (puthash "text" text ht)
    ht))

(defun test-repl-loop--make-tool-use (id name input-alist)
  "Create a tool_use content block with ID, NAME, and INPUT-ALIST."
  (let ((ht (make-hash-table :test 'equal))
        (input (make-hash-table :test 'equal)))
    (dolist (pair input-alist)
      (puthash (car pair) (cdr pair) input))
    (puthash "type" "tool_use" ht)
    (puthash "id" id ht)
    (puthash "name" name ht)
    (puthash "input" input ht)
    ht))

(defun test-repl-loop--make-response (content stop-reason)
  "Create a mock API response with CONTENT vector and STOP-REASON."
  (let ((response (make-hash-table :test 'equal)))
    (puthash "content" content response)
    (puthash "stop_reason" stop-reason response)
    response))

(defmacro test-repl-loop--with-mocks (responses tool-result &rest body)
  "Run BODY with the REPL API and tool dispatch stubbed.
RESPONSES is a form evaluating to a list of mock responses, returned
in order by the stubbed API call (synchronously).  TOOL-RESULT is the
string every stubbed tool dispatch returns."
  (declare (indent 2) (debug t))
  `(let ((test-responses ,responses)
         ;; These tests exercise the loop, not the reviewer (on by
         ;; default); test-review covers the review path with its own
         ;; stub.  Left on, the reviewer's unstubbed API call would
         ;; fail and be approved by policy -- passing, for the wrong
         ;; reason.
         (efrit-review-enabled nil))
     (cl-letf (((symbol-function 'efrit-repl-loop--api-call)
                (lambda (_session _messages callback)
                  (let ((response (pop test-responses)))
                    (if response
                        (funcall callback response nil)
                      (funcall callback nil "mock response queue empty")))))
               ((symbol-function 'efrit-do--execute-tool-string)
                (lambda (_tool-item) ,tool-result))
               ;; Agent buffer rendering is irrelevant here
               ((symbol-function 'efrit-agent-set-status) #'ignore))
       (unwind-protect
           (progn ,@body)
         (clrhash efrit-repl-loop--active)))))

;;; Tests

(ert-deftest test-repl-loop-tool-round-trip-ends-idle ()
  "A turn with one tool call accumulates messages and returns to idle."
  (let ((session (efrit-repl-session-create))
        (turn-reason nil))
    (test-repl-loop--with-mocks
        (list (test-repl-loop--make-response
               (vector (test-repl-loop--make-text "Computing.")
                       (test-repl-loop--make-tool-use
                        "tool-1" "eval_sexp" '(("expr" . "(+ 2 2)"))))
               "tool_use")
              (test-repl-loop--make-response
               (vector (test-repl-loop--make-text "The answer is 4."))
               "end_turn"))
        "4"
      (efrit-repl-continue session "what is 2+2?"
                           (lambda (_s reason) (setq turn-reason reason)))
      (should (equal turn-reason "end_turn"))
      (should (eq (efrit-repl-session-status session) 'idle))
      (should-not (efrit-repl-loop-active-p session))
      ;; user, assistant tool_use, user tool_result, assistant final
      (let ((messages (efrit-repl-session-api-messages session)))
        (should (= (length messages) 4))
        (should (equal (mapcar (lambda (m) (alist-get 'role m)) messages)
                       '("user" "assistant" "user" "assistant")))))))

(ert-deftest test-repl-loop-sandbox-denial-continues-turn ()
  "A sandbox denial is a failed tool result: the model answers after it.
The turn does not end at the denial (it used to), and the model's
follow-up response is delivered."
  (require 'efrit-sandbox)
  (let ((session (efrit-repl-session-create))
        (turn-reason nil))
    (test-repl-loop--with-mocks
        (list (test-repl-loop--make-response
               (vector (test-repl-loop--make-tool-use
                        "tool-1" "shell_exec" '(("command" . "cat ~/x"))))
               "tool_use")
              (test-repl-loop--make-response
               (vector (test-repl-loop--make-text "No access to that file; here is what I can do."))
               "end_turn"))
        (concat efrit-sandbox-denied-prefix "run shell commands. The user declined.")
      (efrit-repl-continue session "read my notes"
                           (lambda (_s reason) (setq turn-reason reason)))
      (should (equal turn-reason "end_turn"))
      (should (eq (efrit-repl-session-status session) 'idle))
      (let ((messages (efrit-repl-session-api-messages session)))
        (should (= (length messages) 4))
        ;; the denial went back as an is_error tool_result
        (let* ((tr (nth 2 messages))
               (block (aref (alist-get 'content tr) 0)))
          (should (eq (alist-get 'is_error block) t))
          (should (string-prefix-p efrit-sandbox-denied-prefix (alist-get 'content block))))))))

(ert-deftest test-repl-loop-steer-delivered-with-tool-results ()
  "A `steer' event during a turn puts its text into the user message
that carries the next tool results, as a text block after them; a
`steered' event says so.  Steering that finds no tool round is queued
when the turn ends."
  (let* ((session (efrit-repl-session-create))
         (turn-reason nil) (steered nil)
         (listener (lambda (e) (push (alist-get :text e) steered))))
    (efrit-subscribe 'steered listener)
    (unwind-protect
        (test-repl-loop--with-mocks
            (list (test-repl-loop--make-response
                   (vector (test-repl-loop--make-tool-use
                            "tool-1" "eval_sexp" '(("expr" . "(+ 2 2)"))))
                   "tool_use")
                  (test-repl-loop--make-response
                   (vector (test-repl-loop--make-text "Done, in French."))
                   "end_turn"))
            "4"
          ;; The mock API is synchronous, so steer before the turn: the
          ;; event lands on the working session as soon as it exists.
          (cl-letf* ((orig (symbol-function 'efrit-repl-loop--api-call))
                     ((symbol-function 'efrit-repl-loop--api-call)
                      (lambda (s messages callback)
                        (when (= 1 (length messages))
                          (efrit-publish 'steer `((:session-id . ,(efrit-repl-session-id s))
                                                  (:text . "answer in French"))))
                        (funcall orig s messages callback))))
            (efrit-repl-continue session "what is 2+2?"
                                 (lambda (_s reason) (setq turn-reason reason))))
          (should (equal turn-reason "end_turn"))
          (should (equal '("answer in French") steered))
          (let* ((messages (efrit-repl-session-api-messages session))
                 (results (nth 2 messages))
                 (blocks (append (alist-get 'content results) nil)))
            (should (= 4 (length messages)))
            (should (equal "user" (alist-get 'role results)))
            (should (= 2 (length blocks)))
            (should (equal "tool_result" (alist-get 'type (nth 0 blocks))))
            (should (equal "text" (alist-get 'type (nth 1 blocks))))
            (should (string-prefix-p efrit-repl-steering-frame (alist-get 'text (nth 1 blocks))))
            (should (string-suffix-p "answer in French" (alist-get 'text (nth 1 blocks)))))
          (should-not (efrit-repl-session-steering session))
          ;; A steer with no tool round left: queued at turn end
          (efrit-repl-session-steer session "and shorter")
          (efrit-repl-loop--end-turn session "end_turn")
          (should (equal '("and shorter") (efrit-repl-session-queue session)))
          (should-not (efrit-repl-session-steering session)))
      (efrit-unsubscribe 'steered listener))))

(ert-deftest test-repl-loop-cancelled-request-publishes-turn-complete ()
  "A stream cancelled by the user ends the turn as interrupted AND
publishes turn-complete, like every other ending."
  (let* ((session (efrit-repl-session-create))
         (seen nil)
         (listener (lambda (e) (push (alist-get :stop-reason e) seen))))
    (efrit-subscribe 'turn-complete listener)
    (unwind-protect
        (progn
          (puthash (efrit-repl-session-id session) (list session nil 1) efrit-repl-loop--active)
          (efrit-repl-session-set-status session 'working)
          (efrit-repl-loop--on-api-error session "interrupted")
          (should (equal '("interrupted") seen))
          (should (eq 'idle (efrit-repl-session-status session))))
      (efrit-unsubscribe 'turn-complete listener)
      (clrhash efrit-repl-loop--active))))

(ert-deftest test-repl-loop-c-g-ends-turn ()
  "C-g during a tool still ends the turn as interrupted."
  (let ((session (efrit-repl-session-create))
        (turn-reason nil))
    (test-repl-loop--with-mocks
        (list (test-repl-loop--make-response
               (vector (test-repl-loop--make-tool-use
                        "tool-1" "eval_sexp" '(("expr" . "(sleep-for 9)"))))
               "tool_use"))
        (signal 'quit nil)
      (efrit-repl-continue session "wait"
                           (lambda (_s reason) (setq turn-reason reason)))
      (should (equal turn-reason "interrupted")))))

(ert-deftest test-repl-loop-waiting-for-user-pauses-turn ()
  "A request_user_input result pauses the turn in waiting status."
  (let ((session (efrit-repl-session-create))
        (turn-reason nil))
    (test-repl-loop--with-mocks
        (list (test-repl-loop--make-response
               (vector (test-repl-loop--make-tool-use
                        "tool-1" "request_user_input" '(("prompt" . "Which?"))))
               "tool_use"))
        "[WAITING-FOR-USER] Which?"
      (efrit-repl-continue session "do the thing"
                           (lambda (_s reason) (setq turn-reason reason)))
      (should (equal turn-reason "waiting-for-user"))
      (should (eq (efrit-repl-session-status session) 'waiting))
      (should-not (efrit-repl-loop-active-p session)))))

(ert-deftest test-repl-loop-recovers-a-stale-working-session ()
  "A session left `working' with no loop (a reload dropped the loop
table) is ended as interrupted the next time something asks whether
it is busy; a held (pretend-busy) session is left alone."
  (let ((session (efrit-repl-session-create)) (seen nil))
    (cl-letf (((symbol-function 'efrit-agent-set-status) #'ignore))
      (let ((listener (lambda (e) (push (alist-get :stop-reason e) seen))))
        (efrit-subscribe 'turn-complete listener)
        (unwind-protect
            (progn
              (efrit-repl-session-set-status session 'working)
              (should (efrit-repl-loop-recover-stale session))
              (should (eq 'idle (efrit-repl-session-status session)))
              (should (equal '("interrupted") seen))
              ;; held: not stale
              (efrit-repl-loop-hold session)
              (should-not (efrit-repl-loop-recover-stale session))
              (should (eq 'working (efrit-repl-session-status session)))
              (efrit-repl-loop-release session)
              (should (eq 'idle (efrit-repl-session-status session))))
          (efrit-unsubscribe 'turn-complete listener))))))

(ert-deftest test-repl-loop-api-error-fails-turn ()
  "An API error ends the turn with the api-error reason."
  (let ((session (efrit-repl-session-create))
        (turn-reason nil))
    (test-repl-loop--with-mocks (list) "unused"
      ;; Empty queue makes the stubbed API call report an error
      (efrit-repl-continue session "hello"
                           (lambda (_s reason) (setq turn-reason reason)))
      (should (equal turn-reason "api-error"))
      (should-not (efrit-repl-loop-active-p session)))))

(ert-deftest test-repl-loop-session-complete-ends-turn ()
  "A session_complete tool result ends the turn with its reason."
  (let ((session (efrit-repl-session-create))
        (turn-reason nil))
    (test-repl-loop--with-mocks
        (list (test-repl-loop--make-response
               (vector (test-repl-loop--make-tool-use
                        "tool-1" "session_complete" '(("summary" . "done"))))
               "tool_use"))
        "[SESSION-COMPLETE: All done]"
      (efrit-repl-continue session "finish up"
                           (lambda (_s reason) (setq turn-reason reason)))
      (should (equal turn-reason "session-complete"))
      (should (eq (efrit-repl-session-status session) 'idle)))))

(ert-deftest test-repl-loop-max-tokens-ends-turn-with-note ()
  "An answer cut at max_tokens ends the turn like end_turn, keeps the
text, and publishes a note naming the limit."
  (let ((session (efrit-repl-session-create))
        (turn-reason nil) (notes nil))
    (test-repl-loop--with-mocks
        (list (test-repl-loop--make-response
               (vector (test-repl-loop--make-text "the first half of a long briefing"))
               "max_tokens"))
        "unused"
      (let ((fn (lambda (event) (push (alist-get :text event) notes))))
        (efrit-subscribe 'note fn)
        (unwind-protect
            (efrit-repl-continue session "brief me"
                                 (lambda (_s reason) (setq turn-reason reason)))
          (efrit-unsubscribe 'note fn)))
      (should (equal turn-reason "end_turn"))
      (should (eq (efrit-repl-session-status session) 'idle))
      (should (= 1 (length notes)))
      (should (string-match-p "cut at [0-9]+ output tokens" (car notes)))
      (should (string-match-p "efrit-default-max-tokens" (car notes)))
      ;; The request asked for the configured limit, not a literal.
      (should (integerp efrit-default-max-tokens))
      (should (> efrit-default-max-tokens 8192)))))

(ert-deftest test-repl-loop-turn-resets-tool-counters ()
  "Each turn starts with fresh tool counters; they used to span the Emacs process."
  (require 'efrit-tools)
  (let ((session (efrit-repl-session-create)))
    (setq efrit-tools--eval-count 99 efrit-tools--total-call-count 99)
    (test-repl-loop--with-mocks
        (list (test-repl-loop--make-response
               (vector (test-repl-loop--make-text "ok")) "end_turn"))
        "unused"
      (efrit-repl-continue session "hello" #'ignore)
      (should (= efrit-tools--eval-count 0))
      (should (= efrit-tools--total-call-count 0)))))

(ert-deftest test-repl-loop-turn-resets-circuit-breaker ()
  "A tripped breaker from an earlier turn does not block the next one."
  (require 'efrit-do-circuit-breaker)
  (let ((session (efrit-repl-session-create)))
    (setq efrit-do--session-tool-count 30
          efrit-do--circuit-breaker-tripped "Session limit reached: 30/30")
    (test-repl-loop--with-mocks
        (list (test-repl-loop--make-response
               (vector (test-repl-loop--make-text "ok")) "end_turn"))
        "unused"
      (efrit-repl-continue session "hello" #'ignore)
      (should (= efrit-do--session-tool-count 0))
      (should-not efrit-do--circuit-breaker-tripped)
      (should (car (efrit-do--circuit-breaker-check-limits "read_file" nil))))))

(provide 'test-repl-loop)

;;; test-repl-loop.el ends here
