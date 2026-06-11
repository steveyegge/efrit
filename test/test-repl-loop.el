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
  `(let ((test-responses ,responses))
     (cl-letf (((symbol-function 'efrit-repl-loop--api-call)
                (lambda (_session _messages callback)
                  (let ((response (pop test-responses)))
                    (if response
                        (funcall callback response nil)
                      (funcall callback nil "mock response queue empty")))))
               ((symbol-function 'efrit-do--execute-tool)
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

(provide 'test-repl-loop)

;;; test-repl-loop.el ends here
