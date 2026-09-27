;;; test-repl-session.el --- Tests for efrit-repl-session -*- lexical-binding: t; -*-

;;; Commentary:

;; History marks, the last-answer reader, the context guard, and the
;; session persistence round trip.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'efrit-repl-session)
(require 'efrit-session-persist)
(require 'efrit-config)   ; `efrit-default-model' must be special for the let below

(defun test-repl-session--assistant (session text)
  "Add TEXT as an assistant text block to SESSION."
  (efrit-repl-session-add-assistant-message
   session (vector `((type . "text") (text . ,text)))))

(defun test-repl-session--tool-round (session id result)
  "Add a tool_use ID with RESULT to SESSION, as a turn does."
  (efrit-repl-session-add-assistant-message
   session (vector `((type . "tool_use") (id . ,id) (name . "gnus_articles")
                     (input . (("refs" . ["g#1"]))))))
  (efrit-repl-session-add-tool-result session id result))

(ert-deftest test-repl-session-mark-rewind-last-answer ()
  "A mark is the history length; rewind drops what came after it and keeps
the human conversation; last-answer reads the newest assistant text
after the mark, or nil when the turns after it have none."
  (let ((session (efrit-repl-session-create)))
    (efrit-repl-session-add-user-message session "q1")
    (test-repl-session--assistant session "a1")
    (let ((mark (efrit-repl-session-history-mark session)))
      (should (= 2 mark))
      (should (equal "a1" (efrit-repl-session-last-answer session)))
      (should-not (efrit-repl-session-last-answer session mark))
      (efrit-repl-session-add-user-message session "q2")
      (test-repl-session--tool-round session "t1" "tool text")
      (test-repl-session--assistant session "a2")
      (should (equal "a2" (efrit-repl-session-last-answer session mark)))
      ;; The tool_use message has no text: it is not an answer
      (should (= 6 (length (efrit-repl-session-api-messages session))))
      (should (= 4 (efrit-repl-session-rewind session mark)))
      (should (= 2 (length (efrit-repl-session-api-messages session))))
      (should (= 0 (efrit-repl-session-rewind session mark)))
      ;; The conversation the user saw is untouched (q1 a1 q2 tool a2)
      (should (= 5 (length (efrit-repl-session-conversation session))))
      (should (equal "a1" (efrit-repl-session-last-answer session))))))

(ert-deftest test-repl-session-fit-context-elides-oldest-tool-results-first ()
  "Over the budget, the oldest tool results are elided first, then the
oldest user messages; assistant messages and the last user message are
kept; a note is published; the elision persists in the history."
  (let ((session (efrit-repl-session-create))
        (efrit-repl-context-budget 200)   ; tokens: 700 chars at 3.5/token
        (notes nil))
    (efrit-repl-session-add-user-message session (make-string 300 ?u))
    (test-repl-session--tool-round session "t1" (make-string 300 ?r))
    (test-repl-session--assistant session (make-string 100 ?a))
    (efrit-repl-session-add-user-message session (concat "the current question" (make-string 80 ?q)))
    (cl-letf (((symbol-function 'efrit-publish)
               (lambda (type data) (push (cons type data) notes))))
      ;; 300 + tool_use input + 300 + 100 + 100 chars ~ 230 tokens: over
      (should (= 1 (efrit-repl-session-fit-context session))))
    (let ((messages (efrit-repl-session-api-messages session)))
      ;; The tool result body is gone, the user message is still whole
      (should (= 300 (length (cdr (assq 'content (nth 0 messages))))))
      (let ((result (aref (cdr (assq 'content (nth 2 messages))) 0)))
        (should (equal "tool_result" (cdr (assq 'type result))))
        (should (string-prefix-p "[elided: a tool result, 300 characters"
                                 (cdr (assq 'content result))))
        (should (equal "t1" (cdr (assq 'tool_use_id result)))))
      (should (= 100 (length (cdr (assq 'text (aref (cdr (assq 'content (nth 3 messages))) 0))))))
      (should (string-prefix-p "the current question" (cdr (assq 'content (nth 4 messages))))))
    (should (equal 'note (car (car notes))))
    (should (string-match-p "elided 1 old message bodies" (alist-get :text (cdr (car notes)))))
    ;; Now it fits: nothing more happens, no second note
    (cl-letf (((symbol-function 'efrit-publish)
               (lambda (type data) (push (cons type data) notes))))
      (should (= 0 (efrit-repl-session-fit-context session))))
    (should (= 1 (length notes)))
    ;; A tighter budget takes the oldest user message next, never the last one
    (let ((efrit-repl-context-budget 90))
      (cl-letf (((symbol-function 'efrit-publish) #'ignore))
        (should (= 1 (efrit-repl-session-fit-context session))))
      (let ((messages (efrit-repl-session-api-messages session)))
        (should (string-prefix-p "[elided: a user message, 300 characters"
                                 (cdr (assq 'content (nth 0 messages)))))
        (should (string-prefix-p "the current question" (cdr (assq 'content (nth 4 messages)))))))
    ;; get-api-messages runs the guard; the question is no longer last and goes too
    (let ((efrit-repl-context-budget 1))
      (cl-letf (((symbol-function 'efrit-publish) #'ignore))
        (efrit-repl-session-add-user-message session "next")
        (efrit-repl-session-get-api-messages session)
        (should (string-prefix-p "[elided: a user message"
                                 (cdr (assq 'content (nth 4 (efrit-repl-session-api-messages session))))))))))

(ert-deftest test-repl-session-context-budget-from-model-window ()
  "Without an explicit budget, the budget is the model's window less headroom."
  (let ((efrit-repl-context-budget nil)
        (efrit-repl-context-headroom 0.15)
        (efrit-usage-context-window 200000)
        (efrit-usage-context-windows '(("fable" . 1000000))))
    (let ((efrit-default-model "claude-fable-5-1"))
      (should (= 850000 (efrit-repl-session-context-budget))))
    (let ((efrit-default-model "claude-sonnet-4-5-20250929"))
      (should (= 170000 (efrit-repl-session-context-budget))))
    (let ((efrit-repl-context-budget 123))
      (should (= 123 (efrit-repl-session-context-budget))))))

(ert-deftest test-repl-session-persist-round-trip-keeps-api-messages ()
  "Saved API messages come back with symbol keys and vector content: the
serializer used string keys against symbol-keyed messages and every
message was written as null."
  (let* ((efrit-session-persist-dir (make-temp-file "efrit-persist-" t))
         (session (efrit-repl-session-create)))
    (unwind-protect
        (progn
          (efrit-repl-session-add-user-message session "shown" "api text")
          (test-repl-session--tool-round session "t1" "result")
          (test-repl-session--assistant session "done")
          (should (efrit-session-persist-save session))
          (let* ((loaded (efrit-session-persist-load (efrit-repl-session-id session)))
                 (messages (efrit-repl-session-api-messages loaded)))
            (should (= 4 (length messages)))
            (should (equal "user" (cdr (assq 'role (nth 0 messages)))))
            (should (equal "api text" (cdr (assq 'content (nth 0 messages)))))
            (should (vectorp (cdr (assq 'content (nth 1 messages)))))
            (should (equal "done" (efrit-repl-session-last-answer loaded)))
            ;; The loaded history goes through the API path unchanged
            (should (= 4 (length (efrit-repl-session-get-api-messages loaded))))))
      (delete-directory efrit-session-persist-dir t))))

(ert-deftest test-repl-session-upgrade-from-old-layout ()
  "A record from the 14-slot layout is rebuilt with its values in the
right slots by name and the new slots at their defaults (a stale
`working' becomes idle); the registry and the active session point at
the new one; a current record is left alone."
  (let* ((old (record 'efrit-repl-session
                      "repl-old" '(1 2) '(3 4) 'working 'buf '(conv) '(api) 'budget
                      '(tools) '(start) "/root/" "title" '(q) t))
         (efrit-repl-session--registry (make-hash-table :test 'equal))
         (efrit-repl-session--active old))
    (puthash "repl-old" old efrit-repl-session--registry)
    (should (= 15 (length old)))
    ;; the new accessor is exactly what broke live
    (should-error (efrit-repl-session-queue old) :type 'args-out-of-range)
    (let ((new (efrit-repl-session-upgrade old)))
      (should new)
      (should (equal "repl-old" (efrit-repl-session-id new)))
      ;; a turn cannot survive the reload that made the upgrade
      ;; necessary: working becomes idle, or every submit says busy
      (should (eq 'idle (efrit-repl-session-status new)))
      (should (equal "/root/" (efrit-repl-session-project-root new)))
      (should (equal '(q) (efrit-repl-session-pending-question new)))
      (should (eq t (efrit-repl-session-interrupt-requested new)))
      (should-not (efrit-repl-session-queue new))
      (should-not (efrit-repl-session-steering new))
      (should (eq new (gethash "repl-old" efrit-repl-session--registry)))
      (should (eq new efrit-repl-session--active))
      (should-not (efrit-repl-session-upgrade new))
      (should (equal (list (cons old new)) (progn (puthash "x" old efrit-repl-session--registry)
                                                  (efrit-repl-session-upgrade-all)))))))

(provide 'test-repl-session)
;;; test-repl-session.el ends here
