;;; test-questions.el --- answer-first guard and prose choices -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'efrit-questions)

(defun test-questions--content (&rest items)
  "A response content vector from ITEMS: strings are text blocks, lists are (NAME . INPUT) tool uses."
  (vconcat
   (mapcar (lambda (it)
             (let ((h (make-hash-table :test 'equal)))
               (if (stringp it)
                   (progn (puthash "type" "text" h) (puthash "text" it h))
                 (puthash "type" "tool_use" h) (puthash "id" "tu-1" h)
                 (puthash "name" (car it) h) (puthash "input" (or (cdr it) (make-hash-table :test 'equal)) h))
               h))
           items)))

(defun test-questions--messages (&rest turns)
  "API messages from TURNS: (ROLE . TEXT)."
  (vconcat (mapcar (lambda (t) `((role . ,(car t)) (content . ,(cdr t)))) turns)))

(ert-deftest test-questions-user-asked-p ()
  (should (efrit-questions-user-asked-p "which of the two is missing those features?"))
  (should (efrit-questions-user-asked-p "What are they good for?  "))
  (should (efrit-questions-user-asked-p "how does wgrep stage edits"))
  (should-not (efrit-questions-user-asked-p "Keep wgrep as is"))
  (should-not (efrit-questions-user-asked-p "1"))
  (should-not (efrit-questions-user-asked-p "")))

(ert-deftest test-questions-answer-first-refuses-a-bare-re-ask ()
  "A bare request_user_input after the user asked something is refused;
one with text before it, or after a plain answer, is not."
  (let ((asked (test-questions--messages '("user" . "drop wgrep") '("assistant" . "ok")
                                         '("user" . "which of the two is missing those features?")))
        (answered (test-questions--messages '("user" . "drop wgrep") '("assistant" . "ok")
                                            '("user" . "Keep wgrep as is"))))
    (should (efrit-questions-answer-first-p asked (test-questions--content '("request_user_input"))))
    (should-not (efrit-questions-answer-first-p
                 asked (test-questions--content "wgrep stages edits; the built-in does not." '("request_user_input"))))
    (should-not (efrit-questions-answer-first-p answered (test-questions--content '("request_user_input"))))
    ;; another tool is not a re-ask
    (should-not (efrit-questions-answer-first-p asked (test-questions--content '("read_file"))))
    ;; a tool-result-only user message is not the user's text: the
    ;; question before it still stands
    (let ((tool-results (vconcat (list `((role . "user")
                                         (content . ,(vector `((type . "tool_result") (content . "x")))))))))
      (should (efrit-questions-answer-first-p
               (vconcat asked tool-results) (test-questions--content '("request_user_input")))))
    ;; but only one refusal per user message: after it the re-ask goes through
    (let ((refused (vconcat (list `((role . "user")
                                    (content . ,(vector `((type . "tool_result")
                                                          (content . ,efrit-questions-answer-first-message)))))))))
      (should-not (efrit-questions-answer-first-p
                   (vconcat asked refused) (test-questions--content '("request_user_input"))))
      ;; a new user text resets the bound
      (should (efrit-questions-answer-first-p
               (vconcat asked refused (test-questions--messages '("user" . "why two?")))
               (test-questions--content '("request_user_input")))))))

(ert-deftest test-questions-answer-first-result-quotes-the-question ()
  (let ((text (efrit-questions-answer-first-result
               (test-questions--messages '("user" . "which one has the long vertical menu?")))))
    (should (string-prefix-p efrit-questions-answer-first-message text))
    (should (string-match-p "which one has the long vertical menu\\?" text))
    (should (string-match-p "plain text" text))))

(ert-deftest test-questions-parse-prose-choices ()
  "A final message with a numbered list and a closing 'Which one?' yields
the question and the item labels, cut at the first sentence; prose
without such a list, or a list with no question, yields nil."
  (let ((text "**Item 2: etags-regen-mode.** Keeps a TAGS file up to date. Three options:

1. Enable globally in libraries.el as a built-in :ensure nil block with ignores. My pick: you have dumb-jump already.
2. Skip. If eglot covers the languages you work in, etags adds little.
3. Try it live for a week (M-x etags-regen-mode), decide later.

Which one?"))
    (let ((parsed (efrit-questions-parse-prose-choices text)))
      (should (equal "Which one?" (car parsed)))
      (should (= 3 (length (cdr parsed))))
      (should (equal "Enable globally in libraries.el as a built-in :ensure nil block with ignores"
                     (nth 0 (cdr parsed))))
      (should (equal "Skip" (nth 1 (cdr parsed))))
      (should (string-prefix-p "Try it live for a week" (nth 2 (cdr parsed))))))
  ;; a list that restarts keeps the last one
  (should (= 2 (length (cdr (efrit-questions-parse-prose-choices
                             "Done:\n1. a\n2. b\n3. c\n\nNext:\n1. keep\n2. drop\n\nWhich do you prefer?")))))
  ;; lettered
  (should (equal '("alpha" "beta") (cdr (efrit-questions-parse-prose-choices "a) alpha\nb) beta\nWhich one?"))))
  ;; no question at the end: not a choice
  (should-not (efrit-questions-parse-prose-choices "1. a\n2. b\nDone."))
  ;; a question but no list
  (should-not (efrit-questions-parse-prose-choices "All set. Anything else you want?"))
  ;; one item is not a choice
  (should-not (efrit-questions-parse-prose-choices "1. only\nWhich one?"))
  ;; a y/n in words is a yes/no question
  (should (equal '("#27: your answer on winner-mode is pending (y/n)." "yes" "no")
                 (efrit-questions-parse-prose-choices "Done: #26.\n\n#27: your answer on winner-mode is pending (y/n).\n\nTo make live: C-M-x the block."))))

(provide 'test-questions)

;;; test-questions.el ends here
