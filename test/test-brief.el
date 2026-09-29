;;; test-brief.el --- briefs, suffix hook, verbatim, grill-me, question turns -*- lexical-binding: t; -*-
;;; Code:
(require 'ert)
(require 'efrit-brief)
(require 'efrit-agent-mentions)
(require 'efrit-sandbox)

(ert-deftest test-brief-sections-and-question-kind ()
  (let ((b (efrit-brief :goal "Rename foo" :scope "src/a.el" :instruction "Do it")))
    (should (string-match-p "^Goal:\nRename foo" b))
    (should (string-match-p "Boundaries:\nMake only" b))
    (should (string-match-p "Agent responsibilities:" b))
    (should (string-match-p "Verification evidence:" b))
    (should-not (string-match-p "Context:" b)))
  (let ((q (efrit-brief :goal "Why?" :kind 'question)))
    (should (string-match-p "Answer the question only" q))
    (should-not (string-match-p "responsibilities" q)))
  ;; an explicit empty string drops a default section
  (should-not (string-match-p "Boundaries" (efrit-brief :goal "x" :boundaries ""))))

(ert-deftest test-prompt-suffix-hook-with-memo-and-abort ()
  (let ((efrit-prompt-suffix-functions nil) (asked 0))
    (should (equal "hi" (efrit-prompt-apply-suffixes "hi")))
    (add-hook 'efrit-prompt-suffix-functions
              (lambda (ctx) (efrit-prompt-context-memoize ctx 'mode (lambda () (cl-incf asked) "tdd"))
                (format "A:%s" (efrit-prompt-context-memoize ctx 'mode #'ignore))))
    (add-hook 'efrit-prompt-suffix-functions
              (lambda (ctx) (format "B:%s:%s" (efrit-prompt-context-memoize ctx 'mode #'ignore)
                                    (efrit-prompt-context-command ctx))) 10)
    (add-hook 'efrit-prompt-suffix-functions (lambda (_) nil) 20)
    (let ((out (efrit-prompt-apply-suffixes "hi" 'my-cmd)))
      (should (equal "hi\n\nA:tdd\n\nB:tdd:my-cmd" out))
      (should (= 1 asked)))
    (add-hook 'efrit-prompt-suffix-functions (lambda (_) (error "no send")) 30)
    (should-error (efrit-prompt-apply-suffixes "hi"))))

(ert-deftest test-verbatim-text-is-not-scanned-for-mentions ()
  (let ((text (concat "see @real.el and " (propertize "diff: @fake.el changed" 'efrit-verbatim t))))
    (should (equal '((17 . 39)) (efrit-verbatim-spans text)))
    (should (equal '("real.el") (efrit-agent-mentions-in text)))))

(ert-deftest test-grill-me-suffix-fires-once ()
  (let ((efrit-grill-me nil) (efrit-prompt-suffix-functions '(efrit-grill-me-suffix)))
    (should (equal "q" (efrit-prompt-apply-suffixes "q")))
    (setq efrit-grill-me t)
    (should (string-match-p "clarifying questions" (efrit-prompt-apply-suffixes "q")))
    (should-not efrit-grill-me)
    (should (equal "q" (efrit-prompt-apply-suffixes "q")))))

(ert-deftest test-question-turn-refuses-write-shell-eval-without-a-prompt ()
  (let ((efrit-sandbox--question-turn nil) (asked nil)
        (efrit-sandbox-request-function (lambda (_) (setq asked t) 'session))
        (efrit-sandbox-enabled t))
    (efrit-brief-question-turn t)
    (should (efrit-sandbox-question-turn-p 'write))
    (should-not (efrit-sandbox-question-turn-p 'read))
    (should-error (efrit-sandbox-check 'shell "ls" "shell_exec") :type 'efrit-sandbox-denied)
    (should-not asked)
    (efrit-sandbox-end-turn)
    (should-not (efrit-sandbox-question-turn-p 'write))))

(provide 'test-brief)
;;; test-brief.el ends here
