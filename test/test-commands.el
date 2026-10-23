;;; test-commands.el --- efrit-commands, prompt library placeholders -*- lexical-binding: t; -*-
;;; Code:
(require 'ert)
(require 'efrit-commands)
(require 'efrit-prompts-library)
(require 'efrit-scope)

(defmacro test-cmd--capture (&rest body)
  "BODY with efrit-submit captured into SENT as (shown api question-p)."
  (declare (indent 0))
  `(let ((sent nil) (efrit-sandbox--question-turn nil))
     (cl-letf (((symbol-function 'efrit-submit)
                (lambda (shown api &rest _) (setq sent (list shown api efrit-sandbox--question-turn)) t))
               ((symbol-function 'efrit-commands--require) #'ignore))
       ,@body
       sent)))

(ert-deftest test-fix-errors-in-scope-lists-the-diagnostics-in-a-brief ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun a ()\n  (undefined-thing))\n\n(defun b () 2)\n")
    (goto-char (point-min)) (forward-line 1)
    (cl-letf (((symbol-function 'efrit-tool-get-diagnostics--from-flymake)
               (lambda (_) '(((source . "flymake") (severity . "warning") (message . "void undefined-thing") (line . 2) (column . 3))
                             ((source . "flymake") (severity . "error") (message . "elsewhere") (line . 4) (column . 0)))))
              ((symbol-function 'efrit-tool-get-diagnostics--from-flycheck) (lambda (_) nil)))
      (let ((sent (test-cmd--capture (efrit-fix-errors-in-scope 'defun))))
        (should (string-match-p "fix 1 diagnostic" (nth 0 sent)))
        (should (string-match-p "Goal:\nFix the 1 diagnostic" (nth 1 sent)))
        (should (string-match-p ":2:3  warning: void undefined-thing" (nth 1 sent)))
        (should-not (string-match-p "elsewhere" (nth 1 sent)))
        (should (string-match-p "Fix only the listed" (nth 1 sent)))
        (should-not (nth 2 sent)))
      ;; a line with no diagnostic: nothing to send
      (goto-char (point-min)) (forward-line 2)
      (should-error (test-cmd--capture (efrit-fix-errors-in-scope 'line)) :type 'user-error))))

(ert-deftest test-investigate-exception-is-a-question-turn ()
  (let ((comp (get-buffer-create "*compilation*")))
    (unwind-protect
        (save-window-excursion
          (with-current-buffer comp (erase-buffer) (insert "make: *** [all] Error 2\n"))
          (set-window-buffer (selected-window) comp)
          (with-temp-buffer
            (insert "code\n")
            (let ((sent (test-cmd--capture (efrit-investigate-exception))))
              (should (string-match-p "investigate the failure in \\*compilation\\*" (nth 0 sent)))
              (should (string-match-p "Error 2" (nth 1 sent)))
              (should (string-match-p "Answer the question only" (nth 1 sent)))
              (should (nth 2 sent)))))
      (kill-buffer comp))))

(ert-deftest test-checkpoint-steers-when-busy-else-asks ()
  (let ((steered nil))
    (cl-letf (((symbol-function 'efrit-commands--require) #'ignore)
              ((symbol-function 'efrit-agent-target-buffer) (lambda (&rest _) (current-buffer)))
              ((symbol-function 'efrit-agent--session-busy-p) (lambda () t))
              ((symbol-function 'efrit-agent-busy-submit-steer) (lambda (text) (setq steered text))))
      (efrit-agent-checkpoint)
      (should (string-match-p "CHECKPOINT" steered)))))

(ert-deftest test-scope-fill-asks-for-placeholders-with-defaults ()
  "{{{?name|Prompt|default}}} asks once and reuses; defaults from a value key."
  (let ((asked nil))
    (cl-letf (((symbol-function 'read-string)
               (lambda (prompt &optional _i _h default) (push prompt asked) (or default "typed"))))
      (let ((out (efrit-scope-fill "Rename {{{?old|Old name|symbol-at-point}}} to {{{?new|New name|}}}; again {{{?old}}} in {{{:file}}}"
                                   '((symbol-at-point . "foo") (file . "f.el")))))
        (should (equal "Rename foo to typed; again foo in f.el" out))
        (should (= 2 (length asked)))
        (should (string-match-p "Old name (default foo)" (cadr asked)))))))

(ert-deftest test-refactoring-prompts-exist-and-filter-by-scope ()
  (should (member "refactor: Extract Method" (efrit-refactoring-names 'region)))
  (should-not (member "refactor: Move Method" (efrit-refactoring-names 'region)))
  (should (efrit-prompts-get "refactor: Rename"))
  (should (efrit-prompts-get "blame-analysis"))
  (should (eq 'single (efrit-prompts-kind (efrit-prompts-get "log-analysis")))))

(provide 'test-commands)
;;; test-commands.el ends here

(ert-deftest test-scope-fill-keeps-match-data-across-askers ()
  "A placeholder default that searches (symbol-at-point) must not break the fill.
Drive section 12 caught `Args out of range' from `replace-regexp-in-string'."
  (require 'efrit-scope)
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun greet (name) name)")
    (goto-char 9)
    (cl-letf (((symbol-function 'read-string)
               (lambda (_p &optional _i _h default) (or default "shout"))))
      (should (equal "rename greet to shout now"
                     (efrit-scope-fill "rename {{{?old|Old|symbol-at-point}}} to {{{?new|New|}}} now"
                                       (list (cons 'symbol-at-point (lambda () (thing-at-point 'symbol t))))))))))
