;;; test-permissions.el --- Tests for efrit-permissions -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'efrit-permissions)
(require 'efrit-do-dispatch)
(require 'efrit-result-struct)
(require 'efrit-do-handlers)
(require 'efrit-sandbox)
(defvar efrit-sandbox-enabled)

(defun test-perm--input (&rest kvs)
  (let ((h (make-hash-table :test 'equal)))
    (while kvs (puthash (pop kvs) (pop kvs) h))
    h))

(defmacro test-perm--with-prompt-answer (answer &rest body)
  "Run BODY with the interactive prompt answering ANSWER (a symbol)."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'efrit-permission--prompt)
              (lambda (_req) ,answer)))
     ,@body))

(defmacro test-perm--fresh (&rest body)
  "The per-call prompt is the legacy layer; it only runs with the scope
sandbox off, so these tests bind it off."
  (declare (indent 0))
  `(let ((efrit-sandbox-enabled nil)
         (efrit-permission-policy '(write exec))
         (efrit-permission-responder-function nil)
         (efrit-permission--session-grants (make-hash-table :test 'equal)))
     ,@body))

;;; Classification

(ert-deftest test-perm-classes ()
  (should (eq (efrit-permission-tool-class "eval_sexp") 'exec))
  (should (eq (efrit-permission-tool-class "edit_file") 'write))
  (should (eq (efrit-permission-tool-class "read_file") 'read))
  (should (eq (efrit-permission-tool-class "session_complete") 'control))
  (should (eq (efrit-permission-tool-class "no_such_tool") 'read)))

(ert-deftest test-perm-read-and-control-never-gated ()
  (test-perm--fresh
    (cl-letf (((symbol-function 'efrit-permission--prompt)
               (lambda (_) (error "must not prompt"))))
      (should (eq (efrit-permission-check "read_file" (test-perm--input "path" "x")) 'allow))
      (should (eq (efrit-permission-check "todo_write" (test-perm--input)) 'allow))
      (should (eq (efrit-permission-check "editor_state" (test-perm--input)) 'allow)))))

(ert-deftest test-perm-policy-nil-disables ()
  (test-perm--fresh
    (let ((efrit-permission-policy nil))
      (cl-letf (((symbol-function 'efrit-permission--prompt)
                 (lambda (_) (error "must not prompt"))))
        (should (eq (efrit-permission-check "eval_sexp" (test-perm--input "expr" "(x)")) 'allow))))))

;;; Prompt outcomes

(ert-deftest test-perm-prompt-allow-once-does-not-persist ()
  (test-perm--fresh
    (test-perm--with-prompt-answer 'allow
      (should (eq (efrit-permission-check "eval_sexp" (test-perm--input "expr" "1") "s1") 'allow)))
    (test-perm--with-prompt-answer 'deny
      (should (eq (efrit-permission-check "eval_sexp" (test-perm--input "expr" "2") "s1") 'deny)))))

(ert-deftest test-perm-allow-tool-persists-for-session-only ()
  (test-perm--fresh
    (test-perm--with-prompt-answer 'allow-tool
      (should (eq (efrit-permission-check "edit_file" (test-perm--input "path" "a") "s1") 'allow)))
    ;; Same tool, same session: no prompt
    (cl-letf (((symbol-function 'efrit-permission--prompt)
               (lambda (_) (error "must not prompt"))))
      (should (eq (efrit-permission-check "edit_file" (test-perm--input "path" "b") "s1") 'allow)))
    ;; Different tool, same session: prompts
    (test-perm--with-prompt-answer 'deny
      (should (eq (efrit-permission-check "shell_exec" (test-perm--input "command" "ls") "s1") 'deny)))
    ;; Same tool, other session: prompts
    (test-perm--with-prompt-answer 'deny
      (should (eq (efrit-permission-check "edit_file" (test-perm--input "path" "c") "s2") 'deny)))))

(ert-deftest test-perm-allow-all-and-reset ()
  (test-perm--fresh
    (test-perm--with-prompt-answer 'allow-all
      (should (eq (efrit-permission-check "shell_exec" (test-perm--input "command" "ls") "s1") 'allow)))
    (cl-letf (((symbol-function 'efrit-permission--prompt)
               (lambda (_) (error "must not prompt"))))
      (should (eq (efrit-permission-check "eval_sexp" (test-perm--input "expr" "1") "s1") 'allow)))
    (efrit-permission-reset "s1")
    (test-perm--with-prompt-answer 'deny
      (should (eq (efrit-permission-check "eval_sexp" (test-perm--input "expr" "1") "s1") 'deny)))))

(ert-deftest test-perm-quit-at-prompt-denies ()
  (test-perm--fresh
    (cl-letf (((symbol-function 'efrit-permission--prompt)
               (lambda (_) (signal 'quit nil))))
      (should (eq (efrit-permission-check "eval_sexp" (test-perm--input "expr" "1")) 'deny)))))

;;; Responder function

(ert-deftest test-perm-responder-decides-and-sees-request ()
  (test-perm--fresh
    (let ((seen nil))
      (setq efrit-permission-responder-function
            (lambda (req) (setq seen req)
              (if (equal (alist-get :tool req) "shell_exec") 'deny 'allow)))
      (cl-letf (((symbol-function 'efrit-permission--prompt)
                 (lambda (_) (error "must not prompt"))))
        (should (eq (efrit-permission-check "shell_exec" (test-perm--input "command" "rm -rf x") "s1") 'deny))
        (should (equal (alist-get :class seen) 'exec))
        (should (string-match-p "rm -rf x" (alist-get :summary seen)))
        (should (equal (alist-get :session-id seen) "s1"))
        (should (eq (efrit-permission-check "edit_file" (test-perm--input "path" "p") "s1") 'allow))))))

(ert-deftest test-perm-responder-nil-falls-through-and-error-is-tolerated ()
  (test-perm--fresh
    (setq efrit-permission-responder-function (lambda (_) nil))
    (test-perm--with-prompt-answer 'allow
      (should (eq (efrit-permission-check "eval_sexp" (test-perm--input "expr" "1")) 'allow)))
    (setq efrit-permission-responder-function (lambda (_) (error "responder broke")))
    (test-perm--with-prompt-answer 'deny
      (should (eq (efrit-permission-check "eval_sexp" (test-perm--input "expr" "1")) 'deny)))))

(ert-deftest test-perm-responder-allow-tool-is-remembered ()
  (test-perm--fresh
    (setq efrit-permission-responder-function (lambda (_) 'allow-tool))
    (should (eq (efrit-permission-check "edit_file" (test-perm--input "path" "p") "s9") 'allow))
    (setq efrit-permission-responder-function nil)
    (cl-letf (((symbol-function 'efrit-permission--prompt)
               (lambda (_) (error "must not prompt"))))
      (should (eq (efrit-permission-check "edit_file" (test-perm--input "path" "q") "s9") 'allow)))))

;;; Summaries

(ert-deftest test-perm-summaries ()
  (should (string-match-p "\\$ git status"
                          (efrit-permission-summarize "shell_exec" (test-perm--input "command" "git status"))))
  (should (string-match-p "(message \"hi\")"
                          (efrit-permission-summarize "eval_sexp" (test-perm--input "expr" "(message \"hi\")"))))
  (let ((s (efrit-permission-summarize "edit_file"
                                       (test-perm--input "path" "a.el" "old_str" "foo" "new_str" "bar"))))
    (should (string-match-p "a.el" s))
    (should (string-match-p "--- old_str\nfoo" s))
    (should (string-match-p "\\+\\+\\+ new_str\nbar" s)))
  (let ((efrit-permission-summary-max-lines 2))
    (should (string-match-p "more lines"
                            (efrit-permission-summarize "eval_sexp" (test-perm--input "expr" "a\nb\nc\nd"))))))

;;; Dispatcher integration

(ert-deftest test-perm-dispatch-denial-returns-sentinel-without-running ()
  (test-perm--fresh
    (let ((ran nil))
      (cl-letf (((symbol-function 'efrit-permission--prompt) (lambda (_) 'deny))
                ((symbol-function 'efrit-do--handle-eval-sexp)
                 (lambda (&rest _) (setq ran t) "ran")))
        (efrit-do--circuit-breaker-reset)
        (let* ((item (test-perm--input "id" "t1" "name" "eval_sexp"
                                       "input" (test-perm--input "expr" "(delete-file \"x\")")))
               (result (efrit-do--execute-tool item)))
          (should-not ran)
          (should (eq (efrit-tool-result-status result) 'denied))
          (should (efrit-tool-result-error-p result))
          (should (string= (efrit-tool-result-text result)
                           efrit-permission-denied-result)))))))

(ert-deftest test-perm-dispatch-allow-runs-tool ()
  (test-perm--fresh
    (cl-letf (((symbol-function 'efrit-permission--prompt) (lambda (_) 'allow))
              ((symbol-function 'efrit-do--handle-eval-sexp)
               (lambda (&rest _) "ran")))
      (efrit-do--circuit-breaker-reset)
      (let* ((item (test-perm--input "id" "t2" "name" "eval_sexp"
                                     "input" (test-perm--input "expr" "(+ 1 1)")))
             (result (efrit-do--execute-tool item)))
        (should (eq (efrit-tool-result-status result) 'ok))
        (should (string-match-p "ran" (efrit-tool-result-text result)))))))

(ert-deftest test-perm-edit-choice-substitutes-input ()
  "Choosing [e] and changing the field makes dispatch run the edited input."
  (test-perm--fresh
    (let ((ran-with nil))
      (cl-letf (((symbol-function 'read-char-choice)
                 (let ((n 0)) (lambda (&rest _) (cl-incf n) (if (= n 1) ?e ?y))))
                ((symbol-function 'efrit-edit-in-buffer)
                 (lambda (_text _desc &optional _mode) "(edited)"))
                ((symbol-function 'efrit-show-preview) (lambda (&rest _) nil))
                ((symbol-function 'efrit-do--handle-eval-sexp)
                 (lambda (str &rest _) (setq ran-with str) "ok")))
        (efrit-do--circuit-breaker-reset)
        (let ((item (test-perm--input "id" "t3" "name" "eval_sexp"
                                      "input" (test-perm--input "expr" "(original)"))))
          (efrit-do--execute-tool item)
          (should (equal ran-with "(edited)")))))))

(ert-deftest test-perm-edit-unchanged-reprompts ()
  (test-perm--fresh
    (cl-letf (((symbol-function 'read-char-choice)
               (let ((n 0)) (lambda (&rest _) (cl-incf n) (if (= n 1) ?e ?n))))
              ((symbol-function 'efrit-edit-in-buffer) (lambda (text &rest _) text))
              ((symbol-function 'efrit-show-preview) (lambda (&rest _) nil)))
      (should (eq (efrit-permission-check "eval_sexp" (test-perm--input "expr" "x")) 'deny)))))

(ert-deftest test-perm-preview-text ()
  (let ((pv (efrit-permission--preview-text
             "edit_file" (test-perm--input "path" "a.el" "old_str" "foo" "new_str" "bar"))))
    (should (eq (cdr pv) 'diff-mode))
    (should (string-match-p "^-foo" (car pv)))
    (should (string-match-p "^\\+bar" (car pv))))
  (let ((pv (efrit-permission--preview-text
             "create_file" (test-perm--input "path" "n.txt" "content" "l1\nl2"))))
    (should (string-match-p "\\+\\+\\+ b/n.txt" (car pv)))
    (should (string-match-p "^\\+l1\n\\+l2" (car pv))))
  (should-not (efrit-permission--preview-text "shell_exec" (test-perm--input "command" "ls"))))

(ert-deftest test-perm-fence-for ()
  (should (equal (efrit-fence-for "no ticks") "```"))
  (should (equal (efrit-fence-for "a ``` b") "````"))
  (should (equal (efrit-fence-for "x `````` y") "```````")))

(provide 'test-permissions)
;;; test-permissions.el ends here
