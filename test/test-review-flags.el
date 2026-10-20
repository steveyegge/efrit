;;; test-review-flags.el --- what a Lisp change does, for the reviewer -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'efrit-review-flags)
(require 'efrit-review)

(defun test-rf--use (name &rest kv)
  (let ((input (make-hash-table :test 'equal)))
    (while kv (puthash (pop kv) (pop kv) input))
    (list "id" name input)))

(ert-deftest test-review-flags-effects-of-forms ()
  "Keys, hooks, advice, core variables, :vc, shadowing, loads and side effects are found;
plain defuns under the file's own prefix and quoted data are not."
  (let ((eff (efrit-review-flags--text-effects
              "(global-set-key (kbd \"C-c x\") #'foo)
               (add-hook 'after-save-hook #'bar)
               (advice-add 'find-file :around #'baz)
               (setq load-path (cons \"/x\" load-path))
               (use-package magit :vc (:url \"https://example.invalid/m.git\" :rev \"abc\") :init (setq m 1))
               (defmacro transient-describe-global-set-key (&rest _) nil)
               (defun tzz-fine () 1)
               (require 'dash)
               (delete-file \"/tmp/x\")
               '(global-set-key \"not real\")"
              '("tzz"))))
    (should (assq 'key eff))
    (should (string-match-p "global" (cdr (assq 'key eff))))
    (should (string-match-p "after-save-hook" (cdr (assq 'hook eff))))
    (should (string-match-p "find-file" (cdr (assq 'advice eff))))
    (should (string-match-p "load-path" (cdr (assq 'core-var eff))))
    (should (string-match-p "example.invalid" (cdr (assq 'vc eff))))
    (should (assq 'init eff))
    (should (string-match-p "transient-describe-global-set-key" (cdr (assq 'shadow eff))))
    (should (string-match-p "dash" (cdr (assq 'load eff))))
    (should (string-match-p "delete-file" (cdr (assq 'effect eff))))
    ;; one key flag only: the quoted one is data
    (should (= 1 (cl-count 'key eff :key #'car)))
    ;; tzz-fine is the file's own
    (should-not (cl-some (lambda (e) (string-match-p "tzz-fine" (cdr e))) eff))))

(ert-deftest test-review-flags-edit-reports-only-what-changes ()
  "An edit_file to an .el file is flagged for the effects the new text adds,
not for what the old text already did; a non-Lisp file is not analysed."
  (let ((flags (efrit-review-flags-for-use
                (test-rf--use "edit_file"
                              "path" "/home/u/autodist/emacs/tzz.emacs.libraries.el"
                              "old_str" "(use-package expand-region :bind (\"C-=\" . er/expand-region))"
                              "new_str" "(use-package expand-region :bind (\"C-=\" . er/expand-region))
                                         (use-package repeat :ensure nil :config (repeat-mode 1))
                                         (advice-add 'save-buffer :before #'tzz-note)"))))
    (should (cl-some (lambda (f) (string-match-p "advice-add save-buffer" f)) flags))
    (should-not (cl-some (lambda (f) (string-match-p "expand-region" f)) flags)))
  (should-not (efrit-review-flags-for-use
               (test-rf--use "edit_file" "path" "/home/u/notes.txt" "old_str" "a" "new_str" "(advice-add 'x :around #'y)")))
  ;; eval: the form itself
  (should (cl-some (lambda (f) (string-match-p "shadow" f))
                   (efrit-review-flags-for-use
                    (test-rf--use "eval_sexp" "expr" "(defmacro transient-describe-global-set-key (&rest _) nil)")))))

(ert-deftest test-review-flags-reach-the-reviewer-text ()
  "The batch text the reviewer reads carries the flags under the call, and
the system prompt explains the kinds."
  (let* ((input (make-hash-table :test 'equal))
         (use (let ((u (make-hash-table :test 'equal)))
                (puthash "path" "/x/init.el" input)
                (puthash "content" "(use-package evil :vc (:url \"https://example.invalid/evil\"))" input)
                (puthash "type" "tool_use" u) (puthash "id" "t1" u)
                (puthash "name" "create_file" u) (puthash "input" input u) u))
         (batch (efrit-review-describe-batch (vector use))))
    (should (string-match-p "\\[FLAG vc: use-package evil :vc" batch))
    (should (string-match-p "vc = a use-package" efrit-review--system-prompt))))

(ert-deftest test-review-flags-eval-shelling-out-to-what-emacs-does ()
  "An eval that runs `shell-command-to-string \"git …\"' is flagged with the
facility the sandbox will name when it refuses (2026-10-19: approved
without a flag); a real program only gets the plain effect flag."
  (require 'efrit-sandbox)
  (let* ((flags (lambda (expr)
                  (let ((input (make-hash-table :test 'equal)))
                    (puthash "expr" expr input)
                    (efrit-review-flags-for-use (list "t1" "eval_sexp" input))))))
    (let ((git (funcall flags "(shell-command-to-string \"git status --short x.el\")")))
      (should (cl-some (lambda (f) (string-match-p "\\[FLAG shell: Emacs does this natively: git -> vcs_status" f)) git)))
    (let ((tmux (funcall flags "(call-process \"tmux\" nil nil nil \"ls\")")))
      (should (cl-some (lambda (f) (string-match-p "\\[FLAG effect: call-process" f)) tmux))
      (should-not (cl-some (lambda (f) (string-match-p "Emacs does this natively" f)) tmux)))))

(ert-deftest test-review-flags-eval-reading-files-by-hand ()
  "An eval that does insert-file-contents + re-search-forward on a literal
path is flagged `tool' with read_file / search_content named; a form that
computes nothing file-like gets no such flag."
  (let* ((flags (lambda (expr)
                  (let ((input (make-hash-table :test 'equal)))
                    (puthash "expr" expr input)
                    (efrit-review-flags-for-use (list "t1" "eval_sexp" input))))))
    (let ((f (funcall flags "(with-temp-buffer (insert-file-contents (expand-file-name \"~/x/a.el\")) (goto-char (point-min)) (re-search-forward \"foo\" nil t))")))
      (should (cl-some (lambda (x) (string-match-p "\\[FLAG tool: insert-file-contents by hand: efrit has read_file" x)) f))
      (should (cl-some (lambda (x) (string-match-p "re-search-forward by hand: efrit has search_content" x)) f)))
    (should-not (cl-some (lambda (x) (string-match-p "FLAG tool" x))
                         (funcall flags "(progn (global-completion-preview-mode -1) (list major-mode))")))))

(provide 'test-review-flags)
;;; test-review-flags.el ends here
