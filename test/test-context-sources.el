;;; test-context-sources.el --- Tests for efrit-context-sources -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'efrit-context-sources)
(require 'efrit-repl-session)

(defmacro test-ctx--with-buffer (name content &rest body)
  "Run BODY in a fresh emacs-lisp-mode buffer NAME containing CONTENT."
  (declare (indent 2))
  `(with-temp-buffer
     (rename-buffer ,name t)
     (emacs-lisp-mode)
     (insert ,content)
     (goto-char (point-min))
     ,@body))

(ert-deftest test-ctx-snapshot-basic-fields ()
  (test-ctx--with-buffer "ctx-basic.el" "(defun foo ()\n  (bar))\n"
    (forward-line 1)
    (let* ((efrit-context-sources '(buffer position))
           (snap (efrit-context-snapshot (current-buffer))))
      (should (string-prefix-p "<editor-context>" snap))
      (should (string-suffix-p "</editor-context>" snap))
      (should (string-match-p "Buffer: ctx-basic.el" snap))
      (should (string-match-p "Mode: emacs-lisp-mode" snap))
      (should (string-match-p "Point: line 2, column 0" snap))
      (should (string-match-p "State: modified" snap)))))

(ert-deftest test-ctx-snapshot-region ()
  (test-ctx--with-buffer "ctx-region" "alpha\nbeta\ngamma\n"
    (transient-mark-mode 1)
    (set-mark 7)                        ; start of "beta"
    (goto-char 11)                      ; end of "beta"
    (activate-mark)
    (let* ((efrit-context-sources '(region))
           (snap (efrit-context-snapshot (current-buffer))))
      (should (string-match-p "Active region: line 2 col 0 to line 2 col 4 (4 chars)" snap))
      (should (string-match-p "<<<REGION\nbeta\n>>>" snap)))))

(ert-deftest test-ctx-snapshot-region-truncated ()
  (test-ctx--with-buffer "ctx-region-big" (make-string 100 ?x)
    (transient-mark-mode 1)
    (set-mark (point-min))
    (goto-char (point-max))
    (activate-mark)
    (let* ((efrit-context-sources '(region))
           (efrit-context-region-max-chars 10)
           (snap (efrit-context-snapshot (current-buffer))))
      (should (string-match-p "\\[truncated\\]" snap))
      (should (string-match-p "<<<REGION\nxxxxxxxxxx\n\\.\\.\\." snap)))))

(ert-deftest test-ctx-snapshot-nil-when-no-sources ()
  (test-ctx--with-buffer "ctx-none" "x"
    (let ((efrit-context-sources nil))
      (should-not (efrit-context-snapshot (current-buffer))))))

(ert-deftest test-ctx-custom-source-function-and-error-isolation ()
  (test-ctx--with-buffer "ctx-custom" "x"
    (let* ((efrit-context-sources
            (list (lambda (_b) (error "kaboom"))
                  (lambda (b) (format "Custom: %s" (buffer-name b)))
                  'nonexistent-source))
           (snap (efrit-context-snapshot (current-buffer))))
      (should (string-match-p "Custom: ctx-custom" snap))
      (should-not (string-match-p "kaboom" snap)))))

(ert-deftest test-ctx-snapshot-does-not-move-point-or-narrowing ()
  (test-ctx--with-buffer "ctx-pure" "one\ntwo\nthree\n"
    (goto-char 5)
    (narrow-to-region 5 9)
    (let ((efrit-context-sources '(buffer position region project)))
      (efrit-context-snapshot (current-buffer))
      (should (= (point) 5))
      (should (buffer-narrowed-p)))))

(ert-deftest test-ctx-target-buffer-skips-agent-buffer ()
  "From an efrit UI buffer, the target is another buffer, never efrit's own."
  (let ((efrit-context--agent-buffer-p-function
         (lambda (b) (string-prefix-p "*efrit-fake*" (buffer-name b)))))
    (with-temp-buffer
      (rename-buffer "ctx-user-buffer" t)
      (let ((user (current-buffer)))
        (with-temp-buffer
          (rename-buffer "*efrit-fake*" t)
          (let ((target (efrit-context-target-buffer (current-buffer))))
            (should-not (string-prefix-p "*efrit-fake*" (buffer-name target)))))
        ;; From a normal buffer, the target is itself
        (should (eq (efrit-context-target-buffer user) user))))))

(ert-deftest test-ctx-wrap-user-input ()
  (test-ctx--with-buffer "ctx-wrap" "x"
    (let ((efrit-context-sources '(buffer)))
      (let ((wrapped (efrit-context-wrap-user-input "hello" (current-buffer))))
        (should (string-prefix-p "<editor-context>" wrapped))
        (should (string-suffix-p "\n\nhello" wrapped))))
    (let ((efrit-context-sources nil))
      (should (string= (efrit-context-wrap-user-input "hello" (current-buffer))
                       "hello")))))

(ert-deftest test-ctx-repl-session-keeps-plain-text-in-conversation ()
  "Conversation shows what the user typed; api-messages carry the wrapped text."
  (let ((session (efrit-repl-session-create default-directory)))
    (efrit-repl-session-add-user-message session "plain" "<editor-context>..</editor-context>\n\nplain")
    (should (string= (plist-get (car (last (efrit-repl-session-conversation session)))
                                :content)
                     "plain"))
    (should (string-prefix-p "<editor-context>"
                             (alist-get 'content
                                        (car (last (efrit-repl-session-api-messages session))))))))

(provide 'test-context-sources)
;;; test-context-sources.el ends here
