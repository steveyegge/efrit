;;; test-scope.el --- efrit-scope: prompts over region/defun/buffer -*- lexical-binding: t; -*-
;;; Code:
(require 'ert)
(require 'efrit-scope)

(ert-deftest test-scope-fill-placeholders ()
  (should (equal "a X b Y c {{{:zzz}}}"
                 (efrit-scope-fill "a {{{:one}}} b {{{:two}}} c {{{:zzz}}}"
                                   `((one . "X") (two . ,(lambda () "Y"))))))
  (let ((v "sym")) (ignore v))
  (should (equal "emacs" (efrit-scope-fill "{{{:m}}}" '((m . "emacs"))))))

(ert-deftest test-scope-bounds-region-defun-buffer ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun a () 1)\n\n(defun b ()\n  2)\n")
    ;; defun at point in prog-mode
    (goto-char (point-max)) (forward-line -1)
    (should (eq 'defun (nth 2 (efrit-scope-bounds))))
    (should (string-match-p "defun b" (buffer-substring (nth 0 (efrit-scope-bounds)) (nth 1 (efrit-scope-bounds)))))
    ;; region wins
    (set-mark 1) (goto-char 5) (activate-mark)
    (let ((transient-mark-mode t))
      (should (equal '(1 5 region) (efrit-scope-bounds))))
    (deactivate-mark))
  ;; prose: never a "defun", the buffer
  (with-temp-buffer
    (text-mode)
    (insert "Some words.\n\nMore words.\n")
    (should (eq 'buffer (nth 2 (efrit-scope-bounds))))))

(ert-deftest test-scope-run-submits-filled-prompt ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun greet (n) (format \"hi %s\" n))\n")
    (goto-char 5)
    (let ((sent nil))
      (cl-letf (((symbol-function 'efrit-submit) (lambda (shown api) (setq sent (list shown api)) t)))
        (efrit-scope-run (efrit-prompts-pair "explain"))
        (should (string-match-p "\\`explain: defun 1-1 of" (car sent)))
        (should (string-match-p "Explain what this defun" (cadr sent)))
        (should (string-match-p "emacs-lisp" (cadr sent)))
        ;; the text is appended in a fence since the prompt does not place it
        (should (string-match-p "```emacs-lisp\n(defun greet" (cadr sent)))
        ;; a free question works too
        (efrit-scope-run "why is this slow?")
        (should (string-match-p "\\`why is this slow\\?: defun" (car sent)))
        (should (string-match-p "(defun greet" (cadr sent)))))))

(ert-deftest test-scope-builtins-are-single-prompts ()
  (dolist (name '("explain" "fix" "document" "tests" "review" "simplify"))
    (let ((p (efrit-prompts-get name)))
      (should p)
      (should (eq 'single (efrit-prompts-kind p)))
      (should (string-match-p "{{{:scope}}}" (plist-get p :item))))))

(provide 'test-scope)
;;; test-scope.el ends here
