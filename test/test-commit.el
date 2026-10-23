;;; test-commit.el --- efrit-commit-message and the candidates panel -*- lexical-binding: t; -*-
;;; Code:
(require 'ert)
(require 'efrit-commit)
(require 'efrit-candidates)

(ert-deftest test-candidates-panel-picks-raw-text ()
  "The panel returns the raw candidate, not its decorated display; numbers,
n/p and RET work; one candidate needs no panel; q gives nothing."
  (let ((picked nil))
    (cl-letf (((symbol-function 'pop-to-buffer) (lambda (b &rest _) (set-buffer b)))
              ((symbol-function 'run-at-time) (lambda (_s _r fn &rest args) (apply fn args)))
              ((symbol-function 'quit-window) #'ignore))
      (efrit-candidates-choose '("only") (lambda (c) (setq picked c)))
      (should (equal "only" picked))
      (setq picked nil)
      (let ((buf (efrit-candidates-choose '("first line\nmore" "second **bold**" "third")
                                          (lambda (c) (setq picked c)) "t")))
        (with-current-buffer buf
          (should (derived-mode-p 'efrit-candidates-mode))
          (efrit-candidates-next)
          (should (equal (gethash (get-text-property (point) 'efrit-candidate) efrit-candidates--table)
                         "second **bold**"))
          (efrit-candidates-previous)
          (efrit-candidates-pick))
        (should (equal "first line\nmore" picked))
        (setq picked nil)
        (with-current-buffer buf (efrit-candidates-pick-number 3))
        (should (equal "third" picked))
        (setq picked 'untouched)
        (with-current-buffer buf (efrit-candidates-quit))
        (should (eq 'untouched picked))
        (kill-buffer buf)))))

(ert-deftest test-commit-message-inserts-from-the-staged-diff ()
  "The prompt carries the staged diff and the repo's instructions; the
reply lands at point, fence stripped; a prefix asks for candidates."
  (skip-unless (executable-find "git"))
  (let* ((root (file-name-as-directory (make-temp-file "efrit-commit-" t)))
         (sent nil))
    (unwind-protect
        (let ((default-directory root))
          (call-process "git" nil nil nil "init" "-q")
          (call-process "git" nil nil nil "config" "user.email" "t@example.invalid")
          (call-process "git" nil nil nil "config" "user.name" "t")
          (with-temp-file "a.txt" (insert "one\n"))
          (call-process "git" nil nil nil "add" "a.txt")
          (call-process "git" nil nil nil "commit" "-q" "-m" "first")
          (with-temp-file "a.txt" (insert "one\ntwo\n"))
          (make-directory ".github")
          (with-temp-file ".github/git-commit-instructions.md" (insert "Always mention the ticket.\n"))
          ;; nothing staged: refused
          (with-temp-buffer
            (setq default-directory root)
            (should-error (efrit-commit-message) :type 'user-error))
          (call-process "git" nil nil nil "add" "a.txt")
          (cl-letf (((symbol-function 'efrit-ask-once)
                     (lambda (prompt cb &rest _) (setq sent prompt)
                       (funcall cb "```\nfeat(a): add two\n```" nil) nil)))
            (with-temp-buffer
              (setq default-directory root)
              (insert "\n# comments\n")
              (goto-char (point-min))
              (efrit-commit-message)
              (should (string-prefix-p "feat(a): add two\n" (buffer-string)))
              (should (string-match-p "\\+two" sent))
              (should (string-match-p "Always mention the ticket" sent))))
          ;; candidates: the panel gets them
          (let ((shown nil))
            (cl-letf (((symbol-function 'efrit-ask-candidates)
                       (lambda (_prompt n cb &rest _) (should (= n efrit-commit-candidates))
                         (funcall cb '("m1" "m2" "m3") nil) nil))
                      ((symbol-function 'efrit-candidates-choose)
                       (lambda (cands on-choose &rest _) (setq shown cands) (funcall on-choose (cadr cands)))))
              (with-temp-buffer
                (setq default-directory root)
                (efrit-commit-message '(4))
                (should (equal '("m1" "m2" "m3") shown))
                (should (string-prefix-p "m2\n" (buffer-string)))))))
      (delete-directory root t))))

(provide 'test-commit)
;;; test-commit.el ends here
