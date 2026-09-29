;;; test-vcs.el --- efrit-vcs: version control through VC -*- lexical-binding: t; -*-

;;; Commentary:
;; A temp Git repository per test; no `git' subprocess in the code under
;; test (the tests use git only to build the fixture).

;;; Code:

(require 'ert)
(require 'efrit-vcs)

(defmacro test-vcs--with-repo (&rest body)
  "Run BODY with ROOT bound to a fresh repository with one commit and
uncommitted changes (a.txt edited, new.txt untracked)."
  (declare (indent 0))
  `(let* ((root (file-name-as-directory (make-temp-file "efrit-vcs-" t))))
     (skip-unless (executable-find "git"))
     (unwind-protect
         (let ((default-directory root))
           (call-process "git" nil nil nil "init" "-q")
           (call-process "git" nil nil nil "config" "user.email" "t@example.invalid")
           (call-process "git" nil nil nil "config" "user.name" "t")
           (with-temp-file "a.txt" (insert "one\ntwo\n"))
           (make-directory "sub")
           (with-temp-file "sub/b.txt" (insert "b\n"))
           (call-process "git" nil nil nil "add" ".")
           (call-process "git" nil nil nil "commit" "-q" "-m" "first")
           (with-temp-file "a.txt" (insert "one\nTWO\n"))
           (with-temp-file "new.txt" (insert "n\n"))
           ,@body)
       (delete-directory root t))))

(ert-deftest test-vcs-repo-facts ()
  (test-vcs--with-repo
    (should (eq 'Git (efrit-vcs-backend root)))
    (should (equal (file-truename root) (efrit-vcs-root root)))
    (should (equal (file-truename root) (efrit-vcs-root (expand-file-name "sub" root))))
    (should (member (efrit-vcs-branch root) '("master" "main")))
    (should (= 40 (length (efrit-vcs-working-revision root))))
    (should-not (efrit-vcs-special-state root))
    (should-not (efrit-vcs-upstream root))
    (should-error (efrit-vcs-require temporary-file-directory) :type 'efrit-vcs-error)))

(ert-deftest test-vcs-status-diff-log-annotate-files ()
  (test-vcs--with-repo
    (let ((status (efrit-vcs-status-files root)))
      (should (equal '(("a.txt" edited) ("new.txt" unregistered))
                     (sort status (lambda (x y) (string< (car x) (car y)))))))
    (let ((diff (efrit-vcs-diff nil nil nil root)))
      (should (string-match-p "^-two$" diff))
      (should (string-match-p "^\\+TWO$" diff)))
    (should (string-empty-p (efrit-vcs-diff-staged nil root)))
    (should (string-match-p "first" (efrit-vcs-log nil 5 nil root)))
    (let ((blame (efrit-vcs-annotate (expand-file-name "a.txt" root) nil root)))
      ;; full ids and ISO dates were asked for
      (should (string-match-p "\\`[0-9a-f]\\{40\\} " blame))
      (should (string-match-p "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T" blame))
      (should (string-match-p "Not Committed Yet" blame)))
    (should (equal '("a.txt" "new.txt" "sub/b.txt") (sort (efrit-vcs-files root) #'string<)))))

(ert-deftest test-vcs-stash-named-and-round-trips ()
  "A checkpoint stash carries the efrit prefix, the id and the
description; apply with pop restores the tree and removes it."
  (test-vcs--with-repo
    ;; a buffer visits the changed file, as in the user's Emacs: after
    ;; the push it must show the clean text, or its next save would
    ;; make the pop fail
    (let* ((visiting (find-file-noselect (expand-file-name "a.txt" root)))
           (ref (efrit-vcs-stash-push "efrit-20260928-abc123" "before the risky edit" root)))
      (should (equal "stash@{0}" ref))
      (should (equal "one\ntwo\n" (with-current-buffer visiting (buffer-string))))
      (should-not (buffer-modified-p visiting))
      (let ((entry (car (efrit-vcs-stash-list root))))
        (should (string-match-p "efrit-checkpoint efrit-20260928-abc123: before the risky edit" (cdr entry))))
      ;; tracked changes stashed; the untracked file stays where it is
      (should (equal '(("new.txt" unregistered)) (efrit-vcs-status-files root)))
      (should (equal ref (efrit-vcs-stash-find "efrit-20260928-abc123" root)))
      ;; a save hook writes the old buffer text back before the pop
      ;; (what the user's Emacs did): the pop must still succeed
      (with-current-buffer visiting
        (goto-char (point-max)) (insert "TWO-again\n") (save-buffer))
      (should (member '("a.txt" edited) (efrit-vcs-status-files root)))
      (efrit-vcs-stash-apply "efrit-20260928-abc123" t root)
      (should (equal "one\nTWO\n" (with-temp-buffer (insert-file-contents (expand-file-name "a.txt" root)) (buffer-string))))
      (should (equal "one\nTWO\n" (with-current-buffer visiting (buffer-string))))
      (kill-buffer visiting)
      (should (= 2 (length (efrit-vcs-status-files root))))
      (should-not (efrit-vcs-stash-list root))
      ;; nothing to stash now that... there is: push again, then drop
      (efrit-vcs-stash-push "efrit-2" "x" root)
      (should (efrit-vcs-stash-drop "efrit-2" root))
      (should-not (efrit-vcs-stash-drop "efrit-2" root))
      ;; a clean tree cannot be checkpointed
      (should-error (efrit-vcs-stash-push "efrit-3" "y" root) :type 'efrit-vcs-error))))

(ert-deftest test-vcs-snapshot-fallback ()
  "Without Git the checkpoint is a file snapshot that restores."
  (let ((root (file-name-as-directory (make-temp-file "efrit-snap-" t))))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "f.txt" root) (insert "v1\n"))
          (should-not (efrit-vcs-git-p root))
          (should-error (efrit-vcs-stash-push "id" "d" root) :type 'efrit-vcs-error)
          ;; not a project either: every file is snapshotted
          (cl-letf (((symbol-function 'efrit-vcs-files) (lambda (&optional _) '("f.txt"))))
            (should (= 1 (efrit-vcs-snapshot-create "id1" root))))
          (with-temp-file (expand-file-name "f.txt" root) (insert "v2\n"))
          (should (= 1 (efrit-vcs-snapshot-restore "id1" root)))
          (should (equal "v1\n" (with-temp-buffer (insert-file-contents (expand-file-name "f.txt" root)) (buffer-string))))
          (should (efrit-vcs-snapshot-delete "id1" root))
          (should-error (efrit-vcs-snapshot-restore "id1" root) :type 'efrit-vcs-error))
      (delete-directory root t))))

(ert-deftest test-vcs-diff-strings ()
  (let ((d (efrit-vcs-diff-strings "a\nb\n" "a\nc\n" "a/x" "b/x")))
    (should (string-match-p "^--- a/x" d))
    (should (string-match-p "^-b$" d))
    (should (string-match-p "^\\+c$" d))
    (should-not (string-match-p "Diff finished" d)))
  (should (string-empty-p (efrit-vcs-diff-strings "same\n" "same\n"))))

(provide 'test-vcs)
;;; test-vcs.el ends here
