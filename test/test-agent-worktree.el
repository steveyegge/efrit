;;; test-agent-worktree.el --- An agent in its own worktree; resume honesty -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'efrit-agent)
(require 'efrit-agent-instances)
(require 'efrit-session-persist)
(require 'efrit-vcs)

(defun test-wt--git (&rest args)
  (apply #'call-process "git" nil nil nil args))

(defmacro test-wt--with-repo (root &rest body)
  (declare (indent 1))
  `(let* ((,root (file-name-as-directory (file-truename (make-temp-file "efrit-wt-" t))))
          (efrit-data-directory (file-name-as-directory (make-temp-file "efrit-wt-data-" t)))
          (efrit-agent-auto-show nil))
     (let ((default-directory ,root))
       (test-wt--git "init" "-q")
       (test-wt--git "config" "user.email" "t@example.invalid")
       (test-wt--git "config" "user.name" "t")
       (with-temp-file "a.txt" (insert "one\n"))
       (test-wt--git "add" "a.txt")
       (test-wt--git "commit" "-q" "-m" "first"))
     (unwind-protect (progn ,@body)
       (dolist (b (efrit-agent-buffers))
         (when (string-prefix-p ,root (buffer-local-value 'default-directory b))
           (kill-buffer b)))
       (ignore-errors (delete-directory (concat (directory-file-name ,root) "-fix") t))
       (ignore-errors (delete-directory ,root t))
       (ignore-errors (delete-directory efrit-data-directory t)))))

(ert-deftest test-wt-vcs-worktree-add-refuses-existing ()
  (skip-unless (executable-find "git"))
  (test-wt--with-repo root
    (let ((dest (concat (directory-file-name root) "-fix")))
      (should-error (efrit-vcs-worktree-add "x y" dest root) :type 'efrit-vcs-error)
      (should-error (efrit-vcs-worktree-add (efrit-vcs-branch root) dest root) :type 'efrit-vcs-error)
      (let ((wt (efrit-vcs-worktree-add "agent/fix" dest root)))
        (should (file-directory-p wt))
        (should (equal "agent/fix" (efrit-vcs-branch wt)))
        (should (seq-find (lambda (e) (equal (car e) wt)) (efrit-vcs-worktree-list root)))
        ;; a second time: the directory and the branch both exist
        (should-error (efrit-vcs-worktree-add "agent/other" dest root) :type 'efrit-vcs-error)
        (should-error (efrit-vcs-worktree-add "agent/fix" (concat dest "2") root) :type 'efrit-vcs-error)))))

(ert-deftest test-wt-instance-records-worktree-and-resume-checks-it ()
  "The new buffer's project is the worktree; the session persists
worktree and branch; once the worktree is gone, resume refuses
instead of opening a buffer elsewhere; relocate fixes the record."
  (skip-unless (executable-find "git"))
  (test-wt--with-repo root
    (let* ((dest (concat (directory-file-name root) "-fix"))
           (buf (efrit-agent-new-worktree-instance "fix" "agent/fix" dest root))
           (session (buffer-local-value 'efrit-agent--repl-session buf))
           (id (efrit-repl-session-id session))
           (wt (file-name-as-directory dest)))
      (should (equal wt (buffer-local-value 'default-directory buf)))
      (should (equal wt (efrit-repl-session-meta-get session :worktree)))
      (should (equal "agent/fix" (efrit-repl-session-meta-get session :branch)))
      (should (string-match-p "fix" (buffer-name buf)))
      (efrit-session-persist-save session)
      (kill-buffer buf)
      (let ((loaded (efrit-session-persist-load id)))
        (should (equal wt (efrit-repl-session-meta-get loaded :worktree)))
        (should (equal "agent/fix" (efrit-repl-session-meta-get loaded :branch))))
      ;; the worktree disappears
      (delete-directory dest t)
      (let ((before (length (efrit-agent-buffers))))
        (should-error (efrit-resume-session id) :type 'user-error)
        (should (= before (length (efrit-agent-buffers)))))
      ;; relocate to the main checkout, confirmed
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
        (should (equal root (efrit-agent-relocate-session id root))))
      (let ((loaded (efrit-session-persist-load id)))
        (should (equal root (efrit-repl-session-meta-get loaded :worktree)))
        (should (equal root (efrit-repl-session-project-root loaded)))))))

(ert-deftest test-wt-meta-round-trips-without-meta-key ()
  "A session file from before `meta' existed loads with an empty meta."
  (let* ((s (efrit-repl-session-create temporary-file-directory))
         (data (efrit-session-persist--serialize-session s)))
    (should (hash-table-p (cdr (assoc "meta" data))))
    (efrit-repl-session-meta-put s :folder "Work/Billing")
    (let* ((json (json-encode (efrit-session-persist--serialize-session s)))
           (back (let ((json-object-type 'alist) (json-key-type 'string) (json-array-type 'list))
                   (efrit-session-persist--deserialize-session (json-read-from-string json)))))
      (should (equal "Work/Billing" (efrit-repl-session-meta-get back :folder))))
    (efrit-repl-session-meta-put s :folder nil)
    (should (null (efrit-repl-session-meta s)))))

(ert-deftest test-wt-bound-agent-reports-its-own-project ()
  "The editor-context snapshot of a turn names the session's directory as
the project, not the directory of the buffer the user was looking at."
  (require 'efrit-repl-loop)
  (require 'efrit-context-sources)
  (let* ((agent-dir (file-name-as-directory (make-temp-file "efrit-bound-" t)))
         (other-dir (file-name-as-directory (make-temp-file "efrit-other-" t)))
         (efrit-agent-auto-show nil)
         (efrit-context-sources '(buffer project))
         (seen nil))
    (unwind-protect
        (cl-letf (((symbol-function 'efrit-agent-display) (lambda (b &rest _) b)))
          (let* ((buf (efrit-agent-new-instance agent-dir "bound"))
                 (session (buffer-local-value 'efrit-agent--repl-session buf)))
            (should (equal agent-dir (buffer-local-value 'default-directory buf)))
            (should (equal agent-dir (efrit-repl-session-project-root session)))
            ;; the user's buffer is somewhere else
            (with-temp-buffer
              (setq default-directory other-dir)
              (rename-buffer " *user-elsewhere*" t)
              (let ((user (current-buffer)))
                (cl-letf (((symbol-function 'efrit-context-target-buffer) (lambda (&rest _) user)))
                  (setq seen (efrit-repl-loop--with-session
                              session (lambda () (efrit-context-wrap-user-input "hi")))))))
            (should (string-match-p (concat "Project root: " (regexp-quote (abbreviate-file-name agent-dir))) seen))
            (should-not (string-match-p (concat "Project root: " (regexp-quote (abbreviate-file-name other-dir))) seen))
            (kill-buffer buf)))
      (ignore-errors (delete-directory agent-dir t))
      (ignore-errors (delete-directory other-dir t)))))

(ert-deftest test-wt-set-directory-moves-root-cwd-and-name ()
  "`/cd DIR' (efrit-agent-set-directory) changes the agent's project root,
its default-directory and its buffer name together."
  (require 'efrit-agent-mentions)
  (let* ((a (file-name-as-directory (make-temp-file "efrit-cd-a-" t)))
         (b (file-name-as-directory (make-temp-file "efrit-cd-b-" t)))
         (efrit-agent-auto-show nil))
    (unwind-protect
        (cl-letf (((symbol-function 'efrit-agent-display) (lambda (buf &rest _) buf)))
          (let ((buf (efrit-agent-new-instance a "mover")))
            (with-current-buffer buf
              (should (string-match-p (regexp-quote (file-name-nondirectory (directory-file-name a))) (buffer-name)))
              (efrit-agent-slash-run (format "/cd %s" b))
              (should (equal b default-directory))
              (should (equal b (efrit-repl-session-project-root efrit-agent--repl-session)))
              (should (equal b (plist-get efrit-agent--instance :project)))
              (should (string-match-p (regexp-quote (file-name-nondirectory (directory-file-name b))) (buffer-name)))
              (should (string-match-p "mover" (buffer-name)))
              (should-error (efrit-agent-set-directory "/nonexistent/efrit-cd") :type 'user-error))
            (kill-buffer buf)))
      (ignore-errors (delete-directory a t))
      (ignore-errors (delete-directory b t)))))

(provide 'test-agent-worktree)

;;; test-agent-worktree.el ends here
