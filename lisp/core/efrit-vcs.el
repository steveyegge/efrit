;;; efrit-vcs.el --- Version control through VC, never a git subprocess -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.9.2
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, vc

;;; Commentary:

;; Everything efrit needs from version control, in one place, done
;; through Emacs's VC layer (and `project' for file lists).  No caller
;; runs `git' itself: VC already knows how to talk to the backend, on
;; the local host or over TRAMP, with the user's settings
;; (`vc-git-program', switches, coding systems), and it keeps VC's own
;; state (`vc-dir', mode lines) in sync after a stash.  Reinventing
;; that with `call-process "git"' was wrong (tzz, 2026-09-28).
;;
;; The API is backend-neutral where VC is: root, state, files, diff,
;; log, annotate.  Stashes are Git-only; `efrit-vcs-stash-*' fall back
;; to a file snapshot under .efrit/checkpoints/ elsewhere, so
;; `checkpoint' works on any tree.
;;
;; Asynchronous VC operations (log, annotate) are awaited here, so
;; callers get strings.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'vc)
(require 'vc-dir)
(require 'project)
(require 'diff)
(require 'efrit-log)

(declare-function vc-git-root "vc-git")
(declare-function vc-git-stash "vc-git")
(declare-function vc-git-stash-apply "vc-git")
(declare-function vc-git-stash-pop "vc-git")
(declare-function vc-git-stash-delete "vc-git")
(declare-function vc-git--run-command-string "vc-git")
(declare-function vc-git--symbolic-ref "vc-git")
(declare-function vc-git--git-path "vc-git")
(declare-function vc-git-log-outgoing "vc-git")
(declare-function vc-git-log-incoming "vc-git")
(declare-function magit-status-setup-buffer "magit-status")
(declare-function magit-diff-working-tree "magit-diff")
(declare-function magit-diff-staged "magit-diff")

(defgroup efrit-vcs nil
  "Version control access for efrit's tools."
  :group 'efrit)

(defcustom efrit-vcs-async-timeout 45
  "Seconds to wait for an asynchronous VC command (log, annotate)."
  :type 'integer
  :group 'efrit-vcs)

(define-error 'efrit-vcs-error "Version control error")

;;;; Repository

(defun efrit-vcs-backend (&optional dir)
  "The VC backend responsible for DIR (default `default-directory'), or nil."
  (let ((dir (or dir default-directory)))
    (and (file-directory-p dir)
         (ignore-errors (vc-responsible-backend dir)))))

(defun efrit-vcs-root (&optional dir)
  "The work tree root DIR is in, as a directory name, or nil."
  (let* ((dir (or dir default-directory))
         (backend (efrit-vcs-backend dir))
         (root (and backend (ignore-errors (vc-call-backend backend 'root dir)))))
    (and root (file-name-as-directory (expand-file-name root)))))

(defun efrit-vcs-git-p (&optional dir)
  "Non-nil when DIR is in a Git work tree."
  (eq (efrit-vcs-backend dir) 'Git))

(defun efrit-vcs-require (&optional dir)
  "The (BACKEND . ROOT) of DIR, or signal `efrit-vcs-error'."
  (let* ((dir (or dir default-directory))
         (backend (efrit-vcs-backend dir))
         (root (and backend (efrit-vcs-root dir))))
    (unless root
      (signal 'efrit-vcs-error
              (list (format "%s is not inside a version-controlled tree"
                            (abbreviate-file-name dir)))))
    (cons backend root)))

;;;; Status

(defun efrit-vcs-status-files (&optional dir)
  "Files with a state of interest under DIR: a list of (FILE STATE).
STATE is a VC state symbol: edited, added, removed, unregistered,
conflict, missing, ...  FILE is relative to the root."
  (pcase-let ((`(,backend . ,root) (efrit-vcs-require dir)))
    (let ((default-directory root))
      (mapcar (lambda (entry)
                (list (file-relative-name (expand-file-name (car entry) root) root)
                      (nth 1 entry)))
              (vc-dir-status-files root nil backend)))))

(defun efrit-vcs-upstream (&optional dir)
  "The upstream branch (\"origin/main\") of DIR's current branch, or nil.
Git only, read from the repository configuration, no subprocess."
  (when-let* ((_ (efrit-vcs-git-p dir))
              (branch (efrit-vcs-branch dir)))
    (require 'vc-git)
    (let* ((default-directory (efrit-vcs-root dir))
           (config (vc-git--git-path "config")))
      (when (file-readable-p config)
        (with-temp-buffer
          (insert-file-contents config)
          (goto-char (point-min))
          (when (re-search-forward (format "^\\[branch \"%s\"\\]" (regexp-quote branch)) nil t)
            (let ((end (save-excursion (or (re-search-forward "^\\[" nil t) (point-max))))
                  remote merge)
              (save-excursion
                (when (re-search-forward "^\\s-*remote\\s-*=\\s-*\\(.+\\)$" end t)
                  (setq remote (string-trim (match-string 1)))))
              (save-excursion
                (when (re-search-forward "^\\s-*merge\\s-*=\\s-*refs/heads/\\(.+\\)$" end t)
                  (setq merge (string-trim (match-string 1)))))
              (and remote merge (format "%s/%s" remote merge)))))))))

(defun efrit-vcs-branch (&optional dir)
  "The current branch name of DIR's tree, or nil (detached, or not Git)."
  (pcase-let ((`(,backend . ,root) (efrit-vcs-require dir)))
    (let ((default-directory root))
      (pcase backend
        ('Git (require 'vc-git)
              (let ((ref (vc-git--symbolic-ref root)))
                (and ref (string-remove-prefix "refs/heads/" ref))))
        (_ (ignore-errors (vc-call-backend backend 'working-revision root)))))))

(defun efrit-vcs-working-revision (&optional dir)
  "The working revision (commit id) of DIR's tree, or nil."
  (pcase-let ((`(,backend . ,root) (efrit-vcs-require dir)))
    (let ((default-directory root))
      (ignore-errors (vc-call-backend backend 'working-revision root)))))

(defun efrit-vcs--count-log-lines (fn)
  "Lines VC's FN (a log-outgoing/incoming function) writes for the default upstream."
  (with-temp-buffer
    (condition-case nil
        (progn
          (funcall fn (current-buffer) nil)
          (efrit-vcs--await-buffer (current-buffer))
          (count-lines (point-min) (point-max)))
      (error nil))))

(defun efrit-vcs-ahead-behind (&optional dir)
  "(AHEAD . BEHIND) commit counts against the upstream, or nil when unknown.
Git only; uses VC's outgoing/incoming logs."
  (when (efrit-vcs-git-p dir)
    (require 'vc-git)
    (let ((default-directory (efrit-vcs-root dir)))
      (let ((ahead (efrit-vcs--count-log-lines #'vc-git-log-outgoing))
            (behind (efrit-vcs--count-log-lines #'vc-git-log-incoming)))
        (and ahead behind (cons ahead behind))))))

(defun efrit-vcs-special-state (&optional dir)
  "Which multi-step Git operation is in progress in DIR's tree, or nil.
One of `rebasing', `merging', `cherry-picking', `reverting', `bisecting'."
  (when (efrit-vcs-git-p dir)
    (require 'vc-git)
    (let ((default-directory (efrit-vcs-root dir)))
      (cl-loop for (marker . state) in '(("rebase-merge" . rebasing) ("rebase-apply" . rebasing)
                                          ("MERGE_HEAD" . merging) ("CHERRY_PICK_HEAD" . cherry-picking)
                                          ("REVERT_HEAD" . reverting) ("BISECT_LOG" . bisecting))
               when (file-exists-p (vc-git--git-path marker))
               return state))))

;;;; Diff, log, annotate

(defun efrit-vcs-diff (&optional files rev1 rev2 dir)
  "The unified diff of FILES (default the whole tree) in DIR's tree.
With REV1 only: that revision against the work tree.  With both: REV1
against REV2.  Neither: uncommitted changes.  Returns a string."
  (pcase-let ((`(,backend . ,root) (efrit-vcs-require dir)))
    (let ((default-directory root)
          (files (or (mapcar #'expand-file-name files) (list root))))
      (with-temp-buffer
        (let ((status (vc-call-backend backend 'diff files rev1 rev2 (current-buffer) nil)))
          (ignore status)
          (efrit-vcs--await-buffer (current-buffer))
          (buffer-string))))))

(defun efrit-vcs-diff-staged (&optional files dir)
  "The diff of the index against HEAD (Git), as a string."
  (pcase-let ((`(,backend . ,root) (efrit-vcs-require dir)))
    (unless (eq backend 'Git)
      (signal 'efrit-vcs-error (list "staged changes exist only in Git")))
    (require 'vc-git)
    (let ((default-directory root))
      (or (apply #'vc-git--run-command-string nil "diff" "--cached" "--"
                 (mapcar (lambda (f) (file-relative-name (expand-file-name f) root))
                         (or files (list root))))
          ""))))

(defun efrit-vcs-log (&optional files limit start-revision dir)
  "The log of FILES (default the tree) in DIR's tree, newest first, as a string.
LIMIT caps the number of entries; START-REVISION is the newest shown."
  (pcase-let ((`(,backend . ,root) (efrit-vcs-require dir)))
    (let ((default-directory root)
          (files (or (mapcar #'expand-file-name files) (list root)))
          (buf (generate-new-buffer " *efrit-vcs-log*")))
      (unwind-protect
          (progn
            (vc-call-backend backend 'print-log files buf nil start-revision limit)
            (efrit-vcs--await-buffer buf)
            (with-current-buffer buf (buffer-string)))
        (kill-buffer buf)))))

(defvar vc-git-annotate-switches)

(defun efrit-vcs-annotate (file &optional rev dir)
  "The annotate (blame) output of FILE at REV, as a string.
For Git the full commit id and the ISO date are requested (VC's
default abbreviates both) so callers can report them."
  (pcase-let ((`(,backend . ,root) (efrit-vcs-require (or dir (file-name-directory (expand-file-name file))))))
    (let ((default-directory root)
          (vc-git-annotate-switches (if (eq backend 'Git)
                                        '("-l" "--root" "--date=iso-strict")
                                      vc-git-annotate-switches))
          (buf (generate-new-buffer " *efrit-vcs-annotate*")))
      (unwind-protect
          (progn
            (vc-call-backend backend 'annotate-command (expand-file-name file) buf rev)
            (efrit-vcs--await-buffer buf)
            (with-current-buffer buf (buffer-string)))
        (kill-buffer buf)))))

(defun efrit-vcs--await-buffer (buffer)
  "Wait for the VC process writing BUFFER to finish, up to the timeout."
  (let ((deadline (+ (float-time) efrit-vcs-async-timeout)))
    (while (and (< (float-time) deadline)
                (let ((proc (get-buffer-process buffer)))
                  (and proc (process-live-p proc))))
      (accept-process-output nil 0.05))
    (when-let* ((proc (get-buffer-process buffer)))
      (when (process-live-p proc)
        (delete-process proc)
        (signal 'efrit-vcs-error
                (list (format "version control command timed out after %ds"
                              efrit-vcs-async-timeout)))))))

;;;; Files

(defun efrit-vcs-files (&optional dir)
  "The tracked (and untracked, not ignored) files of the project at DIR.
Relative to the project root.  Through `project-files', so any project
backend works; nil when DIR is not in a project."
  (when-let* ((project (project-current nil (or dir default-directory))))
    (let ((root (project-root project)))
      (mapcar (lambda (f) (file-relative-name f root))
              (project-files project)))))

;;;; Stashes (checkpoints)
;;
;; Git: real stashes through `vc-git-stash', named so the user can see
;; in `git stash list' where they came from.  Anywhere else: a snapshot
;; of the changed files under .efrit/checkpoints/ID/.

(defconst efrit-vcs-stash-prefix "efrit-checkpoint"
  "Every stash efrit creates is named `efrit-checkpoint ID: DESCRIPTION'.")

(defun efrit-vcs-stash-name (id description)
  "The stash message for checkpoint ID with DESCRIPTION."
  (format "%s %s: %s" efrit-vcs-stash-prefix id description))

(defun efrit-vcs-stash-list (&optional dir)
  "Git stashes of DIR's tree, as (REF . MESSAGE), newest first; nil if not Git."
  (when (efrit-vcs-git-p dir)
    (require 'vc-git)
    (let ((default-directory (efrit-vcs-root dir)))
      (mapcar (lambda (line)
                (if (string-match "\\`\\(stash@{[0-9]+}\\): \\(.*\\)\\'" line)
                    (cons (match-string 1 line) (match-string 2 line))
                  (cons line "")))
              (split-string (or (vc-git--run-command-string nil "stash" "list") "") "\n" t)))))

(defun efrit-vcs-stash-find (id &optional dir)
  "The stash ref whose message carries checkpoint ID, or nil."
  (car (cl-find-if (lambda (e) (string-match-p (regexp-quote id) (cdr e)))
                   (efrit-vcs-stash-list dir))))

(defun efrit-vcs-stash-push (id description &optional dir)
  "Stash the tree's uncommitted changes as checkpoint ID.
Returns the stash ref.  Signals `efrit-vcs-error' when there is
nothing to stash or the backend is not Git."
  (pcase-let ((`(,backend . ,root) (efrit-vcs-require dir)))
    (unless (eq backend 'Git)
      (signal 'efrit-vcs-error (list "stashes need Git")))
    (unless (cl-remove-if (lambda (e) (memq (nth 1 e) '(unregistered ignored)))
                          (efrit-vcs-status-files root))
      (signal 'efrit-vcs-error (list "nothing to checkpoint: no tracked file has changed")))
    (require 'vc-git)
    (let ((default-directory root)
          (name (efrit-vcs-stash-name id description)))
      ;; vc-git-stash deduces the fileset from the current buffer; from
      ;; a non-file buffer that is the whole tree, which is what a
      ;; checkpoint means.  Untracked files are included so a restore
      ;; brings back everything the model may have created.
      ;; Tracked changes only, as `vc-git-stash' does.  With
      ;; --include-untracked the pop refuses whenever an untracked
      ;; file it would recreate exists again, and the user's Emacs
      ;; (backups, save hooks, dired refreshes) recreates them between
      ;; push and pop (2026-09-28 testdrive: red.png, greet.el~).  New
      ;; files the model made stay on disk; the checkpoint protects
      ;; what a bad edit would break, the tracked files.
      (with-temp-buffer
        (setq default-directory root)
        (vc-git-command nil 0 nil "stash" "push" "-m" name))
      ;; What `vc-git-stash' does after its push: buffers visiting the
      ;; stashed files still show the change while the disk is clean;
      ;; the first save (the user's, or an auto-save mode) would write
      ;; it back and the later pop would refuse (2026-09-28, testdrive).
      ;; Not `vc-resynch-buffer' on the root: it matches visited names
      ;; by string prefix, and on macOS the root is /private/var/...
      ;; while buffers visit /var/..., so nothing was resynched.
      (efrit-vcs--resynch-tree root)
      (or (efrit-vcs-stash-find id root)
          (signal 'efrit-vcs-error (list "the stash was not created"))))))

(defun efrit-vcs--resynch-tree (root)
  "Revert every unmodified buffer visiting a file under ROOT, by truename.
After a stash push or pop the disk changed under them."
  (let ((root (file-truename root)))
    (dolist (buffer (buffer-list))
      (when-let* ((file (buffer-file-name buffer)))
        (when (and (string-prefix-p root (file-truename file))
                   (not (buffer-modified-p buffer)))
          (with-current-buffer buffer
            (vc-resynch-buffer buffer-file-name t t)))))))

(defun efrit-vcs-stash-apply (id &optional pop dir)
  "Apply the stash of checkpoint ID; with POP, drop it afterwards."
  (let* ((root (efrit-vcs-root dir))
         (ref (or (efrit-vcs-stash-find id root)
                  (signal 'efrit-vcs-error (list (format "no stash for checkpoint %s" id))))))
    (require 'vc-git)
    (let ((default-directory root))
      ;; Buffers that still hold the pre-stash text (a save hook wrote
      ;; them back) make the tree dirty on the stashed paths and git
      ;; refuses.  Those files' content is the stash's own, so restore
      ;; the index/work tree from HEAD for the stashed paths first,
      ;; then pop.  Anything the user changed since is not touched:
      ;; only paths in the stash are reset.
      (let ((dirty (cl-intersection (efrit-vcs--stash-paths ref root)
                                    (mapcar #'car (efrit-vcs-status-files root))
                                    :test #'equal)))
        (when dirty
          (efrit-log 'info "vcs: resetting %S before applying %s (a save wrote them back)" dirty ref)
          (apply #'vc-git-command nil 0 nil "checkout" "--" dirty)))
      (condition-case err
          (if pop (vc-git-stash-pop ref) (vc-git-stash-apply ref))
        (error
         (signal 'efrit-vcs-error
                 (list (format "%s (%s; dirty: %S)"
                               (if pop "stash pop failed" "stash apply failed")
                               (error-message-string err)
                               (mapcar #'car (efrit-vcs-status-files root)))))))
      ;; vc-git's own resynch has the same prefix problem
      (efrit-vcs--resynch-tree root))
    ref))

(defun efrit-vcs--stash-paths (ref root)
  "The tracked paths stash REF touches, relative to ROOT."
  (let ((default-directory root))
    (split-string (or (vc-git--run-command-string nil "stash" "show" "--name-only" ref) "")
                  "\n" t)))

(defun efrit-vcs-stash-drop (id &optional dir)
  "Drop the stash of checkpoint ID.  Returns the ref, or nil if none."
  (when-let* ((root (efrit-vcs-root dir))
              (ref (efrit-vcs-stash-find id root)))
    (require 'vc-git)
    (let ((default-directory root))
      (vc-git-stash-delete ref))
    ref))

;;;; File snapshot fallback (no Git)

(defun efrit-vcs-snapshot-directory (id root)
  "Where checkpoint ID's snapshot of ROOT lives."
  (expand-file-name (concat ".efrit/checkpoints/" id "/") root))

(defun efrit-vcs-snapshot-create (id root)
  "Copy ROOT's changed files (per VC state, else every file) under the snapshot dir.
Returns the number of files copied."
  (let* ((dir (efrit-vcs-snapshot-directory id root))
         (files (condition-case nil
                    (mapcar #'car (efrit-vcs-status-files root))
                  (efrit-vcs-error (efrit-vcs-files root))))
         (count 0))
    (make-directory dir t)
    (dolist (rel files)
      (let ((src (expand-file-name rel root)))
        (when (file-regular-p src)
          (let ((dst (expand-file-name rel dir)))
            (make-directory (file-name-directory dst) t)
            (copy-file src dst t t)
            (cl-incf count)))))
    (with-temp-file (expand-file-name "MANIFEST" dir)
      (insert (mapconcat #'identity files "\n") "\n"))
    count))

(defun efrit-vcs-snapshot-restore (id root)
  "Copy checkpoint ID's snapshot back over ROOT.  Returns the file count."
  (let* ((dir (efrit-vcs-snapshot-directory id root))
         (manifest (expand-file-name "MANIFEST" dir)))
    (unless (file-exists-p manifest)
      (signal 'efrit-vcs-error (list (format "no snapshot for checkpoint %s" id))))
    (let ((files (with-temp-buffer (insert-file-contents manifest)
                                   (split-string (buffer-string) "\n" t)))
          (count 0))
      (dolist (rel files)
        (let ((src (expand-file-name rel dir)))
          (when (file-regular-p src)
            (let ((dst (expand-file-name rel root)))
              (make-directory (file-name-directory dst) t)
              (copy-file src dst t t)
              (cl-incf count)))))
      count)))

(defun efrit-vcs-snapshot-delete (id root)
  "Remove checkpoint ID's snapshot."
  (let ((dir (efrit-vcs-snapshot-directory id root)))
    (when (file-directory-p dir)
      (delete-directory dir t)
      t)))

;;;; Plain diffs between two texts (no repository)

(defun efrit-vcs-diff-strings (old new &optional old-label new-label)
  "The unified diff of OLD against NEW (strings), via the `diff' library.
OLD-LABEL and NEW-LABEL name the sides."
  (let ((old-file (make-temp-file "efrit-old-"))
        (new-file (make-temp-file "efrit-new-")))
    (unwind-protect
        (progn
          (with-temp-file old-file (insert old))
          (with-temp-file new-file (insert new))
          ;; No --label switches of our own: `diff-no-select' splices
          ;; switches into the shell line unquoted, and when
          ;; `diff-use-labels' is t it adds its own --label pair, so a
          ;; diff that accepts two labels took ours as file names
          ;; ("diff: notes.txt: No such file", 2026-09-28 live run).
          ;; The library labels with the temp names; we rewrite the
          ;; header lines to the labels asked for.
          (let ((diff-use-labels nil))
            (with-current-buffer (diff-no-select old-file new-file "-u" t
                                                 (generate-new-buffer " *efrit-vcs-diff*"))
              (efrit-vcs--await-buffer (current-buffer))
              (prog1 (efrit-vcs--relabel
                      (efrit-vcs--strip-diff-chrome (buffer-string))
                      (or old-label "a") (or new-label "b"))
                (kill-buffer)))))
      (ignore-errors (delete-file old-file))
      (ignore-errors (delete-file new-file)))))

(defun efrit-vcs--relabel (text old-label new-label)
  "TEXT with its ---/+++ header lines naming OLD-LABEL and NEW-LABEL."
  (let ((lines (split-string text "\n")) (done-old nil) (done-new nil))
    (mapconcat (lambda (l)
                 (cond
                  ((and (not done-old) (string-prefix-p "--- " l))
                   (setq done-old t) (concat "--- " old-label))
                  ((and (not done-new) (string-prefix-p "+++ " l))
                   (setq done-new t) (concat "+++ " new-label))
                  (t l)))
               lines "\n")))

(defun efrit-vcs--strip-diff-chrome (text)
  "TEXT without the command line `diff-no-select' writes first and its trailer."
  (let ((lines (split-string text "\n")))
    (setq lines (cl-remove-if (lambda (l) (or (string-prefix-p "diff -u" l)
                                              (string-prefix-p "Diff finished" l)))
                              lines))
    (string-trim-right (mapconcat #'identity lines "\n"))))

;;;; Showing the user

(defun efrit-vcs-show-status (&optional dir)
  "Show DIR's tree to the user: Magit when loaded, else `vc-dir'."
  (interactive)
  (let ((root (efrit-vcs-root dir)))
    (unless root (user-error "Not in a version-controlled tree"))
    (if (fboundp 'magit-status-setup-buffer)
        (magit-status-setup-buffer root)
      (vc-dir root))))

(defun efrit-vcs-show-diff (&optional staged dir)
  "Show the uncommitted diff of DIR's tree: Magit when loaded, else `vc-diff'."
  (interactive "P")
  (let ((root (efrit-vcs-root dir)))
    (unless root (user-error "Not in a version-controlled tree"))
    (let ((default-directory root))
      (cond
       ((and staged (fboundp 'magit-diff-staged)) (magit-diff-staged))
       ((fboundp 'magit-diff-working-tree) (magit-diff-working-tree))
       (t (vc-root-diff nil))))))

(provide 'efrit-vcs)

;;; efrit-vcs.el ends here
