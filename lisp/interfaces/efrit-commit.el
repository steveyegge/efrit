;;; efrit-commit.el --- A commit message from the staged diff -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.8.5
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, vc, ai

;;; Commentary:

;; `efrit-commit-message': in a commit message buffer (Magit's
;; COMMIT_EDITMSG, `vc-log-edit', or any buffer), read the staged
;; diff through VC, ask the model for a message in the repository's
;; conventions, and insert it at point.  With a prefix argument, ask
;; for several and pick one.  A side request; the agent buffer is not
;; involved.  The repository's own instructions file
;; (`efrit-commit-instructions-files') is read and obeyed when present.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'efrit-ask)
(require 'efrit-vcs)
(require 'efrit-candidates)

(defgroup efrit-commit nil
  "Commit messages from the model."
  :group 'efrit)

(defcustom efrit-commit-prompt
  "Write the commit message for this staged diff.

Format: a conventional commit, `<type>(<scope>): <subject>` where type is one of feat, fix, docs, style, refactor, test, chore, perf.  Subject in the imperative mood, no trailing period, at most 60 characters.  For a small change the subject line alone.  For a larger change add a blank line and a body of full sentences, wrapped at 72 columns, saying what changed and why, not how.  No markdown, no bullet decorations, no quotes around the message."
  "What the model is asked, before the diff."
  :type 'string
  :group 'efrit-commit)

(defcustom efrit-commit-instructions-files
  '(".github/git-commit-instructions.md" ".git-commit-instructions.md" "COMMIT_CONVENTIONS.md")
  "Files under the repository root read as the house rules for messages."
  :type '(repeat string)
  :group 'efrit-commit)

(defcustom efrit-commit-max-diff-chars 60000
  "Longest staged diff sent in full; longer ones are cut with a note."
  :type 'integer
  :group 'efrit-commit)

(defcustom efrit-commit-candidates 3
  "How many messages to ask for with a prefix argument."
  :type 'integer
  :group 'efrit-commit)

(defun efrit-commit--instructions (root)
  "The repository's commit instructions text, or nil."
  (cl-loop for rel in efrit-commit-instructions-files
           for file = (expand-file-name rel root)
           when (file-readable-p file)
           return (with-temp-buffer (insert-file-contents file) (buffer-string))))

(defun efrit-commit--prompt (diff instructions)
  (concat efrit-commit-prompt
          (when instructions
            (format "\n\nThis repository's own rules, which take precedence:\n\n%s" instructions))
          "\n\nThe staged diff:\n\n"
          (efrit-fence-for diff) "\n" diff "\n" (efrit-fence-for diff)))

(declare-function efrit-fence-for "efrit-ui-helpers")

(defun efrit-commit--diff (root)
  "The staged diff of ROOT, cut to `efrit-commit-max-diff-chars'."
  (let ((diff (efrit-vcs-diff-staged nil root)))
    (when (string-empty-p (string-trim diff))
      (user-error "Nothing is staged in %s" (abbreviate-file-name root)))
    (if (> (length diff) efrit-commit-max-diff-chars)
        (concat (substring diff 0 efrit-commit-max-diff-chars)
                (format "\n[… %d more characters of diff omitted]" (- (length diff) efrit-commit-max-diff-chars)))
      diff)))

(defun efrit-commit--insert (message)
  "Insert MESSAGE at point in the current buffer, on its own lines."
  (unless (bolp) (insert "\n"))
  (let ((start (point)))
    (insert (string-trim-right (efrit-ask-strip-fence message)) "\n")
    (goto-char start)))

;;;###autoload
(defun efrit-commit-message (&optional arg)
  "Insert a commit message for the staged changes at point.
With prefix ARG, ask for `efrit-commit-candidates' messages and pick one.
The repository is the one of `default-directory' (a commit buffer's is
its work tree)."
  (interactive "P")
  (require 'efrit-ui-helpers)
  (let* ((root (or (efrit-vcs-root default-directory)
                   (user-error "Not in a version-controlled tree")))
         (diff (efrit-commit--diff root))
         (prompt (efrit-commit--prompt diff (efrit-commit--instructions root)))
         (buffer (current-buffer))
         (at (point-marker))
         (insert-there (lambda (text)
                         (when (buffer-live-p buffer)
                           (with-current-buffer buffer
                             (save-excursion
                               (goto-char at)
                               (efrit-commit--insert text))
                             (goto-char at))))))
    (message "efrit: writing a commit message from %d characters of diff…" (length diff))
    (if arg
        (efrit-ask-candidates
         prompt efrit-commit-candidates
         (lambda (candidates msg)
           (if candidates
               (efrit-candidates-choose candidates insert-there "commit message")
             (message "efrit commit: %s" msg)))
         :purpose "commit message candidates" :key "efrit-commit")
      (efrit-ask-once
       prompt
       (lambda (text msg)
         (if text (funcall insert-there text) (message "efrit commit: %s" msg)))
       :purpose "commit message" :key "efrit-commit"))))

(provide 'efrit-commit)

;;; efrit-commit.el ends here
