;;; efrit-tool-vcs-diff.el --- VCS diff tool -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Keywords: ai, tools, git
;; Version: 0.8.5

;;; Commentary:
;;
;; This tool provides git diff functionality.
;;
;; Key features:
;; - Staged and unstaged diffs
;; - Diff against specific commits
;; - Per-file statistics
;; - Configurable context lines

;;; Code:

(require 'efrit-tool-utils)
(require 'efrit-vcs)
(require 'cl-lib)
(defvar vc-git-diff-switches)

;;; Customization

(defcustom efrit-tool-vcs-diff-max-size 100000
  "Maximum diff output size in bytes."
  :type 'integer
  :group 'efrit-tool-utils)

;;; Parsing Functions

(defun efrit-tool-vcs-diff--file-stats (diff)
  "Per-file insertions and deletions read off the unified DIFF text.
A list of ((path . NAME) (insertions . N) (deletions . N)), in order."
  (let ((stats nil) (current nil))
    (dolist (line (split-string diff "\n"))
      (cond
       ((string-match "\\`diff --git a/\\(.*\\) b/" line)
        (setq current (list (cons 'path (match-string 1 line))
                            (cons 'insertions 0) (cons 'deletions 0)))
        (push current stats))
       ((string-prefix-p "+++ " line))
       ((string-prefix-p "--- " line))
       ((and current (string-prefix-p "+" line))
        (cl-incf (alist-get 'insertions current)))
       ((and current (string-prefix-p "-" line))
        (cl-incf (alist-get 'deletions current)))))
    (nreverse stats)))

(defun efrit-tool-vcs-diff--get-summary (file-stats)
  "Calculate summary from FILE-STATS list."
  (let ((files 0)
        (insertions 0)
        (deletions 0))
    (dolist (stat file-stats)
      (when stat
        (setq files (1+ files))
        (setq insertions (+ insertions (or (cdr (assoc 'insertions stat)) 0)))
        (setq deletions (+ deletions (or (cdr (assoc 'deletions stat)) 0)))))
    `((files_changed . ,files)
      (insertions . ,insertions)
      (deletions . ,deletions))))

;;; Main Tool Function

(defun efrit-tool-vcs-diff (args)
  "Get diff output for repository changes.

ARGS is an alist with:
  path          - file or directory (default: all)
  staged        - show staged changes only (default: false)
  commit        - diff against specific commit (optional)
  context_lines - lines of context (default: 3)

Through VC (`efrit-vcs-diff'); context_lines is honoured via
`vc-git-diff-switches' for Git.  Returns a standard tool response."
  (efrit-tool-execute vcs_diff args
    (let* ((path-input (alist-get 'path args))
           (staged (alist-get 'staged args))
           (commit (alist-get 'commit args))
           (context-lines (or (alist-get 'context_lines args) 3))
           (path-info (efrit-resolve-path path-input 'read "vcs_diff"))
           (path (plist-get path-info :path))
           (root (or (efrit-vcs-root (if (file-directory-p path) path (file-name-directory path)))
                     (signal 'user-error (list "Not inside a version-controlled tree"))))
           (files (and path-input (not (equal (file-name-as-directory path) root)) (list path)))
           (warnings '())
           (diff-output
            (condition-case err
                (let ((vc-git-diff-switches (list (format "-U%d" context-lines))))
                  (cond
                   (staged (efrit-vcs-diff-staged files root))
                   (t (efrit-vcs-diff files commit nil root))))
              (efrit-vcs-error (signal 'user-error (cdr err)))))
           (truncated nil)
           (file-stats (efrit-tool-vcs-diff--file-stats diff-output))
           (summary (efrit-tool-vcs-diff--get-summary file-stats)))
      (when (> (length diff-output) efrit-tool-vcs-diff-max-size)
        (setq diff-output (substring diff-output 0 efrit-tool-vcs-diff-max-size))
        (setq truncated t)
        (push (format "Diff truncated at %dKB" (/ efrit-tool-vcs-diff-max-size 1000))
              warnings))
      (efrit-tool-success
       `((diff . ,diff-output)
         (summary . ,summary)
         (files . ,(vconcat file-stats))
         (truncated . ,(if truncated t :json-false))
         (diff_type . ,(cond (commit (format "vs %s" commit))
                             (staged "staged")
                             (t "unstaged"))))
       warnings))))

(provide 'efrit-tool-vcs-diff)

;;; efrit-tool-vcs-diff.el ends here
