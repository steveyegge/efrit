;;; efrit-tool-vcs-diff.el --- VCS diff tool -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Keywords: ai, tools, git
;; Version: 0.3.1

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
(require 'cl-lib)

;;; Customization

(defcustom efrit-tool-vcs-diff-max-size 100000
  "Maximum diff output size in bytes."
  :type 'integer
  :group 'efrit-tool-utils)

;;; Parsing Functions

(defun efrit-tool-vcs-diff--parse-stat-line (line)
  "Parse a line from git diff --stat.
Returns plist with :path, :insertions, :deletions or nil."
  (when (string-match "^ \\(.+?\\) +\\| +\\([0-9]+\\) \\([+-]+\\)$" line)
    (let* ((path (string-trim (match-string 1 line)))
           (changes (string-to-number (match-string 2 line)))
           (bar (match-string 3 line))
           (insertions (length (replace-regexp-in-string "-" "" bar)))
           (deletions (length (replace-regexp-in-string "\\+" "" bar))))
      ;; Scale the counts based on actual number
      (when (> changes 0)
        (let ((insert-pct (/ (float insertions) (+ insertions deletions)))
              (delete-pct (/ (float deletions) (+ insertions deletions))))
          (setq insertions (round (* changes insert-pct)))
          (setq deletions (round (* changes delete-pct)))))
      (list :path path
            :insertions insertions
            :deletions deletions))))

(defun efrit-tool-vcs-diff--parse-numstat-line (line)
  "Parse a line from git diff --numstat.
Returns plist with :path, :insertions, :deletions or nil."
  (when (string-match "^\\([0-9]+\\|-\\)\t\\([0-9]+\\|-\\)\t\\(.+\\)$" line)
    (let ((insertions (match-string 1 line))
          (deletions (match-string 2 line))
          (path (match-string 3 line)))
      (list :path path
            :insertions (if (equal insertions "-") 0 (string-to-number insertions))
            :deletions (if (equal deletions "-") 0 (string-to-number deletions))))))

(defun efrit-tool-vcs-diff--get-numstat (args)
  "Get file statistics using git diff --numstat with ARGS.
Returns list of file stats."
  (let ((result (efrit-tool-run-git (append args '("--numstat")))))
    (when (plist-get result :success)
      (let ((lines (split-string (plist-get result :output) "\n" t)))
        (mapcar (lambda (line)
                  (let ((parsed (efrit-tool-vcs-diff--parse-numstat-line line)))
                    (when parsed
                      `((path . ,(plist-get parsed :path))
                        (insertions . ,(plist-get parsed :insertions))
                        (deletions . ,(plist-get parsed :deletions))))))
                lines)))))

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

Returns a standard tool response with diff output."
  (efrit-tool-execute vcs_diff args
    (let* ((path-input (alist-get 'path args))
           (staged (alist-get 'staged args))
           (commit (alist-get 'commit args))
           (context-lines (or (alist-get 'context_lines args) 3))
           (warnings '()))

      ;; Check if git is available
      (unless (efrit-tool-git-available-p)
        (signal 'user-error (list "Not a git repository or git not available")))

      ;; Build git diff args
      (let* ((diff-args (list "diff"))
             (path-resolved (when path-input
                             (plist-get (efrit-resolve-path path-input) :path-relative))))

        ;; Add context lines
        (push (format "-U%d" context-lines) diff-args)

        ;; Handle staged vs unstaged vs commit
        (cond
         (commit
          (push commit diff-args))
         (staged
          (push "--staged" diff-args)))

        ;; Add path if specified
        (when path-resolved
          (push "--" diff-args)
          (push path-resolved diff-args))

        ;; Finalize args (reverse because we pushed)
        (setq diff-args (nreverse diff-args))

        ;; Get the diff
        (let ((diff-result (efrit-tool-run-git diff-args)))
          (unless (plist-get diff-result :success)
            (signal 'user-error (list "git diff failed"
                                      (plist-get diff-result :error))))

          (let* ((diff-output (plist-get diff-result :output))
                 (truncated nil)
                 ;; Get file stats using same base args
                 (stat-base-args (cond
                                  (commit (list "diff" commit))
                                  (staged '("diff" "--staged"))
                                  (t '("diff"))))
                 (stat-args (if path-resolved
                               (append stat-base-args (list "--" path-resolved))
                             stat-base-args))
                 (file-stats (efrit-tool-vcs-diff--get-numstat stat-args))
                 (summary (efrit-tool-vcs-diff--get-summary file-stats)))

            ;; Truncate if too large
            (when (> (length diff-output) efrit-tool-vcs-diff-max-size)
              (setq diff-output (substring diff-output 0 efrit-tool-vcs-diff-max-size))
              (setq truncated t)
              (push (format "Diff truncated at %dKB" (/ efrit-tool-vcs-diff-max-size 1000))
                    warnings))

            (efrit-tool-success
             `((diff . ,diff-output)
               (summary . ,summary)
               (files . ,(vconcat (seq-remove #'null file-stats)))
               (truncated . ,(if truncated t :json-false))
               (diff_type . ,(cond (commit (format "vs %s" commit))
                                   (staged "staged")
                                   (t "unstaged"))))
             warnings)))))))

(provide 'efrit-tool-vcs-diff)

;;; efrit-tool-vcs-diff.el ends here
