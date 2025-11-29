;;; efrit-tool-vcs-status.el --- VCS status tool -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Keywords: ai, tools, git
;; Version: 0.4.0

;;; Commentary:
;;
;; This tool provides repository status information.
;;
;; Key features:
;; - Current branch and tracking info
;; - Staged/unstaged/untracked files
;; - Stash count
;; - Recent commits
;; - Rebase/merge state detection

;;; Code:

(require 'efrit-tool-utils)
(require 'cl-lib)

;;; Parsing Functions

(defun efrit-tool-vcs-status--parse-branch-line (line)
  "Parse the branch line from git status --porcelain -b.
Returns a plist with :branch, :upstream, :ahead, :behind."
  (let ((branch nil)
        (upstream nil)
        (ahead 0)
        (behind 0)
        (detached nil))
    ;; Format: ## branch...upstream [ahead N, behind M]
    (cond
     ;; Detached HEAD
     ((string-match "^## HEAD (no branch)$" line)
      (setq detached t))
     ;; No commits yet
     ((string-match "^## No commits yet on \\(.+\\)$" line)
      (setq branch (match-string 1 line)))
     ;; Normal branch with possible upstream
     ((string-match "^## \\([^.]+\\)\\(?:\\.\\.\\.\\([^ ]+\\)\\)?\\(?: \\[\\([^]]+\\)\\]\\)?$" line)
      (setq branch (match-string 1 line))
      (setq upstream (match-string 2 line))
      (when-let* ((tracking (match-string 3 line)))
        (when (string-match "ahead \\([0-9]+\\)" tracking)
          (setq ahead (string-to-number (match-string 1 tracking))))
        (when (string-match "behind \\([0-9]+\\)" tracking)
          (setq behind (string-to-number (match-string 1 tracking)))))))
    (list :branch (or branch (when detached "HEAD (detached)"))
          :upstream upstream
          :ahead ahead
          :behind behind
          :detached detached)))

(defun efrit-tool-vcs-status--parse-file-line (line)
  "Parse a file status line from git status --porcelain.
Returns a plist with :status and :path."
  (when (>= (length line) 3)
    (let* ((index-status (aref line 0))
           (worktree-status (aref line 1))
           (path (substring line 3)))
      (list :path path
            :index-status (char-to-string index-status)
            :worktree-status (char-to-string worktree-status)))))

(defun efrit-tool-vcs-status--categorize-files (lines)
  "Categorize file lines into staged, unstaged, untracked.
LINES are the non-branch lines from git status --porcelain.
Returns a plist with :staged, :unstaged, :untracked."
  (let ((staged '())
        (unstaged '())
        (untracked '()))
    (dolist (line lines)
      (when-let* ((parsed (efrit-tool-vcs-status--parse-file-line line)))
        (let ((index (plist-get parsed :index-status))
              (worktree (plist-get parsed :worktree-status))
              (path (plist-get parsed :path)))
          (cond
           ;; Untracked
           ((equal index "?")
            (push path untracked))
           ;; Has staged changes
           ((member index '("M" "A" "D" "R" "C"))
            (push `((path . ,path)
                    (status . ,index))
                  staged)
            ;; Also has unstaged changes
            (when (member worktree '("M" "D"))
              (push `((path . ,path)
                      (status . ,worktree))
                    unstaged)))
           ;; Only unstaged changes
           ((member worktree '("M" "D"))
            (push `((path . ,path)
                    (status . ,worktree))
                  unstaged))))))
    (list :staged (nreverse staged)
          :unstaged (nreverse unstaged)
          :untracked (nreverse untracked))))

(defun efrit-tool-vcs-status--get-stash-count ()
  "Get the number of stashed changes."
  (let ((result (efrit-tool-run-git '("stash" "list"))))
    (if (plist-get result :success)
        (length (seq-remove #'string-empty-p
                            (split-string (plist-get result :output) "\n")))
      0)))

(defun efrit-tool-vcs-status--get-recent-commits (count)
  "Get the last COUNT commits."
  (let ((result (efrit-tool-run-git
                 (list "log" "--oneline" (format "-n%d" count)))))
    (if (plist-get result :success)
        (let ((lines (seq-remove #'string-empty-p
                                 (split-string (plist-get result :output) "\n"))))
          (mapcar (lambda (line)
                    (if (string-match "^\\([a-f0-9]+\\) \\(.*\\)$" line)
                        `((hash . ,(match-string 1 line))
                          (message . ,(match-string 2 line)))
                      `((hash . "")
                        (message . ,line))))
                  lines))
      nil)))

(defun efrit-tool-vcs-status--check-special-state ()
  "Check if repo is in a special state (rebasing, merging, etc.).
Returns a plist with state indicators."
  (let ((git-dir (efrit-tool-vcs-status--get-git-dir)))
    (list :is_rebasing (or (file-exists-p (expand-file-name "rebase-merge" git-dir))
                           (file-exists-p (expand-file-name "rebase-apply" git-dir)))
          :is_merging (file-exists-p (expand-file-name "MERGE_HEAD" git-dir))
          :is_cherry_picking (file-exists-p (expand-file-name "CHERRY_PICK_HEAD" git-dir))
          :is_reverting (file-exists-p (expand-file-name "REVERT_HEAD" git-dir))
          :is_bisecting (file-exists-p (expand-file-name "BISECT_LOG" git-dir)))))

(defun efrit-tool-vcs-status--get-git-dir ()
  "Get the .git directory path."
  (let ((result (efrit-tool-run-git '("rev-parse" "--git-dir"))))
    (when (plist-get result :success)
      (expand-file-name (string-trim (plist-get result :output))))))

;;; Main Tool Function

(defun efrit-tool-vcs-status (args)
  "Get the current repository status.

ARGS is an alist with:
  path - repository path (default: project root)

Returns a standard tool response with repository status."
  (efrit-tool-execute vcs_status args
    (let* ((path-input (alist-get 'path args))
           (path-info (efrit-resolve-path path-input)))
      ;; path-info used to bind default-directory in efrit-tool-execute macro
      (ignore path-info)

      ;; Check if git is available
      (unless (efrit-tool-git-available-p)
        (signal 'user-error (list "Not a git repository or git not available")))

      ;; Get git status
      (let ((status-result (efrit-tool-run-git '("status" "--porcelain" "-b"))))
        (unless (plist-get status-result :success)
          (signal 'user-error (list "git status failed"
                                    (plist-get status-result :error))))

        (let* ((lines (split-string (plist-get status-result :output) "\n"))
               (branch-line (car lines))
               (file-lines (cdr lines))
               (branch-info (efrit-tool-vcs-status--parse-branch-line branch-line))
               (file-info (efrit-tool-vcs-status--categorize-files file-lines))
               (stash-count (efrit-tool-vcs-status--get-stash-count))
               (recent-commits (efrit-tool-vcs-status--get-recent-commits 5))
               (special-state (efrit-tool-vcs-status--check-special-state))
               (is-clean (and (null (plist-get file-info :staged))
                              (null (plist-get file-info :unstaged))
                              (null (plist-get file-info :untracked)))))

          (efrit-tool-success
           `((current_branch . ,(plist-get branch-info :branch))
             (upstream . ,(plist-get branch-info :upstream))
             (ahead . ,(plist-get branch-info :ahead))
             (behind . ,(plist-get branch-info :behind))
             (detached . ,(if (plist-get branch-info :detached) t :json-false))
             (staged_files . ,(vconcat (plist-get file-info :staged)))
             (unstaged_files . ,(vconcat (plist-get file-info :unstaged)))
             (untracked_files . ,(vconcat (plist-get file-info :untracked)))
             (stash_count . ,stash-count)
             (recent_commits . ,(vconcat recent-commits))
             (is_clean . ,(if is-clean t :json-false))
             (is_rebasing . ,(if (plist-get special-state :is_rebasing) t :json-false))
             (is_merging . ,(if (plist-get special-state :is_merging) t :json-false))
             (is_cherry_picking . ,(if (plist-get special-state :is_cherry_picking) t :json-false))
             (is_reverting . ,(if (plist-get special-state :is_reverting) t :json-false))
             (is_bisecting . ,(if (plist-get special-state :is_bisecting) t :json-false)))))))))

(provide 'efrit-tool-vcs-status)

;;; efrit-tool-vcs-status.el ends here
