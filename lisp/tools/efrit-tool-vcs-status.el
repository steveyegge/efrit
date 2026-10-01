;;; efrit-tool-vcs-status.el --- VCS status tool -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Keywords: ai, tools, git
;; Version: 0.6.2

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
(require 'efrit-vcs)
(require 'cl-lib)
(declare-function efrit-tool-vcs-log--parse "efrit-tool-vcs-log")

;;; Parsing Functions

(defun efrit-tool-vcs-status--categorize (entries)
  "Split VC status ENTRIES ((FILE STATE) ...) into staged, unstaged, untracked.
VC's states: `added' and `removed' are staged; `edited', `missing',
`conflict', `needs-merge' are unstaged work-tree changes;
`unregistered' is untracked.  Git's index-vs-worktree split for an
edited tracked file is not visible through VC, so an edited file is
reported as unstaged."
  (let (staged unstaged untracked)
    (pcase-dolist (`(,file ,state) entries)
      (let ((entry `((path . ,file) (status . ,(symbol-name state)))))
        (pcase state
          ((or 'added 'removed) (push entry staged))
          ('unregistered (push file untracked))   ; untracked: plain paths
          ((or 'up-to-date 'ignored) nil)
          (_ (push entry unstaged)))))
    (list :staged (nreverse staged)
          :unstaged (nreverse unstaged)
          :untracked (nreverse untracked))))

(defun efrit-tool-vcs-status--recent-commits (root count)
  "The last COUNT commits of ROOT as ((hash . SHORT) (message . SUBJECT))."
  (require 'efrit-tool-vcs-log)
  (mapcar (lambda (c) `((hash . ,(alist-get 'short_hash c))
                        (message . ,(alist-get 'subject c))))
          (seq-take (efrit-tool-vcs-log--parse
                     (condition-case nil (efrit-vcs-log nil count nil root)
                       (efrit-vcs-error "")))
                    count)))

;;; Main Tool Function

(defun efrit-tool-vcs-status (args)
  "Get the current repository status.

ARGS is an alist with:
  path - repository path (default: project root)

Through VC (`efrit-vcs-status-files' and friends).  Returns a
standard tool response with repository status."
  (efrit-tool-execute vcs_status args
    (let* ((path-input (alist-get 'path args))
           (path-info (efrit-resolve-path path-input 'read "vcs_status"))
           (path (plist-get path-info :path))
           (root (or (efrit-vcs-root (if (file-directory-p path) path (file-name-directory path)))
                     (signal 'user-error
                             (list (format "%s is not inside a git repository (or any version-controlled tree)"
                                           (if (file-remote-p path) path (abbreviate-file-name path)))))))
           (entries (condition-case err (efrit-vcs-status-files root)
                      (efrit-vcs-error (signal 'user-error (cdr err)))))
           (file-info (efrit-tool-vcs-status--categorize entries))
           (branch (efrit-vcs-branch root))
           (ahead-behind (efrit-vcs-ahead-behind root))
           (stashes (efrit-vcs-stash-list root))
           (special (efrit-vcs-special-state root))
           (is-clean (and (null (plist-get file-info :staged))
                          (null (plist-get file-info :unstaged))
                          (null (plist-get file-info :untracked)))))
      (efrit-tool-success
       `((backend . ,(symbol-name (efrit-vcs-backend root)))
         (current_branch . ,(or branch "HEAD"))
         (upstream . ,(efrit-vcs-upstream root))
         (ahead . ,(or (car ahead-behind) 0))
         (behind . ,(or (cdr ahead-behind) 0))
         (detached . ,(if branch :json-false t))
         (staged_files . ,(vconcat (plist-get file-info :staged)))
         (unstaged_files . ,(vconcat (plist-get file-info :unstaged)))
         (untracked_files . ,(vconcat (plist-get file-info :untracked)))
         (stash_count . ,(length stashes))
         (efrit_checkpoints . ,(vconcat (cl-remove-if-not
                                         (lambda (m) (string-match-p efrit-vcs-stash-prefix m))
                                         (mapcar #'cdr stashes))))
         (recent_commits . ,(vconcat (efrit-tool-vcs-status--recent-commits root 5)))
         (is_clean . ,(if is-clean t :json-false))
         (is_rebasing . ,(if (eq special 'rebasing) t :json-false))
         (is_merging . ,(if (eq special 'merging) t :json-false))
         (is_cherry_picking . ,(if (eq special 'cherry-picking) t :json-false))
         (is_reverting . ,(if (eq special 'reverting) t :json-false))
         (is_bisecting . ,(if (eq special 'bisecting) t :json-false)))))))

(provide 'efrit-tool-vcs-status)

;;; efrit-tool-vcs-status.el ends here
