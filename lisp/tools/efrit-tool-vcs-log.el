;;; efrit-tool-vcs-log.el --- VCS log tool -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Keywords: ai, tools, git
;; Version: 0.4.0

;;; Commentary:
;;
;; This tool provides git log functionality.
;;
;; Key features:
;; - Configurable commit count
;; - File/directory filtering
;; - Date and author filters
;; - Commit message search

;;; Code:

(require 'efrit-tool-utils)
(require 'cl-lib)

;;; Constants

(defconst efrit-tool-vcs-log--field-sep "\x1e"
  "Record separator for git log format.")

(defconst efrit-tool-vcs-log--commit-sep "\x1f"
  "Unit separator between commits.")

;;; Parsing Functions

(defun efrit-tool-vcs-log--parse-commit (line)
  "Parse a commit LINE from git log output.
Returns alist with commit info or nil."
  (let ((fields (split-string line efrit-tool-vcs-log--field-sep)))
    (when (>= (length fields) 6)
      `((full_hash . ,(nth 0 fields))
        (hash . ,(nth 1 fields))
        (author_name . ,(nth 2 fields))
        (author_email . ,(nth 3 fields))
        (date . ,(nth 4 fields))
        (message . ,(nth 5 fields))))))

;;; Main Tool Function

(defun efrit-tool-vcs-log (args)
  "Get commit history.

ARGS is an alist with:
  path   - file or directory for filtered history (optional)
  count  - number of commits (default: 10)
  since  - date filter (e.g., '1 week ago')
  author - author filter
  grep   - commit message search

Returns a standard tool response with commit list."
  (efrit-tool-execute vcs_log args
    (let* ((path-input (alist-get 'path args))
           (count (or (alist-get 'count args) 10))
           (since (alist-get 'since args))
           (author (alist-get 'author args))
           (grep (alist-get 'grep args)))

      ;; Check if git is available
      (unless (efrit-tool-git-available-p)
        (signal 'user-error (list "Not a git repository or git not available")))

      ;; Build git log args with custom format
      ;; Format: full_hash|short_hash|author_name|author_email|date|subject
      (let* ((format-str (concat "%H" efrit-tool-vcs-log--field-sep
                                 "%h" efrit-tool-vcs-log--field-sep
                                 "%an" efrit-tool-vcs-log--field-sep
                                 "%ae" efrit-tool-vcs-log--field-sep
                                 "%aI" efrit-tool-vcs-log--field-sep
                                 "%s" efrit-tool-vcs-log--commit-sep))
             (path-resolved (when path-input
                             (plist-get (efrit-resolve-path path-input) :path-relative)))
             ;; Build args list directly in correct order
             (log-args (append
                        (list "log"
                              (format "--format=%s" format-str)
                              "-n" (number-to-string count))
                        (when since (list (format "--since=%s" since)))
                        (when author (list (format "--author=%s" author)))
                        (when grep (list (format "--grep=%s" grep)))
                        (when path-resolved (list "--" path-resolved)))))

        ;; Get the log
        (let ((log-result (efrit-tool-run-git log-args)))
          (unless (plist-get log-result :success)
            (signal 'user-error (list "git log failed"
                                      (plist-get log-result :error))))

          (let* ((output (plist-get log-result :output))
                 (commit-strings (split-string output efrit-tool-vcs-log--commit-sep t "[\n\r]+"))
                 (commits (seq-remove #'null
                                      (mapcar #'efrit-tool-vcs-log--parse-commit
                                              commit-strings))))

            (efrit-tool-success
             `((commits . ,(vconcat commits))
               (count . ,(length commits))
               ,@(when path-resolved
                   `((filtered_by_path . ,path-resolved)))
               ,@(when since
                   `((since . ,since)))
               ,@(when author
                   `((author_filter . ,author)))
               ,@(when grep
                   `((message_filter . ,grep)))))))))))

(provide 'efrit-tool-vcs-log)

;;; efrit-tool-vcs-log.el ends here
