;;; efrit-tool-vcs-log.el --- VCS log tool -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Keywords: ai, tools, git
;; Version: 0.9.2

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
(require 'efrit-vcs)
(require 'cl-lib)

;;; Constants

(defun efrit-tool-vcs-log--parse (text)
  "Commits in VC's log TEXT (the backend's default format), newest first.
Each is an alist with hash, short_hash, author, email, date (ISO 8601
when parseable, else as written), subject."
  (let ((commits nil) (current nil) (body nil))
    (cl-flet ((finish ()
                (when current
                  (let ((subject (car (cl-remove-if #'string-empty-p (nreverse body)))))
                    (push (cons 'subject (or subject "")) current))
                  (push (nreverse current) commits)
                  (setq current nil body nil))))
      (dolist (line (split-string text "\n"))
        (cond
         ((string-match "\\`commit \\([0-9a-f]+\\)" line)
          (finish)
          (let ((hash (match-string 1 line)))
            (setq current (list (cons 'short_hash (substring hash 0 (min 7 (length hash))))
                                (cons 'hash hash)))))
         ((null current))
         ((string-match "\\`Author: *\\(.*?\\) *<\\([^>]*\\)>" line)
          (push (cons 'author (match-string 1 line)) current)
          (push (cons 'email (match-string 2 line)) current))
         ((string-match "\\`Author: *\\(.*\\)" line)
          (push (cons 'author (match-string 1 line)) current))
         ((string-match "\\`Date: *\\(.*\\)" line)
          (let* ((raw (match-string 1 line))
                 (time (ignore-errors (date-to-time raw))))
            (push (cons 'date (if time (format-time-string "%FT%T%z" time) raw)) current)))
         ((string-match "\\`    \\(.*\\)" line)
          (push (match-string 1 line) body))))
      (finish))
    (nreverse commits)))

(defun efrit-tool-vcs-log--since-p (commit since)
  "Non-nil if COMMIT's date is at or after SINCE (a date string)."
  (let ((cutoff (ignore-errors (date-to-time since)))
        (date (ignore-errors (date-to-time (alist-get 'date commit)))))
    (or (null cutoff) (null date) (not (time-less-p date cutoff)))))

;;; Main Tool Function

(defun efrit-tool-vcs-log (args)
  "Get commit history.

ARGS is an alist with:
  path   - file or directory to filter by (default: whole repository)
  count  - number of commits (default: 10)
  since  - only commits at or after this date
  author - only commits whose author name or email contains this
  grep   - only commits whose subject contains this

Through VC (`efrit-vcs-log'); the filters are applied here, so more
entries than COUNT are read when filtering.  Returns a standard tool
response with a `commits' vector."
  (efrit-tool-execute vcs_log args
    (let* ((path-input (alist-get 'path args))
           (count (or (alist-get 'count args) 10))
           (since (alist-get 'since args))
           (author (alist-get 'author args))
           (grep (alist-get 'grep args))
           (path-info (efrit-resolve-path path-input 'read "vcs_log"))
           (path (plist-get path-info :path))
           (root (or (efrit-vcs-root (if (file-directory-p path) path (file-name-directory path)))
                     (signal 'user-error (list "Not inside a version-controlled tree"))))
           (files (and path-input (not (equal (file-name-as-directory path) root)) (list path)))
           (filtering (or since author grep))
           (text (condition-case err
                     (efrit-vcs-log files (if filtering (* 20 count) count) nil root)
                   (efrit-vcs-error (signal 'user-error (cdr err)))))
           (commits (efrit-tool-vcs-log--parse text)))
      (when since
        (setq commits (cl-remove-if-not (lambda (c) (efrit-tool-vcs-log--since-p c since)) commits)))
      (when author
        (setq commits (cl-remove-if-not
                       (lambda (c) (or (string-match-p (regexp-quote author) (or (alist-get 'author c) ""))
                                       (string-match-p (regexp-quote author) (or (alist-get 'email c) ""))))
                       commits)))
      (when grep
        (setq commits (cl-remove-if-not
                       (lambda (c) (string-match-p (regexp-quote grep) (or (alist-get 'subject c) "")))
                       commits)))
      (setq commits (seq-take commits count))
      (efrit-tool-success
       `((commits . ,(vconcat commits))
         (count . ,(length commits))
         ,@(when files `((filtered_by_path . ,(file-relative-name path root))))
         ,@(when since `((since . ,since)))
         ,@(when author `((author_filter . ,author)))
         ,@(when grep `((message_filter . ,grep))))))))

(provide 'efrit-tool-vcs-log)

;;; efrit-tool-vcs-log.el ends here
