;;; efrit-tool-vcs-blame.el --- VCS blame tool -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Keywords: ai, tools, git
;; Version: 0.3.1

;;; Commentary:
;;
;; This tool provides line-by-line code attribution via git blame.
;;
;; Key features:
;; - Line range filtering for performance
;; - Structured output with commit info per line
;; - Timeout protection for large files

;;; Code:

(require 'efrit-tool-utils)
(require 'cl-lib)

;;; Customization

(defcustom efrit-tool-vcs-blame-max-lines 500
  "Maximum number of lines to blame in one request.
Blaming large files can be slow; this limits the output."
  :type 'integer
  :group 'efrit-tool-utils)

;;; Parsing Functions

(defun efrit-tool-vcs-blame--parse-porcelain (output)
  "Parse git blame porcelain OUTPUT.
Returns a list of blame entries, each an alist with:
  line_number, commit, author, author_email, date, content"
  (let ((lines (split-string output "\n"))
        (results '())
        (current-commit nil)
        (current-data nil)
        (line-num nil))
    (dolist (line lines)
      (cond
       ;; New commit line: hash orig-line final-line [group-lines]
       ((string-match "^\\([0-9a-f]\\{40\\}\\) \\([0-9]+\\) \\([0-9]+\\)" line)
        (setq current-commit (match-string 1 line))
        (setq line-num (string-to-number (match-string 3 line)))
        (setq current-data `((commit . ,(substring current-commit 0 8))
                             (full_commit . ,current-commit)
                             (line_number . ,line-num))))

       ;; Continuation line (same commit): hash orig-line final-line
       ((string-match "^\\([0-9a-f]\\{40\\}\\) \\([0-9]+\\) \\([0-9]+\\)$" line)
        (setq current-commit (match-string 1 line))
        (setq line-num (string-to-number (match-string 3 line)))
        (setq current-data `((commit . ,(substring current-commit 0 8))
                             (full_commit . ,current-commit)
                             (line_number . ,line-num))))

       ;; Author
       ((string-match "^author \\(.+\\)$" line)
        (setq current-data (append current-data
                                   `((author . ,(match-string 1 line))))))

       ;; Author email
       ((string-match "^author-mail <\\(.+\\)>$" line)
        (setq current-data (append current-data
                                   `((author_email . ,(match-string 1 line))))))

       ;; Author time (unix timestamp)
       ((string-match "^author-time \\([0-9]+\\)$" line)
        (let* ((timestamp (string-to-number (match-string 1 line)))
               (time (seconds-to-time timestamp))
               (date-str (format-time-string "%Y-%m-%dT%H:%M:%SZ" time t)))
          (setq current-data (append current-data
                                     `((date . ,date-str))))))

       ;; Summary (commit message)
       ((string-match "^summary \\(.+\\)$" line)
        (setq current-data (append current-data
                                   `((summary . ,(match-string 1 line))))))

       ;; Content line (starts with tab)
       ((string-match "^\t\\(.*\\)$" line)
        (setq current-data (append current-data
                                   `((content . ,(match-string 1 line)))))
        ;; Complete entry
        (push current-data results)
        (setq current-data nil))))

    (nreverse results)))

;;; Main Tool Function

(defun efrit-tool-vcs-blame (args)
  "Get line-by-line code attribution.

ARGS is an alist with:
  path       - file to blame (required)
  start_line - optional range start (1-indexed)
  end_line   - optional range end (inclusive)

Returns a standard tool response with blame information per line."
  (efrit-tool-execute vcs_blame args
    (let* ((path-input (alist-get 'path args))
           (start-line (alist-get 'start_line args))
           (end-line (alist-get 'end_line args))
           (warnings '()))

      ;; Validate required path
      (unless path-input
        (signal 'user-error (list "path is required")))

      ;; Check if git is available
      (unless (efrit-tool-git-available-p)
        (signal 'user-error (list "Not a git repository or git not available")))

      ;; Resolve path
      (let* ((path-info (efrit-resolve-path path-input))
             (abs-path (plist-get path-info :path))
             (rel-path (plist-get path-info :path-relative)))

        ;; Verify file exists
        (unless (file-exists-p abs-path)
          (signal 'user-error (list (format "File not found: %s" path-input))))

        ;; Verify it's a file, not directory
        (when (file-directory-p abs-path)
          (signal 'user-error (list "Cannot blame a directory")))

        ;; Validate line range if provided
        (when (and start-line end-line (> start-line end-line))
          (signal 'user-error (list "start_line must be <= end_line")))

        (when (and start-line (< start-line 1))
          (signal 'user-error (list "start_line must be >= 1")))

        ;; Build git blame args
        ;; Use porcelain format for easy parsing
        (let* ((blame-args
                (append
                 (list "blame" "-p")  ; porcelain format
                 (when (and start-line end-line)
                   (list "-L" (format "%d,%d" start-line end-line)))
                 (when (and start-line (not end-line))
                   ;; Start line only: blame from start to end of file
                   (list "-L" (format "%d," start-line)))
                 (list "--" rel-path)))
               (blame-result (efrit-tool-run-git blame-args 45)))  ; 45 sec timeout

          (unless (plist-get blame-result :success)
            (let ((error-msg (or (plist-get blame-result :error) "git blame failed")))
              ;; Check for common error cases
              (cond
               ((string-match-p "no such path" error-msg)
                (signal 'user-error (list (format "File not tracked by git: %s" rel-path))))
               ((string-match-p "no match" error-msg)
                (signal 'user-error (list (format "Invalid line range for file: %s" rel-path))))
               (t
                (signal 'user-error (list error-msg))))))

          (let* ((output (plist-get blame-result :output))
                 (entries (efrit-tool-vcs-blame--parse-porcelain output))
                 (total-lines (length entries)))

            ;; Warn and truncate if too many lines
            (when (> total-lines efrit-tool-vcs-blame-max-lines)
              (push (format "Truncated to %d lines (file has %d in range)"
                            efrit-tool-vcs-blame-max-lines total-lines)
                    warnings)
              (setq entries (seq-take entries efrit-tool-vcs-blame-max-lines)))

            (efrit-tool-success
             `((lines . ,(vconcat entries))
               (file . ,rel-path)
               (line_count . ,(length entries))
               ,@(when start-line `((start_line . ,start-line)))
               ,@(when end-line `((end_line . ,end-line)))
               ,@(when (> total-lines (length entries))
                   `((total_lines_in_range . ,total-lines))))
             warnings)))))))

(provide 'efrit-tool-vcs-blame)

;;; efrit-tool-vcs-blame.el ends here
