;;; efrit-tool-vcs-blame.el --- VCS blame tool -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Keywords: ai, tools, git
;; Version: 0.8.5

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
(require 'efrit-vcs)
(require 'cl-lib)

;;; Customization

(defcustom efrit-tool-vcs-blame-max-lines 500
  "Maximum number of lines to blame in one request.
Blaming large files can be slow; this limits the output."
  :type 'integer
  :group 'efrit-tool-utils)

;;; Parsing Functions

(defun efrit-tool-vcs-blame--parse (output)
  "Entries in VC's annotate OUTPUT (Git's blame format, ISO dates).
Each: line_number, commit (8 chars), full_commit, author, date,
content.  A line VC cannot attribute (uncommitted) has commit
\"00000000\"."
  (let ((entries nil))
    (dolist (line (split-string output "\n"))
      (when (string-match
             (concat "\\`\\^?\\([0-9a-f]+\\)[^(]*(\\(.*?\\) +"
                     "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\(?:T[0-9:]+[-+Z][0-9:]*\\)?\\) +"
                     "\\([0-9]+\\)) \\(.*\\)\\'")
             line)
        (let ((hash (match-string 1 line)))
          (push `((line_number . ,(string-to-number (match-string 4 line)))
                  (commit . ,(substring hash 0 (min 8 (length hash))))
                  (full_commit . ,hash)
                  (author . ,(string-trim (match-string 2 line)))
                  (date . ,(match-string 3 line))
                  (content . ,(match-string 5 line)))
                entries))))
    (nreverse entries)))

;;; Main Tool Function

(defun efrit-tool-vcs-blame (args)
  "Show who last changed each line of a file.

ARGS is an alist with:
  path       - the file (required)
  start_line - first line to show (default 1)
  end_line   - last line to show (default the end)

Through VC (`efrit-vcs-annotate').  Returns a standard tool response
with a `lines' vector."
  (efrit-tool-execute vcs_blame args
    (let* ((path-input (alist-get 'path args))
           (start-line (alist-get 'start_line args))
           (end-line (alist-get 'end_line args))
           (warnings '()))
      (unless path-input
        (signal 'user-error (list "path is required")))
      (let* ((path-info (efrit-resolve-path path-input 'read "vcs_blame"))
             (abs-path (plist-get path-info :path))
             (root (or (efrit-vcs-root (file-name-directory abs-path))
                       (signal 'user-error (list "Not inside a version-controlled tree"))))
             (rel-path (file-relative-name abs-path root)))
        (unless (file-exists-p abs-path)
          (signal 'user-error (list (format "File not found: %s" path-input))))
        (when (file-directory-p abs-path)
          (signal 'user-error (list "Cannot blame a directory")))
        (when (and start-line end-line (> start-line end-line))
          (signal 'user-error (list "start_line must be <= end_line")))
        (when (and start-line (< start-line 1))
          (signal 'user-error (list "start_line must be >= 1")))
        (let* ((output (condition-case err
                           (efrit-vcs-annotate abs-path nil root)
                         (efrit-vcs-error (signal 'user-error (cdr err)))))
               (all (efrit-tool-vcs-blame--parse output))
               (entries (cl-remove-if-not
                         (lambda (e) (let ((n (alist-get 'line_number e)))
                                       (and (or (null start-line) (>= n start-line))
                                            (or (null end-line) (<= n end-line)))))
                         all))
               (total-lines (length entries)))
          (when (and (null all) (not (string-empty-p (string-trim output))))
            (signal 'user-error (list (format "File not tracked, or unexpected annotate output for %s" rel-path))))
          (when (null all)
            (signal 'user-error (list (format "File not tracked by version control: %s" rel-path))))
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
           warnings))))))

(provide 'efrit-tool-vcs-blame)

;;; efrit-tool-vcs-blame.el ends here
