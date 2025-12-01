;;; efrit-tool-search-content.el --- Content search tool -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Keywords: ai, tools
;; Version: 0.4.1

;;; Commentary:
;;
;; This tool provides grep-like content search across the codebase.
;; It uses ripgrep when available for best performance.
;;
;; Key features:
;; - Literal and regex search
;; - Context lines (before/after)
;; - File pattern filtering
;; - Case sensitivity control
;; - Pagination support

;;; Code:

(require 'efrit-tool-utils)
(require 'efrit-common)
(require 'cl-lib)
(require 'json)

;;; Customization

(defcustom efrit-tool-search-max-results 50
  "Default maximum number of search results."
  :type 'integer
  :group 'efrit-tool-utils)

(defcustom efrit-tool-search-context-lines 2
  "Default number of context lines before and after matches."
  :type 'integer
  :group 'efrit-tool-utils)

(defcustom efrit-tool-search-max-line-length 500
  "Maximum line length before truncation in search results."
  :type 'integer
  :group 'efrit-tool-utils)

;;; Tool Detection

(defvar efrit-tool-search--ripgrep-available nil
  "Cache for ripgrep availability check.")

(defun efrit-tool-search--ripgrep-available-p ()
  "Check if ripgrep (rg) is available."
  (or efrit-tool-search--ripgrep-available
      (setq efrit-tool-search--ripgrep-available
            (and (executable-find "rg") t))))

(defun efrit-tool-search--grep-available-p ()
  "Check if grep is available."
  (and (executable-find "grep") t))

;;; Ripgrep Implementation

(defun efrit-tool-search--escape-pattern (pattern is-regex)
  "Escape PATTERN for search.
If IS-REGEX is nil, escape regex special characters."
  (if is-regex
      pattern
    ;; Escape regex special chars for literal search
    (regexp-quote pattern)))

(defun efrit-tool-search--build-rg-args (pattern path file-pattern context-lines
                                                  max-results case-sensitive is-regex)
  "Build ripgrep command arguments."
  (let ((args (list "--json"
                    "--line-number"
                    "--column"
                    (format "--context=%d" context-lines)
                    (format "--max-count=%d" (* max-results 2)))))  ; Get extra for context dedup
    (unless case-sensitive
      (push "--ignore-case" args))
    (when file-pattern
      (push (format "--glob=%s" file-pattern) args))
    (unless is-regex
      (push "--fixed-strings" args))
    ;; Add pattern and path
    (setq args (nreverse args))
    (push pattern args)
    (push path args)
    (nreverse args)))

(defun efrit-tool-search--truncate-line (line max-length)
  "Truncate LINE if longer than MAX-LENGTH.
Uses `efrit-common-truncate-string'."
  (efrit-common-truncate-string line max-length))

(defun efrit-tool-search--parse-rg-output (output project-root max-results)
  "Parse ripgrep JSON output into structured results.
Returns a plist with :matches and :files-searched."
  (let ((matches '())
        (files-seen (make-hash-table :test 'equal))
        (current-file nil)
        (context-before '())
        (match-count 0))
    (dolist (line (split-string output "\n" t))
      (when (string-prefix-p "{" line)
        (condition-case nil
            (let* ((json-object-type 'alist)
                   (json-key-type 'symbol)  ; Ensure symbol keys for alist-get
                   (obj (json-read-from-string line))
                   (type (alist-get 'type obj)))
              (pcase type
                ("begin"
                 ;; Path can be a string or an object with 'text' key
                 (let ((path-data (alist-get 'path (alist-get 'data obj))))
                   (setq current-file (if (stringp path-data)
                                          path-data
                                        (alist-get 'text path-data))))
                 (puthash current-file t files-seen)
                 (setq context-before '()))
                ("context"
                 (let ((text (alist-get 'text (alist-get 'lines (alist-get 'data obj)))))
                   (when text
                     (push (efrit-tool-search--truncate-line
                            (string-trim-right text)
                            efrit-tool-search-max-line-length)
                           context-before)
                     (when (> (length context-before) efrit-tool-search-context-lines)
                       (setq context-before
                             (seq-take context-before efrit-tool-search-context-lines))))))
                ("match"
                 (when (< match-count max-results)
                   (let* ((data (alist-get 'data obj))
                          ;; Path can be string or object with 'text' key
                          (path-data (alist-get 'path data))
                          (path (if (stringp path-data)
                                    path-data
                                  (alist-get 'text path-data)))
                          (line-num (alist-get 'line_number data))
                          (submatches (alist-get 'submatches data))
                          (column (when submatches
                                    (1+ (alist-get 'start (aref submatches 0)))))
                          (text (alist-get 'text (alist-get 'lines data))))
                     (push `((file . ,(expand-file-name path project-root))
                             (file_relative . ,path)
                             (line . ,line-num)
                             (column . ,column)
                             (content . ,(efrit-tool-search--truncate-line
                                          (string-trim-right (or text ""))
                                          efrit-tool-search-max-line-length))
                             (context_before . ,(vconcat (nreverse context-before))))
                           matches)
                     (setq match-count (1+ match-count))
                     (setq context-before '()))))))
          (error nil))))
    (list :matches (vconcat (nreverse matches))
          :files-searched (hash-table-count files-seen)
          :total-matches match-count)))

(defun efrit-tool-search--rg-search (pattern path file-pattern context-lines
                                             max-results case-sensitive is-regex)
  "Search using ripgrep."
  (let* ((default-directory path)
         (args (efrit-tool-search--build-rg-args
                pattern "." file-pattern context-lines
                max-results case-sensitive is-regex))
         (output-buffer (generate-new-buffer " *efrit-rg-output*"))
         exit-code output)
    (unwind-protect
        (progn
          (setq exit-code
                (apply #'call-process "rg" nil output-buffer nil args))
          (setq output (with-current-buffer output-buffer (buffer-string)))
          (if (memq exit-code '(0 1))  ; 0=matches, 1=no matches
              (efrit-tool-search--parse-rg-output output path max-results)
            (list :matches []
                  :files-searched 0
                  :total-matches 0
                  :error (format "rg failed with exit code %d" exit-code))))
      (kill-buffer output-buffer))))

;;; Elisp Fallback Implementation

(defun efrit-tool-search--elisp-search (pattern path file-pattern context-lines
                                                max-results case-sensitive is-regex)
  "Search using Emacs Lisp (slow fallback)."
  (let* ((matches '())
         (files-searched 0)
         (match-count 0)
         (regex (if is-regex
                    pattern
                  (regexp-quote pattern)))
         (case-fold-search (not case-sensitive))
         ;; Get file list
         (files (if (efrit-tool-git-available-p)
                    (let ((result (efrit-tool-run-git '("ls-files"))))
                      (when (plist-get result :success)
                        (mapcar (lambda (f) (expand-file-name f path))
                                (split-string (plist-get result :output) "\n" t))))
                  (directory-files-recursively path "."))))
    ;; Filter by file pattern
    (when file-pattern
      (let ((file-regex (wildcard-to-regexp file-pattern)))
        (setq files (cl-remove-if-not
                     (lambda (f) (string-match-p file-regex f))
                     files))))
    ;; Search each file
    (catch 'done
      (dolist (file files)
        (when (and (file-regular-p file)
                   (not (efrit-tool-binary-file-p file)))
          (setq files-searched (1+ files-searched))
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (while (and (< match-count max-results)
                        (re-search-forward regex nil t))
              (let* ((line-num (line-number-at-pos (match-beginning 0)))
                     (col (- (match-beginning 0) (line-beginning-position)))
                     (line-content (buffer-substring-no-properties
                                    (line-beginning-position)
                                    (line-end-position)))
                     (context-before (efrit-tool-search--get-context-lines
                                      (- line-num context-lines) (1- line-num))))
                (push `((file . ,file)
                        (file_relative . ,(file-relative-name file path))
                        (line . ,line-num)
                        (column . ,(1+ col))
                        (content . ,(efrit-tool-search--truncate-line
                                     line-content
                                     efrit-tool-search-max-line-length))
                        (context_before . ,(vconcat context-before)))
                      matches)
                (setq match-count (1+ match-count))
                (when (>= match-count max-results)
                  (throw 'done nil))))))))
    (list :matches (vconcat (nreverse matches))
          :files-searched files-searched
          :total-matches match-count)))

(defun efrit-tool-search--get-context-lines (start-line end-line)
  "Get context lines from START-LINE to END-LINE in current buffer."
  (let ((lines '()))
    (save-excursion
      (goto-char (point-min))
      (forward-line (1- (max 1 start-line)))
      (dotimes (_ (- (min end-line (line-number-at-pos (point-max))) start-line -1))
        (push (efrit-tool-search--truncate-line
               (buffer-substring-no-properties
                (line-beginning-position) (line-end-position))
               efrit-tool-search-max-line-length)
              lines)
        (forward-line 1)))
    (nreverse lines)))

;;; Main Tool Function

(defun efrit-tool-search-content (args)
  "Search for content across the codebase.

ARGS is an alist with:
  pattern        - search pattern (required)
  is_regex       - treat pattern as regex (default: false)
  path           - search scope (default: project root)
  file_pattern   - glob filter (e.g., \"*.el\")
  context_lines  - lines before/after (default: 2)
  max_results    - limit results (default: 50)
  case_sensitive - case sensitivity (default: false)
  offset         - pagination offset (default: 0)

Returns a standard tool response with search results."
  (efrit-tool-execute search_content args
    (let* ((pattern (alist-get 'pattern args))
           (is-regex (alist-get 'is_regex args))
           (path-input (alist-get 'path args))
           (file-pattern (alist-get 'file_pattern args))
           (context-lines (or (alist-get 'context_lines args)
                              efrit-tool-search-context-lines))
           (max-results (or (alist-get 'max_results args)
                            efrit-tool-search-max-results))
           (case-sensitive (alist-get 'case_sensitive args))
           (offset (or (alist-get 'offset args) 0))
           (warnings '()))

      ;; Validate pattern
      (unless (and pattern (not (string-empty-p pattern)))
        (signal 'user-error (list "Search pattern is required")))

      ;; Resolve path
      (let* ((path-info (efrit-resolve-path path-input))
             (path (plist-get path-info :path)))

        ;; Ensure path is a directory
        (unless (file-directory-p path)
          (signal 'user-error (list "Search path must be a directory" path)))

        ;; Run search - request extra to support offset
        (let* ((fetch-count (+ max-results offset 1))  ; +1 to detect if more available
               (result (if (efrit-tool-search--ripgrep-available-p)
                           (efrit-tool-search--rg-search
                            pattern path file-pattern context-lines
                            fetch-count case-sensitive is-regex)
                         (progn
                           (push "Using slow Elisp fallback (ripgrep not available)" warnings)
                           (efrit-tool-search--elisp-search
                            pattern path file-pattern context-lines
                            fetch-count case-sensitive is-regex)))))

          ;; Apply offset pagination
          (let* ((all-matches (append (plist-get result :matches) nil))
                 (matches-after-offset (seq-drop all-matches offset))
                 (returned-matches (seq-take matches-after-offset max-results))
                 (has-more (> (length matches-after-offset) max-results))
                 (total-fetched (length all-matches)))

            (efrit-tool-success
             `((matches . ,(vconcat returned-matches))
               (total_fetched . ,total-fetched)
               (returned . ,(length returned-matches))
               (offset . ,offset)
               (files_searched . ,(plist-get result :files-searched))
               (truncated . ,(if has-more t :json-false))
               ,@(when has-more
                   `((continuation_hint . ,(format "Use offset=%d to get more"
                                                   (+ offset (length returned-matches))))))
               (search_tool . ,(if (efrit-tool-search--ripgrep-available-p)
                                   "ripgrep" "elisp")))
             warnings)))))))

(provide 'efrit-tool-search-content)

;;; efrit-tool-search-content.el ends here
