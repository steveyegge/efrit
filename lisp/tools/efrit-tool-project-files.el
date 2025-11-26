;;; efrit-tool-project-files.el --- Project file listing tool -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Keywords: ai, tools
;; Version: 0.3.0

;;; Commentary:
;;
;; This tool provides Claude with visibility into project file structure.
;; It lists files in the project directory with optional filtering.
;;
;; Key features:
;; - Uses git ls-files for git repos (fast, respects .gitignore)
;; - Falls back to directory-files-recursively for non-git projects
;; - Glob pattern filtering
;; - Pagination support
;; - Respects project sandbox

;;; Code:

(require 'efrit-tool-utils)
(require 'cl-lib)

;;; Customization

(defcustom efrit-tool-project-files-max-default 500
  "Default maximum number of files to return."
  :type 'integer
  :group 'efrit-tool-utils)

(defcustom efrit-tool-project-files-max-depth-default 5
  "Default maximum directory depth to traverse."
  :type 'integer
  :group 'efrit-tool-utils)

;;; Git-based file listing

(defun efrit-tool-project-files--git-ls-files (root pattern include-hidden)
  "List files using git ls-files.
ROOT is the repository root.
PATTERN is an optional glob filter.
INCLUDE-HIDDEN when non-nil includes dotfiles.
Returns a list of relative paths."
  (let* ((default-directory root)
         (args (list "ls-files" "--cached" "--others" "--exclude-standard"))
         (result (efrit-tool-run-git args)))
    (when (plist-get result :success)
      (let ((files (split-string (plist-get result :output) "\n" t)))
        ;; Filter by pattern if specified
        (when pattern
          (let ((regex (wildcard-to-regexp pattern)))
            (setq files (cl-remove-if-not
                         (lambda (f) (string-match-p regex f))
                         files))))
        ;; Filter hidden files if not requested
        (unless include-hidden
          (setq files (cl-remove-if
                       (lambda (f)
                         (or (string-prefix-p "." f)
                             (string-match-p "/\\." f)))
                       files)))
        files))))

;;; Directory-based file listing

(defun efrit-tool-project-files--dir-recursive (root pattern max-depth include-hidden)
  "List files using directory traversal.
ROOT is the starting directory.
PATTERN is an optional glob filter.
MAX-DEPTH limits recursion depth.
INCLUDE-HIDDEN when non-nil includes dotfiles.
Returns a list of absolute paths."
  (let ((files '())
        (regex (when pattern (wildcard-to-regexp pattern))))
    (efrit-tool-project-files--collect-files
     root root 0 max-depth include-hidden regex files)))

(defun efrit-tool-project-files--collect-files (dir root depth max-depth include-hidden regex files)
  "Recursively collect files from DIR.
ROOT is the project root.
DEPTH is the current depth, MAX-DEPTH is the limit.
INCLUDE-HIDDEN controls dotfile inclusion.
REGEX is the optional file pattern filter.
FILES is accumulated result (modified in place, returns new list)."
  (when (<= depth max-depth)
    (condition-case err
        (dolist (entry (directory-files dir t nil t))
          (let ((name (file-name-nondirectory entry)))
            ;; Skip . and ..
            (unless (member name '("." ".."))
              ;; Skip hidden files unless requested
              (when (or include-hidden (not (string-prefix-p "." name)))
                (cond
                 ;; Directory - recurse
                 ((and (file-directory-p entry)
                       (not (file-symlink-p entry)))
                  (setq files (efrit-tool-project-files--collect-files
                               entry root (1+ depth) max-depth
                               include-hidden regex files)))
                 ;; File - check pattern and add
                 ((file-regular-p entry)
                  (when (or (null regex)
                            (string-match-p regex name))
                    (push entry files))))))))
      (file-error
       ;; Skip directories we can't read
       nil))
    files))

;;; File info gathering

(defun efrit-tool-project-files--file-info (path root)
  "Get info for file at PATH relative to ROOT.
Returns an alist with file metadata."
  (let* ((attrs (file-attributes path))
         (relative (file-relative-name path root)))
    `((path . ,path)
      (path_relative . ,relative)
      (size . ,(file-attribute-size attrs))
      (mtime . ,(efrit-tool-format-time (file-attribute-modification-time attrs)))
      (is_directory . ,(eq (file-attribute-type attrs) t))
      (is_symlink . ,(stringp (file-attribute-type attrs))))))

;;; Main tool function

(defun efrit-tool-project-files (args)
  "List project files with optional filtering.

ARGS is an alist with:
  path          - directory to scan (default: project root)
  pattern       - glob pattern filter (e.g., \"*.el\")
  max_depth     - recursion depth limit (default: 5)
  include_hidden - include dotfiles (default: false)
  max_files     - limit files returned (default: 500)
  offset        - pagination offset (default: 0)

Returns a standard tool response with file listing."
  (efrit-tool-execute project_files args
    (let* ((path-input (alist-get 'path args))
           (pattern (alist-get 'pattern args))
           (max-depth (or (alist-get 'max_depth args)
                          efrit-tool-project-files-max-depth-default))
           (include-hidden (alist-get 'include_hidden args))
           (max-files (or (alist-get 'max_files args)
                          efrit-tool-project-files-max-default))
           (offset (or (alist-get 'offset args) 0))
           ;; Resolve path with sandbox check
           (path-info (efrit-resolve-path path-input))
           (root (plist-get path-info :path))
           (project-root (plist-get path-info :project-root))
           (warnings '())
           files)

      ;; Check path exists and is a directory
      (unless (file-directory-p root)
        (signal 'file-error (list "Not a directory" root)))

      ;; Get files using appropriate method
      (let ((project-type (efrit-tool-detect-project-type)))
        (if (and (eq project-type 'git)
                 (efrit-tool-git-available-p))
            ;; Use git ls-files (fast, respects .gitignore)
            (let ((git-files (efrit-tool-project-files--git-ls-files
                              project-root pattern include-hidden)))
              (setq files (mapcar (lambda (f)
                                    (expand-file-name f project-root))
                                  git-files)))
          ;; Fall back to directory traversal
          (setq files (efrit-tool-project-files--dir-recursive
                       root pattern max-depth include-hidden))
          (when (not (eq project-type 'git))
            (push ".gitignore not available (not a git repo)" warnings)))

        ;; Sort by path
        (setq files (sort files #'string<))

        ;; Count totals before pagination
        (let* ((total-files (length files))
               (dir-count (length (cl-remove-if-not
                                   (lambda (f) (file-directory-p f))
                                   files))))
          ;; Apply pagination
          (let* ((paginated (efrit-tool-paginate files offset max-files))
                 (page-files (plist-get paginated :items))
                 (truncated (plist-get paginated :truncated)))

            ;; Build file info for each file
            (let ((file-infos (mapcar (lambda (f)
                                        (efrit-tool-project-files--file-info f project-root))
                                      page-files)))
              ;; Build result
              (efrit-tool-success
               `((root . ,project-root)
                 (project_type . ,(symbol-name project-type))
                 (files . ,(vconcat file-infos))
                 (directories . ,dir-count)
                 (total_files . ,total-files)
                 (returned . ,(length page-files))
                 (offset . ,offset)
                 (truncated . ,(if truncated t :json-false))
                 ,@(when truncated
                     `((continuation_hint . ,(format "Use offset=%d to get more"
                                                     (+ offset (length page-files))))))
                 (gitignore_respected . ,(if (eq project-type 'git) t :json-false)))
               warnings))))))))

(provide 'efrit-tool-project-files)

;;; efrit-tool-project-files.el ends here
