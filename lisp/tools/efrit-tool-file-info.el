;;; efrit-tool-file-info.el --- File metadata tool -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Keywords: ai, tools
;; Version: 0.4.0

;;; Commentary:
;;
;; This tool provides file metadata without reading full contents.
;; Useful for checking existence, size, and type before reading.
;;
;; Key features:
;; - Batch query multiple files
;; - Binary detection
;; - Optional first line peek
;; - File permissions

;;; Code:

(require 'efrit-tool-utils)
(require 'cl-lib)

;;; Customization

(defcustom efrit-tool-file-info-max-paths 100
  "Maximum number of paths to query in one request."
  :type 'integer
  :group 'efrit-tool-utils)

;;; Implementation

(defun efrit-tool-file-info--get-first-line (path)
  "Get the first line of file at PATH.
Returns nil if file is binary or can't be read."
  (when (and (file-exists-p path)
             (not (efrit-tool-binary-file-p path)))
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents path nil 0 1000)
          (goto-char (point-min))
          (buffer-substring-no-properties
           (point) (line-end-position)))
      (error nil))))

(defun efrit-tool-file-info--get-permissions (attrs)
  "Get file permissions from ATTRS.
Uses file-attribute-modes which returns a string like -rw-r--r--."
  (file-attribute-modes attrs))

(defun efrit-tool-file-info--single (path project-root)
  "Get info for a single file at PATH relative to PROJECT-ROOT."
  (let* ((abs-path (expand-file-name path project-root))
         (exists (file-exists-p abs-path))
         (attrs (when exists (file-attributes abs-path)))
         (is-dir (eq (file-attribute-type attrs) t))
         (is-link (stringp (file-attribute-type attrs))))
    `((path . ,abs-path)
      (path_relative . ,(file-relative-name abs-path project-root))
      (exists . ,(if exists t :json-false))
      ,@(when exists
          `((size . ,(file-attribute-size attrs))
            (mtime . ,(efrit-tool-format-time
                       (file-attribute-modification-time attrs)))
            (permissions . ,(efrit-tool-file-info--get-permissions attrs))
            (is_directory . ,(if is-dir t :json-false))
            (is_symlink . ,(if is-link t :json-false))
            ,@(unless is-dir
                `((is_binary . ,(if (efrit-tool-binary-file-p abs-path) t :json-false))
                  (first_line . ,(efrit-tool-file-info--get-first-line abs-path)))))))))

(defun efrit-tool-file-info (args)
  "Get metadata about files without reading contents.

ARGS is an alist with:
  paths - list of file paths to query (required)
          Can be strings or a single string

Returns a standard tool response with file metadata for each path."
  (efrit-tool-execute file_info args
    (let* ((paths-input (alist-get 'paths args))
           ;; Normalize to list
           (paths (cond
                   ((stringp paths-input) (list paths-input))
                   ((listp paths-input) (append paths-input nil))
                   ((vectorp paths-input) (append paths-input nil))
                   (t (signal 'user-error (list "paths must be a string or list")))))
           (warnings '()))

      ;; Validate
      (unless paths
        (signal 'user-error (list "paths is required")))

      (when (> (length paths) efrit-tool-file-info-max-paths)
        (push (format "Only querying first %d of %d paths"
                      efrit-tool-file-info-max-paths (length paths))
              warnings)
        (setq paths (seq-take paths efrit-tool-file-info-max-paths)))

      ;; Resolve project root once
      (let* ((path-info (efrit-resolve-path nil))
             (project-root (plist-get path-info :project-root))
             (results (mapcar (lambda (p)
                               (efrit-tool-file-info--single p project-root))
                             paths)))

        (efrit-tool-success
         `((files . ,(vconcat results))
           (queried . ,(length paths))
           (project_root . ,project-root))
         warnings)))))

(provide 'efrit-tool-file-info)

;;; efrit-tool-file-info.el ends here
