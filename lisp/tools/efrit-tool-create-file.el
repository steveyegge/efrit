;;; efrit-tool-create-file.el --- Atomic file creation tool -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Keywords: ai, tools
;; Version: 0.4.1

;;; Commentary:
;;
;; This tool provides Claude with the ability to atomically create files
;; with content.  This is the PRIMARY tool for creating new files.
;;
;; Key features:
;; - Atomic file creation (directory + file in one operation)
;; - Automatic parent directory creation
;; - Overwrite protection (requires explicit flag)
;; - Sandbox enforcement (paths must stay within project root)
;; - Git-style diff output for new files

;;; Code:

(require 'efrit-tool-utils)
(require 'cl-lib)

;;; Customization

(defcustom efrit-tool-create-file-max-size 1000000
  "Maximum content size in bytes for file creation (default: 1MB)."
  :type 'integer
  :group 'efrit-tool-utils)

;;; Main Implementation

(defun efrit-tool-create-file (args)
  "Create a new file with the given content.

ARGS is an alist with:
  path      - file to create (required, must be absolute)
  content   - file content (required)
  overwrite - if true, allow overwriting existing files (default: false)

Returns a standard tool response with creation details."
  (efrit-tool-execute create_file args
    (let* ((path-input (or (alist-get 'path args) ""))
           (content (alist-get 'content args))
           (overwrite (efrit-json-bool (alist-get 'overwrite args)))
           (warnings '()))

      ;; Validate required inputs
      (when (string-empty-p path-input)
        (signal 'user-error (list "Path is required")))
      (unless content
        (signal 'user-error (list "Content is required")))

      ;; Check content size
      (when (> (length content) efrit-tool-create-file-max-size)
        (signal 'user-error
                (list (format "Content too large (%d bytes, max %d)"
                              (length content) efrit-tool-create-file-max-size))))

      ;; Resolve path with sandbox check
      (let* ((path-info (efrit-resolve-path path-input))
             (path (plist-get path-info :path))
             (path-relative (plist-get path-info :path-relative))
             (parent-dir (file-name-directory path)))

        ;; Check for sensitive file
        (when (plist-get path-info :is-sensitive)
          (push (format "Creating sensitive file: %s" path-relative) warnings))

        ;; Check if path is a directory (must be before file-exists-p check)
        (when (file-directory-p path)
          (signal 'user-error (list "Path is a directory, not a file" path)))

        ;; Check if file already exists
        (when (file-exists-p path)
          (if overwrite
              (push (format "Overwriting existing file: %s" path-relative) warnings)
            (signal 'file-already-exists
                    (list "File already exists (use overwrite=true to replace)" path))))

        ;; Create parent directories if needed
        (unless (file-directory-p parent-dir)
          (make-directory parent-dir t)
          (push (format "Created directory: %s" (file-relative-name parent-dir (plist-get path-info :project-root)))
                warnings))

        ;; Write the file
        (with-temp-file path
          (insert content))

        ;; Generate a diff-like output for new files
        (let* ((line-count (with-temp-buffer
                             (insert content)
                             (count-lines (point-min) (point-max))))
               (diff-output (format "--- /dev/null\n+++ b/%s\n@@ -0,0 +1,%d @@\n%s"
                                    (file-name-nondirectory path)
                                    line-count
                                    (with-temp-buffer
                                      (insert content)
                                      (goto-char (point-min))
                                      (while (not (eobp))
                                        (insert "+")
                                        (forward-line 1))
                                      (buffer-string)))))

          ;; Return success
          (efrit-tool-success
           `((path . ,path)
             (path_relative . ,path-relative)
             (size . ,(length content))
             (lines . ,line-count)
             (diff . ,diff-output)
             (created . t))
           warnings))))))

(provide 'efrit-tool-create-file)

;;; efrit-tool-create-file.el ends here
