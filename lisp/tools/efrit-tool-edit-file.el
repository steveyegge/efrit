;;; efrit-tool-edit-file.el --- Surgical file editing tool -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Keywords: ai, tools
;; Version: 0.4.1

;;; Commentary:
;;
;; This tool provides Claude with the ability to make surgical edits to files
;; using find-and-replace. This is the PRIMARY tool for file editing.
;;
;; Key features:
;; - Exact string matching (old_str must match exactly)
;; - Uniqueness check (old_str must be unique unless replace_all=true)
;; - Git-style diff output showing changes
;; - Automatic file saving
;; - Sandbox enforcement (paths must stay within project root)

;;; Code:

(require 'efrit-tool-utils)
(require 'cl-lib)
(require 'diff)

;;; Customization

(defcustom efrit-tool-edit-file-backup t
  "When non-nil, create backup before editing files."
  :type 'boolean
  :group 'efrit-tool-utils)

(defcustom efrit-tool-edit-file-max-size 1000000
  "Maximum file size in bytes for editing (default: 1MB)."
  :type 'integer
  :group 'efrit-tool-utils)

;;; Diff Generation

(defun efrit-tool-edit-file--generate-diff (old-content new-content file-path)
  "Generate a unified diff between OLD-CONTENT and NEW-CONTENT.
FILE-PATH is used for the diff header."
  (let ((old-file (make-temp-file "efrit-old-"))
        (new-file (make-temp-file "efrit-new-"))
        diff-output)
    (unwind-protect
        (progn
          (with-temp-file old-file
            (insert old-content))
          (with-temp-file new-file
            (insert new-content))
          (with-temp-buffer
            (call-process "diff" nil t nil
                          "-u"
                          "--label" (format "a/%s" (file-name-nondirectory file-path))
                          "--label" (format "b/%s" (file-name-nondirectory file-path))
                          old-file new-file)
            (setq diff-output (buffer-string))))
      (delete-file old-file)
      (delete-file new-file))
    diff-output))

;;; Main Implementation

(defun efrit-tool-edit-file (args)
  "Edit a file by replacing old_str with new_str.

ARGS is an alist with:
  path        - file to edit (required, must be absolute)
  old_str     - exact text to find (required)
  new_str     - text to replace with (required)
  replace_all - if true, replace all occurrences (default: false)

Returns a standard tool response with diff showing changes."
  (efrit-tool-execute edit_file args
    (let* ((path-input (or (alist-get 'path args) ""))
           (old-str (alist-get 'old_str args))
           (new-str (alist-get 'new_str args))
           (replace-all (efrit-json-bool (alist-get 'replace_all args)))
           (warnings '()))

      ;; Validate required inputs
      (when (string-empty-p path-input)
        (signal 'user-error (list "Path is required")))
      (unless old-str
        (signal 'user-error (list "old_str is required")))
      (when (string-empty-p old-str)
        (signal 'user-error (list "old_str must be a non-empty string")))
      (unless new-str
        (signal 'user-error (list "new_str is required")))

      ;; old_str and new_str must be different
      (when (string= old-str new-str)
        (signal 'user-error (list "old_str and new_str must be different")))

      ;; Resolve path with sandbox check
      (let* ((path-info (efrit-resolve-path path-input))
             (path (plist-get path-info :path))
             (path-relative (plist-get path-info :path-relative)))

        ;; Check file exists
        (unless (file-exists-p path)
          (signal 'file-error (list "File not found" path)))

        ;; Check it's not a directory
        (when (file-directory-p path)
          (signal 'user-error (list "Path is a directory, not a file" path)))

        ;; Check for sensitive file
        (when (plist-get path-info :is-sensitive)
          (push (format "Editing sensitive file: %s" path-relative) warnings))

        ;; Check file size
        (let ((size (file-attribute-size (file-attributes path))))
          (when (> size efrit-tool-edit-file-max-size)
            (signal 'user-error
                    (list (format "File too large (%d bytes, max %d)"
                                  size efrit-tool-edit-file-max-size)))))

        ;; Check if binary
        (when (efrit-tool-binary-file-p path 8192)
          (signal 'user-error (list "Cannot edit binary file" path)))

        ;; Read the file content
        (let* ((original-content (with-temp-buffer
                                   (insert-file-contents path)
                                   (buffer-string)))
               (match-count (with-temp-buffer
                              (insert original-content)
                              (goto-char (point-min))
                              (let ((count 0))
                                (while (search-forward old-str nil t)
                                  (cl-incf count))
                                count))))

          ;; Check for matches
          (cond
           ((= match-count 0)
            (signal 'user-error
                    (list "old_str not found in file"
                          (format "Searched for: %s"
                                  (if (> (length old-str) 100)
                                      (concat (substring old-str 0 100) "...")
                                    old-str)))))

           ((and (> match-count 1) (not replace-all))
            (signal 'user-error
                    (list (format "old_str appears %d times in file. Use replace_all=true to replace all, or add more context to make it unique."
                                  match-count)))))

          ;; Create backup if enabled
          (when efrit-tool-edit-file-backup
            (let ((backup-path (concat path "~")))
              (copy-file path backup-path t)))

          ;; Perform the replacement
          (let ((new-content (with-temp-buffer
                               (insert original-content)
                               (goto-char (point-min))
                               (if replace-all
                                   (while (search-forward old-str nil t)
                                     (replace-match new-str t t))
                                 (when (search-forward old-str nil t)
                                   (replace-match new-str t t)))
                               (buffer-string))))

            ;; Generate diff before saving
            (let ((diff-output (efrit-tool-edit-file--generate-diff
                                original-content new-content path)))

              ;; Write the new content
              (with-temp-file path
                (insert new-content))

              ;; Return success with diff
              (efrit-tool-success
               `((path . ,path)
                 (path_relative . ,path-relative)
                 (replacements . ,match-count)
                 (diff . ,diff-output)
                 (old_size . ,(length original-content))
                 (new_size . ,(length new-content)))
               warnings))))))))

(provide 'efrit-tool-edit-file)

;;; efrit-tool-edit-file.el ends here
