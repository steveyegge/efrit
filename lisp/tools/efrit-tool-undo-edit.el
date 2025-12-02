;;; efrit-tool-undo-edit.el --- Per-file undo editing tool -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Keywords: ai, tools, undo
;; Version: 0.4.1

;;; Commentary:
;;
;; This tool provides Claude with the ability to undo individual file edits.
;;
;; Key features:
;; - Per-file undo tracking (not session-level like checkpoint)
;; - Reverts most recent edit to a specific file
;; - Stores original content automatically when files are edited
;; - Complementary to checkpoint/restore (which works at session level)
;;
;; Usage pattern:
;; 1. Edit file with edit_file
;; 2. Run tests
;; 3. If tests fail, call undo_edit to revert just that file
;; 4. Try a different approach

;;; Code:

(require 'efrit-tool-utils)
(require 'cl-lib)

;;; Undo Registry Management

(defcustom efrit-undo-edit-dir "~/.emacs.d/.efrit/undo/"
  "Directory to store per-file undo history."
  :type 'directory
  :group 'efrit-tool-utils)

(defcustom efrit-undo-edit-max-versions 10
  "Maximum number of undo versions to keep per file."
  :type 'integer
  :group 'efrit-tool-utils)

(defun efrit-undo-edit--ensure-dir ()
  "Ensure undo directory exists."
  (let ((dir (expand-file-name efrit-undo-edit-dir)))
    (unless (file-exists-p dir)
      (make-directory dir t))))

(defun efrit-undo-edit--make-file-key (file-path)
  "Create a unique key for FILE-PATH for storage.
Encodes absolute paths as md5 hashes to avoid filesystem path issues."
  (substring (md5 file-path) 0 12))

(defun efrit-undo-edit--undo-file-for-path (file-path)
  "Get path to undo history file for FILE-PATH."
  (expand-file-name
   (format "%s.undo" (efrit-undo-edit--make-file-key file-path))
   efrit-undo-edit-dir))

(defun efrit-undo-edit--metadata-file-for-path (file-path)
  "Get path to metadata file for FILE-PATH.
Metadata stores the original file path for reverse lookup."
  (expand-file-name
   (format "%s.meta" (efrit-undo-edit--make-file-key file-path))
   efrit-undo-edit-dir))

(defun efrit-undo-edit--read-metadata (file-path)
  "Read metadata for FILE-PATH.
Returns alist with :original_path and :edits count."
  (let ((meta-file (efrit-undo-edit--metadata-file-for-path file-path)))
    (if (file-exists-p meta-file)
        (condition-case nil
            (with-temp-buffer
              (insert-file-contents meta-file)
              (json-read))
          (error nil))
      nil)))

(defun efrit-undo-edit--write-metadata (file-path metadata)
  "Write METADATA for FILE-PATH."
  (efrit-undo-edit--ensure-dir)
  (let ((meta-file (efrit-undo-edit--metadata-file-for-path file-path)))
    (with-temp-file meta-file
      (insert (json-encode metadata)))))

(defun efrit-undo-edit--read-versions (file-path)
  "Read all versions for FILE-PATH.
Returns list of (timestamp . content) pairs in reverse order (newest first)."
  (let ((undo-file (efrit-undo-edit--undo-file-for-path file-path)))
    (if (file-exists-p undo-file)
        (condition-case nil
            (with-temp-buffer
              (insert-file-contents undo-file)
              (let ((versions (json-read)))
                ;; JSON reads into alist, already in newest-first order
                versions))
          (error nil))
      nil)))

(defun efrit-undo-edit--write-versions (file-path versions)
  "Write VERSIONS for FILE-PATH.
VERSIONS is list of (timestamp . content) pairs."
  (efrit-undo-edit--ensure-dir)
  (let ((undo-file (efrit-undo-edit--undo-file-for-path file-path)))
    (with-temp-file undo-file
      (insert (json-encode versions)))))

(defun efrit-undo-edit--add-version (file-path content)
  "Add a new version for FILE-PATH with CONTENT.
Keeps only the last `efrit-undo-edit-max-versions' versions."
  (let* ((timestamp (format-time-string "%Y-%m-%dT%H:%M:%SZ" (current-time)))
         (new-entry `((timestamp . ,timestamp) (content . ,content)))
         (existing (efrit-undo-edit--read-versions file-path))
         ;; Keep only max versions (newest first)
         (trimmed (seq-take (cons new-entry existing) 
                           efrit-undo-edit-max-versions)))
    (efrit-undo-edit--write-versions file-path trimmed)
    timestamp))

;;; Undo Edit Tool

(defun efrit-tool-undo-edit (args)
  "Undo the most recent edit to a file.

ARGS is an alist with:
  path - file to undo (required, must be absolute)

Returns a standard tool response with undo info."
  (efrit-tool-execute undo_edit args
    (let* ((path-input (or (alist-get 'path args) "")))

      ;; Validate required inputs
      (when (string-empty-p path-input)
        (signal 'user-error (list "Path is required")))

      ;; Resolve path with sandbox check
      (let* ((path-info (efrit-resolve-path path-input))
             (path (plist-get path-info :path))
             (path-relative (plist-get path-info :path-relative)))

        ;; Check if file exists
        (unless (file-exists-p path)
          (signal 'user-error (list "File not found" path)))

        ;; Check it's not a directory
        (when (file-directory-p path)
          (signal 'user-error (list "Path is a directory, not a file" path)))

        ;; Check if undo history exists
        (let ((versions (efrit-undo-edit--read-versions path)))
          (unless versions
            (signal 'user-error 
                    (list "No undo history for this file. Did you edit it in this session?")))

          ;; Get the most recent version (first in list)
          (let* ((latest-version (car versions))
                 (timestamp (alist-get 'timestamp latest-version))
                 (original-content (alist-get 'content latest-version))
                 (current-content (with-temp-buffer
                                   (insert-file-contents path)
                                   (buffer-string))))

            ;; Write back the previous version
            (with-temp-file path
              (insert original-content))

            ;; Remove this version from history (so we can undo again if needed)
            (let ((remaining (cdr versions)))
              (if remaining
                  (efrit-undo-edit--write-versions path remaining)
                ;; No more versions, clean up the files
                (let ((undo-file (efrit-undo-edit--undo-file-for-path path))
                      (meta-file (efrit-undo-edit--metadata-file-for-path path)))
                  (when (file-exists-p undo-file)
                    (delete-file undo-file))
                  (when (file-exists-p meta-file)
                    (delete-file meta-file)))))

            ;; Return success with diff info
            (let ((diff-output (efrit-tool-edit-file--generate-diff
                               original-content current-content path)))
              (efrit-tool-success
               `((path . ,path)
                 (path_relative . ,path-relative)
                 (undid_edit_at . ,timestamp)
                 (remaining_versions . ,(length (cdr versions)))
                 (diff . ,diff-output))))))))))

;;; Hook Integration - Track edits automatically

;; Helper to register edit with undo system
(defun efrit-undo-edit--register-edit (file-path old-content)
  "Register an edit: save OLD-CONTENT as undo point for FILE-PATH."
  (efrit-undo-edit--add-version file-path old-content)
  ;; Update metadata
  (let ((existing-meta (efrit-undo-edit--read-metadata file-path)))
    (efrit-undo-edit--write-metadata 
     file-path
     `((original_path . ,file-path)
       (edit_count . ,(1+ (or (alist-get 'edit_count existing-meta) 0)))
       (last_edited_at . ,(format-time-string "%Y-%m-%dT%H:%M:%SZ" 
                                             (current-time)))))))

;; Note: edit_file and create_file tools should call this after writing files
;; This is done by modifying those tools' implementations

(provide 'efrit-tool-undo-edit)

;;; efrit-tool-undo-edit.el ends here
