;;; efrit-tool-checkpoint.el --- Checkpoint/restore tools -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Keywords: ai, tools, git
;; Version: 0.6.2

;;; Commentary:
;;
;; Tools for creating and restoring checkpoints before risky operations.
;;
;; Provides two tools:
;; - checkpoint: Create a restore point using git stash
;; - restore_checkpoint: Restore from a previous checkpoint
;;
;; Key features:
;; - Uses git stash for clean implementation
;; - Tracks checkpoint metadata in a local registry
;; - Supports listing and selective restore

;;; Code:

(require 'parse-time)
(require 'efrit-tool-utils)
(require 'efrit-vcs)
(require 'cl-lib)
(require 'iso8601)
(require 'json)

;;; Customization

(defcustom efrit-checkpoint-dir
  (expand-file-name "checkpoints" efrit-data-directory)
  "Directory to store checkpoint metadata."
  :type 'directory
  :group 'efrit-tool-utils)

(defcustom efrit-checkpoint-max-age-hours 24
  "Checkpoints older than this many hours are reported as expired.
`list_checkpoints' marks them with `expired: true' and an age so the
model (or user) can decide to delete them; nothing is removed
automatically, since the underlying git stash may still be wanted."
  :type 'integer
  :group 'efrit-tool-utils)

(defun efrit-checkpoint--age-hours (created-at)
  "Hours since CREATED-AT (an ISO 8601 string), or nil if unparsable."
  (when (stringp created-at)
    (condition-case nil
        (/ (float-time (time-since (parse-iso8601-time-string created-at)))
           3600.0)
      (error nil))))

;;; Registry Management

(defun efrit-checkpoint--registry-file ()
  "Get path to the checkpoint registry file."
  (expand-file-name "registry.json" efrit-checkpoint-dir))

(defun efrit-checkpoint--ensure-dir ()
  "Ensure checkpoint directory exists."
  (let ((dir (expand-file-name efrit-checkpoint-dir)))
    (unless (file-exists-p dir)
      (make-directory dir t))))

(defun efrit-checkpoint--read-registry ()
  "Read the checkpoint registry.
Returns alist of checkpoint-id -> metadata."
  (efrit-checkpoint--ensure-dir)
  (let ((file (efrit-checkpoint--registry-file)))
    (if (file-exists-p file)
        (condition-case nil
            (with-temp-buffer
              (insert-file-contents file)
              (json-read))
          (error nil))
      nil)))

(defun efrit-checkpoint--write-registry (registry)
  "Write REGISTRY to the checkpoint file."
  (efrit-checkpoint--ensure-dir)
  (let ((file (efrit-checkpoint--registry-file)))
    (with-temp-file file
      (insert (json-encode registry)))))

(defun efrit-checkpoint--add-to-registry (checkpoint-id metadata)
  "Add CHECKPOINT-ID with METADATA to registry."
  (let* ((registry (or (efrit-checkpoint--read-registry) nil))
         (updated (cons (cons checkpoint-id metadata) registry)))
    (efrit-checkpoint--write-registry updated)
    updated))

(defun efrit-checkpoint--remove-from-registry (checkpoint-id)
  "Remove CHECKPOINT-ID from registry.
Note: JSON decodes string keys as symbols, so we check both."
  (let* ((registry (efrit-checkpoint--read-registry))
         (id-sym (intern checkpoint-id))
         (updated (cl-remove-if (lambda (entry)
                                  (let ((key (car entry)))
                                    (or (equal key checkpoint-id)
                                        (eq key id-sym))))
                                registry)))
    (efrit-checkpoint--write-registry updated)
    updated))

(defun efrit-checkpoint--get-from-registry (checkpoint-id)
  "Get metadata for CHECKPOINT-ID from registry.
Note: JSON decodes string keys as symbols, so we check both."
  (let ((registry (efrit-checkpoint--read-registry)))
    (or (alist-get checkpoint-id registry nil nil #'equal)
        (alist-get (intern checkpoint-id) registry))))

;;; Checkpoint ID Generation

(defun efrit-checkpoint--generate-id ()
  "Generate a unique checkpoint ID."
  (format "efrit-%s-%s"
          (format-time-string "%Y%m%d-%H%M%S")
          (substring (md5 (format "%s%s" (random) (current-time))) 0 6)))

;;; Storage: a Git stash, or a file snapshot
;;
;; In a Git tree the checkpoint is a real stash named
;; "efrit-checkpoint ID: DESCRIPTION", so `git stash list' and Magit
;; show where it came from.  Elsewhere (no Git, or the tree is not
;; under version control) the changed files are copied under
;; .efrit/checkpoints/ID/ and copied back on restore.

(defun efrit-checkpoint--root ()
  "The project root checkpoints are taken in."
  (file-name-as-directory (efrit-tool--get-project-root)))

(defun efrit-checkpoint--method (root)
  "How checkpoints are stored for ROOT: `stash' or `snapshot'."
  (if (efrit-vcs-git-p root) 'stash 'snapshot))

(defun efrit-checkpoint--stored-method (metadata)
  "The method recorded in METADATA as a symbol.
The registry is JSON: written as a string, read back as a symbol (the
reader interns object keys and, with `json-object-type' alist, some
values come back as symbols too); older entries have no method."
  (let ((m (alist-get 'method metadata)))
    (cond ((null m) 'stash)
          ((symbolp m) m)
          ((stringp m) (intern m))
          (t 'stash))))

;;; Checkpoint Tool

(defun efrit-tool-checkpoint (args)
  "Create a restore point before risky operations.

ARGS is an alist with:
  description - what operation we're about to do (required)

Returns a standard tool response with checkpoint info."
  (efrit-tool-execute checkpoint args
    (efrit-resolve-path nil 'write "checkpoint")
    (let* ((description (alist-get 'description args))
           (root (efrit-checkpoint--root))
           (method (efrit-checkpoint--method root)))
      (unless description
        (signal 'user-error (list "description is required")))
      (let* ((checkpoint-id (efrit-checkpoint--generate-id))
             (ref nil) (count nil) (failure nil))
        (condition-case err
            (pcase method
              ('stash (setq ref (efrit-vcs-stash-push checkpoint-id description root)))
              (_ (setq count (efrit-vcs-snapshot-create checkpoint-id root))
                 (when (zerop count) (setq failure "No changed files to checkpoint"))))
          (efrit-vcs-error (setq failure (cadr err))))
        (if failure
            (efrit-tool-success
             `((created . :json-false)
               (reason . ,failure)
               (checkpoint_id . nil)))
          (let ((metadata `((description . ,description)
                            (created_at . ,(efrit-tool-format-time nil))
                            (method . ,(symbol-name method))
                            (stash_ref . ,ref)
                            (project_root . ,root))))
            (efrit-checkpoint--add-to-registry checkpoint-id metadata)
            (efrit-tool-success
             `((created . t)
               (checkpoint_id . ,checkpoint-id)
               (description . ,description)
               (method . ,(if (eq method 'stash) "git_stash" "file_snapshot"))
               ,@(when ref `((stash_ref . ,ref)
                             (stash_name . ,(efrit-vcs-stash-name checkpoint-id description))))
               ,@(when count `((files_saved . ,count)))
               (restore_command . ,(format "Use restore_checkpoint with checkpoint_id: %s"
                                          checkpoint-id))))))))))

;;; Restore Checkpoint Tool

(defun efrit-tool-restore-checkpoint (args)
  "Restore from a previous checkpoint.

ARGS is an alist with:
  checkpoint_id - which checkpoint to restore (required)
  keep_checkpoint - if true, don't delete the checkpoint after restore

Returns a standard tool response with restore result."
  (efrit-tool-execute restore_checkpoint args
    (efrit-resolve-path nil 'write "restore_checkpoint")
    (let* ((checkpoint-id (alist-get 'checkpoint_id args))
           (keep-checkpoint (alist-get 'keep_checkpoint args))
           (root (efrit-checkpoint--root)))
      (unless checkpoint-id
        (signal 'user-error (list "checkpoint_id is required")))
      (let ((metadata (efrit-checkpoint--get-from-registry checkpoint-id)))
        (unless metadata
          (signal 'user-error (list (format "Checkpoint not found: %s" checkpoint-id))))
        (let* ((method (efrit-checkpoint--stored-method metadata))
               (failure
                (condition-case err
                    (progn
                      (pcase method
                        ('stash (efrit-vcs-stash-apply checkpoint-id (not keep-checkpoint) root))
                        (_ (efrit-vcs-snapshot-restore checkpoint-id root)
                           (unless keep-checkpoint (efrit-vcs-snapshot-delete checkpoint-id root))))
                      nil)
                  ;; every failure, VC's own included, must reach the
                  ;; caller with its text: a plain value from
                  ;; `efrit-tool-error' here was dropped and the tool
                  ;; went on to report success (2026-09-28)
                  (error (error-message-string err)))))
          (if failure
              (efrit-tool-error 'execution_error
                                (format "Failed to restore checkpoint: %s" failure)
                                `((checkpoint_id . ,checkpoint-id) (method . ,(symbol-name method))))
            (unless keep-checkpoint
              (efrit-checkpoint--remove-from-registry checkpoint-id))
            (efrit-tool-success
             `((restored . t)
               (checkpoint_id . ,checkpoint-id)
               (description . ,(alist-get 'description metadata))
               (method . ,(symbol-name method))
               (kept . ,(if keep-checkpoint t :json-false))))))))))

;;; List Checkpoints Tool

(defun efrit-tool-list-checkpoints (_args)
  "List all available checkpoints.

Returns a standard tool response with checkpoint list."
  (efrit-tool-execute list_checkpoints nil
    (let* ((registry (efrit-checkpoint--read-registry))
           (checkpoints
            (mapcar (lambda (entry)
                      (let ((id (car entry))
                            (meta (cdr entry)))
                        (let* ((created (alist-get 'created_at meta))
                               (age (efrit-checkpoint--age-hours created)))
                          `((checkpoint_id . ,id)
                            (description . ,(alist-get 'description meta))
                            (created_at . ,created)
                            (method . ,(or (alist-get 'method meta) "stash"))
                            (stash_ref . ,(alist-get 'stash_ref meta))
                            ,@(when age
                                `((age_hours . ,(/ (round (* age 10)) 10.0))
                                  (expired . ,(if (> age efrit-checkpoint-max-age-hours)
                                                  t :json-false))))))))
                    registry))
           (expired (cl-count-if (lambda (c) (eq (alist-get 'expired c) t))
                                 checkpoints)))
      (efrit-tool-success
       `((checkpoints . ,(vconcat checkpoints))
         (count . ,(length checkpoints))
         (expired_count . ,expired)
         ,@(when (> expired 0)
             `((note . ,(format "%d checkpoint(s) older than %d hours; consider delete_checkpoint"
                                expired efrit-checkpoint-max-age-hours)))))))))

;;; Delete Checkpoint Tool

(defun efrit-tool-delete-checkpoint (args)
  "Delete a checkpoint without restoring.

ARGS is an alist with:
  checkpoint_id - which checkpoint to delete (required)

Returns a standard tool response."
  (efrit-tool-execute delete_checkpoint args
    (efrit-resolve-path nil 'write "delete_checkpoint")
    (let* ((checkpoint-id (alist-get 'checkpoint_id args))
           (root (efrit-checkpoint--root)))
      (unless checkpoint-id
        (signal 'user-error (list "checkpoint_id is required")))
      (let ((metadata (efrit-checkpoint--get-from-registry checkpoint-id)))
        (unless metadata
          (signal 'user-error (list (format "Checkpoint not found: %s" checkpoint-id))))
        (let* ((method (efrit-checkpoint--stored-method metadata))
               (dropped (pcase method
                          ('stash (and (efrit-vcs-stash-drop checkpoint-id root) t))
                          (_ (efrit-vcs-snapshot-delete checkpoint-id root)))))
          (efrit-checkpoint--remove-from-registry checkpoint-id)
          (efrit-tool-success
           `((deleted . t)
             (checkpoint_id . ,checkpoint-id)
             (method . ,(symbol-name method))
             (stash_dropped . ,(if dropped t :json-false)))))))))

(provide 'efrit-tool-checkpoint)

;;; efrit-tool-checkpoint.el ends here
