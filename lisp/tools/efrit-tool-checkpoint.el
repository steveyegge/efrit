;;; efrit-tool-checkpoint.el --- Checkpoint/restore tools -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Keywords: ai, tools, git
;; Version: 0.3.0

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

(require 'efrit-tool-utils)
(require 'cl-lib)
(require 'json)

;;; Customization

(defcustom efrit-checkpoint-dir "~/.emacs.d/.efrit/checkpoints/"
  "Directory to store checkpoint metadata."
  :type 'directory
  :group 'efrit-tool-utils)

(defcustom efrit-checkpoint-max-age-hours 24
  "Auto-expire checkpoints older than this many hours."
  :type 'integer
  :group 'efrit-tool-utils)

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

;;; Git Stash Integration

(defun efrit-checkpoint--create-stash (message)
  "Create a git stash with MESSAGE.
Returns plist with :success, :stash-ref, :error."
  (let* ((stash-msg (format "efrit-checkpoint: %s" message))
         ;; First check if there are changes to stash
         (status-result (efrit-tool-run-git '("status" "--porcelain")))
         (has-changes (and (plist-get status-result :success)
                          (not (string-empty-p
                                (string-trim (plist-get status-result :output)))))))
    (if (not has-changes)
        (list :success nil :error "No changes to checkpoint")
      ;; Create stash including untracked files
      (let ((stash-result (efrit-tool-run-git
                          (list "stash" "push"
                                "--include-untracked"
                                "-m" stash-msg))))
        (if (plist-get stash-result :success)
            ;; Get the stash reference
            (let* ((list-result (efrit-tool-run-git '("stash" "list" "-n" "1")))
                   (stash-ref (when (plist-get list-result :success)
                               (car (split-string
                                     (plist-get list-result :output)
                                     ":")))))
              (list :success t :stash-ref (or stash-ref "stash@{0}")))
          (list :success nil
                :error (or (plist-get stash-result :error) "Failed to create stash")))))))

(defun efrit-checkpoint--apply-stash (stash-ref &optional pop)
  "Apply stash at STASH-REF.
If POP is non-nil, drop the stash after applying."
  (let ((cmd (if pop "pop" "apply")))
    (efrit-tool-run-git (list "stash" cmd stash-ref))))

(defun efrit-checkpoint--drop-stash (stash-ref)
  "Drop stash at STASH-REF."
  (efrit-tool-run-git (list "stash" "drop" stash-ref)))

(defun efrit-checkpoint--find-stash-by-message (checkpoint-id)
  "Find stash reference containing CHECKPOINT-ID in message."
  (let ((list-result (efrit-tool-run-git '("stash" "list"))))
    (when (plist-get list-result :success)
      (let ((lines (split-string (plist-get list-result :output) "\n" t)))
        (cl-loop for line in lines
                 when (string-match-p (regexp-quote checkpoint-id) line)
                 return (car (split-string line ":")))))))

;;; Checkpoint Tool

(defun efrit-tool-checkpoint (args)
  "Create a restore point before risky operations.

ARGS is an alist with:
  description - what operation we're about to do (required)

Returns a standard tool response with checkpoint info."
  (efrit-tool-execute checkpoint args
    (let* ((description (alist-get 'description args)))

      ;; Validate
      (unless description
        (signal 'user-error (list "description is required")))

      ;; Check git availability
      (unless (efrit-tool-git-available-p)
        (signal 'user-error (list "Not a git repository or git not available")))

      ;; Generate checkpoint ID
      (let* ((checkpoint-id (efrit-checkpoint--generate-id))
             (stash-message (format "%s | %s" checkpoint-id description))
             (stash-result (efrit-checkpoint--create-stash stash-message)))

        (if (not (plist-get stash-result :success))
            ;; Failed to create stash
            (efrit-tool-success
             `((created . :json-false)
               (reason . ,(plist-get stash-result :error))
               (checkpoint_id . nil)))

          ;; Success - save to registry
          (let* ((metadata `((description . ,description)
                             (created_at . ,(efrit-tool-format-time nil))
                             (stash_ref . ,(plist-get stash-result :stash-ref))
                             (project_root . ,(efrit-tool--get-project-root)))))
            (efrit-checkpoint--add-to-registry checkpoint-id metadata)

            (efrit-tool-success
             `((created . t)
               (checkpoint_id . ,checkpoint-id)
               (description . ,description)
               (stash_ref . ,(plist-get stash-result :stash-ref))
               (method . "git_stash")
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
    (let* ((checkpoint-id (alist-get 'checkpoint_id args))
           (keep-checkpoint (alist-get 'keep_checkpoint args)))

      ;; Validate
      (unless checkpoint-id
        (signal 'user-error (list "checkpoint_id is required")))

      ;; Check git availability
      (unless (efrit-tool-git-available-p)
        (signal 'user-error (list "Not a git repository or git not available")))

      ;; Look up checkpoint
      (let ((metadata (efrit-checkpoint--get-from-registry checkpoint-id)))
        (unless metadata
          (signal 'user-error
                  (list (format "Checkpoint not found: %s" checkpoint-id))))

        ;; Find the stash
        (let ((stash-ref (efrit-checkpoint--find-stash-by-message checkpoint-id)))
          (unless stash-ref
            (signal 'user-error
                    (list (format "Stash for checkpoint not found (may have been manually removed): %s"
                                 checkpoint-id))))

          ;; Apply the stash
          (let ((apply-result (efrit-checkpoint--apply-stash
                               stash-ref
                               (not keep-checkpoint))))
            (if (not (plist-get apply-result :success))
                ;; Failed to apply
                (efrit-tool-error
                 'execution_error
                 (format "Failed to restore checkpoint: %s"
                        (or (plist-get apply-result :error) "unknown error"))
                 `((checkpoint_id . ,checkpoint-id)))

              ;; Success
              (unless keep-checkpoint
                (efrit-checkpoint--remove-from-registry checkpoint-id))

              (efrit-tool-success
               `((restored . t)
                 (checkpoint_id . ,checkpoint-id)
                 (description . ,(alist-get 'description metadata))
                 (kept . ,(if keep-checkpoint t :json-false)))))))))))

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
                        `((checkpoint_id . ,id)
                          (description . ,(alist-get 'description meta))
                          (created_at . ,(alist-get 'created_at meta))
                          (stash_ref . ,(alist-get 'stash_ref meta)))))
                    registry)))
      (efrit-tool-success
       `((checkpoints . ,(vconcat checkpoints))
         (count . ,(length checkpoints)))))))

;;; Delete Checkpoint Tool

(defun efrit-tool-delete-checkpoint (args)
  "Delete a checkpoint without restoring.

ARGS is an alist with:
  checkpoint_id - which checkpoint to delete (required)

Returns a standard tool response."
  (efrit-tool-execute delete_checkpoint args
    (let* ((checkpoint-id (alist-get 'checkpoint_id args)))

      ;; Validate
      (unless checkpoint-id
        (signal 'user-error (list "checkpoint_id is required")))

      ;; Check git availability
      (unless (efrit-tool-git-available-p)
        (signal 'user-error (list "Not a git repository or git not available")))

      ;; Look up checkpoint
      (let ((metadata (efrit-checkpoint--get-from-registry checkpoint-id)))
        (unless metadata
          (signal 'user-error
                  (list (format "Checkpoint not found: %s" checkpoint-id))))

        ;; Find and drop the stash
        (let ((stash-ref (efrit-checkpoint--find-stash-by-message checkpoint-id)))
          (when stash-ref
            (efrit-checkpoint--drop-stash stash-ref))

          ;; Remove from registry
          (efrit-checkpoint--remove-from-registry checkpoint-id)

          (efrit-tool-success
           `((deleted . t)
             (checkpoint_id . ,checkpoint-id)
             (stash_dropped . ,(if stash-ref t :json-false)))))))))

(provide 'efrit-tool-checkpoint)

;;; efrit-tool-checkpoint.el ends here
