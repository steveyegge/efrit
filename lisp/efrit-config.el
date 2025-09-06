;;; efrit-config.el --- Configuration management for Efrit -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai, config
;; URL: https://github.com/stevey/efrit

;;; Commentary:
;; Centralized configuration management for Efrit, including data directory
;; organization and path management.

;;; Code:

(require 'files)

;;; Customization

(defgroup efrit nil
  "AI assistant integration for Emacs."
  :group 'applications
  :prefix "efrit-")

(defcustom efrit-data-directory "~/.emacs.d/.efrit"
  "Directory for storing efrit data files.
This includes context files, session data, communication queues, logs,
and cache. The directory will be created automatically if it doesn't exist."
  :type 'directory
  :group 'efrit
  :set (lambda (symbol value)
         (set-default symbol (expand-file-name value))))

;;; Directory Management

(defun efrit-config--ensure-directories ()
  "Ensure all efrit data directories exist."
  (let ((base-dir efrit-data-directory))
    (when base-dir
      (dolist (subdir '("" "cache" "sessions" "queues" "queues/requests" 
                       "queues/processing" "queues/responses" "queues/archive"
                       "logs" "context" "workspace" "workspace/auto-saves" 
                       "workspace/backups"))
        (let ((dir (if (string= "" subdir)
                      base-dir 
                    (expand-file-name subdir base-dir))))
          (unless (file-directory-p dir)
            (make-directory dir t)))))))

(defun efrit-config-data-file (filename &optional subdir)
  "Return full path for FILENAME in efrit data directory.
Optional SUBDIR specifies a subdirectory within the data directory."
  (let ((dir (if subdir 
                (expand-file-name subdir efrit-data-directory)
              efrit-data-directory)))
    (expand-file-name filename dir)))

(defun efrit-config-queue-dir (&optional subdir)
  "Return path to efrit queue directory.
Optional SUBDIR specifies a subdirectory within queues/ (e.g., \\='requests\\=')."
  (efrit-config-data-file (or subdir "") "queues"))

(defun efrit-config-workspace-dir (&optional subdir)
  "Return path to efrit workspace directory.
Optional SUBDIR specifies a subdirectory within workspace/."
  (efrit-config-data-file (or subdir "") "workspace"))

(defun efrit-config-context-file (filename)
  "Return full path for context FILENAME."
  (efrit-config-data-file filename "context"))

(defun efrit-config-log-file (filename)
  "Return full path for log FILENAME."
  (efrit-config-data-file filename "logs"))

(defun efrit-config-cache-file (filename)
  "Return full path for cache FILENAME."
  (efrit-config-data-file filename "cache"))

;;; Migration Support

(defun efrit-config-migrate-old-files ()
  "Migrate efrit files from old locations to new data directory structure.
This is called automatically when the data directory is initialized."
  (let ((old-files `(("~/.emacs.d/efrit-do-context.el" . ,(efrit-config-context-file "efrit-do-context.el"))
                     ("~/.emacs.d/efrit-queue" . ,(efrit-config-queue-dir))
                     ("~/.emacs.d/efrit-queue-ai" . ,(efrit-config-data-file "queue-ai"))
                     ("~/.emacs.d/efrit-ai-workspace" . ,(efrit-config-workspace-dir)))))
    (dolist (mapping old-files)
      (let ((old-path (expand-file-name (car mapping)))
            (new-path (cdr mapping)))
        (when (file-exists-p old-path)
          (unless (file-exists-p new-path)
            (if (file-directory-p old-path)
                (copy-directory old-path new-path nil t t)
              (copy-file old-path new-path))
            (message "Migrated %s -> %s" old-path new-path)))))))

;;; Initialization

(defun efrit-config-initialize ()
  "Initialize efrit configuration and data directories."
  (efrit-config--ensure-directories)
  (efrit-config-migrate-old-files))

;; Initialize on load
(efrit-config-initialize)

(provide 'efrit-config)

;;; Model Configuration

(defcustom efrit-default-model "claude-3-5-sonnet-20241022"
  "Default Claude model for all efrit operations."
  :type 'string
  :group 'efrit)

(defcustom efrit-completion-model "claude-3-5-sonnet-20241022"
  "Claude model for completion assessment and lightweight operations."
  :type 'string
  :group 'efrit)

(defcustom efrit-agent-model "claude-4-sonnet-20250514"
  "Claude model for agent operations."
  :type 'string
  :group 'efrit)

(defcustom efrit-default-max-tokens 8192
  "Default maximum tokens for responses."
  :type 'integer
  :group 'efrit)

;;; efrit-config.el ends here
