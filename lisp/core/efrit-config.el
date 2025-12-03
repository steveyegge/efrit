;;; efrit-config.el --- Configuration management for Efrit -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai, config
;; URL: https://github.com/stevey/efrit

;;; Commentary:
;; Centralized configuration management for Efrit, including data directory
;; organization and path management.

;;; Code:

(require 'files)

;;; Version Management

(defconst efrit-version "0.4.1"
  "Version number of Efrit.
This is the canonical version used throughout the project.
Update this when releasing new versions.")

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

;; Control whether configuration auto-initializes on load.
;; Default is t for backward compatibility; users of use-package can set
;; this to nil in :init to avoid side effects during load.
(defcustom efrit-auto-initialize t
  "Whether Efrit should initialize data directories automatically on load."
  :type 'boolean
  :group 'efrit)

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

;; Initialize on load only if enabled (default t for backward compatibility)
(when efrit-auto-initialize
  (efrit-config-initialize))

(provide 'efrit-config)

;;; Session Safety Limits
;; These are the canonical, shared limits for all Efrit modes (chat, do, executor).
;; Individual modules may provide local overrides, but these are the defaults.

(defcustom efrit-max-retries 3
  "Maximum retry attempts when tool execution fails.
Shared default for chat and do modes."
  :type 'integer
  :group 'efrit)

(defcustom efrit-retry-on-errors t
  "Whether to automatically retry failed tool executions.
When non-nil, errors are sent back to Claude for correction."
  :type 'boolean
  :group 'efrit)

(defcustom efrit-max-tool-calls-per-session 100
  "Maximum tool calls allowed per session (safety limit).
Prevents runaway sessions from executing indefinitely."
  :type 'integer
  :group 'efrit)

(defcustom efrit-max-continuations-per-session 50
  "Maximum API calls (turns) per session before emergency stop.
Most tasks complete within 20 turns; complex exploration may need more."
  :type 'integer
  :group 'efrit)

(defcustom efrit-session-timeout 300
  "Maximum seconds for a session before timeout.
Default is 5 minutes (300 seconds)."
  :type 'integer
  :group 'efrit)

(defcustom efrit-max-eval-per-session 100
  "Maximum eval_sexp calls allowed per session.
Set to 0 to disable this specific limit."
  :type 'integer
  :group 'efrit)

;;; Model Configuration

(defcustom efrit-default-model "claude-sonnet-4-5-20250929"
  "Default Claude model for all efrit operations."
  :type 'string
  :group 'efrit)

;; Backwards compatibility alias - declare it before the variable
(defvaralias 'efrit-max-tokens 'efrit-default-max-tokens)

(defcustom efrit-default-max-tokens 8192
  "Default maximum tokens for responses.
Claude 3.5 Sonnet supports up to 8192 tokens with beta headers."
  :type 'integer
  :group 'efrit)

;;; efrit-config.el ends here
