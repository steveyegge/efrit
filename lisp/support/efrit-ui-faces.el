;;; efrit-ui-faces.el --- Face definitions for Efrit UI -*- lexical-binding: t -*-

;; Copyright (C) 2025 Steven Yegge

;; Author: Steven Yegge
;; Version: 0.4.0
;; Package-Requires: ((emacs "28.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Face definitions for all Efrit UI components.

;;; Code:

(defgroup efrit-progress nil
  "Progress display for Efrit."
  :group 'efrit)

;;; Progress Faces

(defface efrit-progress-timestamp
  '((t :foreground "gray60"))
  "Face for timestamps in progress display."
  :group 'efrit-progress)

(defface efrit-progress-section-header
  '((t :weight bold :foreground "DeepSkyBlue"))
  "Face for section headers in progress display."
  :group 'efrit-progress)

(defface efrit-progress-tool-name
  '((t :weight bold :foreground "DarkOrange"))
  "Face for tool names in progress display."
  :group 'efrit-progress)

(defface efrit-progress-success
  '((t :foreground "green3"))
  "Face for success messages in progress display."
  :group 'efrit-progress)

(defface efrit-progress-error
  '((t :foreground "red3"))
  "Face for error messages in progress display."
  :group 'efrit-progress)

(defface efrit-progress-claude-message
  '((t :foreground "RoyalBlue"))
  "Face for Claude's messages in progress display."
  :group 'efrit-progress)

(defface efrit-progress-diff-added
  '((t :foreground "green4" :background "honeydew1"))
  "Face for added lines in diffs."
  :group 'efrit-progress)

(defface efrit-progress-diff-removed
  '((t :foreground "red4" :background "mistyrose1"))
  "Face for removed lines in diffs."
  :group 'efrit-progress)

;;; TODO Buffer Faces

(defgroup efrit-todos nil
  "Live TODO display for Efrit."
  :group 'efrit)

(defface efrit-todos-header
  '((t :weight bold :height 1.1))
  "Face for the TODO buffer header."
  :group 'efrit-todos)

(defface efrit-todos-pending
  '((t :foreground "gray70"))
  "Face for pending tasks."
  :group 'efrit-todos)

(defface efrit-todos-in-progress
  '((t :weight bold :foreground "orange"))
  "Face for in-progress tasks."
  :group 'efrit-todos)

(defface efrit-todos-completed
  '((t :foreground "gray50" :strike-through t))
  "Face for completed tasks."
  :group 'efrit-todos)

(defface efrit-todos-progress-bar
  '((t :foreground "green"))
  "Face for the progress bar."
  :group 'efrit-todos)

;;; Mode Line Faces

(defface efrit-modeline-task
  '((t :inherit mode-line-emphasis))
  "Face for the task indicator in the mode line."
  :group 'efrit-todos)

(defface efrit-modeline-idle
  '((t :inherit mode-line-inactive))
  "Face for the idle indicator in the mode line."
  :group 'efrit-todos)

(provide 'efrit-ui-faces)

;;; efrit-ui-faces.el ends here
