;;; efrit-diff.el --- Diff abstraction and extraction -*- lexical-binding: t -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (eieio "1.4"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; This module provides EIEIO classes and factory functions for handling
;; unified diff content. It replaces fragile extraction logic in agent-tools
;; with type-safe, discoverable diff handling.
;;
;; Key features:
;; - efrit-diff EIEIO class for structured diff representation
;; - Factory functions for different input formats (alist, string, JSON)
;; - Validators for diff content detection
;; - Accessors with sensible defaults
;; - Display helpers for rendering diffs

;;; Code:

(require 'eieio)

;;; Diff Class

(defclass efrit-diff ()
  ((content :initarg :content :type string :documentation "The unified diff content")
   (format :initarg :format :type string :initform "unified" :documentation "Diff format (unified, context, etc.)")
   (line-range :initarg :line-range :type (or list null) :initform nil :documentation "Optional (start . end) line numbers")
   (metadata :initarg :metadata :type (or list null) :initform nil :documentation "Optional alist with diff metadata"))
  :documentation "Represents a unified diff with metadata.")

;;; Validators

(defun efrit-diff--looks-like-diff-p (text)
  "Return non-nil if TEXT appears to contain unified diff content.
Checks for common diff markers like --- and +++ file headers,
@@ hunk headers, or lines starting with + or - followed by content."
  (and (stringp text)
       (or
        ;; Check for unified diff file headers
        (string-match-p "^---\\s-+\\S-+" text)
        ;; Check for hunk headers
        (string-match-p "^@@\\s--?[0-9]" text)
        ;; Check for diff output from vcs_diff tool
        (string-match-p "^diff --git" text))))

;;; Accessors

(cl-defmethod efrit-diff-get-content ((diff efrit-diff))
  "Get the diff content string."
  (slot-value diff 'content))

(cl-defmethod efrit-diff-get-format ((diff efrit-diff))
  "Get the diff format (usually \"unified\")."
  (slot-value diff 'format))

(cl-defmethod efrit-diff-get-line-range ((diff efrit-diff))
  "Get the optional line range (start . end) or nil."
  (slot-value diff 'line-range))

(cl-defmethod efrit-diff-get-metadata ((diff efrit-diff))
  "Get the optional metadata alist or nil."
  (slot-value diff 'metadata))

(cl-defmethod efrit-diff-valid-p ((diff efrit-diff))
  "Return non-nil if DIFF has valid content."
  (and (efrit-diff-p diff)
       (stringp (efrit-diff-get-content diff))
       (> (length (efrit-diff-get-content diff)) 0)
       (efrit-diff--looks-like-diff-p (efrit-diff-get-content diff))))

;;; Factory Functions

(defun efrit-diff-from-string (diff-string &optional metadata)
  "Create an efrit-diff from a diff string.
Returns nil if the string doesn't look like valid diff content."
  (when (efrit-diff--looks-like-diff-p diff-string)
    (make-instance 'efrit-diff
      :content diff-string
      :format "unified"
      :metadata metadata)))

(defun efrit-diff-from-alist (alist)
  "Create an efrit-diff from an alist (typically from tool results).
Looks for a `diff' key containing the diff content.
Returns nil if no valid diff content found."
  (when (listp alist)
    (let ((diff-content (cdr (assoc 'diff alist))))
      (when (efrit-diff--looks-like-diff-p diff-content)
        (make-instance 'efrit-diff
          :content diff-content
          :format "unified"
          :metadata alist)))))

(defun efrit-diff-from-result (result)
  "Extract a diff from various result formats.
RESULT may be:
  - A string containing diff directly
  - An alist with a `diff' key
  - Any other value that when stringified looks like diff
Returns an efrit-diff object, or nil if no diff found."
  (cond
   ;; Try alist first (from vcs_diff tool)
   ((listp result)
    (efrit-diff-from-alist result))
   ;; Try string directly
   ((stringp result)
    (efrit-diff-from-string result))
   ;; Try stringifying the result
   (t
    (let ((str (format "%s" result)))
      (when (and (not (equal str "nil")) (> (length str) 0))
        (efrit-diff-from-string str))))))

;;; Display Helpers

(defun efrit-diff-line-type (line)
  "Determine the type of diff LINE for formatting.
Returns one of: `header', `hunk', `add', `remove', or `context'."
  (cond
   ;; File headers: diff --git, ---, +++
   ((string-match-p "^diff --git\\|^---\\|^\\+\\+\\+" line) 'header)
   ;; Hunk headers: @@ ... @@
   ((string-match-p "^@@" line) 'hunk)
   ;; Added lines
   ((string-prefix-p "+" line) 'add)
   ;; Removed lines
   ((string-prefix-p "-" line) 'remove)
   ;; Context lines
   (t 'context)))

(defun efrit-diff-format-line (line indent)
  "Format a single diff LINE with appropriate face and INDENT prefix.
Returns the formatted line string with text properties.
This is a standalone version of efrit-agent--format-diff-line."
  (let* ((line-type (efrit-diff-line-type line))
         (face (pcase line-type
                 ('header 'font-lock-keyword-face)
                 ('hunk 'font-lock-function-name-face)
                 ('add 'font-lock-string-face)
                 ('remove 'font-lock-builtin-face)
                 ('context nil)))
         (line-content (concat indent "  " line "\n")))
    (if face
        (propertize line-content 'face face)
      line-content)))

(defun efrit-diff-format-for-display (diff &optional indent)
  "Format DIFF for display with syntax highlighting.
Optional INDENT prefix is added to each line.
Returns a propertized string ready for insertion in buffers."
  (let* ((content (efrit-diff-get-content diff))
         (indent-str (or indent ""))
         (lines (split-string content "\n"))
         (formatted-lines (mapcar (lambda (line)
                                   (efrit-diff-format-line line indent-str))
                                 lines)))
    (mapconcat #'identity formatted-lines "")))

;;; Statistics

(cl-defmethod efrit-diff-line-count ((diff efrit-diff))
  "Get total number of lines in the diff."
  (length (split-string (efrit-diff-get-content diff) "\n")))

(cl-defmethod efrit-diff-additions-count ((diff efrit-diff))
  "Count lines that were added (+)."
  (let ((content (efrit-diff-get-content diff)))
    (length (seq-filter (lambda (line)
                         (string-prefix-p "+" line))
                       (split-string content "\n")))))

(cl-defmethod efrit-diff-deletions-count ((diff efrit-diff))
  "Count lines that were removed (-)."
  (let ((content (efrit-diff-get-content diff)))
    (length (seq-filter (lambda (line)
                         (string-prefix-p "-" line))
                       (split-string content "\n")))))

(cl-defmethod efrit-diff-files-affected ((diff efrit-diff))
  "Count number of files mentioned in diff headers."
  (let ((content (efrit-diff-get-content diff)))
    (length (seq-filter (lambda (line)
                         (string-match-p "^diff --git" line))
                       (split-string content "\n")))))

(provide 'efrit-diff)

;;; efrit-diff.el ends here
