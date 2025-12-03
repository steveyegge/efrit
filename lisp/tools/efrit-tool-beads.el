;;; efrit-tool-beads.el --- Beads issue tracking tool for Efrit -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:
;; This module provides Efrit access to beads (bd) CLI for issue tracking.
;; Allows Claude to check ready work, create/update/close issues, and query status.

;;; Code:

(require 'json)
(require 'efrit-log)

(defvar efrit-tool-beads-workspace nil
  "Workspace root for beads commands. If nil, uses current default-directory.")

(defun efrit-tool-beads--build-command (command &optional args)
  "Build a beads CLI command string.
COMMAND is the beads command (ready, create, list, etc).
ARGS is an optional hash table of arguments.
Returns the command string to execute."
  (let ((cmd (format "bd %s" command)))
    (when args
      (maphash
       (lambda (key value)
         ;; Skip nil values
         (unless (null value)
           ;; Format the argument based on type
           (let ((arg-str 
                  (cond
                   ((eq value t) (format "--%s" key))  ; Boolean flag
                   ((stringp value) (format "--%s '%s'" key (replace-regexp-in-string "'" "\\\\'" value)))
                   ((numberp value) (format "--%s %d" key value))
                   ((listp value) (mapconcat (lambda (v) (format "--%s %s" key v)) value " "))
                   (t (format "--%s %S" key value)))))
             (setq cmd (concat cmd " " arg-str)))))
       args))
    cmd))

(defun efrit-tool-beads--execute (command &optional args)
  "Execute a beads command and return the result.
COMMAND is the beads command name (ready, create, list, etc).
ARGS is an optional hash table of command arguments.
Returns (result . success) where result is the output and success is t/nil."
  (let* ((cmd (efrit-tool-beads--build-command command args))
         (workspace (or efrit-tool-beads-workspace default-directory))
         (result nil))
    (condition-case err
        (let ((output (shell-command-to-string 
                       (format "cd '%s' && %s 2>&1"
                               (replace-regexp-in-string "'" "\\\\'" workspace)
                               cmd))))
          (setq result output)
          ;; Try to parse as JSON if it looks like it
          (let ((trimmed (string-trim output)))
            (if (or (string-prefix-p "{" trimmed)
                    (string-prefix-p "[" trimmed))
                (condition-case json-err
                    (cons (json-read-from-string output) t)
                  (error (cons output t))) ; Return raw output if JSON parse fails
              (cons output t))))
      (error
       (let ((err-msg (error-message-string err)))
         (cons (format "Error executing beads command: %s" err-msg) nil))))))

;;;###autoload
(defun efrit-tool-beads-ready (&optional args)
  "Get ready work (unblocked issues).
ARGS is an optional hash table with:
  - limit: max results to return
  - priority: filter by priority level
  - assignee: filter by assignee"
  (let* ((result (efrit-tool-beads--execute "ready" args))
         (success (cdr result))
         (output (car result)))
    (if success
        (format "[Beads Ready Work]\n%s" output)
      (format "[Beads Error]\n%s" output))))

;;;###autoload
(defun efrit-tool-beads-create (title &optional args)
  "Create a new issue.
TITLE is the issue title.
ARGS is an optional hash table with:
  - type: bug, feature, task, epic, chore
  - priority: 0-4 (critical to backlog)
  - description: detailed description
  - acceptance: acceptance criteria"
  (let ((create-args (or args (make-hash-table :test 'equal))))
    (puthash "title" title create-args)
    (let* ((result (efrit-tool-beads--execute "create" create-args))
           (success (cdr result))
           (output (car result)))
      (if success
          (format "[Beads Issue Created]\n%s" output)
        (format "[Beads Error]\n%s" output)))))

;;;###autoload
(defun efrit-tool-beads-update (issue-id &optional args)
  "Update an issue.
ISSUE-ID is the issue identifier.
ARGS is an optional hash table with:
  - status: open, in_progress, blocked, closed
  - priority: 0-4
  - assignee: person to assign to
  - description: new description"
  (let ((update-args (or args (make-hash-table :test 'equal))))
    (puthash "issue_id" issue-id update-args)
    (let* ((result (efrit-tool-beads--execute "update" update-args))
           (success (cdr result))
           (output (car result)))
      (if success
          (format "[Beads Issue Updated]\n%s" output)
        (format "[Beads Error]\n%s" output)))))

;;;###autoload
(defun efrit-tool-beads-close (issue-id &optional reason)
  "Close an issue.
ISSUE-ID is the issue identifier.
REASON is an optional close reason."
  (let ((close-args (make-hash-table :test 'equal)))
    (when reason
      (puthash "reason" reason close-args))
    (let* ((result (efrit-tool-beads--execute "close" close-args))
           (success (cdr result))
           (output (car result)))
      (if success
          (format "[Beads Issue Closed]\n%s" output)
        (format "[Beads Error]\n%s" output)))))

;;;###autoload
(defun efrit-tool-beads-list (&optional args)
  "List issues with optional filtering.
ARGS is an optional hash table with:
  - status: open, in_progress, blocked, closed
  - priority: specific priority (0-4)
  - type: issue type (bug, feature, task, etc)
  - assignee: filter by assignee
  - limit: max results"
  (let* ((result (efrit-tool-beads--execute "list" args))
         (success (cdr result))
         (output (car result)))
    (if success
        (format "[Beads Issues]\n%s" output)
      (format "[Beads Error]\n%s" output))))

;;;###autoload
(defun efrit-tool-beads-show (issue-id)
  "Show detailed information about an issue.
ISSUE-ID is the issue identifier."
  (let* ((result (efrit-tool-beads--execute "show" (let ((h (make-hash-table :test 'equal)))
                                                      (puthash "issue_id" issue-id h)
                                                      h)))
         (success (cdr result))
         (output (car result)))
    (if success
        (format "[Beads Issue Details]\n%s" output)
      (format "[Beads Error]\n%s" output))))

(provide 'efrit-tool-beads)

;;; efrit-tool-beads.el ends here
