;;; efrit-todo.el --- TODO item management for Efrit -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai
;; URL: https://github.com/stevey/efrit
;; Package: efrit

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; This file defines the TODO item data structure and state management
;; for Efrit sessions. It provides a foundation that both efrit-tools.el
;; and efrit-do.el can depend on, avoiding circular dependencies.

;;; Code:

(require 'cl-lib)

;; Logging support
(declare-function efrit-log "efrit-log")
(condition-case nil
    (require 'efrit-log)
  (error
   (defun efrit-log (_level _format-string &rest _args)
     "Fallback logging function when efrit-log is not available."
     nil)))

;;; TODO Item Structure

(cl-defstruct (efrit-todo-item
                (:constructor efrit-todo-item-create)
                (:type vector))
  "TODO item structure."
  id
  content
  status    ; 'todo, 'in-progress, 'completed
  priority  ; 'low, 'medium, 'high
  created-at
  completed-at)

;;; TODO State

(defvar efrit-todo--current-todos nil
  "Current TODO list for the active command session.")

(defvar efrit-todo--counter 0
  "Counter for generating unique TODO IDs.")

;;; TODO ID Generation

(defun efrit-todo--generate-id ()
  "Generate a unique TODO ID."
  (setq efrit-todo--counter (1+ efrit-todo--counter))
  (format "efrit-todo-%d" efrit-todo--counter))

;;; TODO Management Functions

(defun efrit-todo-add (content &optional priority)
  "Add a new TODO item with CONTENT and optional PRIORITY."
  (unless (and content (stringp content) (not (string= "" (string-trim content))))
    (error "TODO content must be a non-empty string"))
  (unless (member priority '(low medium high nil))
    (error "TODO priority must be one of: low, medium, high"))
  (let ((todo (efrit-todo-item-create
               :id (efrit-todo--generate-id)
               :content content
               :status 'todo
               :priority (or priority 'medium)
               :created-at (current-time)
               :completed-at nil)))
    (push todo efrit-todo--current-todos)
    (efrit-log 'debug "Added TODO: %s (priority: %s)" content (or priority 'medium))
    todo))

(defun efrit-todo-update-status (id new-status)
  "Update TODO with ID to NEW-STATUS."
  (unless (and (stringp id) (not (string= "" id)))
    (error "TODO ID must be a non-empty string"))
  (unless (member new-status '(todo in-progress completed))
    (error "TODO status must be one of: todo, in-progress, completed"))
  (when-let* ((todo (seq-find (lambda (item)
                               (string= (efrit-todo-item-id item) id))
                             efrit-todo--current-todos)))
    (setf (efrit-todo-item-status todo) new-status)
    (when (eq new-status 'completed)
      (setf (efrit-todo-item-completed-at todo) (current-time)))
    (efrit-log 'debug "Updated TODO %s to status: %s" id new-status)
    todo))

(defun efrit-todo-find (id)
  "Find TODO item by ID."
  (seq-find (lambda (item)
              (string= (efrit-todo-item-id item) id))
           efrit-todo--current-todos))

(defun efrit-todo-get-all ()
  "Get the current TODO list."
  efrit-todo--current-todos)

(defun efrit-todo-set-all (todos)
  "Set the current TODO list to TODOS."
  (setq efrit-todo--current-todos todos))

(defun efrit-todo-clear ()
  "Clear all current TODOs and reset counter."
  (setq efrit-todo--current-todos nil)
  (setq efrit-todo--counter 0))

(defun efrit-todo-count ()
  "Return the number of current TODOs."
  (length efrit-todo--current-todos))

(defun efrit-todo-set-counter (n)
  "Set the TODO counter to N."
  (setq efrit-todo--counter n))

;;; TODO Formatting

(defun efrit-todo-format-for-display ()
  "Format current TODOs for user display in raw order."
  (if (null efrit-todo--current-todos)
      "No current TODOs"
    (mapconcat (lambda (todo)
                 (format "%s [%s] %s (%s)"
                         (pcase (efrit-todo-item-status todo)
                           ('todo "☐")
                           ('in-progress "⟳")
                           ('completed "☑"))
                         (upcase (symbol-name (efrit-todo-item-priority todo)))
                         (efrit-todo-item-content todo)
                         (efrit-todo-item-id todo)))
               efrit-todo--current-todos "\n")))

(defun efrit-todo-format-for-prompt ()
  "Format current TODOs for AI prompt context."
  (if (null efrit-todo--current-todos)
      ""
    (concat "\n\nCURRENT TODOs:\n" (efrit-todo-format-for-display) "\n")))

(defun efrit-todo-format-list (todos &optional sort-by)
  "Format TODOS list with optional SORT-BY criteria.
SORT-BY can be \\='status, \\='priority, \\='created, or nil (no sort).
This provides explicit sorting control to Claude rather than hardcoded logic."
  (when todos
    (let ((sorted-todos (cond
                         ((eq sort-by 'status)
                          (sort (copy-sequence todos)
                                (lambda (a b)
                                  (let ((status-order '(in-progress todo completed)))
                                    (< (seq-position status-order (efrit-todo-item-status a))
                                       (seq-position status-order (efrit-todo-item-status b)))))))
                         ((eq sort-by 'priority)
                          (sort (copy-sequence todos)
                                (lambda (a b)
                                  (let ((priority-order '(high medium low)))
                                    (< (seq-position priority-order (efrit-todo-item-priority a))
                                       (seq-position priority-order (efrit-todo-item-priority b)))))))
                         (t todos))))  ; No sorting
      (mapconcat (lambda (todo)
                   (format "%s [%s] %s (%s)"
                           (pcase (efrit-todo-item-status todo)
                             ('todo "☐")
                             ('in-progress "⟳")
                             ('completed "☑"))
                           (upcase (symbol-name (efrit-todo-item-priority todo)))
                           (efrit-todo-item-content todo)
                           (efrit-todo-item-id todo)))
                 sorted-todos "\n"))))

;;; Backward Compatibility Aliases
;; These aliases allow existing code using efrit-do-todo-item-* to continue working

(defalias 'efrit-do-todo-item-id 'efrit-todo-item-id)
(defalias 'efrit-do-todo-item-content 'efrit-todo-item-content)
(defalias 'efrit-do-todo-item-status 'efrit-todo-item-status)
(defalias 'efrit-do-todo-item-priority 'efrit-todo-item-priority)
(defalias 'efrit-do-todo-item-created-at 'efrit-todo-item-created-at)
(defalias 'efrit-do-todo-item-completed-at 'efrit-todo-item-completed-at)
(defalias 'efrit-do-todo-item-create 'efrit-todo-item-create)

(provide 'efrit-todo)
;;; efrit-todo.el ends here
