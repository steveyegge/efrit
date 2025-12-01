;;; efrit-tool-inputs.el --- Tool input validators using EIEIO -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, validation, ai

;;; Commentary:

;; This module provides EIEIO classes for validating and extracting fields
;; from tool inputs. Replaces ad-hoc gethash+conversion patterns throughout
;; efrit-do-handlers.el with type-safe, self-documenting accessors.
;;
;; Each tool input type has:
;; - EIEIO class with slots for all expected fields
;; - Factory function to construct from hash table
;; - Accessor methods with sensible defaults
;; - Conversion helpers (string→symbol for enums)
;;
;; This makes tool handlers more readable and testable.

;;; Code:

(require 'eieio)

;;; Base class with common validation

(defclass efrit-tool-input-base ()
  ()
  :abstract t
  :documentation "Base class for all tool inputs.
Common interface for validation and field access.")

(cl-defmethod efrit-tool-input-to-alist ((_input efrit-tool-input-base))
  "Convert _INPUT to an alist suitable for passing to tool functions.
Subclasses should override to export their fields."
  nil)

;;; Todo Write Input

(defclass efrit-todo-write-input (efrit-tool-input-base)
  ((todos :initarg :todos
          :initform '()
          :type list
          :documentation "Array of todo items from Claude."))
  :documentation "Validator for todo_write tool input.")

(defun efrit-todo-write-input-create (tool-input)
  "Create and validate a todo_write input from TOOL-INPUT hash table."
  (make-instance
   'efrit-todo-write-input
   :todos (or (and (hash-table-p tool-input)
                   (let ((todos-arr (gethash "todos" tool-input)))
                     (when (and todos-arr (> (length todos-arr) 0))
                       (append todos-arr nil))))  ; Convert vector to list
              '())))

(cl-defmethod efrit-todo-write-input-get-todos ((input efrit-todo-write-input))
  "Return the todos array from INPUT."
  (oref input todos))

;;; Buffer Create Input

(defclass efrit-buffer-create-input (efrit-tool-input-base)
  ((name :initarg :name
         :initform ""
         :type string
         :documentation "Buffer name")
   (content :initarg :content
            :initform ""
            :type string
            :documentation "Buffer content")
   (mode :initarg :mode
         :initform nil
         :type (or null symbol)
         :documentation "Major mode symbol (e.g., 'python-mode)"))
  :documentation "Validator for buffer_create tool input.")

(defun efrit-buffer-create-input-create (tool-input)
  "Create and validate a buffer_create input from TOOL-INPUT hash table."
  (make-instance
   'efrit-buffer-create-input
   :name (or (and (hash-table-p tool-input)
                  (gethash "name" tool-input))
             "")
   :content (or (and (hash-table-p tool-input)
                     (gethash "content" tool-input))
                "")
   :mode (and (hash-table-p tool-input)
              (let ((mode-str (gethash "mode" tool-input)))
                (when mode-str (intern mode-str))))))

(cl-defmethod efrit-buffer-create-input-get-name ((input efrit-buffer-create-input))
  "Return the buffer name from INPUT."
  (oref input name))

(cl-defmethod efrit-buffer-create-input-get-content ((input efrit-buffer-create-input))
  "Return the buffer content from INPUT."
  (oref input content))

(cl-defmethod efrit-buffer-create-input-get-mode ((input efrit-buffer-create-input))
  "Return the major mode symbol from INPUT, or nil if not specified."
  (oref input mode))

;;; Glob Files Input

(defclass efrit-glob-files-input (efrit-tool-input-base)
  ((pattern :initarg :pattern
            :initform ""
            :type string
            :documentation "Directory path or glob pattern")
   (extension :initarg :extension
              :initform "*"
              :type string
              :documentation "File extensions to match (e.g., 'el' or 'py,js' or '*')")
   (recursive :initarg :recursive
              :initform t
              :type boolean
              :documentation "Whether to search recursively"))
  :documentation "Validator for glob_files tool input.")

(defun efrit-glob-files-input-create (tool-input)
  "Create and validate a glob_files input from TOOL-INPUT hash table."
  (make-instance
   'efrit-glob-files-input
   :pattern (or (and (hash-table-p tool-input)
                     (gethash "pattern" tool-input))
                "")
   :extension (or (and (hash-table-p tool-input)
                       (gethash "extension" tool-input))
                  "*")
   :recursive (if (hash-table-p tool-input)
                  ;; When key exists, use its value (nil or t)
                  ;; When key doesn't exist, default to t
                  (let ((val (gethash "recursive" tool-input 'no-value)))
                    (if (eq val 'no-value)
                        t  ; Key doesn't exist, default to recursive
                      val))  ; Use the explicit value (t or nil)
                t)))

(cl-defmethod efrit-glob-files-input-get-pattern ((input efrit-glob-files-input))
  "Return the directory pattern from INPUT."
  (oref input pattern))

(cl-defmethod efrit-glob-files-input-get-extension ((input efrit-glob-files-input))
  "Return the file extension(s) from INPUT."
  (oref input extension))

(cl-defmethod efrit-glob-files-input-get-recursive ((input efrit-glob-files-input))
  "Return the recursive flag from INPUT."
  (oref input recursive))

(cl-defmethod efrit-glob-files-input-is-valid ((input efrit-glob-files-input))
  "Validate that INPUT has required fields.
Returns (valid-p . error-msg) where valid-p is t/nil."
  (let ((pattern (efrit-glob-files-input-get-pattern input))
        (extension (efrit-glob-files-input-get-extension input)))
    (cond
     ((or (null pattern) (string-empty-p pattern))
      (cons nil "glob_files requires 'pattern' field (directory path)"))
     ((or (null extension) (string-empty-p extension))
      (cons nil "glob_files requires 'extension' field (e.g., 'el' or 'py,js' or '*')"))
     (t (cons t nil)))))

;;; Request User Input

(defclass efrit-request-user-input-input (efrit-tool-input-base)
  ((question :initarg :question
             :initform ""
             :type string
             :documentation "Question to ask the user")
   (options :initarg :options
            :initform '()
            :type list
            :documentation "Optional list of valid answer choices"))
  :documentation "Validator for request_user_input tool input.")

(defun efrit-request-user-input-input-create (tool-input)
  "Create and validate a request_user_input input from TOOL-INPUT hash table."
  (make-instance
   'efrit-request-user-input-input
   :question (or (and (hash-table-p tool-input)
                      (gethash "question" tool-input))
                 "")
   :options (or (and (hash-table-p tool-input)
                     (let ((opts (gethash "options" tool-input)))
                       (when opts (append opts nil))))  ; Convert vector to list
                '())))

(cl-defmethod efrit-request-user-input-input-get-question ((input efrit-request-user-input-input))
  "Return the question from INPUT."
  (oref input question))

(cl-defmethod efrit-request-user-input-input-get-options ((input efrit-request-user-input-input))
  "Return the options list from INPUT."
  (oref input options))

(cl-defmethod efrit-request-user-input-input-is-valid ((input efrit-request-user-input-input))
  "Validate that INPUT has required fields.
Returns (valid-p . error-msg) where valid-p is t/nil."
  (let ((question (efrit-request-user-input-input-get-question input)))
    (if (or (null question) (string-empty-p question))
        (cons nil "request_user_input requires 'question' field")
      (cons t nil))))

;;; Format Todo List Input

(defclass efrit-format-todo-list-input (efrit-tool-input-base)
  ((sort-by :initarg :sort-by
            :initform nil
            :type (or null symbol)
            :documentation "Sort order: 'status, 'priority, 'created, etc."))
  :documentation "Validator for format_todo_list tool input.")

(defun efrit-format-todo-list-input-create (tool-input)
  "Create and validate a format_todo_list input from TOOL-INPUT hash table."
  (make-instance
   'efrit-format-todo-list-input
   :sort-by (and (hash-table-p tool-input)
                 (let ((sort-str (gethash "sort_by" tool-input)))
                   (when sort-str (intern sort-str))))))

(cl-defmethod efrit-format-todo-list-input-get-sort-by ((input efrit-format-todo-list-input))
  "Return the sort key from INPUT, or nil for default sorting."
  (oref input sort-by))

;;; Session Complete Input

(defclass efrit-session-complete-input (efrit-tool-input-base)
  ((message :initarg :message
            :initform ""
            :type string
            :documentation "Completion message from Claude"))
  :documentation "Validator for session_complete tool input.")

(defun efrit-session-complete-input-create (tool-input)
  "Create and validate a session_complete input from TOOL-INPUT hash table."
  (make-instance
   'efrit-session-complete-input
   :message (or (and (hash-table-p tool-input)
                     (gethash "message" tool-input))
                "")))

(cl-defmethod efrit-session-complete-input-get-message ((input efrit-session-complete-input))
  "Return the completion message from INPUT."
  (oref input message))

(provide 'efrit-tool-inputs)

;;; efrit-tool-inputs.el ends here
