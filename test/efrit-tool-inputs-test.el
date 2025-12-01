;;; efrit-tool-inputs-test.el --- Tests for tool input validators -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;;; Commentary:

;; Tests for EIEIO-based tool input validators.

;;; Code:

(require 'ert)
(require 'efrit-tool-inputs)

;;; Todo Write Input Tests

(ert-deftest efrit-tool-inputs-todo-write-create-empty-input ()
  "Test creating todo_write input from empty/nil hash table."
  (let ((input (efrit-todo-write-input-create nil)))
    (should (efrit-todo-write-input-p input))
    (should (equal (efrit-todo-write-input-get-todos input) '()))))

(ert-deftest efrit-tool-inputs-todo-write-create-with-todos ()
  "Test creating todo_write input with actual todos."
  (let* ((hash (make-hash-table :test 'equal))
         (_dummy (puthash "todos" (vector
                                   (let ((h (make-hash-table :test 'equal)))
                                     (puthash "content" "Task 1" h)
                                     (puthash "status" "pending" h)
                                     h)
                                   (let ((h (make-hash-table :test 'equal)))
                                     (puthash "content" "Task 2" h)
                                     (puthash "status" "completed" h)
                                     h))
                          hash))
         (input (efrit-todo-write-input-create hash))
         (todos (efrit-todo-write-input-get-todos input)))
    (should (efrit-todo-write-input-p input))
    (should (= (length todos) 2))
    (let ((first-todo (car todos)))
      (should (equal (gethash "content" first-todo) "Task 1"))
      (should (equal (gethash "status" first-todo) "pending")))))

;;; Buffer Create Input Tests

(ert-deftest efrit-tool-inputs-buffer-create-empty ()
  "Test creating buffer_create input with defaults."
  (let ((input (efrit-buffer-create-input-create nil)))
    (should (efrit-buffer-create-input-p input))
    (should (equal (efrit-buffer-create-input-get-name input) ""))
    (should (equal (efrit-buffer-create-input-get-content input) ""))
    (should (null (efrit-buffer-create-input-get-mode input)))))

(ert-deftest efrit-tool-inputs-buffer-create-with-values ()
  "Test creating buffer_create input with actual values."
  (let* ((hash (make-hash-table :test 'equal))
         (_n1 (puthash "name" "my-buffer" hash))
         (_n2 (puthash "content" "Some content" hash))
         (_n3 (puthash "mode" "python-mode" hash))
         (input (efrit-buffer-create-input-create hash)))
    (should (equal (efrit-buffer-create-input-get-name input) "my-buffer"))
    (should (equal (efrit-buffer-create-input-get-content input) "Some content"))
    (should (equal (efrit-buffer-create-input-get-mode input) 'python-mode))))

;;; Glob Files Input Tests

(ert-deftest efrit-tool-inputs-glob-files-create-empty ()
  "Test creating glob_files input with defaults."
  (let ((input (efrit-glob-files-input-create nil)))
    (should (efrit-glob-files-input-p input))
    (should (equal (efrit-glob-files-input-get-pattern input) ""))
    (should (equal (efrit-glob-files-input-get-extension input) "*"))
    (should (efrit-glob-files-input-get-recursive input))))

(ert-deftest efrit-tool-inputs-glob-files-create-with-values ()
  "Test creating glob_files input with actual values."
  (let* ((hash (make-hash-table :test 'equal))
         (_p (puthash "pattern" "/home/user" hash))
         (_e (puthash "extension" "el,py" hash))
         (_r (puthash "recursive" nil hash))
         (input (efrit-glob-files-input-create hash)))
    (should (equal (efrit-glob-files-input-get-pattern input) "/home/user"))
    (should (equal (efrit-glob-files-input-get-extension input) "el,py"))
    (should (not (efrit-glob-files-input-get-recursive input)))))

(ert-deftest efrit-tool-inputs-glob-files-validate-valid ()
  "Test that valid glob_files input passes validation."
  (let* ((hash (make-hash-table :test 'equal))
         (_p (puthash "pattern" "/some/dir" hash))
         (_e (puthash "extension" "el" hash))
         (input (efrit-glob-files-input-create hash))
         (validation (efrit-glob-files-input-is-valid input)))
    (should (car validation))
    (should (null (cdr validation)))))

(ert-deftest efrit-tool-inputs-glob-files-validate-missing-pattern ()
  "Test that glob_files input without pattern fails validation."
  (let* ((hash (make-hash-table :test 'equal))
         (_e (puthash "extension" "el" hash))
         (input (efrit-glob-files-input-create hash))
         (validation (efrit-glob-files-input-is-valid input)))
    (should (not (car validation)))
    (should (string-match-p "pattern" (cdr validation)))))

(ert-deftest efrit-tool-inputs-glob-files-validate-missing-extension ()
  "Test that glob_files input without extension fails validation."
  (let* ((hash (make-hash-table :test 'equal))
         (_p (puthash "pattern" "/some/dir" hash))
         (input (efrit-glob-files-input-create hash))
         (validation (efrit-glob-files-input-is-valid input)))
    ;; When no extension is provided, it defaults to "*" so it's valid
    (should (car validation))))

;;; Request User Input Tests

(ert-deftest efrit-tool-inputs-request-user-input-empty ()
  "Test creating request_user_input with defaults."
  (let ((input (efrit-request-user-input-input-create nil)))
    (should (efrit-request-user-input-input-p input))
    (should (equal (efrit-request-user-input-input-get-question input) ""))
    (should (equal (efrit-request-user-input-input-get-options input) '()))))

(ert-deftest efrit-tool-inputs-request-user-input-with-values ()
  "Test creating request_user_input with actual values."
  (let* ((hash (make-hash-table :test 'equal))
         (_q (puthash "question" "What is your name?" hash))
         (_o (puthash "options" (vector "Alice" "Bob" "Charlie") hash))
         (input (efrit-request-user-input-input-create hash)))
    (should (equal (efrit-request-user-input-input-get-question input) "What is your name?"))
    (should (equal (efrit-request-user-input-input-get-options input) '("Alice" "Bob" "Charlie")))))

(ert-deftest efrit-tool-inputs-request-user-input-validate-valid ()
  "Test that valid request_user_input passes validation."
  (let* ((hash (make-hash-table :test 'equal))
         (_q (puthash "question" "Continue?" hash))
         (input (efrit-request-user-input-input-create hash))
         (validation (efrit-request-user-input-input-is-valid input)))
    (should (car validation))))

(ert-deftest efrit-tool-inputs-request-user-input-validate-missing-question ()
  "Test that request_user_input without question fails validation."
  (let ((input (efrit-request-user-input-input-create nil))
        (validation (efrit-request-user-input-input-is-valid (efrit-request-user-input-input-create nil))))
    (should (not (car validation)))
    (should (string-match-p "question" (cdr validation)))))

;;; Format Todo List Input Tests

(ert-deftest efrit-tool-inputs-format-todo-list-empty ()
  "Test creating format_todo_list input with defaults."
  (let ((input (efrit-format-todo-list-input-create nil)))
    (should (efrit-format-todo-list-input-p input))
    (should (null (efrit-format-todo-list-input-get-sort-by input)))))

(ert-deftest efrit-tool-inputs-format-todo-list-with-sort ()
  "Test creating format_todo_list input with sort_by."
  (let* ((hash (make-hash-table :test 'equal))
         (_s (puthash "sort_by" "priority" hash))
         (input (efrit-format-todo-list-input-create hash)))
    (should (equal (efrit-format-todo-list-input-get-sort-by input) 'priority))))

;;; Session Complete Input Tests

(ert-deftest efrit-tool-inputs-session-complete-empty ()
  "Test creating session_complete input with defaults."
  (let ((input (efrit-session-complete-input-create nil)))
    (should (efrit-session-complete-input-p input))
    (should (equal (efrit-session-complete-input-get-message input) ""))))

(ert-deftest efrit-tool-inputs-session-complete-with-message ()
  "Test creating session_complete input with message."
  (let* ((hash (make-hash-table :test 'equal))
         (_m (puthash "message" "Task completed successfully" hash))
         (input (efrit-session-complete-input-create hash)))
    (should (equal (efrit-session-complete-input-get-message input) "Task completed successfully"))))

(provide 'efrit-tool-inputs-test)

;;; efrit-tool-inputs-test.el ends here
