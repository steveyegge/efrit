;;; test-do-handlers.el --- Tests for efrit-do-handlers -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;;; Commentary:
;; Tests for efrit-do-handlers.el tool handler functions.
;; Covers critical handlers: eval_sexp, shell_exec, todo_write, glob_files.

;;; Code:

(require 'ert)
(require 'json)

;; Add load paths for test
(add-to-list 'load-path (expand-file-name "../lisp" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../lisp/core" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../lisp/interfaces" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../lisp/tools" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../lisp/support" (file-name-directory load-file-name)))

(require 'efrit-common)
(require 'efrit-tool-inputs)
(require 'efrit-do-handlers)

;; Mock the TODO struct accessors if not available
(unless (fboundp 'efrit-do-todo-item-create)
  (cl-defstruct (efrit-do-todo-item (:constructor efrit-do-todo-item-create))
    id content status priority created-at completed-at))

;; Variables needed by handlers
(defvar efrit-do--current-todos nil)
(defvar efrit-do--todo-counter 0)

;;; ============================================================
;;; Tests for efrit-do--validate-elisp
;;; ============================================================

(ert-deftest test-validate-elisp-valid-simple ()
  "Test validation of simple valid elisp."
  (let ((result (efrit-do--validate-elisp "(+ 1 2)")))
    (should (car result))
    (should (null (cdr result)))))

(ert-deftest test-validate-elisp-valid-complex ()
  "Test validation of complex valid elisp."
  (let ((result (efrit-do--validate-elisp "(let ((x 1)) (+ x 2))")))
    (should (car result))
    (should (null (cdr result)))))

(ert-deftest test-validate-elisp-invalid-unbalanced ()
  "Test validation catches unbalanced parens."
  (let ((result (efrit-do--validate-elisp "(+ 1 2")))
    (should-not (car result))
    (should (stringp (cdr result)))))

(ert-deftest test-validate-elisp-valid-with-trailing-chars ()
  "Test validation succeeds for expression with trailing chars.
Note: read-from-string only reads the first complete sexp,
so extra chars after are ignored (not a syntax error)."
  (let ((result (efrit-do--validate-elisp "(+ 1 2))")))
    (should (car result))))

(ert-deftest test-validate-elisp-nil-input ()
  "Test validation handles nil input."
  (let ((result (efrit-do--validate-elisp nil)))
    (should (null result))))

(ert-deftest test-validate-elisp-empty-string ()
  "Test validation of empty string."
  (let ((result (efrit-do--validate-elisp "")))
    (should-not (car result))))

;;; ============================================================
;;; Tests for efrit-do--handle-eval-sexp
;;; ============================================================

(ert-deftest test-handle-eval-sexp-valid-arithmetic ()
  "Test eval-sexp with valid arithmetic."
  (let ((result (efrit-do--handle-eval-sexp "(+ 1 2)")))
    (should (stringp result))
    (should (string-match-p "Executed" result))
    (should (string-match-p "Result" result))
    (should (string-match-p "3" result))))

(ert-deftest test-handle-eval-sexp-valid-string ()
  "Test eval-sexp returning a string."
  (let ((result (efrit-do--handle-eval-sexp "(format \"hello %s\" \"world\")")))
    (should (stringp result))
    (should (string-match-p "hello world" result))))

(ert-deftest test-handle-eval-sexp-syntax-error ()
  "Test eval-sexp with syntax error."
  (let ((result (efrit-do--handle-eval-sexp "(+ 1")))
    (should (stringp result))
    (should (string-match-p "Syntax Error" result))))

(ert-deftest test-handle-eval-sexp-runtime-error ()
  "Test eval-sexp with runtime error."
  (let ((result (efrit-do--handle-eval-sexp "(/ 1 0)")))
    (should (stringp result))
    (should (string-match-p "Error" result))))

(ert-deftest test-handle-eval-sexp-list-result ()
  "Test eval-sexp with list result."
  (let ((result (efrit-do--handle-eval-sexp "'(a b c)")))
    (should (stringp result))
    (should (string-match-p "Result" result))))

;;; ============================================================
;;; Tests for efrit-do--validate-shell-command
;;; ============================================================

(ert-deftest test-validate-shell-allowed-command ()
  "Test validation allows whitelisted commands."
  (let ((efrit-do-shell-security-enabled t))
    (dolist (cmd '("ls" "pwd" "date" "whoami" "echo hello" "git status"))
      (let ((result (efrit-do--validate-shell-command cmd)))
        (should (car result))))))

(ert-deftest test-validate-shell-blocked-command ()
  "Test validation blocks dangerous commands."
  (let ((efrit-do-shell-security-enabled t))
    (dolist (cmd '("rm file.txt" "sudo ls" "rmdir test" "chmod 777 file"))
      (let ((result (efrit-do--validate-shell-command cmd)))
        (should-not (car result))
        (should (stringp (cdr result)))))))

(ert-deftest test-validate-shell-blocked-patterns ()
  "Test validation blocks forbidden patterns."
  (let ((efrit-do-shell-security-enabled t))
    (dolist (cmd '("ls && rm" "ls ; rm" "echo $(whoami)" "echo `date`"))
      (let ((result (efrit-do--validate-shell-command cmd)))
        (should-not (car result))))))

(ert-deftest test-validate-shell-empty-command ()
  "Test validation rejects empty command."
  (let ((efrit-do-shell-security-enabled t))
    (let ((result (efrit-do--validate-shell-command "")))
      (should-not (car result))
      (should (string-match-p "Empty" (cdr result))))))

(ert-deftest test-validate-shell-too-long ()
  "Test validation rejects overly long commands.
Note: whitelist check happens first, so very long commands fail that check first."
  (let ((efrit-do-shell-security-enabled t))
    ;; Use an allowed command that's too long
    (let ((result (efrit-do--validate-shell-command (concat "echo " (make-string 250 ?a)))))
      (should-not (car result))
      (should (string-match-p "too long" (cdr result))))))

(ert-deftest test-validate-shell-security-disabled ()
  "Test all commands allowed when security disabled."
  (let ((efrit-do-shell-security-enabled nil))
    (let ((result (efrit-do--validate-shell-command "rm -rf /")))
      (should (car result)))))

;;; ============================================================
;;; Tests for efrit-do--handle-shell-exec
;;; ============================================================

(ert-deftest test-handle-shell-exec-allowed ()
  "Test shell-exec with allowed command."
  (let ((efrit-do-shell-security-enabled t))
    (let ((result (efrit-do--handle-shell-exec "echo hello")))
      (should (stringp result))
      (should (string-match-p "Executed" result))
      (should (string-match-p "hello" result)))))

(ert-deftest test-handle-shell-exec-blocked ()
  "Test shell-exec blocks dangerous command."
  (let ((efrit-do-shell-security-enabled t))
    (let ((result (efrit-do--handle-shell-exec "rm important.txt")))
      (should (stringp result))
      (should (string-match-p "SECURITY" result))
      (should (string-match-p "blocked" result)))))

(ert-deftest test-handle-shell-exec-output-truncation ()
  "Test shell-exec truncates long output."
  (let ((efrit-do-shell-security-enabled t))
    ;; Generate output longer than 1000 chars
    (let ((result (efrit-do--handle-shell-exec "cat CLAUDE.md")))
      (should (stringp result))
      (when (> (length result) 1200)
        (should (string-match-p "truncated" result))))))

;;; ============================================================
;;; Tests for efrit-do--handle-todo-write
;;; ============================================================

(ert-deftest test-handle-todo-write-basic ()
  "Test todo_write with basic todo list."
  (setq efrit-do--current-todos nil)
  (setq efrit-do--todo-counter 0)
  (let* ((todo1 (make-hash-table :test 'equal))
         (todo2 (make-hash-table :test 'equal))
         (input (make-hash-table :test 'equal)))
    (puthash "content" "First task" todo1)
    (puthash "status" "pending" todo1)
    (puthash "content" "Second task" todo2)
    (puthash "status" "in_progress" todo2)
    (puthash "todos" (vector todo1 todo2) input)
    (let ((result (efrit-do--handle-todo-write input)))
      (should (stringp result))
      (should (string-match-p "2 total" result))
      (should (= (length efrit-do--current-todos) 2)))))

(ert-deftest test-handle-todo-write-status-mapping ()
  "Test todo_write correctly maps status strings."
  (setq efrit-do--current-todos nil)
  (setq efrit-do--todo-counter 0)
  (let* ((todo1 (make-hash-table :test 'equal))
         (todo2 (make-hash-table :test 'equal))
         (todo3 (make-hash-table :test 'equal))
         (input (make-hash-table :test 'equal)))
    (puthash "content" "Pending task" todo1)
    (puthash "status" "pending" todo1)
    (puthash "content" "In progress task" todo2)
    (puthash "status" "in_progress" todo2)
    (puthash "content" "Completed task" todo3)
    (puthash "status" "completed" todo3)
    (puthash "todos" (vector todo1 todo2 todo3) input)
    (efrit-do--handle-todo-write input)
    (should (= (length efrit-do--current-todos) 3))
    ;; Check status mapping
    (let ((statuses (mapcar #'efrit-do-todo-item-status efrit-do--current-todos)))
      (should (member 'todo statuses))
      (should (member 'in-progress statuses))
      (should (member 'completed statuses)))))

(ert-deftest test-handle-todo-write-empty ()
  "Test todo_write with empty list."
  (setq efrit-do--current-todos '(dummy))  ; Start with something
  (setq efrit-do--todo-counter 1)
  (let ((input (make-hash-table :test 'equal)))
    (puthash "todos" (vector) input)
    (let ((result (efrit-do--handle-todo-write input)))
      (should (stringp result))
      (should (string-match-p "0 total" result))
      (should (= (length efrit-do--current-todos) 0)))))

(ert-deftest test-handle-todo-write-replaces-list ()
  "Test todo_write completely replaces existing list."
  (setq efrit-do--current-todos nil)
  (setq efrit-do--todo-counter 0)
  ;; First, create initial list
  (let* ((todo1 (make-hash-table :test 'equal))
         (input1 (make-hash-table :test 'equal)))
    (puthash "content" "Initial task" todo1)
    (puthash "status" "pending" todo1)
    (puthash "todos" (vector todo1) input1)
    (efrit-do--handle-todo-write input1))
  (should (= (length efrit-do--current-todos) 1))
  ;; Now replace with new list
  (let* ((todo2 (make-hash-table :test 'equal))
         (todo3 (make-hash-table :test 'equal))
         (input2 (make-hash-table :test 'equal)))
    (puthash "content" "New task 1" todo2)
    (puthash "status" "pending" todo2)
    (puthash "content" "New task 2" todo3)
    (puthash "status" "completed" todo3)
    (puthash "todos" (vector todo2 todo3) input2)
    (efrit-do--handle-todo-write input2))
  (should (= (length efrit-do--current-todos) 2))
  ;; Check content is from new list
  (let ((contents (mapcar #'efrit-do-todo-item-content efrit-do--current-todos)))
    (should (member "New task 1" contents))
    (should (member "New task 2" contents))
    (should-not (member "Initial task" contents))))

(ert-deftest test-handle-todo-write-current-task-reported ()
  "Test todo_write reports current in-progress task."
  (setq efrit-do--current-todos nil)
  (setq efrit-do--todo-counter 0)
  (let* ((todo1 (make-hash-table :test 'equal))
         (todo2 (make-hash-table :test 'equal))
         (input (make-hash-table :test 'equal)))
    (puthash "content" "Done task" todo1)
    (puthash "status" "completed" todo1)
    (puthash "content" "Working on this" todo2)
    (puthash "status" "in_progress" todo2)
    (puthash "todos" (vector todo1 todo2) input)
    (let ((result (efrit-do--handle-todo-write input)))
      (should (string-match-p "Current task" result))
      (should (string-match-p "Working on this" result)))))

;;; ============================================================
;;; Tests for efrit-do--handle-glob-files
;;; ============================================================

(ert-deftest test-handle-glob-files-returns-string ()
  "Test glob_files returns a string, not a symbol.
This is a regression test for ef-lwj where paren imbalance caused
the function to return 'efrit-do--handle-request-user-input instead."
  (let* ((input (make-hash-table :test 'equal)))
    (puthash "pattern" "lisp/core" input)
    (puthash "extension" "el" input)
    (let ((result (efrit-do--handle-glob-files input)))
      (should (stringp result))
      (should-not (symbolp result))
      (should (string-match-p "\\[Found" result)))))

(ert-deftest test-handle-glob-files-missing-pattern ()
  "Test glob_files error on missing pattern."
  (let* ((input (make-hash-table :test 'equal)))
    (puthash "extension" "el" input)
    (let ((result (efrit-do--handle-glob-files input)))
      (should (stringp result))
      (should (string-match-p "Error" result)))))

(ert-deftest test-handle-glob-files-defaults-extension ()
  "Test glob_files defaults extension to '*' (all files)."
  (let* ((input (make-hash-table :test 'equal)))
    (puthash "pattern" "lisp/core" input)
    ;; No extension provided - should default to "*"
    (let ((result (efrit-do--handle-glob-files input)))
      (should (stringp result))
      (should (string-match-p "\\[Found" result)))))

;;; ============================================================
;;; Tests for helper functions
;;; ============================================================

(ert-deftest test-validate-hash-table-valid ()
  "Test validate-hash-table with valid input."
  (let ((ht (make-hash-table :test 'equal)))
    (should (null (efrit-do--validate-hash-table ht "test_tool")))))

(ert-deftest test-validate-hash-table-invalid ()
  "Test validate-hash-table with invalid input."
  (should (stringp (efrit-do--validate-hash-table nil "test_tool")))
  (should (stringp (efrit-do--validate-hash-table "string" "test_tool")))
  (should (stringp (efrit-do--validate-hash-table '(a b) "test_tool"))))

(ert-deftest test-validate-required-present ()
  "Test validate-required with present field."
  (let ((ht (make-hash-table :test 'equal)))
    (puthash "field" "value" ht)
    (should (null (efrit-do--validate-required ht "test_tool" "field")))))

(ert-deftest test-validate-required-missing ()
  "Test validate-required with missing field."
  (let ((ht (make-hash-table :test 'equal)))
    (should (stringp (efrit-do--validate-required ht "test_tool" "field")))))

(ert-deftest test-validate-required-empty-string ()
  "Test validate-required with empty string value."
  (let ((ht (make-hash-table :test 'equal)))
    (puthash "field" "" ht)
    (should (stringp (efrit-do--validate-required ht "test_tool" "field")))))

(ert-deftest test-extract-fields-basic ()
  "Test extract-fields with basic fields."
  (let ((ht (make-hash-table :test 'equal)))
    (puthash "name" "value1" ht)
    (puthash "count" 42 ht)
    (let ((result (efrit-do--extract-fields ht '("name" "count"))))
      (should (= (length result) 2))
      (should (equal (alist-get 'name result) "value1"))
      (should (equal (alist-get 'count result) 42)))))

(ert-deftest test-extract-fields-with-transform ()
  "Test extract-fields with transformation function."
  (let ((ht (make-hash-table :test 'equal)))
    (puthash "items" [1 2 3] ht)
    (let ((result (efrit-do--extract-fields
                   ht `(("items" . ,#'efrit-do--vector-to-list)))))
      (should (equal (alist-get 'items result) '(1 2 3))))))

(ert-deftest test-extract-fields-skips-nil ()
  "Test extract-fields skips nil values."
  (let ((ht (make-hash-table :test 'equal)))
    (puthash "present" "value" ht)
    ;; "missing" is not in hash table
    (let ((result (efrit-do--extract-fields ht '("present" "missing"))))
      (should (= (length result) 1))
      (should (null (alist-get 'missing result))))))

(ert-deftest test-vector-to-list ()
  "Test vector-to-list conversion."
  (should (equal (efrit-do--vector-to-list [1 2 3]) '(1 2 3)))
  (should (equal (efrit-do--vector-to-list '(a b c)) '(a b c)))
  (should (equal (efrit-do--vector-to-list []) nil)))

(provide 'test-do-handlers)

;;; test-do-handlers.el ends here
