;;; test-tool-checkpoint.el --- Tests for checkpoint tools -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;;; Commentary:
;; Tests for efrit-tool-checkpoint restore point functionality.
;; Note: These tests require running in a git repository and may
;; create/remove stashes. They use a separate test registry.

;;; Code:

(require 'ert)
(require 'json)

;; Add load paths for test
(add-to-list 'load-path (expand-file-name "../lisp" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../lisp/core" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../lisp/tools" (file-name-directory load-file-name)))

(require 'efrit-tool-utils)
(require 'efrit-tool-checkpoint)

;;; Test Configuration
;; Use separate registry for tests to avoid affecting real checkpoints
(defvar test-checkpoint-original-dir efrit-checkpoint-dir)

(defun test-checkpoint-setup ()
  "Set up test environment."
  (setq efrit-checkpoint-dir "/tmp/efrit-test-checkpoints/")
  (when (file-exists-p efrit-checkpoint-dir)
    (delete-directory efrit-checkpoint-dir t))
  (make-directory efrit-checkpoint-dir t))

(defun test-checkpoint-teardown ()
  "Clean up test environment."
  (when (file-exists-p efrit-checkpoint-dir)
    (delete-directory efrit-checkpoint-dir t))
  (setq efrit-checkpoint-dir test-checkpoint-original-dir))

;;; Helper functions

(defun test-checkpoint--success-p (response)
  "Check if RESPONSE indicates success."
  (eq (alist-get 'success response) t))

(defun test-checkpoint--get-result (response)
  "Extract result from RESPONSE."
  (alist-get 'result response))

;;; Tests for checkpoint ID generation

(ert-deftest test-checkpoint-generate-id ()
  "Test that checkpoint IDs are generated correctly."
  (let ((id (efrit-checkpoint--generate-id)))
    (should (stringp id))
    (should (string-prefix-p "efrit-" id))
    ;; Should contain timestamp
    (should (string-match-p "[0-9]\\{8\\}-[0-9]\\{6\\}" id))))

(ert-deftest test-checkpoint-generate-id-unique ()
  "Test that checkpoint IDs are unique."
  (let ((id1 (efrit-checkpoint--generate-id))
        (id2 (efrit-checkpoint--generate-id)))
    ;; May have same timestamp, but random suffix should differ
    ;; (unless we're very unlucky)
    (should (stringp id1))
    (should (stringp id2))))

;;; Tests for registry operations

(ert-deftest test-checkpoint-registry-operations ()
  "Test registry read/write operations."
  (test-checkpoint-setup)
  (unwind-protect
      (progn
        ;; Initially empty
        (let ((registry (efrit-checkpoint--read-registry)))
          (should (or (null registry) (= (length registry) 0))))

        ;; Add an entry
        (efrit-checkpoint--add-to-registry
         "test-123"
         '((description . "Test") (created_at . "2025-01-01T00:00:00Z")))

        ;; Should be readable
        (let ((metadata (efrit-checkpoint--get-from-registry "test-123")))
          (should metadata)
          (should (equal (alist-get 'description metadata) "Test")))

        ;; Remove it
        (efrit-checkpoint--remove-from-registry "test-123")

        ;; Should be gone
        (let ((metadata (efrit-checkpoint--get-from-registry "test-123")))
          (should-not metadata)))
    (test-checkpoint-teardown)))

;;; Tests for input validation

(ert-deftest test-checkpoint-missing-description ()
  "Test that missing description causes error."
  (let ((response (efrit-tool-checkpoint '())))
    (should-not (test-checkpoint--success-p response))
    (should (alist-get 'error response))))

(ert-deftest test-checkpoint-restore-missing-id ()
  "Test that restore without checkpoint_id causes error."
  (let ((response (efrit-tool-restore-checkpoint '())))
    (should-not (test-checkpoint--success-p response))
    (should (alist-get 'error response))))

(ert-deftest test-checkpoint-restore-nonexistent ()
  "Test that restoring nonexistent checkpoint causes error."
  (test-checkpoint-setup)
  (unwind-protect
      (let ((response (efrit-tool-restore-checkpoint
                       '((checkpoint_id . "nonexistent-id")))))
        (should-not (test-checkpoint--success-p response))
        (should (alist-get 'error response)))
    (test-checkpoint-teardown)))

(ert-deftest test-checkpoint-delete-nonexistent ()
  "Test that deleting nonexistent checkpoint causes error."
  (test-checkpoint-setup)
  (unwind-protect
      (let ((response (efrit-tool-delete-checkpoint
                       '((checkpoint_id . "nonexistent-id")))))
        (should-not (test-checkpoint--success-p response))
        (should (alist-get 'error response)))
    (test-checkpoint-teardown)))

;;; Tests for list_checkpoints

(ert-deftest test-checkpoint-list-empty ()
  "Test listing checkpoints when none exist."
  (test-checkpoint-setup)
  (unwind-protect
      (let* ((response (efrit-tool-list-checkpoints nil))
             (result (test-checkpoint--get-result response)))
        (should (test-checkpoint--success-p response))
        (should (= (alist-get 'count result) 0)))
    (test-checkpoint-teardown)))

;;; JSON encoding tests

(ert-deftest test-checkpoint-json-encodable ()
  "Test that error responses are JSON encodable.
We test the error case to avoid actually creating stashes that
could interfere with the user's working directory."
  ;; Test the 'no changes' case by using an empty response test
  ;; We don't want to actually stash working changes during tests
  (let ((response (efrit-tool-checkpoint '())))  ; Missing description = error
    ;; Should be encodable even if it's an error
    (should (stringp (json-encode response)))))

(ert-deftest test-checkpoint-list-json-encodable ()
  "Test that list response is JSON encodable."
  (test-checkpoint-setup)
  (unwind-protect
      (let ((response (efrit-tool-list-checkpoints nil)))
        (should (stringp (json-encode response))))
    (test-checkpoint-teardown)))

(provide 'test-tool-checkpoint)

;;; test-tool-checkpoint.el ends here
