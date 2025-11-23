;;; test-remote-queue-validation.el --- Tests for remote queue validation -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;;; Commentary:
;; Tests for request/response validation and version compatibility
;; in efrit-remote-queue.

;;; Code:

(require 'ert)
(require 'json)
(require 'efrit-remote-queue)

;;; Version validation tests

(ert-deftest test-version-compatibility-valid ()
  "Test that current schema version is accepted."
  (should (efrit-remote-queue--check-version-compatibility "1.0.0")))

(ert-deftest test-version-compatibility-invalid ()
  "Test that incompatible versions are rejected."
  (should-not (efrit-remote-queue--check-version-compatibility "2.0.0"))
  (should-not (efrit-remote-queue--check-version-compatibility "0.9.0"))
  (should-not (efrit-remote-queue--check-version-compatibility "1.0.1"))
  (should-not (efrit-remote-queue--check-version-compatibility "invalid")))

;;; Status validation tests

(ert-deftest test-status-validation-valid ()
  "Test that valid status values are accepted."
  (should (equal "success" (efrit-remote-queue--validate-status "success")))
  (should (equal "error" (efrit-remote-queue--validate-status "error")))
  (should (equal "timeout" (efrit-remote-queue--validate-status "timeout"))))

(ert-deftest test-status-validation-invalid ()
  "Test that invalid status values are rejected."
  (should-error (efrit-remote-queue--validate-status "pending"))
  (should-error (efrit-remote-queue--validate-status "processing"))
  (should-error (efrit-remote-queue--validate-status "completed"))
  (should-error (efrit-remote-queue--validate-status "invalid"))
  (should-error (efrit-remote-queue--validate-status "")))

;;; Request validation tests

(ert-deftest test-request-validation-valid ()
  "Test that a valid request passes validation."
  (let ((request (make-hash-table :test 'equal)))
    (puthash "id" "test-123" request)
    (puthash "version" "1.0.0" request)
    (puthash "type" "command" request)
    (puthash "content" "test content" request)
    (let ((result (efrit-remote-queue--validate-request request)))
      (should (car result))
      (should-not (cdr result)))))

(ert-deftest test-request-validation-missing-id ()
  "Test that request without ID is rejected."
  (let ((request (make-hash-table :test 'equal)))
    (puthash "version" "1.0.0" request)
    (puthash "type" "command" request)
    (puthash "content" "test content" request)
    (let ((result (efrit-remote-queue--validate-request request)))
      (should-not (car result))
      (should (string-match-p "id" (cdr result))))))

(ert-deftest test-request-validation-missing-version ()
  "Test that request without version is rejected."
  (let ((request (make-hash-table :test 'equal)))
    (puthash "id" "test-123" request)
    (puthash "type" "command" request)
    (puthash "content" "test content" request)
    (let ((result (efrit-remote-queue--validate-request request)))
      (should-not (car result))
      (should (string-match-p "version" (cdr result))))))

(ert-deftest test-request-validation-incompatible-version ()
  "Test that request with incompatible version is rejected."
  (let ((request (make-hash-table :test 'equal)))
    (puthash "id" "test-123" request)
    (puthash "version" "2.0.0" request)
    (puthash "type" "command" request)
    (puthash "content" "test content" request)
    (let ((result (efrit-remote-queue--validate-request request)))
      (should-not (car result))
      (should (string-match-p "Incompatible schema version" (cdr result))))))

(ert-deftest test-request-validation-missing-content ()
  "Test that request without content is rejected."
  (let ((request (make-hash-table :test 'equal)))
    (puthash "id" "test-123" request)
    (puthash "version" "1.0.0" request)
    (puthash "type" "command" request)
    (let ((result (efrit-remote-queue--validate-request request)))
      (should-not (car result))
      (should (string-match-p "content" (cdr result))))))

(ert-deftest test-request-validation-invalid-type ()
  "Test that request with invalid type is rejected."
  (let ((request (make-hash-table :test 'equal)))
    (puthash "id" "test-123" request)
    (puthash "version" "1.0.0" request)
    (puthash "type" "invalid" request)
    (puthash "content" "test content" request)
    (let ((result (efrit-remote-queue--validate-request request)))
      (should-not (car result))
      (should (string-match-p "type.*must be one of" (cdr result))))))

(ert-deftest test-request-validation-all-valid-types ()
  "Test that all valid request types are accepted."
  (dolist (type '("command" "chat" "eval"))
    (let ((request (make-hash-table :test 'equal)))
      (puthash "id" "test-123" request)
      (puthash "version" "1.0.0" request)
      (puthash "type" type request)
      (puthash "content" "test content" request)
      (let ((result (efrit-remote-queue--validate-request request)))
        (should (car result))
        (should-not (cdr result))))))

;;; Response creation tests

(ert-deftest test-response-creation-success ()
  "Test creating a success response."
  (let ((response (efrit-remote-queue--create-response "test-123" "success" "result data")))
    (should (hash-table-p response))
    (should (equal "test-123" (gethash "id" response)))
    (should (equal "1.0.0" (gethash "version" response)))
    (should (equal "success" (gethash "status" response)))
    (should (equal "result data" (gethash "result" response)))
    (should (gethash "timestamp" response))))

(ert-deftest test-response-creation-error ()
  "Test creating an error response."
  (let ((response (efrit-remote-queue--create-response "test-123" "error" nil "error message")))
    (should (hash-table-p response))
    (should (equal "test-123" (gethash "id" response)))
    (should (equal "1.0.0" (gethash "version" response)))
    (should (equal "error" (gethash "status" response)))
    (should (equal "error message" (gethash "error" response)))
    (should (gethash "timestamp" response))))

(ert-deftest test-response-creation-timeout ()
  "Test creating a timeout response."
  (let ((response (efrit-remote-queue--create-response "test-123" "timeout")))
    (should (hash-table-p response))
    (should (equal "test-123" (gethash "id" response)))
    (should (equal "1.0.0" (gethash "version" response)))
    (should (equal "timeout" (gethash "status" response)))
    (should (gethash "timestamp" response))))

(ert-deftest test-response-creation-invalid-status ()
  "Test that creating response with invalid status signals error."
  (should-error (efrit-remote-queue--create-response "test-123" "invalid")))

(ert-deftest test-response-creation-with-context ()
  "Test creating response with context information."
  (let* ((context (make-hash-table :test 'equal))
         (response nil))
    (puthash "cwd" "/home/user" context)
    (setq response (efrit-remote-queue--create-response
                    "test-123" "success" "result" nil 1.5 context))
    (should (equal 1.5 (gethash "execution_time" response)))
    (should (hash-table-p (gethash "context" response)))))

;;; Integration test: full request/response cycle

(ert-deftest test-full-validation-cycle ()
  "Test a complete validation cycle from request to response."
  (let ((request (make-hash-table :test 'equal)))
    ;; Create valid request
    (puthash "id" "integration-test" request)
    (puthash "version" "1.0.0" request)
    (puthash "type" "eval" request)
    (puthash "content" "(+ 1 2)" request)

    ;; Validate request
    (let ((validation (efrit-remote-queue--validate-request request)))
      (should (car validation))

      ;; Create response
      (let ((response (efrit-remote-queue--create-response
                      (gethash "id" request) "success" "3")))
        (should (equal "integration-test" (gethash "id" response)))
        (should (equal "1.0.0" (gethash "version" response)))
        (should (equal "success" (gethash "status" response)))
        (should (equal "3" (gethash "result" response)))))))

(provide 'test-remote-queue-validation)
;;; test-remote-queue-validation.el ends here
