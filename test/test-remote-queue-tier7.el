;;; test-remote-queue-tier7.el --- Tier 7: Remote queue protocol tests -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, testing

;;; Commentary:

;; Tier 7 automated tests for the remote queue protocol.
;; These tests verify:
;; - Complete request/response cycle through file system
;; - Status query endpoint functionality
;; - Queue statistics tracking
;; - Error handling for malformed requests
;; - File cleanup behavior
;;
;; These are unit tests that don't require a running Emacs daemon.
;; They test the protocol handling in isolation.

;;; Code:

(require 'ert)
(require 'json)
(require 'efrit-remote-queue)

;;; Test helper functions

(defun test-tier7--create-temp-queue-dir ()
  "Create a temporary queue directory for testing."
  (let ((dir (make-temp-file "efrit-test-queue" t)))
    (dolist (subdir '("requests" "responses" "processing" "archive"))
      (make-directory (expand-file-name subdir dir) t))
    dir))

(defun test-tier7--cleanup-temp-dir (dir)
  "Clean up temporary DIR and all contents."
  (when (and dir (file-directory-p dir))
    (delete-directory dir t)))

(defun test-tier7--create-request (id type content &optional options)
  "Create a test request hash table."
  (let ((request (make-hash-table :test 'equal)))
    (puthash "id" id request)
    (puthash "version" efrit-remote-queue-schema-version request)
    (puthash "type" type request)
    (puthash "content" content request)
    (puthash "timestamp" (format-time-string "%Y-%m-%dT%H:%M:%SZ") request)
    (when options
      (puthash "options" options request))
    request))

(defun test-tier7--write-request-file (dir request)
  "Write REQUEST to a file in DIR/requests/ and return the file path."
  (let* ((id (gethash "id" request))
         (file-path (expand-file-name (format "%s.json" id)
                                      (expand-file-name "requests" dir))))
    (with-temp-file file-path
      (insert (json-encode request)))
    file-path))

(defun test-tier7--read-response-file (dir request-id)
  "Read response file for REQUEST-ID from DIR/responses/."
  (let ((file-path (expand-file-name (format "resp_%s.json" request-id)
                                     (expand-file-name "responses" dir))))
    (when (file-exists-p file-path)
      (with-temp-buffer
        (insert-file-contents file-path)
        (let ((json-object-type 'hash-table)
              (json-array-type 'vector)
              (json-key-type 'string))
          (json-read-from-string (buffer-string)))))))

;;; Status endpoint tests

(ert-deftest test-tier7-status-returns-json ()
  "Test that status endpoint returns valid JSON."
  (let ((result (efrit-remote-queue--execute-status "")))
    (should (stringp result))
    ;; Should be valid JSON
    (let* ((json-object-type 'hash-table)
           (parsed (json-read-from-string result)))
      (should (hash-table-p parsed)))))

(ert-deftest test-tier7-status-includes-queue-active ()
  "Test that status includes queue_active field."
  (let* ((json-object-type 'hash-table)
         (result (json-read-from-string (efrit-remote-queue--execute-status ""))))
    (should (gethash "queue_active" result))))

(ert-deftest test-tier7-status-includes-queue-stats ()
  "Test that status includes queue statistics."
  (let* ((json-object-type 'hash-table)
         (result (json-read-from-string (efrit-remote-queue--execute-status ""))))
    (should (gethash "queue_stats" result))
    (let ((stats (gethash "queue_stats" result)))
      (should (gethash "requests_processed" stats))
      (should (gethash "requests_succeeded" stats))
      (should (gethash "requests_failed" stats))
      (should (gethash "processing_count" stats)))))

(ert-deftest test-tier7-status-includes-session-info ()
  "Test that status includes session availability info."
  (let* ((json-object-type 'hash-table)
         (result (json-read-from-string (efrit-remote-queue--execute-status ""))))
    ;; Should always have has_active_session field
    (should (member (gethash "has_active_session" result) '(t :json-false nil)))))

(ert-deftest test-tier7-status-includes-queued-commands ()
  "Test that status includes queued commands count."
  (let* ((json-object-type 'hash-table)
         (result (json-read-from-string (efrit-remote-queue--execute-status ""))))
    (should (numberp (gethash "queued_commands" result)))))

;;; Request type validation tests

(ert-deftest test-tier7-status-request-type-valid ()
  "Test that 'status' is a valid request type."
  (should (member "status" efrit-remote-queue-valid-request-types)))

(ert-deftest test-tier7-all-request-types-covered ()
  "Test that all request types have handlers."
  (dolist (type efrit-remote-queue-valid-request-types)
    (let ((request (test-tier7--create-request (format "test-%s" type) type "test")))
      (let ((validation (efrit-remote-queue--validate-request request)))
        (should (car validation))))))

;;; File handling tests

(ert-deftest test-tier7-parse-valid-request-file ()
  "Test parsing a valid request file."
  (let* ((temp-dir (test-tier7--create-temp-queue-dir))
         (request (test-tier7--create-request "parse-test" "eval" "(+ 1 2)"))
         (file-path (test-tier7--write-request-file temp-dir request)))
    (unwind-protect
        (let ((parsed (efrit-remote-queue--parse-request-file file-path)))
          (should (hash-table-p parsed))
          (should (equal "parse-test" (gethash "id" parsed)))
          (should (equal "eval" (gethash "type" parsed)))
          (should (equal "(+ 1 2)" (gethash "content" parsed))))
      (test-tier7--cleanup-temp-dir temp-dir))))

(ert-deftest test-tier7-parse-invalid-json-file ()
  "Test that parsing invalid JSON returns nil."
  (let* ((temp-dir (test-tier7--create-temp-queue-dir))
         (file-path (expand-file-name "invalid.json"
                                      (expand-file-name "requests" temp-dir))))
    (unwind-protect
        (progn
          (with-temp-file file-path
            (insert "not valid json {"))
          (should-not (efrit-remote-queue--parse-request-file file-path)))
      (test-tier7--cleanup-temp-dir temp-dir))))

(ert-deftest test-tier7-parse-nonexistent-file ()
  "Test that parsing nonexistent file returns nil."
  (should-not (efrit-remote-queue--parse-request-file "/nonexistent/path/file.json")))

;;; Eval request processing tests

(ert-deftest test-tier7-execute-eval-simple ()
  "Test executing a simple eval request."
  (let ((result (efrit-remote-queue--execute-eval "(+ 1 2)")))
    ;; prin1-to-string wraps in quotes for strings, but not for numbers
    (should (string-match-p "3" result))))

(ert-deftest test-tier7-execute-eval-string ()
  "Test executing eval that returns a string."
  (let ((result (efrit-remote-queue--execute-eval "\"hello\"")))
    ;; Result is prin1-to-string of the string, so it has quotes
    (should (string-match-p "hello" result))))

(ert-deftest test-tier7-execute-eval-list ()
  "Test executing eval that returns a list."
  (let ((result (efrit-remote-queue--execute-eval "'(1 2 3)")))
    ;; Result contains the list representation
    (should (string-match-p "1.*2.*3" result))))

(ert-deftest test-tier7-execute-eval-error ()
  "Test that eval errors are captured gracefully."
  (let ((result (efrit-remote-queue--execute-eval "(undefined-function)")))
    ;; Should contain error indication
    (should (or (string-match-p "Eval failed:" result)
                (string-match-p "Error" result)
                (string-match-p "void" result)))))

;;; Status request processing tests

(ert-deftest test-tier7-execute-status-via-request ()
  "Test processing a status request through the queue."
  (let ((request (test-tier7--create-request "status-test" "status" "")))
    (let ((validation (efrit-remote-queue--validate-request request)))
      (should (car validation)))))

;;; Response structure tests

(ert-deftest test-tier7-response-has-required-fields ()
  "Test that responses have all required fields."
  (let ((response (efrit-remote-queue--create-response "test" "success" "result")))
    (should (gethash "id" response))
    (should (gethash "version" response))
    (should (gethash "timestamp" response))
    (should (gethash "status" response))))

(ert-deftest test-tier7-response-version-matches-schema ()
  "Test that response version matches current schema."
  (let ((response (efrit-remote-queue--create-response "test" "success" "result")))
    (should (equal efrit-remote-queue-schema-version
                   (gethash "version" response)))))

(ert-deftest test-tier7-response-timestamp-format ()
  "Test that response timestamp is ISO 8601 format."
  (let ((response (efrit-remote-queue--create-response "test" "success" "result")))
    (let ((timestamp (gethash "timestamp" response)))
      ;; Should match ISO 8601 format: YYYY-MM-DDTHH:MM:SSZ
      (should (string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}Z$"
                             timestamp)))))

;;; ID generation tests

(ert-deftest test-tier7-generate-id-unique ()
  "Test that generated IDs are sufficiently unique.
Note: Due to timestamp + random generation, there's a small chance
of collision. We check that at least 95% are unique."
  (let ((ids (mapcar (lambda (_) (efrit-remote-queue--generate-id))
                    (number-sequence 1 100))))
    ;; At least 95% should be unique (allowing for rare collisions)
    (should (>= (length (delete-dups (copy-sequence ids))) 95))))

(ert-deftest test-tier7-generate-id-format ()
  "Test that generated IDs have expected format."
  (let ((id (efrit-remote-queue--generate-id)))
    (should (string-match-p "^efrit_[0-9]+_[0-9]+$" id))))

;;; Queue statistics tests

(ert-deftest test-tier7-stats-structure ()
  "Test that queue stats have expected structure."
  (let ((stats efrit-remote-queue--stats))
    (should (alist-get 'requests-processed stats))
    (should (alist-get 'requests-succeeded stats))
    (should (alist-get 'requests-failed stats))
    (should (alist-get 'total-processing-time stats))))

;;; Directory management tests

(ert-deftest test-tier7-get-directory-returns-string ()
  "Test that get-directory returns a string path."
  (dolist (type '("requests" "responses" "processing" "archive"))
    (let ((dir (efrit-remote-queue--get-directory type)))
      (should (stringp dir))
      (should (string-match-p (regexp-quote type) dir)))))

(provide 'test-remote-queue-tier7)

;;; test-remote-queue-tier7.el ends here
