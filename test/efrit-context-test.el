;;; efrit-context-test.el --- Tests for context builder -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;;; Commentary:

;; Tests for EIEIO-based context builder.

;;; Code:

(require 'ert)
(require 'efrit-context)

;;; API Context Tests

(ert-deftest efrit-context-api-context-create ()
  "Test creating a basic API context."
  (let ((ctx (make-instance 'efrit-api-context)))
    (should (efrit-api-context-p ctx))
    (should (vectorp (efrit-api-context-get-messages ctx)))
    (should (hash-table-p (efrit-api-context-get-metadata ctx)))
    (should (hash-table-p (efrit-api-context-get-environment ctx)))
    (should (not (efrit-api-context-include-metadata-p ctx)))))

(ert-deftest efrit-context-api-context-with-messages ()
  "Test creating API context with messages."
  (let* ((messages (vector `(("role" . "user") ("content" . "Hello"))
                            `(("role" . "assistant") ("content" . "Hi"))))
         (ctx (make-instance 'efrit-api-context :messages messages)))
    (should (equal (efrit-api-context-get-messages ctx) messages))
    (should (= (length (efrit-api-context-get-messages ctx)) 2))))

(ert-deftest efrit-context-api-context-valid-empty ()
  "Test validation of empty context (invalid)."
  (let ((ctx (make-instance 'efrit-api-context)))
    (let ((validation (efrit-api-context-valid-p ctx)))
      (should (not (car validation)))
      (should (string-match-p "at least one message" (cdr validation))))))

(ert-deftest efrit-context-api-context-valid-with-messages ()
  "Test validation of context with messages (valid)."
  (let* ((messages (vector `(("role" . "user") ("content" . "Hello"))))
         (ctx (make-instance 'efrit-api-context :messages messages)))
    (let ((validation (efrit-api-context-valid-p ctx)))
      (should (car validation))
      (should (null (cdr validation))))))

(ert-deftest efrit-context-api-context-to-api-format-without-metadata ()
  "Test API format conversion without metadata."
  (let* ((messages (vector `(("role" . "user") ("content" . "Hello"))))
         (ctx (make-instance 'efrit-api-context :messages messages)))
    (let ((api-format (efrit-api-context-to-api-format ctx)))
      (should (assoc "messages" api-format))
      (should (not (assoc "metadata" api-format))))))

(ert-deftest efrit-context-api-context-to-api-format-with-metadata ()
  "Test API format conversion with metadata."
  (let* ((messages (vector `(("role" . "user") ("content" . "Hello"))))
         (metadata (make-hash-table :test 'equal))
         (_m (puthash "key1" "value1" metadata))
         (ctx (make-instance 'efrit-api-context
                            :messages messages
                            :metadata metadata
                            :include-metadata t)))
    (let ((api-format (efrit-context-builder-build
                      (let ((b (efrit-context-builder)))
                        (oset b messages messages)
                        (oset b metadata metadata)
                        (oset b include-metadata t)
                        b))))
      (should (assoc "messages" (efrit-api-context-to-api-format api-format)))
      (should (assoc "metadata" (efrit-api-context-to-api-format api-format))))))

;;; Context Builder Tests

(ert-deftest efrit-context-builder-create ()
  "Test creating a context builder."
  (let ((builder (efrit-context-builder)))
    (should (efrit-context-builder-impl-p builder))
    (should (vectorp (oref builder messages)))
    (should (hash-table-p (oref builder metadata)))
    (should (hash-table-p (oref builder environment)))))

(ert-deftest efrit-context-builder-add-single-message ()
  "Test adding a single message to builder."
  (let* ((builder (efrit-context-builder))
         (builder-with-msg (efrit-context-builder-add-message builder 'user "Hello")))
    (should (eq builder builder-with-msg))  ; Returns self for chaining
    (should (= (length (oref builder messages)) 1))
    (let ((msg (aref (oref builder messages) 0)))
      (should (equal (assoc "role" msg) '("role" . "user")))
      (should (equal (assoc "content" msg) '("content" . "Hello"))))))

(ert-deftest efrit-context-builder-add-multiple-messages ()
  "Test adding multiple messages via add-messages."
  (let* ((builder (efrit-context-builder))
         (messages '((:role user :content "First")
                     (:role assistant :content "Second")))
         (builder-with-msgs (efrit-context-builder-add-messages builder messages)))
    (should (eq builder builder-with-msgs))
    (should (= (length (oref builder messages)) 2))))

(ert-deftest efrit-context-builder-chaining ()
  "Test method chaining on builder."
  (let* ((builder (efrit-context-builder))
         (_msg1 (efrit-context-builder-add-message builder 'user "Q1"))
         (_msg2 (efrit-context-builder-add-message builder 'assistant "A1"))
         (_meta (efrit-context-builder-add-metadata builder "key" "value"))
         (_include (efrit-context-builder-include-metadata builder t)))
    (should (= (length (oref builder messages)) 2))
    (should (equal (gethash "key" (oref builder metadata)) "value"))
    (should (oref builder include-metadata))))

(ert-deftest efrit-context-builder-build ()
  "Test building a context from builder."
  (let* ((builder (efrit-context-builder))
         (builder-with-msg (efrit-context-builder-add-message builder 'user "Test"))
         (ctx (efrit-context-builder-build builder-with-msg)))
    (should (efrit-api-context-p ctx))
    (should (= (length (efrit-api-context-get-messages ctx)) 1))))

(ert-deftest efrit-context-builder-metadata ()
  "Test adding metadata to builder."
  (let* ((builder (efrit-context-builder))
         (builder-with-metadata (efrit-context-builder-add-metadata builder "key1" "val1"))
         (builder-with-more (efrit-context-builder-add-metadata builder-with-metadata "key2" "val2")))
    (should (eq builder-with-metadata builder))  ; Modifies in place
    (should (equal (gethash "key1" (oref builder metadata)) "val1"))
    (should (equal (gethash "key2" (oref builder metadata)) "val2"))))

(ert-deftest efrit-context-builder-environment ()
  "Test adding environment to builder."
  (let* ((builder (efrit-context-builder))
         (builder-with-env (efrit-context-builder-add-environment builder "HOME" "/home/user")))
    (should (eq builder-with-env builder))
    (should (equal (gethash "HOME" (oref builder environment)) "/home/user"))))

(ert-deftest efrit-context-builder-set-metadata ()
  "Test setting entire metadata hash table."
  (let* ((builder (efrit-context-builder))
         (meta (make-hash-table :test 'equal))
         (_m1 (puthash "a" "1" meta))
         (_m2 (puthash "b" "2" meta))
         (builder-with-meta (efrit-context-builder-set-metadata builder meta)))
    (should (eq builder-with-meta builder))
    (should (equal (gethash "a" (oref builder metadata)) "1"))
    (should (equal (gethash "b" (oref builder metadata)) "2"))))

(ert-deftest efrit-context-builder-set-environment ()
  "Test setting entire environment hash table."
  (let* ((builder (efrit-context-builder))
         (env (make-hash-table :test 'equal))
         (_e1 (puthash "VAR1" "value1" env))
         (_e2 (puthash "VAR2" "value2" env))
         (builder-with-env (efrit-context-builder-set-environment builder env)))
    (should (eq builder-with-env builder))
    (should (equal (gethash "VAR1" (oref builder environment)) "value1"))
    (should (equal (gethash "VAR2" (oref builder environment)) "value2"))))

(ert-deftest efrit-context-builder-include-metadata ()
  "Test toggling metadata inclusion flag."
  (let* ((builder (efrit-context-builder))
         (builder-included (efrit-context-builder-include-metadata builder t)))
    (should (eq builder-included builder))
    (should (oref builder include-metadata))))

(ert-deftest efrit-context-builder-complex-workflow ()
  "Test a realistic builder workflow."
  (let* ((builder (efrit-context-builder))
         (_msg1 (efrit-context-builder-add-message builder 'user "What is 2+2?"))
         (_msg2 (efrit-context-builder-add-message builder 'assistant "4"))
         (_msg3 (efrit-context-builder-add-message builder 'user "Thank you"))
         (_meta1 (efrit-context-builder-add-metadata builder "session_id" "session-123"))
         (_meta2 (efrit-context-builder-add-metadata builder "mode" "test"))
         (_include (efrit-context-builder-include-metadata builder t))
         (ctx (efrit-context-builder-build builder)))
    (should (= (length (efrit-api-context-get-messages ctx)) 3))
    (should (= (hash-table-count (efrit-api-context-get-metadata ctx)) 2))
    (should (efrit-api-context-include-metadata-p ctx))
    (let ((api-format (efrit-api-context-to-api-format ctx)))
      (should (assoc "messages" api-format))
      (should (assoc "metadata" api-format)))))

;;; Convenience function tests

(ert-deftest efrit-context-create-from-messages-basic ()
  "Test creating context from messages list."
  (let* ((messages '((:role user :content "Hello")
                     (:role assistant :content "Hi")))
         (ctx (efrit-context-create-from-messages messages)))
    (should (efrit-api-context-p ctx))
    (should (= (length (efrit-api-context-get-messages ctx)) 2))))

(ert-deftest efrit-context-create-from-messages-with-metadata ()
  "Test creating context from messages with metadata flag."
  (let* ((messages '((:role user :content "Q")))
         (ctx (efrit-context-create-from-messages messages t)))
    (should (efrit-api-context-include-metadata-p ctx))))

(provide 'efrit-context-test)

;;; efrit-context-test.el ends here
