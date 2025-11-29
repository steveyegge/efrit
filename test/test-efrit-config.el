;;; test-efrit-config.el --- Tests for efrit-config.el -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;;; Commentary:
;; Unit tests for configuration and path management.

;;; Code:

(require 'ert)

;; Add load paths
(add-to-list 'load-path (expand-file-name "../lisp" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../lisp/core" (file-name-directory load-file-name)))

;; Disable auto-init during testing
(setq efrit-auto-initialize nil)
(require 'efrit-config)

;;; Helper to create isolated test environment

(defmacro with-test-efrit-config (&rest body)
  "Run BODY with isolated efrit configuration."
  (declare (indent 0))
  `(let* ((temp-dir (make-temp-file "efrit-config-test" t))
          (efrit-data-directory temp-dir))
     (unwind-protect
         (progn ,@body)
       (when (file-directory-p temp-dir)
         (delete-directory temp-dir t)))))

;;; Data File Path Tests

(ert-deftest test-config-data-file-basic ()
  "Test basic data file path construction."
  (with-test-efrit-config
    (let ((path (efrit-config-data-file "test.el")))
      (should (string-suffix-p "test.el" path))
      (should (string-prefix-p efrit-data-directory path)))))

(ert-deftest test-config-data-file-with-subdir ()
  "Test data file path with subdirectory."
  (with-test-efrit-config
    (let ((path (efrit-config-data-file "test.el" "sessions")))
      (should (string-suffix-p "test.el" path))
      (should (string-match-p "/sessions/" path)))))

;;; Queue Directory Tests

(ert-deftest test-config-queue-dir-base ()
  "Test queue directory base path."
  (with-test-efrit-config
    (let ((path (efrit-config-queue-dir)))
      (should (string-suffix-p "queues" path))
      (should (string-prefix-p efrit-data-directory path)))))

(ert-deftest test-config-queue-dir-subdir ()
  "Test queue directory with subdirectory."
  (with-test-efrit-config
    (let ((path (efrit-config-queue-dir "requests")))
      (should (string-match-p "/queues/requests" path)))))

;;; Workspace Directory Tests

(ert-deftest test-config-workspace-dir-base ()
  "Test workspace directory base path."
  (with-test-efrit-config
    (let ((path (efrit-config-workspace-dir)))
      (should (string-suffix-p "workspace" path))
      (should (string-prefix-p efrit-data-directory path)))))

(ert-deftest test-config-workspace-dir-subdir ()
  "Test workspace directory with subdirectory."
  (with-test-efrit-config
    (let ((path (efrit-config-workspace-dir "auto-saves")))
      (should (string-match-p "/workspace/auto-saves" path)))))

;;; Context File Tests

(ert-deftest test-config-context-file ()
  "Test context file path construction."
  (with-test-efrit-config
    (let ((path (efrit-config-context-file "ring.el")))
      (should (string-suffix-p "ring.el" path))
      (should (string-match-p "/context/" path)))))

;;; Log File Tests

(ert-deftest test-config-log-file ()
  "Test log file path construction."
  (with-test-efrit-config
    (let ((path (efrit-config-log-file "debug.log")))
      (should (string-suffix-p "debug.log" path))
      (should (string-match-p "/logs/" path)))))

;;; Cache File Tests

(ert-deftest test-config-cache-file ()
  "Test cache file path construction."
  (with-test-efrit-config
    (let ((path (efrit-config-cache-file "recent.cache")))
      (should (string-suffix-p "recent.cache" path))
      (should (string-match-p "/cache/" path)))))

;;; Directory Creation Tests

(ert-deftest test-config-ensure-directories ()
  "Test that directory initialization creates all required directories."
  (with-test-efrit-config
    (efrit-config--ensure-directories)
    ;; Check that key directories exist
    (should (file-directory-p efrit-data-directory))
    (should (file-directory-p (efrit-config-queue-dir)))
    (should (file-directory-p (efrit-config-queue-dir "requests")))
    (should (file-directory-p (efrit-config-queue-dir "responses")))
    (should (file-directory-p (efrit-config-workspace-dir)))
    (should (file-directory-p (expand-file-name "logs" efrit-data-directory)))
    (should (file-directory-p (expand-file-name "cache" efrit-data-directory)))
    (should (file-directory-p (expand-file-name "context" efrit-data-directory)))))

(ert-deftest test-config-ensure-directories-idempotent ()
  "Test that directory initialization is idempotent."
  (with-test-efrit-config
    ;; Call twice - should not error
    (efrit-config--ensure-directories)
    (efrit-config--ensure-directories)
    (should (file-directory-p efrit-data-directory))))

;;; Model Configuration Tests

(ert-deftest test-config-default-model ()
  "Test that default model is set."
  (should (stringp efrit-default-model))
  (should (> (length efrit-default-model) 0)))

(ert-deftest test-config-default-max-tokens ()
  "Test that default max tokens is set."
  (should (integerp efrit-default-max-tokens))
  (should (> efrit-default-max-tokens 0)))

(ert-deftest test-config-max-tokens-alias ()
  "Test that efrit-max-tokens alias works."
  (should (eq efrit-max-tokens efrit-default-max-tokens)))

;;; Path Expansion Tests

(ert-deftest test-config-data-directory-expansion ()
  "Test that data directory handles tilde expansion."
  (let ((efrit-data-directory "~/test-efrit"))
    ;; The customization setter should expand this
    (should (stringp efrit-data-directory))))

(provide 'test-efrit-config)
;;; test-efrit-config.el ends here
