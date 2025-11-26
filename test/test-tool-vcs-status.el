;;; test-tool-vcs-status.el --- Tests for vcs_status tool -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;;; Commentary:
;; Tests for efrit-tool-vcs-status git status functionality.
;; Note: These tests require running in a git repository.

;;; Code:

(require 'ert)
(require 'json)

;; Add load paths for test
(add-to-list 'load-path (expand-file-name "../lisp" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../lisp/core" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../lisp/tools" (file-name-directory load-file-name)))

(require 'efrit-tool-utils)
(require 'efrit-tool-vcs-status)

;;; Helper functions

(defun test-vcs-status--get-result (response)
  "Extract result from RESPONSE alist."
  (alist-get 'result response))

(defun test-vcs-status--success-p (response)
  "Check if RESPONSE indicates success."
  (eq (alist-get 'success response) t))

;;; Basic functionality tests

(ert-deftest test-vcs-status-basic ()
  "Test basic vcs status."
  (let ((response (efrit-tool-vcs-status '())))
    (should (test-vcs-status--success-p response))
    (let ((result (test-vcs-status--get-result response)))
      ;; Should have branch info
      (should (alist-get 'current_branch result))
      ;; Should have file arrays (may be empty)
      (should (vectorp (alist-get 'staged_files result)))
      (should (vectorp (alist-get 'unstaged_files result)))
      (should (vectorp (alist-get 'untracked_files result))))))

(ert-deftest test-vcs-status-branch-info ()
  "Test that branch info is populated."
  (let* ((response (efrit-tool-vcs-status '()))
         (result (test-vcs-status--get-result response)))
    (should (test-vcs-status--success-p response))
    ;; Current branch should be a non-empty string
    (should (stringp (alist-get 'current_branch result)))
    (should (> (length (alist-get 'current_branch result)) 0))))

(ert-deftest test-vcs-status-tracking-info ()
  "Test that tracking info fields exist."
  (let* ((response (efrit-tool-vcs-status '()))
         (result (test-vcs-status--get-result response)))
    (should (test-vcs-status--success-p response))
    ;; These fields should exist (values depend on branch state)
    (should (or (null (alist-get 'upstream result))
                (stringp (alist-get 'upstream result))))
    (should (integerp (alist-get 'ahead result)))
    (should (integerp (alist-get 'behind result)))
    (should (member (alist-get 'detached result) '(t :json-false)))))

(ert-deftest test-vcs-status-stash-count ()
  "Test that stash count is present."
  (let* ((response (efrit-tool-vcs-status '()))
         (result (test-vcs-status--get-result response)))
    (should (test-vcs-status--success-p response))
    ;; Stash count should be an integer >= 0
    (should (integerp (alist-get 'stash_count result)))
    (should (>= (alist-get 'stash_count result) 0))))

(ert-deftest test-vcs-status-recent-commits ()
  "Test that recent commits are returned."
  (let* ((response (efrit-tool-vcs-status '()))
         (result (test-vcs-status--get-result response))
         (commits (alist-get 'recent_commits result)))
    (should (test-vcs-status--success-p response))
    (should (vectorp commits))
    ;; Should have some commits in efrit repo
    (should (> (length commits) 0))
    ;; Check commit structure
    (let ((first-commit (aref commits 0)))
      (should (alist-get 'hash first-commit))
      (should (alist-get 'message first-commit)))))

(ert-deftest test-vcs-status-commit-hash-format ()
  "Test that commit hashes have correct format."
  (let* ((response (efrit-tool-vcs-status '()))
         (result (test-vcs-status--get-result response))
         (commits (alist-get 'recent_commits result)))
    (should (test-vcs-status--success-p response))
    (when (> (length commits) 0)
      (let* ((first-commit (aref commits 0))
             (hash (alist-get 'hash first-commit)))
        ;; Hash should be hexadecimal (short form)
        (should (string-match-p "^[0-9a-f]+$" hash))))))

;;; Clean/dirty state tests

(ert-deftest test-vcs-status-clean-indicator ()
  "Test that is_clean indicator is present."
  (let* ((response (efrit-tool-vcs-status '()))
         (result (test-vcs-status--get-result response)))
    (should (test-vcs-status--success-p response))
    ;; is_clean should be a boolean
    (should (member (alist-get 'is_clean result) '(t :json-false)))))

(ert-deftest test-vcs-status-clean-calculation ()
  "Test that is_clean is calculated correctly."
  (let* ((response (efrit-tool-vcs-status '()))
         (result (test-vcs-status--get-result response))
         (staged (alist-get 'staged_files result))
         (unstaged (alist-get 'unstaged_files result))
         (untracked (alist-get 'untracked_files result))
         (is-clean (alist-get 'is_clean result)))
    (should (test-vcs-status--success-p response))
    ;; If all arrays are empty, should be clean
    (when (and (= (length staged) 0)
               (= (length unstaged) 0)
               (= (length untracked) 0))
      (should (eq is-clean t)))
    ;; If any array is non-empty, should not be clean
    (when (or (> (length staged) 0)
              (> (length unstaged) 0)
              (> (length untracked) 0))
      (should (eq is-clean :json-false)))))

;;; Special state tests

(ert-deftest test-vcs-status-special-states ()
  "Test that special state indicators are present."
  (let* ((response (efrit-tool-vcs-status '()))
         (result (test-vcs-status--get-result response)))
    (should (test-vcs-status--success-p response))
    ;; All these should be boolean indicators
    (should (member (alist-get 'is_rebasing result) '(t :json-false)))
    (should (member (alist-get 'is_merging result) '(t :json-false)))
    (should (member (alist-get 'is_cherry_picking result) '(t :json-false)))
    (should (member (alist-get 'is_reverting result) '(t :json-false)))
    (should (member (alist-get 'is_bisecting result) '(t :json-false)))))

(ert-deftest test-vcs-status-normal-state ()
  "Test that in normal state, special indicators are false."
  ;; This test assumes we're not in the middle of a rebase/merge
  (let* ((response (efrit-tool-vcs-status '()))
         (result (test-vcs-status--get-result response)))
    (should (test-vcs-status--success-p response))
    ;; In a normal working state (not rebasing, etc.)
    ;; Note: We can't guarantee this, so just verify types
    (should (member (alist-get 'is_rebasing result) '(t :json-false)))))

;;; File status format tests

(ert-deftest test-vcs-status-staged-file-format ()
  "Test staged file format when present."
  (let* ((response (efrit-tool-vcs-status '()))
         (result (test-vcs-status--get-result response))
         (staged (alist-get 'staged_files result)))
    (should (test-vcs-status--success-p response))
    ;; If there are staged files, check format
    (when (> (length staged) 0)
      (let ((first-file (aref staged 0)))
        (should (alist-get 'path first-file))
        (should (alist-get 'status first-file))))))

(ert-deftest test-vcs-status-unstaged-file-format ()
  "Test unstaged file format when present."
  (let* ((response (efrit-tool-vcs-status '()))
         (result (test-vcs-status--get-result response))
         (unstaged (alist-get 'unstaged_files result)))
    (should (test-vcs-status--success-p response))
    ;; If there are unstaged files, check format
    (when (> (length unstaged) 0)
      (let ((first-file (aref unstaged 0)))
        (should (alist-get 'path first-file))
        (should (alist-get 'status first-file))))))

(ert-deftest test-vcs-status-untracked-file-format ()
  "Test untracked file format when present."
  (let* ((response (efrit-tool-vcs-status '()))
         (result (test-vcs-status--get-result response))
         (untracked (alist-get 'untracked_files result)))
    (should (test-vcs-status--success-p response))
    ;; Untracked files are just strings (paths)
    (when (> (length untracked) 0)
      (should (stringp (aref untracked 0))))))

;;; Result completeness tests

(ert-deftest test-vcs-status-all-fields-present ()
  "Test that all expected fields are present in response."
  (let* ((response (efrit-tool-vcs-status '()))
         (result (test-vcs-status--get-result response)))
    (should (test-vcs-status--success-p response))
    ;; Check all expected fields exist
    (should (assq 'current_branch result))
    (should (assq 'upstream result))
    (should (assq 'ahead result))
    (should (assq 'behind result))
    (should (assq 'detached result))
    (should (assq 'staged_files result))
    (should (assq 'unstaged_files result))
    (should (assq 'untracked_files result))
    (should (assq 'stash_count result))
    (should (assq 'recent_commits result))
    (should (assq 'is_clean result))
    (should (assq 'is_rebasing result))
    (should (assq 'is_merging result))
    (should (assq 'is_cherry_picking result))
    (should (assq 'is_reverting result))
    (should (assq 'is_bisecting result))))

;;; JSON encoding test

(ert-deftest test-vcs-status-json-encodable ()
  "Test that response is JSON encodable."
  (let ((response (efrit-tool-vcs-status '())))
    (should (stringp (json-encode response)))))

(provide 'test-tool-vcs-status)

;;; test-tool-vcs-status.el ends here
