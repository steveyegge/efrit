;;; test-efrit-diff.el --- Tests for efrit-diff module -*- lexical-binding: t -*-

(require 'ert)

;; Add load paths
(add-to-list 'load-path (expand-file-name "../lisp" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../lisp/core" (file-name-directory load-file-name)))

(require 'efrit-diff)

;;; Test Data

(defconst test-diff-content
  "diff --git a/test.el b/test.el
index 1234567..abcdef0 100644
--- a/test.el
+++ b/test.el
@@ -1,3 +1,3 @@
 ;; Test file
-(defun old-func ()
+(defun new-func ()
   (message \"Hello\"))
"
  "Sample unified diff for testing.")

(defconst test-diff-file-headers
  "--- a/file1.txt
+++ b/file1.txt
"
  "Diff with just file headers.")

(defconst test-diff-hunk-header
  "@@ -1,3 +1,4 @@
"
  "Diff with hunk header.")

(defconst test-non-diff-content
  "This is just plain text
It doesn't look like diff
No special markers here
"
  "Non-diff content for testing.")

;;; Creation Tests

(ert-deftest efrit-diff-from-string ()
  "Test creating diff from string."
  (let ((diff (efrit-diff-from-string test-diff-content)))
    (should (efrit-diff-p diff))
    (should (equal (efrit-diff-get-content diff) test-diff-content))
    (should (equal (efrit-diff-get-format diff) "unified"))))

(ert-deftest efrit-diff-from-string-with-metadata ()
  "Test creating diff from string with metadata."
  (let* ((metadata '((file . "test.el") (status . "modified")))
         (diff (efrit-diff-from-string test-diff-content metadata)))
    (should (equal (efrit-diff-get-metadata diff) metadata))))

(ert-deftest efrit-diff-from-string-invalid ()
  "Test that invalid diff string returns nil."
  (let ((diff (efrit-diff-from-string test-non-diff-content)))
    (should (equal diff nil))))

(ert-deftest efrit-diff-from-alist ()
  "Test creating diff from alist."
  (let* ((alist `((tool . "vcs_diff") (diff . ,test-diff-content)))
         (diff (efrit-diff-from-alist alist)))
    (should (efrit-diff-p diff))
    (should (equal (efrit-diff-get-content diff) test-diff-content))))

(ert-deftest efrit-diff-from-alist-no-diff-key ()
  "Test that alist without diff key returns nil."
  (let* ((alist '((tool . "other-tool") (result . "something")))
         (diff (efrit-diff-from-alist alist)))
    (should (equal diff nil))))

(ert-deftest efrit-diff-from-result-string ()
  "Test extracting diff from result string."
  (let ((diff (efrit-diff-from-result test-diff-content)))
    (should (efrit-diff-p diff))
    (should (equal (efrit-diff-get-content diff) test-diff-content))))

(ert-deftest efrit-diff-from-result-alist ()
  "Test extracting diff from result alist."
  (let* ((alist `((tool . "vcs_diff") (diff . ,test-diff-content)))
         (diff (efrit-diff-from-result alist)))
    (should (efrit-diff-p diff))))

(ert-deftest efrit-diff-from-result-stringified ()
  "Test extracting diff from stringified result."
  (let* ((result (concat "some output\n" test-diff-content))
         (diff (efrit-diff-from-result result)))
    (should (efrit-diff-p diff))))

(ert-deftest efrit-diff-from-result-nil ()
  "Test that nil result returns nil."
  (let ((diff (efrit-diff-from-result nil)))
    (should (equal diff nil))))

;;; Validation Tests

(ert-deftest efrit-diff-valid-p-true ()
  "Test validation of valid diff."
  (let ((diff (efrit-diff-from-string test-diff-content)))
    (should (efrit-diff-valid-p diff))))

(ert-deftest efrit-diff-looks-like-diff-file-headers ()
  "Test that file headers are recognized as diff."
  (should (efrit-diff--looks-like-diff-p test-diff-file-headers)))

(ert-deftest efrit-diff-looks-like-diff-hunk ()
  "Test that hunk headers are recognized as diff."
  (should (efrit-diff--looks-like-diff-p test-diff-hunk-header)))

(ert-deftest efrit-diff-looks-like-diff-git ()
  "Test that git diff headers are recognized."
  (should (efrit-diff--looks-like-diff-p "diff --git a/file.txt b/file.txt")))

(ert-deftest efrit-diff-looks-like-diff-negative ()
  "Test that non-diff content is rejected."
  (should (not (efrit-diff--looks-like-diff-p test-non-diff-content))))

;;; Line Type Tests

(ert-deftest efrit-diff-line-type-git-header ()
  "Test recognizing git diff header."
  (should (equal (efrit-diff-line-type "diff --git a/file.txt b/file.txt") 'header)))

(ert-deftest efrit-diff-line-type-file-header-minus ()
  "Test recognizing --- file header."
  (should (equal (efrit-diff-line-type "--- a/file.txt") 'header)))

(ert-deftest efrit-diff-line-type-file-header-plus ()
  "Test recognizing +++ file header."
  (should (equal (efrit-diff-line-type "+++ b/file.txt") 'header)))

(ert-deftest efrit-diff-line-type-hunk ()
  "Test recognizing hunk header."
  (should (equal (efrit-diff-line-type "@@ -1,3 +1,4 @@") 'hunk)))

(ert-deftest efrit-diff-line-type-add ()
  "Test recognizing added line."
  (should (equal (efrit-diff-line-type "+new line content") 'add)))

(ert-deftest efrit-diff-line-type-remove ()
  "Test recognizing removed line."
  (should (equal (efrit-diff-line-type "-old line content") 'remove)))

(ert-deftest efrit-diff-line-type-context ()
  "Test recognizing context line."
  (should (equal (efrit-diff-line-type " unchanged line") 'context)))

;;; Statistics Tests

(ert-deftest efrit-diff-line-count ()
  "Test counting total lines."
  (let ((diff (efrit-diff-from-string test-diff-content)))
    (should (> (efrit-diff-line-count diff) 0))))

(ert-deftest efrit-diff-additions-count ()
  "Test counting added lines."
  (let ((diff (efrit-diff-from-string test-diff-content)))
    (should (> (efrit-diff-additions-count diff) 0))))

(ert-deftest efrit-diff-deletions-count ()
  "Test counting removed lines."
  (let ((diff (efrit-diff-from-string test-diff-content)))
    (should (> (efrit-diff-deletions-count diff) 0))))

(ert-deftest efrit-diff-files-affected ()
  "Test counting files affected."
  (let ((diff (efrit-diff-from-string test-diff-content)))
    (should (equal (efrit-diff-files-affected diff) 1))))

(ert-deftest efrit-diff-files-affected-multiple ()
  "Test counting multiple files in diff."
  (let* ((content (concat "diff --git a/file1.txt b/file1.txt\n"
                          "--- a/file1.txt\n"
                          "+++ b/file1.txt\n"
                          "diff --git a/file2.txt b/file2.txt\n"
                          "--- a/file2.txt\n"
                          "+++ b/file2.txt\n"))
         (diff (efrit-diff-from-string content)))
    (should (equal (efrit-diff-files-affected diff) 2))))

;;; Formatting Tests

(ert-deftest efrit-diff-format-for-display ()
  "Test formatting diff for display."
  (let ((diff (efrit-diff-from-string test-diff-content)))
    (let ((formatted (efrit-diff-format-for-display diff)))
      (should (stringp formatted))
      (should (> (length formatted) 0)))))

(ert-deftest efrit-diff-format-for-display-with-indent ()
  "Test formatting diff with indent."
  (let ((diff (efrit-diff-from-string test-diff-content)))
    (let ((formatted (efrit-diff-format-for-display diff "  ")))
      (should (stringp formatted))
      (should (string-match "  " formatted)))))

(ert-deftest efrit-diff-format-line ()
  "Test formatting a single diff line."
  (let ((formatted (efrit-diff-format-line "+new content" "")))
    (should (stringp formatted))
    (should (string-match "new content" formatted))))

;;; Edge Cases

(ert-deftest efrit-diff-empty-string ()
  "Test that empty string is not a valid diff."
  (let ((diff (efrit-diff-from-string "")))
    (should (equal diff nil))))

(ert-deftest efrit-diff-whitespace-only ()
  "Test that whitespace-only string is not a valid diff."
  (let ((diff (efrit-diff-from-string "   \n\t\n   ")))
    (should (equal diff nil))))

(ert-deftest efrit-diff-only-header ()
  "Test diff with only file headers."
  (let ((content "--- a/test.txt\n+++ b/test.txt\n"))
    (let ((diff (efrit-diff-from-string content)))
      (should (efrit-diff-p diff))
      (should (> (efrit-diff-line-count diff) 0)))))

(provide 'test-efrit-diff)

;;; test-efrit-diff.el ends here
