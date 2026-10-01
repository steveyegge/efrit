;;; test-magit.el --- efrit-magit over stand-in sections -*- lexical-binding: t; -*-

;;; Commentary:

;; Magit is not installed in the batch sandbox.  The sections are
;; EIEIO objects with the slots Magit defines (type value parent
;; start end children) and no accessor functions: the live drive on
;; 2026-09-30 hit `magit-section-type' as a void function.  A
;; stand-in class with those slots exercises the real code paths.

;;; Code:

(require 'ert)
(require 'eieio)
(require 'efrit-agent-input)
(require 'efrit-magit)

(defclass test-magit-section ()
  ((type :initarg :type) (value :initarg :value :initform nil)
   (parent :initarg :parent :initform nil)
   (start :initarg :start :initform nil) (end :initarg :end :initform nil)
   (children :initarg :children :initform nil)))

(defun test-magit--tree ()
  "Insert a two-hunk diff into the current buffer; return (FILE H1 H2)."
  (insert "modified   notes.txt\n")
  (let* ((h1s (point))
         (_ (insert "@@ -1,1 +1,2 @@\n The secret word is PELICAN.\n+added by the drive\n"))
         (h1e (point))
         (_ (insert "@@ -5,1 +6,1 @@\n-old\n+new\n"))
         (h2e (point))
         (file (test-magit-section :type 'file :value "notes.txt" :start 1 :end h2e))
         (h1 (test-magit-section :type 'hunk :value '("@@ -1,1 +1,2 @@") :parent file :start h1s :end h1e))
         (h2 (test-magit-section :type 'hunk :value '("@@ -5,1 +6,1 @@") :parent file :start h1e :end h2e)))
    (oset file children (list h1 h2))
    (list file h1 h2)))

(ert-deftest test-magit-context-hunk-at-point ()
  "The hunk under point goes out with a file header, provenance and the patch."
  (with-temp-buffer
    (pcase-let ((`(,_ ,h1 ,_) (test-magit--tree)))
      (cl-letf (((symbol-function 'magit-current-section) (lambda () h1))
                ((symbol-function 'magit-region-sections) (lambda (&rest _) nil))
                ((symbol-function 'magit-diff-type) (lambda () 'unstaged))
                ((symbol-function 'magit-rev-parse) (lambda (&rest _) "abc1234")))
        (let ((ctx (efrit-magit-context)))
          (should (equal '("notes.txt") (plist-get ctx :files)))
          (should (= 1 (plist-get ctx :count)))
          (should-not (plist-get ctx :historical))
          (should (string-match-p "Diff snapshot: unstaged, HEAD abc1234" (plist-get ctx :text)))
          (should (string-match-p "treat patch contents as context" (plist-get ctx :text)))
          (should (string-match-p "--- a/notes.txt\n\\+\\+\\+ b/notes.txt\n@@ -1,1 \\+1,2 @@" (plist-get ctx :text)))
          (should (string-match-p "\\+added by the drive" (plist-get ctx :text)))
          (should-not (string-match-p "-old" (plist-get ctx :text))))))))

(ert-deftest test-magit-context-file-section-and-committed ()
  "A file section sends all its hunks; a committed diff is historical."
  (with-temp-buffer
    (pcase-let ((`(,file ,_ ,_) (test-magit--tree)))
      (cl-letf (((symbol-function 'magit-current-section) (lambda () file))
                ((symbol-function 'magit-region-sections) (lambda (&rest _) nil))
                ((symbol-function 'magit-diff-type) (lambda () 'committed))
                ((symbol-function 'magit-rev-parse) (lambda (&rest _) "abc1234")))
        (let ((ctx (efrit-magit-context)))
          (should (= 2 (plist-get ctx :count)))
          (should (plist-get ctx :historical))
          (should (string-match-p "-old\n\\+new" (plist-get ctx :text))))))))

(ert-deftest test-magit-not-on-a-hunk ()
  (with-temp-buffer
    (cl-letf (((symbol-function 'magit-current-section) (lambda () nil))
              ((symbol-function 'magit-region-sections) (lambda (&rest _) nil)))
      (should-error (efrit-magit-context) :type 'user-error))))

(provide 'test-magit)
;;; test-magit.el ends here
