;;; test-diff-preview-open.el --- Opening the file at a change from the diff preview -*- lexical-binding: t; -*-

;;; Commentary:
;; The preview's hunk line numbers are relative to what the model sent,
;; so `efrit-diff-preview-open-file' searches the file for the hunk's
;; text instead.

;;; Code:

(require 'ert)
(require 'efrit-tool-show-diff-preview)

(defun test-diff-preview--with-file (body)
  "Run BODY with a temp dir holding sample.txt whose fragment lines 1-3 sit at lines 11-13."
  (let* ((dir (make-temp-file "efrit-diff-" t))
         (file (expand-file-name "sample.txt" dir)))
    (unwind-protect
        (progn
          (with-temp-file file
            (dotimes (i 10) (insert (format "filler %d\n" i)))
            (insert "alpha\nbeta\ngamma\n")
            (dotimes (i 5) (insert (format "tail %d\n" i))))
          (funcall body dir file))
      (dolist (b (buffer-list))
        (when (equal (buffer-file-name b) file) (kill-buffer b)))
      (when (get-buffer efrit-diff-preview-buffer-name)
        (kill-buffer efrit-diff-preview-buffer-name))
      (delete-directory dir t))))

(ert-deftest test-diff-preview-open-file-finds-the-change ()
  "RET / o on a hunk opens the file at the changed line, found by text,
not by the hunk's own line number (which says 2)."
  (test-diff-preview--with-file
   (lambda (dir _file)
     (let ((efrit-diff-preview--root dir)
           (efrit-diff-preview--apply-mode 'all_or_nothing)
           (opened nil))
       (cl-letf (((symbol-function 'pop-to-buffer) (lambda (b &rest _) (set-buffer b)))
                 ((symbol-function 'find-file-other-window)
                  (lambda (f) (setq opened f) (switch-to-buffer (find-file-noselect f))))
                 ((symbol-function 'recenter) #'ignore))
         (efrit-diff-preview--display
          '(((file . "sample.txt") (old_content . "alpha\nbeta\ngamma\n")
             (new_content . "alpha\nBETA\ngamma\n")))
          "rename beta" 'all_or_nothing)
         (with-current-buffer efrit-diff-preview-buffer-name
           ;; the hunk header claims line 1
           (goto-char (point-min))
           (should (re-search-forward "^@@ -1" nil t))
           ;; point on the removed line
           (re-search-forward "^-beta")
           (pcase-let ((`(,file ,old ,new ,offset) (efrit-diff-preview--target-at-point)))
             (should (equal "sample.txt" file))
             (should (equal "alpha\nbeta\ngamma" old))
             (should (equal "alpha\nBETA\ngamma" new))
             (should (= 1 offset)))
           (efrit-diff-preview-toggle-or-open))
         (should (equal (expand-file-name "sample.txt" dir) opened))
         (with-current-buffer (get-file-buffer opened)
           (should (= 12 (line-number-at-pos)))
           (should (looking-at "beta"))))))))

(ert-deftest test-diff-preview-open-file-falls-back-to-new-side ()
  "When the change was applied already the new-side text is found."
  (test-diff-preview--with-file
   (lambda (dir file)
     (with-temp-file file (insert "x\nalpha\nBETA\ngamma\n"))
     (let ((efrit-diff-preview--root dir))
       (cl-letf (((symbol-function 'pop-to-buffer) (lambda (b &rest _) (set-buffer b)))
                 ((symbol-function 'find-file-other-window)
                  (lambda (f) (switch-to-buffer (find-file-noselect f))))
                 ((symbol-function 'recenter) #'ignore))
         (efrit-diff-preview--display
          '(((file . "sample.txt") (old_content . "alpha\nbeta\ngamma\n")
             (new_content . "alpha\nBETA\ngamma\n")))
          nil 'all_or_nothing)
         (with-current-buffer efrit-diff-preview-buffer-name
           (goto-char (point-min))
           (re-search-forward "^@@")
           (efrit-diff-preview-open-file))
         (with-current-buffer (get-file-buffer file)
           (should (= 3 (line-number-at-pos)))
           (should (looking-at "BETA"))))))))

(provide 'test-diff-preview-open)
;;; test-diff-preview-open.el ends here
