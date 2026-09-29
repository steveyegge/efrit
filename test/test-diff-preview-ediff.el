;;; test-diff-preview-ediff.el --- E in the diff preview -*- lexical-binding: t; -*-
;;; Code:
(require 'ert)
(require 'efrit-tool-show-diff-preview)

(ert-deftest test-diff-preview-ediff-accept-replaces-the-change ()
  "Accepting after editing B makes B's text the change's new_content,
marks it, and the approval result carries it in user_edits."
  (let ((efrit-diff-preview--changes (list (list (cons 'file "a.el")
                                                 (cons 'old_content "one\n")
                                                 (cons 'new_content "two\n"))))
        (efrit-diff-preview--apply-mode 'all_or_nothing)
        (efrit-diff-preview--edited nil)
        (efrit-diff-preview--description "d")
        (efrit-diff-preview--root temporary-file-directory)
        (a (generate-new-buffer "A")) (b (generate-new-buffer "B")))
    (unwind-protect
        (progn
          (with-current-buffer (get-buffer-create efrit-diff-preview-buffer-name)
            (efrit-diff-preview--redraw))
          (with-current-buffer b (insert "two edited\n"))
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                    ((symbol-function 'pop-to-buffer) (lambda (buf &rest _) (set-buffer buf))))
            (efrit-diff-preview--ediff-finish 0 "two edited\n" a b nil))
          (should (equal "two edited\n" (alist-get 'new_content (car efrit-diff-preview--changes))))
          (should (equal '(0) efrit-diff-preview--edited))
          (should-not (buffer-live-p a))
          (with-current-buffer efrit-diff-preview-buffer-name
            (should (string-match-p "edited by you in ediff" (buffer-string)))
            (should (string-match-p "\\+two edited" (buffer-string))))
          (efrit-diff-preview-approve)
          (let ((edits (alist-get 'user_edits efrit-diff-preview--result)))
            (should (vectorp edits))
            (should (equal "two edited\n" (alist-get 'new_content (aref edits 0))))
            (should (= 0 (alist-get 'index (aref edits 0))))))
      (when (buffer-live-p a) (kill-buffer a))
      (when (buffer-live-p b) (kill-buffer b))
      (when (get-buffer efrit-diff-preview-buffer-name) (kill-buffer efrit-diff-preview-buffer-name)))))

(ert-deftest test-diff-preview-ediff-decline-keeps-the-proposal ()
  (let ((efrit-diff-preview--changes (list (list (cons 'file "a.el")
                                                 (cons 'old_content "one\n")
                                                 (cons 'new_content "two\n"))))
        (efrit-diff-preview--apply-mode 'all_or_nothing)
        (efrit-diff-preview--edited nil)
        (a (generate-new-buffer "A")) (b (generate-new-buffer "B")))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil))
              ((symbol-function 'pop-to-buffer) #'ignore))
      (efrit-diff-preview--ediff-finish 0 "changed\n" a b nil))
    (should (equal "two\n" (alist-get 'new_content (car efrit-diff-preview--changes))))
    (should-not efrit-diff-preview--edited)
    (efrit-diff-preview-approve)
    (should (eq :json-false (alist-get 'user_edits efrit-diff-preview--result)))))

(provide 'test-diff-preview-ediff)
;;; test-diff-preview-ediff.el ends here
