;;; test-tool-create-file.el --- create_file -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'efrit-tool-create-file)
(defvar efrit-project-root)
(defvar efrit-sandbox-enabled)

(ert-deftest test-create-file-show-opens-a-window-on-the-file ()
  "create_file show=true displays the new file in another window without
selecting it, and reports shown; without show nothing is displayed."
  (let* ((dir (make-temp-file "efrit-cf-" t))
         (path (expand-file-name "review.md" dir))
         (efrit-sandbox-enabled nil)
         (efrit-project-root (file-name-as-directory dir))
         (default-directory (file-name-as-directory dir))
         (displayed nil))
    (unwind-protect
        (cl-letf (((symbol-function 'display-buffer)
                   (lambda (buf &rest _) (setq displayed buf) (selected-window))))
          (let* ((r (efrit-tool-create-file `((path . ,path) (content . "# hi\n") (show . t))))
                 (res (alist-get 'result r)))
            (should (eq t (alist-get 'success r)))
            (should (eq t (alist-get 'shown res)))
            (should (equal path (buffer-file-name displayed))))
          (setq displayed nil)
          (let* ((r (efrit-tool-create-file `((path . ,(expand-file-name "quiet.md" dir)) (content . "x\n"))))
                 (res (alist-get 'result r)))
            (should (eq t (alist-get 'success r)))
            (should-not (assq 'shown res))
            (should-not displayed)))
      (when-let* ((b (get-file-buffer path))) (kill-buffer b))
      (delete-directory dir t))))

(provide 'test-tool-create-file)

;;; test-tool-create-file.el ends here
