;;; test-default-project-root.el --- the project when no repository is around -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'efrit-tool-utils)

(ert-deftest test-default-project-root-outside-any-repository ()
  "From a directory in no repository the project is the configured
default (a named directory, user-emacs-directory) rather than the
directory itself; `default-directory' keeps the old behaviour; a
directory that does not exist is skipped."
  (let* ((outside (file-name-as-directory (make-temp-file "efrit-norepo-" t)))
         (conf (file-name-as-directory (make-temp-file "efrit-conf-" t)))
         (default-directory outside)
         (efrit-project-root nil))
    (unwind-protect
        (cl-letf (((symbol-function 'project-current) (lambda (&rest _) nil))
                  ((symbol-function 'efrit-sandbox--git-toplevel) (lambda (&rest _) nil)))
          (let ((efrit-default-project-root conf))
            (should (equal conf (efrit-tool--get-project-root))))
          (let ((efrit-default-project-root 'user-emacs-directory)
                (user-emacs-directory conf))
            (should (equal conf (efrit-tool--get-project-root))))
          (let ((efrit-default-project-root 'default-directory))
            (should (equal outside (efrit-tool--get-project-root))))
          (let ((efrit-default-project-root (expand-file-name "nope" conf)))
            (should (equal outside (efrit-tool--get-project-root)))))
      (delete-directory outside t)
      (delete-directory conf t))))

(provide 'test-default-project-root)

;;; test-default-project-root.el ends here
