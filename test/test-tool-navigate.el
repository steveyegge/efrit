;;; test-tool-navigate.el --- xref/imenu/treesit/show_location tools -*- lexical-binding: t; -*-
;;; Code:
(require 'ert)
(require 'efrit-test-sandbox-helpers)
(require 'efrit-tool-navigate)

(defmacro test-nav--with-project (&rest body)
  "BODY in a temp project with two elisp files; ROOT and FILE bound."
  (declare (indent 0))
  `(let* ((root (file-name-as-directory (make-temp-file "efrit-nav-" t)))
          (file (expand-file-name "lib.el" root))
          (other (expand-file-name "use.el" root))
          (efrit-project-root root))
     (make-directory (expand-file-name ".git" root))   ; a project for project.el
     (with-temp-file file
       (insert ";;; lib.el --- x -*- lexical-binding: t; -*-\n"
               "(defun nav-helper (x)\n  \"Doc.\"\n  (* x 2))\n\n"
               "(defvar nav-count 0)\n\n"
               "(defun nav-caller ()\n  (nav-helper nav-count))\n"
               "(provide 'lib)\n"))
     (with-temp-file other
       (insert ";;; use.el -*- lexical-binding: t; -*-\n(require 'lib)\n(nav-helper 3)\n"))
     (unwind-protect (progn ,@body)
       (dolist (f (list file other))
         (when-let* ((b (find-buffer-visiting f))) (kill-buffer b)))
       (delete-directory root t))))

(defun test-nav--result (response)
  (should (eq t (alist-get 'success response)))
  (alist-get 'result response))

(ert-deftest test-nav-imenu-symbols-lists-definitions-with-lines ()
  (test-nav--with-project
    (let* ((r (test-nav--result (efrit-tool-imenu-symbols `((file . "lib.el")))))
           (names (mapcar (lambda (s) (alist-get 'name s)) (append (alist-get 'symbols r) nil))))
      (should (member "nav-helper" names))
      (should (member "nav-caller" names))
      (should (member "nav-count" names))
      (should (equal "emacs-lisp-mode" (alist-get 'mode r)))
      (let ((helper (cl-find "nav-helper" (append (alist-get 'symbols r) nil)
                             :key (lambda (s) (alist-get 'name s)) :test #'equal)))
        (should (= 2 (alist-get 'line helper)))))))

(ert-deftest test-nav-xref-references-through-the-elisp-backend ()
  "The elisp backend finds uses in loaded code: load the file first."
  (test-nav--with-project
    (load file nil t)
    (let* ((r (test-nav--result (efrit-tool-xref-references `((symbol . "nav-helper") (file . "lib.el")))))
           (refs (append (alist-get 'references r) nil)))
      (should (equal "elisp" (alist-get 'backend r)))
      (should (>= (alist-get 'count r) 1))
      (should (cl-some (lambda (x) (string-match-p "nav-caller\\|lib.el" (alist-get 'text x))) refs)))
    ;; a missing symbol is a clean error result, not a crash
    (should (eq :json-false (alist-get 'success (efrit-tool-xref-references '((file . "lib.el"))))))))

(ert-deftest test-nav-xref-apropos-finds-definitions ()
  (test-nav--with-project
    (load file nil t)
    (let* ((r (test-nav--result (efrit-tool-xref-apropos `((pattern . "nav-helper") (file . "lib.el")))))
           (defs (append (alist-get 'definitions r) nil)))
      (should (cl-some (lambda (d) (string-match-p "nav-helper" (alist-get 'summary d))) defs)))))

(ert-deftest test-nav-show-location-by-text-anchor-and-by-line ()
  (test-nav--with-project
    (let ((r (test-nav--result (efrit-tool-show-location `((file . "lib.el") (start_text . "(defun nav-caller")
                                                             (end_text . "nav-count))"))))))
      (should (equal "text" (alist-get 'found_by r)))
      (should (= 8 (alist-get 'line r))))
    (let ((r (test-nav--result (efrit-tool-show-location `((file . "lib.el") (line . 6))))))
      (should (equal "line" (alist-get 'found_by r)))
      (should (= 6 (alist-get 'line r))))
    (should (eq :json-false (alist-get 'success (efrit-tool-show-location '((file . "nope.el"))))))))

(ert-deftest test-nav-treesit-info-without-a-parser-is-a-clean-error ()
  (test-nav--with-project
    (let ((r (efrit-tool-treesit-info `((file . "lib.el")))))
      (should (eq :json-false (alist-get 'success r)))
      (should (string-match-p "tree-sitter\\|parser" (alist-get 'message (alist-get 'error r)))))))

(ert-deftest test-nav-diagnostics-project-scope-names-files ()
  "path=\"project\" scans every file buffer under the root; entries carry :file."
  (require 'efrit-tool-get-diagnostics)
  (test-nav--with-project
    (find-file-noselect file)
    (cl-letf (((symbol-function 'efrit-tool-get-diagnostics--from-flymake)
               (lambda (buf) (list `((source . "flymake") (severity . "error") (message . ,(buffer-name buf))
                                     (line . 1) (column . 0))))))
      (let* ((r (alist-get 'result (efrit-tool-get-diagnostics '((path . "project") (sources . ("flymake"))))))
             (diags (append (alist-get 'diagnostics r) nil)))
        (should (>= (length diags) 1))
        (should (cl-every (lambda (d) (alist-get 'file d)) diags))
        (should (cl-some (lambda (d) (string-suffix-p "lib.el" (alist-get 'file d))) diags))))))

(provide 'test-tool-navigate)
;;; test-tool-navigate.el ends here
