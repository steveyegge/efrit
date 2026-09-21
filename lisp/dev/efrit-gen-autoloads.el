;;; efrit-gen-autoloads.el --- Generate lisp/efrit-autoloads.el -*- lexical-binding: t; -*-

;;; Commentary:
;; package.el only scans the top-level lisp/ directory for autoload
;; cookies, so commands defined in lisp/{core,interfaces,support,tools}
;; (efrit-doctor, efrit-select-model, efrit-menu, ...) would be
;; unreachable until something loaded efrit.el.  This generates one
;; autoloads file covering every subdirectory, with a prelude that puts
;; those subdirectories on load-path.
;;
;; Run via `make autoloads', or:
;;   emacs --batch -l lisp/dev/efrit-gen-autoloads.el
;; from the repository root.  Users who install with :load-path then
;; (load "efrit-autoloads") -- see docs/examples/proxy-config.el.

;;; Code:

(require 'loaddefs-gen nil t)   ; Emacs 29+
;; Emacs 28 fallback; deprecated from 29 where loaddefs-gen is used instead
(with-suppressed-warnings ((obsolete autoload))
  (require 'autoload))
(declare-function loaddefs-generate "loaddefs-gen")
(declare-function make-directory-autoloads "autoload")

(defconst efrit-gen-autoloads--subdirs '("core" "interfaces" "support" "tools" "dev"))

(defun efrit-gen-autoloads (&optional root)
  "Write ROOT/lisp/efrit-autoloads.el.  ROOT defaults to the repo root."
  (let* ((root (or root (locate-dominating-file (or load-file-name default-directory) "lisp/efrit.el")
                   (error "Run from the efrit repository root")))
         (lisp (expand-file-name "lisp" root))
         (dirs (cons lisp (mapcar (lambda (s) (expand-file-name s lisp))
                                  efrit-gen-autoloads--subdirs)))
         (out (expand-file-name "efrit-autoloads.el" lisp))
         (prelude (format ";; Put efrit's subdirectories on load-path before any autoload fires.\n(let ((d (file-name-directory (or load-file-name buffer-file-name))))\n  (dolist (s '%S)\n    (let ((p (expand-file-name s d)))\n      (when (file-directory-p p) (add-to-list 'load-path p)))))\n"
                          efrit-gen-autoloads--subdirs)))
    (setq dirs (cl-remove-if-not #'file-directory-p dirs))
    (if (fboundp 'loaddefs-generate)
        (loaddefs-generate dirs out nil prelude)
      ;; Emacs 28: make-directory-autoloads then prepend the prelude
      (let ((generated-autoload-file out))
        (with-suppressed-warnings ((obsolete make-directory-autoloads))
          (make-directory-autoloads dirs out)))
      (with-temp-buffer
        (insert-file-contents out)
        (goto-char (point-min))
        (forward-line 1)
        (insert prelude)
        (write-region (point-min) (point-max) out)))
    (message "Wrote %s" out)
    out))

(when noninteractive
  (require 'cl-lib)
  (efrit-gen-autoloads))

(provide 'efrit-gen-autoloads)
;;; efrit-gen-autoloads.el ends here
