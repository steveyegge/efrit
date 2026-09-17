;;; test-menu.el --- the efrit-menu definition, checked as data -*- lexical-binding: t; -*-

;;; Commentary:
;; Walks `efrit-menu--definition' rather than transient's internal
;; layout objects (which differ across bundled versions) and checks the
;; two mistakes people actually make: a suffix naming a command that
;; doesn't exist, and two suffixes on the same key.  (copilot-menu's
;; test approach.)

;;; Code:

(require 'ert)
(require 'efrit-menu)
(require 'efrit)

(defun test-menu--suffixes (form)
  "Collect (KEY . COMMAND) from every suffix vector in FORM."
  (let (out)
    (cl-labels ((walk (x)
                  (cond
                   ((vectorp x) (mapc #'walk (append x nil)))
                   ((and (consp x) (stringp (car x)) (not (keywordp (cadr x))))
                    ;; ("k" "desc" cmd ...) or ("k" cmd :description ...)
                    (let* ((rest (cdr x))
                           (cmd (if (stringp (car rest)) (cadr rest) (car rest))))
                      (push (cons (car x) cmd) out)))
                   ((consp x) (mapc #'walk x)))))
      (walk form))
    out))

(ert-deftest test-menu-every-command-exists ()
  (dolist (s (test-menu--suffixes efrit-menu--definition))
    (let ((cmd (cdr s)))
      (should (or (and (symbolp cmd) (fboundp cmd))
                  (and (consp cmd) (eq (car cmd) 'lambda))))
      (when (symbolp cmd)
        (should (commandp cmd))))))

(ert-deftest test-menu-keys-are-unique ()
  (let ((keys (mapcar #'car (test-menu--suffixes efrit-menu--definition))))
    (should (= (length keys) (length (delete-dups (copy-sequence keys)))))))

(ert-deftest test-menu-descriptions-are-safe-without-session ()
  "Description functions must not signal when nothing is loaded/active."
  (should (stringp (efrit-menu--desc-model)))
  (should (stringp (efrit-menu--desc-endpoint)))
  (should (stringp (efrit-menu--desc-permissions)))
  (should (stringp (efrit-menu--desc-header)))
  (should (stringp (efrit-menu--desc-usage)))
  (should (stringp (efrit-menu--desc-toggle "X" 'no-such-var))))

(ert-deftest test-menu-is-a-transient-prefix ()
  (skip-unless (featurep 'transient))
  (should (commandp 'efrit-menu))
  (should (get 'efrit-menu 'transient--prefix)))

(provide 'test-menu)
;;; test-menu.el ends here
