;;; test-load-isolation.el --- Each entry library loads on its own -*- lexical-binding: t; -*-

;;; Commentary:
;; The ERT run loads every test file into one Emacs, so a library that
;; forgets a `require' still works there because a sibling loaded the
;; dependency.  In a fresh Emacs it fails: on 2026-09-17 eval_sexp
;; signalled void-function efrit-sandbox-eval-form because the require
;; in efrit-tools.el had ended up inside a condition-case as a handler
;; clause.  Each check here starts a separate Emacs, loads ONE library,
;; and asserts the functions it calls at run time are defined.

;;; Code:

(require 'ert)

(defconst test-load-isolation--root
  (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name)))
  "The checkout root, resolved while this file loads.")

(defun test-load-isolation--run (library &rest symbols)
  "In a fresh `emacs --batch', require LIBRARY; return the unbound SYMBOLS."
  (let* ((dirs (mapcar (lambda (d) (expand-file-name d test-load-isolation--root))
                       '("lisp" "lisp/core" "lisp/interfaces" "lisp/support" "lisp/tools" "lisp/dev")))
         (args (append (list "--batch" "-Q")
                       (mapcan (lambda (d) (list "-L" d)) dirs)
                       (list "-l" (symbol-name library)
                             "--eval"
                             (format "(princ (prin1-to-string (seq-remove (lambda (s) (or (fboundp s) (boundp s))) '%S)))" symbols))))
         (emacs (concat invocation-directory invocation-name)))
    (with-temp-buffer
      (let ((status (apply #'call-process emacs nil t nil args)))
        (goto-char (point-max))
        (if (/= status 0)
            (list (format "emacs exited %s: %s" status
                          (string-trim (buffer-substring (max (point-min) (- (point-max) 400)) (point-max)))))
          (forward-line 0)
          (car (read-from-string (buffer-substring (point) (point-max)))))))))

(ert-deftest test-load-isolation-efrit-tools ()
  "eval_sexp's runtime dependencies are present after (require 'efrit-tools) alone."
  (should (equal nil (test-load-isolation--run
                      'efrit-tools
                      'efrit-sandbox-eval-form 'efrit-sandbox-check 'efrit-log))))

(ert-deftest test-load-isolation-efrit-do-dispatch ()
  (should (equal nil (test-load-isolation--run
                      'efrit-do-dispatch
                      'efrit-permission-check 'efrit-sandbox-denied-tool-result
                      'efrit-do--extract-error-info))))

(ert-deftest test-load-isolation-efrit-agent ()
  (should (equal nil (test-load-isolation--run
                      'efrit-agent
                      'efrit-sandbox-ui-prompt 'efrit-agent-svg-header 'efrit-agent--spinner-frame))))

(ert-deftest test-load-isolation-efrit-loop ()
  (should (equal nil (test-load-isolation--run
                      'efrit-loop
                      'efrit-sandbox-denied-prefix 'efrit-permission-denied-result))))

(provide 'test-load-isolation)
;;; test-load-isolation.el ends here
