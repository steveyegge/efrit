;;; test-common-helpers.el --- truncate-output, warn-once, templates, syntax check -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'efrit-common)

(ert-deftest test-helpers-truncate-output ()
  (should (equal (efrit-truncate-output "short" 10) "short"))
  (let ((s (efrit-truncate-output "0123456789ABCDEF" 6)))
    (should (string-suffix-p "ABCDEF" s))
    (should (string-match-p "first 10 chars omitted" s)))
  (let ((s (efrit-truncate-output "0123456789ABCDEF" 6 'head)))
    (should (string-prefix-p "012345" s))
    (should (string-match-p "10 more chars" s)))
  (let ((s (efrit-truncate-output "0123456789ABCDEF" 6 'both)))
    (should (string-prefix-p "012" s))
    (should (string-suffix-p "DEF" s))))

(ert-deftest test-helpers-warn-once ()
  (let ((efrit--warned (make-hash-table :test 'equal)) (shown 0))
    (cl-letf (((symbol-function 'display-warning) (lambda (&rest _) (cl-incf shown))))
      (should (efrit-warn-once 'k "same %s" 1))
      (should-not (efrit-warn-once 'k "same %s" 1))
      (should (efrit-warn-once 'k "different %s" 2))  ; message changed
      (should-not (efrit-warn-once 'k "different %s" 2))
      (should (= shown 2)))))

(ert-deftest test-helpers-expand-template ()
  (cl-flet ((lk (alist) (lambda (k) (alist-get k alist))))
    (should (equal (efrit-expand-template "a {{{:x}}} b {{{:y}}}" (lk '((:x . "X") (:y . "Y"))))
                   "a X b Y"))
    ;; nil slot -> empty; missing braces left alone; no rescan of inserted text
    (should (equal (efrit-expand-template "[{{{:gone}}}]" (lk nil)) "[]"))
    (should (equal (efrit-expand-template "{{{:open" (lk nil)) "{{{:open"))
    (should (equal (efrit-expand-template "{{{:x}}}" (lk '((:x . "{{{:y}}}")))) "{{{:y}}}"))
    ;; backslashes and regexp specials pass through verbatim
    (should (equal (efrit-expand-template "p={{{:p}}}" (lk '((:p . "C:\\dir\\1 \\& $"))))
                   "p=C:\\dir\\1 \\& $"))
    ;; functions and bound symbols as slot values
    (defvar test-helpers--slot "from-var")
    (should (equal (efrit-expand-template "{{{:f}}}/{{{:v}}}"
                                          (lk `((:f . ,(lambda () "from-fn")) (:v . test-helpers--slot))))
                   "from-fn/from-var"))
    ;; non-string value is an error, not silently formatted
    (should-error (efrit-expand-template "{{{:n}}}" (lk '((:n . 42)))) :type 'wrong-type-argument)))

(ert-deftest test-helpers-lisp-syntax-problem ()
  (should-not (efrit-lisp-syntax-problem "(defun f () (list 1 2))\n;; a ) in a comment\n\"a ( in a string\"\n"))
  (let ((p (efrit-lisp-syntax-problem "(defun f ()\n  (list 1 2)\n")))
    (should p)
    (should (string-match-p "line [0-9]+" p)))
  (should (efrit-lisp-syntax-problem "(foo \"unterminated)")))

(provide 'test-common-helpers)
;;; test-common-helpers.el ends here
