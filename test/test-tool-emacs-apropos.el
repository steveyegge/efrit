;;; test-tool-emacs-apropos.el --- Tests for emacs_apropos -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'efrit-tool-emacs-apropos)
(require 'recentf)
(require 'dired)
(require 'vc)

(defun test-apropos--symbols (query &rest args)
  (let* ((r (efrit-tool-emacs-apropos (append `((query . ,query)) args)))
         (res (alist-get 'result r)))
    (should (eq (alist-get 'success r) t))
    (mapcar (lambda (e) (alist-get 'symbol e))
            (append (alist-get 'results res) nil))))

(ert-deftest test-apropos-plain-words-find-commands ()
  "Words, not names: \"recent files\" finds recentf, \"revert buffer\" finds revert-buffer."
  (let ((syms (test-apropos--symbols "recent files")))
    (should (member "recentf-open-files" syms))
    (should (string-prefix-p "recentf" (car syms))))
  (should (member "revert-buffer" (test-apropos--symbols "revert buffer")))
  (should (member "vc-diff" (test-apropos--symbols "vc diff")))
  (should (member "dired-mark" (test-apropos--symbols "dired mark"))))

(ert-deftest test-apropos-kind-filters-and-values ()
  (let* ((r (efrit-tool-emacs-apropos '((query . "recentf list") (kind . "variable"))))
         (entries (append (alist-get 'results (alist-get 'result r)) nil))
         (rl (seq-find (lambda (e) (equal (alist-get 'symbol e) "recentf-list")) entries)))
    (should rl)
    (should (equal (alist-get 'kind rl) "variable"))
    ;; the live value is reported
    (should (stringp (alist-get 'value rl)))
    ;; and commands are not among variable results
    (should-not (seq-find (lambda (e) (equal (alist-get 'kind e) "command")) entries)))
  ;; kind=command excludes plain functions
  (should-not (member "file-name-sans-extension" (test-apropos--symbols "file name extension")))
  (should (member "file-name-sans-extension" (test-apropos--symbols "file name extension" '(kind . "function")))))

(ert-deftest test-apropos-entry-shape ()
  (let* ((r (efrit-tool-emacs-apropos '((query . "^dired-jump$") (kind . "all"))))
         (e (aref (alist-get 'results (alist-get 'result r)) 0)))
    (should (equal (alist-get 'symbol e) "dired-jump"))
    (should (equal (alist-get 'kind e) "command"))
    (should (string-match-p "Dired" (alist-get 'doc e)))
    (should (alist-get 'from e))
    (should (stringp (alist-get 'signature e)))))

(ert-deftest test-apropos-internal-hidden-by-default ()
  (should-not (seq-find (lambda (s) (string-match-p "--" s))
                        (test-apropos--symbols "recentf" '(kind . "all") '(max . 500))))
  (should (seq-find (lambda (s) (string-match-p "--" s))
                    (test-apropos--symbols "recentf" '(kind . "all") '(max . 500) '(include_internal . t)))))

(ert-deftest test-apropos-max-and-warnings ()
  (let* ((r (efrit-tool-emacs-apropos '((query . "dired") (max . 3))))
         (res (alist-get 'result r)))
    (should (= 3 (length (alist-get 'results res))))
    (should (> (alist-get 'total res) 3))
    (should (seq-find (lambda (w) (string-match-p "showing the 3 best" w))
                      (append (alist-get 'warnings r) nil))))
  (let ((r (efrit-tool-emacs-apropos '((query . "zzqxjv nothing here")))))
    (should (= 0 (alist-get 'total (alist-get 'result r))))
    (should (seq-find (lambda (w) (string-match-p "No match" w))
                      (append (alist-get 'warnings r) nil)))))

(ert-deftest test-apropos-validates-input ()
  (let ((r (efrit-tool-emacs-apropos '((query . "")))))
    (should (eq (alist-get 'success r) :json-false)))
  (let ((r (efrit-tool-emacs-apropos '((query . "x") (kind . "bogus")))))
    (should (eq (alist-get 'success r) :json-false))))

(provide 'test-tool-emacs-apropos)
;;; test-tool-emacs-apropos.el ends here
