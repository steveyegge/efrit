;;; test-elisp-fix.el --- balancing the model's Lisp -*- lexical-binding: t; -*-
;;; Code:
(require 'ert)
(require 'efrit-elisp-fix)
(require 'efrit-tools)
(require 'efrit-buffer-watch)
(require 'efrit-tool-edit-buffer)

(ert-deftest test-elisp-fix-balances ()
  (should (equal '("(list 1 2)" . "1 closing paren added") (efrit-elisp-fix "(list 1 2")))
  (should (equal '("(a [b {c])" . "2 closing parens added") (efrit-elisp-fix "(a [b {c")))
  (should (equal '("(list 1 2)" . "1 stray closing paren removed") (efrit-elisp-fix "(list 1 2))")))
  (should (equal "(concat \"a\")" (car (efrit-elisp-fix "(concat \"a"))))
  ;; parens inside strings and char literals are not counted
  (should (equal "(format \"%s)\" x)" (car (efrit-elisp-fix "(format \"%s)\" x"))))
  (should (equal "(list ?\\) 1)" (car (efrit-elisp-fix "(list ?\\) 1"))))
  ;; balanced or hopeless: nil
  (should-not (efrit-elisp-fix "(ok)"))
  (should-not (efrit-elisp-fix ")"))
  (should-not (efrit-elisp-fix "")))

(ert-deftest test-elisp-fix-applies-in-eval-sexp-and-says-so ()
  (let ((efrit-tools-sexp-evaluation-enabled t)
        (efrit-sandbox-enabled nil))
    (cl-letf (((symbol-function 'efrit-tools--check-rate-limit) #'ignore)
              ((symbol-function 'efrit-tools--increment-rate-limit) #'ignore))
      (let ((out (efrit-tools-eval-sexp "(+ 1 2")))
        (should (string-prefix-p "3" out))
        (should (string-match-p "unbalanced: 1 closing paren added" out)))
      ;; balanced input: no note
      (should (equal "3" (efrit-tools-eval-sexp "(+ 1 2)"))))))

(ert-deftest test-buffer-watch-reports-a-change-since-the-read ()
  (with-temp-buffer
    (insert "line one\nline two\n")
    (should-not (efrit-buffer-watch-changes (current-buffer)))   ; never read
    (efrit-buffer-watch-note-read (current-buffer))
    (should-not (efrit-buffer-watch-changes (current-buffer)))   ; unchanged
    (goto-char (point-min)) (forward-line 1) (insert "INSERTED ")
    (let ((change (efrit-buffer-watch-changes (current-buffer))))
      (should change)
      (when (efrit-buffer-watch-available-p)
        (should (= 2 (plist-get change :line)))
        (should (<= (plist-get change :begin) 10 (plist-get change :end))))
      (should (string-match-p "changed since you read it" (efrit-buffer-watch-describe change (current-buffer)))))
    ;; a new read resets
    (efrit-buffer-watch-note-read (current-buffer))
    (should-not (efrit-buffer-watch-changes (current-buffer)))))

(ert-deftest test-edit-buffer-refuses-stale-positions ()
  (with-temp-buffer
    (rename-buffer "efrit-stale-test" t)
    (insert "abc\ndef\n")
    (let ((efrit-sandbox-enabled nil))
      (should (stringp (efrit-tool-read-buffer `((buffer . ,(current-buffer))))))
      (goto-char (point-min)) (insert "X")
      (let ((r (efrit-tool-edit-buffer `((buffer . ,(current-buffer)) (text . "Y") (position . 2)))))
        (should (string-match-p "changed since you read it" r)))
      ;; appending at the end does not depend on positions: allowed
      (should (string-match-p "Inserted" (efrit-tool-edit-buffer `((buffer . ,(current-buffer)) (text . "Y") (position . end)))))
      ;; after a fresh read the positional edit goes through
      (efrit-tool-read-buffer `((buffer . ,(current-buffer))))
      (should (string-match-p "Inserted" (efrit-tool-edit-buffer `((buffer . ,(current-buffer)) (text . "Z") (position . 2))))))))

(provide 'test-elisp-fix)
;;; test-elisp-fix.el ends here
