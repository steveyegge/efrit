;;; test-choice.el --- efrit-choice: one chooser for every question -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'efrit-choice)
(require 'efrit-tool-confirm-action)

(ert-deftest test-choice-keys-are-assigned-and-unique ()
  "Items without keys get 1-9 then a-z, skipping efrit's own keys and
any the caller gave; given keys are kept."
  (let ((items (efrit-choice--assign-keys
                (append (list '("n" "no" no) '("yes" yes) '("maybe" maybe t))
                        (cl-loop for i from 1 to 12 collect (list (format "item %d" i) i))))))
    (should (equal "n" (nth 0 (nth 0 items))))
    (should (equal "1" (nth 0 (nth 1 items))))
    (should (eq t (nth 3 (nth 2 items))))
    (let ((keys (mapcar #'car items)))
      (should (= (length keys) (length (delete-dups (copy-sequence keys)))))
      (should-not (cl-intersection keys efrit-choice--reserved-keys :test #'equal)))))

(ert-deftest test-choice-read-falls-back-without-a-display ()
  "In batch the chooser uses read-multiple-choice / completing-read-multiple
and returns the value(s); multi preselects; C-g is `cancelled'."
  (cl-letf (((symbol-function 'read-multiple-choice)
             (lambda (_prompt choices &rest _) (assq ?y choices))))
    (should (eq 'yes (efrit-choice-read "Install?" '(("y" "yes" yes) ("n" "no" no))))))
  (cl-letf (((symbol-function 'completing-read-multiple)
             (lambda (_prompt _table &rest args)
               ;; the preselected set is offered as the initial input
               (should (equal "beta" (nth 2 args)))
               (list "alpha" "beta"))))
    (should (equal '(a b) (efrit-choice-read "Which?" '(("alpha" a) ("beta" b t) ("gamma" c)) :multi t))))
  (cl-letf (((symbol-function 'read-multiple-choice) (lambda (&rest _) (signal 'quit nil))))
    (should (eq 'cancelled (efrit-choice-read "Install?" '(("y" "yes" yes) ("n" "no" no)))))))

(ert-deftest test-confirm-action-goes-through-the-chooser ()
  "confirm_action asks with efrit-choice-read: yes confirms, no declines
with a note, cancel is user_cancelled; danger offers Y not y."
  (let ((seen nil))
    (cl-letf (((symbol-function 'efrit-choice-read)
               (cl-function (lambda (heading items &key detail &allow-other-keys)
                              (push (list heading (mapcar #'car items) detail) seen)
                              (nth 2 (car items))))))
      (let ((r (efrit-tool-confirm-action '((action . "Install it") (severity . "info")))))
        (should (eq t (alist-get 'confirmed (alist-get 'result r))))
        (should (equal '("y" "n") (nth 1 (car seen)))))
      (let ((r (efrit-tool-confirm-action '((action . "Wipe it") (severity . "danger") (details . ["a" "b"])))))
        (should (eq t (alist-get 'confirmed (alist-get 'result r))))
        (should (equal '("Y" "n") (nth 1 (car seen))))
        (should (string-match-p "DANGER" (nth 2 (car seen))))
        (should (string-match-p "- a" (nth 2 (car seen))))))
    (cl-letf (((symbol-function 'efrit-choice-read) (lambda (&rest _) "No")))
      (let ((res (alist-get 'result (efrit-tool-confirm-action '((action . "x") (severity . "warning"))))))
        (should (eq :json-false (alist-get 'confirmed res)))
        (should (string-match-p "declined" (alist-get 'note res)))))
    (cl-letf (((symbol-function 'efrit-choice-read) (lambda (&rest _) 'cancelled)))
      (let ((res (alist-get 'result (efrit-tool-confirm-action '((action . "x"))))))
        (should (equal "user_cancelled" (alist-get 'reason res)))))))

(provide 'test-choice)

;;; test-choice.el ends here
