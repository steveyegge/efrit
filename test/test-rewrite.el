;;; test-rewrite.el --- efrit-rewrite-region -*- lexical-binding: t; -*-
;;; Code:
(require 'ert)
(require 'efrit-rewrite)

(defmacro test-rw--with-model (reply &rest body)
  "Run BODY with the model answering REPLY (a string or a function of the prompt)."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'efrit-ask-once)
              (lambda (prompt cb &rest _)
                (funcall cb (if (functionp ,reply) (funcall ,reply prompt) ,reply) nil)
                nil))
             ((symbol-function 'efrit-show-preview) (lambda (&rest _) nil))
             ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
     ,@body))

(defun test-rw--wrap (text)
  (format "%s\n%s\n%s" efrit-rewrite--start-marker text efrit-rewrite--end-marker))

(ert-deftest test-rewrite-parse-and-trim ()
  (should (equal "new" (efrit-rewrite-parse (test-rw--wrap "new"))))
  (should-not (efrit-rewrite-parse "no markers"))
  (should-not (efrit-rewrite-parse (concat (test-rw--wrap "a") (test-rw--wrap "b"))))
  ;; echoed context is trimmed only when it spans a line
  (should (equal "mid\n" (efrit-rewrite-trim-echo "before\nmid\nafter\n" "x\nbefore\n" "after\nz")))
  (should (equal "abc" (efrit-rewrite-trim-echo "abc" "ab" "bc"))))

(ert-deftest test-rewrite-replaces-region-keeping-markers ()
  (with-temp-buffer
    (insert "head\nold text here\ntail\n")
    (let ((after (copy-marker (- (point-max) 2))))
      (test-rw--with-model (lambda (prompt)
                             (should (string-match-p "old text here" prompt))
                             (should (string-match-p "make it new" prompt))
                             (test-rw--wrap "new text here"))
        (efrit-rewrite-region 6 19 "make it new"))
      (should (equal "head\nnew text here\ntail\n" (buffer-string)))
      ;; a marker after the region is still after it
      (should (= (marker-position after) (- (point-max) 2))))))

(ert-deftest test-rewrite-refuses-when-the-region-moved ()
  (with-temp-buffer
    (insert "head\nold\ntail\n")
    (let ((kill-ring nil) (msg nil))
      (cl-letf (((symbol-function 'efrit-ask-once)
                 (lambda (_prompt cb &rest _)
                   ;; the user types before the answer arrives
                   (goto-char (point-min)) (insert "X")
                   (funcall cb (test-rw--wrap "new") nil)
                   nil))
                ((symbol-function 'message) (lambda (f &rest a) (setq msg (apply #'format f a)))))
        (efrit-rewrite-region 6 9 "change"))
      (should (equal "Xhead\nold\ntail\n" (buffer-string)))
      (should (string-match-p "changed while the model worked" msg))
      (should (equal "new" (car kill-ring))))))

(ert-deftest test-rewrite-bad-reply-goes-to-kill-ring ()
  (with-temp-buffer
    (insert "old")
    (let ((kill-ring nil))
      (test-rw--with-model "Here you go: new"
        (efrit-rewrite-region 1 4 "x"))
      (should (equal "old" (buffer-string)))
      (should (equal "Here you go: new" (car kill-ring))))))

(ert-deftest test-rewrite-read-only-goes-to-kill-ring ()
  (with-temp-buffer
    (insert "old")
    (setq buffer-read-only t)
    (let ((kill-ring nil))
      (test-rw--with-model (test-rw--wrap "new")
        (efrit-rewrite-region 1 4 "x"))
      (should (equal "old" (buffer-string)))
      (should (equal "new" (car kill-ring))))))

(ert-deftest test-rewrite-inline-preview-is-shown-then-cleared ()
  "With `efrit-rewrite-preview' inline, the overlays are up while the
question is asked and gone after, whichever the answer."
  (skip-unless (executable-find diff-command))
  (with-temp-buffer
    (insert "head\nold text here\ntail\n")
    (let ((start (progn (goto-char (point-min)) (forward-line 1) (point)))
          (end (progn (forward-line 1) (point)))
          (seen nil)
          (efrit-rewrite-preview 'inline))
      (cl-letf (((symbol-function 'efrit-ask-once)
                 (lambda (_prompt cb &rest _)
                   (funcall cb (test-rw--wrap "new text here") nil) nil))
                ((symbol-function 'y-or-n-p)
                 (lambda (&rest _) (setq seen (efrit-inline-diff-active-p)) nil)))
        (efrit-rewrite-region start end "change it"))
      (should seen)
      (should-not (efrit-inline-diff-active-p))
      (should (equal "head\nold text here\ntail\n" (buffer-string))))))

(provide 'test-rewrite)
;;; test-rewrite.el ends here
