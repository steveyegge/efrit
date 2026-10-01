;;; test-inline-diff.el --- efrit-inline-diff -*- lexical-binding: t; -*-
;;; Code:
(require 'ert)
(require 'efrit-inline-diff)

(ert-deftest test-inline-diff-hunks-from-the-diff-library ()
  "Replace, pure insert, pure delete, each as one hunk with old-line numbers."
  (skip-unless (executable-find diff-command))
  (let ((hunks (efrit-inline-diff-hunks "a\nb\nc\nd\ne\n" "a\nB\nc\nd\nX\ne\n")))
    (should (= 2 (length hunks)))
    (should (equal '(:old-start 2 :old-count 1 :removed ("b") :added ("B")) (nth 0 hunks)))
    (should (equal '(:old-start 5 :old-count 0 :removed nil :added ("X")) (nth 1 hunks))))
  (let ((hunks (efrit-inline-diff-hunks "a\nb\nc\n" "a\nc\n")))
    (should (equal '(:old-start 2 :old-count 1 :removed ("b") :added nil) (car hunks))))
  (should-not (efrit-inline-diff-hunks "same\n" "same\n")))

(ert-deftest test-inline-diff-overlays-do-not-touch-the-text ()
  "Removed lines get the removed face, added lines ride the same
overlay's after-string, an insertion is a before-string, and clear
leaves nothing."
  (skip-unless (executable-find diff-command))
  (with-temp-buffer
    (insert "head\none\ntwo\nthree\ntail\n")
    (let* ((start (progn (goto-char (point-min)) (forward-line 1) (point)))
           (end (progn (forward-line 3) (point)))
           (before (buffer-string))
           (n (efrit-inline-diff-show (current-buffer) start end "one\nTWO\nthree\nfour\n")))
      (should (= 2 n))
      (should (equal before (buffer-string)))
      (should (efrit-inline-diff-active-p))
      (let* ((ovs (overlays-in (point-min) (point-max)))
             (replaced (cl-find-if (lambda (o) (overlay-get o 'after-string)) ovs))
             (inserted (cl-find-if (lambda (o) (overlay-get o 'before-string)) ovs)))
        (should (memq 'efrit-inline-diff-removed (ensure-list (overlay-get replaced 'face))))
        (should (member '(:strike-through t) (ensure-list (overlay-get replaced 'face))))
        (should (equal "two\n" (buffer-substring (overlay-start replaced) (overlay-end replaced))))
        (should (equal "TWO\n" (substring-no-properties (overlay-get replaced 'after-string))))
        (should (= (overlay-start inserted) (overlay-end inserted)))
        (should (equal "four\n" (substring-no-properties (overlay-get inserted 'before-string))))
        ;; the insertion sits where old line 5 ("tail") starts
        (should (= (overlay-start inserted) (save-excursion (goto-char (point-min)) (forward-line 4) (point)))))
      (efrit-inline-diff-clear)
      (should-not (efrit-inline-diff-active-p))
      (should-not (cl-some (lambda (o) (overlay-get o 'efrit-inline-diff))
                           (overlays-in (point-min) (point-max)))))))

(provide 'test-inline-diff)
;;; test-inline-diff.el ends here
