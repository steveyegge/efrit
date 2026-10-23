;;; test-text-window.el --- efrit-text-window -*- lexical-binding: t; -*-
;;; Code:
(require 'ert)
(require 'efrit-text-window)

(defun test-tw--buffer (lines)
  "A buffer of LINES numbered lines \"line N\\n\"."
  (let ((buf (generate-new-buffer " *tw*")))
    (with-current-buffer buf
      (dotimes (i lines) (insert (format "line %02d\n" (1+ i)))))
    buf))

(ert-deftest test-text-window-everything-fits ()
  (let ((buf (test-tw--buffer 4)))
    (unwind-protect
        (with-current-buffer buf
          (goto-char (point-min)) (forward-line 2)
          (let ((w (efrit-text-window :chars 1000)))
            (should (equal "line 01\nline 02\n" (plist-get w :before)))
            (should (equal "line 03\nline 04\n" (plist-get w :after)))
            (should-not (plist-get w :before-cut))
            (should-not (plist-get w :after-cut))))
      (kill-buffer buf))))

(ert-deftest test-text-window-cuts-whole-lines-and-shares-slack ()
  "A cut side loses its partial line.  Point near the start gives the
after side the slack; near the end the before side gets it."
  (let ((buf (test-tw--buffer 100)))          ; 800 chars, 8 per line
    (unwind-protect
        (with-current-buffer buf
          ;; middle: 40 before, 40 after with ratio .5 -> 5 whole lines each
          (goto-char (point-min)) (forward-line 50)
          (let ((w (efrit-text-window :chars 84 :ratio 0.5)))
            (should (plist-get w :before-cut))
            (should (plist-get w :after-cut))
            (should (string-prefix-p "line " (plist-get w :before)))
            (should (string-suffix-p "\n" (plist-get w :after)))
            (should (= 0 (% (length (plist-get w :before)) 8)))
            (should (= 0 (% (length (plist-get w :after)) 8)))
            ;; the boundary positions match the strings
            (should (= (plist-get w :before-start) (- (point) (length (plist-get w :before)))))
            (should (= (plist-get w :after-end) (+ (point) (length (plist-get w :after))))))
          ;; near the start: before side fits, the after side gets the rest
          (goto-char (point-min)) (forward-line 1)
          (let ((w (efrit-text-window :chars 100 :ratio 0.75)))
            (should-not (plist-get w :before-cut))
            (should (equal "line 01\n" (plist-get w :before)))
            (should (plist-get w :after-cut))
            (should (> (length (plist-get w :after)) 25)))
          ;; near the end: the other way round
          (goto-char (point-max)) (forward-line -1)
          (let ((w (efrit-text-window :chars 100 :ratio 0.25)))
            (should-not (plist-get w :after-cut))
            (should (equal "line 100\n" (plist-get w :after)))
            (should (plist-get w :before-cut))
            (should (> (length (plist-get w :before)) 25))))
      (kill-buffer buf))))

(ert-deftest test-text-window-region-excluded-and-mid-line-cut ()
  "The region itself is not returned; a cut in the middle of a line
drops that line, so the strings start and end on line boundaries."
  (with-temp-buffer
    (insert "aaaa\nbbbb\ncccc\ndddd\neeee\n")
    (let* ((start (progn (goto-char (point-min)) (search-forward "cccc") (match-beginning 0)))
           (end (match-end 0))
           (w (efrit-text-window :start start :end end :chars 7 :ratio 0.5)))
      ;; 3 chars before would be "bb\n": no whole line in it, so the
      ;; raw cut stays; after "\ndd" -> whole line "\n"
      (should (equal "bb\n" (plist-get w :before)))
      (should (equal "\n" (plist-get w :after)))
      (should (plist-get w :before-cut))
      (should (plist-get w :after-cut)))))

(ert-deftest test-text-window-header ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (setq-local indent-tabs-mode nil)
    (let ((lisp-indent-offset nil))
      (should (equal "; language: emacs-lisp, indentation: 2 spaces" (efrit-text-window-header)))))
  (with-temp-buffer
    (setq-local indent-tabs-mode t tab-width 8 comment-start "# ")
    (should (equal "# language: fundamental, indentation: tabs of width 8" (efrit-text-window-header))))
  (with-temp-buffer
    (setq-local indent-tabs-mode nil)
    (should (equal "language: fundamental, indentation: 4 spaces" (efrit-text-window-header)))))

(provide 'test-text-window)
;;; test-text-window.el ends here
