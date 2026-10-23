;;; test-last-error.el --- get_last_error, eval observation, next steps -*- lexical-binding: t; -*-
;;; Code:
(require 'ert)
(require 'efrit-tool-last-error)
(require 'efrit-next-steps)
(require 'efrit-agent-input)   ; loaded before efrit-submit is stubbed

(ert-deftest test-last-error-records-command-errors-with-frames ()
  (let ((efrit-last-error--ring nil) (efrit-last-error-keep 2))
    (efrit-last-error-mode 1)
    (unwind-protect
        (progn
          (should (advice-member-p #'efrit-last-error--record 'command-error-default-function))
          ;; the recorder itself (the real function prints and, in
          ;; batch, stops the test run)
          (let ((this-command 'my-cmd))
            (efrit-last-error--record '(void-variable nme) "" 'my-cmd)
            (efrit-last-error--record '(error "second") "" 'other)
            (efrit-last-error--record '(error "third") "" 'other))
          (should (= 2 (length efrit-last-error--ring)))
          (let* ((r (efrit-tool-get-last-error '((count . 5))))
                 (res (alist-get 'result r))
                 (errs (append (alist-get 'errors res) nil)))
            (should (eq t (alist-get 'recording res)))
            (should (= 2 (alist-get 'count res)))
            (should (equal "third" (alist-get 'error (car errs))))
            (should (equal "other" (alist-get 'command (car errs))))
            (should (vectorp (alist-get 'frames (car errs))))))
      (efrit-last-error-mode -1))
    (let ((res (alist-get 'result (efrit-tool-get-last-error nil))))
      (should (eq :json-false (alist-get 'recording res)))
      (should (alist-get 'note res)))))

(ert-deftest test-eval-observe-reports-messages-and-changed-buffers ()
  (let ((buf (generate-new-buffer "observed")))
    (unwind-protect
        (pcase-let ((`(,value ,messages ,changed)
                     (efrit-eval-observe (lambda ()
                                           (message "efrit-observe-probe %d" 42)
                                           (with-current-buffer buf (insert "x"))
                                           7))))
          (should (= 7 value))
          (should (cl-some (lambda (m) (string-match-p "efrit-observe-probe 42" m)) messages))
          (should (member "observed" changed)))
      (kill-buffer buf))))

(ert-deftest test-next-steps-are-found-marked-and-selectable ()
  (with-temp-buffer
    (insert "Some answer text.\n\nNext steps:\n1. Run the tests (Recommended)\n2. Refactor the loop\n   over two lines\n3. Ship it\n\n")
    (let ((items (efrit-next-steps-find (point-min) (point-max))))
      (should (= 3 (length items)))
      (should (equal "Refactor the loop" (nth 1 (nth 1 items)))))
    (should (= 3 (efrit-next-steps-mark (point-min) (point-max))))
    (goto-char (point-min)) (search-forward "Ship")
    (should (equal '(3 . "Ship it") (get-text-property (point) 'efrit-next-step)))
    (should (equal '((1 . "Run the tests (Recommended)") (2 . "Refactor the loop") (3 . "Ship it"))
                   (efrit-next-steps-of-last-answer)))
    ;; marked twice (the final render pass does that to the last line):
    ;; still one item per step
    (efrit-next-steps-mark (point-min) (point-max))
    (should (= 3 (length (efrit-next-steps-of-last-answer))))
    (let (sent)
      (cl-letf (((symbol-function 'efrit-submit) (lambda (shown api &rest _) (setq sent (list shown api)) t)))
        (efrit-next-step 2)
        (should (string-match-p "step 2" (car sent)))
        (should (string-match-p "Refactor the loop" (cadr sent)))
        (should-error (efrit-next-step 4) :type 'user-error)))
    ;; a list that is not at the end, or without the heading, is not steps
    (erase-buffer)
    (insert "1. a\n2. b\n\nmore text after\n")
    (should-not (efrit-next-steps-find (point-min) (point-max)))))

(provide 'test-last-error)
;;; test-last-error.el ends here
