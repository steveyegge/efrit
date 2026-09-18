;;; test-ui-helpers.el --- popups: preview mode, q dismisses, dedicated -*- lexical-binding: t; -*-

;;; Commentary:
;; `efrit-show-preview' must produce a buffer where `q' is bound to
;; `quit-window' whatever the major mode, in a dedicated window so
;; quitting removes the window instead of leaving it on another
;; buffer.  Window assertions run only when a real frame exists
;; \(batch Emacs has one frame with one window, which is enough).

;;; Code:

(require 'ert)
(require 'efrit-ui-helpers)

(defun test-ui--q-binding (buf)
  (with-current-buffer buf (key-binding (kbd "q"))))

(ert-deftest test-ui-preview-default-mode-quits-with-q ()
  (let ((buf nil))
    (unwind-protect
        (progn
          (efrit-show-preview "*efrit-test-preview*" "hello")
          (setq buf (get-buffer "*efrit-test-preview*"))
          (should buf)
          (with-current-buffer buf
            (should (derived-mode-p 'efrit-preview-mode))
            (should buffer-read-only)
            (should (equal (buffer-string) "hello")))
          (should (eq (test-ui--q-binding buf) #'quit-window)))
      (when buf (ignore-errors (delete-window (get-buffer-window buf))) (kill-buffer buf)))))

(ert-deftest test-ui-preview-diff-mode-gets-q-too ()
  "A non-special mode (diff-mode) still dismisses with q via the composed map."
  (let ((buf nil))
    (unwind-protect
        (progn
          (efrit-show-preview "*efrit-test-diff*" "--- a\n+++ b\n" 'diff-mode)
          (setq buf (get-buffer "*efrit-test-diff*"))
          (with-current-buffer buf
            (should (derived-mode-p 'diff-mode))
            (should buffer-read-only))
          (should (eq (test-ui--q-binding buf) #'quit-window)))
      (when buf (ignore-errors (delete-window (get-buffer-window buf))) (kill-buffer buf)))))

(ert-deftest test-ui-preview-window-is-dedicated-and-quit-removes-it ()
  (let ((buf nil) (win nil))
    (unwind-protect
        (progn
          (setq win (efrit-show-preview "*efrit-test-dedicated*" "x"))
          (setq buf (get-buffer "*efrit-test-dedicated*"))
          (skip-unless (window-live-p win))
          ;; batch Emacs may refuse to split a tiny frame; only assert
          ;; when we really got a second window
          (skip-unless (not (eq win (frame-root-window))))
          (should (window-dedicated-p win))
          (with-selected-window win (quit-window))
          (should-not (window-live-p win)))
      (when buf (kill-buffer buf)))))

(ert-deftest test-ui-preview-reuses-buffer ()
  (let ((buf nil))
    (unwind-protect
        (progn
          (efrit-show-preview "*efrit-test-reuse*" "one")
          (efrit-show-preview "*efrit-test-reuse*" "two")
          (setq buf (get-buffer "*efrit-test-reuse*"))
          (with-current-buffer buf (should (equal (buffer-string) "two"))))
      (when buf (ignore-errors (delete-window (get-buffer-window buf))) (kill-buffer buf)))))

(provide 'test-ui-helpers)
;;; test-ui-helpers.el ends here
