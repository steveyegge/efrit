;;; test-agent-scroll.el --- following-windows scrolling -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'efrit-agent)

(ert-deftest test-agent-scroll-only-following-windows-move ()
  "A window scrolled up to read stays put; one at the end follows."
  (skip-unless (not noninteractive))
  (with-temp-buffer
    (efrit-agent-mode)
    (efrit-agent--setup-regions)
    (dotimes (i 200) (efrit-agent--append-to-conversation (format "line %d\n" i) nil))
    (let* ((reader (split-window))
           (follower (selected-window)))
      (set-window-buffer reader (current-buffer))
      (set-window-buffer follower (current-buffer))
      (set-window-point reader 10)
      (set-window-point follower (point-max))
      (unwind-protect
          (progn
            (efrit-agent--append-to-conversation "new tail\n" nil)
            (should (= (window-point reader) 10))
            (should (>= (window-point follower) (- (point-max) 20))))
        (delete-window reader)))))

(ert-deftest test-agent-scroll-following-windows-predicate ()
  "Pure check of the follower predicate in batch mode."
  (with-temp-buffer
    (efrit-agent-mode)
    (efrit-agent--setup-regions)
    (dotimes (i 50) (efrit-agent--append-to-conversation (format "l%d\n" i) nil))
    (let ((w (selected-window)))
      (set-window-buffer w (current-buffer))
      (set-window-point w (point-min))
      (should-not (memq w (efrit-agent--following-windows)))
      (set-window-point w (marker-position efrit-agent--conversation-end))
      (should (memq w (efrit-agent--following-windows))))))

(provide 'test-agent-scroll)
;;; test-agent-scroll.el ends here
