;;; test-agent-screenshot.el --- /shot layout and show_ui_element -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'efrit-test-sandbox-helpers)
(require 'efrit-agent-screenshot)
(require 'efrit-agent-mentions)
(require 'efrit-do-handlers)

(ert-deftest test-screenshot-layout-names-windows-and-lines ()
  "The layout text lists every window with its buffer, rectangle,
selection mark and line geometry, and the pieces of a mode line; the
slash command is registered; capture refuses where frames cannot be
exported instead of failing oddly."
  (let ((a (generate-new-buffer "*shot-a*"))
        (b (generate-new-buffer "*shot-b*")))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (set-window-buffer (selected-window) a)
          (with-current-buffer a
            (setq-local mode-line-format '("ml-a " (:eval (format "%s" 'piece-a)))))
          (let ((w2 (split-window)))
            (set-window-buffer w2 b)
            (with-current-buffer b (setq-local header-line-format '("hdr-b"))))
          (let ((layout (efrit-agent-screenshot-layout)))
            (should (string-match-p "^FRAME [0-9]+x[0-9]+ px" layout))
            (should (string-match-p "^- \"\\*shot-a\\*\"  [0-9]+ [0-9]+ [0-9]+ [0-9]+  SELECTED" layout))
            (should (string-match-p "^- \"\\*shot-b\\*\"  [0-9]+ [0-9]+ [0-9]+ [0-9]+\n" layout))
            (should (string-match-p "mode-line: y [0-9]+-[0-9]+, x" layout))
            (should (string-match-p "header-line: y" layout))
            ;; the pieces of a's mode line are named
            (should (string-match-p "(:eval (format …))" layout))
            (should (string-match-p "GLOBAL-MODE-STRING" layout))))
      (kill-buffer a) (kill-buffer b)))
  (should (assoc "shot" efrit-agent-slash-commands))
  (when noninteractive
    (should-error (efrit-agent-screenshot-capture) :type 'user-error)))

(ert-deftest test-show-ui-element-pulses-a-shown-window ()
  "show_ui_element remaps the line's face in the buffer for a while,
reports the line's pixel rows, and refuses a buffer that is not shown
or an unknown element; the handler is wired."
  (let ((buf (generate-new-buffer "*pulse-me*"))
        (efrit-agent-ui-pulse-seconds 0.05))
    (unwind-protect
        (save-window-excursion
          (set-window-buffer (selected-window) buf)
          (let* ((r (efrit-tool-show-ui-element '((buffer . "*pulse-me*") (element . "mode-line"))))
                 (res (alist-get 'result r)))
            (should (eq t (alist-get 'success r)))
            (should (equal "mode-line" (alist-get 'element res)))
            (should (= 1 (length (alist-get 'windows res))))
            (should (integerp (alist-get 'y (aref (alist-get 'windows res) 0))))
            ;; the remap is in place, then gone
            (with-current-buffer buf
              (should (assq 'mode-line face-remapping-alist)))
            (sleep-for 0.2)
            (with-current-buffer buf
              (should-not (assq 'mode-line face-remapping-alist))))
          (should-not (eq t (alist-get 'success (efrit-tool-show-ui-element '((buffer . "*pulse-me*") (element . "fringe"))))))
          (let ((input (make-hash-table :test 'equal)))
            (puthash "buffer" "*pulse-me*" input)
            (should (string-match-p "\"element\":\"mode-line\"" (efrit-do--handle-show-ui-element input)))))
      (kill-buffer buf)))
  ;; not shown anywhere: refused with the reason
  (let ((hidden (generate-new-buffer "*pulse-hidden*")))
    (unwind-protect
        (let ((r (efrit-tool-show-ui-element '((buffer . "*pulse-hidden*")))))
          (should-not (eq t (alist-get 'success r)))
          (should (string-match-p "not shown" (format "%s" r))))
      (kill-buffer hidden))))

(provide 'test-agent-screenshot)

;;; test-agent-screenshot.el ends here
