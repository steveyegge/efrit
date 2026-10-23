;;; test-copilot-batch.el --- notify, presets, code blocks, spinner, shell C-g -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'efrit-markdown)
(require 'efrit-presets)
(require 'efrit-notify)
(require 'efrit-agent-core)
(require 'efrit-do-handlers)

;;;; Code blocks as things

(defun test-cb--render (text)
  "A buffer with TEXT rendered; the caller kills it."
  (let ((buf (generate-new-buffer " *test-cb*")))
    (with-current-buffer buf
      (insert text)
      (efrit-markdown-render (point-min) (point-max) t))
    buf))

(ert-deftest test-code-block-at-point-and-listing ()
  "Rendered blocks carry their language and raw body; the listing is in order and deduplicated."
  (let ((buf (test-cb--render "intro\n\n```elisp\n(+ 1 2)\n(* 3 4)\n```\n\nmiddle\n\n```\nplain\n```\n")))
    (unwind-protect
        (with-current-buffer buf
          (let ((blocks (efrit-markdown-blocks)))
            (should (= 2 (length blocks)))
            (should (equal "elisp" (car (nth 0 blocks))))
            (should (equal "(+ 1 2)\n(* 3 4)\n" (cdr (nth 0 blocks))))
            (should (equal "" (car (nth 1 blocks))))
            (should (equal "plain\n" (cdr (nth 1 blocks)))))
          (goto-char (point-min))
          (should-not (efrit-markdown-block-at))
          (search-forward "(* 3")
          (should (equal "elisp" (car (efrit-markdown-block-at))))
          (should (string-match-p "^2: \\[code\\] plain" (efrit-markdown--block-label (nth 1 (efrit-markdown-blocks)) 2))))
      (kill-buffer buf))))

(ert-deftest test-code-block-copy-and-insert-other-window ()
  "Copy puts the raw body on the kill ring; insert puts it at the
other window's point, keeps focus, and refuses a read-only target."
  (let* ((buf (test-cb--render "```sh\nls -l\n```\n"))
         (target (generate-new-buffer " *test-cb-target*"))
         (kill-ring nil))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (set-window-buffer (selected-window) buf)
          (let ((other (split-window)))
            (set-window-buffer other target)
            (with-current-buffer target (insert "top\n") (set-window-point other (point-max)))
            (with-current-buffer buf
              (goto-char (point-min)) (search-forward "ls")
              (efrit-markdown-copy-block)
              (should (equal "ls -l\n" (current-kill 0)))
              (efrit-markdown-insert-block-other-window)
              (should (eq (current-buffer) buf))
              (should (equal "top\nls -l\n" (with-current-buffer target (buffer-string))))
              (with-current-buffer target (setq buffer-read-only t))
              (should-error (efrit-markdown-insert-block-other-window) :type 'user-error))))
      (kill-buffer buf)
      (kill-buffer target))))

(ert-deftest test-code-block-picker-when-not-on-a-block ()
  "Off a block: one block is taken as is; several go through completion; none errors."
  (let ((buf (test-cb--render "a\n\n```\none\n```\n\n```\ntwo\n```\n")))
    (unwind-protect
        (with-current-buffer buf
          (goto-char (point-min))
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (_p coll &rest _) (car (nth 1 coll)))))
            (should (equal "two\n" (cdr (efrit-markdown-read-block))))))
      (kill-buffer buf))
    (with-temp-buffer
      (should-error (efrit-markdown-read-block) :type 'user-error))))

;;;; Presets

(ert-deftest test-preset-apply-sets-only-present-keys ()
  (let ((efrit-presets '((one :review nil :display-mode minimal)
                         (two :model "m-2" :grants (read write))))
        (efrit-review-enabled t)
        (efrit-agent-display-mode 'smart)
        (efrit-default-model "m-0")
        (efrit-sandbox-default-project-grants '(read))
        (efrit-preset-current nil))
    (efrit-preset-apply 'one)
    (should-not efrit-review-enabled)
    (should (eq 'minimal efrit-agent-display-mode))
    (should (equal "m-0" efrit-default-model))
    (should (eq 'one efrit-preset-current))
    (efrit-preset-apply 'two)
    (should (equal "m-2" efrit-default-model))
    (should (equal '(read write) efrit-sandbox-default-project-grants))
    (should (eq 'minimal efrit-agent-display-mode))
    (should-error (efrit-preset-apply 'none) :type 'user-error)))

;;;; Notify

(ert-deftest test-notify-fires-for-slow-unwatched-turns-only ()
  "Off by default; on, a slow turn whose buffer is not selected notifies
with the reason; a fast one or a watched one does not."
  (let* ((session (efrit-repl-session-create default-directory))
         (buf (generate-new-buffer " *test-notify*"))
         (got nil)
         (efrit-notify-function (lambda (title body) (push (cons title body) got)))
         (efrit-notify-min-seconds 10)
         (event `((:type . turn-complete) (:session-id . ,(efrit-repl-session-id session))
                  (:stop-reason . "end_turn"))))
    (unwind-protect
        (progn
          (setf (efrit-repl-session-buffer session) buf)
          (efrit-repl-session-begin-turn session)
          (aset session (cl-struct-slot-offset 'efrit-repl-session 'current-turn-start)
                (time-subtract (current-time) 30))
          (cl-letf (((symbol-function 'efrit-notify--unwatched-p) (lambda (_) t)))
            (let ((efrit-notify-enabled nil))
              (efrit-notify--on-turn-complete event)
              (should-not got))
            (let ((efrit-notify-enabled t))
              (efrit-notify--on-turn-complete event)
              (should (= 1 (length got)))
              (should (string-match-p "Turn finished after 30 s" (cdar got)))
              (efrit-notify--on-turn-complete (cons '(:stop-reason . "waiting-for-user")
                                                    (assq-delete-all :stop-reason (copy-alist event))))
              (should (string-match-p "question" (cdar got)))
              ;; too fast: no notification
              (aset session (cl-struct-slot-offset 'efrit-repl-session 'current-turn-start) (current-time))
              (efrit-notify--on-turn-complete event)
              (should (= 2 (length got)))))
          ;; watched: no notification even when slow
          (aset session (cl-struct-slot-offset 'efrit-repl-session 'current-turn-start)
                (time-subtract (current-time) 30))
          (cl-letf (((symbol-function 'efrit-notify--unwatched-p) (lambda (_) nil)))
            (let ((efrit-notify-enabled t))
              (efrit-notify--on-turn-complete event)
              (should (= 2 (length got))))))
      (kill-buffer buf))))

(ert-deftest test-notify-default-falls-back-to-message ()
  "Without alert and without D-Bus, the echo area gets it."
  (let ((msg nil))
    (cl-letf (((symbol-function 'require)
               (lambda (feature &rest _) (not (memq feature '(alert notifications)))))
              ((symbol-function 'message) (lambda (f &rest a) (setq msg (apply #'format f a)))))
      (efrit-notify-default "efrit" "done"))
    (should (equal "efrit: done" msg))))

;;;; Spinner timer

(ert-deftest test-spinner-timer-cancels-itself-after-stop-and-buffer-death ()
  "A tick after `efrit-agent--spinner-stop', or after the buffer is
killed, cancels the timer that fired it."
  (let ((buf (generate-new-buffer " *test-spinner*")))
    (with-current-buffer buf
      (efrit-agent--spinner-start "thinking")
      (let ((timer efrit-agent--spinner-timer))
        (should (memq timer timer-list))
        (efrit-agent--spinner-tick buf timer)
        (should (memq timer timer-list))
        (setq efrit-agent--thinking-label nil)
        (efrit-agent--spinner-tick buf timer)
        (should-not (memq timer timer-list))
        (should-not efrit-agent--spinner-timer)))
    (with-current-buffer buf (efrit-agent--spinner-start "again"))
    (let ((timer (buffer-local-value 'efrit-agent--spinner-timer buf)))
      (kill-buffer buf)
      (efrit-agent--spinner-tick buf timer)
      (should-not (memq timer timer-list)))))

;;;; Shell: C-g kills the command

(ert-deftest test-shell-exec-quit-kills-the-process ()
  "A quit while waiting for the command deletes the process."
  (skip-unless (executable-find "sleep"))
  (let ((procs-before (length (process-list)))
        (calls 0))
    (cl-letf (((symbol-function 'efrit-tool--get-project-root) (lambda () temporary-file-directory))
              ((symbol-function 'accept-process-output)
               (lambda (&rest _) (when (> (cl-incf calls) 2) (signal 'quit nil)) nil)))
      (should (eq 'quit (condition-case nil
                            (progn (efrit-do--shell-exec-to-string "sleep 30" 60) nil)
                          (quit 'quit)))))
    (should (= procs-before (length (process-list))))))

(provide 'test-copilot-batch)
;;; test-copilot-batch.el ends here
