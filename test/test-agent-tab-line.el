;;; test-agent-tab-line.el --- Pills, agents in the tab line, mark unread -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'tab-line)
(require 'efrit-agent)
(require 'efrit-agent-ring)
(require 'efrit-agent-tab-line)
(require 'efrit-pill)
(require 'efrit-repl-session)

(defun test-tl--buffer (name)
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t)) (erase-buffer))
      (efrit-agent-mode)
      (efrit-agent--init-regions)
      (efrit-agent--setup-regions)
      (setq default-directory temporary-file-directory)
      (setq efrit-agent--repl-session (efrit-repl-session-create temporary-file-directory))
      (setf (efrit-repl-session-buffer efrit-agent--repl-session) buf))
    buf))

(defun test-tl--id (buf)
  (efrit-repl-session-id (buffer-local-value 'efrit-agent--repl-session buf)))

(defmacro test-tl--with-two (a b &rest body)
  (declare (indent 2))
  `(let* ((efrit-agent-auto-show nil)
          (,a (test-tl--buffer "*efrit[proj]*"))
          (,b (test-tl--buffer "*efrit[proj:fix]*")))
     (unwind-protect (progn ,@body)
       (efrit-agent-tab-line-mode -1)
       (efrit-agent-ring--stop-heartbeat)
       (kill-buffer ,a) (kill-buffer ,b))))

(ert-deftest test-tl-pill-falls-back-to-text-and-keeps-colours-stable ()
  "In batch there is no SVG: the text comes back with a face; the same
key gives the same colour; a state picks its colour; a user drawer wins."
  (let ((p (efrit-pill "hello" :key "k1")))
    (should (equal "hello" (substring-no-properties p)))
    (should (get-text-property 0 'face p)))
  (should (equal (efrit-pill-hue-color "k1") (efrit-pill-hue-color "k1")))
  (should-not (equal (efrit-pill-hue-color "k1") (efrit-pill-hue-color "k2")))
  (should (equal "#c0392b" (efrit-pill-state-color 'waiting)))
  (should (string-prefix-p "#" (efrit-pill-state-color 'waiting t)))
  (should (null (efrit-pill-state-color 'nonsense)))
  (should (equal "tip" (get-text-property 0 'help-echo (efrit-pill "x" :help-echo "tip"))))
  (let ((efrit-pill-function (lambda (text &rest _) (concat "<" text ">"))))
    (should (equal "<x>" (efrit-pill "x")))))

(ert-deftest test-tl-agents-are-appended-and-drawn-as-pills ()
  (test-tl--with-two a b
    (efrit-agent-tab-line-mode 1)
    (with-temp-buffer
      (rename-buffer " *tl-plain*" t)
      (let* ((plain (current-buffer))
             (tabs (efrit-agent-tab-line--tabs (list plain))))
        ;; appended after the window's own buffers, in ring order, once
        (should (equal (list plain a b) (seq-filter (lambda (x) (memq x (list plain a b))) tabs)))
        (should (= 1 (cl-count a (efrit-agent-tab-line--tabs (list plain a)))))
        ;; a plain tab goes to the original formatter, an agent to the pill
        (let* ((calls 0)
               (orig (lambda (tab _tabs) (cl-incf calls) (format "orig:%s" (buffer-name tab))))
               (p (efrit-agent-tab-line--format orig plain tabs))
               (q (efrit-agent-tab-line--format orig a tabs)))
          (should (= 1 calls))
          (should (equal (format "orig:%s" (buffer-name plain)) p))
          (should (equal "● proj" (substring-no-properties q)))
          (should (eq a (get-text-property 0 'tab q)))
          (should (get-text-property 0 'keymap q)))))
    ;; states and labels
    (should (eq 'idle (efrit-agent-tab-line-state a)))
    (should (equal "proj:fix" (efrit-agent-tab-line-label-default b)))
    (efrit-publish 'prompt-open (list (cons :label "sandbox") (cons :session-id (test-tl--id b))))
    (should (eq 'waiting (efrit-agent-tab-line-state b)))
    (should (string-prefix-p "● " (substring-no-properties (efrit-agent-tab-line-pill b))))
    (efrit-publish 'prompt-closed (list (cons :label "sandbox") (cons :session-id (test-tl--id b))))
    (efrit-agent-mark-unread b)
    (should (eq 'unread (efrit-agent-tab-line-state b)))
    (should (string-prefix-p "● " (substring-no-properties (efrit-agent-tab-line-pill b))))
    (should (string-prefix-p "● " (substring-no-properties (efrit-agent-tab-line-pill a))))
    (should (string-suffix-p "=proj=" (substring-no-properties (efrit-agent-tab-line-pill a t))))
    ;; the cache key carries the states, so a change invalidates
    (let ((k1 (efrit-agent-tab-line--cache-key '(x))))
      (efrit-agent-mark-read b)
      (should-not (equal k1 (efrit-agent-tab-line--cache-key '(x)))))
    ;; C-<prior> in a dedicated agent window switches in place
    (save-window-excursion
      (set-window-buffer (selected-window) a)
      (set-window-dedicated-p (selected-window) t)
      (tab-line-switch-to-next-tab nil 1)
      (should (eq b (window-buffer (selected-window))))
      (should (window-dedicated-p (selected-window)))
      (tab-line-switch-to-prev-tab nil 1)
      (should (eq a (window-buffer (selected-window))))
      (set-window-dedicated-p (selected-window) nil))
    ;; the real pipeline, through tab-line's own functions
    (should (memq a (funcall tab-line-tabs-function)))
    (efrit-agent-tab-line-mode -1)
    (should-not (advice-member-p #'efrit-agent-tab-line--tabs tab-line-tabs-function))))

(ert-deftest test-tl-mark-unread-is-pinned-until-you-leave ()
  "A hand-set unread mark survives the dwell of the visit it was set in;
after the buffer leaves view, the next five-second look clears it."
  (test-tl--with-two a b
    (cl-letf (((symbol-function 'frame-focus-state) (lambda (&rest _) t))
              ((symbol-function 'window-end) (lambda (&rest _) (with-current-buffer (window-buffer) (point-max)))))
      (save-window-excursion
        (set-window-buffer (selected-window) a)
        (efrit-agent-mark-unread a)
        (should (buffer-local-value 'efrit-agent--unread a))
        (dotimes (i 8) (efrit-agent-ring-check-dwell (+ 100.0 i)))
        (should (buffer-local-value 'efrit-agent--unread a))
        (should (memq a (efrit-agent-ring-attention-queue)))
        ;; leave, come back, look: cleared
        (set-window-buffer (selected-window) b)
        (efrit-agent-ring-check-dwell 200.0)
        (set-window-buffer (selected-window) a)
        (dotimes (i 8) (efrit-agent-ring-check-dwell (+ 300.0 i)))
        (should-not (buffer-local-value 'efrit-agent--unread a))
        ;; toggle: unread again, then read
        (efrit-agent-toggle-unread a)
        (should (buffer-local-value 'efrit-agent--unread a))
        (efrit-agent-toggle-unread a)
        (should-not (buffer-local-value 'efrit-agent--unread a))))))

(provide 'test-agent-tab-line)

;;; test-agent-tab-line.el ends here
