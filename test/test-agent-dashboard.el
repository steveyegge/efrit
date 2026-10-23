;;; test-agent-dashboard.el --- Folders, state, diagnose, focus -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'efrit-agent)
(require 'efrit-agent-ring)
(require 'efrit-agent-dashboard)
(require 'efrit-agent-diagnose)
(require 'efrit-repl-session)

(defun test-dash--buffer (name)
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

(defmacro test-dash--with-two (a b &rest body)
  (declare (indent 2))
  `(let* ((efrit-agent-auto-show nil)
          (efrit-data-directory (file-name-as-directory (make-temp-file "efrit-dash-" t)))
          (,a (test-dash--buffer " *dash-a*"))
          (,b (test-dash--buffer " *dash-b*")))
     (unwind-protect (progn ,@body)
       (efrit-agent-ring--stop-heartbeat)
       (when (get-buffer efrit-agent-dashboard-buffer) (kill-buffer efrit-agent-dashboard-buffer))
       (kill-buffer ,a) (kill-buffer ,b)
       (ignore-errors (delete-directory efrit-data-directory t)))))

(ert-deftest test-dash-groups-by-folder-with-marks-and-counts ()
  (test-dash--with-two a b
    (should (equal "" (efrit-agent-folder a)))
    (efrit-agent-set-folder a "Work//Billing/")
    (should (equal "Work/Billing" (efrit-agent-folder a)))
    (efrit-publish 'prompt-open (list (cons :label "q") (cons :session-id (test-dash--id b))))
    (let ((entries (efrit-agent-dashboard--entries)))
      ;; two folder headings ("" and Work/Billing) and two agents, headings first in their group
      (should (= 4 (length entries)))
      (should (equal "folder:" (car (nth 0 entries))))
      (should (eq b (car (nth 1 entries))))
      (should (equal "folder:Work/Billing" (car (nth 2 entries))))
      (should (eq a (car (nth 3 entries))))
      ;; b waits: mark and status say so
      (should (equal "●" (substring-no-properties (aref (cadr (nth 1 entries)) 0))))
      (should (equal "●" (substring-no-properties (aref (cadr (nth 3 entries)) 0))))
      (should (equal "waiting" (substring-no-properties (aref (cadr (nth 1 entries)) 6)))))
    (should (string-match-p "1 waiting" (efrit-agent-dashboard--header)))
    (efrit-publish 'prompt-closed (list (cons :label "q") (cons :session-id (test-dash--id b))))
    (efrit-agent-set-folder a "")
    ;; no folders at all: flat rows, no headings
    (should (= 2 (length (efrit-agent-dashboard--entries))))
    (cl-letf (((symbol-function 'pop-to-buffer) (lambda (buf &rest _) (set-buffer buf))))
      (efrit-agent-dashboard)
      (should (derived-mode-p 'efrit-agent-dashboard-mode))
      (should (string-match-p "dash-a" (buffer-string)))
      (should (eq 'revert-buffer (key-binding (kbd "g"))))
      (should (eq 'tabulated-list-sort (key-binding (kbd "S")))))))

(defun test-dash--id (buf)
  (efrit-repl-session-id (buffer-local-value 'efrit-agent--repl-session buf)))

(ert-deftest test-dash-diagnose-rows-are-read-only-facts ()
  (test-dash--with-two a b
    (let* ((efrit-default-model "some-model")
           (sent 0))
      (cl-letf (((symbol-function 'efrit-api-request-async) (lambda (&rest _) (cl-incf sent)))
                ((symbol-function 'efrit-common-get-api-key) (lambda () "k")))
        (let ((rows (efrit-agent-diagnose-collect a)))
          (should (= 0 sent))
          (should (assoc "Session" rows))
          (should (eq 'ok (cadr (assoc "Buffer" rows))))
          (should (eq 'ok (cadr (assoc "Project root" rows))))
          (should (eq 'ok (cadr (assoc "Model" rows))))
          (should (eq 'ok (cadr (assoc "API key" rows))))
          (should (eq 'info (cadr (assoc "Session file" rows))))
          (should-not (cl-some (lambda (r) (eq 'blocked (cadr r))) rows)))
        ;; a recorded worktree that is gone blocks
        (efrit-repl-session-meta-put (buffer-local-value 'efrit-agent--repl-session a)
                                     :worktree "/nonexistent/efrit-wt/")
        (let ((rows (efrit-agent-diagnose-collect a)))
          (should (eq 'blocked (cadr (assoc "Recorded worktree" rows))))
          (should (string-match-p "relocate" (caddr (assoc "Recorded worktree" rows)))))
        ;; an open prompt is a warning row
        (efrit-publish 'prompt-open (list (cons :label "sandbox: x") (cons :session-id (test-dash--id a))))
        (should (eq 'warning (cadr (assoc "Prompt open" (efrit-agent-diagnose-collect a)))))
        (efrit-publish 'prompt-closed (list (cons :label "sandbox: x") (cons :session-id (test-dash--id a))))
        ;; the shareable text has no home path and no error text
        (let ((text (efrit-agent-diagnose-text (list (list "Last Emacs error" 'info "secret details")
                                                     (list "Project root" 'ok (expand-file-name "~/x")))
                                               t)))
          (should-not (string-match-p "secret" text))
          (should-not (string-match-p (regexp-quote (expand-file-name "~")) text)))
        ;; an unknown id: one blocked row, no error
        (should (eq 'blocked (cadr (car (efrit-agent-diagnose-collect "repl-nope-0000")))))))))

(ert-deftest test-dash-focus-and-restore ()
  (test-dash--with-two a b
    (cl-letf (((symbol-function 'delete-other-windows) #'ignore)
              ((symbol-function 'switch-to-buffer) (lambda (buf &rest _) (set-window-buffer (selected-window) buf))))
      (set-frame-parameter nil 'efrit-agent-focus-layout nil)
      (efrit-agent-focus a)
      (should (frame-parameter nil 'efrit-agent-focus-layout))
      (should (eq a (window-buffer (selected-window))))
      (efrit-agent-focus)
      (should-not (frame-parameter nil 'efrit-agent-focus-layout)))))

(provide 'test-agent-dashboard)

;;; test-agent-dashboard.el ends here
