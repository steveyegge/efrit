;;; test-testdrive.el --- the live test drive's machinery -*- lexical-binding: t; -*-

;;; Commentary:
;; The drive itself needs a person and a model.  What is tested here is
;; the machinery under it: the step macro and its report lines, the
;; throwaway project, event collection over the bus, the turn driver
;; over a stubbed `efrit-submit', consent, and cleanup.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'efrit-testdrive)
(require 'efrit-agent-input)   ; loaded before `efrit-submit' is stubbed

(defmacro test-td--fresh (&rest body)
  "Run BODY with a fresh report buffer and drive state, no windows touched."
  (declare (indent 0))
  `(let ((efrit-testdrive--results nil)
         (efrit-testdrive--events nil)
         (efrit-testdrive--turns 0)
         (efrit-testdrive--layout nil)
         (efrit-testdrive--root nil)
         (efrit-testdrive--buffer " *test-testdrive*"))
     (cl-letf (((symbol-function 'efrit-testdrive--show-report) #'ignore)
               ((symbol-function 'efrit-testdrive--restore-layout) #'ignore)
               ((symbol-function 'message) (lambda (&rest _) nil)))
       (unwind-protect (progn ,@body)
         (when efrit-testdrive--root
           (ignore-errors (delete-directory efrit-testdrive--root t)))
         (when-let* ((b (get-buffer efrit-testdrive--buffer))) (kill-buffer b))))))

(defun test-td--report ()
  (with-current-buffer (efrit-testdrive--buf)
    (buffer-substring-no-properties (point-min) (point-max))))

(ert-deftest test-testdrive-step-records-every-outcome ()
  (test-td--fresh
    (efrit-testdrive--step 1 "passes" 'PASS)
    (efrit-testdrive--step 1 "passes by default" nil)
    (efrit-testdrive--step 1 "fails with a note" (cons 'FAIL "saw nothing"))
    (efrit-testdrive--step 1 "skips" 'SKIP)
    (efrit-testdrive--step 1 "errors" (error "boom"))
    (let ((rs (reverse efrit-testdrive--results)))
      (should (equal (mapcar #'caddr rs) '(PASS PASS FAIL SKIP FAIL)))
      (should (equal (nth 3 (nth 2 rs)) "saw nothing"))
      (should (string-match-p "error: boom" (nth 3 (nth 4 rs)))))
    (let ((report (test-td--report)))
      (should (string-match-p "^\\*\\*\\* passes$" report))
      (should (string-match-p "^\\*\\* PASS passes  ([0-9.]+s)$" report))
      (should (string-match-p "^\\*\\* FAIL fails with a note" report))
      (should (string-match-p "^   saw nothing$" report))
      (should (string-match-p "^\\*\\* SKIP skips" report)))))

(ert-deftest test-testdrive-step-flags-slow-and-quits ()
  (test-td--fresh
    (let ((efrit-testdrive-step-budget 0))
      (efrit-testdrive--step 2 "slow one" (sleep-for 0.01) 'PASS)
      (should (string-match-p "SLOW: 0s, budget 0s" (nth 3 (car efrit-testdrive--results)))))
    ;; time spent waiting for the user is not the step's time
    (let ((efrit-testdrive-step-budget 0))
      (cl-letf (((symbol-function 'read-char-choice)
                 (lambda (&rest _) (cl-incf efrit-testdrive--waited 100) ?y)))
        (efrit-testdrive--step 2 "waited" (efrit-testdrive--ask "ok?"))
        (should (eq (caddr (car efrit-testdrive--results)) 'PASS))
        (should (< (nth 4 (car efrit-testdrive--results)) 1))))
    ;; q inside a step stops the drive; C-g too
    (should-error (efrit-testdrive--step 2 "quits" (signal 'efrit-testdrive-quit nil))
                  :type 'efrit-testdrive-quit)
    (should-error (efrit-testdrive--step 2 "c-g" (signal 'quit nil))
                  :type 'efrit-testdrive-quit)))

(ert-deftest test-testdrive-ask-and-confirm-map-keys ()
  (test-td--fresh
    (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _) ?y)))
      (should (eq (efrit-testdrive--ask "?") 'PASS))
      (should (efrit-testdrive--confirm "?")))
    (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _) ?s)))
      (should (eq (efrit-testdrive--ask "?") 'SKIP))
      (should-not (efrit-testdrive--confirm "?")))
    (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _) ?n))
              ((symbol-function 'read-string) (lambda (&rest _) "it was blank")))
      (should (equal (efrit-testdrive--ask "?") '(FAIL . "it was blank"))))
    (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _) ?q)))
      (should-error (efrit-testdrive--ask "?") :type 'efrit-testdrive-quit)
      (should-error (efrit-testdrive--confirm "?") :type 'efrit-testdrive-quit))
    ;; after-confirm: a skip is a SKIP result, a yes runs the body
    (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _) ?s)))
      (should (equal (efrit-testdrive--after-confirm "?" 'PASS) '(SKIP . "skipped by user"))))
    (cl-letf (((symbol-function 'read-char-choice) (lambda (&rest _) ?\r)))
      (should (eq (efrit-testdrive--after-confirm "?" 'PASS) 'PASS)))))

(ert-deftest test-testdrive-project-is-throwaway-and-cleaned ()
  (test-td--fresh
    (setq efrit-testdrive--root (efrit-testdrive--make-project))
    (should (file-directory-p efrit-testdrive--root))
    (should (string-prefix-p (file-truename temporary-file-directory)
                             (file-truename efrit-testdrive--root)))
    (should (string-match-p "PELICAN" (efrit-testdrive--file-text "notes.txt")))
    (should (string-match-p "Hello, %s!" (efrit-testdrive--file-text "greet.el")))
    (should-not (efrit-testdrive--file-text "missing.txt"))
    ;; a session grant on it goes away with it
    (efrit-sandbox-grant 'write efrit-testdrive--root 'session efrit-testdrive--root)
    (should (efrit-sandbox-grants efrit-testdrive--root))
    (efrit-testdrive--cleanup)
    (should-not (file-exists-p efrit-testdrive--root))
    (should-not (efrit-sandbox-grants efrit-testdrive--root))
    (should (string-match-p "Cleaned up" (test-td--report)))
    (setq efrit-testdrive--root nil)))

(ert-deftest test-testdrive-collects-events-and-reads-them ()
  (test-td--fresh
    (efrit-subscribe t #'efrit-testdrive--on-event)
    (unwind-protect
        (progn
          (efrit-publish 'text-delta '((:session-id . "s") (:text . "PO")))
          (efrit-publish 'text-delta '((:session-id . "s") (:text . "NG")))
          (efrit-publish 'tool-result '((:session-id . "s") (:tool . "read_file") (:success . t)))
          (efrit-publish 'tool-result '((:session-id . "s") (:tool . "shell_exec") (:success . t)))
          (efrit-publish 'turn-complete '((:session-id . "s") (:stop-reason . "end_turn")))
          (should (equal (efrit-testdrive--reply-text) "PONG"))
          (should (equal (efrit-testdrive--tools-run) '("read_file" "shell_exec")))
          (let ((ev (car (efrit-testdrive--events-of 'turn-complete))))
            (should (equal (efrit-testdrive--stop-reason ev) "end_turn"))
            (should (string-match-p "stop end_turn; tools (read_file shell_exec)"
                                    (efrit-testdrive--turn-note ev))))
          (efrit-testdrive--clear-events)
          (should-not (efrit-testdrive--events-of 'turn-complete))
          (should (equal (efrit-testdrive--reply-text) "")))
      (efrit-unsubscribe t #'efrit-testdrive--on-event))))

(ert-deftest test-testdrive-turn-drives-submit-and-waits ()
  "The turn driver sends through `efrit-submit' with the project bound
and returns the turn-complete event; a busy buffer is an error; a
silent model is a timeout that cancels."
  (test-td--fresh
    (setq efrit-testdrive--root (efrit-testdrive--make-project))
    (efrit-subscribe t #'efrit-testdrive--on-event)
    (unwind-protect
        (let ((sent nil) (root-seen nil) (cancelled nil))
          (cl-letf (((symbol-function 'efrit-submit)
                     (lambda (shown &optional api-input)
                       (setq sent (list shown api-input) root-seen efrit-project-root)
                       ;; the model answers on the next event-loop tick
                       (run-at-time 0.01 nil
                                    (lambda ()
                                      (efrit-publish 'text-delta '((:text . "PONG")))
                                      (efrit-publish 'turn-complete '((:stop-reason . "end_turn")))))
                       t))
                    ((symbol-function 'efrit-testdrive--agent-buffer) (lambda () (current-buffer)))
                    ((symbol-function 'efrit-agent-cancel) (lambda () (setq cancelled t))))
            (let ((ev (efrit-testdrive--turn "ping" "say PONG")))
              (should (equal sent '("ping" "say PONG")))
              (should (equal root-seen efrit-testdrive--root))
              (should (equal (efrit-testdrive--stop-reason ev) "end_turn"))
              (should (equal (efrit-testdrive--reply-text) "PONG"))
              (should (= efrit-testdrive--turns 1))
              (should-not cancelled)))
          ;; busy
          (cl-letf (((symbol-function 'efrit-submit) (lambda (&rest _) nil)))
            (should-error (efrit-testdrive--turn "ping")))
          ;; timeout: nil back, the turn is cancelled
          (cl-letf (((symbol-function 'efrit-submit) (lambda (&rest _) t))
                    ((symbol-function 'efrit-testdrive--agent-buffer) (lambda () (current-buffer)))
                    ((symbol-function 'efrit-agent-cancel) (lambda () (setq cancelled t)))
                    (efrit-testdrive-turn-timeout 0))
            (should-not (efrit-testdrive--turn "ping"))
            (should cancelled)))
      (efrit-unsubscribe t #'efrit-testdrive--on-event))))

(ert-deftest test-testdrive-summary-counts-and-lists-failures ()
  (test-td--fresh
    (setq efrit-testdrive--results
          (list (list 3 "third" 'FAIL "broke" 1.0)
                (list 2 "second" 'SKIP nil 0.5)
                (list 1 "first" 'PASS nil 70.0)))
    (setq efrit-testdrive--turns 4)
    (efrit-testdrive--summary)
    (let ((report (test-td--report)))
      (should (string-match-p "^\\* Summary: 1 PASS, 1 FAIL, 1 SKIP in 72s, 4 model turn(s)$" report))
      (should (string-match-p "^- SLOW \\[1\\] first: 70s$" report))
      (should (string-match-p "^- FAIL \\[3\\] third: broke$" report)))))

(ert-deftest test-testdrive-declined-sends-nothing ()
  (test-td--fresh
    (let ((submitted nil))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil))
                ((symbol-function 'efrit-submit) (lambda (&rest _) (setq submitted t) t))
                ((symbol-function 'current-window-configuration) (lambda () nil)))
        (should-error (efrit-testdrive) :type 'user-error)
        (should-not submitted)
        (should-not efrit-testdrive--root)
        (should (string-match-p "declined; nothing was sent" (test-td--report)))))))

(ert-deftest test-testdrive-sections-are-well-formed ()
  (dolist (s efrit-testdrive--sections)
    (should (numberp (car s)))
    (should (stringp (cadr s)))
    (should (fboundp (caddr s))))
  ;; the model cannot start a drive from eval_sexp
  (require 'efrit-sandbox-eval)
  (should (efrit-sandbox-eval-inspect '(efrit-testdrive)))
  (should (efrit-sandbox-eval-inspect '(efrit-testdrive--cleanup))))

(provide 'test-testdrive)
;;; test-testdrive.el ends here
