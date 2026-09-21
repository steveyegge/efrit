;;; test-limits.el --- per-project loop limits and the raise prompt -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'efrit-limits)
(require 'efrit-repl-session)
(require 'efrit-repl-loop)
(require 'efrit-do)

(defvar efrit-project-root)

(defmacro test-limits--in-project (&rest body)
  (declare (indent 0))
  `(let* ((root (file-name-as-directory (make-temp-file "efrit-lim-" t)))
          (efrit-project-root root)
          (efrit-data-directory (expand-file-name "data" root))
          (efrit-limits--session (make-hash-table :test 'equal))
          (efrit-settings--cache (make-hash-table :test 'equal))
          (efrit-limits--once (make-hash-table :test 'equal)))
     (unwind-protect (progn ,@body)
       (delete-directory root t))))

(ert-deftest test-limits-effective-precedence ()
  (test-limits--in-project
    (should (= 100 (efrit-limits-effective 'max-iterations 100)))
    (efrit-limits-set 'max-iterations 150 'project)
    (should (= 150 (efrit-limits-effective 'max-iterations 100)))
    (efrit-limits-set 'max-iterations 200 'session)
    (should (= 200 (efrit-limits-effective 'max-iterations 100)))
    (efrit-limits-set 'max-iterations 260 'once)
    (should (= 260 (efrit-limits-effective 'max-iterations 100)))
    (efrit-limits-reset-once)
    (should (= 200 (efrit-limits-effective 'max-iterations 100)))))

(ert-deftest test-limits-project-persists-0600-and-reloads ()
  (test-limits--in-project
    (efrit-limits-set 'max-iterations 150 'project)
    (let ((file (efrit-limits-file root)))
      (should (file-exists-p file))
      (should (= 0 (logand (file-modes file) #o077)))
      (efrit-settings-forget)
      (should (= 150 (efrit-limits-effective 'max-iterations 100)))
      ;; another section in the same file survives a limits save
      (efrit-settings-put root "review" (let ((h (make-hash-table :test 'equal))) (puthash "enabled" :false h) h))
      (efrit-limits-set 'max-tool-calls 60 'project)
      (efrit-settings-forget)
      (should (hash-table-p (efrit-settings-get root "review")))
      (should (= 60 (efrit-limits-effective 'max-tool-calls 30)))
      ;; removing the last override removes the section, not the file
      (efrit-limits-set 'max-iterations nil 'project)
      (efrit-limits-set 'max-tool-calls nil 'project)
      (efrit-settings-forget)
      (should-not (efrit-settings-get root "limits"))
      (should (file-exists-p file)))))

(ert-deftest test-limits-bad-file-is-ignored ()
  (test-limits--in-project
    (let ((file (efrit-limits-file root)))
      (make-directory (file-name-directory file) t)
      (with-temp-file file (insert "{\"limits\": {\"max-iterations\": \"lots\", \"evil\": 1}}"))
      (should (= 100 (efrit-limits-effective 'max-iterations 100))))
    (let ((file (efrit-limits-file root)))
      (with-temp-file file (insert "not json"))
      (efrit-settings-forget)
      (should (= 100 (efrit-limits-effective 'max-iterations 100))))))

(ert-deftest test-limits-ask-noninteractive-stops ()
  "With no way to ask, the answer is stop: the old behaviour."
  (test-limits--in-project
    (let ((noninteractive t) (efrit-limits-ask t))
      (should-not (efrit-limits-ask-to-raise 'max-iterations 100)))
    (let ((efrit-limits-ask nil))
      (should-not (efrit-limits-ask-to-raise 'max-iterations 100)))))

(ert-deftest test-limits-ask-answers-apply ()
  (test-limits--in-project
    (let ((efrit-limits-continue-step 50) (noninteractive nil))
      (cl-letf (((symbol-function 'efrit-limits--define-menu) (lambda () nil))
                ((symbol-function 'efrit-limits--ask-in-echo-area) (lambda () 'once)))
        (should (= 150 (efrit-limits-ask-to-raise 'max-iterations 100)))
        (should (= 150 (efrit-limits-effective 'max-iterations 100)))
        (efrit-limits-reset-once)
        (should (= 100 (efrit-limits-effective 'max-iterations 100))))
      (cl-letf (((symbol-function 'efrit-limits--define-menu) (lambda () nil))
                ((symbol-function 'efrit-limits--ask-in-echo-area) (lambda () 'project)))
        ;; 100+50 rounded up to a multiple of 50 = 150
        (should (= 150 (efrit-limits-ask-to-raise 'max-iterations 100)))
        (should (file-exists-p (efrit-limits-file root))))
      (cl-letf (((symbol-function 'efrit-limits--define-menu) (lambda () nil))
                ((symbol-function 'efrit-limits--ask-in-echo-area) (lambda () nil)))
        (should-not (efrit-limits-ask-to-raise 'max-iterations 150))))))

;;; Loop integration: the cap asks, a raise continues, stop ends the turn

(defun test-limits--text (text)
  (let ((ht (make-hash-table :test 'equal)))
    (puthash "type" "text" ht) (puthash "text" text ht) ht))
(defun test-limits--tool-use (id)
  (let ((ht (make-hash-table :test 'equal)) (in (make-hash-table :test 'equal)))
    (puthash "expr" "1" in)
    (puthash "type" "tool_use" ht) (puthash "id" id ht) (puthash "name" "eval_sexp" ht) (puthash "input" in ht) ht))
(defun test-limits--response (content stop)
  (let ((r (make-hash-table :test 'equal)))
    (puthash "content" content r) (puthash "stop_reason" stop r) r))

(defmacro test-limits--with-loop (responses &rest body)
  (declare (indent 1))
  `(let ((test-responses ,responses) (efrit-review-enabled nil))
     (cl-letf (((symbol-function 'efrit-repl-loop--api-call)
                (lambda (_s _m cb)
                  (let ((r (pop test-responses)))
                    (if r (funcall cb r nil) (funcall cb nil "queue empty")))))
               ((symbol-function 'efrit-do--execute-tool-string) (lambda (_i) "1"))
               ((symbol-function 'efrit-agent-set-status) #'ignore))
       (unwind-protect (progn ,@body) (clrhash efrit-repl-loop--active)))))

(ert-deftest test-limits-loop-stops-at-cap-when-declined ()
  (test-limits--in-project
    (let ((efrit-repl-loop-max-iterations 2) (efrit-limits-ask nil)
          (session (efrit-repl-session-create)) (reason nil))
      (test-limits--with-loop
          (list (test-limits--response (vector (test-limits--tool-use "a")) "tool_use")
                (test-limits--response (vector (test-limits--tool-use "b")) "tool_use")
                (test-limits--response (vector (test-limits--text "unreached")) "end_turn"))
        (efrit-repl-continue session "go" (lambda (_s r) (setq reason r)))
        (should (equal reason "iteration-limit"))))))

(ert-deftest test-limits-loop-raise-once-continues ()
  (test-limits--in-project
    (let ((efrit-repl-loop-max-iterations 2) (efrit-limits-ask t) (noninteractive nil)
          (efrit-limits-continue-step 5)
          (session (efrit-repl-session-create)) (reason nil) (asked 0))
      (cl-letf (((symbol-function 'efrit-limits--define-menu) (lambda () nil))
                ((symbol-function 'efrit-limits--ask-in-echo-area)
                 (lambda () (cl-incf asked) 'once)))
        (test-limits--with-loop
            (list (test-limits--response (vector (test-limits--tool-use "a")) "tool_use")
                  (test-limits--response (vector (test-limits--tool-use "b")) "tool_use")
                  (test-limits--response (vector (test-limits--text "done")) "end_turn"))
          (efrit-repl-continue session "go" (lambda (_s r) (setq reason r)))
          (should (equal reason "end_turn"))
          (should (= asked 1))
          ;; the once-raise is gone after the turn
          (should (= 2 (efrit-limits-effective 'max-iterations 2))))))))

(ert-deftest test-limits-eval-cannot-raise-cap ()
  (require 'efrit-sandbox-eval)
  (should (efrit-sandbox-eval-inspect '(efrit-limits-set 'max-iterations 9999 'session)))
  (should (efrit-sandbox-eval-inspect '(setq efrit-limits-ask nil))))

(ert-deftest test-limits-circuit-breaker-cap-asks-and-raises ()
  "At the tool-call cap the breaker asks; a raise lets the call through."
  (require 'efrit-do-circuit-breaker)
  (test-limits--in-project
    (let ((efrit-do-max-tool-calls-per-session 3)
          (efrit-do-circuit-breaker-enabled t)
          (efrit-limits-ask t) (noninteractive nil)
          (efrit-limits-continue-step 5)
          (answers (list 'once nil)))
      (efrit-do--circuit-breaker-reset)
      (setq efrit-do--session-tool-count 3)
      (cl-letf (((symbol-function 'efrit-limits--define-menu) (lambda () nil))
                ((symbol-function 'efrit-limits--ask-in-echo-area) (lambda () (pop answers))))
        ;; first: user says continue once -> allowed, cap now 8
        (should (car (efrit-do--circuit-breaker-check-limits "read_file" nil)))
        (should (= 8 (efrit-do--tool-call-cap)))
        (should-not efrit-do--circuit-breaker-tripped)
        ;; at the raised cap the user says no -> tripped, message names the variable
        (setq efrit-do--session-tool-count 8)
        (let ((r (efrit-do--circuit-breaker-check-limits "read_file" nil)))
          (should-not (car r))
          (should (string-match-p "efrit-do-max-tool-calls-per-session" (cdr r))))
        (should efrit-do--circuit-breaker-tripped))
      (efrit-do--circuit-breaker-reset))))

(ert-deftest test-limits-menu-details-toggle-and-yank ()
  (test-limits--in-project
    (let ((efrit-limits--context (list :name 'max-tool-calls :current 30 :step 50 :root root))
          (efrit-limits--details-shown nil)
          (kill-ring nil))
      (should (equal (efrit-limits--toggle-label) "show limits in force"))
      (should-not (string-match-p "max-iterations" (efrit-limits--menu-description)))
      (efrit-limits-toggle-details)
      (should (equal (efrit-limits--toggle-label) "hide limits in force"))
      (should (string-match-p "max-iterations .*default" (efrit-limits--menu-description)))
      (efrit-limits-yank-details)
      (should (string-match-p "Settings: " (car kill-ring))))))

(provide 'test-limits)
;;; test-limits.el ends here
