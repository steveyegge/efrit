;;; test-reconnect.el --- Riding out a lost connection -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'efrit-reconnect)
(require 'efrit-loop)
(require 'efrit-repl-session)
(require 'efrit-repl-loop)

(defun test-rc--drain (seconds)
  "Let timers run for SECONDS."
  (let ((end (+ (float-time) seconds)))
    (while (< (float-time) end)
      (accept-process-output nil 0.05)
      (sit-for 0.01))))

(ert-deftest test-reconnect-classifies-errors ()
  "Transport failures are retried; the server's verdicts on the request are not."
  (should (efrit-reconnect-transient-p "… failed.\nNo response within 300s (the connection stalled; the transfer was dropped)"))
  (should (efrit-reconnect-transient-p "curl: (28) Operation timed out"))
  (should (efrit-reconnect-transient-p "HTTP error: (error http 502)"))
  (should (efrit-reconnect-transient-p "{\"type\":\"error\",\"error\":{\"type\":\"overloaded_error\"}}"))
  (should (efrit-reconnect-transient-p "Could not resolve host"))
  (should-not (efrit-reconnect-transient-p "API Error: {\"type\":\"invalid_request_error\",\"message\":\"Output blocked by content filtering policy\"} 400"))
  (should-not (efrit-reconnect-transient-p "HTTP error: (error http 401)"))
  (should-not (efrit-reconnect-transient-p "interrupted"))
  (should (= 5 (efrit-reconnect-backoff 1)))
  (should (= 90 (efrit-reconnect-backoff 9))))

(ert-deftest test-reconnect-retries-then-asks-then-waits ()
  "Two failures → two retries after the probe says up; past the budget the
ask thunk runs; `keep waiting' polls and retries when the probe comes back;
an answer of abort clears the state."
  (let* ((id "rc-1")
         (efrit-reconnect-backoff-seconds '(0.05 0.05 0.05))
         (efrit-reconnect-max-retries 2)
         (efrit-reconnect-wait-poll-seconds 0.05)
         (up t)
         (efrit-reconnect-probe-function (lambda (cb) (funcall cb up)))
         (retries 0) (asked 0)
         (retry (lambda () (cl-incf retries)))
         (ask (lambda () (cl-incf asked)))
         (notes nil)
         (sub (lambda (e) (push (alist-get :text e) notes))))
    (efrit-subscribe 'note sub)
    (unwind-protect
        (progn
          (efrit-reconnect-reset id)
          ;; a permanent error is not taken over
          (should-not (efrit-reconnect-handle-failure id "HTTP error: (error http 400)" retry ask))
          ;; first two transient failures: retried once each
          (should (efrit-reconnect-handle-failure id "connection stalled" retry ask))
          (test-rc--drain 0.3)
          (should (= 1 retries))
          (should (efrit-reconnect-handle-failure id "connection stalled" retry ask))
          (test-rc--drain 0.3)
          (should (= 2 retries))
          ;; third: over the budget, the question
          (should (efrit-reconnect-handle-failure id "connection stalled" retry ask))
          (should (= 1 asked))
          (should (cl-some (lambda (n) (string-match-p "asking what to do" n)) notes))
          ;; the user keeps waiting while the endpoint is down: polls, no retry, no second question
          (setq up nil)
          (should (eq 'wait (efrit-reconnect-answer id "Keep waiting and resume when it is back")))
          (test-rc--drain 0.4)
          (should (= 2 retries))
          (should (= 1 asked))
          (should (cl-some (lambda (n) (string-match-p "still no answer" n)) notes))
          ;; it comes back: one retry, by itself
          (setq up t)
          (test-rc--drain 0.3)
          (should (= 3 retries))
          ;; abort clears
          (should (eq 'abort (efrit-reconnect-answer id "Abort the turn")))
          (should (= 0 (efrit-reconnect-attempts id))))
      (efrit-unsubscribe 'note sub)
      (efrit-reconnect-reset id))))

(ert-deftest test-reconnect-loop-keeps-the-turn-and-resends ()
  "Through the REPL loop: an api-call-fn that fails twice in transit then
answers sees three calls with the same messages, the turn ends normally,
and the session's history is intact."
  (let* ((calls 0) (seen nil)
         (session (efrit-repl-session-create default-directory))
         (efrit-reconnect-backoff-seconds '(0.05))
         (efrit-reconnect-max-retries 5)
         (efrit-reconnect-probe-function (lambda (cb) (funcall cb t)))
         (done nil)
         (fake (lambda (_session messages callback)
                 (cl-incf calls)
                 (push (length messages) seen)
                 (if (< calls 3)
                     (run-at-time 0 nil callback nil "No response within 300s (the connection stalled; the transfer was dropped)")
                   (run-at-time 0 nil callback
                                (let ((r (make-hash-table :test 'equal))
                                      (c (make-hash-table :test 'equal)))
                                  (puthash "type" "text" c) (puthash "text" "done" c)
                                  (puthash "content" (vector c) r) (puthash "stop_reason" "end_turn" r)
                                  (puthash "role" "assistant" r) r)
                                nil)))))
    (cl-letf (((efrit-loop-adapter-api-call-fn efrit-repl-loop--adapter) fake))
      (efrit-repl-continue session "hello" (lambda (_s reason) (setq done reason)))
      (test-rc--drain 1.5))
    (should (equal done "end_turn"))
    (should (= 3 calls))
    ;; the same conversation each time: nothing was dropped or doubled
    (should (= 1 (length (delete-dups seen))))
    (should (eq 'idle (efrit-repl-session-status session)))
    (should (= 0 (efrit-reconnect-attempts (efrit-repl-session-id session))))))

(provide 'test-reconnect)
;;; test-reconnect.el ends here
