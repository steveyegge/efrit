;;; efrit-reconnect.el --- Ride out a lost connection without losing the turn -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.10.3
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, ai

;;; Commentary:

;; 2026-10-04 20:02: the model's sixth request of a turn stalled; after
;; the watchdog the turn ended as "api-error" and the fourteen fetched
;; articles, the searches and the half-written answer were gone.  tzz:
;; "I'd like efrit to try to resolve the connectivity issue … check
;; periodically if the connection is back up, or ask the user if they
;; want to abort the work."
;;
;; A failed request is not a failed turn.  The messages that were about
;; to be sent are still in the session; sending them again costs
;; nothing but time.  So:
;;
;;   1. Classify the error.  Transport trouble (a stall, a timeout, DNS,
;;      connection refused or reset, 5xx, 429, 529) is retried.  Anything
;;      the server said about the request itself (400, 401, 403, a
;;      content-policy refusal) is not: sending it again gets the same
;;      answer.
;;   2. Retry the same request with a growing pause
;;      (`efrit-reconnect-backoff-seconds'), noting each try in the
;;      transcript.  Before each retry a cheap probe asks the endpoint
;;      whether it answers at all; while it does not, the pause is
;;      spent waiting rather than failing.
;;   3. After the budget (`efrit-reconnect-max-retries') the turn pauses
;;      on a question: keep waiting (probe every
;;      `efrit-reconnect-wait-poll-seconds', resume by itself), retry now,
;;      or abort.  No timeout answers it; unattended mode answers
;;      "keep waiting".
;;
;; Nothing here is streaming-specific: the loop's `api-call-fn' is
;; wrapped, and both transports report errors the same way.

;;; Code:

(require 'cl-lib)
(require 'url)
(require 'efrit-log)
(require 'efrit-events)

(declare-function efrit-common-get-base-url "efrit-common")

(defgroup efrit-reconnect nil
  "Retrying API requests that fail for network reasons."
  :group 'efrit)

(defcustom efrit-reconnect-backoff-seconds '(5 15 45 90)
  "Pauses before the first, second, … retry of a request that failed in transit.
The last value repeats for later retries while the user has chosen to
keep waiting."
  :type '(repeat number)
  :group 'efrit-reconnect)

(defcustom efrit-reconnect-max-retries 4
  "Retries before the turn pauses and asks whether to keep waiting or abort.
nil: never ask, keep retrying with the last backoff."
  :type '(choice (const :tag "Retry forever" nil) integer)
  :group 'efrit-reconnect)

(defcustom efrit-reconnect-wait-poll-seconds 60
  "While the user (or unattended mode) chose to keep waiting: probe this often."
  :type 'number
  :group 'efrit-reconnect)

(defcustom efrit-reconnect-probe-timeout 10
  "Seconds the connectivity probe waits for the endpoint."
  :type 'integer
  :group 'efrit-reconnect)

(defconst efrit-reconnect--transport-regexps
  '("connection stalled" "No response within" "timed out" "timeout"
    "Name or service not known" "Could not resolve" "DNS" "connection refused"
    "Connection reset" "connection broken" "Network is unreachable" "Temporary failure"
    "failed to connect" "exited abnormally" "curl: (\\(?:6\\|7\\|28\\|35\\|52\\|55\\|56\\))"
    "HTTP error: (error http 5[0-9][0-9])" "\"type\": *\"overloaded_error\""
    "\\b5[0-9][0-9]\\b.*\\(?:Bad Gateway\\|Service Unavailable\\|Gateway Time-out\\|Internal Server Error\\)"
    "Bad Gateway" "Service Unavailable" "Gateway Time-?out" "\\b429\\b" "\\b529\\b"
    "rate_limit_error" "api_error")
  "Error texts that mean the request never got a real answer and may be repeated.")

(defconst efrit-reconnect--permanent-regexps
  '("\\b40[013]\\b" "invalid_request_error" "authentication_error" "permission_error"
    "not_found_error" "content filtering" "Output blocked" "interrupted")
  "Error texts that would come back the same: do not retry.")

(defun efrit-reconnect-transient-p (error-text)
  "Non-nil when ERROR-TEXT describes a transport failure worth retrying."
  (and (stringp error-text)
       (not (cl-some (lambda (re) (string-match-p re error-text)) efrit-reconnect--permanent-regexps))
       (cl-some (lambda (re) (string-match-p re error-text)) efrit-reconnect--transport-regexps)))

(defun efrit-reconnect-backoff (attempt)
  "Seconds to wait before retry number ATTEMPT (1-based)."
  (let ((list efrit-reconnect-backoff-seconds))
    (or (nth (1- attempt) list) (car (last list)) 30)))

;;;; The probe

(defvar efrit-reconnect-probe-function #'efrit-reconnect--probe-endpoint
  "Function (CALLBACK) that calls CALLBACK with non-nil when the endpoint answers.
Tests and the drive replace it.")

(defun efrit-reconnect--probe-endpoint (callback)
  "GET the API base URL; any HTTP answer at all counts as reachable."
  (condition-case err
      (let* ((url (concat (string-remove-suffix "/" (efrit-common-get-base-url)) "/"))
             (settled nil)
             (watchdog nil)
             (buffer
              (url-retrieve
               url
               (lambda (status)
                 (unless settled
                   (setq settled t)
                   (when watchdog (cancel-timer watchdog))
                   (let* ((err (plist-get status :error))
                          ;; an HTTP status, even 401/404, means the host
                          ;; answered; only a connection-level error means no
                          (up (or (null err)
                                  (and (consp err) (eq (cadr err) 'http)))))
                     (efrit-log 'info "reconnect: probe %s -> %s" url (if up "answers" (format "%S" err)))
                     (let ((kill-buffer-query-functions nil))
                       (ignore-errors (kill-buffer (current-buffer))))
                     (funcall callback up))))
               nil t t)))
        (setq watchdog
              (run-at-time efrit-reconnect-probe-timeout nil
                           (lambda ()
                             (unless settled
                               (setq settled t)
                               (when (and buffer (buffer-live-p buffer))
                                 (when-let* ((p (get-buffer-process buffer)))
                                   (set-process-query-on-exit-flag p nil)
                                   (delete-process p))
                                 (let ((kill-buffer-query-functions nil)) (kill-buffer buffer)))
                               (efrit-log 'info "reconnect: probe %s -> no answer in %ds" url efrit-reconnect-probe-timeout)
                               (funcall callback nil))))))
    (error
     (efrit-log 'warn "reconnect: probe could not start: %s" (error-message-string err))
     (funcall callback nil))))

;;;; Per-session state

(defvar efrit-reconnect--state (make-hash-table :test #'equal)
  "Session id -> plist (:attempts N :timer TIMER :waiting BOOL :retry THUNK).")

(defun efrit-reconnect--get (session-id key)
  (plist-get (gethash session-id efrit-reconnect--state) key))

(defun efrit-reconnect--set (session-id key value)
  (puthash session-id (plist-put (gethash session-id efrit-reconnect--state) key value)
           efrit-reconnect--state))

(defun efrit-reconnect-reset (session-id)
  "Forget SESSION-ID's retry state: a request got through, or the turn ended."
  (when-let* ((timer (efrit-reconnect--get session-id :timer)))
    (when (timerp timer) (cancel-timer timer)))
  (remhash session-id efrit-reconnect--state))

(defun efrit-reconnect-attempts (session-id)
  (or (efrit-reconnect--get session-id :attempts) 0))

(defun efrit-reconnect--note (session-id text &optional face)
  (efrit-publish 'note `((:session-id . ,session-id) (:text . ,(concat "⇄ " text))
                         (:face . ,(or face 'warning)) (:kind . reconnect))))

;;;; Deciding

(defun efrit-reconnect-handle-failure (session-id error-text retry ask)
  "Decide what to do after a request for SESSION-ID failed with ERROR-TEXT.
RETRY is a thunk that sends the same request again.  ASK is a thunk
that pauses the turn with the keep-waiting / retry / abort question.
Returns non-nil when the failure was taken over (a retry or the
question is scheduled); nil when the caller should treat it as final."
  (when (efrit-reconnect-transient-p error-text)
    (let* ((attempt (1+ (efrit-reconnect-attempts session-id)))
           (waiting (efrit-reconnect--get session-id :waiting)))
      (efrit-reconnect--set session-id :attempts attempt)
      (efrit-reconnect--set session-id :retry retry)
      (efrit-reconnect--set session-id :ask ask)
      (efrit-publish 'reconnect-failure `((:session-id . ,session-id) (:attempt . ,attempt)
                                          (:error . ,error-text)))
      (cond
       ;; the user said keep waiting: probe on the slow clock, never ask again
       (waiting
        (efrit-reconnect--schedule-probe session-id efrit-reconnect-wait-poll-seconds t)
        t)
       ((and efrit-reconnect-max-retries (> attempt efrit-reconnect-max-retries))
        (efrit-reconnect--note session-id
                               (format "the connection failed %d times (%s); asking what to do"
                                       (1- attempt) (efrit-reconnect--short error-text)))
        (funcall ask)
        t)
       (t
        (let ((pause (efrit-reconnect-backoff attempt)))
          (efrit-reconnect--note session-id
                                 (format "request failed in transit (%s); retry %d%s in %ss, the turn's work is kept"
                                         (efrit-reconnect--short error-text) attempt
                                         (if efrit-reconnect-max-retries (format " of %d" efrit-reconnect-max-retries) "")
                                         pause))
          (efrit-reconnect--schedule-probe session-id pause nil)
          t))))))

(defun efrit-reconnect--short (error-text)
  "The useful part of ERROR-TEXT for a note: the last line, trimmed."
  (let ((lines (split-string (or error-text "") "\n" t)))
    (truncate-string-to-width (string-trim (or (car (last lines)) "")) 90 nil nil "…")))

(defun efrit-reconnect--schedule-probe (session-id delay slow)
  "After DELAY seconds, probe; when the endpoint answers, retry; else wait again.
SLOW means the user chose to keep waiting: the poll interval applies
and no question is asked."
  (efrit-reconnect--set
   session-id :timer
   (run-at-time delay nil
                (lambda ()
                  (funcall efrit-reconnect-probe-function
                           (lambda (up)
                             (cond
                              ((not (gethash session-id efrit-reconnect--state)) nil) ; turn ended meanwhile
                              (up
                               (efrit-reconnect--note session-id "the endpoint answers again; retrying the request" 'success)
                               (efrit-publish 'reconnect-retry `((:session-id . ,session-id)
                                                                 (:attempt . ,(efrit-reconnect-attempts session-id))))
                               (when-let* ((retry (efrit-reconnect--get session-id :retry)))
                                 (funcall retry)))
                              (slow
                               (efrit-reconnect--note session-id
                                                      (format "still no answer from the endpoint; checking again in %ss"
                                                              efrit-reconnect-wait-poll-seconds) 'shadow)
                               (efrit-reconnect--schedule-probe session-id efrit-reconnect-wait-poll-seconds t))
                              (t
                               ;; count the dead probe as a failed attempt so the
                               ;; budget runs down even when nothing is sent
                               (efrit-reconnect-handle-failure
                                session-id "No response from the endpoint (probe)"
                                (efrit-reconnect--get session-id :retry)
                                (efrit-reconnect--get session-id :ask))))))))))

;;;; The user's answer

(defconst efrit-reconnect-question-options
  '("Keep waiting and resume when it is back" "Retry now" "Abort the turn")
  "What the user is offered when the retry budget is spent.")

(defun efrit-reconnect-answer (session-id answer)
  "Act on the user's ANSWER to the connectivity question for SESSION-ID.
Returns `wait', `retry' or `abort'.  For `wait' and `retry' the turn
resumes from here; the caller must not start a new turn with the answer."
  (let ((a (downcase (string-trim (or answer "")))))
    (cond
     ((or (string-prefix-p "keep" a) (string-prefix-p "wait" a))
      (efrit-reconnect--set session-id :waiting t)
      (efrit-reconnect--note session-id
                             (format "waiting for the connection; checking every %ss, the turn resumes by itself"
                                     efrit-reconnect-wait-poll-seconds))
      (efrit-reconnect--schedule-probe session-id (min 1 efrit-reconnect-wait-poll-seconds) t)
      'wait)
     ((string-prefix-p "retry" a)
      (efrit-reconnect--set session-id :attempts 0)
      (when-let* ((retry (efrit-reconnect--get session-id :retry)))
        (run-at-time 0 nil retry))
      'retry)
     (t
      (efrit-reconnect-reset session-id)
      'abort))))

(provide 'efrit-reconnect)

;;; efrit-reconnect.el ends here
