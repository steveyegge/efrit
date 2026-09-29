;;; efrit-events.el --- Publish/subscribe bus for session events -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; One public hook instead of advice.  `efrit-agent-integration.el'
;; wires the agent buffer to the loop with nine `advice-add's on
;; progress and session functions, the REPL adapter passes no
;; `event-fn' at all, and `efrit-event.el' (singular) is only a
;; formatter for the progress buffer.  Anyone wanting a desktop
;; notification when a turn finishes, or a mode-line token count, has
;; nowhere to hook in.
;;
;; This module is deliberately tiny (after agent-shell's
;; `agent-shell-subscribe-to'):
;;
;;   (efrit-subscribe 'turn-complete
;;                    (lambda (ev) (notify (alist-get :session-id ev))))
;;   (efrit-publish 'turn-complete `((:session-id . ,id) (:stop-reason . ,r)))
;;
;; Every subscriber runs inside `condition-case': a broken subscriber
;; is logged and cannot take the loop down.  Subscribing to `t' gets
;; every event.
;;
;; Event vocabulary (all carry :session-id and :time):
;;
;;   turn-start        :input
;;   api-request       :iteration
;;   api-response      :usage (hash: input_tokens output_tokens
;;                             cache_read_input_tokens
;;                             cache_creation_input_tokens) :stop-reason
;;   tool-start        :tool :input
;;   tool-result       :tool :result :success :elapsed
;;   permission        :tool :decision
;;   steer             :text -- the user spoke to a running turn; the
;;                     REPL loop stores the text and delivers it with
;;                     the next tool results (`efrit-repl-loop--steer')
;;   steered           :text -- the text was delivered to the model
;;   queued            :text :count -- an input waits for the turn to end
;;   turn-complete     :stop-reason :completion-message
;;   idle              :idle-event (the event that started the idle
;;                     timer, e.g. turn-complete) -- fires after
;;                     `efrit-idle-delay' seconds with no new activity,
;;                     the hook for "ping me when it's done and I've
;;                     wandered off".

;;; Code:

(require 'cl-lib)
(require 'efrit-log)

(defgroup efrit-events nil
  "Session event bus."
  :group 'efrit
  :prefix "efrit-events-")

(defcustom efrit-idle-delay 30
  "Seconds after `turn-complete' with no activity before `idle' is published.
nil disables the idle event."
  :type '(choice (const nil) number)
  :group 'efrit-events)

(defvar efrit-events--subscribers nil
  "Alist of (EVENT-TYPE . (FN ...)).  EVENT-TYPE `t' means all events.")

(defvar efrit-current-session-id nil
  "The session on whose behalf the current code runs, or nil.
The loops bind it around tool dispatch and API callbacks
\(`efrit-with-session').  `efrit-publish' stamps it on an event that
carries no :session-id of its own (sandbox and limits notes, todo
changes), so subscribers can route to the right agent buffer when
several sessions run.")

(defvar efrit-session-local-variables nil
  "Variables that hold per-turn state and must be kept per session.
Each entry is (SYMBOL . INITIAL-VALUE).  `efrit-with-session' sets
every one of them from the session's store on entry (INITIAL-VALUE
for a new session) and saves them back on exit, so the code that uses
them with plain `setq' stays as it is while two sessions no longer
share counters or standing answers.  Modules register theirs with
`efrit-session-local'.")

(defvar efrit-session--locals (make-hash-table :test 'equal)
  "Session id -> alist (SYMBOL . VALUE) of its session-local variables.")

(defun efrit-session-local (&rest symbols)
  "Declare SYMBOLS session-local (see `efrit-session-local-variables').
Their value at this call (normally the defvar's) is what a new
session starts with."
  (dolist (sym symbols)
    (unless (assq sym efrit-session-local-variables)
      (push (cons sym (and (boundp sym) (symbol-value sym)))
            efrit-session-local-variables))))

(defun efrit-session-local-forget (session-id)
  "Drop SESSION-ID's stored session-local values."
  (remhash session-id efrit-session--locals))

(defun efrit-session--locals-enter (session-id)
  "Set every session-local variable from SESSION-ID's store.
Returns the values the variables had, to restore on exit."
  (let ((stored (gethash session-id efrit-session--locals))
        (saved nil))
    (pcase-dolist (`(,sym . ,initial) efrit-session-local-variables)
      (when (boundp sym)
        (push (cons sym (symbol-value sym)) saved)
        (let ((cell (assq sym stored)))
          (set sym (if cell (cdr cell) initial)))))
    saved))

(defun efrit-session--locals-exit (session-id saved)
  "Store the session-local variables for SESSION-ID and restore SAVED."
  (let ((stored nil))
    (pcase-dolist (`(,sym . ,_) efrit-session-local-variables)
      (when (boundp sym)
        (push (cons sym (symbol-value sym)) stored)))
    (puthash session-id stored efrit-session--locals))
  (dolist (cell saved) (set (car cell) (cdr cell))))

(defmacro efrit-with-session (session-id &rest body)
  "Run BODY as SESSION-ID's code.
`efrit-current-session-id' is bound to it and the session-local
variables hold its values (`efrit-session-local-variables').
Re-entrant: nested with the same id it does nothing extra; nested
with another id (one session's tools running inside another
session's prompt) it swaps the values in and back out."
  (declare (indent 1))
  (let ((id (make-symbol "id")) (saved (make-symbol "saved")))
    `(let* ((,id ,session-id)
            (efrit-current-session-id ,id)
            (,saved (and ,id (efrit-session--locals-enter ,id))))
       (unwind-protect
           (progn ,@body)
         (when ,id (efrit-session--locals-exit ,id ,saved))))))

(defun efrit-subscribe (type fn)
  "Call FN with an event alist whenever an event of TYPE is published.
TYPE is a symbol from the vocabulary in the Commentary, or `t' for
every event.  FN is added at most once per TYPE.  Returns FN."
  (let ((cell (assq type efrit-events--subscribers)))
    (if cell
        (unless (memq fn (cdr cell))
          ;; Append so subscribers run in registration order
          (setcdr cell (append (cdr cell) (list fn))))
      (push (cons type (list fn)) efrit-events--subscribers)))
  fn)

(defun efrit-unsubscribe (type fn)
  "Stop calling FN for events of TYPE."
  (when-let* ((cell (assq type efrit-events--subscribers)))
    (setcdr cell (delq fn (cdr cell)))))

(defun efrit-events--brief (data)
  "DATA for the debug log: keys with short values, long strings cut."
  (mapconcat (lambda (cell)
               (let ((v (cdr cell)))
                 (format "%s=%s" (car cell)
                         (cond ((stringp v)
                                (let ((one (replace-regexp-in-string "\n" "⏎" v)))
                                  (if (> (length one) 60) (concat (substring one 0 60) "…") one)))
                               ((hash-table-p v) (format "#<hash %d>" (hash-table-count v)))
                               ((bufferp v) (buffer-name v))
                               ((and (consp v) (proper-list-p v) (> (length v) 5))
                                (format "(%d items)" (length v)))
                               (t (let ((s (format "%S" v)))
                                    (if (> (length s) 60) (concat (substring s 0 60) "…") s)))))))
             data " "))

;;; Time spent waiting on the user
;;
;; Every prompt (sandbox, permission, limits, confirm_action, the
;; diff preview, a question) goes through `efrit-with-user-waiting'.
;; The time inside is recorded, and every deadline that measures a
;; turn or a tool subtracts it: reading a prompt for ten minutes must
;; not time out the turn, or the tool, the moment it is answered
;; (2026-09-27).  Tool `with-timeout's are suspended as well, the way
;; the debugger does it.

(defvar efrit-user-waiting-seconds 0
  "Seconds spent inside `efrit-with-user-waiting' so far in this Emacs.
Clocks take a reading at their start and subtract the difference.")

(defvar efrit-user-waiting-depth 0
  "How many `efrit-with-user-waiting' forms are active.")

;; Waiting is booked to the session whose prompt it was: a prompt of
;; A must not shorten B's turn clock.  Outside any session the plain
;; global value is used.
(efrit-session-local 'efrit-user-waiting-seconds 'efrit-user-waiting-depth)

(defmacro efrit-with-user-waiting (&rest body)
  "Run BODY, a prompt to the user, with efrit's clocks paused.
Adds the time BODY takes to `efrit-user-waiting-seconds' and suspends
any enclosing `with-timeout'.  Nested uses count the time once."
  (declare (indent 0) (debug t))
  (let ((start (make-symbol "start")) (suspended (make-symbol "suspended")))
    `(let ((,start (float-time))
           (,suspended (with-timeout-suspend)))
       (cl-incf efrit-user-waiting-depth)
       (unwind-protect
           (progn ,@body)
         (cl-decf efrit-user-waiting-depth)
         (when (zerop efrit-user-waiting-depth)
           (cl-incf efrit-user-waiting-seconds (- (float-time) ,start)))
         (with-timeout-unsuspend ,suspended)))))

;;; One modal prompt at a time
;;
;; The sandbox and the limits ask the user through a menu over a
;; `recursive-edit'.  Process sentinels keep running inside it, so a
;; second session's tool can want its own prompt while the first is
;; still up; the two would share the menu's state, and the second
;; cannot wait: it runs inside the first's command loop, and blocking
;; there would stop the first prompt from being answered at all.  So
;; the second request is not asked: `efrit-with-prompt-turn' returns
;; DEFAULT for it (a denial) and the asking session is told.  The
;; model gets the usual denied result and can retry.

(defvar efrit-prompt--owner nil
  "The session id (or t) whose modal prompt is up, or nil.")

(defmacro efrit-with-prompt-turn (label default &rest body)
  "Run BODY, a modal prompt, unless another session's prompt is up.
Then BODY is skipped, DEFAULT is returned, and a `note' is published
for the asking session saying that LABEL was refused because another
prompt is open.  Nested prompts of the same session run at once (a
prompt that asks another question, the details popup)."
  (declare (indent 2))
  (let ((me (make-symbol "me")))
    `(let ((,me (or efrit-current-session-id t)))
       (if (and efrit-prompt--owner (not (equal efrit-prompt--owner ,me)))
           (progn
             (efrit-log 'warn "prompt for %s refused: another session's prompt is open" ,label)
             (efrit-publish 'note
                            (list (cons :text (format "⛨ not asked: %s came while another session's prompt was open; denied, the model can retry" ,label))
                                  (cons :face 'warning) (cons :kind 'sandbox)))
             ,default)
         (let ((outer efrit-prompt--owner))
           (setq efrit-prompt--owner ,me)
           (unwind-protect (progn ,@body)
             (setq efrit-prompt--owner outer)))))))

(defun efrit-elapsed-working (since &optional waiting-at-start)
  "Seconds since SINCE (a time value) minus time spent waiting on the user.
WAITING-AT-START is `efrit-user-waiting-seconds' when the clock
started; nil means all waiting so far is subtracted, which is right
for a clock that started before any prompt of its own."
  (max 0 (- (float-time (time-since since))
            (- efrit-user-waiting-seconds (or waiting-at-start 0)))))

(defun efrit-publish (type &optional data)
  "Publish an event of TYPE with DATA (an alist of :keyword . value).
:type and :time are added.  Subscribers to TYPE and to `t' are called
in registration order; errors are logged and swallowed.  Returns the
event alist."
  (let ((event (append `((:type . ,type) (:time . ,(current-time)))
                       (if (or (assq :session-id data) (null efrit-current-session-id))
                           data
                         (cons (cons :session-id efrit-current-session-id) data)))))
    (efrit-log 'debug "event %s %s" type (efrit-events--brief data))
    (dolist (fn (append (cdr (assq type efrit-events--subscribers))
                        (cdr (assq t efrit-events--subscribers))))
      (condition-case err
          (funcall fn event)
        (error
         (efrit-log 'warn "efrit event subscriber %S failed on %s: %s"
                    fn type (error-message-string err)))))
    (efrit-events--track-idle event)
    event))

;;; Idle timer

(defvar efrit-events--idle-timers (make-hash-table :test 'equal)
  "Session id -> the timer that will publish its `idle' event.")

(defun efrit-events--track-idle (event)
  "Arm or cancel the idle timer of EVENT's session based on EVENT.
One timer per session: another session's traffic does not suppress
this one's idle event."
  (let ((id (alist-get :session-id event)))
    (when-let* ((timer (gethash id efrit-events--idle-timers)))
      (when (timerp timer) (cancel-timer timer))
      (remhash id efrit-events--idle-timers))
    (when (and efrit-idle-delay
               (memq (alist-get :type event) '(turn-complete permission)))
      (puthash id
               (run-with-timer efrit-idle-delay nil
                               (lambda ()
                                 (remhash id efrit-events--idle-timers)
                                 (efrit-publish
                                  'idle
                                  `((:session-id . ,id)
                                    (:idle-event . ,(alist-get :type event))))))
               efrit-events--idle-timers))))

;;; Convenience: notify when the agent buffer isn't visible

(defun efrit-events-notify-when-hidden (event)
  "Example subscriber: `message' about EVENT if no efrit buffer is visible.
Add with (efrit-subscribe \\='turn-complete #\\='efrit-events-notify-when-hidden).
Replace `message' with `notifications-notify' or `alert' to taste."
  (unless (cl-some (lambda (w)
                     (with-current-buffer (window-buffer w)
                       (derived-mode-p 'efrit-agent-mode)))
                   (window-list nil 'no-minibuf))
    (message "Efrit: turn finished (%s)"
             (or (alist-get :stop-reason event) (alist-get :type event)))))

(provide 'efrit-events)

;;; efrit-events.el ends here
