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

(defun efrit-publish (type &optional data)
  "Publish an event of TYPE with DATA (an alist of :keyword . value).
:type and :time are added.  Subscribers to TYPE and to `t' are called
in registration order; errors are logged and swallowed.  Returns the
event alist."
  (let ((event (append `((:type . ,type) (:time . ,(current-time))) data)))
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

(defvar efrit-events--idle-timer nil)

(defun efrit-events--track-idle (event)
  "Arm or cancel the idle timer based on EVENT."
  (when (timerp efrit-events--idle-timer)
    (cancel-timer efrit-events--idle-timer)
    (setq efrit-events--idle-timer nil))
  (when (and efrit-idle-delay
             (memq (alist-get :type event) '(turn-complete permission)))
    (setq efrit-events--idle-timer
          (run-with-timer efrit-idle-delay nil
                          (lambda ()
                            (setq efrit-events--idle-timer nil)
                            (efrit-publish
                             'idle
                             `((:session-id . ,(alist-get :session-id event))
                               (:idle-event . ,(alist-get :type event)))))))))

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
