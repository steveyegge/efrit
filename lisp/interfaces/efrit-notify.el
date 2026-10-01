;;; efrit-notify.el --- Tell the user a slow turn ended while they were elsewhere -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.5.3
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, ai, convenience

;;; Commentary:

;; Off by default (`efrit-notify-enabled').  When on, a turn that took
;; at least `efrit-notify-min-seconds' and ended while the agent buffer
;; was not in the selected window produces a desktop notification: a
;; 115-article mail analysis finishes while you read something else.
;;
;; The notification goes through John Wiegley's `alert' when it is
;; installed, category `efrit', so the user's own `alert-add-rule'
;; routing (macOS notifier, growl, libnotify, mode line...) decides
;; how it shows.  Without alert: `notifications-notify' where D-Bus
;; is available (Linux desktops), else `message'.  Nothing here shells
;; out; alert owns its osascript style.

;;; Code:

(require 'efrit-events)
(require 'efrit-repl-session)

(defgroup efrit-notify nil
  "Desktop notifications when a turn ends unwatched."
  :group 'efrit)

(defcustom efrit-notify-enabled nil
  "Non-nil notifies when a slow turn ends while the agent buffer is not selected."
  :type 'boolean
  :group 'efrit-notify)

(defcustom efrit-notify-min-seconds 20
  "Turns shorter than this never notify: you were probably still watching."
  :type 'integer
  :group 'efrit-notify)

(defcustom efrit-notify-function #'efrit-notify-default
  "Function called with TITLE and BODY to show the notification."
  :type 'function
  :group 'efrit-notify)

(declare-function alert "alert")
(declare-function notifications-notify "notifications")

(defun efrit-notify-default (title body)
  "Show TITLE and BODY: through `alert' if present, else D-Bus, else the echo area."
  (cond
   ((require 'alert nil t)
    (alert body :title title :category 'efrit :severity 'normal))
   ((and (featurep 'dbusbind) (require 'notifications nil t)
         (condition-case nil (notifications-notify :title title :body body) (error nil))))
   (t (message "%s: %s" title body))))

(defun efrit-notify--unwatched-p (buffer)
  "Non-nil when BUFFER is not what the user is looking at."
  (not (and (buffer-live-p buffer)
            (eq buffer (window-buffer (selected-window)))
            (frame-focus-state))))

(defun efrit-notify--on-turn-complete (event)
  (when efrit-notify-enabled
    (when-let* ((id (alist-get :session-id event))
                (session (efrit-repl-session-get id))
                (start (efrit-repl-session-current-turn-start session)))
      (let ((secs (float-time (time-since start)))
            (buffer (efrit-repl-session-buffer session))
            (reason (alist-get :stop-reason event)))
        (when (and (>= secs efrit-notify-min-seconds)
                   (efrit-notify--unwatched-p buffer))
          (funcall efrit-notify-function
                   "efrit"
                   (format "%s after %d s%s"
                           (pcase reason
                             ((or "end_turn" "session-complete" "unknown") "Turn finished")
                             ("waiting-for-user" "efrit has a question")
                             ("interrupted" "Turn interrupted")
                             (_ (format "Turn ended (%s)" reason)))
                           (round secs)
                           (if (and buffer (buffer-live-p buffer))
                               (format " in %s" (buffer-name buffer)) ""))))))))

(efrit-subscribe 'turn-complete #'efrit-notify--on-turn-complete)

(provide 'efrit-notify)

;;; efrit-notify.el ends here
