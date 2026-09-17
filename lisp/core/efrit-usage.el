;;; efrit-usage.el --- Token usage tracking from API responses -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; Subscribes to `api-response' events and keeps, per session, the
;; last request's input size (which is the live context-window
;; occupancy) and cumulative totals.  `efrit-usage-indicator' renders
;; a compact string for the header-line, in the style of agent-shell's
;; usage segment: "29k/200k ▃", coloured by fill level, with the full
;; breakdown as help-echo.
;;
;; The numbers come from the API's own usage block, so they are exact
;; where `efrit-budget' estimates.

;;; Code:

(require 'cl-lib)
(require 'efrit-events)

(defgroup efrit-usage nil
  "Token usage display."
  :group 'efrit
  :prefix "efrit-usage-")

(defcustom efrit-usage-context-window 200000
  "Context window size assumed for the fill indicator."
  :type 'integer
  :group 'efrit-usage)

(defcustom efrit-usage-warn-fraction 0.6
  "Fill fraction at which the indicator turns to the warning face."
  :type 'number
  :group 'efrit-usage)

(defcustom efrit-usage-danger-fraction 0.85
  "Fill fraction at which the indicator turns to the error face."
  :type 'number
  :group 'efrit-usage)

(cl-defstruct (efrit-usage (:constructor efrit-usage--make))
  (context 0)          ; input tokens of the most recent request (live occupancy)
  (input 0)            ; cumulative uncached input
  (output 0)           ; cumulative output
  (cache-read 0)       ; cumulative cache reads
  (cache-write 0)      ; cumulative cache writes
  (requests 0))

(defvar efrit-usage--by-session (make-hash-table :test 'equal)
  "Session-id -> `efrit-usage'.")

(defun efrit-usage-for (session-id)
  "Return the `efrit-usage' record for SESSION-ID, or nil."
  (gethash session-id efrit-usage--by-session))

(defun efrit-usage-reset (&optional session-id)
  "Forget usage for SESSION-ID, or everything when nil."
  (if session-id
      (remhash session-id efrit-usage--by-session)
    (clrhash efrit-usage--by-session)))

(defun efrit-usage--n (table key)
  (or (and (hash-table-p table) (gethash key table)) 0))

(defun efrit-usage--on-api-response (event)
  "Subscriber: fold EVENT's :usage into the session's record."
  (when-let* ((usage (alist-get :usage event))
              (id (alist-get :session-id event)))
    (let* ((u (or (gethash id efrit-usage--by-session)
                  (puthash id (efrit-usage--make) efrit-usage--by-session)))
           (in (efrit-usage--n usage "input_tokens"))
           (out (efrit-usage--n usage "output_tokens"))
           (cr (efrit-usage--n usage "cache_read_input_tokens"))
           (cw (efrit-usage--n usage "cache_creation_input_tokens")))
      ;; Everything the model read this request = live context size
      (setf (efrit-usage-context u) (+ in cr cw))
      (cl-incf (efrit-usage-input u) in)
      (cl-incf (efrit-usage-output u) out)
      (cl-incf (efrit-usage-cache-read u) cr)
      (cl-incf (efrit-usage-cache-write u) cw)
      (cl-incf (efrit-usage-requests u))
      ;; Header-lines using `efrit-usage-indicator' pick this up on
      ;; the next redisplay; make sure one happens.
      (force-mode-line-update t))))

(efrit-subscribe 'api-response #'efrit-usage--on-api-response)

;;; Rendering

(defun efrit-usage-compact-number (n)
  "Format N as 1.2k / 3m, like agent-shell."
  (cond ((>= n 1000000) (format "%.1fm" (/ n 1000000.0)))
        ((>= n 10000) (format "%dk" (round (/ n 1000.0))))
        ((>= n 1000) (format "%.1fk" (/ n 1000.0)))
        (t (number-to-string n))))

(defconst efrit-usage--bars ["▁" "▂" "▃" "▄" "▅" "▆" "▇" "█"])

(defun efrit-usage-indicator (session-id)
  "Return a propertized header-line segment for SESSION-ID, or nil."
  (when-let* ((u (efrit-usage-for session-id)))
    (let* ((ctx (efrit-usage-context u))
           (frac (min 1.0 (/ ctx (float efrit-usage-context-window))))
           (bar (aref efrit-usage--bars
                      (min 7 (floor (* frac 8)))))
           (face (cond ((>= frac efrit-usage-danger-fraction) 'error)
                       ((>= frac efrit-usage-warn-fraction) 'warning)
                       (t 'success)))
           (help (format "Context: %d of %d tokens (%.0f%%)\nRequests: %d\nInput: %d  Output: %d\nCache read: %d  Cache write: %d"
                         ctx efrit-usage-context-window (* 100 frac)
                         (efrit-usage-requests u)
                         (efrit-usage-input u) (efrit-usage-output u)
                         (efrit-usage-cache-read u) (efrit-usage-cache-write u))))
      (propertize (format "%s/%s %s"
                          (efrit-usage-compact-number ctx)
                          (efrit-usage-compact-number efrit-usage-context-window)
                          bar)
                  'face face 'help-echo help))))

(provide 'efrit-usage)

;;; efrit-usage.el ends here
