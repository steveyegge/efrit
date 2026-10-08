;;; efrit-throttle.el --- Debounce and rate-limit automatic work -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.10.3
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools

;;; Commentary:

;; Anything in efrit that runs by itself (not from a command) goes
;; through a throttle: a debounce (wait until things are quiet), a
;; minimum interval between runs (cost control when the run calls the
;; API), a "not right after one of our own commands" rule (accepting
;; a result must not trigger the next request), and a block predicate
;; hook.  After minuet's `minuet--maybe-show-suggestion' (2026-09-28).
;;
;; A throttle is a struct made with `efrit-throttle-create'; call
;; `efrit-throttle-request' each time the trigger fires.  The function
;; runs at most once per quiet period and never more often than the
;; interval.

;;; Code:

(require 'cl-lib)

(cl-defstruct (efrit-throttle (:constructor efrit-throttle--make))
  name
  function            ; called with no arguments when the throttle fires
  (debounce 0.5)      ; seconds of quiet before firing
  (interval 1.0)      ; least seconds between two firings
  (own-command-prefix nil) ; a string: skip when `this-command' starts with it
  (block-functions nil)    ; functions; any returning non-nil blocks the run
  (timer nil)
  (last-run 0.0)
  (runs 0)
  (skipped 0))

(cl-defun efrit-throttle-create (name function &key (debounce 0.5) (interval 1.0)
                                      own-command-prefix block-functions)
  "A throttle NAME around FUNCTION.  See the struct for the keys."
  (efrit-throttle--make :name name :function function :debounce debounce
                        :interval interval :own-command-prefix own-command-prefix
                        :block-functions block-functions))

(defun efrit-throttle--blocked-p (th)
  "Why TH must not run now, as a symbol, or nil."
  (cond
   ((and (efrit-throttle-own-command-prefix th)
         (symbolp this-command) this-command
         (string-prefix-p (efrit-throttle-own-command-prefix th) (symbol-name this-command)))
    'own-command)
   ((cl-some (lambda (f) (funcall f)) (efrit-throttle-block-functions th)) 'blocked)))

(defun efrit-throttle-request (th &rest args)
  "Ask TH to run its function with ARGS once things are quiet.
Every call resets the debounce.  When the debounce ends, the function
runs unless it ran less than the interval ago (then the run waits for
the interval) or a block applies (then the request is dropped).
Returns the symbol naming why it was dropped, else nil."
  (if-let* ((why (efrit-throttle--blocked-p th)))
      (progn (cl-incf (efrit-throttle-skipped th)) why)
    (when (efrit-throttle-timer th)
      (cancel-timer (efrit-throttle-timer th)))
    (let* ((since (- (float-time) (efrit-throttle-last-run th)))
           (wait (max (efrit-throttle-debounce th)
                      (- (efrit-throttle-interval th) since))))
      ;; a plain timer restarted on every request is the debounce;
      ;; idle timers would not fire while a command loop is busy
      (setf (efrit-throttle-timer th)
            (run-at-time wait nil #'efrit-throttle--fire th args)))
    nil))

(defun efrit-throttle--fire (th args)
  (setf (efrit-throttle-timer th) nil
        (efrit-throttle-last-run th) (float-time))
  (cl-incf (efrit-throttle-runs th))
  (condition-case err
      (apply (efrit-throttle-function th) args)
    (error (when (fboundp 'efrit-log)
             (efrit-log 'warn "throttle %s: %s" (efrit-throttle-name th) (error-message-string err))))))

(defun efrit-throttle-cancel (th)
  "Drop TH's pending run, if any."
  (when (efrit-throttle-timer th)
    (cancel-timer (efrit-throttle-timer th))
    (setf (efrit-throttle-timer th) nil)))

(provide 'efrit-throttle)

;;; efrit-throttle.el ends here
