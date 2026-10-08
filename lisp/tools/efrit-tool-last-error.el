;;; efrit-tool-last-error.el --- What just went wrong in this Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.10.3
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, ai

;;; Commentary:

;; "Why did my last command fail?" needs the error, and the error is
;; gone by the time the user asks.  `efrit-last-error-mode' keeps it:
;; `command-error-default-function' is advised to record every
;; command error with a summary of the backtrace frames, the command,
;; the buffer and the time.  The `get_last_error' tool returns the
;; recent ones, newest first, so the model can answer without a
;; reproduction (after ai-code-interface's debug tools, 2026-09-28).
;;
;; `efrit-eval-observe' is the other half: around an evaluation it
;; collects the new `*Messages*' lines and the buffers whose text
;; changed, for `eval_sexp' to report alongside the value.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'efrit-tool-utils)

(defgroup efrit-last-error nil
  "Recorded command errors for the model."
  :group 'efrit)

(defcustom efrit-last-error-keep 10
  "How many command errors are kept."
  :type 'integer
  :group 'efrit-last-error)

(defcustom efrit-last-error-frames 12
  "Backtrace frames summarised per error."
  :type 'integer
  :group 'efrit-last-error)

(defvar efrit-last-error--ring nil
  "Recorded errors, newest first: plists (:time :command :buffer :error :frames).")

(defun efrit-last-error--frame-summary ()
  "The innermost frames of the current backtrace, one line each.
Frames from this recorder and from the error machinery are skipped."
  (let ((out nil) (n 0))
    (mapbacktrace
     (lambda (evald func args _flags)
       (when (and (< n efrit-last-error-frames)
                  (symbolp func)
                  (not (memq func '(efrit-last-error--record efrit-last-error--frame-summary
                                    mapbacktrace command-error-default-function
                                    signal error user-error apply funcall))))
         (cl-incf n)
         (push (format "%s%s" (symbol-name func)
                       (if (and evald args)
                           (format " %s" (truncate-string-to-width (format "%S" args) 80 nil nil "…"))
                         ""))
               out))))
    (nreverse out)))

(defun efrit-last-error--record (data context caller)
  "Around `command-error-default-function': keep the error, then behave as before."
  (condition-case nil
      (push (list :time (current-time)
                  :command (format "%S" (or caller this-command))
                  :buffer (buffer-name)
                  :error (error-message-string data)
                  :condition (format "%S" (car-safe data))
                  :context (and context (format "%s" context))
                  :frames (efrit-last-error--frame-summary))
            efrit-last-error--ring)
    (error nil))
  (when (> (length efrit-last-error--ring) efrit-last-error-keep)
    (setcdr (nthcdr (1- efrit-last-error-keep) efrit-last-error--ring) nil)))

;;;###autoload
(define-minor-mode efrit-last-error-mode
  "Keep the recent command errors with their backtraces for `get_last_error'."
  :global t
  :group 'efrit-last-error
  (if efrit-last-error-mode
      (advice-add 'command-error-default-function :before #'efrit-last-error--record)
    (advice-remove 'command-error-default-function #'efrit-last-error--record)))

(defun efrit-last-error-list ()
  "The recorded errors, newest first."
  efrit-last-error--ring)

(defun efrit-tool-get-last-error (args)
  "The recent command errors of this Emacs, newest first.
ARGS: count (default 1, most `efrit-last-error-keep')."
  (efrit-tool-execute get_last_error args
    (let* ((count (or (alist-get 'count args) 1))
           (errors (seq-take efrit-last-error--ring count)))
      (efrit-tool-success
       `((recording . ,(if efrit-last-error-mode t :json-false))
         (count . ,(length errors))
         (errors . ,(vconcat
                     (mapcar (lambda (e)
                               `((when . ,(format-time-string "%H:%M:%S" (plist-get e :time)))
                                 (ago_seconds . ,(round (float-time (time-since (plist-get e :time)))))
                                 (command . ,(plist-get e :command))
                                 (buffer . ,(plist-get e :buffer))
                                 (error . ,(plist-get e :error))
                                 (condition . ,(plist-get e :condition))
                                 (frames . ,(vconcat (plist-get e :frames)))))
                             errors)))
         ,@(unless efrit-last-error-mode
             '((note . "efrit-last-error-mode is off: only errors since it was last on are here; suggest the user turn it on"))))))))

;;;; Observing an evaluation

(defun efrit-eval-observe (thunk)
  "Call THUNK; return (VALUE MESSAGES CHANGED-BUFFERS).
MESSAGES are the `*Messages*' lines written during the call,
CHANGED-BUFFERS the names of buffers whose text changed."
  (let* ((msgbuf (messages-buffer))
         (msg-start (with-current-buffer msgbuf (point-max)))
         (ticks (mapcar (lambda (b) (cons b (buffer-chars-modified-tick b))) (buffer-list)))
         (value (funcall thunk))
         (messages (with-current-buffer msgbuf
                     (split-string (buffer-substring-no-properties (min msg-start (point-max)) (point-max))
                                   "\n" t)))
         (changed (cl-loop for (b . tick) in ticks
                           when (and (buffer-live-p b)
                                     (/= tick (buffer-chars-modified-tick b))
                                     (not (eq b msgbuf))
                                     (not (string-prefix-p " " (buffer-name b))))
                           collect (buffer-name b))))
    (list value messages changed)))

(provide 'efrit-tool-last-error)

;;; efrit-tool-last-error.el ends here
