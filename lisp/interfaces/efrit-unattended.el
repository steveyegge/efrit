;;; efrit-unattended.el --- Keep working while the user is away -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.8.5
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, ai, convenience

;;; Commentary:

;; A turn that reaches a modal prompt (sandbox menu, limits menu, the
;; reviewer's escalation, a diff preview) waits for the user.  Away
;; for lunch, that is an afternoon lost (tzz, 2026-10-01: "what can I
;; do here to make it less of a problem if I walk away").
;;
;; `efrit-unattended-mode' answers those prompts by policy instead:
;;
;;   sandbox request   denied -- the model is told and carries on; the
;;                     expected ones never reach a prompt anyway
;;   limit reached     raised once, up to `efrit-unattended-max-raises'
;;                     per limit per turn, then stopped
;;   review escalation drop the action, continue the task
;;   diff preview      rejected (nothing is applied unseen)
;;
;; Every answer given this way is noted in the transcript at the time
;; and listed again in one summary note when the turn ends, so the
;; decisions can be reviewed afterwards.  Nothing destructive is ever
;; allowed by policy: the mode only ever denies, drops, or extends.
;;
;; `efrit-notify' now also announces every prompt that does wait, so
;; with the mode off a desktop notification says efrit is blocked.

;;; Code:

(require 'cl-lib)
(require 'efrit-events)

(defgroup efrit-unattended nil
  "Answering efrit's prompts by policy while the user is away."
  :group 'efrit)

(defcustom efrit-unattended-max-raises 2
  "How many times a limit is raised unattended in one turn before the turn stops."
  :type 'integer
  :group 'efrit-unattended)

(defvar efrit-unattended--answers nil
  "Answers given this turn, newest first: plists (:label :answer :why :time).")

(defvar efrit-unattended--raises (make-hash-table :test 'equal)
  "Label -> raises given this turn.")

(defun efrit-unattended-answer (label default)
  "The unattended answer for the prompt LABEL, or nil to let it open.
Returns (ANSWER . WHY).  DEFAULT is what a refused prompt returns."
  (cond
   ;; limits: "the max-iterations limit prompt"
   ((string-match "\\`the \\([a-z-]+\\) limit prompt\\'" label)
    (let ((n (gethash label efrit-unattended--raises 0)))
      (if (< n efrit-unattended-max-raises)
          (progn (puthash label (1+ n) efrit-unattended--raises)
                 (cons 'once (format "unattended: raised once (%d of %d)" (1+ n) efrit-unattended-max-raises)))
        (cons nil (format "unattended: not raised again after %d raise(s); the turn stops" n)))))
   ;; sandbox: "<tool>'s sandbox request"
   ((string-suffix-p "sandbox request" label)
    (cons nil "unattended: denied; the model was told and continues"))
   ;; the diff preview
   ((equal label "the diff preview")
    (cons default "unattended: not applied unseen"))
   (t (cons default "unattended: refused by default"))))

(defun efrit-unattended--record (event)
  "Keep the unattended answer in EVENT for the end-of-turn summary."
  (push (list :label (alist-get :label event) :answer (alist-get :answer event)
              :why (alist-get :why event) :time (current-time))
        efrit-unattended--answers)
  (efrit-publish 'note `((:text . ,(format "⛨ %s: %s" (alist-get :label event) (alist-get :why event)))
                         (:face . warning) (:kind . unattended))))

(defun efrit-unattended--on-turn-start (_event)
  (setq efrit-unattended--answers nil)
  (clrhash efrit-unattended--raises))

(defun efrit-unattended--on-turn-complete (event)
  "One summary note of what was answered unattended this turn."
  (when efrit-unattended--answers
    (let ((n (length efrit-unattended--answers)))
      (efrit-publish 'note
                     `((:session-id . ,(alist-get :session-id event))
                       (:text . ,(format "⛨ unattended: %d prompt(s) answered by policy this turn:\n%s"
                                         n
                                         (mapconcat (lambda (a)
                                                      (format "    %s  %s — %s"
                                                              (format-time-string "%H:%M:%S" (plist-get a :time))
                                                              (plist-get a :label) (plist-get a :why)))
                                                    (reverse efrit-unattended--answers) "\n")))
                       (:face . warning) (:kind . unattended))))
    (setq efrit-unattended--answers nil)))

;;;###autoload
(define-minor-mode efrit-unattended-mode
  "Answer efrit's prompts by policy so a turn never waits for an absent user.
Denies unusual sandbox requests, raises limits a bounded number of
times, drops actions the reviewer keeps rejecting, rejects diff
previews.  Each answer is noted; the turn's end carries a summary."
  :global t
  :group 'efrit-unattended
  :lighter " efrit-away"
  (if efrit-unattended-mode
      (progn
        (setq efrit-prompt-policy-function #'efrit-unattended-answer)
        (efrit-subscribe 'prompt-answered-unattended #'efrit-unattended--record)
        (efrit-subscribe 'turn-start #'efrit-unattended--on-turn-start)
        (efrit-subscribe 'turn-complete #'efrit-unattended--on-turn-complete)
        (message "efrit: unattended — prompts are answered by policy (deny / raise / drop); the transcript lists each"))
    (when (eq efrit-prompt-policy-function #'efrit-unattended-answer)
      (setq efrit-prompt-policy-function nil))
    (efrit-unsubscribe 'prompt-answered-unattended #'efrit-unattended--record)
    (efrit-unsubscribe 'turn-start #'efrit-unattended--on-turn-start)
    (efrit-unsubscribe 'turn-complete #'efrit-unattended--on-turn-complete)
    (message "efrit: attended again")))

(provide 'efrit-unattended)

;;; efrit-unattended.el ends here
