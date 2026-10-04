;;; efrit-transcript.el --- A readable log of each session, written as it goes -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.9.2
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; The session store (`efrit-session-persist') keeps API messages as
;; JSON so a session can resume.  That is not something to read or
;; send to a colleague.  This file writes a Markdown transcript of
;; every REPL session as the turn goes: the user's input, the model's
;; answer, each tool call with a clipped result, questions, errors,
;; steering.  One file per session under
;; `efrit-data-directory'/transcripts/, path from
;; `efrit-transcript-file-function'.
;;
;; It listens to the same events the agent buffer renders, so it works
;; for turns started from Lisp (`efrit-submit') too.  Text is appended
;; with `write-region', never held in memory: a crash loses nothing
;; already written.
;;
;; `efrit-transcript-open' visits the current session's file;
;; `efrit-transcript-enabled' turns the writer off.

;;; Code:

(require 'cl-lib)
(require 'efrit-config)
(require 'efrit-events)
(require 'efrit-log)
(require 'efrit-repl-session)

(defgroup efrit-transcript nil
  "Readable per-session transcripts."
  :group 'efrit)

(defcustom efrit-transcript-enabled t
  "Non-nil writes a Markdown transcript of every REPL session."
  :type 'boolean
  :group 'efrit-transcript)

(defcustom efrit-transcript-directory nil
  "Where transcripts go; nil means `efrit-data-directory'/transcripts."
  :type '(choice (const :tag "efrit-data-directory/transcripts" nil) directory)
  :group 'efrit-transcript)

(defcustom efrit-transcript-file-function #'efrit-transcript-default-file
  "Function of a REPL session returning its transcript path, or nil for none."
  :type 'function
  :group 'efrit-transcript)

(defcustom efrit-transcript-result-max-chars 2000
  "Longest tool result written in full; longer ones are clipped."
  :type 'integer
  :group 'efrit-transcript)

(defvar efrit-transcript--answers (make-hash-table :test 'equal)
  "Session id -> text of the assistant message being streamed.")

(defvar efrit-transcript--started (make-hash-table :test 'equal)
  "Session ids whose file has its header.")

(defun efrit-transcript-directory ()
  "The transcript directory, created."
  (let ((dir (or efrit-transcript-directory
                 (expand-file-name "transcripts" efrit-data-directory))))
    (make-directory dir t)
    dir))

(defun efrit-transcript-default-file (session)
  "SESSION's transcript: <created date>-<id>.md in `efrit-transcript-directory'."
  (expand-file-name
   (format "%s-%s.md"
           (format-time-string "%Y-%m-%d" (efrit-repl-session-created-at session))
           (efrit-repl-session-id session))
   (efrit-transcript-directory)))

(defun efrit-transcript-file (session)
  "SESSION's transcript path, or nil when transcripts are off."
  (and efrit-transcript-enabled session
       (funcall efrit-transcript-file-function session)))

(defun efrit-transcript--session (event)
  "The REPL session EVENT is about, or nil."
  (when-let* ((id (alist-get :session-id event)))
    (efrit-repl-session-get id)))

(defun efrit-transcript--append (session text)
  "Append TEXT to SESSION's transcript, writing the header first."
  (when-let* ((file (efrit-transcript-file session)))
    (condition-case err
        (let ((id (efrit-repl-session-id session)))
          (unless (or (gethash id efrit-transcript--started) (file-exists-p file))
            (write-region
             (format "# efrit session %s\n\nStarted %s in `%s`.\n\n" id
                     (format-time-string "%Y-%m-%d %H:%M" (efrit-repl-session-created-at session))
                     (or (efrit-repl-session-project-root session) default-directory))
             nil file nil 'quiet))
          (puthash id t efrit-transcript--started)
          (write-region text nil file t 'quiet))
      (error (efrit-log 'warn "transcript: cannot write %s: %s" file (error-message-string err))))))

(defun efrit-transcript--fence (text)
  "TEXT in a fence longer than any backtick run in it."
  (let ((longest 0) (start 0))
    (while (string-match "`+" text start)
      (setq longest (max longest (- (match-end 0) (match-beginning 0)))
            start (match-end 0)))
    (let ((fence (make-string (max 3 (1+ longest)) ?`)))
      (concat fence "\n" text (if (string-suffix-p "\n" text) "" "\n") fence "\n"))))

(defun efrit-transcript--clip (text)
  "TEXT, clipped to `efrit-transcript-result-max-chars' with a note."
  (let ((text (format "%s" (or text ""))))
    (if (<= (length text) efrit-transcript-result-max-chars)
        text
      (format "%s\n… %d more characters"
              (substring text 0 efrit-transcript-result-max-chars)
              (- (length text) efrit-transcript-result-max-chars)))))

(defun efrit-transcript--stamp ()
  "The time, for a heading."
  (format-time-string "%H:%M:%S"))

;;;; Event handlers

(defun efrit-transcript--on-turn-start (event)
  (when-let* ((session (efrit-transcript--session event)))
    (remhash (efrit-repl-session-id session) efrit-transcript--answers)
    (efrit-transcript--append
     session (format "\n## %s You\n\n%s\n" (efrit-transcript--stamp) (alist-get :input event)))))

(defun efrit-transcript--on-steer (event)
  (when-let* ((session (efrit-transcript--session event)))
    (efrit-transcript--append
     session (format "\n> **steer** %s\n" (alist-get :text event)))))

(defun efrit-transcript--on-tool-start (event)
  (when-let* ((session (efrit-transcript--session event)))
    (efrit-transcript--flush-answer session)
    (efrit-transcript--append
     session (format "\n### tool `%s`\n\n%s"
                     (alist-get :tool event)
                     (efrit-transcript--fence
                      (efrit-transcript--clip
                       (condition-case nil (json-encode (alist-get :input event))
                         (error (format "%S" (alist-get :input event))))))))))

(defun efrit-transcript--on-tool-result (event)
  (when-let* ((session (efrit-transcript--session event)))
    (efrit-transcript--append
     session (format "%s in %.1fs:\n\n%s"
                     (if (alist-get :success event) "ok" "**failed**")
                     (or (alist-get :elapsed event) 0)
                     (efrit-transcript--fence (efrit-transcript--clip (alist-get :result event)))))))

(defun efrit-transcript--on-text-delta (event)
  "Collect the model's text; it is written at `text-end' (both
transports publish deltas, the non-streaming one as a single delta)."
  (when-let* ((session (efrit-transcript--session event))
              (text (alist-get :text event)))
    (let ((id (efrit-repl-session-id session)))
      (puthash id (concat (gethash id efrit-transcript--answers "") text)
               efrit-transcript--answers))))

(defun efrit-transcript--flush-answer (session)
  "Write the assistant text collected for SESSION so far, if any."
  (let* ((id (efrit-repl-session-id session))
         (text (gethash id efrit-transcript--answers)))
    (when (and text (not (string-empty-p (string-trim text))))
      (remhash id efrit-transcript--answers)
      (efrit-transcript--append session (format "\n### efrit\n\n%s\n" (string-trim text))))))

(defun efrit-transcript--on-text-end (event)
  (when-let* ((session (efrit-transcript--session event)))
    (efrit-transcript--flush-answer session)))

(defun efrit-transcript--on-turn-complete (event)
  (when-let* ((session (efrit-transcript--session event)))
    (efrit-transcript--flush-answer session)
    (let ((reason (alist-get :stop-reason event)))
      (unless (member reason '("end_turn" "session-complete" "unknown" nil))
        (efrit-transcript--append session (format "\n_turn ended: %s_\n" reason))))))

(defun efrit-transcript--on-question (event)
  (when-let* ((session (efrit-transcript--session event)))
    (efrit-transcript--flush-answer session)
    (efrit-transcript--append
     session (format "\n### efrit asks\n\n%s\n" (or (alist-get :question event) "")))))

(defun efrit-transcript--on-error (event)
  (when-let* ((session (efrit-transcript--session event)))
    (efrit-transcript--append session (format "\n**error:** %s\n" (alist-get :message event)))))

(defconst efrit-transcript--subscriptions
  '((turn-start . efrit-transcript--on-turn-start)
    (steer . efrit-transcript--on-steer)
    (tool-start . efrit-transcript--on-tool-start)
    (tool-result . efrit-transcript--on-tool-result)
    (text-delta . efrit-transcript--on-text-delta)
    (text-end . efrit-transcript--on-text-end)
    (turn-complete . efrit-transcript--on-turn-complete)
    (question . efrit-transcript--on-question)
    (error . efrit-transcript--on-error))
  "What the transcript listens to: (EVENT . HANDLER).")

(defun efrit-transcript-setup ()
  "Subscribe the transcript writer.  Idempotent."
  (dolist (sub efrit-transcript--subscriptions)
    (efrit-subscribe (car sub) (cdr sub))))

(defun efrit-transcript-teardown ()
  "Unsubscribe the transcript writer."
  (dolist (sub efrit-transcript--subscriptions)
    (efrit-unsubscribe (car sub) (cdr sub))))

(efrit-transcript-setup)

;;;; Commands

(defvar efrit-agent--repl-session)

;;;###autoload
(defun efrit-transcript-open (&optional session)
  "Visit the transcript of SESSION (default: the agent buffer's) in another window.
The file is reverted from disk each time, so it shows what was
written so far."
  (interactive)
  (let* ((session (or session
                      (and (boundp 'efrit-agent--repl-session) efrit-agent--repl-session)
                      (user-error "No REPL session here")))
         (file (or (efrit-transcript-file session)
                   (user-error "Transcripts are off (`efrit-transcript-enabled')"))))
    (unless (file-exists-p file)
      (user-error "No transcript yet for %s" (efrit-repl-session-id session)))
    (let ((buf (find-file-noselect file)))
      (with-current-buffer buf
        (revert-buffer t t t)
        (goto-char (point-max))
        ;; read-only with `q' to close: it is a log to read, not a
        ;; file to edit (tour 2026-09-29: q typed a q into it)
        (view-mode 1))
      (pop-to-buffer buf))))

;;;###autoload
(defun efrit-transcript-list ()
  "Visit the transcript directory in Dired, newest first."
  (interactive)
  (dired (efrit-transcript-directory) "-lt"))

(provide 'efrit-transcript)

;;; efrit-transcript.el ends here
