;;; efrit-grant-history.el --- What the user answered to sandbox prompts, over time -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.9.2
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, ai

;;; Commentary:

;; Session grants vanish with the Emacs; project grants are saved per
;; repository.  Neither tells the reviewer what the user has tended to
;; answer: "read under ~/ai_workspace/source/autodist, granted eleven
;; times, denied never" is the fact that makes a twelfth prompt
;; pointless (tzz, 2026-10-03: "derived from past interactions and from
;; some basic rules").
;;
;; This file keeps, per (CAP . PATTERN), the user's answers as dated
;; events.  A pattern is coarser than a grant target: a path is reduced
;; to its repository (or directory when outside any), a host to itself,
;; a shell line to its sorted command set.  The store is a JSON file in
;; `efrit-data-directory'.  Nothing here grants anything; see
;; `efrit-review-confidence' for how the record is weighed.
;;
;; Two things about an answer matter besides yes/no (tzz, 2026-10-03:
;; "add a decay factor for my approvals; a permanent grant is worth
;; more than a session which is more than a one-time grant"): its
;; SCOPE, weighted by `efrit-grant-history-scope-weights', and its AGE,
;; halved every `efrit-grant-history-half-life-days'.  A denial weighs
;; against, more than any single yes weighs for.  `efrit-grant-history-
;; strength' is the decayed, weighted sum the reviewer reads.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'efrit-config)
(require 'efrit-log)

(declare-function efrit-sandbox-repo-of "efrit-sandbox")
(declare-function efrit-sandbox-shell-commands "efrit-sandbox")
(declare-function efrit-sandbox-canonical "efrit-sandbox")
(declare-function efrit-sandbox-remote-p "efrit-sandbox")

(defgroup efrit-grant-history nil
  "Memory of the user's answers to sandbox prompts."
  :group 'efrit)

(defcustom efrit-grant-history-enabled t
  "When non-nil, every sandbox answer is recorded for the reviewer's confidence."
  :type 'boolean
  :group 'efrit-grant-history)

(defcustom efrit-grant-history-scope-weights
  '((project . 3.0) (session . 2.0) (once . 1.0) (denied . -3.0))
  "What one answer is worth before decay.
A saved grant is a stronger statement than a session grant, which is
stronger than a one-time yes; one no outweighs any single yes."
  :type '(alist :key-type symbol :value-type number)
  :group 'efrit-grant-history)

(defcustom efrit-grant-history-half-life-days 60
  "An answer's weight halves every this many days.
nil: answers never fade."
  :type '(choice (const nil) number)
  :group 'efrit-grant-history)

(defcustom efrit-grant-history-max-events 50
  "Events kept per pattern; the oldest go first."
  :type 'integer
  :group 'efrit-grant-history)

(defconst efrit-grant-history--file-name "grant-history.json")

(defvar efrit-grant-history--table nil
  "Key string -> list of events (SCOPE . TIME-STRING), newest first; nil before loading.
SCOPE is `project', `session', `once' or `denied'.")

(defun efrit-grant-history-file ()
  (efrit-config-data-file efrit-grant-history--file-name))

;;;; Patterns

(defun efrit-grant-history-pattern (cap target)
  "The history key for CAP on TARGET: coarse enough to recur, fine enough to mean something.
Returns a string, or nil for targets no pattern should ever cover
\(remote paths, blanket requests)."
  (pcase cap
    ((or 'read 'write 'buffer)
     (cond
      ((and (consp target) (eq cap 'buffer)) nil)
      ((not (stringp target)) nil)
      ((efrit-sandbox-remote-p target) nil)
      (t (let* ((path (efrit-sandbox-canonical target))
                (repo (efrit-sandbox-repo-of path))
                (dir (or repo
                         (if (directory-name-p path) path (file-name-directory path)))))
           (and dir (format "%s %s" cap (file-name-as-directory dir)))))))
    ('net
     (when (and (consp target) (eq (car target) 'host) (stringp (cdr target)))
       (format "net %s" (downcase (cdr target)))))
    ('shell
     (when (stringp target)
       (let ((names (ignore-errors (efrit-sandbox-shell-commands target))))
         (and names (format "shell %s" (string-join (sort (copy-sequence names) #'string<) " "))))))
    (_ nil)))

;;;; Store

(defun efrit-grant-history--load ()
  (unless efrit-grant-history--table
    (setq efrit-grant-history--table (make-hash-table :test #'equal))
    (let ((file (efrit-grant-history-file)))
      (when (file-readable-p file)
        (condition-case err
            (let ((data (with-temp-buffer
                          (insert-file-contents file)
                          (json-parse-buffer :object-type 'alist :array-type 'list))))
              (dolist (entry (alist-get 'entries data))
                (let ((key (alist-get 'key entry))
                      (events (alist-get 'events entry)))
                  (when (stringp key)
                    (puthash key
                             (if events
                                 (mapcar (lambda (e) (cons (intern (alist-get 'scope e)) (alist-get 'at e)))
                                         events)
                               ;; version 1 kept counts: replay them as
                               ;; session-strength grants and denials at
                               ;; the last-seen time
                               (let ((at (alist-get 'last entry)) (out nil))
                                 (dotimes (_ (or (alist-get 'granted entry) 0)) (push (cons 'session at) out))
                                 (dotimes (_ (or (alist-get 'denied entry) 0)) (push (cons 'denied at) out))
                                 out))
                             efrit-grant-history--table)))))
          (error (efrit-log 'warn "grant history: could not read %s: %s"
                            file (error-message-string err)))))))
  efrit-grant-history--table)

(defun efrit-grant-history--save ()
  (let ((file (efrit-grant-history-file)) (entries nil))
    (maphash (lambda (k v)
               (push `((key . ,k)
                       (events . ,(mapcar (lambda (e) `((scope . ,(symbol-name (car e))) (at . ,(cdr e)))) v)))
                     entries))
             efrit-grant-history--table)
    (condition-case err
        (progn
          (make-directory (file-name-directory file) t)
          (with-temp-file file
            (insert (json-encode `((version . 2) (entries . ,(nreverse entries)))))))
      (error (efrit-log 'warn "grant history: could not write %s: %s"
                        file (error-message-string err))))))

(defun efrit-grant-history-record (cap target answer &optional at)
  "Record that the user gave ANSWER (`once', `session', `project', or nil) for CAP on TARGET.
AT is the time string (default now); tests use it to age events."
  (when efrit-grant-history-enabled
    (when-let* ((key (efrit-grant-history-pattern cap target)))
      (let* ((table (efrit-grant-history--load))
             (scope (if (memq answer '(once session project)) answer 'denied))
             (events (cons (cons scope (or at (format-time-string "%FT%T%z")))
                           (gethash key table))))
        (when (> (length events) efrit-grant-history-max-events)
          (setq events (seq-take events efrit-grant-history-max-events)))
        (puthash key events table)
        (efrit-grant-history--save)
        events))))

(defun efrit-grant-history-events (cap target)
  "The events (SCOPE . TIME) for CAP on TARGET, newest first, or nil."
  (when-let* ((key (efrit-grant-history-pattern cap target)))
    (gethash key (efrit-grant-history--load))))

(defun efrit-grant-history--decay (time-string)
  "The factor 0..1 an answer given at TIME-STRING keeps today."
  (if (null efrit-grant-history-half-life-days)
      1.0
    (let* ((then (condition-case nil (date-to-time time-string) (error nil)))
           (days (if then (/ (float-time (time-since then)) 86400.0) 0.0)))
      (expt 0.5 (/ (max 0.0 days) (float efrit-grant-history-half-life-days))))))

(defun efrit-grant-history-strength (cap target)
  "What the user's past answers for CAP on TARGET add up to today.
A plist (:yes Y :no N :net Y-N :count C :last TIME): Y and N are the
decayed, scope-weighted sums of grants and denials.  Nil when nothing
was recorded."
  (when-let* ((events (efrit-grant-history-events cap target)))
    (let ((yes 0.0) (no 0.0))
      (dolist (e events)
        (let ((w (* (or (alist-get (car e) efrit-grant-history-scope-weights) 0.0)
                    (efrit-grant-history--decay (cdr e)))))
          (if (eq (car e) 'denied) (cl-incf no (- w)) (cl-incf yes w))))
      (list :yes yes :no no :net (- yes no) :count (length events) :last (cdar events)))))

(defun efrit-grant-history-lookup (cap target)
  "Counts for CAP on TARGET as (:granted N :denied N :last TIME), or nil.
Raw counts, undecayed: for display and tests.  The reviewer reads
`efrit-grant-history-strength'."
  (when-let* ((events (efrit-grant-history-events cap target)))
    (list :granted (cl-count-if-not (lambda (e) (eq (car e) 'denied)) events)
          :denied (cl-count-if (lambda (e) (eq (car e) 'denied)) events)
          :last (cdar events))))

(defun efrit-grant-history-forget ()
  "Drop the whole history (tests, or a fresh start)."
  (interactive)
  (setq efrit-grant-history--table (make-hash-table :test #'equal))
  (efrit-grant-history--save))

(provide 'efrit-grant-history)

;;; efrit-grant-history.el ends here
