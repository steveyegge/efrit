;;; efrit-edit-history.el --- What the user changed lately, as diffs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.8.5
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, ai

;;; Commentary:

;; "Continue what I was doing" needs the model to know what you were
;; doing.  With `efrit-edit-history-mode' on, each watched buffer
;; keeps a snapshot of its text; when the buffer has changed
;; (`buffer-chars-modified-tick' moved) and Emacs is idle, the
;; snapshot is diffed against the buffer through the diff library
;; (`efrit-vcs-diff-strings'), the hunks are recorded as one entry,
;; and the snapshot advances.  Nothing runs per keystroke: a one-shot
;; idle timer per buffer, rearmed by the change hook only when it is
;; not already pending.  After minuet-duet-history (2026-09-28).
;;
;; The entries feed the `edit-history' context source
;; (`efrit-context-sources'): the newest entries first, within a
;; character budget, so the model sees the last few bursts of editing
;; in the target buffer.  Off by default: it costs an idle diff per
;; edit burst in every watched buffer.
;;
;; Diff headers are stripped, so temp paths never reach the model.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'efrit-vcs)
(require 'efrit-throttle)

(defgroup efrit-edit-history nil
  "Recent edits as context for the model."
  :group 'efrit)

(defcustom efrit-edit-history-idle-seconds 2.0
  "Idle time after an edit before the burst is diffed and recorded."
  :type 'number
  :group 'efrit-edit-history)

(defcustom efrit-edit-history-max-entries 20
  "Entries kept per buffer; older ones are dropped."
  :type 'integer
  :group 'efrit-edit-history)

(defcustom efrit-edit-history-max-chars 3000
  "Longest edit history included in the context, newest entries first."
  :type 'integer
  :group 'efrit-edit-history)

(defcustom efrit-edit-history-max-buffer-size 500000
  "Buffers larger than this are not watched: the snapshot would be too costly."
  :type 'integer
  :group 'efrit-edit-history)

(defvar-local efrit-edit-history--snapshot nil
  "The buffer text at the last recorded point, or nil before the first.")
(defvar-local efrit-edit-history--tick nil
  "The `buffer-chars-modified-tick' the snapshot was taken at.")
(defvar-local efrit-edit-history--throttle nil
  "This buffer's `efrit-throttle': the idle diff, debounced per burst.")
(defvar-local efrit-edit-history--entries nil
  "Recorded bursts, newest first: plists (:time :diff :chars).")
(defvar efrit-edit-history-mode)

(defun efrit-edit-history--watchable-p (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (and (not (minibufferp))
         (not (string-prefix-p " " (buffer-name)))
         (<= (buffer-size) efrit-edit-history-max-buffer-size))))

(defun efrit-edit-history--take-snapshot ()
  (setq efrit-edit-history--snapshot (buffer-substring-no-properties (point-min) (point-max))
        efrit-edit-history--tick (buffer-chars-modified-tick)))

(defun efrit-edit-history--strip-headers (diff)
  "DIFF without the ---/+++ file lines."
  (mapconcat #'identity
             (cl-remove-if (lambda (l) (or (string-prefix-p "--- " l) (string-prefix-p "+++ " l)))
                           (split-string diff "\n"))
             "\n"))

(defun efrit-edit-history-record (&optional buffer)
  "Diff BUFFER against its snapshot now and record a burst if it changed.
Returns the new entry, or nil when nothing changed."
  (with-current-buffer (or buffer (current-buffer))
    (when (and efrit-edit-history--snapshot
               (/= efrit-edit-history--tick (buffer-chars-modified-tick)))
      (let* ((now (buffer-substring-no-properties (point-min) (point-max)))
             (diff (efrit-edit-history--strip-headers
                    (efrit-vcs-diff-strings efrit-edit-history--snapshot now
                                            (buffer-name) (buffer-name)))))
        (setq efrit-edit-history--snapshot now
              efrit-edit-history--tick (buffer-chars-modified-tick))
        (unless (string-empty-p (string-trim diff))
          (let ((entry (list :time (current-time) :diff diff :chars (length diff))))
            (push entry efrit-edit-history--entries)
            (when (> (length efrit-edit-history--entries) efrit-edit-history-max-entries)
              (setcdr (nthcdr (1- efrit-edit-history-max-entries) efrit-edit-history--entries) nil))
            entry))))))

(defun efrit-edit-history--after-change (&rest _)
  "Ask the throttle for an idle diff; each keystroke only resets its timer."
  (when efrit-edit-history--throttle
    (efrit-throttle-request efrit-edit-history--throttle (current-buffer))))

(defun efrit-edit-history--idle (buffer)
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when efrit-edit-history-mode
        (efrit-edit-history-record)))))

;;;###autoload
(define-minor-mode efrit-edit-history-mode
  "Record this buffer's edit bursts as diffs for the model's context."
  :lighter nil
  (if efrit-edit-history-mode
      (if (not (efrit-edit-history--watchable-p))
          (setq efrit-edit-history-mode nil)
        (efrit-edit-history--take-snapshot)
        (setq efrit-edit-history--throttle
              (efrit-throttle-create (format "edit-history %s" (buffer-name))
                                     #'efrit-edit-history--idle
                                     :debounce efrit-edit-history-idle-seconds
                                     :interval efrit-edit-history-idle-seconds))
        (add-hook 'after-change-functions #'efrit-edit-history--after-change nil t))
    (remove-hook 'after-change-functions #'efrit-edit-history--after-change t)
    (when efrit-edit-history--throttle
      (efrit-throttle-cancel efrit-edit-history--throttle)
      (setq efrit-edit-history--throttle nil))
    (setq efrit-edit-history--snapshot nil)))

;;;###autoload
(define-globalized-minor-mode efrit-global-edit-history-mode efrit-edit-history-mode
  (lambda () (when (efrit-edit-history--watchable-p) (efrit-edit-history-mode 1)))
  :group 'efrit-edit-history)

(defun efrit-edit-history-text (&optional buffer max-chars)
  "BUFFER's recent edits, newest first, within MAX-CHARS, or nil.
An in-progress burst is recorded first so the text is current."
  (with-current-buffer (or buffer (current-buffer))
    (when efrit-edit-history-mode
      (efrit-edit-history-record))
    (when efrit-edit-history--entries
      (let ((budget (or max-chars efrit-edit-history-max-chars))
            (parts nil) (used 0))
        (cl-loop for e in efrit-edit-history--entries
                 for text = (format "@ %s ago\n%s"
                                    (efrit-edit-history--ago (plist-get e :time))
                                    (plist-get e :diff))
                 while (<= (+ used (length text)) budget)
                 do (push text parts) (cl-incf used (length text)))
        (when parts
          (concat "Recent edits in this buffer, newest first (unified diff hunks):\n"
                  (mapconcat #'identity (nreverse parts) "\n")))))))

(defun efrit-edit-history--ago (time)
  (let ((s (round (float-time (time-since time)))))
    (cond ((< s 60) (format "%ds" s))
          ((< s 3600) (format "%dm" (/ s 60)))
          (t (format "%dh" (/ s 3600))))))

(defun efrit-edit-history-clear (&optional buffer)
  "Forget BUFFER's recorded edits."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (setq efrit-edit-history--entries nil)))

(provide 'efrit-edit-history)

;;; efrit-edit-history.el ends here
