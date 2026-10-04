;;; efrit-buffer-watch.el --- Has this buffer changed since the model read it? -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.9.2
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools

;;; Commentary:

;; The model reads a buffer, thinks, and edits it by position.  If
;; the user typed in between, the positions are stale and the edit
;; lands in the wrong place.  This file remembers, per buffer, what
;; the model last saw, and tells an edit tool whether the buffer moved
;; since, and where.
;;
;; With `track-changes' (Emacs 30, or GNU ELPA) the report is exact:
;; the changed region and its old text, coalesced safely even for
;; edits made with hooks inhibited.  Without it, the fallback is
;; `buffer-chars-modified-tick': changed or not, no region.
;;
;; `efrit-buffer-watch-note-read' after a read; `efrit-buffer-watch-changes'
;; before an edit returns nil (unchanged) or a plist describing the
;; change; the edit tool puts it in its result so the model re-reads.

;;; Code:

(require 'cl-lib)

(defvar efrit-buffer-watch--trackers (make-hash-table :test 'eq :weakness 'key)
  "Buffer -> (TICK . TRACKER-ID-OR-NIL) at the model's last read.")

(defun efrit-buffer-watch-available-p ()
  "Non-nil when `track-changes' can give exact change regions."
  (and (require 'track-changes nil t) (fboundp 'track-changes-register)))

(declare-function track-changes-register "track-changes")
(declare-function track-changes-fetch "track-changes")
(declare-function track-changes-unregister "track-changes")

(defun efrit-buffer-watch-note-read (buffer)
  "Remember that the model read BUFFER as it is now."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when-let* ((old (gethash buffer efrit-buffer-watch--trackers))
                  (id (cdr old)))
        (ignore-errors (track-changes-unregister id)))
      (puthash buffer
               (cons (buffer-chars-modified-tick)
                     (and (efrit-buffer-watch-available-p)
                          (condition-case nil
                              (track-changes-register #'ignore :nobefore nil)
                            (error nil))))
               efrit-buffer-watch--trackers))))

(defun efrit-buffer-watch-changes (buffer)
  "How BUFFER changed since the model last read it, or nil.
nil also when the model never read it (nothing to be stale against).
Otherwise a plist: :begin :end (the changed region now), :before
\(the text that was there, when known), :lines (the first changed
line number)."
  (when-let* ((entry (and (buffer-live-p buffer) (gethash buffer efrit-buffer-watch--trackers))))
    (with-current-buffer buffer
      (let ((tick (car entry)) (id (cdr entry)))
        (cond
         ((= tick (buffer-chars-modified-tick)) nil)
         (id
          (let (report)
            (condition-case nil
                (track-changes-fetch
                 id (lambda (begin end before)
                      (setq report (list :begin begin :end end
                                         :before (if (stringp before) before nil)
                                         :line (line-number-at-pos begin)))))
              (error nil))
            (or report (list :begin (point-min) :end (point-max) :before nil :line nil))))
         (t (list :begin (point-min) :end (point-max) :before nil :line nil)))))))

(defun efrit-buffer-watch-describe (change buffer)
  "CHANGE (from `efrit-buffer-watch-changes') as one line for a tool result."
  (if (plist-get change :line)
      (format "buffer %s changed since you read it: lines from %d (%d chars now%s); positions may be stale, read it again"
              (buffer-name buffer) (plist-get change :line)
              (- (plist-get change :end) (plist-get change :begin))
              (if (plist-get change :before)
                  (format ", was %d chars" (length (plist-get change :before))) ""))
    (format "buffer %s changed since you read it; positions may be stale, read it again"
            (buffer-name buffer))))

(provide 'efrit-buffer-watch)

;;; efrit-buffer-watch.el ends here
