;;; efrit-file-io.el --- Buffer-aware, verified file reads and writes -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.12.12
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; The file tools used to read and write the file on *disk* even when
;; a buffer was visiting it.  Two bad outcomes: the model read stale
;; content when the user had unsaved edits, and edit_file rewrote the
;; disk copy underneath a live buffer, which then fought the user with
;; "file changed on disk".  agent-shell's fs/read_text_file and
;; fs/write_text_file go through the open buffer for this reason, and
;; copilot-chat's region rewrite re-verifies the target before
;; applying.  This module is efrit's version of both:
;;
;; - `efrit-file-read-string' returns the visiting buffer's contents
;;   if there is one (unsaved edits included), else the file's.
;; - `efrit-file-replace' applies OLD -> NEW to a file: in the visiting
;;   buffer when one exists (one undo group, buffer saved unless
;;   `efrit-file-save-after-edit' is nil), else on disk.  Before
;;   applying it re-reads and re-verifies that the text still matches
;;   what the caller saw, because between the model's read and its
;;   write the user may have typed, a timer may have run, or a
;;   permission prompt may have blocked for a minute.
;; - `efrit-file-write-string' likewise writes whole content via the
;;   buffer when visiting.
;;
;; All mutation runs under `save-excursion'/`save-restriction' with
;; the buffer widened, and never signals from inside the buffer
;; operation with the buffer half-changed: verification happens first.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup efrit-file-io nil
  "Buffer-aware file access for tools."
  :group 'efrit
  :prefix "efrit-file-")

(defcustom efrit-file-save-after-edit t
  "When non-nil, edits applied through a visiting buffer are saved to disk.
When nil the buffer is left modified for the user to save; the model
is told either way."
  :type 'boolean
  :group 'efrit-file-io)

(define-error 'efrit-file-changed
  "File content changed since it was read; edit not applied")

(defun efrit-file-visiting-buffer (path)
  "Return a live buffer visiting PATH, or nil.  Follows symlinks."
  (or (get-file-buffer path)
      (find-buffer-visiting path)))

(defun efrit-file-read-string (path)
  "Contents of PATH as seen by the user: the visiting buffer if any, else disk."
  (if-let* ((buf (efrit-file-visiting-buffer path)))
      (with-current-buffer buf
        (save-restriction
          (widen)
          (buffer-substring-no-properties (point-min) (point-max))))
    (with-temp-buffer
      (insert-file-contents path)
      (buffer-string))))

(defun efrit-file--count-occurrences (needle haystack)
  (let ((n 0) (start 0))
    (while (setq start (string-search needle haystack start))
      (cl-incf n)
      (setq start (+ start (max 1 (length needle)))))
    n))

(defun efrit-file--apply-in-buffer (buf old new replace-all)
  "Replace OLD with NEW in BUF (all occurrences when REPLACE-ALL).
Returns the number of replacements.  One undo group; point and
narrowing restored."
  (with-current-buffer buf
    (let ((count 0) (inhibit-read-only nil))
      (barf-if-buffer-read-only)
      (save-excursion
        (save-restriction
          (widen)
          (atomic-change-group
            (goto-char (point-min))
            (while (and (search-forward old nil t)
                        (or replace-all (= count 0)))
              (replace-match new t t)
              (cl-incf count)))))
      count)))

(defun efrit-file--stale-buffer-p (buf)
  "Non-nil when BUF visits a file whose disk copy changed since the visit."
  (and (buffer-file-name buf)
       (file-exists-p (buffer-file-name buf))
       (not (verify-visited-file-modtime buf))))

(defun efrit-file--disk-string (path)
  (with-temp-buffer (insert-file-contents path) (buffer-string)))

(defun efrit-file-reconcile-buffer (buf old)
  "Bring BUF up to date with its file when the file changed under it.
efrit decides; the user is never asked y/n/r (tzz, 2026-10-20: a
reinstall changed the file, `basic-save-buffer' asked, and the model
read the \"File reverted\" outcome as a failed edit).

- Buffer unmodified, file changed: revert quietly.  The buffer held
  nothing of its own.
- Buffer modified, file changed, and OLD (the text about to be
  replaced) reads the same in both: keep the buffer.  Its edits are
  the newer intent; the save below overwrites the disk copy on
  purpose, with the supersession question answered yes.
- Buffer modified, file changed, and OLD differs between them or is
  missing from the disk copy: signal `efrit-file-changed' with both
  versions.  Nobody can pick for the user here.

Returns `reverted', `kept' or nil (nothing was stale)."
  (when (efrit-file--stale-buffer-p buf)
    (with-current-buffer buf
      (if (not (buffer-modified-p))
          (progn
            (let ((inhibit-message t))
              (revert-buffer t t t))
            'reverted)
        (let* ((disk (efrit-file--disk-string (buffer-file-name)))
               (here (save-restriction (widen) (buffer-substring-no-properties (point-min) (point-max))))
               (in-disk (efrit-file--count-occurrences old disk))
               (in-here (efrit-file--count-occurrences old here)))
          (if (and (> in-disk 0) (= in-disk in-here))
              'kept
            (signal 'efrit-file-changed
                    (list (buffer-file-name)
                          (format "%s changed on disk while its buffer holds unsaved edits, and the text to replace differs between them (buffer: %d match%s, disk: %d). Read the file (the buffer is what read_file returns) and the disk copy, decide which is current, and retry; or ask the user."
                                  (file-name-nondirectory (buffer-file-name))
                                  in-here (if (= in-here 1) "" "es") in-disk)))))))))

(defun efrit-file--save-buffer-quietly (buf)
  "Save BUF without the supersession question: the buffer's content wins.
`basic-save-buffer' asks \"has changed since visited or saved.  Save
anyway?\" through `yes-or-no-p' itself; the modtime is refreshed first
so the question never arises, and the editing-time check
\(`ask-user-about-supersession-threat') is a no-op for the save."
  (with-current-buffer buf
    (save-excursion
      (let ((inhibit-message t))
        (cl-letf (((symbol-function 'ask-user-about-supersession-threat) #'ignore))
          (set-visited-file-modtime)
          (basic-save-buffer))))))

(defun efrit-file-replace (path old new &optional replace-all expected-content)
  "Replace OLD with NEW in PATH; return a plist describing what happened.

When a buffer visits PATH the change is made there (and saved per
`efrit-file-save-after-edit'); otherwise the file is rewritten.  A
buffer whose file changed on disk is reconciled first
\(`efrit-file-reconcile-buffer'): no question reaches the user.

EXPECTED-CONTENT, if given, is the full content the caller previously
read.  The current content is compared against it immediately before
applying; a mismatch signals `efrit-file-changed' with no change made.
Even without it, OLD must still be present exactly the expected number
of times at apply time.

Returns (:count N :via buffer|disk :saved BOOL :buffer-modified BOOL
:content NEW-CONTENT :reconciled reverted|kept|nil)."
  (let* ((buf (efrit-file-visiting-buffer path))
         (reconciled (and buf (efrit-file-reconcile-buffer buf old)))
         (current (efrit-file-read-string path)))
    (when (and expected-content (not (string= current expected-content)))
      (signal 'efrit-file-changed
              (list path (format "%s changed between read and write (%d vs %d chars); re-read it and retry"
                                 (file-name-nondirectory path)
                                 (length current) (length expected-content)))))
    (let ((occurrences (efrit-file--count-occurrences old current)))
      (cond
       ((zerop occurrences)
        (signal 'user-error (list "old_str not found in file" path)))
       ((and (> occurrences 1) (not replace-all))
        (signal 'user-error
                (list (format "old_str appears %d times; use replace_all=true or add context" occurrences)))))
      (if buf
          (let ((count (efrit-file--apply-in-buffer buf old new replace-all))
                (saved nil))
            (when efrit-file-save-after-edit
              (efrit-file--save-buffer-quietly buf)
              (setq saved t))
            (list :count count :via 'buffer :saved saved
                  :buffer-modified (buffer-modified-p buf)
                  :content (efrit-file-read-string path)
                  :reconciled reconciled))
        (let* ((new-content
                (with-temp-buffer
                  (insert current)
                  (goto-char (point-min))
                  (let ((c 0))
                    (while (and (search-forward old nil t) (or replace-all (= c 0)))
                      (replace-match new t t) (cl-incf c)))
                  (buffer-string))))
          (with-temp-file path (insert new-content))
          (list :count (if replace-all occurrences 1) :via 'disk :saved t
                :buffer-modified nil :content new-content :reconciled nil))))))

(defun efrit-file-write-string (path content)
  "Write CONTENT to PATH, through the visiting buffer if there is one.
Returns (:via buffer|disk :saved BOOL)."
  (if-let* ((buf (efrit-file-visiting-buffer path)))
      (with-current-buffer buf
        (barf-if-buffer-read-only)
        (save-excursion
          (save-restriction
            (widen)
            (atomic-change-group
              (delete-region (point-min) (point-max))
              (insert content))))
        (let ((saved nil))
          (when efrit-file-save-after-edit
            ;; whole-content write: the caller means this content;
            ;; a changed disk copy is overwritten without a question
            (efrit-file--save-buffer-quietly (current-buffer))
            (setq saved t))
          (list :via 'buffer :saved saved)))
    (let ((dir (file-name-directory path)))
      (when (and dir (not (file-directory-p dir)))
        (make-directory dir t)))
    (with-temp-file path (insert content))
    (list :via 'disk :saved t)))

(provide 'efrit-file-io)

;;; efrit-file-io.el ends here
