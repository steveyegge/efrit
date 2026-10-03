;;; efrit-diagnostics-baseline.el --- New diagnostics after each edit, mechanically -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.8.5
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, ai

;;; Commentary:

;; "Done" for a code change should mean: no diagnostics that were not
;; there before.  ai-code-interface asks the model to keep a baseline
;; and compare; efrit owns its loop, so it does it itself.
;;
;; Before a write tool touches a file the first time in a turn, the
;; file's Flymake/Flycheck diagnostics are recorded.  After each write
;; the checker gets a moment to run, the diagnostics are read again,
;; and the ones not in the baseline are appended to the tool result:
;;
;;   [new diagnostics in greet.el: 12: error: void-variable nme]
;;
;; The model sees the damage in the same result that reported the
;; edit, and fixes it before saying it is done.  Per turn: the
;; baseline is dropped at the next turn's start
;; (`efrit-diagnostics-baseline-begin-turn').

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'efrit-tool-get-diagnostics)

(defgroup efrit-diagnostics-baseline nil
  "New diagnostics reported after each edit."
  :group 'efrit)

(defcustom efrit-diagnostics-baseline-enabled t
  "Non-nil appends the new diagnostics of an edited file to the edit's result."
  :type 'boolean
  :group 'efrit-diagnostics-baseline)

(defcustom efrit-diagnostics-baseline-wait 0.6
  "Seconds given to the checker after an edit before diagnostics are read.
Flymake runs on an idle timer; without the wait the read sees the old state."
  :type 'number
  :group 'efrit-diagnostics-baseline)

(defcustom efrit-diagnostics-baseline-max 12
  "Most new diagnostics listed in one result."
  :type 'integer
  :group 'efrit-diagnostics-baseline)

(defconst efrit-diagnostics-baseline-write-tools
  '("edit_file" "create_file" "edit_buffer" "format_file")
  "Tools whose result gets the new-diagnostics note.")

(defvar efrit-diagnostics-baseline--per-turn (make-hash-table :test 'equal)
  "File name -> list of diagnostic keys recorded before the first write this turn.")

(defun efrit-diagnostics-baseline-begin-turn ()
  "Forget the baselines: the next write starts a new comparison."
  (clrhash efrit-diagnostics-baseline--per-turn))

(declare-function efrit-resolve-path-simple "efrit-tool-utils")

(defun efrit-diagnostics-baseline--read (buffer)
  "The diagnostics of BUFFER as a list of (KEY . LINE-TEXT).
KEY ignores the line number (an edit shifts lines; the same message
on a moved line is not new)."
  (when (buffer-live-p buffer)
    (mapcar (lambda (d)
              (cons (format "%s|%s|%s" (alist-get 'source d) (alist-get 'severity d) (alist-get 'message d))
                    (format "%s: %s: %s" (alist-get 'line d) (alist-get 'severity d)
                            (string-trim (or (alist-get 'message d) "")))))
            (append (efrit-tool-get-diagnostics--from-flymake buffer)
                    (efrit-tool-get-diagnostics--from-flycheck buffer)))))

(defun efrit-diagnostics-baseline--file-of (tool input)
  "The file TOOL's INPUT touches, absolute, or nil."
  (let ((path (cond ((hash-table-p input)
                     (or (gethash "path" input) (gethash "file" input) (gethash "file_path" input)))
                    ((stringp input) nil))))
    (cond
     ((and (stringp path) (not (string-empty-p path)))
      (ignore-errors (efrit-resolve-path-simple path)))
     ((and (equal tool "edit_buffer") (hash-table-p input))
      (when-let* ((name (gethash "buffer" input))
                  (buf (get-buffer name)))
        (buffer-file-name buf))))))

(defun efrit-diagnostics-baseline-record (file)
  "Record FILE's diagnostics as this turn's baseline, once, before its first write."
  (when-let* ((buf (or (find-buffer-visiting file)
                       (and (file-exists-p file) (find-file-noselect file)))))
    (let ((key (file-truename file)))
      (unless (gethash key efrit-diagnostics-baseline--per-turn)
        (puthash key (or (mapcar #'car (efrit-diagnostics-baseline--read buf)) '(none))
                 efrit-diagnostics-baseline--per-turn)))))

(defun efrit-diagnostics-baseline-before-tool (tool input)
  "Before TOOL runs on INPUT: take the baseline of the file it will write."
  (when (and efrit-diagnostics-baseline-enabled
             (member tool efrit-diagnostics-baseline-write-tools))
    (condition-case err
        (when-let* ((file (efrit-diagnostics-baseline--file-of tool input)))
          (efrit-diagnostics-baseline-record file))
      (error (when (fboundp 'efrit-log)
               (efrit-log 'warn "diagnostics baseline: %s" (error-message-string err)))))))

(defun efrit-diagnostics-baseline-note (file)
  "The diagnostics of FILE not in this turn's baseline, as a note string, or nil.
A file with no baseline (created this turn) compares against nothing."
  (when-let* ((buf (or (find-buffer-visiting file)
                       (and (file-exists-p file) (find-file-noselect file)))))
    (let ((key (file-truename file)))
      (unless (gethash key efrit-diagnostics-baseline--per-turn)
        (puthash key '(none) efrit-diagnostics-baseline--per-turn))
      (progn
        ;; edit_file wrote to disk: a visiting buffer that has no
        ;; unsaved changes of its own must show the new text, or the
        ;; checker judges the old one
        (with-current-buffer buf
          (when (and buffer-file-name (not (buffer-modified-p))
                     (not (verify-visited-file-modtime)))
            (revert-buffer t t t)))
        ;; the checker is on an idle timer: give it its moment
        (when (> efrit-diagnostics-baseline-wait 0)
          (with-current-buffer buf
            (when (bound-and-true-p flymake-mode) (ignore-errors (flymake-start)))
            (when (and (bound-and-true-p flycheck-mode) (fboundp 'flycheck-buffer))
              (ignore-errors (flycheck-buffer))))
          (sit-for efrit-diagnostics-baseline-wait))
        (let* ((base (gethash key efrit-diagnostics-baseline--per-turn))
               (now (efrit-diagnostics-baseline--read buf))
               (new (cl-remove-if (lambda (d) (member (car d) base)) now)))
          (when new
            (format "\n[new diagnostics in %s since your first edit this turn (%d): %s%s]"
                    (file-name-nondirectory file) (length new)
                    (mapconcat #'cdr (seq-take new efrit-diagnostics-baseline-max) "; ")
                    (if (> (length new) efrit-diagnostics-baseline-max) "; …" ""))))))))

(declare-function flymake-start "flymake")
(declare-function flycheck-buffer "flycheck")

(defun efrit-diagnostics-baseline-after-tool (tool input result)
  "RESULT of TOOL on INPUT, with the new-diagnostics note when there is one.
Called by the dispatcher for every tool; only the write tools do
anything.  The baseline was taken by `efrit-diagnostics-baseline-before-tool'
before the file's first write this turn."
  (if (and efrit-diagnostics-baseline-enabled
           (member tool efrit-diagnostics-baseline-write-tools)
           (stringp result)
           (not (string-match-p "\\`\\(?:\n\\)?\\[?Error" result)))
      (condition-case err
          (let ((file (efrit-diagnostics-baseline--file-of tool input)))
            (if-let* ((note (and file (efrit-diagnostics-baseline-note file))))
                (concat result note)
              result))
        (error (when (fboundp 'efrit-log)
                 (efrit-log 'warn "diagnostics baseline: %s" (error-message-string err)))
               result))
    result))

(provide 'efrit-diagnostics-baseline)

;;; efrit-diagnostics-baseline.el ends here
