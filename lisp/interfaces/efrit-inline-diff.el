;;; efrit-inline-diff.el --- Show a proposed change in the buffer itself -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.8.5
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, ai

;;; Commentary:

;; A rewrite preview drawn where the text is, not in a diff buffer:
;; lines that go away get the `diff-removed' face, lines that arrive
;; are shown after them in `diff-added'.  Nothing in the buffer
;; changes; overlays only.  `efrit-inline-diff-show' draws,
;; `efrit-inline-diff-clear' removes.
;;
;; The hunks come from Emacs's diff library, through
;; `efrit-vcs-diff-strings' (`diff-no-select'): no diff algorithm
;; lives here (the reuse rule, 2026-09-28).  `efrit-inline-diff-hunks'
;; parses the unified output into line ranges.
;;
;; Inserted lines that follow a replaced line are appended to the
;; *same* overlay's `after-string'.  Separate overlays sharing an
;; anchor render in an order Emacs does not promise (minuet-duet
;; found this on indented blank lines).

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'diff-mode)
(require 'efrit-vcs)

(defface efrit-inline-diff-removed
  '((t :inherit diff-removed :strike-through t :extend t))
  "A line the rewrite removes (struck through: the colour alone was not enough on macOS, tour 2026-09-29)."
  :group 'efrit)

(defface efrit-inline-diff-added
  '((t :inherit diff-added :extend t))
  "A line the rewrite adds."
  :group 'efrit)

;;;; Hunks from the diff library

(defun efrit-inline-diff-hunks (old new)
  "The changes from OLD to NEW (strings) as a list of hunks.
Each hunk is a plist: :old-start (1-based line in OLD), :old-count,
:removed (the OLD lines), :added (the NEW lines).  Context lines are
folded away, so every hunk is one contiguous change."
  (let ((text (efrit-vcs-diff-strings old new))
        (hunks nil))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (while (re-search-forward "^@@ -\\([0-9]+\\)\\(?:,\\([0-9]+\\)\\)? \\+[0-9]+\\(?:,[0-9]+\\)? @@" nil t)
        (let ((old-line (string-to-number (match-string 1)))
              (removed nil) (added nil) (change-start nil))
          (forward-line 1)
          (cl-flet ((flush ()
                      (when (or removed added)
                        (push (list :old-start change-start
                                    :old-count (length removed)
                                    :removed (nreverse removed)
                                    :added (nreverse added))
                              hunks))
                      (setq removed nil added nil change-start nil)))
            (while (and (not (eobp)) (not (looking-at "^@@")))
              (let ((c (char-after))
                    (line (buffer-substring-no-properties (1+ (point)) (line-end-position))))
                (pcase c
                  (?- (unless change-start (setq change-start old-line))
                      (push line removed)
                      (cl-incf old-line))
                  (?+ (unless change-start (setq change-start old-line))
                      (push line added))
                  (?\\ nil)                    ; "\ No newline at end of file"
                  (_ (flush) (cl-incf old-line))))
              (forward-line 1))
            (flush)))))
    (nreverse hunks)))

;;;; Drawing

(defvar-local efrit-inline-diff--overlays nil
  "The overlays of the preview shown in this buffer.")

(defun efrit-inline-diff--line-bounds (start line)
  "Buffer positions (BEG . END) of LINE (1-based from START), END after its newline."
  (save-excursion
    (goto-char start)
    (forward-line (1- line))
    (let ((beg (point)))
      (forward-line 1)
      (cons beg (point)))))

(defun efrit-inline-diff--added-string (lines)
  (mapconcat (lambda (l) (propertize (concat l "\n") 'face 'efrit-inline-diff-added)) lines ""))

(defun efrit-inline-diff-show (buffer start end new)
  "Show in BUFFER how START..END would read as NEW, with overlays.
Returns the number of hunks drawn.  Call `efrit-inline-diff-clear' to
take the preview down.  Text in the buffer is not changed."
  (with-current-buffer buffer
    (efrit-inline-diff-clear)
    (let* ((old (buffer-substring-no-properties start end))
           (hunks (efrit-inline-diff-hunks old new)))
      (dolist (h hunks)
        (let* ((old-start (plist-get h :old-start))
               (count (plist-get h :old-count))
               (added (plist-get h :added))
               (ov (if (> count 0)
                       ;; replaced or removed lines: cover them
                       (let ((b (efrit-inline-diff--line-bounds start old-start))
                             (e (efrit-inline-diff--line-bounds start (+ old-start count -1))))
                         (make-overlay (car b) (cdr e) buffer))
                     ;; pure insertion before old line OLD-START: a
                     ;; zero-width overlay at that line's start
                     (let ((b (efrit-inline-diff--line-bounds start old-start)))
                       (make-overlay (car b) (car b) buffer)))))
          (overlay-put ov 'efrit-inline-diff t)
          (overlay-put ov 'priority 100)
          (when (> count 0)
            ;; the strike-through is spelled out here as well as in the
            ;; face: a `defface' changed after first load keeps its old
            ;; spec in a running Emacs (tour 2026-09-30 saw red, no line)
            (overlay-put ov 'face '(efrit-inline-diff-removed (:strike-through t))))
          (when added
            ;; one string on the same overlay: order is guaranteed
            (overlay-put ov (if (> count 0) 'after-string 'before-string)
                         (efrit-inline-diff--added-string added)))
          (push ov efrit-inline-diff--overlays)))
      (length hunks))))

(defun efrit-inline-diff-clear (&optional buffer)
  "Remove the inline preview from BUFFER (default the current one)."
  (with-current-buffer (or buffer (current-buffer))
    (mapc #'delete-overlay efrit-inline-diff--overlays)
    (setq efrit-inline-diff--overlays nil)))

(defun efrit-inline-diff-active-p (&optional buffer)
  "Non-nil while BUFFER shows an inline preview."
  (and (buffer-local-value 'efrit-inline-diff--overlays (or buffer (current-buffer))) t))

(provide 'efrit-inline-diff)

;;; efrit-inline-diff.el ends here
