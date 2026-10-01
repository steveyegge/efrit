;;; efrit-text-window.el --- Text around a point, cut on whole lines -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.5.3
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, ai

;;; Commentary:

;; Whenever efrit sends the model "the text around here", it has a
;; character budget and a point (or a region) to centre on.
;; `efrit-text-window' splits the budget between the text before and
;; the text after by a ratio, handles the three cases (the start of
;; the buffer fits, the end fits, the middle needs both sides cut),
;; and never hands the model half a line: a cut side loses its partial
;; boundary line.  After minuet's `minuet--get-context' (2026-09-28).
;;
;; `efrit-text-window-header' is the one-line description of the
;; buffer's language and indentation, built from `comment-start',
;; `indent-tabs-mode' and `tab-width', for prompts that ask for code.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defcustom efrit-text-window-chars 16000
  "Default character budget of a text window."
  :type 'integer
  :group 'efrit)

(defcustom efrit-text-window-ratio 0.75
  "Share of the budget given to the text before the point of interest.
The rest goes after it.  When one side is short, the other gets the
slack."
  :type 'float
  :group 'efrit)

(defun efrit-text-window--whole-lines-before (text)
  "TEXT without its first line when TEXT does not start at a line start."
  (if-let* ((nl (string-search "\n" text)))
      (substring text (1+ nl))
    ""))

(defun efrit-text-window--whole-lines-after (text)
  "TEXT without its last line when TEXT does not end at a line end."
  (let ((nl (cl-position ?\n text :from-end t)))
    (if nl (substring text 0 (1+ nl)) "")))

(cl-defun efrit-text-window (&key (start (point)) (end start)
                                  (chars efrit-text-window-chars)
                                  (ratio efrit-text-window-ratio)
                                  (buffer (current-buffer)))
  "The text around START..END of BUFFER within CHARS characters.
Returns a plist: :before and :after are the strings on each side,
whole lines only; :before-cut and :after-cut say whether that side
was cut; :before-start and :after-end are the buffer positions the
strings begin and end at.  The region START..END itself is not
included and not counted.

RATIO of CHARS goes before START.  If the text before START is
shorter than its share, the text after END gets the rest, and the
other way round.  A side that does not fit loses its partial boundary
line, so the model never sees half a line."
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (let* ((before-len (- start (point-min)))
             (after-len (- (point-max) end))
             (want-before (floor (* chars ratio)))
             (want-after (- chars want-before))
             before-take after-take)
        (cond
         ;; everything fits
         ((<= (+ before-len after-len) chars)
          (setq before-take before-len after-take after-len))
         ;; the start fits: give the slack to the end
         ((<= before-len want-before)
          (setq before-take before-len after-take (- chars before-len)))
         ;; the end fits: give the slack to the start
         ((<= after-len want-after)
          (setq after-take (min after-len want-after) before-take (- chars after-take)))
         (t (setq before-take want-before after-take want-after)))
        (setq before-take (min before-take before-len)
              after-take (min after-take after-len))
        (let* ((before-cut (< before-take before-len))
               (after-cut (< after-take after-len))
               (before-start (- start before-take))
               (after-end (+ end after-take))
               (before (buffer-substring-no-properties before-start start))
               (after (buffer-substring-no-properties end after-end)))
          ;; Whole lines only.  When the cut side is one long line
          ;; (minified text, a single-line region) dropping the
          ;; partial line would drop everything: keep the raw cut then.
          (when before-cut
            (let ((whole (efrit-text-window--whole-lines-before before)))
              (unless (string-empty-p whole)
                (setq before-start (- start (length whole)) before whole))))
          (when after-cut
            (let ((whole (efrit-text-window--whole-lines-after after)))
              (unless (string-empty-p whole)
                (setq after-end (+ end (length whole)) after whole))))
          (list :before before :after after
                :before-cut before-cut :after-cut after-cut
                :before-start before-start :after-end after-end))))))

(defun efrit-text-window-header (&optional buffer)
  "One line naming BUFFER's language and indentation, as a comment.
For example \"# language: python, indentation: 4 spaces\" or
\";; language: emacs-lisp, indentation: 2 spaces\".  In a buffer
without `comment-start' the line has no comment leader."
  (with-current-buffer (or buffer (current-buffer))
    (let* ((lang (string-remove-suffix "-mode" (symbol-name major-mode)))
           (lang (string-remove-suffix "-ts" lang))
           (indent (if indent-tabs-mode
                       (format "tabs of width %d" tab-width)
                     (format "%d spaces" (efrit-text-window--indent-offset))))
           (leader (if (and (stringp comment-start) (not (string-empty-p comment-start)))
                       (concat (string-trim-right comment-start) " ")
                     "")))
      (format "%slanguage: %s, indentation: %s" leader lang indent))))

(defconst efrit-text-window--indent-vars
  '((python-mode . python-indent-offset) (python-ts-mode . python-indent-offset)
    (c-mode . c-basic-offset) (c++-mode . c-basic-offset) (java-mode . c-basic-offset)
    (c-ts-mode . c-ts-mode-indent-offset) (c++-ts-mode . c-ts-mode-indent-offset)
    (js-mode . js-indent-level) (js-ts-mode . js-indent-level)
    (typescript-ts-mode . typescript-ts-mode-indent-offset)
    (sh-mode . sh-basic-offset) (bash-ts-mode . sh-basic-offset)
    (ruby-mode . ruby-indent-level) (rust-mode . rust-indent-offset)
    (rust-ts-mode . rust-ts-mode-indent-offset) (go-ts-mode . go-ts-mode-indent-offset)
    (css-mode . css-indent-offset) (perl-mode . perl-indent-level)
    (cperl-mode . cperl-indent-level) (yaml-mode . yaml-indent-offset))
  "Indentation variable per major mode (the mode or a parent).")

(defun efrit-text-window--indent-offset ()
  "The indentation step of the current buffer, best effort.
The mode's own variable when the mode has one (a global value of some
other mode's variable says nothing about this buffer), else 2 for
Lisp, else `standard-indent'."
  (or (cl-loop for (mode . var) in efrit-text-window--indent-vars
               when (and (derived-mode-p mode) (boundp var) (integerp (symbol-value var)))
               return (symbol-value var))
      (and (derived-mode-p 'lisp-data-mode 'emacs-lisp-mode 'lisp-mode) 2)
      (and (boundp 'standard-indent) (integerp standard-indent) standard-indent)
      4))

(provide 'efrit-text-window)

;;; efrit-text-window.el ends here
