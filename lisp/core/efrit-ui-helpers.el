;;; efrit-ui-helpers.el --- Small blocking UI primitives -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; Two primitives used by the permission prompt and elsewhere:
;;
;; - `efrit-edit-in-buffer': edit arbitrary text in a real buffer with
;;   a mode, blocking via `recursive-edit', returning the text or
;;   signalling `quit' on cancel.  (copilot-chat's edit-in-buffer.)
;; - `efrit-show-preview': show a -/+ or plain text preview in a
;;   pop-up window fitted to content, returning the window so the
;;   caller can delete it after a prompt.
;;
;; And a string helper: `efrit-fence-for' returns a code fence longer
;; than any backtick run in the text, so model output can be embedded
;; in a prompt without breaking out of it.
;;
;; `efrit-ui-badge' is a small coloured pill (an SVG image over a plain
;; word) for severities and verdicts in reports; on a text display, or
;; when the properties are stripped, the word itself remains.

;;; Code:

(require 'subr-x)
(require 'color)
(declare-function svg-create "svg")
(declare-function svg-rectangle "svg")
(declare-function svg-text "svg")
(declare-function svg-image "svg")

(defvar efrit-edit-in-buffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'exit-recursive-edit)
    (define-key map (kbd "C-c C-k") #'abort-recursive-edit)
    map)
  "Keys active in `efrit-edit-in-buffer'.")

(defun efrit-edit-in-buffer (text description &optional mode)
  "Let the user edit TEXT in a buffer described by DESCRIPTION; return the result.
MODE, if non-nil, is a major-mode function to enable (e.g.
`emacs-lisp-mode').  Blocks in a recursive edit: \\`C-c C-c' returns
the buffer contents, \\`C-c C-k' signals `quit'.  The buffer and its
window are cleaned up either way."
  (let ((buf (generate-new-buffer (format "*efrit edit: %s*" description))))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (insert (or text ""))
            (goto-char (point-min))
            (when (and mode (fboundp mode))
              (condition-case nil (funcall mode) (error nil)))
            (use-local-map (make-composed-keymap efrit-edit-in-buffer-map
                                                 (current-local-map)))
            (setq header-line-format
                  (substitute-command-keys
                   (format "Edit %s, then \\[exit-recursive-edit] to use it, \\[abort-recursive-edit] to cancel"
                           description))))
          (pop-to-buffer buf)
          (recursive-edit)
          (with-current-buffer buf (buffer-string)))
      (when-let* ((w (get-buffer-window buf t)))
        (ignore-errors (quit-window nil w)))
      (kill-buffer buf))))

(define-derived-mode efrit-preview-mode special-mode "Efrit-Preview"
  "Read-only popup for previews and details.
\\<special-mode-map>\\[quit-window] dismisses it and removes the window."
  (setq buffer-read-only t))

(defvar efrit-preview-keys-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keys layered on any preview whose major mode lacks a quit binding.
`diff-mode' and other non-special modes get `q' this way; modes that
already bind it (special-mode descendants) win because the composed
map puts the mode map first.")

(defun efrit-show-preview (name text &optional mode)
  "Display TEXT in a popup buffer NAME, in a window fitted to its size.
TEXT is a string, or a function called in the emptied buffer to insert
the content itself (for reports with faces, images, and buttons).
MODE is an optional major mode (e.g. `diff-mode'); without it the
buffer is in `efrit-preview-mode'.  Either way the buffer is
read-only and `q' dismisses it: the window is deleted, not left
showing another buffer, because it is displayed as a popup with
`quit-restore' set by `display-buffer'.  Returns the window.  The
buffer is reused across calls."
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (if (functionp text) (funcall text) (insert text))
        (goto-char (point-min)))
      (let ((wanted (if (and mode (fboundp mode)) mode #'efrit-preview-mode)))
        (unless (eq major-mode wanted)
          (condition-case nil (funcall wanted) (error (efrit-preview-mode)))))
      (unless (derived-mode-p 'special-mode)
        (use-local-map (make-composed-keymap (current-local-map) efrit-preview-keys-map)))
      (setq buffer-read-only t))
    (let ((win (display-buffer
                buf
                '((display-buffer-reuse-window display-buffer-at-bottom)
                  (window-height . fit-window-to-buffer)
                  (dedicated . t)))))
      (when (window-live-p win)
        (fit-window-to-buffer win (/ (frame-height) 2) 4))
      win)))

(defun efrit-show-popup (name text &optional mode)
  "Like `efrit-show-preview' but select the popup so `q' works at once.
For popups the user reads and dismisses (details, listings), not for
previews shown beside a prompt that is still reading keys."
  (let ((win (efrit-show-preview name text mode)))
    (when (window-live-p win) (select-window win))
    win))

(defun efrit-ui-badge-available-p ()
  "Non-nil when an SVG badge can be displayed here."
  (and (display-graphic-p) (image-type-available-p 'svg)))

(defun efrit-ui--face-hex (face attribute)
  "FACE's ATTRIBUTE as #rrggbb, resolving inheritance; grey when unknown."
  (let* ((name (let ((v (face-attribute face attribute nil t)))
                 (if (stringp v) v (face-attribute 'default attribute nil t))))
         (rgb (and (stringp name)
                   (condition-case nil (color-name-to-rgb name) (error nil)))))
    (if rgb (apply #'color-rgb-to-hex (append rgb '(2))) "#808080")))

(defun efrit-ui-badge (text face)
  "TEXT as a small pill in FACE's colours: a string with an image on it.
The image is a `display' property over the plain TEXT, so a text
terminal, `buffer-substring-no-properties', and the kill ring all see
the word.  FACE's :background is the pill, :foreground the letters;
without SVG the string carries FACE instead."
  (let ((word (propertize text 'face face)))
    (if (not (efrit-ui-badge-available-p))
        word
      (require 'svg)
      (let* ((height (max 12 (round (* 1.05 (default-font-height)))))
             (font-size (round (* 0.62 height)))
             (family (face-attribute 'default :family))
             (char-w (default-font-width))
             (width (+ (* (length text) (round (* 0.66 font-size))) char-w))
             (svg (svg-create width height)))
        (svg-rectangle svg 0 1 width (- height 2) :rx (/ height 2.0)
                       :fill (efrit-ui--face-hex face :background))
        (svg-text svg text
                  :x (/ width 2.0) :y (/ height 2.0)
                  :text-anchor "middle" :dominant-baseline "central"
                  :font-family family :font-weight "bold" :font-size font-size
                  :fill (efrit-ui--face-hex face :foreground))
        (propertize word 'display (svg-image svg :ascent 'center :scale 1))))))

(defun efrit-fence-for (text)
  "Return a backtick fence longer than any run of backticks in TEXT.
Minimum three.  Use as both the opening and closing fence when
embedding TEXT in markdown so it cannot terminate the block early."
  (let ((longest 0) (start 0))
    (while (string-match "`+" (or text "") start)
      (setq longest (max longest (- (match-end 0) (match-beginning 0)))
            start (match-end 0)))
    (make-string (max 3 (1+ longest)) ?`)))

(provide 'efrit-ui-helpers)

;;; efrit-ui-helpers.el ends here
