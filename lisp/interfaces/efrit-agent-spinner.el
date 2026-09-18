;;; efrit-agent-spinner.el --- Quiet SVG activity indicator -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; A small rotating arc drawn with `svg.el', sized to the line height,
;; that the agent buffer shows while a request is in flight.  It sits
;; in the header line next to the status word and at the end of the
;; in-buffer "thinking" line.  One frame per tick of the existing
;; spinner timer; frames are cached per (size, colour, index), so a
;; tick is a hash lookup and a redisplay, not a rasterisation.
;;
;; Without SVG support (terminal, or Emacs built without librsvg) the
;; caller falls back to the braille frames in efrit-agent-core.

;;; Code:

(require 'svg)
(require 'color)
(require 'cl-lib)

(defvar efrit-agent--spinner-index)

(defgroup efrit-agent-spinner nil
  "Activity indicator for the agent buffer."
  :group 'efrit-agent
  :prefix "efrit-agent-spinner-")

(defcustom efrit-agent-spinner-steps 12
  "Frames per revolution.  With `efrit-agent-spinner-interval' 0.15 that is one turn every 1.8 s."
  :type 'integer
  :group 'efrit-agent-spinner)

(defface efrit-agent-spinner
  '((((background dark)) :foreground "#88c0d0")
    (t :foreground "#5e81ac"))
  "Colour of the spinner arc.  Only :foreground is used."
  :group 'efrit-agent-spinner)

(defun efrit-agent-spinner-available-p ()
  "Non-nil when an SVG spinner can be displayed here."
  (and (display-graphic-p) (image-type-available-p 'svg)))

(defun efrit-agent-spinner--hex (face &optional attribute)
  "FACE's ATTRIBUTE (default :foreground) as #rrggbb."
  (let* ((name (face-attribute face (or attribute :foreground) nil t))
         (rgb (and (stringp name)
                   (condition-case nil (color-name-to-rgb name) (error nil)))))
    (if rgb (apply #'color-rgb-to-hex (append rgb '(2))) "#808080")))

(defun efrit-agent-spinner--parse-hex (color)
  "COLOR \"#rrggbb\" as a list of three floats 0..1.
`color-name-to-rgb' is avoided: without a display it quantizes to the
terminal palette, which mangles blends."
  (if (and (stringp color) (string-match "\\`#\\([0-9a-fA-F]\\{2\\}\\)\\([0-9a-fA-F]\\{2\\}\\)\\([0-9a-fA-F]\\{2\\}\\)\\'" color))
      (mapcar (lambda (i) (/ (string-to-number (match-string i color) 16) 255.0)) '(1 2 3))
    '(0.5 0.5 0.5)))

(defun efrit-agent-spinner--blend (color bg alpha)
  "COLOR over BG (both #rrggbb) at ALPHA, as #rrggbb.
Opacity is baked into the colour so the arc looks the same in every
SVG renderer; librsvg would honour `stroke-opacity', others do not."
  (let ((c (efrit-agent-spinner--parse-hex color))
        (b (efrit-agent-spinner--parse-hex bg)))
    (apply #'format "#%02x%02x%02x"
           (cl-mapcar (lambda (x y) (round (* 255 (+ (* alpha x) (* (- 1 alpha) y))))) c b))))

(defun efrit-agent-spinner--arc (svg cx cy r start sweep stroke width)
  "Draw on SVG an arc of radius R around CX,CY from angle START over SWEEP degrees."
  (let* ((a0 (* float-pi (/ start 180.0)))
         (a1 (* float-pi (/ (+ start sweep) 180.0)))
         (x0 (+ cx (* r (cos a0)))) (y0 (+ cy (* r (sin a0))))
         (x1 (+ cx (* r (cos a1)))) (y1 (+ cy (* r (sin a1))))
         (large (if (> sweep 180) 1 0)))
    (svg-node svg 'path
              :d (format "M %.2f %.2f A %.2f %.2f 0 %d 1 %.2f %.2f" x0 y0 r r large x1 y1)
              :fill "none" :stroke stroke :stroke-width width
              :stroke-linecap "round")))

(defun efrit-agent-spinner--svg (size color index &optional bg)
  "The spinner frame INDEX as an SVG DOM, SIZE pixels square, in COLOR over BG."
  (let* ((svg (svg-create size size))
         (bg (or bg (efrit-agent-spinner--hex 'default :background)))
         (c (/ size 2.0))
         (w (max 1.2 (/ size 10.0)))
         (r (- c w 1))
         (angle (* index (/ 360.0 efrit-agent-spinner-steps))))
    ;; Faint full ring so the eye has a fixed reference, then a
    ;; three-quarter arc whose head is solid: reads as motion without
    ;; flicker.
    (svg-circle svg c c r :fill "none" :stroke-width w
                :stroke (efrit-agent-spinner--blend color bg 0.15))
    (efrit-agent-spinner--arc svg c c r angle 200 (efrit-agent-spinner--blend color bg 0.4) w)
    (efrit-agent-spinner--arc svg c c r (+ angle 200) 70 color w)
    svg))

(defvar efrit-agent-spinner--cache (make-hash-table :test #'equal)
  "(size color index) -> propertized one-character string with the image.")

(defun efrit-agent-spinner-frame (&optional index size)
  "A string displaying spinner frame INDEX (default the shared counter).
SIZE defaults to the frame's character height.  Returns nil when SVG
is unavailable so callers can fall back to text frames."
  (when (efrit-agent-spinner-available-p)
    (let* ((index (mod (or index efrit-agent--spinner-index) efrit-agent-spinner-steps))
           (size (or size (- (frame-char-height) 2)))
           (color (efrit-agent-spinner--hex 'efrit-agent-spinner))
           (bg (efrit-agent-spinner--hex 'default :background))
           (key (list size color bg index)))
      (or (gethash key efrit-agent-spinner--cache)
          (puthash key
                   (propertize " "
                               'display (svg-image (efrit-agent-spinner--svg size color index bg)
                                                   :ascent 'center :scale 1)
                               'rear-nonsticky t)
                   efrit-agent-spinner--cache)))))

(defun efrit-agent-spinner-reset-cache ()
  "Drop cached frames (after a theme or font change)."
  (clrhash efrit-agent-spinner--cache))

;;; Mode-line indicator
;;
;; A small construct for `global-mode-string': the spinner while any
;; agent buffer has a request in flight, nothing otherwise.  It is
;; visible from every window, so the user sees efrit is busy without
;; the agent buffer on screen.  The agent buffer's own spinner timer
;; already calls `force-mode-line-update' on each tick; this construct
;; reads that buffer's frame index, so it animates with no timer of
;; its own.

(defvar efrit-agent-buffer-name)
(defvar efrit-agent--thinking-label)
(defvar efrit-agent--spinner-index)
(defvar efrit-agent--spinner-frames-unicode)
(defvar efrit-agent--spinner-frames-ascii)
(defvar efrit-agent-display-style)

(defcustom efrit-agent-spinner-mode-line t
  "When non-nil, show a spinner in every mode line while efrit works."
  :type 'boolean
  :group 'efrit-agent-spinner)

(defun efrit-agent-spinner--busy-buffer ()
  "The agent buffer with a request in flight, or nil."
  (when (boundp 'efrit-agent-buffer-name)
    (let ((buf (get-buffer efrit-agent-buffer-name)))
      (and buf (buffer-live-p buf)
           (buffer-local-value 'efrit-agent--thinking-label buf)
           buf))))

(defun efrit-agent-spinner-mode-line-string ()
  "The mode-line text: the spinner frame while efrit works, else nil.
SVG on a graphic display, a text frame in a terminal.  Clicking it
shows the agent buffer."
  (when efrit-agent-spinner-mode-line
    (when-let* ((buf (efrit-agent-spinner--busy-buffer)))
      (let* ((index (buffer-local-value 'efrit-agent--spinner-index buf))
             (frame (or (efrit-agent-spinner-frame index)
                        (let ((frames (if (eq efrit-agent-display-style 'unicode)
                                          efrit-agent--spinner-frames-unicode
                                        efrit-agent--spinner-frames-ascii)))
                          (aref frames (mod index (length frames)))))))
        (propertize (concat " " frame " ")
                    'help-echo "efrit is working — mouse-1: show the agent buffer"
                    'mouse-face 'mode-line-highlight
                    'local-map (let ((m (make-sparse-keymap)))
                                 (define-key m [mode-line mouse-1]
                                             (lambda () (interactive) (pop-to-buffer buf)))
                                 m))))))

(defconst efrit-agent-spinner--mode-line-construct
  '(:eval (efrit-agent-spinner-mode-line-string)))

(defun efrit-agent-spinner-mode-line-install ()
  "Add the spinner construct to `global-mode-string' once."
  (unless (member efrit-agent-spinner--mode-line-construct global-mode-string)
    (setq global-mode-string
          (append (or global-mode-string '(""))
                  (list efrit-agent-spinner--mode-line-construct)))))

(defun efrit-agent-spinner-mode-line-remove ()
  "Remove the spinner construct from `global-mode-string'."
  (setq global-mode-string
        (delete efrit-agent-spinner--mode-line-construct global-mode-string)))

(efrit-agent-spinner-mode-line-install)

(provide 'efrit-agent-spinner)

;;; efrit-agent-spinner.el ends here
