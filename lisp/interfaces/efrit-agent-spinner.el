;;; efrit-agent-spinner.el --- Quiet SVG activity indicator -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.12.12
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; A small animated e drawn with `svg.el', sized to the line height,
;; that the agent buffer shows while a request is in flight.  It sits
;; in the header line next to the status word, at the start of the
;; in-buffer "thinking" line, and as the agent buffer's `mode-name'
;; (`efrit-agent-ring-mode-line-string').  One frame per tick of the
;; existing spinner timer; frames are cached per (size, colour,
;; index), so a tick is a hash lookup and a redisplay, not a
;; rasterisation.  There is no `global-mode-string' copy: a second
;; spinner at the right edge of the same mode line was noise (tzz,
;; 2026-10-20).
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

(defcustom efrit-agent-spinner-shape 'e
  "What the spinner draws: a lowercase e with a highlight running along its
edge, or the ring it replaced (tzz, 2026-10-03)."
  :type '(choice (const :tag "lowercase e" e) (const :tag "ring" ring))
  :group 'efrit-agent-spinner)

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

;;;; The e

;; A single-stroke lowercase e, as a pen would draw it: from the right
;; end of the crossbar, left along the bar, then counter-clockwise
;; around the bowl (up over the top, down the left side, under the
;; bottom) to the tail at the lower right.  Sampled as a polyline so a
;; highlight can be placed by arc length and the frame is one <path>
;; per segment, nothing clever that librsvg might not have.

(defun efrit-agent-spinner--e-points (size)
  "Points (X . Y) along the e's stroke, in drawing order, for a SIZE box.
The stroke runs along the crossbar from left to right, then round the
bowl from the bar's right end over the top, down the left and along
the bottom, ending in the tail at lower right -- the way a lowercase e
is written (2026-10-14: the earlier bowl ran the other way and the e
read mirrored once it was shown at rest)."
  (let* ((c (/ size 2.0))
         (r (* size 0.36))                ; bowl radius
         (bar-y c)                        ; the crossbar sits on the centre line
         (pts nil)
         (n 24))
    ;; crossbar, left to right
    (dotimes (i (1+ n))
      (let ((x (+ (- c r) (* i (/ (* 2 r) (float n))))))
        (push (cons x bar-y) pts)))
    ;; the bowl: angles with y down, so decreasing angles go over the
    ;; top.  From the bar's right end (0) through the top (-90), the
    ;; left (-180), the bottom (-270) to the tail at -315 (45 below the
    ;; horizontal on the right)
    (let* ((start 0.0) (end -315.0) (steps (* 3 n)))
      (dotimes (i (1+ steps))
        (let* ((a (* float-pi (/ (+ start (* i (/ (- end start) (float steps)))) 180.0))))
          (push (cons (+ c (* r (cos a))) (+ c (* r (sin a)))) pts))))
    (nreverse pts)))

(defun efrit-agent-spinner--polyline (svg pts stroke width)
  "Draw PTS on SVG as one path in STROKE of WIDTH."
  (when (cdr pts)
    (svg-node svg 'path
              :d (concat (format "M %.2f %.2f" (caar pts) (cdar pts))
                         (mapconcat (lambda (p) (format " L %.2f %.2f" (car p) (cdr p))) (cdr pts) ""))
              :fill "none" :stroke stroke :stroke-width width
              :stroke-linecap "round" :stroke-linejoin "round")))

(defun efrit-agent-spinner--e-svg (size color index bg)
  "Frame INDEX of the e spinner: the letter faint, a highlight running along it."
  (let* ((svg (svg-create size size))
         (w (max 1.2 (/ size 12.0)))
         (pts (efrit-agent-spinner--e-points size))
         (len (length pts))
         ;; the highlight: a head of ~a quarter of the stroke, a tail of
         ;; the same again at half strength, advancing one step per tick
         ;; around the whole letter and wrapping at the tail
         (head-len (max 3 (/ len 3)))
         (pos (floor (* len (/ index (float efrit-agent-spinner-steps)))))
         (take (lambda (from count)
                 (let (out)
                   (dotimes (i count)
                     (push (nth (mod (+ from i) len) pts) out))
                   (nreverse out)))))
    (efrit-agent-spinner--polyline svg pts (efrit-agent-spinner--blend color bg 0.18) w)
    ;; the e is not a closed curve: a highlight that wraps from the tail
    ;; to the bar's right end would draw a chord across the counter, so
    ;; split at the wrap point
    (let* ((tail-start (- pos head-len))
           (seg (lambda (from count stroke)
                  (let ((from (mod from len)) (count count))
                    (while (> count 0)
                      (let ((run (min count (- len from))))
                        (efrit-agent-spinner--polyline svg (funcall take from run) stroke w)
                        (setq from 0 count (- count run))))))))
      (funcall seg tail-start head-len (efrit-agent-spinner--blend color bg 0.45))
      (funcall seg pos head-len color))
    svg))

(defun efrit-agent-spinner--svg (size color index &optional bg)
  "The spinner frame INDEX as an SVG DOM, SIZE pixels square, in COLOR over BG."
  (let ((bg (or bg (efrit-agent-spinner--hex 'default :background))))
    (if (eq efrit-agent-spinner-shape 'e)
        (efrit-agent-spinner--e-svg size color index bg)
      (let* ((svg (svg-create size size))
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
        svg))))

(defvar efrit-agent-spinner--cache (make-hash-table :test #'equal)
  "(size color bg index shape) -> propertized one-character string with the image.")

(defun efrit-agent-spinner-still (&optional size color bg-face)
  "The e drawn whole in COLOR: the logo at rest, no highlight.
SIZE and BG-FACE as in `efrit-agent-spinner-frame'.  Nil without SVG."
  (when (efrit-agent-spinner-available-p)
    (let* ((size (or size (- (frame-char-height) 2)))
           (color (or color (efrit-agent-spinner--hex 'efrit-agent-spinner)))
           (bg (efrit-agent-spinner--hex (or bg-face 'default) :background))
           (key (list 'still size color bg)))
      (or (gethash key efrit-agent-spinner--cache)
          (puthash key
                   (let ((svg (svg-create size size)))
                     (efrit-agent-spinner--polyline svg (efrit-agent-spinner--e-points size) color
                                                    (max 1.2 (/ size 12.0)))
                     (propertize " " 'display (svg-image svg :ascent 'center :scale 1) 'rear-nonsticky t))
                   efrit-agent-spinner--cache)))))

(defun efrit-agent-spinner-frame (&optional index size color bg-face)
  "A string displaying spinner frame INDEX (default the shared counter).
SIZE defaults to the frame's character height; COLOR (#rrggbb) to the
`efrit-agent-spinner' face; the faint strokes are blended towards
BG-FACE's background (default `default'; `mode-line' for a mode line).
Returns nil when SVG is unavailable so callers can fall back to text."
  (when (efrit-agent-spinner-available-p)
    (let* ((index (mod (or index efrit-agent--spinner-index) efrit-agent-spinner-steps))
           (size (or size (- (frame-char-height) 2)))
           (color (or color (efrit-agent-spinner--hex 'efrit-agent-spinner)))
           (bg (efrit-agent-spinner--hex (or bg-face 'default) :background))
           (key (list size color bg index efrit-agent-spinner-shape)))
      (or (gethash key efrit-agent-spinner--cache)
          (puthash key
                   (propertize " "
                               'display (svg-image (efrit-agent-spinner--svg size color index bg)
                                                   :ascent 'center :scale 1)
                               'rear-nonsticky t)
                   efrit-agent-spinner--cache)))))

(defun efrit-agent-spinner-reset-cache ()
  "Drop cached frames (after a theme or font change, or a reload)."
  (clrhash efrit-agent-spinner--cache))

;; A reload must not keep serving frames drawn by the old code (the
;; keys carry size and colours, not the geometry; 2026-10-14 the
;; mirrored e survived two reloads this way).
(efrit-agent-spinner-reset-cache)

(provide 'efrit-agent-spinner)

;;; efrit-agent-spinner.el ends here
