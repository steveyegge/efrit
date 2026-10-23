;;; efrit-pill.el --- Small SVG pills and badges for labels and states -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.12.36
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, ai, faces

;;; Commentary:

;; A PILL is a short text drawn on a rounded rectangle: the tab line's
;; agent tabs, the dashboard's states and folders, the diagnosis
;; severities, the review badges.  One primitive so they all match.
;;
;; `efrit-pill' returns TEXT with a `display' image.  The colour comes
;; from a KEY (the same key always gives the same hue, so a project or
;; an agent keeps its colour everywhere) or from an explicit STATE
;; (`efrit-pill-state-colors': working, waiting, unread, idle, failed,
;; ok, info, warning, blocked), or from BG/FG given outright.  Without
;; SVG (a terminal, batch) the same text comes back with a face, so
;; every caller can insert the result as is.
;;
;; `efrit-pill-function' lets a user with a pill drawer of their own
;; (tzz, 2026-10-14: "Use the same nice graphics I already have")
;; plug it in: it is called with the same keywords and its result is
;; used when non-nil.  Images are cached by their inputs and the cache
;; is dropped when the font metrics change.

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'svg)

(require 'dom)

(defgroup efrit-pill nil
  "Pills and badges."
  :group 'efrit)

(defcustom efrit-pill-function nil
  "A function that draws pills instead of `efrit-pill', or nil.
Called with (TEXT &key key bg fg scale help-echo); its non-nil result
is used as is.  For users who already have a pill drawer."
  :type '(choice (const nil) function)
  :group 'efrit-pill)

(defcustom efrit-pill-scale 0.9
  "Pill height as a fraction of the line height."
  :type 'number
  :group 'efrit-pill)

(defcustom efrit-pill-saturation 0.55
  "Saturation of the hue derived from a key."
  :type 'number
  :group 'efrit-pill)

(defcustom efrit-pill-background-lightness 0.86
  "Lightness of a pale (background) variant."
  :type 'number
  :group 'efrit-pill)

(defcustom efrit-pill-foreground-lightness 0.32
  "Lightness of a saturated (foreground) variant."
  :type 'number
  :group 'efrit-pill)

(defcustom efrit-pill-state-colors
  '((working . "#d98e04") (waiting . "#c0392b") (unread . "#6c3fc5") (idle . "#2e8b57")
    (failed . "#8b0000") (stopped . "#6e6e6e")
    (ok . "#2e8b57") (info . "#3b6ea5") (warning . "#d98e04") (blocked . "#b22222")
    (suggestion . "#3b6ea5") (comment . "#b7791f") (lgtm . "#2e8b57"))
  "The colour of each state a pill may show, as #rrggbb.
The pill takes a pale variant as background and the colour itself as text,
or the reverse when `:strong' is asked for."
  :type '(alist :key-type symbol :value-type string)
  :group 'efrit-pill)

(defface efrit-pill-fallback '((t :inherit bold :box (:line-width -1)))
  "A pill where no image can be drawn." :group 'efrit-pill)

(defvar efrit-pill--cache (make-hash-table :test 'equal))
(defvar efrit-pill--cache-key nil)

;; A reload of this file (efrit-reload, tzz-reload) must not keep
;; serving images drawn by the old code: the cache keys carry only
;; text and colours, not the drawing (2026-10-14, the new e did not
;; show after a reload).
(clrhash efrit-pill--cache)
(setq efrit-pill--cache-key nil)

(defun efrit-pill-flush ()
  "Drop every cached pill and logo image so the next draw uses the current code."
  (interactive)
  (clrhash efrit-pill--cache)
  (setq efrit-pill--cache-key nil)
  (when (fboundp 'efrit-agent-spinner-reset-cache) (efrit-agent-spinner-reset-cache))
  (when (fboundp 'tab-line-force-update) (tab-line-force-update t))
  (force-mode-line-update t))

(defun efrit-pill-available-p ()
  "Non-nil when pills can be drawn as images here."
  (and (display-graphic-p) (image-type-available-p 'svg)))

(defun efrit-pill--rgb (color)
  "COLOR (#rrggbb or #rgb, or a name) as (R G B) in 0..1.
Hex is parsed here: `color-name-to-rgb' consults the display and in
batch returns wrong values for hex colours."
  (cond
   ((and (stringp color) (string-match "\\`#\\([0-9a-fA-F]\\{6\\}\\)\\'" color))
    (mapcar (lambda (i) (/ (string-to-number (substring color (1+ (* 2 i)) (+ 3 (* 2 i))) 16) 255.0)) '(0 1 2)))
   ((and (stringp color) (string-match "\\`#\\([0-9a-fA-F]\\{3\\}\\)\\'" color))
    (mapcar (lambda (i) (/ (string-to-number (substring color (1+ i) (+ 2 i)) 16) 15.0)) '(0 1 2)))
   (t (or (color-name-to-rgb color) '(0.5 0.5 0.5)))))

(defun efrit-pill--hex (rgb)
  (apply #'color-rgb-to-hex (append rgb '(2))))

(defun efrit-pill-hue-color (key &optional light)
  "A stable colour for KEY as #rrggbb; LIGHT gives the pale variant."
  (let ((h (/ (mod (sxhash-equal key) 360) 360.0)))
    (efrit-pill--hex (color-hsl-to-rgb h efrit-pill-saturation
                                       (if light efrit-pill-background-lightness
                                         efrit-pill-foreground-lightness)))))

(defun efrit-pill-state-color (state &optional light)
  "The colour of STATE; LIGHT gives a pale variant of it.  Nil for an unknown state."
  (when-let* ((hex (alist-get state efrit-pill-state-colors)))
    (if (not light) hex
      (pcase-let* ((`(,r ,g ,b) (efrit-pill--rgb hex))
                   (`(,h ,s ,_l) (color-rgb-to-hsl r g b)))
        (efrit-pill--hex (color-hsl-to-rgb h (* 0.9 s) efrit-pill-background-lightness))))))

(defun efrit-pill--reset-cache-if-needed ()
  (let ((key (cons (default-font-height) (default-font-width))))
    (unless (equal key efrit-pill--cache-key)
      (clrhash efrit-pill--cache)
      (setq efrit-pill--cache-key key))))

(defcustom efrit-pill-style 'raised
  "How pills are drawn.
`raised': a slight top-to-bottom gradient, a soft shadow beneath and a
hairline highlight along the top, so the pill stands off its surround;
the text is flat on top at full contrast.  `flat': one colour."
  :type '(choice (const raised) (const flat))
  :group 'efrit-pill)

(defun efrit-pill--shade (color amount)
  "COLOR lightened (AMOUNT > 0) or darkened (AMOUNT < 0) in HSL lightness."
  (pcase-let* ((`(,r ,g ,b) (efrit-pill--rgb color))
               (`(,h ,s ,l) (color-rgb-to-hsl r g b)))
    (efrit-pill--hex (color-hsl-to-rgb h s (max 0.0 (min 1.0 (+ l amount)))))))

(defun efrit-pill--lozenge-path (x y w h)
  "Path data of a stadium at X Y, W by H: explicit arcs at both ends.
Some renderers drop `rx' on small `rect's; arcs always draw."
  (let ((r (/ h 2.0)))
    (format "M%.2f,%.2f H%.2f A%.2f,%.2f 0 0 1 %.2f,%.2f H%.2f A%.2f,%.2f 0 0 1 %.2f,%.2f Z"
            (+ x r) y (- (+ x w) r)
            r r (- (+ x w) r) (+ y h)
            (+ x r)
            r r (+ x r) y)))

(defvar efrit-pill--id 0 "Counter for unique ids within SVG documents.")

(defun efrit-pill-draw-lozenge (svg x y w h fill &optional style)
  "Append to SVG a stadium at X Y, W by H in FILL, per STYLE (default `efrit-pill-style').
`raised': a `linearGradient' lighter at the top and darker at the
bottom (within a few percent of FILL's lightness, so text on it keeps
its contrast), a blurred darker copy offset downward as a shadow, and
a near-white hairline along the upper arc.  `flat': FILL alone.  Draw
text afterwards; it sits flat on top.  Shared with tzz-pill and
nngmail so every pill on the screen is the same material."
  (pcase (or style efrit-pill-style)
    ('flat (svg-node svg 'path :d (efrit-pill--lozenge-path x y w h) :fill fill))
    (_
     (let* ((n (cl-incf efrit-pill--id))
            (grad (format "efrit-pill-g%d" n))
            (blur (format "efrit-pill-b%d" n))
            (defs (or (car (dom-by-tag svg 'defs))
                      (let ((d (dom-node 'defs))) (svg--append svg d) d)))
            (dark-pill (< (nth 2 (apply #'color-rgb-to-hsl (efrit-pill--rgb fill))) 0.5))
            (light (efrit-pill--shade fill (if dark-pill 0.14 0.07)))
            (dark (efrit-pill--shade fill (if dark-pill -0.10 -0.16)))
            (shadow (efrit-pill--shade fill -0.55))
            (highlight-opacity (if dark-pill "0.25" "0.6")))
       (dom-append-child
        defs (dom-node 'linearGradient `((id . ,grad) (x1 . "0") (y1 . "0") (x2 . "0") (y2 . "1"))
                       (dom-node 'stop `((offset . "0%") (stop-color . ,light)))
                       (dom-node 'stop `((offset . "100%") (stop-color . ,dark)))))
       (dom-append-child
        defs (dom-node 'filter `((id . ,blur) (x . "-10%") (y . "-30%") (width . "120%") (height . "180%"))
                       (dom-node 'feGaussianBlur `((stdDeviation . ,(format "%.2f" (* 0.09 h)))))))
       (svg-node svg 'path :d (efrit-pill--lozenge-path x (+ y (* 0.12 h)) w h)
                 :fill shadow :fill-opacity "0.6" :filter (format "url(#%s)" blur))
       (svg-node svg 'path :d (efrit-pill--lozenge-path x y w h) :fill (format "url(#%s)" grad))
       (let ((r (/ h 2.0)) (inset (* 0.08 h)))
         (svg-node svg 'path
                   :d (format "M%.2f,%.2f H%.2f" (+ x r) (+ y inset) (- (+ x w) r))
                   :fill "none" :stroke "#ffffff" :stroke-opacity highlight-opacity
                   :stroke-width (format "%.2f" (max 0.8 (* 0.06 h))) :stroke-linecap "round"))))))

(defun efrit-pill--draw (text bg fg scale &optional dot)
  "TEXT as a pill image string with BG and FG colours at SCALE.
DOT non-nil draws a disc inside the pill, before the text: red when
DOT is `filled' (the agent needs you), a dimple in the background
colour otherwise.  The pill itself is always filled (tzz, 2026-10-14)."
  (let* ((lh (default-font-height))
         ;; Raised pills leave room below for the shadow (12% of the pill).
         (height (max 12 (round (* scale lh (if (eq efrit-pill-style 'raised) 0.86 1.0)))))
         (font-size (round (* 0.62 height)))
         (family (face-attribute 'default :family))
         (dot-d (and dot (round (* 0.6 height))))
         (text-only (string-empty-p text))
         (dot-space (if dot (+ dot-d (if text-only 0 (round (* 0.35 font-size)))) 0))
         (width (+ (* (string-width text) (round (* 0.66 font-size))) (default-font-width) dot-space))
         ;; The canvas is the full line height with the pill inset, so
         ;; the shadow has room and pills of different scales centre.
         (top (if (eq efrit-pill-style 'raised)
                  (* 0.4 (- lh height))      ; a little high: the shadow falls below
                (/ (- lh height) 2.0)))
         (svg (svg-create width lh)))
    (dom-set-attribute svg 'viewBox (format "0 0 %s %s" width lh))
    (efrit-pill-draw-lozenge svg 0 top width height bg)
    (when dot
      ;; the mark: a solid embossed disc, red when the agent needs you,
      ;; a background-coloured dimple otherwise (tzz, 2026-10-14: a
      ;; drawn e did not read well at tab size)
      (efrit-pill--dot svg (/ (default-font-width) 2.0) (+ top (/ (- height dot-d) 2.0)) dot-d bg
                       (max 0.6 (/ dot-d 16.0)) (eq dot 'filled)))
    (unless text-only
      ;; Baseline nudged by 0.35 em rather than `dominant-baseline',
      ;; which some renderers ignore; this centres the x-height.
      (svg-text svg text :x (+ (/ (+ width dot-space) 2.0)) :y (+ top (* height 0.5) (* font-size 0.35))
                :text-anchor "middle"
                :font-family family :font-weight "bold" :font-size font-size :fill fg))
    (propertize (if text-only "●" text) 'display (svg-image svg :ascent 'center :scale 1))))

(defcustom efrit-pill-e-alert-color "#d62828"
  "The attention dot when the agent needs you: red, whatever the pill's colour."
  :type 'string
  :group 'efrit-pill)

(defun efrit-pill--frame-background ()
  "The frame's background as #rrggbb (the default face's), #000000 when unknown."
  (let ((bg (face-attribute 'default :background nil t)))
    (if (and (stringp bg) (not (string= bg "unspecified-bg")))
        (efrit-pill--hex (efrit-pill--rgb bg))
      "#000000")))

(defun efrit-pill--dot (svg x y size color width alert)
  "Draw a disc on SVG at X Y in a SIZE square.
With ALERT it is `efrit-pill-e-alert-color' (red), embossed.  Otherwise
it is a dimple: the frame background colour at the centre, fading to
COLOR (the pill's colour) at the rim, so it signals nothing and sits
well on any pill, red ones included (tzz, 2026-10-15: \"draw the circle
in the background color, with a gentle embossing gradient from the
pill color\").  WIDTH is the edge ring."
  (let* ((cx (+ x (/ size 2.0))) (cy (+ y (/ size 2.0))) (r (* size 0.42))
         (bg (efrit-pill--frame-background))
         (id (format "efrit-dot-%s" (substring (md5 (format "%s%s%s" color bg alert)) 0 8))))
    (if alert
        (let* ((tone efrit-pill-e-alert-color)
               (light (efrit-pill--mix tone "#ffffff" 0.5))
               (dark (efrit-pill--mix tone "#000000" 0.4)))
          (svg--def svg (dom-node 'radialGradient `((id . ,id) (cx . "35%") (cy . "30%") (r . "70%"))
                                  (dom-node 'stop `((offset . "0%") (stop-color . ,light)))
                                  (dom-node 'stop `((offset . "60%") (stop-color . ,tone)))
                                  (dom-node 'stop `((offset . "100%") (stop-color . ,dark)))))
          (svg-circle svg cx cy r :fill (format "url(#%s)" id) :stroke dark :stroke-width width))
      ;; the dimple: background in the middle, the pill colour creeping
      ;; in at the rim, a little more from below (light from above)
      (svg--def svg (dom-node 'radialGradient `((id . ,id) (cx . "40%") (cy . "35%") (r . "65%"))
                              (dom-node 'stop `((offset . "0%") (stop-color . ,bg)))
                              (dom-node 'stop `((offset . "60%") (stop-color . ,bg)))
                              (dom-node 'stop `((offset . "85%") (stop-color . ,(efrit-pill--mix bg color 0.35))))
                              (dom-node 'stop `((offset . "100%") (stop-color . ,(efrit-pill--mix bg color 0.8))))))
      (svg-circle svg cx cy r :fill (format "url(#%s)" id)
                  :stroke (efrit-pill--mix color bg 0.3) :stroke-width (* 0.7 width)))))

(defun efrit-pill--mix (a b amount)
  "Colour A moved AMOUNT (0..1) towards colour B, as #rrggbb."
  (pcase-let ((`(,r1 ,g1 ,b1) (efrit-pill--rgb a))
              (`(,r2 ,g2 ,b2) (efrit-pill--rgb b)))
    (efrit-pill--hex (list (+ r1 (* amount (- r2 r1))) (+ g1 (* amount (- g2 g1))) (+ b1 (* amount (- b2 b1)))))))

(cl-defun efrit-pill (text &key key state strong dot bg fg (scale efrit-pill-scale) help-echo)
  "TEXT drawn as a pill.
KEY picks a stable hue (default TEXT); STATE picks a colour from
`efrit-pill-state-colors' instead; STRONG swaps to a saturated
background with pale text (the selected tab, an alarm); DOT puts a
disc inside the pill before the text: red when `filled' (needs you),
a dimple in the background colour otherwise.  BG and FG
override the colours.  SCALE is the height relative to the line.
HELP-ECHO becomes the tooltip.  Without SVG, TEXT with a face (and
● in front when DOT)."
  (or (and efrit-pill-function (not dot)
           (ignore-errors (funcall efrit-pill-function text :key key :bg bg :fg fg :scale scale :help-echo help-echo)))
      (progn
        (efrit-pill--reset-cache-if-needed)
        (let* ((key (or key text))
               (pale (or (and state (efrit-pill-state-color state t)) (efrit-pill-hue-color key t)))
               (deep (or (and state (efrit-pill-state-color state)) (efrit-pill-hue-color key)))
               (bg (or bg (if strong deep pale)))
               (fg (or fg (if strong pale deep)))
               (cache-key (list text bg fg scale dot efrit-pill-style))
               (drawn (or (gethash cache-key efrit-pill--cache)
                          (puthash cache-key
                                   (if (efrit-pill-available-p)
                                       (efrit-pill--draw text bg fg scale dot)
                                     (propertize (if (string-empty-p text) "●" (concat (if dot "● " "") text))
                                                 'face (list :inherit 'efrit-pill-fallback :foreground deep)))
                                   efrit-pill--cache))))
          (if help-echo (propertize drawn 'help-echo help-echo) (copy-sequence drawn))))))

(defun efrit-pill-state (state &optional text)
  "A pill naming STATE (or TEXT) in STATE's colour."
  (efrit-pill (or text (symbol-name state)) :state state))

(provide 'efrit-pill)

;;; efrit-pill.el ends here
