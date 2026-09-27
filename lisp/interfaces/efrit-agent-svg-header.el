;;; efrit-agent-svg-header.el --- Graphical header for the agent buffer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; A two-line SVG header for `efrit-agent-mode', after agent-shell's
;; graphical header:
;;
;;   +------+
;;   |  ef  |  efrit ➤ claude-sonnet-4-5 ➤ 29k/200k ▃
;;   |      |  ~/src/proj ➤ ⠋ Working 0:42 ➤ 3 tools
;;   +------+
;;
;; Set `efrit-agent-header-style' to `graphical' (the default on a
;; graphical display with SVG support) or `text' (the original
;; single-line header).
;;
;; Everything drawn comes from a *header model*: a plain alist of the
;; values that can appear.  The model doubles as a cache key, so the
;; 8 Hz spinner tick only re-rasterises when a glyph, count or status
;; actually changed; identical models return the cached image.
;;
;; Colours are taken from the faces the text header already uses, via
;; `face-attribute' with inheritance, and converted to #rrggbb because
;; librsvg does not know X11 colour names (agent-shell learned this
;; the hard way: `Green3' renders black).  The font family and pixel
;; size are the frame's, so `string-pixel-width' predicts what librsvg
;; draws and the canvas can be shrunk to fit.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'svg)
(require 'dom)
(require 'color)
(require 'efrit-log)

(defvar efrit-agent--status)
(defvar efrit-agent--thinking-label)
(defvar efrit-agent--start-time)
(defvar efrit-agent--activities)
(defvar efrit-agent--repl-session)
(defvar efrit-agent--session-id)
(defvar efrit-agent-display-mode)
(defvar efrit-default-model)
(declare-function efrit-agent--status-string "efrit-agent-core")
(declare-function efrit-agent--format-elapsed "efrit-agent-core")
(require 'efrit-agent-spinner)
(defvar efrit-agent--spinner-index)
(declare-function efrit-agent--format-header-line "efrit-agent-render")
(declare-function efrit-agent--usage-segment "efrit-agent-render")
(declare-function efrit-agent-input-hint "efrit-agent-render")
(declare-function efrit-repl-session-id "efrit-repl-session")
(declare-function efrit-tool--get-project-root "efrit-tool-utils")

(defgroup efrit-agent-header nil
  "Agent buffer header."
  :group 'efrit-agent
  :prefix "efrit-agent-header-")

(defcustom efrit-agent-header-style
  (if (and (display-graphic-p) (image-type-available-p 'svg)) 'graphical 'text)
  "How the agent buffer header is drawn.
`graphical' is a two-line SVG with an icon; `text' is the one-line
propertized string; `none' hides the header."
  :type '(choice (const graphical) (const text) (const none))
  :group 'efrit-agent-header)

(defcustom efrit-agent-header-icon-text "ef"
  "Text drawn in the header icon tile."
  :type 'string
  :group 'efrit-agent-header)

(defface efrit-agent-header-name
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face for the efrit name in the graphical header."
  :group 'efrit-agent-header)

(defface efrit-agent-header-model
  '((t :inherit font-lock-type-face))
  "Face for the model name in the graphical header."
  :group 'efrit-agent-header)

(defface efrit-agent-header-directory
  '((t :inherit font-lock-comment-face))
  "Face for the project directory in the graphical header."
  :group 'efrit-agent-header)

(defface efrit-agent-header-hint
  '((t :inherit shadow))
  "Face for the key hint (RET sends, S-RET newline) in the header."
  :group 'efrit-agent-header)

(defface efrit-agent-header-border
  '((((background dark)) :foreground "#4c566a")
    (t :foreground "#c0c6d0"))
  "Colour (foreground) of the rule drawn along the bottom of the header."
  :group 'efrit-agent-header)

(defface efrit-agent-header-icon
  '((((background dark)) :background "#3b4252" :foreground "#eceff4")
    (t :background "#d8dee9" :foreground "#2e3440"))
  "Colours for the icon tile (background = tile, foreground = glyph)."
  :group 'efrit-agent-header)

;;; Colours and metrics

(defun efrit-agent-svg--hex (face &optional attribute)
  "FACE's ATTRIBUTE (default :foreground) as #rrggbb, resolving inheritance."
  (let* ((attr (or attribute :foreground))
         (name (let ((v (face-attribute face attr nil t)))
                 (if (stringp v) v (face-attribute 'default attr nil t))))
         (rgb (and (stringp name) (color-name-to-rgb name))))
    (if rgb (apply #'color-rgb-to-hex (append rgb '(2))) "#808080")))

(defcustom efrit-agent-header-text-scale 1.0
  "Multiplier on the header's text size relative to the buffer text.
1.0 matches the default face; raise or lower it if the header looks
out of proportion on your display."
  :type 'number
  :group 'efrit-agent-header)

(defun efrit-agent-svg--font-size ()
  "Font size for the header SVG so its text matches the buffer text.
The SVG is drawn at :scale 1 in the same units as `frame-char-height',
so the font's own pixel size is the right value; fall back to a
fraction of the char height when the font object has no size."
  (let ((size (or (when-let* (((display-graphic-p))
                              (font (face-attribute 'default :font))
                              ((fontp font))
                              (px (font-get font :size))
                              ((and (numberp px) (> px 0))))
                    px)
                  (* 0.8 (frame-char-height)))))
    (max 8 (round (* size efrit-agent-header-text-scale)))))

(defun efrit-agent-svg--content-width (svg)
  "Rightmost pixel any text run in SVG reaches (absolute x plus its width)."
  (let ((w 0))
    (dolist (node (dom-by-tag svg 'text))
      ;; a text node with a direct string child (the icon glyph)
      (when (stringp (car (dom-children node)))
        (setq w (max w (+ (string-to-number (format "%s" (dom-attr node 'x)))
                          (string-pixel-width (car (dom-children node)))))))
      (dolist (child (dom-children node))
        (when (and (consp child) (eq (dom-tag child) 'tspan))
          (setq w (max w (+ (string-to-number (format "%s" (or (dom-attr child 'x) 0)))
                            (string-to-number (format "%s" (or (dom-attr child 'textLength) 0)))))))))
    w))

(defun efrit-agent-svg--window-width ()
  "Widest window showing this buffer, else the frame, in pixels."
  (if-let* ((wins (get-buffer-window-list (current-buffer) nil t)))
      (apply #'max (mapcar #'window-pixel-width wins))
    (frame-pixel-width)))

;;; Model

(defun efrit-agent-svg--session-id ()
  (cond ((and (boundp 'efrit-agent--repl-session) efrit-agent--repl-session
              (fboundp 'efrit-repl-session-id))
         (efrit-repl-session-id efrit-agent--repl-session))
        ((boundp 'efrit-agent--session-id) efrit-agent--session-id)))

(defun efrit-agent-svg--status-word (status)
  "STATUS without its leading glyph: the word after the first space.
The glyph is one token, then whitespace, in both display styles."
  (let ((s (substring-no-properties status)))
    (if (string-match "\\`[^[:space:]]+[[:space:]]+" s)
        (substring s (match-end 0))
      s)))

(defcustom efrit-agent-header-elapsed-step 10
  "Seconds between changes of the elapsed time in the graphical header
while a turn runs.  The header is one raster image, re-drawn whenever
anything in it changes; an elapsed counter ticking every second (and
changing width) made the spinner's twelve frames never repeat, so the
cache missed on nearly every tick.  Once the turn is over the exact
time is shown."
  :type 'integer)

(defun efrit-agent-svg--elapsed ()
  "The elapsed time for the header: coarse while running, exact after."
  (when efrit-agent--start-time
    (if (and efrit-agent--thinking-label (> efrit-agent-header-elapsed-step 1))
        (let* ((secs (floor (float-time (time-subtract (current-time) efrit-agent--start-time))))
               (secs (* efrit-agent-header-elapsed-step (/ secs efrit-agent-header-elapsed-step))))
          (format "%d:%02d" (/ secs 60) (% secs 60)))
      (efrit-agent--format-elapsed))))

(defun efrit-agent-svg--model ()
  "Collect everything the header shows into one alist (also the cache key).
Values that change often (the spinner frame, the elapsed time) are
kept coarse so the cache repeats: see `efrit-agent-header-elapsed-step'."
  (let* ((tools (and (boundp 'efrit-agent--activities)
                     (cl-count-if (lambda (a) (eq (plist-get a :type) 'tool))
                                  efrit-agent--activities)))
         (root (ignore-errors (efrit-tool--get-project-root)))
         (status (efrit-agent--status-string))
         (usage (and (fboundp 'efrit-agent--usage-segment)
                     (efrit-agent--usage-segment))))
    `((:name . "efrit")
      (:model . ,(and (boundp 'efrit-default-model) efrit-default-model))
      (:usage . ,(and usage (substring-no-properties usage)))
      (:usage-face . ,(and usage (get-text-property 0 'face usage)))
      ;; Key hint while nothing is running: the chat-client convention
      ;; is not the Emacs one, so say it where the eye lands
      (:hint . ,(and (not efrit-agent--thinking-label) (efrit-agent-input-hint)))
      (:directory . ,(and root (abbreviate-file-name root)))
      (:status . ,(substring-no-properties status))
      (:status-face . ,(or (get-text-property 0 'face status) 'default))
      ;; The header is one SVG, so the spinner is drawn into it.  While
      ;; a request is in flight the arc replaces the status glyph and
      ;; the label is the status word itself ("Working"), so the slot
      ;; keeps its text and width instead of swapping to "thinking...".
      (:spinner . ,(and efrit-agent--thinking-label
                        (efrit-agent-svg--status-word status)))
      (:spinner-index . ,(and efrit-agent--thinking-label
                              (mod efrit-agent--spinner-index efrit-agent-spinner-steps)))
      (:spinner-color . ,(and efrit-agent--thinking-label
                              (efrit-agent-spinner--hex 'efrit-agent-spinner)))
      (:elapsed . ,(efrit-agent-svg--elapsed))
      (:tools . ,(and tools (> tools 0) (format "%d tools" tools)))
      (:mode . ,(and (boundp 'efrit-agent-display-mode)
                     (format "%s" efrit-agent-display-mode)))
      (:session . ,(when-let* ((id (efrit-agent-svg--session-id)))
                     (truncate-string-to-width id 12 nil nil "…")))
      (:width . ,(efrit-agent-svg--window-width))
      (:char-height . ,(frame-char-height))
      (:font-size . ,(efrit-agent-svg--font-size))
      (:font-family . ,(face-attribute 'default :family))
      (:bg . ,(frame-parameter nil 'background-mode)))))

;;; Rendering

(defun efrit-agent-svg--row (x y font-size family segments)
  "Build a text NODE at X,Y from SEGMENTS, a list of (TEXT . FACE).
Segments are separated by ➤ in the default foreground.  A segment
whose TEXT is (:spinner INDEX COLOR LABEL) reserves an arc slot drawn
afterwards by `efrit-agent-svg--place-spinners' and shows LABEL.

Every tspan gets an ABSOLUTE x, computed here from the widths Emacs
measures with `string-pixel-width'.  Relative `dx' positioning was
wrong twice over: the renderer's advance for the previous run differs
from Emacs's measurement, and with `textLength' some renderers do not
advance at all, piling every run at the row start.  With absolute x
the text, the separators and the arc are placed by one calculation,
and `textLength' merely keeps each run inside its cell."
  (let ((node (dom-node 'text `((x . ,x) (y . ,y)
                                (font-size . ,font-size)
                                (font-family . ,family))))
        (cursor x)
        (gap 8)
        (first t))
    (cl-flet ((put-run (text fill &optional extra)
                (let ((w (max 1 (string-pixel-width text))))
                  (dom-append-child node (dom-node 'tspan
                                                   `((x . ,(format "%d" cursor))
                                                     (fill . ,fill)
                                                     (textLength . ,(format "%d" w))
                                                     (lengthAdjust . "spacingAndGlyphs")
                                                     ,@extra)
                                                   ;; svg-print writes text nodes
                                                   ;; verbatim: "S-<return>" in the
                                                   ;; key hint became a tag and the
                                                   ;; whole header failed to parse
                                                   (svg--encode-text text)))
                  (setq cursor (+ cursor w gap)))))
      (dolist (seg segments)
        (let* ((text (car seg))
               (spin (and (consp text) (eq (car text) :spinner) text))
               (label (if spin (nth 3 spin) text)))
          (when (and label (not (string-empty-p label)))
            (unless first
              (put-run "➤" (efrit-agent-svg--hex 'default)))
            (when spin
              ;; An empty run marking the arc's left edge; the arc is a
              ;; font-size square, so advance by that plus the gap
              (dom-append-child node (dom-node 'tspan
                                               `((x . ,(format "%d" cursor))
                                                 (efrit-spinner . ,spin))
                                               ""))
              (setq cursor (+ cursor font-size gap)))
            (put-run label (efrit-agent-svg--hex (cdr seg)))
            (setq first nil)))))
    node))

(defun efrit-agent-svg--place-spinners (svg font-size)
  "Draw the arc for every tspan in SVG that carries `efrit-spinner'.
The tspan's absolute x is the arc's left edge; the arc is a FONT-SIZE
square centred on the letters beside it (baseline minus ~0.35em)."
  (dolist (row (dom-by-tag svg 'text))
    (let ((y (string-to-number (format "%s" (dom-attr row 'y)))))
      (dolist (child (dom-children row))
        (when (and (consp child) (eq (dom-tag child) 'tspan))
          (when-let* ((spin (dom-attr child 'efrit-spinner)))
            (let* ((x (string-to-number (format "%s" (dom-attr child 'x))))
                   (size font-size)
                   (frame (efrit-agent-spinner--svg size (nth 1 spin) (nth 2 spin)
                                                    (efrit-agent-svg--hex 'default :background)))
                   (g (dom-node 'g `((transform . ,(format "translate(%.1f,%.1f)"
                                                           x (- y (* 0.35 size) (/ size 2.0))))))))
              (dolist (n (dom-children frame)) (dom-append-child g n))
              (svg--append svg g))))))))

(defun efrit-agent-svg--build (model)
  "Build the SVG DOM for MODEL (pure; no image support needed)."
  (let* ((ch (alist-get :char-height model))
         (fs (alist-get :font-size model))
         (family (alist-get :font-family model))
         (pad (/ fs 2))
         (icon (* 2 ch))
         (icon-x 6)
         (text-x (+ icon-x icon 10))
         (border 1)
         (total-h (+ icon pad pad border))
         (y1 (+ pad ch (/ (- ch fs) 2) (- (/ ch 4))))
         (y2 (+ y1 ch))
         (width (alist-get :width model))
         (svg (svg-create width total-h)))
    ;; Bottom border: the header's edge is part of the graphic, not a
    ;; row of characters in the buffer
    (svg-rectangle svg 0 (- total-h border) width border
                   :fill (efrit-agent-svg--hex 'efrit-agent-header-border))
    ;; Icon tile
    (svg-rectangle svg icon-x pad icon icon :rx 6
                   :fill (efrit-agent-svg--hex 'efrit-agent-header-icon :background))
    (svg-text svg efrit-agent-header-icon-text
              :x (+ icon-x (/ icon 2)) :y (+ pad (/ icon 2))
              :text-anchor "middle" :dominant-baseline "central"
              :font-weight "bold" :font-size (* 0.55 icon) :font-family family
              :fill (efrit-agent-svg--hex 'efrit-agent-header-icon))
    ;; Row 1: name ➤ model ➤ usage
    (svg--append svg (efrit-agent-svg--row
                      text-x y1 fs family
                      `((,(alist-get :name model) . efrit-agent-header-name)
                        (,(alist-get :model model) . efrit-agent-header-model)
                        (,(alist-get :usage model) . ,(or (alist-get :usage-face model) 'default))
                        (,(alist-get :hint model) . efrit-agent-header-hint))))
    ;; Row 2: directory ➤ status/spinner ➤ elapsed ➤ tools ➤ mode ➤ session
    (svg--append svg (efrit-agent-svg--row
                      text-x y2 fs family
                      `((,(alist-get :directory model) . efrit-agent-header-directory)
                        (,(if (alist-get :spinner model)
                              (list :spinner (alist-get :spinner-color model)
                                    (alist-get :spinner-index model)
                                    (alist-get :spinner model))
                            (alist-get :status model))
                         . ,(alist-get :status-face model))
                        (,(alist-get :elapsed model) . font-lock-comment-face)
                        (,(alist-get :tools model) . font-lock-comment-face)
                        (,(and (alist-get :mode model) (format "[%s]" (alist-get :mode model)))
                         . font-lock-comment-face)
                        (,(alist-get :session model) . font-lock-comment-face))))
    (efrit-agent-svg--place-spinners svg fs)
    ;; Shrink the canvas to what was drawn: every pixel is re-blitted on
    ;; each spinner tick.  Overshoot is transparent, undershoot clips.
    (dom-set-attribute svg 'width
                       (min (dom-attr svg 'width)
                            (max (+ icon-x icon 16)
                                 (+ (efrit-agent-svg--content-width svg) 16))))
    svg))

(defun efrit-agent-svg--render (model)
  "Rasterise MODEL into a propertized string carrying the SVG image.
The SVG is laid out in the frame's character units, so it must be
shown at `:scale 1': `svg-insert-image' leaves the scale to
`image-scaling-factor', which is 2 on HiDPI displays and drew the
whole header at twice the body text.  (The spinner already did this.)"
  (let ((svg (efrit-agent-svg--build model)))
    (propertize (concat " "
                        (propertize " " 'display (svg-image svg :ascent 'center :scale 1)))
                'help-echo "efrit agent")))

;;; Cache and entry point

(defvar-local efrit-agent-svg--cache nil
  "Hash of model -> rendered header, cleared when it grows past a few dozen.")

(defvar efrit-agent-svg--last-error nil
  "The last error the header value function caught, for `efrit-doctor'.")

(defun efrit-agent-svg-header ()
  "Return the header-line string per `efrit-agent-header-style'.
Never signals: an error inside a `header-line-format' :eval makes
redisplay blank the header and set the format to nil for the buffer,
which showed up as no logo until the next open.  Errors are logged
and the plain text header is returned instead."
  (condition-case err
      (efrit-agent-svg--header-1)
    (error
     (setq efrit-agent-svg--last-error (error-message-string err))
     (efrit-log 'warn "header: %s" efrit-agent-svg--last-error)
     (condition-case nil
         (efrit-agent--format-header-line)
       (error " efrit")))))

(defun efrit-agent-svg--header-1 ()
  "The header string; may signal (see `efrit-agent-svg-header')."
  (pcase efrit-agent-header-style
    ('none nil)
    ('graphical
     (if (not (and (display-graphic-p) (image-type-available-p 'svg)))
         (efrit-agent--format-header-line)
       (let ((model (efrit-agent-svg--model)))
         (unless efrit-agent-svg--cache
           (setq efrit-agent-svg--cache (make-hash-table :test #'equal)))
         ;; A full spinner cycle at one elapsed step is 12 entries; a
         ;; minute of running is 72.  Keep a few minutes.
         (when (> (hash-table-count efrit-agent-svg--cache) 256)
           (clrhash efrit-agent-svg--cache))
         (or (gethash model efrit-agent-svg--cache)
             ;; A failed render is shown as the text header but NOT
             ;; cached: caching it pinned the fallback to this model
             ;; key until something in the header changed, which is
             ;; why a transient error at startup left a blank header
             ;; until the first RET.
             (condition-case err
                 (puthash model (efrit-agent-svg--render model) efrit-agent-svg--cache)
               (error
                (efrit-log 'warn "svg header: %s" (error-message-string err))
                (efrit-agent--format-header-line)))))))
    (_ (efrit-agent--format-header-line))))

(defun efrit-agent-svg--resize ()
  "Drop the cache when the window width changed, so clipping is redone."
  (when (and (derived-mode-p 'efrit-agent-mode) efrit-agent-svg--cache)
    (clrhash efrit-agent-svg--cache)))

(defun efrit-agent-cycle-header-style ()
  "Cycle `efrit-agent-header-style' between graphical, text and none."
  (interactive)
  (setq efrit-agent-header-style
        (pcase efrit-agent-header-style
          ('graphical 'text) ('text 'none) (_ 'graphical)))
  (force-mode-line-update t)
  (message "efrit header: %s" efrit-agent-header-style))

(provide 'efrit-agent-svg-header)

;;; efrit-agent-svg-header.el ends here
