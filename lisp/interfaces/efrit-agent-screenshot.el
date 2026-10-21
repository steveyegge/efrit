;;; efrit-agent-screenshot.el --- Frame screenshots with window anchors -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.13.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, ai

;;; Commentary:

;; "Drop this thing" with a 700x250 crop of a mode line is hard to
;; answer: the model sees an e and a time, and nothing says which
;; window, which line, which construct.  Two questions were asked
;; before the user gave up (2026-10-20).
;;
;; `/shot' (and `efrit-agent-attach-screenshot') captures the whole
;; selected frame with `x-export-frames' (no external program: NS,
;; PGTK and cairo X builds render the frame to PNG themselves) and
;; attaches it to the input as an image mention, together with a
;; LAYOUT text: every window's buffer, its pixel rectangle, where its
;; header line, mode line and tab line are, which window is selected,
;; and the frame's own size and scale.  A crop pasted next to it can
;; be placed: "that is the mode line of *efrit-agent*, the right end".
;; The layout also names what each line is made of, through
;; `efrit-tool-introspect--construct-pieces', so the model can go from
;; pixels to a variable in one step.
;;
;; The other direction, the model pointing at a UI element, is the
;; `show_ui_element' tool: it pulses a window's mode line, header line
;; or tab line (by buffer name) for a moment, and names it in the
;; result, so the model can say "this one" and the user sees it.
;;
;; Frames are not readable in batch or on a terminal; both commands
;; say so instead of failing.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'face-remap)
(require 'efrit-log)
(require 'efrit-tool-utils)

(declare-function efrit-agent-dnd--insert-mention "efrit-agent-mentions")
(declare-function efrit-agent-dropped-dir "efrit-agent-mentions")
(declare-function efrit-agent--in-input-region-p "efrit-agent-input")
(declare-function efrit-agent-define-slash-command "efrit-agent-mentions")
(declare-function efrit-tool-introspect--construct-pieces "efrit-tool-introspect")
(declare-function x-export-frames "xfns.c")

(defgroup efrit-agent-screenshot nil
  "Frame screenshots attached to the conversation."
  :group 'efrit-agent)

(defcustom efrit-agent-screenshot-layout-pieces t
  "When non-nil the layout names the constructs of each window's lines.
That is one `format-mode-line' per piece per window; off, the layout
is geometry and buffer names only."
  :type 'boolean
  :group 'efrit-agent-screenshot)

;;;; Capture

(defun efrit-agent-screenshot-available-p (&optional frame)
  "Non-nil when FRAME (default the selected frame) can be exported to PNG."
  (and (fboundp 'x-export-frames)
       (display-graphic-p frame)
       (not noninteractive)))

(defun efrit-agent-screenshot--png (frame)
  "FRAME as PNG bytes (a unibyte string)."
  (let ((data (x-export-frames (list frame) 'png)))
    (if (multibyte-string-p data) (encode-coding-string data 'binary) data)))

(defun efrit-agent-screenshot-capture (&optional frame)
  "Save a PNG of FRAME under `efrit-agent-dropped-dir'; return the path.
Signals `user-error' where frames cannot be exported."
  (unless (efrit-agent-screenshot-available-p frame)
    (user-error "This Emacs cannot export its frame to an image (no display, or built without it)"))
  (require 'efrit-agent-mentions)
  (let* ((frame (or frame (selected-frame)))
         (path (expand-file-name (format "shot-%s.png" (format-time-string "%Y%m%d-%H%M%S"))
                                 (efrit-agent-dropped-dir)))
         (bytes (efrit-agent-screenshot--png frame)))
    (let ((coding-system-for-write 'binary))
      (with-temp-file path (set-buffer-multibyte nil) (insert bytes)))
    path))

;;;; Layout

(defun efrit-agent-screenshot--line-geometry (window)
  "Pixel rows of WINDOW's tab line, header line and mode line: an alist of (KIND TOP BOTTOM)."
  (let* ((edges (window-pixel-edges window))
         (top (nth 1 edges)) (bottom (nth 3 edges))
         (tab (window-tab-line-height window))
         (header (window-header-line-height window))
         (mode (window-mode-line-height window))
         (rows nil))
    (when (> tab 0) (push (list 'tab-line top (+ top tab)) rows))
    (when (> header 0) (push (list 'header-line (+ top tab) (+ top tab header)) rows))
    (when (> mode 0) (push (list 'mode-line (- bottom mode) bottom) rows))
    (nreverse rows)))

(defun efrit-agent-screenshot--pieces-text (construct)
  "CONSTRUCT's pieces as \"source => text\" lines, or nil when off."
  (when (and efrit-agent-screenshot-layout-pieces construct
             (require 'efrit-tool-introspect nil t))
    (let ((pieces (ignore-errors (efrit-tool-introspect--construct-pieces construct))))
      (when pieces
        (mapconcat (lambda (p)
                     (format "      %s => %S" (car p)
                             (truncate-string-to-width (substring-no-properties (cdr p)) 60 nil nil "…")))
                   pieces "\n")))))

(defun efrit-agent-screenshot-layout (&optional frame)
  "A text description of FRAME's windows for the model to read beside the image.
Pixel coordinates are the frame's own, origin top-left, in the
image's scale: `frame-scale-factor' is given so a HiDPI capture can
be mapped (image pixel = frame pixel * factor)."
  (let* ((frame (or frame (selected-frame)))
         (selected (frame-selected-window frame))
         (scale (if (fboundp 'frame-scale-factor) (frame-scale-factor frame) 1.0)))
    (with-temp-buffer
      (insert (format "FRAME %dx%d px, scale %s (image px = frame px * scale), origin top-left.\n"
                      (frame-pixel-width frame) (frame-pixel-height frame) scale))
      (insert (format "Frame-wide: tab-bar %d px tall at the top; tool-bar %d px.\n"
                      (if (fboundp 'tab-bar-height) (tab-bar-height frame t) 0)
                      (if (fboundp 'tool-bar-height) (tool-bar-height frame t) 0)))
      (insert "WINDOWS (left top right bottom):\n")
      (dolist (w (window-list frame 'nomini))
        (let ((edges (window-pixel-edges w))
              (buf (window-buffer w)))
          (insert (format "- %S  %d %d %d %d%s%s\n"
                          (buffer-name buf)
                          (nth 0 edges) (nth 1 edges) (nth 2 edges) (nth 3 edges)
                          (if (eq w selected) "  SELECTED" "")
                          (if (window-dedicated-p w) "  dedicated" "")))
          (insert (format "    major-mode %s; mode-name %S\n"
                          (buffer-local-value 'major-mode buf)
                          (with-selected-window w
                            (with-current-buffer buf
                              (substring-no-properties (format-mode-line mode-name))))))
          (dolist (row (efrit-agent-screenshot--line-geometry w))
            (pcase-let ((`(,kind ,top ,bottom) row))
              (insert (format "    %s: y %d-%d, x %d-%d\n" kind top bottom (nth 0 edges) (nth 2 edges)))
              (let ((construct (with-current-buffer buf
                                 (pcase kind
                                   ('mode-line mode-line-format)
                                   ('header-line header-line-format)
                                   ('tab-line tab-line-format)))))
                (when-let* ((text (with-selected-window w
                                    (with-current-buffer buf
                                      (efrit-agent-screenshot--pieces-text construct)))))
                  (insert text "\n")))))))
      (when (and efrit-agent-screenshot-layout-pieces (require 'efrit-tool-introspect nil t))
        (insert "GLOBAL-MODE-STRING (shared by every mode line):\n")
        (insert (or (with-selected-window selected
                      (with-current-buffer (window-buffer selected)
                        (efrit-agent-screenshot--pieces-text global-mode-string)))
                    "      (empty)")
                "\n"))
      (buffer-string))))

;;;; The command

(defun efrit-agent-attach-screenshot (&optional note)
  "Capture the selected frame and attach it to the input, with its layout.
The image becomes a mention (previewed); the layout follows as a
fenced block the model reads with it.  NOTE, when given, is put
before both.  `/shot [note]' from the input."
  (interactive)
  (require 'efrit-agent-mentions)
  (require 'efrit-agent-input)
  ;; The agent buffer is the selected window while the user types the
  ;; command: capture the frame as it is, which includes that buffer.
  ;; The user who wants another frame selects it and uses M-x.
  (let* ((frame (selected-frame))
         (layout (efrit-agent-screenshot-layout frame))
         (path (efrit-agent-screenshot-capture frame)))
    (unless (efrit-agent--in-input-region-p) (goto-char (point-max)))
    (when (and note (not (string-empty-p note))) (insert note " "))
    (efrit-agent-dnd--insert-mention path)
    (insert "\n```layout\n" layout "```\n")
    (efrit-log 'info "screenshot: %s (%d windows)" path (length (window-list frame 'nomini)))
    (message "efrit: frame captured to %s; the layout is in the input" (abbreviate-file-name path))
    path))

(defun efrit-agent-screenshot--define-slash-command ()
  (when (fboundp 'efrit-agent-define-slash-command)
    (efrit-agent-define-slash-command
     "shot" "Attach a screenshot of this frame with its window layout"
     (lambda (args) (efrit-agent-attach-screenshot args)))))

(with-eval-after-load 'efrit-agent-mentions
  (efrit-agent-screenshot--define-slash-command))

;;;; show_ui_element: the model points

(defface efrit-agent-ui-pulse
  '((t :background "orange" :foreground "black"))
  "Face flashed over a mode line, header line or tab line to point at it."
  :group 'efrit-agent-screenshot)

(defcustom efrit-agent-ui-pulse-seconds 2.0
  "How long `show_ui_element' keeps the pointed line highlighted."
  :type 'number
  :group 'efrit-agent-screenshot)

(defvar efrit-agent-ui--pulses nil
  "Live pulses: (BUFFER FACE-REMAP-COOKIE TIMER), cleared when they end.")

(defun efrit-agent-ui--unpulse (entry)
  (pcase-let ((`(,buf ,cookie ,_timer) entry))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (face-remap-remove-relative cookie)
        (force-mode-line-update))))
  (setq efrit-agent-ui--pulses (delq entry efrit-agent-ui--pulses)))

(defun efrit-tool-show-ui-element (args)
  "Flash a window's mode line, header line or tab line so the user sees which one.
ARGS: buffer (name, required); element (\"mode-line\" default,
\"header-line\" or \"tab-line\").  The face of that line is remapped
to `efrit-agent-ui-pulse' in that buffer for `efrit-agent-ui-pulse-seconds'.
Returns where the line is on screen, so the words can match."
  (efrit-tool-execute show_ui_element args
    (let* ((name (or (alist-get 'buffer args) ""))
           (element (or (alist-get 'element args) "mode-line"))
           (buf (get-buffer name))
           (face (pcase element
                   ("mode-line" 'mode-line)
                   ("header-line" 'header-line)
                   ("tab-line" 'tab-line)
                   (_ (signal 'user-error (list "element must be mode-line, header-line or tab-line"))))))
      (unless buf (signal 'user-error (list (format "No buffer named %s" name))))
      (let ((windows (get-buffer-window-list buf nil t)))
        (unless windows
          (signal 'user-error (list (format "%s is not shown in any window; the user cannot see its %s" name element))))
        (with-current-buffer buf
          (let* ((cookie (face-remap-add-relative face 'efrit-agent-ui-pulse))
                 (entry (list buf cookie nil)))
            (setf (nth 2 entry)
                  (run-at-time efrit-agent-ui-pulse-seconds nil #'efrit-agent-ui--unpulse entry))
            (push entry efrit-agent-ui--pulses)
            (force-mode-line-update t)))
        (efrit-tool-success
         `((buffer . ,name)
           (element . ,element)
           (seconds . ,efrit-agent-ui-pulse-seconds)
           (windows . ,(vconcat
                        (mapcar (lambda (w)
                                  (let ((edges (window-pixel-edges w))
                                        (row (assq (intern element) (efrit-agent-screenshot--line-geometry w))))
                                    `((frame . ,(format "%s" (frame-parameter (window-frame w) 'name)))
                                      (x . ,(nth 0 edges)) (right . ,(nth 2 edges))
                                      (y . ,(and row (nth 1 row))) (bottom . ,(and row (nth 2 row)))
                                      (selected . ,(if (eq w (selected-window)) t :json-false)))))
                                windows)))))))))

(provide 'efrit-agent-screenshot)

;;; efrit-agent-screenshot.el ends here
