;;; test-svg-header.el --- Tests for efrit-agent-svg-header -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'efrit-agent)
(require 'efrit-agent-svg-header)

(defmacro test-svg--in-agent-buffer (&rest body)
  (declare (indent 0))
  `(with-temp-buffer
     (efrit-agent-mode)
     (setq efrit-agent--status 'working
           efrit-agent--start-time (current-time)
           efrit-agent--thinking-label "waiting for Claude...")
     ,@body))

(ert-deftest test-svg-header-model-has-core-fields ()
  (test-svg--in-agent-buffer
    (let ((m (efrit-agent-svg--model)))
      (should (equal (alist-get :name m) "efrit"))
      (should (stringp (alist-get :model m)))
      (should (string-match-p "Working" (alist-get :status m)))
      ;; while a request is in flight the arc replaces the status
      ;; glyph and the label is the status word, not the thinking text
      (should (equal (alist-get :spinner m) "Working"))
      (should (numberp (alist-get :width m)))
      (should (numberp (alist-get :font-size m))))))

(ert-deftest test-svg-header-build-produces-valid-svg ()
  "The DOM is checked directly so this runs on an Emacs without image support."
  (test-svg--in-agent-buffer
    (let* ((model (efrit-agent-svg--model))
           (svg (efrit-agent-svg--build model))
           (xml (with-temp-buffer (svg-print svg) (buffer-string))))
      (should (eq (dom-tag svg) 'svg))
      (should (string-match-p "<svg" xml))
      (should (string-match-p ">efrit<" xml))
      (should (string-match-p "Working\\|waiting for Claude" xml))
      ;; two text rows plus the icon glyph
      (should (= 3 (length (dom-by-tag svg 'text))))
      ;; colours are hex, never X11 names ("none" is the unfilled arc)
      (let ((case-fold-search nil))
        (should-not (string-match-p "fill=\"\\(?:[A-Za-mo-z]\\|n[^o]\\)" xml)))
      ;; the spinner arc is drawn while a request is in flight
      (should (string-match-p "<path" xml))
      ;; Every run has an absolute x.  The arc marker's x is the arc's
      ;; left edge; the status word must start at or beyond x + font-size,
      ;; so the two never overlap whatever the renderer's font metrics.
      (let* ((row (seq-find (lambda (n) (seq-some (lambda (c) (and (consp c) (dom-attr c 'efrit-spinner)))
                                                   (dom-children n)))
                            (dom-by-tag svg 'text)))
             (fs (string-to-number (format "%s" (dom-attr row 'font-size))))
             (marker (seq-find (lambda (c) (and (consp c) (dom-attr c 'efrit-spinner))) (dom-children row)))
             (label (seq-find (lambda (c) (and (consp c) (equal (car (dom-children c)) "Working")))
                              (dom-children row)))
             (arc (car (dom-by-tag svg 'g))))
        (should marker) (should label) (should arc)
        (let ((mx (string-to-number (dom-attr marker 'x)))
              (lx (string-to-number (dom-attr label 'x))))
          (should (>= lx (+ mx fs)))
          ;; the arc group is translated to the marker's x
          (should (string-match-p (format "translate(%.1f," mx) (dom-attr arc 'transform))))
        ;; runs are pinned to Emacs's measured width
        (should (dom-attr label 'textLength)))
      (should-not (string-match-p "waiting for Claude" xml))
      ;; canvas never exceeds the window width (in batch the frame is
      ;; 80 "pixels" wide, so shrinking cannot be observed; on a real
      ;; display the canvas ends just past the last glyph)
      (should (<= (string-to-number (format "%s" (dom-attr svg 'width)))
                  (alist-get :width model))))))

(ert-deftest test-svg-header-cache-hits-on-identical-model ()
  (test-svg--in-agent-buffer
    (let ((efrit-agent-header-style 'graphical)
          (renders 0))
      (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) t))
                ((symbol-function 'image-type-available-p) (lambda (&rest _) t))
                ((symbol-function 'efrit-agent-svg--render)
                 (lambda (_m) (cl-incf renders) "rendered")))
        (should (equal (efrit-agent-svg-header) "rendered"))
        (should (equal (efrit-agent-svg-header) "rendered"))
        (should (= renders 1))
        ;; A model change re-renders
        (setq efrit-agent--status 'idle)
        (efrit-agent-svg-header)
        (should (= renders 2))
        ;; Resize drops the cache
        (efrit-agent-svg--resize)
        (efrit-agent-svg-header)
        (should (= renders 3))))))

(ert-deftest test-svg-header-falls-back-to-text ()
  (test-svg--in-agent-buffer
    (let ((efrit-agent-header-style 'text))
      (should (string-match-p "Working" (efrit-agent-svg-header))))
    (let ((efrit-agent-header-style 'none))
      (should-not (efrit-agent-svg-header)))
    ;; graphical requested but no SVG support -> text
    (let ((efrit-agent-header-style 'graphical))
      (cl-letf (((symbol-function 'image-type-available-p) (lambda (&rest _) nil)))
        (should (string-match-p "Working" (efrit-agent-svg-header)))))))

(ert-deftest test-svg-header-hex-colours ()
  (should (string-match-p "\\`#[0-9a-f]\\{6\\}\\'" (efrit-agent-svg--hex 'default)))
  (should (string-match-p "\\`#[0-9a-f]\\{6\\}\\'" (efrit-agent-svg--hex 'success)))
  (should (string-match-p "\\`#[0-9a-f]\\{6\\}\\'"
                          (efrit-agent-svg--hex 'efrit-agent-header-icon :background))))

(provide 'test-svg-header)
;;; test-svg-header.el ends here

;;; Spinner

(ert-deftest test-spinner-svg-frames-rotate-and-blend ()
  (require 'efrit-agent-spinner)
  (let* ((a (with-temp-buffer (svg-print (efrit-agent-spinner--svg 16 "#88c0d0" 0 "#2e3440")) (buffer-string)))
         (b (with-temp-buffer (svg-print (efrit-agent-spinner--svg 16 "#88c0d0" 3 "#2e3440")) (buffer-string))))
    ;; two arcs and a reference ring, different geometry per index
    (should (= 2 (cl-count ?A a)))
    (should-not (equal a b))
    ;; opacity is baked into the colours: no opacity attributes
    (should-not (string-match-p "opacity" a))
    (should (string-match-p "stroke=\"#88c0d0\"" a))
    (should (equal (efrit-agent-spinner--blend "#ffffff" "#000000" 0.5) "#808080"))))

(ert-deftest test-spinner-frame-falls-back-without-display ()
  (require 'efrit-agent-spinner)
  (should-not (efrit-agent-spinner-frame 0))       ; batch: no SVG
  (with-temp-buffer
    (efrit-agent-mode)
    (setq efrit-agent--spinner-index 0)
    (should (stringp (efrit-agent--spinner-frame)))
    (should (member (efrit-agent--spinner-frame t)
                    (append efrit-agent--spinner-frames-unicode nil)))))
