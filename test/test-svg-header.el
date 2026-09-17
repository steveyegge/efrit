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
      (should (string-match-p "waiting for Claude" (alist-get :spinner m)))
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
      ;; colours are hex, never X11 names
      (should-not (string-match-p "fill=\"[A-Za-z]" xml))
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
