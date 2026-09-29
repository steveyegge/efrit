;;; test-diagnostics-baseline.el --- new diagnostics after edits -*- lexical-binding: t; -*-
;;; Code:
(require 'ert)
(require 'efrit-diagnostics-baseline)

(defmacro test-dbl--with-fake-diags (var &rest body)
  "BODY with flymake diagnostics of every buffer read from VAR (a list of messages)."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'efrit-tool-get-diagnostics--from-flymake)
              (lambda (_buf) (mapcar (lambda (m) `((source . "flymake") (severity . "error")
                                                     (message . ,m) (line . 3)))
                                     ,var)))
             ((symbol-function 'efrit-tool-get-diagnostics--from-flycheck) (lambda (_) nil)))
     ,@body))

(ert-deftest test-diagnostics-baseline-reports-only-new-ones ()
  "The first write records the baseline; later writes report what is
new against it, not what was already wrong; a new turn starts over."
  (let* ((file (make-temp-file "efrit-dbl-" nil ".el" "(defun a ())\n"))
         (diags '("old warning"))
         (input (make-hash-table :test 'equal))
         (efrit-diagnostics-baseline-wait 0)
         (efrit-diagnostics-baseline--per-turn (make-hash-table :test 'equal)))
    (puthash "path" file input)
    (unwind-protect
        (test-dbl--with-fake-diags diags
          (cl-letf (((symbol-function 'efrit-resolve-path-simple) (lambda (p &rest _) p)))
            (efrit-diagnostics-baseline-before-tool "edit_file" input)
            ;; first write: nothing new
            (should (equal "ok" (efrit-diagnostics-baseline-after-tool "edit_file" input "ok")))
            ;; the edit broke something
            (setq diags '("old warning" "void-variable nme"))
            (let ((r (efrit-diagnostics-baseline-after-tool "edit_file" input "ok")))
              (should (string-match-p "new diagnostics in efrit-dbl-.*(1): 3: error: void-variable nme" r))
              (should-not (string-match-p "old warning" r)))
            ;; a read tool is untouched; an error result is untouched
            (should (equal "x" (efrit-diagnostics-baseline-after-tool "read_file" input "x")))
            (should (equal "Error: no" (efrit-diagnostics-baseline-after-tool "edit_file" input "Error: no")))
            ;; next turn: the broken state is the new baseline
            (efrit-diagnostics-baseline-begin-turn)
            (efrit-diagnostics-baseline-before-tool "edit_file" input)
            (should (equal "ok" (efrit-diagnostics-baseline-after-tool "edit_file" input "ok")))
            ;; off: nothing appended
            (setq diags '("old warning" "void-variable nme" "third"))
            (let ((efrit-diagnostics-baseline-enabled nil))
              (should (equal "ok" (efrit-diagnostics-baseline-after-tool "edit_file" input "ok"))))))
      (when-let* ((b (find-buffer-visiting file))) (kill-buffer b))
      (delete-file file))))

(provide 'test-diagnostics-baseline)
;;; test-diagnostics-baseline.el ends here
