;;; test-context-indicator.el --- context describe/dismiss, range mentions -*- lexical-binding: t; -*-
;;; Code:
(require 'ert)
(require 'efrit-context-sources)
(require 'efrit-agent-mentions)

(ert-deftest test-context-describe-and-dismiss ()
  "The label names file:line, adds the region size, and says dismissed;
dismissal removes the file-bound sources and ends on a region."
  (let ((efrit-context--dismissed nil)
        (efrit-context-sources '(buffer position region project))
        (file (make-temp-file "efrit-ctx-" nil ".txt" "one\ntwo\nthree\n")))
    (unwind-protect
        (with-current-buffer (find-file-noselect file)
          (goto-char (point-min)) (forward-line 1)
          (let ((name (file-name-nondirectory file)))
            (should (equal (format "⧉ %s:2" name) (efrit-context-describe (current-buffer))))
            (transient-mark-mode 1)
            (set-mark (point-min)) (goto-char (point-max)) (activate-mark)
            (should (equal (format "⧉ %s:1, 3 lines" name) (efrit-context-describe (current-buffer))))
            (deactivate-mark)
            (cl-letf (((symbol-function 'efrit-context-target-buffer) (lambda (&rest _) (current-buffer))))
              (efrit-context-dismiss)
              (should (efrit-context-dismissed-p (current-buffer)))
              (should (equal (format "⧉ %s (dismissed)" name) (efrit-context-describe (current-buffer))))
              (should (equal '(project) (efrit-context-active-sources (current-buffer))))
              (let ((snap (efrit-context-snapshot (current-buffer))))
                (should-not (and snap (string-match-p "Point: line" snap))))
              ;; a region brings it back
              (set-mark (point-min)) (goto-char (point-max)) (activate-mark)
              (should-not (efrit-context-dismissed-p (current-buffer)))
              (deactivate-mark)
              (efrit-context-restore)
              (should-not (efrit-context-dismissed-p (current-buffer))))))
      (when-let* ((b (find-buffer-visiting file))) (kill-buffer b))
      (delete-file file))))

(ert-deftest test-mention-range-expands-only-those-lines ()
  "@path#L2-L3 inlines lines 2-3 labelled; @path#L2 one line; plain @path the file."
  (let* ((root (file-name-as-directory (make-temp-file "efrit-mr-" t)))
         (efrit-project-root root))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "f.txt" root) (insert "l1\nl2\nl3\nl4\n"))
          (should (equal '("f.txt" 2 3) (efrit-agent-mention-split "f.txt#L2-L3")))
          (should (equal '("f.txt" 2 2) (efrit-agent-mention-split "f.txt#L2")))
          (should (equal '("f.txt" nil nil) (efrit-agent-mention-split "f.txt")))
          (let ((out (efrit-agent-mentions-expand "see @f.txt#L2-L3 there")))
            (should (string-match-p "lines 2-3 of 4" out))
            (should (string-match-p "\nl2\nl3\n" out))
            (should-not (string-match-p "l1" out))
            (should-not (string-match-p "l4\n" out)))
          (let ((out (efrit-agent-mentions-expand "@f.txt")))
            (should (string-match-p "l1\nl2\nl3\nl4" out))))
      (delete-directory root t))))

(ert-deftest test-mention-range-command-inserts-into-the-input ()
  (require 'efrit-agent)
  (let* ((root (file-name-as-directory (make-temp-file "efrit-mr-" t)))
         (efrit-project-root root)
         (file (expand-file-name "g.el" root))
         (efrit-agent-auto-show nil)
         (agent (get-buffer-create " *mr-agent*")))
    (unwind-protect
        (progn
          (with-temp-file file (insert "a\nb\nc\nd\n"))
          (with-current-buffer agent
            (efrit-agent-mode) (efrit-agent--init-regions) (efrit-agent--setup-regions))
          (with-current-buffer (find-file-noselect file)
            (cl-letf (((symbol-function 'efrit-agent-target-buffer) (lambda (&rest _) agent))
                      ((symbol-function 'efrit-agent-display) #'ignore))
              (efrit-agent-mention-range 3 (point-max))))
          (with-current-buffer agent
            (should (string-match-p "@g.el#L2-L4 $"
                                    (buffer-substring-no-properties efrit-agent--input-start (point-max))))))
      (when-let* ((b (find-buffer-visiting file))) (kill-buffer b))
      (kill-buffer agent)
      (delete-directory root t))))

(ert-deftest test-context-scope-block-which-function-fallback ()
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "(defun outer-fn (x)\n  (list x))\n")
    (goto-char (point-min)) (forward-line 1)
    ;; which-function reads the imenu index; a fresh buffer has none yet
    (require 'which-func)
    (setq imenu--index-alist nil)
    (imenu--make-index-alist t)
    (let ((block (efrit-context-scope-block)))
      (should (string-match-p "outer-fn" block)))
    ;; the position source carries it
    (should (string-match-p "outer-fn" (efrit-context--source-position (current-buffer))))))

(ert-deftest test-context-pins-are-per-project-and-render ()
  (let* ((root (file-name-as-directory (make-temp-file "efrit-pin-" t)))
         (efrit-project-root root)
         (efrit-context--pins (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "p.txt" root) (insert "1\n2\n3\n4\n"))
          (with-current-buffer (find-file-noselect (expand-file-name "p.txt" root))
            (efrit-context-pin 3 6)          ; lines 2-3
            (should (equal '("p.txt#L2-L3") (efrit-context-pins root)))
            (efrit-context-pin nil nil)
            (should (equal '("p.txt#L2-L3" "p.txt") (efrit-context-pins root)))
            (let ((text (efrit-context--source-pins (current-buffer))))
              (should (string-match-p "Pinned by the user" text))
              (should (string-match-p "p.txt#L2-L3 (lines 2-3 of 4)\n```\n2\n3\n```" text))
              (should (string-match-p "p.txt (whole file)" text)))
            (should (string-match-p "\\+2 pins" (efrit-context-describe (current-buffer))))
            (efrit-context-unpin "p.txt")
            (should (equal '("p.txt#L2-L3") (efrit-context-pins root)))
            (efrit-context-clear-pins)
            (should-not (efrit-context-pins root))
            (kill-buffer)))
      (delete-directory root t))))

(ert-deftest test-mention-symbol-completion-after-file ()
  "After @file#, the file's definitions complete; accepting writes @file#Lstart-Lend."
  (require 'efrit-agent)
  (let* ((root (file-name-as-directory (make-temp-file "efrit-sym-" t)))
         (efrit-project-root root)
         (efrit-agent-auto-show nil)
         (agent (get-buffer-create " *sym-agent*")))
    (unwind-protect
        (progn
          (with-temp-file (expand-file-name "m.el" root)
            (insert ";;; m.el -*- lexical-binding: t; -*-\n(defun alpha () 1)\n\n(defun beta ()\n  2)\n(provide 'm)\n"))
          (with-current-buffer agent
            (efrit-agent-mode) (efrit-agent--init-regions) (efrit-agent--setup-regions)
            (goto-char (point-max)) (insert "@m.el#be")
            (let ((capf (efrit-agent-mention-symbol-completion-at-point)))
              (should capf)
              (let* ((table (nth 2 capf))
                     (cands (all-completions "be" table)))
                (should (equal '("beta") cands))
                (delete-region (nth 0 capf) (nth 1 capf))
                (insert "beta")
                (funcall (plist-get (nthcdr 3 capf) :exit-function) "beta" 'finished)
                (should (string-match-p "@m.el#L4-L7 $"
                                        (buffer-substring-no-properties efrit-agent--input-start (point-max))))))))
      (when-let* ((b (find-buffer-visiting (expand-file-name "m.el" root)))) (kill-buffer b))
      (kill-buffer agent)
      (delete-directory root t))))

(provide 'test-context-indicator)
;;; test-context-indicator.el ends here
