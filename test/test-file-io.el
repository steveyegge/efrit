;;; test-file-io.el --- buffer-aware, verified file IO -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'efrit-test-sandbox-helpers)
(require 'efrit-file-io)
(require 'efrit-tool-edit-file)
(require 'efrit-tool-read-file)

(defmacro test-fio--with-file (content &rest body)
  "Run BODY with `path' bound to a temp file containing CONTENT."
  (declare (indent 1))
  `(let ((path (make-temp-file "efrit-fio-" nil ".el" ,content)))
     (unwind-protect (progn ,@body)
       (when-let* ((b (get-file-buffer path))) (with-current-buffer b (set-buffer-modified-p nil)) (kill-buffer b))
       (ignore-errors (delete-file path))
       (ignore-errors (delete-file (concat path "~"))))))

(ert-deftest test-fio-read-prefers-visiting-buffer ()
  (test-fio--with-file "disk\n"
    (should (equal (efrit-file-read-string path) "disk\n"))
    (with-current-buffer (find-file-noselect path)
      (goto-char (point-max)) (insert "unsaved\n"))
    (should (equal (efrit-file-read-string path) "disk\nunsaved\n"))))

(ert-deftest test-fio-replace-on-disk-when-not-visited ()
  (test-fio--with-file "a b a\n"
    ;; ambiguous without replace_all
    (should-error (efrit-file-replace path "a" "X") :type 'user-error)
    (should (equal (efrit-file-read-string path) "a b a\n"))
    (let ((r (efrit-file-replace path "a" "X" t)))
      (should (eq (plist-get r :via) 'disk))
      (should (= (plist-get r :count) 2))
      (should (equal (efrit-file-read-string path) "X b X\n")))
    (should-error (efrit-file-replace path "zzz" "y") :type 'user-error)
    (let ((r (efrit-file-replace path "b" "B")))
      (should (= (plist-get r :count) 1))
      (should (equal (efrit-file-read-string path) "X B X\n")))))

(ert-deftest test-fio-replace-in-buffer-one-undo-group-and-saves ()
  (test-fio--with-file "foo\nbar\nfoo\n"
    (let ((buf (find-file-noselect path)))
      ;; point on "bar" (outside any replaced text) must be preserved
      (with-current-buffer buf (goto-char 6) (buffer-enable-undo))
      (let ((r (efrit-file-replace path "foo" "baz" t)))
        (should (eq (plist-get r :via) 'buffer))
        (should (= (plist-get r :count) 2))
        (should (plist-get r :saved))
        (with-current-buffer buf
          (should (equal (buffer-string) "baz\nbar\nbaz\n"))
          (should (= (point) 6))                 ; point preserved
          (should-not (buffer-modified-p))
          ;; one undo reverts both replacements
          (primitive-undo 1 buffer-undo-list)
          (should (equal (buffer-string) "foo\nbar\nfoo\n")))
        (with-temp-buffer (insert-file-contents path)
                          (should (equal (buffer-string) "baz\nbar\nbaz\n")))))))

(ert-deftest test-fio-replace-refuses-when-content-changed ()
  "If the file changed after the caller read it, nothing is applied."
  (test-fio--with-file "one\n"
    (let ((seen (efrit-file-read-string path)))
      (with-temp-file path (insert "one\ntwo\n"))
      (should-error (efrit-file-replace path "one" "1" nil seen) :type 'efrit-file-changed)
      (should (equal (efrit-file-read-string path) "one\ntwo\n")))))

(ert-deftest test-fio-replace-respects-read-only-buffer ()
  (test-fio--with-file "x\n"
    (with-current-buffer (find-file-noselect path) (setq buffer-read-only t))
    (should-error (efrit-file-replace path "x" "y") :type 'buffer-read-only)
    (should (equal (efrit-file-read-string path) "x\n"))))

(ert-deftest test-fio-write-string-through-buffer ()
  (test-fio--with-file "old\n"
    (find-file-noselect path)
    (let ((r (efrit-file-write-string path "new\n")))
      (should (eq (plist-get r :via) 'buffer))
      (should (equal (with-current-buffer (get-file-buffer path) (buffer-string)) "new\n"))
      (with-temp-buffer (insert-file-contents path) (should (equal (buffer-string) "new\n"))))))

(ert-deftest test-fio-edit-file-tool-uses-buffer-and-reports ()
  (test-fio--with-file "alpha\n"
    (let ((efrit-project-root (file-name-directory path)) (efrit-project-sandbox t))
      (with-current-buffer (find-file-noselect path)
        (goto-char (point-max)) (insert "beta\n"))   ; unsaved
      (let* ((resp (efrit-tool-edit-file `((path . ,path) (old_str . "beta") (new_str . "gamma"))))
             (result (alist-get 'result resp)))
        (should (eq (alist-get 'success resp) t))
        (should (equal (alist-get 'applied_via result) "buffer"))
        (should (string-match-p "-beta" (alist-get 'diff result)))
        (should (equal (efrit-file-read-string path) "alpha\ngamma\n"))))))

(ert-deftest test-fio-read-file-tool-flags-unsaved ()
  (test-fio--with-file "saved\n"
    (let ((efrit-project-root (file-name-directory path)) (efrit-project-sandbox t))
      (with-current-buffer (find-file-noselect path) (insert "dirty "))
      (let* ((resp (efrit-tool-read-file `((path . ,path))))
             (result (alist-get 'result resp)))
        (should (equal (alist-get 'content result) "dirty saved\n"))
        (should (eq (alist-get 'from_live_buffer result) t))
        (should (cl-some (lambda (w) (string-match-p "UNSAVED" w))
                         (append (alist-get 'warnings resp) nil)))))))

(ert-deftest test-fio-edit-file-warns-on-unbalanced-lisp ()
  (test-fio--with-file "(defun ok () 1)\n"
    (let ((efrit-project-root (file-name-directory path)) (efrit-project-sandbox t))
      (let ((resp (efrit-tool-edit-file `((path . ,path) (old_str . "1)") (new_str . "1")))))
        (should (eq (alist-get 'success resp) t))   ; the edit is applied...
        (should (cl-some (lambda (w) (string-match-p "Lisp syntax problem" w))  ; ...and flagged
                         (append (alist-get 'warnings resp) nil)))))))

(provide 'test-file-io)
;;; test-file-io.el ends here
