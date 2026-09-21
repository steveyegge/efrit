;;; test-settings.el --- per-project settings file and registry -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'efrit-settings)

(defvar efrit-project-root)

(defmacro test-settings--in-project (&rest body)
  (declare (indent 0))
  `(let* ((root (file-name-as-directory (make-temp-file "efrit-set-" t)))
          (efrit-project-root root)
          (efrit-data-directory (expand-file-name "data" root))
          (efrit-settings--cache (make-hash-table :test 'equal)))
     (unwind-protect (progn ,@body)
       (delete-directory root t))))

(defun test-settings--obj (&rest kvs)
  (let ((h (make-hash-table :test 'equal)))
    (while kvs (puthash (pop kvs) (pop kvs) h))
    h))

(ert-deftest test-settings-put-get-roundtrip-keeps-other-sections ()
  (test-settings--in-project
    (should-not (efrit-settings-get root "review"))
    (efrit-settings-put root "review" (test-settings--obj "enabled" :false "classes" '("write")))
    (efrit-settings-put root "limits" (test-settings--obj "max-iterations" 200))
    (efrit-settings-forget)
    (let ((review (efrit-settings-get root "review"))
          (limits (efrit-settings-get root "limits")))
      (should (equal (gethash "classes" review) '("write")))
      (should (eq (gethash "enabled" review) :false))
      (should (= 200 (gethash "max-iterations" limits))))
    ;; file is private and carries the version
    (let ((file (efrit-settings-file root)))
      (should (= 0 (logand (file-modes file) #o077)))
      (with-temp-buffer
        (insert-file-contents file)
        (should (string-match-p "\"version\":1" (buffer-string)))))
    ;; removing one section leaves the other; removing the last deletes the file
    (efrit-settings-put root "review" nil)
    (efrit-settings-forget)
    (should-not (efrit-settings-get root "review"))
    (should (efrit-settings-get root "limits"))
    (efrit-settings-put root "limits" nil)
    (should-not (file-exists-p (efrit-settings-file root)))))

(ert-deftest test-settings-unknown-section-survives-a-save ()
  "A newer efrit's section is not dropped by an older one's save."
  (test-settings--in-project
    (let ((file (efrit-settings-file root)))
      (make-directory (file-name-directory file) t)
      (with-temp-file file (insert "{\"version\":1,\"future\":{\"x\":[1,2]}}"))
      (efrit-settings-put root "limits" (test-settings--obj "max-iterations" 5))
      (efrit-settings-forget)
      (should (equal (gethash "x" (efrit-settings-get root "future")) '(1 2))))))

(ert-deftest test-settings-bad-file-reads-as-empty-and-is-not-clobbered-silently ()
  (test-settings--in-project
    (let ((file (efrit-settings-file root)))
      (make-directory (file-name-directory file) t)
      (with-temp-file file (insert "not json"))
      (should-not (efrit-settings-get root "limits"))
      (with-temp-file file (insert "[1,2,3]"))
      (efrit-settings-forget)
      (should-not (efrit-settings-get root "limits")))))

(ert-deftest test-settings-helpers-validate ()
  (should (equal (efrit-settings-symbol-list '("write" "exec") '(write exec net)) '(write exec)))
  (should-not (efrit-settings-symbol-list '("write" "bogus") '(write exec net)))
  (should-not (efrit-settings-symbol-list "write" '(write)))
  (should (equal (efrit-settings-symbol-list nil '(write)) nil))
  (should (eq (efrit-settings-json-bool t) t))
  (should (eq (efrit-settings-json-bool :false) nil))
  (should (eq (efrit-settings-json-bool nil) nil))
  (should (eq (efrit-settings-json-bool "yes") 'unset)))

(ert-deftest test-settings-registry-records-projects-most-recent-first ()
  (test-settings--in-project
    (let* ((a (file-name-as-directory (expand-file-name "a" root)))
           (b (file-name-as-directory (expand-file-name "b" root)))
           (gone (file-name-as-directory (expand-file-name "gone" root))))
      (make-directory a) (make-directory b) (make-directory gone)
      (should-not (efrit-settings-known-projects))
      (efrit-settings-put a "limits" (test-settings--obj "max-iterations" 1))
      (efrit-settings-register-project b)
      (efrit-settings-register-project gone)
      (should (equal (efrit-settings-known-projects) (list gone b a)))
      ;; re-registering moves to the front; a vanished root is filtered
      (efrit-settings-register-project a)
      (delete-directory gone)
      (should (equal (efrit-settings-known-projects) (list a b)))
      (should (= 0 (logand (file-modes (efrit-settings-registry-file)) #o077))))))

(ert-deftest test-settings-write-json-is-utf-8-without-prompt ()
  "`json-serialize' returns unibyte UTF-8; the writer must not hand it to a
multibyte buffer, where it becomes raw bytes and a coding prompt."
  (let* ((dir (file-name-as-directory (make-temp-file "efrit-sj-" t)))
         (file (expand-file-name "sub/x.json" dir))
         (h (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          (puthash "t" "a \u2014 \u00D8 \U0001F600" h)
          (cl-letf (((symbol-function 'select-safe-coding-system)
                     (lambda (&rest _) (error "coding system prompt reached"))))
            (efrit-settings-write-json file h))
          (should (= 0 (logand (file-modes file) #o077)))
          (with-temp-buffer
            (set-buffer-multibyte nil)
            (insert-file-contents-literally file)
            ;; the em dash on disk is its three UTF-8 bytes
            (should (string-match-p "a \xe2\x80\x94 " (buffer-string))))
          (should (equal (gethash "t" (with-temp-buffer (insert-file-contents file)
                                                       (json-parse-buffer)))
                         "a \u2014 \u00D8 \U0001F600")))
      (delete-directory dir t))))

(provide 'test-settings)
;;; test-settings.el ends here
