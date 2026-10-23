;;; test-tool-outline.el --- Whole-file reads under a budget; project outline -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'efrit-test-sandbox-helpers)
(require 'efrit-tool-outline)
(require 'efrit-do-handlers)

(defmacro test-outline--with-project (&rest body)
  "BODY with `root' a throwaway project of three Lisp files and a text file."
  (declare (indent 0))
  `(let* ((root (file-name-as-directory (make-temp-file "efrit-outline-" t)))
          (efrit-project-root root)
          (default-directory root))
     (unwind-protect
         (progn
           (make-directory (expand-file-name "lisp" root))
           (with-temp-file (expand-file-name "lisp/a.el" root)
             (insert ";;; a.el --- a -*- lexical-binding: t; -*-\n\n(defun a-one () 1)\n\n(defvar a-var 2)\n\n(defun a-two (x) (* x 2))\n"))
           (with-temp-file (expand-file-name "lisp/b.el" root)
             (insert ";;; b.el --- b -*- lexical-binding: t; -*-\n\n(defmacro b-with (&rest body) `(progn ,@body))\n\n(defun b-fn () (a-one))\n"))
           (with-temp-file (expand-file-name "lisp/big.el" root)
             (insert ";;; big.el -*- lexical-binding: t; -*-\n")
             (dotimes (i 400) (insert (format "(defun big-%d () \"%s\" %d)\n\n" i (make-string 60 ?x) i))))
           (with-temp-file (expand-file-name "notes.txt" root) (insert "plain\n"))
           ,@body)
       (clrhash efrit-tool-outline--cache)
       (delete-directory root t))))

(ert-deftest test-outline-of-file-lists-definitions-and-caches-by-mtime ()
  (test-outline--with-project
    (let ((o (efrit-tool-outline-of-file (expand-file-name "lisp/a.el" root))))
      (should (equal '("a-one" "a-var" "a-two") (mapcar #'car o)))
      (should (= 3 (nth 2 (car o))))
      ;; no buffer was left behind
      (should-not (find-buffer-visiting (expand-file-name "lisp/a.el" root)))
      ;; cached: a second call returns the same object
      (should (eq o (efrit-tool-outline-of-file (expand-file-name "lisp/a.el" root))))
      ;; a change invalidates it
      (sleep-for 1.1)
      (with-temp-file (expand-file-name "lisp/a.el" root) (insert "(defun a-only () 1)\n"))
      (should (equal '("a-only") (mapcar #'car (efrit-tool-outline-of-file (expand-file-name "lisp/a.el" root))))))))

(ert-deftest test-read-files-whole-under-a-budget-with-outline-fallback ()
  "Several files come back whole in one result; one that would not fit in
the remaining budget comes back as its outline with a note; missing and
binary files are reported, not fatal; the handler routes paths= here."
  (test-outline--with-project
    (let* ((args `((paths . ,(vector "lisp/a.el" "lisp/b.el" "lisp/big.el" "nope.el"))
                   (budget . 3000)))
           (r (efrit-tool-read-files args))
           (files (append (alist-get 'files (alist-get 'result r)) nil)))
      (should (eq t (alist-get 'success r)))
      (should (= 4 (length files)))
      (should (string-match-p "a-two" (alist-get 'content (nth 0 files))))
      (should (string-match-p "b-with" (alist-get 'content (nth 1 files))))
      ;; big.el (~30 KB) is over a 3000-token budget: outline, not content
      (should-not (alist-get 'content (nth 2 files)))
      (should (string-match-p "big-399" (alist-get 'outline (nth 2 files))))
      (should (string-match-p "over the budget" (alist-get 'note (nth 2 files))))
      (should (equal "not found" (alist-get 'error (nth 3 files))))
      (should (cl-some (lambda (w) (string-match-p "did not fit" w)) (alist-get 'warnings r))))
    ;; the handler dispatches on paths
    (let ((input (make-hash-table :test 'equal)))
      (puthash "paths" (vector "lisp/a.el") input)
      (should (string-match-p "a-one" (efrit-do--handle-read-file input))))
    (let ((input (make-hash-table :test 'equal)))
      (puthash "path" "lisp/a.el" input) (puthash "start_line" 3 input) (puthash "end_line" 3 input)
      (let ((out (efrit-do--handle-read-file input)))
        (should (string-match-p "a-one" out))
        (should-not (string-match-p "a-two" out))))))

(ert-deftest test-project-outline-walks-the-project ()
  (test-outline--with-project
    (let* ((r (efrit-tool-project-outline '((pattern . "*.el"))))
           (res (alist-get 'result r))
           (entries (append (alist-get 'entries res) nil)))
      (should (eq t (alist-get 'success r)))
      (should (= 3 (alist-get 'files res)))
      (should (= (+ 3 2 400) (alist-get 'definitions res)))
      (should (cl-some (lambda (e) (and (equal "lisp/b.el" (alist-get 'file e))
                                        (string-match-p "b-with" (alist-get 'outline e))))
                       entries))
      ;; notes.txt is not a source file
      (should-not (cl-some (lambda (e) (equal "notes.txt" (alist-get 'file e))) entries)))
    ;; max_files caps with a warning
    (let ((r (efrit-tool-project-outline '((max_files . 1)))))
      (should (= 1 (alist-get 'files (alist-get 'result r))))
      (should (cl-some (lambda (w) (string-match-p "not shown" w)) (alist-get 'warnings r))))))

(provide 'test-tool-outline)

;;; test-tool-outline.el ends here
