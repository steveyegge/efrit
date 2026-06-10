;;; lint-toplevel.el --- Detect definition forms swallowed by paren errors -*- lexical-binding: t; -*-

;; Insurance against compensating-paren bugs (see commit 098182c): two
;; balanced-but-wrong parens nested a defun inside its own caller, making
;; it dead code while check-parens and byte-compile both passed.
;;
;; Two checks, run over every .el under lisp/:
;;
;; 1. Depth check: every definition form (defun/defmacro/defvar/defcustom
;;    etc.) must sit at syntax depth 0, unless every enclosing form is an
;;    allowed wrapper (condition-case fallback shims, eval-when-compile...).
;;    A definition nested inside another definition is always an error.
;;
;; 2. Forward-reference check: byte-compile each file and flag any
;;    "function X is not known to be defined" warning where X *is*
;;    textually defined in that same file.  A definition present in the
;;    source but invisible to the compiler means it was swallowed into an
;;    earlier form.
;;
;; Run via: emacs --batch -L ... -l test/lint-toplevel.el (see Makefile
;; lint-toplevel target).  Exits non-zero on any finding.

;;; Code:

(require 'bytecomp)

(defvar lint-toplevel--def-symbols
  '(defun defmacro defvar defconst defcustom defsubst defgroup defface
    defalias define-derived-mode define-minor-mode define-error
    cl-defun cl-defmacro cl-defsubst cl-defstruct cl-defmethod cl-defgeneric)
  "Forms that define something and are expected at top level.")

(defvar lint-toplevel--def-re
  (concat "(" (regexp-opt (mapcar #'symbol-name lint-toplevel--def-symbols) t)
          "\\_>")
  "Regexp matching an open paren followed by a definition symbol.")

(defvar lint-toplevel--allowed-wrappers
  '(condition-case eval-when-compile eval-and-compile cl-eval-when
    when unless if progn)
  "Forms a definition may legitimately be nested inside.
A condition-case handler (whose head is an error symbol) is also
allowed when its parent is a condition-case.")

(defun lint-toplevel--head-at (open-pos)
  "Return the symbol right after the open paren at OPEN-POS, or nil."
  (save-excursion
    (goto-char (1+ open-pos))
    (when-let* ((sym (thing-at-point 'symbol t)))
      (intern-soft sym))))

(defun lint-toplevel--check-wrappers (opens)
  "Validate enclosing forms OPENS (outermost first, buffer positions).
Return nil if all are allowed wrappers, else a string describing
the offending enclosing form."
  (let ((prev nil))
    (catch 'bad
      (dolist (open opens)
        (let ((head (lint-toplevel--head-at open)))
          (cond
           ((memq head lint-toplevel--def-symbols)
            (throw 'bad (format "nested inside (%s ...) — definition swallowed by another definition"
                                head)))
           ((memq head lint-toplevel--allowed-wrappers) nil)
           ((eq prev 'condition-case) nil) ; handler clause, e.g. (error (defun ...))
           (t (throw 'bad (format "nested inside (%s ...)" head))))
          (setq prev head)))
      nil)))

(defun lint-toplevel--check-depth (file)
  "Return list of depth-check error strings for FILE."
  (let ((errors nil))
    (with-temp-buffer
      (insert-file-contents file)
      (delay-mode-hooks (emacs-lisp-mode))
      (goto-char (point-min))
      (while (re-search-forward lint-toplevel--def-re nil t)
        (let* ((pos (match-beginning 0))
               (next (match-end 0))
               (name (match-string 1))
               ;; syntax-ppss does not preserve point; guard against it
               ;; dragging point back and re-matching forever
               (ppss (save-excursion (syntax-ppss pos))))
          (unless (or (nth 3 ppss) (nth 4 ppss) ; skip strings/comments
                      ;; skip quoted templates like `(defun ,name ...) in macros
                      (memq (char-before pos) '(?\` ?\' ?, ?@)))
            (when (> (nth 0 ppss) 0)
              (when-let* ((why (lint-toplevel--check-wrappers (nth 9 ppss))))
                (push (format "%s:%d: (%s ...) not at top level: %s"
                              file (line-number-at-pos pos) name why)
                      errors))))
          (goto-char next))))
    (nreverse errors)))

(defun lint-toplevel--defined-in-file-p (symbol file-contents)
  "Non-nil if SYMBOL appears to be defined by name in FILE-CONTENTS."
  (string-match-p
   (concat "(\\(?:cl-\\)?def\\(?:un\\|macro\\|subst\\|alias\\|method\\|generic\\)"
           "[ \t\n]+'?" (regexp-quote (symbol-name symbol)) "\\_>")
   file-contents))

(defun lint-toplevel--check-unresolved (file)
  "Byte-compile FILE; return errors for unresolved-but-present functions."
  (let* ((warnings nil)
         (errors nil)
         (text-quoting-style 'grave)
         (byte-compile-error-on-warn nil)
         (dest (make-temp-file "lint-toplevel" nil ".elc"))
         (byte-compile-dest-file-function (lambda (_) dest))
         (byte-compile-log-warning-function
          (lambda (string &rest _) (push string warnings))))
    (unwind-protect
        (byte-compile-file file)
      (ignore-errors (delete-file dest)))
    (when warnings
      (let ((contents (with-temp-buffer
                        (insert-file-contents file)
                        (buffer-string))))
        (dolist (w warnings)
          (when (string-match "the function `\\([^']+\\)' is not known to be defined" w)
            (let ((sym (intern (match-string 1 w))))
              (when (lint-toplevel--defined-in-file-p sym contents)
                (push (format "%s: `%s' is defined in this file but invisible to the byte-compiler — definition likely swallowed by a paren error"
                              file sym)
                      errors)))))))
    (nreverse errors)))

(defun lint-toplevel-run ()
  "Run both lints over lisp/; exit non-zero on findings."
  (let ((files (directory-files-recursively "lisp" "\\.el\\'"))
        (all-errors nil))
    (dolist (file files)
      (setq all-errors (nconc all-errors (lint-toplevel--check-depth file))))
    (dolist (file files)
      (setq all-errors (nconc all-errors (lint-toplevel--check-unresolved file))))
    (if all-errors
        (progn
          (message "❌ Top-level form lint failed:")
          (dolist (e all-errors) (message "  %s" e))
          (kill-emacs 1))
      (message "✅ Top-level form lint passed (%d files)" (length files)))))

(lint-toplevel-run)

;;; lint-toplevel.el ends here
