;;; efrit-review-flags.el --- What a proposed Lisp change does, for the reviewer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.9.2
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, ai, lisp

;;; Commentary:

;; The reviewer sees a tool call as JSON.  For an edit to an Emacs Lisp
;; file, or an eval, the JSON says little about what the change DOES:
;; whether it rebinds a global key, adds advice to a core function,
;; points a `use-package' `:vc' block at a different repository, or
;; shadows a library's macro with its own `defmacro'.  Those are the
;; things that make a change dangerous, and the reviewer should weigh
;; them against the request (tzz, 2026-10-01: "the deeper
;; understanding of elisp will also be useful with use-package-vc to
;; understand if a change is dangerous").
;;
;; `efrit-review-flags-for-use' reads the forms a tool call would
;; write or evaluate, collects their EFFECTS (a flat list of (KIND
;; . DETAIL)), and for an edit keeps only the effects the new text has
;; and the old text did not.  Each effect becomes one FLAG line in the
;; reviewer's batch text.  This is static: forms are read, never
;; evaluated; macros are not expanded (the names are what matter).

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup efrit-review-flags nil
  "Static analysis of proposed Lisp changes for the reviewer."
  :group 'efrit-review)

(defcustom efrit-review-flags-max 8
  "At most this many FLAG lines per tool call; the rest are counted."
  :type 'integer
  :group 'efrit-review-flags)

(defconst efrit-review-flags--global-key-fns
  '(global-set-key global-unset-key keymap-global-set keymap-global-unset
    define-key keymap-set bind-key bind-key* bind-keys bind-keys*)
  "Functions and macros that bind keys.")

(defconst efrit-review-flags--hook-fns '(add-hook remove-hook))
(defconst efrit-review-flags--advice-fns '(advice-add advice-remove add-function remove-function defadvice))
(defconst efrit-review-flags--definers '(defun defmacro defalias defsubst cl-defun cl-defmacro define-advice))
(defconst efrit-review-flags--load-fns '(load load-file require autoload))
(defconst efrit-review-flags--danger-fns
  '(delete-file delete-directory kill-emacs save-buffers-kill-emacs shell-command
    call-process start-process make-process async-shell-command url-retrieve
    set-file-modes rename-file copy-file write-region)
  "Calls whose presence in config or an eval is worth a second look.")

(defconst efrit-review-flags--core-vars
  '(load-path exec-path process-environment auth-sources custom-file user-emacs-directory
    file-name-handler-alist enable-local-variables enable-local-eval safe-local-variable-values
    package-archives package-user-dir gnutls-verify-error network-security-level
    tramp-default-method inhibit-startup-screen initial-major-mode)
  "Variables whose assignment changes how Emacs itself behaves.")

;;;; Reading

(defun efrit-review-flags--read-all (text)
  "Every top-level form in TEXT, or nil when it does not read as Lisp."
  (condition-case nil
      (with-temp-buffer
        (insert text)
        (goto-char (point-min))
        (let ((forms nil) (read-circle nil))
          (condition-case nil
              (while t (push (read (current-buffer)) forms))
            (end-of-file nil))
          (nreverse forms)))
    (error nil)))

(defun efrit-review-flags--lisp-text-p (path)
  "Non-nil when PATH names an Emacs Lisp file."
  (and (stringp path) (string-match-p "\\.el\\'" path)))

;;;; Walking

(defun efrit-review-flags--name (x)
  "X printed for a flag: a quoted or sharp-quoted symbol shows bare."
  (format "%S" (if (and (consp x) (memq (car x) '(quote function)) (symbolp (cadr x))) (cadr x) x)))

(defun efrit-review-flags--symbol-lib (sym)
  "A short guess at where SYM comes from: the prefix before the first dash, or nil."
  (let ((name (symbol-name sym)))
    (and (string-match "\\`\\([a-zA-Z0-9]+\\)[-/]" name) (match-string 1 name))))

(defun efrit-review-flags--effects (form &optional own-prefixes)
  "The effects of FORM as a list of (KIND . DETAIL) strings.
OWN-PREFIXES are symbol prefixes that count as the file's own (a
`defmacro' of a name with another prefix is a shadowing)."
  (let ((out nil))
    (cl-labels
        ((walk (f)
           (when (consp f)
             (let ((head (car f)))
               (cond
                ((memq head efrit-review-flags--global-key-fns)
                 (let* ((args (cdr f))
                        (global (memq head '(global-set-key global-unset-key keymap-global-set
                                             keymap-global-unset bind-key bind-key* bind-keys bind-keys*)))
                        (map (and (memq head '(define-key keymap-set)) (car args))))
                   (push (cons 'key (format "%s %s%s" head
                                            (if global "(global)" (format "in %S" map))
                                            (if (eq head 'bind-keys) "" (format " %s" (efrit-review-flags--name (if global (car args) (cadr args)))))))
                         out)))
                ((memq head efrit-review-flags--hook-fns)
                 (push (cons 'hook (format "%s %s %s" head (efrit-review-flags--name (cadr f)) (efrit-review-flags--name (caddr f)))) out))
                ((memq head efrit-review-flags--advice-fns)
                 (push (cons 'advice (format "%s %s" head (efrit-review-flags--name (cadr f)))) out))
                ((memq head efrit-review-flags--definers)
                 (let* ((name (cadr f))
                        (lib (and (symbolp name) (efrit-review-flags--symbol-lib name))))
                   (when (and lib (not (member lib own-prefixes))
                              (memq head '(defmacro defalias defun cl-defmacro)))
                     (push (cons 'shadow (format "%s %S defines a name with another library's prefix (%s)" head name lib)) out))))
                ((memq head '(setq setq-default setopt customize-set-variable set-default))
                 (let ((args (cdr f)))
                   (while args
                     (let ((var (if (memq head '(customize-set-variable set-default)) (cadr (car args)) (car args))))
                       (when (and (symbolp var) (memq var efrit-review-flags--core-vars))
                         (push (cons 'core-var (format "%s %S" head var)) out)))
                     (setq args (if (memq head '(customize-set-variable set-default)) nil (cddr args))))))
                ((memq head efrit-review-flags--load-fns)
                 (push (cons 'load (format "%s %s" head (efrit-review-flags--name (cadr f)))) out))
                ((memq head efrit-review-flags--danger-fns)
                 (push (cons 'effect (format "%s %s" head (truncate-string-to-width (format "%S" (cdr f)) 60 nil nil "…"))) out))
                ((eq head 'use-package)
                 (let ((name (cadr f)) (plist (cddr f)))
                   (let ((vc (plist-get plist :vc)))
                     (when vc (push (cons 'vc (format "use-package %S :vc %S" name vc)) out)))
                   (when (plist-get plist :init)
                     (push (cons 'init (format "use-package %S has :init (runs at startup, before the package loads)" name)) out))
                   (when (plist-get plist :ensure)
                     (push (cons 'ensure (format "use-package %S :ensure %S (installs a package)" name (plist-get plist :ensure))) out)))))
               ;; recurse into everything but quoted data; the tail
               ;; may be dotted ("C-=" . er/expand-region)
               (unless (memq head '(quote function))
                 (let ((rest (cdr f)))
                   (while (consp rest)
                     (walk (pop rest)))))))))
      (walk form))
    (nreverse out)))

(defun efrit-review-flags--text-effects (text own-prefixes)
  "Effects of every form in TEXT; nil when TEXT is not Lisp."
  (let ((forms (efrit-review-flags--read-all text)))
    (cl-mapcan (lambda (f) (efrit-review-flags--effects f own-prefixes)) forms)))

(defun efrit-review-flags--own-prefixes (path)
  "Symbol prefixes a file at PATH may define without it being a shadowing."
  (when (stringp path)
    (let ((base (file-name-base path)))
      (delete-dups
       (delq nil (list (and (string-match "\\`\\([a-zA-Z0-9]+\\)" base) (match-string 1 base))
                       ;; tzz.emacs.libraries.el defines tzz- names
                       (and (string-match "\\`\\([a-zA-Z0-9]+\\)\\." base) (match-string 1 base))))))))

;;;; Per tool call

(defun efrit-review-flags--diff (old new)
  "Effects in NEW not in OLD (compared as strings)."
  (cl-remove-if (lambda (e) (member e old)) new))

(defun efrit-review-flags-for-use (use)
  "FLAG strings for tool USE, a (TOOL-ID TOOL-NAME INPUT) triple, or nil.
Edits to Lisp files are judged by the effects their new text adds;
evals by the effects of the form; shell lines by what they start."
  (let* ((name (nth 1 use)) (input (nth 2 use))
         (get (lambda (k) (and (hash-table-p input) (gethash k input))))
         (effects
          (pcase name
            ((or "edit_file" "edit_buffer")
             (let ((path (or (funcall get "path") (funcall get "buffer"))))
               (when (or (efrit-review-flags--lisp-text-p path) (funcall get "buffer"))
                 (let ((own (efrit-review-flags--own-prefixes path)))
                   (efrit-review-flags--diff
                    (efrit-review-flags--text-effects (or (funcall get "old_str") (funcall get "old_text") "") own)
                    (efrit-review-flags--text-effects (or (funcall get "new_str") (funcall get "new_text") "") own))))))
            ("create_file"
             (let ((path (funcall get "path")))
               (when (efrit-review-flags--lisp-text-p path)
                 (efrit-review-flags--text-effects (or (funcall get "content") "")
                                                   (efrit-review-flags--own-prefixes path)))))
            ("eval_sexp"
             (efrit-review-flags--text-effects (or (funcall get "expr") "") nil))
            (_ nil))))
    (let ((lines (mapcar (lambda (e) (format "[FLAG %s: %s]" (car e) (cdr e))) (delete-dups effects))))
      (if (> (length lines) efrit-review-flags-max)
          (append (seq-take lines efrit-review-flags-max)
                  (list (format "[… %d more flag(s)]" (- (length lines) efrit-review-flags-max))))
        lines))))

(provide 'efrit-review-flags)

;;; efrit-review-flags.el ends here
