;;; efrit-sandbox-eval.el --- Sandbox enforcement for eval_sexp -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; eval_sexp is efrit's most powerful tool and, unguarded, makes every
;; other check moot.  Elisp lets us do better than a blocklist:
;;
;; 1. STATIC.  Before evaluation the form is `macroexpand-all'ed and
;;    walked.  Every function position and every quoted/sharp-quoted
;;    symbol is collected.  Forms that touch the sandbox's own
;;    machinery (efrit-sandbox-*, the handler alist, advice on the
;;    checked primitives, `fset'/`defalias' of them) are refused outright with an explanation
;;    the model can act on -- no grant covers them.
;;
;; 2. DYNAMIC.  During evaluation:
;;    - a `file-name-handler-alist' entry matching every file name
;;      routes each primitive file operation through
;;      `efrit-sandbox-check' with the right capability, then
;;      delegates to the real operation.  This catches `find-file',
;;      `insert-file-contents', `write-region', `delete-file',
;;      `rename-file', `directory-files', `save-buffer' (via
;;      write-region), `process-file'/`start-file-process', Tramp,
;;      everything that resolves a file name.
;;    - `make-process', `call-process', `call-process-region',
;;      `start-process', `shell-command*' and `make-network-process'
;;      are advised to require the `shell' / `net' capability.
;;    - `save-buffer'/`write-file' on an existing visiting buffer go
;;      through write-region and are therefore checked for `write' on
;;      the buffer's file.
;;
;; 3. NEEDS `elisp' ITSELF.  Evaluating anything at all requires the
;;    `elisp' capability; that is what the first eval_sexp of a
;;    project asks for.  Once granted, the per-operation checks above
;;    still apply, so "may evaluate Lisp" does not mean "may write
;;    ~/.emacs.d".
;;
;; Limits, stated plainly: this is enforcement inside the same Lisp
;; image as the code being enforced.  A form that first `fset's
;; `efrit-sandbox-check' would defeat it -- which is why the static
;; walk refuses any reference to those symbols, including via `intern'
;; or `symbol-function' on a computed name (those are refused too).
;; Dynamic modules, `call-interactively' of a command that shells out,
;; and timers/hooks that fire *after* the eval returns are outside the
;; dynamic window; the process advice stays installed while any
;; efrit-registered timer is pending to close the common case.  Every
;; refusal and every grant is logged.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'pp)
(require 'efrit-sandbox)
(require 'efrit-log)

;;; 1. Static inspection

(defconst efrit-sandbox-eval--forbidden-symbols
  '(;; the sandbox itself
    efrit-sandbox-check efrit-sandbox-grant efrit-sandbox-revoke
    efrit-sandbox-allowed-p efrit-sandbox-reset-session
    efrit-sandbox-enabled efrit-sandbox-default-project-grants
    efrit-sandbox-always-deny efrit-sandbox-request-function
    efrit-sandbox--session-grants efrit-sandbox--project-grants
    efrit-sandbox--once-grant efrit-sandbox-store-save efrit-sandbox-store-load
    efrit-sandbox-store--loaded efrit-sandbox-store-forget
    efrit-sandbox-eval--active efrit-sandbox-eval-form
    efrit-permission-policy efrit-permission-responder-function
    efrit-permission-reset efrit-permission--session-grants
    efrit-permission-tool-classes efrit-project-sandbox efrit-project-root
    ;; mechanisms that could disable the checks
    file-name-handler-alist inhibit-file-name-handlers inhibit-file-name-operation
    advice-add advice-remove advice--add-function add-function remove-function
    fset defalias fmakunbound symbol-function
    custom-set-variables
    ;; indirection that would hide the above from this walk
    intern intern-soft eval funcall-interactively call-interactively
    load load-file eval-buffer eval-region
    module-load)
  "Symbols an eval_sexp form may not reference in any position.")

(defconst efrit-sandbox-eval--forbidden-prefixes
  '("efrit-sandbox" "efrit-permission" "efrit-review" "efrit-limits" "efrit-settings"
    "efrit-package-review" "efrit-testdrive")
  "Symbol-name prefixes refused wherever they appear.
efrit-review, efrit-limits and efrit-settings are included so the model
cannot switch off its own reviewer, raise its own iteration cap, or
rewrite the project settings file from eval_sexp.")

(defun efrit-sandbox-eval--forbidden-p (sym)
  (and (symbolp sym) sym
       (or (memq sym efrit-sandbox-eval--forbidden-symbols)
           (let ((n (symbol-name sym)))
             (cl-some (lambda (p) (string-prefix-p p n))
                      efrit-sandbox-eval--forbidden-prefixes)))))

(defun efrit-sandbox-eval--walk (form acc)
  "Collect into ACC (a cons cell holding a list) every symbol in FORM.
Descends into quoted data too: a quoted symbol can be `funcall'ed."
  (cond
   ((symbolp form) (push form (car acc)))
   ((consp form)
    (efrit-sandbox-eval--walk (car form) acc)
    (let ((rest (cdr form)))
      (while (consp rest)
        (efrit-sandbox-eval--walk (car rest) acc)
        (setq rest (cdr rest)))
      (when rest (efrit-sandbox-eval--walk rest acc))))
   ((vectorp form) (mapc (lambda (x) (efrit-sandbox-eval--walk x acc)) form))
   ((stringp form)
    ;; a string that names a forbidden symbol is how one would reach
    ;; it via `intern'; `intern' is refused, but be thorough
    (when (efrit-sandbox-eval--forbidden-p (intern-soft form))
      (push (intern-soft form) (car acc)))))
  acc)

(defun efrit-sandbox-eval-inspect (form)
  "Return nil if FORM is acceptable, else a string saying why not.
Expands macros first so nothing hides behind a macro."
  (let* ((expanded (condition-case err
                       (macroexpand-all form)
                     (error (list 'efrit-sandbox--unexpandable
                                  (error-message-string err)))))
         (acc (efrit-sandbox-eval--walk expanded (list nil)))
         (bad (cl-remove-duplicates
               (cl-remove-if-not #'efrit-sandbox-eval--forbidden-p (car acc)))))
    (when bad
      (format "the form references %s, which the sandbox never permits from eval_sexp. Use the dedicated tools (read_file, edit_file, shell_exec) or ask the user."
              (mapconcat (lambda (s) (format "`%s'" s)) bad ", ")))))

;;; 2. Dynamic enforcement

(defvar efrit-sandbox-eval--active nil
  "Non-nil while a sandboxed eval is running (dynamic extent).")

(defvar efrit-sandbox-eval--in-guard nil
  "Non-nil while a guard runs its check (see the process guard).")

(defconst efrit-sandbox-eval--write-ops
  '(write-region delete-file rename-file copy-file make-directory
    delete-directory set-file-modes set-file-times add-name-to-file
    make-symbolic-link copy-directory set-file-acl set-file-selinux-context)
  "File-name-handler operations that modify the filesystem.")

(defconst efrit-sandbox-eval--exec-ops
  '(process-file start-file-process shell-command make-process)
  "Operations that run programs (the handler sees these for remote names).")

(defconst efrit-sandbox-eval--read-ops
  '(insert-file-contents file-attributes directory-files
    directory-files-and-attributes file-name-all-completions
    file-name-completion load access-file file-system-info file-acl
    file-selinux-context)
  "Operations that reveal file *contents or listings*; need `read'.
Existence/type probes (file-exists-p, file-directory-p, file-symlink-p,
file-truename, file-readable-p, ...) are deliberately not gated: Emacs
calls them on every ancestor while resolving any path, they leak
almost nothing, and gating them makes `expand-file-name' on a path
outside the sandbox fail before the real operation is even attempted.")

(defun efrit-sandbox-eval--op-cap (op)
  "Capability OP needs, or nil for pure name manipulation
\(expand-file-name, file-name-directory, abbreviate-file-name, ...)."
  (cond ((memq op efrit-sandbox-eval--write-ops) 'write)
        ((memq op efrit-sandbox-eval--exec-ops) 'shell)
        ((memq op efrit-sandbox-eval--read-ops) 'read)
        (t nil)))

(defun efrit-sandbox-eval--op-paths (op args)
  "The file name arguments of OP in ARGS that need checking."
  (pcase op
    ((or 'rename-file 'copy-file 'add-name-to-file 'make-symbolic-link 'copy-directory)
     (list (nth 0 args) (nth 1 args)))
    ((or 'write-region) (list (nth 2 args)))
    ((or 'process-file 'start-file-process 'shell-command) nil) ; capability only
    (_ (and (stringp (car args)) (list (car args))))))

(defun efrit-sandbox-eval--handler (op &rest args)
  "The file-name handler: check, then run the real OP."
  (let ((inhibit-file-name-handlers
         (cons #'efrit-sandbox-eval--handler
               (and (eq inhibit-file-name-operation op) inhibit-file-name-handlers)))
        (inhibit-file-name-operation op))
    (when (and efrit-sandbox-eval--active (not efrit-sandbox-eval--in-guard))
      (let ((cap (efrit-sandbox-eval--op-cap op))
            (efrit-sandbox-eval--in-guard t))
        (cond
         ((null cap) nil)
         ((eq cap 'shell)
          ;; the command line is not recoverable from the handler
          ;; arguments in general; ask for the blanket grant
          (efrit-sandbox-check 'shell t "eval_sexp" (format "%s" op)))
         (t
          (dolist (p (efrit-sandbox-eval--op-paths op args))
            (when (and (stringp p) (not (string-empty-p p)))
              (efrit-sandbox-check cap p "eval_sexp" (format "%s" op))))))))
    (apply op args)))

(defconst efrit-sandbox-eval--handler-entry
  (cons "\\`.*\\'" #'efrit-sandbox-eval--handler))

;; Process / network advice.  Installed once; only active inside a
;; sandboxed eval.
(defun efrit-sandbox-eval--guard-process (orig &rest args)
  (when (and efrit-sandbox-eval--active (not efrit-sandbox-eval--in-guard))
    (let ((efrit-sandbox-eval--in-guard t))
      (let* ((command (or (plist-get args :command)
                          (and (stringp (car args)) (list (car args)))))
             (line (cond ((and (listp command) command (cl-every #'stringp command))
                          (mapconcat #'shell-quote-argument command " "))
                         ((stringp command) command)
                         (t t))))
        ;; a process with a known argv is checked like a shell line, so
        ;; a grant for "git" covers (call-process "git" ...) as well
        (efrit-sandbox-check 'shell line "eval_sexp"
                             (if (stringp line) line "process")))))
  ;; A nested advised call (call-process under shell-command-to-string)
  ;; runs with the guard suppressed: the outer check already passed.
  (let ((efrit-sandbox-eval--in-guard t))
    (apply orig args)))

(defun efrit-sandbox-eval--guard-network (orig &rest args)
  (when (and efrit-sandbox-eval--active (not efrit-sandbox-eval--in-guard))
    (let ((efrit-sandbox-eval--in-guard t))
      (efrit-sandbox-check 'net t "eval_sexp"
                           (format "%s" (or (plist-get args :host) "network")))))
  (let ((efrit-sandbox-eval--in-guard t))
    (apply orig args)))

;; Buffer guard.  A live buffer visiting a file outside the project
;; exposes that file's contents through buffer operations that resolve
;; no file name, so the file-name handler above never sees them.
;; `set-buffer' is the chokepoint `with-current-buffer' and
;; `save-current-buffer' expand to; the cross-buffer readers below take
;; another buffer without switching.  Only file-visiting out-of-project
;; buffers are checked: fileless buffers (`with-temp-buffer', output
;; buffers) are the model's own scratch space and must pass freely, or
;; every eval that formats output would prompt.
(defun efrit-sandbox-eval--check-buffer (buffer-or-name op)
  "Check BUFFER-OR-NAME for the `buffer' capability if it visits a file.
OP names the operation for the prompt.  Fileless or missing buffers
pass; efrit's buffer check applies the target/in-project exemptions."
  (let ((buffer (and buffer-or-name (get-buffer buffer-or-name))))
    (when (and buffer
               (buffer-local-value 'buffer-file-name buffer)
               (not (efrit-sandbox-buffer-allowed-p buffer)))
      (efrit-sandbox-check-buffer buffer "eval_sexp" (format "%s" op)))))

(defun efrit-sandbox-eval--guard-set-buffer (orig &rest args)
  (when (and efrit-sandbox-eval--active (not efrit-sandbox-eval--in-guard))
    (let ((efrit-sandbox-eval--in-guard t))
      (efrit-sandbox-eval--check-buffer (car args) 'set-buffer)))
  (apply orig args))

(defun efrit-sandbox-eval--guard-read-buffer (orig &rest args)
  "Guard readers whose *source* buffer is another buffer.
For `insert-buffer'/`insert-buffer-substring*' the source is the first
argument; for `replace-buffer-contents' too; `buffer-swap-text' swaps
the current buffer with its argument, so both must be allowed, but the
current buffer is where eval already runs."
  (when (and efrit-sandbox-eval--active (not efrit-sandbox-eval--in-guard))
    (let ((efrit-sandbox-eval--in-guard t))
      (efrit-sandbox-eval--check-buffer (car args) 'read-buffer)))
  (apply orig args))

(defconst efrit-sandbox-eval--process-fns
  '(make-process call-process call-process-region start-process
    shell-command shell-command-to-string async-shell-command
    process-lines start-file-process process-file))

(defconst efrit-sandbox-eval--read-buffer-fns
  '(insert-buffer insert-buffer-substring insert-buffer-substring-no-properties
    replace-buffer-contents buffer-swap-text)
  "Readers/mutators that take another buffer without switching to it.")

(defun efrit-sandbox-eval--install-advice ()
  (dolist (fn efrit-sandbox-eval--process-fns)
    (unless (advice-member-p #'efrit-sandbox-eval--guard-process fn)
      (advice-add fn :around #'efrit-sandbox-eval--guard-process)))
  (dolist (fn '(make-network-process open-network-stream url-retrieve
                url-retrieve-synchronously))
    (unless (advice-member-p #'efrit-sandbox-eval--guard-network fn)
      (advice-add fn :around #'efrit-sandbox-eval--guard-network)))
  (unless (advice-member-p #'efrit-sandbox-eval--guard-set-buffer 'set-buffer)
    (advice-add 'set-buffer :around #'efrit-sandbox-eval--guard-set-buffer))
  (dolist (fn efrit-sandbox-eval--read-buffer-fns)
    (unless (advice-member-p #'efrit-sandbox-eval--guard-read-buffer fn)
      (advice-add fn :around #'efrit-sandbox-eval--guard-read-buffer))))

(defcustom efrit-sandbox-eval-detail-max-chars 1200
  "Longest form shown in the sandbox prompt; longer ones are cut with a note."
  :type 'integer
  :group 'efrit-sandbox)

(defun efrit-sandbox-eval--describe-form (form)
  "FORM pretty-printed for the sandbox prompt, cut to a readable size."
  (let ((text (condition-case nil
                  (string-trim-right (pp-to-string form))
                (error (format "%S" form)))))
    (if (> (length text) efrit-sandbox-eval-detail-max-chars)
        (concat (substring text 0 efrit-sandbox-eval-detail-max-chars)
                (format "\n… [%d more chars]" (- (length text) efrit-sandbox-eval-detail-max-chars)))
      text)))

(defun efrit-sandbox-eval-form (form &optional evaluator)
  "Evaluate FORM under the sandbox; return its value.
Requires the `elisp' capability, refuses forms the static inspection
rejects (signalling `efrit-sandbox-denied' with a request whose
detail explains), and runs FORM with the file-name handler and
process advice active.  EVALUATOR, if given, is called with FORM
instead of `eval' (the caller's timeout/input-blocking wrapper)."
  ;; The detail is the form itself: that is what the user is deciding
  ;; about.  Pretty-printed and capped so a huge form stays readable.
  (efrit-sandbox-check 'elisp t "eval_sexp" (efrit-sandbox-eval--describe-form form))
  (when-let* ((why (efrit-sandbox-eval-inspect form)))
    (efrit-log 'warn "sandbox: refused eval form: %s" why)
    (signal 'efrit-sandbox-denied
            (list (efrit-sandbox-request-create :cap 'elisp :target t :tool "eval_sexp"
                                                :detail why))))
  (efrit-sandbox-eval--install-advice)
  (let ((efrit-sandbox-eval--active t)
        (file-name-handler-alist (cons efrit-sandbox-eval--handler-entry
                                       file-name-handler-alist)))
    (if evaluator (funcall evaluator form) (eval form t))))

(provide 'efrit-sandbox-eval)

;;; efrit-sandbox-eval.el ends here
