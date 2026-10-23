;;; efrit-sandbox-eval.el --- Sandbox enforcement for eval_sexp -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.12.12
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
(require 'xdg)

;;; 1. Static inspection

(defconst efrit-sandbox-eval--forbidden-symbols
  '(;; the sandbox itself
    efrit-sandbox-check efrit-sandbox-grant efrit-sandbox-revoke
    efrit-sandbox-allowed-p efrit-sandbox-reset-session
    efrit-sandbox-enabled efrit-sandbox-default-project-grants
    efrit-sandbox-always-deny efrit-sandbox-request-function
    efrit-sandbox--session-grants efrit-sandbox--project-grants
    efrit-sandbox--turn-state efrit-sandbox-store-save efrit-sandbox-store-load
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

(defconst efrit-sandbox-eval--blocking-symbols
  '(package-refresh-contents package-install package-install-file
    package-install-selected-packages package-upgrade package-upgrade-all
    package-reinstall package-delete package-autoremove package-menu-execute
    package-vc-install package-vc-upgrade package-vc-upgrade-all
    package-vc-install-from-checkout package-vc-checkout
    package-import-keyring package-download-transaction
    url-retrieve-synchronously url-insert-file-contents url-copy-file
    url-file-local-copy)
  "Functions that hold the Emacs event loop until they finish.
Package operations download archives, verify signatures with gpg
through `call-process' and compile, with the user's Emacs frozen the
whole time and no timer able to interrupt them (the eval timeout did
not fire; 2026-10-21 a `package-upgrade' from eval_sexp sat on
\"Importing package-keyring.gpg...\" until the user's C-g).  They
belong in a batch Emacs via run_program, or in the user's hands.")

(defun efrit-sandbox-eval--blocking-p (sym)
  (and (symbolp sym) (memq sym efrit-sandbox-eval--blocking-symbols)))

(defun efrit-sandbox-eval-inspect (form)
  "Return nil if FORM is acceptable, else a string saying why not.
Expands macros first so nothing hides behind a macro.  Two kinds of
refusal: symbols the sandbox never permits (its own machinery), and
symbols that freeze the user's Emacs (`efrit-sandbox-eval--blocking-symbols')."
  (let* ((expanded (condition-case err
                       (macroexpand-all form)
                     (error (list 'efrit-sandbox--unexpandable
                                  (error-message-string err)))))
         (acc (efrit-sandbox-eval--walk expanded (list nil)))
         (symbols (cl-remove-duplicates (car acc)))
         (bad (cl-remove-if-not #'efrit-sandbox-eval--forbidden-p symbols))
         (blocking (cl-remove-if-not #'efrit-sandbox-eval--blocking-p symbols)))
    (cond
     (bad
      (format "the form references %s, which the sandbox never permits from eval_sexp. Use the dedicated tools instead: read_file, edit_file, run_program; load_project_file to load a project .el file; run_ert to run tests. Or ask the user."
              (mapconcat (lambda (s) (format "`%s'" s)) bad ", ")))
     (blocking
      (format "the form calls %s, which freezes the user's Emacs until it finishes and cannot be interrupted or timed out. Run it in a separate Emacs with run_program (emacs --batch --eval ...), or ask the user to run it themselves; then verify the outcome from eval_sexp (package-installed-p, locate-library)."
              (mapconcat (lambda (s) (format "`%s'" s)) blocking ", "))))))

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
    file-name-completion access-file file-system-info file-acl
    file-selinux-context)
  "Operations that reveal file *contents or listings*; need `read'.
Existence/type probes (file-exists-p, file-directory-p, file-symlink-p,
file-truename, file-readable-p, ...) are deliberately not gated: Emacs
calls them on every ancestor while resolving any path, they leak
almost nothing, and gating them makes `expand-file-name' on a path
outside the sandbox fail before the real operation is even attempted.

`load' is not gated either (it was until 2026-09-27).  Loading a
library is not the model reading the user's files: the file comes
from `load-path', which `efrit-sandbox-eval--exempt-p' exempts anyway.
Handling `load' ourselves was also harmful: the bare name was checked
against `default-directory' (a remote root made that a spurious
remote read), and a prompt raised from inside the loader wedged Emacs
until answered, after which the .elc open failed with \"Opening stdio
stream: Invalid argument\".")

(defun efrit-sandbox-eval--cache-directories ()
  "Directories Emacs libraries keep their own state in.
Reading them is the library's housekeeping, not the model reading the
user's data: `find-file-noselect' on an Org file loads the element
cache from `org-persist-directory' (2026-10-22: a clock edit asked
for `~/.cache/org-persist/')."
  (delq nil (list (bound-and-true-p org-persist-directory)
                  (ignore-errors (xdg-cache-home))
                  (and (boundp 'user-emacs-directory)
                       (expand-file-name "var" user-emacs-directory))
                  (and (boundp 'user-emacs-directory)
                       (expand-file-name "eln-cache" user-emacs-directory))
                  (and (boundp 'native-comp-eln-load-path) (car native-comp-eln-load-path)))))


(defun efrit-sandbox-eval--exempt-p (path)
  "Non-nil if PATH is Emacs's own: under `load-path', `data-directory'
or a library cache directory (`efrit-sandbox-eval--cache-directories').
Reading those is never the user's data; a `(require ...)' or
`find-library' must not prompt."
  (let ((path (expand-file-name path)))
    (or (and data-directory (string-prefix-p (file-name-as-directory data-directory) path))
        (cl-some (lambda (dir)
                   (and (stringp dir)
                        (string-prefix-p (file-name-as-directory (expand-file-name dir)) path)))
                 (append load-path (efrit-sandbox-eval--cache-directories))))))

(defcustom efrit-sandbox-eval-allow-emacs nil
  "When non-nil, an eval may start another Emacs after the usual prompt.
Off by default: another Emacs is never what a task needs; the one
case is testing a package in a clean `emacs -Q', and for that bind
this around the call.  Emacs's own native-compiler subprocesses are
recognised and allowed regardless."
  :type 'boolean
  :group 'efrit-sandbox)

(defconst efrit-sandbox-eval--native-comp-frames
  '(comp-trampoline-compile comp--trampoline-compile comp-run-async-workers
    comp--run-async-workers native-compile-async native--compile-async
    comp--native-compile native-compile comp-subr-trampoline-install
    comp--subr-trampoline-install comp-trampoline-search)
  "Functions on the stack when Emacs's native compiler is doing its own work.")

(defconst efrit-sandbox-eval--emacs-helper-frames
  '(jka-compr-call-process jka-compr-insert-file-contents jka-compr-write-region
    jka-compr-file-local-copy jka-compr-load
    epa-file-insert-file-contents epa-file-write-region epg--start
    tramp-sh-handle-file-local-copy tramp-sh-handle-insert-file-contents
    archive-extract archive-extract-by-stdout
    auto-compression-mode
    ;; packages that save alongside the file, through jka-compr
    undo-fu-session--save-impl undo-fu-session-save undo-fu-session--save-safe
    undo-fu-session--recover-impl undo-fu-session-recover undo-fu-session--recover-safe
    basic-save-buffer save-buffer
    ;; VC asking the backend about a file Emacs just visited or saved:
    ;; `git ls-files' under `vc-git-registered' (2026-10-19, from a
    ;; `save-buffer' in the form; the user: "could have been resolved
    ;; with VC mode").  VC is Emacs's; `efrit-vcs' routes the model's
    ;; own version-control work through it on purpose
    vc-git-registered vc-git--out-ok vc-git--call vc-git-command vc-git-state vc-git-root
    vc-registered vc-backend vc-state vc-refresh-state vc-find-file-hook vc-after-save
    vc-call-backend vc-responsible-backend vc-git-mode-line-string vc-mode-line
    after-find-file find-file-noselect find-file-noselect-1)
  "Frames of Emacs file machinery that spawns a helper on the user's behalf.
Visiting `notes.org.gz' runs `sh -c gzip -c -q' (jka-compr); a `.gpg'
file runs gpg; a remote file over TRAMP runs ssh.  Those are how Emacs
reads the file the model asked for, and the file itself already went
through the file-name handler and its grant.  Asking about the helper
told the user efrit wanted \"a shell decompress\" (tzz, 2026-10-16).")

(defun efrit-sandbox-eval--emacs-helper-p ()
  "The Emacs file-machinery frame on the stack, or nil."
  (let ((frame nil))
    (mapbacktrace (lambda (_evald func _args _flags)
                    (when (and (not frame) (memq func efrit-sandbox-eval--emacs-helper-frames))
                      (setq frame func))))
    frame))

(defun efrit-sandbox-eval--native-comp-p (&optional command)
  "Non-nil when the current operation is Emacs's native compiler, not the model.
Calling any not-yet-compiled primitive-advised function makes Emacs
write a trampoline under `temporary-file-directory' and start a child
Emacs in batch mode to compile it (2026-10-05: a buffer_create turn
asked tzz whether eval_sexp may \"run Emacs\").  That is Emacs
housekeeping: the stack holds a comp frame, and COMMAND, when given,
is this very Emacs in batch mode."
  (let ((comp-frame nil))
    (mapbacktrace (lambda (_evald func _args _flags)
                    (when (memq func efrit-sandbox-eval--native-comp-frames)
                      (setq comp-frame func))))
    (cond
     ;; The compiler's command line is unmistakable on its own: this
     ;; Emacs, -no-comp-spawn -Q --batch, loading an emacs-async-comp-*
     ;; (comp-run.el, a timer) or emacs-int-comp-* (comp.el `comp--final',
     ;; synchronous: trampolines, native-compile of a defun) file it
     ;; just wrote.  The frame test alone missed both (tzz, 2026-10-15:
     ;; "why does efrit want to run emacs to pwd?").
     ((and (listp command) (stringp (car command))
           (member "-no-comp-spawn" command)
           (member "--batch" command)
           (cl-some (lambda (a) (and (stringp a) (string-match-p "/emacs-\\(?:async\\|int\\)-comp-.*\\.el\\'" a))) command)
           (efrit-sandbox-eval--this-emacs-p (car command)))
      'comp-run-async-workers)
     ((and comp-frame
           (or (null command)
               (and (listp command) (stringp (car command))
                    (member "--batch" command)
                    (efrit-sandbox-eval--this-emacs-p (car command)))))
      comp-frame))))

(defun efrit-sandbox-eval--this-emacs-p (program)
  "Non-nil when PROGRAM is the running Emacs binary."
  (or (string-suffix-p "/Emacs" program)
      (string-match-p "/emacs\\(?:-[0-9.]+\\)?\\'" program)
      (equal (file-truename program)
             (file-truename (expand-file-name invocation-name invocation-directory)))))

(defun efrit-sandbox-eval--op-cap (op)
  "Capability OP needs, or nil for pure name manipulation
\(expand-file-name, file-name-directory, abbreviate-file-name, ...)."
  (cond ((memq op efrit-sandbox-eval--write-ops) 'write)
        ((memq op efrit-sandbox-eval--exec-ops) 'shell)
        ((memq op efrit-sandbox-eval--read-ops) 'read)
        (t nil)))

(defun efrit-sandbox-eval--op-paths (op args)
  "The file name arguments of OP in ARGS that need checking.
Only absolute names: a relative one is resolved by the operation
against `default-directory', and when that is a remote root (efrit
started from a TRAMP buffer) the check would touch the remote for a
local file.  The operation's own access to the resolved absolute
file comes back through this handler and is checked then."
  (cl-remove-if-not
   (lambda (p) (and (stringp p) (not (string-empty-p p)) (file-name-absolute-p p)))
   (pcase op
     ((or 'rename-file 'copy-file 'add-name-to-file 'make-symbolic-link 'copy-directory)
      (list (nth 0 args) (nth 1 args)))
     ((or 'write-region) (list (nth 2 args)))
     ((or 'process-file 'start-file-process 'shell-command) nil) ; capability only
     (_ (and (stringp (car args)) (list (car args)))))))

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
         ;; the native compiler writing its trampoline / eln: housekeeping
         ((efrit-sandbox-eval--native-comp-p) nil)
         (t
          (dolist (p (efrit-sandbox-eval--op-paths op args))
            (unless (and (eq cap 'read) (efrit-sandbox-eval--exempt-p p))
              (efrit-sandbox-check cap p "eval_sexp" (format "%s" op))))))))
    ;; A remote operation was checked above as the remote name.  What
    ;; TRAMP then does to carry it out (a local temp copy, a
    ;; write-region into `temporary-file-directory') is the transport,
    ;; not the model touching the user's files: run it with the guard
    ;; off, or an allowed remote read asked for a write grant on /tmp
    ;; (2026-09-28).
    (if (and efrit-sandbox-eval--active
             (cl-some (lambda (p) (and (stringp p) (file-remote-p p)))
                      (efrit-sandbox-eval--op-paths op args)))
        (let ((efrit-sandbox-eval--in-guard t))
          (apply op args))
      (apply op args))))

(defconst efrit-sandbox-eval--handler-entry
  (cons "\\`.*\\'" #'efrit-sandbox-eval--handler))

;; A handler that matches every name is consulted for every
;; operation.  For the ones it does not gate it just calls the
;; primitive with itself inhibited, which is harmless for most, but
;; `load' is special: when a handler claims a file, the loader hands
;; the whole load to the handler and does not open the file itself.
;; Our pass-through then re-entered the loader with the handler still
;; present for the .elc and it failed with "Opening stdio stream:
;; Invalid argument" (2026-09-27, `(require 'repeat)').  The
;; `operations' property tells Emacs which operations a handler is
;; for, so `load' (and the pure name operations we never look at)
;; skip it entirely.
(put 'efrit-sandbox-eval--handler 'operations
     (append efrit-sandbox-eval--write-ops efrit-sandbox-eval--exec-ops
             efrit-sandbox-eval--read-ops
             ;; buffer-visiting ops the buffer guard relies on
             '(file-local-copy verify-visited-file-modtime set-visited-file-modtime)))

;; Process / network advice.  Installed once; only active inside a
;; sandboxed eval.
(defun efrit-sandbox-eval--process-argv (fn args)
  "The argv (PROGRAM ARG...) FN was called with, from ARGS, or nil.
`make-process' carries it in :command; `call-process' and
`process-file' as (PROGRAM INFILE DEST DISPLAY ARG...); `start-process'
and `start-file-process' as (NAME BUFFER PROGRAM ARG...);
`call-process-region' as (START END PROGRAM DELETE DEST DISPLAY ARG...).
Taking only the program lost the flags that identify Emacs's own
compiler subprocess (2026-10-15: \"again it's asking me to run Emacs\")."
  (let ((name (and (symbolp fn) fn)))
    (cond
     ((plist-get args :command) (plist-get args :command))
     ((memq name '(call-process process-file))
      (and (stringp (car args)) (cons (car args) (cl-remove-if-not #'stringp (nthcdr 4 args)))))
     ((memq name '(start-process start-file-process))
      (and (stringp (nth 2 args)) (cons (nth 2 args) (cl-remove-if-not #'stringp (nthcdr 3 args)))))
     ((eq name 'call-process-region)
      (and (stringp (nth 2 args)) (cons (nth 2 args) (cl-remove-if-not #'stringp (nthcdr 6 args)))))
     ((stringp (car args)) (list (car args)))
     (t nil))))

(defun efrit-sandbox-eval--guard-process-1 (name orig &rest args)
  "The process guard for primitive NAME (a symbol, or nil when unknown).
Advice receives ORIG as a function object, so NAME comes from the
per-primitive wrapper `efrit-sandbox-eval--install-advice' makes;
it decides how the argv is read out of ARGS."
  (when (and efrit-sandbox-eval--active (not efrit-sandbox-eval--in-guard))
    (let ((efrit-sandbox-eval--in-guard t))
      (let* ((command (efrit-sandbox-eval--process-argv name args))
             (line (cond
                    ;; the shell wrappers take the line itself; quoting
                    ;; it made "git status x" one command word and the
                    ;; prompt read "run x" (2026-10-19)
                    ((and (memq name '(shell-command shell-command-to-string async-shell-command))
                          (stringp (car args)))
                     (car args))
                    ((and (listp command) command (cl-every #'stringp command))
                     (mapconcat #'shell-quote-argument command " "))
                    ((stringp command) command)
                    (t t))))
        ;; a process with a known argv is checked like a shell line, so
        ;; a grant for "git" covers (call-process "git" ...) as well.
        ;; The detail names the primitive, so the prompt reads
        ;; "eval_sexp: call-process emacs ..." and not as if efrit
        ;; itself wanted to run something.
        (cond
         ((efrit-sandbox-eval--native-comp-p command)
          (efrit-log 'debug "sandbox: native compiler (%s) starts a batch Emacs; not the model's doing, allowed"
                     (efrit-sandbox-eval--native-comp-p command)))
         ;; jka-compr, epa, TRAMP, archive-mode reading a file for the
         ;; model: the file was checked; its helper process is Emacs's
         ((efrit-sandbox-eval--emacs-helper-p)
          (efrit-log 'debug "sandbox: %s runs %s for a file read/write; Emacs's doing, allowed"
                     (efrit-sandbox-eval--emacs-helper-p)
                     (if (stringp line) (truncate-string-to-width line 80 nil nil "…") "a process")))
         ;; The model starting another Emacs is never the right tool:
         ;; everything it needs is in this one.  Refused outright, no
         ;; prompt (tzz, 2026-10-15: "running an external emacs almost
         ;; never makes sense with efrit ... everything it needs should
         ;; be inside Emacs already").  Two ways to the prompt instead:
         ;; `efrit-sandbox-eval-allow-emacs', or the reviewer's
         ;; "emacs": "testing" vouch for this turn (turn state
         ;; :emacs-vouched, set by efrit-loop at the user's threshold).
         ((and (not efrit-sandbox-eval-allow-emacs)
               (not (efrit-sandbox--turn-get :emacs-vouched))
               (listp command) (stringp (car command))
               (efrit-sandbox-eval--this-emacs-p (car command)))
          (signal 'efrit-sandbox-denied
                  (list (format "eval_sexp must not start another Emacs (%s). Do the work in this Emacs: evaluate the form here, use the tools, or run a shell command that is not Emacs."
                                (truncate-string-to-width (if (stringp line) line "process") 120 nil nil "…")))))
         ;; a shell line Emacs does natively (git status, cp, grep…)
         ;; from Lisp is refused the way shell_exec's is, with the
         ;; facility named; `git' included here, efrit-vcs and the
         ;; vcs_* tools exist for it (2026-10-19: an eval ran four
         ;; shell-command-to-string "git …" and asked the user)
         ((and efrit-sandbox-shell-refuse-emacs-equivalents
               (stringp line)
               (let ((efrit-sandbox-shell-emacs-equivalents
                      (cons '("git" . "vcs_status, vcs_diff, vcs_log, vcs_blame, or vc-* / efrit-vcs")
                            efrit-sandbox-shell-emacs-equivalents)))
                 (and (efrit-sandbox-shell-emacs-does-it-p line)
                      (not (efrit-sandbox--mentioned-shell-p line))
                      (progn
                        (efrit-log 'info "sandbox: eval subprocess refused, Emacs does it: %s" line)
                        (signal 'efrit-sandbox-denied
                                (list (efrit-sandbox-request-create
                                       :cap 'shell :target line :tool "eval_sexp"
                                       :detail (efrit-sandbox-shell-emacs-refusal line)))))))))
         (t
          (efrit-sandbox-check 'shell line "eval_sexp"
                               (format "subprocess via %s: %s%s"
                                       (or name "make-process")
                                       (if (stringp line) line "process")
                                       (let ((why (and (listp command) (stringp (car command))
                                                       (efrit-sandbox-eval--this-emacs-p (car command))
                                                       (efrit-sandbox--turn-get :emacs-vouched))))
                                         (if (stringp why) (format "\nreviewer: an external Emacs for testing: %s" why) "")))))))))
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
(defvar efrit-sandbox-eval--denied nil
  "The first request refused during the running eval, or nil.
Set by `efrit-sandbox-eval--note-denial'; the second refusal in one
eval is not asked about, the stored one is re-signalled.")

(defvar efrit-sandbox-eval--sweep-allowed nil
  "Non-nil once the user allowed a buffer-list walk in the running eval.")

(defvar efrit-sandbox-eval--loading nil
  "Non-nil during a `require' or `load' issued inside a sandboxed eval.
What runs then is Emacs and the user's configuration (the library's
top level, `with-eval-after-load' forms, `after-load-functions', mode
hooks), which switch through the user's buffers freely.  A
`(require 'repeat)' asked for a buffer grant on the user's own init
file that way (2026-09-28).  `load-in-progress' is not enough: the
after-load forms run once it is nil again.")

(defun efrit-sandbox-eval--guard-load (orig &rest args)
  "Around `require'/`load': mark the extent so the buffer guard stands down.
The file-name handler still checks any file the loaded code reads or
writes; only the buffer switches are Emacs's own."
  (if efrit-sandbox-eval--active
      (let ((efrit-sandbox-eval--loading t))
        (apply orig args))
    (apply orig args)))

(defun efrit-sandbox-eval--caller-summary ()
  "The Lisp functions on the stack between the eval and this guard, innermost first.
Advice and the guard's own frames are left out.  The prompt shows
this so the user can tell the model's own `with-current-buffer' from
one of their hooks walking the buffer list (2026-09-30: an eval that
only turned on `repeat-mode' asked for a remote YAML buffer the model
never named)."
  (let ((names nil) (n 0))
    (mapbacktrace
     (lambda (_evald func _args _flags)
       (when (and (symbolp func) (< n 8)
                  (not (memq func '(efrit-sandbox-eval--caller-summary efrit-sandbox-eval--check-buffer
                                    efrit-sandbox-eval--guard-set-buffer efrit-sandbox-eval--guard-read-buffer
                                    mapbacktrace apply funcall set-buffer save-current-buffer
                                    eval efrit-sandbox-eval-form)))
                  (not (string-prefix-p "efrit-" (symbol-name func)))
                  (not (string-match-p "\\`\\(?:ad-\\|advice-\\|#\\)" (symbol-name func))))
         (cl-incf n)
         (push (symbol-name func) names))))
    (mapconcat #'identity (nreverse names) " < ")))

(defun efrit-sandbox-eval--switched-by-efrit-p ()
  "Non-nil when the innermost Lisp caller of this buffer switch is efrit's own code.
`efrit-agent-buffers' walks the buffer list with `with-current-buffer'
to test the major mode; a form that only called it was asked to grant
the user's most recent file buffer (2026-10-16: `hrt.md', named
nowhere in the form).  The model chose the function, not the buffer;
efrit's functions switch into buffers to look, and the file-name
handler still guards what they read.  Only efrit's frames count: a
`with-current-buffer' written in the form is still the model's."
  ;; Walk outward from the switch to the eval's own frame.  Any named
  ;; efrit function met on the way (not the sandbox's and not the
  ;; eval machinery's) made the switch.  Interpreted code shows `let',
  ;; `progn' and lambdas as frames, so the nearest named frame is not
  ;; enough; a lambda the model wrote is anonymous and counts for
  ;; nothing, which is the point.
  (let ((found nil) (done nil))
    (mapbacktrace
     (lambda (_evald func _args _flags)
       (unless done
         (when (symbolp func)
           (cond
            ((memq func '(eval efrit-sandbox-eval-form efrit-tools-eval-sexp efrit-tools--eval-with-context
                          efrit-tools--call-with-input-blocked))
             (setq done t))
            ((and (string-prefix-p "efrit-" (symbol-name func))
                  (not (string-prefix-p "efrit-sandbox-eval--" (symbol-name func))))
             (setq found t done t)))))))
    found))

(defun efrit-sandbox-eval--emacs-file-hooks-p ()
  "Non-nil when a buffer switch happens inside Emacs's own hook machinery.
`save-buffer' and `find-file-noselect' run the user's hooks
\(`after-save-hook', `find-file-hook', mode hooks), which walk the
buffer list or update other buffers; a form that saved one file was
asked for the user's unrelated `tzz.md' that way (2026-10-19).  Any
hook run is the same: `run-mode-hooks' from `(eshell-mode)' in a temp
buffer ran a global minor mode's hook that touched ten file buffers
\(2026-10-22).  `run-hooks' and kin are the chokepoint; the model's
own code never calls them."
  (let ((found nil))
    (mapbacktrace (lambda (_evald func _args _flags)
                    (when (and (not found)
                               (memq func '(basic-save-buffer save-buffer save-some-buffers
                                            find-file-noselect find-file-noselect-1 after-find-file
                                            normal-mode set-auto-mode revert-buffer
                                            run-mode-hooks run-hooks run-hook-with-args
                                            run-hook-with-args-until-success run-hook-with-args-until-failure
                                            run-hook-wrapped
                                            ;; the bell and its flash walk the echo area and
                                            ;; minibuffer buffers; (ding) in a form asked for
                                            ;; the user's file buffers (2026-10-22)
                                            ding pulse-faces pulse-momentary-highlight-region
                                            flash-echo-area-bell-function flash-face-bell-function
                                            redisplay force-mode-line-update)))
                      (setq found func))))
    found))

(defun efrit-sandbox-eval--check-buffer (buffer-or-name op)
  "Check BUFFER-OR-NAME for the `buffer' capability if it visits a file.
OP names the operation for the prompt.  Fileless or missing buffers
pass; efrit's buffer check applies the target/in-project exemptions;
a switch made inside one of efrit's own functions passes
\(`efrit-sandbox-eval--switched-by-efrit-p')."
  (let ((buffer (and buffer-or-name (get-buffer buffer-or-name))))
    (when (and buffer
               (not load-in-progress)
               (not efrit-sandbox-eval--loading)
               (buffer-local-value 'buffer-file-name buffer)
               (not (efrit-sandbox-buffer-allowed-p buffer))
               (not (efrit-sandbox-eval--switched-by-efrit-p))
               (not (efrit-sandbox-eval--emacs-file-hooks-p))
               (not efrit-sandbox-eval--sweep-allowed))
      (let* ((callers (efrit-sandbox-eval--caller-summary))
             ;; a form walking `buffer-list' meets every out-of-project
             ;; buffer in turn; asking per buffer is useless (the user
             ;; cannot say yes to the sweep).  Say how many there are
             ;; and ask once; `o'/`s' then covers them all this eval
             ;; (2026-10-19: hrt.md, then tzz.md, for a minor-mode sweep)
             (others (cl-remove-if-not
                      (lambda (b) (and (not (eq b buffer))
                                       (buffer-local-value 'buffer-file-name b)
                                       (not (efrit-sandbox-buffer-allowed-p b))))
                      (buffer-list)))
             (sweep (and others (efrit-sandbox-eval--sweep-p callers))))
        (efrit-log 'info "sandbox: %s on buffer %s from: %s" op (buffer-name buffer) callers)
        (cond
         ;; A sweep that reaches REMOTE buffers is refused, not asked:
         ;; the user's rule is local buffers first, remote ones by
         ;; name, one at a time and only when very likely useful
         ;; (2026-10-20: a search-forward over (buffer-list) entered
         ;; seven TRAMP buffers and asked).  The result names the way.
         ((and sweep
               (cl-some (lambda (b) (file-remote-p (buffer-local-value 'buffer-file-name b)))
                        (cons buffer others)))
          (let ((remote (cl-count-if (lambda (b) (file-remote-p (buffer-local-value 'buffer-file-name b)))
                                     (cons buffer others))))
            (signal 'efrit-sandbox-denied
                    (list (efrit-sandbox-request-create
                           :cap 'buffer :target (efrit-sandbox-buffer-target buffer) :tool "eval_sexp"
                           :detail (format "a walk over the buffer list that enters %d remote (TRAMP) buffer(s); refused without asking. Use buffer_list (remote buffers are flagged), consider local buffers first, then name one remote buffer with buffer_scan buffer=NAME or read_buffer and ask the user before reading it" remote))))))
         ((not sweep)
          (efrit-sandbox-check-buffer buffer "eval_sexp"
                                      (format "%s on %s, called from: %s" op (buffer-name buffer)
                                              (if (string-empty-p callers) "the form itself" callers))))
         (t
          (efrit-sandbox-check-buffer buffer "eval_sexp"
                                      (format "%s on %s and %d more file buffer(s) outside the project (a walk over the buffer list; one answer covers the walk)%s"
                                              op (buffer-name buffer) (length others)
                                              (if (string-empty-p callers) "" (format ", called from: %s" callers))))
          ;; allowed: the rest of the walk passes for this eval
          (setq efrit-sandbox-eval--sweep-allowed t)))))))

(defun efrit-sandbox-eval--sweep-p (callers)
  "Non-nil when the switch comes straight from the form: no named function between.
CALLERS is the caller summary.  Every entry a special form, a macro
or a sequence function means the model's own loop made the switch;
a named function (`efrit-agent-buffers', `ibuffer-update') would be
that function's business."
  (cl-every (lambda (name)
              (let ((sym (intern-soft name)))
                (or (null sym) (special-form-p sym) (macrop sym)
                    (memq sym '(mapc mapcar mapcan seq-do seq-filter seq-map cl-loop funcall apply)))))
            (split-string callers " < " t)))

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
    ;; one named wrapper per primitive, so the guard knows which argv
    ;; shape it is looking at (advice gets the function, not the name)
    (let ((wrapper (intern (format "efrit-sandbox-eval--guard-%s" fn))))
      (unless (fboundp wrapper)
        (fset wrapper
              (lambda (orig &rest args) (apply #'efrit-sandbox-eval--guard-process-1 fn orig args)))
        (put wrapper 'function-documentation
             (format "The eval sandbox's process guard around `%s'." fn)))
      (unless (advice-member-p wrapper fn)
        (advice-add fn :around wrapper))))
  (dolist (fn '(make-network-process open-network-stream url-retrieve
                url-retrieve-synchronously))
    (unless (advice-member-p #'efrit-sandbox-eval--guard-network fn)
      (advice-add fn :around #'efrit-sandbox-eval--guard-network)))
  (unless (advice-member-p #'efrit-sandbox-eval--guard-set-buffer 'set-buffer)
    (advice-add 'set-buffer :around #'efrit-sandbox-eval--guard-set-buffer))
  (dolist (fn '(require load))
    (unless (advice-member-p #'efrit-sandbox-eval--guard-load fn)
      (advice-add fn :around #'efrit-sandbox-eval--guard-load)))
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

(defvar efrit-sandbox-eval--nesting 0
  "How many `efrit-sandbox-eval-form' calls are active; the handler entry
stays on `file-name-handler-alist' until the outermost returns.")

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
  ;; the user may have rewritten the form at the prompt
  (when-let* ((edited (efrit-sandbox-take-edited-input "eval_sexp")))
    (efrit-log 'info "eval_sexp: evaluating the user's edited form instead of the model's")
    (setq form (car (read-from-string edited))))
  (when-let* ((why (efrit-sandbox-eval-inspect form)))
    (efrit-log 'warn "sandbox: refused eval form: %s" why)
    (signal 'efrit-sandbox-denied
            (list (efrit-sandbox-request-create :cap 'elisp :target t :tool "eval_sexp"
                                                :detail why))))
  (efrit-sandbox-eval--install-advice)
  ;; efrit's handler entry goes onto the GLOBAL alist for the eval's
  ;; duration, not into a `let' binding: a library that registers
  ;; handlers while the form runs (tramp, autoloaded by the first
  ;; remote path it meets) would register into the binding and lose
  ;; them when it ends; tramp then stayed loaded with no handler and
  ;; every remote path read as local (2026-10-20, found by a test that
  ;; passed alone and failed after a sweep test).
  (cl-incf efrit-sandbox-eval--nesting)
  (unwind-protect
      (let ((efrit-sandbox-eval--active t)
            (efrit-sandbox-eval--denied nil)
            (efrit-sandbox-eval--sweep-allowed nil))
        (unless (memq efrit-sandbox-eval--handler-entry file-name-handler-alist)
          (push efrit-sandbox-eval--handler-entry file-name-handler-alist))
        (prog1 (if evaluator (funcall evaluator form) (eval form t))
          ;; The form caught the denial (`ignore-errors', a `condition-case'
          ;; on `error', a hook runner) and went on.  The user's no is
          ;; final: the eval fails with it, whatever the form returned
          ;; (2026-10-19: a save hook's `git ls-files' was refused, the
          ;; form carried on and asked for a second buffer, then hung).
          (when efrit-sandbox-eval--denied
            (signal 'efrit-sandbox-denied (list efrit-sandbox-eval--denied)))))
    ;; nested evals (a tool run from a form) share the one entry; the
    ;; outermost removes it
    (cl-decf efrit-sandbox-eval--nesting)
    (when (<= efrit-sandbox-eval--nesting 0)
      (setq efrit-sandbox-eval--nesting 0)
      (setq file-name-handler-alist (delq efrit-sandbox-eval--handler-entry file-name-handler-alist)))))


(defun efrit-sandbox-eval--note-denial (orig &rest args)
  "Around `efrit-sandbox-check' and `efrit-sandbox-check-buffer' inside an eval.
Remembers a denial so `efrit-sandbox-eval-form' can re-raise it, and
refuses further requests of the same eval without a prompt once one
was denied: the user already said no to this form."
  (if (not efrit-sandbox-eval--active)
      (apply orig args)
    (when efrit-sandbox-eval--denied
      (efrit-log 'info "sandbox: a request of this eval was already refused; refusing %s without asking" (car args))
      (signal 'efrit-sandbox-denied (list efrit-sandbox-eval--denied)))
    (condition-case err
        (apply orig args)
      (efrit-sandbox-denied
       (setq efrit-sandbox-eval--denied (cadr err))
       (signal (car err) (cdr err))))))

(dolist (fn '(efrit-sandbox-check efrit-sandbox-check-buffer))
  (unless (advice-member-p #'efrit-sandbox-eval--note-denial fn)
    (advice-add fn :around #'efrit-sandbox-eval--note-denial)))

(provide 'efrit-sandbox-eval)

;;; efrit-sandbox-eval.el ends here
