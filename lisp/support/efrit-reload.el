;;; efrit-reload.el --- Reload every loaded efrit library from source -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; `M-x efrit-reload' re-`load's every efrit feature that is currently
;; loaded, in the order they were first loaded (dependencies before
;; dependents), so a checkout edited in place takes effect without
;; restarting Emacs.
;;
;; What it keeps: your customizations.  `defvar'/`defcustom' do not
;; reassign a bound variable, so settings and buffer-local state
;; survive; `defconst' and `defface' do get their new values.
;; Timers, hooks and advice added with `add-hook'/`advice-add' are
;; idempotent and stay in place.
;;
;; Keymaps are the exception that bit: `(defvar foo-map (let ((map
;; ...)) ...))' is skipped on reload, so a new key binding in the
;; source never reaches the live map.  `efrit-reload' therefore
;; unbinds every `efrit-*-map' variable first, so the defvar runs
;; again and the mode picks up the fresh map.  Minor-mode maps
;; registered in `minor-mode-map-alist' by symbol follow along;
;; `define-derived-mode' maps are re-looked-up per buffer.
;;
;; What it cannot do: a struct whose slots changed keeps existing
;; instances in the old shape until they are recreated (an agent
;; buffer's session, for one), and an `eval-after-load' body runs
;; again.  When something looks stale after a reload, kill the agent
;; buffer and start it fresh; when things look wrong, restart Emacs.
;;
;; The source file is preferred over a stale .elc: `load' with the
;; extension elided picks the newer of the two, as `load-prefer-newer'
;; would, so an edited .el wins over the .elc compiled before it.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defconst efrit-reload--feature-prefix "efrit"
  "Features whose names start with this are reloaded.")

(defconst efrit-reload--never
  '(efrit-reload)
  "Features not reloaded by the main pass.
This file reloads *itself* first, separately (see `efrit-reload'), so
a fix to the reloader takes effect in the same invocation.")

(defvar efrit-reload--self-reloaded nil
  "Non-nil while `efrit-reload' runs its freshly loaded self.")

(defun efrit-reload-features ()
  "The loaded efrit features, oldest first (the order to reload them in).
`features' lists the newest first."
  (reverse
   (seq-filter (lambda (f)
                 (and (string-prefix-p efrit-reload--feature-prefix (symbol-name f))
                      (not (memq f efrit-reload--never))))
               features)))

(defun efrit-reload--library-file (feature)
  "The file FEATURE was loaded from, sans extension, or nil."
  (let ((file (or (locate-library (symbol-name feature))
                  (symbol-file feature 'provide))))
    (and file (file-name-sans-extension file))))

(defun efrit-reload-keymap-variables ()
  "The bound `efrit-...-map' variables that hold keymaps."
  (let ((out nil))
    (mapatoms (lambda (sym)
                (when (and (boundp sym)
                           (string-prefix-p efrit-reload--feature-prefix (symbol-name sym))
                           (string-suffix-p "-map" (symbol-name sym))
                           (keymapp (symbol-value sym)))
                  (push sym out))))
    out))

(defun efrit-reload-transient-prefixes ()
  "The efrit transient prefix commands defined so far.
They are defined lazily behind an `fboundp' guard (the sandbox prompt,
the limits menu, the editor's row menus), so a plain reload keeps the
old menu with its old keys."
  (let ((out nil))
    (when (featurep 'transient)
      (mapatoms (lambda (sym)
                  (when (and (fboundp sym)
                             (string-prefix-p efrit-reload--feature-prefix (symbol-name sym))
                             (get sym 'transient--prefix))
                    (push sym out)))))
    out))

(defun efrit-reload--unbind-transient-prefixes ()
  "Make every efrit transient prefix void so its lazy definition runs again."
  (let ((syms (efrit-reload-transient-prefixes)))
    (dolist (sym syms)
      (fmakunbound sym)
      (put sym 'transient--prefix nil)
      (put sym 'transient--layout nil))
    syms))

(defun efrit-reload-global-minor-modes ()
  "The efrit global minor modes, as (SYMBOL . ON-P) before a reload.
A `define-minor-mode' with :global t re-runs its variable's
`defcustom' on load; its :set function turns the mode off when the
standard value is nil, dropping the advice the mode installed.  The
reloader turns the ones that were on back on."
  (let ((out nil))
    (mapatoms (lambda (sym)
                ;; a global minor mode's variable is a boolean defcustom
                ;; that is not buffer-local; a buffer-local mode
                ;; (efrit-agent-input-mode) is per buffer and untouched
                (when (and (fboundp sym) (boundp sym)
                           (string-prefix-p efrit-reload--feature-prefix (symbol-name sym))
                           (string-suffix-p "-mode" (symbol-name sym))
                           (eq (get sym 'custom-type) 'boolean)
                           (get sym 'standard-value)
                           (not (local-variable-if-set-p sym)))
                  (push (cons sym (symbol-value sym)) out))))
    out))

(defun efrit-reload--restore-global-minor-modes (before)
  "Turn back on the global minor modes in BEFORE that were on.  Returns them."
  (let ((restored nil))
    (dolist (m before)
      (when (and (cdr m) (fboundp (car m)))
        (condition-case err
            (progn (funcall (car m) 1) (push (car m) restored))
          (error (efrit-log 'warn "reload: could not re-enable %s: %s"
                            (car m) (error-message-string err))))))
    restored))

(defun efrit-reload--unbind-keymaps ()
  "Make every efrit keymap variable void so its defvar runs on reload.
Returns the symbols, for `efrit-reload--rebind-keymaps'."
  (let ((syms (efrit-reload-keymap-variables)))
    (dolist (sym syms) (makunbound sym))
    syms))

(defun efrit-reload-option-defaults ()
  "Alist of (SYMBOL . DEFAULT-VALUE) for every efrit user option.
The default is the evaluated `standard-value'; an option whose
default form signals is left out."
  (let ((out nil))
    (mapatoms (lambda (sym)
                (when-let* (((string-prefix-p efrit-reload--feature-prefix (symbol-name sym)))
                            (form (car (get sym 'standard-value))))
                  (condition-case nil
                      (push (cons sym (eval form t)) out)
                    (error nil)))))
    out))

(defun efrit-reload--refresh-changed-defaults (before)
  "Adopt a changed default for options the user never set.
BEFORE is the `efrit-reload-option-defaults' snapshot from before the
reload.  A `defcustom' whose default changed in the source is reset
when its live value still equals the old default: the user did not
`setq' it, and `custom-reevaluate-setting' prefers a saved
customization anyway.  Returns the symbols that were reset."
  (let ((reset nil))
    (dolist (entry (efrit-reload-option-defaults))
      (let* ((sym (car entry))
             (old (assq sym before)))
        (when (and old
                   (not (equal (cdr old) (cdr entry)))
                   (boundp sym)
                   (equal (symbol-value sym) (cdr old)))
          (custom-reevaluate-setting sym)
          (push sym reset))))
    reset))

(defun efrit-reload--rebind-keymaps (syms)
  "After reloading, point live users of the old maps at the new ones.
`minor-mode-map-alist' stores map objects, so a minor mode defined
with :keymap FOO-map keeps the stale object; replace it.  Buffers in
an efrit major mode get the new mode map as their local map."
  (dolist (sym syms)
    (when (and (boundp sym) (keymapp (symbol-value sym)))
      (let* ((name (symbol-name sym))
             (mode (intern-soft (string-remove-suffix "-map" name)))
             (map (symbol-value sym)))
        ;; minor mode registered by this map's variable
        (when-let* ((cell (and mode (assq mode minor-mode-map-alist))))
          (setcdr cell map))
        ;; buffers in this major mode
        (when (and mode (get mode 'derived-mode-parent))
          (dolist (buf (buffer-list))
            (with-current-buffer buf
              (when (eq major-mode mode)
                (use-local-map map)))))))))

;;;###autoload
(defun efrit-reload (&optional verbose)
  "Reload every loaded efrit library from its source, dependencies first.
With VERBOSE (a prefix argument) list each file as it loads.  Files
that fail to load are reported at the end; the rest still load."
  (interactive "P")
  ;; Reload this file first and run the new definition, so a fix to
  ;; the reloader (the keymap refresh was one) applies right away
  ;; instead of one restart later.  The re-entered call sees the flag
  ;; and does the real work.
  (if (and (not efrit-reload--self-reloaded)
           (efrit-reload--library-file 'efrit-reload))
      (let ((efrit-reload--self-reloaded t)
            (load-prefer-newer t))
        (load (efrit-reload--library-file 'efrit-reload) nil t)
        (funcall 'efrit-reload verbose))
    (efrit-reload--run verbose)))

(defcustom efrit-reload-revert-buffers t
  "When non-nil, `efrit-reload' also reverts buffers visiting the reloaded files.
Only unmodified buffers whose file changed on disk are reverted; a
buffer with unsaved edits is left alone and named in the summary."
  :type 'boolean
  :group 'efrit)

(defun efrit-reload--revert-visiting-buffers ()
  "Revert unmodified buffers visiting an efrit source file that changed on disk.
Returns how many were reverted.  Point and window starts are kept
by `revert-buffer' itself (`preserve-modes' is t, so the mode is not
re-run either)."
  (if (not efrit-reload-revert-buffers)
      0
    ;; the .el sources of every reloaded feature, by true name, so a
    ;; symlinked checkout or a .elc-first locate-library still matches
    (let ((sources (delete-dups
                    (delq nil (mapcar (lambda (f)
                                        (when-let* ((base (efrit-reload--library-file f))
                                                    (el (concat base ".el"))
                                                    ((file-exists-p el)))
                                          (file-truename el)))
                                      (efrit-reload-features)))))
          (count 0) (skipped nil))
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (when (and buffer-file-name
                     (member (file-truename buffer-file-name) sources)
                     (not (verify-visited-file-modtime buf)))
            (if (buffer-modified-p)
                (push (buffer-name) skipped)
              (revert-buffer t t t)
              (cl-incf count)))))
      (when skipped
        (message "efrit-reload: not reverting modified buffer%s %s"
                 (if (cdr skipped) "s" "") (mapconcat #'identity skipped ", ")))
      count)))

(defun efrit-reload--run (verbose)
  "The body of `efrit-reload', run from its freshly loaded definition."
  (let ((load-prefer-newer t)
        (loaded 0)
        (failed nil)
        (gone nil)
        (start (float-time)))
    ;; Let keymap defvars re-run (see Commentary)
    (let ((maps (efrit-reload--unbind-keymaps))
          (defaults (efrit-reload-option-defaults))
          (modes (efrit-reload-global-minor-modes))
          (modes-on nil)
          (reset nil))
      (efrit-reload--unbind-transient-prefixes)
      (unwind-protect
          (dolist (feature (efrit-reload-features))
            (let ((file (efrit-reload--library-file feature)))
              (cond
               ;; a feature whose file is gone (deleted from the tree
               ;; since it was loaded) is not a failure: drop it from
               ;; `features' so the next reload does not see it either
               ((or (null file)
                    (not (or (file-exists-p (concat file ".el"))
                             (file-exists-p (concat file ".elc")))))
                (setq features (delq feature features))
                (push feature gone))
               (t
                (condition-case err
                    (progn
                      (load file nil (not verbose))
                      (cl-incf loaded))
                  (error
                   (push (cons feature (error-message-string err)) failed)))))))
        (efrit-reload--rebind-keymaps maps)
        (setq reset (efrit-reload--refresh-changed-defaults defaults))
        (setq modes-on (efrit-reload--restore-global-minor-modes modes)))
      (efrit-reload--report loaded failed reset modes-on start gone))))

(defun efrit-reload--report (loaded failed reset modes-on start &optional gone)
  "Revert visiting buffers, then message and log the reload summary.
LOADED is the library count, FAILED an alist of (FEATURE . ERROR),
RESET the options whose changed default was adopted, MODES-ON the
global minor modes turned back on, START the `float-time' the reload
began, GONE the features whose file no longer exists.  Returns LOADED."
  (let* ((reverted (efrit-reload--revert-visiting-buffers))
         (summary (format "efrit: reloaded %d librar%s in %.1fs%s%s%s%s%s"
                          loaded (if (= loaded 1) "y" "ies")
                          (- (float-time) start)
                          (if (> reverted 0) (format ", reverted %d buffer%s" reverted (if (= reverted 1) "" "s")) "")
                          (if reset (format ", new default for %s" (mapconcat #'symbol-name reset ", ")) "")
                          (if modes-on (format ", re-enabled %s" (mapconcat #'symbol-name modes-on ", ")) "")
                          (if gone (format ", dropped %s (file removed)" (mapconcat #'symbol-name gone ", ")) "")
                          (if failed
                              (format "; %d failed: %s" (length failed)
                                      (mapconcat (lambda (f) (format "%s (%s)" (car f) (cdr f)))
                                                 (nreverse failed) ", "))
                            ""))))
    (when (fboundp 'efrit-log) (funcall 'efrit-log 'info "%s" summary))
    (message "%s" summary)
    loaded))

(provide 'efrit-reload)

;;; efrit-reload.el ends here
