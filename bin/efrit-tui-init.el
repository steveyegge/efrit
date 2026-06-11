;;; efrit-tui-init.el --- bootstrap for the efrit-tui test Emacs -*- lexical-binding: t -*-

;; Loaded via `emacs -nw -Q -l efrit-tui-init.el' by bin/efrit-tui.
;; Sets up the repo load-paths and a named server socket so the same
;; Emacs is reachable both through tmux (keystrokes, screen) and
;; through `bin/efrit -s efrit-tui' (ground-truth eval).

;; Never let a stale .elc shadow edited source
(setq load-prefer-newer t)

(let* ((here (file-name-directory (or load-file-name buffer-file-name)))
       (lisp (expand-file-name "../lisp" here)))
  (dolist (sub '("" "core" "interfaces" "tools" "support" "dev"))
    (let ((dir (expand-file-name sub lisp)))
      (when (file-directory-p dir)
        (add-to-list 'load-path dir)))))

;; Load efrit so its interactive commands are reachable via M-x,
;; like a user init that requires the package
(require 'efrit-do)
(require 'efrit-agent)

;; Deterministic screen for capture-pane assertions
(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(setq ring-bell-function #'ignore)

;; The named socket bin/efrit attaches to.  EFRIT_TUI_SOCKET lets the
;; driver run several harnesses side by side.
(require 'server)
(setq server-name (or (getenv "EFRIT_TUI_SOCKET") "efrit-tui"))
(server-start)

;;; efrit-tui-init.el ends here
