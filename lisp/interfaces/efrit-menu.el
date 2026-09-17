;;; efrit-menu.el --- Transient menu and status dashboard -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1") (transient "0.4"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; `M-x efrit-menu' (C-c C-m in the agent buffer): one place to see
;; the current model, endpoint, permission policy, streaming state
;; and token usage, and to reach every command.  Descriptions are
;; functions, so the menu doubles as a status display (copilot-menu's
;; approach); toggles stay open so several can be flipped in a row.
;;
;; `transient' has been bundled since Emacs 28.1.  The prefix is kept
;; as data and evaluated at load so a build without transient (or a
;; broken one) degrades to a `user-error' rather than a load failure.

;;; Code:

(require 'cl-lib)

(defvar efrit-default-model)
(defvar efrit-api-base-url)
(defvar efrit-api-auth-scheme)
(defvar efrit-api-streaming)
(defvar efrit-api-prompt-caching)
(defvar efrit-permission-policy)
(defvar efrit-agent-header-style)
(defvar efrit-context-sources)
(defvar efrit-agent--repl-session)
(defvar efrit-agent--status)
(declare-function efrit-usage-indicator "efrit-usage")
(declare-function efrit-repl-session-id "efrit-repl-session")
(declare-function efrit-doctor--redact-url "efrit-doctor")
(declare-function efrit-permission-reset "efrit-permissions")

;;; Description helpers (all pure, all safe with modules unloaded)

(defun efrit-menu--desc-model ()
  (format "Select model (%s)" (if (boundp 'efrit-default-model) efrit-default-model "?")))

(defun efrit-menu--desc-endpoint ()
  (format "Endpoint: %s [%s]"
          (if (and (boundp 'efrit-api-base-url) (stringp efrit-api-base-url))
              (condition-case nil
                  (let ((u (url-generic-parse-url efrit-api-base-url)))
                    (or (url-host u) efrit-api-base-url))
                (error efrit-api-base-url))
            "function")
          (if (boundp 'efrit-api-auth-scheme) efrit-api-auth-scheme "?")))

(defun efrit-menu--desc-toggle (label var)
  (format "%s [%s]" label (if (and (boundp var) (symbol-value var)) "on" "off")))

(defun efrit-menu--desc-permissions ()
  (format "Permission policy [%s]"
          (if (boundp 'efrit-permission-policy)
              (if efrit-permission-policy
                  (mapconcat #'symbol-name efrit-permission-policy "+")
                "never ask")
            "?")))

(defun efrit-menu--desc-header ()
  (format "Header style [%s]" (if (boundp 'efrit-agent-header-style) efrit-agent-header-style "?")))

(defun efrit-menu--desc-usage ()
  (let* ((id (and (boundp 'efrit-agent--repl-session) efrit-agent--repl-session
                  (fboundp 'efrit-repl-session-id)
                  (efrit-repl-session-id efrit-agent--repl-session)))
         (u (and id (fboundp 'efrit-usage-indicator) (efrit-usage-indicator id))))
    (format "Session: %s%s"
            (if (boundp 'efrit-agent--status) efrit-agent--status "none")
            (if u (format "  context %s" (substring-no-properties u)) ""))))

;;; Toggle commands

(defun efrit-menu-toggle-streaming ()
  "Toggle `efrit-api-streaming'."
  (interactive)
  (require 'efrit-api-stream)
  (setq efrit-api-streaming (not efrit-api-streaming))
  (message "efrit streaming %s" (if efrit-api-streaming "on" "off")))

(defun efrit-menu-toggle-caching ()
  "Toggle `efrit-api-prompt-caching'."
  (interactive)
  (require 'efrit-api)
  (setq efrit-api-prompt-caching (not efrit-api-prompt-caching))
  (message "efrit prompt caching %s" (if efrit-api-prompt-caching "on" "off")))

(defun efrit-menu-cycle-permissions ()
  "Cycle `efrit-permission-policy': (write exec) -> (exec) -> nil -> ..."
  (interactive)
  (require 'efrit-permissions)
  (setq efrit-permission-policy
        (pcase efrit-permission-policy
          ('(write exec) '(exec))
          ('(exec) nil)
          (_ '(write exec))))
  (message "efrit permission policy: %S" efrit-permission-policy))

(defun efrit-menu-show-log ()
  "Show efrit's log buffer."
  (interactive)
  (require 'efrit-log)
  (if (fboundp 'efrit-log-show) (efrit-log-show)
    (pop-to-buffer (get-buffer-create "*efrit-log*"))))

;;; The menu

(defconst efrit-menu--definition
  '(transient-define-prefix efrit-menu ()
     "Efrit: status and commands."
     [:description efrit-menu--desc-usage
      ["Session"
       ("e" "Open agent (REPL)" efrit)
       ("d" "One-shot command" efrit-do)
       ("n" "New conversation" efrit-agent-new-session)
       ("r" "Resume a saved session" efrit-resume)
       ("k" "Cancel current turn" efrit-agent-cancel)]
      ["Configuration"
       ("m" efrit-select-model :description efrit-menu--desc-model)
       ("M" "Select model (probe all)" (lambda () (interactive) (efrit-select-model t)))
       ("s" efrit-menu-toggle-streaming :transient t
        :description (lambda () (efrit-menu--desc-toggle "Streaming" 'efrit-api-streaming)))
       ("c" efrit-menu-toggle-caching :transient t
        :description (lambda () (efrit-menu--desc-toggle "Prompt caching" 'efrit-api-prompt-caching)))
       ("p" efrit-menu-cycle-permissions :transient t :description efrit-menu--desc-permissions)
       ("P" "Forget session permission grants" efrit-permission-reset :transient t)
       ("h" efrit-agent-cycle-header-style :transient t :description efrit-menu--desc-header)]]
     [["Diagnostics"
       ("D" "Doctor (static)" efrit-doctor)
       ("L" "Doctor with live API check" (lambda () (interactive) (efrit-doctor t)))
       ("l" "Show log" efrit-menu-show-log)
       ("u" "Usage / endpoint" (lambda () (interactive) (message "%s" (efrit-menu--desc-endpoint))))]
      ["View"
       ("TAB" "Toggle tool call at point" efrit-agent-toggle-expand)
       ("E" "Expand all tool calls" efrit-agent-expand-all)
       ("C" "Collapse all tool calls" efrit-agent-collapse-all)
       ("v" "Cycle verbosity" efrit-agent-cycle-verbosity :transient t)
       ("o" "Cycle display mode" efrit-agent-cycle-display-mode :transient t)]])
  "The `efrit-menu' prefix, kept as data (see Commentary).")

(if (require 'transient nil t)
    (eval efrit-menu--definition t)
  (defun efrit-menu ()
    "Efrit menu (requires the `transient' package)."
    (interactive)
    (if (require 'transient nil t)
        (progn (eval efrit-menu--definition t)
               (call-interactively 'efrit-menu))
      (user-error "efrit-menu needs the `transient' package"))))

(provide 'efrit-menu)

;;; efrit-menu.el ends here
