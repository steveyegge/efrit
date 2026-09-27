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
(defvar efrit-sandbox-enabled)
(defvar efrit-agent-header-style)
(defvar efrit-context-sources)
(defvar efrit-agent--repl-session)
(defvar efrit-agent--status)
(declare-function efrit-usage-indicator "efrit-usage")
(declare-function efrit-repl-session-id "efrit-repl-session")
(declare-function efrit-doctor--redact-url "efrit-doctor")
(declare-function efrit-permissions "efrit-permissions-ui")
(declare-function efrit-review-package "efrit-package-review")
(declare-function efrit-review-all-packages "efrit-package-review-ui")
(declare-function efrit-testdrive "efrit-testdrive")
(declare-function efrit-testdrive-tour "efrit-testdrive")
(declare-function efrit-log-toggle-debug "efrit-log")
(defvar efrit-log-level)
(declare-function efrit-sandbox-reset-session "efrit-sandbox")
(declare-function efrit-review-probe-refusal "efrit-review")

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
  (format "Sandbox [%s]"
          (if (bound-and-true-p efrit-sandbox-enabled) "on" "off")))

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

(defun efrit-menu-toggle-sandbox ()
  "Toggle `efrit-sandbox-enabled'."
  (interactive)
  (require 'efrit-sandbox)
  (setq efrit-sandbox-enabled (not efrit-sandbox-enabled))
  (message "efrit sandbox %s" (if efrit-sandbox-enabled "on" "OFF -- every tool runs unchecked")))

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
       ("k" "Cancel current turn" efrit-agent-cancel)
       ("Q" "Show / drop queued inputs" efrit-agent-queue-show)
       ("y" "Copy last answer" efrit-agent-copy-last-output)
       ("i" "Copy session id" efrit-agent-copy-session-id)
       ("N" "Restart in place (fresh session, same windows)" efrit-agent-restart)]
      ["Configuration"
       ("m" efrit-select-model :description efrit-menu--desc-model)
       ("M" "Select model (probe all)" (lambda () (interactive) (efrit-select-model t)))
       ("s" efrit-menu-toggle-streaming :transient t
        :description (lambda () (efrit-menu--desc-toggle "Streaming" 'efrit-api-streaming)))
       ("c" efrit-menu-toggle-caching :transient t
        :description (lambda () (efrit-menu--desc-toggle "Prompt caching" 'efrit-api-prompt-caching)))
       ("b" efrit-menu-toggle-sandbox :transient t :description efrit-menu--desc-permissions)
       ("x" "Permissions editor (grants, review, limits)" efrit-permissions)
       ("p" "Prompt library (per-item / over-everything prompts)" efrit-prompts-manage)
       ("g" "Document source check: Google Drive" efrit-documents-gdrive-check)
       ("w" "Document source check: Confluence" efrit-documents-confluence-check)
       ("K" "Document source check: Google Calendar" efrit-documents-gcalendar-check)
       ("j" "Document source check: Jira" efrit-documents-jira-check)
       ("P" "Review an installed package" efrit-review-package)
       ("V" "Review all installed packages" efrit-review-all-packages)
       ("X" "Forget session sandbox grants" efrit-sandbox-reset-session :transient t)
       ("h" efrit-agent-cycle-header-style :transient t :description efrit-menu--desc-header)]]
     [["Diagnostics"
       ("D" "Doctor (static)" efrit-doctor)
       ("L" "Doctor with live API check" (lambda () (interactive) (efrit-doctor t)))
       ("l" "Show log" efrit-menu-show-log)
       ("G" efrit-log-toggle-debug :transient t
        :description (lambda () (format "Debug logging [%s]" (if (eq efrit-log-level 'debug) "on" "off"))))
       ("R" "Reload efrit from source" efrit-reload)
       ("F" "Bisect the last refused review request" efrit-review-probe-refusal)
       ("T" "Test drive: automatic (live, costs tokens)" efrit-testdrive)
       ("t" "Test drive: the tour (what needs eyes)" efrit-testdrive-tour)
       ("u" "Usage / endpoint" (lambda () (interactive) (message "%s" (efrit-menu--desc-endpoint))))]
      ["View"
       ("TAB" "Toggle tool call at point" efrit-agent-toggle-expand)
       ("E" "Expand all tool calls" efrit-agent-expand-all)
       ("C" "Collapse all tool calls" efrit-agent-collapse-all)
       ("v" "Cycle verbosity" efrit-agent-cycle-verbosity :transient t)
       ("o" "Cycle display mode" efrit-agent-cycle-display-mode :transient t)
       ("q" "Close this menu" transient-quit-one)]])
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
