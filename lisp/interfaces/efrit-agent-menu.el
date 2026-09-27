;;; efrit-agent-menu.el --- The agent buffer's command menu -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; One transient with everything you can do in the agent buffer,
;; grouped by what it acts on: the turn, the transcript's tool rows,
;; the input, the buffer, the session.  `?' in the conversation and
;; `C-c ?' anywhere open it; the header says so.
;;
;; Each entry shows the command's own key in the buffer, looked up
;; when the menu opens, so a user who rebinds sees their keys.  The
;; menu is the discoverable path; the keys are the fast one.  The
;; prefix is kept as data and evaluated at load, so a reload redefines
;; it (a prefix behind `fboundp' kept stale keys, 2026-09-19).
;;
;; This is the buffer's menu.  `efrit-menu' (C-c C-m) is efrit's
;; global one: model, sandbox, diagnostics.

;;; Code:

(require 'cl-lib)

(defvar efrit-agent-mode-map)
(defvar efrit-agent-input-mode-map)
(defvar efrit-agent--status)
(defvar efrit-agent--repl-session)
(declare-function efrit-repl-session-queue "efrit-repl-session")
(declare-function efrit-repl-session-status "efrit-repl-session")

(defun efrit-agent-menu--key (command)
  "The key COMMAND is on in the agent buffer, as a short string, or \"\"."
  (let* ((keys (or (where-is-internal command (list efrit-agent-mode-map) t)
                   (where-is-internal command (list efrit-agent-input-mode-map) t))))
    (if keys (key-description keys) "")))

(defun efrit-agent-menu--desc (label command)
  "LABEL followed by COMMAND's key in the buffer, dimmed."
  (let ((key (efrit-agent-menu--key command)))
    (if (string-empty-p key)
        label
      (concat label "  " (propertize key 'face 'transient-key)))))

(defun efrit-agent-menu--heading ()
  "The menu's heading: status and queue."
  (let* ((session (bound-and-true-p efrit-agent--repl-session))
         (queue (and session (fboundp 'efrit-repl-session-queue)
                     (efrit-repl-session-queue session)))
         (status (and session (fboundp 'efrit-repl-session-status)
                      (efrit-repl-session-status session))))
    (format "efrit agent: %s%s"
            (or status "no session")
            (if queue (format ", %d queued" (length queue)) ""))))

(defconst efrit-agent-menu--definition
  '(transient-define-prefix efrit-agent-menu ()
     "The agent buffer's commands."
     [:description efrit-agent-menu--heading
      ["Turn"
       ("k" efrit-agent-cancel :description (lambda () (efrit-agent-menu--desc "cancel the running turn" 'efrit-agent-cancel)))
       ("p" efrit-agent-pause :description (lambda () (efrit-agent-menu--desc "pause at the next step" 'efrit-agent-pause)))
       ("u" efrit-agent-queue-show :description (lambda () (efrit-agent-menu--desc "queued inputs: show / drop" 'efrit-agent-queue-show)))
       ("U" efrit-agent-queue-resume :description (lambda () (efrit-agent-menu--desc "send the next queued input" 'efrit-agent-queue-resume)))
       ("y" efrit-agent-copy-last-output :description (lambda () (efrit-agent-menu--desc "copy the last answer" 'efrit-agent-copy-last-output)))
       ("Q" efrit-agent-quote-region :description (lambda () (efrit-agent-menu--desc "quote the region into the input" 'efrit-agent-quote-region)))]
      ["Tool rows"
       ("RET" efrit-agent-toggle-expand :description (lambda () (efrit-agent-menu--desc "fold / unfold the row at point" 'efrit-agent-toggle-expand)))
       ("e" efrit-agent-expand-all :description (lambda () (efrit-agent-menu--desc "unfold every row" 'efrit-agent-expand-all)) :transient t)
       ("c" efrit-agent-collapse-all :description (lambda () (efrit-agent-menu--desc "fold every row" 'efrit-agent-collapse-all)) :transient t)
       ("n" efrit-agent-next-tool :description (lambda () (efrit-agent-menu--desc "next row" 'efrit-agent-next-tool)) :transient t)
       ("b" efrit-agent-previous-tool :description (lambda () (efrit-agent-menu--desc "previous row" 'efrit-agent-previous-tool)) :transient t)
       ("o" efrit-agent-open-at-point :description (lambda () (efrit-agent-menu--desc "open the row's file / report" 'efrit-agent-open-at-point)))
       ("w" efrit-agent-copy-tool-output :description (lambda () (efrit-agent-menu--desc "copy the row's output" 'efrit-agent-copy-tool-output)))]
      ["View"
       ("v" efrit-agent-cycle-verbosity :description (lambda () (efrit-agent-menu--desc "verbosity" 'efrit-agent-cycle-verbosity)) :transient t)
       ("m" efrit-agent-cycle-display-mode :description (lambda () (efrit-agent-menu--desc "which rows unfold by default" 'efrit-agent-cycle-display-mode)) :transient t)
       ("h" efrit-agent-cycle-header-style :description (lambda () (efrit-agent-menu--desc "header style" 'efrit-agent-cycle-header-style)) :transient t)
       ("g" efrit-agent-refresh :description (lambda () (efrit-agent-menu--desc "refresh the header" 'efrit-agent-refresh)))
       ("z" efrit-agent-narrow-to-turns :description (lambda () (efrit-agent-menu--desc "narrow to the last turn (C-u N: last N)" 'efrit-agent-narrow-to-turns)))
       ("a" efrit-agent-widen :description (lambda () (efrit-agent-menu--desc "widen: the whole conversation" 'efrit-agent-widen)))
       ("f" efrit-transcript-open :description (lambda () (efrit-agent-menu--desc "open this session's transcript file" 'efrit-transcript-open)))
       ("+" efrit-markdown-image-scale-increase :description (lambda () (efrit-agent-menu--desc "image at point (else all) larger" 'efrit-markdown-image-scale-increase)) :transient t)
       ("-" efrit-markdown-image-scale-decrease :description (lambda () (efrit-agent-menu--desc "image smaller" 'efrit-markdown-image-scale-decrease)) :transient t)
       ("=" efrit-markdown-image-scale-reset :description (lambda () (efrit-agent-menu--desc "image default size" 'efrit-markdown-image-scale-reset)) :transient t)]
      ["Session"
       ("N" efrit-agent-new-conversation :description (lambda () (efrit-agent-menu--desc "new conversation here" 'efrit-agent-new-conversation)))
       ("R" efrit-agent-restart :description (lambda () (efrit-agent-menu--desc "restart: fresh session, same windows" 'efrit-agent-restart)))
       ("r" efrit-agent-browse-sessions :description (lambda () (efrit-agent-menu--desc "resume a saved session" 'efrit-agent-browse-sessions)))
       ("i" efrit-agent-copy-session-id :description (lambda () (efrit-agent-menu--desc "copy the session id" 'efrit-agent-copy-session-id)))
       ("M" efrit-menu :description (lambda () (efrit-agent-menu--desc "efrit menu: model, sandbox, doctor" 'efrit-menu)))
       ("/" efrit-agent-slash-help :description (lambda () "the /commands of the input"))
       ("?" efrit-agent-help :description (lambda () (efrit-agent-menu--desc "all keys, as text" 'efrit-agent-help)))]
      ["" ("q" "close this menu" transient-quit-one)]])
  "The agent buffer menu, kept as data so a reload redefines it.")

(defun efrit-agent-menu--define ()
  "Define `efrit-agent-menu' when transient is available."
  (when (require 'transient nil t)
    (eval efrit-agent-menu--definition t)
    t))

(unless (efrit-agent-menu--define)
  (defun efrit-agent-menu ()
    "The agent buffer's command menu (needs the `transient' package)."
    (interactive)
    (if (efrit-agent-menu--define)
        (call-interactively 'efrit-agent-menu)
      (user-error "efrit-agent-menu needs the `transient' package"))))

(provide 'efrit-agent-menu)

;;; efrit-agent-menu.el ends here
