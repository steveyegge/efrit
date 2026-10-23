;;; efrit-agent-tab-line.el --- The agents in every window's tab line, lit when they need you -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.12.12
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, ai, convenience

;;; Commentary:

;; `efrit-agent-tab-line-mode' puts the live agent buffers at the end
;; of every window's tab line, after that window's own buffers, each
;; as a pill in its state colour: amber while working, green idle,
;; violet with a dot when a reply is unread, and red, saturated, when
;; a prompt waits for you.  Clicking one shows that agent (tzz,
;; 2026-10-14: "a tab buffer line display of the active agents in
;; addition to normal buffers, and add a highlight to them if they are
;; waiting for me").
;;
;; It works with any tab-line setup: the agent tabs are appended by
;; filtering the result of `tab-line-tabs-function', and only the
;; agent tabs are drawn here; every other tab still goes through the
;; user's `tab-line-tab-name-format-function'.  A user who draws their
;; own tabs can instead call `efrit-agent-tab-line-pill' from their
;; formatter for the tabs where `efrit-agent-tab-line-agent-p' is
;; true, and `efrit-agent-tab-line-state' to colour them.  The
;; attention state is part of the tab-line cache key, and an
;; `attention-changed' event forces a redraw, so the pill changes the
;; moment a prompt opens.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'tab-line)
(require 'efrit-events)
(require 'efrit-pill)
(require 'efrit-agent-ring)

(declare-function efrit-agent-buffers "efrit-agent-core")
(declare-function efrit-agent-display "efrit-agent-core")
(declare-function efrit-repl-session-status "efrit-repl-session")
(defvar efrit-agent--repl-session)
(defvar efrit-agent--instance)
(defvar global-tab-line-mode)
(defvar tab-line-exclude)
(defvar efrit-agent-tab-line-mode)

(defgroup efrit-agent-tab-line nil
  "Agent buffers in the tab line."
  :group 'efrit-agent-ring)

(defcustom efrit-agent-tab-line-label-function #'efrit-agent-tab-line-label-default
  "Function from an agent buffer to the short text on its tab."
  :type 'function
  :group 'efrit-agent-tab-line)

;;;; State

(defun efrit-agent-tab-line-agent-p (tab)
  "Non-nil when TAB (a buffer or a tab alist) is a live agent buffer."
  (and (bufferp tab) (buffer-live-p tab)
       (with-current-buffer tab (derived-mode-p 'efrit-agent-mode))))

(defalias 'efrit-agent-tab-line-state #'efrit-agent-attention-state
  "BUFFER's state for colouring: `waiting', `unread', `working', `failed' or `idle'.")

(defun efrit-agent-tab-line-label-default (buffer)
  "\"proj\" or \"proj:name\" from the buffer name, without the *efrit[ ]* wrap."
  (let ((name (buffer-name buffer)))
    (if (string-match "\\`\\*efrit\\[\\(.*\\)\\]\\*\\'" name) (match-string 1 name)
      (string-trim name "\\*+" "\\*+"))))

;;;; Drawing

(defun efrit-agent-tab-line-pill (buffer &optional selected)
  "The pill for agent BUFFER: label, state colour, strong when waiting or SELECTED."
  (let* ((state (efrit-agent-tab-line-state buffer))
         (label (funcall efrit-agent-tab-line-label-function buffer))
         ;; the selected tab reads =name=, as the user's own tabs do
         ;; (tzz, 2026-10-14: "the same for active efrit buffers so
         ;; they are easier to spot")
         (text (if selected (format "=%s=" label) label))
         (help (pcase state
                 ('waiting (format "%s is waiting for you: %s" (buffer-name buffer)
                                   (buffer-local-value 'efrit-agent--waiting buffer)))
                 ('unread (format "%s has a reply you have not read" (buffer-name buffer)))
                 ('working (format "%s is working" (buffer-name buffer)))
                 (_ (format "%s (%s)" (buffer-name buffer) state)))))
    ;; the e's counter is filled when the agent needs you (saturated
    ;; pill when waiting), open otherwise (tzz, 2026-10-14)
    (let ((needs (memq state '(waiting unread))))
      (efrit-pill text :state state
                  :dot (if needs 'filled 'ring)
                  :strong (and needs (or selected (eq state 'waiting)))
                  :scale (if selected 1.1 efrit-pill-scale) :help-echo help))))

(defun efrit-agent-tab-line--format (orig tab tabs)
  "Draw agent tabs as pills; hand every other TAB to ORIG."
  (if (not (efrit-agent-tab-line-agent-p tab))
      (funcall orig tab tabs)
    (let ((selected (eq tab (window-buffer))))
      (apply #'propertize (efrit-agent-tab-line-pill tab selected)
             `(tab ,tab
               ,@(if selected '(selected t))
               keymap ,tab-line-tab-map
               follow-link ignore
               mouse-face tab-line-highlight)))))

;;;; Which tabs

(defun efrit-agent-tab-line--tabs (tabs)
  "TABS with the live agent buffers appended, in ring order, no duplicates."
  (let ((agents (efrit-agent-ring)))
    (if (null agents) tabs
      (append (seq-remove #'efrit-agent-tab-line-agent-p tabs) agents))))

(defun efrit-agent-tab-line--switch-in-place (orig &rest args)
  "Run ORIG (a tab-line switch) so a dedicated agent window changes buffer in place.
`tab-line-switch-to-next-tab' and friends `switch-to-buffer'; a
dedicated window refuses and the buffer pops up elsewhere, so C-<prior>
in an agent window seemed to do nothing (tzz, 2026-10-14).  The
dedication is lifted for the switch and put back on the window when
it still shows an agent."
  (let* ((window (selected-window))
         (was-dedicated (window-dedicated-p window)))
    (if (not (and was-dedicated (efrit-agent-tab-line-agent-p (window-buffer window))))
        (apply orig args)
      (set-window-dedicated-p window nil)
      (unwind-protect (apply orig args)
        (when (efrit-agent-tab-line-agent-p (window-buffer window))
          (set-window-dedicated-p window was-dedicated))))))

(defvar efrit-agent-tab-line--generation 0
  "Bumped whenever an agent's state, name or existence changes.
Part of the tab-line cache key.  The key function runs at every
redisplay of every window; walking the buffer list and asking each
agent its state there made typing stutter (2026-10-16).")

(defun efrit-agent-tab-line--cache-key (keys)
  "KEYS plus the agents' generation, so a state change redraws."
  (append keys (list efrit-agent-tab-line--generation)))

(defun efrit-agent-tab-line--refresh (&rest _)
  (cl-incf efrit-agent-tab-line--generation)
  (when (fboundp 'tab-line-force-update)
    (tab-line-force-update t)))

(defun efrit-agent-tab-line--ensure-in-agent-buffers ()
  "Turn `tab-line-mode' on in every agent buffer.
`global-tab-line-mode' skips them: their names start with `*' and
`tab-line-exclude-buffers' commonly excludes that (tzz, 2026-10-14:
\"I don't see the tab line\").  The agent tab line is where the other
agents are, so these buffers get it regardless of the exclusion."
  (dolist (b (efrit-agent-buffers))
    (with-current-buffer b
      (unless tab-line-mode (tab-line-mode 1)))))

(defun efrit-agent-tab-line--on-agent-mode ()
  (when efrit-agent-tab-line-mode
    (setq-local tab-line-exclude nil)
    (tab-line-mode 1)
    (efrit-agent-tab-line--refresh)))

;;;; Mode

;;;###autoload
(define-minor-mode efrit-agent-tab-line-mode
  "Show the agent buffers at the end of every tab line, lit when they need you.
Works with the tab-line setup you have: it appends the agents to what
`tab-line-tabs-function' returns and draws only those; `tab-line-mode'
itself is yours to turn on (`global-tab-line-mode')."
  :global t
  :group 'efrit-agent-tab-line
  (cond
   (efrit-agent-tab-line-mode
    (add-function :filter-return tab-line-tabs-function #'efrit-agent-tab-line--tabs)
    (add-function :around tab-line-tab-name-format-function #'efrit-agent-tab-line--format)
    (add-function :filter-return tab-line-cache-key-function #'efrit-agent-tab-line--cache-key)
    (dolist (fn '(tab-line-switch-to-next-tab tab-line-switch-to-prev-tab tab-line-select-tab-buffer))
      (advice-add fn :around #'efrit-agent-tab-line--switch-in-place))
    (efrit-subscribe 'attention-changed #'efrit-agent-tab-line--refresh)
    (efrit-subscribe 'status #'efrit-agent-tab-line--refresh)
    (add-hook 'efrit-agent-mode-hook #'efrit-agent-tab-line--on-agent-mode)
    (add-hook 'kill-buffer-hook #'efrit-agent-tab-line--on-kill)
    (efrit-agent-tab-line--ensure-in-agent-buffers))
   (t
    (remove-function tab-line-tabs-function #'efrit-agent-tab-line--tabs)
    (remove-function tab-line-tab-name-format-function #'efrit-agent-tab-line--format)
    (remove-function tab-line-cache-key-function #'efrit-agent-tab-line--cache-key)
    (dolist (fn '(tab-line-switch-to-next-tab tab-line-switch-to-prev-tab tab-line-select-tab-buffer))
      (advice-remove fn #'efrit-agent-tab-line--switch-in-place))
    (efrit-unsubscribe 'attention-changed #'efrit-agent-tab-line--refresh)
    (efrit-unsubscribe 'status #'efrit-agent-tab-line--refresh)
    (remove-hook 'efrit-agent-mode-hook #'efrit-agent-tab-line--on-agent-mode)
    (remove-hook 'kill-buffer-hook #'efrit-agent-tab-line--on-kill)))
  (efrit-agent-tab-line--refresh))

(defun efrit-agent-tab-line--on-kill ()
  (when (derived-mode-p 'efrit-agent-mode)
    (run-at-time 0 nil #'efrit-agent-tab-line--refresh)))

;; On by default where a tab line is in use: with `global-tab-line-mode'
;; already on at load, or when it is turned on later.
(defun efrit-agent-tab-line--follow-global ()
  (when (and (bound-and-true-p global-tab-line-mode) (not efrit-agent-tab-line-mode))
    (efrit-agent-tab-line-mode 1)))

(add-hook 'global-tab-line-mode-hook #'efrit-agent-tab-line--follow-global)
(efrit-agent-tab-line--follow-global)

(provide 'efrit-agent-tab-line)

;;; efrit-agent-tab-line.el ends here
