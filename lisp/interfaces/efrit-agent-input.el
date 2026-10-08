;;; efrit-agent-input.el --- Input handling for efrit-agent -*- lexical-binding: t -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.10.3
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; Input handling module for efrit-agent providing:
;; - Input minor mode for the editable input region
;; - Question display with option buttons
;; - Input prompt management
;; - History navigation (placeholder)

;;; Code:

(require 'cl-lib)
(require 'efrit-log)
(require 'efrit-agent-core)
(require 'efrit-agent-render)
(require 'efrit-session)
(require 'efrit-session-worklog)
(require 'efrit-session-persist)
(require 'efrit-repl-session)
(require 'efrit-repl-loop)
(require 'efrit-agent-mentions)
(require 'efrit-brief)

;; Forward declarations
(declare-function efrit-executor-respond "efrit-executor")
(declare-function efrit-agent-set-status "efrit-agent")
(declare-function efrit-do--start-async-session "efrit-do")
(declare-function efrit-session-id "efrit-session")
(declare-function efrit-agent--begin-session "efrit-agent-core")
(declare-function efrit-agent-toggle-expand "efrit-agent")
(declare-function efrit-agent-cancel "efrit-agent")
(declare-function efrit-agent-mode "efrit-agent")
(declare-function efrit-agent--init-regions "efrit-agent")
(declare-function efrit-agent--setup-regions "efrit-agent")

;;; REPL Session State
;;
;; Each agent buffer has a persistent REPL session that accumulates
;; conversation context across multiple inputs.

(defvar-local efrit-agent--repl-session nil
  "The persistent REPL session for this agent buffer.
Unlike efrit-session which completes after each command, this session
persists and accumulates conversation context.")

;;; Question Display

(declare-function efrit-agent-question-menu "efrit-agent-input")
(declare-function efrit-review-answer-escalation "efrit-review")
(defvar transient--prefix)
(defvar efrit-agent--question-menu-buffer)
(eieio-declare-slots command)
(declare-function transient--emergency-exit "transient")
(declare-function transient-active-prefix "transient")
(declare-function efrit-agent--turn-starts "efrit-agent")
(declare-function efrit-repl-loop-resume-after-reconnect-answer "efrit-repl-loop")

(defvar-local efrit-agent--question-menu-timer nil
  "The timer that will open this buffer's question menu, or nil.
Buffer-local: another session's question must not cancel it.")

(defun efrit-agent--add-question (question &optional options)
  "Add a QUESTION from Claude to the conversation region.
OPTIONS is an optional list of choices the user can select.
Returns the question ID for tracking responses."
  ;; End any streaming Claude message first
  (efrit-agent--stream-end-message)
  (let* ((q-id (format "question-%d" (cl-incf efrit-agent--message-counter)))
         (formatted-text
          (concat
           ;; Question indicator
           (propertize (format "%s " (efrit-agent--char 'status-waiting))
                       'face 'efrit-agent-timestamp)
           ;; Question text
           (propertize question 'face 'efrit-agent-question)
           "\n"
           ;; Options (if provided)
           (when options
             (concat
              "   "
              (mapconcat
               (lambda (opt-pair)
                 (let* ((idx (car opt-pair))
                        (opt (cdr opt-pair)))
                   (efrit-agent--make-option-button opt idx)))
               (cl-loop for opt in options
                        for idx from 1
                        collect (cons idx opt))
               " ")
              "\n"))
           ;; Hint for keyboard selection
           (when options
             (propertize (format "   Press %s or type custom response\n"
                                 (mapconcat #'number-to-string
                                            (number-sequence 1 (min 4 (length options)))
                                            "/"))
                         'face 'efrit-agent-timestamp))
           "\n")))
    (efrit-agent--append-to-conversation
     formatted-text
     (list 'efrit-type 'question
           'efrit-id q-id
           'efrit-question question
           'efrit-options options))
    ;; Update input prompt to indicate we're waiting
    (efrit-agent--update-input-prompt question options)
    ;; A menu for the choices, on top of the transcript.  Opened from
    ;; the command loop: this runs inside a tool result callback.
    (when (and options (efrit-agent-question-menu-available-p))
      (let ((buf (current-buffer)))
        (efrit-agent--cancel-question-menu-timer)
        (setq efrit-agent--question-menu-timer
              (run-at-time 0 nil (lambda ()
                                   (setq efrit-agent--question-menu-timer nil)
                                   (when (buffer-live-p buf)
                                     (with-current-buffer buf
                                       ;; Answered meanwhile (from Lisp, a
                                       ;; fast typist)?  Then no menu.
                                       (when efrit-agent--pending-question
                                         (efrit-agent--open-question-menu question options)))))))))
    q-id))

(defun efrit-agent--cancel-question-menu-timer ()
  (when (timerp efrit-agent--question-menu-timer)
    (cancel-timer efrit-agent--question-menu-timer))
  (setq efrit-agent--question-menu-timer nil))

(defun efrit-agent--close-question-menu ()
  "Make sure no question menu is up or about to come up.
Cancels the pending opener (the timer may not have run yet when the
answer arrives from Lisp: the opener then fired after the answer and
the menu stood over an idle buffer, 2026-09-25) and exits the menu if
it is showing.  `transient-current-command' is bound only while a
suffix runs, so `transient-active-prefix' is the check."
  (efrit-agent--cancel-question-menu-timer)
  ;; only a menu that answers into THIS buffer; another session's
  ;; menu stays up.  `transient--prefix' is read directly, not through
  ;; `transient-active-prefix': that one is nil while the popup is
  ;; still delayed by `transient-show-popup' (0.5 s for tzz), so an
  ;; answer inside that window left the menu to come up over an idle
  ;; buffer and eat every key after (drive 2026-09-30: `1' bound to
  ;; the menu four sections later, then a hang).
  (when (and (boundp 'transient--prefix)
             transient--prefix
             (eq (oref transient--prefix command) 'efrit-agent-question-menu)
             (or (null efrit-agent--question-menu-buffer)
                 (eq efrit-agent--question-menu-buffer (current-buffer))))
    ;; `transient-quit-one' is an empty command: the exit happens in
    ;; transient's pre-command hook when the USER invokes it.  Called
    ;; from Lisp it did nothing and the menu stayed up (2026-09-26).
    ;; This is what transient itself uses to tear down asynchronously.
    (transient--emergency-exit 'efrit)))

;;; Question menu (transient)

(defcustom efrit-agent-question-menu t
  "When non-nil, a question with options opens a menu of the choices.
The choices are still shown in the transcript and 1-4 still select
them; the menu is a layer on top.  Closing it (q, C-g) leaves the
question waiting for a typed answer."
  :type 'boolean
  :group 'efrit-agent)

(defvar efrit-agent--question-menu-options nil
  "Options of the question the menu is showing, for its suffixes.")
(defvar efrit-agent--question-menu-buffer nil
  "The agent buffer the open question menu answers into.")

(defun efrit-agent-question-menu-available-p ()
  "Non-nil when the choices menu can be shown."
  (and efrit-agent-question-menu
       (not noninteractive)
       (require 'transient nil t)))

(defun efrit-agent--question-menu-choose (n)
  "Answer the pending question with option N from the menu."
  (when (buffer-live-p efrit-agent--question-menu-buffer)
    (with-current-buffer efrit-agent--question-menu-buffer
      (when (efrit-agent--select-option n)
        (efrit-agent--clear-input)))))

(defun efrit-agent--question-menu-custom ()
  "Close the menu and leave point in the input for a typed answer."
  (interactive)
  (when (buffer-live-p efrit-agent--question-menu-buffer)
    (efrit-agent-display efrit-agent--question-menu-buffer t)))

(defun efrit-agent--question-menu-description ()
  "The question text as the menu's heading, wrapped to the frame."
  (let ((q (car efrit-agent--pending-question)))
    (with-temp-buffer
      (insert (or q "Choose"))
      (let ((fill-column (max 40 (- (frame-width) 10))))
        (fill-region (point-min) (point-max)))
      (buffer-string))))

(defun efrit-agent--question-menu-option-label (n)
  "Label of option N, or nil past the end.  Long labels are cut."
  (when-let* ((opt (nth (1- n) efrit-agent--question-menu-options)))
    (truncate-string-to-width opt (max 30 (- (frame-width) 12)) nil nil "…")))

(defconst efrit-agent--question-menu-definition
     '(transient-define-prefix efrit-agent-question-menu ()
        "Answer the model's question."
        [:description efrit-agent--question-menu-description
         ["Choose"
          ("1" (lambda () (interactive) (efrit-agent--question-menu-choose 1))
           :description (lambda () (efrit-agent--question-menu-option-label 1))
           :if (lambda () (efrit-agent--question-menu-option-label 1)))
          ("2" (lambda () (interactive) (efrit-agent--question-menu-choose 2))
           :description (lambda () (efrit-agent--question-menu-option-label 2))
           :if (lambda () (efrit-agent--question-menu-option-label 2)))
          ("3" (lambda () (interactive) (efrit-agent--question-menu-choose 3))
           :description (lambda () (efrit-agent--question-menu-option-label 3))
           :if (lambda () (efrit-agent--question-menu-option-label 3)))
          ("4" (lambda () (interactive) (efrit-agent--question-menu-choose 4))
           :description (lambda () (efrit-agent--question-menu-option-label 4))
           :if (lambda () (efrit-agent--question-menu-option-label 4)))
          ("5" (lambda () (interactive) (efrit-agent--question-menu-choose 5))
           :description (lambda () (efrit-agent--question-menu-option-label 5))
           :if (lambda () (efrit-agent--question-menu-option-label 5)))
          ("6" (lambda () (interactive) (efrit-agent--question-menu-choose 6))
           :description (lambda () (efrit-agent--question-menu-option-label 6))
           :if (lambda () (efrit-agent--question-menu-option-label 6)))]
         ["Or"
          ("t" "type an answer" efrit-agent--question-menu-custom)
          ("q" "type an answer" efrit-agent--question-menu-custom)]])
  "The question menu, kept as data so a reload redefines it.")

(defun efrit-agent--define-question-menu ()
  "Define `efrit-agent-question-menu' (the transient) when transient is available.
Evaluated on every call, not behind `fboundp': a prefix defined once
kept stale suffixes across reloads (\"Suffix command ... is not
defined\" after a rename, 2026-09-25)."
  (when (require 'transient nil t)
    (eval efrit-agent--question-menu-definition t)))

(defun efrit-agent--open-question-menu (question options)
  "Show the transient menu for QUESTION with OPTIONS in this agent buffer."
  (when (efrit-agent-question-menu-available-p)
    (efrit-agent--define-question-menu)
    (setq efrit-agent--question-menu-options options
          efrit-agent--question-menu-buffer (current-buffer))
    (ignore question)
    (call-interactively #'efrit-agent-question-menu)))

(defun efrit-agent--set-input-prompt (text)
  "Replace the input prompt with TEXT.
The prompt is the text between the beginning of the prompt line and
`efrit-agent--input-start'; the input region itself starts at the
marker.  Earlier versions inserted AT the marker, which put the prompt
inside the input region where it accumulated on every update and would
have been sent back to Claude as part of the user's answer."
  (when (and (markerp efrit-agent--input-start)
             (marker-position efrit-agent--input-start))
    (let ((inhibit-read-only t)
          (prompt-bol nil))
      (save-excursion
        (goto-char efrit-agent--input-start)
        ;; The prompt is a field: `line-beginning-position' would stop
        ;; after it and the old prompt would survive, growing by one
        ;; "> " per reset.  Ask for the true line start.
        (setq prompt-bol (let ((inhibit-field-text-motion t)) (line-beginning-position)))
        (delete-region prompt-bol efrit-agent--input-start)
        (goto-char efrit-agent--input-start)
        ;; Read-only with rear-nonsticky: typing at the marker inserts
        ;; editable text after the prompt, while a backspace at the
        ;; start of the input, or a kill spanning the prompt, is
        ;; refused.  The initial prompt from `efrit-agent--setup-buffer'
        ;; carries the same properties; earlier versions dropped them
        ;; on every rewrite, which is how the prompt got deleted.
        (insert (propertize text
                            'face 'efrit-agent-input-prompt
                            'efrit-agent-prompt t
                            'read-only t
                            'field 'output
                            'front-sticky '(read-only field)
                            'rear-nonsticky t))
        ;; Inserting at the marker leaves the marker before the text;
        ;; move it back to the start of the (empty) input region.
        (set-marker efrit-agent--input-start (point)))
      ;; The rewrite strands point (and window points) that sat at the
      ;; old prompt before the new text; anything left inside the prompt
      ;; span would type OUTSIDE the input region, so move it in.
      (when (and (>= (point) prompt-bol)
                 (<= (point) efrit-agent--input-start))
        (goto-char (point-max)))
      (dolist (win (get-buffer-window-list (current-buffer) nil t))
        (when (and (>= (window-point win) prompt-bol)
                   (<= (window-point win) efrit-agent--input-start))
          (set-window-point win (point-max)))))))

(defun efrit-agent--update-input-prompt (_question options)
  "Update the input region prompt for question with OPTIONS.
Shows context about what input is expected."
  (efrit-agent--set-input-prompt
   (if options
       (format "Answer (or %s): "
               (mapconcat #'number-to-string
                          (number-sequence 1 (min 4 (length options)))
                          "/"))
     "Answer: ")))

(defun efrit-agent--reset-input-prompt ()
  "Reset the input region prompt to the default state.
Called after responding to a question."
  (efrit-agent--set-input-prompt "> "))

;;; Option Selection

(defun efrit-agent--select-option (n)
  "Select option N (1-indexed) from pending question options.
Returns nil if no options or N is out of range."
  (when (eq efrit-agent--status 'waiting)
    (let* ((options (cadr efrit-agent--pending-question))
           (option (and options (nth (1- n) options))))
      (when option
        ;; Questions can come from a waiting REPL session (request_user_input
        ;; in the REPL loop, ef-dcn) or from an efrit-do executor session.
        (if (and efrit-agent--repl-session
                 (eq (efrit-repl-session-status efrit-agent--repl-session)
                     'waiting))
            (progn
              (efrit-agent--add-user-message option)
              (efrit-agent--repl-send option))
          (efrit-executor-respond option))
        (setq efrit-agent--status 'working)
        t))))

(defun efrit-agent-input--option-available-p (n)
  "Non-nil when a pending question has an option N to pick."
  (and (eq efrit-agent--status 'waiting)
       (> (length (cadr efrit-agent--pending-question)) (1- n))))

(defun efrit-agent-input-select-option (n)
  "Answer the pending question with its option N."
  (interactive "p")
  (if (efrit-agent--select-option n)
      (efrit-agent--clear-input)
    (user-error "No pending question with an option %d" n)))

(defun efrit-agent-input--option-item (n)
  "A menu-item for digit N: the option command while a question has
option N, else nothing (the key falls through and types the digit)."
  (let ((cmd (lambda () (interactive) (efrit-agent-input-select-option n))))
    (list 'menu-item (format "option %d" n) cmd
          :filter (lambda (c) (and (efrit-agent-input--option-available-p n) c)))))

(defun efrit-agent-input--tab-filter (cmd)
  "The TAB command: CMD (indent) on a list item, else `completion-at-point'."
  (if (efrit-agent-input--list-item) cmd #'completion-at-point))

;;; Input Minor Mode
;;
;; A minor mode that activates when point is in the input region.
;; Provides a separate keymap for editing input (RET sends, etc.)

(defvar efrit-agent-input-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Sending input
    ;; RET sends from the input; in the read-only conversation the
    ;; major mode's RET (toggle the row) applies: the item filters to
    ;; nothing there, so the minor mode does not shadow it
    (define-key map (kbd "RET") '(menu-item "" efrit-agent-input-send
                                            :filter efrit-agent-input--ret-filter))
    (define-key map [return] '(menu-item "" efrit-agent-input-send
                                         :filter efrit-agent-input--ret-filter))
    (define-key map (kbd "S-<return>") #'efrit-agent-input-newline)
    (define-key map (kbd "M-<return>") #'efrit-agent-input-send-override)
    (define-key map (kbd "C-j") #'efrit-agent-input-newline)
    (define-key map (kbd "<backtab>") #'efrit-agent-input-dedent-item)
    (define-key map (kbd "S-TAB") #'efrit-agent-input-dedent-item)
    (define-key map (kbd "C-c C-q") #'efrit-agent-queue-show)
    (define-key map (kbd "C-c C-c") #'efrit-agent-input-send)
    (define-key map (kbd "C-c C-s") #'efrit-agent-input-send)
    (define-key map (kbd "C-c C-k") #'efrit-agent-input-clear)
    ;; Restore standard editing keys while typing in the input region.
    ;; The major-mode map keeps C-k/C-q bound to agent actions as a
    ;; convenience for the read-only conversation region; shadow them here
    ;; so they behave like normal editing whenever point is in the input area.
    (define-key map (kbd "C-k") #'kill-line)
    (define-key map (kbd "C-q") #'quoted-insert)
    (define-key map (kbd "d") #'self-insert-command)
    (define-key map (kbd "o") #'self-insert-command)
    (define-key map (kbd "?") #'self-insert-command)
    (define-key map (kbd "+") #'self-insert-command)
    (define-key map (kbd "-") #'self-insert-command)
    (define-key map (kbd "=") #'self-insert-command)
    ;; comint conventions: C-a goes to just after the prompt, C-c C-u
    ;; kills the whole input, C-c C-a is the true beginning of line
    (define-key map (kbd "C-a") #'efrit-agent-input-bol)
    (define-key map (kbd "<home>") #'efrit-agent-input-bol)
    (define-key map (kbd "C-c C-a") #'beginning-of-line)
    (define-key map (kbd "C-c C-u") #'efrit-agent-input-kill)
    ;; History on M-p/M-n, as in comint and eshell.  The arrows are
    ;; movement: an <up> that recalled history on the first input line
    ;; never let the user into the transcript (tzz, 2026-10-01: "how do
    ;; I scroll back? up/down are history").
    (define-key map (kbd "M-p") #'efrit-agent-input-history-prev)
    (define-key map (kbd "M-n") #'efrit-agent-input-history-next)
    (define-key map (kbd "<up>") #'efrit-agent-input-up)
    (define-key map (kbd "<down>") #'efrit-agent-input-down)
    ;; Completion
    ;; On a list item TAB indents it; elsewhere it completes
    (define-key map (kbd "TAB") '(menu-item "" efrit-agent-input-indent-item
                                            :filter efrit-agent-input--tab-filter))
    (define-key map [tab] '(menu-item "" efrit-agent-input-indent-item
                                      :filter efrit-agent-input--tab-filter))
    ;; Quick option selection (1-4 when waiting for question response)
    ;; Only effective when status is 'waiting' (checked in handler)
    ;; The digit answers a pending question; otherwise the binding
    ;; resolves to nothing and the key types the digit.  A menu-item
    ;; :filter decides at lookup time, so `C-h k 1' tells the truth
    ;; and no command has to fake `self-insert-command' (after
    ;; copilot-nes-mode-map, 2026-09-28).
    (define-key map (kbd "1") (efrit-agent-input--option-item 1))
    (define-key map (kbd "2") (efrit-agent-input--option-item 2))
    (define-key map (kbd "3") (efrit-agent-input--option-item 3))
    (define-key map (kbd "4") (efrit-agent-input--option-item 4))
    map)
  "Keymap for `efrit-agent-input-mode'.")

(define-minor-mode efrit-agent-input-mode
  "Minor mode for editing input in the efrit-agent buffer.
Activates when point is in the input region, providing a separate
keymap for input editing.

Key bindings:
\\{efrit-agent-input-mode-map}"
  :lighter " Input"
  :keymap efrit-agent-input-mode-map
  (when efrit-agent-input-mode
    ;; Set up completion when mode is enabled
    (efrit-agent--setup-completion)))

(defun efrit-agent-input--ret-filter (cmd)
  "RET: CMD (send) in the input region; nil elsewhere, so the major
mode's RET (`efrit-agent-toggle-expand') is what runs.  Sending
while a turn runs queues or steers (`efrit-agent-busy-submit-default-function');
M-RET does the other one.  S-RET and C-j insert a newline."
  (and (efrit-agent--in-input-region-p) cmd))

(define-obsolete-function-alias 'efrit-agent-input-send-or-newline
  #'efrit-agent-input-send "0.10.3"
  "RET is a menu-item that resolves to `efrit-agent-input-send' in the
input and to the major mode's binding elsewhere.")

(defun efrit-agent-input-send-override ()
  "Send the input the other way round from RET while a turn runs.
RET queues (or steers) by default; this steers (or queues).  Idle,
it sends like RET."
  (interactive)
  (efrit-agent-input-send t))

(defconst efrit-agent-input--bullet-regexp
  "\\([ \t]*\\)\\([-*+]\\)[ \t]+\\(.*\\)$"
  "A bullet item: indent, marker, content.  Matched with `looking-at'
from the line's start; no `^': the first input line starts after the
prompt field, where `beginning-of-line' stops but `^' does not match.")

(defconst efrit-agent-input--numbered-regexp
  "\\([ \t]*\\)\\([0-9]+\\)\\.[ \t]+\\(.*\\)$"
  "A numbered item: indent, number, content.")

(defconst efrit-agent-input-list-indent 2
  "Columns a list item moves on TAB / S-TAB in the input.")

(defun efrit-agent-input--list-item ()
  "The Markdown list item on the current input line, or nil.
A plist (:type bullet|numbered :indent STRING :marker STRING :content STRING)."
  (when (efrit-agent--in-input-region-p)
    (save-excursion
      (beginning-of-line)
      (cond ((looking-at efrit-agent-input--bullet-regexp)
             (list :type 'bullet :indent (match-string-no-properties 1)
                   :marker (match-string-no-properties 2) :content (match-string-no-properties 3)))
            ((looking-at efrit-agent-input--numbered-regexp)
             (list :type 'numbered :indent (match-string-no-properties 1)
                   :marker (match-string-no-properties 2) :content (match-string-no-properties 3)))))))

(defun efrit-agent-input-newline ()
  "Insert a newline in the input without sending.
On a Markdown list item the new line continues the list (`- ' again,
or the next number); on an empty item the marker is removed and the
list ends.  Idea from agent-shell's list-edit mode."
  (interactive "*")
  (let ((item (efrit-agent-input--list-item)))
    (cond
     ((and item (string-empty-p (string-trim (plist-get item :content))))
      (delete-region (line-beginning-position) (line-end-position))
      (newline))
     (item
      (newline)
      (insert (plist-get item :indent)
              (if (eq (plist-get item :type) 'bullet)
                  (concat (plist-get item :marker) " ")
                (format "%d. " (1+ (string-to-number (plist-get item :marker)))))))
     (t (newline)))))

(defun efrit-agent-input-indent-item ()
  "Indent the list item on this input line one step."
  (interactive "*")
  (save-excursion
    (beginning-of-line)
    (insert (make-string efrit-agent-input-list-indent ?\s))))

(defun efrit-agent-input-dedent-item ()
  "Move the list item on this input line left one step; nothing elsewhere."
  (interactive "*")
  (when-let* ((item (efrit-agent-input--list-item))
              (indent (plist-get item :indent)))
    (when (>= (length indent) efrit-agent-input-list-indent)
      (save-excursion
        (beginning-of-line)
        (delete-char efrit-agent-input-list-indent)))))

;;; Submitting while a turn runs
;;
;; The prompt is writable at all times, so a submission can arrive
;; while the model works.  Two things can be meant: "after this, do
;; that" (queue: the text starts its own turn when this one ends) or
;; "while you are at it" (steer: the text goes to the model with the
;; next tool results, inside this turn).  RET does the first by
;; default and M-RET the second; both are customizable.

(defcustom efrit-agent-busy-submit-default-function #'efrit-agent-busy-submit-queue
  "What RET does with the input while a turn runs.
A function of one argument, the input text, in the agent buffer.
`efrit-agent-busy-submit-queue' starts a new turn with it when this one
ends; `efrit-agent-busy-submit-steer' hands it to the running turn."
  :type '(choice (const :tag "Queue for the next turn" efrit-agent-busy-submit-queue)
                 (const :tag "Steer the running turn" efrit-agent-busy-submit-steer)
                 function)
  :group 'efrit-agent)

(defcustom efrit-agent-busy-submit-override-function #'efrit-agent-busy-submit-steer
  "What M-RET (or C-u RET) does with the input while a turn runs.
See `efrit-agent-busy-submit-default-function'."
  :type '(choice (const :tag "Queue for the next turn" efrit-agent-busy-submit-queue)
                 (const :tag "Steer the running turn" efrit-agent-busy-submit-steer)
                 function)
  :group 'efrit-agent)

(defface efrit-agent-steer-prefix
  '((t :inherit efrit-agent-user-prefix :foreground "orange"))
  "Face of the marker on a user line that steered a running turn."
  :group 'efrit-agent)

(defface efrit-agent-queued-prefix
  '((t :inherit efrit-agent-user-prefix :foreground "gray60"))
  "Face of the marker on a user line that waits for the turn to end."
  :group 'efrit-agent)

(defun efrit-agent--session-busy-p ()
  "Non-nil when the buffer's REPL session is in the middle of a turn.
A session marked working that no loop drives (after a reload) is
recovered first, so it does not read as busy forever."
  (and efrit-agent--repl-session
       (progn (efrit-repl-loop-recover-stale efrit-agent--repl-session)
              (eq (efrit-repl-session-status efrit-agent--repl-session) 'working))))

(defun efrit-agent-busy-submit-queue (input)
  "Queue INPUT: it starts a turn of its own when the running one ends.
Shown in the conversation at once, marked as waiting; the mark is
removed when it is sent."
  (let* ((session efrit-agent--repl-session)
         (count (efrit-repl-session-enqueue session input)))
    (efrit-agent--add-user-message input 'queued)
    (efrit-publish 'queued `((:session-id . ,(efrit-repl-session-id session))
                             (:text . ,input) (:count . ,count)))
    (message "Efrit: queued for after this turn (%d waiting); C-c C-q shows the queue" count)))

(defun efrit-agent-busy-submit-steer (input)
  "Steer the running turn with INPUT: the model reads it with its next tool results.
A turn that ends without another tool round has no seam; the text
then starts the next turn, as if queued."
  (let ((session efrit-agent--repl-session))
    (efrit-agent--add-user-message input 'steer)
    (efrit-publish 'steer `((:session-id . ,(efrit-repl-session-id session))
                            (:text . ,input)))
    (message "Efrit: steering; the model reads it with its next tool results")))

(defun efrit-agent-input-send (&optional override)
  "Send the current input using the persistent REPL session model.
Conversation context accumulates across inputs - Claude remembers
what was discussed previously.

If the REPL session is idle, a new turn starts with the input.  If it
is working, `efrit-agent-busy-submit-default-function' decides (queue
by default); with OVERRIDE, `efrit-agent-busy-submit-override-function'
\(steer by default).  If no REPL session exists, one is created.
Idle, a prefix argument (OVERRIDE) means \"grill me\": the model asks
its clarifying questions before acting (`efrit-grill-me')."
  (interactive "P")
  (when (and override (not (efrit-agent--session-busy-p)))
    (setq efrit-grill-me t))
  (let ((input (efrit-agent--get-input)))
    (cond
     ((or (null input) (string-empty-p (string-trim input)))
      (message "Nothing to send"))
     ;; A /command runs here and sends nothing
     ((efrit-agent-slash-run input))
     (t
      (efrit-agent--add-to-history input)
      (efrit-agent--reset-history-navigation)
      (if (efrit-agent--session-busy-p)
          ;; The busy function renders the line itself, so a signal
          ;; from it (nothing queued) leaves the draft in place
          (progn
            (funcall (if override
                         efrit-agent-busy-submit-override-function
                       efrit-agent-busy-submit-default-function)
                     input)
            (efrit-agent--clear-input))
        (efrit-agent--add-user-message input)
        (efrit-agent--clear-input)
        (when (and efrit-agent--input-start
                   (marker-position efrit-agent--input-start))
          (goto-char efrit-agent--input-start))
        (efrit-agent--repl-send input (efrit-agent--api-input-for input)))))))

(defun efrit-agent--api-input-for (input &optional command)
  "What the model receives for INPUT: mentions expanded, images attached,
the prompt suffixes (`efrit-prompt-suffix-functions') appended.
COMMAND names the command that produced INPUT, for the providers.  A
string when there are no images; else a vector of content blocks
\(the image blocks, then the text) that `efrit-repl-continue' passes
through.  Returns nil when INPUT needs no change, so the caller's
default applies."
  (let* ((expanded (efrit-agent-mentions-expand input))
         (text (efrit-prompt-apply-suffixes expanded (or command this-command)))
         (images (efrit-agent-mentions-content-blocks input)))
    (cond
     (images (vconcat images (list `((type . "text") (text . ,(substring-no-properties text))))))
     ((not (equal text input)) (substring-no-properties text)))))

(defun efrit-agent--send-queued (session)
  "Start the next queued input of SESSION as a turn, keeping the user's draft.
The queued line already shows in the conversation; its waiting mark
is dropped.  Returns non-nil when a turn started."
  (when-let* ((input (efrit-repl-session-dequeue session))
              (buffer (efrit-repl-session-buffer session)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((draft (efrit-agent--get-input)))
          (efrit-agent--unmark-queued-message input)
          (prog1 (efrit-agent--repl-send input)
            ;; The turn start does not touch the input, but keep the
            ;; contract explicit: what the user was typing stays.
            (when (and draft (not (equal draft (efrit-agent--get-input))))
              (efrit-agent--clear-input)
              (save-excursion (goto-char (point-max)) (insert draft)))
            (message "Efrit: sending queued input (%d more waiting)"
                     (length (efrit-repl-session-queue session)))))))))

(defun efrit-agent-queue-show ()
  "List the inputs waiting for the turn to end, with a way to drop one."
  (interactive)
  (let* ((session efrit-agent--repl-session)
         (queue (and session (efrit-repl-session-queue session))))
    (if (null queue)
        (message "Efrit: nothing queued")
      (let* ((choices (cons "(keep all)" (cons "(drop all)" (copy-sequence queue))))
             (choice (completing-read
                      (format "%d queued; drop which? " (length queue)) choices nil t)))
        (cond
         ((equal choice "(drop all)")
          (dolist (input queue) (efrit-agent--unmark-queued-message input 'dropped))
          (setf (efrit-repl-session-queue session) nil)
          (message "Efrit: queue emptied"))
         ((member choice queue)
          (setf (efrit-repl-session-queue session) (remove choice (efrit-repl-session-queue session)))
          (efrit-agent--unmark-queued-message choice 'dropped)
          (message "Efrit: dropped; %d still queued" (length (efrit-repl-session-queue session)))))))))

(defun efrit-agent--repl-send (input &optional api-input)
  "Send INPUT to the REPL session.
Creates a new REPL session if needed, otherwise continues the existing one.
API-INPUT, when given, is what the model receives in place of INPUT
\(see `efrit-repl-continue').  Returns non-nil if the turn started."
  ;; Ensure we have a REPL session
  (unless efrit-agent--repl-session
    (setq efrit-agent--repl-session (efrit-repl-session-create default-directory))
    (setf (efrit-repl-session-buffer efrit-agent--repl-session) (current-buffer)))

  (let ((session efrit-agent--repl-session))
    (pcase (efrit-repl-session-status session)
      ;; Idle - continue with new input
      ('idle
       (efrit-repl-continue session input
                            #'efrit-agent--on-turn-complete api-input)
       (message "Efrit: continuing conversation")
       t)

      ;; Paused - resume and continue
      ('paused
       (efrit-repl-session-resume session)
       (efrit-repl-continue session input
                            #'efrit-agent--on-turn-complete api-input)
       (message "Efrit: resumed and continuing")
       t)

      ;; Working: the interactive path never gets here (it queues or
      ;; steers first); a Lisp caller learns the session is busy.  A
      ;; stale working status was already recovered by
      ;; `efrit-agent--session-busy-p'.
      ('working
       (message "Efrit: session is busy")
       nil)

      ;; Waiting for specific input (question)
      ('waiting
       ;; Handle question response.  A question the loop asked on the
       ;; reviewer's behalf turns the answer into an allow and a plain
       ;; instruction for the model (`efrit-review-answer-escalation').
       (let* ((pending (efrit-repl-session-pending-question session))
              (meta (nth 3 pending))
              (api-input (if (eq (plist-get meta :kind) 'review)
                             (efrit-review-answer-escalation
                              (efrit-repl-session-id session) meta input)
                           api-input)))
         (when pending
           (setf (efrit-repl-session-pending-question session) nil))
         (setq efrit-agent--pending-question nil)
         (efrit-agent--close-question-menu)
         (efrit-agent--reset-input-prompt)
         (if (eq (plist-get meta :kind) 'reconnect)
             ;; the connectivity question: the paused turn resumes
             ;; (or ends); the answer is not a message to the model
             (let ((outcome (efrit-repl-loop-resume-after-reconnect-answer session input)))
               (efrit-agent--add-user-message input)
               (message "Efrit: %s" (if (eq outcome 'aborted) "turn aborted" "resuming the turn"))
               t)
           (efrit-repl-continue session input
                                #'efrit-agent--on-turn-complete api-input)
           (message "Efrit: response sent")
           t)))

      ;; Unknown state
      (_
       (message "Efrit: unknown session state, resetting")
       (efrit-repl-session-reset session)
       (efrit-repl-continue session input
                            #'efrit-agent--on-turn-complete api-input)
       t))))

(defun efrit-agent-target-buffer (&optional buffer)
  "The agent buffer a Lisp caller means: BUFFER, the current one when
it is an agent buffer, else the default agent buffer, created."
  (require 'efrit-agent)
  (cond
   ((bufferp buffer) buffer)
   ((stringp buffer) (get-buffer-create buffer))
   ((derived-mode-p 'efrit-agent-mode) (current-buffer))
   (t (efrit-agent--get-buffer))))

(defun efrit-agent-repl-session (&optional buffer)
  "The REPL session of agent BUFFER, created if the buffer has none.
BUFFER defaults per `efrit-agent-target-buffer'.  For packages that
drive several turns through `efrit-submit' and need the session's
history marks (`efrit-repl-session-history-mark')."
  (require 'efrit-agent)
  (with-current-buffer (efrit-agent-target-buffer buffer)
    (unless (derived-mode-p 'efrit-agent-mode)
      (efrit-agent-mode))
    (unless efrit-agent--repl-session
      (setq efrit-agent--repl-session (efrit-repl-session-create default-directory))
      (setf (efrit-repl-session-buffer efrit-agent--repl-session) (current-buffer)))
    efrit-agent--repl-session))

;;;###autoload
(defun efrit-submit (shown &optional api-input buffer)
  "Start a REPL turn from Lisp: show SHOWN in the conversation, send API-INPUT.
For packages that prepare a prompt over data they gathered (a mail
reader over selected messages, say): SHOWN is the short line the user
sees as their turn, API-INPUT (default SHOWN) the full text the model
receives, with the usual editor-context block prepended.  BUFFER is
the agent buffer to use (default per `efrit-agent-target-buffer'),
opened if needed.  Returns non-nil if the turn started; nil when the
session is busy, in which case nothing was sent and nothing was
shown -- the caller decides whether to wait (`efrit-subscribe' to
`status') or to give up."
  (require 'efrit-agent)
  (let ((buffer (efrit-agent-target-buffer buffer)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'efrit-agent-mode)
        (efrit-agent-mode))
      (unless (and efrit-agent--conversation-end
                   (marker-position efrit-agent--conversation-end))
        (efrit-agent--init-regions)
        (efrit-agent--setup-regions))
      (let ((started (unless (efrit-agent--session-busy-p)
                       (efrit-agent--add-user-message shown)
                       (efrit-agent--repl-send shown api-input))))
        (efrit-agent-display buffer nil)
        started))))

(defun efrit-agent--on-turn-complete (session stop-reason)
  "Callback when a REPL turn completes.
SESSION is the REPL session, STOP-REASON indicates why the turn ended."
  (efrit-log 'debug "REPL turn complete: %s" stop-reason)
  (when-let* ((buf (and session (efrit-repl-session-buffer session))))
    (when (buffer-live-p buf)
      (with-current-buffer buf (efrit-agent--finish-regenerate stop-reason))))
  ;; Update agent buffer status based on stop reason
  ;; Note: efrit-repl-loop--end-turn already sets status, but we need to
  ;; handle the callback consistently. "unknown" typically means Claude
  ;; finished but with an unrecognized stop_reason - treat as success.
  (when (fboundp 'efrit-agent-set-status)
    (efrit-agent-set-status
     (pcase stop-reason
       ((or "end_turn" "session-complete" "unknown") 'idle)
       ("waiting-for-user" 'waiting)
       ("paused" 'paused)
       ("interrupted" 'interrupted)
       ((or "api-error" "error") 'failed)
       (_ 'idle))
     (and session (efrit-repl-session-buffer session))))
  ;; Reset prompt if we were waiting -- but keep the Answer: prompt when
  ;; the turn paused on request_user_input (ef-dcn).  This callback runs
  ;; from the API response handler, whose current buffer is NOT the agent
  ;; buffer; the buffer-local input markers are nil there and crash with
  ;; "markerp nil" (ef-jz6), so switch to the session's buffer first.
  (unless (equal stop-reason "waiting-for-user")
    (let ((buffer (efrit-repl-session-buffer session)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (efrit-agent--reset-input-prompt)))))
  ;; Auto-save session if enabled
  (when (and efrit-session-persist-auto-save session)
    (efrit-agent--auto-save-session session))
  ;; What the user submitted during the turn goes next -- after a
  ;; turn that ended well.  Not after a failure or an interrupt (a
  ;; broken run must not eat the queue: `efrit-agent-queue-resume'
  ;; restarts it), and not for a paused or waiting turn, whose answer
  ;; is the next input.  Off the event: let the turn tear down first.
  (when (efrit-repl-session-queue session)
    (if (member stop-reason '("end_turn" "session-complete" "unknown"))
        (run-at-time 0.1 nil #'efrit-agent--send-queued session)
      (message "Efrit: %d queued input(s) held after %s; C-c C-q lists them, M-x efrit-agent-queue-resume sends"
               (length (efrit-repl-session-queue session)) stop-reason))))

(defun efrit-agent-queue-resume ()
  "Send the next queued input now, after a turn that failed or was interrupted."
  (interactive)
  (cond
   ((null (and efrit-agent--repl-session (efrit-repl-session-queue efrit-agent--repl-session)))
    (message "Efrit: nothing queued"))
   ((efrit-agent--session-busy-p)
    (message "Efrit: a turn is running; the queue continues when it ends"))
   (t (efrit-agent--send-queued efrit-agent--repl-session))))

(defun efrit-agent--auto-save-session (session)
  "Auto-save SESSION to disk.
Saves asynchronously to avoid blocking the UI."
  (condition-case err
      (when (efrit-session-persist-save session)
        (efrit-log 'debug "Auto-saved session %s" (efrit-repl-session-id session)))
    (error
     (efrit-log 'error "Auto-save failed: %s" (error-message-string err)))))

(defun efrit-agent-input-bol ()
  "Move to the start of the input on this line, after the prompt.
On the prompt line that is just after the prompt (like `comint-bol');
on a continuation line of a multi-line input it is the line start.
A second press goes to the real beginning of line."
  (interactive "^")
  (let* ((true-bol (let ((inhibit-field-text-motion t)) (line-beginning-position)))
         (field-start (field-beginning (point) t)))
    (if (and (> field-start true-bol) (/= (point) field-start))
        (goto-char field-start)
      (goto-char true-bol))))

(defun efrit-agent--input-first-line-p ()
  "Non-nil if point is on the first line of the input."
  (<= (let ((inhibit-field-text-motion t)) (line-beginning-position))
      efrit-agent--input-start))

(defun efrit-agent--input-last-line-p ()
  "Non-nil if point is on the last line of the input."
  (= (line-end-position) (point-max)))

(defun efrit-agent-input-up ()
  "Move up one line; from the first input line, into the transcript.
History is on \\[efrit-agent-input-history-prev].  Until 2026-10-01
this recalled history on the first line and the transcript could not
be reached with the arrows."
  (interactive "^")
  (let ((line-move-visual nil)) (line-move -1 t)))

(defun efrit-agent-input-down ()
  "Move down one line, stopping at the end of the input."
  (interactive "^")
  (let ((line-move-visual nil)) (line-move 1 t)))

(defun efrit-agent-input-kill ()
  "Kill the whole current input (like `comint-kill-input'); it goes to the kill ring."
  (interactive)
  (when (and efrit-agent--input-start (marker-position efrit-agent--input-start)
             (< efrit-agent--input-start (point-max)))
    (kill-region efrit-agent--input-start (point-max))
    (goto-char (point-max))))

(defun efrit-agent-input-clear ()
  "Clear the current input."
  (interactive)
  (efrit-agent--clear-input)
  (when (and efrit-agent--input-start
             (marker-position efrit-agent--input-start))
    (goto-char efrit-agent--input-start))
  (message "Input cleared"))

;;; REPL Session Commands

(defun efrit-agent-new-conversation ()
  "Start a fresh conversation, clearing the REPL session context.
The conversation display is cleared and a new REPL session is created."
  (interactive)
  (when efrit-agent--repl-session
    (efrit-repl-session-reset efrit-agent--repl-session))
  ;; Clear conversation display
  (let ((inhibit-read-only t))
    (when (and efrit-agent--conversation-end
               (marker-position efrit-agent--conversation-end))
      (delete-region (point-min) efrit-agent--conversation-end)
      (goto-char (point-min))
      (insert "\n")
      (set-marker efrit-agent--conversation-end (point))))
  ;; Reset status
  (setq efrit-agent--status 'idle)
  (when (fboundp 'efrit-agent-set-status)
    (efrit-agent-set-status 'idle (current-buffer)))
  (message "Efrit: started new conversation"))

;;; Small conveniences

(defun efrit-agent--last-claude-message-bounds ()
  "Start and end of the model's most recent message, or nil.
Found by its `efrit-id': the Markdown pass leaves text inside a
message (a code label, a bullet) without every property, so a walk
over `efrit-type' runs stopped short and returned a tail (2026-09-25)."
  (save-excursion
    (let* ((limit (if (and efrit-agent--conversation-end (marker-position efrit-agent--conversation-end))
                      (marker-position efrit-agent--conversation-end)
                    (point-max)))
           (pos limit) (id nil))
      ;; The newest claude-message id before the input
      (while (and (> pos (point-min)) (not id))
        (setq pos (or (previous-single-property-change pos 'efrit-id nil (point-min)) (point-min)))
        (let ((p (max (point-min) (1- (or (next-single-property-change pos 'efrit-id nil limit) limit)))))
          (when (eq (get-text-property p 'efrit-type) 'claude-message)
            (setq id (get-text-property p 'efrit-id)))))
      (when id
        (let ((start (text-property-any (point-min) limit 'efrit-id id))
              (end nil))
          (when start
            (setq end start)
            ;; The last position still carrying the id (runs may be
            ;; interrupted by inserted chrome without the property)
            (let ((p start))
              (while (setq p (text-property-any p limit 'efrit-id id))
                (setq end (or (next-single-property-change p 'efrit-id nil limit) limit))
                (setq p end)))
            (and (< start end) (cons start end))))))))

(defun efrit-agent-copy-last-output ()
  "Copy the model's most recent message to the kill ring, wherever point is."
  (interactive)
  (if-let* ((bounds (efrit-agent--last-claude-message-bounds)))
      (let ((text (string-trim (buffer-substring-no-properties (car bounds) (cdr bounds)))))
        (kill-new text)
        (message "Copied %d characters of the last answer" (length text)))
    (message "Efrit: no answer to copy yet")))

(defun efrit-agent-block-quote (text)
  "TEXT as a Markdown block quote: every line prefixed with \"> \"."
  (concat "> " (replace-regexp-in-string "\n" "\n> " (string-trim text) t t)))

(defun efrit-agent-quote-region (start end)
  "Put the region START..END into the input as a block quote.
Idle: the quote goes to the end of the input with point below it,
ready for the question about it.  Busy: the quote is queued at once
for the next turn, with a trailer read from the minibuffer (empty
trailer: the quote alone).  Meant for the transcript: select part of
an earlier answer and ask about it."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (user-error "Select the text to quote first")))
  (let ((quoted (efrit-agent-block-quote (buffer-substring-no-properties start end))))
    (deactivate-mark)
    (if (efrit-agent--session-busy-p)
        (let* ((trailer (string-trim (read-string "Queue with the quote (empty: the quote alone): ")))
               (input (if (string-empty-p trailer) quoted (concat quoted "\n\n" trailer))))
          (efrit-agent--add-to-history input)
          (efrit-agent-busy-submit-queue input))
      (goto-char (point-max))
      (let ((draft (efrit-agent--get-input)))
        (insert (if (or (null draft) (string-empty-p draft)) "" "\n\n") quoted "\n\n")))))

(defun efrit-agent-session-id ()
  "The REPL session id of the agent buffer, or nil."
  (and efrit-agent--repl-session (efrit-repl-session-id efrit-agent--repl-session)))

(defun efrit-agent-copy-session-id ()
  "Put the REPL session id in the kill ring."
  (interactive)
  (if-let* ((id (efrit-agent-session-id)))
      (progn (kill-new id) (message "Copied session id %s" id))
    (message "Efrit: no session yet")))

;;; Regenerate: ask the last question again
;;
;; After copilot-chat-retry (2026-09-28): the old exchange stays in
;; the transcript until the new answer has arrived; on failure nothing
;; is lost.  The API history is rewound to before the last user
;; message, so the model does not see its previous answer.

(defvar-local efrit-agent--regenerate nil
  "While a regenerated turn runs: (OLD-START-MARKER OLD-END-MARKER MARK)
of the exchange being replaced and the history mark it was rewound to.")

(defun efrit-agent--last-exchange-bounds ()
  "The (START . END) of the last user line and everything after it, or nil."
  (let ((starts (efrit-agent--turn-starts)))
    (when starts
      (cons (car (last starts))
            (if (and efrit-agent--conversation-end (marker-position efrit-agent--conversation-end))
                (marker-position efrit-agent--conversation-end)
              (point-max))))))

(defun efrit-agent--last-user-input ()
  "The text of the last user message in the transcript, or nil."
  (when-let* ((b (efrit-agent--last-exchange-bounds)))
    (get-text-property (car b) 'efrit-user-text)))

(defun efrit-agent--history-mark-before-last-user (session)
  "The API history mark just before SESSION's last HUMAN message, or nil.
Tool results are user messages too; they do not count.  Rewinding to
one of them would leave a tool_use without its tool_result and the API
refuses every later request of the session (live run 2026-09-28 17:43:
eight steps timed out after one regenerate)."
  (let* ((messages (efrit-repl-session-api-messages session))
         (n (length messages))
         (i (1- n)))
    (while (and (>= i 0)
                (or (not (equal (efrit-repl-session--block-get (nth i messages) "role") "user"))
                    (efrit-repl-session-tool-result-message-p (nth i messages))))
      (cl-decf i))
    ;; 0 is a valid mark (the first message): return a list so the
    ;; caller can tell "none" from "before the first"
    (and (>= i 0) (list i))))

(defun efrit-agent-regenerate (&optional edit)
  "Ask the model the last question again; replace the old answer when the new one arrives.
With EDIT (a prefix argument), edit the question first.  The old
exchange stays until the new turn ends well; a failed or cancelled
turn leaves it in place.  Menu key `g'."
  (interactive "P")
  (when (efrit-agent--session-busy-p)
    (user-error "A turn is running; cancel it or wait"))
  (let* ((session (or efrit-agent--repl-session (user-error "No session in this buffer")))
         (bounds (or (efrit-agent--last-exchange-bounds) (user-error "No turn to regenerate")))
         (input (or (efrit-agent--last-user-input) (user-error "No user message to send again")))
         (mark (car (or (efrit-agent--history-mark-before-last-user session)
                        (user-error "The session history has no user message"))))
         (text (if edit (read-string "Regenerate with: " input) input)))
    (when (string-empty-p (string-trim text)) (user-error "Nothing to send"))
    (efrit-repl-session-rewind session mark)
    (efrit-agent--add-user-message text)
    ;; The old exchange ends where the new user line begins; taken
    ;; after the add, since the conversation-end marker moves with it
    (let ((new-start (car (last (efrit-agent--turn-starts)))))
      (setq efrit-agent--regenerate
            (list (copy-marker (car bounds)) (copy-marker new-start) mark)))
    (unless (efrit-agent--repl-send text)
      (setq efrit-agent--regenerate nil)
      (user-error "The session did not accept the turn"))
    (message "Efrit: regenerating; the old answer goes when the new one has arrived")))

(defun efrit-agent--finish-regenerate (stop-reason)
  "After a regenerated turn: drop the old exchange on success, keep it otherwise."
  (when-let* ((state efrit-agent--regenerate))
    (setq efrit-agent--regenerate nil)
    (pcase-let ((`(,start ,end ,_mark) state))
      (if (and (member stop-reason '("end_turn" "session-complete" "unknown"))
               (marker-position start) (marker-position end) (< start end))
          (efrit-agent--with-render
            (delete-region start end)
            (efrit-agent--reset-undo-history)
            (message "Efrit: regenerated; the earlier answer was replaced"))
        (message "Efrit: the regenerated turn ended with %s; the earlier answer stays" stop-reason))
      (set-marker start nil) (set-marker end nil))))

(defun efrit-agent-restart ()
  "Start over in this buffer: a fresh REPL session, the same windows.
The transcript is cleared, the queue and steering dropped, the windows
that showed the buffer keep showing it, with point in the input.  A
running turn is cancelled first, after confirmation."
  (interactive)
  (when (and (efrit-agent--session-busy-p)
             (not (yes-or-no-p "A turn is running; cancel it and restart? ")))
    (user-error "Not restarted"))
  (when (efrit-agent--session-busy-p)
    (efrit-agent-cancel))
  (let ((windows (get-buffer-window-list (current-buffer) nil t)))
    (efrit-agent-new-conversation)
    (setq efrit-agent--repl-session (efrit-repl-session-create default-directory))
    (setf (efrit-repl-session-buffer efrit-agent--repl-session) (current-buffer))
    (efrit-agent--reset-input-prompt)
    (dolist (w windows)
      (when (window-live-p w)
        (set-window-buffer w (current-buffer))
        (set-window-point w (point-max))))
    (goto-char (point-max))
    (message "Efrit: new session %s" (efrit-agent-session-id))))

(defun efrit-agent-pause ()
  "Pause the current REPL session."
  (interactive)
  (when efrit-agent--repl-session
    (efrit-repl-session-pause efrit-agent--repl-session)
    (message "Efrit: session paused")))

(defun efrit-agent-session-info ()
  "Display information about the current REPL session."
  (interactive)
  (if (null efrit-agent--repl-session)
      (message "No active REPL session")
    (let ((session efrit-agent--repl-session))
      (message "REPL Session: %s | Status: %s | Turns: %d | Elapsed: %s"
               (efrit-repl-session-id session)
               (efrit-repl-session-status session)
               (efrit-repl-session-turn-count session)
               (efrit-repl-session--format-elapsed session)))))

;;; Input History
;;
;; Input history supports M-p/M-n navigation through previous inputs.
;; History is maintained both per-session and globally (persisted across restarts).

(defun efrit-agent--combined-history ()
  "Return combined history list (session + global, deduplicated).
Session history takes precedence (appears first)."
  (let ((seen (make-hash-table :test 'equal))
        (result nil))
    ;; Add session history first
    (dolist (item efrit-agent--input-history)
      (unless (gethash item seen)
        (puthash item t seen)
        (push item result)))
    ;; Add global history (items not already seen)
    (dolist (item efrit-agent--global-history)
      (unless (gethash item seen)
        (puthash item t seen)
        (push item result)))
    (nreverse result)))

(defun efrit-agent--set-input (text)
  "Set the input region to TEXT."
  (when (and efrit-agent--input-start
             (marker-position efrit-agent--input-start))
    (let ((inhibit-read-only t))
      (efrit-agent--clear-input)
      (save-excursion
        (goto-char efrit-agent--input-start)
        (insert text))
      (goto-char (point-max)))))

(defun efrit-agent-input-history-prev ()
  "Navigate to previous input in history.
First M-p saves current input and shows most recent history.
Subsequent M-p moves further back in history."
  (interactive)
  (let ((history (efrit-agent--combined-history)))
    (if (null history)
        (message "No input history")
      (let ((max-idx (1- (length history))))
        ;; If starting navigation, save current input
        (when (= efrit-agent--history-index -1)
          (setq efrit-agent--history-temp (efrit-agent--get-input)))
        ;; Move to previous (older) entry
        (if (>= efrit-agent--history-index max-idx)
            (message "End of history")
          (cl-incf efrit-agent--history-index)
          (efrit-agent--set-input (nth efrit-agent--history-index history))
          (message "History: %d/%d" (1+ efrit-agent--history-index) (length history)))))))

(defun efrit-agent-input-history-next ()
  "Navigate to next (more recent) input in history.
When at the most recent entry, restores what user was typing."
  (interactive)
  (if (< efrit-agent--history-index 0)
      (message "No more history")
    (let ((history (efrit-agent--combined-history)))
      (cl-decf efrit-agent--history-index)
      (if (< efrit-agent--history-index 0)
          ;; Restore the original input the user was typing
          (progn
            (efrit-agent--set-input (or efrit-agent--history-temp ""))
            (setq efrit-agent--history-temp nil)
            (message "End of history (restored input)"))
        (efrit-agent--set-input (nth efrit-agent--history-index history))
        (message "History: %d/%d" (1+ efrit-agent--history-index) (length history))))))

(defun efrit-agent--add-to-history (input)
  "Add INPUT to both session and global history.
Deduplicates by removing any existing identical entry."
  (let ((trimmed (string-trim input)))
    (unless (string-empty-p trimmed)
      ;; Add to session history (remove duplicates first)
      (setq efrit-agent--input-history
            (cons trimmed (delete trimmed efrit-agent--input-history)))
      ;; Trim session history to max size
      (when (> (length efrit-agent--input-history) efrit-agent-history-max-size)
        (setq efrit-agent--input-history
              (seq-take efrit-agent--input-history efrit-agent-history-max-size)))
      ;; Add to global history (remove duplicates first)
      (setq efrit-agent--global-history
            (cons trimmed (delete trimmed efrit-agent--global-history)))
      ;; Trim global history to max size
      (when (> (length efrit-agent--global-history) efrit-agent-history-max-size)
        (setq efrit-agent--global-history
              (seq-take efrit-agent--global-history efrit-agent-history-max-size)))
      ;; Save global history to file
      (efrit-agent--save-history))))

(defun efrit-agent--reset-history-navigation ()
  "Reset history navigation state.
Called after sending input to exit history navigation mode."
  (setq efrit-agent--history-index -1)
  (setq efrit-agent--history-temp nil))

;;; History Persistence

(defun efrit-agent--save-history ()
  "Save global history to `efrit-agent-history-file'."
  (when (and efrit-agent-history-file efrit-agent--global-history)
    (condition-case err
        (with-file-modes #o600
          (with-temp-file efrit-agent-history-file
            (insert ";; Efrit Agent Input History\n")
            (insert ";; Do not edit manually\n")
            ;; Plain strings only: text properties would round-trip
            ;; through `read' and could carry keymap/display props
            (let ((print-length nil) (print-level nil))
              (pp (mapcar (lambda (h) (if (stringp h) (substring-no-properties h) h))
                          efrit-agent--global-history)
                  (current-buffer)))))
      (error
       (message "Failed to save efrit-agent history: %s" (error-message-string err))))))

(defun efrit-agent--load-history ()
  "Load global history from `efrit-agent-history-file'."
  (when (and efrit-agent-history-file
             (file-exists-p efrit-agent-history-file))
    (condition-case err
        (with-temp-buffer
          (insert-file-contents efrit-agent-history-file)
          (goto-char (point-min))
          ;; Skip comment lines
          (while (looking-at "^;")
            (forward-line 1))
          (let ((data (read (current-buffer))))
            ;; Accept only a list of strings, stripped of any properties
            ;; a tampered file might carry (keymap, display, read-only)
            (setq efrit-agent--global-history
                  (if (listp data)
                      (delq nil (mapcar (lambda (h) (and (stringp h) (substring-no-properties h)))
                                        data))
                    nil))))
      (error
       (message "Failed to load efrit-agent history: %s" (error-message-string err))
       (setq efrit-agent--global-history nil)))))

(defun efrit-agent--maybe-enable-input-mode ()
  "Enable or disable input mode based on point position.
Called from `post-command-hook'."
  (if (efrit-agent--in-input-region-p)
      (unless efrit-agent-input-mode
        (efrit-agent-input-mode 1))
    (when efrit-agent-input-mode
      (efrit-agent-input-mode -1))))

;;; Context-Aware Completion
;;
;; Provides intelligent completion in the input region based on conversation context:
;; - File paths mentioned in the conversation
;; - Options from pending questions (1, 2, 3, 4)
;; - Common response patterns

(defvar-local efrit-agent--context-files nil
  "List of file paths extracted from the conversation.
Updated when new tool calls reference files.")

(defun efrit-agent--extract-path-from-input (input)
  "Extract file path from tool INPUT if present.
INPUT can be a plist or alist with :path, path, :file_path, or file_path keys."
  (when input
    (or (plist-get input :path)
        (plist-get input :file_path)
        (and (listp input)
             (or (cdr (assoc 'path input))
                 (cdr (assoc :path input))
                 (cdr (assoc 'file_path input))
                 (cdr (assoc :file_path input)))))))

(defun efrit-agent--extract-file-paths ()
  "Extract file paths mentioned in the conversation.
Returns a list of file path strings found in tool calls."
  (let ((files nil)
        (seen-ids (make-hash-table :test 'equal)))
    ;; Scan buffer for tool-call text properties with input containing paths
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
        (let ((id (get-text-property (point) 'efrit-id))
              (type (get-text-property (point) 'efrit-type))
              (input (get-text-property (point) 'efrit-tool-input)))
          ;; Only process each unique ID once
          (when (and id (not (gethash id seen-ids)))
            (puthash id t seen-ids)
            (when (and (eq type 'tool-call) input)
              (when-let* ((path (efrit-agent--extract-path-from-input input)))
                (push path files)))))
        ;; Move to next property change (by ID to catch all tool calls)
        (goto-char (or (next-single-property-change (point) 'efrit-id)
                       (point-max)))))
    ;; Remove duplicates, return most recent first
    (delete-dups (nreverse files))))

(defun efrit-agent--get-completion-candidates ()
  "Get all completion candidates based on current context.
Returns a list of strings for completion."
  (let ((candidates nil))
    ;; Option numbers if waiting for question response
    (when (and (eq efrit-agent--status 'waiting)
               efrit-agent--pending-question)
      (let ((options (cadr efrit-agent--pending-question)))
        (when options
          ;; Add option numbers
          (dotimes (i (min 4 (length options)))
            (push (number-to-string (1+ i)) candidates))
          ;; Add actual option text
          (dolist (opt options)
            (push opt candidates)))))
    ;; File paths from conversation
    (dolist (path (efrit-agent--extract-file-paths))
      (push path candidates))
    ;; Common response patterns
    (push "yes" candidates)
    (push "no" candidates)
    (push "continue" candidates)
    (push "cancel" candidates)
    (push "skip" candidates)
    ;; Return unique candidates
    (delete-dups candidates)))

(defun efrit-agent--completion-at-point ()
  "Completion-at-point function for efrit-agent input.
Provides context-aware completions based on conversation."
  (when (efrit-agent--in-input-region-p)
    (let* ((end (point))
           (start (save-excursion
                    (skip-chars-backward "^ \t\n")
                    (point)))
           (prefix (buffer-substring-no-properties start end))
           (candidates (efrit-agent--get-completion-candidates)))
      (when (and candidates (>= (length prefix) 0))
        (list start end
              (completion-table-dynamic
               (lambda (_)
                 (cl-remove-if-not
                  (lambda (c) (string-prefix-p prefix c t))
                  candidates)))
              :exclusive 'no)))))

(defun efrit-agent--setup-completion ()
  "Set up completion for the input region."
  (add-hook 'completion-at-point-functions
            #'efrit-agent--completion-at-point
            nil t))

(provide 'efrit-agent-input)

;;; efrit-agent-input.el ends here
