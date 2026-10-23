;;; efrit-agent-ring.el --- Next, previous, and who needs you among the agent buffers -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.12.12
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, ai

;;; Commentary:

;; With several agent buffers the question is "which one, next?".
;; Two models answered it well (tzz, 2026-10-14): the Claude desktop
;; app, where chats are an ordered list and next/previous are one key
;; each, and M-x Machina, where `]' goes to the agent that needs you
;; (an open prompt first, then an unread reply) and a reply counts as
;; read only after it was on screen for a few continuous seconds.
;;
;; The RING is the agent buffers in creation order (not `buffer-list'
;; recency, which changes under you).  `efrit-agent-next' and
;; `efrit-agent-previous' walk it in place: the window showing the
;; current agent shows the next one, so the layout stays.  `C-c 1'..
;; `C-c 9' jump by position.  `efrit-agent-next-attention' walks only
;; the buffers that WAIT (a modal prompt is open: sandbox, question,
;; reconnect) or are UNREAD (a turn ended while the user was not
;; looking at the buffer); waiting ones first.
;;
;; Unread clears by DWELL: `efrit-agent-read-delay' continuous seconds
;; with the buffer in the selected window, the frame focused, and the
;; window showing the end of the transcript.  A switch, a scroll
;; away, a focus loss or new output restarts the count.  No timer of
;; its own: the check runs from `post-command-hook' and the
;; window/focus change hooks, and the dwell clock is wall time
;; between two checks of the same view (a gap over a second is
;; sleep, not reading).
;;
;; Faces: the mode line shows a dot in unread buffers and a question
;; mark in waiting ones; `efrit-agent-ring-summary' is a one-line
;; count for `global-mode-string' and the dashboard.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'efrit-events)
(require 'efrit-pill)
(defvar efrit-pill-e-alert-color)

(declare-function efrit-agent-buffers "efrit-agent-core")
(declare-function efrit-agent-display "efrit-agent-core")
(declare-function efrit-agent-buffer-for "efrit-agent-core")
(declare-function efrit-repl-session-get "efrit-repl-session")
(declare-function efrit-repl-session-buffer "efrit-repl-session")
(declare-function efrit-repl-session-status "efrit-repl-session")
(defvar efrit-agent--repl-session)
(defvar efrit-agent--input-start)

(defgroup efrit-agent-ring nil
  "Moving between agent buffers; unread and waiting marks."
  :group 'efrit)

(defcustom efrit-agent-read-delay 5
  "Continuous seconds a reply must be on screen before it counts as read.
0: the first look counts."
  :type 'number
  :group 'efrit-agent-ring)

(defcustom efrit-agent-ring-mode-line t
  "When non-nil, unread and waiting marks show in the agent buffers' mode lines."
  :type 'boolean
  :group 'efrit-agent-ring)

(defface efrit-agent-unread '((t :inherit font-lock-constant-face :weight bold))
  "The mark of an agent buffer with a reply the user has not read."
  :group 'efrit-agent-ring)

(defface efrit-agent-waiting '((t :inherit warning :weight bold))
  "The mark of an agent buffer whose prompt waits for the user."
  :group 'efrit-agent-ring)

;;;; State

(defvar-local efrit-agent--created-at nil
  "When this agent buffer was first seen by the ring; orders it.")

(defvar-local efrit-agent--unread nil
  "Non-nil when a turn ended here while the user was not looking.")

(defvar-local efrit-agent--waiting nil
  "The label of the modal prompt open here, or nil.")

(defvar efrit-agent-ring--counter 0)

(defvar efrit-agent-ring--dwell nil
  "(WINDOW BUFFER FIRST-SEEN LAST-SEEN) of the view being read, or nil.")

(defun efrit-agent-ring--stamp (buffer)
  (with-current-buffer buffer
    (unless efrit-agent--created-at
      (setq efrit-agent--created-at (cl-incf efrit-agent-ring--counter)))
    efrit-agent--created-at))

(defun efrit-agent-ring ()
  "The agent buffers in creation order."
  (sort (copy-sequence (efrit-agent-buffers))
        (lambda (a b) (< (efrit-agent-ring--stamp a) (efrit-agent-ring--stamp b)))))

(defun efrit-agent-ring--waiting-p (buffer)
  (buffer-local-value 'efrit-agent--waiting buffer))

(defun efrit-agent-ring--unread-p (buffer)
  (buffer-local-value 'efrit-agent--unread buffer))

(defun efrit-agent-ring-attention-queue (&optional buffers)
  "BUFFERS (default the ring) that need the user: waiting first, then unread."
  (let ((ring (or buffers (efrit-agent-ring))))
    (append (seq-filter #'efrit-agent-ring--waiting-p ring)
            (seq-filter (lambda (b) (and (efrit-agent-ring--unread-p b)
                                         (not (efrit-agent-ring--waiting-p b))))
                        ring))))

(defun efrit-agent-ring--target (queue current previous)
  "The entry of QUEUE after CURRENT (before it with PREVIOUS), wrapping.
CURRENT absent: the first entry."
  (when queue
    (let ((index (cl-position current queue)))
      (nth (if index (mod (+ index (if previous -1 1)) (length queue)) 0) queue))))

(defun efrit-agent-ring--current ()
  "The agent buffer the command is about: the current one, or the one at point in a list."
  (cond
   ((derived-mode-p 'efrit-agent-mode) (current-buffer))
   ((and (derived-mode-p 'tabulated-list-mode) (bufferp (tabulated-list-get-id))) (tabulated-list-get-id))
   (t (cl-find-if (lambda (w) (with-current-buffer (window-buffer w) (derived-mode-p 'efrit-agent-mode)))
                  (window-list nil 'no-minibuffer)))))

(defun efrit-agent-ring--show (buffer)
  "Show BUFFER where the current agent is, else per the display action."
  (let ((here (and (derived-mode-p 'efrit-agent-mode) (selected-window))))
    (if (and here (not (eq (window-buffer here) buffer)))
        (progn (set-window-dedicated-p here nil)
               (set-window-buffer here buffer)
               (select-window here)
               (with-current-buffer buffer
                 (when efrit-agent--input-start (goto-char (point-max)))))
      (efrit-agent-display buffer t)))
  (efrit-agent-ring--reset-dwell)
  buffer)

;;;; Commands

(defun efrit-agent-ring--step (previous)
  (let* ((ring (efrit-agent-ring))
         (current (efrit-agent-ring--current))
         (target (efrit-agent-ring--target ring (and (memq current ring) current) previous)))
    (unless target (user-error "No agent buffers"))
    (when (eq target current)
      (message "Only one agent buffer"))
    (efrit-agent-ring--show target)
    (message "%d/%d %s%s" (1+ (cl-position target ring)) (length ring) (buffer-name target)
             (efrit-agent-ring--marks target))))

;;;###autoload
(defun efrit-agent-next ()
  "Show the next agent buffer in creation order, in this window."
  (interactive)
  (efrit-agent-ring--step nil))

;;;###autoload
(defun efrit-agent-previous ()
  "Show the previous agent buffer in creation order, in this window."
  (interactive)
  (efrit-agent-ring--step t))

(defun efrit-agent-ring--attention-step (previous)
  (let* ((queue (efrit-agent-ring-attention-queue))
         (current (efrit-agent-ring--current))
         (target (efrit-agent-ring--target queue (and (memq current queue) current) previous)))
    (if (not target)
        (message "No agent is waiting for you, nothing unread")
      (efrit-agent-ring--show target)
      (message "Attention %d/%d: %s %s" (1+ (cl-position target queue)) (length queue)
               (buffer-name target)
               (if (efrit-agent-ring--waiting-p target)
                   (format "is waiting (%s)" (efrit-agent-ring--waiting-p target))
                 "has an unread reply")))))

;;;###autoload
(defun efrit-agent-next-attention ()
  "Show the next agent buffer that needs you: an open prompt first, else an unread reply."
  (interactive)
  (efrit-agent-ring--attention-step nil))

;;;###autoload
(defun efrit-agent-previous-attention ()
  "Show the previous agent buffer that needs you."
  (interactive)
  (efrit-agent-ring--attention-step t))

;;;###autoload
(defun efrit-agent-switch-to-number (n)
  "Show the Nth agent buffer in creation order (1-based)."
  (interactive "p")
  (let ((ring (efrit-agent-ring)))
    (unless (<= 1 n (length ring))
      (user-error "There are %d agent buffer%s" (length ring) (if (= 1 (length ring)) "" "s")))
    (efrit-agent-ring--show (nth (1- n) ring))))

(defmacro efrit-agent-ring--define-switch-commands (count)
  "Define `efrit-agent-switch-to-1' .. COUNT as top-level commands."
  `(progn
     ,@(cl-loop for n from 1 to count
                collect `(defun ,(intern (format "efrit-agent-switch-to-%d" n)) ()
                           ,(format "Show agent buffer %d." n)
                           (interactive)
                           (efrit-agent-switch-to-number ,n)))))

(efrit-agent-ring--define-switch-commands 9)

(defvar-local efrit-agent--unread-pinned nil
  "Non-nil when the user set the unread mark by hand: the first dwell
in the same visit must not clear it.  Cleared when the buffer leaves view.")

(defun efrit-agent-mark-unread (&optional buffer)
  "Mark BUFFER (default the current agent buffer) unread, to come back to it.
It joins the attention queue after the waiting ones; the dwell clock
does not clear it until you leave and look again."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (unless (derived-mode-p 'efrit-agent-mode) (user-error "Not an agent buffer"))
    (setq efrit-agent--unread t)
    (setq efrit-agent--unread-pinned t)
    (efrit-agent-ring--reset-dwell)
    (force-mode-line-update t)
    (efrit-publish 'attention-changed (list (cons :buffer (current-buffer))))
    (message "%s marked unread; C-c } comes back to it" (buffer-name))))

(defun efrit-agent-toggle-unread (&optional buffer)
  "Mark BUFFER unread, or read when it already is unread."
  (interactive)
  (if (buffer-local-value 'efrit-agent--unread (or buffer (current-buffer)))
      (progn (efrit-agent-mark-read buffer) (message "%s marked read" (buffer-name (or buffer (current-buffer)))))
    (efrit-agent-mark-unread buffer)))

(defun efrit-agent-mark-read (&optional buffer)
  "Clear the unread mark of BUFFER (default the current agent buffer)."
  (interactive)
  (with-current-buffer (or buffer (current-buffer))
    (setq efrit-agent--unread-pinned nil)
    (when efrit-agent--unread
      (setq efrit-agent--unread nil)
      (force-mode-line-update t)
      (efrit-publish 'attention-changed (list (cons :buffer (current-buffer)))))))

;;;; Marks

(defun efrit-agent-ring--marks (buffer)
  (concat (if (efrit-agent-ring--waiting-p buffer) " ?" "")
          (if (efrit-agent-ring--unread-p buffer) " •" "")))

(declare-function efrit-agent-spinner-frame "efrit-agent-spinner")
(declare-function efrit-agent-spinner-available-p "efrit-agent-spinner")

(defun efrit-agent-attention-state (buffer)
  "BUFFER's state: `waiting', `unread', `working', `failed' or `idle'."
  (with-current-buffer buffer
    (cond
     (efrit-agent--waiting 'waiting)
     (efrit-agent--unread 'unread)
     ((and efrit-agent--repl-session (eq (efrit-repl-session-status efrit-agent--repl-session) 'working)) 'working)
     ((and efrit-agent--repl-session (eq (efrit-repl-session-status efrit-agent--repl-session) 'failed)) 'failed)
     (t 'idle))))

(defun efrit-agent-attention-dot (buffer &optional text)
  "BUFFER's attention mark: a disc inside a pill, red when it needs you.
TEXT (default the state) is the pill text; \"\" gives the disc alone."
  (let* ((state (efrit-agent-attention-state buffer))
         (needs (memq state '(waiting unread))))
    (efrit-pill (or text (symbol-name state)) :state state
                :dot (if needs 'filled 'ring)
                :strong (and needs (eq state 'waiting))
                :help-echo (pcase state
                             ('waiting (format "%s is waiting for you: %s" (buffer-name buffer)
                                               (buffer-local-value 'efrit-agent--waiting buffer)))
                             ('unread (format "%s has a reply you have not read (C-c u toggles)" (buffer-name buffer)))
                             ('working (format "%s is working" (buffer-name buffer)))
                             (_ (format "%s: nothing needs you (%s)" (buffer-name buffer) state))))))

(declare-function efrit-agent-spinner-still "efrit-agent-spinner")
(defvar efrit-agent--spinner-index)
(defvar efrit-agent--thinking-label)

(defun efrit-agent-logo (&optional color)
  "The efrit e, whole and still, as a mode-line image in COLOR; nil without SVG."
  (when (and (require 'efrit-agent-spinner nil t) (efrit-agent-spinner-available-p))
    (efrit-agent-spinner-still (+ 2 (frame-char-height)) color 'mode-line)))

(declare-function efrit-agent-menu "efrit-agent-menu")
(declare-function efrit-agent-new-session "efrit-agent")
(declare-function efrit-agent-new-instance "efrit-agent-instances")
(declare-function efrit-agent-new-worktree-instance "efrit-agent-instances")
(declare-function efrit-agent-cancel "efrit-agent")
(declare-function efrit-agent-dashboard "efrit-agent-dashboard")
(declare-function efrit-agent-focus "efrit-agent-dashboard")
(declare-function efrit-agent-diagnose "efrit-agent-diagnose")
(declare-function efrit-code-review "efrit-code-review-ui")
(declare-function efrit-resume "efrit-agent")
(declare-function efrit-menu "efrit-menu")

(defvar efrit-agent-logo-menu
  (let ((menu (make-sparse-keymap "efrit")))
    (define-key menu [dashboard] '(menu-item "Agents dashboard" efrit-agent-dashboard))
    (define-key menu [next-attention] '(menu-item "Next agent that needs me" efrit-agent-next-attention))
    (define-key menu [sep1] '(menu-item "--"))
    (define-key menu [new-session] '(menu-item "New conversation here" efrit-agent-new-session))
    (define-key menu [new-instance] '(menu-item "New agent for this project" efrit-agent-new-instance))
    (define-key menu [new-worktree] '(menu-item "New agent in a fresh worktree" efrit-agent-new-worktree-instance))
    (define-key menu [resume] '(menu-item "Resume a saved session" efrit-resume))
    (define-key menu [sep2] '(menu-item "--"))
    (define-key menu [toggle-unread] '(menu-item "Mark unread / read" efrit-agent-toggle-unread))
    (define-key menu [focus] '(menu-item "Focus this agent (again: restore)" efrit-agent-focus))
    (define-key menu [cancel] '(menu-item "Cancel the running turn" efrit-agent-cancel))
    (define-key menu [sep3] '(menu-item "--"))
    (define-key menu [review] '(menu-item "Code review of the staged changes" efrit-code-review))
    (define-key menu [diagnose] '(menu-item "Diagnose this session" efrit-agent-diagnose))
    (define-key menu [agent-menu] '(menu-item "The buffer's menu (C-c ?)" efrit-agent-menu))
    (define-key menu [efrit-menu] '(menu-item "efrit settings menu" efrit-menu))
    menu)
  "The popup menu on the mode-line logo.")

(defun efrit-agent-logo-popup (event)
  "Pop up `efrit-agent-logo-menu' at mouse EVENT, in the event's buffer."
  (interactive "e")
  (with-selected-window (posn-window (event-start event))
    (popup-menu efrit-agent-logo-menu event)))

(defun efrit-agent-logo-mouse-1 (event)
  "mouse-1 on the logo: the agent buffer's transient menu, else the dashboard."
  (interactive "e")
  (with-selected-window (posn-window (event-start event))
    (if (derived-mode-p 'efrit-agent-mode)
        (progn (require 'efrit-agent-menu) (call-interactively #'efrit-agent-menu))
      (efrit-agent-dashboard))))

(defconst efrit-agent-logo-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mode-line mouse-1] #'efrit-agent-logo-mouse-1)
    (define-key map [mode-line mouse-2] #'efrit-agent-dashboard)
    (define-key map [mode-line down-mouse-3] #'efrit-agent-logo-popup)
    map)
  "Mouse bindings of the mode-line logo.")

(defun efrit-agent-logo-clickable (image help)
  "IMAGE (a propertized string) with the logo's mouse bindings and HELP as tooltip."
  (propertize image
              'help-echo (concat help "\nmouse-1: this agent's menu · mouse-2: dashboard · mouse-3: actions")
              'mouse-face 'mode-line-highlight
              'local-map efrit-agent-logo-map))

(defun efrit-agent-ring-mode-line-string ()
  "The efrit logo for this agent's mode line.
Animated (the highlight running along the e) while a request is in
flight, the spinner timer redrawing mode lines on each tick; the
whole e at rest otherwise; red when a prompt waits or a reply is
unread.  Nil without a graphic display: the dot pill then (tzz,
2026-10-14: \"animated when there's action. Otherwise it should be
the full e\")."
  (when (and efrit-agent-ring-mode-line (derived-mode-p 'efrit-agent-mode))
    (let* ((state (efrit-agent-attention-state (current-buffer)))
           (needs (memq state '(waiting unread)))
           (color (and needs efrit-pill-e-alert-color))
           (help (pcase state
                   ('waiting (format "efrit is waiting for you: %s" efrit-agent--waiting))
                   ('unread "a reply you have not read (C-c u toggles)")
                   ('working "efrit is working")
                   (_ (format "efrit: %s" state)))))
      (if-let* ((frame (and (require 'efrit-agent-spinner nil t) (efrit-agent-spinner-available-p)
                            (if efrit-agent--thinking-label
                                (efrit-agent-spinner-frame efrit-agent--spinner-index (+ 2 (frame-char-height))
                                                           color 'mode-line)
                              (efrit-agent-logo color)))))
          (efrit-agent-logo-clickable frame help)
        (efrit-agent-logo-clickable (efrit-agent-attention-dot (current-buffer) "") help)))))

(defun efrit-agent-ring-summary ()
  "One line: how many agents work, wait, are unread, are idle."
  (let ((ring (efrit-agent-ring)) (working 0) (waiting 0) (unread 0) (idle 0))
    (dolist (b ring)
      (with-current-buffer b
        (cond
         (efrit-agent--waiting (cl-incf waiting))
         ((and efrit-agent--repl-session
               (eq (efrit-repl-session-status efrit-agent--repl-session) 'working))
          (cl-incf working))
         (t (cl-incf idle)))
        (when efrit-agent--unread (cl-incf unread))))
    (when ring
      (string-join
       (delq nil (list (and (> working 0) (efrit-pill (format "%d working" working) :state 'working))
                       (and (> waiting 0) (efrit-pill (format "%d waiting" waiting) :state 'waiting :strong t))
                       (and (> unread 0) (efrit-pill (format "%d unread" unread) :state 'unread))
                       (and (> idle 0) (efrit-pill (format "%d idle" idle) :state 'idle))))
       " "))))

;;;; Events

(defun efrit-agent-ring--buffer-of (event)
  (when-let* ((id (alist-get :session-id event)))
    (or (and (fboundp 'efrit-agent-buffer-for) (efrit-agent-buffer-for id))
        (when-let* ((s (and (fboundp 'efrit-repl-session-get) (efrit-repl-session-get id))))
          (efrit-repl-session-buffer s)))))

(defun efrit-agent-ring--viewing-p (buffer)
  "Non-nil when BUFFER is in the selected window of a focused frame, showing its end."
  (and (buffer-live-p buffer)
       (eq buffer (window-buffer (selected-window)))
       (frame-focus-state)
       (let ((end (with-current-buffer buffer
                    (or efrit-agent--input-start (point-max)))))
         (>= (window-end (selected-window) t) (min end (with-current-buffer buffer (point-max)))))))

(defun efrit-agent-ring--on-turn-complete (event)
  (when-let* ((buf (efrit-agent-ring--buffer-of event)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (efrit-agent-ring--stamp buf)
        (unless (and (efrit-agent-ring--viewing-p buf) (<= efrit-agent-read-delay 0))
          (setq efrit-agent--unread t)
          (efrit-agent-ring--reset-dwell)
          (force-mode-line-update t)
          (efrit-publish 'attention-changed (list (cons :buffer buf))))))))

(defun efrit-agent-ring--on-prompt-open (event)
  (when-let* ((buf (efrit-agent-ring--buffer-of event)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (setq efrit-agent--waiting (or (alist-get :label event) "a prompt"))
        (force-mode-line-update t)
        (efrit-publish 'attention-changed (list (cons :buffer buf)))))))

(defun efrit-agent-ring--on-prompt-closed (event)
  (when-let* ((buf (efrit-agent-ring--buffer-of event)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (setq efrit-agent--waiting nil)
        (force-mode-line-update t)
        (efrit-publish 'attention-changed (list (cons :buffer buf)))))))

;;;; Dwell

(defun efrit-agent-ring--reset-dwell ()
  (setq efrit-agent-ring--dwell nil))

(cl-defun efrit-agent-ring-check-dwell (&optional now)
  "Count reading time for the selected window; mark read after `efrit-agent-read-delay'.
NOW (a float time) is for tests."
  (let* ((buf (window-buffer (selected-window)))
         (now (or now (float-time))))
    (if (not (and (buffer-local-value 'efrit-agent--unread buf)
                  (efrit-agent-ring--viewing-p buf)))
        (progn
          ;; once the pinned buffer is out of view, the next look may clear it
          (dolist (b (efrit-agent-buffers))
            (when (and (buffer-local-value 'efrit-agent--unread-pinned b) (not (eq b buf)))
              (with-current-buffer b (setq efrit-agent--unread-pinned nil))))
          (efrit-agent-ring--reset-dwell))
      (when (buffer-local-value 'efrit-agent--unread-pinned buf)
        (efrit-agent-ring--reset-dwell)
        (cl-return-from efrit-agent-ring-check-dwell nil))
      (let ((view (list (selected-window) buf)))
        ;; the heartbeat ticks once a second, a little late; a gap must
        ;; be clearly more than that to count as sleep rather than
        ;; reading (2026-10-14: at 1.0 every tick restarted the clock
        ;; and a buffer stayed unread for hours)
        (when (or (not (equal view (seq-take efrit-agent-ring--dwell 2)))
                  (> (- now (nth 3 efrit-agent-ring--dwell)) 2.5)
                  (< now (nth 3 efrit-agent-ring--dwell)))
          (setq efrit-agent-ring--dwell (list (selected-window) buf now now)))
        (setf (nth 3 efrit-agent-ring--dwell) now)
        (when (>= (- now (nth 2 efrit-agent-ring--dwell)) (max 0 efrit-agent-read-delay))
          (efrit-agent-ring--reset-dwell)
          (efrit-agent-mark-read buf))))))

(defvar efrit-agent-ring--heartbeat nil)

(defun efrit-agent-ring--view-changed (&rest _)
  "Advance the dwell clock when the view changed.  On `post-command-hook'
for every command in Emacs, so it must cost nothing when there is
nothing to do: no agent buffer is unread, or the selected buffer is
not an agent one and nothing is pinned (2026-10-16: a buffer-list walk
plus `window-end' per keystroke made typing stutter)."
  (when (or (buffer-local-value 'efrit-agent--unread (window-buffer (selected-window)))
            efrit-agent-ring--dwell
            (and efrit-agent-ring--heartbeat
                 (cl-some (lambda (b) (buffer-local-value 'efrit-agent--unread-pinned b))
                          (efrit-agent-buffers))))
    (efrit-agent-ring-check-dwell)))

(defun efrit-agent-ring--heartbeat-tick ()
  "Advance the dwell clock while an unread buffer is on screen and the user is still."
  (if (cl-some #'efrit-agent-ring--unread-p (efrit-agent-buffers))
      (efrit-agent-ring-check-dwell)
    (efrit-agent-ring--stop-heartbeat)))

(defun efrit-agent-ring--start-heartbeat ()
  (unless efrit-agent-ring--heartbeat
    (setq efrit-agent-ring--heartbeat (run-with-timer 1 1 #'efrit-agent-ring--heartbeat-tick))))

(defun efrit-agent-ring--stop-heartbeat ()
  (when efrit-agent-ring--heartbeat
    (cancel-timer efrit-agent-ring--heartbeat)
    (setq efrit-agent-ring--heartbeat nil)))

(defun efrit-agent-ring--on-attention-changed (event)
  (when (buffer-local-value 'efrit-agent--unread (alist-get :buffer event))
    (efrit-agent-ring--start-heartbeat)))

;;;; Install

(defun efrit-agent-ring--stamp-current ()
  (efrit-agent-ring--stamp (current-buffer)))

(defun efrit-agent-ring-apply-mode-name ()
  "Make every live agent buffer show the logo as its mode name.
`efrit-agent-mode' sets it for new buffers; this is for buffers that
exist across a reload (tzz, 2026-10-14: \"the text is still there\")."
  ;; `efrit-agent-buffers' is in efrit-agent-core, which requires this
  ;; file; before core loads there are no agent buffers to retitle
  (when (fboundp 'efrit-agent-buffers)
    (dolist (b (efrit-agent-buffers))
      (with-current-buffer b
        (setq mode-name '(:eval (or (efrit-agent-ring-mode-line-string) "Efrit-Agent")))
        (setq mode-line-process nil)))
    (force-mode-line-update t)))

(defun efrit-agent-ring-install ()
  "Subscribe to the events and hooks the marks depend on."
  (efrit-agent-ring-apply-mode-name)
  (add-hook 'efrit-agent-mode-hook #'efrit-agent-ring--stamp-current)
  (efrit-subscribe 'turn-complete #'efrit-agent-ring--on-turn-complete)
  (efrit-subscribe 'prompt-open #'efrit-agent-ring--on-prompt-open)
  (efrit-subscribe 'prompt-closed #'efrit-agent-ring--on-prompt-closed)
  (efrit-subscribe 'attention-changed #'efrit-agent-ring--on-attention-changed)
  (add-hook 'post-command-hook #'efrit-agent-ring--view-changed)
  (add-hook 'window-selection-change-functions #'efrit-agent-ring--view-changed)
  (add-hook 'window-buffer-change-functions #'efrit-agent-ring--view-changed)
  (add-function :after after-focus-change-function #'efrit-agent-ring--view-changed))

(defun efrit-agent-ring-uninstall ()
  "Undo `efrit-agent-ring-install'."
  (remove-hook 'efrit-agent-mode-hook #'efrit-agent-ring--stamp-current)
  (efrit-unsubscribe 'turn-complete #'efrit-agent-ring--on-turn-complete)
  (efrit-unsubscribe 'prompt-open #'efrit-agent-ring--on-prompt-open)
  (efrit-unsubscribe 'prompt-closed #'efrit-agent-ring--on-prompt-closed)
  (efrit-unsubscribe 'attention-changed #'efrit-agent-ring--on-attention-changed)
  (remove-hook 'post-command-hook #'efrit-agent-ring--view-changed)
  (remove-hook 'window-selection-change-functions #'efrit-agent-ring--view-changed)
  (remove-hook 'window-buffer-change-functions #'efrit-agent-ring--view-changed)
  (remove-function after-focus-change-function #'efrit-agent-ring--view-changed)
  (efrit-agent-ring--stop-heartbeat))

(efrit-agent-ring-install)

(provide 'efrit-agent-ring)

;;; efrit-agent-ring.el ends here
