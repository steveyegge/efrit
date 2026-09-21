;;; efrit-agent-integration.el --- Hook integrations for efrit-agent -*- lexical-binding: t -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; The agent buffer's subscriptions to the event bus: tool rows,
;; streamed text, thinking indicator, questions, TODOs, session
;; lifecycle, review rows.  See "Wiring" below.

;;; Code:

(require 'cl-lib)
(require 'efrit-agent-core)
(require 'efrit-agent-render)
(require 'efrit-agent-tools)
(require 'efrit-agent-input)
(require 'efrit-todo)
(require 'efrit-events)

;; TODO struct accessors and state now come from efrit-todo.el
;; Backward-compatible aliases (efrit-do-todo-item-*, efrit-do--current-todos) are provided there.
;; Declare the aliased variable to silence byte-compiler warnings
(defvar efrit-do--current-todos)

;;; Wiring
;;
;; Everything the agent buffer shows comes in through `efrit-events'
;; (`efrit-subscribe').  The loop engines, the progress layer, the
;; session and the TODO store publish at the source; nothing here
;; advises a function.  The subscribers below are installed at load,
;; once, and are idempotent, so an `efrit-reload' re-running this file
;; changes nothing (efrit-subscribe adds a function at most once).
;;
;; Event vocabulary this file consumes (see efrit-events.el):
;;
;;   session-start    :session-id :command          (efrit-do path)
;;   session-end      :session-id :success
;;   status           :session-id :status           (REPL path)
;;   thinking-start   :session-id :label
;;   thinking-stop    :session-id
;;   text-delta       :session-id :text
;;   text-end         :session-id
;;   tool-start       :session-id :tool :input [:tool-id]
;;   tool-result      :session-id :tool :result :success [:tool-id] [:elapsed]
;;   message          :session-id :text :kind
;;   error            :session-id :message [:buffer]
;;   question         :session-id :question :options
;;   question-answered :session-id :response
;;   todos-changed    :todos
;;   review-start / review-verdict / review-skipped

(defmacro efrit-agent--in-agent-buffer (&rest body)
  "Run BODY in the agent buffer when it exists; otherwise do nothing."
  (declare (indent 0))
  `(let ((buffer (get-buffer efrit-agent-buffer-name)))
     (when (buffer-live-p buffer)
       (with-current-buffer buffer
         ,@body))))

;;; TODOs

(defun efrit-agent--convert-todo-item (todo)
  "Convert an efrit-do-todo-item struct TODO to plist format for display.
Uses proper accessor functions to avoid fragility when struct changes."
  (when (vectorp todo)
    (let* ((id (efrit-do-todo-item-id todo))
           (content (efrit-do-todo-item-content todo))
           (status (efrit-do-todo-item-status todo))
           ;; Convert efrit-do status to agent status
           (agent-status (pcase status
                           ('todo 'pending)
                           ('in-progress 'in_progress)
                           ('completed 'completed)
                           (_ status))))
      (list :id id
            :content content
            :status agent-status))))

(defun efrit-agent-sync-todos ()
  "Sync TODOs from `efrit-do--current-todos' to the agent buffer."
  (when (and (bound-and-true-p efrit-do--current-todos)
             (get-buffer efrit-agent-buffer-name))
    (let ((converted-todos
           (mapcar #'efrit-agent--convert-todo-item
                   efrit-do--current-todos)))
      (efrit-agent-update-todos converted-todos))))

(defun efrit-agent--on-todos-changed (_event)
  "Subscriber: the TODO store changed."
  (efrit-agent-sync-todos))

;;; Tool rows
;;
;; A row is opened on tool-start and closed on tool-result.  The REPL
;; engine sends the API's tool_use id in :tool-id, which pairs the two
;; exactly.  The efrit-do path (via efrit-progress) has no id, so
;; there the most recent open row for the same tool name is closed,
;; as before.

(defun efrit-agent--on-tool-start (event)
  (efrit-agent--in-agent-buffer
    (efrit-agent--init-pending-tools)
    (let* ((tool-name (alist-get :tool event))
           (row (efrit-agent--add-tool-call tool-name (alist-get :input event)))
           (key (or (alist-get :tool-id event) tool-name)))
      (puthash key
               (cons (list :id row :start-time (current-time))
                     (gethash key efrit-agent--pending-tools))
               efrit-agent--pending-tools))))

(defun efrit-agent--on-tool-result (event)
  (efrit-agent--in-agent-buffer
    (efrit-agent--init-pending-tools)
    (let* ((key (or (alist-get :tool-id event) (alist-get :tool event)))
           (pending (gethash key efrit-agent--pending-tools)))
      (when pending
        (let* ((entry (car pending))
               (elapsed (or (alist-get :elapsed event)
                            (when-let* ((start (plist-get entry :start-time)))
                              (float-time (time-subtract (current-time) start))))))
          (efrit-agent--update-tool-result (plist-get entry :id)
                                           (alist-get :result event)
                                           (alist-get :success event)
                                           elapsed)
          (puthash key (cdr pending) efrit-agent--pending-tools))))))

;;; Text, thinking, messages, errors, status

(defun efrit-agent--on-text-delta (event)
  (efrit-agent--in-agent-buffer
    (efrit-agent--add-claude-message (alist-get :text event))))

(defun efrit-agent--on-text-end (_event)
  (efrit-agent--in-agent-buffer
    (efrit-agent--stream-end-message)))

(defun efrit-agent--on-thinking-start (event)
  (efrit-agent--in-agent-buffer
    (efrit-agent--show-thinking (alist-get :label event))))

(defun efrit-agent--on-thinking-stop (_event)
  (efrit-agent--in-agent-buffer
    (efrit-agent--hide-thinking)))

(defun efrit-agent--on-message (event)
  "Subscriber: a progress message (the efrit-do path).
Claude messages are rendered inline; errors with the error face."
  (efrit-agent--in-agent-buffer
    (let ((message (alist-get :text event)))
      (pcase (alist-get :kind event)
        ('claude (efrit-agent--add-claude-message (or message "")))
        ('error
         (efrit-agent--append-to-conversation
          (concat (propertize (format "%s Error: " (efrit-agent--char 'error-icon))
                              'face 'efrit-agent-error)
                  (propertize (or message "") 'face 'efrit-agent-error)
                  "\n\n")
          (list 'efrit-type 'error-message
                'efrit-id (format "err-%d" (cl-incf efrit-agent--message-counter)))))
        (_ (efrit-agent--add-claude-message (or message "")))))))

(defun efrit-agent--on-error (event)
  "Subscriber: the REPL loop failed a turn; show it in the transcript."
  (efrit-agent--in-agent-buffer
    (efrit-agent--add-error-message (alist-get :message event))))

(defun efrit-agent--on-note (event)
  "Subscriber: a one-line note from the sandbox, the limits prompt, or review."
  (efrit-agent--in-agent-buffer
    (efrit-agent--append-to-conversation
     (concat (propertize (concat "  " (alist-get :text event)) 'face (alist-get :face event)) "\n")
     (list 'efrit-type (intern (format "%s-note" (alist-get :kind event)))))))

(defun efrit-agent--on-status (event)
  (efrit-agent-set-status (alist-get :status event)))

;;; Session lifecycle (the efrit-do path)

(defun efrit-agent--on-session-start (event)
  (efrit-agent-start-session (alist-get :session-id event) (alist-get :command event))
  (setq efrit-agent--activity-counter 0))

(defun efrit-agent--on-session-end (event)
  (efrit-agent-end-session (alist-get :success event)))

;;; Questions

(defun efrit-agent--on-question (event)
  "Subscriber: the model asked the user something; show it and wait."
  (efrit-agent--in-agent-buffer
    (let* ((question (alist-get :question event))
           (options (alist-get :options event))
           (opts (when options (if (listp options) options (append options nil)))))
      (setq efrit-agent--pending-question
            (list question opts (format-time-string "%Y-%m-%dT%H:%M:%S%z")))
      (setq efrit-agent--status 'waiting)
      (efrit-agent--add-question question opts))))

(defun efrit-agent--on-question-answered (event)
  (efrit-agent--in-agent-buffer
    (when-let* ((response (alist-get :response event)))
      (efrit-agent--add-user-message (format "%s" response)))
    (setq efrit-agent--pending-question nil)
    (setq efrit-agent--status 'working)
    (efrit-agent--reset-input-prompt)))

;;; Subscriptions

(defconst efrit-agent--subscriptions
  '((todos-changed . efrit-agent--on-todos-changed)
    (tool-start . efrit-agent--on-tool-start)
    (tool-result . efrit-agent--on-tool-result)
    (text-delta . efrit-agent--on-text-delta)
    (text-end . efrit-agent--on-text-end)
    (thinking-start . efrit-agent--on-thinking-start)
    (thinking-stop . efrit-agent--on-thinking-stop)
    (message . efrit-agent--on-message)
    (error . efrit-agent--on-error)
    (status . efrit-agent--on-status)
    (note . efrit-agent--on-note)
    (session-start . efrit-agent--on-session-start)
    (session-end . efrit-agent--on-session-end)
    (question . efrit-agent--on-question)
    (question-answered . efrit-agent--on-question-answered)
    (review-start . efrit-agent--on-review-start)
    (review-verdict . efrit-agent--on-review-verdict)
    (review-skipped . efrit-agent--on-review-skipped))
  "What the agent buffer listens to: (EVENT . HANDLER).")

(defun efrit-agent-setup-integration ()
  "Subscribe the agent buffer to the events it renders.  Idempotent."
  (dolist (sub efrit-agent--subscriptions)
    (efrit-subscribe (car sub) (cdr sub))))

(defun efrit-agent-remove-integration ()
  "Unsubscribe the agent buffer from every event."
  (dolist (sub efrit-agent--subscriptions)
    (efrit-unsubscribe (car sub) (cdr sub))))

;;; Public API wrappers (for use by other modules)

(declare-function efrit-agent--add-todos-inline "efrit-agent-render")
(declare-function efrit-agent-start-session "efrit-agent")
(declare-function efrit-agent-end-session "efrit-agent")
(declare-function efrit-agent-set-status "efrit-agent")

(defun efrit-agent-update-todos (todos)
  "Update the TODO list display with TODOS.
TODOS should be a list of plists with :status, :content, :activeForm.
Uses incremental inline update instead of full re-render."
  (let ((buffer (get-buffer efrit-agent-buffer-name)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq efrit-agent--todos todos)
        ;; Use incremental inline update instead of full re-render
        (efrit-agent--add-todos-inline todos)))))

;; efrit-agent-start-session and efrit-agent-end-session are defined in
;; efrit-agent.el. Duplicate wrapper versions here were always shadowed
;; (efrit-agent.el requires this file before defining its own) and were
;; removed (ef-d89).

;;; Review rows
;;
;; The second-model review (efrit-review) publishes review-start and
;; review-verdict.  Show it as a tool-style row so the user sees that
;; the turn was judged, by which model, and the outcome -- approvals
;; included, not only rejections.  The row expands (TAB / d) to what
;; the reviewer was shown and what it answered.

(defvar-local efrit-agent--review-row nil
  "Tool-row id of the review in flight, or nil.")

(defun efrit-agent--on-review-start (event)
  (let ((buffer (get-buffer efrit-agent-buffer-name)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (let ((input (make-hash-table :test 'equal)))
          (puthash "calls" (alist-get :calls event) input)
          (puthash "model" (alist-get :model event) input)
          (puthash "shown to reviewer" (alist-get :prompt event) input)
          (setq efrit-agent--review-row
                (efrit-agent--add-tool-call "review" input)))))))

(defun efrit-agent--on-review-verdict (event)
  (let ((buffer (get-buffer efrit-agent-buffer-name)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when efrit-agent--review-row
          (let* ((verdict (alist-get :verdict event))
                 (reason (alist-get :reason event))
                 (text (pcase verdict
                         ('approve "approved")
                         ('reject (concat "rejected · " (or reason "no reason given")))
                         (_ (format "%s" verdict)))))
            (efrit-agent--update-tool-result efrit-agent--review-row text
                                             (eq verdict 'approve) nil)
            (setq efrit-agent--review-row nil)))))))

(defcustom efrit-agent-show-review-skips t
  "When non-nil, a turn that needs no review gets a one-line dim note.
Reads and efrit's own control tools are not reviewed; the note says
so, for example: not reviewed: read-only turn (fetch_url)."
  :type 'boolean
  :group 'efrit-agent)

(defun efrit-agent--on-review-skipped (event)
  (when efrit-agent-show-review-skips
    (efrit-agent--in-agent-buffer
      (efrit-agent--append-to-conversation
       (concat (propertize (format "  ⚖ not reviewed: %s" (alist-get :reason event))
                           'face 'efrit-agent-timestamp)
               "\n")
       (list 'efrit-type 'review-note)))))

;; Subscribed at load, not in eval-after-load: efrit-events is already
;; required above, and an eval-after-load body would run again on
;; every efrit-reload.  efrit-agent.el calls this too; both are safe.
(efrit-agent-setup-integration)

(provide 'efrit-agent-integration)

;;; efrit-agent-integration.el ends here
