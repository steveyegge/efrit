;;; efrit-agent-integration.el --- Hook integrations for efrit-agent -*- lexical-binding: t -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.4.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; Integration module for efrit-agent providing:
;; - TODO tracking integration with efrit-do
;; - Activity tracking integration with efrit-progress
;; - Session lifecycle integration
;; - User input integration

;;; Code:

(require 'cl-lib)
(require 'efrit-agent-core)
(require 'efrit-agent-render)
(require 'efrit-agent-tools)
(require 'efrit-agent-input)

;; Forward declarations to silence byte-compiler
(defvar efrit-do--current-todos)
(declare-function efrit-do-todo-item-id "efrit-do")
(declare-function efrit-do-todo-item-content "efrit-do")
(declare-function efrit-do-todo-item-status "efrit-do")

;;; Integration with efrit-do TODO tracking

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

(defun efrit-agent--on-todo-update (&rest _args)
  "Advice function called when TODOs are updated in efrit-do.
Syncs the updated TODOs to the agent buffer."
  (efrit-agent-sync-todos))

(defun efrit-agent-setup-todo-integration ()
  "Set up advice to sync TODOs from efrit-do to agent buffer.
Call this after loading efrit-do to enable real-time TODO updates."
  (when (fboundp 'efrit-do--update-todo-status)
    (advice-add 'efrit-do--update-todo-status :after #'efrit-agent--on-todo-update))
  ;; Also hook into todo_write tool handler for batch updates
  (when (fboundp 'efrit-do--handle-todo-write)
    (advice-add 'efrit-do--handle-todo-write :after #'efrit-agent--on-todo-update)))

(defun efrit-agent-remove-todo-integration ()
  "Remove advice for TODO syncing."
  (advice-remove 'efrit-do--update-todo-status #'efrit-agent--on-todo-update)
  (advice-remove 'efrit-do--handle-todo-write #'efrit-agent--on-todo-update))

;;; Integration with efrit-progress activity tracking
;;
;; These hooks connect efrit-progress events to the conversation-first display.
;; Tools are tracked in efrit-agent--pending-tools hash table to match results.

(defun efrit-agent--on-tool-start (tool-name input)
  "Advice function called when a tool starts.
TOOL-NAME is the name of the tool being called.
INPUT is the tool's input parameters (preserved for expanded view).
Uses incremental conversation update instead of activity list."
  (let ((buffer (get-buffer efrit-agent-buffer-name)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (efrit-agent--init-pending-tools)
        ;; Add tool call to conversation and get the tool-id
        (let ((tool-id (efrit-agent--add-tool-call tool-name input)))
          ;; Track the tool-id for later result matching
          ;; Store: tool-id, start-time, and input for potential retry
          ;; Push to front of list (most recent first)
          (let ((existing (gethash tool-name efrit-agent--pending-tools)))
            (puthash tool-name
                     (cons (list :id tool-id
                                 :start-time (current-time)
                                 :input input)
                           existing)
                     efrit-agent--pending-tools)))))))

(defun efrit-agent--on-tool-result (tool-name result success-p)
  "Advice function called when a tool completes.
Updates the most recently started (not yet completed) tool call for TOOL-NAME.
Uses in-place update instead of full re-render.
When a tool fails, stores its context for potential retry."
  (let ((buffer (get-buffer efrit-agent-buffer-name)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (efrit-agent--init-pending-tools)
        ;; Get the first pending tool entry for this tool-name
        (let ((pending-list (gethash tool-name efrit-agent--pending-tools)))
          (when pending-list
            (let* ((entry (car pending-list))
                   (tool-id (plist-get entry :id))
                   (start-time (plist-get entry :start-time))
                   (tool-input (plist-get entry :input))
                   (elapsed (when start-time
                              (float-time (time-subtract (current-time) start-time)))))
              ;; Update the tool in-place
              (efrit-agent--update-tool-result tool-id result success-p elapsed)
              ;; If tool failed, store context for retry
              (when (not success-p)
                (let ((tool-item (make-hash-table :test 'equal)))
                  (puthash "name" tool-name tool-item)
                  (puthash "input" tool-input tool-item)
                  (efrit-agent--store-failed-tool tool-id tool-name tool-input tool-item)))
              ;; Remove from pending list
              (puthash tool-name (cdr pending-list) efrit-agent--pending-tools))))))))

(defun efrit-agent--on-message (message &optional type)
  "Advice function called when a message is shown.
MESSAGE is the text, TYPE indicates the message type.
Claude messages are rendered inline in the conversation.
Error messages are rendered with error face."
  (let ((buffer (get-buffer efrit-agent-buffer-name)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (pcase type
          ('claude
           ;; Claude messages go directly to conversation
           (efrit-agent--add-claude-message (or message "")))
          ('error
           ;; Error messages with special formatting
           (efrit-agent--append-to-conversation
            (concat (propertize (format "%s Error: " (efrit-agent--char 'error-icon))
                                'face 'efrit-agent-error)
                    (propertize (or message "") 'face 'efrit-agent-error)
                    "\n\n")
            (list 'efrit-type 'error-message
                  'efrit-id (format "err-%d" (cl-incf efrit-agent--message-counter)))))
          (_
           ;; Other messages (system, info) - also to conversation
           (efrit-agent--add-claude-message (or message ""))))))))

(defun efrit-agent-setup-progress-integration ()
  "Set up advice to track activities from efrit-progress.
Call this after loading efrit-progress to enable real-time activity updates."
  (when (fboundp 'efrit-progress-show-tool-start)
    (advice-add 'efrit-progress-show-tool-start :after #'efrit-agent--on-tool-start))
  (when (fboundp 'efrit-progress-show-tool-result)
    (advice-add 'efrit-progress-show-tool-result :after #'efrit-agent--on-tool-result))
  (when (fboundp 'efrit-progress-show-message)
    (advice-add 'efrit-progress-show-message :after #'efrit-agent--on-message)))

(defun efrit-agent-remove-progress-integration ()
  "Remove advice for activity tracking."
  (advice-remove 'efrit-progress-show-tool-start #'efrit-agent--on-tool-start)
  (advice-remove 'efrit-progress-show-tool-result #'efrit-agent--on-tool-result)
  (advice-remove 'efrit-progress-show-message #'efrit-agent--on-message))

;;; Session lifecycle integration

(defun efrit-agent--on-session-start (session-id command)
  "Advice function called when a session starts.
Creates and displays the agent buffer for SESSION-ID with COMMAND."
  (efrit-agent-start-session session-id command)
  ;; Reset activity counter for new session
  (setq efrit-agent--activity-counter 0))

(defun efrit-agent--on-session-end (_session-id success-p)
  "Advice function called when a session ends.
Updates the agent buffer with SUCCESS-P status."
  (efrit-agent-end-session success-p))

(defun efrit-agent-setup-session-integration ()
  "Set up advice to track session lifecycle from efrit-progress.
This makes the agent buffer appear automatically when sessions start."
  (when (fboundp 'efrit-progress-start-session)
    (advice-add 'efrit-progress-start-session :after #'efrit-agent--on-session-start))
  (when (fboundp 'efrit-progress-end-session)
    (advice-add 'efrit-progress-end-session :after #'efrit-agent--on-session-end)))

(defun efrit-agent-remove-session-integration ()
  "Remove advice for session lifecycle tracking."
  (advice-remove 'efrit-progress-start-session #'efrit-agent--on-session-start)
  (advice-remove 'efrit-progress-end-session #'efrit-agent--on-session-end))

;;; User input integration

(defun efrit-agent--on-pending-question (_session question &optional options)
  "Advice function called when a pending question is set.
Updates the agent buffer with QUESTION and OPTIONS, sets status to waiting.
Uses incremental conversation update instead of full re-render."
  (let ((buffer (get-buffer efrit-agent-buffer-name)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        ;; Normalize options to a list
        (let ((opts (when options
                      (if (listp options) options (append options nil)))))
          ;; Store the pending question for keyboard shortcut handling
          (setq efrit-agent--pending-question
                (list question opts (format-time-string "%Y-%m-%dT%H:%M:%S%z")))
          ;; Set status to waiting (updates header-line)
          (setq efrit-agent--status 'waiting)
          ;; Add question to conversation incrementally (no full re-render)
          (efrit-agent--add-question question opts))))))

(defun efrit-agent--on-question-response (_session response)
  "Advice function called when user responds to a question.
Adds the user's RESPONSE to conversation and sets status back to working."
  (let ((buffer (get-buffer efrit-agent-buffer-name)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        ;; Add user's response to conversation
        (when response
          (efrit-agent--add-user-message (format "%s" response)))
        ;; Clear the pending question state
        (setq efrit-agent--pending-question nil)
        (setq efrit-agent--status 'working)
        ;; Reset input prompt
        (efrit-agent--reset-input-prompt)))))

(defun efrit-agent-setup-input-integration ()
  "Set up advice to track user input requests from session.
This makes the input section appear when Claude asks questions."
  (when (fboundp 'efrit-session-set-pending-question)
    (advice-add 'efrit-session-set-pending-question :after #'efrit-agent--on-pending-question))
  (when (fboundp 'efrit-session-respond-to-question)
    (advice-add 'efrit-session-respond-to-question :after #'efrit-agent--on-question-response)))

(defun efrit-agent-remove-input-integration ()
  "Remove advice for user input tracking."
  (advice-remove 'efrit-session-set-pending-question #'efrit-agent--on-pending-question)
  (advice-remove 'efrit-session-respond-to-question #'efrit-agent--on-question-response))

;;; Combined setup/teardown

(defun efrit-agent-setup-integration ()
  "Set up all integration hooks for the agent buffer.
Call this after loading efrit-do and efrit-progress."
  (efrit-agent-setup-todo-integration)
  (efrit-agent-setup-progress-integration)
  (efrit-agent-setup-session-integration)
  (efrit-agent-setup-input-integration))

(defun efrit-agent-remove-integration ()
  "Remove all integration hooks."
  (efrit-agent-remove-todo-integration)
  (efrit-agent-remove-progress-integration)
  (efrit-agent-remove-session-integration)
  (efrit-agent-remove-input-integration))

;;; Public API wrappers (for use by other modules)

(declare-function efrit-agent--render "efrit-agent")
(declare-function efrit-agent--add-todos-inline "efrit-agent-render")

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

(defun efrit-agent-start-session (session-id command)
  "Start tracking a new session with SESSION-ID and COMMAND.
Called from efrit-executor when an agentic session begins."
  (efrit-agent--create-buffer session-id command)
  (efrit-agent--show-buffer))

(defun efrit-agent-end-session (success-p)
  "End the current session with SUCCESS-P status."
  (let ((buffer (get-buffer efrit-agent-buffer-name)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        ;; End any streaming message first
        (efrit-agent--stream-end-message)
        (setq efrit-agent--status (if success-p 'complete 'failed))
        ;; Stop the elapsed timer
        (when efrit-agent--elapsed-timer
          (cancel-timer efrit-agent--elapsed-timer)
          (setq efrit-agent--elapsed-timer nil))
        (efrit-agent--render)))))

(provide 'efrit-agent-integration)

;;; efrit-agent-integration.el ends here
