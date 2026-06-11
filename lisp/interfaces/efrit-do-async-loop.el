;;; efrit-do-async-loop.el --- Async agentic execution loop -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; Async agentic loop for efrit-do / efrit-do-silently commands.
;;
;; The loop structure (iteration control, API calls, tool execution)
;; lives in the shared engine `efrit-loop' (ef-0t4); this module
;; configures it with efrit-session semantics: each command runs in
;; its own session, which is *completed* when the loop stops.
;; Contrast with `efrit-repl-loop', which transitions back to idle to
;; keep conversation context.
;;
;; All operations are non-blocking and fire progress events for
;; visibility.

;;; Code:

(require 'cl-lib)
(require 'efrit-log)
(require 'efrit-common)
(require 'efrit-session)
(require 'efrit-loop)
(require 'efrit-progress)
(require 'efrit-progress-buffer)
(require 'efrit-api)
(require 'efrit-agent)

;;; Customization

(defgroup efrit-do-async nil
  "Async agentic execution loop for Efrit."
  :group 'efrit
  :prefix "efrit-do-async-")

(defcustom efrit-do-async-max-iterations 100
  "Maximum iterations (API calls) before stopping.
Prevents runaway loops. Set to 0 for unlimited."
  :type 'integer
  :group 'efrit-do-async)

(defcustom efrit-do-async-show-progress-buffer nil
  "Whether to auto-show the raw progress buffer when execution starts.
The agent buffer is the primary surface; the progress buffer is a
debug-level event record.  Events are recorded either way — this
only controls whether the buffer is displayed."
  :type 'boolean
  :group 'efrit-do-async)

;;; Loop State

(defvar efrit-do-async--loops (make-hash-table :test 'equal)
  "Hash table of active async loops by session ID.
Each entry contains: (session callback iteration-count)")

;;; Engine Adapter

(defvar efrit-do-async--adapter
  (efrit-loop-adapter-create
   :name "Session"
   :state-hash efrit-do-async--loops
   :id-fn #'efrit-session-id
   :messages-fn #'efrit-session-get-api-messages-for-continuation
   :add-assistant-fn #'efrit-session-add-assistant-response
   :add-tool-results-fn #'efrit-session-add-tool-results
   :interrupt-p-fn #'efrit-session-should-interrupt-p
   :on-interrupt-fn
   (lambda (session)
     ;; Clear interrupt flag and record the interruption for Claude
     (setf (efrit-session-interrupt-requested session) nil)
     (efrit-session-add-tool-results session
       (list (efrit-session-build-tool-result
              "system" "User interrupted execution")))
     "interrupted")
   :max-iterations-fn (lambda () efrit-do-async-max-iterations)
   ;; Wall-clock timeout over the whole session (ef-5o5): the session
   ;; completes per command, so session age is the right measure.
   :elapsed-fn (lambda (session)
                 (float-time (time-since (efrit-session-start-time session))))
   :timeout-fn (lambda () efrit-session-timeout)
   ;; Dispatch failures never reach efrit-do--execute-tool's own work
   ;; logging, so record them here (ef-c1h)
   :on-tool-error-fn (lambda (session tool-result tool-name)
                       (efrit-session-add-work session tool-result
                                               (format "(%s)" tool-name)
                                               nil
                                               tool-name))
   :event-fn #'efrit-progress-insert-event
   :thinking-p t
   :handles-waiting-p nil
   :api-call-fn 'efrit-do-async--api-call
   :continue-fn 'efrit-do-async--continue-iteration
   :execute-tools-fn 'efrit-do-async--execute-tools
   :on-api-error-fn 'efrit-do-async--on-api-error
   :finish-fn #'efrit-do-async--finish)
  "Engine adapter binding `efrit-loop' to efrit-do sessions.")

;;; Core Loop Implementation

(defun efrit-do-async-loop (session _system-prompt &optional on-complete)
  "Start async agentic execution loop for SESSION.
_SYSTEM-PROMPT is the initial system message for Claude (reserved).
ON-COMPLETE is an optional callback called when execution finishes.
Returns the session ID."
  (let ((session-id (efrit-session-id session)))
    (efrit-log 'info "Starting async loop for session %s" session-id)

    ;; Always record events in the progress buffer; only display it
    ;; on request — the agent buffer is the single user-facing surface
    (efrit-progress-create-buffer session-id)
    (when efrit-do-async-show-progress-buffer
      (efrit-progress-show-buffer session-id))

    ;; Initialize agent buffer for session
    (efrit-agent-start-session session-id (efrit-session-command session))

    ;; Store loop state
    (puthash session-id
             (list session on-complete 0 nil)
             efrit-do-async--loops)

    ;; Start first iteration
    (efrit-do-async--continue-iteration session)

    session-id))

(defun efrit-do-async--continue-iteration (session)
  "Continue async loop for SESSION, sending next request to Claude."
  (efrit-loop-continue-iteration session efrit-do-async--adapter))

(defun efrit-do-async--api-call (session messages callback)
  "Make async API call to Claude with MESSAGES for SESSION.
CALLBACK is (lambda (response error) ...) called when complete."
  (efrit-loop-api-call (efrit-session-id session) messages callback))

(defun efrit-do-async--execute-tools (session content)
  "Execute tools requested in Claude's CONTENT for SESSION.
CONTENT is a vector of content blocks."
  (efrit-loop-execute-tools session efrit-do-async--adapter content))

(defun efrit-do-async--on-api-error (session error)
  "Handle API ERROR for SESSION."
  (let ((session-id (efrit-session-id session)))
    (efrit-log 'error "Session %s: API error: %s" session-id error)
    (when (fboundp 'efrit-agent-hide-thinking)
      (efrit-agent-hide-thinking))

    ;; Fire error event
    (efrit-progress-insert-event session-id 'error
      `((:message . ,(format "%S" error)) (:level . "ERROR")))

    ;; Stop loop
    (efrit-do-async--stop-loop session "api-error" (format "%s" error))))

(defun efrit-do-async--finish (session stop-reason error-message
                                       completion-message)
  "Engine finish hook: stop SESSION's loop with STOP-REASON.
Translates the engine's canonical reason to this module's historical
strings, which callers and tests match on.  ERROR-MESSAGE and
COMPLETION-MESSAGE pass through to `efrit-do-async--stop-loop'."
  (efrit-do-async--stop-loop session
                             (if (equal stop-reason "iteration-limit")
                                 "iteration-limit-exceeded"
                               stop-reason)
                             error-message completion-message))

(defun efrit-do-async--stop-loop (session stop-reason &optional error-message
                                          completion-message)
  "Stop async loop for SESSION with STOP-REASON.
ERROR-MESSAGE, when non-nil, is surfaced to the user in the agent
buffer and echo area.  COMPLETION-MESSAGE is Claude's final answer
from the session_complete tool, rendered in the agent buffer (ef-ter)."
  (let* ((session-id (efrit-session-id session))
         (loop-state (gethash session-id efrit-do-async--loops))
         (on-complete (nth 1 loop-state))
         (iteration-count (or (nth 2 loop-state) 0)))

    ;; Complete session (this clears active session state)
    (efrit-session-complete session stop-reason)

    ;; Fire appropriate event based on stop reason
    (let ((elapsed (float-time
                   (time-since (efrit-session-start-time session))))
          (tool-count (efrit-progress-tool-call-count)))
      (cond
       ((string= stop-reason "interrupted")
        (efrit-progress-insert-event session-id 'error
          `((:message . ,(format "Execution interrupted after %d tool calls"
                               (max 0 (1- tool-count))))
            (:level . "INTERRUPTED")))
        (efrit-progress-insert-event session-id 'complete
          `((:result . "interrupted") (:elapsed . ,elapsed))))
       (t
        (efrit-progress-insert-event session-id 'complete
          `((:result . ,stop-reason) (:elapsed . ,elapsed))))))

    ;; Signal session completion to agent buffer.  session-complete is
    ;; the success path too: it means Claude called the session_complete
    ;; tool rather than simply ending its turn.
    (efrit-agent-end-session (member stop-reason '("end_turn" "session-complete"))
                             stop-reason error-message completion-message)

    ;; Archive progress buffer
    (efrit-progress-archive-buffer session-id)

    ;; Remove from active loops
    (remhash session-id efrit-do-async--loops)

    ;; Call completion callback if provided
    (when on-complete
      (funcall on-complete session stop-reason))

    (efrit-log 'info "Session %s: loop stopped (%s, %d iterations)"
               session-id stop-reason iteration-count)))

(provide 'efrit-do-async-loop)

;;; efrit-do-async-loop.el ends here
