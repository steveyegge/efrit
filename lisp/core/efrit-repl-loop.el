;;; efrit-repl-loop.el --- Async loop for REPL sessions -*- lexical-binding: t -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; Async agentic loop for REPL sessions.
;;
;; The loop structure (iteration control, API calls, tool execution)
;; lives in the shared engine `efrit-loop' (ef-0t4); this module
;; configures it with REPL-session semantics.  Unlike
;; `efrit-do-async-loop' which "completes" sessions after each command,
;; this loop transitions back to idle state, allowing the conversation
;; to continue with accumulated context.
;;
;; Usage:
;;   (efrit-repl-continue session "user input")
;;
;; The function adds the user message to the session, runs the agentic
;; loop, and returns to idle state when Claude finishes (rather than
;; "completing").

;;; Code:

(require 'cl-lib)
(require 'efrit-log)
(require 'efrit-common)
(require 'efrit-config)
(require 'efrit-repl-session)
(require 'efrit-loop)

(declare-function efrit-agent-set-status "efrit-agent")
(declare-function efrit-agent--add-error-message "efrit-agent-render")

;;; Customization

(defgroup efrit-repl-loop nil
  "Async loop for REPL sessions."
  :group 'efrit-repl
  :prefix "efrit-repl-loop-")

(defcustom efrit-repl-loop-max-iterations 100
  "Maximum iterations (API calls) per turn before stopping.
Prevents runaway loops. Set to 0 for unlimited."
  :type 'integer
  :group 'efrit-repl-loop)

;;; Loop State

(defvar efrit-repl-loop--active (make-hash-table :test 'equal)
  "Hash table of active REPL loops by session ID.
Each entry contains: (session callback iteration-count)")

(defvar efrit-repl-loop--tool-session nil
  "The REPL session whose tool call is currently executing, if any.
Dynamically bound around tool dispatch so session-aware tool handlers
\(like request_user_input) can target the REPL session instead of
requiring an active efrit-do session (ef-dcn).")

;;; Engine Adapter

(defvar efrit-repl-loop--adapter
  (efrit-loop-adapter-create
   :name "REPL session"
   :state-hash efrit-repl-loop--active
   :id-fn #'efrit-repl-session-id
   :messages-fn (lambda (session)
                  (vconcat (efrit-repl-session-get-api-messages session)))
   :add-assistant-fn #'efrit-repl-session-add-assistant-message
   :add-tool-results-fn
   (lambda (session blocks)
     (setf (efrit-repl-session-api-messages session)
           (append (efrit-repl-session-api-messages session)
                   (list `((role . "user")
                           (content . ,(vconcat blocks)))))))
   :interrupt-p-fn #'efrit-repl-session-should-interrupt-p
   :on-interrupt-fn (lambda (_session) "paused")
   :max-iterations-fn (lambda () efrit-repl-loop-max-iterations)
   ;; Wall-clock timeout (ef-5o5) measured over the current turn: the
   ;; session itself is long-lived, so session age would be meaningless.
   :elapsed-fn (lambda (session)
                 (when-let* ((start (efrit-repl-session-current-turn-start
                                     session)))
                   (float-time (time-since start))))
   :timeout-fn (lambda () efrit-session-timeout)
   :record-tool-fn #'efrit-repl-session-record-tool
   :wrap-dispatch-fn (lambda (session thunk)
                       (let ((efrit-repl-loop--tool-session session))
                         (funcall thunk)))
   :thinking-p nil
   :handles-waiting-p t
   ;; Store Claude's final message so the conversation context carries
   ;; into the next turn
   :on-end-turn-fn #'efrit-repl-session-add-assistant-message
   :api-call-fn 'efrit-repl-loop--api-call
   :continue-fn 'efrit-repl-loop--continue-iteration
   :execute-tools-fn 'efrit-repl-loop--execute-tools
   :on-api-error-fn 'efrit-repl-loop--on-api-error
   :finish-fn #'efrit-repl-loop--finish)
  "Engine adapter binding `efrit-loop' to REPL sessions.")

;;; Public API

(defun efrit-repl-continue (session user-input &optional on-turn-complete)
  "Continue REPL SESSION with USER-INPUT.
Adds the user message to the session and runs the agentic loop.
Optional ON-TURN-COMPLETE callback is called when Claude finishes the turn.

Unlike \\='efrit-do-async-loop\\=', this does not complete the session.
Instead, it transitions to idle state, preserving conversation context
for the next input.

Returns the session ID."
  (when (and session user-input (not (string-empty-p user-input)))
    (let ((session-id (efrit-repl-session-id session)))
      ;; Check if already processing
      (when (eq (efrit-repl-session-status session) 'working)
        (efrit-log 'warn "REPL session %s already working, ignoring input" session-id)
        (user-error "Session is already processing a request"))

      ;; Add user message to session
      (efrit-repl-session-add-user-message session user-input)

      ;; Begin the turn
      (efrit-repl-session-begin-turn session)

      ;; Update agent buffer status
      (when (fboundp 'efrit-agent-set-status)
        (efrit-agent-set-status 'working))

      ;; Store loop state
      (puthash session-id
               (list session on-turn-complete 0)
               efrit-repl-loop--active)

      (efrit-log 'info "REPL session %s: continuing with user input (%d chars)"
                 session-id (length user-input))

      ;; Start the loop
      (efrit-repl-loop--continue-iteration session)

      session-id)))

;;; Internal Loop Implementation

(defun efrit-repl-loop--continue-iteration (session)
  "Continue async loop for REPL SESSION, sending next request to Claude."
  (efrit-loop-continue-iteration session efrit-repl-loop--adapter))

(defun efrit-repl-loop--api-call (session messages callback)
  "Make async API call to Claude with MESSAGES for REPL SESSION.
CALLBACK is (lambda (response error) ...) called when complete."
  (efrit-loop-api-call (efrit-repl-session-id session) messages callback))

(defun efrit-repl-loop--execute-tools (session content)
  "Execute tools requested in Claude's CONTENT for REPL SESSION."
  (efrit-loop-execute-tools session efrit-repl-loop--adapter content))

(defun efrit-repl-loop--display-error (session message)
  "Display MESSAGE as an error in SESSION's agent buffer, if it is live."
  (let ((session-id (efrit-repl-session-id session))
        (buffer (efrit-repl-session-buffer session)))
    (when buffer
      (if (buffer-live-p buffer)
          ;; Buffer exists and is live - try to display error
          (with-current-buffer buffer
            (when (fboundp 'efrit-agent--add-error-message)
              (condition-case err
                  (efrit-agent--add-error-message message)
                (error
                 (efrit-log 'error "REPL session %s: Error displaying error message: %s"
                            session-id (error-message-string err))))))
        ;; Buffer reference is stale - clear it
        (setf (efrit-repl-session-buffer session) nil)))))

(defun efrit-repl-loop--on-api-error (session error)
  "Handle API ERROR for REPL SESSION."
  (efrit-log 'error "REPL session %s: API error: %s"
             (efrit-repl-session-id session) error)
  (efrit-repl-loop--display-error session (format "%s" error))
  (efrit-repl-loop--end-turn session "api-error"))

(defun efrit-repl-loop--finish (session stop-reason error-message
                                        _completion-message)
  "Engine finish hook: end SESSION's turn with STOP-REASON.
Translates the engine's canonical reason to this module's historical
strings, which callers and tests match on.  ERROR-MESSAGE, when
non-nil, is shown in the agent buffer."
  (pcase stop-reason
    ;; Internal response-handling errors take the API-error path: the
    ;; message (already labeled as an efrit bug, ef-jz6) is displayed
    ;; and the turn ends as failed.
    ("elisp-error"
     (efrit-repl-loop--on-api-error session error-message))
    ;; "unknown" means Claude finished but with unrecognized
    ;; stop_reason; downstream treats it as idle, not failed.
    ("unknown-stop-reason"
     (efrit-repl-loop--end-turn session "unknown"))
    (_
     (when error-message
       (efrit-repl-loop--display-error session error-message))
     (efrit-repl-loop--end-turn session stop-reason))))

(defun efrit-repl-loop--end-turn (session stop-reason)
  "End the current turn for REPL SESSION with STOP-REASON.
Unlike efrit-do-async--stop-loop, this transitions to idle, not complete."
  (let* ((session-id (efrit-repl-session-id session))
         (loop-state (gethash session-id efrit-repl-loop--active))
         (on-turn-complete (nth 1 loop-state))
         (iteration-count (or (nth 2 loop-state) 0)))

    ;; End the turn (transitions to idle, NOT complete)
    (efrit-repl-session-end-turn session)

    ;; A turn paused on request_user_input stays in waiting state so the
    ;; next input is routed as the answer (ef-dcn).
    (when (equal stop-reason "waiting-for-user")
      (efrit-repl-session-set-status session 'waiting))

    ;; Update agent buffer status
    ;; Note: "unknown" means Claude finished but with unrecognized stop_reason
    ;; This is normal - treat it as idle, not failed.
    (when (fboundp 'efrit-agent-set-status)
      (efrit-agent-set-status
       (cond
        ((equal stop-reason "waiting-for-user") 'waiting)
        ((member stop-reason '("end_turn" "session-complete" "unknown")) 'idle)
        (t 'failed))))

    ;; Remove from active loops
    (remhash session-id efrit-repl-loop--active)

    ;; Call completion callback if provided
    (when on-turn-complete
      (funcall on-turn-complete session stop-reason))

    (efrit-log 'info "REPL session %s: turn ended (%s, %d iterations)"
               session-id stop-reason iteration-count)))

;;; Query Functions

(defun efrit-repl-loop-active-p (session)
  "Return non-nil if SESSION has an active loop running."
  (when session
    (gethash (efrit-repl-session-id session) efrit-repl-loop--active)))

(provide 'efrit-repl-loop)

;;; efrit-repl-loop.el ends here
