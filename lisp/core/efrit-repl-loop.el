;;; efrit-repl-loop.el --- Async loop for REPL sessions -*- lexical-binding: t -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; Async agentic loop for REPL sessions.
;;
;; Unlike `efrit-do-async-loop' which "completes" sessions after each command,
;; this loop transitions back to idle state, allowing the conversation to
;; continue with accumulated context.
;;
;; Usage:
;;   (efrit-repl-continue session "user input")
;;
;; The function adds the user message to the session, runs the agentic loop,
;; and returns to idle state when Claude finishes (rather than "completing").

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'efrit-log)
(require 'efrit-common)
(require 'efrit-repl-session)
(require 'efrit-chat-response)
;; Note: efrit-executor is required at runtime to avoid circular dependency
;; (efrit-executor -> efrit-session -> efrit-repl-loop -> efrit-executor)

(declare-function efrit-do--execute-tool "efrit-do-dispatch")
(declare-function efrit-do--command-system-prompt "efrit-do-prompt")
(declare-function efrit-do--get-current-tools-schema "efrit-do-schema")
(declare-function efrit-executor--api-request "efrit-executor")
(declare-function efrit-agent-stream-content "efrit-agent")
(declare-function efrit-agent-stream-end "efrit-agent")
(declare-function efrit-agent-show-tool-start "efrit-agent")
(declare-function efrit-agent-show-tool-result "efrit-agent")
(declare-function efrit-agent-set-status "efrit-agent")
(defvar efrit-default-model)

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
  (let ((session-id (efrit-repl-session-id session)))
    ;; Check for pause request
    (if (efrit-repl-session-should-interrupt-p session)
        (progn
          (efrit-log 'info "REPL session %s: pause requested" session-id)
          (efrit-repl-loop--end-turn session "paused"))
      ;; Check iteration limit
      (let* ((loop-state (gethash session-id efrit-repl-loop--active))
             (iteration-count (nth 2 loop-state)))
        (if (and (> efrit-repl-loop-max-iterations 0)
                 (>= iteration-count efrit-repl-loop-max-iterations))
            (progn
              (efrit-log 'warn "REPL session %s hit iteration limit (%d)"
                         session-id efrit-repl-loop-max-iterations)
              (efrit-repl-loop--end-turn session "iteration-limit"))
          ;; Increment iteration count
          (let ((new-state (list session (nth 1 loop-state) (1+ iteration-count))))
            (puthash session-id new-state efrit-repl-loop--active))
          ;; Send request
          (efrit-repl-loop--send-request session))))))

(defun efrit-repl-loop--send-request (session)
  "Send request to Claude API for REPL SESSION."
  (let ((messages (efrit-repl-session-get-api-messages session)))
    (efrit-repl-loop--api-call
     session
     messages
     (lambda (response error)
       (if error
           (efrit-repl-loop--on-api-error session error)
         (efrit-repl-loop--on-api-response session response))))))

(defun efrit-repl-loop--api-call (session messages callback)
  "Make async API call to Claude with MESSAGES for REPL SESSION.
CALLBACK is (lambda (response error) ...) called when complete."
  (efrit-log 'debug "REPL API call sending %d messages" (length messages))
  (require 'efrit-executor)
  (require 'efrit-do)
  (let* ((session-id (efrit-repl-session-id session))
         (system-prompt (efrit-do--command-system-prompt nil nil nil session-id nil))
         (request-data
          `(("model" . ,efrit-default-model)
            ("max_tokens" . 8192)
            ("temperature" . 0.0)
            ("messages" . ,(vconcat messages))
            ("system" . ,system-prompt)
            ("tools" . ,(efrit-do--get-current-tools-schema)))))
    (efrit-executor--api-request
     request-data
     (lambda (response)
       (if (and response (efrit-response-error response))
           (funcall callback nil (efrit-error-message (efrit-response-error response)))
         (funcall callback response nil))))))

(defun efrit-repl-loop--on-api-response (session response)
  "Handle API RESPONSE for REPL SESSION."
  (let ((session-id (efrit-repl-session-id session)))
    (efrit-log 'debug "REPL session %s: received response" session-id)

    (let* ((content (efrit-response-content response))
           (stop-reason (efrit-response-stop-reason response)))

      ;; Stream text content to agent buffer
      (when content
        (dotimes (i (length content))
          (let ((item (aref content i)))
            (when (and (hash-table-p item)
                       (string= (gethash "type" item) "text"))
              (when (fboundp 'efrit-agent-stream-content)
                (efrit-agent-stream-content (gethash "text" item))))))
        (when (fboundp 'efrit-agent-stream-end)
          (efrit-agent-stream-end)))

      ;; Process based on stop reason
      (pcase stop-reason
        ("tool_use"
         (efrit-repl-loop--execute-tools session content))
        ("end_turn"
         (efrit-log 'info "REPL session %s: Claude ended turn" session-id)
         ;; Store assistant response
         (efrit-repl-session-add-assistant-message session content)
         (efrit-repl-loop--end-turn session "end_turn"))
        (_
         (efrit-log 'warn "REPL session %s: unknown stop reason: %s"
                    session-id stop-reason)
         (efrit-repl-loop--end-turn session "unknown"))))))

(defun efrit-repl-loop--execute-tools (session content)
  "Execute tools requested in Claude's CONTENT for REPL SESSION."
  (let ((session-id (efrit-repl-session-id session))
        (results nil)
        (session-complete-requested nil))

    ;; Store Claude's response BEFORE executing tools
    (efrit-repl-session-add-assistant-message session content)

    ;; Process each tool_use in content
    (dotimes (i (length content))
      (let* ((item (aref content i))
             (tool-use-info (efrit-content-item-as-tool-use item)))
        (when tool-use-info
          (let* ((tool-id (nth 0 tool-use-info))
                 (tool-name (nth 1 tool-use-info))
                 (input (nth 2 tool-use-info)))

            (efrit-log 'debug "REPL session %s: executing tool %s" session-id tool-name)

            ;; Record tool use
            (efrit-repl-session-record-tool session tool-name)

            ;; Show in agent buffer
            (let ((agent-tool-id (when (fboundp 'efrit-agent-show-tool-start)
                                   (efrit-agent-show-tool-start tool-name input)))
                  (tool-start-time (current-time)))

              ;; Execute tool
              (let* ((tool-result (efrit-repl-loop--execute-single-tool
                                   session tool-id tool-name input))
                     (is-session-complete (string-match-p "\\[SESSION-COMPLETE:" tool-result))
                     (is-error (string-match-p "^Error " tool-result))
                     (elapsed-secs (float-time (time-subtract (current-time) tool-start-time))))

                ;; Show result in agent buffer
                (when (and agent-tool-id (fboundp 'efrit-agent-show-tool-result))
                  (efrit-agent-show-tool-result agent-tool-id tool-result (not is-error) elapsed-secs))

                ;; Collect result
                (push `((type . "tool_result")
                        (tool_use_id . ,tool-id)
                        (content . ,tool-result)
                        ,@(when is-error '((is_error . t))))
                      results)

                (when is-session-complete
                  (setq session-complete-requested t))))))))

    (efrit-log 'info "REPL session %s: executed %d tools" session-id (length results))

    ;; Add tool results to session
    (when results
      (setf (efrit-repl-session-api-messages session)
            (append (efrit-repl-session-api-messages session)
                    (list `((role . "user")
                            (content . ,(vconcat (nreverse results))))))))

    ;; Continue or end
    (if session-complete-requested
        (efrit-repl-loop--end-turn session "session-complete")
      (efrit-repl-loop--continue-iteration session))))

(defun efrit-repl-loop--execute-single-tool (session tool-id tool-name input)
  "Execute single TOOL-NAME with INPUT for REPL SESSION.
TOOL-ID is the tool use ID from Claude's response.
Returns tool result string."
  (let ((session-id (efrit-repl-session-id session)))
    (require 'efrit-do)
    (condition-case err
        (progn
          (unless (hash-table-p input)
            (error "Tool input must be a hash table, got %s" (type-of input)))

          (let ((tool-item (make-hash-table :test 'equal)))
            (puthash "id" tool-id tool-item)
            (puthash "name" tool-name tool-item)
            (puthash "input" input tool-item)

            (let ((result (efrit-do--execute-tool tool-item)))
              (unless (stringp result)
                (setq result (format "%S" result)))
              (efrit-log 'debug "REPL session %s: tool %s returned %d chars"
                         session-id tool-name (length result))
              result)))

      (error
       (let* ((error-msg (error-message-string err))
              (formatted-error (format "Error executing %s: %s" tool-name error-msg)))
         (efrit-log 'error "REPL session %s: %s" session-id formatted-error)
         formatted-error)))))

(defun efrit-repl-loop--on-api-error (session error)
  "Handle API ERROR for REPL SESSION."
  (let ((session-id (efrit-repl-session-id session)))
    (efrit-log 'error "REPL session %s: API error: %s" session-id error)
    (efrit-repl-loop--end-turn session "api-error")))

(defun efrit-repl-loop--end-turn (session stop-reason)
  "End the current turn for REPL SESSION with STOP-REASON.
Unlike efrit-do-async--stop-loop, this transitions to idle, not complete."
  (let* ((session-id (efrit-repl-session-id session))
         (loop-state (gethash session-id efrit-repl-loop--active))
         (on-turn-complete (nth 1 loop-state))
         (iteration-count (or (nth 2 loop-state) 0)))

    ;; End the turn (transitions to idle, NOT complete)
    (efrit-repl-session-end-turn session)

    ;; Update agent buffer status
    (when (fboundp 'efrit-agent-set-status)
      (efrit-agent-set-status
       (if (member stop-reason '("end_turn" "session-complete"))
           'idle
         'failed)))

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
