;;; efrit-do-async-loop.el --- Async agentic execution loop -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; Async agentic loop implementation for Efrit's multi-step execution.
;; This is the core engine that drives async execution of Claude commands.
;;
;; The loop:
;; 1. Sends conversation to Claude (async)
;; 2. Receives response with potential tool_use
;; 3. Executes tools if requested
;; 4. Sends results back to Claude
;; 5. Repeats until end_turn or session_complete
;; 6. Respects user interrupts (C-g) between steps
;;
;; All operations are non-blocking and fire progress events for visibility.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'efrit-log)
(require 'efrit-common)
(require 'efrit-session)
(require 'efrit-progress-buffer)

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

(defcustom efrit-do-async-show-progress-buffer t
  "Whether to auto-show progress buffer when execution starts."
  :type 'boolean
  :group 'efrit-do-async)

;;; Loop State

(defvar efrit-do-async--loops (make-hash-table :test 'equal)
  "Hash table of active async loops by session ID.
Each entry contains: (session callback iteration-count)")

;;; Core Loop Implementation

(defun efrit-do-async-loop (session _system-prompt &optional on-complete)
  "Start async agentic execution loop for SESSION.
_SYSTEM-PROMPT is the initial system message for Claude (reserved).
ON-COMPLETE is an optional callback called when execution finishes.
Returns the session ID."
  (let ((session-id (efrit-session-id session)))
    (efrit-log 'info "Starting async loop for session %s" session-id)
    
    ;; Initialize progress buffer
    (when efrit-do-async-show-progress-buffer
      (efrit-progress-create-buffer session-id)
      (efrit-progress-show-buffer session-id))
    
    ;; Store loop state
    (puthash session-id
            (list session on-complete 0 nil)
            efrit-do-async--loops)
    
    ;; Start first iteration
    (efrit-do-async--continue-iteration session)
    
    session-id))

(defun efrit-do-async--continue-iteration (session)
  "Continue async loop for SESSION, sending next request to Claude."
  (let ((session-id (efrit-session-id session)))
    ;; Check for interruption
    (if (efrit-session-should-interrupt-p session)
        (progn
          (efrit-log 'info "Session %s interrupt requested, stopping loop"
                     session-id)
          (efrit-do-async--stop-loop session "interrupted"))
      ;; Continue with normal flow
      (let* ((loop-state (gethash session-id efrit-do-async--loops))
             (iteration-count (nth 2 loop-state)))
        (if (and (> efrit-do-async-max-iterations 0)
                 (>= iteration-count efrit-do-async-max-iterations))
            (progn
              (efrit-log 'warn "Session %s hit iteration limit (%d)"
                         session-id efrit-do-async-max-iterations)
              (efrit-do-async--stop-loop session "iteration-limit-exceeded"))
          ;; Send request to Claude asynchronously
          (progn
            (efrit-log 'debug "Session %s: sending request (iteration %d)"
                       session-id iteration-count)
            (efrit-do-async--send-request session)))))))

(defun efrit-do-async--send-request (session)
  "Send request to Claude API for SESSION.
Async operation with callback for response handling."
  (let* ((messages (efrit-session-get-api-messages-for-continuation session)))
    ;; This would delegate to the executor's async API call
    ;; For now, we define the callback structure
    (efrit-do-async--api-call
     messages
     (lambda (response error)
       (if error
           (efrit-do-async--on-api-error session error)
         (efrit-do-async--on-api-response session response))))))

(defun efrit-do-async--api-call (messages _callback)
  "Make async API call to Claude with MESSAGES.
_CALLBACK is (lambda (response error) ...) - reserved for future integration.
Placeholder for integration with actual executor."
  (efrit-log 'debug "API call would send %d messages" (length messages))
  ;; This would use efrit-executor--api-request
  ;; For now, just log
  nil)

(defun efrit-do-async--on-api-response (session response)
  "Handle API RESPONSE for SESSION."
  (let ((session-id (efrit-session-id session)))
    (efrit-log 'debug "Session %s: received response" session-id)
    
    ;; Extract content and stop_reason from response
    (let* ((content (alist-get 'content response))
           (stop-reason (alist-get 'stop_reason response)))
      
      ;; Fire progress event for message
      (when content
        (efrit-progress-insert-event session-id 'message
          `((:text . ,(format "%S" content)) (:role . "assistant"))))
      
      ;; Process based on stop reason
      (pcase stop-reason
        ("tool_use"
         ;; Extract and execute tools
         (efrit-do-async--execute-tools session content))
        ("end_turn"
         ;; Claude finished
         (efrit-log 'info "Session %s: Claude ended turn" session-id)
         (efrit-do-async--stop-loop session "end_turn"))
        (_
         (efrit-log 'warn "Session %s: unknown stop reason: %s"
                    session-id stop-reason)
         (efrit-do-async--stop-loop session "unknown-stop-reason"))))))

(defun efrit-do-async--execute-tools (session content)
  "Execute tools requested in Claude's CONTENT for SESSION."
  (let ((session-id (efrit-session-id session))
        (tool-uses (seq-filter
                   (lambda (block)
                     (string= (alist-get 'type block) "tool_use"))
                   content)))
    (efrit-log 'info "Session %s: executing %d tools"
               session-id (length tool-uses))
    
    ;; Execute tools and collect results
    (let ((results nil))
      (dolist (tool-use tool-uses)
        (let* ((tool-id (alist-get 'id tool-use))
               (tool-name (alist-get 'name tool-use))
               (input (alist-get 'input tool-use)))
          ;; Fire progress event
          (efrit-progress-insert-event session-id 'tool_started
            `((:tool . ,tool-name) (:input . ,input)))
          
          ;; Execute tool (would integrate with actual executor)
          (let ((tool-result (efrit-do-async--execute-single-tool
                             session tool-name input)))
            ;; Fire result event
            (efrit-progress-insert-event session-id 'tool_result
              `((:tool . ,tool-name) (:result . ,tool-result)))
            
            ;; Collect result for API call
            (push (efrit-session-build-tool-result tool-id tool-result)
                  results))))
      
      ;; Send results back to Claude
      (efrit-session-add-tool-results session (nreverse results))
      
      ;; Continue loop
      (efrit-do-async--continue-iteration session))))

(defun efrit-do-async--execute-single-tool (session tool-name _input)
  "Execute single TOOL-NAME with _INPUT for SESSION.
Returns tool result or error message."
  (let ((session-id (efrit-session-id session)))
    (efrit-log 'debug "Session %s: executing tool %s" session-id tool-name)
    
    ;; This would delegate to actual tool executor
    ;; For now, return placeholder result
    (format "Tool %s executed with result" tool-name)))

(defun efrit-do-async--on-api-error (session error)
  "Handle API ERROR for SESSION."
  (let ((session-id (efrit-session-id session)))
    (efrit-log 'error "Session %s: API error: %s" session-id error)
    
    ;; Fire error event
    (efrit-progress-insert-event session-id 'error
      `((:message . ,(format "%S" error)) (:level . "ERROR")))
    
    ;; Stop loop
    (efrit-do-async--stop-loop session "api-error")))

(defun efrit-do-async--stop-loop (session stop-reason)
  "Stop async loop for SESSION with STOP-REASON."
  (let* ((session-id (efrit-session-id session))
         (loop-state (gethash session-id efrit-do-async--loops))
         (on-complete (nth 1 loop-state)))
    
    ;; Update session status
    (efrit-session-set-status session 'complete)
    
    ;; Fire completion event
    (let ((elapsed (float-time
                   (time-since (efrit-session-start-time session)))))
      (efrit-progress-insert-event session-id 'complete
        `((:result . ,stop-reason) (:elapsed . ,elapsed))))
    
    ;; Archive progress buffer
    (efrit-progress-archive-buffer session-id)
    
    ;; Remove from active loops
    (remhash session-id efrit-do-async--loops)
    
    ;; Call completion callback if provided
    (when on-complete
      (funcall on-complete session stop-reason))
    
    (efrit-log 'info "Session %s: loop stopped (%s)"
               session-id stop-reason)))

(provide 'efrit-do-async-loop)

;;; efrit-do-async-loop.el ends here
