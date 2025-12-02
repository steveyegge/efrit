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
(require 'efrit-chat-response)
(require 'efrit-executor)

(declare-function efrit-do--execute-tool "efrit-do")
(declare-function efrit-do--command-system-prompt "efrit-do")
(declare-function efrit-do--get-current-tools-schema "efrit-do")
(defvar efrit-default-model)

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
    (efrit-do-async--api-call
     session
     messages
     (lambda (response error)
       (if error
           (efrit-do-async--on-api-error session error)
         (efrit-do-async--on-api-response session response))))))

(defun efrit-do-async--api-call (session messages callback)
  "Make async API call to Claude with MESSAGES for SESSION.
CALLBACK is (lambda (response error) ...) called when complete."
  (efrit-log 'debug "API call sending %d messages" (length messages))
  (require 'efrit-do)
  (let* ((session-id (efrit-session-id session))
         (system-prompt (efrit-do--command-system-prompt nil nil nil session-id nil))
         (request-data
          `(("model" . ,efrit-default-model)
            ("max_tokens" . 8192)
            ("temperature" . 0.0)
            ("messages" . ,messages)
            ("system" . ,system-prompt)
            ("tools" . ,(efrit-do--get-current-tools-schema)))))
    (efrit-executor--api-request
     request-data
     (lambda (response)
       (if (and response (efrit-response-error response))
           (funcall callback nil (efrit-error-message (efrit-response-error response)))
         (funcall callback response nil))))))

(defun efrit-do-async--on-api-response (session response)
  "Handle API RESPONSE for SESSION.
RESPONSE is a hash table from Claude API with \"content\" and \"stop_reason\" fields."
  (let ((session-id (efrit-session-id session)))
    (efrit-log 'debug "Session %s: received response" session-id)
    
    ;; Extract content and stop_reason from response using proper accessors
    (let* ((content (efrit-response-content response))
           (stop-reason (efrit-response-stop-reason response)))
      
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
  "Execute tools requested in Claude's CONTENT for SESSION.
CONTENT is a vector of content blocks from Claude API response."
  (let ((session-id (efrit-session-id session))
        (results nil))
    
    ;; CRITICAL: Store Claude's full response BEFORE executing tools
    ;; This maintains proper message order for API continuation
    (efrit-session-add-assistant-response session content)
    
    ;; Process each content item from the vector
    (dotimes (i (length content))
      (let* ((item (aref content i))
             (tool-use-info (efrit-content-item-as-tool-use item)))
        ;; Execute tool_use blocks only (returns nil if not a tool_use)
        (when tool-use-info
          (let* ((tool-id (nth 0 tool-use-info))
                 (tool-name (nth 1 tool-use-info))
                 (input (nth 2 tool-use-info)))
            
            (efrit-log 'debug "Session %s: executing tool %s (id: %s)"
                       session-id tool-name tool-id)
            
            ;; Fire progress event
            (efrit-progress-insert-event session-id 'tool_started
              `((:tool . ,tool-name) (:input . ,input)))
            
            ;; Execute tool via the real executor
            (let ((tool-result (efrit-do-async--execute-single-tool
                               session tool-id tool-name input)))
              ;; Fire result event
              (efrit-progress-insert-event session-id 'tool_result
                `((:tool . ,tool-name) (:result . ,tool-result)))
              
              ;; Collect result for API call
              (push (efrit-session-build-tool-result tool-id tool-result)
                    results))))))
    
    (efrit-log 'info "Session %s: executed %d tools"
               session-id (length results))
    
    ;; Send results back to Claude if any tools were executed
    (when results
      (efrit-session-add-tool-results session (nreverse results)))
    
    ;; Continue loop
    (efrit-do-async--continue-iteration session)))

(defun efrit-do-async--execute-single-tool (session tool-id tool-name input)
  "Execute single TOOL-NAME with INPUT for SESSION.
TOOL-ID is the tool use ID from Claude's response.
Returns tool result or error message."
  (let ((session-id (efrit-session-id session)))
    (efrit-log 'debug "Session %s: executing tool %s (id: %s)"
               session-id tool-name tool-id)
    (require 'efrit-do)
    (condition-case err
        (let ((tool-item (make-hash-table :test 'equal)))
          (puthash "id" tool-id tool-item)
          (puthash "name" tool-name tool-item)
          (puthash "input" input tool-item)
          (efrit-do--execute-tool tool-item))
      (error
       (let ((error-msg (format "Error executing %s: %s"
                                tool-name (error-message-string err))))
         (efrit-log 'error "Session %s: %s" session-id error-msg)
         error-msg)))))

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
    
    ;; Complete session (this clears active session state)
    (efrit-session-complete session stop-reason)
    
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
