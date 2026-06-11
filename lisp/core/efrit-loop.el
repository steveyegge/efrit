;;; efrit-loop.el --- Shared agentic loop engine -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; The single agentic loop engine behind Efrit's async execution paths
;; (ef-0t4).  Previously `efrit-do-async-loop' and `efrit-repl-loop'
;; were structural clones of the same Claude loop, and fixes landed in
;; one but not the other (ef-5o5, ef-2qx, 098182c, ef-hn6, ef-ter, all
;; fixed in do-async only).  This engine owns the shared structure:
;;
;; 1. Iteration control: interrupt check, wall-clock timeout,
;;    iteration cap
;; 2. Request construction and the async API call
;; 3. Response handling: streaming text to the agent buffer,
;;    dispatching on stop_reason
;; 4. Tool execution: SESSION-COMPLETE / WAITING-FOR-USER / error
;;    detection, result collection, completion-message extraction
;;
;; Everything session-shaped or end-of-turn-shaped is parameterized
;; through an `efrit-loop-adapter' struct.  The do-async path completes
;; its session per command; the REPL path transitions back to idle and
;; keeps conversation context.  Both are now thin configurations over
;; this engine.
;;
;; The adapter's api-call-fn / continue-fn / execute-tools-fn /
;; on-api-error-fn slots hold the *symbols* of per-module functions
;; rather than closures: tests advice-override those names
;; (e.g. test-agent-mock overrides `efrit-do-async--api-call'), so the
;; engine must call through the symbols for advice to take effect.
;;
;; The blocking `efrit-executor--sync-loop' remains separate: it is a
;; synchronous while-loop with different accumulation semantics, and
;; wrapping it over an async engine would buy little.

;;; Code:

(require 'cl-lib)
(require 'efrit-log)
(require 'efrit-api)
(require 'efrit-chat-response)

(declare-function efrit-do--execute-tool "efrit-do-dispatch")
(declare-function efrit-do--command-system-prompt "efrit-do-prompt")
(declare-function efrit-do--get-current-tools-schema "efrit-do-schema")
(declare-function efrit-agent-show-thinking "efrit-agent")
(declare-function efrit-agent-hide-thinking "efrit-agent")
(declare-function efrit-agent-stream-content "efrit-agent")
(declare-function efrit-agent-stream-end "efrit-agent")
(declare-function efrit-agent-show-tool-start "efrit-agent")
(declare-function efrit-agent-show-tool-result "efrit-agent")
(defvar efrit-default-model)

;;; Adapter

(cl-defstruct (efrit-loop-adapter
               (:constructor efrit-loop-adapter-create))
  "Configuration adapting the loop engine to a session type.

Loop state lives in STATE-HASH keyed by session ID; each value is a
list (SESSION CALLBACK ITERATION-COUNT . REST).  The engine reads and
increments ITERATION-COUNT and preserves any trailing elements; the
owning module creates and removes entries (its finish function calls
CALLBACK and does the remhash)."
  name                ; String naming the session kind in log messages.
  state-hash          ; Hash table of session-id -> loop-state list.
  id-fn               ; (session) -> session ID string.
  messages-fn         ; (session) -> messages vector for the API request.
  add-assistant-fn    ; (session content) store assistant response.
  add-tool-results-fn ; (session blocks) store tool_result blocks (in order).
  interrupt-p-fn      ; (session) -> non-nil if an interrupt is pending.
  on-interrupt-fn     ; (session) -> stop-reason string; do any pre-stop work.
  max-iterations-fn   ; () -> iteration cap (0 = unlimited).
  elapsed-fn          ; (session) -> seconds elapsed, or nil if unknown.
  timeout-fn          ; () -> wall-clock limit in seconds (0 = disabled).
  record-tool-fn      ; (session tool-name) per-tool bookkeeping, or nil.
  wrap-dispatch-fn    ; (session thunk) -> result; wraps tool dispatch, or nil.
  on-tool-error-fn    ; (session tool-result tool-name) on dispatch error, or nil.
  event-fn            ; (session-id type data) progress-event sink, or nil.
  thinking-p          ; Non-nil to drive the agent thinking indicator.
  handles-waiting-p   ; Non-nil if WAITING-FOR-USER pauses the turn.
  on-end-turn-fn      ; (session content) called on end_turn before finish, or nil.
  api-call-fn         ; Symbol: (session messages callback) async API call.
  continue-fn         ; Symbol: (session) re-enter the iteration loop.
  execute-tools-fn    ; Symbol: (session content) execute requested tools.
  on-api-error-fn     ; Symbol: (session error) terminal API-error path.
  finish-fn)          ; (session reason error-message completion-message) terminal.

;;; Helpers

(defun efrit-loop--event (adapter session-id type data)
  "Send progress event TYPE with DATA for SESSION-ID via ADAPTER's sink."
  (when-let* ((fn (efrit-loop-adapter-event-fn adapter)))
    (funcall fn session-id type data)))

(defun efrit-loop--finish (session adapter reason
                                   &optional error-message completion-message)
  "End SESSION's loop via ADAPTER's finish function.
REASON is the engine's stop reason; the adapter translates it to its
own conventions.  ERROR-MESSAGE and COMPLETION-MESSAGE are surfaced to
the user where the session type supports it."
  (funcall (efrit-loop-adapter-finish-fn adapter)
           session reason error-message completion-message))

;;; Iteration Control

(defun efrit-loop-continue-iteration (session adapter)
  "Run one iteration of the agentic loop for SESSION using ADAPTER.
Checks interrupt, wall-clock timeout, and iteration cap, then sends
the next API request."
  (let* ((name (efrit-loop-adapter-name adapter))
         (session-id (funcall (efrit-loop-adapter-id-fn adapter) session)))
    (if (funcall (efrit-loop-adapter-interrupt-p-fn adapter) session)
        (let ((reason (funcall (efrit-loop-adapter-on-interrupt-fn adapter)
                               session)))
          (efrit-log 'info "%s %s: interrupt requested, stopping loop"
                     name session-id)
          (efrit-loop--finish session adapter reason))
      (let* ((loop-state (gethash session-id
                                  (efrit-loop-adapter-state-hash adapter)))
             (iteration-count (or (nth 2 loop-state) 0))
             (timeout (funcall (efrit-loop-adapter-timeout-fn adapter)))
             (elapsed (funcall (efrit-loop-adapter-elapsed-fn adapter) session))
             (max-iterations (funcall (efrit-loop-adapter-max-iterations-fn
                                       adapter))))
        (cond
         ;; Wall-clock timeout (ef-5o5).  Checked between iterations,
         ;; so an in-flight API call still completes before the stop.
         ((and timeout (> timeout 0) elapsed (>= elapsed timeout))
          (efrit-log 'warn "%s %s hit wall-clock timeout (%.0fs, limit: %ds)"
                     name session-id elapsed timeout)
          (efrit-loop--finish
           session adapter "session-timeout"
           (format "Session timed out after %.0fs (limit: %ds, see efrit-session-timeout)"
                   elapsed timeout)))
         ((and (> max-iterations 0) (>= iteration-count max-iterations))
          (efrit-log 'warn "%s %s hit iteration limit (%d)"
                     name session-id max-iterations)
          (efrit-loop--finish session adapter "iteration-limit"))
         (t
          ;; Increment iteration count, preserving any extra state
          ;; elements the owning module keeps in the list.
          (puthash session-id
                   (append (list session (nth 1 loop-state)
                                 (1+ iteration-count))
                           (nthcdr 3 loop-state))
                   (efrit-loop-adapter-state-hash adapter))
          (efrit-log 'debug "%s %s: sending request (iteration %d)"
                     name session-id (1+ iteration-count))
          (efrit-loop--send-request session adapter)))))))

;;; Request / Response

(defun efrit-loop--send-request (session adapter)
  "Send the next API request for SESSION via ADAPTER's api-call function."
  (let ((messages (funcall (efrit-loop-adapter-messages-fn adapter) session)))
    ;; Animated indicator while the API call is in flight
    (when (and (efrit-loop-adapter-thinking-p adapter)
               (fboundp 'efrit-agent-show-thinking))
      (efrit-agent-show-thinking "waiting for Claude..."))
    (funcall (efrit-loop-adapter-api-call-fn adapter)
     session
     messages
     (lambda (response error)
       (if error
           (funcall (efrit-loop-adapter-on-api-error-fn adapter) session error)
         ;; An uncaught elisp error here would silently kill the loop
         ;; (see 098182c): catch it and fail the turn visibly.  Label
         ;; it as a client-side bug, not an API failure -- relabeling
         ;; these as API errors sent debugging down the wrong path
         ;; (ef-jz6).
         (condition-case err
             (efrit-loop-handle-response session adapter response)
           (error
            (let ((msg (format "Internal error handling response (efrit bug, not an API failure): %s"
                               (error-message-string err))))
              (efrit-log 'error "%s %s: %s"
                         (efrit-loop-adapter-name adapter)
                         (funcall (efrit-loop-adapter-id-fn adapter) session)
                         msg)
              (efrit-loop--finish session adapter "elisp-error" msg)))))))))

(defun efrit-loop-api-call (session-id messages callback)
  "Make the canonical async Claude API call for SESSION-ID with MESSAGES.
CALLBACK is (lambda (response error) ...) called when complete.
Builds the request from the shared command system prompt and the
current tools schema."
  (efrit-log 'debug "API call sending %d messages" (length messages))
  (require 'efrit-do)
  (let* ((system-prompt (efrit-do--command-system-prompt nil nil nil
                                                         session-id nil))
         (request-data
          `(("model" . ,efrit-default-model)
            ("max_tokens" . 8192)
            ("messages" . ,messages)
            ("system" . ,system-prompt)
            ("tools" . ,(efrit-do--get-current-tools-schema)))))
    ;; Call efrit-api-request-async directly rather than via
    ;; efrit-executor--api-request: the executor's error handler ends
    ;; the progress session generically and invokes the success
    ;; callback with a plain string, which misreported every API
    ;; failure (ef-2qx in the do-async loop, ef-ywc in the REPL loop).
    ;; The loop owns the session lifecycle.
    (efrit-api-request-async
     request-data
     (lambda (response)
       (if (and response (efrit-response-error response))
           (funcall callback nil (efrit-error-message
                                  (efrit-response-error response)))
         (funcall callback response nil)))
     (lambda (error-msg)
       (efrit-log 'error "API request failed: %s" error-msg)
       (funcall callback nil error-msg)))))

(defun efrit-loop-handle-response (session adapter response)
  "Handle API RESPONSE for SESSION using ADAPTER.
Streams text content to the agent buffer, then dispatches on the
response's stop_reason."
  (let* ((name (efrit-loop-adapter-name adapter))
         (session-id (funcall (efrit-loop-adapter-id-fn adapter) session)))
    (efrit-log 'debug "%s %s: received response" name session-id)
    ;; Content has arrived; stop the thinking indicator
    (when (and (efrit-loop-adapter-thinking-p adapter)
               (fboundp 'efrit-agent-hide-thinking))
      (efrit-agent-hide-thinking))
    (let ((content (efrit-response-content response))
          (stop-reason (efrit-response-stop-reason response)))
      (when content
        (efrit-loop--event adapter session-id 'message
                           `((:text . ,(format "%S" content))
                             (:role . "assistant")))
        ;; Stream text content to agent buffer
        (dotimes (i (length content))
          (let ((item (aref content i)))
            (when (and (hash-table-p item)
                       (string= (gethash "type" item) "text"))
              (when (fboundp 'efrit-agent-stream-content)
                (efrit-agent-stream-content (gethash "text" item))))))
        (when (fboundp 'efrit-agent-stream-end)
          (efrit-agent-stream-end)))
      (pcase stop-reason
        ("tool_use"
         (funcall (efrit-loop-adapter-execute-tools-fn adapter)
                  session content))
        ("end_turn"
         (efrit-log 'info "%s %s: Claude ended turn" name session-id)
         (when-let* ((fn (efrit-loop-adapter-on-end-turn-fn adapter)))
           (funcall fn session content))
         (efrit-loop--finish session adapter "end_turn"))
        (_
         (efrit-log 'warn "%s %s: unknown stop reason: %s"
                    name session-id stop-reason)
         (efrit-loop--finish session adapter "unknown-stop-reason"
                             (format "API returned unrecognized stop reason: %S"
                                     stop-reason)))))))

;;; Tool Execution

(defun efrit-loop-execute-tools (session adapter content)
  "Execute the tools Claude requested in CONTENT for SESSION via ADAPTER.
CONTENT is a vector of content blocks.  Stores the assistant response,
executes each tool_use block, collects tool_result blocks, and either
finishes the turn (session_complete, waiting-for-user) or continues
the loop."
  (let* ((name (efrit-loop-adapter-name adapter))
         (session-id (funcall (efrit-loop-adapter-id-fn adapter) session))
         (results nil)
         (session-complete-requested nil)
         (completion-message nil)
         (waiting-for-user nil))
    ;; CRITICAL: Store Claude's full response BEFORE executing tools
    ;; This maintains proper message order for API continuation
    (funcall (efrit-loop-adapter-add-assistant-fn adapter) session content)
    (dotimes (i (length content))
      (let* ((item (aref content i))
             (tool-use-info (efrit-content-item-as-tool-use item)))
        ;; Execute tool_use blocks only (returns nil if not a tool_use)
        (when tool-use-info
          (let* ((tool-id (nth 0 tool-use-info))
                 (tool-name (nth 1 tool-use-info))
                 (input (nth 2 tool-use-info)))
            (efrit-log 'debug "%s %s: executing tool %s (id: %s)"
                       name session-id tool-name tool-id)
            (when-let* ((fn (efrit-loop-adapter-record-tool-fn adapter)))
              (funcall fn session tool-name))
            (efrit-loop--event adapter session-id 'tool_started
                               `((:tool . ,tool-name) (:input . ,input)))
            ;; Show tool start in agent buffer and track time
            (let ((agent-tool-id (when (fboundp 'efrit-agent-show-tool-start)
                                   (efrit-agent-show-tool-start tool-name input)))
                  (tool-start-time (current-time)))
              (let* ((tool-result (efrit-loop--execute-single-tool
                                   session adapter tool-id tool-name input))
                     ;; Check if result contains session_complete signal
                     (is-session-complete
                      (string-match-p "\\[SESSION-COMPLETE:" tool-result))
                     (is-waiting
                      (and (efrit-loop-adapter-handles-waiting-p adapter)
                           (string-match-p "\\[WAITING-FOR-USER\\]" tool-result)))
                     ;; Check if result indicates an error
                     (is-error (string-match-p "^Error " tool-result))
                     (elapsed-secs (float-time (time-subtract (current-time)
                                                              tool-start-time))))
                (efrit-loop--event adapter session-id 'tool_result
                                   `((:tool . ,tool-name)
                                     (:result . ,tool-result)
                                     (:success . ,(not is-error))))
                (when (and agent-tool-id (fboundp 'efrit-agent-show-tool-result))
                  (efrit-agent-show-tool-result agent-tool-id tool-result
                                                (not is-error) elapsed-secs))
                ;; Successful dispatches are already work-logged with
                ;; their full input by efrit-do--execute-tool; logging
                ;; here too double-counted every step (ef-c1h).  Only
                ;; dispatch failures, which never reach that point.
                (when is-error
                  (when-let* ((fn (efrit-loop-adapter-on-tool-error-fn adapter)))
                    (funcall fn session tool-result tool-name)))
                ;; Collect result for API call with error flag
                (push (efrit-api-build-tool-result tool-id tool-result is-error)
                      results)
                ;; Mark if session_complete was requested, keeping
                ;; Claude's final message for the user (ef-ter).  The
                ;; match must span newlines: messages are often
                ;; multi-line.
                (when is-session-complete
                  (setq session-complete-requested t)
                  (when (string-match
                         "\\[SESSION-COMPLETE: \\(\\(?:.\\|\n\\)*\\)\\]"
                         tool-result)
                    (setq completion-message
                          (match-string 1 tool-result))))
                (when is-waiting
                  (setq waiting-for-user t))))))))
    (efrit-log 'info "%s %s: executed %d tools" name session-id (length results))
    ;; Send results back to Claude if any tools were executed
    (when results
      (funcall (efrit-loop-adapter-add-tool-results-fn adapter)
               session (nreverse results)))
    (cond
     (session-complete-requested
      (efrit-log 'info "%s %s: session_complete signal detected, stopping loop"
                 name session-id)
      (efrit-loop--finish session adapter "session-complete"
                          nil completion-message))
     ;; request_user_input pauses the turn; the user's answer arrives
     ;; via the next continue call (ef-dcn).
     (waiting-for-user
      (efrit-loop--finish session adapter "waiting-for-user"))
     (t
      (funcall (efrit-loop-adapter-continue-fn adapter) session)))))

(defun efrit-loop--execute-single-tool (session adapter tool-id tool-name input)
  "Execute single TOOL-NAME with INPUT for SESSION via ADAPTER.
TOOL-ID is the tool use ID from Claude's response.  Wraps execution
with error handling and validation.  Returns the tool result string.
Error messages start with `Error ' for easy detection by callers."
  (let ((name (efrit-loop-adapter-name adapter))
        (session-id (funcall (efrit-loop-adapter-id-fn adapter) session)))
    (require 'efrit-do)
    (condition-case err
        (progn
          ;; Validate input structure
          (unless (hash-table-p input)
            (error "Tool input must be a hash table, got %s" (type-of input)))
          (let ((tool-item (make-hash-table :test 'equal)))
            (puthash "id" tool-id tool-item)
            (puthash "name" tool-name tool-item)
            (puthash "input" input tool-item)
            ;; Execute the tool, letting the adapter establish dynamic
            ;; context around dispatch (e.g. the REPL tool session)
            (let* ((wrap (efrit-loop-adapter-wrap-dispatch-fn adapter))
                   (dispatch (lambda () (efrit-do--execute-tool tool-item)))
                   (result (if wrap
                               (funcall wrap session dispatch)
                             (funcall dispatch))))
              ;; Validate result is a string
              (unless (stringp result)
                (setq result (format "%S" result)))
              (efrit-log 'debug "%s %s: tool %s returned %d chars"
                         name session-id tool-name (length result))
              result)))
      (error
       (let* ((error-msg (error-message-string err))
              (is-interrupt (string-match-p "Quit" error-msg))
              ;; An error signal escaping the dispatch means efrit's own
              ;; machinery failed, not the model's tool input — say so,
              ;; or the model burns turns debugging efrit (ef-hn6).
              ;; Must keep the "Error " prefix: callers detect errors
              ;; via (string-match-p "^Error " result).
              (formatted-error (format "Error inside efrit's dispatch of %s (not caused by your tool input): %s. Do not debug efrit internals. Retry the tool call once; if the error persists, stop and report it to the user."
                                       tool-name error-msg)))
         (if is-interrupt
             (efrit-log 'warn "%s %s: user interrupted tool execution"
                        name session-id)
           (efrit-log 'error "%s %s: %s" name session-id formatted-error))
         formatted-error)))))

(provide 'efrit-loop)

;;; efrit-loop.el ends here
