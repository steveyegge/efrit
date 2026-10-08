;;; efrit-loop.el --- Shared agentic loop engine -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.10.3
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
(require 'efrit-permissions)
(require 'efrit-sandbox)
(require 'efrit-review)
(require 'efrit-limits)
(declare-function efrit-review-confidence-remember-grant "efrit-review-confidence")
(defvar efrit-review--last-vouch)
(require 'efrit-events)
(require 'efrit-result-struct)

(declare-function efrit-api-stream-request "efrit-api-stream")
(defvar efrit-api-streaming)
(defvar efrit-default-model)
(defvar efrit-default-max-tokens)

(defvar efrit-loop--streamed-text nil
  "Dynamically non-nil while handling a response whose text was already streamed.")

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
  before-request-fn   ; (session) called after tool results are stored, before the
                      ; next request goes out: the seam for steering text, or nil.
  system-prompt-fn    ; (session-id) -> system prompt string.
  tools-fn            ; () -> tools schema for the request.
  dispatch-fn         ; (tool-item) -> result string; runs one tool.
  api-call-fn         ; Symbol: (session messages callback) async API call.
  continue-fn         ; Symbol: (session) re-enter the iteration loop.
  execute-tools-fn    ; Symbol: (session content) execute requested tools.
  on-api-error-fn     ; Symbol: (session error) terminal API-error path.
  finish-fn           ; (session reason error-message completion-message) terminal.
  set-pending-question-fn) ; (session question options meta) record a question the
                           ; loop itself asks (review escalation), or nil.

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
  ;; A "continue once" raise of a limit lasts for this turn only.
  ;; This session's project only: another session's raise stands.
  (efrit-limits-reset-once (efrit-limits-project-root))
  (when (fboundp 'efrit-reconnect-reset)
    (efrit-reconnect-reset (funcall (efrit-loop-adapter-id-fn adapter) session)))
  (efrit-publish 'turn-complete
                 `((:session-id . ,(funcall (efrit-loop-adapter-id-fn adapter) session))
                   (:stop-reason . ,reason)
                   (:error-message . ,error-message)
                   (:completion-message . ,completion-message)))
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
             (timeout (efrit-limits-effective
                       'session-timeout (funcall (efrit-loop-adapter-timeout-fn adapter))))
             (elapsed (funcall (efrit-loop-adapter-elapsed-fn adapter) session))
             (max-iterations (efrit-limits-effective
                              'max-iterations
                              (funcall (efrit-loop-adapter-max-iterations-fn adapter)))))
        ;; At the cap, ask before giving up: a long task is not a
        ;; runaway loop.  A raise (once / session / project) changes
        ;; the effective limit and the check below then passes.
        (when (and (> max-iterations 0) (>= iteration-count max-iterations))
          (when-let* ((raised (efrit-limits-ask-to-raise 'max-iterations max-iterations)))
            (efrit-log 'info "%s %s: iteration limit raised to %d" name session-id raised)
            (setq max-iterations raised)))
        ;; The wall clock asks the same way (2026-10-01: a hard stop at
        ;; 300 s ended turns that were making progress)
        (when (and timeout (> timeout 0) elapsed (>= elapsed timeout))
          (when-let* ((raised (efrit-limits-ask-to-raise 'session-timeout timeout)))
            (efrit-log 'info "%s %s: time limit raised to %d s" name session-id raised)
            (setq timeout raised)))
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
          (efrit-publish 'api-request `((:session-id . ,session-id)
                                        (:iteration . ,(1+ iteration-count))))
          (efrit-loop--send-request session adapter)))))))

;;; Request / Response

(defun efrit-loop--send-request (session adapter)
  "Send the next API request for SESSION via ADAPTER's api-call function."
  (let ((messages (funcall (efrit-loop-adapter-messages-fn adapter) session)))
    ;; Animated indicator while the API call is in flight
    (when (efrit-loop-adapter-thinking-p adapter)
      (efrit-publish 'thinking-start
                     `((:session-id . ,(funcall (efrit-loop-adapter-id-fn adapter) session))
                       (:label . "waiting for Claude..."))))
    ;; An error while building the request (system prompt, tools
    ;; schema) used to escape here after thinking-start was published:
    ;; the buffer showed a frozen spinner and the session stayed
    ;; `working' for good.  Fail the turn visibly instead.
    (condition-case err
        (efrit-loop--call-api session adapter messages)
      (error
       (let ((msg (format "Internal error building the request (efrit bug, not an API failure): %s"
                          (error-message-string err))))
         (efrit-log 'error "%s %s: %s" (efrit-loop-adapter-name adapter)
                    (funcall (efrit-loop-adapter-id-fn adapter) session) msg)
         (efrit-loop--finish session adapter "elisp-error" msg))))))

(defun efrit-loop--call-api (session adapter messages)
  "Call ADAPTER's api-call function for SESSION with MESSAGES.
A failure in transit is handed to efrit-reconnect, which retries the
same request after a pause and asks the user when its budget is spent;
the turn's work stays in the session.  Other errors end the turn."
  (funcall (efrit-loop-adapter-api-call-fn adapter)
     session
     messages
     (lambda (response error)
       (if error
           (let ((session-id (funcall (efrit-loop-adapter-id-fn adapter) session)))
             (unless (and (require 'efrit-reconnect nil t)
                          (efrit-reconnect-handle-failure
                           session-id error
                           ;; retry: the same messages, under the session's bindings
                           (lambda ()
                             (when (efrit-loop--session-active-p session adapter)
                               (efrit-loop--run-as-session
                                session adapter
                                (lambda () (efrit-loop--send-request session adapter)))))
                           ;; ask: pause the turn on the connectivity question
                           (lambda () (efrit-loop--ask-about-connection session adapter error))))
               (efrit-reconnect-reset session-id)
               (funcall (efrit-loop-adapter-on-api-error-fn adapter) session error)))
         ;; An uncaught elisp error here would silently kill the loop
         ;; (see 098182c): catch it and fail the turn visibly.  Label
         ;; it as a client-side bug, not an API failure -- relabeling
         ;; these as API errors sent debugging down the wrong path
         ;; (ef-jz6).
         (when (fboundp 'efrit-reconnect-reset)
           (efrit-reconnect-reset (funcall (efrit-loop-adapter-id-fn adapter) session)))
         (condition-case err
             (efrit-loop-handle-response session adapter response)
           (error
            (let ((msg (format "Internal error handling response (efrit bug, not an API failure): %s"
                               (error-message-string err))))
              (efrit-log 'error "%s %s: %s"
                         (efrit-loop-adapter-name adapter)
                         (funcall (efrit-loop-adapter-id-fn adapter) session)
                         msg)
              (efrit-loop--finish session adapter "elisp-error" msg))))))))

(defvar efrit-loop--mark-waiting-function nil
  "Function (SESSION) that puts SESSION in the waiting state without ending its loop.
Set by the REPL adapter; nil for adapters that cannot pause.")

(declare-function efrit-reconnect-handle-failure "efrit-reconnect")
(declare-function efrit-reconnect-reset "efrit-reconnect")
(declare-function efrit-reconnect--short "efrit-reconnect")
(declare-function efrit-reconnect-answer "efrit-reconnect")
(defvar efrit-reconnect-question-options)

(defun efrit-loop--session-active-p (session adapter)
  "Non-nil while SESSION's loop entry still exists (the turn was not ended)."
  (gethash (funcall (efrit-loop-adapter-id-fn adapter) session)
           (efrit-loop-adapter-state-hash adapter)))

(defun efrit-loop--run-as-session (session adapter thunk)
  "Run THUNK as SESSION's code when ADAPTER wraps dispatch, else plainly."
  (if-let* ((wrap (efrit-loop-adapter-wrap-dispatch-fn adapter)))
      (funcall wrap session thunk)
    (funcall thunk)))

(defun efrit-loop--ask-about-connection (session adapter error)
  "Pause SESSION's turn on the keep-waiting / retry / abort question.
Through the `question' path, so the agent buffer shows the menu and
the answer is routed by `efrit-reconnect-answer'; nothing but the
user's key (or unattended mode) answers it."
  (let* ((session-id (funcall (efrit-loop-adapter-id-fn adapter) session))
         (question (format "The connection to the model keeps failing (%s).\nThe turn's work so far is kept. What now?"
                           (efrit-reconnect--short error)))
         (options efrit-reconnect-question-options))
    (cond
     ;; away: keep waiting, as the user would answer; the turn resumes alone
     ((bound-and-true-p efrit-unattended-mode)
      (efrit-publish 'prompt-answered-unattended
                     `((:label . "the connection question") (:answer . "wait")
                       (:why . "unattended: waiting for the connection; the turn resumes when it is back")))
      (efrit-reconnect-answer session-id "Keep waiting"))
     ((and (efrit-loop-adapter-handles-waiting-p adapter) efrit-loop--mark-waiting-function)
      (when-let* ((fn (efrit-loop-adapter-set-pending-question-fn adapter)))
        (funcall fn session question options (list :kind 'reconnect :error error)))
      (efrit-publish 'question `((:session-id . ,session-id) (:question . ,question)
                                 (:options . ,options) (:kind . reconnect)))
      ;; the loop entry stays: the retry thunk re-enters this turn
      (efrit-publish 'turn-complete `((:session-id . ,session-id) (:stop-reason . "waiting-for-user")))
      (efrit-publish 'status `((:session-id . ,session-id) (:status . waiting)))
      (funcall efrit-loop--mark-waiting-function session))
     ;; an adapter that cannot pause (efrit-do): end as before
     (t (efrit-reconnect-reset session-id)
        (efrit-loop--finish session adapter "api-error"
                            (format "the connection to the model failed repeatedly: %s" error))))))



(defun efrit-loop-api-call (session-id messages callback adapter)
  "Make the canonical async Claude API call for SESSION-ID with MESSAGES.
CALLBACK is (lambda (response error) ...) called when complete.
The system prompt and the tools schema come from ADAPTER, so this
engine has no view of which interface owns them."
  (efrit-log 'debug "API call sending %d messages" (length messages))
  (let* ((efrit-api-request-purpose
          (format "the model's next turn (session %s, %d messages)"
                  (truncate-string-to-width (format "%s" session-id) 12 nil nil "…")
                  (length messages)))
         (system-prompt (funcall (efrit-loop-adapter-system-prompt-fn adapter) session-id))
         (request-data
          `(("model" . ,efrit-default-model)
            ("max_tokens" . ,efrit-default-max-tokens)
            ("messages" . ,(efrit-api-cacheable-messages messages))
            ("system" . ,(efrit-api-cacheable-system system-prompt))
            ("tools" . ,(efrit-api-cacheable-tools
                         (funcall (efrit-loop-adapter-tools-fn adapter)))))))
    ;; Call efrit-api-request-async directly rather than via
    ;; efrit-executor--api-request: the executor's error handler ends
    ;; the progress session generically and invokes the success
    ;; callback with a plain string, which misreported every API
    ;; failure (ef-2qx in the do-async loop, ef-ywc in the REPL loop).
    ;; The loop owns the session lifecycle.
    (if (and (bound-and-true-p efrit-api-streaming)
             (require 'efrit-api-stream nil t))
        ;; Streaming transport: text deltas go straight to the agent
        ;; buffer; the assembled response then takes the normal path.
        ;; efrit-loop--streamed-text remembers what was already shown
        ;; so efrit-loop-handle-response doesn't render it twice.
        (let ((shown nil))
          (efrit-api-stream-request
           request-data
           (lambda (response error)
             (when (and response (efrit-response-error response))
               (setq error (efrit-error-message (efrit-response-error response))
                     response nil))
             (when error (efrit-log 'error "API request failed: %s" error))
             (let ((efrit-loop--streamed-text shown))
               (funcall callback response error)))
           (lambda (text)
             (setq shown t)
             (efrit-publish 'thinking-stop `((:session-id . ,session-id)))
             (efrit-publish 'text-delta `((:session-id . ,session-id) (:text . ,text))))))
      (efrit-api-request-async
       request-data
       (lambda (response)
         (if (and response (efrit-response-error response))
             (funcall callback nil (efrit-error-message
                                    (efrit-response-error response)))
           (funcall callback response nil)))
       (lambda (error-msg)
         (efrit-log 'error "API request failed: %s" error-msg)
         (funcall callback nil error-msg))))))

(defun efrit-loop--mark-partial (content)
  "CONTENT with a trailing note in its last text block: the answer was cut.
The model reads its own history next turn; without the note it would
continue as if it had finished."
  (let ((out (copy-sequence content)) (marked nil))
    (cl-loop for i from (1- (length out)) downto 0
             for item = (aref out i)
             when (and (not marked) (hash-table-p item) (equal (gethash "type" item) "text"))
             do (let ((blk (copy-hash-table item)))
                  (puthash "text" (concat (gethash "text" blk) "\n[answer cut short here]") blk)
                  (aset out i blk)
                  (setq marked t)))
    out))

;;; Stall detection
;;
;; A turn that keeps calling the model and getting the same thing back
;; is stuck; a turn that keeps running tools and producing new text is
;; working, however long it takes.  Progress, not wall-clock time, is
;; the signal (tzz, 2026-10-01).  A round counts as progress when it
;; carries a tool call or text the turn has not produced before.

(defcustom efrit-loop-stall-rounds 4
  "API rounds in a row without a tool call or new text before the turn is stalled.
0 disables.  Identical tool calls are the circuit breaker's business."
  :type 'integer
  :group 'efrit)

(defvar efrit-loop--stall (make-hash-table :test 'equal)
  "Session id -> (ROUNDS-WITHOUT-PROGRESS . SEEN-TEXTS) for the current turn.")

(defun efrit-loop-stall-reset (session-id)
  "Forget SESSION-ID's stall record (a turn begins or ends)."
  (remhash session-id efrit-loop--stall))

(defun efrit-loop--note-progress (session-id content)
  "Record what CONTENT brought for SESSION-ID; return rounds without progress."
  (let* ((rec (or (gethash session-id efrit-loop--stall)
                  (puthash session-id (cons 0 nil) efrit-loop--stall)))
         (tool nil) (new-text nil))
    (dotimes (i (length content))
      (let ((item (aref content i)))
        (when (hash-table-p item)
          (pcase (gethash "type" item)
            ("tool_use" (setq tool t))
            ("text" (let ((text (string-trim (or (gethash "text" item) ""))))
                      (when (and (not (string-empty-p text))
                                 (not (member text (cdr rec))))
                        (setq new-text t)
                        (push text (cdr rec)))))))))
    (if (or tool new-text)
        (setcar rec 0)
      (setcar rec (1+ (car rec))))
    (car rec)))

(defun efrit-loop--stalled-p (session-id content)
  "Non-nil when CONTENT makes `efrit-loop-stall-rounds' rounds without progress."
  (and (> efrit-loop-stall-rounds 0)
       (>= (efrit-loop--note-progress session-id content) efrit-loop-stall-rounds)))

(defun efrit-loop-handle-response (session adapter response)
  "Handle API RESPONSE for SESSION using ADAPTER.
Streams text content to the agent buffer, then dispatches on the
response's stop_reason."
  (let* ((name (efrit-loop-adapter-name adapter))
         (session-id (funcall (efrit-loop-adapter-id-fn adapter) session)))
    (efrit-log 'debug "%s %s: received response" name session-id)
    ;; Content has arrived; stop the thinking indicator
    (when (efrit-loop-adapter-thinking-p adapter)
      (efrit-publish 'thinking-stop `((:session-id . ,session-id))))
    (when-let* ((usage (efrit-response-usage response)))
      (efrit-log 'debug "%s %s: usage in=%s out=%s cache_write=%s cache_read=%s"
                 name session-id
                 (gethash "input_tokens" usage)
                 (gethash "output_tokens" usage)
                 (gethash "cache_creation_input_tokens" usage)
                 (gethash "cache_read_input_tokens" usage)))
    (efrit-publish 'api-response
                   `((:session-id . ,session-id)
                     (:usage . ,(efrit-response-usage response))
                     (:stop-reason . ,(efrit-response-stop-reason response))))
    (let ((content (efrit-response-content response))
          (stop-reason (efrit-response-stop-reason response)))
      (when content
        (efrit-loop--event adapter session-id 'message
                           `((:text . ,(format "%S" content))
                             (:role . "assistant")))
        ;; Render text content in the agent buffer -- unless the
        ;; streaming transport already did, delta by delta
        (unless efrit-loop--streamed-text
          (dotimes (i (length content))
            (let ((item (aref content i)))
              (when (and (hash-table-p item)
                         (string= (gethash "type" item) "text"))
                (efrit-publish 'text-delta `((:session-id . ,session-id)
                                             (:text . ,(gethash "text" item))))))))
        (efrit-publish 'text-end `((:session-id . ,session-id))))
      (pcase stop-reason
        ;; no tool call and nothing new said, several rounds running:
        ;; the model is going round in circles; stop and say so
        ((guard (and content (efrit-loop--stalled-p session-id content)))
         (efrit-log 'warn "%s %s: no progress in %d rounds; stalled"
                    (efrit-loop-adapter-name adapter) session-id efrit-loop-stall-rounds)
         (funcall (efrit-loop-adapter-add-assistant-fn adapter) session content)
         (efrit-loop--finish session adapter "stalled"
                             (format "No progress in %d API rounds (no tool call, no new text); the turn was stopped. Say how to proceed."
                                     efrit-loop-stall-rounds)))
        ("tool_use"
         (if (efrit-review-applies-p content (funcall (efrit-loop-adapter-id-fn adapter) session))
             (efrit-loop--review-then-execute session adapter content)
           ;; Say why there is no review row, so a missing one is
           ;; never mistaken for a missed review
           (when-let* ((why (efrit-review-skip-reason content)))
             (efrit-publish 'review-skipped `((:session-id . ,session-id) (:reason . ,why))))
           (funcall (efrit-loop-adapter-execute-tools-fn adapter)
                    session content)))
        ("end_turn"
         (cond
          ;; The user cancelled mid-answer: what arrived is kept in
          ;; the history, marked, and the turn ends as interrupted
          ((efrit-response-cancelled-p response)
           (efrit-log 'info "%s %s: turn cancelled; partial answer kept" name session-id)
           (efrit-publish 'note
                          `((:session-id . ,session-id)
                            (:kind . interrupted) (:face . warning)
                            (:text . "⏹ cancelled; the answer above is incomplete")))
           (when-let* ((fn (efrit-loop-adapter-on-end-turn-fn adapter)))
             (funcall fn session (efrit-loop--mark-partial content)))
           (efrit-loop--finish session adapter "interrupted"))
          ((efrit-response-partial-p response)
           (efrit-log 'warn "%s %s: stream ended early; partial answer kept" name session-id)
           (efrit-publish 'note
                          `((:session-id . ,session-id)
                            (:kind . interrupted) (:face . warning)
                            (:text . "⏹ the stream ended early; the answer above is incomplete")))
           (when-let* ((fn (efrit-loop-adapter-on-end-turn-fn adapter)))
             (funcall fn session (efrit-loop--mark-partial content)))
           (efrit-loop--finish session adapter "end_turn"))
          (t
           (efrit-log 'info "%s %s: Claude ended turn" name session-id)
           (when-let* ((fn (efrit-loop-adapter-on-end-turn-fn adapter)))
             (funcall fn session content))
           (efrit-loop--finish session adapter "end_turn"))))
        ;; The answer was cut at `efrit-default-max-tokens'.  It is
        ;; still an answer: end the turn as usual and say what happened,
        ;; so the user can ask for the rest or raise the limit.
        ("max_tokens"
         (efrit-log 'warn "%s %s: answer cut at max_tokens (%s)"
                    name session-id efrit-default-max-tokens)
         (efrit-publish 'note
                        `((:session-id . ,session-id)
                          (:kind . limits) (:face . warning)
                          (:text . ,(format "⏱ the answer was cut at %s output tokens (efrit-default-max-tokens); ask for the rest, or raise the limit"
                                            efrit-default-max-tokens))))
         (when-let* ((fn (efrit-loop-adapter-on-end-turn-fn adapter)))
           (funcall fn session content))
         (efrit-loop--finish session adapter "end_turn"))
        (_
         (efrit-log 'warn "%s %s: unknown stop reason: %s"
                    name session-id stop-reason)
         (efrit-loop--finish session adapter "unknown-stop-reason"
                             (format "API returned unrecognized stop reason: %S"
                                     stop-reason)))))))

;;; Review before execution

(defun efrit-loop--review-then-execute (session adapter content)
  "Have the reviewer judge CONTENT's mutating tool calls, then act on the verdict.
On approve, execute the tools as usual.  On reject, record the
assistant turn and a rejection tool_result for every tool_use in it
\(the API needs one per call), then continue the loop so the proposer
can revise; after `efrit-review-max-rejections' consecutive rejections
the turn is handed to the user instead."
  (let* ((name (efrit-loop-adapter-name adapter))
         (session-id (funcall (efrit-loop-adapter-id-fn adapter) session))
         (messages (funcall (efrit-loop-adapter-messages-fn adapter) session)))
    (when (efrit-loop-adapter-thinking-p adapter)
      (efrit-publish 'thinking-start `((:session-id . ,session-id) (:label . "reviewing..."))))
    (efrit-review-turn
     session-id messages content
     (lambda (verdict)
       (when (efrit-loop-adapter-thinking-p adapter)
         (efrit-publish 'thinking-stop `((:session-id . ,session-id))))
       (let ((count (efrit-review-note-verdict session-id (car verdict)
                                               (efrit-review-batch-signature content))))
         (pcase (car verdict)
           ('approve
            ;; the reviewer may have vouched for requests the sandbox
            ;; would otherwise ask the user about (0.10.3)
            (when-let* ((vouch (and (boundp 'efrit-review--last-vouch) efrit-review--last-vouch)))
              (setq efrit-review--last-vouch nil)
              (when (require 'efrit-review-confidence nil t)
                (dotimes (i (length content))
                  (when-let* ((use (efrit-content-item-as-tool-use (aref content i))))
                    (efrit-review-confidence-remember-grant
                     use (plist-get vouch :confidence) (plist-get vouch :grant))))))
            (funcall (efrit-loop-adapter-execute-tools-fn adapter) session content))
           ('reject
            (let ((reason (cdr verdict))
                  (results nil))
              (efrit-log 'info "%s %s: reviewer rejected turn (%d consecutive)"
                         name session-id count)
              (funcall (efrit-loop-adapter-add-assistant-fn adapter) session content)
              (dotimes (i (length content))
                (when-let* ((use (efrit-content-item-as-tool-use (aref content i))))
                  (let ((text (if (efrit-review--reviewable-p (nth 1 use))
                                  (efrit-review-rejected-tool-result reason)
                                (concat efrit-review-rejected-prefix
                                        "not run: another call in this batch was rejected."))))
                    (efrit-loop--event adapter session-id 'tool_result
                                       `((:tool . ,(nth 1 use)) (:result . ,text)
                                         (:success . nil)))
                    ;; the rejected call shows as a failed tool row
                    (efrit-publish 'tool-start `((:session-id . ,session-id)
                                                 (:tool-id . ,(nth 0 use))
                                                 (:tool . ,(nth 1 use)) (:input . ,(nth 2 use))))
                    (efrit-publish 'tool-result `((:session-id . ,session-id)
                                                  (:tool-id . ,(nth 0 use))
                                                  (:tool . ,(nth 1 use)) (:result . ,text)
                                                  (:success . nil) (:elapsed . 0)))
                    (push (efrit-api-build-tool-result (nth 0 use) text t) results))))
              (funcall (efrit-loop-adapter-add-tool-results-fn adapter)
                       session (nreverse results))
              (cond
               ;; The model keeps proposing what the reviewer keeps
               ;; rejecting: it is not taking the advice.  Hand the
               ;; disagreement to the user as a question, the way
               ;; request_user_input does, and pause; the answer is the
               ;; model's next input.  Ending the turn here threw away
               ;; the work in progress (tzz, 2026-10-01: "advise the
               ;; model, don't kill the whole job").
               ;; away: drop it and go on, as the user would answer
               ((and (>= count efrit-review-max-rejections)
                     (bound-and-true-p efrit-unattended-mode))
                (efrit-log 'info "%s %s: review disagreement after %d rejections; unattended, dropping it"
                           name session-id count)
                (efrit-publish 'prompt-answered-unattended
                               `((:label . "the review escalation") (:answer . "drop")
                                 (:why . ,(format "unattended: the reviewer rejected the same proposal %d times; dropped, the task goes on" count))))
                (efrit-review-forget-session session-id)
                (funcall (efrit-loop-adapter-continue-fn adapter) session))
               ((and (>= count efrit-review-max-rejections)
                     (efrit-loop-adapter-handles-waiting-p adapter))
                (efrit-loop--escalate-review session adapter content count reason))
               ;; an adapter that cannot pause (efrit-do): the old end
               ((>= count efrit-review-max-rejections)
                (efrit-loop--finish
                 session adapter "review-rejected"
                 (format "The reviewer rejected the same proposal %d times this turn. Last reason: %s"
                         count (or reason "none given"))))
               (t (funcall (efrit-loop-adapter-continue-fn adapter) session)))))))))))

(declare-function efrit-review-describe-batch "efrit-review")
(declare-function efrit-review-batch-signature "efrit-review")
(defvar efrit-unattended-mode)

(defun efrit-loop--escalate-review (session adapter content count reason)
  "Ask the user about a proposal the reviewer rejected COUNT times; pause the turn.
The question names the batch and the last REASON; the options let the
user allow it once, tell the model to drop it, or type their own
steer.  Delivered through the `question' path the REPL already has,
so the agent buffer shows the menu and the next input is the answer."
  (let* ((session-id (funcall (efrit-loop-adapter-id-fn adapter) session))
         (batch (efrit-review-describe-batch content))
         (question (format "The reviewer rejected this %d times: %s\nLast reason: %s\nWhat should the model do?"
                           count (string-trim batch) (or reason "none given")))
         (options '("Allow it this once" "Drop it and continue the task" "Stop here")))
    (efrit-log 'info "%s %s: review disagreement escalated to the user after %d rejections"
               (efrit-loop-adapter-name adapter) session-id count)
    ;; the REPL session remembers what was asked, so the answer can
    ;; be turned into an allow (see `efrit-review-answer-escalation')
    (when-let* ((fn (efrit-loop-adapter-set-pending-question-fn adapter)))
      (funcall fn session question options
               (list :kind 'review :signature (efrit-review-batch-signature content)
                     :count count :reason reason)))
    (efrit-publish 'question `((:session-id . ,session-id) (:question . ,question)
                               (:options . ,options) (:kind . review)))
    (efrit-publish 'review-escalated `((:session-id . ,session-id) (:count . ,count)
                                       (:reason . ,reason) (:batch . ,batch)))
    (efrit-loop--finish session adapter "waiting-for-user")))

;;; Tool Execution

(defconst efrit-loop--interrupt-result
  "Error tool execution interrupted by user (C-g)"
  "Text of the tool_result recorded when C-g aborts a tool (ef-lx4c).
The API conversation stays well-formed; the result's status is
`interrupted' and the loop ends the turn instead of continuing.")

(defun efrit-loop-execute-tools (session adapter content)
  "Execute the tools Claude requested in CONTENT for SESSION via ADAPTER.
CONTENT is a vector of content blocks.  Stores the assistant response,
executes each tool_use block, collects tool_result blocks, and either
finishes the turn (session_complete, waiting-for-user, user C-g) or
continues the loop."
  (let* ((name (efrit-loop-adapter-name adapter))
         (session-id (funcall (efrit-loop-adapter-id-fn adapter) session))
         (results nil)
         (session-complete-requested nil)
         (completion-message nil)
         (waiting-for-user nil)
         (user-interrupted nil))
    ;; CRITICAL: Store Claude's full response BEFORE executing tools
    ;; This maintains proper message order for API continuation
    (funcall (efrit-loop-adapter-add-assistant-fn adapter) session content)
    (dotimes (i (length content))
      (let* ((item (aref content i))
             (tool-use-info (efrit-content-item-as-tool-use item)))
        ;; Execute tool_use blocks only (returns nil if not a tool_use)
        (when tool-use-info
          (if user-interrupted
              ;; A C-g already aborted this turn: don't run further
              ;; tools, but still emit a tool_result for every
              ;; remaining tool_use so the conversation stays valid.
              (push (efrit-api-build-tool-result
                     (nth 0 tool-use-info)
                     "Error skipped: the turn was ended by the user (C-g)" t)
                    results)
            (let* ((tool-id (nth 0 tool-use-info))
                 (tool-name (nth 1 tool-use-info))
                 (input (nth 2 tool-use-info)))
            (efrit-log 'debug "%s %s: executing tool %s (id: %s)"
                       name session-id tool-name tool-id)
            (when-let* ((fn (efrit-loop-adapter-record-tool-fn adapter)))
              (funcall fn session tool-name))
            (efrit-loop--event adapter session-id 'tool_started
                               `((:tool . ,tool-name) (:input . ,input)))
            (efrit-publish 'tool-start `((:session-id . ,session-id)
                                         (:tool-id . ,tool-id)
                                         (:tool . ,tool-name) (:input . ,input)))
            (let ((tool-start-time (current-time)))
              (let* ((result (efrit-loop--execute-single-tool
                              session adapter tool-id tool-name input))
                     (tool-result (efrit-tool-result-text result))
                     (is-session-complete (efrit-tool-result-complete-p result))
                     (is-waiting (and (efrit-loop-adapter-handles-waiting-p adapter)
                                      (efrit-tool-result-waiting-p result)))
                     (is-error (efrit-tool-result-error-p result))
                     (elapsed-secs (float-time (time-subtract (current-time)
                                                              tool-start-time))))
                (efrit-loop--event adapter session-id 'tool_result
                                   `((:tool . ,tool-name)
                                     (:result . ,tool-result)
                                     (:success . ,(not is-error))))
                (efrit-publish 'tool-result `((:session-id . ,session-id)
                                              (:tool-id . ,tool-id)
                                              (:tool . ,tool-name)
                                              (:result . ,tool-result)
                                              (:success . ,(not is-error))
                                              (:elapsed . ,elapsed-secs)))
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
                ;; session_complete keeps Claude's final message for
                ;; the user (ef-ter); the dispatcher extracted it
                (when is-session-complete
                  (setq session-complete-requested t
                        completion-message (efrit-tool-result-message result)))
                ;; Only a C-g mid-tool hands control back to the user.
                ;; A sandbox or permission denial is an ordinary failed
                ;; tool result: the model reads it and carries on, as
                ;; it would after a failed command.
                (when (efrit-tool-result-interrupted-p result)
                  (setq user-interrupted t))
                (when is-waiting
                  (setq waiting-for-user t)))))))))
    (efrit-log 'info "%s %s: executed %d tools" name session-id (length results))
    ;; Send results back to Claude if any tools were executed
    (when results
      (funcall (efrit-loop-adapter-add-tool-results-fn adapter)
               session (nreverse results)))
    ;; Steering: what the user said while the tools ran goes out with
    ;; their results, so the model reads it before its next step.
    (when-let* ((fn (efrit-loop-adapter-before-request-fn adapter)))
      (funcall fn session))
    (cond
     ;; A C-g mid-tool ends the turn cleanly (ef-lx4c): tool_results
     ;; are already recorded above, so the next user input can
     ;; continue the session.
     (user-interrupted
      (efrit-log 'info "%s %s: turn interrupted by user during tool execution"
                 name session-id)
      (efrit-loop--finish session adapter "interrupted"))
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
with error handling and validation.  Returns an `efrit-tool-result';
a dispatcher that still returns a string gets it wrapped as ok."
  (let ((name (efrit-loop-adapter-name adapter))
        (session-id (funcall (efrit-loop-adapter-id-fn adapter) session)))
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
                   (run (efrit-loop-adapter-dispatch-fn adapter))
                   (dispatch (lambda () (funcall run tool-item)))
                   (result (if wrap
                               (funcall wrap session dispatch)
                             (funcall dispatch))))
              (unless (efrit-tool-result-p result)
                (error "Dispatcher for %s returned %s, not an efrit-tool-result"
                       tool-name (type-of result)))
              (efrit-log 'debug "%s %s: tool %s returned %d chars (%s%s)"
                         name session-id tool-name (length (efrit-tool-result-text result))
                         (efrit-tool-result-status result)
                         (if (efrit-tool-result-signal result)
                             (format ", %s" (efrit-tool-result-signal result)) ""))
              result)))
      ;; C-g signals `quit', which an (error ...) handler does NOT
      ;; catch: it used to unwind the entire turn, leaving the UI
      ;; stuck on a running spinner and the session silently dead
      ;; (ef-lx4c).  Convert it into a distinguished tool result so
      ;; efrit-loop-execute-tools can end the turn cleanly.
      (quit
       (efrit-log 'warn "%s %s: user interrupted (C-g) during tool %s"
                  name session-id tool-name)
       (efrit-tool-result-fail efrit-loop--interrupt-result 'interrupted))
      (error
       (let* ((error-msg (error-message-string err))
              (is-interrupt (string-match-p "Quit" error-msg))
              ;; An error signal escaping the dispatch means efrit's own
              ;; machinery failed, not the model's tool input — say so,
              ;; or the model burns turns debugging efrit (ef-hn6).
              (formatted-error (format "Error inside efrit's dispatch of %s (not caused by your tool input): %s. Do not debug efrit internals. Retry the tool call once; if the error persists, stop and report it to the user."
                                       tool-name error-msg)))
         (if is-interrupt
             (efrit-log 'warn "%s %s: user interrupted tool execution"
                        name session-id)
           (efrit-log 'error "%s %s: %s" name session-id formatted-error))
         (efrit-tool-result-fail formatted-error
                                 (if is-interrupt 'interrupted 'error)))))))

(provide 'efrit-loop)

;;; efrit-loop.el ends here
