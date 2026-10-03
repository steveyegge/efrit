;;; efrit-repl-loop.el --- Async loop for REPL sessions -*- lexical-binding: t -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.8.5
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
(require 'efrit-tools)   ; efrit-tools--reset-rate-limits
(require 'efrit-sandbox)
(require 'efrit-diagnostics-baseline)
(require 'efrit-do-circuit-breaker)   ; efrit-do--circuit-breaker-reset
(require 'efrit-do-prompt)            ; efrit-do--command-system-prompt
(require 'efrit-do-schema)            ; efrit-do--get-current-tools-schema
(require 'efrit-do-dispatch)          ; efrit-do--execute-tool
(require 'efrit-do-handlers)          ; the handlers efrit-do-dispatch declares
(require 'efrit-context-sources)
(require 'efrit-events)
(defvar efrit-project-root)
(require 'efrit-models)

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

(defvar efrit-repl-loop--tool-session nil
  "The REPL session whose tool call is currently executing, if any.
Dynamically bound around tool dispatch so session-aware tool handlers
\(like request_user_input) can target the REPL session instead of
requiring an active efrit-do session (ef-dcn).")

;;; Engine Adapter

(defun efrit-repl-loop--system-prompt (session-id)
  "The command system prompt for SESSION-ID."
  (efrit-do--command-system-prompt nil nil nil session-id nil))

;; A defconst, not a defvar: the adapter is an instance of the
;; `efrit-loop-adapter' struct, and when a reload adds a slot to the
;; struct a defvar would keep the old, shorter instance while the new
;; accessors index past it (an efrit-reload after adding
;; system-prompt-fn produced "wrong number of arguments" from the
;; wrong slot).  A defconst is re-evaluated on every load.
(defconst efrit-repl-loop--adapter
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
                   ;; Minus the time the user spent on prompts and
                   ;; questions: a slow answer is not a slow turn
                   (efrit-elapsed-working
                    start (efrit-repl-session-turn-waiting-mark session))))
   :timeout-fn (lambda () efrit-session-timeout)
   :record-tool-fn #'efrit-repl-session-record-tool
   :wrap-dispatch-fn (lambda (session thunk)
                       (let ((efrit-repl-loop--tool-session session))
                         (efrit-repl-loop--with-session session thunk)))
   ;; The prompt, the schema and the dispatcher are the efrit-do
   ;; interface's; the engine only sees these three functions
   :system-prompt-fn #'efrit-repl-loop--system-prompt
   :tools-fn #'efrit-do--get-current-tools-schema
   :dispatch-fn #'efrit-do--execute-tool
   ;; The agent buffer is this loop's surface: show the activity
   ;; indicator while a request is in flight.
   :thinking-p t
   :handles-waiting-p t
   :set-pending-question-fn (lambda (session question options meta)
                              (setf (efrit-repl-session-pending-question session)
                                    (list question options (current-time) meta)))
   ;; Store Claude's final message so the conversation context carries
   ;; into the next turn
   :on-end-turn-fn #'efrit-repl-session-add-assistant-message
   :before-request-fn #'efrit-repl-loop--deliver-steering
   :api-call-fn 'efrit-repl-loop--api-call
   :continue-fn 'efrit-repl-loop--continue-iteration
   :execute-tools-fn 'efrit-repl-loop--execute-tools
   :on-api-error-fn 'efrit-repl-loop--on-api-error
   :finish-fn #'efrit-repl-loop--finish)
  "Engine adapter binding `efrit-loop' to REPL sessions.")

;;; Public API

(defun efrit-repl-continue (session user-input &optional on-turn-complete api-input)
  "Continue REPL SESSION with USER-INPUT.
Adds the user message to the session and runs the agentic loop.
Optional ON-TURN-COMPLETE callback is called when Claude finishes the turn.

An editor-context snapshot (`efrit-context-sources') of the buffer
the user is working in is taken now, at submit time, and prepended to
the copy of USER-INPUT sent to the API; the conversation shows the
plain input.  A caller that has already prepared what the model
should see passes it as API-INPUT (a string); then USER-INPUT is only
what the conversation shows and the snapshot is prepended to API-INPUT
instead.  That is how a package hands efrit a prompt over data it
gathered itself (see `efrit-submit').

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

      ;; Add user message to session, with the editor snapshot for the API
      (efrit-repl-session-add-user-message
       session user-input
       (condition-case err
           (efrit-context-wrap-user-input (or api-input user-input))
         (error
          (efrit-log 'warn "REPL session %s: context snapshot failed: %s"
                     session-id (error-message-string err))
          api-input)))

      ;; Begin the turn.  The per-turn tool counters live in
      ;; efrit-tools and were never reset, so after ~100 tool calls in
      ;; an Emacs session every eval_sexp failed with "rate limit
      ;; exceeded" -- the "tool limit reached" the model then reported.
      ;; As this session's code: the counters and the sandbox's
      ;; standing answer are per session
      (efrit-repl-loop--with-session
       session
       (lambda ()
         (efrit-tools--reset-rate-limits)
         ;; Same for the circuit breaker's counters (30 tool calls per
         ;; *turn*, not per Emacs session)
         (efrit-do--circuit-breaker-reset)
         (efrit-sandbox-begin-turn (or api-input user-input))
         ;; rejections count per turn, not per session
         (efrit-review-forget-session session-id)
         (efrit-loop-stall-reset session-id)
         (efrit-diagnostics-baseline-begin-turn)))
      (efrit-repl-session-begin-turn session)
      (efrit-publish 'turn-start `((:session-id . ,session-id)
                                   (:input . ,user-input)))

      (efrit-publish 'status `((:session-id . ,session-id) (:status . working)))

      ;; Store loop state
      (puthash session-id
               (list session on-turn-complete 0)
               efrit-repl-loop--active)

      (efrit-log 'info "REPL session %s: continuing with user input (%d chars)"
                 session-id (length user-input))

      ;; Start the loop
      (efrit-repl-loop--continue-iteration session)

      session-id)))

;;; Steering: talking to a running turn
;;
;; The agent buffer publishes a `steer' event when the user submits
;; while a turn runs and chose to steer rather than queue.  The loop
;; keeps the text on the session and hands it to the model together
;; with the next tool results.  There is no other seam: a request in
;; flight cannot be changed, and the model only reads new input as a
;; user message.  Text that arrives after the last tool round is not
;; lost: `efrit-repl-loop--flush-steering' turns it into the next input
;; when the turn ends.

(declare-function efrit-review-forget-session "efrit-review")

(defun efrit-repl-loop--session-for-event (event)
  "The working REPL session EVENT's :session-id names, or nil."
  (when-let* ((id (alist-get :session-id event))
              (state (gethash id efrit-repl-loop--active)))
    (nth 0 state)))

(defun efrit-repl-loop--on-steer (event)
  "Subscriber: keep EVENT's :text for the running turn of its session."
  (when-let* ((session (efrit-repl-loop--session-for-event event))
              (text (alist-get :text event)))
    (efrit-repl-session-steer session text)
    (efrit-log 'info "REPL session %s: steering text pending (%d chars)"
               (efrit-repl-session-id session) (length text))))

(efrit-subscribe 'steer #'efrit-repl-loop--on-steer)

(defun efrit-repl-loop--deliver-steering (session)
  "Fold SESSION's pending steering into the message the model reads next.
The adapter's before-request hook."
  (when-let* ((texts (efrit-repl-session-take-steering session)))
    (efrit-repl-session-add-steering-blocks session texts)
    (dolist (text texts)
      (efrit-publish 'steered `((:session-id . ,(efrit-repl-session-id session))
                                (:text . ,text))))))

(defun efrit-repl-loop--flush-steering (session)
  "Steering that never found a tool round becomes queued input.
Called when a turn ends: a turn that answered without tools had no
seam, so the text starts the next turn instead of vanishing."
  (when-let* ((texts (efrit-repl-session-take-steering session)))
    (dolist (text texts)
      (efrit-repl-session-enqueue session text)
      ;; say so: the steer line in the transcript otherwise sits there
      ;; and a turn with the same text starts with no explanation
      (efrit-publish 'steer-queued `((:session-id . ,(efrit-repl-session-id session))
                                     (:text . ,text))))
    (efrit-log 'info "REPL session %s: %d steering text(s) had no seam; queued"
               (efrit-repl-session-id session) (length texts))))

;;; Internal Loop Implementation

(defun efrit-repl-loop--continue-iteration (session)
  "Continue async loop for REPL SESSION, sending next request to Claude."
  (efrit-loop-continue-iteration session efrit-repl-loop--adapter))

(defun efrit-repl-loop--with-session (session thunk)
  "Call THUNK as SESSION's code: its id is the current session and its
project root is `default-directory' and `efrit-project-root'.
Tools and API callbacks run from process sentinels, in whatever
buffer happened to be current; without this the sandbox, the limits,
the settings and shell commands would take the root of that
bystander (multi-session audit, 2026-09-28).  The current buffer is
left alone: tools that act on \"the current buffer\" mean the user's."
  (let* ((root (efrit-repl-session-project-root session))
         (efrit-project-root (or root efrit-project-root))
         (default-directory (or (and root (file-directory-p root) (file-name-as-directory root))
                                default-directory)))
    (efrit-with-session (efrit-repl-session-id session)
      (funcall thunk))))

(defun efrit-repl-loop--api-call (session messages callback)
  "Make async API call to Claude with MESSAGES for REPL SESSION.
CALLBACK is (lambda (response error) ...) called when complete, as
SESSION's code (`efrit-repl-loop--with-session')."
  (efrit-repl-loop--with-session
   session
   (lambda ()
     (efrit-loop-api-call (efrit-repl-session-id session) messages
                          (lambda (response error)
                            (efrit-repl-loop--with-session
                             session (lambda () (funcall callback response error))))
                          efrit-repl-loop--adapter))))

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
          (efrit-publish 'error `((:session-id . ,session-id) (:message . ,message)
                                  (:buffer . ,buffer)))
        ;; Buffer reference is stale - clear it
        (setf (efrit-repl-session-buffer session) nil)))))

(defun efrit-repl-loop--on-api-error (session error)
  "Handle API ERROR for REPL SESSION.
A cancelled streaming request arrives here as \"interrupted\" and is
a user action, not a failure."
  (if (equal error "interrupted")
      (progn
        (efrit-log 'info "REPL session %s: request cancelled by user"
                   (efrit-repl-session-id session))
        ;; This path skips `efrit-loop--finish', which is what publishes
        ;; turn-complete; subscribers (the test drive, efrit-gnus's
        ;; batch driver) must still hear that the turn ended
        (efrit-publish 'turn-complete
                       `((:session-id . ,(efrit-repl-session-id session))
                         (:stop-reason . "interrupted")))
        (efrit-repl-loop--end-turn session "interrupted"))
    (efrit-log 'error "REPL session %s: API error: %s"
               (efrit-repl-session-id session) error)
    (efrit-repl-loop--display-error session (efrit-repl-loop--explain-api-error error))
    (efrit-repl-loop--end-turn session "api-error")))

(defun efrit-repl-loop--explain-api-error (error)
  "Return ERROR as a string, with a next step appended when we know one."
  (let ((msg (format "%s" error)))
    (cond
     ((efrit-models-model-error-p msg)
      (format "%s\n\nThe endpoint does not serve model %s.  Run M-x efrit-select-model (C-u to probe candidates) or C-u M-x efrit-doctor."
              msg efrit-default-model))
     ((string-match-p "401\\|authentication\\|Unauthorized" msg)
      (format "%s\n\nCredentials were rejected.  If the key is minted per session it may have expired; refresh it and run M-x efrit-doctor." msg))
     (t msg))))

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
  (efrit-sandbox-end-turn)
  (let* ((session-id (efrit-repl-session-id session))
         (loop-state (gethash session-id efrit-repl-loop--active))
         (on-turn-complete (nth 1 loop-state))
         (iteration-count (or (nth 2 loop-state) 0)))

    ;; End the turn (transitions to idle, NOT complete)
    (efrit-repl-session-end-turn session)
    (efrit-repl-loop--flush-steering session)
    ;; Whatever path ended the turn (a response, an error, a cancel
    ;; during review), the activity indicator stops with it.  The
    ;; cancel path did not publish this and left the spinner turning.
    (efrit-publish 'thinking-stop `((:session-id . ,session-id)))

    ;; A turn paused on request_user_input stays in waiting state so the
    ;; next input is routed as the answer (ef-dcn).
    (when (equal stop-reason "waiting-for-user")
      (efrit-repl-session-set-status session 'waiting))

    ;; Update agent buffer status
    ;; Note: "unknown" means Claude finished but with unrecognized stop_reason
    ;; This is normal - treat it as idle, not failed.
    (efrit-publish 'status
                   `((:session-id . ,session-id)
                     (:status . ,(cond
                                  ((equal stop-reason "waiting-for-user") 'waiting)
                                  ((member stop-reason '("end_turn" "session-complete" "unknown")) 'idle)
                                  ((equal stop-reason "interrupted") 'interrupted)
                                  (t 'failed)))))

    ;; Remove from active loops
    (remhash session-id efrit-repl-loop--active)

    ;; Call completion callback if provided
    (when on-turn-complete
      (funcall on-turn-complete session stop-reason))

    (efrit-log 'info "REPL session %s: turn ended (%s, %d iterations)"
               session-id stop-reason iteration-count)))

(defun efrit-repl-loop-abandon-turn (session)
  "End SESSION's turn that waits on the user, as interrupted.
For a turn paused on a question: nothing is in flight, so this is the
whole cancel.  The next input starts a new turn."
  (efrit-publish 'turn-complete
                 `((:session-id . ,(efrit-repl-session-id session))
                   (:stop-reason . "interrupted")))
  (efrit-repl-loop--end-turn session "interrupted"))

;;; Query Functions

(defun efrit-repl-loop-active-p (session)
  "Return non-nil if SESSION has an active loop running."
  (when session
    (gethash (efrit-repl-session-id session) efrit-repl-loop--active)))

(defun efrit-repl-loop-hold (session)
  "Mark SESSION working with a placeholder loop entry, so it reads as busy.
For tests and the test drive, which pretend a turn runs without
starting one; without the entry `efrit-repl-loop-recover-stale' would
end the pretend turn at once.  `efrit-repl-loop-release' undoes it."
  (puthash (efrit-repl-session-id session) (list session nil 0) efrit-repl-loop--active)
  (efrit-repl-session-set-status session 'working))

(defun efrit-repl-loop-release (session)
  "Undo `efrit-repl-loop-hold' on SESSION: idle, no loop entry."
  (remhash (efrit-repl-session-id session) efrit-repl-loop--active)
  (efrit-repl-session-set-status session 'idle))

(defun efrit-repl-loop-recover-stale (session)
  "End SESSION's turn when it says `working' but no loop is running it.
Returns non-nil when it did.  How a session gets there: a reload
replaced `efrit-repl-loop--active' with an empty table (or a request
died without its callback) while a turn was in flight, so nothing was
left to call `efrit-repl-loop--end-turn' and every later submit was
refused with \"busy\" (2026-09-27).  Ending the turn publishes
`status' and `turn-complete' as an interruption, so subscribers (a
batch driver) hear it too."
  (when (and session
             (eq (efrit-repl-session-status session) 'working)
             (not (efrit-repl-loop-active-p session)))
    (efrit-log 'warn "REPL session %s: working with no loop; ending the stale turn"
               (efrit-repl-session-id session))
    (efrit-repl-loop-abandon-turn session)
    t))

(provide 'efrit-repl-loop)

;;; efrit-repl-loop.el ends here
