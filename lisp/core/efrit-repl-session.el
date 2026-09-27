;;; efrit-repl-session.el --- Persistent REPL session model -*- lexical-binding: t -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; Persistent REPL session model for efrit-agent.
;;
;; Unlike `efrit-session' which is designed for single-command execution,
;; `efrit-repl-session' persists across multiple user inputs, maintaining
;; full conversation context.
;;
;; Key differences from efrit-session:
;; - Never "completes" - transitions between idle/working states
;; - Conversation history accumulates across turns
;; - API messages grow with each interaction (full context to Claude)
;; - Designed for the REPL interaction model
;;
;; Usage:
;;   (efrit-repl-session-create)          ; Create new session
;;   (efrit-repl-session-continue sess input) ; Add user input, continue
;;   (efrit-repl-session-pause sess)      ; Pause gracefully
;;   (efrit-repl-session-reset sess)      ; Clear conversation, start fresh

;;; Code:

(require 'cl-lib)
(require 'efrit-log)
(require 'efrit-common)
(require 'efrit-budget)
(require 'efrit-usage)
(require 'efrit-events)

;;; Customization

(defgroup efrit-repl nil
  "REPL session management for Efrit."
  :group 'efrit
  :prefix "efrit-repl-")

(defcustom efrit-repl-max-history 100
  "Maximum number of conversation turns to retain.
Older turns are compressed/summarized when this limit is exceeded."
  :type 'integer
  :group 'efrit-repl)

(defcustom efrit-repl-context-budget nil
  "Most input tokens one request may carry, or nil for the model's window.
The history sent with each request is estimated from its size in
characters, and when it is over this budget the oldest tool results
are elided, then the oldest user messages, until it fits.  Assistant
messages stay.  nil means `efrit-usage-window' less
`efrit-repl-context-headroom'."
  :type '(choice (const :tag "From efrit-usage-context-window" nil) integer)
  :group 'efrit-repl)

(defcustom efrit-repl-context-headroom 0.15
  "Fraction of the context window kept free for the answer and the tools schema."
  :type 'number
  :group 'efrit-repl)

(defcustom efrit-repl-steering-frame
  "INSTRUCTION FROM THE USER, sent while you were working. It takes priority over the original request and over the tool results above: follow it from your very next step on, and honor it in your final message. The instruction: "
  "Text put before what the user typed to steer a running turn.
It arrives in the same message as the tool results, which models
weight low; a plain \"the user says\" was ignored three runs out of
three (2026-09-25), hence the emphasis."
  :type 'string
  :group 'efrit-repl)

(defconst efrit-repl-elided-marker "[elided: %s, %d characters, to fit the context window]"
  "Text that replaces a message body the context guard removed.")

;;; REPL Session Data Structure

(cl-defstruct efrit-repl-session
  "Persistent REPL session for multi-turn conversation.

Unlike `efrit-session' which completes after one command, this
session persists and accumulates conversation context."
  ;; Identity
  (id (efrit-repl-session--generate-id))  ; Unique session ID
  (created-at (current-time))             ; Session creation time
  (last-activity (current-time))          ; For auto-save triggers

  ;; Session state (never 'complete - use 'idle instead)
  (status 'idle)                          ; idle/working/paused/waiting

  ;; Buffer association
  buffer                                  ; Associated agent buffer

  ;; Conversation history (human-readable format)
  ;; Each entry: (:role user|assistant :content "..." :timestamp time)
  (conversation nil)

  ;; API messages (Claude format for continuations)
  ;; Grows with each turn - contains full history for context
  ;; Format: list of ((role . "user"|"assistant") (content . ...))
  (api-messages nil)

  ;; Token budget tracking
  (budget (efrit-budget-create))

  ;; Current turn tracking (reset each turn)
  (current-turn-tools nil)                ; Tools used this turn
  (current-turn-start nil)                ; When current turn started

  ;; Metadata
  (project-root nil)                      ; Associated project
  (title nil)                             ; Optional session title

  ;; Pending input handling
  (pending-question nil)                  ; Question waiting for answer

  ;; Interrupt control
  (interrupt-requested nil)               ; Signal graceful pause

  ;; Added 2026-09-25 -- new slots go at the end, see the upgrade
  ;; section below
  (queue nil)                             ; Inputs to send after this turn, oldest first
  (steering nil)                          ; Texts to inject before the next request, oldest first
  ;; Added 2026-09-27
  (turn-waiting-mark 0))                  ; `efrit-user-waiting-seconds' when the turn began

;;; Session ID Generation

(defvar efrit-repl-session--counter 0
  "Counter for generating unique session IDs within an Emacs session.")

(defun efrit-repl-session--generate-id ()
  "Generate a unique session ID."
  (format "repl-%s-%04d"
          (format-time-string "%Y%m%d-%H%M%S")
          (cl-incf efrit-repl-session--counter)))

;;; Session Registry

(defvar efrit-repl-session--registry (make-hash-table :test 'equal)
  "Hash table of all REPL sessions by ID.")

(defvar efrit-repl-session--active nil
  "The currently active REPL session (if any).")

;;; Upgrading a session from an older struct definition
;;
;; `efrit-reload' loads this file again while sessions exist.  A
;; `cl-defstruct' instance is a record whose slots are indexed by
;; position; an instance made before a slot was added is shorter than
;; the new accessors expect and the first new accessor signals
;; args-out-of-range (2026-09-26: `queue' and `steering' added, and
;; `efrit-repl-session-should-interrupt-p' broke every turn until the
;; buffer was recreated).  Records are fixed-size, so an old instance
;; cannot grow: `efrit-repl-session-upgrade' builds a fresh one with
;; the old values copied BY SLOT NAME (positions differ between the
;; layouts) and re-points the registry at it; `efrit-reload' then
;; swaps the agent buffers' references.  Add new slots at the END of
;; the struct and list every historical layout here.

(defconst efrit-repl-session--layouts
  '((14 id created-at last-activity status buffer conversation api-messages budget
        current-turn-tools current-turn-start project-root title pending-question
        interrupt-requested)
    (16 id created-at last-activity status buffer conversation api-messages budget
        current-turn-tools current-turn-start project-root title pending-question
        interrupt-requested queue steering))
  "Slot names of earlier `efrit-repl-session' definitions, by slot count.
The current definition is read from `cl-struct-slot-info'.")

(defun efrit-repl-session--slot-names (record)
  "The slot names of RECORD's layout, current or historical, or nil."
  (let ((n (1- (length record))))
    (if (= n (length (cdr (cl-struct-slot-info 'efrit-repl-session))))
        (mapcar #'car (cdr (cl-struct-slot-info 'efrit-repl-session)))
      (cdr (assq n efrit-repl-session--layouts)))))

(defun efrit-repl-session-upgrade (session)
  "A fresh `efrit-repl-session' with SESSION's values, when SESSION is an old record.
Returns nil when SESSION already has the current layout (or is not a
record this file knows).  The fresh session replaces the old one in
the registry and as the active session."
  (let ((current (mapcar #'car (cdr (cl-struct-slot-info 'efrit-repl-session)))))
    (when (and (recordp session)
               (eq (type-of session) 'efrit-repl-session)
               (/= (1- (length session)) (length current)))
      (let ((old-names (efrit-repl-session--slot-names session)))
        (unless old-names
          (error "efrit-repl-session: no known layout with %d slots" (1- (length session))))
        (let ((fresh (make-efrit-repl-session)))
          (cl-loop for name in old-names
                   for i from 1
                   for j = (cl-position name current)
                   when j do (aset fresh (1+ j) (aref session i)))
          ;; No loop survives a reload (`efrit-repl-loop--active' is a
          ;; fresh table), so a `working' status would never be
          ;; cleared and every later submit would be refused as busy
          (when (eq (efrit-repl-session-status fresh) 'working)
            (setf (efrit-repl-session-status fresh) 'idle)
            (efrit-log 'warn "REPL session %s: was working across the reload; now idle"
                       (efrit-repl-session-id fresh)))
          (puthash (efrit-repl-session-id fresh) fresh efrit-repl-session--registry)
          (when (eq efrit-repl-session--active session)
            (setq efrit-repl-session--active fresh))
          (efrit-log 'info "REPL session %s: upgraded from %d to %d slots"
                     (efrit-repl-session-id fresh) (length old-names) (length current))
          fresh)))))

(defun efrit-repl-session-upgrade-all ()
  "Upgrade every registered session whose record predates the current definition.
Returns an alist (OLD . NEW) of the sessions that were rebuilt, so
holders of the old object (the agent buffer's local variable) can swap."
  (let (swapped)
    (maphash (lambda (_id session)
               (when-let* ((fresh (efrit-repl-session-upgrade session)))
                 (push (cons session fresh) swapped)))
             efrit-repl-session--registry)
    swapped))

;;; Session Lifecycle

(defun efrit-repl-session-create (&optional project-root)
  "Create a new REPL session.
Optional PROJECT-ROOT associates the session with a project."
  (let ((session (make-efrit-repl-session
                  :project-root (or project-root default-directory))))
    (puthash (efrit-repl-session-id session) session efrit-repl-session--registry)
    (efrit-log 'info "Created REPL session %s" (efrit-repl-session-id session))
    session))

(defun efrit-repl-session-get (id)
  "Get REPL session by ID, or nil if not found."
  (gethash id efrit-repl-session--registry))

(defun efrit-repl-session-active ()
  "Return the currently active REPL session, or nil."
  efrit-repl-session--active)

(defun efrit-repl-session-set-active (session)
  "Set SESSION as the active REPL session."
  (setq efrit-repl-session--active session)
  (when session
    (efrit-log 'debug "Active REPL session: %s" (efrit-repl-session-id session))))

;;; Conversation Management

(defun efrit-repl-session-add-user-message (session content &optional api-content)
  "Add a user message to SESSION's conversation.
CONTENT is what the user typed and is stored in the human-readable
conversation.  API-CONTENT, when non-nil, is what is actually sent to
Claude in its place -- typically CONTENT with an editor-context block
prepended (see `efrit-context-wrap-user-input')."
  (when (and session content (not (string-empty-p content)))
    (let ((timestamp (current-time))
          (api-content (or api-content content)))
      ;; Update human-readable conversation
      (setf (efrit-repl-session-conversation session)
            (append (efrit-repl-session-conversation session)
                    (list (list :role 'user
                                :content content
                                :timestamp timestamp))))
      ;; Update API messages (what gets sent to Claude)
      (setf (efrit-repl-session-api-messages session)
            (append (efrit-repl-session-api-messages session)
                    (list `((role . "user")
                            (content . ,api-content)))))
      ;; Track tokens
      (efrit-budget-record-usage (efrit-repl-session-budget session)
                                 'user-message
                                 (efrit-budget-estimate-tokens
                                  (if (stringp api-content) api-content
                                    (efrit-repl-session--content-text api-content))))
      ;; Update activity timestamp
      (setf (efrit-repl-session-last-activity session) timestamp)
      (efrit-log 'debug "REPL session %s: added user message (%d chars)"
                 (efrit-repl-session-id session)
                 (length content)))))

(defun efrit-repl-session-add-assistant-message (session content)
  "Add an assistant message to SESSION's conversation.
CONTENT can be a string or a list of content blocks."
  (when (and session content)
    (let ((timestamp (current-time)))
      ;; Update human-readable conversation
      (setf (efrit-repl-session-conversation session)
            (append (efrit-repl-session-conversation session)
                    (list (list :role 'assistant
                                :content content
                                :timestamp timestamp))))
      ;; Update API messages
      (setf (efrit-repl-session-api-messages session)
            (append (efrit-repl-session-api-messages session)
                    (list `((role . "assistant")
                            (content . ,content)))))
      ;; Update activity timestamp
      (setf (efrit-repl-session-last-activity session) timestamp)
      (efrit-log 'debug "REPL session %s: added assistant message"
                 (efrit-repl-session-id session)))))

(defun efrit-repl-session-add-tool-result (session tool-use-id result &optional is-error)
  "Add a tool result to SESSION's API messages.
TOOL-USE-ID is the ID from the tool_use block.
RESULT is the tool output string.
IS-ERROR indicates if this is an error result."
  (when session
    ;; Tool results are added as user messages with tool_result content
    (let ((tool-result `((type . "tool_result")
                         (tool_use_id . ,tool-use-id)
                         (content . ,result))))
      (when is-error
        (setf (alist-get 'is_error tool-result) t))
      (setf (efrit-repl-session-api-messages session)
            (append (efrit-repl-session-api-messages session)
                    (list `((role . "user")
                            (content . ,(vector tool-result))))))
      (setf (efrit-repl-session-last-activity session) (current-time)))))

(defun efrit-repl-session-get-api-messages (session)
  "Get API messages from SESSION for sending to Claude.
Returns the most recent messages, limited by `efrit-repl-max-history'
turns and by `efrit-repl-context-budget' tokens (see
`efrit-repl-session-fit-context')."
  (when session
    (efrit-repl-session-fit-context session)
    (let ((all-messages (efrit-repl-session-api-messages session))
          (max-messages (* 2 efrit-repl-max-history)))
      (if (<= (length all-messages) max-messages)
          all-messages
        (seq-drop all-messages (- (length all-messages) max-messages))))))

;;; History marks: a caller can run turns and then drop them again

(defun efrit-repl-session-history-mark (session)
  "A mark for the current end of SESSION's API history.
`efrit-repl-session-rewind' takes the history back to it.  A package
that runs several turns over separate data (a batch of mail each) and
wants each turn to start from the same point uses this, so the model
does not read every earlier batch again."
  (length (efrit-repl-session-api-messages session)))

(defun efrit-repl-session-rewind (session mark)
  "Drop the API messages SESSION accumulated after MARK.
The human-readable conversation is kept: the user saw those turns.
Returns the number of messages dropped."
  (let* ((messages (efrit-repl-session-api-messages session))
         (dropped (max 0 (- (length messages) mark))))
    (when (> dropped 0)
      (setf (efrit-repl-session-api-messages session) (seq-take messages mark))
      (efrit-log 'debug "REPL session %s: rewound %d API messages to mark %d"
                 (efrit-repl-session-id session) dropped mark))
    dropped))

(defun efrit-repl-session--block-get (block key)
  "KEY (a string) of BLOCK, a hash table or an alist with string or symbol keys."
  (cond ((hash-table-p block) (gethash key block))
        ((listp block) (or (cdr (assoc key block))
                           (cdr (assq (intern key) block))))))

(defun efrit-repl-session--content-text (content)
  "The text of a message CONTENT: a string, or the text blocks of a vector joined."
  (cond ((stringp content) content)
        ((or (vectorp content) (listp content))
         (mapconcat (lambda (block)
                      (if (equal (efrit-repl-session--block-get block "type") "text")
                          (or (efrit-repl-session--block-get block "text") "")
                        ""))
                    (append content nil) ""))
        (t "")))

(defun efrit-repl-session-last-answer (session &optional mark)
  "The text of the assistant's most recent message in SESSION, or nil.
With MARK, only messages after that history mark count, so a caller
gets the answer of the turns it started and never an older one."
  (let ((messages (nthcdr (or mark 0) (efrit-repl-session-api-messages session)))
        (answer nil))
    (dolist (msg messages)
      (when (equal (efrit-repl-session--block-get msg "role") "assistant")
        (let ((text (efrit-repl-session--content-text
                     (efrit-repl-session--block-get msg "content"))))
          (unless (string-empty-p text)
            (setq answer text)))))
    answer))

;;; Context guard: keep the history under the model's window

(defun efrit-repl-session--message-chars (msg)
  "Characters of MSG's content, tool inputs and results included."
  (let ((content (efrit-repl-session--block-get msg "content")))
    (cond ((stringp content) (length content))
          ((or (vectorp content) (listp content))
           (let ((n 0))
             (dolist (block (append content nil))
               (cl-incf n (efrit-repl-session--block-chars block)))
             n))
          (t 0))))

(defun efrit-repl-session--block-chars (block)
  "Characters of one content BLOCK."
  (pcase (efrit-repl-session--block-get block "type")
    ("text" (length (or (efrit-repl-session--block-get block "text") "")))
    ("tool_result"
     (let ((c (efrit-repl-session--block-get block "content")))
       (if (stringp c) (length c)
         (length (format "%S" c)))))
    ("tool_use" (length (format "%S" (efrit-repl-session--block-get block "input"))))
    (_ (length (format "%S" block)))))

(defun efrit-repl-session-context-budget ()
  "Tokens one request may carry.
`efrit-repl-context-budget', or the model's window less the headroom."
  (or efrit-repl-context-budget
      (floor (* (efrit-usage-window) (- 1 efrit-repl-context-headroom)))))

(defun efrit-repl-session--estimate-tokens (messages)
  "Estimated tokens of MESSAGES."
  (let ((chars 0))
    (dolist (msg messages)
      (cl-incf chars (efrit-repl-session--message-chars msg)))
    (ceiling (/ chars efrit-budget--chars-per-token))))

(defun efrit-repl-session--elided-p (text)
  "Non-nil when TEXT is already an elision marker."
  (and (stringp text) (string-prefix-p "[elided: " text)))

(defun efrit-repl-session--elide-block (block what)
  "BLOCK with its body replaced by `efrit-repl-elided-marker' naming WHAT.
Returns nil when BLOCK has nothing worth eliding, or is elided already."
  (let ((chars (efrit-repl-session--block-chars block))
        (body (pcase (efrit-repl-session--block-get block "type")
                ("tool_result" (efrit-repl-session--block-get block "content"))
                ("text" (efrit-repl-session--block-get block "text")))))
    (when (and (> chars (length efrit-repl-elided-marker))
               (not (efrit-repl-session--elided-p body)))
      (let ((marker (format efrit-repl-elided-marker what chars)))
        (pcase (efrit-repl-session--block-get block "type")
          ("tool_result"
           (if (hash-table-p block)
               (let ((copy (copy-hash-table block)))
                 (puthash "content" marker copy)
                 copy)
             (let ((copy (copy-alist block)))
               (if (assq 'content copy)
                   (setf (alist-get 'content copy) marker)
                 (setf (alist-get "content" copy nil nil #'equal) marker))
               copy)))
          ("text"
           (if (hash-table-p block)
               (let ((copy (copy-hash-table block)))
                 (puthash "text" marker copy)
                 copy)
             (let ((copy (copy-alist block)))
               (if (assq 'text copy)
                   (setf (alist-get 'text copy) marker)
                 (setf (alist-get "text" copy nil nil #'equal) marker))
               copy))))))))

(defun efrit-repl-session--elide-message (msg kind)
  "MSG with the bodies of KIND (`tool-result' or `user-text') elided.
Returns (NEW-MSG . CHARS-SAVED); NEW-MSG is MSG itself when nothing changed."
  (let* ((content (efrit-repl-session--block-get msg "content"))
         (saved 0)
         (new-content
          (cond
           ((and (stringp content) (eq kind 'user-text)
                 (not (efrit-repl-session--elided-p content)))
            (let ((marker (format efrit-repl-elided-marker "a user message" (length content))))
              (when (> (length content) (length marker))
                (setq saved (- (length content) (length marker)))
                marker)))
           ((or (vectorp content) (listp content))
            (let* ((changed nil)
                   (blocks (mapcar
                           (lambda (block)
                             (let* ((type (efrit-repl-session--block-get block "type"))
                                    (new (cond
                                          ((and (eq kind 'tool-result) (equal type "tool_result"))
                                           (efrit-repl-session--elide-block block "a tool result"))
                                          ((and (eq kind 'user-text) (equal type "text"))
                                           (efrit-repl-session--elide-block block "a user message")))))
                               (if new
                                   (progn
                                     (cl-incf saved (- (efrit-repl-session--block-chars block)
                                                       (efrit-repl-session--block-chars new)))
                                     (setq changed t)
                                     new)
                                 block)))
                           (append content nil))))
              (when changed
                (if (vectorp content) (vconcat blocks) blocks)))))))
    (if (and new-content (> saved 0))
        (let ((copy (copy-alist msg)))
          (if (assq 'content copy)
              (setf (alist-get 'content copy) new-content)
            (setf (alist-get "content" copy nil nil #'equal) new-content))
          (cons copy saved))
      (cons msg 0))))

(defun efrit-repl-session-fit-context (session)
  "Elide old message bodies in SESSION until the history fits the budget.
Oldest tool results go first, then the oldest user messages; assistant
messages and the last user message (the current input) are never
touched.  The elision is permanent in the stored history, so it costs
nothing on the next request.  Publishes a `note' event and returns the
number of messages changed, 0 when nothing was needed."
  (let* ((messages (efrit-repl-session-api-messages session))
         (budget (efrit-repl-session-context-budget))
         (tokens (efrit-repl-session--estimate-tokens messages))
         (before tokens)
         (changed 0))
    (when (and (> tokens budget) (> (length messages) 1))
      (let ((protected (car (last messages))))
        (catch 'fits
          (dolist (kind '(tool-result user-text))
            (let ((cell messages))
              (while cell
                (let ((msg (car cell)))
                  (when (and (not (eq msg protected))
                             (equal (efrit-repl-session--block-get msg "role") "user"))
                    (pcase-let ((`(,new . ,saved) (efrit-repl-session--elide-message msg kind)))
                      (when (> saved 0)
                        (setcar cell new)
                        (cl-incf changed)
                        (cl-decf tokens (ceiling (/ saved efrit-budget--chars-per-token)))
                        (when (<= tokens budget) (throw 'fits nil))))))
                (setq cell (cdr cell)))))))
      (when (> changed 0)
        (efrit-log 'info "REPL session %s: elided %d old message bodies, ~%d -> ~%d tokens (budget %d)"
                   (efrit-repl-session-id session) changed before tokens budget)
        (efrit-publish 'note
                       `((:session-id . ,(efrit-repl-session-id session))
                         (:kind . limits) (:face . warning)
                         (:text . ,(format "⏱ elided %d old message bodies to fit the context window (~%s -> ~%s tokens); ask again for anything you still need"
                                           changed
                                           (efrit-usage-compact-number before)
                                           (efrit-usage-compact-number tokens)))))))
    changed))

;;; Queue and steering
;;
;; Two ways to talk to a busy session.  The QUEUE holds whole inputs
;; that start their own turn once this one ends (`efrit-repl-session-
;; dequeue' is called by the turn-complete handler).  STEERING holds
;; text the loop folds into the running turn: `efrit-loop-execute-tools'
;; drains it into the user message that carries the tool results, so
;; the model reads it before its next step.  Both are lists of strings,
;; oldest first.

(defun efrit-repl-session-enqueue (session input)
  "Queue INPUT to start a turn after SESSION's current turn ends.
Returns the queue length."
  (setf (efrit-repl-session-queue session)
        (append (efrit-repl-session-queue session) (list input)))
  (efrit-log 'debug "REPL session %s: queued input (%d waiting)"
             (efrit-repl-session-id session) (length (efrit-repl-session-queue session)))
  (length (efrit-repl-session-queue session)))

(defun efrit-repl-session-dequeue (session)
  "Pop the oldest queued input of SESSION, or nil."
  (when-let* ((input (car (efrit-repl-session-queue session))))
    (setf (efrit-repl-session-queue session) (cdr (efrit-repl-session-queue session)))
    input))

(defun efrit-repl-session-steer (session text)
  "Add TEXT to what SESSION's running turn reads before its next request.
Returns the number of steering texts pending."
  (setf (efrit-repl-session-steering session)
        (append (efrit-repl-session-steering session) (list text)))
  (length (efrit-repl-session-steering session)))

(defun efrit-repl-session-take-steering (session)
  "Return and clear SESSION's pending steering texts, oldest first."
  (prog1 (efrit-repl-session-steering session)
    (setf (efrit-repl-session-steering session) nil)))

(defun efrit-repl-session-add-steering-blocks (session texts)
  "Append TEXTS as text blocks to the last user message of SESSION.
That message carries the tool results of the step that just ran, so
the steering arrives with them; when the last message is not a user
message (nothing ran yet), a new user message is added.  This is how
`efrit-loop-execute-tools' delivers steering."
  (when texts
    (let* ((messages (efrit-repl-session-api-messages session))
           (last (car (last messages)))
           (blocks (mapcar (lambda (text)
                             `((type . "text")
                               (text . ,(concat efrit-repl-steering-frame text))))
                           texts)))
      (if (and last (equal (efrit-repl-session--block-get last "role") "user")
               (vectorp (efrit-repl-session--block-get last "content")))
          (setf (efrit-repl-session-api-messages session)
                (append (butlast messages)
                        (list `((role . "user")
                                (content . ,(vconcat (efrit-repl-session--block-get last "content")
                                                     blocks))))))
        (setf (efrit-repl-session-api-messages session)
              (append messages
                      (list `((role . "user") (content . ,(vconcat blocks)))))))
      (dolist (text texts)
        (setf (efrit-repl-session-conversation session)
              (append (efrit-repl-session-conversation session)
                      (list (list :role 'user :content text :steer t
                                  :timestamp (current-time))))))
      (efrit-log 'info "REPL session %s: %d steering text(s) delivered"
                 (efrit-repl-session-id session) (length texts)))))

;;; Session State Management

(defun efrit-repl-session-set-status (session status)
  "Set SESSION's status to STATUS.
STATUS should be one of: idle, working, paused, waiting."
  (when session
    (setf (efrit-repl-session-status session) status)
    (setf (efrit-repl-session-last-activity session) (current-time))
    (efrit-log 'debug "REPL session %s: status -> %s"
               (efrit-repl-session-id session)
               status)))

(defun efrit-repl-session-pause (session)
  "Pause SESSION gracefully.
Sets interrupt flag and transitions to paused state."
  (when session
    (setf (efrit-repl-session-interrupt-requested session) t)
    (efrit-repl-session-set-status session 'paused)
    (efrit-log 'info "REPL session %s: paused" (efrit-repl-session-id session))))

(defun efrit-repl-session-resume (session)
  "Resume SESSION from paused state."
  (when session
    (setf (efrit-repl-session-interrupt-requested session) nil)
    (efrit-repl-session-set-status session 'idle)
    (efrit-log 'info "REPL session %s: resumed" (efrit-repl-session-id session))))

(defun efrit-repl-session-reset (session)
  "Reset SESSION, clearing conversation but keeping the session alive.
Use this to start a fresh conversation in the same buffer."
  (when session
    (setf (efrit-repl-session-conversation session) nil)
    (setf (efrit-repl-session-api-messages session) nil)
    (setf (efrit-repl-session-current-turn-tools session) nil)
    (setf (efrit-repl-session-pending-question session) nil)
    (setf (efrit-repl-session-queue session) nil)
    (setf (efrit-repl-session-steering session) nil)
    (setf (efrit-repl-session-interrupt-requested session) nil)
    (setf (efrit-repl-session-budget session) (efrit-budget-create))
    (efrit-repl-session-set-status session 'idle)
    (efrit-log 'info "REPL session %s: reset" (efrit-repl-session-id session))))

(defun efrit-repl-session-should-interrupt-p (session)
  "Check if SESSION has been requested to pause."
  (when session
    (efrit-repl-session-interrupt-requested session)))

;;; Turn Management

(defun efrit-repl-session-begin-turn (session)
  "Mark the beginning of a new turn in SESSION.
Resets per-turn state."
  (when session
    (setf (efrit-repl-session-current-turn-tools session) nil)
    (setf (efrit-repl-session-current-turn-start session) (current-time))
    (setf (efrit-repl-session-turn-waiting-mark session) efrit-user-waiting-seconds)
    (efrit-repl-session-set-status session 'working)))

(defun efrit-repl-session-end-turn (session)
  "Mark the end of a turn in SESSION.
Transitions back to idle state."
  (when session
    (setf (efrit-repl-session-interrupt-requested session) nil)
    (efrit-repl-session-set-status session 'idle)))

(defun efrit-repl-session-record-tool (session tool-name)
  "Record that TOOL-NAME was used in SESSION's current turn."
  (when session
    (push (cons tool-name (current-time))
          (efrit-repl-session-current-turn-tools session))))

;;; Session Info

(defun efrit-repl-session-turn-count (session)
  "Return the number of conversation turns in SESSION."
  (when session
    (/ (length (efrit-repl-session-conversation session)) 2)))

(defun efrit-repl-session-elapsed (session)
  "Return elapsed time since SESSION was created, in seconds."
  (when session
    (float-time (time-subtract (current-time)
                               (efrit-repl-session-created-at session)))))

(defun efrit-repl-session-summary (session)
  "Return a one-line summary of SESSION."
  (when session
    (format "[%s] %s - %d turns, %s"
            (efrit-repl-session-id session)
            (efrit-repl-session-status session)
            (efrit-repl-session-turn-count session)
            (efrit-repl-session--format-elapsed session))))

(defun efrit-repl-session--format-elapsed (session)
  "Format elapsed time for SESSION."
  (let ((elapsed (efrit-repl-session-elapsed session)))
    (cond
     ((< elapsed 60) (format "%.0fs" elapsed))
     ((< elapsed 3600) (format "%.0fm" (/ elapsed 60)))
     (t (format "%.1fh" (/ elapsed 3600))))))

;;; Cleanup

(defun efrit-repl-session-cleanup-old (&optional max-age-hours)
  "Clean up REPL sessions older than MAX-AGE-HOURS (default 24).
Only removes sessions not associated with a live buffer."
  (let* ((max-age (or max-age-hours 24))
         (cutoff-time (time-subtract (current-time) (* max-age 3600)))
         (removed 0))
    (maphash (lambda (id session)
               (when (and (time-less-p (efrit-repl-session-last-activity session)
                                       cutoff-time)
                          (not (buffer-live-p (efrit-repl-session-buffer session))))
                 (remhash id efrit-repl-session--registry)
                 (cl-incf removed)))
             efrit-repl-session--registry)
    (when (> removed 0)
      (efrit-log 'info "Cleaned up %d old REPL sessions" removed))
    removed))

(provide 'efrit-repl-session)

;;; efrit-repl-session.el ends here
