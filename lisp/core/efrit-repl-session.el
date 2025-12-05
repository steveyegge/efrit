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
  (interrupt-requested nil))              ; Signal graceful pause

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

(defun efrit-repl-session-add-user-message (session content)
  "Add a user message to SESSION's conversation.
Updates both human-readable conversation and API messages."
  (when (and session content (not (string-empty-p content)))
    (let ((timestamp (current-time)))
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
                            (content . ,content)))))
      ;; Track tokens
      (efrit-budget-record-usage (efrit-repl-session-budget session)
                                 'user-message
                                 (efrit-budget-estimate-tokens content))
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
  "Get all API messages from SESSION for sending to Claude.
Returns the full conversation history in Claude API format."
  (when session
    (efrit-repl-session-api-messages session)))

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
