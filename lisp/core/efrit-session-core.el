;;; efrit-session-core.el --- Core session lifecycle and registry -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; Core session management for Efrit multi-step operations.
;; Provides:
;; - Session data structure (cl-defstruct)
;; - Session lifecycle (create, complete, cancel)
;; - Session registry (active session, lookup by ID)
;; - Session queue for pending operations
;;
;; Following the Zero Client-Side Intelligence principle, this module
;; only RECORDS what happened - it does not make decisions about what
;; to do next.  That's Claude's job.

;;; Code:

(require 'cl-lib)
(require 'efrit-log)
(require 'efrit-common)
(require 'efrit-budget)

;;; Customization

(defgroup efrit-session nil
  "Session management for Efrit."
  :group 'efrit
  :prefix "efrit-session-")

(defcustom efrit-session-max-queue-size 20
  "Maximum number of sessions allowed in the queue."
  :type 'integer
  :group 'efrit-session)

;;; Session Data Structure

(cl-defstruct efrit-session
  "Session tracking for multi-step operations."
  id                      ; Unique identifier from Claude
  command                 ; Original command from user
  work-log                ; List of (result elisp todo-snapshot tool-name) tuples
  start-time              ; When session started
  status                  ; 'active, 'running, 'waiting_for_user, 'complete, 'interrupted, 'cancelled
  buffer                  ; Original buffer for context
  budget                  ; efrit-budget struct for token tracking
  ;; Loop detection and session tracking
  tool-history            ; List of (tool-name timestamp progress-tick input-hash result-hash)
  loop-warnings           ; Hash table of tool -> warning-count
  continuation-count      ; Total API calls in this session
  last-error              ; Last error message if any
  ;; Progress tracking
  last-progress-tick      ; Last time progress was made
  buffer-modifications    ; Count of buffer modifications
  todo-status-changes     ; Count of TODO status changes
  buffers-created         ; Count of buffers created
  files-modified          ; Count of files modified
  execution-outputs       ; Count of non-empty eval_sexp/shell_exec outputs
  last-tool-called        ; Last tool name for simple repeat detection
  ;; Execution events and observability (NEW)
  execution-events        ; List of {type: "tool_started|tool_result|message|todo_updated|complete" :data ...}
  ;; Interruption control (NEW)
  interrupt-requested     ; Boolean flag to signal graceful shutdown
  ;; Command queueing (NEW)
  command-queue           ; Simple list of queued commands for this session
  ;; Conversation history for interactive sessions
  conversation-history    ; List of messages: (role content timestamp)
  pending-question        ; Current question waiting for user input
  user-responses          ; List of user responses received
  ;; API message history (proper tool_use/tool_result format for continuations)
  api-messages)           ; Vector of API messages with tool_use/tool_result blocks

;;; Session Registry

(defvar efrit-session--active nil
  "Currently active multi-step session.")

(defvar efrit-session--queue nil
  "Queue of pending sessions.")

(defvar efrit-session--registry (make-hash-table :test 'equal)
  "Hash table of all multi-step sessions by ID.")

(defvar efrit-session-completed-hook nil
  "Hook run when a multi-step session completes.
Each function is called with two arguments: SESSION and RESULT.
SESSION is the efrit-session struct that completed.
RESULT is the optional completion result (may be nil).")

;;; Session Lifecycle

(defun efrit-session-create (id command)
  "Create a new multi-step session with ID and COMMAND."
  (let* ((budget (efrit-budget-create))
         (session (make-efrit-session
                   :id id
                   :command command
                   :start-time (current-time)
                   :status 'active
                   :buffer (current-buffer)
                   :budget budget
                   :work-log nil
                   :tool-history nil
                   :loop-warnings (make-hash-table :test 'equal)
                   :continuation-count 0
                   :last-error nil
                   :last-progress-tick 0
                   :buffer-modifications 0
                   :todo-status-changes 0
                   :buffers-created 0
                   :files-modified 0
                   :execution-outputs 0
                   :last-tool-called nil
                   ;; Initialize new fields
                   :execution-events nil
                   :interrupt-requested nil
                   :command-queue nil
                   :conversation-history nil
                   :pending-question nil
                   :user-responses nil
                   ;; Initialize with user's original command
                   :api-messages (list `((role . "user")
                                         (content . ,command))))))
    ;; Record user message tokens in budget
    (efrit-budget-record-usage budget 'user-message
                               (efrit-budget-estimate-tokens command))
    (puthash id session efrit-session--registry)
    (efrit-log 'info "Created session %s: %s" id
               (efrit-common-truncate-string command 60))
    session))

(defun efrit-session-get (id)
  "Get multi-step session by ID, or return nil if not found."
  (gethash id efrit-session--registry))

(defun efrit-session-active ()
  "Return the currently active multi-step session, or nil."
  efrit-session--active)

(defun efrit-session-set-active (session)
  "Set SESSION as the active multi-step session."
  (setq efrit-session--active session)
  (when session
    (efrit-log 'debug "Active session: %s" (efrit-session-id session))))

(defun efrit-session-clear-active ()
  "Clear the active multi-step session."
  (setq efrit-session--active nil)
  (efrit-log 'debug "Active session cleared"))

(defun efrit-session-complete (session &optional result)
  "Mark multi-step SESSION as complete with optional final RESULT."
  (when session
    (setf (efrit-session-status session) 'complete)
    (let ((elapsed (float-time (time-since (efrit-session-start-time session))))
          (steps (length (efrit-session-work-log session))))
      (efrit-log 'info "Completed session %s: %.1fs, %d steps%s"
                 (efrit-session-id session) elapsed steps
                 (if result
                     (concat ", result: " (efrit-common-truncate-string
                                          (format "%s" result) 50))
                   "")))
    ;; Clear active if this was active
    (when (eq session efrit-session--active)
      (setq efrit-session--active nil))
    ;; Fire completion hook so async callers can be notified
    (run-hook-with-args 'efrit-session-completed-hook session result)))

(defun efrit-session-cancel (session)
  "Cancel multi-step SESSION and mark it as aborted."
  (when session
    (setf (efrit-session-status session) 'cancelled)
    (efrit-log 'info "Cancelled session %s" (efrit-session-id session))
    (when (eq session efrit-session--active)
      (setq efrit-session--active nil))))

;;; Session Queue Management

(defun efrit-session-queue-add (command)
  "Add COMMAND to session queue, enforcing size limits.
Returns t if added successfully, nil if queue is full."
  (if (>= (length efrit-session--queue) efrit-session-max-queue-size)
      (progn
        (efrit-log 'warn "Session queue full, cannot add: %s"
                   (efrit-common-truncate-string command 50))
        nil)
    (setq efrit-session--queue (append efrit-session--queue (list command)))
    (efrit-log 'debug "Added to queue (size: %d): %s"
               (length efrit-session--queue)
               (efrit-common-truncate-string command 50))
    t))

(defun efrit-session-queue-pop ()
  "Remove and return the next command from the queue, or nil if empty."
  (when efrit-session--queue
    (let ((command (car efrit-session--queue)))
      (setq efrit-session--queue (cdr efrit-session--queue))
      (efrit-log 'debug "Popped from queue (remaining: %d): %s"
                 (length efrit-session--queue)
                 (efrit-common-truncate-string command 50))
      command)))

(defun efrit-session-queue-clear ()
  "Clear all queued sessions."
  (let ((count (length efrit-session--queue)))
    (setq efrit-session--queue nil)
    (efrit-log 'info "Cleared session queue (%d commands)" count)
    count))

(defun efrit-session-queue-length ()
  "Return the number of queued sessions."
  (length efrit-session--queue))

;;; Session Cleanup

(defun efrit-session-cleanup-old (&optional max-age-hours)
  "Clean up sessions older than MAX-AGE-HOURS (default 24).
Removes completed/cancelled sessions from registry."
  (let* ((max-age (or max-age-hours 24))
         (cutoff-time (time-subtract (current-time)
                                    (* max-age 3600)))
         (removed 0))
    (maphash (lambda (id session)
              (when (and (memq (efrit-session-status session)
                              '(complete cancelled))
                        (time-less-p (efrit-session-start-time session)
                                   cutoff-time))
                (remhash id efrit-session--registry)
                (cl-incf removed)))
            efrit-session--registry)
    (when (> removed 0)
      (efrit-log 'info "Cleaned up %d old sessions" removed))
    removed))

;;; User Commands

;;;###autoload
(defun efrit-session-show-status ()
  "Show current multi-step session status."
  (interactive)
  (cond
   (efrit-session--active
    (let* ((session efrit-session--active)
           (elapsed (float-time (time-since (efrit-session-start-time session))))
           (steps (length (efrit-session-work-log session))))
      (message "Efrit: Session %s - %s (%.1fs, %d steps, %d queued)"
               (efrit-session-id session)
               (symbol-name (efrit-session-status session))
               elapsed steps
               (length efrit-session--queue))))
   ((> (length efrit-session--queue) 0)
    (message "Efrit: No active session, %d commands queued"
             (length efrit-session--queue)))
   (t
    (message "No active Efrit session"))))

;;;###autoload
(defun efrit-session-show-queue ()
  "Show the current session queue."
  (interactive)
  (let ((buffer (get-buffer-create "*Efrit Session Queue*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "Efrit Session Queue\n")
      (insert "===================\n\n")

      (if efrit-session--active
          (insert (format "Active: %s\n\n"
                         (efrit-common-truncate-string
                          (efrit-session-command efrit-session--active) 60)))
        (insert "Active: None\n\n"))

      (if (null efrit-session--queue)
          (insert "Queue is empty.")
        (insert (format "Queued commands (%d):\n\n"
                       (length efrit-session--queue)))
        (cl-loop for cmd in efrit-session--queue
                 for i from 1
                 do (insert (format "%d. %s\n" i
                                   (efrit-common-truncate-string cmd 70))))))
    (switch-to-buffer buffer)))

;;;###autoload
(defun efrit-show-queue ()
  "Display the current command queue for efrit-do-async."
  (interactive)
  (if (zerop (efrit-session-queue-length))
      (message "Efrit queue is empty")
    (with-current-buffer (get-buffer-create "*efrit-queue*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Efrit Command Queue\n")
        (insert "===================\n\n")
        (if (efrit-session-active)
            (insert (format "Currently executing: %s\n\n"
                           (efrit-session-command (efrit-session-active))))
          (insert "No command currently executing\n\n"))
        (insert (format "Queued commands: %d\n\n" (efrit-session-queue-length)))
        (let ((pos 1))
          (dolist (command efrit-session--queue)
            (insert (format "%d. %s\n" pos command))
            (setq pos (1+ pos))))
        (goto-char (point-min))
        (view-mode))
      (display-buffer (current-buffer)))))

;;;###autoload
(defun efrit-show-session ()
  "Display information about the active efrit session."
  (interactive)
  (let ((session (efrit-session-active)))
    (if (not session)
        (message "No active efrit session")
      (with-current-buffer (get-buffer-create "*efrit-session*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert "Efrit Session Information\n")
          (insert "=========================\n\n")
          (insert (format "Session ID: %s\n" (efrit-session-id session)))
          (insert (format "Status: %s\n" (efrit-session-status session)))
          (insert (format "Command: %s\n" (efrit-session-command session)))
          (insert (format "Start Time: %s\n"
                         (format-time-string "%Y-%m-%d %H:%M:%S"
                                           (efrit-session-start-time session))))
          (insert (format "Elapsed: %.1f seconds\n"
                         (float-time (time-since (efrit-session-start-time session)))))
          (insert (format "Continuations: %d\n" (or (efrit-session-continuation-count session) 0)))
          (insert (format "Buffer: %s\n" (or (efrit-session-buffer session) "N/A")))
          (when (efrit-session-last-error session)
            (insert (format "\nLast Error: %s\n" (efrit-session-last-error session))))

          ;; Progress metrics
          (insert "\nProgress Metrics:\n")
          (insert (format "  Buffer modifications: %d\n"
                         (or (efrit-session-buffer-modifications session) 0)))
          (insert (format "  TODO status changes: %d\n"
                         (or (efrit-session-todo-status-changes session) 0)))
          (insert (format "  Buffers created: %d\n"
                         (or (efrit-session-buffers-created session) 0)))
          (insert (format "  Files modified: %d\n"
                         (or (efrit-session-files-modified session) 0)))
          (insert (format "  Execution outputs: %d\n"
                         (or (efrit-session-execution-outputs session) 0)))

          ;; Tool history
          (when-let* ((history (efrit-session-tool-history session)))
            (insert (format "\nTool History (%d calls):\n" (length history)))
            (dolist (entry (reverse (seq-take (reverse history) 10)))
              (insert (format "  - %s at %s\n"
                             (car entry)
                             (format-time-string "%H:%M:%S" (nth 1 entry))))))

          ;; Work log summary
          (when-let* ((work-log (efrit-session-work-log session)))
            (insert (format "\nWork Log Entries: %d\n" (length work-log))))

          (goto-char (point-min))
          (view-mode))
        (display-buffer (current-buffer))))))

;;; Work Log and Event Management (NEW FUNCTIONS)

(defun efrit-session-add-work-item (session action result &optional timestamp)
  "Add work item to SESSION tracking what Claude accomplished.
ACTION is a string describing what was done.
RESULT is the outcome of the action.
TIMESTAMP defaults to current time if not provided.
Returns the work item entry."
  (when session
    (let ((ts (or timestamp (current-time)))
          (work-item (list action result nil)))
      ;; Work items are tracked alongside the work-log
      ;; This stores simplified tracking for progress visibility
      (setcar (cddr work-item) ts)
      work-item)))

(defun efrit-session-get-work-log (session)
  "Get the work log entries from SESSION.
Returns a list of work log entries, or nil if none."
  (when session
    (efrit-session-work-log session)))

(defun efrit-session-add-event (session event-type event-data)
  "Add execution event to SESSION for progress tracking.
EVENT-TYPE is one of: `tool_started', `tool_result', `message',
`todo_updated', `complete'.
EVENT-DATA is an alist with event-specific data.
Returns the event entry."
  (when session
    (let ((event (list :type event-type
                      :data event-data
                      :timestamp (current-time))))
      (setf (efrit-session-execution-events session)
            (append (efrit-session-execution-events session) (list event)))
      (efrit-log 'debug "Session %s: event %s added"
                 (efrit-session-id session)
                 event-type)
      event)))

(defun efrit-session-get-events (session)
  "Get all execution events from SESSION.
Returns a list of event entries, or nil if none."
  (when session
    (efrit-session-execution-events session)))

(defun efrit-session-set-status (session status)
  "Update SESSION's status to STATUS.
STATUS should be one of: `running', `waiting_for_user', `complete',
`interrupted'."
  (when session
    (setf (efrit-session-status session) status)
    (efrit-log 'info "Session %s: status set to %s"
               (efrit-session-id session)
               status)))

(defun efrit-session-request-interrupt (session)
  "Signal that SESSION should be gracefully interrupted.
Sets the interrupt-requested flag without immediately terminating."
  (when session
    (setf (efrit-session-interrupt-requested session) t)
    (efrit-log 'info "Session %s: interrupt requested"
               (efrit-session-id session))))

(defun efrit-session-should-interrupt-p (session)
  "Check if SESSION has been requested to interrupt.
Returns non-nil if interrupt was requested."
  (when session
    (efrit-session-interrupt-requested session)))

(defun efrit-session-queue-command (session command)
  "Add COMMAND to SESSION's command queue for execution.
Commands are dequeued and executed one at a time.
Returns t if added successfully."
  (when session
    (setf (efrit-session-command-queue session)
          (append (efrit-session-command-queue session) (list command)))
    (efrit-log 'debug "Session %s: command queued (queue length: %d)"
               (efrit-session-id session)
               (length (efrit-session-command-queue session)))
    t))

(defun efrit-session-dequeue-command (session)
  "Remove and return the next command from SESSION's queue, or nil if empty."
  (when session
    (let* ((queue (efrit-session-command-queue session))
           (next-command (car queue)))
      (when next-command
        (setf (efrit-session-command-queue session) (cdr queue))
        (efrit-log 'debug "Session %s: command dequeued (remaining: %d)"
                   (efrit-session-id session)
                   (length (cdr queue)))
        next-command))))

(defun efrit-session-command-queue-length (session)
  "Get the number of commands queued in SESSION."
  (when session
    (length (efrit-session-command-queue session))))

(provide 'efrit-session-core)

;;; efrit-session-core.el ends here
