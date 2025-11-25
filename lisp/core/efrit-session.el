;;; efrit-session.el --- Session and context management for Efrit -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.3.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; Unified session management for Efrit, providing:
;; - Multi-step execution session tracking (from efrit-session.el)
;; - Metrics tracking and persistence (from efrit-session-tracker.el)
;; - Session data structures and lifecycle
;; - Work log compression for efficient Claude context
;; - Session queueing for async operations
;; - Progress tracking and state management
;; - Command history ring for efrit-do
;;
;; Following the Zero Client-Side Intelligence principle, this module
;; only RECORDS what happened - it does not make decisions about what
;; to do next. That's Claude's job.
;;
;; This consolidates functionality from efrit-session.el and efrit-session-tracker.el

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'ring)
(require 'subr-x)
(require 'efrit-log)
(require 'efrit-common)
(require 'efrit-config)

;;; Customization

(defgroup efrit-session nil
  "Session management for Efrit."
  :group 'efrit
  :prefix "efrit-session-")

(defcustom efrit-session-max-work-log-entries 50
  "Maximum number of work log entries to keep in session history."
  :type 'integer
  :group 'efrit-session)

(defcustom efrit-session-max-queue-size 20
  "Maximum number of sessions allowed in the queue."
  :type 'integer
  :group 'efrit-session)

(defcustom efrit-session-context-compression 'smart
  "Level of context compression for work logs.
- minimal: Only essential information
- smart: Balanced compression (default)
- full: Include all available context"
  :type '(choice (const :tag "Minimal" minimal)
                 (const :tag "Smart" smart)
                 (const :tag "Full" full))
  :group 'efrit-session)

(defcustom efrit-session-max-result-length 200
  "Maximum length of result strings in compressed logs."
  :type 'integer
  :group 'efrit-session)

(defcustom efrit-session-tracking-enabled t
  "Whether session tracking and metrics are enabled."
  :type 'boolean
  :group 'efrit-session)

(defcustom efrit-session-save-interval 60
  "Interval in seconds between automatic session saves."
  :type 'number
  :group 'efrit-session)

(defcustom efrit-context-ring-size 10
  "Size of the command history ring."
  :type 'integer
  :group 'efrit-session)

(defcustom efrit-context-persist-file nil
  "File to persist context data.
If nil, uses the default location in the efrit data directory."
  :type '(choice (const :tag "Default location" nil)
                 (file :tag "Custom file"))
  :group 'efrit-session)

;;; Session Data Structure (Multi-step Execution)

(cl-defstruct efrit-session
  "Session tracking for multi-step operations."
  id                      ; Unique identifier from Claude
  command                 ; Original command from user
  work-log                ; List of (result elisp todo-snapshot) tuples
  start-time              ; When session started
  status                  ; 'active, 'waiting, 'complete, 'cancelled, 'waiting-for-user
  buffer                  ; Original buffer for context
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
  ;; Conversation history for interactive sessions
  conversation-history    ; List of messages: (role content timestamp)
  pending-question        ; Current question waiting for user input
  user-responses)         ; List of user responses received

;;; Session Registry (Multi-step Execution)

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

;;; Metrics Tracking State Variables

(defvar efrit-session-id nil
  "Current metrics tracking session identifier.")

(defvar efrit-session-start-time nil
  "When the current metrics tracking session started.")

(defvar efrit-session-metrics
  '((commands-executed . 0)
    (todos-created . 0)
    (todos-completed . 0)
    (api-calls . 0)
    (errors-encountered . 0)
    (buffers-created . ())
    (files-modified . ())
    (tools-used . ()))
  "Current session metrics.")

(defvar efrit-session-save-timer nil
  "Timer for periodic session saves.")

;;; Session Lifecycle (Multi-step Execution)

(defun efrit-session-create (id command)
  "Create a new multi-step session with ID and COMMAND."
  (let ((session (make-efrit-session
                  :id id
                  :command command
                  :start-time (current-time)
                  :status 'active
                  :buffer (current-buffer)
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
                  :conversation-history nil
                  :pending-question nil
                  :user-responses nil)))
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

;;; Work Log Management (Multi-step Execution)

(defun efrit-session-add-work (session result elisp &optional todo-snapshot)
  "Add a work log entry to SESSION.
Records RESULT from executing ELISP, with optional TODO-SNAPSHOT."
  (when session
    (let ((entry (list result elisp todo-snapshot)))
      (push entry (efrit-session-work-log session))

      ;; Limit work log size to prevent memory growth
      (when (> (length (efrit-session-work-log session))
               efrit-session-max-work-log-entries)
        (setf (efrit-session-work-log session)
              (seq-take (efrit-session-work-log session)
                       efrit-session-max-work-log-entries)))

      ;; Also add to unified context (tool call and result)
      (efrit-unified-context-add-message 'user elisp 'async
                                        `((session . ,(efrit-session-id session))))
      (when result
        (efrit-unified-context-add-message 'assistant result 'async
                                          `((session . ,(efrit-session-id session)))))

      (efrit-log 'debug "Session %s: work log entry %d added"
                 (efrit-session-id session)
                 (length (efrit-session-work-log session))))))

(defun efrit-session-track-tool (session tool-name input-data result)
  "Track tool call in SESSION for loop detection.
Records TOOL-NAME, INPUT-DATA, and RESULT."
  (when session
    (let* ((timestamp (current-time))
           (progress-tick (or (efrit-session-last-progress-tick session) 0))
           (input-hash (secure-hash 'sha1 (format "%s" input-data)))
           (result-hash (secure-hash 'sha1 (format "%s" result)))
           (entry (list tool-name timestamp progress-tick input-hash result-hash)))
      (push entry (efrit-session-tool-history session))

      ;; Update last-tool-called for simple repeat detection
      (setf (efrit-session-last-tool-called session) tool-name))))

;;; Work Log Compression (Multi-step Execution)

(defun efrit-session-compress-log (session)
  "Compress SESSION work log for efficient Claude context usage.
Returns a JSON string representation."
  (if (not session)
      "[]"
    (let ((work-log (efrit-session-work-log session)))
      (cl-case efrit-session-context-compression
        (minimal (efrit-session--compress-minimal work-log))
        (smart (efrit-session--compress-smart work-log))
        (full (efrit-session--compress-full work-log))
        (t (efrit-session--compress-smart work-log))))))

(defun efrit-session--compress-minimal (work-log)
  "Minimal compression - only count and last result."
  (let ((total-steps (length work-log))
        (last-result (when work-log (caar work-log))))
    (json-encode
     `((steps . ,total-steps)
       (last_result . ,(if last-result
                          (efrit-common-truncate-string
                           (format "%s" last-result)
                           efrit-session-max-result-length)
                        "none"))))))

(defun efrit-session--compress-smart (work-log)
  "Smart compression - recent entries with truncation."
  (let* ((total-steps (length work-log))
         (recent-entries (seq-take work-log 5))
         (compressed
          (mapcar (lambda (entry)
                   (let ((result (nth 0 entry))
                         (elisp (nth 1 entry))
                         (todos (nth 2 entry)))
                     `((elisp . ,(efrit-session--compress-code elisp))
                       (result . ,(efrit-session--compress-result result))
                       ,@(when todos
                           `((todos . ,(length todos)))))))
                 recent-entries)))
    (json-encode
     `((total_steps . ,total-steps)
       (recent . ,compressed)))))

(defun efrit-session--compress-full (work-log)
  "Full compression - all entries with classification."
  (let ((compressed
         (mapcar (lambda (entry)
                  (let ((result (nth 0 entry))
                        (elisp (nth 1 entry)))
                    `((elisp . ,elisp)
                      (result . ,(efrit-session--compress-result result))
                      (type . ,(efrit-session--classify-code elisp)))))
                work-log)))
    (json-encode `((entries . ,compressed)))))

(defun efrit-session--compress-code (code)
  "Compress CODE string for context."
  (cond
   ((< (length code) 80) code)
   ((string-match "\\(buffer-substring[^[:space:]]*\\|insert\\|delete-region\\)" code)
    (concat "(" (match-string 1 code) " ...)"))
   (t (efrit-common-truncate-string code 97))))

(defun efrit-session--compress-result (result)
  "Compress RESULT for context usage."
  (let ((result-str (format "%s" result)))
    (cond
     ((or (null result) (string-empty-p result-str)) "nil")
     ((string-match "^#<buffer \\(.+\\)>$" result-str)
      (format "buffer:%s" (match-string 1 result-str)))
     ((> (length result-str) efrit-session-max-result-length)
      (concat (efrit-common-truncate-string
               result-str
               (- efrit-session-max-result-length 3))
              "..."))
     (t result-str))))

(defun efrit-session--classify-code (code)
  "Classify CODE into operation type."
  (cond
   ((string-match "eval-sexp\\|funcall\\|apply" code) 'evaluation)
   ((string-match "insert\\|delete\\|replace" code) 'text-modification)
   ((string-match "find-file\\|switch-to-buffer" code) 'navigation)
   ((string-match "shell-command\\|call-process" code) 'external-command)
   ((string-match "create-buffer\\|generate-new-buffer" code) 'buffer-creation)
   (t 'other)))

;;; Progress Tracking (Multi-step Execution)

(defun efrit-session-record-progress (session progress-type)
  "Record that progress was made in SESSION.
PROGRESS-TYPE can be: buffer-modification, todo-change, buffer-creation,
file-modification, execution-output."
  (when session
    (pcase progress-type
      ('buffer-modification
       (cl-incf (efrit-session-buffer-modifications session)))
      ('todo-change
       (cl-incf (efrit-session-todo-status-changes session)))
      ('buffer-creation
       (cl-incf (efrit-session-buffers-created session)))
      ('file-modification
       (cl-incf (efrit-session-files-modified session)))
      ('execution-output
       (cl-incf (efrit-session-execution-outputs session))))

    ;; Update progress tick
    (setf (efrit-session-last-progress-tick session)
          (+ (efrit-session-buffer-modifications session)
             (efrit-session-todo-status-changes session)
             (efrit-session-buffers-created session)
             (efrit-session-files-modified session)
             (efrit-session-execution-outputs session)))))

(defun efrit-session-progress-made-p (session)
  "Return t if material progress was made since last check in SESSION."
  (when session
    (let ((last-tick (or (efrit-session-last-progress-tick session) 0))
          (current-tick (+ (or (efrit-session-buffer-modifications session) 0)
                          (or (efrit-session-todo-status-changes session) 0)
                          (or (efrit-session-buffers-created session) 0)
                          (or (efrit-session-files-modified session) 0)
                          (or (efrit-session-execution-outputs session) 0))))
      (> current-tick last-tick))))

;;; Conversation History Management (for interactive sessions)

(defun efrit-session-add-message (session role content)
  "Add a message to SESSION's conversation history.
ROLE is either \\='user or \\='assistant.
CONTENT is the message text."
  (when session
    (let ((message (list role content (format-time-string "%Y-%m-%dT%H:%M:%S%z"))))
      (setf (efrit-session-conversation-history session)
            (append (efrit-session-conversation-history session) (list message)))
      (efrit-log 'debug "Added %s message to conversation" role))))

(defun efrit-session-get-conversation (session)
  "Get the conversation history from SESSION as a list.
Each item is (role content timestamp)."
  (when session
    (efrit-session-conversation-history session)))

(defun efrit-session-set-pending-question (session question &optional options)
  "Set QUESTION as pending user input in SESSION.
OPTIONS is an optional list of choices for the user."
  (when session
    (setf (efrit-session-pending-question session)
          (list question options (format-time-string "%Y-%m-%dT%H:%M:%S%z")))
    (setf (efrit-session-status session) 'waiting-for-user)
    (efrit-log 'info "Session %s waiting for user input: %s"
               (efrit-session-id session)
               (efrit-common-truncate-string question 60))))

(defun efrit-session-get-pending-question (session)
  "Get the pending question from SESSION.
Returns (question options timestamp) or nil."
  (when session
    (efrit-session-pending-question session)))

(defun efrit-session-respond-to-question (session response)
  "Record user RESPONSE to pending question in SESSION.
Clears the pending question and returns the session to active status."
  (when (and session (efrit-session-pending-question session))
    ;; Record the response
    (let ((response-entry (list response
                                (car (efrit-session-pending-question session))
                                (format-time-string "%Y-%m-%dT%H:%M:%S%z"))))
      (setf (efrit-session-user-responses session)
            (append (efrit-session-user-responses session) (list response-entry))))
    ;; Add to conversation history
    (efrit-session-add-message session 'user response)
    ;; Clear pending and resume
    (setf (efrit-session-pending-question session) nil)
    (setf (efrit-session-status session) 'active)
    (efrit-log 'info "Session %s received user response"
               (efrit-session-id session))
    response))

(defun efrit-session-waiting-for-user-p (session)
  "Return non-nil if SESSION is waiting for user input."
  (and session
       (eq (efrit-session-status session) 'waiting-for-user)))

(defun efrit-session-format-conversation-for-api (session)
  "Format SESSION's conversation history for Claude API.
Returns a vector of message objects suitable for the messages array."
  (when session
    (let ((history (efrit-session-conversation-history session)))
      (if (null history)
          ;; Just the original command
          (vector `((role . "user")
                   (content . ,(efrit-session-command session))))
        ;; Full conversation
        (vconcat
         (list `((role . "user")
                (content . ,(efrit-session-command session))))
         (mapcar (lambda (msg)
                  (let ((role (nth 0 msg))
                        (content (nth 1 msg)))
                    `((role . ,(if (eq role 'user) "user" "assistant"))
                      (content . ,content))))
                history))))))

;;; Session Queue Management (Multi-step Execution)

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

;;; Session Cleanup (Multi-step Execution)

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

;;; Metrics Tracking Functions

(defun efrit-session-generate-id ()
  "Generate a unique session ID for metrics tracking."
  (format "efrit-%s-%04x"
          (format-time-string "%Y%m%d-%H%M%S")
          (random 65536)))

;;;###autoload
(defun efrit-session-start ()
  "Start a new Efrit metrics tracking session."
  (interactive)
  (setq efrit-session-id (efrit-session-generate-id))
  (setq efrit-session-start-time (current-time))
  (setq efrit-session-metrics
        '((commands-executed . 0)
          (todos-created . 0)
          (todos-completed . 0)
          (api-calls . 0)
          (errors-encountered . 0)
          (buffers-created . ())
          (files-modified . ())
          (tools-used . ())))
  (efrit-session-log "Session started" 'info)
  ;; Only show message in interactive sessions, not during compilation
  (unless noninteractive
    (message "Efrit session started: %s" efrit-session-id)))

(defun efrit-session-end ()
  "End the current Efrit metrics tracking session."
  (interactive)
  (when efrit-session-id
    (efrit-session-save-final)
    (efrit-session-log "Session ended" 'info)
    ;; Only show message in interactive sessions, not during compilation
    (unless noninteractive
      (message "Efrit session ended: %s" efrit-session-id))
    (setq efrit-session-id nil)
    (setq efrit-session-start-time nil))
  ;; Always persist unified context and context ring on session end
  (efrit-unified-context-persist)
  (efrit-context-ring-persist))

(defun efrit-session-track-command (command)
  "Track execution of COMMAND."
  (when efrit-session-tracking-enabled
    (efrit-session-increment-metric 'commands-executed)
    (efrit-session-log (format "Command executed: %s" command) 'debug)))

(defun efrit-session-track-todo-created (todo)
  "Track creation of TODO item."
  (when efrit-session-tracking-enabled
    (efrit-session-increment-metric 'todos-created)
    (efrit-session-log (format "TODO created: %s" (truncate-string-to-width todo 50)) 'debug)))

(defun efrit-session-track-todo-completed (todo)
  "Track completion of TODO item."
  (when efrit-session-tracking-enabled
    (efrit-session-increment-metric 'todos-completed)
    (efrit-session-log (format "TODO completed: %s" (truncate-string-to-width todo 50)) 'debug)))

(defun efrit-session-track-api-call (endpoint)
  "Track API call to ENDPOINT."
  (when efrit-session-tracking-enabled
    (efrit-session-increment-metric 'api-calls)
    (efrit-session-log (format "API call: %s" endpoint) 'debug)))

(defun efrit-session-track-error (error-msg)
  "Track ERROR-MSG encountered."
  (when efrit-session-tracking-enabled
    (efrit-session-increment-metric 'errors-encountered)
    (efrit-session-log (format "Error: %s" error-msg) 'error)))

(defun efrit-session-track-buffer-created (buffer-name)
  "Track creation of BUFFER-NAME."
  (when efrit-session-tracking-enabled
    (let ((buffers (alist-get 'buffers-created efrit-session-metrics)))
      (unless (member buffer-name buffers)
        (setf (alist-get 'buffers-created efrit-session-metrics)
              (cons buffer-name buffers))))
    (efrit-session-log (format "Buffer created: %s" buffer-name) 'debug)))

(defun efrit-session-track-file-modified (file-path)
  "Track modification of FILE-PATH."
  (when efrit-session-tracking-enabled
    (let ((files (alist-get 'files-modified efrit-session-metrics)))
      (unless (member file-path files)
        (setf (alist-get 'files-modified efrit-session-metrics)
              (cons file-path files))))
    (efrit-session-log (format "File modified: %s" file-path) 'debug)))

(defun efrit-session-track-tool-used (tool-name)
  "Track usage of TOOL-NAME."
  (when efrit-session-tracking-enabled
    (let* ((tools (alist-get 'tools-used efrit-session-metrics))
           (existing (assoc tool-name tools))
           (count (if existing (1+ (cdr existing)) 1)))
      (setf (alist-get 'tools-used efrit-session-metrics)
            (cons (cons tool-name count)
                  (delq existing tools))))
    (efrit-session-log (format "Tool used: %s" tool-name) 'debug)))

(defun efrit-session-increment-metric (metric-key)
  "Increment the counter for METRIC-KEY."
  (cl-incf (alist-get metric-key efrit-session-metrics 0)))

(defun efrit-session-get-metric (metric-key)
  "Get the value of METRIC-KEY."
  (alist-get metric-key efrit-session-metrics))

(defun efrit-session-get-duration ()
  "Get the duration of the current session in seconds."
  (if efrit-session-start-time
      (float-time (time-subtract (current-time) efrit-session-start-time))
    0))

(defun efrit-session-get-summary ()
  "Get a summary of the current session."
  (when efrit-session-id
    (list :id efrit-session-id
          :start-time efrit-session-start-time
          :duration (efrit-session-get-duration)
          :metrics efrit-session-metrics)))

;;; Persistence

(defun efrit-session-save ()
  "Save current active session state to disk (multi-step execution).
Also saves legacy session tracking data for compatibility."
  ;; Save new-style session struct (multi-step execution)
  (when-let* ((session (efrit-session-active)))
    (let* ((sessions-dir (expand-file-name "sessions" efrit-data-directory))
           (session-file (expand-file-name
                         (concat (efrit-session-id session) ".json")
                         sessions-dir))
           (session-data `((id . ,(efrit-session-id session))
                          (command . ,(efrit-session-command session))
                          (status . ,(efrit-session-status session))
                          (start-time . ,(format-time-string "%Y-%m-%dT%H:%M:%S"
                                                            (efrit-session-start-time session)))
                          (continuation-count . ,(efrit-session-continuation-count session))
                          (buffer . ,(when (efrit-session-buffer session)
                                      (buffer-name (efrit-session-buffer session))))
                          (last-error . ,(efrit-session-last-error session)))))
      (make-directory sessions-dir t)
      (with-temp-file session-file
        (insert (json-encode session-data)))))

  ;; Also save legacy session tracking (for compatibility)
  (when (and efrit-session-id efrit-session-tracking-enabled)
    (let* ((sessions-dir (expand-file-name "sessions" efrit-data-directory))
           (session-file (expand-file-name (concat efrit-session-id "-legacy.json") sessions-dir))
           (session-data (efrit-session-get-summary)))
      (make-directory sessions-dir t)
      (with-temp-file session-file
        (insert (json-encode session-data))))))

(defun efrit-session-save-final ()
  "Save final session state with completion marker."
  (when efrit-session-id
    (efrit-session-save)
    ;; Also save to a 'completed' subdirectory
    (let* ((completed-dir (expand-file-name "completed"
                                           (expand-file-name "sessions" efrit-data-directory)))
           (completed-file (expand-file-name (concat efrit-session-id ".json") completed-dir))
           (session-data (append (efrit-session-get-summary)
                                (list :completed-at (current-time)))))
      (make-directory completed-dir t)
      (with-temp-file completed-file
        (insert (json-encode session-data))))))

(defun efrit-session-load (session-id)
  "Load session SESSION-ID from disk."
  (let* ((sessions-dir (expand-file-name "sessions" efrit-data-directory))
         (session-file (expand-file-name (concat session-id ".json") sessions-dir)))
    (when (file-exists-p session-file)
      (with-temp-buffer
        (insert-file-contents session-file)
        (condition-case nil
            (json-read-from-string (buffer-string))
          (error nil))))))

(defun efrit-session-list-sessions ()
  "List all available sessions."
  (let ((sessions-dir (expand-file-name "sessions" efrit-data-directory)))
    (when (file-directory-p sessions-dir)
      (mapcar (lambda (file)
                (file-name-sans-extension file))
              (directory-files sessions-dir nil "\\.json$")))))

(defun efrit-session-log (message level)
  "Log MESSAGE at LEVEL."
  (let* ((logs-dir (expand-file-name "logs" efrit-data-directory))
         (log-file (expand-file-name "session.log" logs-dir))
         (timestamp (format-time-string "%Y-%m-%d %H:%M:%S"))
         (session-prefix (if efrit-session-id
                            (format "[%s] " efrit-session-id)
                          "[no-session] "))
         (log-entry (format "%s %s%s: %s\n"
                           timestamp
                           session-prefix
                           (upcase (symbol-name level))
                           message)))
    (make-directory logs-dir t)
    (append-to-file log-entry nil log-file)))

(defun efrit-session-ensure-active ()
  "Ensure there's an active metrics session, creating one if needed."
  (unless efrit-session-id
    (efrit-session-start)))

(defun efrit-session-start-auto-save ()
  "Start automatic session saving."
  (when efrit-session-save-timer
    (cancel-timer efrit-session-save-timer))
  (setq efrit-session-save-timer
        (run-with-timer efrit-session-save-interval
                        efrit-session-save-interval
                        'efrit-session-save)))

(defun efrit-session-stop-auto-save ()
  "Stop automatic session saving."
  (when efrit-session-save-timer
    (cancel-timer efrit-session-save-timer)
    (setq efrit-session-save-timer nil)))

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

;;; Unified Context System
;; This provides a single source of truth for context across all modes

(defvar efrit-unified-context--message-history nil
  "Global message history shared across all modes.
Each entry is an alist with keys: role, content, timestamp, mode, metadata.")

(defvar efrit-unified-context--max-history 100
  "Maximum number of messages to retain in unified history.")

(defun efrit-unified-context-add-message (role content &optional mode metadata)
  "Add a message to unified context history.
ROLE is \\='user or \\='assistant.
CONTENT is the message text.
MODE is the originating mode (\\='chat, \\='do, \\='async, etc.).
METADATA is optional additional data (alist)."
  (let ((entry `((role . ,role)
                (content . ,content)
                (timestamp . ,(current-time))
                (mode . ,(or mode 'unknown))
                (metadata . ,metadata))))
    (push entry efrit-unified-context--message-history)
    ;; Trim to max length
    (when (> (length efrit-unified-context--message-history)
             efrit-unified-context--max-history)
      (setq efrit-unified-context--message-history
            (seq-take efrit-unified-context--message-history
                     efrit-unified-context--max-history)))
    entry))

(defun efrit-unified-context-get-messages (&optional n mode-filter)
  "Get N most recent messages from unified history.
If MODE-FILTER is provided, only return messages from that mode.
Returns messages in chronological order (oldest first)."
  (let* ((count (or n (length efrit-unified-context--message-history)))
         (filtered (if mode-filter
                      (seq-filter (lambda (msg)
                                   (eq (alist-get 'mode msg) mode-filter))
                                 efrit-unified-context--message-history)
                    efrit-unified-context--message-history))
         (recent (seq-take filtered count)))
    (nreverse recent)))

(defun efrit-unified-context-get-for-api (n &optional include-metadata)
  "Get N recent messages formatted for Claude API.
Returns a vector suitable for the \\='messages field.
If INCLUDE-METADATA is nil, strips metadata fields."
  (let ((messages (efrit-unified-context-get-messages n)))
    (vconcat
     (mapcar (lambda (msg)
              (if include-metadata
                  `(("role" . ,(symbol-name (alist-get 'role msg)))
                   ("content" . ,(alist-get 'content msg))
                   ("metadata" . ,(alist-get 'metadata msg)))
                `(("role" . ,(symbol-name (alist-get 'role msg)))
                 ("content" . ,(alist-get 'content msg)))))
            messages))))

(defun efrit-unified-context-format-for-system-prompt (&optional n)
  "Format N recent messages as a string for system prompt inclusion.
Returns a compressed summary suitable for system prompt context."
  (let ((messages (efrit-unified-context-get-messages (or n 5))))
    (if (null messages)
        ""
      (concat
       "\n\nRECENT CONTEXT:\n"
       (mapconcat
        (lambda (msg)
          (let ((role (alist-get 'role msg))
                (content (alist-get 'content msg))
                (mode (alist-get 'mode msg))
                (timestamp (alist-get 'timestamp msg)))
            (format "[%s|%s] %s: %s"
                    (format-time-string "%H:%M:%S" timestamp)
                    mode
                    (capitalize (symbol-name role))
                    (efrit-common-truncate-string content 150))))
        messages
        "\n")))))

(defun efrit-unified-context-clear (&optional mode-filter)
  "Clear unified context history.
If MODE-FILTER is provided, only clear messages from that mode."
  (if mode-filter
      (setq efrit-unified-context--message-history
            (seq-remove (lambda (msg)
                         (eq (alist-get 'mode msg) mode-filter))
                       efrit-unified-context--message-history))
    (setq efrit-unified-context--message-history nil)))

(defun efrit-unified-context-persist ()
  "Persist unified context to file."
  (when efrit-unified-context--message-history
    (let ((file (efrit-config-context-file "efrit-unified-context.el")))
      (with-temp-file file
        (insert ";; Efrit unified context data\n")
        (insert ";; Saved: " (current-time-string) "\n\n")
        (prin1 efrit-unified-context--message-history (current-buffer)))
      (efrit-log 'debug "Persisted %d messages to unified context"
                 (length efrit-unified-context--message-history)))))

(defun efrit-unified-context-restore ()
  "Restore unified context from file."
  (let ((file (efrit-config-context-file "efrit-unified-context.el")))
    (when (file-exists-p file)
      (condition-case err
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (search-forward "\n\n" nil t)
            (let ((messages (read (current-buffer))))
              (setq efrit-unified-context--message-history messages)
              (efrit-log 'debug "Restored %d messages from unified context"
                        (length messages))))
        (error
         (efrit-log 'warn "Failed to restore unified context: %s"
                   (efrit-common-safe-error-message err)))))))

(defun efrit-unified-context-init ()
  "Initialize unified context system."
  (efrit-unified-context-restore)
  (efrit-log 'debug "Unified context system initialized"))

;;; Context Ring (Backward Compatibility)

(cl-defstruct (efrit-context-item
            (:constructor efrit-context-item-create)
            (:type vector))
  "Context item structure for command history."
  timestamp
  command
  result
  buffer
  directory
  window-config
  metadata)

(defvar efrit-context--ring nil
  "Ring buffer for context items.")

(defvar efrit-context--hooks nil
  "Hooks run when context is captured.")

(defun efrit-context-ring-init ()
  "Initialize or reinitialize the context ring."
  (unless efrit-context--ring
    (setq efrit-context--ring (make-ring efrit-context-ring-size))))

(defun efrit-context-ring-add (command result &optional metadata)
  "Add a new context item for COMMAND with RESULT and optional METADATA."
  (efrit-context-ring-init)
  (let ((item (efrit-context-item-create
               :timestamp (current-time)
               :command command
               :result result
               :buffer (buffer-name)
               :directory default-directory
               :window-config (current-window-configuration)
               :metadata metadata)))
    (ring-insert efrit-context--ring item)
    (run-hook-with-args 'efrit-context--hooks item)
    ;; Also add to unified context
    (efrit-unified-context-add-message 'user command 'do metadata)
    (when result
      (efrit-unified-context-add-message 'assistant result 'do metadata))
    item))

(defun efrit-context-ring-get-recent (&optional n)
  "Get N most recent context items (default all)."
  (when efrit-context--ring
    (let ((ring efrit-context--ring)
          (count (or n (ring-length efrit-context--ring)))
          (items '()))
      (dotimes (i (min count (ring-length ring)))
        (push (ring-ref ring i) items))
      (nreverse items))))

(defun efrit-context-item-to-string (item)
  "Convert context ITEM to string representation."
  (format "[%s] %s -> %s (in %s)"
          (format-time-string "%H:%M:%S" (efrit-context-item-timestamp item))
          (efrit-context-item-command item)
          (truncate-string-to-width (or (efrit-context-item-result item) "") 50 nil nil t)
          (efrit-context-item-buffer item)))

(defun efrit-context-ring-clear ()
  "Clear the context ring."
  (when efrit-context--ring
    (setq efrit-context--ring (make-ring efrit-context-ring-size))))

(defun efrit-context-ring--make-serializable (item)
  "Convert ITEM to a serializable form by removing unreadable objects.
Returns a copy with window-config set to nil since window configurations
cannot be serialized."
  (let ((copy (copy-sequence item)))
    ;; Window configurations are unreadable objects (print as #<window-configuration>)
    ;; Set to nil so the context can be serialized and restored
    (setf (efrit-context-item-window-config copy) nil)
    copy))

(defun efrit-context-ring-persist ()
  "Persist context ring to file."
  (when efrit-context--ring
    (let ((file (or efrit-context-persist-file
                   (efrit-config-context-file "efrit-context-ring.el")))
          (items (efrit-context-ring-get-recent)))
      (when items
        (with-temp-file file
          (insert ";; Efrit context ring data\n")
          (insert ";; Saved: " (current-time-string) "\n\n")
          ;; Convert items to serializable form before persisting
          (prin1 (mapcar #'efrit-context-ring--make-serializable items)
                 (current-buffer)))
        (efrit-log 'debug "Persisted %d context items to %s" (length items) file)))))

(defun efrit-context-ring-restore ()
  "Restore context ring from file."
  (let ((file (or efrit-context-persist-file
                 (efrit-config-context-file "efrit-context-ring.el"))))
    (when (file-exists-p file)
      (condition-case err
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (search-forward "\n\n" nil t)
            (let ((items (read (current-buffer))))
              (efrit-context-ring-init)
              (dolist (item (reverse items))
                (ring-insert efrit-context--ring item))
              (efrit-log 'debug "Restored %d context items from %s"
                        (length items) file)))
        (error
         (efrit-log 'warn "Failed to restore context: %s"
                   (efrit-common-safe-error-message err)))))))

(defun efrit-context-init ()
  "Initialize context system."
  (efrit-unified-context-init)
  (efrit-context-ring-init)
  (efrit-context-ring-restore)
  (efrit-log 'debug "Context system initialized"))

;;; User Commands for Unified Context

;;;###autoload
(defun efrit-show-context (&optional n)
  "Show N most recent messages from unified context (default 10).
Displays context shared across all Efrit modes."
  (interactive "p")
  (let ((count (or n 10))
        (messages (efrit-unified-context-get-messages (or n 10))))
    (if (null messages)
        (message "No context history available")
      (with-output-to-temp-buffer "*Efrit Unified Context*"
        (princ (format "Efrit Unified Context (%d most recent messages):\n\n"
                      (length messages)))
        (dolist (msg messages)
          (let ((timestamp (alist-get 'timestamp msg))
                (mode (alist-get 'mode msg))
                (role (alist-get 'role msg))
                (content (alist-get 'content msg)))
            (princ (format "[%s | %s] %s:\n%s\n\n"
                          (format-time-string "%Y-%m-%d %H:%M:%S" timestamp)
                          (upcase (symbol-name mode))
                          (capitalize (symbol-name role))
                          (efrit-common-truncate-string content 500)))))))))

;;;###autoload
(defun efrit-clear-context (&optional mode-filter)
  "Clear unified context history.
With prefix arg, prompt for which mode to clear (chat, do, async).
Otherwise clears all context."
  (interactive
   (list (when current-prefix-arg
          (intern (completing-read "Clear context for mode: "
                                  '("chat" "do" "async" "all")
                                  nil t)))))
  (if (and mode-filter (not (eq mode-filter 'all)))
      (progn
        (efrit-unified-context-clear mode-filter)
        (message "Cleared %s context" mode-filter))
    (efrit-unified-context-clear)
    (message "Cleared all unified context")))

;;;###autoload
(defun efrit-save-context ()
  "Save unified context to disk."
  (interactive)
  (efrit-unified-context-persist)
  (message "Context saved"))

;;; Hooks for Integration

(defun efrit-session-setup-hooks ()
  "Setup hooks for session tracking integration."
  (add-hook 'kill-emacs-hook 'efrit-session-end)
  (add-hook 'after-change-major-mode-hook 'efrit-session-track-mode-change))

(defun efrit-session-track-mode-change ()
  "Track major mode changes for new buffers."
  (when (and (buffer-name)
             (string-prefix-p "*efrit-" (buffer-name)))
    (efrit-session-track-buffer-created (buffer-name))))

;;; Initialize Session Tracking

(efrit-session-setup-hooks)
(efrit-session-start-auto-save)

;; Note: Session tracking will auto-start on first command execution
;; via efrit-session-ensure-active, not on module load

;;; Session Inspection

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

(provide 'efrit-session)

;;; efrit-session.el ends here
