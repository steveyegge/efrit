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
  status                  ; 'active, 'waiting, 'complete, 'cancelled
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
  last-tool-called)       ; Last tool name for simple repeat detection

;;; Session Registry (Multi-step Execution)

(defvar efrit-session--active nil
  "Currently active multi-step session.")

(defvar efrit-session--queue nil
  "Queue of pending sessions.")

(defvar efrit-session--registry (make-hash-table :test 'equal)
  "Hash table of all multi-step sessions by ID.")

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
                  :last-tool-called nil)))
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
      (setq efrit-session--active nil))))

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
  (message "Efrit session started: %s" efrit-session-id))

(defun efrit-session-end ()
  "End the current Efrit metrics tracking session."
  (interactive)
  (when efrit-session-id
    (efrit-session-save-final)
    (efrit-session-log "Session ended" 'info)
    (message "Efrit session ended: %s" efrit-session-id)
    (setq efrit-session-id nil)
    (setq efrit-session-start-time nil)))

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
  "Save current session state to disk."
  (when (and efrit-session-id efrit-session-tracking-enabled)
    (let* ((sessions-dir (expand-file-name "sessions" efrit-data-directory))
           (session-file (expand-file-name (concat efrit-session-id ".json") sessions-dir))
           (session-data (efrit-session-get-summary)))
      (make-directory sessions-dir t)
      (with-temp-file session-file
        (insert (json-encode session-data)))
      (efrit-session-log (format "Session saved to %s" session-file) 'debug))))

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
          (prin1 items (current-buffer)))
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
  (efrit-context-ring-init)
  (efrit-context-ring-restore)
  (efrit-log 'debug "Context system initialized"))

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

;; Auto-start metrics session if tracking is enabled
(when efrit-session-tracking-enabled
  (efrit-session-ensure-active))

(provide 'efrit-session)

;;; efrit-session.el ends here
