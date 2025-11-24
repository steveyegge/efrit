;;; efrit-session.el --- Session and context management for Efrit -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.3.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; Session state and context management for Efrit's multi-step execution.
;; This module handles:
;; - Session data structures and lifecycle
;; - Work log compression for efficient Claude context
;; - Session queueing for async operations
;; - Progress tracking and state management
;;
;; Following the Zero Client-Side Intelligence principle, this module
;; only RECORDS what happened - it does not make decisions about what
;; to do next. That's Claude's job.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'efrit-log)
(require 'efrit-common)

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

;;; Session Data Structure

(cl-defstruct efrit-session
  "Session tracking for multi-step operations."
  id                      ; Unique identifier from Claude
  command                 ; Original command from user
  work-log                ; List of (result elisp todo-snapshot) tuples
  start-time              ; When session started
  status                  ; 'active, 'waiting, 'complete
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

;;; Session Registry

(defvar efrit-session--active nil
  "Currently active session.")

(defvar efrit-session--queue nil
  "Queue of pending sessions.")

(defvar efrit-session--registry (make-hash-table :test 'equal)
  "Hash table of all sessions by ID.")

;;; Session Lifecycle

(defun efrit-session-create (id command)
  "Create a new session with ID and COMMAND."
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
  "Get session by ID, or return nil if not found."
  (gethash id efrit-session--registry))

(defun efrit-session-active ()
  "Return the currently active session, or nil."
  efrit-session--active)

(defun efrit-session-set-active (session)
  "Set SESSION as the active session."
  (setq efrit-session--active session)
  (when session
    (efrit-log 'debug "Active session: %s" (efrit-session-id session))))

(defun efrit-session-complete (session &optional result)
  "Mark SESSION as complete with optional final RESULT."
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
  "Cancel SESSION and mark it as aborted."
  (when session
    (setf (efrit-session-status session) 'cancelled)
    (efrit-log 'info "Cancelled session %s" (efrit-session-id session))
    (when (eq session efrit-session--active)
      (setq efrit-session--active nil))))

;;; Work Log Management

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

;;; Work Log Compression

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

;;; Progress Tracking

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
  "Show current session status."
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
;; Simple command history ring for efrit-do

(require 'ring)

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
    (require 'efrit-config)
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
  (require 'efrit-config)
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

(provide 'efrit-session)
;;; efrit-session.el ends here
