;;; efrit-progress.el --- Progress emission for Efrit operations -*- lexical-binding: t -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.4.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; This module provides progress emission for Efrit operations, allowing
;; external processes (like Claude Code) to monitor what Efrit is doing.
;;
;; Progress is emitted as JSONL (newline-delimited JSON) to a progress file
;; in the session directory. External tools can:
;;   - tail -f the progress file to see real-time updates
;;   - Parse events to track tool calls, results, and Claude's messages
;;   - Inject guidance via the injection file
;;
;; Key concepts:
;;   - Each session has its own progress.jsonl file
;;   - Events are timestamped and typed
;;   - The API is designed to be called from efrit-executor.el
;;
;; Following the Zero Client-Side Intelligence principle, this module
;; only EMITS progress - it does not make decisions based on it.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'efrit-common)
(require 'efrit-log)
(require 'efrit-config)

;;; Customization

(defgroup efrit-progress nil
  "Progress emission for Efrit."
  :group 'efrit
  :prefix "efrit-progress-")

(defcustom efrit-progress-enabled t
  "Whether to emit progress events to files."
  :type 'boolean
  :group 'efrit-progress)

(defcustom efrit-progress-buffer-enabled t
  "Whether to show progress in a buffer for interactive use."
  :type 'boolean
  :group 'efrit-progress)

(defcustom efrit-progress-buffer-name "*Efrit Progress*"
  "Name of the buffer for displaying progress."
  :type 'string
  :group 'efrit-progress)

(defcustom efrit-progress-verbosity 'normal
  "Verbosity level for progress display.
- minimal: Only show major operations
- normal: Show operations and key results
- verbose: Show all details including raw responses"
  :type '(choice (const :tag "Minimal" minimal)
                 (const :tag "Normal" normal)
                 (const :tag "Verbose" verbose))
  :group 'efrit-progress)

(defcustom efrit-progress-timestamp-format "%H:%M:%S"
  "Format for timestamps in progress display."
  :type 'string
  :group 'efrit-progress)

;;; Faces for Buffer Display

(defface efrit-progress-timestamp
  '((t :foreground "gray60"))
  "Face for timestamps in progress display."
  :group 'efrit-progress)

(defface efrit-progress-section-header
  '((t :weight bold :foreground "DeepSkyBlue"))
  "Face for section headers in progress display."
  :group 'efrit-progress)

(defface efrit-progress-tool-name
  '((t :weight bold :foreground "DarkOrange"))
  "Face for tool names in progress display."
  :group 'efrit-progress)

(defface efrit-progress-success
  '((t :foreground "green3"))
  "Face for success messages in progress display."
  :group 'efrit-progress)

(defface efrit-progress-error
  '((t :foreground "red3"))
  "Face for error messages in progress display."
  :group 'efrit-progress)

(defface efrit-progress-claude-message
  '((t :foreground "RoyalBlue"))
  "Face for Claude's messages in progress display."
  :group 'efrit-progress)

;;; State

(defvar efrit-progress--current-session nil
  "Current session ID being tracked.")

(defvar efrit-progress--current-file nil
  "Path to current progress.jsonl file.")

(defvar efrit-progress--event-counter 0
  "Counter for event sequencing within a session.")

(defvar efrit-progress--last-tool nil
  "Track the last tool called to detect loops.")

(defvar efrit-progress--tool-repeat-count 0
  "Count consecutive calls to the same tool.")

(defvar efrit-progress--current-tool nil
  "Currently executing tool name, or nil if no tool running.")

(defvar efrit-progress--current-tool-start nil
  "Start time of currently executing tool.")

(defvar efrit-progress--tool-call-count 0
  "Total number of tool calls in current session.")

;;; Progress File Management

(defun efrit-progress--session-dir (session-id)
  "Return the directory for SESSION-ID progress files."
  (expand-file-name session-id (efrit-config-data-file "" "sessions")))

(defun efrit-progress--progress-file (session-id)
  "Return the progress.jsonl file path for SESSION-ID."
  (expand-file-name "progress.jsonl" (efrit-progress--session-dir session-id)))

(defun efrit-progress--injection-file (session-id)
  "Return the injection file path for SESSION-ID.
External processes can write to this file to inject messages.
DEPRECATED: Use `efrit-progress--inject-dir' for queue-based injection."
  (expand-file-name "inject.json" (efrit-progress--session-dir session-id)))

(defun efrit-progress--inject-dir (session-id)
  "Return the inject queue directory for SESSION-ID.
External processes write JSON files here to inject messages into the session."
  (expand-file-name "inject" (efrit-progress--session-dir session-id)))

(defun efrit-progress--ensure-session-dir (session-id)
  "Ensure the session directory exists for SESSION-ID.
Also creates the inject queue subdirectory."
  (let ((dir (efrit-progress--session-dir session-id))
        (inject-dir (efrit-progress--inject-dir session-id)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    (unless (file-directory-p inject-dir)
      (make-directory inject-dir t))
    dir))

;;; JSONL Event Emission

(defun efrit-progress--emit-event (event-type data)
  "Emit a progress event of EVENT-TYPE with DATA to the progress file.
EVENT-TYPE is a symbol like \\='session-start, \\='tool-start, etc.
DATA is an alist of event-specific data."
  (when (and efrit-progress-enabled
             efrit-progress--current-session
             efrit-progress--current-file)
    (let* ((timestamp (format-time-string "%Y-%m-%dT%H:%M:%S%z"))
           (event `((type . ,(symbol-name event-type))
                   (timestamp . ,timestamp)
                   (seq . ,(cl-incf efrit-progress--event-counter))
                   (session . ,efrit-progress--current-session)
                   ,@data))
           (json-string (json-encode event)))
      (condition-case err
          (with-temp-buffer
            (insert json-string "\n")
            (append-to-file (point-min) (point-max) efrit-progress--current-file))
        (error
         (efrit-log 'warn "Failed to emit progress event: %s"
                   (error-message-string err)))))))

;;; Buffer Display

(defun efrit-progress--get-buffer ()
  "Get or create the progress buffer."
  (when efrit-progress-buffer-enabled
    (let ((buffer (get-buffer-create efrit-progress-buffer-name)))
      (with-current-buffer buffer
        (unless (eq major-mode 'efrit-progress-mode)
          (efrit-progress-mode)))
      buffer)))

(defun efrit-progress--timestamp ()
  "Return formatted timestamp."
  (propertize (format-time-string efrit-progress-timestamp-format)
              'face 'efrit-progress-timestamp))

(defun efrit-progress--buffer-append (text &optional face)
  "Append TEXT to progress buffer with optional FACE."
  (when-let* ((buffer (efrit-progress--get-buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (was-at-end (= (point) (point-max))))
        (goto-char (point-max))
        (insert (efrit-progress--timestamp) " ")
        (insert (if face (propertize text 'face face) text))
        (insert "\n")
        (when was-at-end
          (goto-char (point-max))
          (when-let* ((window (get-buffer-window buffer)))
            (set-window-point window (point))))))))

(defun efrit-progress--truncate (str max-len)
  "Truncate STR to MAX-LEN characters with ellipsis.
Uses `efrit-common-truncate-string' with ellipsis counted in max length."
  (when str
    (efrit-common-truncate-string str max-len t)))

;;; Public API - Called from efrit-executor.el

(defun efrit-progress-start-session (session-id command)
  "Start progress tracking for SESSION-ID with COMMAND."
  ;; Reset state
  (setq efrit-progress--current-session session-id)
  (setq efrit-progress--event-counter 0)
  (setq efrit-progress--last-tool nil)
  (setq efrit-progress--tool-repeat-count 0)
  (setq efrit-progress--current-tool nil)
  (setq efrit-progress--current-tool-start nil)
  (setq efrit-progress--tool-call-count 0)

  ;; Setup progress file
  (efrit-progress--ensure-session-dir session-id)
  (setq efrit-progress--current-file (efrit-progress--progress-file session-id))

  ;; Clear old progress file if exists
  (when (file-exists-p efrit-progress--current-file)
    (delete-file efrit-progress--current-file))

  ;; Emit session-start event
  (efrit-progress--emit-event 'session-start
                              `((command . ,command)))

  ;; Buffer display
  (when-let* ((buffer (efrit-progress--get-buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (efrit-progress--buffer-append
     (format "═══ Starting Session: %s ═══" session-id)
     'efrit-progress-section-header)
    (efrit-progress--buffer-append (format "Command: %s" command))
    (efrit-progress--buffer-append (format "Progress file: %s\n" efrit-progress--current-file))
    ;; Auto-show the progress buffer
    (display-buffer buffer '(display-buffer-at-bottom
                             (window-height . 10))))

  (efrit-log 'info "Progress tracking started for session %s" session-id))

(defun efrit-progress-end-session (session-id success-p)
  "End progress tracking for SESSION-ID with SUCCESS-P status."
  ;; Emit session-end event
  (efrit-progress--emit-event 'session-end
                              `((success . ,(if success-p t :json-false))))

  ;; Buffer display
  (efrit-progress--buffer-append
   (format "═══ Session %s: %s ═══"
           session-id
           (if success-p "Completed" "Failed"))
   (if success-p 'efrit-progress-success 'efrit-progress-error))

  ;; Reset state
  (setq efrit-progress--current-session nil)
  (setq efrit-progress--current-file nil)

  (efrit-log 'info "Progress tracking ended for session %s (success=%s)"
             session-id success-p))

(defun efrit-progress-show-message (message &optional type)
  "Show MESSAGE in progress output with optional TYPE.
TYPE can be \\='claude, \\='error, \\='success, or nil."
  ;; Emit text event
  (efrit-progress--emit-event 'text
                              `((message . ,message)
                               (message_type . ,(if type (symbol-name type) "info"))))

  ;; Buffer display
  (let ((face (pcase type
                ('claude 'efrit-progress-claude-message)
                ('error 'efrit-progress-error)
                ('success 'efrit-progress-success)
                (_ nil))))
    (efrit-progress--buffer-append message face)))

(defun efrit-progress-show-tool-start (tool-name input)
  "Show the start of TOOL-NAME execution with INPUT."
  ;; Track current tool and timing
  (setq efrit-progress--current-tool tool-name)
  (setq efrit-progress--current-tool-start (current-time))
  (cl-incf efrit-progress--tool-call-count)

  ;; Loop detection
  (if (equal tool-name efrit-progress--last-tool)
      (cl-incf efrit-progress--tool-repeat-count)
    (setq efrit-progress--tool-repeat-count 1))
  (setq efrit-progress--last-tool tool-name)

  ;; Prepare input summary for JSONL
  (let ((input-summary (cond
                        ((hash-table-p input)
                         (let ((summary (make-hash-table :test 'equal)))
                           (maphash (lambda (k v)
                                     (puthash k (efrit-progress--truncate
                                                (format "%s" v) 200)
                                             summary))
                                   input)
                           summary))
                        ((stringp input)
                         (efrit-progress--truncate input 200))
                        (t (efrit-progress--truncate (format "%S" input) 200)))))

    ;; Emit tool-start event
    (efrit-progress--emit-event 'tool-start
                                `((tool . ,tool-name)
                                 (input . ,input-summary)
                                 (repeat_count . ,efrit-progress--tool-repeat-count))))

  ;; Buffer display with loop warnings
  (when (memq efrit-progress-verbosity '(normal verbose))
    (cond
     ((= efrit-progress--tool-repeat-count 3)
      (efrit-progress--buffer-append
       (format "⚠️ WARNING: %s called %d times in a row - possible loop!"
               tool-name efrit-progress--tool-repeat-count)
       'efrit-progress-error))
     ((= efrit-progress--tool-repeat-count 5)
      (efrit-progress--buffer-append
       (format "🚨 CRITICAL: %s called %d times!"
               tool-name efrit-progress--tool-repeat-count)
       'efrit-progress-error))
     ((>= efrit-progress--tool-repeat-count 7)
      (efrit-progress--buffer-append
       (format "🛑 EMERGENCY: %s called %d times - forcing tool change!"
               tool-name efrit-progress--tool-repeat-count)
       'efrit-progress-error)))

    (efrit-progress--buffer-append
     (format "▶ %s" tool-name)
     'efrit-progress-tool-name)

    ;; Show meaningful input info
    (when (eq efrit-progress-verbosity 'verbose)
      (efrit-progress--buffer-append
       (format "  Input: %s" (efrit-progress--truncate (format "%S" input) 200))))))

(defun efrit-progress-show-tool-result (tool-name result success-p)
  "Show the RESULT of TOOL-NAME execution.
SUCCESS-P indicates if the execution was successful."
  ;; Calculate elapsed time for this tool
  (let ((tool-elapsed (if efrit-progress--current-tool-start
                          (float-time (time-subtract
                                       (current-time)
                                       efrit-progress--current-tool-start))
                        0)))
    ;; Clear current tool tracking
    (setq efrit-progress--current-tool nil)
    (setq efrit-progress--current-tool-start nil)

    ;; Emit tool-result event (with elapsed time)
    (efrit-progress--emit-event 'tool-result
                                `((tool . ,tool-name)
                                  (success . ,(if success-p t :json-false))
                                  (elapsed_ms . ,(round (* tool-elapsed 1000)))
                                  (result . ,(efrit-progress--truncate
                                              (format "%s" result) 500))))

    ;; Buffer display
    (when (memq efrit-progress-verbosity '(normal verbose))
      (efrit-progress--buffer-append
       (format "◀ %s: %s (%.2fs)" tool-name (if success-p "✓" "✗") tool-elapsed)
       (if success-p 'efrit-progress-success 'efrit-progress-error))
      (when (or (not success-p) (eq efrit-progress-verbosity 'verbose))
        (efrit-progress--buffer-append
         (format "  Result: %s" (efrit-progress--truncate
                                (format "%S" result) 300)))))))

;;; Injection Support (for external process communication)
;;
;; The injection system allows external processes (like Claude Code) to send
;; messages to an active Efrit session. Messages are delivered via a queue
;; directory in the session folder.
;;
;; Protocol:
;; - External process writes JSON file to: <session-dir>/inject/<timestamp>_<type>.json
;; - File must be atomically written (write temp, rename)
;; - File format:
;;   {
;;     "type": "guidance" | "abort" | "priority" | "context",
;;     "message": "string message for Claude",
;;     "timestamp": "ISO8601",
;;     "priority": 0-3 (optional, for priority type)
;;   }
;;
;; Message types:
;; - guidance: Add guidance/hint to Claude's next prompt
;; - abort: Request graceful session termination
;; - priority: Change task priority
;; - context: Add additional context to the session

(defconst efrit-progress-injection-types
  '(guidance abort priority context)
  "Valid injection message types.")

(defun efrit-progress-check-injection ()
  "Check for and return any injected message, clearing the injection file.
Returns nil if no injection is pending.
DEPRECATED: Use `efrit-progress-check-inject-queue' instead."
  (when (and efrit-progress--current-session
             efrit-progress-enabled)
    (let ((inject-file (efrit-progress--injection-file
                       efrit-progress--current-session)))
      (when (file-exists-p inject-file)
        (condition-case err
            (let* ((content (with-temp-buffer
                             (insert-file-contents inject-file)
                             (buffer-string)))
                   (json-object-type 'alist)
                   (json-array-type 'vector)
                   (json-key-type 'string)
                   (data (json-read-from-string content)))
              ;; Clear the injection file
              (delete-file inject-file)
              ;; Emit injection-received event
              (efrit-progress--emit-event 'injection-received
                                          `((content . ,data)))
              data)
          (error
           (efrit-log 'warn "Failed to read injection file: %s"
                     (error-message-string err))
           nil))))))

(defun efrit-progress--list-inject-files (session-id)
  "List pending injection files for SESSION-ID, sorted by timestamp."
  (let ((inject-dir (efrit-progress--inject-dir session-id)))
    (when (file-directory-p inject-dir)
      (sort (directory-files inject-dir t "\\.json$") #'string<))))

(defun efrit-progress--parse-inject-file (file-path)
  "Parse injection FILE-PATH and return alist, or nil on error."
  (condition-case err
      (let* ((content (with-temp-buffer
                        (insert-file-contents file-path)
                        (buffer-string)))
             (json-object-type 'alist)
             (json-array-type 'vector)
             (json-key-type 'string)
             (data (json-read-from-string content)))
        ;; Validate required fields
        (if (and (assoc "type" data)
                 (assoc "message" data))
            data
          (efrit-log 'warn "Invalid injection file (missing type/message): %s" file-path)
          nil))
    (error
     (efrit-log 'warn "Failed to parse injection file %s: %s"
                file-path (error-message-string err))
     nil)))

(defun efrit-progress-check-inject-queue ()
  "Check inject queue directory and return next pending injection.
Returns nil if no injection is pending.
Returns alist with keys: type, message, timestamp, (optional) priority.
Processed files are deleted after reading."
  (when (and efrit-progress--current-session
             efrit-progress-enabled)
    (let ((files (efrit-progress--list-inject-files efrit-progress--current-session)))
      (when files
        (let* ((next-file (car files))
               (data (efrit-progress--parse-inject-file next-file)))
          (when data
            ;; Delete processed file
            (condition-case err
                (delete-file next-file)
              (error
               (efrit-log 'warn "Failed to delete injection file: %s"
                          (error-message-string err))))
            ;; Emit injection-received event
            (efrit-progress--emit-event 'injection-received
                                        `((content . ,data)
                                          (file . ,(file-name-nondirectory next-file))))
            ;; Show in progress buffer
            (let ((msg-type (cdr (assoc "type" data)))
                  (message (cdr (assoc "message" data))))
              (efrit-progress--buffer-append
               (format "📥 Injection [%s]: %s"
                       msg-type
                       (efrit-progress--truncate message 100))
               'efrit-progress-section-header))
            data))))))

(defun efrit-progress-inject-queue-count ()
  "Return count of pending injections in queue."
  (if efrit-progress--current-session
      (length (efrit-progress--list-inject-files efrit-progress--current-session))
    0))

(defun efrit-progress-has-pending-injection-p ()
  "Return non-nil if there are pending injections."
  (> (efrit-progress-inject-queue-count) 0))

;;;###autoload
(defun efrit-progress-inject (session-id type message &optional priority)
  "Inject MESSAGE of TYPE into SESSION-ID.
TYPE must be one of: guidance, abort, priority, context.
PRIORITY is optional (0-3, only used for priority type)."
  (interactive
   (list (or efrit-progress--current-session
             (read-string "Session ID: "))
         (intern (completing-read "Type: " '("guidance" "abort" "priority" "context")))
         (read-string "Message: ")
         nil))
  (unless (memq type efrit-progress-injection-types)
    (error "Invalid injection type: %s (must be one of %s)"
           type efrit-progress-injection-types))
  (let* ((inject-dir (efrit-progress--inject-dir session-id))
         (timestamp (format-time-string "%Y%m%d%H%M%S%3N"))
         (filename (format "%s_%s.json" timestamp type))
         (filepath (expand-file-name filename inject-dir))
         (temp-file (make-temp-file "efrit-inject-" nil ".json"))
         (data `(("type" . ,(symbol-name type))
                 ("message" . ,message)
                 ("timestamp" . ,(format-time-string "%Y-%m-%dT%H:%M:%S%z"))
                 ,@(when priority `(("priority" . ,priority))))))
    ;; Ensure directory exists
    (unless (file-directory-p inject-dir)
      (make-directory inject-dir t))
    ;; Write atomically: temp file then rename
    (with-temp-file temp-file
      (insert (json-encode data)))
    (rename-file temp-file filepath)
    (efrit-log 'info "Injected %s message into session %s" type session-id)
    filepath))

;;; Status Query

(defun efrit-progress-current-tool ()
  "Return the currently executing tool name, or nil."
  efrit-progress--current-tool)

(defun efrit-progress-current-tool-elapsed ()
  "Return elapsed seconds for current tool, or nil if no tool running."
  (when efrit-progress--current-tool-start
    (float-time (time-subtract (current-time) efrit-progress--current-tool-start))))

(defun efrit-progress-tool-call-count ()
  "Return total number of tool calls in current session."
  efrit-progress--tool-call-count)

(defun efrit-progress-get-status ()
  "Return current progress status as an alist.
Useful for external queries."
  `((session . ,efrit-progress--current-session)
    (progress-file . ,efrit-progress--current-file)
    (current-tool . ,efrit-progress--current-tool)
    (tool-call-count . ,efrit-progress--tool-call-count)
    (inject-dir . ,(when efrit-progress--current-session
                     (efrit-progress--inject-dir efrit-progress--current-session)))
    (pending-injections . ,(efrit-progress-inject-queue-count))
    (event-count . ,efrit-progress--event-counter)
    (last-tool . ,efrit-progress--last-tool)
    (tool-repeat-count . ,efrit-progress--tool-repeat-count)))

;;; Progress Mode

(defvar efrit-progress-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'quit-window)
    (define-key map "c" 'efrit-progress-clear)
    (define-key map "v" 'efrit-progress-cycle-verbosity)
    (define-key map "g" 'efrit-progress-refresh)
    map)
  "Keymap for efrit-progress-mode.")

(define-derived-mode efrit-progress-mode special-mode "Efrit-Progress"
  "Major mode for viewing Efrit operation progress.
\\{efrit-progress-mode-map}"
  (setq-local truncate-lines nil)
  (setq-local word-wrap t))

(defun efrit-progress-clear ()
  "Clear the progress buffer."
  (interactive)
  (when-let* ((buffer (get-buffer efrit-progress-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)))))

(defun efrit-progress-cycle-verbosity ()
  "Cycle through verbosity levels."
  (interactive)
  (setq efrit-progress-verbosity
        (pcase efrit-progress-verbosity
          ('minimal 'normal)
          ('normal 'verbose)
          ('verbose 'minimal)))
  (message "Progress verbosity: %s" efrit-progress-verbosity))

(defun efrit-progress-refresh ()
  "Refresh progress display from progress file."
  (interactive)
  (if (not efrit-progress--current-file)
      (message "No active session")
    (message "Progress file: %s" efrit-progress--current-file)))

;;;###autoload
(defun efrit-progress-show ()
  "Show the progress buffer."
  (interactive)
  (let ((buffer (efrit-progress--get-buffer)))
    (if buffer
        (display-buffer buffer)
      (message "Progress buffer display is disabled"))))

;;;###autoload
(defun efrit-progress-tail ()
  "Show current session's progress file for tailing.
Returns the path to the progress file."
  (interactive)
  (if efrit-progress--current-file
      (progn
        (message "tail -f %s" efrit-progress--current-file)
        efrit-progress--current-file)
    (message "No active session")
    nil))

;;;###autoload
(defun efrit-progress-inject-dir ()
  "Show current session's inject directory for injections.
Returns the path to the inject queue directory."
  (interactive)
  (if efrit-progress--current-session
      (let ((dir (efrit-progress--inject-dir efrit-progress--current-session)))
        (message "Inject to: %s/<timestamp>_<type>.json" dir)
        dir)
    (message "No active session")
    nil))

;;; Session Inspector

(defun efrit-progress--list-sessions ()
  "Return list of available session IDs with progress files."
  (let ((sessions-dir (efrit-config-data-file "" "sessions"))
        sessions)
    (when (file-directory-p sessions-dir)
      (dolist (dir (directory-files sessions-dir t "^[^.]"))
        (when (and (file-directory-p dir)
                   (file-exists-p (expand-file-name "progress.jsonl" dir)))
          (push (file-name-nondirectory dir) sessions))))
    (nreverse sessions)))

(defun efrit-progress--parse-jsonl-line (line)
  "Parse a JSONL LINE and return alist."
  (condition-case nil
      (let ((json-object-type 'alist)
            (json-array-type 'vector)
            (json-key-type 'string))
        (json-read-from-string line))
    (error nil)))

(defun efrit-progress--format-event (event)
  "Format EVENT alist for display in inspector buffer."
  (let* ((event-type (cdr (assoc "type" event)))
         (timestamp (cdr (assoc "timestamp" event)))
         (time-str (if timestamp
                       (replace-regexp-in-string ".*T\\([0-9:]+\\).*" "\\1" timestamp)
                     "??:??:??")))
    (pcase event-type
      ("session-start"
       (let ((command (cdr (assoc "command" event))))
         (concat (propertize (format "[%s] " time-str) 'face 'efrit-progress-timestamp)
                 (propertize "═══ SESSION START ═══\n" 'face 'efrit-progress-section-header)
                 (propertize (format "[%s] " time-str) 'face 'efrit-progress-timestamp)
                 (format "Command: %s\n" command))))
      ("session-end"
       (let ((success (cdr (assoc "success" event))))
         (concat (propertize (format "[%s] " time-str) 'face 'efrit-progress-timestamp)
                 (propertize (format "═══ SESSION %s ═══\n"
                                    (if success "COMPLETE" "FAILED"))
                            'face (if success 'efrit-progress-success 'efrit-progress-error)))))
      ("tool-start"
       (let ((tool (cdr (assoc "tool" event)))
             (repeat (cdr (assoc "repeat_count" event))))
         (concat (propertize (format "[%s] " time-str) 'face 'efrit-progress-timestamp)
                 (propertize (format "▶ %s" tool) 'face 'efrit-progress-tool-name)
                 (if (and repeat (> repeat 1))
                     (format " (repeat #%d)" repeat)
                   "")
                 "\n")))
      ("tool-result"
       (let ((tool (cdr (assoc "tool" event)))
             (success (cdr (assoc "success" event))))
         (concat (propertize (format "[%s] " time-str) 'face 'efrit-progress-timestamp)
                 (propertize (format "◀ %s %s\n" tool (if success "✓" "✗"))
                            'face (if success 'efrit-progress-success 'efrit-progress-error)))))
      ("text"
       (let ((message (cdr (assoc "message" event)))
             (msg-type (cdr (assoc "message_type" event))))
         (concat (propertize (format "[%s] " time-str) 'face 'efrit-progress-timestamp)
                 (propertize (efrit-progress--truncate message 200)
                            'face (pcase msg-type
                                    ("claude" 'efrit-progress-claude-message)
                                    ("error" 'efrit-progress-error)
                                    (_ nil)))
                 "\n")))
      ("injection-received"
       (let* ((content (cdr (assoc "content" event)))
              (inject-type (if content (cdr (assoc "type" content)) "?"))
              (inject-msg (if content (cdr (assoc "message" content)) "?")))
         (concat (propertize (format "[%s] " time-str) 'face 'efrit-progress-timestamp)
                 (propertize (format "📥 INJECTION [%s]: %s\n"
                                    inject-type
                                    (efrit-progress--truncate inject-msg 100))
                            'face 'efrit-progress-section-header))))
      (_
       (concat (propertize (format "[%s] " time-str) 'face 'efrit-progress-timestamp)
               (format "%s\n" event-type))))))

(defvar efrit-watch--session-id nil
  "Session ID being watched in current buffer.")

(defvar efrit-watch--last-line-count 0
  "Number of lines already processed from progress file.
Using line count instead of byte position avoids issues with
multibyte UTF-8 characters being split at byte boundaries.")

(defvar efrit-watch--timer nil
  "Timer for auto-refresh.")

(defun efrit-watch--refresh ()
  "Refresh the watch buffer with new events.
Reads the progress file and appends any new lines since last refresh.
Uses line-based tracking to avoid multibyte UTF-8 encoding issues
that can occur when reading from arbitrary byte positions."
  (when-let* ((session-id efrit-watch--session-id)
              (progress-file (efrit-progress--progress-file session-id)))
    (when (file-exists-p progress-file)
      (let ((new-content "")
            (lines-processed 0))
        ;; Read entire file to avoid splitting multibyte characters
        (with-temp-buffer
          (insert-file-contents progress-file)
          (goto-char (point-min))
          ;; Skip lines we've already processed
          (dotimes (_ efrit-watch--last-line-count)
            (forward-line 1))
          ;; Process new lines
          (while (not (eobp))
            (let ((line (buffer-substring-no-properties
                         (line-beginning-position) (line-end-position))))
              (unless (string-empty-p line)
                (when-let* ((event (efrit-progress--parse-jsonl-line line)))
                  (setq new-content
                        (concat new-content (efrit-progress--format-event event)))))
              (cl-incf lines-processed))
            (forward-line 1)))
        ;; Update line count
        (cl-incf efrit-watch--last-line-count lines-processed)
        ;; Insert new content
        (when (not (string-empty-p new-content))
          (let ((inhibit-read-only t)
                (at-end (= (point) (point-max))))
            (save-excursion
              (goto-char (point-max))
              (insert new-content))
            (when at-end
              (goto-char (point-max)))))))))

(defun efrit-watch--start-timer ()
  "Start the auto-refresh timer."
  (unless efrit-watch--timer
    (setq efrit-watch--timer
          (run-with-timer 0.5 0.5 #'efrit-watch--refresh-if-visible))))

(defun efrit-watch--stop-timer ()
  "Stop the auto-refresh timer."
  (when efrit-watch--timer
    (cancel-timer efrit-watch--timer)
    (setq efrit-watch--timer nil)))

(defun efrit-watch--refresh-if-visible ()
  "Refresh watch buffer if it's visible."
  (when-let* ((buffer (get-buffer "*Efrit Watch*")))
    (when (get-buffer-window buffer t)
      (with-current-buffer buffer
        (efrit-watch--refresh)))))

(defvar efrit-watch-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'quit-window)
    (define-key map "g" 'efrit-watch--refresh)
    (define-key map "i" 'efrit-watch-inject)
    (define-key map "s" 'efrit-watch-session)
    map)
  "Keymap for efrit-watch-mode.")

(define-derived-mode efrit-watch-mode special-mode "Efrit-Watch"
  "Major mode for watching Efrit session progress.

\\{efrit-watch-mode-map}"
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  (make-local-variable 'efrit-watch--session-id)
  (make-local-variable 'efrit-watch--last-line-count)
  (add-hook 'kill-buffer-hook #'efrit-watch--stop-timer nil t))

(defun efrit-watch-inject ()
  "Inject a message into the watched session."
  (interactive)
  (if efrit-watch--session-id
      (let* ((type (intern (completing-read
                           "Type: " '("guidance" "abort" "priority" "context"))))
             (message (read-string "Message: ")))
        (efrit-progress-inject efrit-watch--session-id type message)
        (message "Injected %s message to %s" type efrit-watch--session-id))
    (message "No session being watched")))

;;;###autoload
(defun efrit-watch-session (&optional session-id)
  "Watch a session's progress in a live inspector buffer.
If SESSION-ID is nil, prompt for a session from available sessions.
If called interactively with prefix arg, always prompt."
  (interactive
   (list (when (or current-prefix-arg
                   (not efrit-progress--current-session))
           (let ((sessions (efrit-progress--list-sessions)))
             (if sessions
                 (completing-read "Session: " sessions nil t)
               (user-error "No sessions with progress files found"))))))
  ;; Default to current session
  (setq session-id (or session-id efrit-progress--current-session))
  (unless session-id
    (let ((sessions (efrit-progress--list-sessions)))
      (if sessions
          (setq session-id (completing-read "Session: " sessions nil t))
        (user-error "No sessions with progress files found"))))
  ;; Create or get watch buffer
  (let* ((buffer (get-buffer-create "*Efrit Watch*"))
         (progress-file (efrit-progress--progress-file session-id)))
    (unless (file-exists-p progress-file)
      (user-error "No progress file for session %s" session-id))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer))
      (efrit-watch-mode)
      (setq efrit-watch--session-id session-id)
      (setq efrit-watch--last-line-count 0)
      ;; Insert header
      (let ((inhibit-read-only t))
        (insert (propertize (format "Watching session: %s\n" session-id)
                           'face 'efrit-progress-section-header))
        (insert (format "Progress file: %s\n" progress-file))
        (insert "Keys: g=refresh, i=inject, s=switch session, q=quit\n")
        (insert "───────────────────────────────────────\n"))
      ;; Load existing content
      (efrit-watch--refresh)
      ;; Start auto-refresh
      (efrit-watch--start-timer))
    (display-buffer buffer)))

(provide 'efrit-progress)

;;; efrit-progress.el ends here
