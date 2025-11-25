;;; efrit-progress.el --- Progress emission for Efrit operations -*- lexical-binding: t -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.1.0
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

;;; Progress File Management

(defun efrit-progress--session-dir (session-id)
  "Return the directory for SESSION-ID progress files."
  (expand-file-name session-id (efrit-config-data-file "" "sessions")))

(defun efrit-progress--progress-file (session-id)
  "Return the progress.jsonl file path for SESSION-ID."
  (expand-file-name "progress.jsonl" (efrit-progress--session-dir session-id)))

(defun efrit-progress--injection-file (session-id)
  "Return the injection file path for SESSION-ID.
External processes can write to this file to inject messages."
  (expand-file-name "inject.json" (efrit-progress--session-dir session-id)))

(defun efrit-progress--ensure-session-dir (session-id)
  "Ensure the session directory exists for SESSION-ID."
  (let ((dir (efrit-progress--session-dir session-id)))
    (unless (file-directory-p dir)
      (make-directory dir t))
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
  "Truncate STR to MAX-LEN characters with ellipsis."
  (if (and str (> (length str) max-len))
      (concat (substring str 0 (- max-len 3)) "...")
    str))

;;; Public API - Called from efrit-executor.el

(defun efrit-progress-start-session (session-id command)
  "Start progress tracking for SESSION-ID with COMMAND."
  ;; Reset state
  (setq efrit-progress--current-session session-id)
  (setq efrit-progress--event-counter 0)
  (setq efrit-progress--last-tool nil)
  (setq efrit-progress--tool-repeat-count 0)

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
    (efrit-progress--buffer-append (format "Progress file: %s\n" efrit-progress--current-file)))

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
  ;; Emit tool-result event
  (efrit-progress--emit-event 'tool-result
                              `((tool . ,tool-name)
                               (success . ,(if success-p t :json-false))
                               (result . ,(efrit-progress--truncate
                                          (format "%s" result) 500))))

  ;; Buffer display
  (when (memq efrit-progress-verbosity '(normal verbose))
    (efrit-progress--buffer-append
     (format "◀ %s: %s" tool-name (if success-p "✓" "✗"))
     (if success-p 'efrit-progress-success 'efrit-progress-error))
    (when (or (not success-p) (eq efrit-progress-verbosity 'verbose))
      (efrit-progress--buffer-append
       (format "  Result: %s" (efrit-progress--truncate
                              (format "%S" result) 300))))))

;;; Injection Support (for external process communication)

(defun efrit-progress-check-injection ()
  "Check for and return any injected message, clearing the injection file.
Returns nil if no injection is pending."
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

;;; Status Query

(defun efrit-progress-get-status ()
  "Return current progress status as an alist.
Useful for external queries."
  `((session . ,efrit-progress--current-session)
    (progress-file . ,efrit-progress--current-file)
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

(provide 'efrit-progress)

;;; efrit-progress.el ends here
