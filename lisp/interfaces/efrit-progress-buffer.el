;;; efrit-progress-buffer.el --- Real-time progress display for async execution -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; Progress buffer infrastructure for Efrit async execution.
;; Provides a read-only, auto-scrolling buffer that displays real-time updates
;; of Claude's work in progress.
;;
;; This is the "agent buffer" where users can see:
;; - Claude's thinking and reasoning
;; - Tool calls and their results
;; - TODO items being created/updated
;; - Execution status and progress
;; - Errors and warnings
;; - Final summary with timing and token usage
;;
;; Following the Zero Client-Side Intelligence principle, this module only
;; RECORDS what happened - it does not make decisions.  It responds to events
;; fired by the execution engine and displays them.

;;; Code:

(require 'cl-lib)
(require 'efrit-log)
(require 'efrit-common)

;;; Customization

(defgroup efrit-progress nil
  "Real-time progress display for Efrit async execution."
  :group 'efrit
  :prefix "efrit-progress-")

(defcustom efrit-progress-auto-show t
  "Whether to automatically show progress buffer when execution starts."
  :type 'boolean
  :group 'efrit-progress)

(defcustom efrit-progress-scroll-to-bottom t
  "Whether to auto-scroll progress buffer to show latest events."
  :type 'boolean
  :group 'efrit-progress)

(defcustom efrit-progress-timestamp-format "%H:%M:%S"
  "Format string for timestamps in progress buffer.
See `format-time-string' for format specifiers."
  :type 'string
  :group 'efrit-progress)

(defcustom efrit-progress-max-buffer-size 10000000
  "Maximum size in bytes for a progress buffer.
Older content is archived when this limit is exceeded."
  :type 'integer
  :group 'efrit-progress)

;;; Progress Buffer State

(defvar efrit-progress-buffers (make-hash-table :test 'equal)
  "Hash table of progress buffers by session ID.")

(defvar efrit-progress-buffer-count 0
  "Counter for archiving timestamp uniqueness.")

;;; Forward declarations for formatter functions
;;; (These are defined later but used in efrit-progress-insert-event)

(declare-function efrit-progress--format-message "efrit-progress-buffer"
                  (data))
(declare-function efrit-progress--format-tool-started "efrit-progress-buffer"
                  (data))
(declare-function efrit-progress--format-tool-result "efrit-progress-buffer"
                  (data))
(declare-function efrit-progress--format-todo-updated "efrit-progress-buffer"
                  (data))
(declare-function efrit-progress--format-status-changed "efrit-progress-buffer"
                  (data))
(declare-function efrit-progress--format-error "efrit-progress-buffer"
                  (data))
(declare-function efrit-progress--format-complete "efrit-progress-buffer"
                  (data))

;;; Core Progress Buffer Functions

(defun efrit-progress-create-buffer (session-id)
  "Create or retrieve progress buffer for SESSION-ID.
Returns the progress buffer, creating it if it doesn't exist.
Buffer is read-only and set up for real-time event display."
  (let* ((buffer-name (format "*efrit-progress-%s*" session-id))
         (buffer (or (get-buffer buffer-name)
                    (get-buffer-create buffer-name))))
    (with-current-buffer buffer
      ;; Set up buffer-local variables
      (setq-local efrit-progress-session-id session-id)
      (setq-local inhibit-read-only t)
      (setq-local truncate-lines t)
      
      ;; Initialize buffer if empty
      (when (zerop (buffer-size))
        (insert "Efrit Progress Buffer\n")
        (insert "====================\n\n")
        (insert (format "Session: %s\n" session-id))
        (insert (format "Started: %s\n\n" (format-time-string "%Y-%m-%d %H:%M:%S")))
        (insert "--- Events ---\n\n"))
      
      ;; Make read-only by default (will be toggled during inserts)
      (setq-local buffer-read-only t)
      (setq-local mode-line-process " [Progress]")
      
      ;; Store reference
      (puthash session-id buffer efrit-progress-buffers))
    buffer))

(defun efrit-progress-get-buffer (session-id)
  "Get existing progress buffer for SESSION-ID, or nil if not created."
  (gethash session-id efrit-progress-buffers))

(defun efrit-progress-insert-event (session-id event-type event-data)
  "Insert a formatted event into SESSION-ID's progress buffer.
EVENT-TYPE is one of: message, tool_started, tool_result, todo_updated,
status_changed, error, complete.
EVENT-DATA is an alist with event-specific details.
Returns the number of characters inserted."
  (let ((buffer (efrit-progress-get-buffer session-id)))
    (if (not buffer)
        (progn
          (efrit-log 'warn "Progress buffer not found for session %s" session-id)
          0)
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (let ((inserted-chars
                 (save-excursion
                   ;; Move to end of buffer
                   (goto-char (point-max))
                   
                   ;; Format and insert the event
                   (pcase event-type
                     ('message
                      (efrit-progress--format-message event-data))
                     ('tool_started
                      (efrit-progress--format-tool-started event-data))
                     ('tool_result
                      (efrit-progress--format-tool-result event-data))
                     ('todo_updated
                      (efrit-progress--format-todo-updated event-data))
                     ('status_changed
                      (efrit-progress--format-status-changed event-data))
                     ('error
                      (efrit-progress--format-error event-data))
                     ('complete
                      (efrit-progress--format-complete event-data))
                     (_
                      (let ((msg (format "[%s] Unknown event type: %s\n"
                                        (format-time-string efrit-progress-timestamp-format)
                                        event-type)))
                        (insert msg)
                        (length msg)))))))
            
            ;; Auto-scroll if requested
            (when efrit-progress-scroll-to-bottom
              (let ((window (get-buffer-window buffer)))
                (when window
                  (with-selected-window window
                    (goto-char (point-max))
                    (recenter -3)))))
            
            ;; Check buffer size and archive if needed
            (when (> (buffer-size) efrit-progress-max-buffer-size)
              (efrit-progress-archive-buffer session-id))
            
            inserted-chars))))))

;;; Event Formatting Functions

(defun efrit-progress--format-message (data)
  "Format a message event and insert it into current buffer.
DATA is an alist with :text key.
Returns character count inserted."
  (let ((text (alist-get :text data))
        (role (alist-get :role data "assistant")))
    (when text
      (let ((formatted (format "[%s] %s: %s\n"
                              (format-time-string efrit-progress-timestamp-format)
                              role
                              (efrit-common-truncate-string text 100))))
        (insert formatted)
        (length formatted)))))

(defun efrit-progress--format-tool-started (data)
  "Format a tool start event.
DATA is an alist with :tool and :input keys.
Returns character count inserted."
  (let ((tool (alist-get :tool data))
        (input (alist-get :input data))
        (total 0))
    (when tool
      (let ((formatted (format "[%s] 🔧 Tool started: %s\n"
                              (format-time-string efrit-progress-timestamp-format)
                              tool)))
        (insert formatted)
        (setq total (length formatted))
        (when input
          (let ((input-line (format "      Input: %s\n" 
                                   (efrit-common-truncate-string (format "%S" input) 80))))
            (insert input-line)
            (cl-incf total (length input-line))))
        total)))

(defun efrit-progress--format-tool-result (data)
  "Format a tool result event.
DATA is an alist with :tool and :result keys.
Returns character count inserted."
  (let ((tool (alist-get :tool data))
        (result (alist-get :result data)))
    (when tool
      (let ((formatted (format "[%s] ✓ Tool result: %s\n"
                              (format-time-string efrit-progress-timestamp-format)
                              tool)))
        (insert formatted)
        (when result
          (let ((result-str (format "%S" result)))
            (insert (format "      Result: %s\n"
                           (efrit-common-truncate-string result-str 80)))))
        (length formatted)))))

(defun efrit-progress--format-todo-updated (data)
  "Format a TODO update event.
DATA is an alist with :title and :status keys.
Returns character count inserted."
  (let ((title (alist-get :title data))
        (status (alist-get :status data)))
    (when title
      (let* ((status-str (format "%s" (or status "open")))
             (formatted (format "[%s] 📋 TODO: %s [%s]\n"
                               (format-time-string efrit-progress-timestamp-format)
                               (efrit-common-truncate-string title 60)
                               status-str)))
        (insert formatted)
        (length formatted)))))

(defun efrit-progress--format-status-changed (data)
  "Format a status change event.
DATA is an alist with :old-status and :new-status keys.
Returns character count inserted."
  (let ((new-status (alist-get :new-status data)))
    (when new-status
      (let ((formatted (format "[%s] → Status: %s\n"
                              (format-time-string efrit-progress-timestamp-format)
                              new-status)))
        (insert formatted)
        (length formatted)))))

(defun efrit-progress--format-error (data)
  "Format an error event.
DATA is an alist with :message and optional :level keys.
Returns character count inserted."
  (let ((message (alist-get :message data))
        (level (alist-get :level data "ERROR")))
    (when message
      (let ((formatted (format "[%s] ⚠️  %s: %s\n"
                              (format-time-string efrit-progress-timestamp-format)
                              level
                              (efrit-common-truncate-string message 100))))
        (insert formatted)
        (length formatted)))))

(defun efrit-progress--format-complete (data)
  "Format a completion event with summary.
DATA is an alist with :result, :elapsed, :token-usage keys.
Returns character count inserted."
  (let ((result (alist-get :result data))
        (elapsed (alist-get :elapsed data 0))
        (tokens (alist-get :token-usage data 0)))
    (let ((formatted (format "\n[%s] ✅ Execution Complete\n"
                            (format-time-string efrit-progress-timestamp-format))))
      (insert formatted)
      (insert (format "   Elapsed: %.1f seconds\n" elapsed))
      (when (> tokens 0)
        (insert (format "   Tokens used: %d\n" tokens)))
      (when result
        (insert (format "   Result: %s\n" 
                       (efrit-common-truncate-string (format "%S" result) 80))))
      (length formatted))))

;;; Buffer Management

(defun efrit-progress-show-message (session-id message &optional _level)
  "Show MESSAGE in SESSION-ID's progress buffer with optional LEVEL.
LEVEL can be `info', `warn', `error' (defaults to `info').
Creates buffer if it doesn't exist."
  (let ((buffer (or (efrit-progress-get-buffer session-id)
                   (efrit-progress-create-buffer session-id))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-max))
          (insert (format "[%s] %s\n"
                         (format-time-string efrit-progress-timestamp-format)
                         message))))))))

(defun efrit-progress-archive-buffer (session-id)
  "Archive SESSION-ID's progress buffer with timestamp.
Renames buffer from `*efrit-progress-SESSION-ID*' to
`*efrit-progress-TIMESTAMP*' to preserve historical record."
  (let ((buffer (efrit-progress-get-buffer session-id)))
    (when buffer
      (let* ((timestamp (format-time-string "%Y%m%d-%H%M%S"))
             (archive-name (format "*efrit-progress-%s*" timestamp)))
        ;; Rename buffer
        (with-current-buffer buffer
          (rename-buffer archive-name t))
        ;; Remove from active registry
        (remhash session-id efrit-progress-buffers)
        (efrit-log 'debug "Archived progress buffer to %s" archive-name)))))

;;;###autoload
(defun efrit-progress-show-buffer (session-id)
  "Display the progress buffer for SESSION-ID.
Creates buffer if it doesn't exist. Displays in current window."
  (interactive "sSession ID: ")
  (let ((buffer (or (efrit-progress-get-buffer session-id)
                   (efrit-progress-create-buffer session-id))))
    (display-buffer buffer)))

;;;###autoload
(defun efrit-progress-clear-buffer (session-id)
  "Clear all events from SESSION-ID's progress buffer.
Keeps buffer structure but removes event history."
  (interactive "sSession ID: ")
  (let ((buffer (efrit-progress-get-buffer session-id)))
    (when buffer
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert "Efrit Progress Buffer (Cleared)\n")
          (insert "===============================\n\n")
          (insert (format "Session: %s\n" session-id))
          (insert (format "Cleared: %s\n\n" (format-time-string "%Y-%m-%d %H:%M:%S")))
          (insert "--- Events ---\n\n"))))))

;;;###autoload
(defun efrit-progress-list-buffers ()
  "List all active progress buffers."
  (interactive)
  (let ((buffer (get-buffer-create "*Efrit Progress Buffers*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "Efrit Progress Buffers\n")
      (insert "=====================\n\n")
      (if (zerop (hash-table-count efrit-progress-buffers))
          (insert "No active progress buffers\n")
        (maphash (lambda (session-id buf)
                  (insert (format "- %s: %s\n"
                                 session-id
                                 (buffer-name buf))))
                efrit-progress-buffers)))
    (display-buffer buffer)))

(provide 'efrit-progress-buffer)

;;; efrit-progress-buffer.el ends here
