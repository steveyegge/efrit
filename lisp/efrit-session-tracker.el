;;; efrit-session-tracker.el --- Session state management for Efrit -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: AI Assistant
;; Keywords: efrit, session, tracking, metrics
;; Package-Requires: ((emacs "25.1") (cl-lib "0.5"))

;;; Commentary:

;; Session state management and metrics tracking for Efrit operations.
;; Provides persistent storage of session data, buffer tracking, and
;; performance metrics.
;;
;; Usage:
;;   M-x efrit-session-start    ; Start a new session
;;   M-x efrit-session-end      ; End current session
;;
;; Customization:
;;   M-x customize-group RET efrit-session RET
;;
;; The session tracker automatically captures:
;; - Command executions and their timing
;; - TODO item creation and completion
;; - API calls to external services  
;; - Buffer creation and file modifications
;; - Tool usage statistics
;;
;; All data is persistently stored in the efrit data directory
;; for later analysis and dashboard display.

;;; Code:

(require 'efrit-config)
(require 'json)
(require 'cl-lib)
(require 'subr-x)

;;; Customization

(defgroup efrit-session nil
  "Session tracking for Efrit AI assistant."
  :group 'efrit
  :prefix "efrit-session-")

(defcustom efrit-session-tracking-enabled t
  "Whether session tracking is enabled."
  :type 'boolean
  :group 'efrit-session)

;;; Session State Variables

(defvar efrit-session-id nil
  "Current session identifier.")

(defvar efrit-session-start-time nil
  "When the current session started.")

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

;;; Session Management

;;;###autoload
(defun efrit-session-start ()
  "Start a new Efrit session."
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
  "End the current Efrit session."
  (interactive)
  (when efrit-session-id
    (efrit-session-save-final)
    (efrit-session-log "Session ended" 'info)
    (message "Efrit session ended: %s" efrit-session-id)
    (setq efrit-session-id nil)
    (setq efrit-session-start-time nil)))

(defun efrit-session-generate-id ()
  "Generate a unique session ID."
  (format "efrit-%s-%04x"
          (format-time-string "%Y%m%d-%H%M%S")
          (random 65536)))

;;; Metrics Tracking

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

;;; Helper Functions

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

;;; Logging

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
    ;; Use append-to-file for O(1) performance instead of rewriting entire file
    (append-to-file log-entry nil log-file)))

;;; Auto-start Session

(defun efrit-session-ensure-active ()
  "Ensure there's an active session, creating one if needed."
  (unless efrit-session-id
    (efrit-session-start)))

;;; Periodic Save Timer

(defvar efrit-session-save-timer nil
  "Timer for periodic session saves.")

(defcustom efrit-session-save-interval 60
  "Interval in seconds between automatic session saves."
  :type 'number
  :group 'efrit-session)

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

;;; Hooks for Integration

(defun efrit-session-setup-hooks ()
  "Setup hooks for session tracking integration."
  ;; Add hooks to track various events
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

;; Auto-start session if tracking is enabled
(when efrit-session-tracking-enabled
  (efrit-session-ensure-active))

(provide 'efrit-session-tracker)

;;; efrit-session-tracker.el ends here
