;;; efrit-log.el --- Unified logging system for efrit -*- lexical-binding: t -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Keywords: ai, tools, logging

;;; Commentary:
;; Unified logging system for all efrit modules with level support,
;; buffer management, and optional message echoing.

;;; Code:

(defcustom efrit-log-level 'info
  "Minimum log level to record.
Levels in order: debug, info, warn, error, none"
  :type '(choice (const debug) (const info) (const warn) (const error) (const none))
  :group 'efrit)

(defcustom efrit-log-buffer "*efrit-log*"
  "Buffer name for efrit logs."
  :type 'string
  :group 'efrit)

(defcustom efrit-log-max-lines 1000
  "Maximum lines to keep in log buffer."
  :type 'integer
  :group 'efrit)

(defcustom efrit-log-echo-level 'warn
  "Minimum level to echo to message area."
  :type '(choice (const debug) (const info) (const warn) (const error) (const none))
  :group 'efrit)

;;; Core logging

(defconst efrit-log--level-values
  '((debug . 0) (info . 1) (warn . 2) (error . 3) (none . 4))
  "Numeric values for log levels.")

(defun efrit-log--level-enabled-p (level)
  "Return t if LEVEL should be logged."
  (>= (alist-get level efrit-log--level-values 4)
      (alist-get efrit-log-level efrit-log--level-values 4)))

(defun efrit-log--level-echo-p (level)
  "Return t if LEVEL should be echoed to message area."
  (>= (alist-get level efrit-log--level-values 4)
      (alist-get efrit-log-echo-level efrit-log--level-values 4)))

(defun efrit-log (level format-string &rest args)
  "Log message with LEVEL, FORMAT-STRING and ARGS.
Optional SUBSYSTEM can be provided as last argument if it's a string."
  (when (efrit-log--level-enabled-p level)
    (let* ((subsystem (when (and args (stringp (car (last args))))
                       (prog1 (car (last args))
                         (setq args (butlast args)))))
           (message (apply #'format format-string args))
           (timestamp (format-time-string "%H:%M:%S"))
           (level-str (upcase (symbol-name level)))
           (prefix (if subsystem
                      (format "[%s] %s/%s: " timestamp level-str subsystem)
                    (format "[%s] %s: " timestamp level-str))))
      
      ;; Write to log buffer
      (with-current-buffer (get-buffer-create efrit-log-buffer)
        (goto-char (point-max))
        (insert prefix message "\n")
        
        ;; Buffer size management
        (when (> (count-lines (point-min) (point-max)) efrit-log-max-lines)
          (goto-char (point-min))
          (forward-line (/ efrit-log-max-lines 5)) ; Remove 20%
          (delete-region (point-min) (point))))
      
      ;; Echo to message area if needed
      (when (efrit-log--level-echo-p level)
        (message "%s%s" prefix message)))))

;;; Convenience functions

(defsubst efrit-log-debug (format-string &rest args)
  "Log debug message."
  (apply #'efrit-log 'debug format-string args))

(defsubst efrit-log-info (format-string &rest args)
  "Log info message."
  (apply #'efrit-log 'info format-string args))

(defsubst efrit-log-warn (format-string &rest args)
  "Log warning message."
  (apply #'efrit-log 'warn format-string args))

(defsubst efrit-log-error (format-string &rest args)
  "Log error message."
  (apply #'efrit-log 'error format-string args))

(defun efrit-log-section (section-name)
  "Add a section separator to log output."
  (efrit-log 'info "=== %s ===" section-name))

;;; Buffer management

(defun efrit-log-show ()
  "Show the log buffer."
  (interactive)
  (pop-to-buffer (get-buffer-create efrit-log-buffer)))

(defun efrit-log-clear ()
  "Clear the log buffer."
  (interactive)
  (with-current-buffer (get-buffer-create efrit-log-buffer)
    (erase-buffer)
    (efrit-log-info "Log buffer cleared")))

(provide 'efrit-log)

;;; efrit-log.el ends here
