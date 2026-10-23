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

(defface efrit-log-debug '((t :inherit shadow)) "Face of DEBUG log lines." :group 'efrit)
(defface efrit-log-info '((t :inherit default)) "Face of INFO log lines." :group 'efrit)
(defface efrit-log-warn '((t :inherit warning)) "Face of WARN log lines." :group 'efrit)
(defface efrit-log-error '((t :inherit error)) "Face of ERROR log lines." :group 'efrit)
(defface efrit-log-timestamp '((t :inherit shadow)) "Face of the timestamp." :group 'efrit)

(defconst efrit-log-mode-font-lock-keywords
  '(("^\\(\\[[0-9:]+\\]\\) \\(DEBUG\\): \\(.*\\)$"
     (1 'efrit-log-timestamp) (2 'efrit-log-debug) (3 'efrit-log-debug))
    ("^\\(\\[[0-9:]+\\]\\) \\(INFO\\): " (1 'efrit-log-timestamp) (2 'efrit-log-info))
    ("^\\(\\[[0-9:]+\\]\\) \\(WARN\\): \\(.*\\)$"
     (1 'efrit-log-timestamp) (2 'efrit-log-warn) (3 'efrit-log-warn))
    ("^\\(\\[[0-9:]+\\]\\) \\(ERROR\\): \\(.*\\)$"
     (1 'efrit-log-timestamp) (2 'efrit-log-error) (3 'efrit-log-error))))

(defvar efrit-log-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "d") #'efrit-log-toggle-debug)
    (define-key map (kbd "c") #'efrit-log-clear)
    (define-key map (kbd "e") #'efrit-show-errors)
    (define-key map (kbd "g") #'efrit-log-goto-end)
    (define-key map (kbd "?") #'efrit-log-help)
    (define-key map (kbd "q") #'efrit-log-quit)
    map))

(defun efrit-log-quit ()
  "Bury the log buffer: the window closes, the buffer and its text stay."
  (interactive)
  (quit-window nil))

(defun efrit-log-help ()
  "Show the keys of the log buffer."
  (interactive)
  (message "%s"
           (substitute-command-keys
            "\\[efrit-log-toggle-debug] toggle debug logging  \\[efrit-log-clear] clear  \\[efrit-show-errors] errors only  \\[efrit-log-goto-end] newest line  \\[efrit-log-quit] close  \\[efrit-log-help] this help")))

(define-derived-mode efrit-log-mode special-mode "Efrit-Log"
  "The efrit log.  d toggles debug logging, c clears, e shows errors only, q closes."
  (setq-local font-lock-defaults '(efrit-log-mode-font-lock-keywords t))
  (setq-local truncate-lines t)
  (efrit-log--update-header))

(defun efrit-log--update-header ()
  (setq header-line-format
        (format " efrit log · level %s · ? keys" efrit-log-level)))

;;;###autoload
(defun efrit-log-toggle-debug ()
  "Switch `efrit-log-level' between debug and info.
Debug records every bus event, each API request and response, every
sandbox decision: the trace to read when something looks wrong."
  (interactive)
  (setq efrit-log-level (if (eq efrit-log-level 'debug) 'info 'debug))
  (efrit-log 'info "log level now %s" efrit-log-level)
  (with-current-buffer (get-buffer-create efrit-log-buffer)
    (when (derived-mode-p 'efrit-log-mode) (efrit-log--update-header)))
  (message "efrit log level: %s" efrit-log-level))

(defun efrit-log-goto-end ()
  "Go to the newest line."
  (interactive)
  (goto-char (point-max)))

(defun efrit-log--buffer ()
  "The log buffer, in `efrit-log-mode'."
  (let ((buf (get-buffer-create efrit-log-buffer)))
    (with-current-buffer buf
      (unless (derived-mode-p 'efrit-log-mode) (efrit-log-mode)))
    buf))

(defun efrit-log (level format-string &rest args)
  "Log message with LEVEL, FORMAT-STRING and ARGS."
  (when (efrit-log--level-enabled-p level)
    (let* ((message (if args
                       (apply #'format format-string args)
                     format-string))
           (timestamp (format-time-string "%H:%M:%S"))
           (level-str (upcase (symbol-name level)))
           (prefix (format "[%s] %s: " timestamp level-str)))
      
      ;; Write to log buffer; keep windows showing the end at the end
      (with-current-buffer (efrit-log--buffer)
        (let ((inhibit-read-only t)
              (at-end (mapcar (lambda (w) (cons w (>= (window-point w) (1- (point-max)))))
                              (get-buffer-window-list (current-buffer) nil t))))
          (save-excursion
            (goto-char (point-max))
            (insert prefix message "\n"))
          (dolist (w at-end) (when (cdr w) (set-window-point (car w) (point-max))))
          ;; Buffer size management
          (when (> (count-lines (point-min) (point-max)) efrit-log-max-lines)
            (save-excursion
              (goto-char (point-min))
              (forward-line (/ efrit-log-max-lines 5)) ; Remove 20%
              (delete-region (point-min) (point))))))
      
      ;; Echo to message area if needed
      (when (efrit-log--level-echo-p level)
        ;; Escape % characters in message to prevent format string errors
        (let ((safe-message (replace-regexp-in-string "%" "%%" message)))
          (message "%s%s" prefix safe-message))))))

(defun efrit-log-safe (level format-string &rest args)
  "Log message with automatic API key and sensitive data sanitization.
Scans all ARGS for strings that look like API keys and sanitizes them.
LEVEL is the log level (debug, info, warn, error).
FORMAT-STRING and ARGS are as in efrit-log."
  (let ((sanitized-args
         (mapcar (lambda (arg)
                   (if (and (stringp arg)
                            (>= (length arg) 20)
                            (string-prefix-p "sk-" arg))
                       ;; Sanitize API key-like strings
                       (concat (substring arg 0 6) "..." (substring arg -4))
                     arg))
                 args)))
    (apply #'efrit-log level format-string sanitized-args)))

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

;;;###autoload
(defun efrit-log-show ()
  "Show the log buffer, newest line at the bottom."
  (interactive)
  (let ((buf (efrit-log--buffer)))
    (pop-to-buffer buf '((display-buffer-reuse-window display-buffer-at-bottom)
                         (window-height . 0.4)
                         (dedicated . t)))
    (goto-char (point-max))))

(defun efrit-log-clear ()
  "Clear the log buffer."
  (interactive)
  (with-current-buffer (efrit-log--buffer)
    (let ((inhibit-read-only t)) (erase-buffer))
    (efrit-log-info "Log buffer cleared")))

;;;###autoload
(defun efrit-show-errors ()
  "Show only error and warning messages from the log in a dedicated buffer."
  (interactive)
  (let ((errors '())
        (log-buffer (get-buffer efrit-log-buffer)))
    (if (not log-buffer)
        (message "No efrit log buffer exists yet")
      (with-current-buffer log-buffer
        (save-excursion
          (goto-char (point-min))
          (while (not (eobp))
            (let ((line (buffer-substring-no-properties
                        (line-beginning-position)
                        (line-end-position))))
              (when (string-match-p "\\[\\(ERROR\\|WARN\\)\\]" line)
                (push line errors)))
            (forward-line 1))))
      (if (null errors)
          (message "No errors or warnings in log")
        (with-current-buffer (get-buffer-create "*efrit-errors*")
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert "Efrit Errors and Warnings\n")
            (insert "=========================\n\n")
            (dolist (error (reverse errors))
              (insert error "\n"))
            (insert (format "\n[Total: %d errors/warnings]\n" (length errors)))
            (goto-char (point-min))
            (view-mode))
          (display-buffer (current-buffer)))))))

(provide 'efrit-log)

;;; efrit-log.el ends here
