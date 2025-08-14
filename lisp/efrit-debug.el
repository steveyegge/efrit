;;; efrit-debug.el --- Debug logging for efrit -*- lexical-binding: t -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Keywords: ai, tools, debug

;;; Commentary:
;; Simple debug logging system for efrit that writes to *efrit-debug* buffer

;;; Code:

(defvar efrit-debug-enabled nil
  "Whether debug logging is enabled.")

(defvar efrit--debug-buffer-name "*efrit-debug*"
  "Name of the debug buffer.")

(defun efrit--debug-buffer ()
  "Get or create the efrit debug buffer."
  (get-buffer-create efrit--debug-buffer-name))

(defun efrit-debug-log (format-string &rest args)
  "Log a debug message to the efrit debug buffer."
  (when efrit-debug-enabled
    (with-current-buffer (efrit--debug-buffer)
      (goto-char (point-max))
      (insert (format "[%s] %s\n" 
                      (format-time-string "%H:%M:%S")
                      (apply #'format format-string args)))
      ;; Keep buffer size reasonable (last 1000 lines)
      (when (> (count-lines (point-min) (point-max)) 1000)
        (goto-char (point-min))
        (forward-line 200)
        (delete-region (point-min) (point))))))

(defun efrit-debug-clear ()
  "Clear the debug buffer."
  (interactive)
  (with-current-buffer (efrit--debug-buffer)
    (erase-buffer)
    (efrit-debug-log "Debug buffer cleared")))

(defun efrit-debug-show ()
  "Show the debug buffer."
  (interactive)
  (pop-to-buffer (efrit--debug-buffer)))

(defun efrit-debug-section (section-name)
  "Add a section separator to debug output."
  (efrit-debug-log "=== %s ===" section-name))

(provide 'efrit-debug)

;;; efrit-debug.el ends here
