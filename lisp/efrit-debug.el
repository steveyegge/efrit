;;; efrit-debug.el --- Debug logging compatibility layer -*- lexical-binding: t -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Keywords: ai, tools, debug

;;; Commentary:
;; Compatibility layer for legacy efrit-debug calls.
;; New code should use efrit-log.el directly.

;;; Code:

(require 'efrit-log)

;; Compatibility variables (preserved but unused)
;;;###obsolete
(defvar efrit-debug-enabled nil
  "Whether debug logging is enabled.
This variable is obsolete; use `efrit-log-level' instead.")
(make-obsolete-variable 'efrit-debug-enabled 'efrit-log-level "0.2")

;;;###obsolete
(defvar efrit--debug-buffer-name "*efrit-debug*"
  "Name of the debug buffer.
This variable is obsolete; use `efrit-log-buffer' instead.")
(make-obsolete-variable 'efrit--debug-buffer-name 'efrit-log-buffer "0.2")

;; Compatibility functions

;;;###obsolete
(defun efrit--debug-buffer ()
  "Get or create the efrit debug buffer."
  (get-buffer-create efrit-log-buffer))
(make-obsolete 'efrit--debug-buffer 'get-buffer-create "0.2")

;;;###obsolete
(defun efrit-debug-log (format-string &rest args)
  "Log a debug message (compatibility wrapper)."
  (when efrit-debug-enabled
    (apply #'efrit-log-debug format-string args)))
(make-obsolete 'efrit-debug-log 'efrit-log-debug "0.2")

;;;###obsolete
(defun efrit-debug-clear ()
  "Clear the debug buffer (compatibility wrapper)."
  (interactive)
  (efrit-log-clear))
(make-obsolete 'efrit-debug-clear 'efrit-log-clear "0.2")

;;;###obsolete
(defun efrit-debug-show ()
  "Show the debug buffer (compatibility wrapper)."
  (interactive)
  (efrit-log-show))
(make-obsolete 'efrit-debug-show 'efrit-log-show "0.2")

;;;###obsolete
(defun efrit-debug-section (section-name)
  "Add a section separator (compatibility wrapper)."
  (efrit-log-section section-name))
(make-obsolete 'efrit-debug-section 'efrit-log-section "0.2")

(provide 'efrit-debug)

;;; efrit-debug.el ends here
