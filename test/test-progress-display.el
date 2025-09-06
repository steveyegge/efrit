;;; test-progress-display.el --- Test progress display functionality -*- lexical-binding: t -*-

;;; Commentary:
;; Demonstrates how progress is shown for async operations

;;; Code:

(require 'efrit)

(defun test-progress-warnings-scenario ()
  "Test progress display with the warnings buffer scenario."
  (interactive)
  ;; Create mock warnings buffer
  (with-current-buffer (get-buffer-create "*Warnings*")
    (erase-buffer)
    (insert "Warning (initialization): Your 'load-path' shadows the built-in Org version.

~/.emacs.d/lisp/my-config.el: Warning: file has no `lexical-binding' directive on its first line
~/.emacs.d/lisp/my-utils.el: Warning: file has no `lexical-binding' directive on its first line
~/.emacs.d/custom/theme-config.el: Warning: file has no `lexical-binding' directive on its first line"))
  
  ;; Execute command asynchronously
  (efrit-do-async "Fix all the warnings in the *Warnings* buffer")
  
  ;; Show progress buffer
  (efrit-progress-show)
  
  (message "Progress display test started. Watch the *Efrit Progress* buffer..."))

(defun test-progress-simple-command ()
  "Test progress display with a simple command."
  (interactive)
  (efrit-do-async "What files are in the current directory?")
  (efrit-progress-show)
  (message "Simple command test started. Watch the *Efrit Progress* buffer..."))

(defun test-progress-verbosity ()
  "Test cycling through verbosity levels."
  (interactive)
  (efrit-progress-show)
  (with-current-buffer "*Efrit Progress*"
    (efrit-progress-cycle-verbosity)
    (message "Verbosity: %s" efrit-progress-verbosity)
    (sit-for 1)
    (efrit-progress-cycle-verbosity) 
    (message "Verbosity: %s" efrit-progress-verbosity)
    (sit-for 1)
    (efrit-progress-cycle-verbosity)
    (message "Verbosity: %s" efrit-progress-verbosity)))

;; Run a simple test
(when (called-interactively-p 'any)
  (test-progress-simple-command))

(provide 'test-progress-display)
;;; test-progress-display.el ends here