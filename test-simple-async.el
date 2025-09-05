#!/usr/bin/env emacs --script

;; Simple test for async integration

(add-to-list 'load-path (expand-file-name "lisp" default-directory))

(require 'efrit-log)
(require 'efrit-common)
(require 'efrit-config)
(require 'efrit-tools)
(require 'efrit-async)
(require 'efrit-do)

(message "Testing efrit-do-async with a simple command...")

(defun test-completion (result)
  "Handle test completion with RESULT."
  (message "\n=== ASYNC RESULT ===")
  (message "%s" result)
  (message "===================")
  (kill-emacs 0))

;; Test a simple async command
(condition-case err
    (efrit-async-execute-command 
     "(message \"Hello from async Claude!\")"
     #'test-completion)
  (error 
   (message "Error: %s" (error-message-string err))
   (kill-emacs 1)))

;; Keep script running to receive async response
(while t (sleep-for 0.1))