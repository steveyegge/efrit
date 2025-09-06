;;; interactive-todo-test.el --- Interactive test for Efrit TODO system -*- lexical-binding: t -*-

;; Load the system
(add-to-list 'load-path (expand-file-name "lisp"))
(require 'efrit-log)
(require 'efrit-common)
(require 'efrit-config) 
(require 'efrit-context)
(require 'efrit-tools)
(require 'efrit-progress)
(require 'efrit-async)
(require 'efrit-do)

;; Load test system
(load "test-todo-system.el")

(defvar test-session-id nil "Current test session ID.")

(defun test-setup-warnings-buffer ()
  "Create *Warnings* buffer with lexical-binding issues."
  (interactive)
  (with-current-buffer (get-buffer-create "*Warnings*")
    (erase-buffer)
    (insert "Warning messages about missing lexical-binding:\n\n")
    (insert "file1.el:1: Warning: Package lacks a file-local 'lexical-binding' directive\n")
    (insert "file2.el:1: Warning: Package lacks a file-local 'lexical-binding' directive\n") 
    (insert "file3.el:1: Warning: Package lacks a file-local 'lexical-binding' directive\n")
    (insert "file4.el:1: Warning: Package lacks a file-local 'lexical-binding' directive\n")
    (insert "file5.el:1: Warning: Package lacks a file-local 'lexical-binding' directive\n")
    (insert "\nTotal: 5 lexical-binding warnings found\n")
    (insert "\nThese files need to have -*- lexical-binding: t -*- added to their headers.\n")
    (goto-char (point-min))
    (switch-to-buffer (current-buffer))
    (message "Created *Warnings* buffer with 5 lexical-binding warnings")))

(defun test-start-async-session ()
  "Start an async session to fix warnings."
  (interactive)
  (message "Starting async session to fix warnings...")
  (setq test-session-id
        (efrit-async-execute-command 
         "fix all warnings in *Warnings* buffer"
         (lambda (session-id success response)
           (message "Async session completed: %s, Success: %s" session-id success)
           (when response
             (message "Response: %s" (substring response 0 (min 200 (length response)))))))
  (message "Started async session: %s" test-session-id))

(defun test-monitor-todos ()
  "Monitor the current TODOs."
  (interactive)
  (message "=== Current TODO Status ===")
  (if (fboundp 'efrit-async-show-todos)
      (efrit-async-show-todos)
    (message "efrit-async-show-todos not available"))
  
  (message "\n=== Progress Status ===")
  (if (fboundp 'efrit-progress-show)
      (efrit-progress-show)
    (message "efrit-progress-show not available"))
    
  (message "\n=== Session Status ===")
  (if (fboundp 'efrit-async-status)
      (efrit-async-status)
    (message "efrit-async-status not available")))

(defun test-check-session-log ()
  "Check the session work log."
  (interactive)
  (if (fboundp 'efrit-async-show-log)
      (efrit-async-show-log)
    (message "efrit-async-show-log not available")))

(defun test-verify-completion ()
  "Verify if loop prevention is working."
  (interactive)
  (message "=== Verifying TODO Completion and Loop Prevention ===")
  (if (fboundp 'verify-todo-completion)
      (verify-todo-completion)
    (message "verify-todo-completion not available"))
  
  ;; Also check if we have any active sessions
  (message "Active session: %s" test-session-id)
  (when efrit-async--active-session
    (message "System shows active session: %s" 
             (plist-get efrit-async--active-session :id))))

(defun test-full-workflow ()
  "Run the complete test workflow."
  (interactive)
  (message "=== Starting Full Efrit TODO Test Workflow ===")
  
  ;; Step 1: Setup
  (test-setup-warnings-buffer)
  (sleep-for 1)
  
  ;; Step 2: Clear existing state
  (message "Clearing existing TODOs...")
  (when (fboundp 'efrit-do-clear-todos)
    (efrit-do-clear-todos))
  
  ;; Step 3: Start async session (this would require API key)
  (message "Would start async session with command:")
  (message "  'fix all warnings in *Warnings* buffer'")
  (message "")
  (message "To continue manually:")
  (message "1. M-x test-start-async-session")
  (message "2. M-x test-monitor-todos")
  (message "3. M-x test-check-session-log")
  (message "4. M-x test-verify-completion")
  (message "")
  (message "Key bindings:")
  (message "C-c t 1 - Setup warnings buffer")
  (message "C-c t 2 - Start async session")
  (message "C-c t 3 - Monitor TODOs")
  (message "C-c t 4 - Check session log")
  (message "C-c t 5 - Verify completion"))

;; Key bindings for easy testing
(global-set-key (kbd "C-c t 1") 'test-setup-warnings-buffer)
(global-set-key (kbd "C-c t 2") 'test-start-async-session)
(global-set-key (kbd "C-c t 3") 'test-monitor-todos)
(global-set-key (kbd "C-c t 4") 'test-check-session-log)
(global-set-key (kbd "C-c t 5") 'test-verify-completion)
(global-set-key (kbd "C-c t f") 'test-full-workflow)

(message "Interactive Efrit TODO test system loaded!")
(message "Run M-x test-full-workflow to start, or use individual test functions.")
(message "Key bindings: C-c t 1-5 for individual steps, C-c t f for full workflow")

(provide 'interactive-todo-test)