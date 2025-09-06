#!/usr/bin/env emacs --script
;;; test-efrit-todos.el --- Test script for Efrit TODO system -*- lexical-binding: t -*-

;; Add the lisp directory to load path
(add-to-list 'load-path (expand-file-name "lisp" (file-name-directory load-file-name)))

;; Load all required modules
(require 'efrit-log)
(require 'efrit-common) 
(require 'efrit-config)
(require 'efrit-context)
(require 'efrit-tools)
(require 'efrit-progress)
(require 'efrit-async)
(require 'efrit-do)

;; Load the test system
(load (expand-file-name "test-todo-system.el" (file-name-directory load-file-name)))

(message "=== Starting Efrit TODO System Test ===")
(message "Loading completed successfully")

;; Function to create warnings buffer for testing
(defun create-warnings-buffer ()
  "Create a *Warnings* buffer with lexical-binding issues."
  (with-current-buffer (get-buffer-create "*Warnings*")
    (erase-buffer)
    (insert "Warning messages about missing lexical-binding:\n\n")
    (insert "file1.el:1: Warning: Package lacks a file-local 'lexical-binding' directive\n")
    (insert "file2.el:1: Warning: Package lacks a file-local 'lexical-binding' directive\n") 
    (insert "file3.el:1: Warning: Package lacks a file-local 'lexical-binding' directive\n")
    (insert "file4.el:1: Warning: Package lacks a file-local 'lexical-binding' directive\n")
    (insert "\nTotal: 4 lexical-binding warnings found\n")
    (goto-char (point-min))
    (current-buffer)))

;; Test function to run the TODO workflow
(defun test-efrit-todo-workflow ()
  "Run the complete TODO workflow test."
  (message "Step 1: Creating test warnings buffer...")
  (let ((warnings-buffer (create-warnings-buffer)))
    (message "Created buffer: %s with %d characters" 
             (buffer-name warnings-buffer)
             (buffer-size warnings-buffer)))
  
  (message "Step 2: Clearing any existing TODOs...")
  (when (fboundp 'efrit-do-clear-todos)
    (efrit-do-clear-todos))
  
  (message "Step 3: Ready to test efrit-do-async")
  (message "Buffer *Warnings* is ready with lexical-binding warnings")
  (message "You can now run: (efrit-do-async \"fix all warnings in *Warnings* buffer\")")
  
  ;; Show available monitoring commands
  (message "\nMonitoring commands available:")
  (message "- efrit-async-show-todos")
  (message "- efrit-progress-show") 
  (message "- efrit-async-status")
  (message "- efrit-async-show-log")
  
  t)

;; Function to simulate the async command (for testing purposes)
(defun simulate-async-test ()
  "Simulate the async workflow for testing."
  (message "=== Simulating async TODO workflow ===")
  
  ;; Create the warnings buffer
  (create-warnings-buffer)
  
  ;; This would normally be called by efrit-do-async
  (message "Simulating: efrit-do-async 'fix all warnings in *Warnings* buffer'")
  
  ;; Check if the system is properly configured
  (if (and (boundp 'efrit-api-key) efrit-api-key)
      (message "API key configured: %s" (substring efrit-api-key 0 10))
    (message "WARNING: No API key configured - async calls will fail"))
  
  (message "Test setup complete. Manual testing required for actual async execution."))

;; Run the test setup
(condition-case err
    (progn
      (test-efrit-todo-workflow)
      (message "\n=== Test Setup Successful ===")
      (message "Ready for manual testing of TODO system"))
  (error 
   (message "Error during test setup: %s" err)))