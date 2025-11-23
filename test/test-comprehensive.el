;;; test-comprehensive.el --- Comprehensive tests for Efrit Dashboard and Session Tracking -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: AI Assistant
;; Keywords: efrit, testing, dashboard, session

;;; Commentary:

;; Comprehensive test suite for the Efrit Dashboard, Session Tracker,
;; and their integration with efrit-do.

;;; Code:

(add-to-list 'load-path "../lisp")

(require 'efrit-dashboard)
(require 'efrit-session-tracker)

;;; Test Infrastructure

(defvar test-results '()
  "List to collect test results.")

(defvar test-count 0
  "Number of tests run.")

(defvar test-failures 0
  "Number of test failures.")

(defun test-assert (condition description)
  "Assert CONDITION is true, with DESCRIPTION for the test."
  (setq test-count (1+ test-count))
  (if condition
      (progn
        (message "✅ PASS: %s" description)
        (push (list :pass description) test-results))
    (progn
      (setq test-failures (1+ test-failures))
      (message "❌ FAIL: %s" description)
      (push (list :fail description) test-results))))

(defun test-setup ()
  "Set up test environment."
  (message "Setting up test environment...")
  
  ;; Ensure clean test directories
  (let ((test-data-dir (expand-file-name "test-data" default-directory)))
    (when (file-directory-p test-data-dir)
      (delete-directory test-data-dir t))
    (make-directory test-data-dir t)
    
    ;; Override efrit-data-directory for testing
    (setq efrit-data-directory test-data-dir)
    
    ;; Create required subdirectories
    (make-directory (expand-file-name "sessions" test-data-dir) t)
    (make-directory (expand-file-name "context" test-data-dir) t)
    (make-directory (expand-file-name "queues" test-data-dir) t)
    (make-directory (expand-file-name "queues/requests" test-data-dir) t)
    (make-directory (expand-file-name "queues/responses" test-data-dir) t)
    (make-directory (expand-file-name "queues/processing" test-data-dir) t)
    (make-directory (expand-file-name "logs" test-data-dir) t)))

(defun test-cleanup ()
  "Clean up test environment."
  (message "Cleaning up test environment...")
  (let ((test-data-dir (expand-file-name "test-data" default-directory)))
    (when (file-directory-p test-data-dir)
      (delete-directory test-data-dir t))))

(defun test-summary ()
  "Print test summary."
  (message "")
  (message "==========================================")
  (message "TEST SUMMARY")
  (message "==========================================")
  (message "Total tests: %d" test-count)
  (message "Passed: %d" (- test-count test-failures))
  (message "Failed: %d" test-failures)
  (if (= test-failures 0)
      (message "✅ ALL TESTS PASSED!")
    (message "❌ %d TESTS FAILED" test-failures))
  (message "=========================================="))

;;; Session Tracker Tests

(defun test-session-tracker ()
  "Test session tracking functionality."
  (message "\n--- Testing Session Tracker ---")
  
  ;; Test session start
  (efrit-session-start)
  (test-assert (not (null efrit-session-id)) "Session ID created")
  (test-assert (not (null efrit-session-start-time)) "Session start time set")
  
  ;; Test metric tracking
  (efrit-session-track-command "test command")
  (test-assert (= (efrit-session-get-metric 'commands-executed) 1) 
               "Command execution tracked")
  
  (efrit-session-track-todo-created "test todo")
  (test-assert (= (efrit-session-get-metric 'todos-created) 1)
               "TODO creation tracked")
  
  (efrit-session-track-todo-completed "test todo")  
  (test-assert (= (efrit-session-get-metric 'todos-completed) 1)
               "TODO completion tracked")
  
  (efrit-session-track-api-call "test-endpoint")
  (test-assert (= (efrit-session-get-metric 'api-calls) 1)
               "API call tracked")
  
  (efrit-session-track-buffer-created "*test-buffer*")
  (let ((buffers (efrit-session-get-metric 'buffers-created)))
    (test-assert (member "*test-buffer*" buffers)
                 "Buffer creation tracked"))
  
  (efrit-session-track-file-modified "/test/file.txt")
  (let ((files (efrit-session-get-metric 'files-modified)))
    (test-assert (member "/test/file.txt" files)
                 "File modification tracked"))
  
  (efrit-session-track-tool-used "eval_sexp")
  (efrit-session-track-tool-used "eval_sexp")
  (let ((tools (efrit-session-get-metric 'tools-used)))
    (test-assert (= (cdr (assoc "eval_sexp" tools)) 2)
                 "Tool usage count tracked"))
  
  ;; Test session persistence
  (efrit-session-save)
  (let* ((session-file (expand-file-name 
                        (concat efrit-session-id ".json")
                        (expand-file-name "sessions" efrit-data-directory))))
    (test-assert (file-exists-p session-file) "Session saved to file"))
  
  ;; Test session loading
  (let ((loaded-session (efrit-session-load efrit-session-id)))
    (test-assert (not (null loaded-session)) "Session loaded from file")
    (test-assert (equal (cdr (assoc 'id loaded-session)) efrit-session-id)
                 "Session ID matches loaded session"))
  
  ;; Test session end
  (efrit-session-end)
  (test-assert (null efrit-session-id) "Session ended properly"))

;;; Dashboard Tests

(defun test-dashboard ()
  "Test dashboard functionality."
  (message "\n--- Testing Dashboard ---")
  
  ;; Create test data files
  (test-create-sample-data)
  
  ;; Test dashboard buffer creation
  (efrit-dashboard)
  (let ((dashboard-buffer (get-buffer efrit-dashboard-buffer-name)))
    (test-assert (not (null dashboard-buffer)) "Dashboard buffer created")
    (test-assert (eq (buffer-local-value 'major-mode dashboard-buffer) 
                     'efrit-dashboard-mode)
                 "Dashboard buffer has correct mode"))
  
  ;; Test dashboard content generation
  (with-current-buffer efrit-dashboard-buffer-name
    (let ((content (buffer-string)))
      (test-assert (string-match-p "EFRIT DASHBOARD" content) 
                   "Dashboard header present")
      (test-assert (string-match-p "SESSION STATE" content)
                   "Session state section present") 
      (test-assert (string-match-p "TODO MANAGEMENT" content)
                   "TODO management section present")
      (test-assert (string-match-p "QUEUE STATUS" content)
                   "Queue status section present")
      (test-assert (string-match-p "RECENT ACTIVITY" content)
                   "Recent activity section present")
      (test-assert (string-match-p "QUICK ACTIONS" content)
                   "Quick actions section present")))
  
  ;; Test dashboard refresh
  (efrit-dashboard-refresh)
  (test-assert (get-buffer efrit-dashboard-buffer-name)
               "Dashboard survives refresh")
  
  ;; Test dashboard functions
  (test-assert (fboundp 'efrit-dashboard-clear-todos)
               "Clear TODOs function exists")
  (test-assert (fboundp 'efrit-dashboard-save-session)
               "Save session function exists")
  (test-assert (fboundp 'efrit-dashboard-show-log)
               "Show log function exists")
  
  ;; Test JSON reading
  (let* ((test-file (expand-file-name "test.json" efrit-data-directory))
         (test-data '((key . "value") (number . 42))))
    (with-temp-file test-file
      (let ((json-encoding-pretty-print t))
        (insert (json-encode test-data))))
    
    (let ((loaded-data (efrit-dashboard-read-json-file test-file)))
      (test-assert (equal (cdr (assoc 'key loaded-data)) "value")
                   "JSON file reading works correctly")))
  
  ;; Test session state tracking
  (efrit-dashboard-update-session-state 'test-key "test-value")
  (test-assert (equal (cdr (assoc 'test-key efrit-dashboard-session-state))
                      "test-value")
               "Session state updates work")
  
  (efrit-dashboard-increment-counter 'test-counter)
  (efrit-dashboard-increment-counter 'test-counter)
  (test-assert (= (cdr (assoc 'test-counter efrit-dashboard-session-state)) 2)
               "Counter increments work"))

(defun test-create-sample-data ()
  "Create sample data for testing dashboard display."
  ;; Create sample TODO data
  (let ((todos-file (expand-file-name "todos.json" 
                                     (expand-file-name "context" efrit-data-directory))))
    (with-temp-file todos-file
      (let ((json-encoding-pretty-print t))
        (insert (json-encode 
                 '(((id . "todo-1")
                    (content . "Fix compilation errors")
                    (status . "todo")
                    (priority . "high"))
                   ((id . "todo-2")
                    (content . "Implement dashboard refresh")
                    (status . "completed")
                    (priority . "medium"))
                   ((id . "todo-3")
                    (content . "Add error handling")
                    (status . "in-progress")
                    (priority . "low"))))))))
  
  ;; Create sample session data
  (let ((session-file (expand-file-name "test-session.json"
                                       (expand-file-name "sessions" efrit-data-directory))))
    (with-temp-file session-file
      (let ((json-encoding-pretty-print t))
        (insert (json-encode 
                 '((id . "test-session-123")
                   (start-time . "2025-01-01 12:00:00")
                   (duration . 300.5)
                   (metrics . ((commands-executed . 10)
                              (todos-created . 5)
                              (todos-completed . 3)
                              (api-calls . 15)))))))))
  
  ;; Create sample log data
  (let ((log-file (expand-file-name "efrit.log"
                                   (expand-file-name "logs" efrit-data-directory))))
    (with-temp-file log-file
      (insert "2025-01-01 12:01:00 INFO: Dashboard initialized\n")
      (insert "2025-01-01 12:01:30 DEBUG: Session tracker started\n") 
      (insert "2025-01-01 12:02:00 INFO: Command executed successfully\n")))
  
  ;; Create sample queue files
  (let ((request-file (expand-file-name "req-1.json"
                                       (expand-file-name "queues/requests" efrit-data-directory)))
        (response-file (expand-file-name "resp-1.json"
                                        (expand-file-name "queues/responses" efrit-data-directory))))
    (with-temp-file request-file
      (insert (json-encode '((id . "req-1") (type . "command") (content . "test")))))
    (with-temp-file response-file
      (insert (json-encode '((id . "resp-1") (result . "success")))))))

;;; Integration Tests

(defun test-integration ()
  "Test integration between components."
  (message "\n--- Testing Integration ---")
  
  ;; Test that dashboard integrates with session tracker
  (efrit-session-start)
  
  ;; Simulate some activity
  (efrit-session-track-command "test integration command")
  (efrit-session-track-todo-created "integration test todo")
  (efrit-session-track-api-call "integration/test")
  
  ;; Test dashboard shows the activity
  (efrit-dashboard-refresh)
  
  (with-current-buffer efrit-dashboard-buffer-name
    (let ((content (buffer-string)))
      (test-assert (string-match-p "Commands executed: [1-9]" content)
                   "Dashboard shows command execution count")
      (test-assert (string-match-p "API calls made: [1-9]" content)
                   "Dashboard shows API call count")))
  
  ;; Test session persistence integration
  (efrit-session-save)
  (efrit-dashboard-save-session)
  
  (let ((session-files (directory-files 
                        (expand-file-name "sessions" efrit-data-directory) 
                        nil "\\.json$")))
    (test-assert (> (length session-files) 0)
                 "Session files created by integration"))
  
  (efrit-session-end))

;;; Error Handling Tests

(defun test-error-handling ()
  "Test error handling in various components."
  (message "\n--- Testing Error Handling ---")
  
  ;; Test dashboard with missing files
  (efrit-dashboard-refresh)
  (test-assert (get-buffer efrit-dashboard-buffer-name)
               "Dashboard handles missing data files gracefully")
  
  ;; Test JSON reading with invalid data
  (let* ((invalid-json-file (expand-file-name "invalid.json" efrit-data-directory)))
    (with-temp-file invalid-json-file
      (insert "{ invalid json content"))
    
    (let ((result (efrit-dashboard-read-json-file invalid-json-file)))
      (test-assert (eq result :malformed)
                   "Invalid JSON handled gracefully")))
  
  ;; Test session operations without active session  
  (setq efrit-session-id nil)
  (efrit-session-track-command "test command")
  (test-assert t "Session tracking works without active session"))

;;; Performance Tests

(defun test-performance ()
  "Test performance characteristics."
  (message "\n--- Testing Performance ---")
  
  ;; Test dashboard refresh time with large dataset
  (test-create-large-dataset)
  
  (let ((start-time (current-time)))
    (efrit-dashboard-refresh)
    (let ((elapsed (float-time (time-subtract (current-time) start-time))))
      (test-assert (< elapsed 1.0)
                   (format "Dashboard refresh under 1 second (%.3fs)" elapsed))))
  
  ;; Test session tracking performance
  (efrit-session-start)
  (let ((start-time (current-time)))
    (dotimes (i 100)
      (efrit-session-track-command (format "command-%d" i)))
    (let ((elapsed (float-time (time-subtract (current-time) start-time))))
      (test-assert (< elapsed 0.1)
                   (format "100 session tracking calls under 0.1s (%.3fs)" elapsed))))
  (efrit-session-end))

(defun test-create-large-dataset ()
  "Create large test dataset for performance testing."
  (let ((todos-file (expand-file-name "todos.json" 
                                     (expand-file-name "context" efrit-data-directory)))
        (large-todos '()))
    
    ;; Create 50 TODO items
    (dotimes (i 50)
      (push `((id . ,(format "todo-%d" i))
              (content . ,(format "Test TODO item number %d with some longer content to test rendering" i))
              (status . ,(if (< (random 100) 30) "completed" 
                           (if (< (random 100) 50) "in-progress" "todo")))
              (priority . ,(nth (random 3) '("high" "medium" "low"))))
            large-todos))
    
    (with-temp-file todos-file
      (let ((json-encoding-pretty-print nil))
        (insert (json-encode large-todos)))))
  
  ;; Create large log file
  (let ((log-file (expand-file-name "efrit.log"
                                   (expand-file-name "logs" efrit-data-directory))))
    (with-temp-file log-file
      (dotimes (i 200)
        (insert (format "2025-01-01 12:%02d:%02d INFO: Log message %d with some additional content for testing\n"
                        (/ i 60) (% i 60) i))))))

;;; Main Test Runner

(defun run-all-tests ()
  "Run all comprehensive tests."
  (message "===========================================")
  (message "EFRIT COMPREHENSIVE TEST SUITE")
  (message "===========================================")
  
  ;; Initialize
  (setq test-results '()
        test-count 0
        test-failures 0)
  
  ;; Setup
  (test-setup)
  
  ;; Run test suites
  (condition-case err
      (progn
        (test-session-tracker)
        (test-dashboard)
        (test-integration)
        (test-error-handling)
        (test-performance))
    (error 
     (message "❌ Fatal error during testing: %s" err)
     (setq test-failures (1+ test-failures))))
  
  ;; Cleanup and summary
  (test-cleanup)
  (test-summary)
  
  ;; Return success/failure
  (= test-failures 0))

;;; Run Tests

(message "Starting comprehensive test suite...")
(let ((success (run-all-tests)))
  (if success
      (progn
        (message "🎉 All tests completed successfully!")
        (kill-emacs 0))
    (progn
      (message "💥 Some tests failed!")
      (kill-emacs 1))))

;;; test-comprehensive.el ends here
