;;; test-dashboard.el --- Simple test for Efrit Dashboard

;; Test script to validate dashboard functionality

(add-to-list 'load-path "./lisp")

;; Load the dashboard
(require 'efrit-dashboard)

;; Test basic functionality
(message "Testing Efrit Dashboard...")

;; Create some test data
(let ((test-session-file (expand-file-name "test-session.json" 
                                          (expand-file-name "sessions" efrit-data-directory))))
  (make-directory (file-name-directory test-session-file) t)
  (with-temp-file test-session-file
    (insert (json-encode '((id . "test-session")
                          (start-time . "2025-01-01 10:00:00")
                          (metrics . ((commands-executed . 5)
                                     (todos-created . 3)
                                     (todos-completed . 1))))))))

;; Create test TODO data
(let ((test-todos-file (expand-file-name "todos.json" 
                                        (expand-file-name "context" efrit-data-directory))))
  (make-directory (file-name-directory test-todos-file) t)
  (with-temp-file test-todos-file
    (insert (json-encode '[((id . "todo-1")
                           (content . "Test TODO item")
                           (status . "todo")
                           (priority . "high"))
                          ((id . "todo-2")
                           (content . "Another test item")
                           (status . "completed")
                           (priority . "medium"))]))))

;; Test dashboard functions
(message "✅ Dashboard module loads successfully")

;; Test buffer creation
(efrit-dashboard)
(let ((dashboard-buffer (get-buffer "*efrit-dashboard*")))
  (if dashboard-buffer
      (message "✅ Dashboard buffer created successfully")
    (error "❌ Dashboard buffer creation failed")))

;; Test refresh
(efrit-dashboard-refresh)
(message "✅ Dashboard refresh completed")

;; Clean up test files
(delete-file (expand-file-name "test-session.json" 
                              (expand-file-name "sessions" efrit-data-directory)))
(delete-file (expand-file-name "todos.json" 
                              (expand-file-name "context" efrit-data-directory)))

(message "✅ Dashboard test completed successfully!")

;; End test
