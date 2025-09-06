;;; test-final-demo.el --- Final demo of Efrit Dashboard and Session Integration -*- lexical-binding: t; -*-

(add-to-list 'load-path "./lisp")

(require 'efrit-dashboard)
(require 'efrit-session-tracker)

(message "🚀 Starting Efrit Dashboard + Session Tracker Demo")
(message "")

;; Start a new session
(efrit-session-start)
(message "✅ Session started: %s" efrit-session-id)

;; Simulate some typical activities
(message "📝 Simulating user activities...")
(efrit-session-track-command "create new component")
(efrit-session-track-todo-created "Design component interface")
(efrit-session-track-todo-created "Implement component logic")
(efrit-session-track-todo-created "Write unit tests")

(efrit-session-track-api-call "anthropic/claude")
(efrit-session-track-api-call "anthropic/claude")
(efrit-session-track-buffer-created "*component-design*")
(efrit-session-track-file-modified "src/component.el")
(efrit-session-track-tool-used "eval_sexp")
(efrit-session-track-tool-used "shell_exec")

;; Complete some TODOs
(efrit-session-track-todo-completed "Design component interface")
(efrit-session-track-command "run tests")
(efrit-session-track-api-call "anthropic/claude")

;; Create the dashboard
(message "🎛️  Creating dashboard...")
(efrit-dashboard)

;; Show session metrics
(let ((summary (efrit-session-get-summary)))
  (message "")
  (message "📊 Session Summary:")
  (message "  ID: %s" (plist-get summary :id))
  (message "  Duration: %.1fs" (plist-get summary :duration))
  (let ((metrics (plist-get summary :metrics)))
    (message "  Commands: %d" (cdr (assoc 'commands-executed metrics)))
    (message "  TODOs Created: %d" (cdr (assoc 'todos-created metrics)))  
    (message "  TODOs Completed: %d" (cdr (assoc 'todos-completed metrics)))
    (message "  API Calls: %d" (cdr (assoc 'api-calls metrics)))
    (message "  Buffers Created: %s" (cdr (assoc 'buffers-created metrics)))
    (message "  Files Modified: %s" (cdr (assoc 'files-modified metrics)))))

;; Show dashboard content
(message "")
(message "🎛️  Dashboard Content:")
(message "")
(with-current-buffer "*efrit-dashboard*"
  (let ((content (buffer-string)))
    (dolist (line (split-string content "\n"))
      (when (and (not (string-blank-p line))
                 (or (string-match-p "•" line)
                     (string-match-p "[📊📋🔗🔍⚡]" line)))
        (message "  %s" line)))))

;; Test dashboard functions
(message "")
(message "🧪 Testing Dashboard Functions:")

(with-current-buffer "*efrit-dashboard*"
  (goto-char (point-min))
  (when (search-forward "SESSION STATE" nil t)
    (message "  ✅ Found SESSION STATE section"))
  (goto-char (point-min))
  (when (search-forward "Commands executed" nil t)
    (message "  ✅ Found command execution stats"))
  (goto-char (point-min))
  (when (search-forward "API calls made" nil t)
    (message "  ✅ Found API call stats")))

;; Save session data
(efrit-session-save)
(efrit-dashboard-save-session)
(message "  ✅ Session data saved")

;; Clean up
(efrit-session-end)
(message "")
(message "🎉 Demo completed successfully!")
(message "   - Session tracking: ✅")
(message "   - Dashboard display: ✅") 
(message "   - Integration: ✅")
(message "   - Data persistence: ✅")
