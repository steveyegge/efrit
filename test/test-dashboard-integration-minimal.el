;;; test-dashboard-integration-minimal.el --- Minimal dashboard integration test -*- lexical-binding: t -*-

;; Simple integration test for dashboard functionality

;;; Code:

(add-to-list 'load-path "../lisp")

(require 'efrit-dashboard)
(require 'efrit-session-tracker)

(message "🧪 Testing Dashboard Integration (Minimal)")

;; Test 1: Session tracker basic functionality
(efrit-session-start)
(efrit-session-track-command "test integration")
(let ((count (efrit-session-get-metric 'commands-executed)))
  (if (>= count 1)
      (message "✅ Session tracking: %d commands" count)
    (message "❌ Session tracking failed")))

;; Test 2: Dashboard creation
(efrit-dashboard)
(if (get-buffer "*efrit-dashboard*")
    (progn
      (with-current-buffer "*efrit-dashboard*"
        (if (string-match-p "SESSION STATE" (buffer-string))
            (message "✅ Dashboard content generated")
          (message "❌ Dashboard content missing")))
      (message "✅ Dashboard buffer created"))
  (message "❌ Dashboard buffer creation failed"))

;; Test 3: Integration between session and dashboard
(efrit-dashboard-refresh)
(with-current-buffer "*efrit-dashboard*"
  (let ((content (buffer-string)))
    (if (string-match-p "Commands executed: [1-9]" content)
        (message "✅ Dashboard shows session data")
      (message "❌ Dashboard integration failed"))))

(efrit-session-end)
(message "✅ Minimal dashboard integration test completed")

;;; test-dashboard-integration-minimal.el ends here
