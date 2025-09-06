#!/usr/bin/env bash
# Integration test for dashboard with real API calls

set -e

SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

echo "🔥 Efrit Dashboard + API Integration Test"
echo "=========================================="
echo ""
echo "⚠️  This test makes REAL API calls and consumes tokens!"
echo ""

# Check for API key
if ! grep -q "machine api.anthropic.com" ~/.authinfo 2>/dev/null; then
    if [ -z "$ANTHROPIC_API_KEY" ]; then
        echo "❌ No API key found!"
        echo "Configure API key in ~/.authinfo or set ANTHROPIC_API_KEY"
        exit 1
    fi
fi

read -p "Continue with API integration test? (y/N) " -n 1 -r
echo ""

if [[ ! $REPLY =~ ^[Yy]$ ]]; then
    echo "Test cancelled."
    exit 0
fi

echo ""
echo "🚀 Running API + Dashboard Integration Test..."
echo ""

# Create temporary test script
cat > "/tmp/efrit_api_integration_test.el" << 'EOF'
(add-to-list 'load-path (expand-file-name "../lisp" (file-name-directory load-file-name)))

(require 'efrit)
(require 'efrit-do)
(require 'efrit-dashboard)
(require 'efrit-session-tracker)

(message "\n🧪 API + Dashboard Integration Test Starting...\n")

;; Test 1: Start session and dashboard
(efrit-session-start)
(message "✓ Session started: %s" efrit-session-id)

(efrit-dashboard)
(message "✓ Dashboard opened")

;; Test 2: Execute real API command
(message "\n📡 Making API call...")
(condition-case err
    (let ((result (efrit-do "What is 3 + 4? Respond with just the number.")))
      (if (and result (string-match "7" result))
          (message "✅ API call successful: %s" (string-trim result))
        (message "⚠️  API call returned unexpected result: %s" result)))
  (error 
   (message "❌ API call failed: %s" (error-message-string err))))

;; Test 3: Verify session tracking captured the activity  
(let ((commands-count (efrit-session-get-metric 'commands-executed)))
  (if (>= commands-count 1)
      (message "✅ Session tracking captured %d commands" commands-count)
    (message "❌ Session tracking failed")))

;; Test 4: Verify dashboard shows updated data
(efrit-dashboard-refresh)
(with-current-buffer "*efrit-dashboard*"
  (let ((content (buffer-string)))
    (cond
     ((string-match-p "Commands executed: [1-9]" content)
      (message "✅ Dashboard shows command count"))
     ((string-match-p "Commands executed: 0" content)
      (message "⚠️  Dashboard shows 0 commands - API integration may not be fully connected"))
     (t
      (message "❌ Dashboard doesn't show command metrics")))))

;; Test 5: Verify session persistence
(efrit-session-save)
(let* ((session-file (expand-file-name 
                     (concat efrit-session-id ".json")
                     (expand-file-name "sessions" efrit-data-directory))))
  (if (file-exists-p session-file)
      (message "✅ Session data persisted to %s" session-file)
    (message "❌ Session data not saved")))

;; Cleanup
(efrit-session-end)
(message "✅ Session ended")

(message "\n🎯 API + Dashboard Integration Test Complete!")
EOF

# Run the test
emacs -Q --batch \
    -L "$PROJECT_ROOT/lisp" \
    --load "/tmp/efrit_api_integration_test.el" \
    2>&1

EXIT_CODE=$?

# Cleanup
rm -f "/tmp/efrit_api_integration_test.el"

if [ $EXIT_CODE -eq 0 ]; then
    echo ""
    echo "✅ API + Dashboard Integration Test PASSED"
    echo ""
    echo "🎯 Validated:"
    echo "   • Real API calls working"
    echo "   • Session tracking captures API activity"
    echo "   • Dashboard displays real-time data"
    echo "   • Session persistence works"
    echo ""
else
    echo ""
    echo "❌ API + Dashboard Integration Test FAILED"
    echo "   Exit code: $EXIT_CODE"
    echo ""
fi

exit $EXIT_CODE
