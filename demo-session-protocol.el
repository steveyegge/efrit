;;; demo-session-protocol.el --- Demonstration of session protocol in action -*- lexical-binding: t -*-

;; This demonstrates how the session protocol enables multi-step
;; command execution with Claude maintaining context across API calls

(add-to-list 'load-path (expand-file-name "lisp" default-directory))
(require 'efrit)

(message "=== Efrit Session Protocol Demo ===")
(message "")
(message "The session protocol allows Claude to execute multi-step commands")
(message "by maintaining context across API calls. Each step can:")
(message "1. Gather information (Phase 1)")
(message "2. Execute actions (Phase 2)")
(message "3. Continue to next step or complete")
(message "")

(message "Example multi-step commands that now work:")
(message "")
(message "1. Weather Forecast with Formatting:")
(message "   M-x efrit-do-async")
(message "   Command: fetch the 7-day weather forecast for Seattle and format it nicely in a buffer")
(message "")
(message "   Step 1: Claude uses shell_exec to fetch weather data")
(message "   Step 2: Claude uses buffer_create to display formatted result")
(message "   Step 3: Claude uses session_complete to signal completion")
(message "")

(message "2. File Analysis and Report:")
(message "   M-x efrit-do-async")
(message "   Command: analyze all .el files in this directory and create a summary report")
(message "")
(message "   Step 1: Claude uses eval_sexp to list files")
(message "   Step 2: Claude reads each file to analyze")
(message "   Step 3: Claude creates formatted report buffer")
(message "   Step 4: Claude completes with summary")
(message "")

(message "3. Multi-stage Data Processing:")
(message "   M-x efrit-do-async")
(message "   Command: find all TODO comments in the project, organize by priority, and create a task list")
(message "")
(message "   Step 1: Search for TODO comments")
(message "   Step 2: Parse and categorize")
(message "   Step 3: Create organized buffer")
(message "   Step 4: Complete with statistics")
(message "")

(message "Key improvements over single-step execution:")
(message "- Claude can now plan and execute complex workflows")
(message "- Each step builds on previous results via work log")
(message "- No loss of context between API calls")
(message "- Clean separation of concerns (gather → act → continue)")
(message "")

(message "The work log maintains Claude's 'memory' across steps:")
(message "[[\"weather data\", \"(shell_exec 'curl...')\"],")
(message " [\"buffer created\", \"(buffer_create '*Weather*'...)\"],")
(message " [\"SESSION-COMPLETE: Displayed forecast\", \"(session_complete...)\"]]")
(message "")

(message "Try the weather example yourself:")
(message "1. M-x efrit-do-async")
(message "2. Enter: fetch weather forecast for your city and display it formatted")
(message "3. Watch Claude execute multiple steps to complete the task!")

(provide 'demo-session-protocol)
;;; demo-session-protocol.el ends here