;;; demo-async.el --- Demonstration of async efrit integration -*- lexical-binding: t -*-

;; Load the system
(add-to-list 'load-path (expand-file-name "lisp" default-directory))
(require 'efrit-log)
(require 'efrit-common)
(require 'efrit-config)
(require 'efrit-tools)
(require 'efrit-async)
(require 'efrit-do)

(message "=== Efrit Async Integration Demo ===")
(message "")

;; Demonstrate synchronous vs asynchronous commands
(message "Available commands:")
(message "  M-x efrit-do        - Synchronous execution (blocks)")
(message "  M-x efrit-do-async  - Asynchronous execution (non-blocking)")
(message "")

;; Show session management capabilities
(message "Session management features:")
(message "  M-x efrit-async-status     - Show current session status")
(message "  M-x efrit-async-show-log   - Show session work log")
(message "  M-x efrit-async-cancel     - Cancel active session")
(message "")

;; Example of programmatic async usage
(message "Programmatic usage example:")
(message "(efrit-async-execute-command \"your command\" #'callback-function)")
(message "")

(message "Try these test commands:")
(message "  show me the current time")
(message "  create a scratch buffer with hello world")
(message "  list all open buffers")
(message "")

(message "Session features:")
(message "- Progress display in mode line")
(message "- Work log compression for context management")
(message "- Queue management for concurrent commands") 
(message "- Session-based operation tracking")
(message "")

(message "Next steps (future sessions):")
(message "- Session 2: System prompt updates for session protocol")
(message "- Session 3: Context utilities (efrit-context.el)")
(message "- Session 4: Command consolidation (unified entry point)")

(provide 'demo-async)