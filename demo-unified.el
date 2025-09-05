;;; demo-unified.el --- Demonstration of unified command interface -*- lexical-binding: t -*-

;; This demonstrates the unified command interface that follows
;; the zero client-side intelligence principle

(add-to-list 'load-path (expand-file-name "lisp" default-directory))
(require 'efrit)

(message "=== Efrit Unified Command Interface Demo ===")
(message "")
(message "Following the zero client-side intelligence principle:")
(message "Claude decides everything, including execution mode.")
(message "")

(message "Available commands:")
(message "  C-c C-e d  - efrit-do (explicit sync execution)")
(message "  C-c C-e D  - efrit-do-async (explicit async execution)")
(message "  C-c C-e u  - efrit-unified-do (Claude decides mode)")
(message "  C-c C-e S  - efrit-unified-status (show execution status)")
(message "")

(message "How it works:")
(message "1. You enter a natural language command")
(message "2. Claude analyzes the command")
(message "3. Claude decides if it needs sync or async execution")
(message "4. The appropriate mode is used automatically")
(message "")

(message "Current implementation:")
(message "- Defaults to sync mode (simpler, immediate feedback)")
(message "- Future: Claude will indicate when async is needed")
(message "- You can always override with C-c C-e d or D")
(message "")

(message "Why this is better:")
(message "- No client-side guessing or heuristics")
(message "- Claude has full context to make the decision")
(message "- Predictable: Claude tells you what it's doing")
(message "- Extensible: Claude can add new modes as needed")
(message "")

(message "Example session:")
(message "  You: C-c C-e u")
(message "  You: fetch weather for Seattle and format it")
(message "  Claude: [Decides this needs multiple steps, uses async]")
(message "  Mode line: Shows async progress")
(message "  Result: Formatted weather in new buffer")
(message "")

(message "Customization:")
(message "- efrit-unified-default-mode")
(message "  - 'ask-claude (future: Claude decides)")
(message "  - 'sync (always sync unless overridden)")
(message "  - 'async (always async unless overridden)")

(provide 'demo-unified)
;;; demo-unified.el ends here