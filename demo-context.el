;;; demo-context.el --- Demonstration of context utilities -*- lexical-binding: t -*-

;; This demonstrates how context utilities enhance multi-step execution
;; by providing smart work log compression and state tracking

(add-to-list 'load-path (expand-file-name "lisp" default-directory))
(require 'efrit)

(message "=== Efrit Context Utilities Demo ===")
(message "")
(message "Context utilities provide intelligent session memory management:")
(message "")

(message "1. Smart Work Log Compression:")
(message "   - Automatically compresses execution history")
(message "   - Preserves essential information while reducing tokens")
(message "   - Three levels: minimal, smart (default), full")
(message "")

(message "2. State Tracking:")
(message "   - Captures buffer state, point, mark, and window config")
(message "   - Tracks modifications and context changes")
(message "   - Can restore previous states if needed")
(message "")

(message "3. Context-Aware Logging:")
(message "   - Each step includes rich context information")
(message "   - Buffer name, position, and modification status")
(message "   - Helps Claude understand the current state")
(message "")

(message "Example compressed work log sent to Claude:")
(message "{")
(message "  \"total_steps\": 5,")
(message "  \"recent\": [")
(message "    {\"code\": \"(shell_exec ...)\", \"result\": \"weather data\"},")
(message "    {\"code\": \"(buffer_create ...)\", \"result\": \"buffer:*Weather*\"}")
(message "  ],")
(message "  \"buffer_modified\": true")
(message "}")
(message "")

(message "Benefits:")
(message "- Efficient token usage in Claude API calls")
(message "- Better context preservation across steps")
(message "- Automatic memory management (50 entry limit)")
(message "- Smart truncation of long results")
(message "")

(message "Customization variables:")
(message "- efrit-context-max-result-length (default: 200)")
(message "- efrit-context-compression-level (default: 'smart)")
(message "- efrit-context-include-buffer-state (default: t)")
(message "")

(message "The context system enables Claude to:")
(message "1. Remember what was done in previous steps")
(message "2. Understand the current Emacs state")
(message "3. Make intelligent decisions about next actions")
(message "4. Complete complex multi-step workflows reliably")

(provide 'demo-context)
;;; demo-context.el ends here