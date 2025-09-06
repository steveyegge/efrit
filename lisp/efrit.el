;;; efrit.el --- LLM conversational assistant for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Maintainer: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.2.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai, assistant, claude
;; URL: https://github.com/steveyegge/efrit
;; Homepage: https://github.com/steveyegge/efrit

;; This file is not part of GNU Emacs.

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;; Commentary:

;; Efrit is an AI-powered autonomous development platform for Emacs.
;; It provides both user-friendly interfaces and agent-to-agent communication
;; channels, enabling AI systems to enhance Efrit's functionality autonomously.
;;
;; Key features:
;; - Multi-turn conversations with Claude for users
;; - File-based remote queue for AI agent communication  
;; - Natural language command execution (efrit-do)
;; - Autonomous development capabilities for AI agents
;; - Self-enhancement: AI agents can modify Efrit's source code
;; - Support for any AI coding agent (Claude Code, GitHub Copilot, etc.)
;; - Direct Elisp evaluation through integrated agent architecture
;; - Zero client-side intelligence: all AI processing in Claude

;;; Code:

;; Ensure the efrit directory is in load-path
(eval-and-compile
  (let ((efrit-dir (file-name-directory (or load-file-name buffer-file-name default-directory))))
    (add-to-list 'load-path efrit-dir)
    (message "Added to load-path: %s" efrit-dir)))

;; Load the core implementation in dependency order
(condition-case err
    (progn
      ;; Load dependencies first
      (require 'json)
      (require 'url)
      (require 'auth-source)
      
      ;; Load core modules in dependency order
      (require 'efrit-config)     ; Configuration management - must be first
      (require 'efrit-common)     ; Common utilities
      (require 'efrit-log)        ; Logging system
      (require 'efrit-protocol)   ; Shared protocols (fixes circular deps)
      (require 'efrit-tools)
      (require 'efrit-context)    ; Context management utilities
      (require 'efrit-multi-turn) ; Multi-turn conversation management
      (require 'efrit-chat)       ; Depends on efrit-tools, efrit-multi-turn
      (require 'efrit-chat-streamlined) ; New streamlined chat system
      (require 'efrit-do)         ; Depends on efrit-chat, efrit-tools
      (require 'efrit-async)      ; Async infrastructure for non-blocking commands
      (require 'efrit-unified)    ; Unified command interface
      (require 'efrit-remote-queue) ; File-based remote queue system
      
      (message "Efrit modules loaded successfully"))
  (error 
   (message "Error loading efrit modules: %s" (error-message-string err))
   (message "Load-path: %s" load-path)
   (message "Available features: %s" features)))

;; Tests are not loaded by default, but available when needed
;; (require 'efrit-tests)
;; (require 'efrit-use-case-tests)
;; (require 'efrit-integration-tests)

;; Make external API accessible with clear naming
(defalias 'efrit-start 'efrit-chat
  "Start a new Efrit chat session (alias for efrit-chat).")

;; Global keymap for Efrit
(defvar efrit-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") 'efrit-chat)    ; 'c' for chat (classic)
    (define-key map (kbd "s") 'efrit-streamlined-send) ; 's' for streamlined chat
    ;; (define-key map (kbd "e") 'efrit)       ; removed - efrit-command no longer exists  
    ;; (define-key map (kbd "a") 'efrit-agent-run) ; removed
    ;; (define-key map (kbd "o") 'efrit-show-output) ; removed
    (define-key map (kbd "d") 'efrit-do)      ; 'd' for do/execute (sync)
    (define-key map (kbd "D") 'efrit-do-async) ; 'D' for async do/execute
    (define-key map (kbd "u") 'efrit-unified-do) ; 'u' for unified (Claude decides)
    (define-key map (kbd "q") 'efrit-remote-queue-start) ; 'q' for queue
    (define-key map (kbd "Q") 'efrit-remote-queue-status) ; 'Q' for queue status
    (define-key map (kbd "A") 'efrit-async-status) ; 'A' for async status
    (define-key map (kbd "S") 'efrit-unified-status) ; 'S' for unified status
    map)
  "Keymap for Efrit commands.")

;; Set up the global keybinding
(global-set-key (kbd "C-c C-e") efrit-keymap)

;; Initialize efrit
(defun efrit-initialize ()
  "Initialize Efrit."
  (interactive)
  (message "Efrit initialized and ready to use"))



;; For package system (lazy loading)
;;;###autoload
(autoload 'efrit-chat "efrit-chat" "Start a new Efrit chat session" t)

;; efrit-command no longer exists - removed autoloads

;; efrit-agent no longer exists - removed autoload

;;;###autoload
(autoload 'efrit-do "efrit-do" "Execute natural language command in Emacs" t)

;;;###autoload
(autoload 'efrit-do-async "efrit-do" "Execute natural language command in Emacs asynchronously" t)

;;;###autoload
(autoload 'efrit-unified-do "efrit-unified" "Execute command with Claude-determined mode" t)

;;;###autoload
(autoload 'efrit-unified-status "efrit-unified" "Show Efrit execution status" t)

;;;###autoload
(autoload 'efrit-streamlined-send "efrit-chat-streamlined" "Send message via streamlined chat" t)

;;;###autoload
(autoload 'efrit-remote-queue-start "efrit-remote-queue" "Start the remote queue system" t)

;;;###autoload
(autoload 'efrit-remote-queue-stop "efrit-remote-queue" "Stop the remote queue system" t)

;;;###autoload
(autoload 'efrit-remote-queue-status "efrit-remote-queue" "Show remote queue status" t)

;;;###autoload
(autoload 'efrit-async-status "efrit-async" "Show status of async operations" t)

;;;###autoload
(autoload 'efrit-async-show-log "efrit-async" "Show work log for a session" t)

;;;###autoload
(autoload 'efrit-async-cancel "efrit-async" "Cancel an async operation" t)

;; Verify that key functions are available
(unless (and (fboundp 'efrit-chat) (fboundp 'efrit-do))
  (message "Warning: Some efrit functions may not be available. Check for errors above."))

;; Explicitly make commands available when loading directly
(when (and (fboundp 'efrit-do) (not (commandp 'efrit-do)))
  (put 'efrit-do 'interactive-form '(interactive 
                                     (list (read-string "Command: " nil 'efrit-do-history)))))

(when (and (fboundp 'efrit-chat) (not (commandp 'efrit-chat)))
  (put 'efrit-chat 'interactive-form '(interactive)))

(when (and (fboundp 'efrit-do-to-chat) (not (commandp 'efrit-do-to-chat)))
  (put 'efrit-do-to-chat 'interactive-form '(interactive "P")))

(when (and (fboundp 'efrit-do-show-context) (not (commandp 'efrit-do-show-context)))
  (put 'efrit-do-show-context 'interactive-form '(interactive)))

;; Initialize on load
(provide 'efrit)
;;; efrit.el ends here