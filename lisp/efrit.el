;;; efrit.el --- LLM conversational assistant for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Maintainer: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.4.1
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

;; Add subdirectories to load-path for modular organization
;; This is done at load time to ensure subdirectories are accessible
;; for both package.el and manual installations without additional configuration.
(let ((lisp-dir (file-name-directory (or load-file-name buffer-file-name))))
  (dolist (subdir '("core" "interfaces" "support" "dev" "tools"))
    (let ((full-path (expand-file-name subdir lisp-dir)))
      (when (file-directory-p full-path)
        (add-to-list 'load-path full-path)))))

;; Keep load clean: avoid eagerly requiring heavy subsystems.
;; Load path setup above is necessary for subdirectory access.

;; Tests are not loaded by default, but available when needed
;; (require 'efrit-tests)
;; (require 'efrit-use-case-tests)
;; (require 'efrit-integration-tests)

;; Make external API accessible with clear naming
(defalias 'efrit-start 'efrit-chat
  "Start a new Efrit chat session (alias for efrit-chat).")

;;;###autoload
(defun efrit ()
  "Open or switch to the Efrit REPL session buffer.

This is the main entry point for interactive Efrit usage.
The agent buffer provides a conversation-style REPL where you can:
- Issue natural language commands
- See real-time progress and tool execution
- Interact with Claude in a persistent session
- Have your context maintained across multiple commands

Keybindings in the agent buffer:
  RET       - Send input (in the input area) / toggle tool call (in the conversation)
  TAB       - Next section
  Shift-TAB - Previous section
  M-n/M-p   - Next/previous tool call
  C-c C-k   - Cancel current session
  C-c ?     - Show help

Use M-x efrit-help for more information."
  (interactive)
  (require 'efrit-agent)
  (call-interactively #'efrit-agent))

;; Define efrit-mode-map early so it's always available
(defvar efrit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'efrit-send-buffer-message)
    (define-key map (kbd "S-<return>") 'efrit-insert-newline)
    (define-key map (kbd "C-c C-c") 'efrit-send-buffer-message)
    map)
  "Keymap for Efrit mode.")

;; Global keymap for Efrit (not bound by default)
(defvar efrit-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") 'efrit-chat)    ; 'c' for chat (classic)
    (define-key map (kbd "s") 'efrit-streamlined-send) ; 's' for streamlined chat
    (define-key map (kbd "a") 'efrit-agent)   ; 'a' for agent buffer
    (define-key map (kbd "d") 'efrit-do)      ; 'd' for do/execute (async by default)
    (define-key map (kbd "w") 'efrit-do-silently) ; 'w' for background work
    (define-key map (kbd "p") 'efrit-do-show-progress) ; 'p' for progress buffer
    (define-key map (kbd "D") 'efrit-do-sync) ; 'D' for sync do/execute (legacy)
    (define-key map (kbd "q") 'efrit-remote-queue-start) ; 'q' for queue
    (define-key map (kbd "Q") 'efrit-remote-queue-status) ; 'Q' for queue status
    map)
  "Keymap for Efrit commands.")

;; Optional global keybinding (off by default for use-package ergonomics)
(defcustom efrit-enable-global-keymap nil
  "If non-nil, bind `C-c C-e` to `efrit-keymap` at load time."
  :type 'boolean
  :group 'efrit)

(defun efrit-setup-keybindings ()
  "Bind the global Efrit key prefix `C-c C-e`."
  (interactive)
  (global-set-key (kbd "C-c C-e") efrit-keymap))

(when efrit-enable-global-keymap
  (efrit-setup-keybindings))

;; Initialize efrit
(defun efrit-initialize ()
  "Initialize Efrit."
  (interactive)
  (message "Efrit initialized and ready to use"))

;;;###autoload
(defun efrit-help ()
  "Display help information about Efrit modes and commands."
  (interactive)
  (with-current-buffer (get-buffer-create "*efrit-help*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "Efrit - AI Coding Assistant for Emacs\n")
      (insert "======================================\n\n")

      (insert "Main Commands:\n\n")
      (insert "  M-x efrit              - Open REPL session buffer (PRIMARY INTERFACE)\n")
      (insert "                           Persistent conversation with Claude\n")
      (insert "                           Recommended for all interactive use\n\n")
      (insert "  M-x efrit-chat         - Start new chat session (alternative)\n")
      (insert "                           Single-window chat mode\n\n")
      (insert "  M-x efrit-do-sync      - Execute command synchronously (scripting)\n")
      (insert "                           For shell scripts and automation\n\n")

      (insert "Utility Commands:\n\n")
      (insert "  M-x efrit-doctor       - Check configuration and health\n")
      (insert "  M-x efrit-show-session - View active session details\n")
      (insert "  M-x efrit-show-queue   - View async command queue\n")
      (insert "  M-x efrit-show-errors  - View all errors and warnings\n")
      (insert "  M-x efrit-log-show     - View full debug log\n\n")

      (insert "Optional Global Keymap:\n\n")
      (insert "  Set `efrit-enable-global-keymap` to t, or run:\n")
      (insert "  M-x efrit-setup-keybindings\n\n")
      (insert "  Then use C-c C-e prefix:\n")
      (insert "    C-c C-e c  - efrit-chat (multi-turn chat)\n")
      (insert "    C-c C-e d  - efrit-do (async execution, internal API)\n")
      (insert "    C-c C-e D  - efrit-do-sync (sync execution, scripting)\n")
      (insert "    C-c C-e p  - efrit-do-show-progress\n")
      (insert "    C-c C-e q  - Start remote queue\n")
      (insert "    C-c C-e Q  - Queue status\n\n")

      (insert "Configuration:\n\n")
      (insert "  Set ANTHROPIC_API_KEY environment variable, or:\n")
      (insert "  (setq efrit-api-key 'ANTHROPIC_API_KEY)  ; env var\n")
      (insert "  (setq efrit-default-model \"claude-sonnet-4-5-20250929\")\n\n")

      (insert "For more information, see README.md and ARCHITECTURE.md\n")
      (goto-char (point-min))
      (view-mode))
    (display-buffer (current-buffer))))

;; For package system (lazy loading)
;;;###autoload
(autoload 'efrit-chat "efrit-chat" "Start a new Efrit chat session" t)

;;;###autoload
(autoload 'efrit-streamlined-send "efrit-chat" "Send message via streamlined chat" t)

;;;###autoload
(autoload 'efrit-do "efrit-do" "Execute natural language command in Emacs" t)

;;;###autoload
(autoload 'efrit-do-async "efrit-do" "Execute natural language command in Emacs asynchronously" t)

;;;###autoload
(autoload 'efrit-agent "efrit-agent" "Open or switch to the Efrit agent buffer" t)

;;;###autoload
(autoload 'efrit-remote-queue-start "efrit-remote-queue" "Start the remote queue system" t)

;;;###autoload
(autoload 'efrit-remote-queue-stop "efrit-remote-queue" "Stop the remote queue system" t)

;;;###autoload
(autoload 'efrit-remote-queue-status "efrit-remote-queue" "Show remote queue status" t)

;; Keep load clean: avoid runtime mutation of interactive forms or warnings here.

;;; Development utilities

(require 'cl-lib)  ; For cl-some

;; Forward declarations for lazily-loaded functions
(declare-function efrit-common-get-api-key "efrit-common")

(defun efrit-version ()
  "Display Efrit version and perform health checks.
Shows version, installation status, and basic connectivity."
  (interactive)
  (require 'efrit-config)
  (require 'efrit-common)
  (let* ((version "0.4.1")
         (api-key (condition-case nil
                      (efrit-common-get-api-key)
                    (error nil)))
         (has-api-key (not (null api-key)))
         (lisp-dir (file-name-directory (locate-library "efrit")))
         (test-dir (expand-file-name "../test" lisp-dir))
         (has-tests (file-directory-p test-dir))
         (mcp-dir (expand-file-name "../mcp" lisp-dir))
         (has-mcp (file-directory-p mcp-dir)))
    (with-current-buffer (get-buffer-create "*Efrit Version*")
      (erase-buffer)
      (insert "Efrit AI Coding Assistant\n")
      (insert "==========================\n\n")
      (insert (format "Version: %s\n" version))
      (insert (format "Installation: %s\n\n" (or lisp-dir "unknown")))
      (insert "Health Check:\n")
      (insert (format "  [%s] Claude API key configured\n"
                      (if has-api-key "✓" "✗")))
      (insert (format "  [%s] Test suite available\n"
                      (if has-tests "✓" "✗")))
      (insert (format "  [%s] MCP server available\n"
                      (if has-mcp "✓" "✗")))
      (insert "\n")
      (when (not has-api-key)
        (insert "⚠ No API key found. Set ANTHROPIC_API_KEY or configure via efrit-config.\n"))
      (insert "\nQuick Start:\n")
      (insert "  M-x efrit-chat      - Start interactive chat\n")
      (insert "  M-x efrit-do        - Execute natural language command\n")
      (insert "  M-x efrit-run-tests - Run test suite\n")
      (insert "  M-x efrit-doctor    - Verify configuration (C-u: live API check)\n")
      (display-buffer (current-buffer)))))

(defun efrit-run-tests ()
  "Run the Efrit test suite interactively.
Executes all ERT tests and displays results in a buffer."
  (interactive)
  (let* ((lisp-dir (file-name-directory (locate-library "efrit")))
         (test-dir (expand-file-name "../test" lisp-dir))
         (test-files (directory-files test-dir t "^test-.*\\.el$")))
    (if (null test-files)
        (message "No test files found in %s" test-dir)
      (message "Loading %d test files..." (length test-files))
      (dolist (test-file test-files)
        (load test-file nil t))
      (message "Running tests...")
      (ert-run-tests-interactively t))))

;; efrit-doctor lives in lisp/support/, which package.el does not scan
;; for cookies; this cookie makes it reachable from efrit-autoloads,
;; and the plain autoload covers a bare (require 'efrit).
;;;###autoload
(autoload 'efrit-doctor "efrit-doctor"
  "Verify every layer of the efrit configuration and suggest fixes." t)
;;;###autoload
(autoload 'efrit-menu "efrit-menu" "Efrit status and command menu." t)
;;;###autoload
(autoload 'efrit-sandbox "efrit-sandbox-ui" "Show and edit sandbox grants for the project." t)
;;;###autoload
(autoload 'efrit-reload "efrit-reload"
  "Reload every loaded efrit library from source, dependencies first." t)
;;;###autoload
(autoload 'efrit-instructions-show "efrit-instructions"
  "Show which AGENTS.md / CLAUDE.md files apply and what the model receives." t)
;;;###autoload
(autoload 'efrit-select-model "efrit-models"
  "Choose efrit-default-model from what the endpoint offers." t)

;; Initialize on load
(provide 'efrit)
;;; efrit.el ends here
