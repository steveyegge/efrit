;;; efrit.el --- LLM conversational assistant for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Maintainer: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.4.0
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
  (dolist (subdir '("core" "interfaces" "support" "dev"))
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
    (define-key map (kbd "d") 'efrit-do)      ; 'd' for do/execute (sync)
    (define-key map (kbd "D") 'efrit-do-async) ; 'D' for async do/execute
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
      (insert "  M-x efrit-chat         - Interactive multi-turn chat mode\n")
      (insert "                           Use for back-and-forth conversations\n\n")
      (insert "  M-x efrit-do           - Execute single command synchronously\n")
      (insert "                           Blocks until complete, shows result\n\n")
      (insert "  M-x efrit-do-async     - Execute command asynchronously\n")
      (insert "                           Non-blocking, queues multiple commands\n\n")

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
      (insert "    C-c C-e c  - efrit-chat\n")
      (insert "    C-c C-e d  - efrit-do\n")
      (insert "    C-c C-e D  - efrit-do-async\n")
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
  (let* ((version "0.4.0")
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
      (insert "  M-x efrit-doctor    - Validate installation\n")
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

(defun efrit-doctor ()
  "Validate Efrit installation and configuration.
Performs comprehensive checks and suggests fixes for issues."
  (interactive)
  (require 'efrit-config)
  (require 'efrit-common)
  (let ((issues nil)
        (warnings nil))
    (with-current-buffer (get-buffer-create "*Efrit Doctor*")
      (erase-buffer)
      (insert "Efrit Installation Doctor\n")
      (insert "=========================\n\n")

      ;; Check API key
      (insert "Checking API configuration...\n")
      (let ((api-key (condition-case nil
                         (efrit-common-get-api-key)
                       (error nil))))
        (if api-key
            (insert "  ✓ Claude API key configured\n")
          (insert "  ✗ No API key found\n")
          (push "No Claude API key configured" issues)))

      ;; Check required files
      (insert "\nChecking installation files...\n")
      (let ((required-modules '("efrit-config" "efrit-tools" "efrit-chat"
                                "efrit-do" "efrit-remote-queue")))
        (dolist (module required-modules)
          (if (locate-library module)
              (insert (format "  ✓ %s.el found\n" module))
            (insert (format "  ✗ %s.el missing\n" module))
            (push (format "Missing module: %s" module) issues))))

      ;; Check test infrastructure
      (insert "\nChecking test infrastructure...\n")
      (let* ((lisp-dir (file-name-directory (locate-library "efrit")))
             (test-dir (expand-file-name "../test" lisp-dir)))
        (if (file-directory-p test-dir)
            (let ((test-count (length (directory-files test-dir nil "^test-.*\\.el$"))))
              (insert (format "  ✓ Test directory found (%d tests)\n" test-count)))
          (insert "  ⚠ Test directory not found\n")
          (push "Test directory not accessible" warnings)))

      ;; Check MCP server
      (insert "\nChecking MCP server...\n")
      (let* ((lisp-dir (file-name-directory (locate-library "efrit")))
             (mcp-dir (expand-file-name "../mcp" lisp-dir))
             (mcp-package (expand-file-name "package.json" mcp-dir)))
        (if (file-exists-p mcp-package)
            (insert "  ✓ MCP server found\n")
          (insert "  ⚠ MCP server not found\n")
          (push "MCP server not installed" warnings)))

      ;; Check Emacs version
      (insert "\nChecking Emacs version...\n")
      (if (version<= "28.1" emacs-version)
          (insert (format "  ✓ Emacs %s (>= 28.1 required)\n" emacs-version))
        (insert (format "  ✗ Emacs %s (28.1+ required)\n" emacs-version))
        (push (format "Emacs version too old: %s" emacs-version) issues))

      ;; Summary
      (insert "\n")
      (insert "=========================\n")
      (if (and (null issues) (null warnings))
          (insert "✓ All checks passed! Efrit is ready to use.\n")
        (when issues
          (insert (format "\n✗ Found %d critical issue(s):\n" (length issues)))
          (dolist (issue issues)
            (insert (format "  - %s\n" issue))))
        (when warnings
          (insert (format "\n⚠ Found %d warning(s):\n" (length warnings)))
          (dolist (warning warnings)
            (insert (format "  - %s\n" warning)))))

      ;; Remediation
      (when issues
        (insert "\nRemediation:\n")
        (when (member "No Claude API key configured" issues)
          (insert "  - Set ANTHROPIC_API_KEY environment variable\n")
          (insert "    or configure via (setq efrit-api-key \"sk-...\")\n"))
        (when (cl-some (lambda (s) (string-match "Missing module" s)) issues)
          (insert "  - Reinstall Efrit or check load-path\n"))
        (when (cl-some (lambda (s) (string-match "Emacs version" s)) issues)
          (insert "  - Upgrade to Emacs 28.1 or later\n")))

      (display-buffer (current-buffer)))))

;; Initialize on load
(provide 'efrit)
;;; efrit.el ends here
