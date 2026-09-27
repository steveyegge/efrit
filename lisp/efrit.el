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
(autoload 'efrit-sandbox "efrit-permissions-ui" "Show and edit sandbox grants for the project." t)
;;;###autoload
(autoload 'efrit-review-package "efrit-package-review"
  "Review an installed package with efrit and show the report." t)
;;;###autoload
(autoload 'efrit-package-review-mode "efrit-package-review"
  "Have efrit review packages before package-review asks." t)
;;;###autoload
(autoload 'efrit-review-all-packages "efrit-package-review-ui"
  "Review every installed package with efrit; collect the verdicts in one buffer." t)
;;;###autoload
(autoload 'efrit-testdrive "efrit-testdrive"
  "Walk through efrit's live test plan interactively." t)
;;;###autoload
(autoload 'efrit-package-review-probe "efrit-package-review"
  "Send shrinking variants of a package review request to find what the API refuses." t)
;;;###autoload
(autoload 'efrit-permissions "efrit-permissions-ui"
  "Edit grants, review policy and limits for every known project." t)
;;;###autoload
(autoload 'efrit-register-tool "efrit-tool-registry"
  "Offer the model a tool implemented by another package.")
;;;###autoload
(autoload 'efrit-list-registered-tools "efrit-tool-registry"
  "Show the tools other packages have registered with efrit." t)
;;;###autoload
(autoload 'efrit-documents-ensure-tools "efrit-documents"
  "Register the document tools (doc_fetch, doc_search, doc_sources) with efrit." t)
;;;###autoload
(autoload 'efrit-documents-list-sources "efrit-documents"
  "Say which document sources are registered and whether they work." t)
;;;###autoload
(autoload 'efrit-documents-gdrive-check "efrit-documents-gdrive"
  "Check that Google Drive is reachable as a document source." t)
;;;###autoload
(autoload 'efrit-documents-gcalendar-check "efrit-documents-gcalendar"
  "Check that Google Calendar is reachable as a related-documents provider." t)
;;;###autoload
(autoload 'efrit-documents-jira-check "efrit-documents-jira"
  "Check that Jira is reachable as a document source (through jira.el)." t)
;;;###autoload
(autoload 'efrit-documents-confluence-check "efrit-documents-confluence"
  "Check that each configured Confluence site is reachable as a document source." t)
;;;###autoload
(autoload 'efrit-auth-reauthorize "efrit-auth"
  "Run the OAuth2 consent flow again for an auth-source host." t)
;;;###autoload
(autoload 'efrit-auth-check "efrit-auth"
  "Describe the credentials auth-source holds for a host." t)
;;;###autoload
(autoload 'efrit-prompts-manage "efrit-prompts"
  "List and edit efrit's two-part prompts for analyses over many items." t)
;;;###autoload
(autoload 'efrit-prompts-edit "efrit-prompts"
  "Edit one of efrit's two-part prompts, or write a new one." t)
;;;###autoload
(autoload 'efrit-prompts-read "efrit-prompts"
  "Ask the user for a two-part prompt; returns (ITEM . SUMMARY).")
;;;###autoload
(autoload 'efrit-gnus-analyze "efrit-gnus"
  "Ask efrit about the selected articles in a Gnus summary." t)
;;;###autoload
(autoload 'efrit-gnus-analyze-unread "efrit-gnus"
  "Ask efrit about every unread article of a Gnus group." t)
;;;###autoload
(autoload 'efrit-gnus-analyze-search "efrit-gnus"
  "Ask efrit about the results of a Gnus search." t)
;;;###autoload
(autoload 'efrit-gnus-ensure-tools "efrit-gnus"
  "Register the Gnus tools (gnus_groups, gnus_search, gnus_articles) with efrit." t)
;;;###autoload
(autoload 'efrit-submit "efrit-agent-input"
  "Start a REPL turn from Lisp with a prepared prompt.")
(autoload 'efrit-testdrive-tour "efrit-testdrive"
  "Walk through what only eyes can check in the agent buffer." t)
(autoload 'efrit-agent-menu "efrit-agent-menu"
  "The agent buffer's command menu." t)
(autoload 'efrit-agent-repl-session "efrit-agent-input"
  "The agent buffer's REPL session, created if needed.")
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
