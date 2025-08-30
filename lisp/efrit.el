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

;; Keep this file side-effect free for use-package/straight users.
;; Do not modify load-path or eagerly require heavy subsystems here.

;; Tests are not loaded by default, but available when needed
;; (require 'efrit-tests)
;; (require 'efrit-use-case-tests)
;; (require 'efrit-integration-tests)

;; Make external API accessible with clear naming
(defalias 'efrit-start 'efrit-chat
  "Start a new Efrit chat session (alias for efrit-chat).")

;; Global keymap for Efrit (not bound by default)
(defvar efrit-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") 'efrit-chat)    ; 'c' for chat (classic)
    (define-key map (kbd "s") 'efrit-streamlined-send) ; 's' for streamlined chat
    (define-key map (kbd "e") 'efrit)         ; 'e' for command interface  
    (define-key map (kbd "a") 'efrit-agent-run)
    (define-key map (kbd "o") 'efrit-show-output)
    (define-key map (kbd "d") 'efrit-do)      ; 'd' for do/execute
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



;; For package system (lazy loading)
;;;###autoload
(autoload 'efrit-chat "efrit-chat" "Start a new Efrit chat session" t)

;;;###autoload
(autoload 'efrit "efrit-command" "Open Efrit command interface" t)

;;;###autoload
(autoload 'efrit-show-output "efrit-command" "Show Efrit output buffer" t)

;;;###autoload
(autoload 'efrit-agent-run "efrit-agent" "Run the agent loop with a request" t)

;;;###autoload
(autoload 'efrit-do "efrit-do" "Execute natural language command in Emacs" t)

;;;###autoload
(autoload 'efrit-streamlined-send "efrit-chat-streamlined" "Send message via streamlined chat" t)

;;;###autoload
(autoload 'efrit-remote-queue-start "efrit-remote-queue" "Start the remote queue system" t)

;;;###autoload
(autoload 'efrit-remote-queue-stop "efrit-remote-queue" "Stop the remote queue system" t)

;;;###autoload
(autoload 'efrit-remote-queue-status "efrit-remote-queue" "Show remote queue status" t)

;; Keep load clean: avoid runtime mutation of interactive forms or warnings here.

;; Initialize on load
(provide 'efrit)
;;; efrit.el ends here
