;;; efrit-unified.el --- Unified command interface for Efrit -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.2.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; This module provides a unified command interface that lets Claude
;; decide whether to execute synchronously or asynchronously.
;; Following the zero client-side intelligence principle, all decisions
;; about execution mode are made by Claude, not by client heuristics.

;;; Code:

(require 'cl-lib)
(require 'efrit-log)
(require 'efrit-common)
(require 'efrit-do)
(require 'efrit-async)

;;; Customization

(defgroup efrit-unified nil
  "Unified command interface for Efrit."
  :group 'efrit
  :prefix "efrit-unified-")

(defcustom efrit-unified-default-mode 'ask-claude
  "Default execution mode for unified commands.
- ask-claude: Let Claude decide (recommended)
- sync: Always start with sync
- async: Always start with async"
  :type '(choice (const :tag "Ask Claude (Recommended)" ask-claude)
                 (const :tag "Default to Synchronous" sync)
                 (const :tag "Default to Asynchronous" async))
  :group 'efrit-unified)

;;; Mode Decision Protocol

(defun efrit-unified--ask-claude-for-mode (command)
  "Ask Claude whether COMMAND should run sync or async.
Returns \\='sync or \\='async based on Claude's analysis."
  ;; For now, default to sync until we implement the protocol
  ;; TODO: Add special tool for Claude to indicate execution mode
  (efrit-log 'debug "Mode decision for command: %s" command)
  'sync)

;;; Unified Command Interface

;;;###autoload
(defun efrit-unified-do (command &optional force-mode)
  "Execute natural language COMMAND with Claude-determined mode.
Claude analyzes the command and decides whether to execute it
synchronously or asynchronously. With prefix arg, prompts for
FORCE-MODE to override Claude's decision."
  (interactive 
   (list (read-string "Efrit command: " nil 'efrit-do-history)
         (when current-prefix-arg
           (intern (completing-read "Force mode: " 
                                   '("sync" "async") 
                                   nil t)))))
  
  ;; Add to history
  (when (bound-and-true-p efrit-do-history)
    (add-to-history 'efrit-do-history command))
  
  ;; Determine execution mode
  (let ((mode (or force-mode
                  (cl-case efrit-unified-default-mode
                    (ask-claude (efrit-unified--ask-claude-for-mode command))
                    (sync 'sync)
                    (async 'async)
                    (t 'sync)))))
    
    (efrit-log 'info "Executing command in %s mode: %s" mode command)
    
    ;; Dispatch to appropriate handler
    (cl-case mode
      (sync (efrit-do command))
      (async (efrit-do-async command))
      (t (efrit-do command)))))

;;; Simple Dispatchers

;;;###autoload
(defun efrit-unified-sync (command)
  "Execute COMMAND synchronously.
This is a simple wrapper around efrit-do for consistency."
  (interactive 
   (list (read-string "Efrit command (sync): " nil 'efrit-do-history)))
  (efrit-do command))

;;;###autoload
(defun efrit-unified-async (command)
  "Execute COMMAND asynchronously.
This is a simple wrapper around efrit-do-async for consistency."
  (interactive 
   (list (read-string "Efrit command (async): " nil 'efrit-do-history)))
  (efrit-do-async command))

;;; Status and Information

;;;###autoload
(defun efrit-unified-status ()
  "Show status of Efrit execution system."
  (interactive)
  (let ((buffer (get-buffer-create "*Efrit Status*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "=== Efrit Execution Status ===\n\n")
      
      ;; Current settings
      (insert "Settings:\n")
      (insert (format "  Default mode: %s\n" efrit-unified-default-mode))
      (insert "\n")
      
      ;; Active sessions
      (if (bound-and-true-p efrit-async--active-session)
          (progn
            (insert "Active async session:\n")
            (insert (format "  ID: %s\n" 
                            (efrit-session-id efrit-async--active-session)))
            (insert (format "  Command: %s\n" 
                            (efrit-session-command efrit-async--active-session)))
            (insert (format "  Steps completed: %d\n" 
                            (length (efrit-session-work-log 
                                    efrit-async--active-session)))))
        (insert "No active async sessions\n"))
      
      (insert "\n")
      
      ;; Recent commands
      (when (bound-and-true-p efrit-do-history)
        (insert "Recent commands:\n")
        (dolist (cmd (seq-take efrit-do-history 10))
          (insert (format "  - %s\n" cmd)))))
    
    (switch-to-buffer buffer)))

;;; Future Enhancement: Claude Mode Decision

(defconst efrit-unified--mode-decision-tool
  '(("name" . "suggest_execution_mode")
    ("description" . "Suggest whether this command should run synchronously or asynchronously")
    ("input_schema" . (("type" . "object")
                      ("properties" . (("mode" . (("type" . "string")
                                                   ("enum" . ["sync" "async"])
                                                   ("description" . "Suggested execution mode")))
                                      ("reason" . (("type" . "string")
                                                   ("description" . "Brief explanation for the suggestion")))))
                      ("required" . ["mode"]))))
  "Tool schema for Claude to suggest execution mode.
This will be added to the tools schema in a future update.")

;; TODO: Implement protocol for Claude to analyze command and suggest mode
;; This would involve:
;; 1. Special initial API call with just the command
;; 2. Claude uses suggest_execution_mode tool
;; 3. Client dispatches based on Claude's suggestion

(provide 'efrit-unified)
;;; efrit-unified.el ends here