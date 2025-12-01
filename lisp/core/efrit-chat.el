;;; efrit-chat.el --- Conversational assistant for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai
;; URL: https://github.com/steveyegge/efrit

;;; Commentary:
;; Unified chat interface for Efrit, providing both:
;; - Classic interactive chat mode (efrit-chat)
;; - Streamlined single-turn mode (efrit-streamlined-send)
;;
;; This module is the main entry point and public API.
;; Internal implementation details are in:
;; - efrit-chat-buffer.el (buffer management, display)
;; - efrit-chat-api.el (API communication, tool execution)

;;; Code:

(require 'efrit-chat-buffer)
(require 'efrit-chat-api)
(require 'efrit-config)

;; Declare functions from other modules to avoid warnings
(declare-function efrit-log-section "efrit-log")
(declare-function efrit-log-debug "efrit-log")
(declare-function efrit-chat-maybe-restore "efrit-chat-persistence")
(declare-function efrit-chat-save-session "efrit-chat-persistence")
(declare-function efrit-chat--generate-session-id "efrit-chat-persistence")
(declare-function efrit-unified-context-add-message "efrit-session")

;; Declare variables from other modules
(defvar efrit-mode-map)
(defvar efrit-model nil "Model to use for API calls. When nil, uses efrit-default-model from efrit-config")

;;; User Interface Commands - Classic Chat Mode

;;;###autoload
(defun efrit-send-message (message)
  "Send MESSAGE to the Claude API."
  (interactive "sMessage: ")
  (efrit-log-section "SEND MESSAGE")
  (efrit-log-debug "User message: %s" message)
  (with-current-buffer (efrit--setup-buffer)
    (setq buffer-read-only nil)
    (let ((inhibit-read-only t))
      ;; Check if a response is already in progress
      (when efrit--response-in-progress
        (user-error "Please wait for the current response to complete"))

      ;; Display the user message only if not already displayed
      (unless (save-excursion
                (goto-char (point-max))
                (forward-line -1)
                (and (looking-at "You: ")
                     (string-match-p (regexp-quote message) (thing-at-point 'line))))
        (efrit--display-message message 'user))

      ;; Don't use multi-turn in chat mode - users control the conversation
      (setq-local efrit--current-conversation nil)

      ;; Store message for retry on error
      (setq-local efrit--last-user-message message)

      ;; Add message to history (at beginning, as we're using push)
      (push `((role . "user")
             (content . ,message))
            efrit--message-history)

      ;; Also add to unified context
      (require 'efrit-session)
      (efrit-unified-context-add-message 'user message 'chat)

      ;; Show thinking indicator
      (efrit--display-message "Thinking..." 'system)

      ;; Set in-progress flag - will be cleared by response handler
      (setq-local efrit--response-in-progress t)

      ;; Send the API request (async - flag cleared by response handler)
      (condition-case err
          (efrit--send-api-request (reverse efrit--message-history))
        (error
         ;; If url-retrieve fails immediately, clear flag and show error
         (setq-local efrit--response-in-progress nil)
         (efrit--display-message
          (format "Failed to send request: %s" (error-message-string err))
          'system)
         (efrit--insert-prompt))))))

;;;###autoload
(defun efrit-send-buffer-message ()
  "Send the current input message from the buffer."
  (interactive)
  (with-current-buffer (efrit--setup-buffer)
    (setq buffer-read-only nil)
    (let ((inhibit-read-only t))
      (if (and efrit--input-marker
               (marker-position efrit--input-marker))
          (let* ((raw-input (buffer-substring-no-properties
                            efrit--input-marker (point-max)))
                 (message (string-trim
                          (replace-regexp-in-string "^>\\s-*" "" raw-input))))
            (if (string-empty-p message)
                (message "No input to send")
              ;; Replace the input area with properly formatted user message
              (delete-region efrit--input-marker (point-max))
              (goto-char efrit--input-marker)
              (insert (format "You: %s\n" message))
              ;; Update conversation marker to track conversation end
              (set-marker efrit--conversation-marker (point))
              ;; Send message
              (efrit-send-message message)))
        (progn
          (message "Debug: input marker not set up properly. Marker: %S, Position: %S"
                   efrit--input-marker
                   (when efrit--input-marker (marker-position efrit--input-marker)))
          (efrit--insert-prompt))))))

;;;###autoload
(defun efrit-insert-newline ()
  "Insert a newline in the message without sending."
  (interactive)
  (with-current-buffer (efrit--setup-buffer)
    (setq buffer-read-only nil)
    (insert "\n")))

;;;###autoload
(defun efrit-retry-last-message ()
  "Retry the last user message that was sent.
Useful when the previous API call failed."
  (interactive)
  (with-current-buffer (efrit--setup-buffer)
    (if efrit--last-user-message
        (progn
          (message "Retrying: %s" efrit--last-user-message)
          (efrit-send-message efrit--last-user-message))
      (message "No previous message to retry"))))

;;;###autoload
(defun efrit-chat-debug ()
  "Start efrit chat with full debugging (shows all technical details)."
  (interactive)
  (switch-to-buffer (efrit--setup-buffer))
  (setq buffer-read-only nil)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq-local efrit--message-history nil)
    (setq-local efrit--response-in-progress nil)
    (setq-local efrit--conversation-marker nil)
    (setq-local efrit--input-marker nil)
    (setq-local efrit--current-conversation nil))
  (setq-local efrit--conversation-marker (make-marker))
  (set-marker efrit--conversation-marker (point-min))
  (efrit--display-message
   (format "Efrit initialized. Enter your message below and press Enter to send.\nUse Shift+Enter for newlines. Using model: %s"
           (or efrit-model (require 'efrit-config) efrit-default-model))
   'assistant)
  (efrit--insert-prompt))

;;; User Interface Commands - Streamlined Mode

;;;###autoload
(defun efrit-streamlined-send (message)
  "Send MESSAGE using streamlined chat experience."
  (interactive "sMessage: ")
  (let ((messages (list `((role . "user") (content . ,message)))))
    (setq efrit-streamlined--current-messages messages)
    (setq efrit-streamlined--turn-count 0)
    (efrit-streamlined--log-to-work (format "User message: %s" message))
    (message "Efrit: Processing request...")
    (efrit-streamlined--send-request messages)))

;;; Main Chat Interface

;;;###autoload
(defun efrit-chat ()
  "Start efrit chat session - interactive buffer like ChatGPT.
If a previous session exists, offers to restore it.
Returns the chat buffer for programmatic use."
  (interactive)
  (let ((buffer (efrit--setup-buffer)))
    (switch-to-buffer buffer)
    (setq buffer-read-only nil)
    ;; Check if we should restore a previous session
    (unless (efrit-chat-maybe-restore)
      ;; No restore - start fresh
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq-local efrit--message-history nil)
        (setq-local efrit--response-in-progress nil)
        (setq-local efrit--conversation-marker nil)
        (setq-local efrit--input-marker nil)
        (setq-local efrit--current-conversation nil))

      (setq-local efrit--conversation-marker (make-marker))
      (set-marker efrit--conversation-marker (point-min))

      (efrit--display-message
       (format "Efrit Chat Ready - Using model: %s" (or efrit-model (require 'efrit-config) efrit-default-model))
       'assistant)

      (efrit--insert-prompt))
    buffer))

;;;###autoload
(defun efrit-chat-clear ()
  "Clear the conversation history and start fresh in the current chat buffer.
The previous session is saved before clearing if auto-save is enabled."
  (interactive)
  (if (not (eq major-mode 'efrit-mode))
      (user-error "Not in an Efrit chat buffer")
    (when (or (not efrit--message-history)
              (yes-or-no-p "Clear conversation history and start fresh? "))
      ;; Save current session before clearing
      (when efrit--message-history
        (efrit-chat-save-session))
      (setq buffer-read-only nil)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (setq-local efrit--message-history nil)
        (setq-local efrit--response-in-progress nil)
        (setq-local efrit--conversation-marker nil)
        (setq-local efrit--input-marker nil)
        (setq-local efrit--current-conversation nil)
        ;; Start new session
        (setq-local efrit-chat--session-id (efrit-chat--generate-session-id))
        (setq-local efrit-chat--session-modified nil))

      (setq-local efrit--conversation-marker (make-marker))
      (set-marker efrit--conversation-marker (point-min))

      (efrit--display-message
       (format "Conversation cleared - Using model: %s" (or efrit-model (require 'efrit-config) efrit-default-model))
       'assistant)

      (efrit--insert-prompt)
      (message "Efrit chat conversation cleared"))))

(provide 'efrit-chat)

;;; efrit-chat.el ends here
