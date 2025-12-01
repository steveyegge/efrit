;;; efrit-session-context.el --- Unified context and command history -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; Unified context system for Efrit.
;; Provides:
;; - Unified context (cross-mode message history)
;; - Context ring (legacy command history with backward compatibility)
;; - Persistence and restoration of context
;;
;; This module provides a single source of truth for context shared
;; across all Efrit modes (chat, do, async, etc.).

;;; Code:

(require 'cl-lib)
(require 'ring)
(require 'efrit-log)
(require 'efrit-common)
(require 'efrit-config)

;;; Customization

(defcustom efrit-context-ring-size 10
  "Size of the command history ring."
  :type 'integer
  :group 'efrit-session)

(defcustom efrit-context-persist-file nil
  "File to persist context data.
If nil, uses the default location in the efrit data directory."
  :type '(choice (const :tag "Default location" nil)
                 (file :tag "Custom file"))
  :group 'efrit-session)

;;; Unified Context System
;; This provides a single source of truth for context across all modes

(defvar efrit-unified-context--message-history nil
  "Global message history shared across all modes.
Each entry is an alist with keys: role, content, timestamp, mode, metadata.")

(defvar efrit-unified-context--max-history 100
  "Maximum number of messages to retain in unified history.")

(defun efrit-unified-context-add-message (role content &optional mode metadata)
  "Add a message to unified context history.
ROLE is \\='user or \\='assistant.
CONTENT is the message text.
MODE is the originating mode (\\='chat, \\='do, \\='async, etc.).
METADATA is optional additional data (alist)."
  (let ((entry `((role . ,role)
                (content . ,content)
                (timestamp . ,(current-time))
                (mode . ,(or mode 'unknown))
                (metadata . ,metadata))))
    (push entry efrit-unified-context--message-history)
    ;; Trim to max length
    (when (> (length efrit-unified-context--message-history)
             efrit-unified-context--max-history)
      (setq efrit-unified-context--message-history
            (seq-take efrit-unified-context--message-history
                     efrit-unified-context--max-history)))
    entry))

(defun efrit-unified-context-get-messages (&optional n mode-filter)
  "Get N most recent messages from unified history.
If MODE-FILTER is provided, only return messages from that mode.
Returns messages in chronological order (oldest first)."
  (let* ((count (or n (length efrit-unified-context--message-history)))
         (filtered (if mode-filter
                      (seq-filter (lambda (msg)
                                   (eq (alist-get 'mode msg) mode-filter))
                                 efrit-unified-context--message-history)
                    efrit-unified-context--message-history))
         (recent (seq-take filtered count)))
    (nreverse recent)))

(defun efrit-unified-context-get-for-api (n &optional include-metadata)
  "Get N recent messages formatted for Claude API.
Returns a vector suitable for the \\='messages field.
If INCLUDE-METADATA is nil, strips metadata fields."
  (let ((messages (efrit-unified-context-get-messages n)))
    (vconcat
     (mapcar (lambda (msg)
              (if include-metadata
                  `(("role" . ,(symbol-name (alist-get 'role msg)))
                   ("content" . ,(alist-get 'content msg))
                   ("metadata" . ,(alist-get 'metadata msg)))
                `(("role" . ,(symbol-name (alist-get 'role msg)))
                 ("content" . ,(alist-get 'content msg)))))
            messages))))

(defun efrit-unified-context-format-for-system-prompt (&optional n)
  "Format N recent messages as a string for system prompt inclusion.
Returns a compressed summary suitable for system prompt context."
  (let ((messages (efrit-unified-context-get-messages (or n 5))))
    (if (null messages)
        ""
      (concat
       "\n\nRECENT CONTEXT:\n"
       (mapconcat
        (lambda (msg)
          (let ((role (alist-get 'role msg))
                (content (alist-get 'content msg))
                (mode (alist-get 'mode msg))
                (timestamp (alist-get 'timestamp msg)))
            (format "[%s|%s] %s: %s"
                    (format-time-string "%H:%M:%S" timestamp)
                    mode
                    (capitalize (symbol-name role))
                    (efrit-common-truncate-string content 150))))
        messages
        "\n")))))

(defun efrit-unified-context-clear (&optional mode-filter)
  "Clear unified context history.
If MODE-FILTER is provided, only clear messages from that mode."
  (if mode-filter
      (setq efrit-unified-context--message-history
            (seq-remove (lambda (msg)
                         (eq (alist-get 'mode msg) mode-filter))
                       efrit-unified-context--message-history))
    (setq efrit-unified-context--message-history nil)))

(defun efrit-unified-context-persist ()
  "Persist unified context to file."
  (when efrit-unified-context--message-history
    (let ((file (efrit-config-context-file "efrit-unified-context.el")))
      (with-temp-file file
        (insert ";; Efrit unified context data\n")
        (insert ";; Saved: " (current-time-string) "\n\n")
        (prin1 efrit-unified-context--message-history (current-buffer)))
      (efrit-log 'debug "Persisted %d messages to unified context"
                 (length efrit-unified-context--message-history)))))

(defun efrit-unified-context-restore ()
  "Restore unified context from file."
  (let ((file (efrit-config-context-file "efrit-unified-context.el")))
    (when (file-exists-p file)
      (condition-case err
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (search-forward "\n\n" nil t)
            (let ((messages (read (current-buffer))))
              (setq efrit-unified-context--message-history messages)
              (efrit-log 'debug "Restored %d messages from unified context"
                        (length messages))))
        (error
         (efrit-log 'warn "Failed to restore unified context: %s"
                   (efrit-common-safe-error-message err)))))))

(defun efrit-unified-context-init ()
  "Initialize unified context system."
  (efrit-unified-context-restore)
  (efrit-log 'debug "Unified context system initialized"))

;;; Context Ring (Backward Compatibility)

(cl-defstruct (efrit-context-item
            (:constructor efrit-context-item-create)
            (:type vector))
  "Context item structure for command history."
  timestamp
  command
  result
  buffer
  directory
  window-config
  metadata)

(defvar efrit-context--ring nil
  "Ring buffer for context items.")

(defvar efrit-context--hooks nil
  "Hooks run when context is captured.")

(defun efrit-context-ring-init ()
  "Initialize or reinitialize the context ring."
  (unless efrit-context--ring
    (setq efrit-context--ring (make-ring efrit-context-ring-size))))

(defun efrit-context-ring-add (command result &optional metadata)
  "Add a new context item for COMMAND with RESULT and optional METADATA."
  (efrit-context-ring-init)
  (let ((item (efrit-context-item-create
               :timestamp (current-time)
               :command command
               :result result
               :buffer (buffer-name)
               :directory default-directory
               :window-config (current-window-configuration)
               :metadata metadata)))
    (ring-insert efrit-context--ring item)
    (run-hook-with-args 'efrit-context--hooks item)
    ;; Also add to unified context
    (efrit-unified-context-add-message 'user command 'do metadata)
    (when result
      (efrit-unified-context-add-message 'assistant result 'do metadata))
    item))

(defun efrit-context-ring-get-recent (&optional n)
  "Get N most recent context items (default all)."
  (when efrit-context--ring
    (let ((ring efrit-context--ring)
          (count (or n (ring-length efrit-context--ring)))
          (items '()))
      (dotimes (i (min count (ring-length ring)))
        (push (ring-ref ring i) items))
      (nreverse items))))

(defun efrit-context-item-to-string (item)
  "Convert context ITEM to string representation."
  (format "[%s] %s -> %s (in %s)"
          (format-time-string "%H:%M:%S" (efrit-context-item-timestamp item))
          (efrit-context-item-command item)
          (truncate-string-to-width (or (efrit-context-item-result item) "") 50 nil nil t)
          (efrit-context-item-buffer item)))

(defun efrit-context-ring-clear ()
  "Clear the context ring."
  (when efrit-context--ring
    (setq efrit-context--ring (make-ring efrit-context-ring-size))))

(defun efrit-context-ring--make-serializable (item)
  "Convert ITEM to a serializable form by removing unreadable objects.
Returns a copy with window-config set to nil since window configurations
cannot be serialized."
  (let ((copy (copy-sequence item)))
    ;; Window configurations are unreadable objects (print as #<window-configuration>)
    ;; Set to nil so the context can be serialized and restored
    (setf (efrit-context-item-window-config copy) nil)
    copy))

(defun efrit-context-ring-persist ()
  "Persist context ring to file."
  (when efrit-context--ring
    (let ((file (or efrit-context-persist-file
                   (efrit-config-context-file "efrit-context-ring.el")))
          (items (efrit-context-ring-get-recent)))
      (when items
        (with-temp-file file
          (insert ";; Efrit context ring data\n")
          (insert ";; Saved: " (current-time-string) "\n\n")
          ;; Convert items to serializable form before persisting
          (prin1 (mapcar #'efrit-context-ring--make-serializable items)
                 (current-buffer)))
        (efrit-log 'debug "Persisted %d context items to %s" (length items) file)))))

(defun efrit-context-ring-restore ()
  "Restore context ring from file."
  (let ((file (or efrit-context-persist-file
                 (efrit-config-context-file "efrit-context-ring.el"))))
    (when (file-exists-p file)
      (condition-case err
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (search-forward "\n\n" nil t)
            (let ((items (read (current-buffer))))
              (efrit-context-ring-init)
              (dolist (item (reverse items))
                (ring-insert efrit-context--ring item))
              (efrit-log 'debug "Restored %d context items from %s"
                        (length items) file)))
        (error
         (efrit-log 'warn "Failed to restore context: %s"
                   (efrit-common-safe-error-message err)))))))

(defun efrit-context-init ()
  "Initialize context system."
  (efrit-unified-context-init)
  (efrit-context-ring-init)
  (efrit-context-ring-restore)
  (efrit-log 'debug "Context system initialized"))

;;; User Commands

;;;###autoload
(defun efrit-show-context (&optional n)
  "Show N most recent messages from unified context (default 10).
Displays context shared across all Efrit modes."
  (interactive "p")
  (let ((messages (efrit-unified-context-get-messages (or n 10))))
    (if (null messages)
        (message "No context history available")
      (with-output-to-temp-buffer "*Efrit Unified Context*"
        (princ (format "Efrit Unified Context (%d most recent messages):\n\n"
                      (length messages)))
        (dolist (msg messages)
          (let ((timestamp (alist-get 'timestamp msg))
                (mode (alist-get 'mode msg))
                (role (alist-get 'role msg))
                (content (alist-get 'content msg)))
            (princ (format "[%s | %s] %s:\n%s\n\n"
                          (format-time-string "%Y-%m-%d %H:%M:%S" timestamp)
                          (upcase (symbol-name mode))
                          (capitalize (symbol-name role))
                          (efrit-common-truncate-string content 500)))))))))

;;;###autoload
(defun efrit-clear-context (&optional mode-filter)
  "Clear unified context history.
With prefix arg, prompt for which mode to clear (chat, do, async).
Otherwise clears all context."
  (interactive
   (list (when current-prefix-arg
          (intern (completing-read "Clear context for mode: "
                                  '("chat" "do" "async" "all")
                                  nil t)))))
  (if (and mode-filter (not (eq mode-filter 'all)))
      (progn
        (efrit-unified-context-clear mode-filter)
        (message "Cleared %s context" mode-filter))
    (efrit-unified-context-clear)
    (message "Cleared all unified context")))

;;;###autoload
(defun efrit-save-context ()
  "Save unified context to disk."
  (interactive)
  (efrit-unified-context-persist)
  (message "Context saved"))

(provide 'efrit-session-context)

;;; efrit-session-context.el ends here
