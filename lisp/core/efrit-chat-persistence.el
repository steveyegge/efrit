;;; efrit-chat-persistence.el --- Chat session persistence -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; Session persistence for efrit-chat.
;; Provides:
;; - Auto-save chat sessions on buffer kill
;; - Auto-save after each message exchange
;; - Session restore when revisiting chat
;; - List and browse past chat sessions
;;
;; Chat sessions are stored separately from efrit-do transcripts
;; to enable mode-specific browsing and resume.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'json)
(require 'efrit-log)
(require 'efrit-common)
(require 'efrit-config)

;; Forward declarations
(defvar efrit-buffer-name)
(defvar efrit--message-history)
(defvar efrit--conversation-marker)
(defvar efrit--input-marker)
(defvar efrit--response-in-progress)
(defvar efrit--current-conversation)
(defvar efrit-model)

(declare-function efrit--setup-buffer "efrit-chat")
(declare-function efrit--display-message "efrit-chat")
(declare-function efrit--insert-prompt "efrit-chat")
(declare-function efrit-mode "efrit-chat")

;;; Customization

(defgroup efrit-chat-persistence nil
  "Persistence settings for efrit-chat."
  :group 'efrit
  :prefix "efrit-chat-persistence-")

(defcustom efrit-chat-persistence-directory nil
  "Directory for chat session files.
If nil, uses `efrit-data-directory'/chat-sessions."
  :type '(choice (const :tag "Default location" nil)
                 (directory :tag "Custom directory"))
  :group 'efrit-chat-persistence)

(defcustom efrit-chat-auto-save t
  "When non-nil, automatically save chat sessions.
Sessions are saved on buffer kill and after each message exchange."
  :type 'boolean
  :group 'efrit-chat-persistence)

(defcustom efrit-chat-auto-restore t
  "When non-nil, offer to restore previous session when starting chat.
If nil, always starts a fresh session."
  :type 'boolean
  :group 'efrit-chat-persistence)

(defcustom efrit-chat-max-sessions 50
  "Maximum number of chat sessions to keep.
Older sessions are automatically cleaned up."
  :type 'integer
  :group 'efrit-chat-persistence)

;;; Internal State

(defvar-local efrit-chat--session-id nil
  "Current chat session ID for persistence.")

(defvar-local efrit-chat--session-created-at nil
  "Creation timestamp for current session (ISO 8601 format).")

(defvar-local efrit-chat--session-modified nil
  "Non-nil if current session has unsaved changes.")

;;; Directory Management

(defun efrit-chat-persistence-directory ()
  "Return the chat session storage directory, creating if needed."
  (let ((dir (or efrit-chat-persistence-directory
                 (expand-file-name "chat-sessions" efrit-data-directory))))
    (unless (file-directory-p dir)
      (make-directory dir t))
    dir))

;;; Session ID Generation

(defun efrit-chat--generate-session-id ()
  "Generate a unique session ID for a chat session."
  (format "chat-%s-%04x"
          (format-time-string "%Y%m%d-%H%M%S")
          (random 65536)))

;;; Session Data Structure

(defun efrit-chat--session-to-alist (messages)
  "Convert MESSAGES to a session alist for persistence.
MESSAGES is the buffer-local `efrit--message-history'."
  (let* ((message-count (length messages))
         (first-user-msg (seq-find (lambda (m)
                                     (equal (alist-get 'role m) "user"))
                                   (reverse messages)))
         (snippet (if first-user-msg
                      (let ((content (alist-get 'content first-user-msg)))
                        (if (> (length content) 80)
                            (concat (substring content 0 77) "...")
                          content))
                    "(no messages)"))
         (now (format-time-string "%Y-%m-%dT%H:%M:%SZ")))
    ;; Set created_at on first save if not already set
    (unless efrit-chat--session-created-at
      (setq efrit-chat--session-created-at now))
    `((id . ,efrit-chat--session-id)
      (type . "chat")
      (created_at . ,efrit-chat--session-created-at)
      (updated_at . ,now)
      (project_dir . ,default-directory)
      (message_count . ,message-count)
      (snippet . ,snippet)
      (model . ,efrit-model)
      (messages . ,(vconcat (reverse messages))))))

;;; Save/Load Functions

(defun efrit-chat-save-session ()
  "Save current chat session to disk.
Only saves if there are messages and auto-save is enabled."
  (interactive)
  (when (and efrit-chat--session-id
             efrit--message-history
             (or (called-interactively-p 'any)
                 efrit-chat-auto-save))
    (let* ((session-dir (efrit-chat-persistence-directory))
           (session-file (expand-file-name
                          (concat efrit-chat--session-id ".json")
                          session-dir))
           (session-data (efrit-chat--session-to-alist efrit--message-history)))
      (condition-case err
          (progn
            (let ((coding-system-for-write 'utf-8))
              (with-temp-file session-file
                (insert (json-encode session-data))))
            (setq efrit-chat--session-modified nil)
            (efrit-log 'debug "Saved chat session: %s (%d messages)"
                       efrit-chat--session-id
                       (length efrit--message-history))
            ;; Cleanup old sessions if we exceed max
            (efrit-chat-cleanup-old-sessions)
            (when (called-interactively-p 'any)
              (message "Chat session saved: %s" efrit-chat--session-id)))
        (error
         (efrit-log 'error "Failed to save chat session: %s"
                    (error-message-string err)))))))

(defun efrit-chat-load-session (session-id)
  "Load chat session with SESSION-ID from disk.
Returns session alist or nil if not found."
  (let ((session-file (expand-file-name
                       (concat session-id ".json")
                       (efrit-chat-persistence-directory))))
    (when (file-exists-p session-file)
      (condition-case err
          (with-temp-buffer
            (insert-file-contents session-file)
            (json-read-from-string (buffer-string)))
        (error
         (efrit-log 'warn "Failed to load chat session %s: %s"
                    session-id (error-message-string err))
         nil)))))

;;; Session Listing

(defun efrit-chat-list-sessions ()
  "List all available chat sessions with metadata.
Returns list of alists sorted by recency (most recent first)."
  (let* ((session-dir (efrit-chat-persistence-directory))
         (files (when (file-directory-p session-dir)
                  (directory-files session-dir t "\\.json$")))
         (sessions nil))
    (dolist (file files)
      (condition-case nil
          (with-temp-buffer
            (insert-file-contents file)
            (let ((data (json-read-from-string (buffer-string))))
              ;; Only include chat sessions (not do transcripts)
              (when (equal (alist-get 'type data) "chat")
                (push `((id . ,(alist-get 'id data))
                        (snippet . ,(alist-get 'snippet data))
                        (created_at . ,(alist-get 'created_at data))
                        (updated_at . ,(alist-get 'updated_at data))
                        (message_count . ,(alist-get 'message_count data))
                        (project_dir . ,(alist-get 'project_dir data))
                        (model . ,(alist-get 'model data))
                        (file . ,file))
                      sessions))))
        (error nil)))
    ;; Sort by updated_at descending
    (sort sessions
          (lambda (a b)
            (string> (or (alist-get 'updated_at a) "")
                     (or (alist-get 'updated_at b) ""))))))

;;; Session Restore

(defun efrit-chat--restore-session-to-buffer (session-data)
  "Restore SESSION-DATA into the current chat buffer."
  (let ((messages (alist-get 'messages session-data))
        (session-id (alist-get 'id session-data))
        (created-at (alist-get 'created_at session-data)))
    ;; Set session ID and preserve creation timestamp
    (setq efrit-chat--session-id session-id)
    (setq efrit-chat--session-created-at created-at)
    (setq efrit-chat--session-modified nil)

    ;; Clear existing state
    (setq-local efrit--message-history nil)
    (setq-local efrit--response-in-progress nil)
    (setq-local efrit--current-conversation nil)

    ;; Restore messages to history (in reverse order, as history is newest-first)
    (when (vectorp messages)
      (dotimes (i (length messages))
        (let* ((msg (aref messages i))
               (role (alist-get 'role msg))
               (content (alist-get 'content msg)))
          (push `((role . ,role) (content . ,content))
                efrit--message-history))))

    ;; Display messages in buffer
    (let ((inhibit-read-only t))
      (erase-buffer)
      (setq-local efrit--conversation-marker (make-marker))
      (set-marker efrit--conversation-marker (point-min))

      ;; Show restoration message
      (efrit--display-message
       (format "Restored session: %s (%d messages)"
               session-id (length messages))
       'system)

      ;; Display each message (messages are in chronological order in the file)
      (when (vectorp messages)
        (dotimes (i (length messages))
          (let* ((msg (aref messages i))
                 (role (alist-get 'role msg))
                 (content (alist-get 'content msg)))
            (efrit--display-message
             content
             (cond ((equal role "user") 'user)
                   ((equal role "assistant") 'assistant)
                   (t 'system))))))

      ;; Insert prompt for new input
      (efrit--insert-prompt))

    (efrit-log 'info "Restored chat session %s with %d messages"
               session-id (length messages))))

;;; Time Formatting

(defun efrit-chat--format-time-ago (iso-time)
  "Format ISO-TIME string as human-readable time ago."
  (if (not iso-time)
      "unknown"
    (condition-case nil
        (let* ((time (date-to-time (replace-regexp-in-string "T" " " iso-time)))
               (diff (float-time (time-subtract (current-time) time))))
          (cond
           ((< diff 60) "just now")
           ((< diff 3600) (format "%d min ago" (/ diff 60)))
           ((< diff 86400) (format "%d hrs ago" (/ diff 3600)))
           ((< diff 604800) (format "%d days ago" (/ diff 86400)))
           (t (format-time-string "%Y-%m-%d" time))))
      (error "unknown"))))

;;; User Commands

;;;###autoload
(defun efrit-chat-list ()
  "List and select from saved chat sessions."
  (interactive)
  (let* ((sessions (efrit-chat-list-sessions))
         (candidates
          (mapcar (lambda (s)
                    (let* ((id (alist-get 'id s))
                           (snippet (or (alist-get 'snippet s) "(no snippet)"))
                           (count (or (alist-get 'message_count s) 0))
                           (time-ago (efrit-chat--format-time-ago
                                      (alist-get 'updated_at s))))
                      (propertize
                       (format "%s: %s (%d msgs, %s)"
                               id
                               (truncate-string-to-width snippet 50)
                               count time-ago)
                       'efrit-session-id id
                       'efrit-session-data s)))
                  sessions)))
    (if (null candidates)
        (message "No saved chat sessions found")
      (let ((selected (completing-read "Chat session: " candidates nil t)))
        (when selected
          (let ((session-id (get-text-property 0 'efrit-session-id selected)))
            (efrit-chat-restore session-id)))))))

;;;###autoload
(defun efrit-chat-restore (session-id)
  "Restore a previous chat session by SESSION-ID."
  (interactive
   (list (let* ((sessions (efrit-chat-list-sessions))
                (ids (mapcar (lambda (s) (alist-get 'id s)) sessions)))
           (if (null ids)
               (user-error "No saved chat sessions")
             (completing-read "Restore session: " ids nil t)))))
  (let ((session-data (efrit-chat-load-session session-id)))
    (if (not session-data)
        (user-error "Session not found: %s" session-id)
      ;; Set up buffer and restore
      (let ((buffer (get-buffer-create efrit-buffer-name)))
        (with-current-buffer buffer
          (unless (eq major-mode 'efrit-mode)
            (efrit-mode))
          (setq buffer-read-only nil)
          (efrit-chat--restore-session-to-buffer session-data))
        (switch-to-buffer buffer)
        (message "Restored chat session: %s" session-id)))))

;;;###autoload
(defun efrit-chat-delete-session (session-id)
  "Delete chat session with SESSION-ID."
  (interactive
   (list (let* ((sessions (efrit-chat-list-sessions))
                (ids (mapcar (lambda (s) (alist-get 'id s)) sessions)))
           (if (null ids)
               (user-error "No saved chat sessions")
             (completing-read "Delete session: " ids nil t)))))
  (let ((session-file (expand-file-name
                       (concat session-id ".json")
                       (efrit-chat-persistence-directory))))
    (if (not (file-exists-p session-file))
        (message "Session not found: %s" session-id)
      (when (yes-or-no-p (format "Delete chat session %s? " session-id))
        (delete-file session-file)
        (message "Deleted session: %s" session-id)))))

;;; Cleanup

(defun efrit-chat-cleanup-old-sessions ()
  "Remove excess chat sessions beyond `efrit-chat-max-sessions'."
  (interactive)
  (let* ((sessions (efrit-chat-list-sessions))
         (excess-count (- (length sessions) efrit-chat-max-sessions)))
    (when (> excess-count 0)
      (let ((to-delete (seq-drop sessions efrit-chat-max-sessions)))
        (dolist (session to-delete)
          (let ((file (alist-get 'file session)))
            (when (and file (file-exists-p file))
              (delete-file file))))
        (efrit-log 'info "Cleaned up %d old chat sessions" (length to-delete))
        (when (called-interactively-p 'any)
          (message "Cleaned up %d old chat sessions" (length to-delete)))))))

;;; Hooks for Auto-save

(defun efrit-chat--on-message-sent ()
  "Hook called after a message is sent/received.
Marks session as modified and triggers auto-save."
  (setq efrit-chat--session-modified t)
  (efrit-chat-save-session))

(defun efrit-chat--on-buffer-kill ()
  "Hook called when chat buffer is killed.
Saves session if modified."
  (when (and efrit-chat--session-id
             efrit-chat--session-modified
             efrit-chat-auto-save)
    (efrit-chat-save-session)))

(defun efrit-chat--setup-persistence ()
  "Set up persistence for the current chat buffer.
Creates a new session ID if needed."
  (unless efrit-chat--session-id
    (setq efrit-chat--session-id (efrit-chat--generate-session-id)))
  (setq efrit-chat--session-modified nil)
  (add-hook 'kill-buffer-hook #'efrit-chat--on-buffer-kill nil t))

;;; Offer Restore on Chat Start

(defun efrit-chat-maybe-restore ()
  "Check for recent sessions and offer to restore.
Returns t if a session was restored, nil otherwise.
In batch mode (noninteractive), skips restoration to avoid prompting for input."
  (when efrit-chat-auto-restore
    ;; Skip restoration prompts in batch/noninteractive mode
    (unless noninteractive
      (let ((sessions (efrit-chat-list-sessions)))
        (when (and sessions
                   (yes-or-no-p
                    (format "Restore previous chat? (%s, %d msgs) "
                            (efrit-chat--format-time-ago
                             (alist-get 'updated_at (car sessions)))
                            (or (alist-get 'message_count (car sessions)) 0))))
          (efrit-chat--restore-session-to-buffer
           (efrit-chat-load-session (alist-get 'id (car sessions))))
          t)))))

(provide 'efrit-chat-persistence)

;;; efrit-chat-persistence.el ends here
