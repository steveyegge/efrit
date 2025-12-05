;;; efrit-session-persist.el --- Session persistence and serialization -*- lexical-binding: t -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; Session persistence for Efrit REPL sessions.
;;
;; Provides serialization format and I/O functions for saving/loading
;; REPL sessions. Sessions are stored as JSON files in:
;;   ~/.emacs.d/efrit/sessions/{id}.json
;;
;; Format:
;; {
;;   "version": 1,
;;   "id": "session-20250105-143015-001",
;;   "created": "2025-01-15T14:30:15Z",
;;   "last_activity": "2025-01-15T14:45:22Z",
;;   "project": "/home/user/my-project",
;;   "title": "Optional session title",
;;   "conversation": [
;;     {"role": "user", "content": "...", "timestamp": "2025-01-15T14:30:20Z"},
;;     {"role": "assistant", "content": "...", "timestamp": "2025-01-15T14:30:25Z"}
;;   ],
;;   "api_messages": [
;;     {"role": "user", "content": "..."},
;;     {"role": "assistant", "content": "..."}
;;   ],
;;   "metadata": {
;;     "turn_count": 5,
;;     "total_tokens_used": 1234,
;;     "status": "idle"
;;   }
;; }

;;; Code:

(require 'json)
(require 'efrit-log)
(require 'efrit-repl-session)

;;; Customization

(defgroup efrit-session-persist nil
  "Session persistence for Efrit REPL."
  :group 'efrit
  :prefix "efrit-session-persist-")

(defcustom efrit-session-persist-dir
  (expand-file-name "efrit/sessions" user-emacs-directory)
  "Directory where REPL session files are stored."
  :type 'directory
  :group 'efrit-session-persist)

(defcustom efrit-session-persist-auto-save t
  "Whether to automatically save sessions after each turn."
  :type 'boolean
  :group 'efrit-session-persist)

(defcustom efrit-session-persist-retention-days 30
  "Number of days to retain session files. Older sessions are deleted."
  :type 'integer
  :group 'efrit-session-persist)

(defcustom efrit-session-persist-version 1
  "Session persistence format version."
  :type 'integer
  :group 'efrit-session-persist)

;;; Directory Management

(defun efrit-session-persist--ensure-directory ()
  "Ensure the session persistence directory exists."
  (unless (file-directory-p efrit-session-persist-dir)
    (make-directory efrit-session-persist-dir t))
  efrit-session-persist-dir)

(defun efrit-session-persist--get-file-path (session-id)
  "Return the file path for SESSION-ID."
  (expand-file-name
   (concat session-id ".json")
   (efrit-session-persist--ensure-directory)))

;;; Serialization

(defun efrit-session-persist--serialize-session (session)
  "Serialize REPL SESSION to JSON-compatible alist."
  (list
   (cons "version" efrit-session-persist-version)
   (cons "id" (efrit-repl-session-id session))
   (cons "created" (format-time-string "%Y-%m-%dT%H:%M:%SZ"
                                        (efrit-repl-session-created-at session)))
   (cons "last_activity" (format-time-string "%Y-%m-%dT%H:%M:%SZ"
                                              (efrit-repl-session-last-activity session)))
   (cons "project" (or (efrit-repl-session-project-root session) ""))
   (cons "title" (or (efrit-repl-session-title session) ""))
   (cons "status" (symbol-name (efrit-repl-session-status session)))
   
   ;; Conversation history (human-readable)
   (cons "conversation"
         (mapcar (lambda (entry)
                   (list
                    (cons "role" (symbol-name (plist-get entry :role)))
                    (cons "content" (plist-get entry :content))
                    (cons "timestamp"
                          (format-time-string "%Y-%m-%dT%H:%M:%SZ"
                                              (or (plist-get entry :timestamp)
                                                  (current-time))))))
                 (reverse (efrit-repl-session-conversation session))))
   
   ;; API messages (for continuations)
   (cons "api_messages"
         (mapcar (lambda (msg)
                   (list
                    (cons "role" (cdr (assoc "role" msg)))
                    (cons "content" (cdr (assoc "content" msg)))))
                 (reverse (efrit-repl-session-api-messages session))))
   
   ;; Metadata
   (cons "metadata"
         (let ((budget (efrit-repl-session-budget session)))
           (list
            (cons "turn_count" (length (efrit-repl-session-conversation session)))
            (cons "total_tokens_used"
                  (+ (efrit-budget-system-used budget)
                     (efrit-budget-history-used budget)
                     (efrit-budget-tool-results-used budget)
                     (efrit-budget-user-message-used budget))))))))

;;; Deserialization

(defun efrit-session-persist--deserialize-session (data)
  "Deserialize SESSION from JSON data alist."
  (let* ((id (cdr (assoc "id" data)))
         (created (parse-time-string (cdr (assoc "created" data))))
         (last-activity (parse-time-string (cdr (assoc "last_activity" data))))
         (project (cdr (assoc "project" data)))
         (title (cdr (assoc "title" data)))
         (status (intern (cdr (assoc "status" data))))
         (conversation-data (cdr (assoc "conversation" data)))
         (api-messages-data (cdr (assoc "api_messages" data)))
         
         ;; Build conversation history
         (conversation
          (mapcar (lambda (entry)
                    (list
                     :role (intern (cdr (assoc "role" entry)))
                     :content (cdr (assoc "content" entry))
                     :timestamp (parse-time-string (cdr (assoc "timestamp" entry)))))
                  conversation-data))
         
         ;; Build API messages
         (api-messages
          (mapcar (lambda (msg)
                    (list
                     (cons "role" (cdr (assoc "role" msg)))
                     (cons "content" (cdr (assoc "content" msg)))))
                  api-messages-data))
         
         ;; Create session
         (session (make-efrit-repl-session
                   :id id
                   :created-at created
                   :last-activity last-activity
                   :project-root project
                   :title title
                   :status status
                   :conversation conversation
                   :api-messages api-messages)))
    
    session))

;;; I/O Operations

(defun efrit-session-persist-save (session)
  "Save REPL SESSION to disk."
  (let* ((session-id (efrit-repl-session-id session))
         (file-path (efrit-session-persist--get-file-path session-id))
         (json-data (efrit-session-persist--serialize-session session))
         (json-str (json-encode json-data)))
    
    (condition-case err
        (progn
          (with-temp-file file-path
            (insert json-str))
          (efrit-log 'debug "Saved session %s to %s" session-id file-path)
          t)
      (error
       (efrit-log 'error "Failed to save session %s: %s" session-id (error-message-string err))
       nil))))

(defun efrit-session-persist-load (session-id)
  "Load REPL session with SESSION-ID from disk.
Returns the deserialized session, or nil if not found."
  (let ((file-path (efrit-session-persist--get-file-path session-id)))
    (if (not (file-exists-p file-path))
        (progn
          (efrit-log 'warn "Session file not found: %s" file-path)
          nil)
      (condition-case err
          (let* ((json-str (with-temp-buffer
                            (insert-file-contents file-path)
                            (buffer-string)))
                 (json-object-type 'alist)
                 (json-array-type 'list)
                 (json-key-type 'string)
                 (data (json-read-from-string json-str)))
            (efrit-log 'debug "Loaded session %s from %s" session-id file-path)
            (efrit-session-persist--deserialize-session data))
        (error
         (efrit-log 'error "Failed to load session %s: %s" session-id (error-message-string err))
         nil)))))

(defun efrit-session-persist-delete (session-id)
  "Delete the session file for SESSION-ID."
  (let ((file-path (efrit-session-persist--get-file-path session-id)))
    (when (file-exists-p file-path)
      (delete-file file-path)
      (efrit-log 'debug "Deleted session file: %s" file-path)
      t)))

(defun efrit-session-persist-list ()
  "Return a list of all saved session IDs.
Returns list of (id . (created-time last-activity))."
  (let ((dir (efrit-session-persist--ensure-directory))
        (sessions nil))
    (dolist (file (directory-files dir nil "\\.json$"))
      (let* ((session-id (file-name-sans-extension file))
             (file-path (expand-file-name file dir)))
        (condition-case nil
            (let* ((json-str (with-temp-buffer
                              (insert-file-contents file-path)
                              (buffer-string)))
                   (json-object-type 'alist)
                   (json-array-type 'list)
                   (json-key-type 'string)
                   (data (json-read-from-string json-str)))
              (push (cons session-id
                         (list
                          (cdr (assoc "created" data))
                          (cdr (assoc "last_activity" data))))
                    sessions))
          (error nil))))
    (reverse sessions)))

;;; Cleanup

(defun efrit-session-persist-cleanup-old ()
  "Delete session files older than retention period."
  (let ((cutoff-time (time-subtract (current-time)
                                     (days-to-time efrit-session-persist-retention-days)))
        (dir (efrit-session-persist--ensure-directory))
        (deleted-count 0))
    (dolist (file (directory-files dir nil "\\.json$"))
      (let ((file-path (expand-file-name file dir)))
        (when (time-less-p (nth 5 (file-attributes file-path)) cutoff-time)
          (delete-file file-path)
          (cl-incf deleted-count))))
    (when (> deleted-count 0)
      (efrit-log 'info "Cleaned up %d old session files" deleted-count))
    deleted-count))

(provide 'efrit-session-persist)

;;; efrit-session-persist.el ends here
