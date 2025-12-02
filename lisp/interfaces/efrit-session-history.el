;;; efrit-session-history.el --- Session history and archival management -*- lexical-binding: t -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; Session history and archival management for Efrit.
;;
;; Features:
;; - Automatic archival of completed sessions
;; - Session history browser with filtering and sorting
;; - Session replay (show progress buffer from past session)
;; - Cleanup of old sessions with configurable retention policy
;;
;; Sessions are stored in ~/.emacs.d/.efrit/sessions/ with metadata.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'efrit-log)
(require 'efrit-common)
(require 'efrit-progress)

;;; Customization

(defgroup efrit-session-history nil
  "Session history and archival for Efrit."
  :group 'efrit
  :prefix "efrit-session-history-")

(defcustom efrit-session-history-max-sessions 50
  "Maximum number of sessions to keep.
Older sessions are automatically archived/removed."
  :type 'integer
  :group 'efrit-session-history)

(defcustom efrit-session-history-retention-days 30
  "Keep sessions newer than this many days.
Older sessions may be removed during cleanup."
  :type 'integer
  :group 'efrit-session-history)

(defcustom efrit-session-history-auto-archive t
  "Whether to automatically archive completed sessions."
  :type 'boolean
  :group 'efrit-session-history)

(defcustom efrit-session-history-archive-dir nil
  "Directory for archived sessions. If nil, use default sessions dir."
  :type '(choice (const :tag "Default" nil)
                 (directory :tag "Archive directory"))
  :group 'efrit-session-history)

;;; State

(defvar efrit-session-history--current-sort 'date-desc
  "Current sort order for history display.")

(defvar efrit-session-history--current-filter nil
  "Current filter for history display.")

;;; Session Metadata

(defun efrit-session-history--metadata-file (session-id)
  "Return path to metadata file for SESSION-ID."
  (expand-file-name 
   "metadata.json"
   (expand-file-name session-id
                     (efrit-config-data-file "" "sessions"))))

(defun efrit-session-history--read-progress-file (session-id)
  "Read and parse progress.jsonl for SESSION-ID.
Returns (list start-event end-event events-list)."
  (let ((progress-file (efrit-progress--progress-file session-id))
        (json-object-type 'alist)
        (json-array-type 'vector)
        (json-key-type 'string)
        start-event end-event events)
    (when (file-exists-p progress-file)
      (with-temp-buffer
        (insert-file-contents progress-file)
        (goto-char (point-min))
        (while (not (eobp))
          (let ((line (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))))
            (unless (string-empty-p line)
              (condition-case err
                  (let ((event (json-read-from-string line)))
                    (let ((event-type (alist-get "type" event nil nil 'string-equal)))
                      (cond
                       ((string= event-type "session-start") (setq start-event event))
                       ((string= event-type "session-end") (setq end-event event))
                       (t (push event events)))))
                (error
                 (efrit-log 'warn "Failed to parse event in %s: %s"
                            progress-file (error-message-string err))))))
          (forward-line 1))))
    (list start-event end-event (nreverse events))))

(defun efrit-session-history--compute-metadata (session-id)
  "Compute metadata for SESSION-ID from progress.jsonl.
Returns alist with: command, timestamp, duration, status, tool-count, token-estimate."
  (let* ((progress-data (efrit-session-history--read-progress-file session-id))
         (start-event (nth 0 progress-data))
         (end-event (nth 1 progress-data))
         (all-events (nth 2 progress-data))
         (command (alist-get "command" start-event nil nil 'string-equal))
         (start-time (and start-event
                         (alist-get "timestamp" start-event nil nil 'string-equal)))
         (end-time (and end-event
                       (alist-get "timestamp" end-event nil nil 'string-equal)))
         (duration (if (and start-time end-time)
                       (condition-case nil
                           (float-time
                            (time-subtract
                             (parse-iso8601-time-string end-time)
                             (parse-iso8601-time-string start-time)))
                         (error 0))
                     0))
         (success (if end-event
                      (alist-get "success" end-event :json-false nil 'string-equal)
                    :json-false))
         (tool-count (length (seq-filter (lambda (e)
                                           (string= (alist-get "type" e nil nil 'string-equal)
                                                   "tool-start"))
                                        all-events)))
         (token-estimate (max 100 (* tool-count 500))))  ; Rough estimate
    `((command . ,(or command "Unknown"))
      (timestamp . ,(or start-time ""))
      (duration . ,duration)
      (success . ,(if (eq success :json-false) nil t))
      (tool-count . ,tool-count)
      (token-estimate . ,token-estimate))))

(defun efrit-session-history--save-metadata (session-id)
  "Save computed metadata for SESSION-ID."
  (let ((metadata-file (efrit-session-history--metadata-file session-id))
        (metadata (efrit-session-history--compute-metadata session-id)))
    (make-directory (file-name-directory metadata-file) t)
    (with-temp-file metadata-file
      (insert (json-encode metadata)))
    metadata))

(defun efrit-session-history--load-metadata (session-id)
  "Load metadata for SESSION-ID from file, computing if needed."
  (let ((metadata-file (efrit-session-history--metadata-file session-id)))
    (if (file-exists-p metadata-file)
        (condition-case nil
            (let ((json-object-type 'alist)
                  (json-array-type 'vector)
                  (json-key-type 'string)
                  (content (with-temp-buffer
                             (insert-file-contents metadata-file)
                             (buffer-string))))
              (json-read-from-string content))
          (error
           ;; Recompute if load fails
           (efrit-session-history--save-metadata session-id)))
      ;; Compute if not found
      (efrit-session-history--save-metadata session-id))))

;;; History Management

(defun efrit-session-history--list-all-sessions ()
  "Return list of all session IDs with their metadata."
  (let ((sessions-dir (efrit-config-data-file "" "sessions"))
        sessions)
    (when (file-directory-p sessions-dir)
      (dolist (dir (directory-files sessions-dir t "^[^.]"))
        (when (file-directory-p dir)
          (let ((session-id (file-name-nondirectory dir)))
            (condition-case nil
                (let ((metadata (efrit-session-history--load-metadata session-id)))
                  (push (cons session-id metadata) sessions))
              (error nil))))))
    (nreverse sessions)))

(defun efrit-session-history--sort-sessions (sessions sort-type)
  "Sort SESSIONS according to SORT-TYPE.
Types: date-desc, date-asc, duration, success-first."
  (pcase sort-type
    ('date-desc (sort sessions 
                      (lambda (a b)
                        (string> (alist-get "timestamp" (cdr a) "")
                                (alist-get "timestamp" (cdr b) "")))))
    ('date-asc (sort sessions 
                     (lambda (a b)
                       (string< (alist-get "timestamp" (cdr a) "")
                               (alist-get "timestamp" (cdr b) "")))))
    ('duration (sort sessions
                     (lambda (a b)
                       (> (alist-get "duration" (cdr a) 0)
                          (alist-get "duration" (cdr b) 0)))))
    ('success-first (sort sessions
                          (lambda (a b)
                            (and (alist-get "success" (cdr a) nil)
                                 (not (alist-get "success" (cdr b) nil))))))
    (_ sessions)))

(defun efrit-session-history--filter-sessions (sessions filter-fn)
  "Filter SESSIONS using FILTER-FN."
  (seq-filter filter-fn sessions))

(defun efrit-session-history--cleanup ()
  "Remove old sessions exceeding retention limits."
  (let ((all-sessions (efrit-session-history--list-all-sessions))
        (sessions-dir (efrit-config-data-file "" "sessions")))
    
    ;; Sort by date, keep newest
    (let ((sorted (efrit-session-history--sort-sessions all-sessions 'date-desc))
          (cutoff-time (time-subtract (current-time) 
                                     (days-to-time efrit-session-history-retention-days)))
          (removed-count 0))
      
      ;; Remove sessions exceeding max count
      (when (> (length sorted) efrit-session-history-max-sessions)
        (let ((to-remove (seq-drop sorted efrit-session-history-max-sessions)))
          (dolist (session to-remove)
            (let ((session-dir (expand-file-name (car session) sessions-dir)))
              (when (file-directory-p session-dir)
                (delete-directory session-dir t)
                (cl-incf removed-count))))))
      
      ;; Remove sessions older than retention period
      (dolist (session sorted)
        (let ((timestamp (alist-get "timestamp" (cdr session) "")))
          (when (and (not (string-empty-p timestamp))
                    (condition-case nil
                        (time-less-p (parse-iso8601-time-string timestamp)
                                    cutoff-time)
                      (error nil)))
            (let ((session-dir (expand-file-name (car session) sessions-dir)))
              (when (file-directory-p session-dir)
                (delete-directory session-dir t)
                (cl-incf removed-count))))))
      
      (when (> removed-count 0)
        (efrit-log 'info "Session cleanup: removed %d old sessions" removed-count)))))

;;; User Commands

;;;###autoload
(defun efrit-do-show-history (&optional sort-type)
  "Show session history in a buffer.
SORT-TYPE can be: date-desc (default), date-asc, duration, success-first."
  (interactive (list (if current-prefix-arg
                        (intern (completing-read "Sort by: "
                                               '("date-desc" "date-asc" "duration" "success-first")
                                               nil t "date-desc"))
                      'date-desc)))
  (let* ((sessions (efrit-session-history--list-all-sessions))
         (sorted (efrit-session-history--sort-sessions sessions (or sort-type 'date-desc)))
         (buffer (get-buffer-create "*efrit-history*")))
    
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (efrit-session-history--insert-header sort-type)
        
        (if (null sorted)
            (insert "No sessions found.\n")
          (dolist (session sorted)
            (efrit-session-history--insert-session-line session)))))
    
    (display-buffer buffer)))

(defun efrit-session-history--insert-header (sort-type)
  "Insert header line showing sort type and available commands."
  (insert (propertize "═══ Session History ═══\n" 'face 'bold))
  (insert (format "Sorted by: %s | Total: %d sessions | Keys: RET=view, d=detail, c=cleanup\n"
                  (symbol-name (or sort-type 'date-desc))
                  (length (efrit-session-history--list-all-sessions))))
  (insert (propertize "─────────────────────────────────────\n" 'face 'shadow)))

(defun efrit-session-history--insert-session-line (session)
  "Insert a line for SESSION in history buffer."
  (let* ((session-id (car session))
         (metadata (cdr session))
         (command (alist-get "command" metadata "Unknown"))
         (timestamp (alist-get "timestamp" metadata ""))
         (duration (alist-get "duration" metadata 0))
         (success (alist-get "success" metadata nil))
         (tool-count (alist-get "tool-count" metadata 0))
         (status (if success "✓" "✗"))
         (status-face (if success 'success 'error)))
    
    (insert (format "%s %s | %s | Duration: %.1fs | Tools: %d | %s\n"
                    (propertize status 'face status-face)
                    (propertize (substring session-id 0 16) 'face 'font-lock-string-face)
                    (propertize (efrit-common-truncate-string command 40) 'face 'default)
                    duration
                    tool-count
                    (propertize (substring timestamp 0 10) 'face 'font-lock-comment-face)))))

;;;###autoload
(defun efrit-do-open-session ()
  "Open/view a session from history."
  (interactive)
  (let* ((sessions (efrit-session-history--list-all-sessions))
         (choices (mapcar (lambda (s)
                           (format "%s: %s (%.1fs)"
                                  (car s)
                                  (alist-get "command" (cdr s) "Unknown")
                                  (alist-get "duration" (cdr s) 0)))
                         sessions))
         (selected (completing-read "Session: " choices nil t))
         (session-id (car (split-string selected ":"))))
    (efrit-progress-show-session session-id)))

;;;###autoload
(defun efrit-do-replay-session (session-id)
  "Show the progress buffer from past SESSION-ID."
  (interactive (list (let ((sessions (mapcar #'car (efrit-session-history--list-all-sessions))))
                       (if sessions
                           (completing-read "Session: " sessions nil t)
                         (user-error "No sessions found")))))
  (efrit-progress-show-session session-id))

;;;###autoload
(defun efrit-do-clear-history ()
  "Interactively clear old sessions from history."
  (interactive)
  (let* ((sessions (efrit-session-history--list-all-sessions))
         (count (length sessions))
         (response (read-char-choice 
                   (format "Clear sessions? (%d total, keep %d newest): (y)es, (c)lean old, (q)uit? "
                          count efrit-session-history-max-sessions)
                   '(?y ?c ?q))))
    (cond
     ((eq response ?y)
      (let ((sessions-dir (efrit-config-data-file "" "sessions")))
        (dolist (session sessions)
          (delete-directory (expand-file-name (car session) sessions-dir) t))
        (message "Cleared %d sessions" count)))
     
     ((eq response ?c)
      (efrit-session-history--cleanup)
      (message "Cleanup complete"))
     
     ((eq response ?q)
      (message "Cancelled")))))

;;; Lifecycle Integration

(defun efrit-session-history--on-session-complete (session-id)
  "Called when SESSION-ID completes to archive it."
  (when efrit-session-history-auto-archive
    (efrit-session-history--save-metadata session-id)
    (efrit-log 'debug "Session %s archived" session-id)))

;;; Helper for progress module

(defun efrit-progress-show-session (session-id)
  "Show progress events from SESSION-ID in a buffer.
Helper for session history viewing."
  (let* ((progress-data (efrit-session-history--read-progress-file session-id))
         (start-event (nth 0 progress-data))
         (all-events (nth 2 progress-data))
         (buffer (get-buffer-create (format "*efrit-session-%s*" 
                                           (substring session-id 0 8)))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (special-mode)
        
        ;; Header
        (insert (propertize (format "Session: %s\n" session-id) 'face 'bold))
        (let ((command (alist-get "command" start-event nil nil 'string-equal)))
          (when command
            (insert (format "Command: %s\n" command))))
        (insert (propertize "─────────────────────────────────────\n" 'face 'shadow))
        
        ;; Events
        (dolist (event all-events)
          (let ((event-type (alist-get "type" event nil nil 'string-equal)))
            (pcase event-type
              ("tool-start"
               (insert (format "▶ %s\n"
                             (alist-get "tool" event nil nil 'string-equal))))
              ("tool-result"
               (let ((success (alist-get "success" event t nil 'string-equal)))
                 (insert (format "  ◀ %s\n"
                               (if (eq success :json-false) "✗" "✓")))))
              (_ nil))))))
    (display-buffer buffer)))

(provide 'efrit-session-history)

;;; efrit-session-history.el ends here
