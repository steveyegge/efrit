;;; efrit-session-transcript.el --- Session transcript persistence and resume -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.3.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; Session transcript persistence for Efrit.
;; Provides:
;; - Full conversation transcript save/load
;; - Session listing and browsing
;; - Session resume functionality
;;
;; Unlike the metrics-only persistence, this stores actual messages
;; to enable session resume.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'efrit-log)
(require 'efrit-common)
(require 'efrit-config)
(require 'efrit-session-core)

;; Optional require for ISO8601 parsing (Emacs 27+)
(require 'iso8601 nil t)

;; Forward declarations
(defvar efrit-do--resume-messages)
(defvar efrit-do--resume-continuation)
(declare-function efrit-do "efrit-do" (command))

;;; Customization

(defcustom efrit-transcript-directory nil
  "Directory for session transcripts.
If nil, uses `efrit-data-directory'/transcripts."
  :type '(choice (const :tag "Default location" nil)
                 (directory :tag "Custom directory"))
  :group 'efrit-session)

(defcustom efrit-transcript-max-age-days 30
  "Maximum age in days for session transcripts before cleanup.
Set to nil to disable automatic cleanup."
  :type '(choice (integer :tag "Days")
                 (const :tag "Never cleanup" nil))
  :group 'efrit-session)

;;; Resume Variables
;; These are set by efrit-resume-session and consumed by efrit-do

(defvar efrit-do--resume-messages nil
  "Prior messages to inject when resuming a session.
Set by `efrit-resume-session', consumed by `efrit-do'.")

(defvar efrit-do--resume-continuation nil
  "The continuation prompt for a resumed session.")

;;; Transcript Storage

(defun efrit-transcript-directory ()
  "Return the transcript storage directory, creating if needed."
  (let ((dir (or efrit-transcript-directory
                 (expand-file-name "transcripts" efrit-data-directory))))
    (unless (file-directory-p dir)
      (make-directory dir t))
    dir))

(defun efrit-transcript--extract-snippet (session)
  "Extract a display snippet from SESSION's first user message.
Returns first 80 chars of the command."
  (let ((command (efrit-session-command session)))
    (if (and command (> (length command) 0))
        (let ((first-line (car (split-string command "\n" t))))
          (if (> (length first-line) 80)
              (concat (substring first-line 0 77) "...")
            first-line))
      "(no command)")))

(defun efrit-transcript--session-to-transcript (session)
  "Convert SESSION struct to a transcript alist for persistence."
  (let* ((messages (efrit-session-api-messages session))
         (message-count (length messages)))
    `((id . ,(efrit-session-id session))
      (created_at . ,(format-time-string "%Y-%m-%dT%H:%M:%SZ"
                                         (efrit-session-start-time session)))
      (updated_at . ,(format-time-string "%Y-%m-%dT%H:%M:%SZ"))
      (project_dir . ,default-directory)
      (message_count . ,message-count)
      (snippet . ,(efrit-transcript--extract-snippet session))
      (command . ,(efrit-session-command session))
      (status . ,(symbol-name (efrit-session-status session)))
      (messages . ,(vconcat messages)))))

(defun efrit-transcript-save (session)
  "Save SESSION's conversation transcript to disk.
Creates a JSON file with full conversation history for resume."
  (when (and session (efrit-session-api-messages session))
    (let* ((transcript-dir (efrit-transcript-directory))
           (session-id (efrit-session-id session))
           (transcript-file (expand-file-name
                            (concat session-id ".json")
                            transcript-dir))
           (transcript-data (efrit-transcript--session-to-transcript session)))
      (condition-case err
          (with-temp-file transcript-file
            (insert (json-encode transcript-data))
            (efrit-log 'debug "Saved transcript: %s (%d messages)"
                      session-id
                      (length (efrit-session-api-messages session))))
        (error
         (efrit-log 'error "Failed to save transcript %s: %s"
                   session-id (error-message-string err)))))))

(defun efrit-transcript-load (session-id)
  "Load transcript for SESSION-ID from disk.
Returns an alist with id, messages, snippet, etc., or nil if not found."
  (let ((transcript-file (expand-file-name
                         (concat session-id ".json")
                         (efrit-transcript-directory))))
    (when (file-exists-p transcript-file)
      (condition-case err
          (with-temp-buffer
            (insert-file-contents transcript-file)
            (json-read-from-string (buffer-string)))
        (error
         (efrit-log 'warn "Failed to load transcript %s: %s"
                   session-id (error-message-string err))
         nil)))))

(defun efrit-transcript-list ()
  "List all available transcripts with metadata.
Returns a list of alists, each with id, snippet, created_at, message_count.
Sorted by recency (most recent first)."
  (let* ((transcript-dir (efrit-transcript-directory))
         (files (when (file-directory-p transcript-dir)
                  (directory-files transcript-dir t "\\.json$")))
         (transcripts nil))
    (dolist (file files)
      (condition-case nil
          (with-temp-buffer
            (insert-file-contents file)
            (let ((data (json-read-from-string (buffer-string))))
              (push `((id . ,(alist-get 'id data))
                     (snippet . ,(alist-get 'snippet data))
                     (created_at . ,(alist-get 'created_at data))
                     (updated_at . ,(alist-get 'updated_at data))
                     (message_count . ,(alist-get 'message_count data))
                     (project_dir . ,(alist-get 'project_dir data))
                     (file . ,file))
                    transcripts)))
        (error nil)))  ; Skip unreadable files
    ;; Sort by updated_at descending
    (sort transcripts
          (lambda (a b)
            (string> (or (alist-get 'updated_at a) "")
                     (or (alist-get 'updated_at b) ""))))))

;;; Time Formatting

(defun efrit-transcript--format-time-ago (iso-time)
  "Format ISO-TIME string as human-readable time ago."
  (if (not iso-time)
      "unknown"
    (condition-case nil
        (let* ((parsed (if (fboundp 'iso8601-parse)
                          (iso8601-parse iso-time)
                        ;; Fallback: parse manually
                        nil))
               (time (cond
                      ;; iso8601-parse returns a decoded-time, encode it
                      ((and parsed (listp parsed))
                       (encode-time parsed))
                      ;; Try date-to-time as fallback
                      (t (date-to-time (replace-regexp-in-string "T" " " iso-time)))))
               (diff (float-time (time-subtract (current-time) time))))
          (cond
           ((< diff 60) "just now")
           ((< diff 3600) (format "%d minutes ago" (/ diff 60)))
           ((< diff 86400) (format "%d hours ago" (/ diff 3600)))
           ((< diff 604800) (format "%d days ago" (/ diff 86400)))
           (t (format-time-string "%Y-%m-%d" time))))
      (error "unknown"))))

(defun efrit-transcript--annotation-function (candidate)
  "Annotation function for `efrit-list-sessions' completion.
CANDIDATE is the session ID string."
  (when-let* ((data (get-text-property 0 'efrit-transcript candidate)))
    (let ((time-ago (efrit-transcript--format-time-ago
                     (alist-get 'updated_at data)))
          (count (or (alist-get 'message_count data) 0))
          (project (alist-get 'project_dir data)))
      (format "  %s - %d msgs - %s"
              time-ago count
              (if project
                  (file-name-nondirectory (directory-file-name project))
                "-")))))

;;; User Commands

;;;###autoload
(defun efrit-list-sessions ()
  "List and select from saved session transcripts.
Uses completing-read with annotations showing time ago and message count."
  (interactive)
  (let* ((transcripts (efrit-transcript-list))
         (candidates
          (mapcar (lambda (tr)
                    (let ((id (alist-get 'id tr))
                          (snippet (or (alist-get 'snippet tr) "(no snippet)")))
                      (propertize (format "%s: %s" id snippet)
                                  'efrit-transcript tr
                                  'efrit-session-id id)))
                  transcripts)))
    (if (null candidates)
        (message "No saved sessions found")
      (let* ((completion-extra-properties
              '(:annotation-function efrit-transcript--annotation-function))
             (selected (completing-read "Session: " candidates nil t)))
        (when selected
          (let ((session-id (get-text-property 0 'efrit-session-id selected)))
            (efrit-show-transcript session-id)))))))

;;;###autoload
(defun efrit-show-transcript (session-id)
  "Display the transcript for SESSION-ID in a buffer."
  (interactive
   (list (let* ((transcripts (efrit-transcript-list))
                (ids (mapcar (lambda (tr) (alist-get 'id tr)) transcripts)))
           (completing-read "Session ID: " ids nil t))))
  (let ((transcript (efrit-transcript-load session-id)))
    (if (not transcript)
        (message "Transcript not found: %s" session-id)
      (with-current-buffer (get-buffer-create
                           (format "*efrit-transcript: %s*" session-id))
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "Session: %s\n" (alist-get 'id transcript)))
          (insert (format "Created: %s\n" (alist-get 'created_at transcript)))
          (insert (format "Project: %s\n" (or (alist-get 'project_dir transcript) "unknown")))
          (insert (format "Messages: %d\n" (or (alist-get 'message_count transcript) 0)))
          (insert (make-string 60 ?-) "\n\n")
          ;; Display messages
          (let ((messages (alist-get 'messages transcript)))
            (when (vectorp messages)
              (dotimes (i (length messages))
                (let* ((msg (aref messages i))
                       (role (alist-get 'role msg))
                       (content (alist-get 'content msg)))
                  (insert (format "## %s\n\n" (upcase role)))
                  (cond
                   ((stringp content)
                    (insert content))
                   ((vectorp content)
                    ;; Handle tool_use/tool_result blocks
                    (dotimes (j (length content))
                      (let* ((block (aref content j))
                             (type (alist-get 'type block)))
                        (cond
                         ((string= type "text")
                          (insert (or (alist-get 'text block) "")))
                         ((string= type "tool_use")
                          (insert (format "[Tool: %s]\n" (alist-get 'name block))))
                         ((string= type "tool_result")
                          (insert (format "[Result for %s]\n"
                                         (alist-get 'tool_use_id block))))))))
                   (t
                    (insert (format "%S" content))))
                  (insert "\n\n")))))
          (goto-char (point-min))
          (view-mode))
        (display-buffer (current-buffer))))))

;;;###autoload
(defun efrit-resume-session (session-id)
  "Resume a previous session by loading its messages into a new efrit-do call.
SESSION-ID identifies the transcript to resume from."
  (interactive
   (list (let* ((transcripts (efrit-transcript-list))
                (candidates
                 (mapcar (lambda (tr)
                           (let ((id (alist-get 'id tr))
                                 (snippet (or (alist-get 'snippet tr) "")))
                             (propertize (format "%s: %s" id
                                                (truncate-string-to-width snippet 60))
                                        'efrit-session-id id)))
                         transcripts)))
           (if (null candidates)
               (user-error "No saved sessions to resume")
             (let ((selected (completing-read "Resume session: " candidates nil t)))
               (get-text-property 0 'efrit-session-id selected))))))
  (let ((transcript (efrit-transcript-load session-id)))
    (if (not transcript)
        (user-error "Transcript not found: %s" session-id)
      (let* ((messages (alist-get 'messages transcript))
             ;; Create a continuation prompt
             (continue-prompt (read-string
                              "Continue with: "
                              "Please continue from where we left off.")))
        ;; Store the prior messages for efrit-do to pick up
        (setq efrit-do--resume-messages
              (if (vectorp messages) (append messages nil) messages))
        (setq efrit-do--resume-continuation continue-prompt)
        (message "Resuming session %s with %d prior messages"
                session-id (length efrit-do--resume-messages))
        ;; Start efrit-do with the continuation
        (efrit-do continue-prompt)))))

;;; Cleanup

(defun efrit-transcript-cleanup-old ()
  "Remove transcripts older than `efrit-transcript-max-age-days'."
  (interactive)
  (when efrit-transcript-max-age-days
    (let* ((cutoff-time (time-subtract (current-time)
                                       (* efrit-transcript-max-age-days 86400)))
           (transcript-dir (efrit-transcript-directory))
           (files (directory-files transcript-dir t "\\.json$"))
           (removed 0))
      (dolist (file files)
        (when (time-less-p (file-attribute-modification-time
                           (file-attributes file))
                          cutoff-time)
          (delete-file file)
          (cl-incf removed)))
      (when (> removed 0)
        (efrit-log 'info "Cleaned up %d old transcripts" removed))
      removed)))

;;; Auto-save Hook

(defun efrit-transcript--on-session-complete (session _result)
  "Auto-save transcript when SESSION completes."
  (efrit-transcript-save session))

;; Hook into session completion
(add-hook 'efrit-session-completed-hook #'efrit-transcript--on-session-complete)

(provide 'efrit-session-transcript)

;;; efrit-session-transcript.el ends here
