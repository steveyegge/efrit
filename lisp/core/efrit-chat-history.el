;;; efrit-chat-history.el --- Chat history browser and navigation -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.4.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; Enhanced chat history browser with preview, search, and navigation.
;; Extends the basic persistence module with:
;; - Session preview (show recent messages)
;; - Search by content/timestamp
;; - Export conversations to files
;; - Branching (resume conversation from specific point)

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'json)
(require 'efrit-log)
(require 'efrit-chat-persistence)

;; Forward declarations
(declare-function efrit-chat-restore "efrit-chat-persistence")
(declare-function efrit-chat-load-session "efrit-chat-persistence")
(declare-function efrit-chat-list-sessions "efrit-chat-persistence")

;;; Session Preview

(defun efrit-chat-history--format-session-preview (session-id)
  "Return a formatted preview of SESSION-ID's content."
  (let ((session (efrit-chat-load-session session-id)))
    (if (not session)
        "Session not found"
      (let* ((messages (alist-get 'messages session))
             (count (alist-get 'message_count session))
             (lines '("=" "Session Preview" "=")))
        ;; Add metadata
        (push (format "ID: %s" (alist-get 'id session)) lines)
        (push (format "Messages: %d" count) lines)
        (push (format "Created: %s" (alist-get 'created_at session)) lines)
        (push (format "Model: %s" (alist-get 'model session)) lines)
        (push "" lines)
        
        ;; Add last few messages for context
        (push "Recent messages:" lines)
        (push "-" lines)
        
        (when (vectorp messages)
          (let* ((start (max 0 (- (length messages) 4)))
                 (recent (seq-subseq messages start)))
            (dotimes (i (length recent))
              (let* ((msg (aref recent i))
                     (role (alist-get 'role msg))
                     (content (alist-get 'content msg))
                     (preview (if (> (length content) 100)
                                  (concat (substring content 0 97) "...")
                                content)))
                (push (format "%s: %s" (upcase role) preview) lines)))))
        
        (mapconcat #'identity (reverse lines) "\n")))))

;;;###autoload
(defun efrit-chat-history-preview ()
  "Preview a chat session before restoring it."
  (interactive)
  (let* ((sessions (efrit-chat-list-sessions))
         (session (car sessions)))
    (if (not session)
        (message "No chat sessions available")
      (let ((session-id (alist-get 'id session)))
        (let ((buf (get-buffer-create "*efrit-history-preview*")))
          (with-current-buffer buf
            (erase-buffer)
            (insert (efrit-chat-history--format-session-preview session-id))
            (read-only-mode t))
          (pop-to-buffer buf)
          (message "Preview of most recent session. Use M-x efrit-chat-list to restore."))))))

;;; Search and Filter

;;;###autoload
(defun efrit-chat-history-search (query)
  "Search chat history for QUERY (searches snippets and message counts)."
  (interactive "sSearch chat history: ")
  (let* ((sessions (efrit-chat-list-sessions))
         (matches (seq-filter
                   (lambda (s)
                     (let ((snippet (alist-get 'snippet s))
                           (id (alist-get 'id s)))
                       (or (string-match-p (regexp-quote query) (or snippet ""))
                           (string-match-p (regexp-quote query) id))))
                   sessions)))
    (if (null matches)
        (message "No chat sessions match '%s'" query)
      (let ((buf (get-buffer-create "*efrit-history-search*")))
        (with-current-buffer buf
          (setq buffer-read-only nil)
          (erase-buffer)
          (insert (format "Search Results for '%s' (%d matches)\n" query (length matches)))
          (insert "=" (make-string 50 ?=) "\n\n")
          
          (dolist (session matches)
            (let ((id (alist-get 'id session))
                  (snippet (alist-get 'snippet session))
                  (count (alist-get 'message_count session))
                  (timestamp (alist-get 'updated_at session)))
              (insert (format "• %s\n" id))
              (insert (format "  Snippet: %s\n" (truncate-string-to-width snippet 70)))
              (insert (format "  Messages: %d | Updated: %s\n\n" count timestamp))))
          
          (setq buffer-read-only t))
        (pop-to-buffer buf)))))

;;; Export

;;;###autoload
(defun efrit-chat-history-export-session (session-id filename)
  "Export chat SESSION-ID to text file at FILENAME."
  (interactive
   (list
    (let* ((sessions (efrit-chat-list-sessions))
           (ids (mapcar (lambda (s) (alist-get 'id s)) sessions)))
      (if (null ids)
          (user-error "No saved chat sessions")
        (completing-read "Export session: " ids nil t)))
    (read-file-name "Export to file: " (expand-file-name "~") nil nil "chat-export.txt")))
  
  (let ((session (efrit-chat-load-session session-id)))
    (if (not session)
        (message "Session not found: %s" session-id)
      (let ((messages (alist-get 'messages session))
            (lines '()))
        ;; Add header
        (push (concat "Chat Session: " session-id) lines)
        (push (concat "Created: " (alist-get 'created_at session)) lines)
        (push (concat "Model: " (alist-get 'model session)) lines)
        (push (concat "Messages: " (number-to-string (alist-get 'message_count session))) lines)
        (push "" lines)
        (push (make-string 70 ?=) lines)
        (push "" lines)
        
        ;; Add messages
        (when (vectorp messages)
          (dotimes (i (length messages))
            (let* ((msg (aref messages i))
                   (role (alist-get 'role msg))
                   (content (alist-get 'content msg)))
              (push (concat (upcase role) ":") lines)
              (push content lines)
              (push "" lines))))
        
        ;; Write to file
        (condition-case err
            (progn
              (with-temp-file filename
                (insert (mapconcat #'identity (reverse lines) "\n")))
              (message "Exported chat to: %s" filename))
          (error
           (message "Failed to export chat: %s" (error-message-string err))))))))

;;; List with Better Display

;;;###autoload
(defun efrit-chat-history-list-enhanced ()
  "Display chat history with better formatting and options."
  (interactive)
  (let ((sessions (efrit-chat-list-sessions)))
    (if (null sessions)
        (message "No chat sessions available")
      (let ((buf (get-buffer-create "*efrit-chat-history*")))
        (with-current-buffer buf
          (setq buffer-read-only nil)
          (erase-buffer)
          (insert "Chat History\n")
          (insert "=" (make-string 70 ?=) "\n\n")
          
          (let ((counter 1))
            (dolist (session sessions)
              (let ((id (alist-get 'id session))
                    (snippet (alist-get 'snippet session))
                    (count (alist-get 'message_count session))
                    (created (alist-get 'created_at session))
                    (timestamp (alist-get 'updated_at session)))
                (insert (format "%2d. [%s]\n" counter id))
                (insert (format "    Snippet: %s\n" (truncate-string-to-width snippet 65)))
                (insert (format "    Messages: %d  |  Created: %s\n" count created))
                (insert (format "    Updated: %s\n" timestamp))
                (insert "\n")
                (setq counter (1+ counter)))))
          
          (setq buffer-read-only t)
          (goto-char (point-min)))
        (pop-to-buffer buf)))))

(provide 'efrit-chat-history)

;;; efrit-chat-history.el ends here
