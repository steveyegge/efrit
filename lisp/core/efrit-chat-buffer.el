;;; efrit-chat-buffer.el --- Buffer management for Efrit chat -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai
;; URL: https://github.com/steveyegge/efrit

;;; Commentary:
;; Buffer management, display, and UI functions for Efrit chat.
;; Handles buffer creation, message display, work buffer logging, and mode definition.

;;; Code:

(require 'json)
(require 'efrit-config)

;; Declare functions from other modules to avoid warnings
(declare-function efrit-chat--setup-persistence "efrit-chat-persistence")

;;; Customization

(defcustom efrit-buffer-name "*efrit-chat*"
  "Name of the Efrit conversation buffer."
  :type 'string
  :group 'efrit)

(defcustom efrit-work-buffer-name "*efrit-work*"
  "Name of the buffer for detailed work/thinking (streamlined mode)."
  :type 'string
  :group 'efrit)

(defcustom efrit-show-timestamps nil
  "Whether to show timestamps in chat messages.
When non-nil, each message will be prefixed with a timestamp."
  :type 'boolean
  :group 'efrit)

(defcustom efrit-show-work-buffer nil
  "Whether to automatically show the work buffer during operations."
  :type 'boolean
  :group 'efrit)

(defcustom efrit-work-buffer-max-size 100000
  "Maximum size of work buffer before truncation (0 = no limit)."
  :type 'integer
  :group 'efrit)

;;; Internal variables - Classic Chat Mode

(defvar-local efrit--conversation-marker nil
  "Marker for the end of the conversation area.")

(defvar-local efrit--input-marker nil
  "Marker for the start of the input area.")

(defvar-local efrit--response-in-progress nil
  "Flag indicating whether a response is in progress.")

;;; Faces

(defface efrit-user-face
  '((t :inherit font-lock-keyword-face))
  "Face for user messages in Efrit buffer."
  :group 'efrit)

(defface efrit-assistant-face
  '((t :inherit font-lock-doc-face))
  "Face for assistant responses in Efrit buffer."
  :group 'efrit)

(defface efrit-system-face
  '((t :inherit font-lock-comment-face))
  "Face for system messages in Efrit buffer."
  :group 'efrit)

(defface efrit-prompt-face
  '((t :inherit minibuffer-prompt :weight bold))
  "Face for the input prompt."
  :group 'efrit)

;;; Buffer management - Classic Chat Mode

(defun efrit--setup-buffer ()
  "Create and set up the Efrit buffer."
  (let ((buffer (get-buffer-create efrit-buffer-name)))
    (with-current-buffer buffer
      (unless (eq major-mode 'efrit-mode)
        (efrit-mode))
      ;; Always ensure buffer is editable
      (setq buffer-read-only nil)
      ;; Ensure we have markers for conversation and input areas
      (when (not efrit--conversation-marker)
        (setq-local efrit--conversation-marker (make-marker)))
      (when (not efrit--input-marker)
        (setq-local efrit--input-marker (make-marker))))
    buffer))

(defun efrit--insert-prompt ()
  "Insert a prompt in the buffer for user input."
  (with-current-buffer (efrit--setup-buffer)
    ;; Always ensure buffer is editable
    (setq buffer-read-only nil)
    (let ((inhibit-read-only t))
      ;; Go to end of buffer
      (goto-char (point-max))

      ;; Add spacing before input area (single newline since message already has one)
      (unless (bobp) (insert "\n"))

      ;; Set the marker for beginning of input area
      (set-marker efrit--input-marker (point))

      ;; Insert the prompt
      (let ((prompt-start (point)))
        (insert "> ")
        (add-text-properties prompt-start (point) '(face efrit-prompt-face)))

      ;; Ensure buffer is left editable
      (setq buffer-read-only nil))))

(defun efrit--display-message (message role)
  "Display MESSAGE in the Efrit buffer with ROLE (user, assistant, or system)."
  (with-current-buffer (efrit--setup-buffer)
    ;; Always ensure buffer is editable
    (setq buffer-read-only nil)
    (let ((inhibit-read-only t)
          (face (cond
                ((eq role 'user) 'efrit-user-face)
                ((eq role 'assistant) 'efrit-assistant-face)
                ((eq role 'system) 'efrit-system-face)
                (t nil)))
          (prefix (cond
                  ((eq role 'user) "You: ")
                  ((eq role 'assistant) "Assistant: ")
                  ((eq role 'system) "System: ")
                  (t "")))
          (timestamp (when efrit-show-timestamps
                      (format-time-string "[%H:%M:%S] "))))
      ;; All messages go to the conversation area (before input area)
      (if (and efrit--conversation-marker (marker-position efrit--conversation-marker))
          (goto-char (marker-position efrit--conversation-marker))
        ;; Fallback: go to input marker or end of buffer
        (if (and efrit--input-marker (marker-position efrit--input-marker))
            (goto-char (marker-position efrit--input-marker))
          (goto-char (point-max))))

      ;; Add spacing before message (single newline to separate from previous content)
      (unless (bobp) (insert "\n"))

      ;; Insert the message with appropriate prefix and optional timestamp
      (let ((start (point)))
        (when timestamp (insert timestamp))
        (insert prefix message "\n")  ; Add newline after message
        (when face
          (add-text-properties start (- (point) 1) `(face ,face))))  ; Don't apply face to newline

      ;; Update conversation marker to end of this message
      (set-marker efrit--conversation-marker (point))

      ;; Ensure buffer is editable
      (setq buffer-read-only nil))))

;;; Work Buffer Management - Streamlined Mode

(defun efrit-streamlined--get-work-buffer ()
  "Get or create the work buffer for detailed operations."
  (let ((buffer (get-buffer-create efrit-work-buffer-name)))
    (with-current-buffer buffer
      (unless (derived-mode-p 'efrit-work-mode)
        (efrit-work-mode)))
    buffer))

(defun efrit-streamlined--log-to-work (message)
  "Log MESSAGE to the work buffer with timestamp."
  (let ((work-buffer (efrit-streamlined--get-work-buffer)))
    (with-current-buffer work-buffer
      (goto-char (point-max))
      (insert (format "[%s] %s\n"
                      (format-time-string "%H:%M:%S")
                      message))

      ;; Truncate buffer if it gets too large
      (when (and (> efrit-work-buffer-max-size 0)
                 (> (buffer-size) efrit-work-buffer-max-size))
        (goto-char (point-min))
        (forward-line (/ (count-lines (point-min) (point-max)) 2))
        (delete-region (point-min) (point))
        (goto-char (point-min))
        (insert (format "[%s] === Work buffer truncated (size limit: %d) ===\n"
                        (format-time-string "%H:%M:%S")
                        efrit-work-buffer-max-size))
        (goto-char (point-max)))

      (when efrit-show-work-buffer
        (display-buffer work-buffer)))))

(define-derived-mode efrit-work-mode fundamental-mode "Efrit-Work"
  "Major mode for the Efrit work buffer."
  (setq buffer-read-only nil)
  (font-lock-mode 1))

;;; Utility functions for text processing

(defun efrit--sanitize-chat-text (text)
  "Remove technical artifacts from TEXT for clean chat display."
  (let ((cleaned text))
    ;; Remove any remaining [Result: ...] tags
    (setq cleaned (replace-regexp-in-string "\\[Result:[^]]*\\]" "" cleaned))
    ;; Remove empty lines and excessive whitespace
    (setq cleaned (replace-regexp-in-string "\n\n+" "\n\n" cleaned))
    (string-trim cleaned)))

(defun efrit--safe-error-message (err)
  "Safely get error message from ERR, returning \\='Unknown error\\=' if nil."
  (cond
   ((null err) "Unknown error (nil)")
   ((stringp err) err)
   ((and (listp err) (car err))
    (condition-case nil
        (error-message-string err)
      (error "Unknown error (failed to extract message)")))
   (t "Unknown error (invalid format)")))

;;; Mode definition for chat

(defvar efrit-mode-map)

(define-derived-mode efrit-mode text-mode "Efrit"
  "Major mode for interacting with the Efrit conversational assistant."
  ;; Initialize buffer-local variables
  (setq-local efrit--conversation-marker nil)
  (setq-local efrit--input-marker nil)
  (setq-local efrit--response-in-progress nil)
  ;; Enable line wrapping
  (visual-line-mode 1)
  ;; Set up session persistence
  (require 'efrit-chat-persistence)
  (efrit-chat--setup-persistence))

(define-derived-mode efrit-chat-mode fundamental-mode "Efrit-Chat"
  "Major mode for Efrit chat."
  (setq buffer-read-only nil))

(defun efrit-streamlined--setup-chat-mode ()
  "Setup basic chat mode for the buffer."
  (efrit-chat-mode)
  (goto-char (point-max))
  (when (= (point-min) (point-max))
    (insert "Efrit Chat (Streamlined)\n")
    (insert "========================\n\n")))

(defun efrit-streamlined--display-response (content)
  "Display CONTENT in chat buffer."
  (let* ((buffer-name efrit-buffer-name)
         (chat-buffer (get-buffer-create buffer-name)))
    (with-current-buffer chat-buffer
      (unless (derived-mode-p 'efrit-chat-mode)
        (efrit-streamlined--setup-chat-mode))

      (goto-char (point-max))
      (insert (format "\nA: %s\n\n" content))
      (goto-char (point-max)))))

(provide 'efrit-chat-buffer)

;;; efrit-chat-buffer.el ends here
