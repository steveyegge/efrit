;;; efrit-agent-render.el --- Message rendering for efrit-agent -*- lexical-binding: t -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; Rendering module for efrit-agent providing:
;; - User and Claude message rendering
;; - Streaming display for Claude responses
;; - Thinking indicator display and management
;; - Header-line formatting

;;; Code:

(require 'cl-lib)
(require 'efrit-agent-core)

;;; Message Rendering
;;
;; Functions to add messages to the conversation region without full re-render.
;; These use efrit-agent--append-to-conversation for incremental updates.

(defun efrit-agent--add-user-message (text)
  "Add a user message with TEXT to the conversation region.
User messages are prefixed with `> ' and have proper faces applied.
Multi-line messages have continuation lines indented."
  ;; End any streaming Claude message first
  (efrit-agent--stream-end-message)
  ;; Hide thinking indicator when user sends message
  (efrit-agent--hide-thinking)
  (let* ((msg-id (format "user-msg-%d" (cl-incf efrit-agent--message-counter)))
         (lines (split-string text "\n"))
         (first-line (car lines))
         (rest-lines (cdr lines))
         (formatted-text
          (concat
           ;; First line with > prefix
           (propertize "> " 'face 'efrit-agent-user-prefix)
           (propertize first-line 'face 'efrit-agent-user-message)
           ;; Continuation lines with indentation
           (when rest-lines
             (mapconcat
              (lambda (line)
                (concat "\n  " (propertize line 'face 'efrit-agent-user-message)))
              rest-lines
              ""))
           "\n\n")))
    (efrit-agent--append-to-conversation
     formatted-text
     (list 'efrit-type 'user-message
           'efrit-id msg-id))))

(defun efrit-agent--add-claude-message (text)
  "Add a Claude message with TEXT to the conversation region.
If there's an active streaming message, append to it.
Otherwise start a new message.
Claude messages appear inline without a prefix."
  (if efrit-agent--streaming-message
      ;; Append to existing streaming message
      (efrit-agent--stream-append-text text)
    ;; Start a new message
    (efrit-agent--stream-start-message text)))

(defun efrit-agent--stream-start-message (text)
  "Start a new streaming Claude message with TEXT.
Creates markers for tracking the message region for future appends."
  ;; Hide thinking indicator when Claude starts responding
  (efrit-agent--hide-thinking)
  (let* ((msg-id (format "claude-msg-%d" (cl-incf efrit-agent--message-counter)))
         (inhibit-read-only t)
         start-marker end-marker)
    ;; Move to end of conversation region
    (save-excursion
      (goto-char (marker-position efrit-agent--conversation-end))
      (setq start-marker (point-marker))
      ;; Insert the text
      (insert (propertize text 'face 'efrit-agent-claude-message))
      (setq end-marker (point-marker))
      ;; Set the marker type (insert before for end-marker)
      (set-marker-insertion-type end-marker t)
      ;; Apply properties
      (add-text-properties start-marker end-marker
                           (list 'efrit-type 'claude-message
                                 'efrit-id msg-id
                                 'read-only t))
      ;; Update conversation end marker
      (set-marker efrit-agent--conversation-end (point)))
    ;; Track the streaming message
    (setq efrit-agent--streaming-message
          (list msg-id start-marker end-marker))
    ;; Scroll to show new content
    (efrit-agent--scroll-to-bottom)))

(defun efrit-agent--stream-append-text (text)
  "Append TEXT to the current streaming Claude message.
Updates the message region markers."
  (when efrit-agent--streaming-message
    (let* ((msg-id (nth 0 efrit-agent--streaming-message))
           (end-marker (nth 2 efrit-agent--streaming-message))
           (inhibit-read-only t))
      (save-excursion
        ;; Insert at end marker (before it, due to insertion type)
        (goto-char (marker-position end-marker))
        ;; Move back before the marker to insert
        (let ((insert-pos (1- (point))))
          (when (> insert-pos (point-min))
            (goto-char insert-pos)))
        ;; Insert the new text
        (insert (propertize text 'face 'efrit-agent-claude-message))
        ;; Update properties on the new text
        (add-text-properties (- (point) (length text)) (point)
                             (list 'efrit-type 'claude-message
                                   'efrit-id msg-id
                                   'read-only t))
        ;; Update conversation end if needed
        (when (> (point) (marker-position efrit-agent--conversation-end))
          (set-marker efrit-agent--conversation-end (point))))
      ;; Scroll to show new content
      (efrit-agent--scroll-to-bottom))))

(defun efrit-agent--stream-end-message ()
  "End the current streaming message, adding final newlines.
Call this when Claude's message is complete."
  (when efrit-agent--streaming-message
    (let ((inhibit-read-only t)
          (end-marker (nth 2 efrit-agent--streaming-message)))
      (save-excursion
        (goto-char (marker-position end-marker))
        ;; Add trailing newlines for spacing
        (insert "\n\n")
        ;; Update conversation end
        (set-marker efrit-agent--conversation-end (point)))
      ;; Clear streaming state
      (setq efrit-agent--streaming-message nil))))

(defun efrit-agent--scroll-to-bottom ()
  "Scroll the agent buffer window to show the latest content."
  (when-let* ((window (get-buffer-window (current-buffer))))
    (with-selected-window window
      (goto-char (point-max))
      (recenter -3))))

;;; Thinking Indicator
;;
;; Shows when Claude is processing but no tool is running.
;; Disappears when content starts arriving.

(defun efrit-agent--show-thinking (&optional text)
  "Show the thinking indicator with optional TEXT description.
If TEXT is nil, shows just '[thinking...]'.
The indicator is removed when content arrives or explicitly hidden."
  (when (and (not efrit-agent--thinking-indicator)
             efrit-agent--conversation-end
             (marker-position efrit-agent--conversation-end))
    (let ((inhibit-read-only t)
          start-marker end-marker)
      (save-excursion
        (goto-char (marker-position efrit-agent--conversation-end))
        (setq start-marker (point-marker))
        ;; Insert the thinking indicator
        (insert (propertize (format "[%s%s] %s\n"
                                    (efrit-agent--char 'status-waiting)
                                    "thinking..."
                                    (or text ""))
                            'face 'efrit-agent-thinking
                            'efrit-type 'thinking-indicator))
        (setq end-marker (point-marker))
        ;; Set marker insertion types
        (set-marker-insertion-type start-marker nil)
        (set-marker-insertion-type end-marker t)
        ;; Make it read-only
        (add-text-properties start-marker end-marker '(read-only t))
        ;; Update conversation end
        (set-marker efrit-agent--conversation-end (point)))
      ;; Track the indicator
      (setq efrit-agent--thinking-indicator (cons start-marker end-marker))
      ;; Scroll to show
      (efrit-agent--scroll-to-bottom))))

(defun efrit-agent--hide-thinking ()
  "Hide the thinking indicator if currently shown."
  (when efrit-agent--thinking-indicator
    (let ((inhibit-read-only t)
          (start-marker (car efrit-agent--thinking-indicator))
          (end-marker (cdr efrit-agent--thinking-indicator)))
      (when (and (marker-position start-marker)
                 (marker-position end-marker))
        (delete-region start-marker end-marker)
        ;; Update conversation end marker if needed
        (when (> (marker-position efrit-agent--conversation-end)
                 (marker-position start-marker))
          (set-marker efrit-agent--conversation-end start-marker)))
      ;; Clean up markers
      (set-marker start-marker nil)
      (set-marker end-marker nil))
    (setq efrit-agent--thinking-indicator nil)))

(defun efrit-agent--update-thinking (text)
  "Update the thinking indicator TEXT without removing and re-adding.
This provides smooth updates for changing thinking status."
  (if efrit-agent--thinking-indicator
      (let ((inhibit-read-only t)
            (start-marker (car efrit-agent--thinking-indicator))
            (end-marker (cdr efrit-agent--thinking-indicator)))
        (when (and (marker-position start-marker)
                   (marker-position end-marker))
          (save-excursion
            (goto-char start-marker)
            (delete-region start-marker end-marker)
            (insert (propertize (format "[%s%s] %s\n"
                                        (efrit-agent--char 'status-waiting)
                                        "thinking..."
                                        (or text ""))
                                'face 'efrit-agent-thinking
                                'efrit-type 'thinking-indicator
                                'read-only t))
            (set-marker end-marker (point)))))
    ;; No indicator yet, show one
    (efrit-agent--show-thinking text)))

;;; Header Line (status bar at top of window)

(defun efrit-agent--format-header-line ()
  "Format the header-line for display.
Shows: status │ elapsed │ tool count │ hints"
  (let* ((status-str (efrit-agent--status-string))
         (elapsed (efrit-agent--format-elapsed))
         (tool-count (length (cl-remove-if-not
                              (lambda (a) (eq (plist-get a :type) 'tool))
                              efrit-agent--activities)))
         (sep (propertize " │ " 'face 'efrit-agent-session-id)))
    (concat
     " "
     status-str
     sep
     (propertize elapsed 'face 'efrit-agent-timestamp)
     (when (> tool-count 0)
       (concat sep (propertize (format "%d tools" tool-count)
                               'face 'efrit-agent-session-id)))
     ;; Show action hints based on status
     (pcase efrit-agent--status
       ('waiting
        (concat sep (propertize "Type response, C-c C-s to send"
                                'face 'efrit-agent-timestamp)))
       ('working
        (concat sep (propertize "k:cancel  ?:help"
                                'face 'efrit-agent-timestamp)))
       (_ "")))))

(defun efrit-agent--setup-header-line ()
  "Set up the header-line for the agent buffer."
  (setq header-line-format '(:eval (efrit-agent--format-header-line))))

;;; UI Helpers

(defun efrit-agent--make-button (label action &optional help-echo)
  "Create a clickable button with LABEL that runs ACTION when clicked.
HELP-ECHO is the tooltip text shown on hover."
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] action)
    (define-key map (kbd "RET") action)
    (propertize (concat "[" label "]")
                'face 'efrit-agent-button
                'mouse-face 'efrit-agent-button-hover
                'keymap map
                'help-echo (or help-echo (format "Click to %s" (downcase label))))))

(defun efrit-agent--make-option-button (option index)
  "Create a clickable button for OPTION with INDEX (1-indexed)."
  (let* ((label (format "[%d] %s" index option))
         (map (make-sparse-keymap))
         (action (lambda ()
                   (interactive)
                   (efrit-agent--select-option index))))
    (define-key map [mouse-1] action)
    (define-key map (kbd "RET") action)
    (propertize label
                'face 'efrit-agent-option
                'mouse-face 'efrit-agent-option-hover
                'keymap map
                'help-echo (format "Click or press %d to select" index))))

;; Forward declaration for option selection
(declare-function efrit-agent--select-option "efrit-agent-input")

;;; Inline TODO Display
;;
;; TODOs are displayed inline in the conversation as a checklist block.
;; The block is tracked with markers and updated in-place when TODOs change.

(defvar-local efrit-agent--todos-region nil
  "Marker pair (start . end) for the inline TODO block, or nil if none.
Used to update TODOs in-place without full re-render.")

(defun efrit-agent--format-todo-item (todo)
  "Format a single TODO item for display.
TODO is a plist with :status, :content, and optionally :id."
  (let* ((status (plist-get todo :status))
         (content (or (plist-get todo :content) ""))
         (indicator (pcase status
                      ('completed (efrit-agent--char 'task-complete))
                      ('in_progress (efrit-agent--char 'task-in-progress))
                      (_ (efrit-agent--char 'task-pending))))
         (face (pcase status
                 ('completed 'efrit-agent-task-complete)
                 ('in_progress 'efrit-agent-task-current)
                 (_ 'efrit-agent-task-pending))))
    (concat
     "  "
     (propertize (format "%s " indicator) 'face face)
     (propertize content 'face face)
     (when (eq status 'in_progress)
       (propertize " ← current" 'face 'efrit-agent-timestamp))
     "\n")))

(defun efrit-agent--format-todos-block (todos)
  "Format the entire TODO block for TODOS list.
Returns a propertized string ready for insertion."
  (if (null todos)
      ""  ; Don't show anything when no TODOs
    (let* ((total (length todos))
           (complete (cl-count-if (lambda (item) (eq (plist-get item :status) 'completed))
                                  todos))
           (header (propertize (format "Tasks (%d/%d):\n" complete total)
                               'face 'efrit-agent-section-header)))
      (concat
       header
       (mapconcat #'efrit-agent--format-todo-item todos "")
       "\n"))))

(defun efrit-agent--add-todos-inline (todos)
  "Add or update the inline TODO block in the conversation.
TODOS is a list of plists with :status, :content, :id.
If a TODO block already exists, updates it in-place.
If no block exists, inserts one."
  ;; End any streaming message first (so Claude's text is finalized before TODOs)
  (efrit-agent--stream-end-message)
  (let ((inhibit-read-only t)
        (formatted (efrit-agent--format-todos-block todos)))
    (if efrit-agent--todos-region
        ;; Update existing block in-place
        (let ((start-marker (car efrit-agent--todos-region))
              (end-marker (cdr efrit-agent--todos-region)))
          (when (and (marker-position start-marker)
                     (marker-position end-marker))
            (save-excursion
              (delete-region start-marker end-marker)
              (goto-char start-marker)
              (when (> (length formatted) 0)
                (insert (propertize formatted
                                    'efrit-type 'todos-block
                                    'read-only t))
                (set-marker end-marker (point))))))
      ;; Insert new block (if there are todos)
      (when (> (length formatted) 0)
        (let (start-marker end-marker)
          (save-excursion
            (goto-char (marker-position efrit-agent--conversation-end))
            (setq start-marker (point-marker))
            (insert (propertize formatted
                                'efrit-type 'todos-block
                                'read-only t))
            (setq end-marker (point-marker))
            ;; Set marker types
            ;; start-marker: nil = insertions go after (marker stays at start)
            ;; end-marker: nil = insertions go after (marker stays at end, doesn't capture new content)
            (set-marker-insertion-type start-marker nil)
            (set-marker-insertion-type end-marker nil)
            ;; Update conversation end
            (set-marker efrit-agent--conversation-end (point)))
          ;; Track the region
          (setq efrit-agent--todos-region (cons start-marker end-marker)))))))

(defun efrit-agent--clear-todos-inline ()
  "Remove the inline TODO block if present."
  (when efrit-agent--todos-region
    (let ((inhibit-read-only t)
          (start-marker (car efrit-agent--todos-region))
          (end-marker (cdr efrit-agent--todos-region)))
      (when (and (marker-position start-marker)
                 (marker-position end-marker))
        (delete-region start-marker end-marker))
      ;; Clean up markers
      (set-marker start-marker nil)
      (set-marker end-marker nil))
    (setq efrit-agent--todos-region nil)))

(provide 'efrit-agent-render)

;;; efrit-agent-render.el ends here
