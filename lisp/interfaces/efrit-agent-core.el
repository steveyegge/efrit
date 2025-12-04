;;; efrit-agent-core.el --- Core state and region management for efrit-agent -*- lexical-binding: t -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; Core module for efrit-agent providing:
;; - Buffer-local state variables
;; - Region markers and management (conversation/input regions)
;; - Buffer creation and lifecycle management
;; - Display style character tables

;;; Code:

(require 'cl-lib)

;;; Forward declarations
(declare-function efrit-agent-mode "efrit-agent")
(declare-function efrit-agent--add-user-message "efrit-agent-render")

;;; Customization (shared across modules)

(defgroup efrit-agent nil
  "Agentic session buffer for Efrit."
  :group 'efrit
  :prefix "efrit-agent-")

(defcustom efrit-agent-buffer-name "*efrit-agent*"
  "Name of the agent session buffer."
  :type 'string
  :group 'efrit-agent)

(defcustom efrit-agent-auto-show t
  "Whether to automatically show the agent buffer when a session starts.
If nil, use `M-x efrit-agent' to open the buffer manually."
  :type 'boolean
  :group 'efrit-agent)

(defcustom efrit-agent-verbosity 'normal
  "Verbosity level for the agent buffer display.
- minimal: Show only major events
- normal: Show operations and key results
- verbose: Show full tool inputs and outputs"
  :type '(choice (const :tag "Minimal" minimal)
                 (const :tag "Normal" normal)
                 (const :tag "Verbose" verbose))
  :group 'efrit-agent)

(defcustom efrit-agent-display-style 'unicode
  "Display style for the agent buffer.
- unicode: Use Unicode box-drawing characters and symbols
- ascii: Use plain ASCII characters for terminal compatibility"
  :type '(choice (const :tag "Unicode (modern)" unicode)
                 (const :tag "ASCII (compatible)" ascii))
  :group 'efrit-agent)

(defcustom efrit-agent-show-diff t
  "Whether to show inline diffs for tool results containing diff content.
When enabled, unified diff output is syntax-highlighted with diff-mode faces."
  :type 'boolean
  :group 'efrit-agent)

(defcustom efrit-agent-display-mode 'smart
  "Display mode controlling tool result expansion.
- minimal: All tool results collapsed, ignore auto_expand hints
- smart: Respect Claude's auto_expand hints from display_hint tool
- verbose: All tool results expanded, ignore auto_expand hints"
  :type '(choice (const :tag "Minimal (all collapsed)" minimal)
                 (const :tag "Smart (respect hints)" smart)
                 (const :tag "Verbose (all expanded)" verbose))
  :group 'efrit-agent)

(defcustom efrit-agent-diff-context-lines 3
  "Number of context lines to show in diffs.
Only affects newly generated diffs, not pre-formatted diff output."
  :type 'integer
  :group 'efrit-agent)

(defcustom efrit-agent-history-file
  (expand-file-name "efrit-agent-history" user-emacs-directory)
  "File for persisting input history across restarts."
  :type 'file
  :group 'efrit-agent)

(defcustom efrit-agent-history-max-size 100
  "Maximum number of entries to keep in input history."
  :type 'integer
  :group 'efrit-agent)

;;; Display Style Character Tables

(defconst efrit-agent--unicode-chars
  '((box-top-left . "╭")
    (box-top-right . "╮")
    (box-bottom-left . "╰")
    (box-bottom-right . "╯")
    (box-horizontal . ?─)
    (box-vertical . "│")
    (section-line . ?━)
    (status-idle . "○")
    (status-working . "●")
    (status-paused . "⏸")
    (status-waiting . "⏳")
    (status-complete . "✓")
    (status-failed . "✗")
    (task-complete . "✓")
    (task-in-progress . "▶")
    (task-pending . "○")
    (expand-collapsed . "▶")
    (expand-expanded . "▼")
    (tool-running . "⟳")
    (tool-success . "✓")
    (tool-failure . "✗")
    (message-icon . "💬")
    (error-icon . "❌"))
  "Unicode characters for display elements.")

(defconst efrit-agent--ascii-chars
  '((box-top-left . "+")
    (box-top-right . "+")
    (box-bottom-left . "+")
    (box-bottom-right . "+")
    (box-horizontal . ?-)
    (box-vertical . "|")
    (section-line . ?=)
    (status-idle . "o")
    (status-working . "*")
    (status-paused . "||")
    (status-waiting . "...")
    (status-complete . "[x]")
    (status-failed . "[!]")
    (task-complete . "[x]")
    (task-in-progress . "->")
    (task-pending . "[ ]")
    (expand-collapsed . ">")
    (expand-expanded . "v")
    (tool-running . "~")
    (tool-success . "[x]")
    (tool-failure . "[!]")
    (message-icon . "[C]")
    (error-icon . "[E]"))
  "ASCII characters for display elements (terminal compatible).")

(defun efrit-agent--char (name)
  "Get the display character/string for NAME based on current style."
  (let ((table (if (eq efrit-agent-display-style 'unicode)
                   efrit-agent--unicode-chars
                 efrit-agent--ascii-chars)))
    (alist-get name table)))

;;; Buffer-local State Variables

(defvar-local efrit-agent--session-id nil
  "The session ID for this agent buffer.")

(defvar-local efrit-agent--command nil
  "The original command that started this session.")

(defvar-local efrit-agent--status 'idle
  "Current session status.
One of: idle, working, paused, waiting, complete, failed.")

(defvar-local efrit-agent--start-time nil
  "Time when the session started (from `current-time').")

(defvar-local efrit-agent--elapsed-timer nil
  "Timer for updating elapsed time display.")

(defvar-local efrit-agent--todos nil
  "List of TODO items from the session.")

(defvar-local efrit-agent--activities nil
  "List of activity entries (tool calls, messages, errors).")

(defvar-local efrit-agent--expanded-items nil
  "Set of activity item IDs that are expanded.")

(defvar-local efrit-agent--pending-question nil
  "The current pending question from Claude.
Format: (question options timestamp) or nil.")

(defvar-local efrit-agent--input-text ""
  "Current text in the input area.")

(defvar-local efrit-agent--input-history nil
  "Session-local input history (list of strings, most recent first).")

(defvar-local efrit-agent--history-index -1
  "Current position in input history (-1 means not navigating).
When navigating, 0 is the most recent entry, 1 is second most recent, etc.")

(defvar-local efrit-agent--history-temp nil
  "Temporary storage for current input when navigating history.
Stores what the user was typing before pressing M-p.")

(defvar-local efrit-agent--pending-tools nil
  "Hash table mapping tool-name to list of pending tool-ids.
Each tool-name maps to a list of (tool-id . start-time) for in-flight calls.
Uses a list per tool-name to handle parallel calls to the same tool.")

(defvar-local efrit-agent--streaming-message nil
  "Info about current streaming Claude message, or nil if not streaming.
Format: (message-id start-marker end-marker) when active.
Used to append content to an ongoing message stream.")

(defvar-local efrit-agent--thinking-indicator nil
  "Marker positions for the thinking indicator, or nil if not showing.
Format: (start-marker . end-marker) when active.
The indicator is removed when content arrives.")

(defvar-local efrit-agent--message-counter 0
  "Counter for generating unique message IDs.")

(defvar-local efrit-agent--failed-tools nil
  "Alist mapping tool-id to tool execution context for retry.
Each entry is (tool-id . (:name name :input input :item tool-item)).")

(defvar-local efrit-agent--expansion-state nil
  "Hash table mapping tool-id to user-specified expansion state.
When a user explicitly toggles a tool with RET, their preference
is stored here and overrides both display-mode and Claude's auto_expand hints.
Values are t (expanded) or nil (collapsed).")

(defvar efrit-agent--activity-counter 0
  "Counter for generating unique activity IDs.")

(defvar efrit-agent--global-history nil
  "Global input history shared across all sessions.
Persists across restarts via `efrit-agent-history-file'.")

;;; Region Markers (Conversation-first architecture)
;;
;; The buffer is divided into two regions:
;; 1. Conversation region: read-only, contains all messages and tool calls
;; 2. Input region: editable, where user types their input
;;
;; Layout:
;;   (point-min)
;;   [Conversation content - read-only]
;;   efrit-agent--conversation-end (marker)
;;   [Separator line]
;;   efrit-agent--input-start (marker)
;;   [Input area - editable]
;;   (point-max)

(defvar-local efrit-agent--conversation-end nil
  "Marker at the end of the conversation region.
All new conversation content is inserted before this marker.")

(defvar-local efrit-agent--input-start nil
  "Marker at the start of the input region.
Text after this marker is editable by the user.")

(defun efrit-agent--init-regions ()
  "Initialize the conversation and input region markers.
Call this when setting up a new agent buffer."
  ;; Create markers if they don't exist
  ;; conversation-end: insertion-type t means marker stays AFTER inserted text
  ;; So when we insert before this marker, the marker moves forward to stay at the end.
  (unless efrit-agent--conversation-end
    (setq efrit-agent--conversation-end (make-marker))
    (set-marker-insertion-type efrit-agent--conversation-end t))
  ;; input-start: insertion-type nil means marker stays BEFORE inserted text
  ;; So when user types at point-max (after the marker), marker stays put.
  (unless efrit-agent--input-start
    (setq efrit-agent--input-start (make-marker))
    (set-marker-insertion-type efrit-agent--input-start nil)))

(defun efrit-agent--setup-regions ()
  "Set up the initial buffer layout with conversation and input regions.
Should be called after `efrit-agent--init-regions' when the buffer is empty."
  (let ((inhibit-read-only t)
        conversation-end-pos
        input-start-pos)
    (erase-buffer)
    ;; Insert initial conversation area (empty for now)
    (insert "\n")
    ;; Remember where conversation ends (before separator)
    (setq conversation-end-pos (point))
    ;; Insert separator between conversation and input
    (insert (propertize (concat (make-string 60 ?─) "\n")
                        'efrit-agent-separator t
                        'read-only t
                        'rear-nonsticky t))
    ;; Insert input prompt (read-only)
    (insert (propertize "> " 'efrit-agent-prompt t 'read-only t 'rear-nonsticky t))
    ;; Mark start of user input (AFTER the prompt)
    (setq input-start-pos (point))
    ;; Set markers at the saved positions (after all inserts to avoid insertion-type issues)
    (set-marker efrit-agent--conversation-end conversation-end-pos)
    (set-marker efrit-agent--input-start input-start-pos)
    ;; Make conversation region read-only
    (add-text-properties (point-min) efrit-agent--conversation-end
                         '(read-only t))))

(defun efrit-agent--append-to-conversation (text &optional properties)
  "Append TEXT with optional PROPERTIES to the conversation region.
Inserts just before the conversation-end marker, preserving read-only.
Does nothing if the conversation-end marker is not initialized."
  (when (and efrit-agent--conversation-end
             (marker-position efrit-agent--conversation-end))
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char efrit-agent--conversation-end)
        (let ((start (point)))
          (insert text)
          ;; Apply any additional properties
          (when properties
            (add-text-properties start (point) properties))
          ;; Make the new text read-only
          (add-text-properties start (point) '(read-only t)))))))

(defun efrit-agent--get-input ()
  "Get the current user input from the input region.
Returns the text after the input-start marker."
  (when (and efrit-agent--input-start
             (marker-position efrit-agent--input-start))
    (string-trim-right
     (buffer-substring-no-properties efrit-agent--input-start (point-max)))))

(defun efrit-agent--clear-input ()
  "Clear the input region, leaving just the prompt.
Deletes from input-start marker to end of buffer."
  (when (and efrit-agent--input-start
             (marker-position efrit-agent--input-start))
    (let ((inhibit-read-only t))
      (delete-region efrit-agent--input-start (point-max)))))

(defun efrit-agent--in-input-region-p ()
  "Return non-nil if point is in the input region."
  (and efrit-agent--input-start
       (marker-position efrit-agent--input-start)
       (>= (point) efrit-agent--input-start)))

;;; Buffer Creation and Management

(defun efrit-agent--get-buffer ()
  "Get or create the agent buffer."
  (get-buffer-create efrit-agent-buffer-name))

(defun efrit-agent--create-buffer (session-id command)
  "Create and initialize the agent buffer for SESSION-ID with COMMAND."
  (let ((buffer (efrit-agent--get-buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer))
      (efrit-agent-mode)
      ;; Initialize session state
      (setq efrit-agent--session-id session-id)
      (setq efrit-agent--command command)
      (setq efrit-agent--status 'working)
      (setq efrit-agent--start-time (current-time))
      (setq efrit-agent--todos nil)
      (setq efrit-agent--activities nil)
      ;; Initialize pending tools hash table for incremental updates
      (setq efrit-agent--pending-tools (make-hash-table :test 'equal))
      ;; Set up conversation/input regions (incremental architecture)
      (efrit-agent--setup-regions)
      ;; Start elapsed time timer (updates header-line only, not buffer)
      (when efrit-agent--elapsed-timer
        (cancel-timer efrit-agent--elapsed-timer))
      (setq efrit-agent--elapsed-timer
            (run-at-time 1 1 #'efrit-agent--update-elapsed buffer)))
    buffer))

(defun efrit-agent--show-buffer ()
  "Display the agent buffer.
Does nothing in batch mode or when `efrit-agent-auto-show' is nil."
  (when (and efrit-agent-auto-show
             (not noninteractive))
    (let ((buffer (efrit-agent--get-buffer)))
      (display-buffer buffer '(display-buffer-at-bottom
                               (window-height . 15))))))

(defun efrit-agent--cleanup-timer ()
  "Clean up the elapsed timer when buffer is killed."
  (when efrit-agent--elapsed-timer
    (cancel-timer efrit-agent--elapsed-timer)
    (setq efrit-agent--elapsed-timer nil)))

(defun efrit-agent--save-session-on-kill ()
  "Save the current session when the agent buffer is killed.
Only saves if session has content worth preserving."
  (when efrit-agent--session-id
    (condition-case err
        (progn
          (require 'efrit-do-async-loop)
          (require 'efrit-session-transcript)
          (let* ((session-id efrit-agent--session-id)
                 ;; Get session from async loop state
                 (loop-state (gethash session-id efrit-do-async--loops))
                 (session (and loop-state (car loop-state))))
            ;; Only save if we have a session object and it has content
            (when (and session
                       (> (length (efrit-session-get-api-messages-for-continuation session)) 0))
              (efrit-transcript-save session)
              (require 'efrit-log)
              (efrit-log 'info "Saved agent session %s on buffer kill" session-id))))
      (error
       (require 'efrit-log)
       (efrit-log 'warn "Failed to save session on kill: %s" (error-message-string err))))))

(defun efrit-agent--update-elapsed (buffer)
  "Update the elapsed time display in BUFFER.
Forces a redisplay of the header-line which contains the elapsed time."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (memq efrit-agent--status '(working paused waiting))
        ;; Force header-line redisplay (it uses :eval so this triggers update)
        (force-mode-line-update)))))

(defun efrit-agent--format-elapsed ()
  "Format elapsed time since session start."
  (if efrit-agent--start-time
      (let* ((elapsed (float-time (time-subtract (current-time) efrit-agent--start-time)))
             (mins (floor (/ elapsed 60)))
             (secs (floor (mod elapsed 60))))
        (if (> mins 0)
            (format "%d:%02d" mins secs)
          (format "%.1fs" elapsed)))
    "0.0s"))

(defun efrit-agent--status-string ()
  "Return the status string with appropriate face."
  (pcase efrit-agent--status
    ('idle (propertize (format "%s Idle" (efrit-agent--char 'status-idle))
                       'face 'efrit-agent-session-id))
    ('working (propertize (format "%s Working" (efrit-agent--char 'status-working))
                          'face 'efrit-agent-status-working))
    ('paused (propertize (format "%s Paused" (efrit-agent--char 'status-paused))
                         'face 'efrit-agent-status-paused))
    ('waiting (propertize (format "%s Waiting" (efrit-agent--char 'status-waiting))
                          'face 'efrit-agent-status-waiting))
    ('complete (propertize (format "%s Complete" (efrit-agent--char 'status-complete))
                           'face 'efrit-agent-status-complete))
    ('failed (propertize (format "%s Failed" (efrit-agent--char 'status-failed))
                         'face 'efrit-agent-status-failed))
    (_ (format "? %s" efrit-agent--status))))

(defun efrit-agent--init-pending-tools ()
  "Initialize the pending tools hash table if needed."
  (unless efrit-agent--pending-tools
    (setq efrit-agent--pending-tools (make-hash-table :test 'equal))))

(provide 'efrit-agent-core)

;;; efrit-agent-core.el ends here
