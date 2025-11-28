;;; efrit-agent.el --- Agentic session buffer for Efrit -*- lexical-binding: t -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; This module provides a structured, real-time view of agentic Efrit sessions.
;; Unlike the raw `*Efrit Progress*` buffer which shows all events linearly,
;; this buffer organizes information for interactive agentic workflows.
;;
;; The buffer shows:
;; - Session status and elapsed time
;; - Task progress from TODO tracking
;; - Activity log with expandable tool calls
;; - Input area for user interaction
;;
;; Following the Zero Client-Side Intelligence principle, this module
;; only DISPLAYS state - it does not make decisions about it.

;;; Code:

(require 'cl-lib)

;; Forward declarations to silence byte-compiler
(defvar efrit-do--current-todos)
(declare-function efrit-do-todo-item-id "efrit-do")
(declare-function efrit-do-todo-item-content "efrit-do")
(declare-function efrit-do-todo-item-status "efrit-do")
(declare-function efrit-executor-cancel "efrit-executor")
(declare-function efrit-executor-respond "efrit-executor")
(declare-function efrit-executor-pending-question "efrit-executor")
(declare-function efrit-session-active "efrit-session")
(declare-function efrit-session-waiting-for-user-p "efrit-session")
(declare-function efrit-progress-inject "efrit-progress")

;;; Customization

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
- unicode: Use Unicode box-drawing characters and symbols (requires font support)
- ascii: Use plain ASCII characters for maximum terminal compatibility"
  :type '(choice (const :tag "Unicode (modern)" unicode)
                 (const :tag "ASCII (compatible)" ascii))
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

;;; Faces

(defface efrit-agent-header
  '((t :weight bold :height 1.1))
  "Face for the buffer header."
  :group 'efrit-agent)

(defface efrit-agent-session-id
  '((t :foreground "gray60"))
  "Face for session ID display."
  :group 'efrit-agent)

(defface efrit-agent-command
  '((t :foreground "SkyBlue" :slant italic))
  "Face for the command summary."
  :group 'efrit-agent)

(defface efrit-agent-status-working
  '((t :foreground "green3" :weight bold))
  "Face for working status indicator."
  :group 'efrit-agent)

(defface efrit-agent-status-paused
  '((t :foreground "gold" :weight bold))
  "Face for paused status indicator."
  :group 'efrit-agent)

(defface efrit-agent-status-waiting
  '((t :foreground "DeepSkyBlue" :weight bold))
  "Face for waiting-for-input status indicator."
  :group 'efrit-agent)

(defface efrit-agent-status-complete
  '((t :foreground "green3" :weight bold))
  "Face for completed status indicator."
  :group 'efrit-agent)

(defface efrit-agent-status-failed
  '((t :foreground "red3" :weight bold))
  "Face for failed status indicator."
  :group 'efrit-agent)

(defface efrit-agent-section-header
  '((t :weight bold :foreground "DeepSkyBlue"))
  "Face for section headers."
  :group 'efrit-agent)

(defface efrit-agent-task-complete
  '((t :foreground "gray60"))
  "Face for completed tasks."
  :group 'efrit-agent)

(defface efrit-agent-task-current
  '((t :weight bold :foreground "yellow"))
  "Face for the current in-progress task."
  :group 'efrit-agent)

(defface efrit-agent-task-pending
  '((t :foreground "gray80"))
  "Face for pending tasks."
  :group 'efrit-agent)

(defface efrit-agent-timestamp
  '((t :foreground "gray60"))
  "Face for timestamps."
  :group 'efrit-agent)

(defface efrit-agent-tool-name
  '((t :weight bold :foreground "DarkOrange"))
  "Face for tool names."
  :group 'efrit-agent)

(defface efrit-agent-claude-message
  '((t :foreground "RoyalBlue"))
  "Face for Claude's messages."
  :group 'efrit-agent)

(defface efrit-agent-error
  '((t :foreground "red3"))
  "Face for error messages."
  :group 'efrit-agent)

(defface efrit-agent-button
  '((t :box (:line-width -1 :style released-button) :foreground "gray80"))
  "Face for clickable buttons."
  :group 'efrit-agent)

(defface efrit-agent-button-hover
  '((t :box (:line-width -1 :style released-button) :foreground "white" :background "gray30"))
  "Face for buttons on mouse hover."
  :group 'efrit-agent)

(defface efrit-agent-question
  '((t :weight bold :foreground "cyan"))
  "Face for pending questions from Claude."
  :group 'efrit-agent)

(defface efrit-agent-option
  '((t :box (:line-width -1 :style released-button) :foreground "LightGreen"))
  "Face for clickable option buttons."
  :group 'efrit-agent)

(defface efrit-agent-option-hover
  '((t :box (:line-width -1 :style released-button) :foreground "white" :background "DarkGreen"))
  "Face for option buttons on hover."
  :group 'efrit-agent)

(defface efrit-agent-input-prompt
  '((t :foreground "gold"))
  "Face for the input prompt."
  :group 'efrit-agent)

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

;;; Keymap

(defvar efrit-agent-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Navigation
    (define-key map (kbd "TAB") #'efrit-agent-next-section)
    (define-key map (kbd "<backtab>") #'efrit-agent-prev-section)
    (define-key map (kbd "S-TAB") #'efrit-agent-prev-section)

    ;; Actions
    (define-key map (kbd "q") #'efrit-agent-quit)
    (define-key map (kbd "k") #'efrit-agent-cancel)
    (define-key map (kbd "p") #'efrit-agent-pause)
    (define-key map (kbd "r") #'efrit-agent-resume)
    (define-key map (kbd "g") #'efrit-agent-refresh)
    (define-key map (kbd "RET") #'efrit-agent-toggle-expand)
    (define-key map (kbd "v") #'efrit-agent-cycle-verbosity)
    (define-key map (kbd "?") #'efrit-agent-help)

    ;; Input handling
    (define-key map (kbd "C-c C-s") #'efrit-agent-send-input)
    (define-key map (kbd "C-c C-c") #'efrit-agent-abort)
    (define-key map (kbd "i") #'efrit-agent-inject-guidance)
    (define-key map (kbd "1") #'efrit-agent-select-option-1)
    (define-key map (kbd "2") #'efrit-agent-select-option-2)
    (define-key map (kbd "3") #'efrit-agent-select-option-3)
    (define-key map (kbd "4") #'efrit-agent-select-option-4)

    map)
  "Keymap for `efrit-agent-mode'.")

;;; Major Mode Definition

(define-derived-mode efrit-agent-mode special-mode "Efrit-Agent"
  "Major mode for Efrit agentic session display.

This buffer provides a structured view of an Efrit agent session,
showing task progress, activity log, and allowing user interaction.

\\{efrit-agent-mode-map}"
  :group 'efrit-agent
  ;; Display settings
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  (setq-local line-spacing 0.1)
  ;; Don't let cursor jump around during updates
  (setq-local cursor-in-non-selected-windows nil)
  ;; Initialize state
  (setq efrit-agent--expanded-items (make-hash-table :test 'equal))
  ;; Clean up timer when buffer is killed (fixes memory leak)
  (add-hook 'kill-buffer-hook #'efrit-agent--cleanup-timer nil t))

;;; Interactive Commands (Placeholder Implementations)

(defun efrit-agent-quit ()
  "Quit the agent buffer without canceling the session."
  (interactive)
  (quit-window))

(defun efrit-agent-cancel ()
  "Cancel the current session."
  (interactive)
  (if (memq efrit-agent--status '(working paused waiting))
      (progn
        (efrit-executor-cancel)
        (efrit-agent-set-status 'failed)
        (efrit-agent--render))
    (message "No active session to cancel")))

(defun efrit-agent-pause ()
  "Pause the current session.
Note: Currently Efrit sessions can only be paused by Claude
requesting user input. Arbitrary pause/resume is not yet supported."
  (interactive)
  (if (eq efrit-agent--status 'working)
      (message "Arbitrary pause not yet implemented. Sessions pause when waiting for user input.")
    (message "Session is not currently working")))

(defun efrit-agent-resume ()
  "Resume a paused/waiting session by prompting for user input.
If the session is waiting for user input, prompts for a response."
  (interactive)
  (cond
   ((eq efrit-agent--status 'waiting)
    ;; Session is waiting for user input - use efrit-executor-respond
    (call-interactively #'efrit-executor-respond)
    (efrit-agent-set-status 'working)
    (efrit-agent--render))
   ((eq efrit-agent--status 'paused)
    (message "Session resume not yet implemented"))
   (t
    (message "Session is not paused or waiting for input"))))

(defun efrit-agent-refresh ()
  "Refresh the buffer display."
  (interactive)
  (efrit-agent--render))

(defun efrit-agent-next-section ()
  "Move to the next section in the buffer."
  (interactive)
  (let ((pos (next-single-property-change (point) 'efrit-agent-section)))
    (when pos
      (goto-char pos)
      (when (get-text-property pos 'efrit-agent-section)
        (forward-line 1)))))

(defun efrit-agent-prev-section ()
  "Move to the previous section in the buffer."
  (interactive)
  (let ((pos (previous-single-property-change (point) 'efrit-agent-section)))
    (when pos
      (goto-char pos)
      (when-let* ((prev (previous-single-property-change pos 'efrit-agent-section)))
        (goto-char prev)))))

(defun efrit-agent-toggle-expand ()
  "Toggle expansion of the item at point."
  (interactive)
  (when-let* ((item-id (get-text-property (point) 'efrit-agent-item-id)))
    (if (gethash item-id efrit-agent--expanded-items)
        (remhash item-id efrit-agent--expanded-items)
      (puthash item-id t efrit-agent--expanded-items))
    (efrit-agent--render)))

(defun efrit-agent-cycle-verbosity ()
  "Cycle through verbosity levels."
  (interactive)
  (setq efrit-agent-verbosity
        (pcase efrit-agent-verbosity
          ('minimal 'normal)
          ('normal 'verbose)
          ('verbose 'minimal)))
  (message "Verbosity: %s" efrit-agent-verbosity)
  (efrit-agent--render))

(defun efrit-agent-help ()
  "Show help for agent buffer key bindings."
  (interactive)
  (let ((help-text
         (format "Efrit Agent Buffer Help
%s

Navigation:
  TAB / S-TAB    Move between sections
  RET            Expand/collapse tool call at point

Actions:
  q              Quit buffer (session continues)
  k              Kill/cancel session
  p              Pause session
  r              Resume paused session
  g              Refresh display
  v              Cycle verbosity (minimal/normal/verbose)

Verbosity Levels:
  minimal        Show 20 chars result, 3 lines when expanded
  normal         Show 40 chars result, 10 lines when expanded
  verbose        Show 80 chars result, 50 lines when expanded

Expand/Collapse:
  Tool calls show %s (collapsed) or %s (expanded)
  Press RET on a tool call to toggle details
  Expanded view shows input parameters and full result

Display Style:
  Current: %s
  Set `efrit-agent-display-style' to 'ascii for terminal compatibility

Input (when waiting for response):
  C-c C-s        Send typed input to Claude
  C-c C-c        Cancel current operation
  1-4            Select option 1-4 (when options available)
  i              Inject guidance to Claude mid-session

Press q to close this help."
                 (make-string 43 ?=)
                 (efrit-agent--char 'expand-collapsed)
                 (efrit-agent--char 'expand-expanded)
                 efrit-agent-display-style)))
    (with-help-window "*Efrit Agent Help*"
      (princ help-text))))

(defun efrit-agent-send-input ()
  "Send user input to the session.
If the session is waiting for user input, prompts for a response."
  (interactive)
  (if (eq efrit-agent--status 'waiting)
      (progn
        (call-interactively #'efrit-executor-respond)
        (efrit-agent-set-status 'working)
        (efrit-agent--render))
    (message "Session is not waiting for input")))

(defun efrit-agent-abort ()
  "Abort the current operation."
  (interactive)
  (efrit-agent-cancel))

(defun efrit-agent--select-option (n)
  "Select option N (1-indexed) from pending question options.
Returns nil if no options or N is out of range."
  (when (eq efrit-agent--status 'waiting)
    (let* ((options (cadr efrit-agent--pending-question))
           (option (and options (nth (1- n) options))))
      (when option
        (efrit-executor-respond option)
        (efrit-agent-set-status 'working)
        t))))

(defun efrit-agent-select-option-1 ()
  "Select option 1."
  (interactive)
  (unless (efrit-agent--select-option 1)
    (message "No option 1 available")))

(defun efrit-agent-select-option-2 ()
  "Select option 2."
  (interactive)
  (unless (efrit-agent--select-option 2)
    (message "No option 2 available")))

(defun efrit-agent-select-option-3 ()
  "Select option 3."
  (interactive)
  (unless (efrit-agent--select-option 3)
    (message "No option 3 available")))

(defun efrit-agent-select-option-4 ()
  "Select option 4."
  (interactive)
  (unless (efrit-agent--select-option 4)
    (message "No option 4 available")))

(defun efrit-agent-inject-guidance ()
  "Inject guidance into the current session.
This allows you to provide hints or direction to Claude mid-session,
even when not waiting for explicit input."
  (interactive)
  (unless efrit-agent--session-id
    (user-error "No active session"))
  (let ((guidance (read-string "Guidance for Claude: ")))
    (when (and guidance (not (string-empty-p guidance)))
      (efrit-progress-inject efrit-agent--session-id 'guidance guidance)
      ;; Add to activity log
      (efrit-agent-add-activity
       (list :type 'message
             :text (format "💡 Guidance: %s" guidance)
             :timestamp (current-time)))
      (message "Guidance injected"))))

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
      ;; Start elapsed time timer
      (when efrit-agent--elapsed-timer
        (cancel-timer efrit-agent--elapsed-timer))
      (setq efrit-agent--elapsed-timer
            (run-at-time 1 1 #'efrit-agent--update-elapsed buffer))
      ;; Initial render
      (efrit-agent--render))
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

(defun efrit-agent--update-elapsed (buffer)
  "Update the elapsed time display in BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (memq efrit-agent--status '(working paused waiting))
        ;; Only re-render the header section for efficiency
        (efrit-agent--render-header-only)))))

;;; Rendering

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

(defun efrit-agent--render ()
  "Render the entire buffer."
  (let ((inhibit-read-only t)
        (pos (point)))
    (erase-buffer)
    (efrit-agent--render-header)
    (efrit-agent--render-tasks)
    (efrit-agent--render-activity)
    (efrit-agent--render-input)
    ;; Restore point approximately
    (goto-char (min pos (point-max)))))

(defun efrit-agent--render-header-only ()
  "Re-render just the elapsed time in the header for efficiency.
Updates only the elapsed time region marked with `efrit-agent-elapsed' property,
avoiding full buffer re-render which causes cursor flicker."
  (save-excursion
    (goto-char (point-min))
    ;; Find the elapsed time region by text property
    (let ((elapsed-start (text-property-any (point-min) (point-max)
                                            'efrit-agent-elapsed t)))
      (when elapsed-start
        (let* ((elapsed-end (next-single-property-change elapsed-start 'efrit-agent-elapsed))
               (new-elapsed (format " (%s)" (efrit-agent--format-elapsed)))
               (inhibit-read-only t))
          ;; Replace just the elapsed time text
          (goto-char elapsed-start)
          (delete-region elapsed-start (or elapsed-end (point-max)))
          (insert (propertize new-elapsed 'efrit-agent-elapsed t)))))))

(defun efrit-agent--render-header ()
  "Render the header section."
  (let ((h-char (efrit-agent--char 'box-horizontal))
        (v-char (efrit-agent--char 'box-vertical)))
    (insert (propertize (efrit-agent--char 'box-top-left) 'face 'efrit-agent-header))
    (insert (make-string 58 h-char))
    (insert (propertize (concat (efrit-agent--char 'box-top-right) "\n")
                        'face 'efrit-agent-header))

    ;; Session line
    (insert (propertize v-char 'face 'efrit-agent-header))
    (insert " ")
    (insert (propertize "Efrit Agent" 'face 'efrit-agent-header))
    (when efrit-agent--session-id
      (insert " ")
      (insert (propertize (truncate-string-to-width efrit-agent--session-id 30)
                          'face 'efrit-agent-session-id)))
    (insert (make-string (max 1 (- 58 (current-column))) ? ))
    (insert (propertize (concat v-char "\n") 'face 'efrit-agent-header))

    ;; Command line
    (insert (propertize v-char 'face 'efrit-agent-header))
    (insert " Command: ")
    (insert (propertize (or (truncate-string-to-width (or efrit-agent--command "") 45) "")
                        'face 'efrit-agent-command))
    (insert (make-string (max 1 (- 58 (current-column))) ? ))
    (insert (propertize (concat v-char "\n") 'face 'efrit-agent-header))

    ;; Status line with buttons
    (insert (propertize v-char 'face 'efrit-agent-header))
    (insert " Status: ")
    (insert (efrit-agent--status-string))
    ;; Mark elapsed time position for efficient partial updates
    (let ((elapsed-start (point)))
      (insert (format " (%s)" (efrit-agent--format-elapsed)))
      (put-text-property elapsed-start (point) 'efrit-agent-elapsed t))
    ;; Show tool call count
    (let ((tool-count (length (cl-remove-if-not
                               (lambda (a) (eq (plist-get a :type) 'tool))
                               efrit-agent--activities))))
      (when (> tool-count 0)
        (insert (propertize (format " [%d tools]" tool-count)
                            'face 'efrit-agent-session-id))))
    ;; Add action buttons for active sessions
    (when (memq efrit-agent--status '(working paused waiting))
      (insert "  ")
      (if (eq efrit-agent--status 'paused)
          (insert (efrit-agent--make-button "Resume" #'efrit-agent-resume "Resume the paused session"))
        (insert (efrit-agent--make-button "Pause" #'efrit-agent-pause "Pause the session")))
      (insert " ")
      (insert (efrit-agent--make-button "Cancel" #'efrit-agent-cancel "Cancel the session")))
    (insert (make-string (max 1 (- 58 (current-column))) ? ))
    (insert (propertize (concat v-char "\n") 'face 'efrit-agent-header))

    ;; Bottom border
    (insert (propertize (efrit-agent--char 'box-bottom-left) 'face 'efrit-agent-header))
    (insert (make-string 58 h-char))
    (insert (propertize (concat (efrit-agent--char 'box-bottom-right) "\n\n")
                        'face 'efrit-agent-header))))

(defun efrit-agent--render-tasks ()
  "Render the tasks section."
  (let ((start (point))
        (s-char (efrit-agent--char 'section-line)))
    (insert (propertize (format "%c%c%c Tasks " s-char s-char s-char)
                        'face 'efrit-agent-section-header
                        'efrit-agent-section 'tasks))
    ;; Task count
    (when efrit-agent--todos
      (let* ((total (length efrit-agent--todos))
             (complete (cl-count-if (lambda (item) (eq (plist-get item :status) 'completed))
                                    efrit-agent--todos)))
        (insert (propertize (format "(%d/%d complete) " complete total)
                            'face 'efrit-agent-section-header))))
    (insert (propertize (make-string (max 1 (- 60 (- (point) start))) s-char)
                        'face 'efrit-agent-section-header))
    (insert "\n")

    ;; Task list
    (if efrit-agent--todos
        (dolist (todo efrit-agent--todos)
          (when todo  ; Skip nil entries from failed conversions
            (let* ((status (plist-get todo :status))
                   (content (plist-get todo :content))
                   (indicator (pcase status
                                ('completed (format "  %s " (efrit-agent--char 'task-complete)))
                                ('in_progress (format "  %s " (efrit-agent--char 'task-in-progress)))
                                ('pending (format "  %s " (efrit-agent--char 'task-pending)))
                                (_ (format "  %s " (efrit-agent--char 'task-pending)))))
                   (face (pcase status
                           ('completed 'efrit-agent-task-complete)
                           ('in_progress 'efrit-agent-task-current)
                           (_ 'efrit-agent-task-pending))))
              (insert (propertize indicator 'face face))
              (insert (propertize (or content "") 'face face))
              (when (eq status 'in_progress)
                (insert (propertize " <- current" 'face 'efrit-agent-timestamp)))
              (insert "\n"))))
      (insert (propertize "  No tasks yet\n" 'face 'efrit-agent-timestamp)))
    (insert "\n")))

(defun efrit-agent--render-activity ()
  "Render the activity section."
  (let ((start (point))
        (s-char (efrit-agent--char 'section-line)))
    (insert (propertize (format "%c%c%c Activity " s-char s-char s-char)
                        'face 'efrit-agent-section-header
                        'efrit-agent-section 'activity))
    (insert (propertize (make-string (max 1 (- 60 (- (point) start))) s-char)
                        'face 'efrit-agent-section-header))
    (insert "\n")

    ;; Activity list
    (if efrit-agent--activities
        (dolist (activity efrit-agent--activities)
          (efrit-agent--render-activity-item activity))
      (insert (propertize "  No activity yet\n" 'face 'efrit-agent-timestamp)))
    (insert "\n")))

(defun efrit-agent--render-activity-item (activity)
  "Render a single ACTIVITY item."
  (let* ((type (plist-get activity :type))
         (timestamp (plist-get activity :timestamp))
         (time-str (if timestamp
                       (format-time-string "[%M:%S]" timestamp)
                     "[--:--]"))
         (item-id (plist-get activity :id))
         (success (plist-get activity :success))
         (elapsed (plist-get activity :elapsed))
         (expanded (and item-id (gethash item-id efrit-agent--expanded-items)))
         (line-start (point)))
    (insert (propertize time-str 'face 'efrit-agent-timestamp))
    (insert " ")
    (pcase type
      ('tool
       (let ((tool-name (plist-get activity :tool))
             (result (plist-get activity :result))
             (input (plist-get activity :input)))
         ;; Show expand/collapse indicator for tool calls with details
         (when item-id
           (insert (propertize (format "%s "
                                       (if expanded
                                           (efrit-agent--char 'expand-expanded)
                                         (efrit-agent--char 'expand-collapsed)))
                               'face 'efrit-agent-timestamp)))
         ;; Show success/failure indicator when result is available
         (if result
             (insert (format "%s " (if success
                                       (efrit-agent--char 'tool-success)
                                     (efrit-agent--char 'tool-failure))))
           ;; Running tool - show elapsed time if available
           (let ((running-elapsed (and timestamp
                                       (float-time (time-subtract
                                                    (current-time) timestamp)))))
             (if (and running-elapsed (> running-elapsed 0.1))
                 (insert (format "%s %.1fs "
                                (efrit-agent--char 'tool-running)
                                running-elapsed))
               (insert (format "%s " (efrit-agent--char 'tool-running))))))
         (insert (propertize tool-name 'face 'efrit-agent-tool-name))
         ;; Show elapsed time for completed tools
         (when (and result elapsed)
           (insert (propertize (format " (%.2fs)" elapsed)
                               'face 'efrit-agent-timestamp)))
         ;; Summary line based on verbosity (single line, no newlines)
         (when result
           (insert " -> ")
           (let* ((result-face (if success nil 'efrit-agent-error))
                  (max-len (pcase efrit-agent-verbosity
                             ('minimal 20)
                             ('normal 40)
                             ('verbose 80)))
                  ;; Flatten result to single line for summary
                  (result-str (replace-regexp-in-string
                               "[\n\r]+" " "
                               (format "%s" result))))
             (insert (propertize (truncate-string-to-width result-str max-len)
                                 'face result-face))))
         ;; Mark the line with item-id for toggle functionality
         (when item-id
           (put-text-property line-start (point) 'efrit-agent-item-id item-id))
         (insert "\n")
         ;; Render expanded details if expanded
         (when (and expanded (or input result))
           (efrit-agent--render-tool-details tool-name input result success))))
      ('message
       (insert (format "%s " (efrit-agent--char 'message-icon)))
       (insert (propertize "Claude: " 'face 'efrit-agent-claude-message))
       (insert (or (plist-get activity :text) ""))
       (insert "\n"))
      ('error
       (insert (format "%s " (efrit-agent--char 'error-icon)))
       (insert (propertize "Error: " 'face 'efrit-agent-error))
       (insert (propertize (or (plist-get activity :text) "") 'face 'efrit-agent-error))
       (insert "\n")))))

(defun efrit-agent--render-tool-details (_tool-name input result success)
  "Render expanded details for a tool call.
_TOOL-NAME is the tool (unused, shown in summary line), INPUT is the input
parameters, RESULT is the output, SUCCESS indicates whether the tool succeeded."
  (let ((indent "       "))  ; Align with content after timestamp and indicator
    ;; Input section
    (when input
      (insert indent)
      (insert (propertize "Input: " 'face 'efrit-agent-section-header))
      (insert "\n")
      (efrit-agent--render-indented-content input (concat indent "  ")))
    ;; Result section
    (when result
      (insert indent)
      (insert (propertize (if success "Result: " "Error: ")
                          'face (if success 'efrit-agent-section-header 'efrit-agent-error)))
      (insert "\n")
      (efrit-agent--render-indented-content result (concat indent "  ")))
    ;; Separator
    (insert indent)
    (insert (propertize (make-string 50 (efrit-agent--char 'box-horizontal))
                        'face 'efrit-agent-timestamp))
    (insert "\n")))

(defun efrit-agent--render-indented-content (content indent)
  "Render CONTENT with INDENT prefix on each line.
CONTENT can be a string or a complex object (will be pretty-printed)."
  (let* ((content-str (if (stringp content)
                          content
                        (pp-to-string content)))
         ;; Limit displayed content based on verbosity
         (max-lines (pcase efrit-agent-verbosity
                      ('minimal 3)
                      ('normal 10)
                      ('verbose 50)))
         (lines (split-string content-str "\n" t))
         (truncated (> (length lines) max-lines))
         (display-lines (seq-take lines max-lines)))
    (dolist (line display-lines)
      (insert indent)
      (insert (propertize line 'face 'efrit-agent-session-id))
      (insert "\n"))
    (when truncated
      (insert indent)
      (insert (propertize (format "... (%d more lines)" (- (length lines) max-lines))
                          'face 'efrit-agent-timestamp))
      (insert "\n"))))

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

(defun efrit-agent--render-input ()
  "Render the input section.
Shows pending question from Claude with options if available."
  (when (eq efrit-agent--status 'waiting)
    (let ((start (point))
          (s-char (efrit-agent--char 'section-line))
          (question (car efrit-agent--pending-question))
          (options (cadr efrit-agent--pending-question)))
      ;; Section header
      (insert (propertize (format "%c%c%c Input " s-char s-char s-char)
                          'face 'efrit-agent-section-header
                          'efrit-agent-section 'input))
      (insert (propertize (make-string (max 1 (- 60 (- (point) start))) s-char)
                          'face 'efrit-agent-section-header))
      (insert "\n")

      ;; Question from Claude
      (when question
        (insert (propertize "Question: " 'face 'efrit-agent-timestamp))
        (insert (propertize question 'face 'efrit-agent-question))
        (insert "\n"))

      ;; Options (if available)
      (when options
        (insert (propertize "Options: " 'face 'efrit-agent-timestamp))
        (let ((idx 1))
          (dolist (opt options)
            (insert (efrit-agent--make-option-button opt idx))
            (insert " ")
            (cl-incf idx)))
        (insert "\n"))

      ;; Instructions
      (insert "\n")
      (if options
          (insert (propertize "Press 1-4 to select, or type custom response:\n"
                              'face 'efrit-agent-timestamp))
        (insert (propertize "Type response (C-c C-s to send, C-c C-c to cancel):\n"
                            'face 'efrit-agent-timestamp)))

      ;; Input prompt
      (insert (propertize "> " 'face 'efrit-agent-input-prompt)))))

;;; Public API

;;;###autoload
(defun efrit-agent ()
  "Open or switch to the Efrit agent buffer."
  (interactive)
  (let ((buffer (efrit-agent--get-buffer)))
    (unless (buffer-live-p buffer)
      (with-current-buffer buffer
        (efrit-agent-mode)))
    (pop-to-buffer buffer)))

(defun efrit-agent-start-session (session-id command)
  "Start tracking a new session with SESSION-ID and COMMAND.
Called from efrit-executor when an agentic session begins."
  (efrit-agent--create-buffer session-id command)
  (efrit-agent--show-buffer))

(defun efrit-agent-end-session (success-p)
  "End the current session with SUCCESS-P status."
  (let ((buffer (get-buffer efrit-agent-buffer-name)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq efrit-agent--status (if success-p 'complete 'failed))
        ;; Stop the elapsed timer
        (when efrit-agent--elapsed-timer
          (cancel-timer efrit-agent--elapsed-timer)
          (setq efrit-agent--elapsed-timer nil))
        (efrit-agent--render)))))

(defun efrit-agent-update-todos (todos)
  "Update the TODO list display with TODOS.
TODOS should be a list of plists with :status, :content, :activeForm."
  (let ((buffer (get-buffer efrit-agent-buffer-name)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq efrit-agent--todos todos)
        (efrit-agent--render)))))

(defun efrit-agent-add-activity (activity)
  "Add an ACTIVITY entry to the activity log.
ACTIVITY is a plist with :type, :timestamp, and type-specific fields."
  (let ((buffer (get-buffer efrit-agent-buffer-name)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        ;; Add unique ID if not present
        (unless (plist-get activity :id)
          (setq activity (plist-put activity :id (format "act-%d" (length efrit-agent--activities)))))
        ;; Append to end of list (O(1) with nconc when keeping tail pointer,
        ;; but for simplicity we use nconc which is O(n) but only traverses once)
        (setq efrit-agent--activities (nconc efrit-agent--activities (list activity)))
        (efrit-agent--render)))))

(defun efrit-agent-set-status (status)
  "Set the session STATUS.
STATUS should be one of: working, paused, waiting, complete, failed."
  (let ((buffer (get-buffer efrit-agent-buffer-name)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq efrit-agent--status status)
        (efrit-agent--render)))))

;;; Integration with efrit-do TODO tracking

(defun efrit-agent--convert-todo-item (todo)
  "Convert an efrit-do-todo-item struct TODO to plist format for display.
Uses proper accessor functions to avoid fragility when struct changes."
  (when (vectorp todo)
    (let* ((id (efrit-do-todo-item-id todo))
           (content (efrit-do-todo-item-content todo))
           (status (efrit-do-todo-item-status todo))
           ;; Convert efrit-do status to agent status
           (agent-status (pcase status
                           ('todo 'pending)
                           ('in-progress 'in_progress)
                           ('completed 'completed)
                           (_ status))))
      (list :id id
            :content content
            :status agent-status))))

(defun efrit-agent-sync-todos ()
  "Sync TODOs from `efrit-do--current-todos' to the agent buffer."
  (when (and (bound-and-true-p efrit-do--current-todos)
             (get-buffer efrit-agent-buffer-name))
    (let ((converted-todos
           (mapcar #'efrit-agent--convert-todo-item
                   efrit-do--current-todos)))
      (efrit-agent-update-todos converted-todos))))

(defun efrit-agent--on-todo-update (&rest _args)
  "Advice function called when TODOs are updated in efrit-do.
Syncs the updated TODOs to the agent buffer."
  (efrit-agent-sync-todos))

(defun efrit-agent-setup-todo-integration ()
  "Set up advice to sync TODOs from efrit-do to agent buffer.
Call this after loading efrit-do to enable real-time TODO updates."
  (when (fboundp 'efrit-do--update-todo-status)
    (advice-add 'efrit-do--update-todo-status :after #'efrit-agent--on-todo-update))
  ;; Also hook into todo_write tool handler for batch updates
  (when (fboundp 'efrit-do--handle-todo-write)
    (advice-add 'efrit-do--handle-todo-write :after #'efrit-agent--on-todo-update)))

(defun efrit-agent-remove-todo-integration ()
  "Remove advice for TODO syncing."
  (advice-remove 'efrit-do--update-todo-status #'efrit-agent--on-todo-update)
  (advice-remove 'efrit-do--handle-todo-write #'efrit-agent--on-todo-update))

;;; Integration with efrit-progress activity tracking

(defvar efrit-agent--activity-counter 0
  "Counter for generating unique activity IDs.")

(defun efrit-agent--on-tool-start (tool-name input)
  "Advice function called when a tool starts.
TOOL-NAME is the name of the tool being called.
INPUT is the tool's input parameters (preserved for expanded view)."
  (when (get-buffer efrit-agent-buffer-name)
    (efrit-agent-add-activity
     (list :id (format "tool-%d" (cl-incf efrit-agent--activity-counter))
           :type 'tool
           :tool tool-name
           :input input
           :timestamp (current-time)
           :result nil
           :success nil))))

(defun efrit-agent--on-tool-result (tool-name result success-p)
  "Advice function called when a tool completes.
Updates the most recent incomplete tool activity with RESULT and SUCCESS-P.
Uses reverse iteration to find the most recently started (not yet completed)
instance of TOOL-NAME, which correctly handles parallel tool calls."
  (let ((buffer (get-buffer efrit-agent-buffer-name)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        ;; Find the most recent incomplete activity for this tool
        ;; by iterating in reverse (most recent first)
        (let ((found nil)
              (activities-rev (reverse efrit-agent--activities)))
          (dolist (activity activities-rev)
            (when (and (not found)
                       (eq (plist-get activity :type) 'tool)
                       (equal (plist-get activity :tool) tool-name)
                       (null (plist-get activity :result)))
              (setq found t)
              ;; Calculate elapsed time from stored timestamp
              (let* ((start-time (plist-get activity :timestamp))
                     (elapsed (when start-time
                                (float-time (time-subtract (current-time) start-time)))))
                (plist-put activity :result
                           (truncate-string-to-width (format "%s" result) 100))
                (plist-put activity :success success-p)
                (when elapsed
                  (plist-put activity :elapsed elapsed))))))
        (efrit-agent--render)))))

(defun efrit-agent--on-message (message &optional type)
  "Advice function called when a message is shown.
MESSAGE is the text, TYPE indicates the message type."
  (when (get-buffer efrit-agent-buffer-name)
    (let ((activity-type (pcase type
                           ('claude 'message)
                           ('error 'error)
                           (_ 'message))))
      (efrit-agent-add-activity
       (list :id (format "msg-%d" (cl-incf efrit-agent--activity-counter))
             :type activity-type
             :text (truncate-string-to-width (or message "") 200)
             :timestamp (current-time))))))

(defun efrit-agent-setup-progress-integration ()
  "Set up advice to track activities from efrit-progress.
Call this after loading efrit-progress to enable real-time activity updates."
  (when (fboundp 'efrit-progress-show-tool-start)
    (advice-add 'efrit-progress-show-tool-start :after #'efrit-agent--on-tool-start))
  (when (fboundp 'efrit-progress-show-tool-result)
    (advice-add 'efrit-progress-show-tool-result :after #'efrit-agent--on-tool-result))
  (when (fboundp 'efrit-progress-show-message)
    (advice-add 'efrit-progress-show-message :after #'efrit-agent--on-message)))

(defun efrit-agent-remove-progress-integration ()
  "Remove advice for activity tracking."
  (advice-remove 'efrit-progress-show-tool-start #'efrit-agent--on-tool-start)
  (advice-remove 'efrit-progress-show-tool-result #'efrit-agent--on-tool-result)
  (advice-remove 'efrit-progress-show-message #'efrit-agent--on-message))

;;; Session lifecycle integration

(defun efrit-agent--on-session-start (session-id command)
  "Advice function called when a session starts.
Creates and displays the agent buffer for SESSION-ID with COMMAND."
  (efrit-agent-start-session session-id command)
  ;; Reset activity counter for new session
  (setq efrit-agent--activity-counter 0))

(defun efrit-agent--on-session-end (_session-id success-p)
  "Advice function called when a session ends.
Updates the agent buffer with SUCCESS-P status."
  (efrit-agent-end-session success-p))

(defun efrit-agent-setup-session-integration ()
  "Set up advice to track session lifecycle from efrit-progress.
This makes the agent buffer appear automatically when sessions start."
  (when (fboundp 'efrit-progress-start-session)
    (advice-add 'efrit-progress-start-session :after #'efrit-agent--on-session-start))
  (when (fboundp 'efrit-progress-end-session)
    (advice-add 'efrit-progress-end-session :after #'efrit-agent--on-session-end)))

(defun efrit-agent-remove-session-integration ()
  "Remove advice for session lifecycle tracking."
  (advice-remove 'efrit-progress-start-session #'efrit-agent--on-session-start)
  (advice-remove 'efrit-progress-end-session #'efrit-agent--on-session-end))

;;; User input integration

(defun efrit-agent--on-pending-question (_session question &optional options)
  "Advice function called when a pending question is set.
Updates the agent buffer with QUESTION and OPTIONS, sets status to waiting."
  (let ((buffer (get-buffer efrit-agent-buffer-name)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        ;; Store the pending question
        (setq efrit-agent--pending-question
              (list question
                    (when options (if (listp options) options (append options nil)))
                    (format-time-string "%Y-%m-%dT%H:%M:%S%z")))
        ;; Set status to waiting
        (setq efrit-agent--status 'waiting)
        (efrit-agent--render)))))

(defun efrit-agent--on-question-response (_session _response)
  "Advice function called when user responds to a question.
Clears the pending question and sets status back to working."
  (let ((buffer (get-buffer efrit-agent-buffer-name)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq efrit-agent--pending-question nil)
        (setq efrit-agent--status 'working)
        (efrit-agent--render)))))

(defun efrit-agent-setup-input-integration ()
  "Set up advice to track user input requests from session.
This makes the input section appear when Claude asks questions."
  (when (fboundp 'efrit-session-set-pending-question)
    (advice-add 'efrit-session-set-pending-question :after #'efrit-agent--on-pending-question))
  (when (fboundp 'efrit-session-respond-to-question)
    (advice-add 'efrit-session-respond-to-question :after #'efrit-agent--on-question-response)))

(defun efrit-agent-remove-input-integration ()
  "Remove advice for user input tracking."
  (advice-remove 'efrit-session-set-pending-question #'efrit-agent--on-pending-question)
  (advice-remove 'efrit-session-respond-to-question #'efrit-agent--on-question-response))

;;; Combined setup/teardown

(defun efrit-agent-setup-integration ()
  "Set up all integration hooks for the agent buffer.
Call this after loading efrit-do and efrit-progress."
  (efrit-agent-setup-todo-integration)
  (efrit-agent-setup-progress-integration)
  (efrit-agent-setup-session-integration)
  (efrit-agent-setup-input-integration))

(defun efrit-agent-remove-integration ()
  "Remove all integration hooks."
  (efrit-agent-remove-todo-integration)
  (efrit-agent-remove-progress-integration)
  (efrit-agent-remove-session-integration)
  (efrit-agent-remove-input-integration))

(provide 'efrit-agent)

;;; efrit-agent.el ends here
