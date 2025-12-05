;;; efrit-agent.el --- Agentic session buffer for Efrit -*- lexical-binding: t -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.4.1
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
;;
;; Module Structure:
;; - efrit-agent-core.el: State variables, region management, buffer lifecycle
;; - efrit-agent-render.el: Message rendering, streaming, thinking indicator
;; - efrit-agent-tools.el: Tool call display, expansion, diff formatting
;; - efrit-agent-input.el: Input minor mode, question handling
;; - efrit-agent-integration.el: Hook integrations with efrit-do/progress
;; - efrit-agent.el: Public API, faces, mode definition, keymaps (this file)

;;; Code:

(require 'cl-lib)
(require 'efrit-agent-core)
(require 'efrit-agent-render)
(require 'efrit-agent-tools)
(require 'efrit-agent-input)
(require 'efrit-agent-integration)

;; Forward declarations to silence byte-compiler
(declare-function efrit-executor-cancel "efrit-executor")
(declare-function efrit-executor-respond "efrit-executor")
(declare-function efrit-progress-inject "efrit-progress")

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

(defface efrit-agent-user-message
  '((t :foreground "gray90"))
  "Face for user messages (excluding the > prefix)."
  :group 'efrit-agent)

(defface efrit-agent-user-prefix
  '((t :foreground "gold" :weight bold))
  "Face for the > prefix on user messages."
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

(defface efrit-agent-thinking
  '((t :foreground "gray60" :slant italic))
  "Face for thinking indicator."
  :group 'efrit-agent)

;; Diff-related faces (inherit from diff-mode where appropriate)
(defface efrit-agent-diff-header
  '((t :inherit diff-file-header))
  "Face for diff file headers."
  :group 'efrit-agent)

(defface efrit-agent-diff-hunk-header
  '((t :inherit diff-hunk-header))
  "Face for diff hunk headers (@@ ... @@)."
  :group 'efrit-agent)

(defface efrit-agent-diff-added
  '((t :inherit diff-added))
  "Face for added lines in diffs."
  :group 'efrit-agent)

(defface efrit-agent-diff-removed
  '((t :inherit diff-removed))
  "Face for removed lines in diffs."
  :group 'efrit-agent)

(defface efrit-agent-diff-context
  '((t :inherit diff-context))
  "Face for context lines in diffs."
  :group 'efrit-agent)

;; Importance level faces (for tool results)
(defface efrit-agent-importance-normal
  '((t :foreground "gray80"))
  "Face for normal importance tool results."
  :group 'efrit-agent)

(defface efrit-agent-importance-success
  '((t :foreground "green3" :weight bold))
  "Face for successful tool results."
  :group 'efrit-agent)

(defface efrit-agent-importance-warning
  '((t :foreground "gold" :weight bold))
  "Face for warning-level tool results."
  :group 'efrit-agent)

(defface efrit-agent-importance-error
  '((t :foreground "red3" :weight bold))
  "Face for error-level tool results, visually prominent."
  :group 'efrit-agent)

(defface efrit-agent-success
  '((t :foreground "green3"))
  "Face for success messages."
  :group 'efrit-agent)

(defface efrit-agent-warning
  '((t :foreground "gold"))
  "Face for warning messages."
  :group 'efrit-agent)

;;; Keymap

(defvar efrit-agent-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Navigation
    (define-key map (kbd "TAB") #'efrit-agent-next-section)
    (define-key map (kbd "<backtab>") #'efrit-agent-prev-section)
    (define-key map (kbd "S-TAB") #'efrit-agent-prev-section)
    (define-key map (kbd "M-n") #'efrit-agent-next-tool)
    (define-key map (kbd "M-p") #'efrit-agent-previous-tool)
    (define-key map (kbd "n") #'efrit-agent-next-tool)
    (define-key map (kbd "p") #'efrit-agent-previous-tool)
    (define-key map (kbd "w") #'efrit-agent-copy-tool-output)

    ;; Actions
    (define-key map (kbd "q") #'efrit-agent-quit)
    (define-key map (kbd "k") #'efrit-agent-cancel)
    (define-key map (kbd "P") #'efrit-agent-pause)
    (define-key map (kbd "r") #'efrit-agent-resume)
    (define-key map (kbd "g") #'efrit-agent-refresh)
    (define-key map (kbd "RET") #'efrit-agent-toggle-expand)
    (define-key map (kbd "E") #'efrit-agent-expand-all)
    (define-key map (kbd "C") #'efrit-agent-collapse-all)
    (define-key map (kbd "v") #'efrit-agent-cycle-verbosity)
    (define-key map (kbd "M") #'efrit-agent-cycle-display-mode)
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

This buffer provides a conversation-first view of an Efrit agent session.
The buffer is divided into:
- Conversation region (read-only): messages and tool calls
- Input region (editable): where you type responses

Status is shown in the header-line at top of window.

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
  ;; Initialize user expansion state tracking (persists across buffer updates)
  (unless efrit-agent--expansion-state
    (setq efrit-agent--expansion-state (make-hash-table :test 'equal)))
  ;; Initialize region markers
  (efrit-agent--init-regions)
  ;; Set up header-line for status display
  (efrit-agent--setup-header-line)
  ;; Load global history on first use
  (unless efrit-agent--global-history
    (efrit-agent--load-history))
  ;; Enable input mode when point moves to input region
  (add-hook 'post-command-hook #'efrit-agent--maybe-enable-input-mode nil t)
  ;; Save session and clean up timer when buffer is killed
  (add-hook 'kill-buffer-hook #'efrit-agent--save-session-on-kill nil t)
  (add-hook 'kill-buffer-hook #'efrit-agent--cleanup-timer nil t))

;;; Interactive Commands

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

(defun efrit-agent-next-tool ()
  "Move to the next tool call in the buffer."
  (interactive)
  (let ((start (point))
        (found nil))
    ;; First, move past current tool if we're on one
    (when (get-text-property (point) 'efrit-id)
      (goto-char (or (next-single-property-change (point) 'efrit-id)
                     (point-max))))
    ;; Search for next tool-call
    (while (and (< (point) (point-max)) (not found))
      (let ((id (get-text-property (point) 'efrit-id))
            (type (get-text-property (point) 'efrit-type)))
        (if (and id (eq type 'tool-call))
            (setq found t)
          (goto-char (or (next-single-property-change (point) 'efrit-id)
                         (point-max))))))
    (if found
        (message "Tool: %s" (get-text-property (point) 'efrit-tool-name))
      (goto-char start)
      (message "No more tool calls"))))

(defun efrit-agent-previous-tool ()
  "Move to the previous tool call in the buffer."
  (interactive)
  (let ((start (point))
        (found nil))
    ;; Move before current position
    (when (> (point) (point-min))
      (goto-char (1- (point))))
    ;; Search backwards for tool-call
    (while (and (> (point) (point-min)) (not found))
      (let ((id (get-text-property (point) 'efrit-id))
            (type (get-text-property (point) 'efrit-type)))
        (if (and id (eq type 'tool-call))
            (setq found t)
          (goto-char (or (previous-single-property-change (point) 'efrit-id)
                         (point-min))))))
    ;; If we found one, make sure we're at the start of it
    (when found
      (let ((id (get-text-property (point) 'efrit-id)))
        (while (and (> (point) (point-min))
                    (equal (get-text-property (1- (point)) 'efrit-id) id))
          (goto-char (1- (point))))))
    (if found
        (message "Tool: %s" (get-text-property (point) 'efrit-tool-name))
      (goto-char start)
      (message "No more tool calls"))))

(defun efrit-agent-copy-tool-output ()
  "Copy the current tool's full result to the kill ring."
  (interactive)
  (let* ((tool-id (get-text-property (point) 'efrit-id))
         (tool-type (get-text-property (point) 'efrit-type)))
    (unless (and tool-id (eq tool-type 'tool-call))
      (user-error "No tool at point"))
    (let ((result (get-text-property (point) 'efrit-tool-result))
          (name (get-text-property (point) 'efrit-tool-name)))
      (if result
          (progn
            (kill-new (format "%s" result))
            (message "Copied %s result (%d chars)" name (length (format "%s" result))))
        (message "Tool %s has no result yet" name)))))

(defun efrit-agent-toggle-expand ()
  "Toggle expansion of the tool call at point.
In conversation region, expands/collapses tool calls to show input and results."
  (interactive)
  ;; Try new conversation-first tool expansion
  (unless (efrit-agent--toggle-tool-expansion)
    ;; Fall back to old behavior for legacy activity items
    (when-let* ((item-id (get-text-property (point) 'efrit-agent-item-id)))
      (if (gethash item-id efrit-agent--expanded-items)
          (remhash item-id efrit-agent--expanded-items)
        (puthash item-id t efrit-agent--expanded-items))
      (efrit-agent--render))))

(defun efrit-agent-expand-all ()
  "Expand all tool calls in the buffer.
Sets user expansion state for all tools, overriding display-mode and hints."
  (interactive)
  (let ((count 0))
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
        (let ((tool-id (get-text-property (point) 'efrit-id))
              (tool-type (get-text-property (point) 'efrit-type))
              (expanded (get-text-property (point) 'efrit-tool-expanded)))
          (when (and tool-id (eq tool-type 'tool-call))
            ;; Record user preference for expansion
            (when efrit-agent--expansion-state
              (puthash tool-id t efrit-agent--expansion-state))
            ;; Expand if not already
            (unless expanded
              (efrit-agent--toggle-tool-expansion)
              (cl-incf count))))
        (goto-char (or (next-single-property-change (point) 'efrit-id)
                       (point-max)))))
    (message "Expanded %d tool call%s" count (if (= count 1) "" "s"))))

(defun efrit-agent-collapse-all ()
  "Collapse all tool calls in the buffer.
Sets user expansion state for all tools, overriding display-mode and hints."
  (interactive)
  (let ((count 0))
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
        (let ((tool-id (get-text-property (point) 'efrit-id))
              (tool-type (get-text-property (point) 'efrit-type))
              (expanded (get-text-property (point) 'efrit-tool-expanded)))
          (when (and tool-id (eq tool-type 'tool-call))
            ;; Record user preference for collapse
            (when efrit-agent--expansion-state
              (puthash tool-id nil efrit-agent--expansion-state))
            ;; Collapse if expanded
            (when expanded
              (efrit-agent--toggle-tool-expansion)
              (cl-incf count))))
        (goto-char (or (next-single-property-change (point) 'efrit-id)
                       (point-max)))))
    (message "Collapsed %d tool call%s" count (if (= count 1) "" "s"))))

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

(defun efrit-agent-cycle-display-mode ()
  "Cycle through display modes (minimal/smart/verbose).
minimal: All tool results collapsed, ignore auto_expand hints.
smart: Respect Claude's auto_expand hints.
verbose: All tool results expanded, ignore auto_expand hints."
  (interactive)
  (setq efrit-agent-display-mode
        (pcase efrit-agent-display-mode
          ('minimal 'smart)
          ('smart 'verbose)
          ('verbose 'minimal)))
  (message "Display mode: %s" efrit-agent-display-mode)
  (force-mode-line-update))

(defun efrit-agent-help ()
  "Show help for agent buffer key bindings."
  (interactive)
  (let ((help-text
         (format "Efrit Agent Buffer Help
%s

Navigation:
  TAB / S-TAB    Move between sections
  n / M-n        Next tool call
  p / M-p        Previous tool call (when not in input)
  RET            Expand/collapse tool call at point
  w              Copy tool result to kill ring

Actions:
  q              Quit buffer (session continues)
  k              Kill/cancel session
  p              Pause session
  r              Resume paused session
  g              Refresh display
  E              Expand all tool calls
  C              Collapse all tool calls
  v              Cycle verbosity (minimal/normal/verbose)
  M              Cycle display mode (minimal/smart/verbose)

Verbosity Levels:
  minimal        Show 20 chars result, 3 lines when expanded
  normal         Show 40 chars result, 10 lines when expanded
  verbose        Show 80 chars result, 50 lines when expanded

Display Modes:
  minimal        All tool results collapsed (ignore hints)
  smart          Respect Claude's auto_expand hints (default)
  verbose        All tool results expanded (ignore hints)

Expand/Collapse:
  Tool calls show %s (collapsed) or %s (expanded)
  Press RET on a tool call to toggle details
  Expanded view shows input parameters and full result

Display Style:
  Current: %s
  Set `efrit-agent-display-style' to 'ascii for terminal compatibility

Input (when in input region):
  RET            Send single-line input (or newline for multi-line)
  S-RET          Insert newline (for multi-line input)
  C-c C-c        Send input
  C-c C-s        Send input
  C-c C-k        Clear input
  M-p            Previous input history
  M-n            Next input history (or restore what you were typing)
  TAB            Context-aware completion
  1-4            Select option 1-4 (when options available)
  i              Inject guidance to Claude mid-session

Completion:
  TAB completes based on conversation context:
  - File paths from tool calls (read_file, write_file, etc.)
  - Options from pending questions (1, 2, 3, 4 and option text)
  - Common responses (yes, no, continue, cancel, skip)

History:
  Input history is saved per-session and globally.
  Global history persists across Emacs restarts.
  Configure with `efrit-agent-history-file' and
  `efrit-agent-history-max-size'.

Press q to close this help."
                 (make-string 43 ?=)
                 (efrit-agent--char 'expand-collapsed)
                 (efrit-agent--char 'expand-expanded)
                 efrit-agent-display-style)))
    (with-help-window "*Efrit Agent Help*"
      (princ help-text))))

(defun efrit-agent-send-input ()
  "Send user input to the session.
This is a wrapper that delegates to efrit-agent-input-send from the input module.
The actual implementation reads from the input region and routes appropriately."
  (interactive)
  (require 'efrit-agent-input)
  (call-interactively #'efrit-agent-input-send))

(defun efrit-agent-abort ()
  "Abort the current operation."
  (interactive)
  (efrit-agent-cancel))

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

;;; Rendering (legacy full-buffer render)

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
                            ('minimal 30)
                            ('normal 70)
                            ('verbose 120)))
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
(defun efrit ()
  "Open the Efrit REPL-style agent buffer.
Type at the > prompt to start a session, or continue an active one.

This is the recommended way to interact with Efrit.
For one-off commands without the REPL UI, use \\[efrit-do] instead."
  (interactive)
  (efrit-agent-open))

;;;###autoload
(defun efrit-agent-open ()
  "Open or switch to the Efrit agent buffer in idle mode.
Provides a persistent prompt buffer for interacting with Efrit.
Type at the > prompt to start a session.

This is the recommended entry point for the REPL-style Efrit interface."
  (interactive)
  (let ((buffer (efrit-agent--get-buffer)))
    (with-current-buffer buffer
      ;; Initialize mode if not already done
      (unless (derived-mode-p 'efrit-agent-mode)
        (efrit-agent-mode))
      ;; Initialize regions if not set up
      (unless (and efrit-agent--conversation-end
                   (marker-position efrit-agent--conversation-end))
        (efrit-agent--init-regions)
        (efrit-agent--setup-regions))
      ;; Set idle state if no active session
      (unless efrit-agent--session-id
        (setq efrit-agent--status 'idle)
        (setq efrit-agent--start-time nil)))
    ;; Display at bottom and focus
    (display-buffer buffer '(display-buffer-at-bottom (window-height . 15)))
    (when-let ((win (get-buffer-window buffer)))
      (select-window win))
    ;; Move point to input region
    (with-current-buffer buffer
      (when (and efrit-agent--input-start
                 (marker-position efrit-agent--input-start))
        (goto-char efrit-agent--input-start)))))

;;;###autoload
(defun efrit-agent ()
  "Open or switch to the Efrit agent buffer."
  (interactive)
  (let ((buffer (efrit-agent--get-buffer)))
    ;; Initialize mode if not already done
    (unless (eq (buffer-local-value 'major-mode buffer) 'efrit-agent-mode)
      (with-current-buffer buffer
        (efrit-agent-mode)))
    (pop-to-buffer buffer)))

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
        ;; Force header-line update instead of full re-render
        (force-mode-line-update)))))

(defun efrit-agent-end-session (success-p)
  "End the current agent session.
SUCCESS-P determines whether to show complete or failed status."
  (efrit-agent-set-status (if success-p 'complete 'failed)))

(defun efrit-agent-start-session (session-id command)
  "Start agent buffer for SESSION-ID with COMMAND.
Creates and displays the agent buffer, initializes session tracking,
and sets status to working."
  (let ((buffer (efrit-agent--get-buffer)))
    ;; Initialize mode if not already done
    (unless (eq (buffer-local-value 'major-mode buffer) 'efrit-agent-mode)
      (with-current-buffer buffer
        (efrit-agent-mode)))
    ;; Set session context
    (with-current-buffer buffer
      (setq efrit-agent--session-id session-id)
      (setq efrit-agent--command command)
      (setq efrit-agent--status 'working)
      (efrit-agent--render))
    ;; Display the buffer
    (pop-to-buffer buffer)))

(defun efrit-agent-add-message (text &optional type)
  "Add a message with TEXT to the conversation.
TYPE can be:
  nil or `user' - User message with > prefix
  `claude' - Claude's response
  `error' - Error message with error styling"
  (let ((buffer (get-buffer efrit-agent-buffer-name)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (pcase type
          ((or 'nil 'user) (efrit-agent--add-user-message text))
          ('claude (efrit-agent--add-claude-message text))
          ('error
           (efrit-agent--append-to-conversation
            (concat (propertize (format "%s Error: " (efrit-agent--char 'error-icon))
                                'face 'efrit-agent-error)
                    (propertize text 'face 'efrit-agent-error)
                    "\n\n")
            (list 'efrit-type 'error-message
                  'efrit-id (format "err-%d" (cl-incf efrit-agent--message-counter))))))))))

(defun efrit-agent-show-tool-start (tool-name &optional input)
  "Show that TOOL-NAME has started with optional INPUT.
Returns a tool-id that can be used with `efrit-agent-show-tool-result'.
Uses incremental update - does not trigger full re-render."
  (let ((buffer (get-buffer efrit-agent-buffer-name)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (efrit-agent--add-tool-call tool-name input)))))

(defun efrit-agent-show-tool-result (tool-id result success-p &optional elapsed)
  "Update tool TOOL-ID with RESULT, SUCCESS-P status, and optional ELAPSED time.
Uses in-place update - does not trigger full re-render."
  (let ((buffer (get-buffer efrit-agent-buffer-name)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (efrit-agent--update-tool-result tool-id result success-p elapsed)))))

(defun efrit-agent-show-question (question &optional options)
  "Display a QUESTION from Claude with optional OPTIONS for the user to select.
OPTIONS is a list of strings representing the available choices.
Returns a question-id for tracking.
Uses incremental update - does not trigger full re-render."
  (let ((buffer (get-buffer efrit-agent-buffer-name)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        ;; Store for keyboard shortcut handling
        (setq efrit-agent--pending-question
              (list question options (format-time-string "%Y-%m-%dT%H:%M:%S%z")))
        (setq efrit-agent--status 'waiting)
        ;; Add to conversation incrementally
        (efrit-agent--add-question question options)))))

(defun efrit-agent-stream-content (text)
  "Stream TEXT content from Claude to the conversation.
Consecutive calls append to the same message until a non-text event occurs.
This provides smooth character-by-character or chunk-by-chunk display."
  (let ((buffer (get-buffer efrit-agent-buffer-name)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (efrit-agent--add-claude-message text)))))

(defun efrit-agent-stream-end ()
  "End the current streaming message.
Call this when Claude's text response is complete."
  (let ((buffer (get-buffer efrit-agent-buffer-name)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (efrit-agent--stream-end-message)))))

(defun efrit-agent-show-thinking (&optional text)
  "Show the thinking indicator with optional TEXT description.
Call this when Claude is processing but no tool is running.
The indicator will automatically hide when content starts arriving."
  (let ((buffer (get-buffer efrit-agent-buffer-name)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (efrit-agent--show-thinking text)))))

(defun efrit-agent-hide-thinking ()
  "Hide the thinking indicator.
Usually not needed as it hides automatically when content arrives."
  (let ((buffer (get-buffer efrit-agent-buffer-name)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (efrit-agent--hide-thinking)))))

(defun efrit-agent-update-thinking (text)
  "Update the thinking indicator with new TEXT.
Use this to show progress during thinking, e.g., \"analyzing code...\"."
  (let ((buffer (get-buffer efrit-agent-buffer-name)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (efrit-agent--update-thinking text)))))

(defun efrit-agent-show-todos (todos)
  "Display or update TODOS inline in the conversation.
TODOS is a list of plists with :status, :content, :id.
Status can be: pending, in_progress, completed.
Updates in-place if TODOs already exist in the conversation."
  (let ((buffer (get-buffer efrit-agent-buffer-name)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (setq efrit-agent--todos todos)
        (efrit-agent--add-todos-inline todos)))))

        ;;; Integration Setup
        ;;
        ;; Set up hooks to connect with efrit-do, efrit-progress, and session lifecycle.
        ;; This happens on module load to ensure real-time updates are connected.

        (require 'efrit-agent-integration)
        (efrit-agent-setup-integration)

        (provide 'efrit-agent)

;;; efrit-agent.el ends here
