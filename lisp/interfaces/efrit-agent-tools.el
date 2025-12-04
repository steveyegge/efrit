;;; efrit-agent-tools.el --- Tool call display for efrit-agent -*- lexical-binding: t -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; Tool display module for efrit-agent providing:
;; - Tool call display in conversation
;; - Expansion/collapse toggle for tool details
;; - Inline diff formatting with syntax highlighting
;; - Error recovery action buttons

;;; Code:

(require 'cl-lib)
(require 'efrit-agent-core)
(require 'efrit-agent-render)
(require 'efrit-do-dispatch)

;; Forward declarations
(declare-function efrit-executor-cancel "efrit-executor")
(declare-function efrit-progress-inject "efrit-progress")

;;; Tool Call Display

(defun efrit-agent--add-tool-call (tool-name &optional input)
  "Add a tool call indicator for TOOL-NAME with optional INPUT.
Tool calls appear inline in the conversation.
Returns the tool ID for later update with result."
  ;; End any streaming Claude message first
  (efrit-agent--stream-end-message)
  ;; Hide thinking indicator when tool starts
  (efrit-agent--hide-thinking)
  (let* ((tool-id (format "tool-%d" (cl-incf efrit-agent--message-counter)))
         (formatted-text
          (concat
           "  "
           (propertize (format "%s " (efrit-agent--char 'tool-running))
                       'face 'efrit-agent-timestamp)
           (propertize tool-name 'face 'efrit-agent-tool-name)
           (propertize "..." 'face 'efrit-agent-timestamp)
           "\n")))
    ;; Store the tool info for later result update
    (efrit-agent--append-to-conversation
     formatted-text
     (list 'efrit-type 'tool-call
           'efrit-id tool-id
           'efrit-tool-name tool-name
           'efrit-tool-input input
           'efrit-tool-running t
           'efrit-tool-start-time (current-time)))
    tool-id))

(defun efrit-agent--find-tool-region (tool-id)
  "Find the buffer region for tool call with TOOL-ID.
Returns (START . END) or nil if not found."
  (save-excursion
    (goto-char (point-min))
    (let ((start nil)
          (end nil))
      ;; Search for the tool-id in text properties
      (while (and (not start) (< (point) (point-max)))
        (let ((id (get-text-property (point) 'efrit-id)))
          (if (equal id tool-id)
              (setq start (point))
            (goto-char (or (next-single-property-change (point) 'efrit-id)
                           (point-max))))))
      (when start
        ;; Find where this tool's properties end
        (setq end (or (next-single-property-change start 'efrit-id)
                      (point-max)))
        (cons start end)))))

(defun efrit-agent--update-tool-result (tool-id result success-p &optional elapsed)
  "Update tool call with result and status.
TOOL-ID identifies the tool. RESULT is the result value.
SUCCESS-P indicates if the call succeeded. ELAPSED is optional time."
  (let ((region (efrit-agent--find-tool-region tool-id)))
    (when region
      (let* ((inhibit-read-only t)
             (start (car region))
             (end (cdr region))
             ;; Get stored properties from the tool call
             (tool-name (get-text-property start 'efrit-tool-name))
             (tool-input (get-text-property start 'efrit-tool-input))
             (start-time (get-text-property start 'efrit-tool-start-time))
             ;; Calculate elapsed if not provided
             (elapsed-time (or elapsed
                               (when start-time
                                 (float-time (time-subtract (current-time) start-time)))))
             ;; Auto-expand short results, keep long ones collapsed
             (auto-expand (and success-p
                               (< (length result) 200)
                               (< (cl-count ?\n result) 5)))
             ;; Format indicators - expanded or collapsed based on size
             (expand-char (efrit-agent--char (if auto-expand 'expand-expanded 'expand-collapsed)))
             (status-char (if success-p
                              (efrit-agent--char 'tool-success)
                            (efrit-agent--char 'tool-failure)))
             (status-face (if success-p nil 'efrit-agent-error))
             ;; Build the updated tool line (collapsed view)
             ;; Defensive: stringify result before truncate-string-to-width to handle
             ;; any malformed results, though tool handlers are contracted to return strings
             (result-summary (truncate-string-to-width
                              (replace-regexp-in-string "[\n\r]+" " " (format "%s" result))
                              (pcase efrit-agent-verbosity
                                ('minimal 20)
                                ('normal 40)
                                ('verbose 80))))
             (new-text
              (concat
               "  "
               (propertize (format "%s " expand-char) 'face 'efrit-agent-timestamp)
               (propertize (format "%s " status-char)
                           'face (if success-p 'efrit-agent-timestamp 'efrit-agent-error))
               (propertize (or tool-name "tool") 'face 'efrit-agent-tool-name)
               (when elapsed-time
                 (propertize (format " (%.2fs)" elapsed-time) 'face 'efrit-agent-timestamp))
               " -> "
               (propertize result-summary 'face status-face)
               "\n")))
        ;; Replace the tool call region with updated content
        (save-excursion
          (goto-char start)
          (delete-region start end)
          (insert new-text)
          ;; Re-apply properties for future reference (collapsed state)
          (add-text-properties start (point)
                               (list 'efrit-type 'tool-call
                                     'efrit-id tool-id
                                     'efrit-tool-name tool-name
                                     'efrit-tool-input tool-input
                                     'efrit-tool-result result
                                     'efrit-tool-success success-p
                                     'efrit-tool-elapsed elapsed-time
                                     'efrit-tool-running nil
                                     'efrit-tool-expanded auto-expand
                                     'read-only t)))))))

;;; Tool Call Expansion (collapsed/expanded toggle)

(defun efrit-agent--format-tool-expansion (tool-input result success-p &optional tool-id render-type)
  "Format the expansion content for a tool call.
TOOL-INPUT is the input parameters, RESULT is the output.
SUCCESS-P indicates status. TOOL-ID enables error recovery buttons.
RENDER-TYPE (optional) hints how to format result (text, diff, elisp, json, shell, grep, markdown, error).
Diffs are syntax-highlighted if `efrit-agent-show-diff' is non-nil."
  (let ((indent "       ")
        (max-lines (pcase efrit-agent-verbosity
                     ('minimal 3)
                     ('normal 10)
                     ('verbose 50))))
    (concat
     ;; Input section
     (when tool-input
       (concat
        indent (propertize "Input: " 'face 'efrit-agent-section-header) "\n"
        (efrit-agent--format-indented-lines
         (pp-to-string tool-input) indent max-lines)))
     ;; Result section (with render-type aware formatting)
     (when result
       (concat
        indent (propertize (if success-p "Result: " "Error: ")
                           'face (if success-p 'efrit-agent-section-header 'efrit-agent-error))
        "\n"
        ;; Use render-type if available, otherwise use old diff detection
        (if render-type
            (efrit-agent--format-by-render-type (format "%s" result) render-type indent max-lines)
          (efrit-agent--format-tool-result-with-diff result success-p indent max-lines))))
     ;; Error recovery buttons (only when tool failed and tool-id is known)
     (when (and tool-id (not success-p))
       (efrit-agent--format-error-recovery-buttons tool-id result tool-input))
     ;; Separator
     indent (propertize (make-string 50 (efrit-agent--char 'box-horizontal))
                        'face 'efrit-agent-timestamp) "\n")))

(defun efrit-agent--format-indented-lines (text indent max-lines)
  "Format TEXT with INDENT prefix, limiting to MAX-LINES."
  (let* ((lines (split-string text "\n" t))
         (truncated (> (length lines) max-lines))
         (display-lines (seq-take lines max-lines))
         (result ""))
    (dolist (line display-lines)
      (setq result (concat result indent "  "
                           (propertize line 'face 'efrit-agent-session-id) "\n")))
    (when truncated
      (setq result (concat result indent "  "
                           (propertize (format "... (%d more lines)"
                                               (- (length lines) max-lines))
                                       'face 'efrit-agent-timestamp) "\n")))
    result))

(defun efrit-agent--format-code-block (text language indent max-lines)
  "Format TEXT as a code block with LANGUAGE syntax highlighting.
TEXT is the code content, LANGUAGE is the mode name (elisp, python, bash, etc).
INDENT is the prefix for each line, MAX-LINES limits output.
Returns formatted string with syntax highlighting applied via font-lock."
  (let* ((mode (intern-soft (concat language "-mode")))
         (highlighted
          (if (and mode (fboundp mode))
              (with-temp-buffer
                (insert text)
                ;; Load the mode and ensure font-lock highlighting
                (funcall mode)
                (font-lock-ensure)
                ;; Return the buffer contents with faces preserved
                (buffer-string))
            ;; Fallback if mode not available
            text)))
    (efrit-agent--format-indented-lines highlighted indent max-lines)))

(defun efrit-agent--format-by-render-type (text render-type indent max-lines)
  "Format TEXT with highlighting based on RENDER-TYPE.
RENDER-TYPE should be one of: text, diff, elisp, json, shell, grep, markdown, error.
INDENT is the prefix for each line, MAX-LINES limits output."
  (pcase render-type
    ;; Diff format - unified diff with color highlighting
    ('diff
     (if (efrit-agent--diff-content-p text)
         (efrit-agent--format-diff-content text indent max-lines)
       ;; Fallback if text doesn't look like diff
       (efrit-agent--format-indented-lines text indent max-lines)))
    
    ;; Emacs Lisp format
    ('elisp
     (efrit-agent--format-code-block text "emacs-lisp" indent max-lines))
    
    ;; Shell script format
    ('shell
     (efrit-agent--format-code-block text "shell" indent max-lines))
    
    ;; JSON format - try formatting as JSON first, then apply syntax highlighting
    ('json
     (efrit-agent--format-code-block text "json" indent max-lines))
    
    ;; Grep output - treat like text for now, could add grep-specific highlighting later
    ('grep
     (efrit-agent--format-indented-lines text indent max-lines))
    
    ;; Markdown format
    ('markdown
     (efrit-agent--format-code-block text "markdown" indent max-lines))
    
    ;; Error format - same as text, but caller should style differently
    ('error
     (efrit-agent--format-indented-lines text indent max-lines))
    
    ;; Text format (default)
    (_ (efrit-agent--format-indented-lines text indent max-lines))))

;;; Inline Diff Display
;;
;; Functions for detecting and formatting unified diff content in tool results.

(defun efrit-agent--diff-content-p (text)
  "Return non-nil if TEXT appears to contain unified diff content.
Checks for common diff markers like --- and +++ file headers,
@@ hunk headers, or lines starting with + or - followed by content."
  (and (stringp text)
       (or
        ;; Check for unified diff file headers
        (string-match-p "^---\\s-+\\S-+" text)
        ;; Check for hunk headers
        (string-match-p "^@@\\s--?[0-9]" text)
        ;; Check for diff output from vcs_diff tool (has diff field)
        (string-match-p "^diff --git" text))))

(defun efrit-agent--extract-diff-from-result (result)
  "Extract diff content from RESULT if present.
RESULT may be a string containing diff directly, or an alist with a diff field.
Returns the diff string or nil if no diff content found."
  (cond
   ;; Result is an alist with 'diff key (from vcs_diff tool)
   ((and (listp result) (assoc 'diff result))
    (cdr (assoc 'diff result)))
   ;; Result is a string that looks like diff
   ((and (stringp result) (efrit-agent--diff-content-p result))
    result)
   ;; Check if stringified result looks like diff
   ((let ((str (format "%s" result)))
      (when (efrit-agent--diff-content-p str)
        str)))
   (t nil)))

(defun efrit-agent--format-diff-line (line indent)
  "Format a single diff LINE with appropriate face and INDENT prefix.
Returns the formatted line string with text properties."
  (let* ((line-content (concat indent "  " line "\n"))
         (face (cond
                ;; File headers
                ((string-match-p "^diff --git\\|^---\\|^\\+\\+\\+" line)
                 'efrit-agent-diff-header)
                ;; Hunk headers
                ((string-match-p "^@@" line)
                 'efrit-agent-diff-hunk-header)
                ;; Added lines
                ((string-match-p "^\\+" line)
                 'efrit-agent-diff-added)
                ;; Removed lines
                ((string-match-p "^-" line)
                 'efrit-agent-diff-removed)
                ;; Context lines (including empty lines and lines starting with space)
                (t
                 'efrit-agent-diff-context))))
    (propertize line-content 'face face)))

(defun efrit-agent--extract-code-block (text)
  "Extract a code block from TEXT if present.
Returns (language . content) or nil if no code block found.
Looks for markdown-style ```language...``` blocks."
  (when (string-match "^```\\([a-z-]*\\)\n\\(.*\\)\n```$" text)
    (let ((language (match-string 1 text))
          (content (match-string 2 text)))
      ;; Use language if specified, default to text
      (cons (or (and (> (length language) 0) language) "text") content))))

(defun efrit-agent--format-diff-content (diff-text indent max-lines)
  "Format DIFF-TEXT with syntax highlighting and INDENT prefix.
Limits output to MAX-LINES. Returns formatted string with faces."
  (let* ((lines (split-string diff-text "\n"))
         (truncated (> (length lines) max-lines))
         (display-lines (seq-take lines max-lines))
         (result ""))
    (dolist (line display-lines)
      (setq result (concat result (efrit-agent--format-diff-line line indent))))
    (when truncated
      (setq result (concat result indent "  "
                           (propertize (format "... (%d more lines)"
                                               (- (length lines) max-lines))
                                       'face 'efrit-agent-timestamp) "\n")))
    result))

(defun efrit-agent--format-tool-result-with-diff (result _success-p indent max-lines)
  "Format tool RESULT, detecting and highlighting diff/code block content.
_SUCCESS-P is reserved for future use (error styling).
INDENT is the prefix for each line.
MAX-LINES limits the output.
Returns formatted string with appropriate faces."
  (let ((result-str (format "%s" result)))
    ;; Try to detect and format code blocks first
    (let ((code-block (efrit-agent--extract-code-block result-str)))
      (if code-block
          ;; Found code block - format with syntax highlighting
          (efrit-agent--format-code-block (cdr code-block) (car code-block) indent max-lines)
        ;; No code block - check for diff
        (if (not efrit-agent-show-diff)
            ;; Diff display disabled, use regular formatting
            (efrit-agent--format-indented-lines result-str indent max-lines)
          ;; Try to extract and format diff content
          (let ((diff-content (efrit-agent--extract-diff-from-result result)))
            (if diff-content
                ;; Found diff content - format with syntax highlighting
                (efrit-agent--format-diff-content diff-content indent max-lines)
              ;; No diff content - use regular formatting
              (efrit-agent--format-indented-lines result-str indent max-lines))))))))

;;; Error Recovery Actions
;;
;; When a tool fails, provide actionable buttons: [Retry] [Skip] [Abort]
;; These allow users to recover from errors without restarting the session.

(defun efrit-agent--store-failed-tool (tool-id tool-name tool-input tool-item)
  "Store failed tool context for TOOL-ID with TOOL-NAME, TOOL-INPUT, and TOOL-ITEM.
This allows the tool to be retried later."
  (setf (alist-get tool-id efrit-agent--failed-tools nil nil #'equal)
        (list :name tool-name :input tool-input :item tool-item)))

(defun efrit-agent--get-failed-tool (tool-id)
  "Get the stored context for failed tool TOOL-ID.
Returns plist (:name :input :item) or nil if not found."
  (alist-get tool-id efrit-agent--failed-tools nil nil #'equal))

(defun efrit-agent--make-error-recovery-button (label action tool-id &optional help-echo)
  "Create an error recovery button with LABEL that runs ACTION for TOOL-ID.
ACTION is a function that takes TOOL-ID as argument.
HELP-ECHO is optional tooltip text."
  (let ((map (make-sparse-keymap))
        (action-fn (lambda ()
                     (interactive)
                     (funcall action tool-id))))
    (define-key map [mouse-1] action-fn)
    (define-key map (kbd "RET") action-fn)
    (propertize (concat "[" label "]")
                'face 'efrit-agent-button
                'mouse-face 'efrit-agent-button-hover
                'keymap map
                'help-echo (or help-echo (format "Click to %s" (downcase label))))))

(defun efrit-agent--retry-tool (tool-id)
  "Retry the failed tool identified by TOOL-ID."
  (interactive)
  (let ((tool-ctx (efrit-agent--get-failed-tool tool-id)))
    (if (not tool-ctx)
        (message "Cannot retry: tool context not found for %s" tool-id)
      (let ((tool-item (plist-get tool-ctx :item))
            (tool-name (plist-get tool-ctx :name)))
        (message "Retrying %s..." tool-name)
        ;; Execute the tool again through efrit-do
        (condition-case err
            (let ((result (efrit-do--execute-tool tool-item)))
              ;; Update the display with success
              (efrit-agent--update-tool-result tool-id result t)
              ;; Remove from failed tools list
              (setf (alist-get tool-id efrit-agent--failed-tools nil 'remove #'equal) nil)
              (message "Retry succeeded: %s" tool-name))
          (error
           (message "Retry failed: %s" (error-message-string err))))))))

(defun efrit-agent--skip-tool (tool-id)
  "Skip the failed tool TOOL-ID and tell Claude to continue.
Injects a message telling Claude the tool failed and to proceed without it."
  (interactive)
  (let ((tool-ctx (efrit-agent--get-failed-tool tool-id)))
    (when tool-ctx
      (let ((tool-name (plist-get tool-ctx :name)))
        ;; Inject guidance to continue
        (when (and efrit-agent--session-id
                   (fboundp 'efrit-progress-inject))
          (efrit-progress-inject
           efrit-agent--session-id
           'guidance
           (format "The %s tool failed. Please continue without this result and find an alternative approach." tool-name)))
        ;; Remove from failed tools list
        (setf (alist-get tool-id efrit-agent--failed-tools nil 'remove #'equal) nil)
        (message "Skipped %s - Claude will continue" tool-name)))))

(defun efrit-agent--abort-session (_tool-id)
  "Abort the current session due to tool failure.
TOOL-ID is ignored but required for button callback signature."
  (interactive)
  (when (yes-or-no-p "Abort the current session? ")
    (efrit-executor-cancel)
    (message "Session aborted")))

(defun efrit-agent--permission-error-p (result)
  "Return non-nil if RESULT indicates a permission error."
  (and (stringp result)
       (or (string-match-p "permission denied" result)
           (string-match-p "Permission denied" result)
           (string-match-p "read-only" result)
           (string-match-p "not writable" result))))

(defun efrit-agent--extract-file-path (tool-input result)
  "Try to extract a file path from TOOL-INPUT or RESULT.
Returns the path string or nil."
  (cond
   ;; Check tool-input for common file path keys
   ((and (listp tool-input)
         (or (alist-get 'path tool-input)
             (alist-get 'file tool-input)
             (alist-get 'file_path tool-input)
             (alist-get :path tool-input)
             (alist-get :file tool-input))))
   ;; Try to extract from result string
   ((and (stringp result)
         (string-match "\\(/[^ \n\t:]+\\)" result))
    (match-string 1 result))
   (t nil)))

(defun efrit-agent--make-writable (tool-id)
  "Attempt to make a file writable for the failed tool TOOL-ID."
  (interactive)
  (let ((tool-ctx (efrit-agent--get-failed-tool tool-id)))
    (if (not tool-ctx)
        (message "Cannot find tool context for %s" tool-id)
      (let* ((tool-input (plist-get tool-ctx :input))
             (file-path (efrit-agent--extract-file-path tool-input nil)))
        (if (not file-path)
            (message "Cannot determine file path from tool input")
          (if (not (file-exists-p file-path))
              (message "File does not exist: %s" file-path)
            (condition-case err
                (progn
                  (set-file-modes file-path
                                  (logior (file-modes file-path) #o200))
                  (message "Made %s writable. Use [Retry] to try again." file-path))
              (error
               (message "Failed to make writable: %s" (error-message-string err))))))))))

(defun efrit-agent--format-error-recovery-buttons (tool-id result _tool-input)
  "Format error recovery buttons for failed tool TOOL-ID.
RESULT is the error message, _TOOL-INPUT is reserved for future use."
  (let ((indent "       ")
        (buttons nil))
    ;; Always show Retry
    (push (efrit-agent--make-error-recovery-button
           "Retry" #'efrit-agent--retry-tool tool-id
           "Retry this tool with the same inputs")
          buttons)
    ;; Always show Skip
    (push (efrit-agent--make-error-recovery-button
           "Skip" #'efrit-agent--skip-tool tool-id
           "Skip this tool and tell Claude to continue")
          buttons)
    ;; Show Make Writable for permission errors
    (when (efrit-agent--permission-error-p result)
      (push (efrit-agent--make-error-recovery-button
             "Make Writable" #'efrit-agent--make-writable tool-id
             "Make the file writable (chmod +w)")
            buttons))
    ;; Always show Abort
    (push (efrit-agent--make-error-recovery-button
           "Abort" #'efrit-agent--abort-session tool-id
           "Abort the entire session")
          buttons)
    ;; Format buttons in a row
    (concat indent
            (mapconcat #'identity (nreverse buttons) " ")
            "\n")))

;;; Tool Expansion Toggle

(defun efrit-agent--toggle-tool-expansion ()
  "Toggle expansion of the tool call at point.
Returns t if toggled, nil if no tool at point."
  (let* ((tool-id (get-text-property (point) 'efrit-id))
         (tool-type (get-text-property (point) 'efrit-type)))
    (when (and tool-id (eq tool-type 'tool-call))
      (let* ((region (efrit-agent--find-tool-region tool-id))
             (start (car region))
             (end (cdr region))
             (expanded (get-text-property start 'efrit-tool-expanded))
             (tool-name (get-text-property start 'efrit-tool-name))
             (tool-input (get-text-property start 'efrit-tool-input))
             (tool-result (get-text-property start 'efrit-tool-result))
             (tool-success (get-text-property start 'efrit-tool-success))
             (tool-elapsed (get-text-property start 'efrit-tool-elapsed))
             (tool-running (get-text-property start 'efrit-tool-running))
             (inhibit-read-only t))
        (if expanded
            ;; Collapse: remove expansion, update indicator
            (efrit-agent--collapse-tool tool-id start end tool-name tool-result
                                        tool-success tool-elapsed tool-running tool-input)
          ;; Expand: add expansion content, update indicator
          (efrit-agent--expand-tool tool-id start end tool-name tool-result
                                    tool-success tool-elapsed tool-running tool-input))
        t))))

(defun efrit-agent--collapse-tool (tool-id start end tool-name result success-p elapsed running input)
  "Collapse tool TOOL-ID, removing expansion content."
  (save-excursion
    (goto-char start)
    (delete-region start end)
    ;; Re-render as collapsed
    (let* ((expand-char (efrit-agent--char 'expand-collapsed))
           (status-char (cond (running (efrit-agent--char 'tool-running))
                              (success-p (efrit-agent--char 'tool-success))
                              (t (efrit-agent--char 'tool-failure))))
           (status-face (if (and (not running) (not success-p)) 'efrit-agent-error nil))
           (render-type (get-text-property start 'efrit-tool-render-type))
           (result-summary (when result
                             (truncate-string-to-width
                              (replace-regexp-in-string "[\n\r]+" " " (format "%s" result))
                              (pcase efrit-agent-verbosity
                                ('minimal 20) ('normal 40) ('verbose 80)))))
           (new-text
            (concat
             "  "
             (propertize (format "%s " expand-char) 'face 'efrit-agent-timestamp)
             (propertize (format "%s " status-char)
                         'face (or status-face 'efrit-agent-timestamp))
             (propertize (or tool-name "tool") 'face 'efrit-agent-tool-name)
             (when elapsed
               (propertize (format " (%.2fs)" elapsed) 'face 'efrit-agent-timestamp))
             (if result
                 (concat " -> " (propertize result-summary 'face status-face))
               (propertize "..." 'face 'efrit-agent-timestamp))
             "\n")))
      (insert new-text)
      (add-text-properties start (point)
                           (list 'efrit-type 'tool-call
                                 'efrit-id tool-id
                                 'efrit-tool-name tool-name
                                 'efrit-tool-input input
                                 'efrit-tool-result result
                                 'efrit-tool-success success-p
                                 'efrit-tool-elapsed elapsed
                                 'efrit-tool-running running
                                 'efrit-tool-render-type render-type
                                 'efrit-tool-expanded nil
                                 'read-only t)))))

(defun efrit-agent--expand-tool (tool-id start end tool-name result success-p elapsed running input)
  "Expand tool TOOL-ID, showing full input and result."
  (save-excursion
    (goto-char start)
    (delete-region start end)
    ;; Re-render as expanded with expansion content
    (let* ((expand-char (efrit-agent--char 'expand-expanded))
           (status-char (cond (running (efrit-agent--char 'tool-running))
                              (success-p (efrit-agent--char 'tool-success))
                              (t (efrit-agent--char 'tool-failure))))
           (status-face (if (and (not running) (not success-p)) 'efrit-agent-error nil))
           (render-type (get-text-property start 'efrit-tool-render-type))
           (result-summary (when result
                             (truncate-string-to-width
                              (replace-regexp-in-string "[\n\r]+" " " (format "%s" result))
                              (pcase efrit-agent-verbosity
                                ('minimal 20) ('normal 40) ('verbose 80)))))
           ;; Header line
           (header-text
            (concat
             "  "
             (propertize (format "%s " expand-char) 'face 'efrit-agent-timestamp)
             (propertize (format "%s " status-char)
                         'face (or status-face 'efrit-agent-timestamp))
             (propertize (or tool-name "tool") 'face 'efrit-agent-tool-name)
             (when elapsed
               (propertize (format " (%.2fs)" elapsed) 'face 'efrit-agent-timestamp))
             (if result
                 (concat " -> " (propertize result-summary 'face status-face))
               (propertize "..." 'face 'efrit-agent-timestamp))
             "\n"))
           ;; Expansion content (pass tool-id and render-type)
           (expansion-text (efrit-agent--format-tool-expansion input result success-p tool-id render-type))
           (new-text (concat header-text expansion-text)))
      (insert new-text)
      (add-text-properties start (point)
                           (list 'efrit-type 'tool-call
                                 'efrit-id tool-id
                                 'efrit-tool-name tool-name
                                 'efrit-tool-input input
                                 'efrit-tool-result result
                                 'efrit-tool-success success-p
                                 'efrit-tool-elapsed elapsed
                                 'efrit-tool-running running
                                 'efrit-tool-render-type render-type
                                 'efrit-tool-expanded t
                                 'read-only t)))))

;;; Display Hints for Tool Results
;;
;; Functions for applying display hints to control how tool results are rendered
;; in the agent buffer.

(defun efrit-agent--apply-display-hint (tool-use-id summary &optional render-type auto-expand importance annotations)
  "Apply display hint to control how a tool result is rendered.
TOOL-USE-ID is the ID of the tool call to modify.
SUMMARY is the text to show when collapsed.
RENDER-TYPE (optional) is one of: text, diff, elisp, json, shell, grep, markdown.
AUTO-EXPAND (optional) controls default expansion state.
IMPORTANCE (optional) is one of: normal, success, warning, error."
  (let ((region (efrit-agent--find-tool-region tool-use-id)))
    (unless region
      (error "Tool call not found: %s" tool-use-id))
    
    (let* ((start (car region))
           (inhibit-read-only t)
           ;; Get existing properties
           (tool-name (get-text-property start 'efrit-tool-name))
           (tool-input (get-text-property start 'efrit-tool-input))
           (tool-result (get-text-property start 'efrit-tool-result))
           (tool-success (get-text-property start 'efrit-tool-success))
           (tool-elapsed (get-text-property start 'efrit-tool-elapsed))
           (tool-running (get-text-property start 'efrit-tool-running))
           ;; Determine auto-expand default if not specified
           (should-expand (if (not (null auto-expand))
                             auto-expand
                           ;; Default: expand errors, collapse normal results
                           (or tool-running (eq importance 'error))))
           ;; Determine status face based on importance
           (status-face (pcase importance
                         ('error 'efrit-agent-importance-error)
                         ('warning 'efrit-agent-importance-warning)
                         ('success 'efrit-agent-importance-success)
                         (_ 'efrit-agent-importance-normal)))
           ;; Format indicators
           (expand-char (efrit-agent--char (if should-expand 'expand-expanded 'expand-collapsed)))
           ;; Status char: use error icon if importance is 'error, otherwise use success/failure based on tool-success
           (status-char (cond
                         ((eq importance 'error) (efrit-agent--char 'error-icon))
                         (tool-success (efrit-agent--char 'tool-success))
                         (t (efrit-agent--char 'tool-failure))))
           ;; Build the updated tool line
           (new-text
            (concat
             "  "
             (propertize (format "%s " expand-char) 'face 'efrit-agent-timestamp)
             (propertize (format "%s " status-char)
                         'face (pcase importance
                                 ('error 'efrit-agent-importance-error)
                                 ('warning 'efrit-agent-importance-warning)
                                 ('success 'efrit-agent-importance-success)
                                 (_ (if tool-success 'efrit-agent-timestamp 'efrit-agent-error))))
             (propertize (or tool-name "tool") 'face 'efrit-agent-tool-name)
             (when tool-elapsed
               (propertize (format " (%.2fs)" tool-elapsed) 'face 'efrit-agent-timestamp))
             " -> "
             (propertize summary 'face status-face)
             "\n")))
      ;; Replace the tool display region with new text
      (save-excursion
        (goto-char start)
        (delete-region start (cdr region))
        (insert new-text)
        ;; Update properties with display hints
        (add-text-properties start (point)
                             (list 'efrit-type 'tool-call
                                   'efrit-id tool-use-id
                                   'efrit-tool-name tool-name
                                   'efrit-tool-input tool-input
                                   'efrit-tool-result tool-result
                                   'efrit-tool-success tool-success
                                   'efrit-tool-elapsed tool-elapsed
                                   'efrit-tool-running tool-running
                                   'efrit-tool-expanded should-expand
                                   'efrit-tool-summary summary
                                   'efrit-tool-render-type (or render-type "text")
                                   'efrit-tool-importance (or importance "normal")
                                   'efrit-tool-annotations annotations
                                   'read-only t)))
      ;; Return confirmation message
      (format "Display hint applied to %s: %s" tool-use-id summary))))

(provide 'efrit-agent-tools)

;;; efrit-agent-tools.el ends here
