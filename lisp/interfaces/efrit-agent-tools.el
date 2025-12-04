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

;;; Tool View Struct
;;
;; Centralized representation of a tool call for rendering.
;; All tool display functions build one of these and call the unified renderer.

(cl-defstruct (efrit-agent-tool-view (:constructor efrit-agent-tool-view-create))
  "Struct representing a tool call for rendering in the agent buffer."
  id                  ; String: unique tool-call ID (e.g., "tool-3")
  name                ; String: tool name (e.g., "Read")
  input               ; Any: input parameters (plist, alist, or string)
  result              ; Any: tool result (usually string)
  success-p           ; Bool: did the tool succeed?
  elapsed             ; Number or nil: seconds elapsed
  running             ; Bool: is the tool still running?
  render-type         ; Symbol: text, diff, elisp, json, shell, grep, markdown, error
  summary             ; String or nil: explicit summary for collapsed view
  importance          ; Symbol: normal, success, warning, error
  annotations         ; Plist or nil: structured metadata for smart summaries
  expanded-p)         ; Bool: is this tool expanded?

;;; Centralized Tool Rendering

(defun efrit-agent--tool-view-summary (tv)
  "Get the summary text for tool view TV.
Follows precedence: explicit summary > annotations > truncated result."
  (or (efrit-agent-tool-view-summary tv)
      (efrit-agent--summary-from-annotations tv)
      (efrit-agent--default-summary tv)))

(defun efrit-agent--default-summary (tv)
  "Generate default summary from TV's result by truncating."
  (let ((result (efrit-agent-tool-view-result tv)))
    (when result
      (truncate-string-to-width
       (replace-regexp-in-string "[\n\r]+" " " (format "%s" result))
       (pcase efrit-agent-verbosity
         ('minimal 20)
         ('normal 40)
         ('verbose 80))))))

(defun efrit-agent--summary-from-annotations (tv)
  "Generate summary from TV's structured annotations.
Returns nil if no annotations or unknown kind."
  (when-let* ((ann (efrit-agent-tool-view-annotations tv)))
    (pcase (plist-get ann :kind)
      ('diff-summary
       (format "%d files changed (+%d/-%d)%s"
               (or (plist-get ann :files-changed) 0)
               (or (plist-get ann :insertions) 0)
               (or (plist-get ann :deletions) 0)
               (let ((files (plist-get ann :files)))
                 (if (and files (listp files) (> (length files) 0))
                     (format " [%s]"
                             (mapconcat #'identity (seq-take files 3) ", "))
                   ""))))
      ('grep-summary
       (format "%d matches in %d files for %S"
               (or (plist-get ann :matches) 0)
               (or (plist-get ann :files) 0)
               (plist-get ann :pattern)))
      ('test-summary
       (format "%s: %d failed, %d passed, %d skipped (%.2fs)"
               (or (plist-get ann :framework) "tests")
               (or (plist-get ann :failed) 0)
               (or (plist-get ann :passed) 0)
               (or (plist-get ann :skipped) 0)
               (or (plist-get ann :duration-seconds) 0.0)))
      ('shell-summary
       (format "%s (exit %d, %d lines)"
               (or (plist-get ann :command) "shell")
               (or (plist-get ann :exit-code) 0)
               (or (plist-get ann :lines) 0)))
      ('fs-read-summary
       (format "Read %s (%d lines)"
               (or (plist-get ann :path) "file")
               (or (plist-get ann :line-count) 0)))
      ('fs-write-summary
       (format "Wrote %s (%d bytes)"
               (or (plist-get ann :path) "file")
               (or (plist-get ann :bytes-written) 0)))
      (_ nil))))

(defun efrit-agent--render-tool-call (tv)
  "Render tool call described by TV (efrit-agent-tool-view) at point.
Inserts both header line and (if expanded) body content.
Returns the end position of the inserted content."
  (let* ((id (efrit-agent-tool-view-id tv))
         (name (efrit-agent-tool-view-name tv))
         (input (efrit-agent-tool-view-input tv))
         (result (efrit-agent-tool-view-result tv))
         (success-p (efrit-agent-tool-view-success-p tv))
         (elapsed (efrit-agent-tool-view-elapsed tv))
         (running (efrit-agent-tool-view-running tv))
         (render-type (efrit-agent-tool-view-render-type tv))
         (importance (efrit-agent-tool-view-importance tv))
         (annotations (efrit-agent-tool-view-annotations tv))
         (expanded-p (efrit-agent-tool-view-expanded-p tv))
         ;; Compute display elements
         (expand-char (efrit-agent--char
                       (if expanded-p 'expand-expanded 'expand-collapsed)))
         (status-char (cond
                       (running (efrit-agent--char 'tool-running))
                       ((eq importance 'error) (efrit-agent--char 'tool-failure))
                       (success-p (efrit-agent--char 'tool-success))
                       (t (efrit-agent--char 'tool-failure))))
         (status-face (pcase importance
                        ('error 'efrit-agent-importance-error)
                        ('warning 'efrit-agent-importance-warning)
                        ('success 'efrit-agent-importance-success)
                        (_ (if (and (not running) (not success-p))
                               'efrit-agent-error
                             nil))))
         (summary (efrit-agent--tool-view-summary tv))
         (start (point)))
    ;; Insert header line
    (insert "  ")
    (insert (propertize (format "%s " expand-char) 'face 'efrit-agent-timestamp))
    (insert (propertize (format "%s " status-char)
                        'face (or status-face 'efrit-agent-timestamp)))
    (insert (propertize (or name "tool") 'face 'efrit-agent-tool-name))
    (when elapsed
      (insert (propertize (format " (%.2fs)" elapsed) 'face 'efrit-agent-timestamp)))
    (cond
     (running
      (insert (propertize "..." 'face 'efrit-agent-timestamp)))
     (result
      (insert " -> ")
      (insert (propertize (or summary "") 'face status-face))))
    (insert "\n")
    ;; Insert expanded body if expanded
    (when (and expanded-p (or input result))
      (insert (efrit-agent--format-tool-expansion input result success-p id render-type annotations)))
    ;; Apply text properties to the entire region
    (add-text-properties start (point)
                         (list 'efrit-type 'tool-call
                               'efrit-id id
                               'efrit-tool-name name
                               'efrit-tool-input input
                               'efrit-tool-result result
                               'efrit-tool-success success-p
                               'efrit-tool-elapsed elapsed
                               'efrit-tool-running running
                               'efrit-tool-render-type render-type
                               'efrit-tool-summary (efrit-agent-tool-view-summary tv)
                               'efrit-tool-importance importance
                               'efrit-tool-annotations annotations
                               'efrit-tool-expanded expanded-p
                               'read-only t))
    (point)))

(defun efrit-agent--tool-view-from-properties (start)
  "Create a tool-view from text properties at position START.
Returns an efrit-agent-tool-view struct."
  (efrit-agent-tool-view-create
   :id (get-text-property start 'efrit-id)
   :name (get-text-property start 'efrit-tool-name)
   :input (get-text-property start 'efrit-tool-input)
   :result (get-text-property start 'efrit-tool-result)
   :success-p (get-text-property start 'efrit-tool-success)
   :elapsed (get-text-property start 'efrit-tool-elapsed)
   :running (get-text-property start 'efrit-tool-running)
   :render-type (get-text-property start 'efrit-tool-render-type)
   :summary (get-text-property start 'efrit-tool-summary)
   :importance (get-text-property start 'efrit-tool-importance)
   :annotations (get-text-property start 'efrit-tool-annotations)
   :expanded-p (get-text-property start 'efrit-tool-expanded)))

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

(defun efrit-agent--compute-auto-expand (success-p result user-override)
  "Compute whether a tool result should auto-expand.
SUCCESS-P is the tool success status, RESULT is the tool output.
USER-OVERRIDE is 'not-found or a boolean from user's explicit toggle.
Returns t if the tool should be expanded by default."
  ;; If user has explicitly toggled, respect that
  (if (not (eq user-override 'not-found))
      user-override
    ;; Otherwise apply display mode logic
    (pcase efrit-agent-display-mode
      ('minimal nil)       ; Always collapsed
      ('verbose t)         ; Always expanded
      ('smart
       ;; Smart mode: expand errors, collapse successes unless short
       (if (not success-p)
           t  ; Errors auto-expand
         ;; Successes: only expand if very short
         (and (< (length (format "%s" result)) 200)
              (< (cl-count ?\n (format "%s" result)) 5))))
      (_ nil))))  ; Fallback to collapsed

(defun efrit-agent--update-tool-result (tool-id result success-p &optional elapsed)
  "Update tool call with result and status.
TOOL-ID identifies the tool. RESULT is the result value.
SUCCESS-P indicates if the call succeeded. ELAPSED is optional time.
Uses the centralized tool-view renderer for consistent display."
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
             ;; Check for user override
             (user-override (and efrit-agent--expansion-state
                                 (gethash tool-id efrit-agent--expansion-state 'not-found)))
             ;; Compute auto-expand with new logic (errors expand, successes collapse)
             (auto-expand (efrit-agent--compute-auto-expand success-p result user-override))
             ;; Determine importance based on success
             (importance (if success-p 'normal 'error))
             ;; Build tool-view struct
             (tv (efrit-agent-tool-view-create
                  :id tool-id
                  :name tool-name
                  :input tool-input
                  :result result
                  :success-p success-p
                  :elapsed elapsed-time
                  :running nil
                  :render-type nil  ; Will be set by display_hint if needed
                  :summary nil      ; Use default summary
                  :importance importance
                  :annotations nil
                  :expanded-p auto-expand)))
        ;; Replace the tool call region using centralized renderer
        (save-excursion
          (goto-char start)
          (delete-region start end)
          (efrit-agent--render-tool-call tv))))))

;;; Tool Call Expansion (collapsed/expanded toggle)

(defun efrit-agent--format-tool-expansion (tool-input result success-p &optional tool-id render-type annotations)
  "Format the expansion content for a tool call.
TOOL-INPUT is the input parameters, RESULT is the output.
SUCCESS-P indicates status. TOOL-ID enables error recovery buttons.
RENDER-TYPE (optional) hints how to format result (text, diff, elisp, json, shell, grep, markdown, error).
ANNOTATIONS (optional) is a list of (line . note) pairs for line-level notes.
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
     ;; Annotations section (line-level notes from display_hint)
     (when (and annotations (listp annotations) (> (length annotations) 0))
       (efrit-agent--format-annotations annotations indent))
     ;; Error recovery buttons (only when tool failed and tool-id is known)
     (when (and tool-id (not success-p))
       (efrit-agent--format-error-recovery-buttons tool-id result tool-input))
     ;; Separator
     indent (propertize (make-string 50 (efrit-agent--char 'box-horizontal))
                        'face 'efrit-agent-timestamp) "\n")))

(defun efrit-agent--format-annotations (annotations indent)
  "Format ANNOTATIONS list as a section with INDENT prefix.
ANNOTATIONS is a list of hash-tables or alists with line and note keys."
  (let ((result (concat indent
                        (propertize "Notes: " 'face 'efrit-agent-section-header)
                        "\n")))
    (dolist (ann annotations)
      (let ((line (if (hash-table-p ann)
                      (gethash "line" ann)
                    (cdr (assoc 'line ann))))
            (note (if (hash-table-p ann)
                      (gethash "note" ann)
                    (cdr (assoc 'note ann)))))
        (when (and line note)
          (setq result
                (concat result indent "  "
                        (propertize (format "L%d: " line)
                                    'face 'efrit-agent-tool-name)
                        (propertize note 'face 'efrit-agent-session-id)
                        "\n")))))
    result))

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
@@ hunk headers, or lines starting with + or - followed by content.
Uses multiline matching so patterns can appear anywhere in the text."
  (and (stringp text)
       (or
        ;; Check for unified diff file headers (anywhere in text)
        (string-match-p "^---\\s-+[ab]/\\|^---\\s-+\\S-+" text)
        ;; Check for hunk headers (anywhere in text)
        (string-match-p "^@@\\s-+-?[0-9]" text)
        ;; Check for diff --git header
        (string-match-p "^diff --git" text))))

(defun efrit-agent--extract-diff-from-result (result)
  "Extract diff content from RESULT if present.
RESULT may be a string containing diff directly, or an alist with a diff field.
Tool results are typically wrapped: ((success . t) (result . ((diff . \"...\") ...)))
Returns the diff string or nil if no diff content found."
  (cond
   ;; Result is a string that looks like diff
   ((and (stringp result) (efrit-agent--diff-content-p result))
    result)
   ;; Result is wrapped tool response: ((success . t) (result . ((diff . ...) ...)))
   ((and (listp result)
         (assoc 'result result)
         (listp (cdr (assoc 'result result)))
         (assoc 'diff (cdr (assoc 'result result))))
    (cdr (assoc 'diff (cdr (assoc 'result result)))))
   ;; Result is an alist with 'diff key directly (from vcs_diff tool)
   ((and (listp result) (assoc 'diff result))
    (cdr (assoc 'diff result)))
   ;; Check if stringified result contains diff markers
   ((let ((str (format "%s" result)))
      (when (efrit-agent--diff-content-p str)
        str)))
   (t nil)))

(defun efrit-agent--extract-diff-filepath (line)
  "Extract file path from diff header LINE.
Returns the absolute or relative path, or nil if not a file header."
  (cond
   ;; diff --git a/path/to/file b/path/to/file
   ((string-match "^diff --git a/\\(.+\\) b/\\(.+\\)$" line)
    (match-string 2 line))
   ;; --- a/path/to/file
   ((string-match "^--- a/\\(.+\\)$" line)
    (match-string 1 line))
   ;; +++ b/path/to/file
   ((string-match "^\\+\\+\\+ b/\\(.+\\)$" line)
    (match-string 1 line))
   (t nil)))

(defun efrit-agent--format-diff-line (line indent)
  "Format a single diff LINE with appropriate face and INDENT prefix.
Returns the formatted line string with text properties.
File header lines are clickable to open the file."
  (let* ((line-content (concat indent "  " line "\n"))
         (filepath (efrit-agent--extract-diff-filepath line))
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
                 'efrit-agent-diff-context)))
         (result (propertize line-content 'face face)))
    ;; Make file headers clickable
    (when filepath
      (let ((map (make-sparse-keymap))
            (action (lambda ()
                      (interactive)
                      (let ((full-path (if (file-name-absolute-p filepath)
                                           filepath
                                         (expand-file-name filepath default-directory))))
                        (if (file-exists-p full-path)
                            (find-file-other-window full-path)
                          (message "File not found: %s" full-path))))))
        (define-key map [mouse-1] action)
        (define-key map (kbd "RET") action)
        (setq result (propertize result
                                 'keymap map
                                 'mouse-face 'highlight
                                 'help-echo (format "Click to open %s" filepath)))))
    result))

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
Records the user's preference to override display-mode and Claude's hints.
Uses the centralized tool-view renderer for consistent display.
Returns t if toggled, nil if no tool at point."
  (let* ((tool-id (get-text-property (point) 'efrit-id))
         (tool-type (get-text-property (point) 'efrit-type)))
    (when (and tool-id (eq tool-type 'tool-call))
      (let* ((region (efrit-agent--find-tool-region tool-id))
             (start (car region))
             (end (cdr region))
             (inhibit-read-only t)
             ;; Build tool-view from existing properties
             (tv (efrit-agent--tool-view-from-properties start))
             ;; Toggle expansion state
             (new-state (not (efrit-agent-tool-view-expanded-p tv))))
        ;; Record user's explicit preference (overrides display-mode and hints)
        (when efrit-agent--expansion-state
          (puthash tool-id new-state efrit-agent--expansion-state))
        ;; Update the tool-view with new expansion state
        (setf (efrit-agent-tool-view-expanded-p tv) new-state)
        ;; Re-render using centralized renderer
        (save-excursion
          (goto-char start)
          (delete-region start end)
          (efrit-agent--render-tool-call tv))
        t))))

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
IMPORTANCE (optional) is one of: normal, success, warning, error.
The actual expansion state is determined by `efrit-agent-display-mode':
  minimal: Always collapsed (ignores auto-expand)
  smart: Respects auto-expand hint
  verbose: Always expanded (ignores auto-expand)
Uses the centralized tool-view renderer for consistent display."
  (let ((region (efrit-agent--find-tool-region tool-use-id)))
    (unless region
      (error "Tool call not found: %s" tool-use-id))
    
    (let* ((start (car region))
           (end (cdr region))
           (inhibit-read-only t)
           ;; Build tool-view from existing properties
           (tv (efrit-agent--tool-view-from-properties start))
           ;; Check for user override first
           (user-override (and efrit-agent--expansion-state
                               (gethash tool-use-id efrit-agent--expansion-state 'not-found)))
           ;; Determine hint-expand from args or default
           (hint-expand (if (not (null auto-expand))
                            auto-expand
                          ;; Default: expand errors, collapse normal results
                          (or (efrit-agent-tool-view-running tv)
                              (eq importance 'error))))
           ;; Apply display mode override (only if no user override)
           (should-expand (if (not (eq user-override 'not-found))
                              user-override  ; User explicitly set this
                            (pcase efrit-agent-display-mode
                              ('minimal nil)       ; Always collapsed
                              ('smart hint-expand) ; Respect hint
                              ('verbose t)         ; Always expanded
                              (_ hint-expand)))))  ; Fallback to hint
      ;; Update tool-view with display hint values
      (setf (efrit-agent-tool-view-summary tv) summary)
      (setf (efrit-agent-tool-view-render-type tv) (or render-type 'text))
      (setf (efrit-agent-tool-view-importance tv) (or importance 'normal))
      (setf (efrit-agent-tool-view-annotations tv) annotations)
      (setf (efrit-agent-tool-view-expanded-p tv) should-expand)
      ;; Re-render using centralized renderer
      (save-excursion
        (goto-char start)
        (delete-region start end)
        (efrit-agent--render-tool-call tv))
      ;; Return confirmation message
      (format "Display hint applied to %s: %s" tool-use-id summary))))

(provide 'efrit-agent-tools)

;;; efrit-agent-tools.el ends here
