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
(require 'efrit-sandbox)   ; efrit-sandbox-denied-prefix
(require 'efrit-review)    ; efrit-review-rejected-prefix

;; Forward declarations

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

(defun efrit-agent--rejected-result-p (result)
  "Non-nil if RESULT is a reviewer rejection (efrit-review), not a tool failure."
  (and (stringp result)
       (string-prefix-p efrit-review-rejected-prefix result)))

(defun efrit-agent--denied-result-p (result)
  "Non-nil if RESULT is a denial or rejection, not a tool failure.
Covers sandbox and permission denials by the user and reviewer
rejections: all are rows the tool never ran, rendered muted."
  (and (stringp result)
       (or (string-prefix-p efrit-sandbox-denied-prefix result)
           (string-prefix-p "Error permission denied" result)
           (efrit-agent--rejected-result-p result))))

(defun efrit-agent--error-gist (result)
  "The first sentence of an error RESULT, without the leading `Error' noise."
  (let* ((s (replace-regexp-in-string "[\n\r]+" " " (format "%s" result)))
         (s (replace-regexp-in-string "\\`Error:? *\\(sandbox denied: \\|permission denied: \\|review rejected: \\)?" "" s))
         (end (and (string-match "[.!?]\\( \\|\\'\\)" s) (match-beginning 0))))
    (string-trim (if end (substring s 0 end) s))))

(defun efrit-agent--smart-result-summary (tool-name result success-p)
  "Generate a smart summary for TOOL-NAME's RESULT.
SUCCESS-P indicates if the tool succeeded."
  (let ((result-str (format "%s" result)))
    (cond
     ;; A reviewer rejection: the reason is the summary
     ((efrit-agent--rejected-result-p result)
      (concat "rejected · " (efrit-agent--error-gist result-str)))
     ;; A denial: say so, plus what was refused unless the row's
     ;; target (the command) already says it
     ((efrit-agent--denied-result-p result)
      (if (string-match-p "bash\\|shell" tool-name)
          "denied"
        (concat "denied · "
                (replace-regexp-in-string " *([^)]*)\\'" ""
                                          (efrit-agent--error-gist result-str)))))
     ;; Any other failure: the error's first sentence beats a bare
     ;; "Failed" or a bogus line count
     ((not success-p) (efrit-agent--error-gist result-str))
     ;; session_complete - its message IS Claude's final answer; show it
     ;; in full, never truncated (ef-gi82)
     ((string-match-p "session_complete" tool-name)
      (replace-regexp-in-string
       "[\n\r]+" " "
       (if (string-match "\\[SESSION-COMPLETE: \\(\\(?:.\\|\n\\)*\\)\\]" result-str)
           (match-string 1 result-str)
         result-str)))
     ;; A report buffer: name it and say how to open it
     ((and success-p (member tool-name '("buffer_create" "create_buffer")))
      (if (string-match "Created buffer '\\([^']+\\)' with \\([0-9]+\\) characters" result-str)
          (format "%s (%s chars) · o opens" (match-string 1 result-str) (match-string 2 result-str))
        "report buffer · o opens"))
     ;; Read tool - show size
     ((string-match-p "Read\\|read" tool-name)
      (let ((lines (length (split-string result-str "\n"))))
        (if (= lines 1)
            (format "%d chars" (length result-str))
          (format "%d lines" lines))))
     ;; Edit/Create tool - show change summary
     ((string-match-p "edit_file\\|create_file\\|Edit\\|Create" tool-name)
      (cond
       ((string-match-p "Created\\|created" result-str) "Created")
       ((string-match "\\([0-9]+\\) lines?" result-str)
        (format "%s lines changed" (match-string 1 result-str)))
       (success-p "Done")
       (t "Failed")))
     ;; Grep - show match count
     ((string-match-p "Grep\\|grep\\|search" tool-name)
      (let ((matches (length (split-string result-str "\n" t))))
        (if (> matches 0)
            (format "%d matches" matches)
          "No matches")))
     ;; Bash - show exit status or output preview
     ((string-match-p "Bash\\|bash\\|shell" tool-name)
      (if success-p
          (let ((lines (split-string result-str "\n" t)))
            (if (> (length lines) 1)
                (format "%d lines output" (length lines))
              ;; one line: show it; the row chops it to the window
              (car lines)))
        "Failed"))
     ;; Fallback
     (t nil))))

(defun efrit-agent--default-summary (tv)
  "Generate default summary from TV's result by truncating."
  (let ((result (efrit-agent-tool-view-result tv))
        (tool-name (efrit-agent-tool-view-name tv))
        (success-p (efrit-agent-tool-view-success-p tv)))
    (when result
      (or (efrit-agent--smart-result-summary tool-name result success-p)
          ;; The row chops to the window width; these caps only bound
          ;; how much of the result is offered as a summary at all
          (truncate-string-to-width
           (replace-regexp-in-string "[\n\r]+" " " (format "%s" result))
           (pcase efrit-agent-verbosity
             ('minimal 40)
             ('normal 200)
             ('verbose 400)))))))

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

(defun efrit-agent--session-complete-message (result)
  "The user-facing message inside a session_complete RESULT string, or nil."
  (let ((s (format "%s" (or result ""))))
    (when (string-match "\\[SESSION-COMPLETE: \\(\\(?:.\\|\n\\)*\\)\\]" s)
      (string-trim (match-string 1 s)))))

(defun efrit-agent--turn-has-streamed-text-p ()
  "Non-nil if the assistant streamed prose in this turn, before point.
Walks back over tool rows and blank lines to the start of the turn (the
user's message); any `claude-message' on the way means the answer is
already on screen."
  (save-excursion
    (let ((found nil) (done nil))
      (while (and (not found) (not done) (> (point) (point-min)))
        (let ((type (get-text-property (1- (point)) 'efrit-type)))
          (cond
           ((eq type 'claude-message) (setq found t))
           ((eq type 'user-message) (setq done t))
           (t (goto-char (or (previous-single-property-change (point) 'efrit-type)
                             (point-min)))))))
      found)))

(defun efrit-agent--render-tool-call (tv)
  "Render tool call described by TV (efrit-agent-tool-view) at point.
Inserts both header line and (if expanded) body content.
Returns the end position of the inserted content.

session_complete is not shown as a tool at all: its message is the
assistant's final answer and is rendered as prose, with a thin rule
marking the end of the turn."
  (if (and (equal (efrit-agent-tool-view-name tv) "session_complete")
           (not (efrit-agent-tool-view-running tv)))
      (let* ((start (point))
             (msg (or (efrit-agent--session-complete-message
                       (efrit-agent-tool-view-result tv))
                      (efrit-agent--format-tool-input (efrit-agent-tool-view-input tv)))))
        ;; In the REPL, session_complete is the end-of-turn signal; the
        ;; answer is the text the model streamed before it.  The prompt
        ;; makes the model repeat the answer in the message (efrit-do,
        ;; with no transcript, shows nothing else), so rendering both
        ;; gave the same paragraph twice.  Show the message only when
        ;; no text was streamed this turn and it IS the answer.
        (when (and msg (not (string-empty-p msg))
                   (not (efrit-agent--turn-has-streamed-text-p)))
          (insert (propertize msg 'face 'efrit-agent-claude-message) "\n"))
        (insert (propertize (concat (make-string 3 ?·) "\n") 'face 'efrit-agent-timestamp))
        (add-text-properties start (point)
                             (list 'efrit-type 'tool-call
                                   'efrit-id (efrit-agent-tool-view-id tv)
                                   'efrit-tool-name "session_complete"
                                   'efrit-tool-result (efrit-agent-tool-view-result tv)
                                   'efrit-tool-success t))
        (point))
    (efrit-agent--render-tool-call-1 tv)))

(defun efrit-agent--render-tool-call-1 (tv)
  "Render a regular tool call TV at point (see `efrit-agent--render-tool-call')."
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
         ;; Extract target for display
         (target (efrit-agent--extract-tool-target name input))
         ;; Compute display elements
         (expand-char (efrit-agent--char
                       (if expanded-p 'expand-expanded 'expand-collapsed)))
         (denied (efrit-agent--denied-result-p result))
         (status-char (cond
                       (running (efrit-agent--char 'tool-running))
                       (denied (efrit-agent--char 'tool-denied))
                       ((eq importance 'error) (efrit-agent--char 'tool-failure))
                       (success-p (efrit-agent--char 'tool-success))
                       (t (efrit-agent--char 'tool-failure))))
         (status-face (cond
                       (denied 'efrit-agent-timestamp)
                       (t (pcase importance
                            ('error 'efrit-agent-importance-error)
                            ('warning 'efrit-agent-importance-warning)
                            ('success 'efrit-agent-importance-success)
                            (_ (if (and (not running) (not success-p))
                                   'efrit-agent-error
                                 nil))))))
         (summary (efrit-agent--tool-view-summary tv))
         (start (point)))
    ;; Insert header line
    (insert "  ")
    (insert (propertize (format "%s " expand-char) 'face 'efrit-agent-timestamp))
    (insert (propertize (format "%s " status-char)
                        'face (or status-face
                                  (if running 'efrit-agent-status-working 'efrit-agent-timestamp))))
    (insert (propertize (or name "tool") 'face 'efrit-agent-tool-name))
    ;; Show target (file path, pattern, etc.)
    (when target
      (insert (propertize (format ": %s" target) 'face 'efrit-agent-session-id)))
    ;; One line: the summary follows the name on the same row; the
    ;; status glyph at the start already says success/failure
    (cond
     (running
      (insert (propertize (format "  %s" (efrit-agent--tool-progress-text name input))
                          'face 'efrit-agent-timestamp)))
     (result
      (let ((sum (string-trim (or summary "done"))))
        ;; Elapsed time goes before the summary so the summary is the
        ;; only variable-width part and can be chopped flush to the
        ;; right edge.
        (when (and elapsed (>= elapsed 0.05))
          (insert (propertize (format "  %.1fs" elapsed) 'face 'efrit-agent-timestamp)))
        (unless (string-empty-p sum)
          (insert (propertize " · " 'face 'efrit-agent-timestamp))
          (insert (efrit-agent--chop-to-row
                   (string-width (buffer-substring start (point)))
                   sum
                   (cond (success-p 'efrit-agent-session-id)
                         (denied 'efrit-agent-timestamp)
                         (t 'efrit-agent-error))))))))
    (insert "\n")
    ;; Insert expanded body if expanded
    (when (and expanded-p (or input result))
      (insert (efrit-agent--format-tool-expansion input result success-p id render-type annotations name)))
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

(defun efrit-agent--input-field (input &rest keys)
  "First non-nil value among KEYS in INPUT (hash table, alist or plist)."
  (catch 'found
    (dolist (k keys)
      (let ((v (cond
                ((hash-table-p input) (gethash k input))
                ((and (listp input) (keywordp (car-safe input)))
                 (plist-get input (intern (concat ":" k))))
                ((listp input)
                 (or (cdr (assoc k input))
                     (cdr (assoc (intern k) input))
                     (cdr (assoc (intern (concat ":" k)) input)))))))
        (when v (throw 'found v))))
    nil))

(defun efrit-agent--extract-tool-target (tool-name input)
  "Extract the primary target from INPUT for TOOL-NAME.
Returns a short string describing what the tool is operating on."
  (when input
    (let ((path (efrit-agent--input-field input "path" "file_path" "file"))
          (pattern (and (string-match-p "grep\\|search" tool-name)
                        (efrit-agent--input-field input "pattern" "query")))
          (cmd (and (string-match-p "bash\\|shell" tool-name)
                    (efrit-agent--input-field input "cmd" "command")))
          (calls (and (equal tool-name "review") (efrit-agent--input-field input "calls"))))
      (cond
       ;; the review row: how many calls, judged by which model
       (calls
        (format "%s tool call%s%s" calls (if (eql calls 1) "" "s")
                (if-let* ((m (efrit-agent--input-field input "model")))
                    (format " · %s" m) "")))
       ((stringp path)
        ;; the last path component; a bare "/" or "~" has none, so
        ;; show the path itself rather than an empty target
        (let* ((name (file-name-nondirectory (directory-file-name path)))
               (name (if (string-empty-p name) (abbreviate-file-name path) name)))
          (if (> (length name) 30) (concat "..." (substring name -27)) name)))
       ((stringp pattern)
        (truncate-string-to-width (format "\"%s\"" pattern) 30 nil nil "…"))
       ((stringp cmd)
        (truncate-string-to-width (replace-regexp-in-string "[\n\r]+" " " cmd) 40 nil nil "…"))
       (t nil)))))

(defun efrit-agent--tool-progress-text (tool-name _input)
  "Generate progress text for TOOL-NAME with _INPUT (reserved)."
  (pcase tool-name
    ((pred (lambda (n) (string-match-p "Read\\|read" n))) "Reading...")
    ((pred (lambda (n) (string-match-p "edit_file\\|Edit" n))) "Editing...")
    ((pred (lambda (n) (string-match-p "create_file\\|Create" n))) "Creating...")
    ((pred (lambda (n) (string-match-p "Grep\\|grep\\|search" n))) "Searching...")
    ((pred (lambda (n) (string-match-p "Bash\\|bash\\|shell" n))) "Running...")
    ((pred (lambda (n) (string-match-p "glob\\|find" n))) "Finding...")
    (_ "...")))

(defun efrit-agent--add-tool-call (tool-name &optional input)
  "Add a tool call indicator for TOOL-NAME with optional INPUT.
Tool calls appear inline in the conversation.
Returns the tool ID for later update with result."
  ;; End any streaming Claude message first
  (efrit-agent--stream-end-message)
  ;; Hide thinking indicator when tool starts
  (efrit-agent--hide-thinking)
  (let* ((tool-id (format "tool-%d" (cl-incf efrit-agent--message-counter)))
         (silent (equal tool-name "session_complete"))
         (target (efrit-agent--extract-tool-target tool-name input))
         (progress (efrit-agent--tool-progress-text tool-name input))
         (formatted-text
          (if silent
              ;; session_complete: the result renders as the answer; a
              ;; zero-width placeholder keeps the id findable for update
              (propertize " " 'invisible t)
            (concat
             "  "
             (propertize (format "%s " (efrit-agent--char 'tool-running))
                         'face 'efrit-agent-status-working)
             (propertize tool-name 'face 'efrit-agent-tool-name)
             (when target
               (propertize (format ": %s" target) 'face 'efrit-agent-session-id))
             (propertize (format "  %s" progress) 'face 'efrit-agent-timestamp)
             "\n"))))
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
       ;; Smart mode: failures expand (the message matters), successes
       ;; and denials stay collapsed -- the row already summarises them
       (and (not success-p)
            (not (efrit-agent--denied-result-p result))))
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

(defun efrit-agent--format-tool-expansion (tool-input result success-p &optional _tool-id render-type annotations tool-name)
  "Format the expansion content for a tool call.
TOOL-INPUT is the input parameters, RESULT is the output.
SUCCESS-P indicates status.  TOOL-NAME picks the bare-input marker.
RENDER-TYPE (optional) hints how to format result (text, diff, elisp, json, shell, grep, markdown, error).
ANNOTATIONS (optional) is a list of (line . note) pairs for line-level notes.
Diffs are syntax-highlighted if `efrit-agent-show-diff' is non-nil."
  (let ((indent "       ")
        (max-lines (pcase efrit-agent-verbosity
                     ('minimal 3)
                     ('normal 10)
                     ('verbose 50))))
    (concat
     ;; Input section.  A bare command/expr is shown as-is with a
     ;; `$' (shell) or `λ' (lisp) marker; other inputs keep Input:.
     (when tool-input
       (let ((text (efrit-agent--format-tool-input tool-input))
             (marker (efrit-agent--bare-input-marker tool-name tool-input)))
         (if marker
             (efrit-agent--format-indented-lines text (concat indent marker) max-lines)
           (concat
            indent (propertize "Input: " 'face 'efrit-agent-section-header) "\n"
            (efrit-agent--format-indented-lines text indent max-lines)))))
     ;; Result section (with render-type aware formatting)
     (when result
       (cond
        ;; A reviewer rejection: the reviewer's reason, dim
        ((efrit-agent--rejected-result-p result)
         (concat indent "  "
                 (propertize (concat "Reviewer: " (efrit-agent--error-gist result))
                             'face 'efrit-agent-timestamp)
                 "\n"))
        ;; A denial: one dim line for the human.  The rest of the
        ;; result text is instructions for the model, not for here.
        ((efrit-agent--denied-result-p result)
         (concat indent "  "
                 (propertize "You declined; the model was told to continue without it."
                             'face 'efrit-agent-timestamp)
                 "\n"))
        (success-p
         (concat
          indent (propertize "Result: " 'face 'efrit-agent-section-header) "\n"
          (if render-type
              (efrit-agent--format-by-render-type (format "%s" result) render-type indent max-lines)
            (efrit-agent--format-tool-result-with-diff result success-p indent max-lines))))
        (t
         ;; Failure: the message itself, wrapped, in the error face.
         (efrit-agent--format-wrapped-paragraph
          (replace-regexp-in-string "\\`Error:? *" "" (format "%s" result))
          (concat indent "  ") 'efrit-agent-error))))
     ;; Annotations section (line-level notes from display_hint)
     (when (and annotations (listp annotations) (> (length annotations) 0))
       (efrit-agent--format-annotations annotations indent))
)))

(defconst efrit-agent--chop-marker "…"
  "Glyph placed at the right edge of a row whose text was chopped.")

(defface efrit-agent-chop-marker
  '((t :inherit shadow :slant italic))
  "Face of the right-margin marker on chopped rows."
  :group 'efrit-agent)

(defun efrit-agent--row-width ()
  "Columns available on one row of the agent buffer.
The widest window showing the buffer, else `fill-column'; minus the
right-edge marker and a column of slack so the row never wraps."
  (let* ((wins (get-buffer-window-list (current-buffer) nil t))
         (w (if wins (apply #'max (mapcar #'window-body-width wins))
              (max fill-column 80))))
    (max 40 (- w 2))))

(defun efrit-agent--chop-to-row (prefix-width text face)
  "TEXT in FACE, cut so that PREFIX-WIDTH plus TEXT fits one row.
Like `less -S': no wrapping, the surplus is chopped and a dim
`efrit-agent--chop-marker' is pinned at the right window edge with a
`display' space so it reads as a margin mark, not part of the text.
The full text is in the row's expansion; nothing is lost.
Returns a propertized string, possibly TEXT unchanged."
  (let* ((text (replace-regexp-in-string "[\n\r\t]+" " " text))
         (avail (- (efrit-agent--row-width) prefix-width)))
    (if (<= (string-width text) avail)
        (propertize text 'face face)
      (concat (propertize (truncate-string-to-width text (max 8 (1- avail))) 'face face)
              ;; a stretch space up to the marker's column, then the
              ;; marker: it sits flush right however wide the text was
              (propertize " " 'display `(space :align-to (- right ,(1+ (string-width efrit-agent--chop-marker)))))
              (propertize efrit-agent--chop-marker 'face 'efrit-agent-chop-marker
                          'help-echo "Row chopped to the window width; expand for the full text")))))

(defun efrit-agent--format-wrapped-paragraph (text indent face)
  "TEXT filled to the window width, every line prefixed with INDENT, in FACE."
  (let ((width (max 40 (- (or (and (get-buffer-window) (window-width)) fill-column) (length indent) 2))))
    (with-temp-buffer
      (insert (string-trim text))
      (let ((fill-column width)
            (fill-prefix nil))
        (fill-region (point-min) (point-max)))
      (concat (mapconcat (lambda (l) (concat indent (propertize l 'face face)))
                         (split-string (buffer-string) "\n")
                         "\n")
              "\n"))))

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

(defun efrit-agent--bare-input-marker (tool-name input)
  "Marker string for a single-field INPUT shown bare, or nil.
`$' when TOOL-NAME runs a shell, `λ' when it evaluates Lisp."
  (when (and (hash-table-p input) (= (hash-table-count input) 1))
    (let ((key (catch 'k (maphash (lambda (k _) (throw 'k (format "%s" k))) input))))
      (cond
       ((member key '("command" "cmd")) "$")
       ((and (member key '("expr" "expression" "code"))
             (stringp tool-name) (string-match-p "sexp\\|eval\\|elisp" tool-name))
        (efrit-agent--char 'lisp-marker))
       (t nil)))))

(defun efrit-agent--format-tool-input (input)
  "Return INPUT (hash table, alist, plist or string) as readable text.
Hash tables are printed key: value, one per line, instead of the
#s(hash-table ...) reader syntax; a lone `expr'/`command' value is
shown bare since that IS the call."
  (cond
   ((null input) "")
   ((stringp input) input)
   ((hash-table-p input)
    (let (pairs)
      (maphash (lambda (k v) (push (cons (format "%s" k) v) pairs)) input)
      (setq pairs (sort pairs (lambda (a b) (string< (car a) (car b)))))
      (if (and (= (length pairs) 1)
               (member (caar pairs) '("expr" "expression" "code" "command")))
          (format "%s" (cdar pairs))
        (mapconcat (lambda (p)
                     (let ((v (cdr p)))
                       (format "%s: %s" (car p)
                               (if (and (stringp v) (string-match-p "\n" v))
                                   (concat "\n" v) v))))
                   pairs "\n"))))
   (t (pp-to-string input))))

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
