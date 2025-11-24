;;; efrit-ui.el --- Unified UI, monitoring, and performance for Efrit -*- lexical-binding: t -*-

;; Copyright (C) 2025 Steven Yegge

;; Author: Steven Yegge
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module consolidates all UI, monitoring, and performance functionality:
;; - Real-time progress display (from efrit-progress)
;; - Dashboard with session/TODO management (from efrit-dashboard)
;; - Performance optimization (caching, memory management) (from efrit-performance)

;; Replaces: efrit-progress.el, efrit-performance.el, efrit-dashboard.el

;;; Code:

(require 'efrit-common)
(require 'efrit-log)
(require 'ansi-color)
(require 'json)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(declare-function efrit-do-todo-item-status "efrit-do")
(declare-function efrit-do-todo-item-content "efrit-do")
(declare-function efrit-context-state-buffer-name "efrit-context")
(declare-function efrit-context-state-buffer-mode "efrit-context")
(declare-function efrit-context-state-directory "efrit-context")

(defvar efrit-data-directory (expand-file-name "~/.emacs.d/.efrit/")
  "Directory for efrit data storage.")

;;; ========================================================================
;;; SECTION 1: PROGRESS DISPLAY
;;; Real-time operation tracking and loop detection
;;; ========================================================================

(defgroup efrit-progress nil
  "Progress display for Efrit."
  :group 'efrit)

(defcustom efrit-progress-buffer-name "*Efrit Progress*"
  "Name of the buffer for displaying progress."
  :type 'string
  :group 'efrit-progress)

(defcustom efrit-progress-auto-show t
  "Whether to automatically show progress buffer when operations start."
  :type 'boolean
  :group 'efrit-progress)

(defcustom efrit-progress-verbosity 'normal
  "Verbosity level for progress display.
- minimal: Only show major operations
- normal: Show operations and key results
- verbose: Show all details including raw responses"
  :type '(choice (const :tag "Minimal" minimal)
                 (const :tag "Normal" normal)
                 (const :tag "Verbose" verbose))
  :group 'efrit-progress)

(defcustom efrit-progress-timestamp-format "%H:%M:%S"
  "Format for timestamps in progress display."
  :type 'string
  :group 'efrit-progress)

(defcustom efrit-progress-auto-shrink t
  "When non-nil, progress buffer automatically shrinks to fit content."
  :type 'boolean
  :group 'efrit-progress)

;;; Progress Faces

(defface efrit-progress-timestamp
  '((t :foreground "gray60"))
  "Face for timestamps in progress display."
  :group 'efrit-progress)

(defface efrit-progress-section-header
  '((t :weight bold :foreground "DeepSkyBlue"))
  "Face for section headers in progress display."
  :group 'efrit-progress)

(defface efrit-progress-tool-name
  '((t :weight bold :foreground "DarkOrange"))
  "Face for tool names in progress display."
  :group 'efrit-progress)

(defface efrit-progress-success
  '((t :foreground "green3"))
  "Face for success messages in progress display."
  :group 'efrit-progress)

(defface efrit-progress-error
  '((t :foreground "red3"))
  "Face for error messages in progress display."
  :group 'efrit-progress)

(defface efrit-progress-claude-message
  '((t :foreground "RoyalBlue"))
  "Face for Claude's messages in progress display."
  :group 'efrit-progress)

(defface efrit-progress-diff-added
  '((t :foreground "green4" :background "honeydew1"))
  "Face for added lines in diffs."
  :group 'efrit-progress)

(defface efrit-progress-diff-removed
  '((t :foreground "red4" :background "mistyrose1"))
  "Face for removed lines in diffs."
  :group 'efrit-progress)

;;; Progress State

(defvar efrit-progress--current-session nil
  "Current session being displayed.")

(defvar efrit-progress--last-tool nil
  "Track the last tool called to detect loops.")

(defvar efrit-progress--tool-repeat-count 0
  "Count consecutive calls to the same tool.")

;;; Progress Buffer Management

(defun efrit-progress--get-buffer ()
  "Get or create the progress buffer."
  (let ((buffer (get-buffer-create efrit-progress-buffer-name)))
    (with-current-buffer buffer
      (unless (eq major-mode 'efrit-progress-mode)
        (efrit-progress-mode)))
    buffer))

(defun efrit-progress--timestamp ()
  "Return formatted timestamp."
  (propertize (format-time-string efrit-progress-timestamp-format)
              'face 'efrit-progress-timestamp))

(defun efrit-progress--append (text &optional face)
  "Append TEXT to progress buffer with optional FACE."
  (let ((buffer (efrit-progress--get-buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (was-at-end (= (point) (point-max))))
        (goto-char (point-max))
        (insert (efrit-progress--timestamp) " ")
        (insert (if face (propertize text 'face face) text))
        (insert "\n")
        (when was-at-end
          (goto-char (point-max))
          (when (get-buffer-window buffer)
            (set-window-point (get-buffer-window buffer) (point))))))))

(defun efrit-progress--append-section (header content)
  "Append a section with HEADER and CONTENT."
  (efrit-progress--append header 'efrit-progress-section-header)
  (efrit-progress--append content))

;;; Progress Public API

(defun efrit-progress-start-session (session-id command)
  "Start progress tracking for SESSION-ID with COMMAND."
  (setq efrit-progress--current-session session-id)
  (let ((buffer (efrit-progress--get-buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (efrit-progress--append
     (format "═══ Starting Efrit Session: %s ═══" session-id)
     'efrit-progress-section-header)
    (efrit-progress--append (format "Command: %s\n" command))
    (when efrit-progress-auto-show
      (display-buffer buffer))))

(defun efrit-progress-show-message (message &optional type)
  "Show MESSAGE in progress buffer with optional TYPE.
TYPE can be \\='claude, \\='error, \\='success, or nil."
  (let ((face (pcase type
                ('claude 'efrit-progress-claude-message)
                ('error 'efrit-progress-error)
                ('success 'efrit-progress-success)
                (_ nil))))
    (efrit-progress--append message face)))

(defun efrit-progress-show-tool-start (tool-name input)
  "Show the start of TOOL-NAME execution with INPUT."
  (when (memq efrit-progress-verbosity '(normal verbose))
    ;; Detect repeated tool calls
    (if (equal tool-name efrit-progress--last-tool)
        (progn
          (setq efrit-progress--tool-repeat-count (1+ efrit-progress--tool-repeat-count))
          (cond
           ;; First warning at 3 repetitions
           ((= efrit-progress--tool-repeat-count 3)
            (efrit-progress--append
             (format "\n⚠️ WARNING: %s called %d times in a row - possible loop!\n🚨 NEXT ACTION: Try a different tool or approach!"
                     tool-name efrit-progress--tool-repeat-count)
             'error))
           ;; Stronger warning at 5 repetitions
           ((= efrit-progress--tool-repeat-count 5)
            (efrit-progress--append
             (format "\n🚨 CRITICAL LOOP: %s called %d times!\n🔄 MANDATORY: Change your approach - current strategy is failing!\n💡 SUGGESTION: If using todo_analyze repeatedly, try eval_sexp instead!"
                     tool-name efrit-progress--tool-repeat-count)
             'error))
           ;; Emergency stop at 7 repetitions
           ((>= efrit-progress--tool-repeat-count 7)
            (efrit-progress--append
             (format "\n🛑 EMERGENCY STOP: %s called %d times - FORCING TOOL CHANGE!\n⚠️ The system is preventing infinite loops. You MUST use a different tool."
                     tool-name efrit-progress--tool-repeat-count)
             'error))))
      (setq efrit-progress--tool-repeat-count 1))
    (setq efrit-progress--last-tool tool-name)

    (efrit-progress--append
     (format "▶ Executing tool: %s" tool-name)
     'efrit-progress-tool-name)
    ;; Show meaningful info for TODO tools
    (cond
     ((and (string= tool-name "todo_add") input)
      (let ((content (if (hash-table-p input)
                       (gethash "content" input)
                     input)))
        (when content
          (efrit-progress--append
           (format "\n  → Adding: %s" content)
           'font-lock-comment-face))))
     ((and (string= tool-name "todo_update") input)
      (let ((id (when (hash-table-p input) (gethash "id" input)))
            (status (when (hash-table-p input) (gethash "status" input))))
        (when (and id status)
          (efrit-progress--append
           (format "\n  → Marking %s as %s" id status)
           'font-lock-comment-face))))
     ((eq efrit-progress-verbosity 'verbose)
      (efrit-progress--append
       (format "  Input: %s" (efrit-common-truncate-string
                              (format "%S" input) 200)))))))

(defun efrit-progress-show-tool-result (tool-name result success-p)
  "Show the RESULT of TOOL-NAME execution.
SUCCESS-P indicates if the execution was successful."
  (when (memq efrit-progress-verbosity '(normal verbose))
    (efrit-progress--append
     (format "◀ %s: %s" tool-name (if success-p "Success" "Failed"))
     (if success-p 'efrit-progress-success 'efrit-progress-error))
    (when (or (not success-p) (eq efrit-progress-verbosity 'verbose))
      (efrit-progress--append
       (format "  Result: %s" (efrit-common-truncate-string
                                (format "%S" result) 300))))))

(defun efrit-progress-show-elisp-eval (code result)
  "Show Elisp CODE evaluation and RESULT."
  (when (memq efrit-progress-verbosity '(normal verbose))
    (efrit-progress--append "▶ Evaluating Elisp:" 'efrit-progress-tool-name)
    (efrit-progress--append (format "  %s" code))
    (efrit-progress--append
     (format "  → %s" (efrit-common-truncate-string
                       (format "%S" result) 200)))))

(defun efrit-progress-show-file-edit (file old-content new-content)
  "Show FILE edit with OLD-CONTENT and NEW-CONTENT."
  (when (memq efrit-progress-verbosity '(normal verbose))
    (efrit-progress--append
     (format "📝 Editing file: %s" file) 'efrit-progress-tool-name)
    (when (eq efrit-progress-verbosity 'verbose)
      ;; Simple diff display
      (let ((old-lines (split-string old-content "\n"))
            (new-lines (split-string new-content "\n")))
        (efrit-progress--append "  Changes:")
        (dotimes (i (max (length old-lines) (length new-lines)))
          (let ((old-line (nth i old-lines))
                (new-line (nth i new-lines)))
            (cond
             ((and old-line new-line (not (string= old-line new-line)))
              (efrit-progress--append
               (format "  - %s" old-line) 'efrit-progress-diff-removed)
              (efrit-progress--append
               (format "  + %s" new-line) 'efrit-progress-diff-added))
             ((and new-line (not old-line))
              (efrit-progress--append
               (format "  + %s" new-line) 'efrit-progress-diff-added))
             ((and old-line (not new-line))
              (efrit-progress--append
               (format "  - %s" old-line) 'efrit-progress-diff-removed)))))))))

(defun efrit-progress-show-command-output (command output exit-code)
  "Show COMMAND execution with OUTPUT and EXIT-CODE."
  (efrit-progress--append
   (format "$ %s" command) 'efrit-progress-tool-name)
  (when (not (string-empty-p output))
    ;; Apply ANSI color codes if present
    (let ((colored-output (ansi-color-apply output)))
      (dolist (line (split-string colored-output "\n"))
        (unless (string-empty-p line)
          (efrit-progress--append (format "  %s" line))))))
  (when (not (zerop exit-code))
    (efrit-progress--append
     (format "  Exit code: %d" exit-code) 'efrit-progress-error)))

(defun efrit-progress-end-session (session-id success-p)
  "End progress tracking for SESSION-ID with SUCCESS-P status."
  (efrit-progress--append
   (format "═══ Session %s: %s ═══"
           session-id
           (if success-p "Completed" "Failed"))
   (if success-p 'efrit-progress-success 'efrit-progress-error))
  (setq efrit-progress--current-session nil))

;;; Progress Mode

(defvar efrit-progress-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'quit-window)
    (define-key map "c" 'efrit-progress-clear)
    (define-key map "v" 'efrit-progress-cycle-verbosity)
    map)
  "Keymap for efrit-progress-mode.")

(define-derived-mode efrit-progress-mode special-mode "Efrit-Progress"
  "Major mode for viewing Efrit operation progress.
\\{efrit-progress-mode-map}"
  (setq-local truncate-lines nil)
  (setq-local word-wrap t))

(defun efrit-progress-clear ()
  "Clear the progress buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defun efrit-progress-cycle-verbosity ()
  "Cycle through verbosity levels."
  (interactive)
  (setq efrit-progress-verbosity
        (pcase efrit-progress-verbosity
          ('minimal 'normal)
          ('normal 'verbose)
          ('verbose 'minimal)))
  (message "Progress verbosity: %s" efrit-progress-verbosity))

(defun efrit-progress-show ()
  "Show the progress buffer with shrink-to-fit."
  (interactive)
  (let* ((buffer (efrit-progress--get-buffer))
         (window (display-buffer buffer
                                (if efrit-progress-auto-shrink
                                    '((display-buffer-reuse-window
                                       display-buffer-below-selected)
                                      (window-height . fit-window-to-buffer)
                                      (window-parameters . ((no-delete-other-windows . t))))
                                  '(display-buffer-reuse-window
                                    display-buffer-below-selected)))))
    (when (and window efrit-progress-auto-shrink)
      (fit-window-to-buffer window nil nil 20 nil))))

(defun efrit-progress-show-todos ()
  "Display current TODOs in progress buffer."
  (require 'efrit-do)
  (when (bound-and-true-p efrit-do--current-todos)
    (let ((buffer (efrit-progress--get-buffer)))
      (with-current-buffer buffer
        (goto-char (point-max))
        (efrit-progress--append "\n━━━ TODO List ━━━\n" 'font-lock-function-name-face)
        (if (null efrit-do--current-todos)
            (efrit-progress--append "No TODOs\n")
          (let ((total (length efrit-do--current-todos))
                (completed (seq-count (lambda (todo)
                                       (eq (efrit-do-todo-item-status todo) 'completed))
                                     efrit-do--current-todos))
                (in-progress (seq-count (lambda (todo)
                                         (eq (efrit-do-todo-item-status todo) 'in-progress))
                                       efrit-do--current-todos)))
            (efrit-progress--append (format "Progress: %d/%d completed, %d in progress\n\n"
                                          completed total in-progress)
                                  'font-lock-comment-face)
            (dolist (todo efrit-do--current-todos)
              (let* ((status (efrit-do-todo-item-status todo))
                     (icon (pcase status
                             ('todo "☐")
                             ('in-progress "⟳")
                             ('completed "☑")))
                     (face (pcase status
                             ('todo 'default)
                             ('in-progress 'font-lock-warning-face)
                             ('completed 'font-lock-comment-face))))
                (efrit-progress--append (format "%s %s\n" icon
                                              (efrit-do-todo-item-content todo))
                                      face)))))))))

(defun efrit-progress-update-todo (todo-id new-status)
  "Update TODO display when TODO-ID changes to NEW-STATUS."
  (efrit-progress-show-message
   (format "TODO %s → %s" todo-id new-status)
   (if (eq new-status 'completed) 'success 'info)))

;;; ========================================================================
;;; SECTION 2: PERFORMANCE OPTIMIZATION
;;; Caching, memory management, metrics tracking
;;; ========================================================================

(defgroup efrit-performance nil
  "Performance optimizations for Efrit."
  :group 'efrit)

;;; Memory Management

(defcustom efrit-performance-max-sessions 10
  "Maximum number of sessions to keep in memory."
  :type 'integer
  :group 'efrit-performance)

(defcustom efrit-performance-session-ttl 3600
  "Time to live for inactive sessions in seconds."
  :type 'integer
  :group 'efrit-performance)

(defvar efrit-performance--session-timestamps (make-hash-table :test 'equal)
  "Track last access time for sessions.")

(defun efrit-performance-cleanup-old-sessions ()
  "Clean up sessions older than TTL."
  (let ((cutoff-time (- (float-time) efrit-performance-session-ttl))
        (sessions-to-remove nil))
    ;; Find expired sessions
    (maphash (lambda (session-id timestamp)
               (when (< timestamp cutoff-time)
                 (push session-id sessions-to-remove)))
             efrit-performance--session-timestamps)
    ;; Remove them
    (dolist (session-id sessions-to-remove)
      (efrit-log 'debug "Cleaning up expired session: %s" session-id)
      (efrit-performance--remove-session session-id))))

(defun efrit-performance--remove-session (session-id)
  "Remove SESSION-ID from all tracking structures."
  ;; Remove from async module
  (when (boundp 'efrit-async--sessions)
    (remhash session-id efrit-async--sessions))
  ;; Remove from timestamps
  (remhash session-id efrit-performance--session-timestamps)
  ;; Clean up associated buffers
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (boundp 'efrit-session-id)
                 (equal efrit-session-id session-id))
        (kill-buffer buffer)))))

(defun efrit-performance-touch-session (session-id)
  "Update last access time for SESSION-ID."
  (puthash session-id (float-time) efrit-performance--session-timestamps))

;;; Request Caching

(defvar efrit-performance--request-cache (make-hash-table :test 'equal)
  "Cache for recent API requests.")

(defcustom efrit-performance-cache-ttl 300
  "Cache time to live in seconds."
  :type 'integer
  :group 'efrit-performance)

(cl-defstruct efrit-cache-entry
  "Cache entry for API responses."
  response
  timestamp
  hit-count)

(defun efrit-performance-cache-key (command context)
  "Generate cache key from COMMAND and CONTEXT."
  (secure-hash 'sha256 (format "%s:%s" command
                                (if context
                                    (efrit-common-truncate-string
                                     (json-encode
                                     (if (and context (fboundp 'efrit-context-state-buffer-name))
                                     ;; Convert struct to alist for JSON encoding
                                     (list (cons 'buffer-name (efrit-context-state-buffer-name context))
                                     (cons 'buffer-mode (format "%s" (efrit-context-state-buffer-mode context)))
                                     (cons 'directory (efrit-context-state-directory context)))
                                     context))
                                     1000)
                                  ""))))

(defun efrit-performance-get-cached (key)
  "Get cached response for KEY if still valid."
  (let ((entry (gethash key efrit-performance--request-cache)))
    (if (and entry
             (< (- (float-time) (efrit-cache-entry-timestamp entry))
                efrit-performance-cache-ttl))
        (progn
          (cl-incf (efrit-cache-entry-hit-count entry))
          (efrit-log 'debug "Cache hit for key %s (hits: %d)"
                     (substring key 0 8)
                     (efrit-cache-entry-hit-count entry))
          (efrit--record-metric 'cache-hits 1)
          (efrit-cache-entry-response entry))
      ;; Cache miss
      (when entry  ; Key exists but expired
        (efrit--record-metric 'cache-misses 1))
      nil)))

(defun efrit-performance-cache-put (key response)
  "Store RESPONSE in cache under KEY."
  (puthash key
           (make-efrit-cache-entry
            :response response
            :timestamp (float-time)
            :hit-count 0)
           efrit-performance--request-cache))

(defun efrit-performance-clear-cache ()
  "Clear the request cache."
  (interactive)
  (clrhash efrit-performance--request-cache)
  (message "Efrit request cache cleared"))

;;; Response Streaming

(defvar efrit-performance--streaming-enabled nil
  "Whether to enable response streaming.")

(defun efrit-performance-parse-streaming-chunk (chunk)
  "Parse a streaming CHUNK from the API."
  ;; This would parse SSE (Server-Sent Events) format
  ;; For now, just a placeholder
  (when (string-match "^data: \\(.+\\)$" chunk)
    (condition-case nil
        (json-read-from-string (match-string 1 chunk))
      (error nil))))

;;; Optimized JSON Handling

(defun efrit-performance-json-read-optimized (string)
  "Optimized JSON reading for large responses."
  ;; Use native JSON if available (Emacs 27+)
  (if (fboundp 'json-parse-string)
      (json-parse-string string
                         :object-type 'alist
                         :array-type 'list
                         :null-object nil
                         :false-object nil)
    ;; Fall back to traditional parser
    (json-read-from-string string)))

;;; Performance Monitoring

(defvar efrit-performance--api-times nil
  "List of recent API call times for monitoring.")

(defvar efrit--metrics (make-hash-table :test 'equal)
  "Hash table tracking various performance metrics.
Keys:
  api-call-times - List of API call durations
  token-usage - List of token counts (input/output)
  cache-hits - Total cache hit count
  cache-misses - Total cache miss count
  total-requests - Total API requests made
  errors - List of error occurrences")

(defun efrit--record-metric (metric-key value)
  "Record VALUE for METRIC-KEY in the metrics system.
Metric types:
  - Counters: total-requests, cache-hits, cache-misses, errors
  - Lists: api-call-times, token-usage (kept to last 100 entries)
  - Values: Any other key stores the raw value"
  (let ((current (gethash metric-key efrit--metrics)))
    (cond
     ;; Counter metrics - increment
     ((memq metric-key '(total-requests cache-hits cache-misses errors))
      (puthash metric-key (1+ (or current 0)) efrit--metrics))

     ;; List metrics - append and limit to 100
     ((memq metric-key '(api-call-times token-usage))
      (let ((updated-list (cons value (or current nil))))
        (when (> (length updated-list) 100)
          (setq updated-list (cl-subseq updated-list 0 100)))
        (puthash metric-key updated-list efrit--metrics)))

     ;; Other metrics - store value directly
     (t
      (puthash metric-key value efrit--metrics)))))

(defun efrit--get-metric (metric-key)
  "Get the value of METRIC-KEY from the metrics system."
  (gethash metric-key efrit--metrics))

(defun efrit--clear-metrics ()
  "Clear all metrics data."
  (interactive)
  (clrhash efrit--metrics)
  (message "Efrit metrics cleared"))

(defun efrit-performance-record-api-time (elapsed-time)
  "Record ELAPSED-TIME for performance monitoring."
  (push elapsed-time efrit-performance--api-times)
  ;; Keep only last 100 times
  (when (> (length efrit-performance--api-times) 100)
    (setq efrit-performance--api-times
          (cl-subseq efrit-performance--api-times 0 100)))
  ;; Also record in new metrics system
  (efrit--record-metric 'api-call-times elapsed-time)
  (efrit--record-metric 'total-requests 1))

(defun efrit-performance-get-stats ()
  "Get performance statistics."
  (when efrit-performance--api-times
    (let* ((times (sort (copy-sequence efrit-performance--api-times) #'<))
           (count (length times))
           (median (if (cl-oddp count)
                       (nth (/ count 2) times)
                     (/ (+ (nth (1- (/ count 2)) times)
                           (nth (/ count 2) times))
                        2.0)))
           (mean (/ (cl-reduce #'+ times) (float count))))
      (list :count count
            :mean mean
            :median median
            :min (car times)
            :max (car (last times))))))

(defun efrit-performance-show-stats ()
  "Display performance statistics."
  (interactive)
  (let ((stats (efrit-performance-get-stats)))
    (if stats
        (message "API Performance - Calls: %d, Mean: %.2fs, Median: %.2fs, Range: %.2f-%.2fs"
                 (plist-get stats :count)
                 (plist-get stats :mean)
                 (plist-get stats :median)
                 (plist-get stats :min)
                 (plist-get stats :max))
      (message "No performance data collected yet"))))

;;; Background Cleanup Timer

(defvar efrit-performance--cleanup-timer nil
  "Timer for periodic cleanup tasks.")

(defun efrit-performance-start-cleanup-timer ()
  "Start periodic cleanup timer."
  (when efrit-performance--cleanup-timer
    (cancel-timer efrit-performance--cleanup-timer))
  (setq efrit-performance--cleanup-timer
        (run-with-timer 300 300 #'efrit-performance-run-cleanup)))

(defun efrit-performance-stop-cleanup-timer ()
  "Stop the cleanup timer."
  (when efrit-performance--cleanup-timer
    (cancel-timer efrit-performance--cleanup-timer)
    (setq efrit-performance--cleanup-timer nil)))

(defun efrit-performance-run-cleanup ()
  "Run all cleanup tasks."
  (efrit-log 'debug "Running performance cleanup")
  (efrit-performance-cleanup-old-sessions)
  (efrit-performance-cleanup-cache))

(defun efrit-performance-cleanup-cache ()
  "Remove expired cache entries."
  (let ((cutoff-time (- (float-time) efrit-performance-cache-ttl))
        (keys-to-remove nil))
    (maphash (lambda (key entry)
               (when (< (efrit-cache-entry-timestamp entry) cutoff-time)
                 (push key keys-to-remove)))
             efrit-performance--request-cache)
    (dolist (key keys-to-remove)
      (remhash key efrit-performance--request-cache))))

;;; ========================================================================
;;; SECTION 3: DASHBOARD
;;; Session and TODO management UI
;;; ========================================================================

(defgroup efrit-dashboard nil
  "Dashboard interface for Efrit AI assistant."
  :group 'efrit
  :prefix "efrit-dashboard-")

(defconst efrit-dashboard-buffer-name "*efrit-dashboard*"
  "Name of the efrit dashboard buffer.")

(defcustom efrit-dashboard-auto-refresh-interval 5
  "Seconds between dashboard auto-refreshes."
  :type 'number
  :group 'efrit-dashboard)

(defcustom efrit-dashboard-use-unicode-symbols t
  "Whether to use Unicode symbols in dashboard headings.
When nil, uses ASCII alternatives for better TTY compatibility."
  :type 'boolean
  :group 'efrit-dashboard)

(defvar efrit-dashboard-refresh-timer nil
  "Timer for auto-refreshing the dashboard.")

(defvar efrit-dashboard-session-state
  '((start-time . nil)
    (commands-executed . 0)
    (todos-completed . 0)
    (todos-active . 0)
    (buffers-created . ())
    (files-modified . ())
    (api-calls . 0)
    (last-activity . nil))
  "Current session state for dashboard display.")

;;; Dashboard Mode

(defvar efrit-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") 'efrit-dashboard-refresh)
    (define-key map (kbd "q") 'efrit-dashboard-quit)
    (define-key map (kbd "c") 'efrit-dashboard-clear-todos)
    (define-key map (kbd "l") 'efrit-dashboard-show-log)
    (define-key map (kbd "s") 'efrit-dashboard-save-session)
    (define-key map (kbd "TAB") 'efrit-dashboard-next-section)
    (define-key map (kbd "S-TAB") 'efrit-dashboard-prev-section)
    (define-key map (kbd "RET") 'efrit-dashboard-action-at-point)
    map)
  "Keymap for efrit dashboard mode.")

(define-derived-mode efrit-dashboard-mode special-mode "Efrit-Dashboard"
  "Major mode for the Efrit Dashboard.

\\{efrit-dashboard-mode-map}"
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (hl-line-mode 1))

;;; Dashboard Core Functions

;;;###autoload
(defun efrit-dashboard ()
  "Open or refresh the Efrit Dashboard."
  (interactive)
  (let ((buffer (get-buffer-create efrit-dashboard-buffer-name)))
    (with-current-buffer buffer
      (efrit-dashboard-mode)
      (efrit-dashboard-refresh)
      (efrit-dashboard-setup-auto-refresh))
    (display-buffer buffer)
    (select-window (get-buffer-window buffer))))

(defun efrit-dashboard-refresh ()
  "Refresh the dashboard content."
  (interactive)
  (with-current-buffer (get-buffer-create efrit-dashboard-buffer-name)
    (let ((inhibit-read-only t)
          (point-pos (point)))
      (erase-buffer)
      (efrit-dashboard-insert-header)
      (efrit-dashboard-insert-session-state)
      (efrit-dashboard-insert-todo-panel)
      (efrit-dashboard-insert-queue-status)
      (efrit-dashboard-insert-recent-activity)
      (efrit-dashboard-insert-quick-actions)
      (goto-char (min point-pos (point-max))))))

(defun efrit-dashboard-insert-header ()
  "Insert the dashboard header."
  (insert (propertize "EFRIT DASHBOARD" 'face 'bold 'font-lock-face 'bold) "\n")
  (insert (format "Session started: %s\n"
                  (or (cdr (assoc 'start-time efrit-dashboard-session-state))
                      (format-time-string "%Y-%m-%d %H:%M:%S"))))
  (insert (make-string 50 ?═) "\n\n"))

(defun efrit-dashboard--insert-section-header (title icon)
  "Insert section header with TITLE and optional ICON."
  (let ((header-text (if (and efrit-dashboard-use-unicode-symbols icon)
                         (format "%s %s" icon title)
                       title)))
    (insert (propertize (format "### %s" header-text)
                        'face 'bold
                        'efrit-section-header t)
            "\n")))

(defun efrit-dashboard-insert-session-state ()
  "Insert session state panel."
  (efrit-dashboard--insert-section-header "SESSION STATE" "📊")

  ;; Try to get data from session tracker if available
  (let ((session-data (when (fboundp 'efrit-session-get-summary)
                        (efrit-session-get-summary)))
        (dashboard-state efrit-dashboard-session-state))

    (if session-data
        ;; Use session tracker data
        (let ((metrics (plist-get session-data :metrics)))
          (insert (format "• Commands executed: %d\n"
                          (or (cdr (assoc 'commands-executed metrics)) 0)))
          (insert (format "• TODOs created: %d\n"
                          (or (cdr (assoc 'todos-created metrics)) 0)))
          (insert (format "• TODOs completed: %d\n"
                          (or (cdr (assoc 'todos-completed metrics)) 0)))
          (insert (format "• API calls made: %d\n"
                          (or (cdr (assoc 'api-calls metrics)) 0)))
          (insert (format "• Session duration: %.1fs\n"
                          (or (plist-get session-data :duration) 0))))

      ;; Fall back to dashboard state
      (insert (format "• Commands executed: %d\n"
                      (cdr (assoc 'commands-executed dashboard-state))))
      (insert (format "• TODOs completed: %d\n"
                      (cdr (assoc 'todos-completed dashboard-state))))
      (insert (format "• TODOs active: %d\n"
                      (cdr (assoc 'todos-active dashboard-state))))
      (insert (format "• API calls made: %d\n"
                      (cdr (assoc 'api-calls dashboard-state))))
      (insert (format "• Last activity: %s\n"
                      (or (cdr (assoc 'last-activity dashboard-state)) "None")))))
  (insert "\n"))

(defun efrit-dashboard-insert-todo-panel ()
  "Insert TODO management panel."
  (efrit-dashboard--insert-section-header "TODO MANAGEMENT" "📋")

  ;; Get TODOs from current session if available
  (let* ((todos-file (expand-file-name "todos.json"
                                       (expand-file-name "context" efrit-data-directory)))
         (todos (when (file-exists-p todos-file)
                  (efrit-dashboard-read-json-file todos-file))))

    (if todos
        (progn
          ;; Active TODOs
          (insert (propertize "Active TODOs:" 'face 'font-lock-keyword-face) "\n")
          (let ((active-count 0))
            (dolist (todo todos)
              (let ((status (cdr (assoc 'status todo)))
                    (priority (cdr (assoc 'priority todo)))
                    (content (cdr (assoc 'content todo))))
                (when (not (string= status "completed"))
                  (setq active-count (1+ active-count))
                  (insert (format "  %s [%s] %s\n"
                                  (if (string= status "in-progress") "⟳" "☐")
                                  (upcase priority)
                                  content)))))
            (when (= active-count 0)
              (insert "  (No active TODOs)\n")))

          ;; Completed TODOs (last 3)
          (insert "\n" (propertize "Recently Completed:" 'face 'font-lock-keyword-face) "\n")
          (let ((completed-todos (seq-filter (lambda (todo)
                                               (string= (cdr (assoc 'status todo)) "completed"))
                                             todos))
                (shown-count 0))
            (if completed-todos
                (dolist (todo (seq-take completed-todos 3))
                  (insert (format "  ☑ %s\n" (cdr (assoc 'content todo))))
                  (setq shown-count (1+ shown-count)))
              (insert "  (No completed TODOs)\n"))))

      (insert "  (No TODO data available)\n")))
  (insert "\n"))

(defun efrit-dashboard-insert-queue-status ()
  "Insert AI communication queue status."
  (efrit-dashboard--insert-section-header "QUEUE STATUS" "🔗")

  (let* ((queues-dir (expand-file-name "queues" efrit-data-directory))
         (requests-dir (expand-file-name "requests" queues-dir))
         (processing-dir (expand-file-name "processing" queues-dir))
         (responses-dir (expand-file-name "responses" queues-dir)))

    (insert (format "• Pending requests: %d\n"
                    (if (file-directory-p requests-dir)
                        (length (directory-files requests-dir nil "^[^.]"))
                        0)))
    (insert (format "• Processing: %d\n"
                    (if (file-directory-p processing-dir)
                        (length (directory-files processing-dir nil "^[^.]"))
                        0)))
    (insert (format "• Completed responses: %d\n"
                    (if (file-directory-p responses-dir)
                        (length (directory-files responses-dir nil "^[^.]"))
                        0))))
  (insert "\n"))

(defun efrit-dashboard-insert-recent-activity ()
  "Insert recent activity log."
  (efrit-dashboard--insert-section-header "RECENT ACTIVITY" "🔍")

  (let* ((log-file (expand-file-name "efrit.log"
                                     (expand-file-name "logs" efrit-data-directory))))
    (if (file-exists-p log-file)
        (with-temp-buffer
          (insert-file-contents log-file nil nil nil t)
          (goto-char (point-max))
          (forward-line -3) ; Show last 3 lines
          (let ((recent-log (buffer-substring-no-properties (point) (point-max))))
            (if (string-blank-p recent-log)
                (insert "  (No recent activity)\n")
              (insert "  " (replace-regexp-in-string "\n" "\n  " recent-log)))))
      (insert "  (No log file found)\n")))
  (insert "\n"))

(defun efrit-dashboard-insert-quick-actions ()
  "Insert quick action buttons."
  (efrit-dashboard--insert-section-header "QUICK ACTIONS" "⚡")
  (insert "  g - Refresh dashboard\n")
  (insert "  c - Clear completed TODOs\n")
  (insert "  l - Show full log\n")
  (insert "  s - Save session state\n")
  (insert "  q - Quit dashboard\n"))

;;; Dashboard Helper Functions

(defun efrit-dashboard-read-json-file (file)
  "Read and parse JSON from FILE.
Returns parsed data structure on success, nil if file doesn't exist,
or :malformed if JSON parsing fails."
  (cond
   ((not (file-exists-p file)) nil)
   (t
    (with-temp-buffer
      (insert-file-contents file)
      (condition-case err
          (let ((json-array-type 'list)      ; Parse arrays as lists
                (json-object-type 'alist))   ; Parse objects as alists
            (json-read-from-string (buffer-string)))
        (error
         (message "Warning: Malformed JSON in %s: %s" file (error-message-string err))
         :malformed))))))

(defun efrit-dashboard-update-session-state (key value)
  "Update session state KEY with VALUE."
  (setf (alist-get key efrit-dashboard-session-state) value)
  (setf (alist-get 'last-activity efrit-dashboard-session-state)
        (format-time-string "%H:%M:%S")))

(defun efrit-dashboard-increment-counter (key)
  "Increment session state counter KEY."
  (cl-incf (alist-get key efrit-dashboard-session-state 0))
  (setf (alist-get 'last-activity efrit-dashboard-session-state)
        (format-time-string "%H:%M:%S")))

;;; Dashboard Auto-refresh

(defun efrit-dashboard-setup-auto-refresh ()
  "Setup auto-refresh timer for the dashboard."
  (when efrit-dashboard-refresh-timer
    (cancel-timer efrit-dashboard-refresh-timer))
  (setq efrit-dashboard-refresh-timer
        (run-with-timer efrit-dashboard-auto-refresh-interval
                        efrit-dashboard-auto-refresh-interval
                        'efrit-dashboard-refresh-if-visible)))

(defun efrit-dashboard-refresh-if-visible ()
  "Refresh dashboard only if it's currently visible."
  (when (and (buffer-live-p (get-buffer efrit-dashboard-buffer-name))
             (get-buffer-window efrit-dashboard-buffer-name))
    (efrit-dashboard-refresh)))

;;; Dashboard Interactive Commands

(defun efrit-dashboard-quit ()
  "Quit the dashboard and clean up."
  (interactive)
  (when efrit-dashboard-refresh-timer
    (cancel-timer efrit-dashboard-refresh-timer)
    (setq efrit-dashboard-refresh-timer nil))
  (quit-window t))

(defun efrit-dashboard-clear-todos ()
  "Clear completed TODOs."
  (interactive)
  (let ((todos-file (expand-file-name "todos.json"
                                     (expand-file-name "context" efrit-data-directory))))
    (when (file-exists-p todos-file)
      (let* ((todos (efrit-dashboard-read-json-file todos-file))
             (active-todos (seq-filter (lambda (todo)
                                        (not (string= (cdr (assoc 'status todo)) "completed")))
                                      todos)))
        (with-temp-file todos-file
          (insert (json-encode active-todos)))))
    (efrit-dashboard-refresh)
    (message "Cleared completed TODOs")))

(defun efrit-dashboard-show-log ()
  "Show the full efrit log."
  (interactive)
  (let ((log-file (expand-file-name "efrit.log"
                                   (expand-file-name "logs" efrit-data-directory))))
    (if (file-exists-p log-file)
        (find-file-other-window log-file)
      (message "No log file found"))))

(defun efrit-dashboard-save-session ()
  "Save current session state."
  (interactive)
  (let ((session-file (expand-file-name "current-session.json"
                                       (expand-file-name "sessions" efrit-data-directory))))
    (make-directory (file-name-directory session-file) t)
    (with-temp-file session-file
      (insert (json-encode efrit-dashboard-session-state)))
    (message "Session state saved")))

(defun efrit-dashboard-next-section ()
  "Navigate to next section."
  (interactive)
  (let ((pos (next-single-property-change (point) 'efrit-section-header)))
    (when pos
      (goto-char pos))))

(defun efrit-dashboard-prev-section ()
  "Navigate to previous section."
  (interactive)
  (let ((pos (previous-single-property-change (point) 'efrit-section-header)))
    (when pos
      (goto-char pos))))

(defun efrit-dashboard-action-at-point ()
  "Perform action at current point."
  (interactive)
  (message "Action at point not implemented yet"))

;;; Dashboard Integration Hooks

(defun efrit-dashboard-track-command ()
  "Track command execution for dashboard."
  (efrit-dashboard-increment-counter 'commands-executed))

(defun efrit-dashboard-track-todo-completion ()
  "Track TODO completion for dashboard."
  (efrit-dashboard-increment-counter 'todos-completed))

(defun efrit-dashboard-track-api-call ()
  "Track API call for dashboard."
  (efrit-dashboard-increment-counter 'api-calls))

;;; ========================================================================
;;; SECTION 4: METRICS DASHBOARD
;;; Comprehensive performance metrics visualization
;;; ========================================================================

(defun efrit-show-metrics ()
  "Display comprehensive metrics dashboard in a buffer."
  (interactive)
  (with-current-buffer (get-buffer-create "*Efrit Metrics*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "Efrit Performance Metrics Dashboard\n")
      (insert "====================================\n\n")

      ;; API Call Statistics
      (insert "API Calls\n")
      (insert "---------\n")
      (let ((total-requests (or (efrit--get-metric 'total-requests) 0))
            (api-times (efrit--get-metric 'api-call-times)))
        (insert (format "Total requests: %d\n" total-requests))
        (when api-times
          (let* ((times (sort (copy-sequence api-times) #'<))
                 (count (length times))
                 (mean (/ (cl-reduce #'+ times) (float count)))
                 (median (if (cl-oddp count)
                            (nth (/ count 2) times)
                          (/ (+ (nth (1- (/ count 2)) times)
                                (nth (/ count 2) times))
                             2.0))))
            (insert (format "Recent calls analyzed: %d\n" count))
            (insert (format "Mean response time: %.2fs\n" mean))
            (insert (format "Median response time: %.2fs\n" median))
            (insert (format "Min response time: %.2fs\n" (car times)))
            (insert (format "Max response time: %.2fs\n" (car (last times)))))))
      (insert "\n")

      ;; Cache Statistics
      (insert "Cache Performance\n")
      (insert "-----------------\n")
      (let ((hits (or (efrit--get-metric 'cache-hits) 0))
            (misses (or (efrit--get-metric 'cache-misses) 0)))
        (insert (format "Cache hits: %d\n" hits))
        (insert (format "Cache misses: %d\n" misses))
        (when (> (+ hits misses) 0)
          (let ((hit-rate (* 100.0 (/ (float hits) (+ hits misses)))))
            (insert (format "Hit rate: %.1f%%\n" hit-rate)))))
      (let ((cache-size (hash-table-count efrit-performance--request-cache)))
        (insert (format "Cached entries: %d\n" cache-size)))
      (insert "\n")

      ;; Token Usage
      (insert "Token Usage\n")
      (insert "-----------\n")
      (let ((token-data (efrit--get-metric 'token-usage)))
        (if token-data
            (let ((total-input 0)
                  (total-output 0))
              (dolist (entry token-data)
                (when (consp entry)
                  (cl-incf total-input (or (alist-get 'input entry) 0))
                  (cl-incf total-output (or (alist-get 'output entry) 0))))
              (insert (format "Total input tokens: %d\n" total-input))
              (insert (format "Total output tokens: %d\n" total-output))
              (insert (format "Total tokens: %d\n" (+ total-input total-output))))
          (insert "No token usage data recorded yet\n")))
      (insert "\n")

      ;; Session Statistics
      (insert "Session Management\n")
      (insert "------------------\n")
      (let ((active-sessions (hash-table-count efrit-performance--session-timestamps)))
        (insert (format "Active sessions: %d\n" active-sessions))
        (insert (format "Max sessions: %d\n" efrit-performance-max-sessions))
        (insert (format "Session TTL: %d seconds\n" efrit-performance-session-ttl)))
      (insert "\n")

      ;; Error Statistics
      (insert "Errors\n")
      (insert "------\n")
      (let ((error-count (or (efrit--get-metric 'errors) 0)))
        (insert (format "Total errors: %d\n" error-count)))
      (insert "\n")

      ;; Actions
      (insert "====================================\n")
      (insert "Actions:\n")
      (insert "  C-c r - Refresh dashboard\n")
      (insert "  C-c c - Clear metrics\n")
      (insert "  C-c C - Clear cache\n")
      (insert "  q     - Quit dashboard\n"))

    ;; Set up key bindings
    (local-set-key (kbd "C-c r") #'efrit-show-metrics)
    (local-set-key (kbd "C-c c") #'efrit--clear-metrics)
    (local-set-key (kbd "C-c C") #'efrit-performance-clear-cache)
    (local-set-key (kbd "q") #'quit-window)

    (setq buffer-read-only t)
    (goto-char (point-min))
    (display-buffer (current-buffer))))

;;; ========================================================================
;;; INITIALIZATION
;;; ========================================================================

;; Initialize session state
(unless (cdr (assoc 'start-time efrit-dashboard-session-state))
  (efrit-dashboard-update-session-state 'start-time
                                       (format-time-string "%Y-%m-%d %H:%M:%S")))

;; Start cleanup timer
(efrit-performance-start-cleanup-timer)

(provide 'efrit-ui)

;;; efrit-ui.el ends here
