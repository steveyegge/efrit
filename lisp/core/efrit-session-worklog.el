;;; efrit-session-worklog.el --- Work log management and compression -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.4.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; Work log management for Efrit multi-step sessions.
;; Provides:
;; - Work log entry management (add, evict, compress)
;; - Budget-aware history management with LRU eviction
;; - Session budget API wrappers
;; - Work log compression for Claude context
;; - Progress tracking
;; - Conversation history for interactive sessions
;; - API message management (tool_use/tool_result format)
;;
;; Depends on efrit-session-core for the session struct.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'efrit-log)
(require 'efrit-common)
(require 'efrit-budget)
(require 'efrit-session-core)

;;; Customization

(defcustom efrit-session-max-work-log-entries 50
  "Maximum number of work log entries to keep in session history."
  :type 'integer
  :group 'efrit-session)

(defcustom efrit-session-context-compression 'smart
  "Level of context compression for work logs.
- minimal: Only essential information
- smart: Balanced compression (default)
- full: Include all available context"
  :type '(choice (const :tag "Minimal" minimal)
                 (const :tag "Smart" smart)
                 (const :tag "Full" full))
  :group 'efrit-session)

(defcustom efrit-session-max-result-length 200
  "Maximum length of result strings in compressed logs."
  :type 'integer
  :group 'efrit-session)

;;; Forward declarations for unified context (defined in efrit-session-context.el)
(declare-function efrit-unified-context-add-message "efrit-session-context")

;;; Work Log Management

(defun efrit-session-add-work (session result elisp &optional todo-snapshot tool-name)
  "Add a work log entry to SESSION.
Records RESULT from executing ELISP, with optional TODO-SNAPSHOT.
TOOL-NAME identifies which tool generated this result (for compression).
Updates budget tracking and may evict old entries if history budget exceeded."
  (when session
    (let* ((budget (efrit-session-budget session))
           ;; Compress result for history storage using tool-specific compression
           (compressed-result (if (and tool-name result)
                                  (efrit-budget-compress-for-history tool-name result)
                                (if (stringp result)
                                    (efrit-common-truncate-string result 500)
                                  (format "%s" result))))
           (entry (list compressed-result elisp todo-snapshot tool-name))
           (entry-tokens (efrit-budget-estimate-tokens
                          (format "%s %s" compressed-result elisp))))
      (push entry (efrit-session-work-log session))

      ;; Update budget and evict if over history budget
      (when budget
        (efrit-session--update-history-budget session entry-tokens))

      ;; Safety backstop: hard limit on entry count (should rarely trigger
      ;; if token-based eviction is working, but protects against edge cases)
      (when (> (length (efrit-session-work-log session))
               efrit-session-max-work-log-entries)
        (setf (efrit-session-work-log session)
              (seq-take (efrit-session-work-log session)
                       efrit-session-max-work-log-entries)))

      ;; Also add to unified context (tool call and result)
      ;; Note: Uses original result, not compressed, for richer context
      (when (fboundp 'efrit-unified-context-add-message)
        (efrit-unified-context-add-message 'user elisp 'async
                                          `((session . ,(efrit-session-id session))))
        (when result
          (efrit-unified-context-add-message 'assistant result 'async
                                            `((session . ,(efrit-session-id session))))))

      (efrit-log 'debug "Session %s: work log entry %d added"
                 (efrit-session-id session)
                 (length (efrit-session-work-log session))))))

(defun efrit-session-track-tool (session tool-name input-data result)
  "Track tool call in SESSION for loop detection.
Records TOOL-NAME, INPUT-DATA, and RESULT."
  (when session
    (let* ((timestamp (current-time))
           (progress-tick (or (efrit-session-last-progress-tick session) 0))
           (input-hash (secure-hash 'sha1 (format "%s" input-data)))
           (result-hash (secure-hash 'sha1 (format "%s" result)))
           (entry (list tool-name timestamp progress-tick input-hash result-hash)))
      (push entry (efrit-session-tool-history session))

      ;; Update last-tool-called for simple repeat detection
      (setf (efrit-session-last-tool-called session) tool-name))))

;;; Budget-Aware Work Log Management

(defun efrit-session-evict-oldest-work (session)
  "Remove the oldest work log entry from SESSION.
Returns the removed entry, or nil if work log is empty."
  (when (and session (efrit-session-work-log session))
    (let* ((work-log (efrit-session-work-log session))
           (oldest (car (last work-log))))
      (setf (efrit-session-work-log session) (butlast work-log))
      (efrit-log 'debug "Session %s: evicted oldest work log entry"
                 (efrit-session-id session))
      oldest)))

(defun efrit-session--calculate-history-tokens (session)
  "Calculate total tokens used by work log history in SESSION."
  (when session
    (let ((total 0))
      (dolist (entry (efrit-session-work-log session))
        (let ((result (nth 0 entry))
              (elisp (nth 1 entry)))
          (setq total (+ total (efrit-budget-estimate-tokens
                                (format "%s %s" result elisp))))))
      total)))

(defun efrit-session--update-history-budget (session new-entry-tokens)
  "Update SESSION's history budget after adding NEW-ENTRY-TOKENS.
Performs LRU eviction if history exceeds budget limit."
  (when-let* ((budget (efrit-session-budget session)))
    (let ((new-total (+ (efrit-budget-history-used budget) new-entry-tokens)))
      ;; Update with new total
      (efrit-budget-record-usage budget 'history new-total)
      ;; Evict if over budget (recalculates tokens only during eviction)
      (when (efrit-budget-history-over-limit-p budget new-total)
        (efrit-session--evict-until-under-budget session)))))

(defun efrit-session--evict-until-under-budget (session)
  "Evict oldest work log entries until SESSION history is under budget.
Uses LRU (Least Recently Used) eviction - oldest entries are removed first.
Recalculates token count after eviction to ensure accuracy."
  (when-let* ((budget (efrit-session-budget session)))
    ;; Recalculate from scratch to ensure accuracy after eviction
    (let ((history-tokens (efrit-session--calculate-history-tokens session)))
      (while (and (efrit-budget-history-over-limit-p budget history-tokens)
                  (> (length (efrit-session-work-log session)) 1))
        (efrit-session-evict-oldest-work session)
        (setq history-tokens (efrit-session--calculate-history-tokens session)))
      ;; Update budget with actual history tokens
      (efrit-budget-record-usage budget 'history history-tokens))))

;;; Session Budget API
;;
;; These functions provide a consistent session-level API for budget operations.
;; While they mostly delegate to efrit-budget functions, they:
;; - Handle nil sessions gracefully
;; - Encapsulate the budget struct implementation detail
;; - Allow future changes without affecting callers

(defun efrit-session-get-budget (session)
  "Get the budget struct for SESSION.
Returns nil if session has no budget tracking."
  (when session
    (efrit-session-budget session)))

(defun efrit-session-budget-status (session)
  "Return budget summary string for SESSION.
Suitable for inclusion in Claude context."
  (if-let* ((budget (efrit-session-get-budget session)))
      (efrit-budget-summary budget)
    "No budget tracking"))

(defun efrit-session-budget-for-claude (session)
  "Return budget state formatted for Claude API context.
Returns an alist suitable for JSON encoding."
  (when-let* ((budget (efrit-session-get-budget session)))
    (efrit-budget-for-claude budget)))

(defun efrit-session-budget-warning (session)
  "Return budget warning string if SESSION is low on budget.
Returns nil if no warning needed."
  (when-let* ((budget (efrit-session-get-budget session)))
    (efrit-budget-format-warning budget)))

(defun efrit-session-record-tool-result (session result)
  "Record a tool RESULT's token usage in SESSION's budget.
RESULT is the tool result string or object.
Returns the number of tokens recorded."
  (when-let* ((budget (efrit-session-get-budget session)))
    (efrit-budget-record-tool-result budget result)))

(defun efrit-session-reset-tool-results (session)
  "Reset tool results usage for a new API turn in SESSION.
Call this at the start of each Claude API call."
  (when-let* ((budget (efrit-session-get-budget session)))
    (efrit-budget-reset-tool-results budget)))

(defun efrit-session-allocate-tool-budget (session &optional tool-name)
  "Get token allocation for a tool call in SESSION.
TOOL-NAME is optional and reserved for future per-tool budgets.
Returns the number of tokens the tool should try to stay within."
  (if-let* ((budget (efrit-session-get-budget session)))
      (efrit-budget-allocate-tool budget tool-name)
    efrit-budget-per-tool-default))

;;; Work Log Compression

(defun efrit-session-compress-log (session)
  "Compress SESSION work log for efficient Claude context usage.
Returns a JSON string representation."
  (if (not session)
      "[]"
    (let ((work-log (efrit-session-work-log session)))
      (cl-case efrit-session-context-compression
        (minimal (efrit-session--compress-minimal work-log))
        (smart (efrit-session--compress-smart work-log))
        (full (efrit-session--compress-full work-log))
        (t (efrit-session--compress-smart work-log))))))

(defun efrit-session--compress-minimal (work-log)
  "Minimal compression - only count and last result."
  (let ((total-steps (length work-log))
        (last-result (when work-log (caar work-log))))
    (json-encode
     `((steps . ,total-steps)
       (last_result . ,(if last-result
                          (efrit-common-truncate-string
                           (format "%s" last-result)
                           efrit-session-max-result-length)
                        "none"))))))

(defun efrit-session--compress-smart (work-log)
  "Smart compression - recent entries with truncation."
  (let* ((total-steps (length work-log))
         (recent-entries (seq-take work-log 5))
         (compressed
          (mapcar (lambda (entry)
                   (let ((result (nth 0 entry))
                         (elisp (nth 1 entry))
                         (todos (nth 2 entry))
                         (tool-name (nth 3 entry)))
                     `((elisp . ,(efrit-session--compress-code elisp))
                       (result . ,(efrit-session--compress-result result))
                       ,@(when tool-name
                           `((tool . ,tool-name)))
                       ,@(when todos
                           `((todos . ,(length todos)))))))
                 recent-entries)))
    (json-encode
     `((total_steps . ,total-steps)
       (recent . ,compressed)))))

(defun efrit-session--compress-full (work-log)
  "Full compression - all entries with classification."
  (let ((compressed
         (mapcar (lambda (entry)
                  (let ((result (nth 0 entry))
                        (elisp (nth 1 entry)))
                    `((elisp . ,elisp)
                      (result . ,(efrit-session--compress-result result))
                      (type . ,(efrit-session--classify-code elisp)))))
                work-log)))
    (json-encode `((entries . ,compressed)))))

(defun efrit-session--compress-code (code)
  "Compress CODE string for context."
  (cond
   ((< (length code) 80) code)
   ((string-match "\\(buffer-substring[^[:space:]]*\\|insert\\|delete-region\\)" code)
    (concat "(" (match-string 1 code) " ...)"))
   (t (efrit-common-truncate-string code 97))))

(defun efrit-session--compress-result (result)
  "Compress RESULT for context usage."
  (let ((result-str (format "%s" result)))
    (cond
     ((or (null result) (string-empty-p result-str)) "nil")
     ((string-match "^#<buffer \\(.+\\)>$" result-str)
      (format "buffer:%s" (match-string 1 result-str)))
     ((> (length result-str) efrit-session-max-result-length)
      (concat (efrit-common-truncate-string
               result-str
               (- efrit-session-max-result-length 3))
              "..."))
     (t result-str))))

(defun efrit-session--classify-code (code)
  "Classify CODE into operation type."
  (cond
   ((string-match "eval-sexp\\|funcall\\|apply" code) 'evaluation)
   ((string-match "insert\\|delete\\|replace" code) 'text-modification)
   ((string-match "find-file\\|switch-to-buffer" code) 'navigation)
   ((string-match "shell-command\\|call-process" code) 'external-command)
   ((string-match "create-buffer\\|generate-new-buffer" code) 'buffer-creation)
   (t 'other)))

;;; Progress Tracking

(defun efrit-session-record-progress (session progress-type)
  "Record that progress was made in SESSION.
PROGRESS-TYPE can be: buffer-modification, todo-change, buffer-creation,
file-modification, execution-output."
  (when session
    (pcase progress-type
      ('buffer-modification
       (cl-incf (efrit-session-buffer-modifications session)))
      ('todo-change
       (cl-incf (efrit-session-todo-status-changes session)))
      ('buffer-creation
       (cl-incf (efrit-session-buffers-created session)))
      ('file-modification
       (cl-incf (efrit-session-files-modified session)))
      ('execution-output
       (cl-incf (efrit-session-execution-outputs session))))

    ;; Update progress tick
    (setf (efrit-session-last-progress-tick session)
          (+ (efrit-session-buffer-modifications session)
             (efrit-session-todo-status-changes session)
             (efrit-session-buffers-created session)
             (efrit-session-files-modified session)
             (efrit-session-execution-outputs session)))))

(defun efrit-session-progress-made-p (session)
  "Return t if material progress was made since last check in SESSION."
  (when session
    (let ((last-tick (or (efrit-session-last-progress-tick session) 0))
          (current-tick (+ (or (efrit-session-buffer-modifications session) 0)
                          (or (efrit-session-todo-status-changes session) 0)
                          (or (efrit-session-buffers-created session) 0)
                          (or (efrit-session-files-modified session) 0)
                          (or (efrit-session-execution-outputs session) 0))))
      (> current-tick last-tick))))

;;; Conversation History Management (for interactive sessions)

(defun efrit-session-add-message (session role content)
  "Add a message to SESSION's conversation history.
ROLE is either \\='user or \\='assistant.
CONTENT is the message text."
  (when session
    (let ((message (list role content (format-time-string "%Y-%m-%dT%H:%M:%S%z"))))
      (setf (efrit-session-conversation-history session)
            (append (efrit-session-conversation-history session) (list message)))
      (efrit-log 'debug "Added %s message to conversation" role))))

(defun efrit-session-get-conversation (session)
  "Get the conversation history from SESSION as a list.
Each item is (role content timestamp)."
  (when session
    (efrit-session-conversation-history session)))

(defun efrit-session-set-pending-question (session question &optional options)
  "Set QUESTION as pending user input in SESSION.
OPTIONS is an optional list of choices for the user."
  (when session
    (setf (efrit-session-pending-question session)
          (list question options (format-time-string "%Y-%m-%dT%H:%M:%S%z")))
    (setf (efrit-session-status session) 'waiting-for-user)
    (efrit-log 'info "Session %s waiting for user input: %s"
               (efrit-session-id session)
               (efrit-common-truncate-string question 60))))

(defun efrit-session-get-pending-question (session)
  "Get the pending question from SESSION.
Returns (question options timestamp) or nil."
  (when session
    (efrit-session-pending-question session)))

(defun efrit-session-respond-to-question (session response)
  "Record user RESPONSE to pending question in SESSION.
Clears the pending question and returns the session to active status."
  (when (and session (efrit-session-pending-question session))
    ;; Record the response
    (let ((response-entry (list response
                                (car (efrit-session-pending-question session))
                                (format-time-string "%Y-%m-%dT%H:%M:%S%z"))))
      (setf (efrit-session-user-responses session)
            (append (efrit-session-user-responses session) (list response-entry))))
    ;; Add to conversation history
    (efrit-session-add-message session 'user response)
    ;; Clear pending and resume
    (setf (efrit-session-pending-question session) nil)
    (setf (efrit-session-status session) 'active)
    (efrit-log 'info "Session %s received user response"
               (efrit-session-id session))
    response))

(defun efrit-session-waiting-for-user-p (session)
  "Return non-nil if SESSION is waiting for user input."
  (and session
       (eq (efrit-session-status session) 'waiting-for-user)))

(defun efrit-session-format-conversation-for-api (session)
  "Format SESSION's conversation history for Claude API.
Returns a vector of message objects suitable for the messages array."
  (when session
    (let ((history (efrit-session-conversation-history session)))
      (if (null history)
          ;; Just the original command
          (vector `((role . "user")
                   (content . ,(efrit-session-command session))))
        ;; Full conversation
        (vconcat
         (list `((role . "user")
                (content . ,(efrit-session-command session))))
         (mapcar (lambda (msg)
                  (let ((role (nth 0 msg))
                        (content (nth 1 msg)))
                    `((role . ,(if (eq role 'user) "user" "assistant"))
                      (content . ,content))))
                history))))))

;;; API Message Management (proper tool_use/tool_result format)
;; These functions manage the session's api-messages list which stores
;; the full conversation history in Claude API format, including:
;; - User messages with text content
;; - Assistant messages with tool_use content blocks
;; - User messages with tool_result content blocks

(defun efrit-session-add-assistant-response (session content-array)
  "Add Claude's assistant response to SESSION's api-messages.
CONTENT-ARRAY is the full content from Claude's response (with tool_use).
Must be called BEFORE executing tools to maintain proper message order."
  (when (and session content-array)
    (let ((api-messages (efrit-session-api-messages session)))
      (setf (efrit-session-api-messages session)
            (append api-messages
                    (list `((role . "assistant")
                            (content . ,content-array)))))
      (efrit-log 'debug "Added assistant response to api-messages (total: %d)"
                 (length (efrit-session-api-messages session))))))

(defun efrit-session-add-tool-results (session tool-results)
  "Add tool results to SESSION's api-messages.
TOOL-RESULTS should be a list of tool_result alists with type, tool_use_id,
and content. Added as a user message with vector of tool_result blocks."
  (when (and session tool-results (> (length tool-results) 0))
    (let ((api-messages (efrit-session-api-messages session)))
      (setf (efrit-session-api-messages session)
            (append api-messages
                    (list `((role . "user")
                            (content . ,(vconcat tool-results))))))
      (efrit-log 'debug "Added %d tool results to api-messages (total: %d)"
                 (length tool-results)
                 (length (efrit-session-api-messages session))))))

(defun efrit-session-get-api-messages-for-continuation (session)
  "Get SESSION's api-messages formatted for API continuation.
Returns a vector of messages suitable for the messages field."
  (when session
    (let ((messages (efrit-session-api-messages session)))
      (if messages
          (vconcat messages)
        ;; Fallback: just the original command
        (vector `((role . "user")
                  (content . ,(efrit-session-command session))))))))

(defun efrit-session-build-tool-result (tool-id result &optional is-error)
  "Build a tool_result content block for TOOL-ID with RESULT.
If IS-ERROR is non-nil, marks the result as an error.
Returns an alist:
  ((type . \"tool_result\")
   (tool_use_id . TOOL-ID)
   (content . RESULT)
   [is_error . t])"
  (let ((block `((type . "tool_result")
                 (tool_use_id . ,tool-id)
                 (content . ,(if (stringp result)
                                result
                              (format "%S" result))))))
    (when is-error
      (setq block (append block '((is_error . t)))))
    block))

(provide 'efrit-session-worklog)

;;; efrit-session-worklog.el ends here
