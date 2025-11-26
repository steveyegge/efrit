;;; efrit-budget.el --- Context budget management for Efrit -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.3.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; Token budget management for Efrit sessions.
;;
;; This module provides:
;; - Token estimation (character-based, ~3.5 chars/token)
;; - Budget allocation for tool calls
;; - Usage tracking by category (system, history, tool results)
;; - Low-budget warnings for Claude
;;
;; Following the Zero Client-Side Intelligence principle, this module
;; only TRACKS and REPORTS budget state - it does not make decisions
;; about what to keep or discard.  Claude receives budget information
;; and makes all content decisions.
;;
;; See docs/design/context-budget-management.md for design rationale.

;;; Code:

(require 'cl-lib)
(require 'json)

;;; Customization

(defgroup efrit-budget nil
  "Token budget management for Efrit sessions."
  :group 'efrit
  :prefix "efrit-budget-")

(defcustom efrit-budget-total-target 100000
  "Target total token budget for sessions.
This is set conservatively below Claude's context limits to ensure
reliable operation.  Claude 3.5 Sonnet has 200k context; we target
100k to leave room for response generation and safety margin.

Users with newer models or specific needs can increase this."
  :type 'integer
  :group 'efrit-budget)

(defcustom efrit-budget-per-tool-default 8000
  "Default token budget per tool result.
Tool results larger than this may be paginated or truncated.
Individual tools can specify different defaults."
  :type 'integer
  :group 'efrit-budget)

(defcustom efrit-budget-history-max 12000
  "Maximum tokens for compressed work history.
Work log entries are compressed and older entries evicted
when this limit is approached."
  :type 'integer
  :group 'efrit-budget)

(defcustom efrit-budget-warning-threshold 20000
  "Remaining tokens that trigger budget warning.
When remaining budget falls below this, a warning is included
in the system prompt for Claude to consider."
  :type 'integer
  :group 'efrit-budget)

(defcustom efrit-budget-system-prompt-estimate 8000
  "Estimated tokens for system prompt.
This includes tool schemas and instructions.
Used for initial budget allocation."
  :type 'integer
  :group 'efrit-budget)

(defcustom efrit-budget-user-message-estimate 10000
  "Estimated tokens for user message.
This includes the user's request and context.
Used for budget planning."
  :type 'integer
  :group 'efrit-budget)

(defcustom efrit-budget-response-buffer 30000
  "Tokens reserved for Claude's response generation.
This buffer ensures Claude has room to produce output."
  :type 'integer
  :group 'efrit-budget)

;;; Data Structure

(cl-defstruct efrit-budget
  "Token budget tracking for a session."
  (total-limit efrit-budget-total-target
               :documentation "Total target budget for session.")
  (system-used 0
               :documentation "Tokens used by system prompt.")
  (history-used 0
                :documentation "Tokens used by compressed work log.")
  (tool-results-used 0
                     :documentation "Tokens used by current tool results.")
  (user-message-used 0
                     :documentation "Tokens used by user message.")
  (warnings nil
            :documentation "List of budget warning messages."))

;;; Token Estimation

(defconst efrit-budget--chars-per-token 3.5
  "Estimated characters per token.
This is a rough average for English text and code.
JSON/code tends to be ~3 chars/token, prose ~4 chars/token.
3.5 is a reasonable middle ground for mixed content.")

(defun efrit-budget-estimate-tokens (text)
  "Estimate token count for TEXT.
Uses character-based estimation (~3.5 chars/token).
This is accurate to within ~20% for most content, which is
sufficient for budget management purposes."
  (if (or (null text) (string-empty-p text))
      0
    (ceiling (/ (float (length text)) efrit-budget--chars-per-token))))

(defun efrit-budget-estimate-tokens-json (obj)
  "Estimate tokens for OBJ when serialized to JSON.
OBJ can be any Elisp data structure that `json-encode' accepts."
  (efrit-budget-estimate-tokens (json-encode obj)))

;;; Budget Creation and Management

(defun efrit-budget-create (&optional total-limit)
  "Create a new budget for a session.
TOTAL-LIMIT overrides the default `efrit-budget-total-target'."
  (make-efrit-budget
   :total-limit (or total-limit efrit-budget-total-target)
   :system-used efrit-budget-system-prompt-estimate
   :history-used 0
   :tool-results-used 0
   :user-message-used 0
   :warnings nil))

(defun efrit-budget-remaining (budget)
  "Calculate remaining available token budget.
BUDGET is an efrit-budget struct."
  (when budget
    (- (efrit-budget-total-limit budget)
       (efrit-budget-system-used budget)
       (efrit-budget-history-used budget)
       (efrit-budget-tool-results-used budget)
       (efrit-budget-user-message-used budget)
       efrit-budget-response-buffer)))

(defun efrit-budget-available-for-tools (budget)
  "Calculate tokens available for tool results.
BUDGET is an efrit-budget struct.
This excludes reserved space for system, history, user message, and response."
  (when budget
    (let ((remaining (efrit-budget-remaining budget)))
      ;; Tool results can use the remaining budget
      ;; minus what's already been used by tool results
      (max 0 (+ remaining (efrit-budget-tool-results-used budget))))))

;;; Tool Budget Allocation

(defun efrit-budget-allocate-tool (budget &optional tool-name)
  "Get token allocation for a tool call.
BUDGET is an efrit-budget struct.
TOOL-NAME is optional and can be used for tool-specific allocations
in future versions (currently unused, reserved for per-tool budgets).

Returns the number of tokens the tool result should try to stay within.
Returns 0 if budget is exhausted."
  (ignore tool-name) ; Reserved for future per-tool budget allocations
  (when budget
    (let ((available (efrit-budget-available-for-tools budget))
          (default-allocation efrit-budget-per-tool-default))
      ;; Return the smaller of default allocation and available budget
      (min default-allocation (max 0 available)))))

(defun efrit-budget-suggest-limit (budget item-size &optional min-items)
  "Suggest a limit for paginated results based on BUDGET.
ITEM-SIZE is the estimated tokens per item.
MIN-ITEMS is the minimum number of items to return (default 10).

Returns a suggested item count that fits within the tool budget allocation."
  (when budget
    (let* ((allocation (efrit-budget-allocate-tool budget))
           (suggested (if (> item-size 0)
                         (floor (/ (float allocation) item-size))
                       100))
           (minimum (or min-items 10)))
      (max minimum suggested))))

;;; Usage Recording

(defun efrit-budget-record-usage (budget category tokens)
  "Record token usage in BUDGET.
CATEGORY is one of: system, history, tool-results, user-message.
TOKENS is the number of tokens used."
  (when budget
    (pcase category
      ('system
       (setf (efrit-budget-system-used budget) tokens))
      ('history
       (setf (efrit-budget-history-used budget) tokens))
      ('tool-results
       (setf (efrit-budget-tool-results-used budget)
             (+ (efrit-budget-tool-results-used budget) tokens)))
      ('user-message
       (setf (efrit-budget-user-message-used budget) tokens))
      (_
       (error "Unknown budget category: %s" category)))
    ;; Check if we should add a warning
    (efrit-budget--check-warning budget)))

(defun efrit-budget-record-tool-result (budget result)
  "Record a tool RESULT in BUDGET.
RESULT is the tool result string or object.
Updates tool-results-used and checks for warnings."
  (when budget
    (let ((tokens (if (stringp result)
                     (efrit-budget-estimate-tokens result)
                   (efrit-budget-estimate-tokens-json result))))
      (efrit-budget-record-usage budget 'tool-results tokens)
      tokens)))

(defun efrit-budget-reset-tool-results (budget)
  "Reset tool results usage for a new API turn.
Call this at the start of each Claude API call to track
per-turn tool usage separately."
  (when budget
    (setf (efrit-budget-tool-results-used budget) 0)))

;;; Warning System

(defun efrit-budget--check-warning (budget)
  "Check if BUDGET needs a warning and add it if so."
  (when budget
    (let ((remaining (efrit-budget-remaining budget)))
      (cond
       ;; Critical: less than 10k remaining
       ((< remaining 10000)
        (efrit-budget--add-warning budget 'critical
                                   "CRITICAL: Less than 10k tokens remaining"))
       ;; Warning: less than threshold remaining
       ((< remaining efrit-budget-warning-threshold)
        (efrit-budget--add-warning budget 'low
                                   (format "Budget low: ~%dk tokens remaining"
                                          (/ remaining 1000))))))))

(defun efrit-budget--add-warning (budget level message)
  "Add a warning to BUDGET at LEVEL with MESSAGE.
Only adds if this warning level isn't already present."
  (when budget
    (let ((existing (assq level (efrit-budget-warnings budget))))
      (unless existing
        (push (cons level message) (efrit-budget-warnings budget))))))

(defun efrit-budget-format-warning (budget)
  "Generate budget warning string for Claude if needed.
BUDGET is an efrit-budget struct.
Returns nil if no warning needed, otherwise a warning string
suitable for inclusion in system prompt."
  (when budget
    (let ((remaining (efrit-budget-remaining budget)))
      (cond
       ((< remaining 10000)
        (format "[BUDGET CRITICAL: ~%dk tokens remaining. Complete task soon or summarize progress.]"
                (/ remaining 1000)))
       ((< remaining efrit-budget-warning-threshold)
        (format "[Budget Alert: ~%dk tokens remaining. Consider completing or summarizing soon.]"
                (/ remaining 1000)))
       (t nil)))))

(defun efrit-budget-clear-warnings (budget)
  "Clear all warnings from BUDGET."
  (when budget
    (setf (efrit-budget-warnings budget) nil)))

;;; Budget Summary

(defun efrit-budget-summary (budget)
  "Generate a human-readable summary of BUDGET state.
Returns a string suitable for logging or display."
  (if (not budget)
      "No budget tracking"
    (let ((total (efrit-budget-total-limit budget))
          (system (efrit-budget-system-used budget))
          (history (efrit-budget-history-used budget))
          (tools (efrit-budget-tool-results-used budget))
          (user (efrit-budget-user-message-used budget))
          (remaining (efrit-budget-remaining budget)))
      (format "Budget: %dk/%dk remaining (sys:%dk hist:%dk tools:%dk user:%dk resp:%dk)"
              (/ remaining 1000)
              (/ total 1000)
              (/ system 1000)
              (/ history 1000)
              (/ tools 1000)
              (/ user 1000)
              (/ efrit-budget-response-buffer 1000)))))

(defun efrit-budget-for-claude (budget)
  "Format BUDGET state for inclusion in Claude context.
Returns an alist suitable for JSON encoding."
  (when budget
    `((total_budget . ,(efrit-budget-total-limit budget))
      (used . ((system . ,(efrit-budget-system-used budget))
               (history . ,(efrit-budget-history-used budget))
               (tool_results . ,(efrit-budget-tool-results-used budget))
               (user_message . ,(efrit-budget-user-message-used budget))
               (response_reserve . ,efrit-budget-response-buffer)))
      (remaining . ,(efrit-budget-remaining budget))
      (per_tool_budget . ,(efrit-budget-allocate-tool budget))
      ,@(when-let* ((warning (efrit-budget-format-warning budget)))
          `((warning . ,warning))))))

;;; History Budget Management

(defun efrit-budget-history-over-limit-p (budget history-tokens)
  "Check if HISTORY-TOKENS would exceed history budget in BUDGET."
  (when budget
    (> history-tokens efrit-budget-history-max)))

(defun efrit-budget-history-tokens-to-evict (budget history-tokens)
  "Calculate tokens to evict from history to fit HISTORY-TOKENS in BUDGET.
Returns 0 if no eviction needed."
  (if (not budget)
      0
    (let ((over (- history-tokens efrit-budget-history-max)))
      (max 0 over))))

;;; Tool-Specific Result Compression
;;
;; These functions compress tool results for work log history.
;; Target: ~500 chars max per compressed result.
;; Each function takes a tool result (alist) and returns a summary string.

(defconst efrit-budget-compress-max-chars 500
  "Maximum characters for compressed tool result summary.")

(defun efrit-budget--truncate-list (items max-items &optional format-fn)
  "Truncate ITEMS to MAX-ITEMS, applying FORMAT-FN to each.
Returns formatted string with \"...\" if truncated."
  (let* ((format-fn (or format-fn #'identity))
         (total (length items))
         (shown (seq-take items max-items))
         (formatted (mapcar format-fn shown)))
    (if (> total max-items)
        (concat (string-join formatted ", ") (format "... (+%d more)" (- total max-items)))
      (string-join formatted ", "))))

(defun efrit-budget-compress-project-files (result)
  "Compress project_files RESULT for work log history.
Output format: \"N files. Types: .el (M), .md (K). Key: path1/, path2/\""
  (let* ((data (alist-get 'data result))
         (files (append (alist-get 'files data) nil))
         (total (or (alist-get 'total_files data) (length files)))
         (type-counts (make-hash-table :test 'equal))
         (dir-counts (make-hash-table :test 'equal)))
    ;; Count file types
    (dolist (file files)
      (let* ((path (or (alist-get 'path_relative file) ""))
             (ext (file-name-extension path)))
        (when ext
          (puthash ext (1+ (gethash ext type-counts 0)) type-counts))
        ;; Count top-level directories
        (when (string-match "^\\([^/]+\\)/" path)
          (let ((dir (match-string 1 path)))
            (puthash dir (1+ (gethash dir dir-counts 0)) dir-counts)))))
    ;; Build summary
    (let* ((type-list (let (pairs)
                        (maphash (lambda (k v) (push (cons k v) pairs)) type-counts)
                        (seq-sort (lambda (a b) (> (cdr a) (cdr b))) pairs)))
           (dir-list (let (pairs)
                       (maphash (lambda (k v) (push (cons k v) pairs)) dir-counts)
                       (seq-sort (lambda (a b) (> (cdr a) (cdr b))) pairs)))
           (type-str (efrit-budget--truncate-list
                      (seq-take type-list 4)
                      4
                      (lambda (p) (format ".%s (%d)" (car p) (cdr p)))))
           (dir-str (efrit-budget--truncate-list
                     (seq-take dir-list 3)
                     3
                     (lambda (p) (format "%s/" (car p))))))
      (format "%d files. Types: %s. Key: %s"
              total
              (if (string-empty-p type-str) "(no extensions)" type-str)
              (if (string-empty-p dir-str) "(flat)" dir-str)))))

(defun efrit-budget-compress-search-content (result)
  "Compress search_content RESULT for work log history.
Output format: \"N matches in M files: file1 (C), file2 (D)...\""
  (let* ((data (alist-get 'data result))
         (matches (append (alist-get 'matches data) nil))
         (total-fetched (or (alist-get 'total_fetched data) (length matches)))
         ;; Count matches per file
         (file-counts (make-hash-table :test 'equal)))
    (dolist (match matches)
      (let ((file (or (alist-get 'file_relative match) "")))
        (puthash file (1+ (gethash file file-counts 0)) file-counts)))
    ;; Build file list sorted by match count
    (let* ((file-list (let (pairs)
                        (maphash (lambda (k v) (push (cons k v) pairs)) file-counts)
                        (seq-sort (lambda (a b) (> (cdr a) (cdr b))) pairs)))
           (file-str (efrit-budget--truncate-list
                      file-list
                      5
                      (lambda (p) (format "%s (%d)" (car p) (cdr p))))))
      (format "%d matches in %d files: %s"
              total-fetched
              (hash-table-count file-counts)
              (if (string-empty-p file-str) "(no matches)" file-str)))))

(defun efrit-budget-compress-read-file (result)
  "Compress read_file RESULT for work log history.
Output format: \"Read path/file.el (N lines, MKB)\""
  (let* ((data (alist-get 'data result))
         (path (or (alist-get 'path_relative data) (alist-get 'path data) "unknown"))
         (total-lines (alist-get 'total_lines data))
         (size (alist-get 'size data))
         (is-binary (alist-get 'is_binary data))
         (truncated (eq (alist-get 'truncated data) t)))
    (cond
     (is-binary
      (format "Read %s (binary, %s)"
              path
              (efrit-budget--format-size size)))
     (total-lines
      (format "Read %s (%d lines, %s)%s"
              path
              total-lines
              (efrit-budget--format-size size)
              (if truncated " [truncated]" "")))
     (t
      (format "Read %s (%s)"
              path
              (efrit-budget--format-size size))))))

(defun efrit-budget--format-size (bytes)
  "Format BYTES as human-readable size."
  (cond
   ((null bytes) "?B")
   ((< bytes 1024) (format "%dB" bytes))
   ((< bytes (* 1024 1024)) (format "%.1fKB" (/ bytes 1024.0)))
   (t (format "%.1fMB" (/ bytes (* 1024.0 1024.0))))))

(defun efrit-budget-compress-vcs-status (result)
  "Compress vcs_status RESULT for work log history.
Output format: \"branch (clean)\" or \"branch: N staged, M unstaged, K untracked\""
  (let* ((data (alist-get 'data result))
         (branch (or (alist-get 'current_branch data) "unknown"))
         (staged (length (alist-get 'staged_files data)))
         (unstaged (length (alist-get 'unstaged_files data)))
         (untracked (length (alist-get 'untracked_files data)))
         (is-clean (eq (alist-get 'is_clean data) t))
         (ahead (or (alist-get 'ahead data) 0))
         (behind (or (alist-get 'behind data) 0)))
    (if is-clean
        (format "%s (clean)%s"
                branch
                (cond
                 ((and (> ahead 0) (> behind 0))
                  (format " [ahead %d, behind %d]" ahead behind))
                 ((> ahead 0) (format " [ahead %d]" ahead))
                 ((> behind 0) (format " [behind %d]" behind))
                 (t "")))
      (let ((parts '()))
        (when (> staged 0) (push (format "%d staged" staged) parts))
        (when (> unstaged 0) (push (format "%d modified" unstaged) parts))
        (when (> untracked 0) (push (format "%d untracked" untracked) parts))
        (format "%s: %s" branch (string-join (nreverse parts) ", "))))))

(defun efrit-budget-compress-vcs-diff (result)
  "Compress vcs_diff RESULT for work log history.
Output format: \"N files, +M/-K lines. Modified: file1, file2...\""
  (let* ((data (alist-get 'data result))
         (summary (alist-get 'summary data))
         (files (append (alist-get 'files data) nil))
         (files-changed (or (alist-get 'files_changed summary) (length files)))
         (insertions (or (alist-get 'insertions summary) 0))
         (deletions (or (alist-get 'deletions summary) 0))
         (diff-type (or (alist-get 'diff_type data) "diff")))
    (if (zerop files-changed)
        (format "No changes (%s)" diff-type)
      (let ((file-names (mapcar (lambda (f) (alist-get 'path f)) files)))
        (format "%d files, +%d/-%d lines (%s). %s"
                files-changed
                insertions
                deletions
                diff-type
                (efrit-budget--truncate-list file-names 4))))))

(defun efrit-budget-compress-vcs-log (result)
  "Compress vcs_log RESULT for work log history.
Output format: \"N commits. Recent: abc1234 msg1, def5678 msg2...\""
  (let* ((data (alist-get 'data result))
         (commits (append (alist-get 'commits data) nil))
         (count (or (alist-get 'count data) (length commits))))
    (if (zerop count)
        "No commits found"
      (let ((commit-strs (mapcar
                          (lambda (c)
                            (let ((hash (or (alist-get 'hash c) "?"))
                                  (msg (or (alist-get 'message c) "")))
                              ;; Truncate message to ~30 chars
                              (when (> (length msg) 30)
                                (setq msg (concat (substring msg 0 27) "...")))
                              (format "%s %s" hash msg)))
                          commits)))
        (format "%d commits. Recent: %s"
                count
                (efrit-budget--truncate-list commit-strs 3))))))

;;; Compression Dispatch

(defun efrit-budget-compress-for-history (tool-name result)
  "Compress RESULT from TOOL-NAME for work log history.
TOOL-NAME is a string or symbol identifying the tool.
RESULT is the tool response alist.
Returns a compressed string summary (~500 chars max)."
  (let* ((tool (if (symbolp tool-name) (symbol-name tool-name) tool-name))
         (compressor (cond
                      ((string= tool "project_files") #'efrit-budget-compress-project-files)
                      ((string= tool "search_content") #'efrit-budget-compress-search-content)
                      ((string= tool "read_file") #'efrit-budget-compress-read-file)
                      ((string= tool "vcs_status") #'efrit-budget-compress-vcs-status)
                      ((string= tool "vcs_diff") #'efrit-budget-compress-vcs-diff)
                      ((string= tool "vcs_log") #'efrit-budget-compress-vcs-log)
                      (t nil))))
    (if compressor
        (condition-case err
            (let ((summary (funcall compressor result)))
              ;; Ensure we stay within max chars
              (if (> (length summary) efrit-budget-compress-max-chars)
                  (concat (substring summary 0 (- efrit-budget-compress-max-chars 3)) "...")
                summary))
          (error
           (format "[%s error: %s]" tool (error-message-string err))))
      ;; Fallback for unknown tools
      (format "[%s result]" tool))))

(provide 'efrit-budget)

;;; efrit-budget.el ends here
