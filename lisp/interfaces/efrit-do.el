;;; efrit-do.el --- Execute natural language commands in Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai
;; URL: https://github.com/stevey/efrit

;;; Commentary:
;; This file provides a simple command interface for executing natural language
;; commands in Emacs using the Efrit assistant.  It allows users to issue
;; commands like "open Dired to my downloads folder" and have them executed
;; immediately.

;;; Code:

;; Require extracted modules
(require 'efrit-do-prompt)
(require 'efrit-do-dispatch)

;; Declare external functions from efrit-chat (optional dependency)
(declare-function efrit--setup-buffer "efrit-chat")
(declare-function efrit--display-message "efrit-chat")
(declare-function efrit--insert-prompt "efrit-chat")
(declare-function efrit-api-build-headers "efrit-api")

;; Declare external functions from efrit-executor
(declare-function efrit-execute "efrit-executor")
(declare-function efrit-execute-async "efrit-executor")

;; Declare external functions from efrit-tool-confirm-action
(declare-function efrit-tool-confirm-action "efrit-tool-confirm-action")

;; Declare external functions from efrit-tool-checkpoint
(declare-function efrit-tool-checkpoint "efrit-tool-checkpoint")
(declare-function efrit-tool-restore-checkpoint "efrit-tool-checkpoint")
(declare-function efrit-tool-list-checkpoints "efrit-tool-checkpoint")
(declare-function efrit-tool-delete-checkpoint "efrit-tool-checkpoint")

;; Declare external functions from efrit-tool-show-diff-preview
(declare-function efrit-tool-show-diff-preview "efrit-tool-show-diff-preview")

;; Declare external functions from efrit-tool-web-search
(declare-function efrit-tool-web-search "efrit-tool-web-search")

;; Declare external functions from efrit-tool-fetch-url
(declare-function efrit-tool-fetch-url "efrit-tool-fetch-url")

;; Declare external functions from Phase 1/2 tools
(declare-function efrit-tool-project-files "efrit-tool-project-files")
(declare-function efrit-tool-search-content "efrit-tool-search-content")
(declare-function efrit-tool-read-file "efrit-tool-read-file")
(declare-function efrit-tool-file-info "efrit-tool-file-info")
(declare-function efrit-tool-vcs-status "efrit-tool-vcs-status")
(declare-function efrit-tool-vcs-diff "efrit-tool-vcs-diff")
(declare-function efrit-tool-vcs-log "efrit-tool-vcs-log")
(declare-function efrit-tool-vcs-blame "efrit-tool-vcs-blame")
(declare-function efrit-tool-elisp-docs "efrit-tool-elisp-docs")

;; Declare external functions from efrit-tool-utils for project root management
(declare-function efrit-tool--get-project-root "efrit-tool-utils")
(declare-function efrit-set-project-root "efrit-tool-utils")
(declare-function efrit-clear-project-root "efrit-tool-utils")
(declare-function efrit-tool--format-agent-instructions-for-prompt "efrit-tool-utils")
(defvar efrit-project-root)

;; Declare external functions from efrit-agent
(declare-function efrit-agent--get-buffer "efrit-agent-core")
(declare-function efrit-agent--show-buffer "efrit-agent-core")
(declare-function efrit-agent--begin-session "efrit-agent-core")
(declare-function efrit-agent-mode "efrit-agent")

(require 'efrit-tools)
(require 'efrit-config)
(require 'efrit-common)
(require 'efrit-session)
(require 'efrit-todo)
(require 'efrit-chat)
(require 'efrit-progress)
(require 'efrit-tool-utils)
(require 'efrit-do-circuit-breaker)
(require 'efrit-do-schema)
(require 'efrit-do-handlers)
(require 'efrit-do-async-loop)
(require 'efrit-do-queue)
(require 'cl-lib)
(require 'seq)
(require 'ring)
(require 'url)
(require 'json)

;;; Customization

(defgroup efrit-do nil
  "Natural language command execution for Efrit."
  :group 'efrit
  :prefix "efrit-do-")

(defcustom efrit-do-buffer-name "*efrit-do*"
  "Name of the buffer for command execution results."
  :type 'string
  :group 'efrit-do)

(defcustom efrit-do-show-results t
  "Whether to show command results in a buffer."
  :type 'boolean
  :group 'efrit-do)

(defcustom efrit-do-show-errors-only t
  "When non-nil, only show the *efrit-do* buffer for errors.
When nil, show buffer for all results (controlled by `efrit-do-show-results')."
  :type 'boolean
  :group 'efrit-do)

(defcustom efrit-do-auto-shrink-todo-buffers t
  "When non-nil, TODO display buffers automatically shrink to fit content.
This affects the TODO display buffer."
  :type 'boolean
  :group 'efrit-do)

(defcustom efrit-do-history-max 50
  "Maximum number of commands to keep in history."
  :type 'integer
  :group 'efrit-do)

(defcustom efrit-do-debug nil
  "Whether to show debug information during command execution."
  :type 'boolean
  :group 'efrit-do)

;; Circuit breaker customizations moved to efrit-do-circuit-breaker.el

(defcustom efrit-do-show-tool-execution t
  "Whether to show feedback when tools are executed.
When non-nil, displays messages like \\='Executing tool eval_sexp...\\=' during
command execution.  This provides visibility into what operations are
being performed."
  :type 'boolean
  :group 'efrit-do)

(defcustom efrit-do-max-buffer-lines 1000
  "Maximum number of lines to keep in the results buffer.
When the buffer exceeds this limit, older results are automatically
truncated to keep only the most recent results as specified by
`efrit-do-keep-results'.  Set to 0 to disable automatic truncation."
  :type 'integer
  :group 'efrit-do)

(defcustom efrit-do-keep-results 10
  "Number of recent command results to keep when truncating the buffer.
When the results buffer exceeds `efrit-do-max-buffer-lines', it is
truncated to keep this many recent results."
  :type 'integer
  :group 'efrit-do)

(defcustom efrit-do-use-agent-ui nil
  "When non-nil, show the efrit-agent buffer for efrit-do sessions.
This provides a REPL-like view of command execution alongside
the traditional progress buffer."
  :type 'boolean
  :group 'efrit-do)

;; Context configuration moved to efrit-context.el

;; efrit-do-max-retries and efrit-do-retry-on-errors are now aliases
;; to the shared config in efrit-config.el for backward compatibility.
(defvaralias 'efrit-do-max-retries 'efrit-max-retries
  "Alias to `efrit-max-retries' in efrit-config.el.")
(defvaralias 'efrit-do-retry-on-errors 'efrit-retry-on-errors
  "Alias to `efrit-retry-on-errors' in efrit-config.el.")

;; Use centralized model configuration from efrit-config
(defvar efrit-model)  ;; Forward declaration for efrit-chat's alias

(defcustom efrit-api-channel nil
  "API channel to use. Can be \\='ai-efrit or nil for default."
  :type '(choice (const :tag "Default" nil)
                 (const :tag "AI-Efrit" "ai-efrit"))
  :group 'efrit-do)

;; efrit-max-tokens is now aliased in efrit-config.el

;; efrit-api-url is defined in efrit-common.el (legacy, deprecated)

;;; Internal variables

(defvar efrit-do-history nil
  "History of executed commands.")

(defvar efrit-do--last-result nil
  "Result of the last executed command.")

;; TODO state is now managed by efrit-todo.el
;; Keep backward-compatible variable aliases
(defvaralias 'efrit-do--current-todos 'efrit-todo--current-todos)
(defvaralias 'efrit-do--todo-counter 'efrit-todo--counter)

(defvar efrit-do--force-complete nil
  "When t, forces session completion on next API response.")

;; Circuit breaker state and error loop detection state moved to efrit-do-circuit-breaker.el
;; Tool schemas (JSON definitions for Claude) moved to efrit-do-schema.el

;; Tool dispatch table and execution -> efrit-do-dispatch.el
;; Budget hints, tool schemas -> efrit-do-schema.el
;; Circuit breaker, error loop detection -> efrit-do-circuit-breaker.el

;;; Context system - delegates to efrit-context.el

;;; Context ring management
;; NOTE: efrit-do--session-protocol-instructions moved to efrit-do-prompt.el

(defun efrit-do--ensure-context-ring ()
  "Ensure the context system is initialized."
  (efrit-context-init))

(defun efrit-do--capture-context (command result)
  "Capture current context after executing COMMAND with RESULT."
  (efrit-do--ensure-context-ring)
  (let ((metadata (list :point (point)
                       :mark (mark)
                       :major-mode major-mode)))
    (efrit-context-ring-add command result metadata)))

(defun efrit-do--get-context-items (&optional n)
  "Get N most recent context items (default all)."
  (efrit-do--ensure-context-ring)
  (efrit-context-ring-get-recent n))

(defun efrit-do--context-to-string (item)
  "Convert context ITEM to string representation."
  (efrit-context-item-to-string item))

(defun efrit-do--clear-context ()
  "Clear the context ring."
  (efrit-context-ring-clear))

;;; TODO Management
;; The TODO item struct (efrit-todo-item) and state are defined in efrit-todo.el.
;; Backward-compatible aliases (efrit-do-todo-item-*) are provided there.

;; Delegate to efrit-todo.el functions
(defalias 'efrit-do--generate-todo-id 'efrit-todo--generate-id)
(defalias 'efrit-do--add-todo 'efrit-todo-add)
(defalias 'efrit-do--update-todo-status 'efrit-todo-update-status)
(defalias 'efrit-do--find-todo 'efrit-todo-find)
(defalias 'efrit-do--format-todos-for-display 'efrit-todo-format-for-display)
(defalias 'efrit-do--format-todos-for-prompt 'efrit-todo-format-for-prompt)

(defun efrit-do--clear-todos ()
  "Clear all current TODOs and reset circuit breaker for new session."
  (efrit-todo-clear)
  ;; Reset circuit breaker for new session
  (efrit-do--circuit-breaker-reset))

;;; Context persistence

(defun efrit-do--save-context ()
  "Save context ring to file."
  (efrit-context-ring-persist))

(defun efrit-do--load-context ()
  "Load context ring from file."
  (efrit-context-ring-restore))

;;; Helper functions for improved error handling
;; Note: efrit-do--validate-elisp moved to efrit-do-handlers.el

(defun efrit-do--build-error-context ()
  "Build rich contextual information for Claude when fixing errors.
Returns a string with current Emacs state, buffer info, and recent history."
  (let* ((current-buffer-name (buffer-name))
         (current-mode (symbol-name major-mode))
         (current-point (point))
         (buffer-size (buffer-size))
         (current-dir default-directory)
         (visible-buffers (mapcar #'buffer-name 
                                 (mapcar #'window-buffer (window-list))))
         (recent-context (efrit-do--get-context-items 2))
         (context-parts '()))
    
    ;; Current buffer information
    (push (format "CURRENT BUFFER: %s (mode: %s, point: %d/%d)" 
                  current-buffer-name current-mode current-point buffer-size)
          context-parts)
    
    ;; Current directory
    (push (format "CURRENT DIRECTORY: %s" current-dir) context-parts)
    
    ;; Visible buffers
    (push (format "VISIBLE BUFFERS: %s" 
                  (string-join visible-buffers ", "))
          context-parts)
    
    ;; Buffer content around point (if buffer has content)
    (when (> buffer-size 0)
      (let* ((start-pos (max (point-min) (- current-point 200)))
             (end-pos (min (point-max) (+ current-point 200)))
             (content-snippet (buffer-substring-no-properties start-pos end-pos))
             (lines (split-string content-snippet "\n" t)) ; Remove empty lines
             (truncated-lines (if (> (length lines) 10)
                                 (append (seq-take lines 4)
                                        '("...")
                                        (nthcdr (- (length lines) 4) lines))
                               lines)))
        (push (format "BUFFER CONTENT AROUND POINT:\n%s" 
                      (string-join truncated-lines "\n"))
              context-parts)))
    
    ;; Recent command history
    (when recent-context
      (push "RECENT COMMANDS:" context-parts)
      (dolist (item recent-context)
        (push (format "  %s -> %s" 
                      (efrit-context-item-command item)
                      (truncate-string-to-width 
                       (or (efrit-context-item-result item) "no result") 
                       60 nil nil t))
              context-parts)))
    
    ;; Window configuration info
    (push (format "WINDOW LAYOUT: %d windows" (length (window-list)))
          context-parts)
    
    ;; Join all context parts
    (string-join (reverse context-parts) "\n")))

(defun efrit-do--extract-error-info (result)
  "Extract error information from RESULT string.
Returns (error-p . error-msg) where error-p is t if errors found."
  (when (stringp result)
    (cond
     ;; Syntax errors
     ((string-match "\\[Syntax Error in \\([^:]+\\): \\(.+\\)\\]" result)
      (cons t (format "Syntax error in %s: %s" 
                      (match-string 1 result) 
                      (match-string 2 result))))
     ;; Runtime errors
     ((string-match "\\[Error executing \\([^:]+\\): \\(.+\\)\\]" result)
      (cons t (format "Runtime error in %s: %s" 
                      (match-string 1 result) 
                      (match-string 2 result))))
     ;; API errors
     ((string-match "API Error" result)
      (cons t result))
     ;; General errors - match at start of line to avoid false positives
     ((string-match "^Error:" result)
      (cons t result))
     ;; No error detected
     (t (cons nil nil)))))

(defun efrit-do--extract-executed-code (result)
  "Extract the executed code from RESULT string.
Returns the code string that was executed, or nil if not found."
  (when (stringp result)
    (cond
     ;; Extract from syntax error message
     ((string-match "\\[Syntax Error in \\([^:]+\\):" result)
      (match-string 1 result))
     ;; Extract from runtime error message
     ((string-match "\\[Error executing \\([^:]+\\):" result)
      (match-string 1 result))
     ;; Extract from successful execution message
     ((string-match "\\[Executed: \\([^]]+\\)\\]" result)
      (match-string 1 result))
     ;; No code found
     (t nil))))

(defun efrit-do--extract-response-text (response-buffer)
  "Extract response text from RESPONSE-BUFFER with proper cleanup.
Returns the response body as a string, or nil if extraction fails.
The RESPONSE-BUFFER is automatically killed after extraction."
  (unwind-protect
      (when (buffer-live-p response-buffer)
        (with-current-buffer response-buffer
          (goto-char (point-min))
          (when (search-forward-regexp "^$" nil t)
            (buffer-substring-no-properties (point) (point-max)))))
    (when (buffer-live-p response-buffer)
      (kill-buffer response-buffer))))

;;; Tool handlers moved to efrit-do-handlers.el
;;; Tool dispatch table and execution moved to efrit-do-dispatch.el

;;; Command execution
;; NOTE: efrit-do--command-examples, efrit-do--command-formatting-tools,
;; efrit-do--command-common-tasks, efrit-do--command-project-workflow,
;; efrit-do--classify-task-complexity, and efrit-do--command-system-prompt
;; have been moved to efrit-do-prompt.el

(defun efrit-do--format-result (command result)
  "Format COMMAND and RESULT for display."
  (with-temp-buffer
    (insert (format "Command: %s\n" command))
    (insert (make-string 60 ?-) "\n")
    (insert result)
    (buffer-string)))

(defun efrit-do--truncate-results-buffer ()
  "Truncate results buffer to keep only recent results within size limits.
Keeps the last `efrit-do-keep-results' command results when the buffer
exceeds `efrit-do-max-buffer-lines' lines."
  (when (and (> efrit-do-max-buffer-lines 0)
             (> (count-lines (point-min) (point-max)) efrit-do-max-buffer-lines))
    (save-excursion
      (goto-char (point-min))
      ;; Find boundaries of results by looking for "Command: " markers
      (let ((result-positions nil))
        ;; Collect positions of all result boundaries
        (while (re-search-forward "^Command: " nil t)
          (push (line-beginning-position) result-positions))
        (setq result-positions (nreverse result-positions))

        ;; Keep only the last N results
        (when (> (length result-positions) efrit-do-keep-results)
          (let* ((delete-up-to (nth (- (length result-positions)
                                      efrit-do-keep-results)
                                   result-positions))
                 (inhibit-read-only t))
            (delete-region (point-min) delete-up-to)
            (goto-char (point-min))
            (insert (format "[... %d older result%s truncated ...]\n\n"
                           (- (length result-positions) efrit-do-keep-results)
                           (if (> (- (length result-positions) efrit-do-keep-results) 1)
                               "s" "")))))))))

(defun efrit-do--display-result (command result &optional error-p)
  "Display COMMAND and RESULT in the results buffer.
If ERROR-P is non-nil, this indicates an error result.
When `efrit-do-show-errors-only' is non-nil, only show buffer for errors."
  (when error-p
    (require 'efrit-log)
    (efrit-log-error "[efrit-do] Command failed: %s\nResult: %s" command result))
  (when efrit-do-show-results
    ;; Always use standard results buffer - no smart detection
    (with-current-buffer (get-buffer-create efrit-do-buffer-name)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (unless (bobp)
          (insert "\n\n"))
        (insert (efrit-do--format-result command result))
        ;; Auto-truncate if buffer is too large
        (efrit-do--truncate-results-buffer))
      ;; Only display buffer if not in errors-only mode, or if this is an error
      (when (or (not efrit-do-show-errors-only) error-p)
        (display-buffer (current-buffer)
                        '(display-buffer-reuse-window
                          display-buffer-below-selected
                          (window-height . 10)))))))

(defun efrit-do--execute-command (command &optional retry-count error-msg previous-code)
  "Execute natural language COMMAND and return the result.
Uses improved error handling. If RETRY-COUNT is provided, this is a retry 
attempt with ERROR-MSG and PREVIOUS-CODE from the failed attempt."
  (condition-case api-err
      (let* ((api-key (efrit-common-get-api-key))
             (url-request-method "POST")
             (url-request-extra-headers (efrit-api-build-headers api-key))
             (system-prompt (efrit-do--command-system-prompt retry-count error-msg previous-code))
             (request-data
              `(("model" . ,efrit-default-model)
                ("max_tokens" . ,efrit-max-tokens)
                ("temperature" . 0.0)
                ("messages" . [(("role" . "user")
                               ("content" . ,command))])
                ("system" . ,system-prompt)
                ("tools" . ,(efrit-do--get-current-tools-schema))))
             (json-string (json-encode request-data))
             ;; Convert unicode characters to JSON escape sequences to prevent multibyte HTTP errors
              (escaped-json (efrit-common-escape-json-unicode json-string))
              (url-request-data (encode-coding-string escaped-json 'utf-8)))
        
        (if-let* ((api-url (or efrit-api-url (efrit-common-get-api-url)))
                  (response-buffer (with-local-quit
                                     (url-retrieve-synchronously api-url)))
                  (response-text (efrit-do--extract-response-text response-buffer)))
            (efrit-do--process-api-response response-text)
          (error "Failed to get response from API")))
    (error
     (format "API error: %s" (error-message-string api-err)))))

(defun efrit-do--process-api-response (response-text)
  "Process API RESPONSE-TEXT and execute any tools."
  (condition-case json-err
      (let* ((json-object-type 'hash-table)
             (json-array-type 'vector)
             (json-key-type 'string)
             (response (json-read-from-string response-text))
             (message-text ""))
        
        (efrit-log 'debug "Raw API Response: %s" response-text)
        (efrit-log 'debug "Parsed response: %S" response)
        
        ;; Check for API errors first
        (if-let* ((error-obj (gethash "error" response)))
            (let ((error-type (gethash "type" error-obj))
                  (error-message (gethash "message" error-obj)))
              (format "API Error (%s): %s" error-type error-message))
          
          ;; Process successful response
          (let ((content (gethash "content" response)))
            (when efrit-do-debug
              (message "API Response content: %S" content))
            
            (when content
              (dotimes (i (length content))
                (let* ((item (aref content i))
                       (type (gethash "type" item)))
                  (cond
                   ;; Handle text content
                   ((string= type "text")
                    (when-let* ((text (gethash "text" item)))
                      (setq message-text (concat message-text text))))
                   
                   ;; Handle tool use
                   ((string= type "tool_use")
                    (setq message-text 
                          (concat message-text 
                                  (efrit-do--execute-tool item))))))))
            
            (or message-text "Command executed"))))
    (error
     (format "JSON parsing error: %s" (error-message-string json-err)))))

(defun efrit-do--execute-with-retry (command)
  "Execute COMMAND with retry logic if enabled.
Returns the final result after all retry attempts."
  (let ((attempt 0)
        (max-attempts (if efrit-do-retry-on-errors (1+ efrit-do-max-retries) 1))
        result error-info last-error last-code final-result)
    
    (while (and (< attempt max-attempts) (not final-result))
      (setq attempt (1+ attempt))
      
      (when (> attempt 1)
        (message "Retry attempt %d/%d..." (1- attempt) efrit-do-max-retries))
      
      (condition-case err
          (progn
            (setq result (if (= attempt 1)
                            (efrit-do--execute-command command)
                          (efrit-do--execute-command command attempt last-error last-code)))
            
            (if result
                ;; Check if result contains errors
                (progn
                  (setq error-info (efrit-do--extract-error-info result))
                  (if (and (car error-info) efrit-do-retry-on-errors (< attempt max-attempts))
                      ;; Error detected and retries available
                      (progn
                        (setq last-error (cdr error-info))
                        (setq last-code (efrit-do--extract-executed-code result))
                        (efrit-log 'debug "Error detected: %s. Will retry..." last-error))
                    ;; Success or no more retries
                    (setq final-result result)))
              ;; No result - treat as error
              (let ((no-result-error "No result returned from command execution"))
                (if (and efrit-do-retry-on-errors (< attempt max-attempts))
                    (setq last-error no-result-error)
                  (setq final-result no-result-error)))))
        (error
         ;; Handle API or system errors
         (let ((error-msg (format "Error: %s" (error-message-string err))))
           (if (and efrit-do-retry-on-errors (< attempt max-attempts))
               (setq last-error error-msg)
             (setq final-result error-msg))))))
    
    (cons final-result attempt)))

(defun efrit-do--start-progress-timer (start-time _command)
  "Start a timer to show progress feedback during command execution.
START-TIME is when the command started.
_COMMAND is a short description (currently unused)."
  (run-at-time 1 1
               (lambda ()
                 (let ((elapsed (float-time (time-since start-time))))
                   (message "Efrit: Executing (%.1fs)..." elapsed)))))

(defun efrit-do--process-result (command result attempt)
  "Process the final RESULT from executing COMMAND after ATTEMPT attempts.
Returns RESULT for programmatic use."
  (if result
      (let* ((error-info (efrit-do--extract-error-info result))
             (is-error (car error-info)))
        (setq efrit-do--last-result result)
        (efrit-do--capture-context command result)
        (efrit-do--display-result command result is-error)

        (if is-error
            (progn
              (message "Command failed after %d attempt%s"
                      attempt (if (> attempt 1) "s" ""))
              (user-error "%s" (cdr error-info)))
          (message "Command executed successfully%s"
                  (if (> attempt 1)
                      (format " (after %d attempts)" attempt)
                      ""))
          ;; Return the result for programmatic use
          result))
    ;; Should never reach here, but handle just in case
    (let ((fallback-error "Failed to execute command"))
      (setq efrit-do--last-result fallback-error)
      (efrit-do--display-result command fallback-error t)
      (user-error "%s" fallback-error))))

;;;###autoload
(defun efrit-do-async-legacy (command)
  "Execute natural language COMMAND using legacy async executor.

DEPRECATED: This function is deprecated and should not be used.
Use `efrit-do' instead, which provides proper async execution with
progress buffer, queueing, and interruption support, or use
`efrit-do-sync' for synchronous blocking execution.

This function uses the older `efrit-execute-async' path which lacks
the full agentic loop infrastructure. It is kept for backward
compatibility only."
  (interactive
   (list (read-string "Command (legacy async): " nil 'efrit-do-history)))

  ;; Add to history
  (add-to-history 'efrit-do-history command efrit-do-history-max)

  ;; Track command execution
  (efrit-session-track-command command)

  ;; Reset circuit breaker for new command session
  (efrit-do--circuit-breaker-reset)

  ;; Execute asynchronously
  (require 'efrit-executor)
  (message "Executing asynchronously: %s..." command)
  (efrit-execute-async
   command
   (lambda (result)
     (let* ((error-info (efrit-do--extract-error-info result))
            (is-error (car error-info)))
       ;; Store result and update context
       (setq efrit-do--last-result result)
       (efrit-do--capture-context command result)
       (efrit-do--display-result command result is-error)

       ;; Show completion message
       (if is-error
           (message "Async command failed: %s" (cdr error-info))
         (message "Async command completed successfully"))))))

;;; Async Infrastructure Integration

(defun efrit-do--create-session-for-command (command)
  "Create and initialize an Efrit session for COMMAND.
Returns the session object, ready for async loop execution."
  (let* ((session-id (format "efrit-do-%s" (format-time-string "%Y%m%d%H%M%S")))
         (session (efrit-session-create session-id command)))
    ;; Set as active session
    (efrit-session-set-active session)
    ;; Reset circuit breaker for new command session
    (efrit-do--circuit-breaker-reset)
    ;; Reset TODO list for new session
    (setq efrit-do--current-todos nil)
    (setq efrit-do--todo-counter 0)
    session))

(defun efrit-do--start-async-session (command)
  "Internal helper to start an async efrit-do session for COMMAND.
Does not prompt or validate; assumes COMMAND is already validated.
Returns the session object for the caller to attach to their UI.

This function can be called from:
- `efrit-do' (minibuffer path)
- `efrit-agent-input-send' (REPL path)"
  ;; Add to history
  (add-to-history 'efrit-do-history command efrit-do-history-max)
  ;; Track command execution
  (efrit-session-track-command command)
  ;; Create session and start async loop
  (let ((session (efrit-do--create-session-for-command command)))
    (message "Efrit: starting async execution...")
    
    ;; Optionally show agent buffer UI
    (when efrit-do-use-agent-ui
      (require 'efrit-agent)
      (let ((buffer (efrit-agent--get-buffer)))
        (with-current-buffer buffer
          (unless (derived-mode-p 'efrit-agent-mode)
            (efrit-agent-mode))
          (efrit-agent--begin-session session command))
        (efrit-agent--show-buffer)))
    
    (efrit-do-async-loop session nil #'efrit-do--on-async-complete)
    session))

(defun efrit-do--on-async-complete (session stop-reason)
  "Handle completion of async efrit-do SESSION with STOP-REASON.
Displays final results and processes queued commands."
  (let* ((session-id (efrit-session-id session))
         (command (efrit-session-command session))
         ;; Build a result summary from session state
         (work-log (efrit-session-work-log session))
         (result (if work-log
                     (format "Completed %d steps" (length work-log))
                   "Completed")))
    ;; Store result
    (setq efrit-do--last-result result)
    (efrit-do--capture-context command result)
    
    ;; Display result based on stop reason
    (let ((is-error (member stop-reason '("api-error" "interrupted"))))
      (when is-error
        (setq result (format "Session ended: %s" stop-reason)))
      (efrit-do--display-result command result is-error))
    
    ;; Show completion message
    (message "Efrit: %s (session %s)"
             (pcase stop-reason
               ("end_turn" "completed")
               ("interrupted" "interrupted by user")
               ("api-error" "failed with API error")
               ("iteration-limit-exceeded" "hit iteration limit")
               (_ stop-reason))
             session-id)
    
    ;; Process next queued command if available (on normal completion)
    (when (string= stop-reason "end_turn")
      (when-let* ((next-cmd (efrit-do-queue-pop-command)))
        (message "Efrit: starting queued command...")
        ;; Use run-at-time to avoid deep recursion
        (run-at-time 0.1 nil #'efrit-do next-cmd)))))

;;;###autoload
(defun efrit-do (command)
  "Execute natural language COMMAND in Emacs asynchronously.

This is an internal API function. For user interaction, use M-x efrit
to open the REPL session buffer instead.

PROGRAMMATIC USE ONLY:
This function is designed for programmatic use from other elisp code.
For direct user execution, see `efrit-agent-input-send' in the agent
buffer, or use the main `efrit' REPL entry point.

FEATURES:
- Progress buffer with real-time updates (automatic display)
- Interruption support with \\[keyboard-quit] (C-g)
- Command queuing if a session is already running
- Non-blocking execution keeps Emacs responsive
- Session state tracking and restoration

Returns the session object for programmatic use."
  
  ;; Validate command is not empty or whitespace-only
  (when (string-empty-p (string-trim command))
    (user-error "Command cannot be empty"))
  
  ;; Check if there's already an active session
  (if (efrit-session-active)
      ;; Queue this command for later
      (progn
        (if (efrit-do-queue-add-command command)
            (message "Efrit: command queued (position %d)"
                     (efrit-do-queue-size))
          (message "Efrit: queue full, command not added"))
        nil)
    
    ;; No active session - use the helper to start execution
    (efrit-do--start-async-session command)))

;;;###autoload
(defun efrit-do-sync (command)
  "Execute natural language COMMAND in Emacs synchronously (blocking).

This is a synchronous/blocking execution path designed primarily for:
- Shell scripts and automation where blocking is required
- Scripting contexts that cannot handle asynchronous execution
- One-off commands where you need to wait for completion

For interactive use, prefer M-x efrit (the REPL agent buffer) which
provides a better interactive experience with persistent context.

The command is sent to Claude, which translates it into Elisp
and executes it immediately. Results are displayed in a dedicated
buffer if `efrit-do-show-results' is non-nil.

Progress feedback shows elapsed time and tool executions.
Use \\[keyboard-quit] (C-g) to cancel execution during API calls.

Returns the result string for programmatic use."
  (interactive
   (list (read-string "Command (sync): " nil 'efrit-do-history)))

  ;; Validate command is not empty or whitespace-only
  (when (string-empty-p (string-trim command))
    (user-error "Command cannot be empty"))

  ;; Add to history
  (add-to-history 'efrit-do-history command efrit-do-history-max)

  ;; Track command execution
  (efrit-session-track-command command)

  ;; Reset circuit breaker for new command session
  (efrit-do--circuit-breaker-reset)

  ;; Reset TODO list for new session
  (setq efrit-do--current-todos nil)
  (setq efrit-do--todo-counter 0)

  ;; Use efrit-execute which has proper agentic loop with continuation
  (require 'efrit-executor)
  (let* ((start-time (current-time))
         (progress-timer (efrit-do--start-progress-timer start-time command))
         result)
    (unwind-protect
        (progn
          (message "Executing: %s..." command)
          (setq result (efrit-execute command))
          ;; Process and display result
          (when result
            (let* ((error-info (efrit-do--extract-error-info result))
                   (is-error (car error-info)))
              (efrit-do--capture-context command result)
              (efrit-do--display-result command result is-error)
              (setq efrit-do--last-result result)))
          result)
      ;; Always cancel the timer when done
      (when progress-timer
        (cancel-timer progress-timer)))))

;;;###autoload
(defun efrit-do-silently (command)
  "Execute COMMAND asynchronously in background without progress buffer.

Execute a command in the background without displaying the progress buffer.
Progress is visible only in:
- Modeline status indicator
- Minibuffer messages

DEPRECATED: For most interactive use, prefer M-x efrit (the REPL).

This function is useful for:
- Background tasks that shouldn't interrupt your workflow
- When called from other elisp code
- Legacy automation scripts

Show progress buffer manually with \\[efrit-do-show-progress].
Use \\[keyboard-quit] (C-g) to interrupt execution.

If a command is running, this command is queued for later execution.
Use `efrit-do-show-queue' to view queued commands.

Returns the session object for programmatic use."
  (interactive
   (list (read-string "Command (silent): " nil 'efrit-do-history)))
  
  ;; Check if there's already an active session
  (if (efrit-session-active)
      ;; Queue this command for later
      (progn
        (if (efrit-do-queue-add-command command)
            (message "Efrit: command queued (position %d)"
                     (efrit-do-queue-size))
          (message "Efrit: queue full, command not added"))
        nil)
    
    ;; No active session - start async execution
    ;; Add to history
    (add-to-history 'efrit-do-history command efrit-do-history-max)
    
    ;; Track command execution
    (efrit-session-track-command command)
    
    ;; Create session and start async loop WITHOUT showing progress buffer
    (let ((session (efrit-do--create-session-for-command command))
          (original-show efrit-do-async-show-progress-buffer))
      (unwind-protect
          (progn
            (setq efrit-do-async-show-progress-buffer nil)
            (message "Efrit: starting async execution (background)...")
            (efrit-do-async-loop session nil #'efrit-do--on-async-complete))
        (setq efrit-do-async-show-progress-buffer original-show))
      session)))

;;;###autoload
(defun efrit-do-show-progress (&optional session-id)
  "Show the progress buffer for SESSION-ID or the current session.
If called interactively without SESSION-ID, uses the active session.
If SESSION-ID is provided, shows the progress buffer for that session."
  (interactive)
  (let* ((session-id (or session-id
                        (when (efrit-session-active)
                          (efrit-session-id (efrit-session-active)))))
         (buffer (when session-id
                   (efrit-progress-get-buffer session-id))))
    (cond
     (buffer
      (display-buffer buffer)
      (message "Showing progress buffer for session %s" session-id))
     (session-id
      (message "Progress buffer not found for session %s" session-id))
     (t
      (message "No active session. Use M-x efrit-do to start a command.")))))

;;;###autoload
(defun efrit-do-show-queue ()
  "Show the current command queue.
Displays queued commands with their positions and status."
  (interactive)
  (efrit-session-show-queue))

;;;###autoload
(defun efrit-do-repeat ()
  "Repeat the last efrit-do command."
  (interactive)
  (if efrit-do-history
      (efrit-do (car efrit-do-history))
    (message "No previous command to repeat")))

;;;###autoload
(defun efrit-do-clear-results ()
  "Clear the efrit-do results buffer."
  (interactive)
  (if-let* ((buffer (get-buffer efrit-do-buffer-name)))
      (progn
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (erase-buffer)))
        (message "Results buffer cleared"))
    (message "No results buffer to clear")))

;;;###autoload
(defun efrit-do-truncate-old-results ()
  "Truncate old results, keeping only recent ones.
Keeps the last `efrit-do-keep-results' command results in the buffer."
  (interactive)
  (if-let* ((buffer (get-buffer efrit-do-buffer-name)))
      (with-current-buffer buffer
        (let ((before-lines (count-lines (point-min) (point-max))))
          (efrit-do--truncate-results-buffer)
          (let ((after-lines (count-lines (point-min) (point-max))))
            (if (< after-lines before-lines)
                (message "Truncated results buffer (%d lines -> %d lines)"
                        before-lines after-lines)
              (message "Results buffer already within limits")))))
    (message "No results buffer to truncate")))

;;; Context system user commands

;;;###autoload
(defun efrit-do-show-context ()
  "Show recent context items."
  (interactive)
  (let ((items (efrit-do--get-context-items)))
    (if items
        (with-output-to-temp-buffer "*efrit-do-context*"
          (princ "Recent efrit-do context:\n\n")
          (dolist (item items)
            (princ (efrit-do--context-to-string item))
            (princ "\n")))
      (message "No context items found"))))

;;;###autoload
(defun efrit-do-clear-context ()
  "Clear the context ring."
  (interactive)
  (efrit-do--clear-context)
  (message "Context cleared"))

;;;###autoload
(defun efrit-do-clear-history ()
  "Clear efrit-do command history and context."
  (interactive)
  (setq efrit-do-history nil)
  (efrit-do--clear-context)
  (setq efrit-do--last-result nil)
  (message "History and context cleared"))

;;;###autoload
(defun efrit-do-show-todos ()
  "Show current TODO items in a shrink-to-fit buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*efrit-do-todos*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Current efrit-do TODOs\n")
        (insert "=====================\n\n")
        
        (if (null efrit-do--current-todos)
            (insert "No current TODOs.\n")
          ;; Show statistics
          (let ((total (length efrit-do--current-todos))
                (completed (seq-count (lambda (todo)
                                       (eq (efrit-do-todo-item-status todo) 'completed))
                                     efrit-do--current-todos))
                (in-progress (seq-count (lambda (todo)
                                         (eq (efrit-do-todo-item-status todo) 'in-progress))
                                       efrit-do--current-todos)))
            (insert (format "Total: %d | Completed: %d | In Progress: %d | Pending: %d\n\n"
                           total completed in-progress (- total completed in-progress))))
          
          ;; Show TODO list
          (insert (efrit-do--format-todos-for-display)))
        
        (insert "\n\nCommands:\n")
        (insert "  M-x efrit-do-clear-todos - Clear all TODOs\n")
        (insert "  M-x efrit-progress-show  - Show progress buffer\n")
        
        (goto-char (point-min))
        (special-mode)))
    
    ;; Display buffer with optional shrink-to-fit
    (let ((window (display-buffer buffer
                                 (if efrit-do-auto-shrink-todo-buffers
                                     '((display-buffer-reuse-window
                                        display-buffer-below-selected)
                                       (window-height . fit-window-to-buffer)
                                       (window-parameters . ((no-delete-other-windows . t))))
                                   '(display-buffer-reuse-window
                                     display-buffer-below-selected)))))
      (when (and window efrit-do-auto-shrink-todo-buffers)
        (fit-window-to-buffer window nil nil 15 nil)))))

;;;###autoload
(defun efrit-do-clear-todos ()
  "Clear all TODO items."
  (interactive)
  (efrit-do--clear-todos)
  (message "All TODOs cleared"))

;;;###autoload
(defun efrit-do-clear-all ()
  "Clear all efrit-do state.
This includes: history, context, results buffer, TODOs, and conversations."
  (interactive)
  (setq efrit-do-history nil)
  (efrit-do--clear-context)
  (efrit-do--clear-todos)
  (setq efrit-do--last-result nil)
  
  ;; Clear results buffer if it exists
  (when-let* ((buffer (get-buffer efrit-do-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer))))

  (message "All efrit-do state cleared"))

;;;###autoload
(defun efrit-do-reset ()
  "Interactive reset with options for different levels of clearing."
  (interactive)
  (let ((choice (read-char-choice 
                 "Reset efrit-do: (h)istory, (c)ontext, (r)esults, (t)odos, (a)ll state, or (q)uit? "
                 '(?h ?c ?r ?t ?a ?q))))
    (cond
     ((eq choice ?h) (setq efrit-do-history nil)
                     (setq efrit-do--last-result nil)
                     (message "Command history cleared"))
     ((eq choice ?c) (efrit-do-clear-context))
     ((eq choice ?r) (efrit-do-clear-results))
     ((eq choice ?t) (efrit-do-clear-todos))
     ((eq choice ?a) (efrit-do-clear-all))
     ((eq choice ?q) (message "Reset cancelled")))))

;;;###autoload
(defun efrit-do-to-chat (&optional n)
  "Convert recent efrit-do context to efrit-chat session.
Include last N commands (default 5)."
  (interactive "P")
  (require 'efrit-chat)
  (let* ((count (or n 5))
         (items (efrit-do--get-context-items count))
         (buffer (efrit--setup-buffer)))
    
    (if (not items)
        (message "No efrit-do context to convert")
      
      (with-current-buffer buffer
        (setq buffer-read-only nil)
        (let ((inhibit-read-only t))
          ;; Clear existing content and history
          (erase-buffer)
          (setq-local efrit--message-history nil)
          
          ;; Add context summary
          (efrit--display-message 
           (format "Chat session from %d recent efrit-do commands:" (length items))
           'system)
          
          ;; Convert each context item to conversation
          (dolist (item items) ; Show oldest first (items already in oldest-first order)
            (let ((command (efrit-context-item-command item))
                  (result (efrit-context-item-result item))
                  (timestamp (efrit-context-item-timestamp item)))
              
              ;; Display user command
              (efrit--display-message 
               (format "[%s] %s" 
                       (format-time-string "%H:%M:%S" timestamp)
                       command)
               'user)
              
              ;; Display result (truncated if too long)
              (efrit--display-message 
               (truncate-string-to-width result 500 nil nil t)
               'assistant)))
          
          ;; Build history in correct order for efrit-chat (newest first)
          ;; Process items in reverse order so newest ends up first
          (dolist (item items)
            (let ((command (efrit-context-item-command item))
                  (result (efrit-context-item-result item)))
              (push `((role . "assistant") (content . ,result)) efrit--message-history)
              (push `((role . "user") (content . ,command)) efrit--message-history)))
          
          ;; Insert prompt for new input
          (efrit--insert-prompt)))
      
      ;; Switch to the chat buffer
      (switch-to-buffer buffer)
      (message "Converted %d efrit-do commands to chat session" (length items)))))

;;; Initialization

(defun efrit-do--initialize ()
  "Initialize the efrit-do context system."
  (efrit-do--load-context)
  (add-hook 'kill-emacs-hook #'efrit-do--save-context))

(defun efrit-do--uninitialize ()
  "Uninitialize the efrit-do context system."
  (efrit-do--save-context)
  (remove-hook 'kill-emacs-hook #'efrit-do--save-context))

(add-hook 'after-init-hook #'efrit-do--initialize)

(provide 'efrit-do)

;;; efrit-do.el ends here
