;;; efrit-executor.el --- Command execution for Efrit -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.3.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; Unified command execution interface for Efrit.
;; This module handles:
;; - Synchronous execution (blocking)
;; - Asynchronous execution (with callbacks)
;; - API communication with Claude
;; - Response processing and tool execution
;; - Session continuation management
;;
;; Following the Zero Client-Side Intelligence principle, this module
;; does NOT decide what to execute - it just executes what Claude says.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'url)
(require 'efrit-log)
(require 'efrit-common)
(require 'efrit-session)
(require 'efrit-progress)
(declare-function efrit-do--command-system-prompt "efrit-do")
(declare-function efrit-do--execute-tool "efrit-do")
(declare-function efrit-do--get-current-tools-schema "efrit-do")
(defvar efrit-do--tools-schema)
(defvar efrit-default-model)

;;; Customization

(defgroup efrit-executor nil
  "Command execution for Efrit."
  :group 'efrit
  :prefix "efrit-executor-")

(defcustom efrit-executor-show-progress t
  "Whether to show progress in mode line."
  :type 'boolean
  :group 'efrit-executor)

(defcustom efrit-executor-max-continuations 50
  "Maximum API calls per session before emergency stop.
This is a safety limit to prevent runaway sessions.
Most tasks complete within 20 API calls; complex exploration may need more."
  :type 'integer
  :group 'efrit-executor)

(defcustom efrit-executor-max-tool-calls 100
  "Maximum tool calls per session (safety limit)."
  :type 'integer
  :group 'efrit-executor)

(defcustom efrit-executor-session-timeout 300
  "Maximum seconds for a session before timeout.
This prevents runaway sessions from blocking indefinitely.
Default is 5 minutes (300 seconds)."
  :type 'integer
  :group 'efrit-executor)

;;; Progress Display

(defvar efrit-executor-mode-line-string nil
  "Mode line indicator for active Efrit execution.")

(unless (member '(efrit-executor-mode-line-string
                  (" " efrit-executor-mode-line-string " "))
                mode-line-misc-info)
  (add-to-list 'mode-line-misc-info
               '(efrit-executor-mode-line-string
                 (" " efrit-executor-mode-line-string " "))))

(defun efrit-executor--update-mode-line (message)
  "Update mode line with MESSAGE."
  (when efrit-executor-show-progress
    (when-let* ((session (efrit-session-active)))
      (let* ((elapsed (float-time (time-since (efrit-session-start-time session))))
             (tool-count (efrit-progress-tool-call-count))
             (current-tool (efrit-progress-current-tool))
             (tool-info (if current-tool
                            (format " [%s]" current-tool)
                          "")))
        (setq efrit-executor-mode-line-string
              (format "[Efrit: %s%s #%d (%.1fs)]"
                      message tool-info tool-count elapsed))
        (force-mode-line-update t)))))

(defun efrit-executor--clear-mode-line ()
  "Clear mode line indicators."
  (setq efrit-executor-mode-line-string nil)
  (force-mode-line-update t))

;;; API Communication

(defun efrit-executor--api-request (request-data callback)
  "Send REQUEST-DATA to Claude API asynchronously.
Calls CALLBACK with parsed response or error information."
  (efrit-log 'debug "API request starting")
  (condition-case err
      (let* ((api-key (efrit-common-get-api-key))
             (json-string (json-encode request-data))
             (escaped-json (efrit-common-escape-json-unicode json-string))
             (url-request-data (encode-coding-string escaped-json 'utf-8))
             (url-request-method "POST")
             (url-request-extra-headers (efrit-common-build-headers api-key))
             (start-time (float-time)))

        (url-retrieve
         (efrit-common-get-api-url)
         (lambda (status)
           (let ((elapsed (- (float-time) start-time)))
             (efrit-log 'info "API call completed in %.2fs" elapsed)
             (efrit-executor--handle-url-response status callback)))
         nil t))
    (error
     (efrit-log 'error "API request setup failed: %s" (error-message-string err))
     (efrit-executor--handle-error err callback))))

(defun efrit-executor--handle-url-response (status callback)
  "Handle url-retrieve STATUS and call CALLBACK with parsed response."
  (unwind-protect
      (condition-case err
          (progn
            (when (plist-get status :error)
              (error "HTTP error: %s" (plist-get status :error)))

            (goto-char (point-min))
            (when (search-forward-regexp "^$" nil t)
              (let ((response-text (buffer-substring-no-properties (point) (point-max))))
                (if (string-empty-p (string-trim response-text))
                    (funcall callback nil)
                  (let* ((json-object-type 'hash-table)
                         (json-array-type 'vector)
                         (json-key-type 'string)
                         (parsed-response (json-read-from-string response-text)))
                    (funcall callback parsed-response))))))
        (error
         (efrit-log 'error "Response handling failed: %s" (error-message-string err))
         (funcall callback nil)))
    (when (buffer-live-p (current-buffer))
      (kill-buffer (current-buffer)))))

(defun efrit-executor--handle-error (error &optional callback)
  "Handle ERROR during execution.
Optionally calls CALLBACK with error message."
  (let ((message (if (stringp error)
                     error
                   (error-message-string error))))
    (efrit-executor--update-mode-line "Error!")
    (message "Efrit error: %s" message)

    (efrit-progress-show-message message 'error)

    (when-let* ((session (efrit-session-active)))
      (setf (efrit-session-last-error session) message)
      (efrit-progress-end-session (efrit-session-id session) nil))

    (efrit-session-set-active nil)
    (efrit-executor--clear-mode-line)

    (when callback
      (funcall callback (format "Error: %s" message)))

    ;; Process next queued command if any
    (efrit-executor--process-queue)))

;;; Response Processing

(defun efrit-executor--handle-response (response callback)
  "Handle RESPONSE from Claude API and execute tools.
Calls CALLBACK with the final result string.

CRITICAL: This function maintains proper Claude API message format by:
1. Storing Claude's full response (with tool_use blocks) BEFORE executing tools
2. Collecting tool_result blocks during execution
3. Adding tool_results to session's api-messages for proper continuation"
  (efrit-log 'debug "Handling Claude API response")
  (condition-case err
      (if (not response)
          (efrit-executor--handle-error "No response from API" callback)

        ;; Check for API errors
        (if-let* ((error-obj (gethash "error" response)))
            (let* ((error-type (gethash "type" error-obj))
                   (error-message (gethash "message" error-obj))
                   (error-str (format "API Error (%s): %s" error-type error-message)))
              (efrit-executor--handle-error error-str callback))

          ;; Process successful response
          (let ((content (gethash "content" response))
                (stop-reason (gethash "stop_reason" response))
                (result-text "")
                (session-complete-p nil)
                (completion-message nil)
                (tool-results-for-work-log '())  ; For legacy work log
                (tool-result-blocks '())  ; For proper API format (tool_result blocks)
                (session (efrit-session-active)))

            ;; Check stop_reason for session completion
            ;; "end_turn" means Claude is done (no more tool calls)
            ;; "tool_use" means Claude wants to use tools
            (when (string= stop-reason "end_turn")
              (efrit-log 'info "stop_reason=end_turn, session will complete")
              (setq session-complete-p t))

            (when content
              ;; CRITICAL: Store Claude's full response BEFORE executing tools
              ;; This ensures proper message ordering: [user, assistant-with-tool_use, user-with-tool_result]
              (when session
                (efrit-session-add-assistant-response session content))

              ;; Process each content item
              (dotimes (i (length content))
                (let* ((item (aref content i))
                       (type (gethash "type" item)))
                  (cond
                   ;; Handle text content
                   ((string= type "text")
                    (when-let* ((text (gethash "text" item)))
                      (setq result-text (concat result-text text))
                      (efrit-progress-show-message text 'claude)))

                   ;; Handle tool use
                   ((string= type "tool_use")
                    (let* ((tool-id (gethash "id" item))
                           (tool-input (gethash "input" item))
                           (tool-result nil)
                           (is-error nil))

                      ;; Execute the tool and capture result
                      (condition-case err
                          (setq tool-result (efrit-executor--execute-tool item session))
                        (error
                         (setq tool-result (format "Error: %s" (error-message-string err)))
                         (setq is-error t)))

                      (setq result-text (concat result-text tool-result))

                      ;; Build tool_result block for API (proper format)
                      (push (efrit-session-build-tool-result tool-id tool-result is-error)
                            tool-result-blocks)

                      ;; Check for session completion
                      (when (string-match "\\[SESSION-COMPLETE: \\(.+\\)\\]" tool-result)
                        (setq session-complete-p t)
                        (setq completion-message (match-string 1 tool-result)))

                      ;; Check for waiting-for-user (paused session)
                      (when (string-match "\\[WAITING-FOR-USER\\]" tool-result)
                        ;; Session is paused waiting for user input
                        ;; The session state has already been set by the handler
                        (setq session-complete-p t)  ; Stop continuation
                        (setq completion-message nil))  ; Don't mark as complete

                      ;; Check if circuit breaker has tripped - stop session immediately
                      (when (and (boundp 'efrit-do--circuit-breaker-tripped)
                                 efrit-do--circuit-breaker-tripped)
                        (efrit-log 'warn "Circuit breaker tripped - stopping session: %s"
                                   efrit-do--circuit-breaker-tripped)
                        (setq session-complete-p t)
                        (setq completion-message
                              (format "[CIRCUIT BREAKER] Session terminated: %s"
                                      efrit-do--circuit-breaker-tripped)))

                      ;; Track tool execution for legacy work log
                      (when session
                        (push (list tool-result
                                  (if (hash-table-p tool-input)
                                      (json-encode tool-input)
                                    (format "%S" tool-input)))
                              tool-results-for-work-log))))))))

            ;; Add tool results to session's api-messages (for proper continuation)
            (when (and session tool-result-blocks)
              (efrit-session-add-tool-results session (nreverse tool-result-blocks)))

            ;; Handle session continuation or completion
            (cond
             ;; Session waiting for user input - don't complete, just pause
             ((and session (efrit-session-waiting-for-user-p session))
              (efrit-executor--update-mode-line "Waiting for user...")
              (efrit-progress-show-message "Session paused - awaiting user input" 'info)
              ;; Don't clear active session - it's paused, not done
              (when callback
                (funcall callback result-text)))

             ;; Session complete or no active session
             ((or (not session)
                  session-complete-p
                  (and (boundp 'efrit-do--force-complete)
                       efrit-do--force-complete))
              ;; Combine result-text and completion-message for full detail
              ;; result-text contains tool execution details, don't lose them
              (let ((final-result (cond
                                   ;; Both present: combine them
                                   ((and result-text (not (string-empty-p result-text))
                                         completion-message)
                                    (concat result-text "\n\n" completion-message))
                                   ;; Only result-text
                                   ((and result-text (not (string-empty-p result-text)))
                                    result-text)
                                   ;; Only completion-message
                                   (completion-message completion-message)
                                   ;; Force complete fallback
                                   ((and (boundp 'efrit-do--force-complete)
                                         efrit-do--force-complete)
                                    "Session auto-completed after successful code execution")
                                   ;; Default
                                   (t "Session completed"))))
                (when session
                  (efrit-executor--complete-session session final-result))
                (when callback
                  (funcall callback final-result))))

             ;; Session needs to continue
             (t
              ;; Update session work log (legacy - for compression/history)
              (dolist (result (nreverse tool-results-for-work-log))
                (efrit-session-add-work session (car result) (cadr result)))
              ;; Continue the session with proper message format
              (efrit-executor--continue-session session callback))))))
    (error
     (efrit-executor--handle-error
      (format "Response handling error: %s" (error-message-string err))
      callback))))

;;; Tool Execution

(defun efrit-executor--execute-tool (tool-item session)
  "Execute a tool from TOOL-ITEM with SESSION context.
Returns the tool result string."
  (let ((tool-name (gethash "name" tool-item))
        (input-data (gethash "input" tool-item)))

    ;; Safety limit check
    (when (and session
               (> (length (efrit-session-work-log session))
                  efrit-executor-max-tool-calls))
      (error "SESSION SAFETY LIMIT: Over %d tool calls - session terminated"
             efrit-executor-max-tool-calls))

    ;; Show progress
    (efrit-progress-show-tool-start tool-name input-data)

    ;; Execute via efrit-do's tool handler
    (require 'efrit-do)
    (let ((result
           (condition-case err
               (efrit-do--execute-tool tool-item)
             (error
              (let ((error-msg (error-message-string err)))
                (efrit-progress-show-tool-result tool-name error-msg nil)
                (signal (car err) (cdr err)))))))

      ;; Show result
      (efrit-progress-show-tool-result tool-name result t)

      ;; Track in session
      (when session
        (efrit-session-track-tool session tool-name input-data result))

      result)))

;;; Session Continuation

(defun efrit-executor--process-injection (injection session)
  "Process INJECTION message for SESSION.
Returns non-nil if session should abort."
  (let ((inject-type (cdr (assoc "type" injection)))
        (inject-msg (cdr (assoc "message" injection))))
    (efrit-log 'info "Processing injection [%s]: %s"
               inject-type (efrit-common-truncate-string inject-msg 100))
    (pcase inject-type
      ("abort"
       ;; Add abort reason to work log and signal abort
       (efrit-session-add-work session
                               (format "[INJECTION-ABORT] %s" inject-msg)
                               "User requested abort via injection")
       t)  ; Return t to signal abort
      ("guidance"
       ;; Add guidance to work log so Claude sees it
       (efrit-session-add-work session
                               (format "[USER-GUIDANCE] %s" inject-msg)
                               "Guidance injected by supervising process")
       nil)
      ("context"
       ;; Add context to work log
       (efrit-session-add-work session
                               (format "[ADDITIONAL-CONTEXT] %s" inject-msg)
                               "Context added by external process")
       nil)
      ("priority"
       ;; Log priority change (informational)
       (efrit-session-add-work session
                               (format "[PRIORITY-CHANGE] %s" inject-msg)
                               "Priority changed by external process")
       nil)
      (_
       (efrit-log 'warn "Unknown injection type: %s" inject-type)
       nil))))

(defun efrit-executor--drain-injections (session)
  "Process all pending injections for SESSION.
Returns non-nil if session should abort."
  (let ((should-abort nil))
    (while (efrit-progress-has-pending-injection-p)
      (when-let* ((injection (efrit-progress-check-inject-queue)))
        (when (efrit-executor--process-injection injection session)
          (setq should-abort t))))
    should-abort))

(defun efrit-executor--continue-session (session callback)
  "Continue multi-step SESSION by calling Claude again.
Calls CALLBACK with the final result.

CRITICAL: Uses proper Claude API message format with tool_use/tool_result
history in the messages array, NOT in the system prompt.  This ensures
Claude remembers previous tool calls and their results."
  (let* ((session-id (efrit-session-id session))
         (continuation-count (efrit-session-continuation-count session)))

    ;; Check for circuit breaker FIRST - if tripped, don't continue
    (if (and (boundp 'efrit-do--circuit-breaker-tripped)
             efrit-do--circuit-breaker-tripped)
        (progn
          (efrit-log 'warn "Session %s stopped by circuit breaker: %s"
                     session-id efrit-do--circuit-breaker-tripped)
          (efrit-executor--complete-session
           session
           (format "[CIRCUIT BREAKER] %s" efrit-do--circuit-breaker-tripped))
          (when callback
            (funcall callback (format "Circuit breaker: %s" efrit-do--circuit-breaker-tripped))))

      ;; Check for injected messages before continuing
      (if (efrit-executor--drain-injections session)
          ;; Abort requested via injection
          (progn
            (efrit-log 'info "Session %s aborted via injection" session-id)
            (efrit-executor--complete-session session "[ABORTED] Session terminated by user request")
            (when callback
              (funcall callback "Session aborted via injection")))

        ;; Safety check: prevent infinite loops
        (if (>= continuation-count efrit-executor-max-continuations)
          (progn
            (efrit-executor--handle-error
             (format "SESSION LIMIT: Maximum continuations (%d) reached"
                     efrit-executor-max-continuations)
             callback)
            nil)  ; Return early

        ;; Increment continuation counter
        (cl-incf (efrit-session-continuation-count session))

        ;; Warn when approaching the limit (80%)
        (when (>= continuation-count (floor (* 0.8 efrit-executor-max-continuations)))
          (efrit-log 'warn "Session %s approaching turn limit (%d/%d)"
                     session-id continuation-count efrit-executor-max-continuations)
          (message "Efrit: Session approaching turn limit (%d/%d) - consider batching tool calls"
                   continuation-count efrit-executor-max-continuations))

        ;; Build system prompt (WITHOUT work log - that's now in messages)
        (require 'efrit-do)
        (let* ((system-prompt (efrit-do--command-system-prompt
                               nil nil nil session-id nil))  ; No work-log in system prompt
               ;; CRITICAL: Use proper api-messages with tool_use/tool_result history
               ;; This is the correct Claude API format for multi-turn tool conversations
               (api-messages (efrit-session-get-api-messages-for-continuation session))
               (request-data
                `(("model" . ,efrit-default-model)
                  ("max_tokens" . 8192)
                  ("temperature" . 0.0)
                  ("messages" . ,api-messages)
                  ("system" . ,system-prompt)
                  ("tools" . ,(efrit-executor--get-tools-schema)))))

          (efrit-log 'info "Continuing session %s (continuation #%d, %d messages)"
                     session-id continuation-count (length api-messages))
          (efrit-executor--update-mode-line "Continuing...")

          (efrit-executor--api-request
           request-data
           (lambda (response)
             (efrit-executor--handle-response response callback)))))))))

(defun efrit-executor--complete-session (session result)
  "Mark SESSION complete with final RESULT and process queue."
  (efrit-executor--update-mode-line "Complete!")
  (efrit-session-complete session result)
  (efrit-executor--clear-mode-line)

  (efrit-progress-end-session (efrit-session-id session) t)

  (let ((elapsed (float-time (time-since (efrit-session-start-time session))))
        (steps (length (efrit-session-work-log session))))
    (message "Efrit: Session %s complete (%.1fs, %d steps)"
             (efrit-session-id session) elapsed steps))

  ;; Process next queued command if any
  (efrit-executor--process-queue))

(defun efrit-executor--process-queue ()
  "Process next queued command if any."
  (when (and (> (efrit-session-queue-length) 0)
             (not (efrit-session-active)))
    (when-let* ((next-command (efrit-session-queue-pop)))
      (efrit-log 'info "Processing queued command: %s (%d remaining)"
                 (efrit-common-truncate-string next-command 50)
                 (efrit-session-queue-length))
      (message "Efrit: Processing queued command (%d remaining)"
               (efrit-session-queue-length))
      (efrit-execute-async
       next-command
       (lambda (result)
         (efrit-log 'debug "Queued command completed: %s"
                   (efrit-common-truncate-string (format "%s" result) 100)))))))

;;; Tools Schema

(defun efrit-executor--get-tools-schema ()
  "Get tools schema from efrit-do."
  (require 'efrit-do)
  (if (fboundp 'efrit-do--get-current-tools-schema)
      (efrit-do--get-current-tools-schema)
    efrit-do--tools-schema))

(defun efrit-executor--build-system-prompt (&optional session-id work-log extra-context)
  "Build system prompt for command execution.
If SESSION-ID is provided, include session protocol with WORK-LOG.
EXTRA-CONTEXT is optional additional text appended to the prompt."
  (require 'efrit-do)
  (let ((base-prompt (efrit-do--command-system-prompt nil nil nil session-id work-log)))
    (if extra-context
        (concat base-prompt extra-context)
      base-prompt)))

;;; Main Execution API

;;;###autoload
(defun efrit-execute (command)
  "Execute natural language COMMAND synchronously.
This is a blocking call that waits for Claude's complete response,
including multi-turn tool use conversations.

If `efrit-do--resume-messages' is set, resumes from that conversation
history instead of starting fresh."
  (interactive (list (read-string "Efrit command: " nil 'efrit-do-history)))

  ;; Check for resume state
  (let* ((resuming (and (boundp 'efrit-do--resume-messages)
                        efrit-do--resume-messages))
         (resume-messages (when resuming
                           (prog1 efrit-do--resume-messages
                             (setq efrit-do--resume-messages nil))))
         (session-id (format "%s-%s"
                            (if resuming "resume" "sync")
                            (format-time-string "%Y%m%d%H%M%S")))
         (session (efrit-session-create session-id command))
         (system-prompt (efrit-executor--build-system-prompt
                        session-id "[]"
                        (when resuming
                          "\n\nNOTE: This is a RESUMED session. The conversation history above contains prior context. Continue naturally from where we left off.")))
         ;; Build messages: either fresh start or resume with prior history
         (messages (if resuming
                      ;; Resume: prior messages + new user message
                      (vconcat (apply #'vector resume-messages)
                              (vector `(("role" . "user")
                                       ("content" . ,command))))
                    ;; Fresh: just the command
                    (vector `(("role" . "user") ("content" . ,command)))))
         (final-result nil)
         (accumulated-results "")  ; Accumulate results across all turns
         (continuation-count 0)
         (session-start-time (current-time))
         (done nil))

    (when resuming
      (efrit-log 'info "Resuming session with %d prior messages"
                (length resume-messages)))

    (efrit-session-set-active session)
    (efrit-log 'info "Executing synchronously: %s" command)

    ;; Synchronous execution loop - handles multi-turn tool use
    (condition-case err
        (progn
          (while (and (not done)
                      (< continuation-count efrit-executor-max-continuations)
                      (< (float-time (time-since session-start-time))
                         efrit-executor-session-timeout))
            (let* ((request-data
                    `(("model" . ,efrit-default-model)
                      ("max_tokens" . 8192)
                      ("temperature" . 0.0)
                      ("messages" . ,messages)
                      ("system" . ,system-prompt)
                      ("tools" . ,(efrit-executor--get-tools-schema))))
                   (api-key (efrit-common-get-api-key))
                   (json-string (json-encode request-data))
                   (escaped-json (efrit-common-escape-json-unicode json-string))
                   (url-request-method "POST")
                   (url-request-extra-headers (efrit-common-build-headers api-key))
                   (url-request-data (encode-coding-string escaped-json 'utf-8))
                   (response-buffer (url-retrieve-synchronously
                                    (efrit-common-get-api-url) nil t 60)))

              (unless response-buffer
                (error "Failed to get response from API"))

              (with-current-buffer response-buffer
                (goto-char (point-min))
                (re-search-forward "\n\n" nil t)
                (let* ((json-object-type 'hash-table)
                       (json-array-type 'vector)
                       (json-key-type 'string)
                       (response (json-read))
                       (stop-reason (gethash "stop_reason" response))
                       (content (gethash "content" response))
                       (result-text "")
                       (tool-result-blocks '()))
                  (kill-buffer)

                  ;; Process response content
                  (when content
                    ;; Add assistant response to messages (required for continuation)
                    (setq messages (vconcat messages
                                            (vector `(("role" . "assistant")
                                                     ("content" . ,content)))))

                    (dotimes (i (length content))
                      (let* ((item (aref content i))
                             (type (gethash "type" item)))
                        (cond
                         ;; Handle text
                         ((string= type "text")
                          (setq result-text (concat result-text (gethash "text" item))))

                         ;; Handle tool use
                         ((string= type "tool_use")
                          (let* ((tool-id (gethash "id" item))
                                 (tool-result
                                  (condition-case tool-err
                                      (efrit-executor--execute-tool item session)
                                    (error
                                     (format "Error: %s" (error-message-string tool-err))))))
                            (setq result-text (concat result-text tool-result))
                            ;; Collect tool results for continuation
                            (push `(("type" . "tool_result")
                                    ("tool_use_id" . ,tool-id)
                                    ("content" . ,tool-result))
                                  tool-result-blocks)))))))

                  ;; Accumulate results from this turn
                  (when (and result-text (not (string-empty-p result-text)))
                    (setq accumulated-results
                          (if (string-empty-p accumulated-results)
                              result-text
                            (concat accumulated-results "\n" result-text))))

                  ;; Check if we need to continue
                  (if (string= stop-reason "end_turn")
                      ;; Done - Claude finished
                      (progn
                        (setq done t)
                        (setq final-result accumulated-results)
                        ;; Sync messages back to session for transcript persistence
                        (setf (efrit-session-api-messages session)
                              (append messages nil))
                        (efrit-executor--complete-session session accumulated-results))

                    ;; Need to continue - add tool results to messages
                    (when tool-result-blocks
                      (setq messages (vconcat messages
                                              (vector `(("role" . "user")
                                                       ("content" . ,(vconcat (nreverse tool-result-blocks))))))))
                    (cl-incf continuation-count)
                    (efrit-log 'info "Continuing session (turn %d)" continuation-count))))))

          ;; Check if we hit limits
          (let ((elapsed (float-time (time-since session-start-time))))
            (cond
             ((>= elapsed efrit-executor-session-timeout)
              (setq final-result
                    (format "Session timeout after %.0fs (limit: %ds). Last result: %s"
                            elapsed efrit-executor-session-timeout
                            (or accumulated-results ""))))
             ((>= continuation-count efrit-executor-max-continuations)
              (setq final-result
                    (format "Session limit reached after %d turns. Last result: %s"
                            continuation-count (or accumulated-results ""))))))

          final-result)

      (error
       (efrit-executor--handle-error err)
       nil))))

;;;###autoload
(defun efrit-execute-async (command &optional callback)
  "Execute natural language COMMAND asynchronously.
Calls optional CALLBACK with the result when complete."
  (interactive (list (read-string "Efrit command (async): " nil 'efrit-do-history)))

  ;; If there's already an active session, queue this command
  (if (efrit-session-active)
      (progn
        (if (efrit-session-queue-add command)
            (message "Efrit: Command queued (position %d)"
                     (efrit-session-queue-length))
          (message "Efrit: Queue full, command not added")))

    ;; No active session, execute now
    (let* ((session-id (format "async-%s" (format-time-string "%Y%m%d%H%M%S")))
           (session (efrit-session-create session-id command))
           (system-prompt (efrit-executor--build-system-prompt session-id "[]"))
           (request-data
            `(("model" . ,efrit-default-model)
              ("max_tokens" . 8192)
              ("temperature" . 0.0)
              ("messages" . [(("role" . "user")
                             ("content" . ,command))])
              ("system" . ,system-prompt)
              ("tools" . ,(efrit-executor--get-tools-schema)))))

      (efrit-session-set-active session)
      (efrit-executor--update-mode-line "Processing...")
      (efrit-log 'info "Executing asynchronously: %s" command)

      (efrit-progress-start-session session-id command)

      (efrit-executor--api-request
       request-data
       (lambda (response)
         (efrit-executor--handle-response
          response
          (lambda (result)
            (efrit-executor--update-mode-line "Complete!")
            (when callback (funcall callback result)))))))))

;;; Session Resume (for user input responses)

;;;###autoload
(defun efrit-executor-respond (response)
  "Respond to a waiting session's pending question with RESPONSE.
If there's an active session waiting for user input, this submits
the response and resumes execution."
  (interactive
   (let ((session (efrit-session-active)))
     (if (and session (efrit-session-waiting-for-user-p session))
         (let* ((pending (efrit-session-get-pending-question session))
                (question (car pending))
                (options (cadr pending)))
           (list (if options
                     (completing-read (format "%s " question) options nil t)
                   (read-string (format "%s " question)))))
       (user-error "No session waiting for input"))))
  (let ((session (efrit-session-active)))
    (unless session
      (user-error "No active session"))
    (unless (efrit-session-waiting-for-user-p session)
      (user-error "Session is not waiting for user input"))

    ;; Record the response
    (efrit-session-respond-to-question session response)

    ;; Log the response
    (efrit-log 'info "User responded: %s"
               (efrit-common-truncate-string response 100))

    ;; Emit progress event
    (efrit-progress-show-message (format "👤 User: %s" response) 'user)

    ;; Resume the session with the updated conversation
    (efrit-executor--resume-session session)))

(defun efrit-executor--resume-session (session &optional callback)
  "Resume a paused SESSION after user input.
Continues the API call chain with the user's response in context."
  (let* ((session-id (efrit-session-id session))
         (work-log (efrit-session-compress-log session))
         (messages (efrit-session-format-conversation-for-api session))
         (continuation-count (efrit-session-continuation-count session)))

    ;; Safety check: prevent infinite loops
    (if (>= continuation-count efrit-executor-max-continuations)
        (progn
          (efrit-executor--handle-error
           (format "SESSION LIMIT: Maximum continuations (%d) reached"
                   efrit-executor-max-continuations)
           callback)
          nil)

      ;; Increment continuation counter
      (cl-incf (efrit-session-continuation-count session))

      ;; Build system prompt with user response context
      (require 'efrit-do)
      (let* ((last-response (car (last (efrit-session-user-responses session))))
             (response-context (when last-response
                                (format "\n\n[USER RESPONDED TO QUESTION]\nQ: %s\nA: %s\n\nContinue with the user's answer."
                                        (cadr last-response)  ; question
                                        (car last-response))))  ; response
             (base-system-prompt (efrit-do--command-system-prompt
                                  nil nil nil session-id work-log))
             (system-prompt (if response-context
                               (concat base-system-prompt response-context)
                             base-system-prompt))
             (request-data
              `(("model" . ,efrit-default-model)
                ("max_tokens" . 8192)
                ("temperature" . 0.0)
                ("messages" . ,messages)
                ("system" . ,system-prompt)
                ("tools" . ,(efrit-executor--get-tools-schema)))))

        (efrit-log 'info "Resuming session %s after user response" session-id)
        (efrit-executor--update-mode-line "Resuming...")

        (efrit-executor--api-request
         request-data
         (lambda (response)
           (efrit-executor--handle-response response callback)))))))

;;;###autoload
(defun efrit-executor-pending-question ()
  "Return the pending question for the active session, if any.
Returns (question options timestamp) or nil."
  (when-let* ((session (efrit-session-active)))
    (efrit-session-get-pending-question session)))

;;; User Commands

;;;###autoload
(defun efrit-executor-cancel ()
  "Cancel the current execution session."
  (interactive)
  (if-let* ((session (efrit-session-active)))
      (when (y-or-n-p "Cancel active Efrit session? ")
        (efrit-session-cancel session)
        (efrit-executor--clear-mode-line)
        (message "Efrit: Session cancelled")
        (efrit-executor--process-queue))
    (message "No active Efrit session")))

;;;###autoload
(defun efrit-executor-clear-queue ()
  "Clear all queued commands."
  (interactive)
  (let ((count (efrit-session-queue-length)))
    (when (and (> count 0)
               (y-or-n-p (format "Clear %d queued commands? " count)))
      (efrit-session-queue-clear)
      (message "Efrit: Queue cleared"))))

(provide 'efrit-executor)
;;; efrit-executor.el ends here
