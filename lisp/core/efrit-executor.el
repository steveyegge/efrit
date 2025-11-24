;;; efrit-executor.el --- Command execution for Efrit -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.3.0
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

;; Forward declarations
(declare-function efrit-progress-start-session "efrit-progress")
(declare-function efrit-progress-end-session "efrit-progress")
(declare-function efrit-progress-show-message "efrit-progress")
(declare-function efrit-progress-show-tool-start "efrit-progress")
(declare-function efrit-progress-show-tool-result "efrit-progress")
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

(defcustom efrit-executor-max-continuations 30
  "Maximum API calls per session before emergency stop."
  :type 'integer
  :group 'efrit-executor)

(defcustom efrit-executor-max-tool-calls 100
  "Maximum tool calls per session (safety limit)."
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
      (let ((elapsed (float-time (time-since (efrit-session-start-time session)))))
        (setq efrit-executor-mode-line-string
              (format "[Efrit: %s (%.1fs)]" message elapsed))
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

    (when (fboundp 'efrit-progress-show-message)
      (efrit-progress-show-message message 'error))

    (when-let* ((session (efrit-session-active)))
      (setf (efrit-session-last-error session) message)
      (when (fboundp 'efrit-progress-end-session)
        (efrit-progress-end-session (efrit-session-id session) nil)))

    (efrit-session-set-active nil)
    (efrit-executor--clear-mode-line)

    (when callback
      (funcall callback (format "Error: %s" message)))))

;;; Response Processing

(defun efrit-executor--handle-response (response callback)
  "Handle RESPONSE from Claude API and execute tools.
Calls CALLBACK with the final result string."
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
                (result-text "")
                (session-complete-p nil)
                (completion-message nil)
                (tool-results '())
                (session (efrit-session-active)))

            (when content
              ;; Process each content item
              (dotimes (i (length content))
                (let* ((item (aref content i))
                       (type (gethash "type" item)))
                  (cond
                   ;; Handle text content
                   ((string= type "text")
                    (when-let* ((text (gethash "text" item)))
                      (setq result-text (concat result-text text))
                      (when (fboundp 'efrit-progress-show-message)
                        (efrit-progress-show-message text 'claude))))

                   ;; Handle tool use
                   ((string= type "tool_use")
                    (let* ((tool-result (efrit-executor--execute-tool item session))
                           (tool-input (gethash "input" item)))
                      (setq result-text (concat result-text tool-result))

                      ;; Check for session completion
                      (when (string-match "\\[SESSION-COMPLETE: \\(.+\\)\\]" tool-result)
                        (setq session-complete-p t)
                        (setq completion-message (match-string 1 tool-result)))

                      ;; Track tool execution
                      (when session
                        (push (list tool-result
                                  (if (hash-table-p tool-input)
                                      (json-encode tool-input)
                                    (format "%S" tool-input)))
                              tool-results))))))))

            ;; Handle session continuation or completion
            (cond
             ;; Session complete or no active session
             ((or (not session)
                  session-complete-p
                  (and (boundp 'efrit-do--force-complete)
                       efrit-do--force-complete))
              (when session
                (efrit-executor--complete-session
                 session
                 (or completion-message result-text
                     (when (and (boundp 'efrit-do--force-complete)
                               efrit-do--force-complete)
                       "Session auto-completed after successful code execution"))))
              (when callback
                (funcall callback (or completion-message result-text))))

             ;; Session needs to continue
             (t
              ;; Update session work log
              (dolist (result (nreverse tool-results))
                (efrit-session-add-work session (car result) (cadr result)))
              ;; Continue the session
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
      (error "🚨 SESSION SAFETY LIMIT: Over %d tool calls - session terminated"
             efrit-executor-max-tool-calls))

    ;; Show progress
    (when (fboundp 'efrit-progress-show-tool-start)
      (efrit-progress-show-tool-start tool-name input-data))

    ;; Execute via efrit-do's tool handler
    (require 'efrit-do)
    (let ((result
           (condition-case err
               (efrit-do--execute-tool tool-item)
             (error
              (let ((error-msg (error-message-string err)))
                (when (fboundp 'efrit-progress-show-tool-result)
                  (efrit-progress-show-tool-result tool-name error-msg nil))
                (signal (car err) (cdr err)))))))

      ;; Show result
      (when (fboundp 'efrit-progress-show-tool-result)
        (efrit-progress-show-tool-result tool-name result t))

      ;; Track in session
      (when session
        (efrit-session-track-tool session tool-name input-data result))

      result)))

;;; Session Continuation

(defun efrit-executor--continue-session (session callback)
  "Continue multi-step SESSION by calling Claude again.
Calls CALLBACK with the final result."
  (let* ((session-id (efrit-session-id session))
         (work-log (efrit-session-compress-log session))
         (original-command (efrit-session-command session))
         (continuation-count (efrit-session-continuation-count session)))

    ;; Safety check: prevent infinite loops
    (if (>= continuation-count efrit-executor-max-continuations)
        (progn
          (efrit-executor--handle-error
           (format "🚨 SESSION LIMIT: Maximum continuations (%d) reached"
                   efrit-executor-max-continuations)
           callback)
          nil)  ; Return early

      ;; Increment continuation counter
      (cl-incf (efrit-session-continuation-count session))

      ;; Build system prompt
      (require 'efrit-do)
      (let* ((system-prompt (efrit-do--command-system-prompt
                             nil nil nil session-id work-log))
             (request-data
              `(("model" . ,efrit-default-model)
                ("max_tokens" . 8192)
                ("temperature" . 0.0)
                ("messages" . [(("role" . "user")
                               ("content" . ,original-command))])
                ("system" . ,system-prompt)
                ("tools" . ,(efrit-executor--get-tools-schema)))))

        (efrit-log 'debug "Continuing session %s (continuation #%d)"
                   session-id continuation-count)
        (efrit-executor--update-mode-line "Continuing...")

        (efrit-executor--api-request
         request-data
         (lambda (response)
           (efrit-executor--handle-response response callback)))))))

(defun efrit-executor--complete-session (session result)
  "Mark SESSION complete with final RESULT and process queue."
  (efrit-executor--update-mode-line "Complete!")
  (efrit-session-complete session result)
  (efrit-executor--clear-mode-line)

  (when (fboundp 'efrit-progress-end-session)
    (efrit-progress-end-session (efrit-session-id session) t))

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

(defun efrit-executor--build-system-prompt (&optional session-id work-log)
  "Build system prompt for command execution.
If SESSION-ID is provided, include session protocol with WORK-LOG."
  (require 'efrit-do)
  (efrit-do--command-system-prompt nil nil nil session-id work-log))

;;; Main Execution API

;;;###autoload
(defun efrit-execute (command)
  "Execute natural language COMMAND synchronously.
This is a blocking call that waits for Claude's response."
  (interactive (list (read-string "Efrit command: " nil 'efrit-do-history)))

  (let* ((session-id (format "sync-%s" (format-time-string "%Y%m%d%H%M%S")))
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
    (efrit-log 'info "Executing synchronously: %s" command)

    ;; Synchronous API call
    (condition-case err
        (let* ((api-key (efrit-common-get-api-key))
               (json-data (efrit-common-escape-json-unicode
                          (json-encode request-data)))
               (url-request-method "POST")
               (url-request-extra-headers (efrit-common-build-headers api-key))
               (url-request-data json-data)
               (response-buffer (url-retrieve-synchronously
                                (efrit-common-get-api-url) nil t 30)))

          (unless response-buffer
            (error "Failed to get response from API"))

          (with-current-buffer response-buffer
            (goto-char (point-min))
            (re-search-forward "\n\n" nil t)
            (let* ((json-object-type 'hash-table)
                   (json-array-type 'vector)
                   (json-key-type 'string)
                   (response (json-read)))
              (kill-buffer)

              ;; Process response synchronously
              (let ((result nil))
                (efrit-executor--handle-response
                 response
                 (lambda (r) (setq result r)))
                result))))
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

      (when (fboundp 'efrit-progress-start-session)
        (efrit-progress-start-session session-id command))

      (efrit-executor--api-request
       request-data
       (lambda (response)
         (efrit-executor--handle-response
          response
          (lambda (result)
            (efrit-executor--update-mode-line "Complete!")
            (when callback (funcall callback result)))))))))

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
