;;; efrit-async.el --- Async infrastructure for Efrit -*- lexical-binding: t -*-

;; Copyright (C) 2025

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai, async

;;; Commentary:

;; Async session management for Efrit's unified command interface.
;; Provides non-blocking execution with Claude-controlled session flow.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'url)
(require 'efrit-log)
(require 'efrit-common)
(require 'efrit-context)
(require 'efrit-protocol)
(require 'efrit-performance)

;; Forward declarations
(declare-function efrit-tools-eval-sexp "efrit-tools")
(declare-function efrit-do--command-system-prompt "efrit-do")
(defvar efrit-do--tools-schema)

(defvar efrit-async--active-session nil
  "Currently active session.")

(defvar efrit-async--session-queue nil  
  "Queue of pending commands.")

(defvar efrit-async--sessions (make-hash-table :test 'equal)
  "Hash table of all sessions by ID.")

;;; Customization

(defcustom efrit-async-show-progress t
  "Whether to show progress in mode line."
  :type 'boolean
  :group 'efrit)

(defcustom efrit-async-max-work-log-entries 50
  "Maximum number of work log entries to keep in session history."
  :type 'integer
  :group 'efrit)

(defcustom efrit-async-max-session-queue-size 20
  "Maximum number of sessions allowed in the queue."
  :type 'integer
  :group 'efrit)

;;; Progress Display

(defvar efrit-async-mode-line-string nil
  "Mode line indicator for active Efrit session.")

(defun efrit-async--clear-mode-line ()
  "Clear mode line indicators and force update."
  (setq efrit-async-mode-line-string nil)
  (force-mode-line-update t))

(defun efrit-async--show-progress (message)
  "Update progress indicators with MESSAGE."
  (when efrit-async-show-progress
    (when efrit-async--active-session
      (let* ((elapsed (float-time (time-since (efrit-session-start-time 
                                              efrit-async--active-session)))))
        (setq efrit-async-mode-line-string
              (format "[Efrit: %s (%.1fs)]" message elapsed))
        (force-mode-line-update t))))
  (message "Efrit: %s" message))

;; Add to mode-line if not already there
(unless (member '(efrit-async-mode-line-string 
                  (" " efrit-async-mode-line-string " "))
                mode-line-misc-info)
  (add-to-list 'mode-line-misc-info 
               '(efrit-async-mode-line-string 
                 (" " efrit-async-mode-line-string " "))))

;;; Session Management

(defun efrit-async--update-session (session-id result elisp)
  "Update or create session with SESSION-ID, logging RESULT and ELISP."
  (unless efrit-async--active-session
    (when session-id
      (setq efrit-async--active-session
            (make-efrit-session 
             :id session-id
             :start-time (current-time)
             :status 'active
             :buffer (current-buffer)))
      ;; Track session for memory management
      (puthash session-id efrit-async--active-session efrit-async--sessions)
      (efrit-performance-touch-session session-id)
      (efrit-log 'info "Started session %s" session-id)))
  
  (when efrit-async--active-session
    ;; Store the entry - note work log stores (result . code) pairs
    (push (cons result elisp) 
          (efrit-session-work-log efrit-async--active-session))
    
    ;; Limit work log size to prevent memory growth
    (let ((work-log (efrit-session-work-log efrit-async--active-session)))
      (when (> (length work-log) efrit-async-max-work-log-entries)
        (setf (efrit-session-work-log efrit-async--active-session)
              (seq-take work-log efrit-async-max-work-log-entries))))
    
    ;; Log with context info
    (let ((context-state (efrit-context-capture-state)))
      (efrit-log 'debug "Session %s: step %d in buffer %s at point %d" 
                 session-id
                 (length (efrit-session-work-log efrit-async--active-session))
                 (efrit-context-state-buffer-name context-state)
                 (efrit-context-state-point context-state)))))

(defun efrit-async--complete-session (session-id result)
  "Mark session SESSION-ID complete with final RESULT and process queue."
  (efrit-async--show-progress "Complete!")
  (when efrit-async--active-session
    (let ((elapsed (float-time (time-since (efrit-session-start-time 
                                           efrit-async--active-session))))
          (steps (length (efrit-session-work-log efrit-async--active-session)))
          (command (efrit-session-command efrit-async--active-session)))
      (message "Efrit: Session %s complete (%.1fs, %d steps)"
               session-id elapsed steps)
      (efrit-log 'info "Completed session %s: %.1fs, %d steps, result: %s" 
                 session-id elapsed steps 
                 (efrit-common-truncate-string (format "%s" result) 100))
      
      ;; Cache the result for future use
      (when command
        (let* ((context (efrit-context-capture-state))
               (cache-key (efrit-performance-cache-key command context)))
          (efrit-performance-cache-put cache-key result)
          (efrit-log 'debug "Cached result for command: %s" 
                     (efrit-common-truncate-string command 50))))))
  
  (setq efrit-async--active-session nil)
  (efrit-async--clear-mode-line)
  (efrit-async--process-queue))

(defun efrit-async--add-to-queue (command)
  "Add COMMAND to session queue, enforcing size limits."
  (when (>= (length efrit-async--session-queue) efrit-async-max-session-queue-size)
    ;; Queue is full - remove oldest entries
    (efrit-log 'warn "Session queue full, dropping oldest entries")
    (setq efrit-async--session-queue 
          (seq-take efrit-async--session-queue 
                    (1- efrit-async-max-session-queue-size))))
  
  (setq efrit-async--session-queue 
        (append efrit-async--session-queue (list command)))
  (efrit-log 'debug "Added command to queue (size: %d)" (length efrit-async--session-queue)))

(defun efrit-async--process-queue ()
  "Process next queued command if any."
  (when (and efrit-async--session-queue
             (not efrit-async--active-session))
    (let ((next-command (pop efrit-async--session-queue)))
      (efrit-log 'info "Processing queued command: %s (%d remaining)" 
                 (efrit-common-truncate-string next-command 50)
                 (length efrit-async--session-queue))
      (message "Efrit: Processing queued command (%d remaining)" 
               (length efrit-async--session-queue))
      ;; Execute the command asynchronously
      (efrit-async-execute-command 
       next-command
       (lambda (result)
         ;; When this command completes, the completion handler
         ;; will call process-queue again
         (efrit-log 'debug "Queued command completed: %s" 
                   (efrit-common-truncate-string result 100)))))))

;;; Work Log Compression

(defun efrit-session--compress-log (session)
  "Create compressed summary of SESSION work for Claude."
  (when session
    ;; Use the context utilities for smart compression
    (efrit-context-compress-work-log 
     (efrit-session-work-log session)
     'smart)))

;;; User Commands

(defun efrit-async-status ()
  "Show current Efrit session status."
  (interactive)
  (cond
   (efrit-async--active-session
    (let* ((session efrit-async--active-session)
           (elapsed (float-time (time-since (efrit-session-start-time session))))
           (steps (length (efrit-session-work-log session))))
      (message "Efrit: Session %s - %s (%.1fs, %d steps, %d queued)"
               (efrit-session-id session)
               (efrit-session-status session)
               elapsed steps
               (length efrit-async--session-queue))))
   ((> (length efrit-async--session-queue) 0)
    (message "Efrit: No active session, %d commands queued" 
             (length efrit-async--session-queue)))
   (t
    (message "No active Efrit session"))))

(defun efrit-async-cancel ()
  "Cancel current Efrit session."
  (interactive)
  (when (and efrit-async--active-session
             (y-or-n-p "Cancel active Efrit session? "))
    (efrit-log 'info "User cancelled session %s" 
               (efrit-session-id efrit-async--active-session))
    (setq efrit-async--active-session nil)
    (efrit-async--clear-mode-line)
    (message "Efrit: Session cancelled")
    (efrit-async--process-queue)))

(defun efrit-async-show-log ()
  "Show work log for current session."
  (interactive)
  (if efrit-async--active-session
      (let ((buffer (get-buffer-create "*Efrit Session Log*")))
        (with-current-buffer buffer
          (erase-buffer)
          (insert "Efrit Session Log\n")
          (insert "================\n\n")
          (insert (format "Session ID: %s\n" 
                         (efrit-session-id efrit-async--active-session)))
          (insert (format "Status: %s\n" 
                         (efrit-session-status efrit-async--active-session)))
          (insert (format "Started: %s\n\n" 
                         (format-time-string "%Y-%m-%d %H:%M:%S" 
                                           (efrit-session-start-time efrit-async--active-session))))
          (dolist (entry (reverse (efrit-session-work-log 
                                  efrit-async--active-session)))
            (insert "─────────────────────────────────────\n")
            (insert (format "ELISP:\n%s\n\n" (car entry)))
            (insert (format "RESULT:\n%S\n\n" (cdr entry))))
          (goto-char (point-min))
          (special-mode))  ; Read-only mode
        (pop-to-buffer buffer))
    (message "No active Efrit session")))

;;; Async API Infrastructure

(defun efrit-async--api-request (request-data callback)
  "Send REQUEST-DATA to Claude API asynchronously, calling CALLBACK with response.
REQUEST-DATA should be the JSON data structure to send.
CALLBACK is called with the parsed response or error information."
  (efrit-log 'debug "Async API request starting")
  (condition-case err
      (let* ((api-key (efrit-common-get-api-key))
             (json-string (json-encode request-data))
             ;; Convert unicode characters to JSON escape sequences to prevent multibyte HTTP errors
             (escaped-json (efrit-common-escape-json-unicode json-string))
             (url-request-data (encode-coding-string escaped-json 'utf-8))
             (url-request-method "POST")
             (url-request-extra-headers (efrit-common-build-headers api-key))
             (start-time (float-time)))
        
        (url-retrieve
         efrit-common-api-url
         (lambda (status)
           (let ((elapsed (- (float-time) start-time)))
             (efrit-performance-record-api-time elapsed)
             (efrit-log 'info "API call completed in %.2fs" elapsed))
           (efrit-async--handle-url-response status callback))
         nil t))
    (error 
     (efrit-log 'error "API request setup failed: %s" (error-message-string err))
     (efrit-async--handle-error err))))

(defun efrit-async--handle-url-response (status callback)
  "Handle url-retrieve STATUS and call CALLBACK with parsed response."
  (unwind-protect
      (condition-case err
          (progn
            ;; Check for HTTP errors
            (when (plist-get status :error)
              (error "HTTP error: %s" (plist-get status :error)))
            
            ;; Extract response text
            (goto-char (point-min))
            (when (search-forward-regexp "^$" nil t)
              (let ((response-text (buffer-substring-no-properties (point) (point-max))))
                
                ;; Parse and process the response
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
    ;; Always clean up the buffer in unwind-protect
    (when (buffer-live-p (current-buffer))
      (kill-buffer (current-buffer)))))

(defun efrit-async--handle-error (error)
  "Handle ERROR during async operations."
  (let ((message (if (stringp error)
                     error
                   (error-message-string error))))
    (efrit-async--show-progress "Error!")
    (message "Efrit error: %s" message)
    
    ;; Clear session on error
    (setq efrit-async--active-session nil)
    (efrit-async--clear-mode-line)))

(defun efrit-async--handle-response (response callback)
  "Handle RESPONSE from Claude API and execute tools, then call CALLBACK.
RESPONSE should be the parsed JSON response from Claude API.
CALLBACK will be called with the final result string."
  (efrit-log 'debug "Handling Claude API response")
  (condition-case err
      (if (not response)
          (progn
            (efrit-async--handle-error "No response from API")
            (when callback (funcall callback "Error: No response from API")))
        
        ;; Check for API errors first
        (if-let* ((error-obj (gethash "error" response)))
            (let* ((error-type (gethash "type" error-obj))
                   (error-message (gethash "message" error-obj))
                   (error-str (format "API Error (%s): %s" error-type error-message)))
              (efrit-async--handle-error error-str)
              (when callback (funcall callback error-str)))
          
          ;; Process successful response - similar to efrit-do's implementation
          (let ((content (gethash "content" response))
                (result-text "")
                (session-complete-p nil)
                (completion-message nil)
                (tool-results '()))
            
            (when content
              ;; Process each content item
              (dotimes (i (length content))
                (let* ((item (aref content i))
                       (type (gethash "type" item)))
                  (cond
                   ;; Handle text content
                   ((string= type "text")
                    (when-let* ((text (gethash "text" item)))
                      (setq result-text (concat result-text text))))
                   
                   ;; Handle tool use - delegate to efrit-do's tool execution
                   ((string= type "tool_use")
                    (let* ((tool-result (efrit-async--execute-tool item))
                           (tool-input (gethash "input" item)))
                      (setq result-text 
                            (concat result-text tool-result))
                      ;; Check for session completion
                      (when (string-match "\\[SESSION-COMPLETE: \\(.+\\)\\]" tool-result)
                        (setq session-complete-p t)
                        (setq completion-message (match-string 1 tool-result)))
                      ;; Track tool execution for work log
                      (when efrit-async--active-session
                        (push (list tool-result 
                                  (format "%S" tool-input))
                              tool-results))))))))
            
            ;; Handle session continuation or completion
            (cond
             ;; Session is complete or no active session
             ((or (not efrit-async--active-session) session-complete-p)
              (when efrit-async--active-session
                (efrit-async--complete-session 
                 (efrit-session-id efrit-async--active-session) 
                 (or completion-message result-text)))
              (when callback 
                (funcall callback (or completion-message result-text))))
             
             ;; Session needs to continue
             (t
              ;; Update session work log with new results
              (dolist (result (nreverse tool-results))
                (efrit-async--update-session 
                 (efrit-session-id efrit-async--active-session) 
                 (car result) 
                 (cadr result)))
              ;; Continue the session
              (efrit-async--continue-session efrit-async--active-session callback))))))
    (error
     (let ((error-msg (format "Response handling error: %s" (error-message-string err))))
       (efrit-async--handle-error error-msg)
       (when callback (funcall callback error-msg))))))

(defun efrit-async--execute-tool (tool-item)
  "Execute a tool using the shared protocol.
TOOL-ITEM is the tool_use object from Claude's response."
  (let ((tool-name (cdr (assoc 'name tool-item)))
        (input-data (cdr (assoc 'input tool-item))))
    (efrit-protocol-execute-tool tool-name input-data)))

;;; Main Async Interface

(defun efrit-async--continue-session (session callback)
  "Continue a multi-step SESSION by calling Claude again.
CALLBACK is the original completion callback."
  (let* ((session-id (efrit-session-id session))
         (work-log (efrit-session--compress-log session))
         (original-command (efrit-session-command session))
         (system-prompt (efrit-async--build-system-prompt session-id work-log))
         (request-data
          `(("model" . "claude-sonnet-4-20250514")
            ("max_tokens" . 8192)
            ("temperature" . 0.0)
            ("messages" . [(("role" . "user")
                           ("content" . ,original-command))])
            ("system" . ,system-prompt)
            ("tools" . ,(efrit-async--get-tools-schema)))))
    
    (efrit-log 'debug "Continuing session %s with work log: %S" session-id work-log)
    (efrit-async--show-progress "Continuing...")
    
    (efrit-async--api-request 
     request-data
     (lambda (response)
       (efrit-async--handle-response response callback)))))

(defun efrit-async-execute-command (command callback)
  "Execute natural language COMMAND asynchronously.
CALLBACK is called with the result when complete.
This is the async version of efrit-do's command execution."
  ;; If there's already an active session, queue this command
  (if efrit-async--active-session
      (progn
        (efrit-async--add-to-queue command)
        (message "Efrit: Command queued (position %d)" 
                 (length efrit-async--session-queue)))
    
    ;; No active session, check cache first
    (let* ((context (efrit-context-capture-state))
           (cache-key (efrit-performance-cache-key command context))
           (cached-response (efrit-performance-get-cached cache-key)))
      
      (if cached-response
          ;; Use cached response
          (progn
            (efrit-log 'info "Using cached response for command: %s"
                       (efrit-common-truncate-string command 50))
            (funcall callback cached-response))
        
        ;; No cache hit, execute normally
        (let ((session-id (format "async-%s" (format-time-string "%Y%m%d%H%M%S"))))
          (setq efrit-async--active-session
                (make-efrit-session
                 :id session-id
                 :command command
                 :status 'active
                 :start-time (current-time)
                 :work-log '()))
          
          (efrit-async--show-progress "Processing...")
      
          (let* ((system-prompt (efrit-async--build-system-prompt session-id "[]"))
             (request-data
              `(("model" . "claude-sonnet-4-20250514")
                ("max_tokens" . 8192)
                ("temperature" . 0.0)
                ("messages" . [(("role" . "user")
                               ("content" . ,command))])
                ("system" . ,system-prompt)
                ("tools" . ,(efrit-async--get-tools-schema)))))
        
        (efrit-async--api-request 
         request-data
         (lambda (response)
           (efrit-async--handle-response 
            response
            (lambda (result)
              (efrit-async--show-progress "Complete!")
              (when callback (funcall callback result))))))))))

(defun efrit-async--build-system-prompt (&optional session-id work-log)
  "Build system prompt for async commands - delegates to efrit-do.
If SESSION-ID is provided, include session protocol with WORK-LOG."
  (when (require 'efrit-do nil t)
    (efrit-do--command-system-prompt nil nil nil session-id work-log)))

(defun efrit-async--get-tools-schema ()
  "Get tools schema - delegates to efrit-do."
  (when (require 'efrit-do nil t)
    efrit-do--tools-schema))

;;;###autoload
(defun efrit-async-show-queue ()
  "Show the current command queue."
  (interactive)
  (let ((buffer (get-buffer-create "*Efrit Queue*")))
    (with-current-buffer buffer
      (erase-buffer)
      (insert "Efrit Command Queue\n")
      (insert "==================\n\n")
      
      (if efrit-async--active-session
          (insert (format "Active: %s\n\n" 
                         (efrit-common-truncate-string 
                          (efrit-session-command efrit-async--active-session) 60)))
        (insert "Active: None\n\n"))
      
      (if (null efrit-async--session-queue)
          (insert "Queue is empty.")
        (insert (format "Queued commands (%d):\n\n" 
                       (length efrit-async--session-queue)))
        (cl-loop for cmd in efrit-async--session-queue
                 for i from 1
                 do (insert (format "%d. %s\n" i 
                                   (efrit-common-truncate-string cmd 70))))))
    (switch-to-buffer buffer)))

;;;###autoload
(defun efrit-async-clear-queue ()
  "Clear the command queue."
  (interactive)
  (when (and efrit-async--session-queue
             (y-or-n-p (format "Clear %d queued commands? " 
                              (length efrit-async--session-queue))))
    (setq efrit-async--session-queue nil)
    (message "Efrit: Queue cleared")))

(provide 'efrit-async)
;;; efrit-async.el ends here