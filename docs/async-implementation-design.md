# Efrit Async Implementation Design

## Overview

This document details the async implementation for efrit-do, providing non-blocking execution with session management where Claude controls the flow.

## Core Architecture

### Session Data Structure

```elisp
;;; efrit-async.el --- Async infrastructure for Efrit -*- lexical-binding: t -*-

(defstruct efrit-session
  id              ; Unique identifier from Claude
  work-log        ; List of (elisp . result) pairs  
  start-time      ; When session started
  status          ; 'active, 'waiting, 'complete
  buffer)         ; Original buffer for context

(defvar efrit-async--active-session nil
  "Currently active session.")

(defvar efrit-async--session-queue nil  
  "Queue of pending commands.")
```

### Async API Infrastructure

```elisp
(defun efrit-async--api-request (data callback)
  "Send DATA to Claude API asynchronously, calling CALLBACK with response."
  (let ((url-request-method "POST")
        (url-request-extra-headers (efrit--api-headers))
        (url-request-data (json-encode data)))
    (url-retrieve 
     efrit-api-url
     (lambda (status)
       (if-let ((error (plist-get status :error)))
           (efrit-async--handle-error error)
         (condition-case err
             (funcall callback (efrit--parse-response))
           (error (efrit-async--handle-error err)))))
     nil t t)))

(defun efrit--parse-response ()
  "Parse JSON response from current buffer."
  (goto-char (point-min))
  (re-search-forward "^$")  ; Skip headers
  (json-read))
```

## Main Entry Point

```elisp
(defun efrit-do (command)
  "Execute COMMAND with session-based async execution.
If a session is active, queues the command for Claude to evaluate
whether it's a continuation or new work."
  (interactive "sCommand: ")
  
  (cond
   ;; Active session - queue and let Claude decide
   (efrit-async--active-session
    (push command efrit-async--session-queue)
    (message "Efrit: Queued. Session %s active with %d pending"
             (efrit-session-id efrit-async--active-session)
             (length efrit-async--session-queue)))
   
   ;; No active session - start immediately
   (t
    (efrit-async--start-command command))))

(defun efrit-async--start-command (command)
  "Start executing COMMAND."
  (let ((request-data `((command . ,command)
                       (buffer . ,(buffer-name))
                       (mode . ,major-mode))))
    
    (efrit-async--show-progress "Analyzing request...")
    (efrit-async--api-request 
     request-data
     #'efrit-async--handle-response)))
```

## Response Handling

```elisp
(defun efrit-async--handle-response (response)
  "Handle RESPONSE from Claude - always execute elisp and check status."
  (let* ((elisp (alist-get 'elisp response))
         (session-info (alist-get 'session response))
         (session-id (alist-get 'id session-info))
         (status (alist-get 'status session-info)))
    
    ;; Always execute the elisp first
    (let ((result (efrit-async--execute-in-context elisp)))
      
      ;; Create or update session
      (efrit-async--update-session session-id result elisp)
      
      ;; Handle status
      (pcase status
        ("continue"
         (efrit-async--show-progress "Continuing...")
         (efrit-async--continue-session session-id result))
        
        ("complete"
         (efrit-async--complete-session session-id result))
        
        ("need-input"
         (efrit-async--wait-for-input session-id))))))

(defun efrit-async--execute-in-context (elisp)
  "Execute ELISP in the context of the original buffer."
  (if efrit-async--active-session
      (let ((buffer (efrit-session-buffer efrit-async--active-session)))
        (if (buffer-live-p buffer)
            (with-current-buffer buffer
              (efrit-tools-eval-sexp elisp))
          ;; Buffer gone - execute in current context
          (efrit-tools-eval-sexp elisp)))
    ;; No session yet - current buffer
    (efrit-tools-eval-sexp elisp)))
```

## Session Management

```elisp
(defun efrit-async--update-session (session-id result elisp)
  "Update or create session with SESSION-ID, logging RESULT and ELISP."
  (unless efrit-async--active-session
    (when session-id
      (setq efrit-async--active-session
            (make-efrit-session 
             :id session-id
             :start-time (current-time)
             :status 'active
             :buffer (current-buffer)))))
  
  (when efrit-async--active-session
    (push (cons elisp result) 
          (efrit-session-work-log efrit-async--active-session))))

(defun efrit-async--continue-session (session-id result)
  "Continue session SESSION-ID with RESULT of last execution."
  (let ((work-summary (efrit-session--compress-log efrit-async--active-session)))
    (efrit-async--api-request
     `((session-id . ,session-id)
       (last-result . ,result)
       (work-summary . ,work-summary))
     #'efrit-async--handle-response)))

(defun efrit-async--complete-session (session-id result)
  "Mark session SESSION-ID complete and process queue."
  (efrit-async--show-progress "Complete!")
  (message "Efrit: Session %s complete (%.1fs, %d steps)"
           session-id
           (float-time (time-since (efrit-session-start-time 
                                   efrit-async--active-session)))
           (length (efrit-session-work-log efrit-async--active-session)))
  
  (setq efrit-async--active-session nil)
  (efrit-async--process-queue))

(defun efrit-async--process-queue ()
  "Process next queued command if any."
  (when efrit-async--session-queue
    (let ((next-command (pop efrit-async--session-queue)))
      (message "Efrit: Processing queued command (%d remaining)" 
               (length efrit-async--session-queue))
      (efrit-async--start-command next-command))))
```

## Progress Feedback

```elisp
(defvar efrit-async-mode-line-string nil
  "Mode line indicator for active Efrit session.")

(defun efrit-async--show-progress (message)
  "Update progress indicators with MESSAGE."
  (when efrit-async--active-session
    (let* ((elapsed (float-time (time-since (efrit-session-start-time 
                                            efrit-async--active-session))))
           (steps (length (efrit-session-work-log efrit-async--active-session))))
      (setq efrit-async-mode-line-string
            (format "[Efrit: %s (%.1fs)]" message elapsed))
      (force-mode-line-update t)))
  (message "Efrit: %s" message))

;; Add to mode-line
(add-to-list 'mode-line-misc-info 
             '(efrit-async-mode-line-string 
               (" " efrit-async-mode-line-string " ")))
```

## User Commands

```elisp
(defun efrit-status ()
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
   (t
    (message "No active Efrit session"))))

(defun efrit-cancel ()
  "Cancel current Efrit session."
  (interactive)
  (when (and efrit-async--active-session
             (y-or-n-p "Cancel active Efrit session? "))
    (setq efrit-async--active-session nil)
    (setq efrit-async-mode-line-string nil)
    (force-mode-line-update t)
    (message "Efrit: Session cancelled")
    (efrit-async--process-queue)))

(defun efrit-show-log ()
  "Show work log for current session."
  (interactive)
  (if efrit-async--active-session
      (let ((buffer (get-buffer-create "*Efrit Session Log*")))
        (with-current-buffer buffer
          (erase-buffer)
          (insert "Efrit Session Log\n")
          (insert "================\n\n")
          (dolist (entry (reverse (efrit-session-work-log 
                                  efrit-async--active-session)))
            (insert (format "ELISP: %s\n" (car entry)))
            (insert (format "RESULT: %S\n\n" (cdr entry))))
          (goto-char (point-min)))
        (pop-to-buffer buffer))
    (message "No active Efrit session")))
```

## Error Handling

```elisp
(defun efrit-async--handle-error (error)
  "Handle ERROR during async operations."
  (let ((message (if (stringp error)
                     error
                   (error-message-string error))))
    (efrit-async--show-progress "Error!")
    (message "Efrit error: %s" message)
    
    ;; Clear session on error
    (setq efrit-async--active-session nil)
    (setq efrit-async-mode-line-string nil)
    (force-mode-line-update t)
    
    ;; Log to results buffer if configured
    (when efrit-do-show-results
      (efrit-do--display-error "async operation" message))))

(defun efrit-async--wait-for-input (session-id)
  "Handle need-input status for SESSION-ID."
  (setf (efrit-session-status efrit-async--active-session) 'waiting)
  (efrit-async--show-progress "Waiting for input...")
  ;; The elisp execution should have prompted user
  ;; Next command will continue the session
  )
```

## Work Log Compression

```elisp
(defun efrit-session--compress-log (session)
  "Create compressed summary of SESSION work for Claude."
  (let ((log (efrit-session-work-log session)))
    `((step-count . ,(length log))
      (elapsed-time . ,(float-time (time-since (efrit-session-start-time session))))
      (last-result . ,(cdr (car log)))
      ;; Include key context without full elisp history
      (buffer-modified . ,(when (buffer-live-p (efrit-session-buffer session))
                           (with-current-buffer (efrit-session-buffer session)
                             (buffer-modified-p))))
      (recent-operations . ,(efrit-session--summarize-operations log 3)))))

(defun efrit-session--summarize-operations (log count)
  "Summarize recent COUNT operations from LOG."
  (mapcar (lambda (entry)
            (let ((elisp (car entry))
                  (result (cdr entry)))
              `(:type ,(efrit-session--categorize-elisp elisp)
                :success ,(not (string-match-p "^Error:" (format "%s" result))))))
          (seq-take log count)))

(defun efrit-session--categorize-elisp (elisp)
  "Categorize ELISP for summary purposes."
  (cond
   ((string-match-p "^(list" elisp) 'context-gathering)
   ((string-match-p "^(progn" elisp) 'execution)
   ((string-match-p "^(read-" elisp) 'user-input)
   (t 'other)))
```

## Key Features

1. **Non-blocking** - All API calls are async, Emacs stays responsive
2. **Session continuity** - Work log maintained across steps
3. **Queue management** - Commands queued during active sessions
4. **Progress feedback** - Mode line and message updates
5. **Error resilience** - Graceful error handling with session cleanup
6. **Context preservation** - Operations execute in original buffer

## Integration Points

- Replaces synchronous execution in current efrit-do
- Reuses existing efrit-tools-eval-sexp for execution
- Compatible with existing retry and context systems
- Maintains backward compatibility through API