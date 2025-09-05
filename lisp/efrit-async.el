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

;; Forward declarations
(declare-function efrit-tools-eval-sexp "efrit-tools")
(declare-function efrit--api-headers "efrit-common")
(declare-function efrit-do--display-error "efrit-do")

;;; Session Data Structure

(cl-defstruct efrit-session
  "Session tracking for multi-step operations."
  id              ; Unique identifier from Claude
  work-log        ; List of (elisp . result) pairs  
  start-time      ; When session started
  status          ; 'active, 'waiting, 'complete
  buffer)         ; Original buffer for context

(defvar efrit-async--active-session nil
  "Currently active session.")

(defvar efrit-async--session-queue nil  
  "Queue of pending commands.")

;;; Customization

(defcustom efrit-async-show-progress t
  "Whether to show progress in mode line."
  :type 'boolean
  :group 'efrit)

;;; Progress Display

(defvar efrit-async-mode-line-string nil
  "Mode line indicator for active Efrit session.")

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
      (efrit-log 'info "Started session %s" session-id)))
  
  (when efrit-async--active-session
    (push (cons elisp result) 
          (efrit-session-work-log efrit-async--active-session))
    (efrit-log 'debug "Session %s: logged step %d" 
               session-id
               (length (efrit-session-work-log efrit-async--active-session)))))

(defun efrit-async--complete-session (session-id _result)
  "Mark session SESSION-ID complete with final RESULT and process queue."
  (efrit-async--show-progress "Complete!")
  (when efrit-async--active-session
    (let ((elapsed (float-time (time-since (efrit-session-start-time 
                                           efrit-async--active-session))))
          (steps (length (efrit-session-work-log efrit-async--active-session))))
      (message "Efrit: Session %s complete (%.1fs, %d steps)"
               session-id elapsed steps)
      (efrit-log 'info "Completed session %s: %.1fs, %d steps" 
                 session-id elapsed steps)))
  
  (setq efrit-async--active-session nil)
  (setq efrit-async-mode-line-string nil)
  (force-mode-line-update t)
  (efrit-async--process-queue))

(defun efrit-async--process-queue ()
  "Process next queued command if any."
  (when efrit-async--session-queue
    (pop efrit-async--session-queue)
    (message "Efrit: Processing queued command (%d remaining)" 
             (length efrit-async--session-queue))
    ;; This will be connected to efrit-do in the next step
    ))

;;; Work Log Compression

(defun efrit-session--compress-log (session)
  "Create compressed summary of SESSION work for Claude."
  (when session
    (let ((log (efrit-session-work-log session)))
      `((step-count . ,(length log))
        (elapsed-time . ,(float-time (time-since (efrit-session-start-time session))))
        (last-result . ,(when log (cdr (car log))))
        ;; Include key context without full elisp history
        (buffer-modified . ,(when (buffer-live-p (efrit-session-buffer session))
                             (with-current-buffer (efrit-session-buffer session)
                               (buffer-modified-p))))))))

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
    (setq efrit-async-mode-line-string nil)
    (force-mode-line-update t)
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

(defun efrit-async--api-request (data callback)
  "Send DATA to Claude API asynchronously, calling CALLBACK with response.
This is a minimal implementation for testing - will be expanded later."
  (efrit-log 'debug "Async API request: %S" data)
  (condition-case err
      ;; For now, just simulate async with a timer
      ;; Real implementation will use url-retrieve
      (run-with-timer 
       0.5 nil
       (lambda ()
         ;; Simulate a response
         (let ((response `((elisp . "(message \"Hello from async!\")")
                          (session . ((id . "test-session")
                                    (status . "complete"))))))
           (funcall callback response))))
    (error 
     (efrit-log 'error "API request failed: %s" (error-message-string err))
     (efrit-async--handle-error err))))

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
    (force-mode-line-update t)))

(defun efrit-async--handle-response (response)
  "Handle RESPONSE from Claude - execute elisp and check status.
Minimal implementation for testing."
  (efrit-log 'debug "Handling response: %S" response)
  (let* ((elisp (alist-get 'elisp response))
         (session-info (alist-get 'session response))
         (session-id (alist-get 'id session-info))
         (status (alist-get 'status session-info)))
    
    ;; Execute the elisp
    (let ((result (if elisp
                      (eval (car (read-from-string elisp)))
                    "No elisp provided")))
      
      ;; Update session
      (when elisp
        (efrit-async--update-session session-id result elisp))
      
      ;; Handle status
      (cond
       ((equal status "continue")
        (efrit-async--show-progress "Continuing...")
        (message "Would continue session %s here" session-id))
       
       ((equal status "complete")
        (efrit-async--complete-session session-id result))
       
       ((equal status "need-input")
        (message "Would wait for input here"))))))

(provide 'efrit-async)
;;; efrit-async.el ends here