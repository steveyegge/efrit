;;; efrit-protocol.el --- Shared protocol definitions for Efrit -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; This module defines shared protocols and interfaces used by multiple
;; Efrit modules, breaking circular dependencies between efrit-async
;; and efrit-do.

;;; Code:

(require 'cl-lib)
(require 'efrit-common)
(require 'efrit-log)

;;; Session Data Structure

(cl-defstruct efrit-session
  "Session tracking for multi-step operations."
  id              ; Unique identifier from Claude
  command         ; Original command from user
  work-log        ; List of (elisp . result) pairs  
  start-time      ; When session started
  status          ; 'active, 'waiting, 'complete
  buffer)         ; Original buffer for context

;;; Tool Execution Protocol

(defun efrit-protocol-execute-tool (tool-name input-data)
  "Execute TOOL-NAME with INPUT-DATA.
This is a dispatcher that routes to the appropriate tool handler.
Returns the result of the tool execution or signals an error."
  (efrit-log 'debug "Executing tool: %s" tool-name)
  
  ;; Import the tool executor from efrit-do
  (require 'efrit-do)
  (require 'efrit-progress)
  
  ;; Show tool start in progress buffer
  (efrit-progress-show-tool-start tool-name input-data)
  
  ;; Create tool item in the format expected by efrit-do
  (let ((tool-item (make-hash-table :test 'equal)))
    (puthash "name" tool-name tool-item)
    (puthash "input" input-data tool-item)
    
    (condition-case err
        (let ((result (efrit-do--execute-tool tool-item)))
          ;; Show successful result
          (efrit-progress-show-tool-result tool-name result t)
          result)
      (error
       ;; Show error result
       (let ((error-msg (error-message-string err)))
         (efrit-progress-show-tool-result tool-name error-msg nil)
         (signal (car err) (cdr err)))))))

;;; API Communication Protocol

(defun efrit-protocol-make-api-request (messages &optional session-data)
  "Make an API request with MESSAGES and optional SESSION-DATA.
Returns the parsed response data."
  (let* ((api-key (efrit-common-get-api-key))
         (model (or (bound-and-true-p efrit-model) 
                   "claude-3-5-sonnet-20241022"))
         (max-tokens (or (bound-and-true-p efrit-max-tokens) 8192))
         (request-data `(("model" . ,model)
                        ("max_tokens" . ,max-tokens)
                        ("messages" . ,messages))))
    
    ;; Add tools if available
    (when (bound-and-true-p efrit-do--tools-schema)
      (push `("tools" . ,efrit-do--tools-schema) request-data))
    
    ;; Add session data if provided
    (when session-data
      (push `("metadata" . (("session_id" . ,(efrit-session-id session-data))
                           ("work_log_size" . ,(length (efrit-session-work-log session-data)))))
            request-data))
    
    ;; Make the request
    (let* ((json-data (efrit-common-escape-json-unicode 
                      (json-encode request-data)))
           (url-request-method "POST")
           (url-request-extra-headers (efrit-common-build-headers api-key))
           (url-request-data json-data)
           (response-buffer (url-retrieve-synchronously
                            efrit-common-api-url nil t)))
      
      (unless response-buffer
        (error "Failed to get response from API"))
      
      (with-current-buffer response-buffer
        (goto-char (point-min))
        (re-search-forward "\n\n" nil t)
        (let ((response-data (json-read)))
          (kill-buffer)
          response-data)))))

;;; Response Processing Protocol

(defun efrit-protocol-process-response (response-data)
  "Process RESPONSE-DATA from Claude API.
Returns a plist with :content, :tool-calls, and :stop-reason."
  (let* ((content-array (cdr (assoc 'content response-data)))
         (stop-reason (cdr (assoc 'stop_reason response-data)))
         (text-content "")
         (tool-calls '()))
    
    ;; Extract text and tool calls
    (dolist (item content-array)
      (let ((item-type (cdr (assoc 'type item))))
        (cond
         ((string= item-type "text")
          (setq text-content (concat text-content (cdr (assoc 'text item)))))
         ((string= item-type "tool_use")
          (push item tool-calls)))))
    
    (list :content text-content
          :tool-calls (nreverse tool-calls)
          :stop-reason stop-reason)))

;;; Session Management Protocol

(defun efrit-protocol-should-continue-session-p (response-data work-log)
  "Determine if session should continue based on RESPONSE-DATA and WORK-LOG.
Returns t if session should continue, nil otherwise."
  (let* ((processed (efrit-protocol-process-response response-data))
         (stop-reason (plist-get processed :stop-reason))
         (tool-calls (plist-get processed :tool-calls)))
    
    ;; Check for explicit completion
    (not (or (string= stop-reason "end_turn")
             (seq-some (lambda (tool-call)
                        (string= (cdr (assoc 'name tool-call)) 
                                "session_complete"))
                      tool-calls)
             ;; Safety: don't continue forever
             (> (length work-log) 20)))))

(provide 'efrit-protocol)
;;; efrit-protocol.el ends here