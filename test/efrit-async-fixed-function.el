;;; Fixed version of efrit-async-execute-command

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
          
          ;; Track session for memory management
          (puthash session-id efrit-async--active-session efrit-async--sessions)
          (efrit-performance-touch-session session-id)
          
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
                  (when callback (funcall callback result))))))))))))

;; Count parens in this function:
;; Opens: defun=1, if=1, progn=1, let*=3, if=1, progn=1, let=1, lambda=2 = 11 total
;; That should equal closes