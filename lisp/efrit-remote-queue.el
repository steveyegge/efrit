;;; efrit-remote-queue.el --- File-based remote queue for AI interaction -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (filenotify "0.2"))
;; Keywords: tools, convenience, ai, queue
;; URL: https://github.com/stevey/efrit

;;; Commentary:
;; This provides a file-based remote queue system that allows AI systems
;; to interact with Efrit by writing JSON request files to a watched directory.
;; Efrit processes these requests and writes JSON response files.

;;; Code:

(require 'filenotify)
(require 'json)
(require 'efrit-tools)

;; Conditional requires for efrit subsystems
(declare-function efrit-do "efrit-do")
(declare-function efrit-streamlined-send "efrit-chat-streamlined")

;; Variable declarations
(defvar efrit-do-show-results)

;;; Customization

(defgroup efrit-remote-queue nil
  "File-based remote queue for AI interaction with Efrit."
  :group 'efrit
  :prefix "efrit-remote-queue-")

(defcustom efrit-remote-queue-directory "~/.emacs.d/efrit-queue"
  "Root directory for the remote queue system."
  :type 'directory
  :group 'efrit-remote-queue)

(defcustom efrit-remote-queue-max-file-size (* 1024 1024)
  "Maximum size in bytes for request/response files (1MB default)."
  :type 'integer
  :group 'efrit-remote-queue)

(defcustom efrit-remote-queue-timeout 30
  "Default timeout in seconds for request processing."
  :type 'integer
  :group 'efrit-remote-queue)

(defcustom efrit-remote-queue-cleanup-delay 300
  "Seconds to wait before cleaning up processed files (5 minutes default)."
  :type 'integer
  :group 'efrit-remote-queue)

(defcustom efrit-remote-queue-max-concurrent 10
  "Maximum number of concurrent requests to process."
  :type 'integer
  :group 'efrit-remote-queue)

(defcustom efrit-remote-queue-debug nil
  "Enable debug logging for queue operations."
  :type 'boolean
  :group 'efrit-remote-queue)

;;; Internal variables

(defvar efrit-remote-queue--watcher nil
  "File watcher descriptor for the requests directory.")

(defvar efrit-remote-queue--active nil
  "Whether the queue system is currently active.")

(defvar efrit-remote-queue--processing (make-hash-table :test 'equal)
  "Hash table of currently processing requests.")

(defvar efrit-remote-queue--stats
  '((requests-processed . 0)
    (requests-succeeded . 0)
    (requests-failed . 0)
    (total-processing-time . 0.0)
    (started-at . nil))
  "Statistics for queue operations.")

;;; Directory management

(defun efrit-remote-queue--ensure-directories ()
  "Ensure all required queue directories exist."
  (let ((queue-dir (expand-file-name efrit-remote-queue-directory)))
    (dolist (subdir '("requests" "responses" "processing" "archive"))
      (let ((dir (expand-file-name subdir queue-dir)))
        (unless (file-directory-p dir)
          (make-directory dir t))))))

(defun efrit-remote-queue--get-directory (type)
  "Get the full path for a queue directory of TYPE."
  (expand-file-name 
   type 
   (expand-file-name efrit-remote-queue-directory)))

;;; Request/Response file handling

(defun efrit-remote-queue--generate-id ()
  "Generate a unique request ID."
  (format "efrit_%d_%d" 
          (floor (float-time))
          (random 10000)))

(defun efrit-remote-queue--parse-request-file (file-path)
  "Parse a request file at FILE-PATH and return the request object."
  (when (and (file-readable-p file-path)
             (< (file-attribute-size (file-attributes file-path))
                efrit-remote-queue-max-file-size))
    (condition-case err
        (with-temp-buffer
          (insert-file-contents file-path)
          (let ((json-object-type 'hash-table)
                (json-array-type 'vector)
                (json-key-type 'string))
            (json-read-from-string (buffer-string))))
      (error
       (when efrit-remote-queue-debug
         (message "Error parsing request file %s: %s" file-path (error-message-string err)))
       nil))))

(defun efrit-remote-queue--write-response-file (request-id response-data)
  "Write RESPONSE-DATA to a response file for REQUEST-ID."
  (let* ((response-file (expand-file-name 
                        (format "resp_%s.json" request-id)
                        (efrit-remote-queue--get-directory "responses")))
         (json-encoding-pretty-print t))
    (condition-case err
        (with-temp-file response-file
          (insert (json-encode response-data)))
      (error
       (when efrit-remote-queue-debug
         (message "Error writing response file %s: %s" response-file (error-message-string err)))
       nil))))

(defun efrit-remote-queue--move-to-processing (file-path)
  "Move request file from requests to processing directory."
  (let* ((filename (file-name-nondirectory file-path))
           (processing-path (expand-file-name filename (efrit-remote-queue--get-directory "processing"))))
    (condition-case err
        (progn
          (rename-file file-path processing-path)
          processing-path)
      (error
       (when efrit-remote-queue-debug
         (message "Error moving file to processing: %s" (error-message-string err)))
       nil))))

(defun efrit-remote-queue--cleanup-processed-file (file-path &optional delay)
  "Clean up a processed file after optional DELAY seconds."
  (if delay
      (run-at-time delay nil #'efrit-remote-queue--cleanup-processed-file file-path)
    (condition-case err
        (when (file-exists-p file-path)
          (delete-file file-path))
      (error
       (when efrit-remote-queue-debug
         (message "Error cleaning up file %s: %s" file-path (error-message-string err)))))))

(defun efrit-remote-queue--startup-cleanup ()
  "Clean up any stale files from previous sessions on startup."
  (let ((cleanup-count 0))
    (dolist (subdir '("processing" "requests" "responses"))
      (let ((dir (efrit-remote-queue--get-directory subdir)))
        (when (file-directory-p dir)
          (dolist (file (directory-files dir t "\\.json$"))
            (when (file-regular-p file)
              ;; Clean up files older than cleanup delay
              (let* ((file-time (nth 5 (file-attributes file)))
                     (age (float-time (time-subtract (current-time) file-time))))
                (when (> age efrit-remote-queue-cleanup-delay)
                  (condition-case err
                      (progn
                        (delete-file file)
                        (cl-incf cleanup-count)
                        (when efrit-remote-queue-debug
                          (message "Cleaned up stale file: %s" file)))
                    (error
                     (when efrit-remote-queue-debug
                       (message "Error cleaning up stale file %s: %s" 
                               file (error-message-string err))))))))))))
    (when (and efrit-remote-queue-debug (> cleanup-count 0))
      (message "Startup cleanup: removed %d stale files" cleanup-count))))

;;; Request processing

(defun efrit-remote-queue--validate-request (request)
  "Validate a REQUEST object. Return (valid-p . error-message)."
  (cond
   ((not (hash-table-p request))
    (cons nil "Request is not a valid JSON object"))
   
   ((not (gethash "id" request))
    (cons nil "Request missing required 'id' field"))
   
   ((not (gethash "content" request))
    (cons nil "Request missing required 'content' field"))
   
   ((not (member (gethash "type" request) '("command" "chat" "eval")))
    (cons nil "Request 'type' must be one of: command, chat, eval"))
   
   (t (cons t nil))))

(defun efrit-remote-queue--create-response (request-id status &optional result error execution-time context)
  "Create a response hash table for REQUEST-ID."
  (let ((response (make-hash-table :test 'equal)))
    (puthash "id" request-id response)
    (puthash "timestamp" (format-time-string "%Y-%m-%dT%H:%M:%SZ") response)
    (puthash "status" status response)
    (when result (puthash "result" result response))
    (when error (puthash "error" error response))
    (when execution-time (puthash "execution_time" execution-time response))
    (when context (puthash "context" context response))
    response))

(defun efrit-remote-queue--process-request (request _file-path)
  "Process a REQUEST from FILE-PATH and return response data."
  (let* ((request-id (gethash "id" request))
         (request-type (gethash "type" request))
         (content (gethash "content" request))
         (options (gethash "options" request))
         (timeout (or (and options (gethash "timeout" options)) efrit-remote-queue-timeout))
         (return-context (and options (gethash "return_context" options)))
         (start-time (float-time))
         response)
    
    (when efrit-remote-queue-debug
      (message "Processing %s request %s: %s" request-type request-id content))
    
    ;; Mark as processing
    (puthash request-id start-time efrit-remote-queue--processing)
    
    (condition-case err
        (let ((result
               (with-timeout (timeout (error "Request timed out"))
                 (cond
                  ((string= request-type "command")
                   (efrit-remote-queue--execute-command content))
                  
                  ((string= request-type "chat")
                   (efrit-remote-queue--execute-chat content))
                  
                  ((string= request-type "eval")
                   (efrit-remote-queue--execute-eval content))
                  
                  (t (error "Unknown request type: %s" request-type))))))
          
          (let ((execution-time (- (float-time) start-time))
                (context (when return-context (efrit-tools-get-context))))
            
            (setq response (efrit-remote-queue--create-response 
                           request-id "success" result nil execution-time context))
            
            ;; Update stats
            (cl-incf (alist-get 'requests-succeeded efrit-remote-queue--stats))
            (cl-incf (alist-get 'total-processing-time efrit-remote-queue--stats) execution-time)))
      
      (error
       (let ((execution-time (- (float-time) start-time))
             (error-msg (error-message-string err)))
         (setq response (efrit-remote-queue--create-response 
                        request-id "error" nil error-msg execution-time))
         
         ;; Update stats
         (cl-incf (alist-get 'requests-failed efrit-remote-queue--stats))
         (cl-incf (alist-get 'total-processing-time efrit-remote-queue--stats) execution-time)
         
         (when efrit-remote-queue-debug
           (message "Error processing request %s: %s" request-id error-msg)))))
    
    ;; Clean up processing tracking
    (remhash request-id efrit-remote-queue--processing)
    (cl-incf (alist-get 'requests-processed efrit-remote-queue--stats))
    
    response))

;;; Request execution handlers

(defun efrit-remote-queue--execute-command (content)
  "Execute a command-type request with CONTENT."
  (if (fboundp 'efrit-do)
      (condition-case err
          ;; Capture the result by temporarily redirecting efrit-do output
          (let ((original-show-results efrit-do-show-results)
                result-captured)
            (setq efrit-do-show-results nil) ; Don't show UI during remote execution
            (unwind-protect
                (progn
                  ;; Execute the command and capture output
                  (with-temp-buffer
                    (let ((standard-output (current-buffer)))
                      (efrit-do content)
                      (setq result-captured (buffer-string))))
                  (if (string-empty-p result-captured)
                      "Command executed successfully"
                    result-captured))
              (setq efrit-do-show-results original-show-results)))
        (error
         (format "Command execution failed: %s" (error-message-string err))))
    (error "efrit-do not available")))

(defun efrit-remote-queue--execute-chat (content)
  "Execute a chat-type request with CONTENT."
  (condition-case err
      (progn
        ;; Load efrit-chat-streamlined if not already loaded
        (unless (fboundp 'efrit-streamlined-send)
          (require 'efrit-chat-streamlined))
        
        ;; Execute the chat request
        ;; Note: This is a simplified version - real implementation would need
        ;; to handle async responses properly
        (efrit-streamlined-send content)
        "Chat request sent to efrit-streamlined-send")
    (error
     (format "Chat execution failed: %s" (error-message-string err)))))

(defun efrit-remote-queue--execute-eval (content)
  "Execute an eval-type request with CONTENT."
  (condition-case err
      (let ((result (efrit-tools-eval-sexp content)))
        (prin1-to-string result))
    (error
     (format "Eval failed: %s" (error-message-string err)))))

;;; File monitoring and event handling

(defun efrit-remote-queue--on-file-event (event)
  "Handle file system EVENT in the requests directory."
  (let* ((event-type (nth 1 event))
         (file-path (nth 2 event))
         (filename (file-name-nondirectory file-path)))
    
    (when (and (member event-type '(created changed))
               (string-match-p "\\.json$" filename)
               (not (string-match-p "^\\." filename))) ; Ignore hidden files
      
      (when efrit-remote-queue-debug
        (message "Queue event: %s on %s" event-type filename))
      
      ;; Process the request file (with a small delay to ensure file is fully written)
      (run-at-time 0.1 nil #'efrit-remote-queue--process-file file-path))))

(defun efrit-remote-queue--process-file (file-path)
  "Process a single request FILE-PATH."
  (when (and (file-exists-p file-path)
             (< (hash-table-count efrit-remote-queue--processing)
                efrit-remote-queue-max-concurrent))
    
    (let ((request (efrit-remote-queue--parse-request-file file-path)))
      (if request
          (let ((validation (efrit-remote-queue--validate-request request)))
            (if (car validation)
                ;; Valid request - process it
                (let ((processing-path (efrit-remote-queue--move-to-processing file-path)))
                  (when processing-path
                    (let ((response (efrit-remote-queue--process-request request processing-path)))
                      (when response
                        (efrit-remote-queue--write-response-file 
                         (gethash "id" request) response))
                      ;; Clean up processing file
                      (efrit-remote-queue--cleanup-processed-file 
                       processing-path efrit-remote-queue-cleanup-delay))))
              
              ;; Invalid request - write error response
              (let* ((request-id (or (gethash "id" request) "unknown"))
                     (error-response (efrit-remote-queue--create-response 
                                     request-id "error" nil (cdr validation))))
                (efrit-remote-queue--write-response-file request-id error-response)
                (efrit-remote-queue--cleanup-processed-file file-path 1))))
        
        ;; Failed to parse - clean up malformed file
        (efrit-remote-queue--cleanup-processed-file file-path 1)))))

;;; Public API

;;;###autoload
(defun efrit-remote-queue-start ()
  "Start the remote queue system."
  (interactive)
  (when efrit-remote-queue--active
    (user-error "Remote queue is already active"))
  
  (condition-case err
      (progn
        ;; Ensure directories exist
        (efrit-remote-queue--ensure-directories)
        
        ;; Clean up any stale files from previous sessions
        (efrit-remote-queue--startup-cleanup)
        
        ;; Set up file watcher
        (let ((requests-dir (efrit-remote-queue--get-directory "requests")))
          (setq efrit-remote-queue--watcher
                (file-notify-add-watch requests-dir
                                     '(change attribute-change)
                                     #'efrit-remote-queue--on-file-event)))
        
        ;; Mark as active and reset stats
        (setq efrit-remote-queue--active t)
        (setf (alist-get 'started-at efrit-remote-queue--stats) (current-time))
        
        (message "Efrit remote queue started - watching %s" 
                 (efrit-remote-queue--get-directory "requests")))
    
    (error
     (efrit-remote-queue-stop)
     (user-error "Failed to start remote queue: %s" (error-message-string err)))))

;;;###autoload
(defun efrit-remote-queue-stop ()
  "Stop the remote queue system."
  (interactive)
  (when efrit-remote-queue--watcher
    (file-notify-rm-watch efrit-remote-queue--watcher)
    (setq efrit-remote-queue--watcher nil))
  
  (setq efrit-remote-queue--active nil)
  (clrhash efrit-remote-queue--processing)
  
  (message "Efrit remote queue stopped"))

;;;###autoload
(defun efrit-remote-queue-status ()
  "Show status of the remote queue system."
  (interactive)
  (if efrit-remote-queue--active
      (let* ((stats efrit-remote-queue--stats)
             (processed (alist-get 'requests-processed stats))
             (succeeded (alist-get 'requests-succeeded stats))
             (failed (alist-get 'requests-failed stats))
             (total-time (alist-get 'total-processing-time stats))
             (started-at (alist-get 'started-at stats))
             (uptime (if started-at (float-time (time-subtract (current-time) started-at)) 0))
             (avg-time (if (> processed 0) (/ total-time processed) 0))
             (processing-count (hash-table-count efrit-remote-queue--processing)))
        
        (message "Queue Status: %d processed (%d ok, %d failed), %d processing, %.1fs avg, %.0fs uptime"
                 processed succeeded failed processing-count avg-time uptime))
    (message "Remote queue is not active")))

;;;###autoload
(defun efrit-remote-queue-process (request-file)
  "Process a single REQUEST-FILE and return the response file path.
This is the main entry point for external AI systems to communicate with Efrit."
  (when (not (file-exists-p request-file))
    (error "Request file does not exist: %s" request-file))
  
  (let* ((request (efrit-remote-queue--parse-request-file request-file))
         (response-file (concat (file-name-sans-extension request-file) "_response.json")))
    
    (if request
        (let ((validation (efrit-remote-queue--validate-request request)))
          (if (car validation)
              ;; Valid request - process it
              (let ((response (efrit-remote-queue--process-request request request-file)))
                (when response
                  (efrit-remote-queue--write-response-file 
                   (gethash "id" request) response)
                  response-file))
            ;; Invalid request
            (let* ((request-id (or (gethash "id" request) "unknown"))
                   (error-response (efrit-remote-queue--create-response 
                                   request-id "error" nil (cdr validation))))
              (efrit-remote-queue--write-response-file request-id error-response)
              response-file)))
      ;; Failed to parse
      (let ((error-response (efrit-remote-queue--create-response 
                            "unknown" "error" nil "Failed to parse request file")))
        (efrit-remote-queue--write-response-file "unknown" error-response)
        response-file))))

;;;###autoload
(defun efrit-remote-queue-reset ()
  "Reset queue statistics and clean up old files."
  (interactive)
  (when (y-or-n-p "Reset queue statistics and clean up files? ")
    ;; Reset stats
    (setq efrit-remote-queue--stats
          '((requests-processed . 0)
            (requests-succeeded . 0) 
            (requests-failed . 0)
            (total-processing-time . 0.0)
            (started-at . nil)))
    
    ;; Clean up directories
    (dolist (dir-type '("responses" "processing" "archive"))
      (let ((dir (efrit-remote-queue--get-directory dir-type)))
        (when (file-directory-p dir)
          (dolist (file (directory-files dir t "\\.json$"))
            (delete-file file)))))
    
    (message "Queue reset complete")))

(provide 'efrit-remote-queue)
;;; efrit-remote-queue.el ends here
