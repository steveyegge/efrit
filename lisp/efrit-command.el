;;; efrit-command.el --- Efrit interactive commands -*- lexical-binding: t; -*-

;;; Commentary:
;; Implementation of efrit-command using the agent system for proper LLM interaction

;;; Code:

(require 'efrit-tools)
(require 'efrit-agent)

;; Basic customization options
(defcustom efrit-command-popup-height 10
  "Height of the Efrit command popup buffer in lines."
  :type 'integer
  :group 'efrit)

(defcustom efrit-command-popup-buffer-name "*efrit-command*"
  "Name of the Efrit command popup buffer."
  :type 'string
  :group 'efrit)

(defcustom efrit-command-output-buffer-name "*efrit-output*"
  "Name of the Efrit output buffer."
  :type 'string
  :group 'efrit)

;; Variables
(defvar-local efrit-command--source-buffer nil
  "Buffer that was active when Efrit command was invoked.")

(defvar-local efrit-command-last-query nil
  "Last query sent to the API from this buffer.")

;; Faces
(defface efrit-command-output-face
  '((t :inherit font-lock-doc-face))
  "Face for Efrit command output."
  :group 'efrit)

;; Logging function
(defun efrit-command--log (format-string &rest args)
  "Log a message to the Efrit output buffer with FORMAT-STRING and ARGS."
  (let ((output-buffer (get-buffer-create efrit-command-output-buffer-name)))
    (with-current-buffer output-buffer
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (insert (propertize (apply #'format (concat "[%s] " format-string "\n")
                                   (cons (format-time-string "%H:%M:%S") args))
                            'face 'efrit-command-output-face)))
      (when (and (get-buffer-window output-buffer) 
                 (window-live-p (get-buffer-window output-buffer)))
        (set-window-point (get-buffer-window output-buffer) (point-max)))))
  nil)

;; Setup functions
(defun efrit-command--setup-output-buffer ()
  "Set up the Efrit output buffer."
  (let ((buffer (get-buffer-create efrit-command-output-buffer-name)))
    (with-current-buffer buffer
      (unless (eq major-mode 'efrit-output-mode)
        (efrit-output-mode))
      (when (not (get-buffer-window buffer))
        (display-buffer buffer
                        '((display-buffer-in-side-window)
                          (side . bottom)
                          (slot . 1)
                          (window-height . 15)))))
    buffer))

(defun efrit-command--setup-popup-buffer ()
  "Set up the Efrit command popup buffer."
  (let ((buffer (get-buffer-create efrit-command-popup-buffer-name)))
    (with-current-buffer buffer
      (unless (eq major-mode 'efrit-command-mode)
        (efrit-command-mode))
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert "Enter your query below and press C-c C-c to send:\n\n")
      (goto-char (point-max)))
    buffer))

;; Core functionality
(defun efrit-command--send-request (query buffer)
  "Send QUERY to process for BUFFER using agent system for LLM interaction."
  (efrit-command--log "Processing: %s" query)
  
  ;; Store the query in the buffer's local variable
  (with-current-buffer buffer
    (setq-local efrit-command-last-query query))
  
  ;; Use the agent system to process the request
  ;; This implements the proper two-step LLM interaction:
  ;; 1. First LLM call: Generate a plan for the request
  ;; 2. Second+ LLM calls: Execute and refine the plan
  (condition-case err
      (progn
        (efrit-command--log "Using agent system for request processing")
        
        ;; Switch to the target buffer before running the agent
        (with-current-buffer buffer
          ;; Run the agent with the query
          (let ((agent-result (efrit-agent-run query buffer)))
            (efrit-command--log "Agent execution completed")
            
            ;; Check if there were any errors (agent-result is a hash table)
            (when (gethash "error" agent-result)
              (efrit-command--log "Agent reported error: %s" (gethash "error" agent-result)))
            
            ;; Log the summary if available
            (when (gethash "summary" agent-result)
              (efrit-command--log "Agent summary: %s" (gethash "summary" agent-result)))
            
            ;; Return the agent result
            agent-result)))
    (error
     (efrit-command--log "ERROR in agent processing: %s" (error-message-string err))
     ;; Fallback to simple processing for basic commands only
     (efrit-command--log "Falling back to simple command processing")
     (efrit-command--fallback-processing query buffer err))))

(defun efrit-command--fallback-processing (_query _buffer original-error)
  "Simple fallback when agent system fails - no client-side parsing.
Per Zero Client-Side Intelligence principle, just inform user of error."
  (efrit-command--log "Agent system failed: %s" (error-message-string original-error))
  
  ;; No client-side command parsing - just show error and suggest efrit-chat
  (let ((error-message (format "Efrit agent system unavailable. Error: %s. Try: M-x efrit-chat for full functionality." 
                              (error-message-string original-error))))
    (efrit-command--log "Showing error to user: %s" error-message)
    (message "%s" error-message)
    ;; Return basic error response
    (list (cons 'result error-message)
          (cons 'status "error")
          (cons 'summary "Agent system unavailable"))))

(defun efrit-command-send-query ()
  "Send the query in the popup buffer to the API."
  (interactive)
  (let* ((raw-text (buffer-substring-no-properties (point-min) (point-max)))
         (instruction-end (string-match "\n\n" raw-text))
         (query (if instruction-end
                    (string-trim (substring raw-text (+ instruction-end 2)))
                  (string-trim raw-text)))
         (source-buffer efrit-command--source-buffer))
    (if (string-empty-p query)
        (message "No query to send.")
      (progn
        (efrit-command--log "Query from user: %s" query)
        (efrit-command--send-request query source-buffer)))))

(defun efrit-command-quit ()
  "Close the Efrit command popup buffer."
  (interactive)
  (when efrit-command--source-buffer
    (switch-to-buffer efrit-command--source-buffer))
  (kill-buffer))

;;;###autoload
(defun efrit ()
  "Open the Efrit command interface to execute actions in the current buffer."
  (interactive)
  ;; Set up the output buffer
  (efrit-command--setup-output-buffer)
  
  ;; Remember the current buffer
  (let ((source-buffer (current-buffer)))
    ;; Set up and display the popup buffer
    (with-current-buffer (efrit-command--setup-popup-buffer)
      (setq-local efrit-command--source-buffer source-buffer)
      (let ((popup-window (display-buffer
                           (current-buffer)
                           '((display-buffer-in-side-window)
                             (side . top)
                             (window-height . fit-window-to-buffer)
                             (preserve-size . (nil . t))))))
        (when popup-window
          (select-window popup-window)
          (fit-window-to-buffer popup-window efrit-command-popup-height))))))

;; Mode definitions
(defvar efrit-command-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'efrit-command-send-query)
    (define-key map (kbd "C-c C-q") 'efrit-command-quit)
    map)
  "Keymap for Efrit command mode.")

(define-derived-mode efrit-command-mode text-mode "Efrit Command"
  "Major mode for interacting with the Efrit command interface."
  (visual-line-mode 1))

(defvar efrit-output-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for Efrit output mode.")

(define-derived-mode efrit-output-mode special-mode "Efrit Output"
  "Major mode for Efrit output buffer."
  (visual-line-mode 1))

;;;###autoload
(defun efrit-show-output ()
  "Show the Efrit output buffer."
  (interactive)
  (let ((buffer (get-buffer-create efrit-command-output-buffer-name)))
    (with-current-buffer buffer
      (unless (eq major-mode 'efrit-output-mode)
        (efrit-output-mode)))
    (display-buffer buffer
                    '((display-buffer-in-side-window)
                      (side . bottom)
                      (slot . 1)
                      (window-height . 15)))))

(provide 'efrit-command)
;;; efrit-command.el ends here