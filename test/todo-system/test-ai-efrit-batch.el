;;; test-ai-efrit-batch.el --- Batch test with ai-efrit -*- lexical-binding: t -*-

(add-to-list 'load-path (expand-file-name "lisp" default-directory))
(require 'efrit)

;; Configure for ai-efrit channel
(setq efrit-api-channel "ai-efrit")
(setq efrit-api-key nil) ; Use ai-efrit channel
(setq efrit-log-level 'debug)

;; Test the API configuration
(message "\n=== Testing ai-efrit channel configuration ===")
(message "API Channel: %s" efrit-api-channel)
(message "API URL: %s" efrit-api-url)

;; Create warnings
(with-current-buffer (get-buffer-create "*Warnings*")
  (erase-buffer)
  (insert "Warning: file1.el:1: Warning: file `file1.el' lacks lexical-binding directive
Warning: file2.el:1: Warning: file `file2.el' lacks lexical-binding directive")
  (goto-char (point-min)))

;; Try to get the tools schema for ai-efrit
(condition-case err
    (let ((url-request-method "GET")
          (url-request-extra-headers 
           `(("x-api-channel" . ,efrit-api-channel)
             ("Content-Type" . "application/json"))))
      (message "Checking ai-efrit channel availability...")
      
      ;; Test with a simple completion
      (let* ((test-data `((model . ,efrit-model)
                         (max_tokens . 100)
                         (messages . [((role . "user") (content . "Say 'test successful'"))])
                         (system . "You are a test bot. Reply with exactly: test successful")))
             (json-data (json-encode test-data))
             (url-request-method "POST")
             (url-request-data json-data))
        
        (message "Sending test request to ai-efrit channel...")
        (let ((response-buffer (url-retrieve-synchronously efrit-api-url nil t 5)))
          (if response-buffer
              (with-current-buffer response-buffer
                (goto-char (point-min))
                (when (search-forward-regexp "^$" nil t)
                  (let ((response (buffer-substring (point) (point-max))))
                    (message "Response received: %s" (substring response 0 (min 200 (length response))))
                    (kill-buffer)))
                (message "ai-efrit channel appears to be working!"))
            (message "Failed to get response from ai-efrit channel")))))
  (error 
   (message "Error testing ai-efrit channel: %s" (error-message-string err))))

;; Now test the async command with TODOs
(message "\n=== Testing TODO workflow ===")
(efrit-do-clear-todos)

(message "\nTo test the full workflow interactively:")
(message "1. Load this file in Emacs")
(message "2. M-x efrit-do-async RET fix all warnings in *Warnings* buffer RET")
(message "3. M-x efrit-progress-show to see progress")
(message "4. M-x efrit-async-show-todos to see TODO list")
(message "5. Watch if it creates TODOs and works through them without looping")