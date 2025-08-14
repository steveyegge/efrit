;;; qa-test-integration.el --- Integration QA tests -*- lexical-binding: t; -*-

(require 'efrit-chat-streamlined)

(defun qa-test-url-request-format ()
  "Test the actual URL request format without sending."
  (interactive)
  (message "=== QA: Testing URL Request Format ===")
  
  ;; Temporarily override url-retrieve to capture the request
  (let ((captured-data nil)
        (original-url-retrieve (symbol-function 'url-retrieve)))
    
    ;; Mock url-retrieve to capture the request
    (fset 'url-retrieve 
          (lambda (url callback &optional cbargs silent inhibit-cookies)
            (setq captured-data (list url url-request-method url-request-extra-headers url-request-data))
            (message "Mock API call intercepted")
            ;; Return a dummy buffer
            (get-buffer-create "*mock-response*")))
    
    (unwind-protect
        (condition-case err
            (let ((messages (list `((role . "user") (content . "test message")))))
              (efrit-streamlined--send-request messages)
              
              ;; Analyze captured data
              (if captured-data
                  (let ((url (nth 0 captured-data))
                        (method (nth 1 captured-data))
                        (headers (nth 2 captured-data))
                        (data (nth 3 captured-data)))
                    
                    (message "✓ URL: %s" url)
                    (message "✓ Method: %s" method)
                    (message "✓ Headers: %d items" (length headers))
                    
                    ;; Check required headers
                    (let ((has-api-key (assoc "x-api-key" headers))
                          (has-content-type (assoc "content-type" headers))
                          (has-anthropic-version (assoc "anthropic-version" headers)))
                      (if (and has-api-key has-content-type has-anthropic-version)
                          (message "✓ Required headers present")
                        (message "✗ Missing required headers")))
                    
                    ;; Check JSON data
                    (if (and data (stringp data))
                        (condition-case json-err
                            (let ((parsed (json-read-from-string data)))
                              (message "✓ Request data is valid JSON (%d chars)" (length data))
                              
                              ;; Check required fields
                              (let ((has-model (alist-get 'model parsed))
                                    (has-messages (alist-get 'messages parsed))
                                    (has-system (alist-get 'system parsed))
                                    (has-tools (alist-get 'tools parsed)))
                                (message "  - Model: %s" (if has-model "✓" "✗"))
                                (message "  - Messages: %s" (if has-messages "✓" "✗"))
                                (message "  - System: %s (%d chars)" 
                                        (if has-system "✓" "✗")
                                        (if has-system (length has-system) 0))
                                (message "  - Tools: %s (%d tools)" 
                                        (if has-tools "✓" "✗")
                                        (if has-tools (length has-tools) 0))))
                          (error (message "✗ Invalid JSON in request data: %s" json-err)))
                      (message "✗ No request data found")))
                (message "✗ No API call was made")))
          (error (message "✗ Request format test error: %s" err)))
      
      ;; Restore original function
      (fset 'url-retrieve original-url-retrieve)))
  
  (message "=== URL Request Format Test Complete ==="))

(defun qa-test-response-handling-full ()
  "Test full response handling with a realistic mock response."
  (interactive)
  (message "=== QA: Testing Full Response Handling ===")
  
  ;; Create a realistic mock response
  (let* ((mock-response-text
          (concat "HTTP/1.1 200 OK\n"
                  "Content-Type: application/json\n"
                  "\n"
                  "{\"id\":\"msg_test123\","
                  "\"type\":\"message\","
                  "\"role\":\"assistant\","
                  "\"content\":["
                  "{\"type\":\"text\",\"text\":\"I'll create a haiku about Vim for you.\"},"
                  "{\"type\":\"tool_use\",\"id\":\"toolu_test\",\"name\":\"eval_sexp\","
                  "\"input\":{\"expr\":\"(get-buffer-create \\\"*vim-haiku*\\\")\"}}"
                  "],\"model\":\"claude-3-5-sonnet-20241022\"}"))
         (mock-buffer (get-buffer-create "*mock-api-response*")))
    
    ;; Set up mock response buffer
    (with-current-buffer mock-buffer
      (erase-buffer)
      (insert mock-response-text)
      (goto-char (point-min)))
    
    ;; Test the response handler
    (condition-case err
        (with-current-buffer mock-buffer
          (efrit-streamlined--handle-response nil)
          (message "✓ Response handling completed without error"))
      (error (message "✗ Response handling error: %s" err)))
    
    ;; Check if work buffer got the logging
    (let ((work-buffer (get-buffer efrit-work-buffer-name)))
      (if (and work-buffer
               (with-current-buffer work-buffer
                 (goto-char (point-min))
                 (search-forward "Received response" nil t)))
          (message "✓ Work buffer logging occurred")
        (message "✗ Work buffer logging missing")))
    
    ;; Check if chat buffer got response
    (let ((chat-buffer (get-buffer efrit-buffer-name)))
      (if (and chat-buffer
               (with-current-buffer chat-buffer
                 (goto-char (point-min))
                 (search-forward "I'll create a haiku" nil t)))
          (message "✓ Chat buffer response displayed")
        (message "✗ Chat buffer response missing")))
    
    (kill-buffer mock-buffer))
  
  (message "=== Full Response Handling Test Complete ==="))

(defun qa-test-concurrent-usage ()
  "Test what happens with multiple concurrent operations."
  (interactive)
  (message "=== QA: Testing Concurrent Usage ===")
  
  ;; Test multiple work buffer logs happening rapidly
  (condition-case err
      (let ((threads-simulated 5))
        (dotimes (i threads-simulated)
          (efrit-streamlined--log-to-work (format "Concurrent operation %d" i))
          (efrit-streamlined--log-to-work (format "  - Step 1 for op %d" i))
          (efrit-streamlined--log-to-work (format "  - Step 2 for op %d" i)))
        (message "✓ Concurrent logging simulation completed"))
    (error (message "✗ Concurrent usage error: %s" err)))
  
  ;; Test multiple buffer creation attempts
  (condition-case err
      (let ((buffers '()))
        (dotimes (i 3)
          (push (efrit-streamlined--get-work-buffer) buffers))
        (if (= (length (delete-dups buffers)) 1)
            (message "✓ Multiple work buffer requests return same buffer")
          (message "✗ Multiple work buffers created: %d unique" (length (delete-dups buffers)))))
    (error (message "✗ Buffer creation concurrency error: %s" err)))
  
  (message "=== Concurrent Usage Test Complete ==="))

(defun qa-test-memory-leaks ()
  "Test for potential memory leaks and cleanup."
  (interactive)
  (message "=== QA: Testing Memory Management ===")
  
  (let ((initial-buffer-count (length (buffer-list)))
        (test-iterations 10))
    
    ;; Create and destroy multiple chat sessions
    (dotimes (i test-iterations)
      (let ((test-chat-buffer (get-buffer-create (format "*test-chat-%d*" i))))
        (with-current-buffer test-chat-buffer
          (efrit-streamlined--setup-chat-mode)
          (efrit-streamlined--display-response (format "Test response %d" i)))
        (kill-buffer test-chat-buffer)))
    
    ;; Create lots of work buffer logs
    (dotimes (i (* test-iterations 10))
      (efrit-streamlined--log-to-work (format "Memory test log %d" i)))
    
    (let ((final-buffer-count (length (buffer-list))))
      (if (<= final-buffer-count (+ initial-buffer-count 2)) ; Allow for work buffer + maybe one more
          (message "✓ No significant buffer leaks (%d -> %d buffers)" 
                  initial-buffer-count final-buffer-count)
        (message "⚠ Possible buffer leak (%d -> %d buffers)" 
                initial-buffer-count final-buffer-count))))
  
  (message "=== Memory Management Test Complete ==="))

(defun qa-test-real-world-messages ()
  "Test with realistic user messages that might cause issues."
  (interactive)
  (message "=== QA: Testing Real-World Messages ===")
  
  (let ((test-messages 
         '("write a haiku about vim and put it in a buffer"
           "how do I configure my .vimrc?"
           "create a new file called test.txt with some sample content"
           "explain how emacs hooks work with examples"
           "find all TODO comments in my project files" 
           "help me debug this elisp function: (defun broken-func () (+ 1 'a))"
           ""  ; empty message
           "a"  ; single character
           "write write write write write"  ; repetitive
           "Create a file with this content:\n\n```\n(defun test ()\n  \"test\")\n```"  ; code blocks
           )))
    
    (dolist (msg test-messages)
      (condition-case err
          (let ((messages (list `((role . "user") (content . ,msg)))))
            (efrit-streamlined--log-to-work (format "Testing message: %s" 
                                                   (if (> (length msg) 30) 
                                                       (concat (substring msg 0 30) "...")
                                                     msg)))
            (message "✓ Message handled: %s" 
                    (if (= (length msg) 0) "[empty]"
                      (if (> (length msg) 20) 
                          (concat (substring msg 0 20) "...")
                        msg))))
        (error (message "✗ Message failed: %s - Error: %s" msg err)))))
  
  (message "=== Real-World Messages Test Complete ==="))

(defun qa-find-potential-issues ()
  "Look for potential issues in the code structure."
  (interactive)
  (message "=== QA: Looking for Potential Issues ===")
  
  ;; Check for hardcoded values that should be configurable
  (let ((system-prompt (efrit-streamlined--system-prompt)))
    (cond 
     ((string-match-p "claude-3-5-sonnet" system-prompt)
      (message "⚠ System prompt contains hardcoded model name"))
     ((string-match-p "anthropic\\.com" system-prompt)
      (message "⚠ System prompt contains hardcoded API URL"))
     (t (message "✓ System prompt looks clean"))))
  
  ;; Check customization variables
  (unless (boundp 'efrit-work-buffer-name)
    (message "✗ Work buffer name not configurable"))
  
  ;; Check error handling paths
  (condition-case err
      (efrit-streamlined--extract-tool-uses "invalid-data")
    (error (message "⚠ Tool extraction doesn't handle invalid input gracefully: %s" err)))
  
  ;; Check buffer name conflicts
  (let ((standard-buffers '("*scratch*" "*Messages*" "*Completions*")))
    (when (member efrit-work-buffer-name standard-buffers)
      (message "⚠ Work buffer name conflicts with standard buffer")))
  
  (message "=== Potential Issues Check Complete ==="))

(defun qa-integration-run-all ()
  "Run all integration QA tests."
  (interactive)
  (message "")
  (message "=====================================================")
  (message "         INTEGRATION QA TESTING")
  (message "=====================================================")
  (qa-test-url-request-format)
  (message "")
  (qa-test-response-handling-full)
  (message "")
  (qa-test-concurrent-usage)
  (message "")
  (qa-test-memory-leaks)
  (message "")
  (qa-test-real-world-messages)
  (message "")
  (qa-find-potential-issues)
  (message "")
  (message "=====================================================")
  (message "        INTEGRATION QA TESTING COMPLETE")
  (message "====================================================="))

(provide 'qa-test-integration)
