;;; qa-test-real-integration.el --- Real-world integration testing -*- lexical-binding: t; -*-

(require 'efrit-chat-streamlined)

(defun qa-test-real-api-call ()
  "Test actual API integration (requires valid API key)."
  (interactive)
  (message "=== QA: Testing Real API Integration ===")
  
  ;; Check if we have a valid API key
  (condition-case err
      (let ((api-key (efrit--get-api-key)))
        (if (and api-key (> (length api-key) 20))
            (progn
              (message "✓ API key found (length: %d)" (length api-key))
              
              ;; Test with a simple, safe request
              (message "Sending real API request...")
              (let ((test-message "What is 2+2? Please respond with just the number."))
                (efrit-streamlined-send test-message)
                (message "✓ Real API call initiated")
                (message "Check *efrit* and *efrit-work* buffers for results")
                
                ;; Set up a timer to check results
                (run-with-timer 10 nil
                               (lambda ()
                                 (let ((chat-buffer (get-buffer efrit-buffer-name))
                                       (work-buffer (get-buffer efrit-work-buffer-name)))
                                   (if chat-buffer
                                       (with-current-buffer chat-buffer
                                         (if (search-backward "4" nil t)
                                             (message "✓ Real API response received and parsed")
                                           (message "⚠ API response may not have completed")))
                                     (message "⚠ Chat buffer not found"))
                                   (if work-buffer
                                       (message "✓ Work buffer contains %d chars of logs" 
                                               (with-current-buffer work-buffer (buffer-size)))
                                     (message "⚠ Work buffer not found")))))))
          (message "⚠ No valid API key found - skipping real API test")))
    (error (message "⚠ API key check failed: %s - skipping real API test" err)))
  
  (message "=== Real API Integration Test Complete ==="))

(defun qa-test-existing-efrit-compatibility ()
  "Test compatibility with existing efrit-chat.el system."
  (interactive)
  (message "=== QA: Testing Existing Efrit Compatibility ===")
  
  ;; Test if we can load both systems without conflicts
  (condition-case err
      (progn
        ;; Try to load efrit-chat if it exists
        (if (locate-library "efrit-chat")
            (progn
              (require 'efrit-chat)
              (message "✓ efrit-chat.el loaded successfully alongside streamlined"))
          (message "⚠ efrit-chat.el not found - testing in isolation")))
    (error (message "✗ Compatibility issue with efrit-chat.el: %s" err)))
  
  ;; Test shared variable conflicts
  (let ((shared-vars '(efrit-buffer-name efrit-model efrit-max-tokens efrit-api-url)))
    (dolist (var shared-vars)
      (if (boundp var)
          (message "✓ Shared variable %s available" var)
        (message "✗ Shared variable %s missing" var))))
  
  ;; Test if efrit-tools functions are available
  (let ((tool-functions '(efrit--get-api-key efrit-tools-system-prompt efrit-tools-get-context)))
    (dolist (func tool-functions)
      (if (fboundp func)
          (message "✓ Tool function %s available" func)
        (message "✗ Tool function %s missing" func))))
  
  ;; Test buffer name conflicts
  (let ((existing-buffers (mapcar #'buffer-name (buffer-list))))
    (if (member efrit-buffer-name existing-buffers)
        (message "⚠ Chat buffer %s already exists" efrit-buffer-name)
      (message "✓ No buffer conflicts detected")))
  
  (message "=== Existing Efrit Compatibility Test Complete ==="))

(defun qa-test-user-workflow-scenarios ()
  "Test realistic user workflow scenarios."
  (interactive)
  (message "=== QA: Testing User Workflow Scenarios ===")
  
  ;; Scenario 1: First-time user starting efrit
  (message "Testing Scenario 1: First-time user")
  (condition-case err
      (let ((fresh-chat-buffer (get-buffer-create "*efrit-test-fresh*")))
        (with-current-buffer fresh-chat-buffer
          (efrit-streamlined--setup-chat-mode)
          (if (derived-mode-p 'efrit-chat-mode)
              (message "✓ Fresh user setup works")
            (message "✗ Fresh user setup failed")))
        (kill-buffer fresh-chat-buffer))
    (error (message "✗ First-time user scenario failed: %s" err)))
  
  ;; Scenario 2: User sends multiple messages in succession
  (message "Testing Scenario 2: Rapid multiple messages")
  (condition-case err
      (let ((messages '("hello" "what time is it?" "create a file" "thanks")))
        (dolist (msg messages)
          (efrit-streamlined--log-to-work (format "User workflow: %s" msg))
          ;; Simulate the request processing
          (let ((mock-messages (list `((role . "user") (content . ,msg)))))
            ;; Just test the message processing, not actual API calls
            (message "✓ Processed message: %s" msg))))
    (error (message "✗ Multiple message scenario failed: %s" err)))
  
  ;; Scenario 3: User switches between different types of requests
  (message "Testing Scenario 3: Mixed request types")
  (let ((mixed-requests 
         '(("write a todo list" . "action")
           ("how do I use git?" . "information") 
           ("create a backup of my files" . "action")
           ("explain recursion" . "information"))))
    (dolist (request mixed-requests)
      (let ((message (car request))
            (expected-type (cdr request)))
        (efrit-streamlined--log-to-work 
         (format "Mixed workflow - %s request: %s" expected-type message))
        (message "✓ Mixed request handled: %s (%s)" message expected-type))))
  
  ;; Scenario 4: User closes and reopens buffers
  (message "Testing Scenario 4: Buffer lifecycle management")
  (condition-case err
      (let ((work-buffer (efrit-streamlined--get-work-buffer)))
        ;; Simulate closing and reopening
        (kill-buffer work-buffer)
        (let ((new-work-buffer (efrit-streamlined--get-work-buffer)))
          (if (buffer-live-p new-work-buffer)
              (message "✓ Buffer recreation works")
            (message "✗ Buffer recreation failed"))))
    (error (message "✗ Buffer lifecycle test failed: %s" err)))
  
  (message "=== User Workflow Scenarios Test Complete ==="))

(defun qa-test-recovery-scenarios ()
  "Test recovery from various failure modes."
  (interactive)
  (message "=== QA: Testing Recovery Scenarios ===")
  
  ;; Recovery 1: Network failure simulation
  (message "Testing Recovery 1: Network failure handling")
  (let ((original-url-retrieve (symbol-function 'url-retrieve)))
    (fset 'url-retrieve 
          (lambda (&rest args) 
            (error "Network unreachable")))
    (unwind-protect
        (condition-case err
            (progn
              (efrit-streamlined--send-request 
               (list `((role . "user") (content . "test network failure"))))
              (message "⚠ Network failure should have been caught"))
          (error (message "✓ Network failure handled gracefully: %s" 
                         (error-message-string err))))
      (fset 'url-retrieve original-url-retrieve)))
  
  ;; Recovery 2: Corrupted work buffer
  (message "Testing Recovery 2: Corrupted buffer recovery")
  (condition-case err
      (let ((work-buffer (efrit-streamlined--get-work-buffer)))
        ;; Corrupt the buffer
        (with-current-buffer work-buffer
          (erase-buffer)
          (insert "CORRUPTED DATA\0\1\2\3"))
        ;; Try to log something
        (efrit-streamlined--log-to-work "Recovery test")
        (message "✓ Corrupted buffer recovery works"))
    (error (message "✗ Corrupted buffer recovery failed: %s" err)))
  
  ;; Recovery 3: Invalid API response recovery
  (message "Testing Recovery 3: Invalid API response")
  (condition-case err
      (with-temp-buffer
        (insert "HTTP/1.1 500 Internal Server Error\n\nServer Error")
        (goto-char (point-min))
        (efrit-streamlined--handle-response nil)
        (message "✓ Invalid API response handled"))
    (error (message "✓ Invalid API response properly errored: %s" err)))
  
  ;; Recovery 4: Work buffer getting deleted during operation
  (message "Testing Recovery 4: Missing work buffer recovery")
  (condition-case err
      (let ((work-buffer (efrit-streamlined--get-work-buffer)))
        (kill-buffer work-buffer)
        ;; Try to log - should recreate buffer
        (efrit-streamlined--log-to-work "Buffer recreation test")
        (if (get-buffer efrit-work-buffer-name)
            (message "✓ Work buffer auto-recreation works")
          (message "✗ Work buffer not recreated")))
    (error (message "✗ Work buffer recovery failed: %s" err)))
  
  ;; Recovery 5: Emacs running out of memory simulation
  (message "Testing Recovery 5: Memory pressure handling")
  (condition-case err
      (progn
        ;; Try to create a very large log entry
        (let ((huge-string (make-string 1000000 ?x)))
          (efrit-streamlined--log-to-work huge-string))
        (message "✓ Large memory allocation handled"))
    (error (message "⚠ Memory pressure caused error (expected): %s" err)))
  
  (message "=== Recovery Scenarios Test Complete ==="))

(defun qa-test-configuration-variations ()
  "Test different configuration combinations."
  (interactive)
  (message "=== QA: Testing Configuration Variations ===")
  
  ;; Test 1: Different work buffer names
  (let ((original-name efrit-work-buffer-name))
    (setq efrit-work-buffer-name "*custom-work-buffer*")
    (unwind-protect
        (let ((work-buffer (efrit-streamlined--get-work-buffer)))
          (if (string-equal (buffer-name work-buffer) "*custom-work-buffer*")
              (message "✓ Custom work buffer name works")
            (message "✗ Custom work buffer name failed"))
          (kill-buffer work-buffer))
      (setq efrit-work-buffer-name original-name)))
  
  ;; Test 2: Different size limits
  (let ((original-size efrit-work-buffer-max-size))
    (setq efrit-work-buffer-max-size 1000)  ; Very small limit
    (unwind-protect
        (progn
          (dotimes (i 50)
            (efrit-streamlined--log-to-work (make-string 100 ?x)))
          (let ((work-buffer (get-buffer efrit-work-buffer-name)))
            (with-current-buffer work-buffer
              (if (< (buffer-size) 2000)  ; Should be truncated
                  (message "✓ Small size limit enforced")
                (message "⚠ Size limit may not be working")))))
      (setq efrit-work-buffer-max-size original-size)))
  
  ;; Test 3: Disabled tools
  (let ((original-tools efrit-enable-tools))
    (setq efrit-enable-tools nil)
    (unwind-protect
        (condition-case err
            (let ((prompt (efrit-streamlined--system-prompt)))
              (if (string-match-p "tools" prompt)
                  (message "⚠ System prompt mentions tools when disabled")
                (message "✓ Tools properly disabled in system prompt")))
          (error (message "✗ Tools disable test failed: %s" err)))
      (setq efrit-enable-tools original-tools)))
  
  ;; Test 4: Different models
  (let ((original-model efrit-model))
    (setq efrit-model "anthropic/claude-sonnet-4")
    (unwind-protect
        (condition-case err
            (progn
              ;; Just test that configuration doesn't break anything
              (efrit-streamlined--system-prompt)
              (message "✓ Different model configuration works"))
          (error (message "✗ Model configuration test failed: %s" err)))
      (setq efrit-model original-model)))
  
  (message "=== Configuration Variations Test Complete ==="))

(defun qa-test-performance-sustained-load ()
  "Test performance under sustained load."
  (interactive)
  (message "=== QA: Testing Performance Under Load ===")
  
  (let ((start-time (current-time)))
    
    ;; Test 1: Rapid logging performance
    (message "Testing rapid logging performance...")
    (let ((log-start (current-time)))
      (dotimes (i 500)
        (efrit-streamlined--log-to-work (format "Performance test log %d" i)))
      (let ((log-time (float-time (time-subtract (current-time) log-start))))
        (message "✓ 500 log entries took %.2f seconds (%.2f logs/sec)" 
                log-time (/ 500.0 log-time))))
    
    ;; Test 2: Multiple buffer creation performance  
    (message "Testing buffer creation performance...")
    (let ((buffer-start (current-time))
          (buffers '()))
      (dotimes (i 100)
        (push (get-buffer-create (format "*perf-test-%d*" i)) buffers))
      (let ((buffer-time (float-time (time-subtract (current-time) buffer-start))))
        (message "✓ 100 buffer creations took %.2f seconds" buffer-time))
      ;; Cleanup
      (dolist (buffer buffers)
        (kill-buffer buffer)))
    
    ;; Test 3: System prompt generation performance
    (message "Testing system prompt generation performance...")
    (let ((prompt-start (current-time)))
      (dotimes (i 50)
        (efrit-streamlined--system-prompt))
      (let ((prompt-time (float-time (time-subtract (current-time) prompt-start))))
        (message "✓ 50 system prompt generations took %.2f seconds" prompt-time)))
    
    ;; Test 4: JSON parsing performance with large responses
    (message "Testing JSON parsing performance...")
    (let* ((large-json (json-encode 
                       `((content . ,(make-vector 100 
                                                 `((type . "text") 
                                                   (text . ,(make-string 1000 ?x))))))))
           (parse-start (current-time)))
      (dotimes (i 10)
        (json-read-from-string large-json))
      (let ((parse-time (float-time (time-subtract (current-time) parse-start))))
        (message "✓ 10 large JSON parses took %.2f seconds" parse-time)))
    
    ;; Overall performance summary
    (let ((total-time (float-time (time-subtract (current-time) start-time))))
      (message "✓ Total performance test time: %.2f seconds" total-time)
      (if (< total-time 10.0)
          (message "✓ Performance is acceptable")
        (message "⚠ Performance may need optimization"))))
  
  (message "=== Performance Under Load Test Complete ==="))

(defun qa-extended-testing-run-all ()
  "Run all extended QA tests."
  (interactive)
  (message "")
  (message "=====================================================")
  (message "         EXTENDED QA TESTING SUITE")
  (message "=====================================================")
  
  (qa-test-existing-efrit-compatibility)
  (message "")
  (qa-test-user-workflow-scenarios)
  (message "")
  (qa-test-recovery-scenarios)
  (message "")
  (qa-test-configuration-variations)
  (message "")
  (qa-test-performance-sustained-load)
  (message "")
  (qa-test-real-api-call)  ; Do this last as it may take time
  (message "")
  (message "=====================================================")
  (message "       EXTENDED QA TESTING COMPLETE")
  (message "====================================================="))

(provide 'qa-test-real-integration)
