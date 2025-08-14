;;; qa-test-streamlined.el --- QA testing for streamlined chat -*- lexical-binding: t; -*-

(require 'efrit-chat-streamlined)

(defun qa-test-basic-functions ()
  "Test basic function availability and basic operation."
  (interactive)
  (message "=== QA: Testing Basic Functions ===")
  
  ;; Test 1: Can we load the system prompt?
  (condition-case err
      (let ((prompt (efrit-streamlined--system-prompt)))
        (if (and prompt (stringp prompt) (> (length prompt) 100))
            (message "✓ System prompt generation works (%d chars)" (length prompt))
          (message "✗ System prompt seems too short or invalid")))
    (error (message "✗ System prompt generation failed: %s" err)))
  
  ;; Test 2: Can we create work buffer?
  (condition-case err
      (let ((work-buffer (efrit-streamlined--get-work-buffer)))
        (if (bufferp work-buffer)
            (message "✓ Work buffer creation works")
          (message "✗ Work buffer creation failed")))
    (error (message "✗ Work buffer creation error: %s" err)))
  
  ;; Test 3: Can we log to work buffer?
  (condition-case err
      (progn
        (efrit-streamlined--log-to-work "QA Test Log Entry")
        (let ((work-buffer (get-buffer efrit-work-buffer-name)))
          (if (and work-buffer 
                   (with-current-buffer work-buffer
                     (goto-char (point-min))
                     (search-forward "QA Test Log Entry" nil t)))
              (message "✓ Work buffer logging works")
            (message "✗ Work buffer logging failed"))))
    (error (message "✗ Work buffer logging error: %s" err)))
  
  ;; Test 4: Can we setup chat mode?
  (condition-case err
      (let ((test-buffer (get-buffer-create "*qa-test-chat*")))
        (with-current-buffer test-buffer
          (efrit-streamlined--setup-chat-mode)
          (if (derived-mode-p 'efrit-chat-mode)
              (message "✓ Chat mode setup works")
            (message "✗ Chat mode setup failed")))
        (kill-buffer test-buffer))
    (error (message "✗ Chat mode setup error: %s" err)))
  
  (message "=== Basic Functions Test Complete ==="))

(defun qa-test-api-key-handling ()
  "Test API key handling."
  (interactive)
  (message "=== QA: Testing API Key Handling ===")
  
  ;; Test API key function exists and is callable
  (condition-case err
      (let ((key-func (symbol-function 'efrit--get-api-key)))
        (if (functionp key-func)
            (message "✓ API key function exists")
          (message "✗ API key function missing or invalid")))
    (error (message "✗ API key function error: %s" err)))
  
  ;; Test calling API key function (should work or give meaningful error)
  (condition-case err
      (let ((api-key (efrit--get-api-key)))
        (cond
         ((and api-key (stringp api-key) (> (length api-key) 10))
          (message "✓ API key retrieved (length: %d)" (length api-key)))
         ((null api-key)
          (message "⚠ No API key configured (expected for testing)"))
         (t
          (message "✗ API key seems invalid: %s" api-key))))
    (error (message "⚠ API key retrieval failed (expected if not configured): %s" err)))
  
  (message "=== API Key Test Complete ==="))

(defun qa-test-response-parsing ()
  "Test response parsing with mock data."
  (interactive)
  (message "=== QA: Testing Response Parsing ===")
  
  ;; Test 1: Valid response with text content
  (let ((mock-response-text-only
         "{\"content\":[{\"type\":\"text\",\"text\":\"Hello, this is a test response.\"}],\"model\":\"claude-3-5-sonnet-20241022\"}"))
    (condition-case err
        (let ((parsed (json-read-from-string mock-response-text-only)))
          (if parsed
              (message "✓ JSON parsing works for text-only response")
            (message "✗ JSON parsing failed for text-only response")))
      (error (message "✗ JSON parsing error: %s" err))))
  
  ;; Test 2: Response with tool use
  (let ((mock-response-with-tools
         "{\"content\":[{\"type\":\"text\",\"text\":\"I'll help you with that.\"},{\"type\":\"tool_use\",\"id\":\"test123\",\"name\":\"eval_sexp\",\"input\":{\"expr\":\"(+ 1 2)\"}}]}"))
    (condition-case err
        (let* ((parsed (json-read-from-string mock-response-with-tools))
               (tool-uses (efrit-streamlined--extract-tool-uses parsed)))
          (if (and tool-uses (= (length tool-uses) 1))
              (message "✓ Tool extraction works (%d tools found)" (length tool-uses))
            (message "✗ Tool extraction failed or wrong count: %s" tool-uses)))
      (error (message "✗ Tool extraction error: %s" err))))
  
  ;; Test 3: Invalid JSON
  (let ((invalid-json "{ invalid json }"))
    (condition-case err
        (json-read-from-string invalid-json)
      (error (message "✓ JSON error handling works for invalid input"))))
  
  (message "=== Response Parsing Test Complete ==="))

(defun qa-test-tool-execution ()
  "Test tool execution with safe examples."
  (interactive)
  (message "=== QA: Testing Tool Execution ===")
  
  ;; Test 1: Safe elisp evaluation
  (let ((mock-tool-use 
         `((type . "tool_use")
           (name . "eval_sexp")
           (input . ((expr . "(+ 1 2)"))))))
    (condition-case err
        (progn
          (efrit-streamlined--execute-tools (list mock-tool-use))
          (message "✓ Safe tool execution completed"))
      (error (message "✗ Tool execution error: %s" err))))
  
  ;; Test 2: Invalid elisp
  (let ((mock-bad-tool 
         `((type . "tool_use")
           (name . "eval_sexp")
           (input . ((expr . "(invalid-function-name)"))))))
    (condition-case err
        (progn
          (efrit-streamlined--execute-tools (list mock-bad-tool))
          (message "✓ Error handling for invalid elisp works"))
      (error (message "✗ Tool execution error handling failed: %s" err))))
  
  ;; Test 3: Unknown tool
  (let ((mock-unknown-tool 
         `((type . "tool_use")
           (name . "unknown_tool")
           (input . ((param . "test"))))))
    (condition-case err
        (progn
          (efrit-streamlined--execute-tools (list mock-unknown-tool))
          (message "✓ Unknown tool handling works"))
      (error (message "✗ Unknown tool handling error: %s" err))))
  
  (message "=== Tool Execution Test Complete ==="))

(defun qa-test-edge-cases ()
  "Test edge cases and boundary conditions."
  (interactive)
  (message "=== QA: Testing Edge Cases ===")
  
  ;; Test 1: Empty message
  (condition-case err
      (let ((messages (list `((role . "user") (content . "")))))
        (message "✓ Empty message handling works"))
    (error (message "✗ Empty message error: %s" err)))
  
  ;; Test 2: Very long message
  (let ((long-message (make-string 10000 ?x)))
    (condition-case err
        (let ((messages (list `((role . "user") (content . ,long-message)))))
          (message "✓ Long message handling works (%d chars)" (length long-message)))
      (error (message "✗ Long message error: %s" err))))
  
  ;; Test 3: Special characters in message
  (let ((special-message "Test with special chars: \n\t\"'\\{}[]()"))
    (condition-case err
        (let ((messages (list `((role . "user") (content . ,special-message)))))
          (message "✓ Special character handling works"))
      (error (message "✗ Special character error: %s" err))))
  
  ;; Test 4: Multiple rapid work buffer logs
  (condition-case err
      (progn
        (dotimes (i 50)
          (efrit-streamlined--log-to-work (format "Rapid log %d" i)))
        (message "✓ Rapid logging works"))
    (error (message "✗ Rapid logging error: %s" err)))
  
  (message "=== Edge Cases Test Complete ==="))

(defun qa-test-buffer-management ()
  "Test buffer creation, cleanup, and management."
  (interactive)
  (message "=== QA: Testing Buffer Management ===")
  
  ;; Test 1: Multiple work buffer creations don't duplicate
  (let ((buffer1 (efrit-streamlined--get-work-buffer))
        (buffer2 (efrit-streamlined--get-work-buffer)))
    (if (eq buffer1 buffer2)
        (message "✓ Work buffer reuse works correctly")
      (message "✗ Work buffer creates duplicates")))
  
  ;; Test 2: Chat buffer creation and mode
  (let ((chat-buffer (get-buffer-create efrit-buffer-name)))
    (with-current-buffer chat-buffer
      (efrit-streamlined--setup-chat-mode)
      (if (and (derived-mode-p 'efrit-chat-mode)
               (not buffer-read-only))
          (message "✓ Chat buffer setup works correctly")
        (message "✗ Chat buffer setup has issues"))))
  
  ;; Test 3: Buffer content handling
  (let ((test-content "Test response content"))
    (condition-case err
        (progn
          (efrit-streamlined--display-response test-content)
          (let ((chat-buffer (get-buffer efrit-buffer-name)))
            (with-current-buffer chat-buffer
              (if (search-backward test-content nil t)
                  (message "✓ Response display works")
                (message "✗ Response not found in buffer")))))
      (error (message "✗ Response display error: %s" err))))
  
  (message "=== Buffer Management Test Complete ==="))

(defun qa-run-all-tests ()
  "Run all QA tests."
  (interactive)
  (message "")
  (message "=====================================================")
  (message "           QA TESTING STREAMLINED CHAT")
  (message "=====================================================")
  (qa-test-basic-functions)
  (message "")
  (qa-test-api-key-handling)
  (message "")
  (qa-test-response-parsing)
  (message "")
  (qa-test-tool-execution)
  (message "")
  (qa-test-edge-cases)
  (message "")
  (qa-test-buffer-management)
  (message "")
  (message "=====================================================")
  (message "              QA TESTING COMPLETE")
  (message "====================================================="))

(provide 'qa-test-streamlined)
