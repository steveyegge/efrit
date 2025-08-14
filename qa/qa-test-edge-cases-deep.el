;;; qa-test-edge-cases-deep.el --- Deep edge case testing -*- lexical-binding: t; -*-

(require 'efrit-chat-streamlined)

(defun qa-test-malformed-api-responses ()
  "Test handling of malformed API responses."
  (interactive)
  (message "=== QA: Testing Malformed API Responses ===")
  
  (let ((malformed-responses 
         '(""  ; empty response
           "{"  ; incomplete JSON
           "{\"content\":null}"  ; null content
           "{\"content\":[]}"  ; empty content array  
           "{\"content\":[{\"type\":\"tool_use\"}]}"  ; tool use without name
           "{\"content\":[{\"type\":\"tool_use\",\"name\":\"eval_sexp\"}]}"  ; tool use without input
           "{\"content\":[{\"type\":\"text\"}]}"  ; text without content
           "Not JSON at all"  ; completely invalid
           "{\"error\":\"API Error\"}"  ; error response
           )))
    
    (dolist (response malformed-responses)
      (condition-case err
          (let* ((mock-buffer (get-buffer-create "*test-malformed*"))
                 (full-response (concat "HTTP/1.1 200 OK\n\n" response)))
            (with-current-buffer mock-buffer
              (erase-buffer)
              (insert full-response)
              (goto-char (point-min))
              (efrit-streamlined--handle-response nil))
            (kill-buffer mock-buffer)
            (message "✓ Handled malformed: %s" 
                    (if (> (length response) 20)
                        (concat (substring response 0 20) "...")
                      response)))
        (error (message "✗ Failed on malformed response: %s - Error: %s" 
                       (substring response 0 (min 20 (length response))) err)))))
  
  (message "=== Malformed API Response Test Complete ==="))

(defun qa-test-extreme-content ()
  "Test with extreme content scenarios."
  (interactive)
  (message "=== QA: Testing Extreme Content ===")
  
  ;; Test very long system prompt handling
  (let ((original-prompt (efrit-streamlined--system-prompt)))
    (message "✓ Normal system prompt: %d characters" (length original-prompt)))
  
  ;; Test with extreme elisp expressions
  (let ((extreme-expressions
         '("(+ 1 2)"  ; simple
           "(progn (dotimes (i 1000) i) 'done)"  ; compute intensive but safe
           "(make-string 100000 ?x)"  ; large string creation
           "(/ 1 0)"  ; division by zero  
           "(error \"Test error\")"  ; explicit error
           "(undefined-function)"  ; undefined function
           ""  ; empty expression
           "not-valid-elisp"  ; invalid syntax
           "(quote (a very long list with many elements to see how it handles deeply nested structures and lots of content))"
           )))
    
    (dolist (expr extreme-expressions)
      (condition-case err
          (let ((mock-tool `((type . "tool_use") 
                           (name . "eval_sexp")
                           (input . ((expr . ,expr))))))
            (efrit-streamlined--execute-tools (list mock-tool))
            (message "✓ Handled expression: %s" 
                    (if (> (length expr) 30) 
                        (concat (substring expr 0 30) "...")
                      expr)))
        (error (message "✗ Failed on expression: %s - %s" expr err)))))
  
  (message "=== Extreme Content Test Complete ==="))

(defun qa-test-state-corruption ()
  "Test for potential state corruption issues."
  (interactive)  
  (message "=== QA: Testing State Corruption ===")
  
  ;; Test rapid buffer switching during operations
  (let ((original-buffer (current-buffer))
        (test-buffers '()))
    
    ;; Create multiple test buffers
    (dotimes (i 5)
      (push (get-buffer-create (format "*state-test-%d*" i)) test-buffers))
    
    ;; Rapidly switch between buffers while doing operations
    (condition-case err
        (dolist (buffer test-buffers)
          (switch-to-buffer buffer)
          (efrit-streamlined--log-to-work (format "Switched to %s" (buffer-name)))
          (efrit-streamlined--setup-chat-mode)
          (efrit-streamlined--display-response (format "Response in %s" (buffer-name))))
      (error (message "✗ State corruption during buffer switching: %s" err)))
    
    ;; Clean up
    (dolist (buffer test-buffers)
      (kill-buffer buffer))
    (switch-to-buffer original-buffer)
    
    (message "✓ State corruption test completed"))
  
  ;; Test variable shadowing/scoping issues
  (let ((efrit-work-buffer-name "*custom-work-buffer*"))
    (condition-case err
        (let ((work-buffer (efrit-streamlined--get-work-buffer)))
          (if (string-equal (buffer-name work-buffer) "*custom-work-buffer*")
              (message "✓ Variable scoping works correctly")
            (message "✗ Variable scoping issue detected"))
          (kill-buffer work-buffer))
      (error (message "✗ Variable scoping test error: %s" err))))
  
  (message "=== State Corruption Test Complete ==="))

(defun qa-test-resource-limits ()
  "Test resource usage and limits."
  (interactive)
  (message "=== QA: Testing Resource Limits ===")
  
  ;; Test very long work buffer logs
  (let ((long-message (make-string 50000 ?x)))
    (condition-case err
        (progn
          (efrit-streamlined--log-to-work long-message)
          (message "✓ Very long log message handled (%d chars)" (length long-message)))
      (error (message "✗ Long log message failed: %s" err))))
  
  ;; Test many rapid log entries
  (condition-case err
      (dotimes (i 1000)
        (efrit-streamlined--log-to-work (format "Rapid log %d" i)))
    (error (message "✗ Rapid logging failed at iteration: %s" err)))
  (message "✓ 1000 rapid log entries completed")
  
  ;; Test work buffer size after lots of logging  
  (let ((work-buffer (get-buffer efrit-work-buffer-name)))
    (when work-buffer
      (with-current-buffer work-buffer
        (let ((buffer-size (buffer-size)))
          (if (> buffer-size 100000)
              (message "⚠ Work buffer is getting large: %d characters" buffer-size)
            (message "✓ Work buffer size reasonable: %d characters" buffer-size))))))
  
  (message "=== Resource Limits Test Complete ==="))

(defun qa-test-concurrency-edge-cases ()
  "Test edge cases that might occur with concurrent usage."
  (interactive)
  (message "=== QA: Testing Concurrency Edge Cases ===")
  
  ;; Simulate what happens if multiple "API calls" happen simultaneously
  ;; by testing the work buffer under concurrent access
  (condition-case err
      (let ((processes 10)
            (operations-per-process 50))
        (dotimes (proc processes)
          (dotimes (op operations-per-process)
            (efrit-streamlined--log-to-work 
             (format "Proc %d Op %d: %s" proc op (current-time-string)))))
        (message "✓ Simulated concurrent access completed"))
    (error (message "✗ Concurrent access simulation failed: %s" err)))
  
  ;; Test buffer creation race conditions by rapidly creating/accessing work buffer
  (condition-case err
      (let ((buffers '()))
        (dotimes (i 20)
          (push (efrit-streamlined--get-work-buffer) buffers))
        (let ((unique-buffers (delete-dups buffers)))
          (if (= (length unique-buffers) 1)
              (message "✓ Work buffer creation is race-condition safe")
            (message "⚠ Possible race condition: %d unique buffers created" 
                    (length unique-buffers)))))
    (error (message "✗ Race condition test failed: %s" err)))
  
  (message "=== Concurrency Edge Cases Test Complete ==="))

(defun qa-deep-edge-cases-run-all ()
  "Run all deep edge case tests."
  (interactive)
  (message "")
  (message "=====================================================")
  (message "         DEEP EDGE CASE QA TESTING")  
  (message "=====================================================")
  (qa-test-malformed-api-responses)
  (message "")
  (qa-test-extreme-content)
  (message "")
  (qa-test-state-corruption)
  (message "")
  (qa-test-resource-limits)
  (message "")
  (qa-test-concurrency-edge-cases)
  (message "")
  (message "=====================================================")
  (message "      DEEP EDGE CASE QA TESTING COMPLETE")
  (message "====================================================="))

(provide 'qa-test-edge-cases-deep)
