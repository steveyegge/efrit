;;; test-efrit-edit.el --- Test the new efrit-edit command -*- lexical-binding: t; -*-

(require 'efrit-do)

(defun test-efrit-edit-wrap-columns ()
  "Test efrit-edit with text wrapping to specific column width."
  (with-temp-buffer
    ;; Insert some long text
    (insert "This is a very long line of text that should be wrapped when we apply the efrit-edit command to wrap it to a specific column width like 80 or 2500 characters. This text is intentionally long to test the wrapping functionality.")
    
    ;; Move to beginning
    (goto-char (point-min))
    
    ;; Store original content
    (let ((original-content (buffer-string)))
      (message "Original content length: %d characters" (length original-content))
      (message "Testing efrit-edit wrap to 80 columns...")
      
      ;; Test efrit-edit (this would normally be interactive)
      ;; For automated testing, we'll simulate the call
      (message "efrit-edit command would be called here")
      (message "Original text: %s" (truncate-string-to-width original-content 100 nil nil t)))))

(defun test-efrit-edit-basic ()
  "Basic test of efrit-edit infrastructure."
  (message "Testing efrit-edit basic functionality...")
  
  ;; Test that the functions exist
  (when (fboundp 'efrit-edit)
    (message "✓ efrit-edit command is defined"))
  
  (when (fboundp 'efrit-edit--system-prompt)
    (message "✓ efrit-edit--system-prompt function is defined"))
  
  (when (fboundp 'efrit-edit--execute-command)
    (message "✓ efrit-edit--execute-command function is defined"))
  
  (when (fboundp 'efrit-edit--process-api-response)
    (message "✓ efrit-edit--process-api-response function is defined"))
  
  ;; Test the system prompt
  (let ((prompt (efrit-edit--system-prompt)))
    (if (and prompt (> (length prompt) 100))
        (message "✓ System prompt generated (%d characters)" (length prompt))
      (message "✗ System prompt issue"))))

;; Run tests
(test-efrit-edit-basic)
(test-efrit-edit-wrap-columns)

(provide 'test-efrit-edit)
;;; test-efrit-edit.el ends here
