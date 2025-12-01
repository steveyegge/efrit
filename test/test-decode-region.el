;;; test-decode-region.el --- Test decode-coding-region -*- lexical-binding: t; -*-

(message "Testing decode-coding-region...")

(let ((test-buffer (generate-new-buffer "*test*")))
  (with-current-buffer test-buffer
    ;; Insert some test data with UTF-8 content
    (insert "HTTP/1.1 200 OK\n\n{\"test\": \"data\"}")
    
    ;; Find the blank line separator
    (goto-char (point-min))
    (search-forward-regexp "^$" nil t)
    (message "Point after separator: %d" (point))
    (message "Buffer from separator to end: %s" (buffer-substring (point) (point-max)))
    
    ;; Now test decode-coding-region
    (let* ((start (point))
           (end (point-max))
           (result (decode-coding-region start end 'utf-8 t)))
      (message "decode-coding-region returned: %s (type: %s)" result (type-of result))
      (message "Buffer after decode: %s" (buffer-string))))
  (kill-buffer test-buffer))

(message "✅ Test complete!")

;;; test-decode-region.el ends here
