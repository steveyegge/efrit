;;; test-parse-api-response.el --- Test efrit--parse-api-response -*- lexical-binding: t; -*-

(add-to-list 'load-path "./lisp")
(add-to-list 'load-path "./lisp/core")

(require 'json)

;; Simulate what url-retrieve returns
(let ((test-buffer (generate-new-buffer "*test-response*")))
  (with-current-buffer test-buffer
    ;; Insert a mock HTTP response with JSON
    (insert "HTTP/1.1 200 OK\r\n")
    (insert "Content-Type: application/json\r\n")
    (insert "\r\n")
    (insert "{\"content\": [{\"type\": \"text\", \"text\": \"Hello!\"}], \"other\": \"data\"}")
    
    (message "Raw buffer content:")
    (message "%s" (buffer-string))
    
    ;; Now parse it like efrit--parse-api-response does
    (message "\nAttempting parse...")
    (goto-char (point-min))
    (if (search-forward-regexp "^$" nil t)
        (message "✅ Found blank line at %d" (point))
      (message "❌ No blank line found")
      ;; Try with \r\n
      (goto-char (point-min))
      (if (search-forward "\r\n\r\n" nil t)
          (message "✅ Found CRLF separator at %d" (point))
        (message "❌ No CRLF separator")))
    
    ;; Parse the JSON
    (let* ((json-object-type 'hash-table)
           (json-array-type 'vector)
           (json-key-type 'string)
           (coding-system-for-read 'utf-8)
           (raw-response (decode-coding-region (point) (point-max) 'utf-8 t)))
      (message "\nRaw response string: %s" raw-response)
      (condition-case err
          (let ((response (json-read-from-string raw-response)))
            (message "✅ JSON parsed successfully")
            (message "Response type: %s" (type-of response))
            (if (hash-table-p response)
                (let ((content (gethash "content" response)))
                  (message "Content: %s" content)
                  (if content
                      (message "✅ Got content from hash-table")
                    (message "❌ content is nil from hash-table")))
              (message "Response is not a hash-table, it's: %s" (type-of response))))
        (error
         (message "❌ JSON parse error: %s" err)))))
  
  (kill-buffer test-buffer))

(message "\n✅ Test complete!")

;;; test-parse-api-response.el ends here
