;;; test-batch-response.el --- Debug batch response parsing -*- lexical-binding: t; -*-

(add-to-list 'load-path "./lisp")
(add-to-list 'load-path "./lisp/core")
(add-to-list 'load-path "./lisp/support")
(add-to-list 'load-path "./lisp/interfaces")
(add-to-list 'load-path "./lisp/tools")

;; Just test the synchronous request directly
(require 'url)
(require 'json)

(message "Testing synchronous HTTP request...")

(let* ((api-key (getenv "ANTHROPIC_API_KEY"))
       (url-request-method "POST")
       (url-request-extra-headers `(("x-api-key" . ,api-key)
                                    ("anthropic-version" . "2023-06-01")
                                    ("content-type" . "application/json")))
       (request-data `(("model" . "claude-3-5-sonnet-20241022")
                      ("max_tokens" . 100)
                      ("messages" . [
                                     (("role" . "user")
                                      ("content" . "Say hello"))
                                     ])))
       (json-data (json-encode request-data))
       (url-request-data (encode-coding-string json-data 'utf-8)))

  (if (not api-key)
      (message "❌ ANTHROPIC_API_KEY not set!")
    (message "API Key detected: %s" (substring api-key 0 10))
    
    (condition-case err
        (let ((response-buffer (url-retrieve-synchronously 
                               "https://api.anthropic.com/v1/messages"
                               t t 30)))
          (message "✅ Got response buffer: %s" response-buffer)
          
          (if response-buffer
              (with-current-buffer response-buffer
                (goto-char (point-min))
                (message "Response buffer size: %d bytes" (buffer-size))
                (message "First 500 chars of response:")
                (message "%s" (buffer-substring (point-min) (min (+ (point-min) 500) (point-max))))
                
                ;; Try to find the header/body separator
                (if (search-forward "\n\n" nil t)
                    (let* ((headers-end (point))
                           (body (buffer-substring headers-end (point-max))))
                      (message "Found headers/body separator at position %d" headers-end)
                      (message "Body size: %d bytes" (length body))
                      (message "Body (first 500 chars): %s" (substring body 0 (min 500 (length body))))
                      
                      ;; Try to parse JSON
                      (condition-case json-err
                          (let ((parsed (json-read-from-string body)))
                            (message "✅ JSON parsed successfully")
                            (message "Parsed keys: %s" (hash-table-keys parsed)))
                        (error
                         (message "❌ JSON parse error: %s" json-err))))
                  (message "❌ Could not find header/body separator")))
            (message "❌ No response buffer returned")))
      (error
       (message "❌ Error: %s" err)))))

(message "\n✅ Test completed!")

;;; test-batch-response.el ends here
