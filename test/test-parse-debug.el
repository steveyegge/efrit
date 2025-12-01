;;; test-parse-debug.el --- Debug the response parsing issue -*- lexical-binding: t; -*-

(add-to-list 'load-path "./lisp")
(add-to-list 'load-path "./lisp/core")
(add-to-list 'load-path "./lisp/support")
(add-to-list 'load-path "./lisp/interfaces")
(add-to-list 'load-path "./lisp/tools")

(require 'url)
(require 'json)
(require 'efrit-config)
(require 'efrit-common)

(message "Debugging response parsing...")

(let* ((api-key (efrit-common-get-api-key))
       (url-request-method "POST")
       (url-request-extra-headers `(("x-api-key" . ,api-key)
                                    ("anthropic-version" . "2023-06-01")
                                    ("content-type" . "application/json")))
       (model "claude-3-5-sonnet-20241022")
       (request-data `(("model" . ,model)
                      ("max_tokens" . 100)
                      ("messages" . [
                                     (("role" . "user")
                                      ("content" . "Say hello"))
                                     ])))
       (json-data (json-encode request-data))
       (url-request-data (encode-coding-string json-data 'utf-8)))

  (condition-case err
      (let ((response-buffer (url-retrieve-synchronously
                             "https://api.anthropic.com/v1/messages"
                             t t 30)))
        (if response-buffer
            (with-current-buffer response-buffer
              (message "Buffer: %s, Size: %d bytes" response-buffer (buffer-size))
              
              ;; Show raw content
              (goto-char (point-min))
              (message "First 200 chars:\n%s" (buffer-substring (point-min) (min 200 (point-max))))
              
              ;; Try the original parsing method
              (goto-char (point-min))
              (if (search-forward-regexp "^$" nil t)
                  (let ((header-end (point)))
                    (message "Found blank line at %d" header-end)
                    (let* ((json-object-type 'hash-table)
                           (json-array-type 'vector)
                           (json-key-type 'string)
                           (raw-response (decode-coding-region header-end (point-max) 'utf-8 t))
                           (response (json-read-from-string raw-response))
                           (content (gethash "content" response)))
                      (message "✅ Parsing succeeded with hash-table")
                      (message "Content type: %s" (type-of content))
                      (message "Content length: %s" (if (vectorp content) (length content) "n/a"))))
                (message "❌ Could not find blank line separator")))
          (message "❌ No response buffer")))
    (error
     (message "❌ Error: %s" err))))

(message "\n✅ Debug complete!")

;;; test-parse-debug.el ends here
