;;; test-http-status.el --- Check HTTP response status -*- lexical-binding: nil; -*-

(add-to-list 'load-path "./lisp")
(add-to-list 'load-path "./lisp/core")

(require 'efrit-config)
(require 'efrit-common)
(require 'json)

(let* ((api-key (efrit-common-get-api-key))
       (url-request-method "POST")
       (url-request-extra-headers `(("x-api-key" . ,api-key)
                                    ("anthropic-version" . "2023-06-01")
                                    ("content-type" . "application/json")))
       (model "claude-sonnet-4-5-20250929")
       (request-data `(("model" . ,model)
                      ("max_tokens" . 100)
                      ("messages" . [
                                     (("role" . "user")
                                      ("content" . "Say hello"))
                                     ])))
       (json-data (json-encode request-data))
       (url-request-data (encode-coding-string json-data 'utf-8)))
  
  (let ((buf (url-retrieve-synchronously "https://api.anthropic.com/v1/messages" t t 30)))
    (with-current-buffer buf
      (goto-char (point-min))
      (let ((first-line (buffer-substring (point-min) (line-end-position))))
        (message "First line: %s" first-line)
        (if (string-match "^HTTP/[0-9.]+ \\([0-9]+\\) " first-line)
            (message "✅ HTTP Status: %s" (match-string 1 first-line))
          (message "❌ Could not parse status")))
      (message "Response size: %d bytes" (buffer-size)))
    (kill-buffer buf)))

(message "\n✅ Done!")

;;; test-http-status.el ends here
