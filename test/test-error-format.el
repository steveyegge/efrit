;;; test-error-format.el --- Test what error format url-retrieve returns -*- lexical-binding: nil; -*-

(add-to-list 'load-path "./lisp")
(add-to-list 'load-path "./lisp/core")

(require 'url)

;; Test 1: Simulate connection error
(message "Test 1: Connection error (invalid URL)")
(let ((buf (url-retrieve-synchronously "https://invalid-nonexistent-domain.invalid" t)))
  (if buf
      (message "Got buffer: %s" buf)
    (message "Got nil")))

;; Test 2: Check what status looks like on 404
(message "\nTest 2: HTTP 404 (deliberately hitting wrong API path)")
(let ((url-request-method "GET"))
  (let ((buf (url-retrieve-synchronously "https://api.anthropic.com/nonexistent" t)))
    (if buf
        (with-current-buffer buf
          (goto-char (point-min))
          (let ((first-line (buffer-substring (point-min) (line-end-position))))
            (message "Response: %s" first-line)))
      (message "Got nil"))))

(message "\n✅ Test complete!")

;;; test-error-format.el ends here
