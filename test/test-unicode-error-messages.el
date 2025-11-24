;;; test-unicode-error-messages.el --- Tests for unicode in error messages -*- lexical-binding: t; -*-

;; This test verifies that efrit-chat properly handles unicode characters
;; (specifically U+2019 RIGHT SINGLE QUOTATION MARK) in error messages
;; by escaping them before sending to the API.

(add-to-list 'load-path (expand-file-name "../lisp/core" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../lisp/tools" (file-name-directory load-file-name)))

(require 'ert)
(require 'efrit-chat)
(require 'efrit-tools)
(require 'efrit-log)
(require 'efrit-session)

(ert-deftest test-unicode-apostrophe-in-error ()
  "Test that unicode apostrophe (U+2019) in error messages is properly escaped."
  ;; Mock url-retrieve to capture the request
  (let ((captured-request nil))
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (url callback &optional cbargs silent inhibit-cookies)
                 (setq captured-request url-request-data))))

      ;; Generate an error message with unicode apostrophe (like Emacs does)
      (condition-case err
          (fibonacci 10) ; This will produce "Symbol's function..." with U+2019
        (error
         (let ((error-msg (error-message-string err)))
           ;; Verify the error contains unicode
           (should (= (aref error-msg 6) 8217)) ; U+2019

           ;; Send via efrit-chat
           (with-temp-buffer
             (efrit-mode)
             (setq efrit--message-history
                   (list `((role . "user") (content . ,error-msg))))
             (efrit--send-api-request (reverse efrit--message-history)))

           ;; Verify the request was captured
           (should captured-request)

           ;; Verify request is unibyte (required for HTTP)
           (should-not (multibyte-string-p captured-request))

           ;; Verify no literal unicode in request
           (should-not (string-match-p "[^\x00-\x7F]" captured-request))

           ;; Verify unicode was escaped to \u2019
           (should (string-match-p "\\\\u2019" captured-request))))))))

(ert-deftest test-various-unicode-chars ()
  "Test that various unicode characters are properly escaped."
  (let ((test-cases `(("Em dash—test" . "\\\\u2014")
                      ("Ellipsis…" . "\\\\u2026")
                      ("Regular apostrophe's" . nil)))) ; ASCII should not be escaped

    (dolist (test test-cases)
      (let ((input (car test))
            (expected-pattern (cdr test))
            (captured-request nil))

        (cl-letf (((symbol-function 'url-retrieve)
                   (lambda (url callback &optional cbargs silent inhibit-cookies)
                     (setq captured-request url-request-data))))

          ;; Send message
          (with-temp-buffer
            (efrit-mode)
            (setq efrit--message-history
                  (list `((role . "user") (content . ,input))))
            (efrit--send-api-request (reverse efrit--message-history)))

          ;; Verify request is unibyte
          (should-not (multibyte-string-p captured-request))

          ;; Verify no literal unicode
          (should-not (string-match-p "[^\x00-\x7F]" captured-request))

          ;; Verify expected escape pattern if provided
          (when expected-pattern
            (should (string-match-p expected-pattern captured-request))))))))

(ert-deftest test-unicode-escaping-function ()
  "Test the unicode escaping logic directly."
  (require 'json)

  ;; Generate an error with unicode
  (condition-case err
      (fibonacci 10)
    (error
     (let* ((error-msg (error-message-string err))
            (json-string (json-encode error-msg))
            (escaped (replace-regexp-in-string
                      "[^\x00-\x7F]"
                      (lambda (char)
                        (format "\\\\u%04X" (string-to-char char)))
                      json-string))
            (final (encode-coding-string escaped 'utf-8)))

       ;; Verify the pipeline
       (should (multibyte-string-p json-string))
       (should (multibyte-string-p escaped))
       (should-not (multibyte-string-p final))
       (should-not (string-match-p "[^\x00-\x7F]" final))
       (should (string-match-p "\\\\u2019" final))))))

;;; Run tests
(ert-run-tests-batch-and-exit)
