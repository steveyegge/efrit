;;; test-executor-unicode.el --- Tests for unicode handling in efrit-executor -*- lexical-binding: t; -*-

;; This test verifies that efrit-executor properly handles unicode characters
;; in both async and sync API request paths.

(add-to-list 'load-path (expand-file-name "../lisp/core" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../lisp/tools" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../lisp/interfaces" (file-name-directory load-file-name)))

(require 'ert)
(require 'efrit-executor)
(require 'efrit-log)
(require 'efrit-session)

(ert-deftest test-executor-async-unicode-escaping ()
  "Test that async API requests properly escape and encode unicode."
  (let ((captured-request nil))
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (url callback &optional cbargs silent inhibit-cookies)
                 (setq captured-request url-request-data)))
              ((symbol-function 'efrit-common-get-api-key)
               (lambda () "test-key"))
              ((symbol-function 'efrit-common-get-api-url)
               (lambda () "https://api.test.com"))
              ((symbol-function 'efrit-common-build-headers)
               (lambda (key) '(("Content-Type" . "application/json")))))

      ;; Make a request with unicode content (smart quote U+2019)
      ;; Use (string 8217) to ensure we get the actual unicode character
      (let ((unicode-msg (concat "Symbol" (string 8217) "s function")))
        (efrit-executor--api-request
         `((messages . [((role . "user") (content . ,unicode-msg))])
           (model . "test"))
         #'ignore))

      ;; Verify request was captured
      (should captured-request)

      ;; Verify request is unibyte (required for HTTP)
      (should-not (multibyte-string-p captured-request))

      ;; Verify no literal unicode in request
      (should-not (string-match-p "[^\x00-\x7F]" captured-request))

      ;; Verify unicode was escaped to \u2019
      (should (string-match-p "\\\\u2019" captured-request)))))

(ert-deftest test-executor-sync-unicode-escaping ()
  "Test that synchronous API requests properly escape and encode unicode."
  (let ((captured-request nil))
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (url &optional silent inhibit-cookies timeout)
                 (setq captured-request url-request-data)
                 ;; Return a fake response buffer
                 (let ((buf (generate-new-buffer " *test-response*")))
                   (with-current-buffer buf
                     (insert "HTTP/1.1 200 OK\n\n")
                     (insert "{\"content\": [{\"type\": \"text\", \"text\": \"done\"}], \"stop_reason\": \"end_turn\"}"))
                   buf)))
              ((symbol-function 'efrit-common-get-api-key)
               (lambda () "test-key"))
              ((symbol-function 'efrit-common-get-api-url)
               (lambda () "https://api.test.com"))
              ((symbol-function 'efrit-common-build-headers)
               (lambda (key) '(("Content-Type" . "application/json"))))
              ((symbol-function 'efrit-session-create)
               (lambda (&rest _) (list :id "test-session")))
              ((symbol-function 'efrit-session-set-active)
               #'ignore)
              ((symbol-function 'efrit-log)
               #'ignore)
              ((symbol-function 'efrit-executor--build-system-prompt)
               (lambda (&rest _) "Test system prompt"))
              ((symbol-function 'efrit-executor--get-tools-schema)
               (lambda () []))
              ;; Mock the efrit-default-model variable access
              (efrit-default-model "test-model"))

      ;; Make a synchronous request with unicode content (smart quote U+2019)
      ;; Use (string 8217) to ensure we get the actual unicode character
      (let ((unicode-msg (concat "Symbol" (string 8217) "s function")))
        (ignore-errors
          (efrit-execute unicode-msg)))

      ;; Verify request was captured
      (should captured-request)

      ;; Verify request is unibyte (required for HTTP)
      (should-not (multibyte-string-p captured-request))

      ;; Verify no literal unicode in request
      (should-not (string-match-p "[^\x00-\x7F]" captured-request))

      ;; Verify unicode was escaped to \u2019
      (should (string-match-p "\\\\u2019" captured-request)))))

;;; Run tests
(ert-run-tests-batch-and-exit)
