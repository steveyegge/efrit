;;; test-api.el --- request context in API failures -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'efrit-api)
(require 'efrit-config)

(ert-deftest test-api-describe-failure-names-endpoint-purpose-model ()
  (let ((efrit-api-request-purpose nil) (efrit-default-model "m-default"))
    (let ((s (efrit-api-describe-failure "curl exit 7 (connection refused)"
                                         "https://gw.example.com/v1/messages?key=SECRET"
                                         "m-1" "curl (streaming)" "the model's next turn")))
      (should (string-match-p "\\`the model's next turn → https://gw.example.com/v1/messages (model m-1, via curl (streaming)) failed\\.\n" s))
      (should (string-match-p "connection refused" s))
      ;; the query string never appears
      (should-not (string-match-p "SECRET\\|key=" s)))
    ;; defaults: purpose from the dynamic var, model from efrit-default-model
    (let ((efrit-api-request-purpose "listing models"))
      (should (string-match-p "\\`listing models → .*model m-default" (efrit-api-describe-failure "x"))))))

(ert-deftest test-api-async-failure-carries-context ()
  "An error before the request leaves (no key) still says what was attempted."
  (let ((efrit-api-key nil) (got nil)
        (efrit-api-request-purpose "reviewing 2 proposed tool call(s)"))
    (cl-letf (((symbol-function 'efrit-common-get-api-key) (lambda () (error "No API key found"))))
      (efrit-api-request-async '(("model" . "m-2")) #'ignore (lambda (e) (setq got e)))
      (should got)
      (should (string-match-p "reviewing 2 proposed tool call" got))
      (should (string-match-p "model m-2" got))
      (should (string-match-p "No API key found" got)))))

(ert-deftest test-api-refusal-retries-with-inlined-system ()
  "A pre-generation refusal of a request with a system prompt is retried
once with the prompt folded into the first user message; the retry's
response is marked.  A refusal without a system prompt is returned as is."
  (let ((sent nil) (efrit-api-inline-system-on-refusal t))
    (cl-letf (((symbol-function 'efrit-api--request-sync-1)
               (lambda (req &optional _timeout)
                 (push req sent)
                 (let ((r (make-hash-table :test 'equal)))
                   (if (alist-get "system" req nil nil #'equal)
                       (progn (puthash "stop_reason" "refusal" r) (puthash "content" (vector) r))
                     (puthash "stop_reason" "end_turn" r))
                   r))))
      (let ((r (efrit-api-request-sync '(("model" . "m") ("system" . "Be brief.")
                                         ("messages" . [(("role" . "user") ("content" . "hi"))])))))
        (should (equal (gethash "stop_reason" r) "end_turn"))
        (should (gethash "efrit-inlined-system" r))
        (should (= 2 (length sent)))
        (let* ((retry (car sent))
               (first (aref (alist-get "messages" retry nil nil #'equal) 0)))
          (should-not (alist-get "system" retry nil nil #'equal))
          (should (string-match-p "\\`Be brief\\.\n\n---\n\nhi\\'" (alist-get "content" first nil nil #'equal)))))
      ;; a cacheable (vector) system prompt is flattened to its text
      (setq sent nil)
      (let ((r (efrit-api-request-sync `(("model" . "m")
                                         ("system" . ,(vector '(("type" . "text") ("text" . "Sys A")) '(("type" . "text") ("text" . "Sys B"))))
                                         ("messages" . [(("role" . "user") ("content" . "hi"))])))))
        (should (gethash "efrit-inlined-system" r))
        (should (string-prefix-p "Sys A\nSys B" (alist-get "content" (aref (alist-get "messages" (car sent) nil nil #'equal) 0) nil nil #'equal))))
      ;; no system prompt: no retry
      (setq sent nil)
      (cl-letf (((symbol-function 'efrit-api--request-sync-1)
                 (lambda (&rest _) (let ((r (make-hash-table :test 'equal))) (puthash "stop_reason" "refusal" r) r))))
        (let ((r (efrit-api-request-sync '(("model" . "m") ("messages" . [(("role" . "user") ("content" . "hi"))])))))
          (should (equal (gethash "stop_reason" r) "refusal")))))))

(ert-deftest test-api-sync-wait-keeps-emacs-alive ()
  "The synchronous wait runs the event loop: the callback set from a
timer lands, the status line is redrawn with the purpose and elapsed
seconds, and the response buffer comes back."
  (let ((messages nil) (efrit-api-sync-status t))
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (_url callback &rest _)
                 (let ((buf (generate-new-buffer " *fake-response*")))
                   (run-at-time 0.3 nil (lambda () (with-current-buffer buf (funcall callback nil))))
                   buf)))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (when fmt (push (apply #'format fmt args) messages)) nil)))
      (let ((buf (efrit-api--retrieve-and-wait "https://x" 5 "reviewing package foo" (float-time))))
        (should (buffer-live-p buf))
        (kill-buffer buf))
      (should (cl-some (lambda (m) (string-match-p "reviewing package foo… [0-9]+s/5s" m)) messages))
      ;; more than one frame was drawn: the spinner moved
      (should (> (length (delete-dups (mapcar (lambda (m) (substring m 0 1)) messages))) 1)))))

(ert-deftest test-api-sync-wait-times-out-and-stops-the-transfer ()
  (let ((deleted nil) (efrit-api-sync-status nil))
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (&rest _)
                 (let ((buf (generate-new-buffer " *fake-response*")))
                   (start-process "fake-transfer" buf "sleep" "30")
                   buf)))
              ((symbol-function 'delete-process)
               (lambda (p) (setq deleted t) (set-process-query-on-exit-flag p nil)
                 (ignore-errors (kill-process p)))))
      (should-not (efrit-api--retrieve-and-wait "https://x" 1 "slow thing" (float-time)))
      (should deleted)
      (should-not (get-buffer " *fake-response*"))))
  ;; the caller turns the nil into the usual failure message
  (cl-letf (((symbol-function 'efrit-api--retrieve-and-wait) (lambda (&rest _) nil))
            ((symbol-function 'efrit-common-get-api-key) (lambda () "sk-ant-test-key-0123456789"))
            ((symbol-function 'efrit-common-get-api-url) (lambda () "https://x/v1/messages")))
    (let ((err (should-error (efrit-api--request-sync-1 '(("model" . "m") ("messages" . [])) 7))))
      (should (string-match-p "No response within 7s" (cadr err))))))

(ert-deftest test-api-sync-status-line ()
  (should (equal (efrit-api--sync-status "probe x" (- (float-time) 12.4) 120 0)
                 "⠋ probe x… 12s/120s  (C-g stops the wait)"))
  (should (string-match-p "\\`⠙ waiting for the API… 0s  " (efrit-api--sync-status nil (float-time) nil 1))))

(ert-deftest test-api-invalid-json-error-keeps-the-body ()
  "An api_error about invalid JSON carries a report on the body that was
sent and the file it was saved to, so the parser at the other end can
be checked against the same bytes."
  (let* ((root (file-name-as-directory (make-temp-file "efrit-api-" t)))
         (efrit-data-directory root))
    (unwind-protect
        (progn
          (efrit-api-encode-request '(("model" . "m") ("messages" . [(("role" . "user") ("content" . "hé \U0001F600"))])))
          (with-temp-buffer
            (insert "HTTP/1.1 400 Bad Request\nContent-Type: application/json\n\n"
                    "{\"type\":\"error\",\"error\":{\"type\":\"api_error\",\"message\":\"Invalid JSON\"}}")
            (let ((err (should-error (efrit-api-parse-response))))
              (should (string-match-p "API Error (api_error): Invalid JSON -- last request body: [0-9]+ bytes, parses by Emacs" (cadr err)))
              (should (string-match-p "non-ASCII 0, raw controls 0, 5\\+ digit escapes 0, lone high surrogates 0" (cadr err)))
              (should (string-match-p (regexp-quote root) (cadr err))))
            (should (string-match-p "Invalid JSON -- last request body" (efrit-api--error-from-body))))
          (let ((files (directory-files root nil "request-body-.*\\.json\\'")))
            (should files)
            (should (= 0 (logand (file-modes (expand-file-name (car files) root)) #o077)))
            (with-temp-buffer
              (set-buffer-multibyte nil)
              (insert-file-contents-literally (expand-file-name (car files) root))
              (should (equal (buffer-string) efrit-api--last-body)))))
      (delete-directory root t))))

(provide 'test-api)
;;; test-api.el ends here
