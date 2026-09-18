;;; test-api.el --- request context in API failures -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'efrit-api)

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

(provide 'test-api)
;;; test-api.el ends here
