;;; test-chat-response.el --- Tests for efrit-chat-response module -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'efrit-chat-response)

;;; Error Handling Tests

(ert-deftest efrit-response-error-returns-nil-for-success ()
  "efrit-response-error returns nil for successful response."
  (let ((response (make-hash-table :test 'equal)))
    (puthash "content" (vector) response)
    (puthash "stop_reason" "end_turn" response)
    (should-not (efrit-response-error response))))

(ert-deftest efrit-response-error-returns-error-object ()
  "efrit-response-error returns error object when present."
  (let* ((error-obj (make-hash-table :test 'equal))
         (response (make-hash-table :test 'equal)))
    (puthash "type" "invalid_request_error" error-obj)
    (puthash "message" "Invalid API key" error-obj)
    (puthash "error" error-obj response)
    (should (eq (efrit-response-error response) error-obj))))

(ert-deftest efrit-error-type-returns-type-string ()
  "efrit-error-type extracts type field."
  (let ((error-obj (make-hash-table :test 'equal)))
    (puthash "type" "rate_limit_error" error-obj)
    (should (string= (efrit-error-type error-obj) "rate_limit_error"))))

(ert-deftest efrit-error-type-returns-unknown-for-missing-type ()
  "efrit-error-type returns 'unknown' when type is missing."
  (let ((error-obj (make-hash-table :test 'equal)))
    (should (string= (efrit-error-type error-obj) "unknown"))))

(ert-deftest efrit-error-type-handles-non-hash-table ()
  "efrit-error-type handles non-hash-table input gracefully."
  (should (string= (efrit-error-type "not a hash") "unknown")))

(ert-deftest efrit-error-message-returns-message-string ()
  "efrit-error-message extracts message field."
  (let ((error-obj (make-hash-table :test 'equal)))
    (puthash "message" "Rate limit exceeded" error-obj)
    (should (string= (efrit-error-message error-obj) "Rate limit exceeded"))))

(ert-deftest efrit-error-message-returns-default-for-missing ()
  "efrit-error-message returns default when message is missing."
  (let ((error-obj (make-hash-table :test 'equal)))
    (should (string= (efrit-error-message error-obj) "unknown error"))))

;;; Response Content Tests

(ert-deftest efrit-response-content-returns-content-vector ()
  "efrit-response-content extracts content field."
  (let* ((content (vector (make-hash-table :test 'equal)))
         (response (make-hash-table :test 'equal)))
    (puthash "content" content response)
    (should (eq (efrit-response-content response) content))))

(ert-deftest efrit-response-content-returns-nil-when-missing ()
  "efrit-response-content returns nil when content is missing."
  (let ((response (make-hash-table :test 'equal)))
    (should-not (efrit-response-content response))))

(ert-deftest efrit-response-stop-reason-returns-stop-reason ()
  "efrit-response-stop-reason extracts stop_reason field."
  (let ((response (make-hash-table :test 'equal)))
    (puthash "stop_reason" "end_turn" response)
    (should (string= (efrit-response-stop-reason response) "end_turn"))))

(ert-deftest efrit-response-stop-reason-returns-nil-when-missing ()
  "efrit-response-stop-reason returns nil when missing."
  (let ((response (make-hash-table :test 'equal)))
    (should-not (efrit-response-stop-reason response))))

(ert-deftest efrit-response-usage-returns-usage-object ()
  "efrit-response-usage extracts usage object."
  (let* ((usage (make-hash-table :test 'equal))
         (response (make-hash-table :test 'equal)))
    (puthash "input_tokens" 100 usage)
    (puthash "usage" usage response)
    (should (eq (efrit-response-usage response) usage))))

(ert-deftest efrit-usage-input-tokens-returns-token-count ()
  "efrit-usage-input-tokens extracts input token count."
  (let ((usage (make-hash-table :test 'equal)))
    (puthash "input_tokens" 250 usage)
    (should (= (efrit-usage-input-tokens usage) 250))))

(ert-deftest efrit-usage-input-tokens-returns-zero-when-missing ()
  "efrit-usage-input-tokens returns 0 when missing."
  (let ((usage (make-hash-table :test 'equal)))
    (should (= (efrit-usage-input-tokens usage) 0))))

(ert-deftest efrit-usage-output-tokens-returns-token-count ()
  "efrit-usage-output-tokens extracts output token count."
  (let ((usage (make-hash-table :test 'equal)))
    (puthash "output_tokens" 350 usage)
    (should (= (efrit-usage-output-tokens usage) 350))))

;;; Content Item Tests

(ert-deftest efrit-content-item-type-returns-type ()
  "efrit-content-item-type extracts type field."
  (let ((item (make-hash-table :test 'equal)))
    (puthash "type" "text" item)
    (should (string= (efrit-content-item-type item) "text"))))

(ert-deftest efrit-content-item-type-returns-nil-for-missing ()
  "efrit-content-item-type returns nil when type is missing."
  (let ((item (make-hash-table :test 'equal)))
    (should-not (efrit-content-item-type item))))

(ert-deftest efrit-content-item-text-returns-text-content ()
  "efrit-content-item-text extracts text field for text blocks."
  (let ((item (make-hash-table :test 'equal)))
    (puthash "type" "text" item)
    (puthash "text" "Hello, world!" item)
    (should (string= (efrit-content-item-text item) "Hello, world!"))))

(ert-deftest efrit-content-item-text-returns-nil-for-non-text ()
  "efrit-content-item-text returns nil for non-text blocks."
  (let ((item (make-hash-table :test 'equal)))
    (puthash "type" "tool_use" item)
    (puthash "text" "Should be ignored" item)
    (should-not (efrit-content-item-text item))))

(ert-deftest efrit-content-item-text-returns-nil-when-missing ()
  "efrit-content-item-text returns nil when text field is missing."
  (let ((item (make-hash-table :test 'equal)))
    (puthash "type" "text" item)
    (should-not (efrit-content-item-text item))))

(ert-deftest efrit-content-item-as-tool-use-parses-tool-block ()
  "efrit-content-item-as-tool-use parses tool_use blocks."
  (let* ((input (make-hash-table :test 'equal))
         (item (make-hash-table :test 'equal)))
    (puthash "pattern" "*.el" input)
    (puthash "type" "tool_use" item)
    (puthash "id" "tool-123" item)
    (puthash "name" "find_files" item)
    (puthash "input" input item)
    
    (let ((result (efrit-content-item-as-tool-use item)))
      (should result)
      (should (string= (car result) "tool-123"))
      (should (string= (cadr result) "find_files"))
      (should (eq (caddr result) input)))))

(ert-deftest efrit-content-item-as-tool-use-returns-nil-for-non-tool ()
  "efrit-content-item-as-tool-use returns nil for non-tool blocks."
  (let ((item (make-hash-table :test 'equal)))
    (puthash "type" "text" item)
    (puthash "id" "text-456" item)
    (should-not (efrit-content-item-as-tool-use item))))

;;; Tool Use Accessor Tests

(ert-deftest efrit-tool-use-id-returns-id ()
  "efrit-tool-use-id extracts id field."
  (let ((item (make-hash-table :test 'equal)))
    (puthash "id" "toolcall-789" item)
    (should (string= (efrit-tool-use-id item) "toolcall-789"))))

(ert-deftest efrit-tool-use-name-returns-name ()
  "efrit-tool-use-name extracts name field."
  (let ((item (make-hash-table :test 'equal)))
    (puthash "name" "write_todo" item)
    (should (string= (efrit-tool-use-name item) "write_todo"))))

(ert-deftest efrit-tool-use-input-returns-input ()
  "efrit-tool-use-input extracts input field."
  (let* ((input (make-hash-table :test 'equal))
         (item (make-hash-table :test 'equal)))
    (puthash "key" "value" input)
    (puthash "input" input item)
    (should (eq (efrit-tool-use-input item) input))))

;;; Error Classification Tests

(ert-deftest efrit-classify-error-recognizes-rate-limit ()
  "efrit-classify-error identifies rate_limit_error."
  (let ((error-obj (make-hash-table :test 'equal)))
    (puthash "type" "rate_limit_error" error-obj)
    (should (eq (efrit-classify-error error-obj) :rate-limit))))

(ert-deftest efrit-classify-error-recognizes-auth-error ()
  "efrit-classify-error identifies authentication errors."
  (let ((error-obj (make-hash-table :test 'equal)))
    (puthash "type" "authentication_error" error-obj)
    (should (eq (efrit-classify-error error-obj) :auth))))

(ert-deftest efrit-classify-error-recognizes-invalid-error ()
  "efrit-classify-error identifies invalid request errors."
  (let ((error-obj (make-hash-table :test 'equal)))
    (puthash "type" "invalid_request_error" error-obj)
    (should (eq (efrit-classify-error error-obj) :invalid))))

(ert-deftest efrit-classify-error-recognizes-server-error ()
  "efrit-classify-error identifies server errors."
  (let ((error-obj (make-hash-table :test 'equal)))
    (puthash "type" "api_error" error-obj)
    (should (eq (efrit-classify-error error-obj) :server))))

(ert-deftest efrit-classify-error-defaults-to-unknown ()
  "efrit-classify-error returns :unknown for unrecognized types."
  (let ((error-obj (make-hash-table :test 'equal)))
    (puthash "type" "future_error_type" error-obj)
    (should (eq (efrit-classify-error error-obj) :unknown))))

(provide 'test-chat-response)
;;; test-chat-response.el ends here
