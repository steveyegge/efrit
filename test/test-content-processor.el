;;; test-content-processor.el --- Tests for efrit-content-processor module -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'efrit-content-processor)
(require 'efrit-chat-response)

;;; Text Block Tests

(ert-deftest efrit-text-block-creation ()
  "Text blocks can be created with text content."
  (let ((block (efrit-text-block-make "Hello, world!")))
    (should (efrit-text-block-p block))
    (should (string= (efrit-text-block-text block) "Hello, world!"))
    (should (string= (efrit-content-block-type block) "text"))))

(ert-deftest efrit-text-block-valid-p-accepts-valid ()
  "Text block validation passes for valid blocks."
  (let ((block (efrit-text-block-make "Some text")))
    (should (efrit-text-block-valid-p block))))

(ert-deftest efrit-text-block-valid-p-rejects-empty ()
  "Text block validation rejects empty text."
  (let ((block (efrit-text-block-make "")))
    (should-not (efrit-text-block-valid-p block))))

;;; Tool Use Block Tests

(ert-deftest efrit-tool-use-block-creation ()
  "Tool use blocks can be created with all fields."
  (let* ((input (make-hash-table :test 'equal))
         (block (efrit-tool-use-block-make "tool-123" "find_files" input)))
    (should (efrit-tool-use-block-p block))
    (should (string= (efrit-tool-use-block-id block) "tool-123"))
    (should (string= (efrit-tool-use-block-name block) "find_files"))
    (should (eq (efrit-tool-use-block-input block) input))
    (should (string= (efrit-content-block-type block) "tool_use"))))

(ert-deftest efrit-tool-use-block-valid-p-accepts-valid ()
  "Tool use block validation passes for valid blocks."
  (let* ((input (make-hash-table :test 'equal))
         (block (efrit-tool-use-block-make "id" "name" input)))
    (should (efrit-tool-use-block-valid-p block))))

(ert-deftest efrit-tool-use-block-valid-p-rejects-empty-id ()
  "Tool use block validation rejects empty ID."
  (let* ((input (make-hash-table :test 'equal))
         (block (efrit-tool-use-block-make "" "name" input)))
    (should-not (efrit-tool-use-block-valid-p block))))

(ert-deftest efrit-tool-use-block-valid-p-rejects-empty-name ()
  "Tool use block validation rejects empty name."
  (let* ((input (make-hash-table :test 'equal))
         (block (efrit-tool-use-block-make "id" "" input)))
    (should-not (efrit-tool-use-block-valid-p block))))

;;; Tool Result Block Tests

(ert-deftest efrit-tool-result-block-creation ()
  "Tool result blocks can be created."
  (let ((block (efrit-tool-result-block-make "tool-123" "Success result")))
    (should (efrit-tool-result-block-p block))
    (should (string= (efrit-tool-result-block-tool-id block) "tool-123"))
    (should (string= (efrit-tool-result-block-content block) "Success result"))
    (should-not (efrit-tool-result-block-is-error block))
    (should (string= (efrit-content-block-type block) "tool_result"))))

(ert-deftest efrit-tool-result-block-creation-with-error ()
  "Tool result blocks can be created with is-error flag."
  (let ((block (efrit-tool-result-block-make "tool-456" "Error message" t)))
    (should (efrit-tool-result-block-is-error block))))

(ert-deftest efrit-tool-result-block-valid-p-accepts-valid ()
  "Tool result block validation passes for valid blocks."
  (let ((block (efrit-tool-result-block-make "id" "content")))
    (should (efrit-tool-result-block-valid-p block))))

(ert-deftest efrit-tool-result-block-valid-p-rejects-empty-id ()
  "Tool result block validation rejects empty ID."
  (let ((block (efrit-tool-result-block-make "" "content")))
    (should-not (efrit-tool-result-block-valid-p block))))

;;; Factory Function Tests

(ert-deftest efrit-content-block-from-hash-parses-text ()
  "Factory function creates text blocks from hash-tables."
  (let ((item (make-hash-table :test 'equal)))
    (puthash "type" "text" item)
    (puthash "text" "Hello" item)
    
    (let ((block (efrit-content-block-from-hash item)))
      (should (efrit-text-block-p block))
      (should (string= (efrit-text-block-text block) "Hello")))))

(ert-deftest efrit-content-block-from-hash-parses-tool-use ()
  "Factory function creates tool_use blocks from hash-tables."
  (let* ((input (make-hash-table :test 'equal))
         (item (make-hash-table :test 'equal)))
    (puthash "pattern" "*.el" input)
    (puthash "type" "tool_use" item)
    (puthash "id" "toolcall-789" item)
    (puthash "name" "find_files" item)
    (puthash "input" input item)
    
    (let ((block (efrit-content-block-from-hash item)))
      (should (efrit-tool-use-block-p block))
      (should (string= (efrit-tool-use-block-id block) "toolcall-789"))
      (should (string= (efrit-tool-use-block-name block) "find_files")))))

(ert-deftest efrit-content-block-from-hash-returns-nil-for-invalid ()
  "Factory function returns nil for invalid blocks."
  (let ((item (make-hash-table :test 'equal)))
    (puthash "type" "unknown_type" item)
    (should-not (efrit-content-block-from-hash item))))

(ert-deftest efrit-content-block-from-hash-handles-empty-text ()
  "Factory function skips empty text blocks (via when-let check)."
  (let ((item (make-hash-table :test 'equal)))
    (puthash "type" "text" item)
    ;; Note: Not setting "text" field - gethash will return nil
    (should-not (efrit-content-block-from-hash item))))

(ert-deftest efrit-content-block-from-hash-returns-nil-for-non-hash ()
  "Factory function returns nil for non-hash-table input."
  (should-not (efrit-content-block-from-hash "not a hash")))

;;; Conversion Tests

(ert-deftest efrit-content-block-to-hash-text ()
  "Text blocks convert back to hash-table correctly."
  (let* ((block (efrit-text-block-make "Hello"))
         (hash (efrit-content-block-to-hash block)))
    (should (string= (gethash "type" hash) "text"))
    (should (string= (gethash "text" hash) "Hello"))))

(ert-deftest efrit-content-block-to-hash-tool-use ()
  "Tool use blocks convert back to hash-table correctly."
  (let* ((input (make-hash-table :test 'equal))
         (block (efrit-tool-use-block-make "id123" "mytool" input))
         (hash (efrit-content-block-to-hash block)))
    (should (string= (gethash "type" hash) "tool_use"))
    (should (string= (gethash "id" hash) "id123"))
    (should (string= (gethash "name" hash) "mytool"))
    (should (eq (gethash "input" hash) input))))

(ert-deftest efrit-content-block-to-hash-tool-result ()
  "Tool result blocks convert back to hash-table correctly."
  (let* ((block (efrit-tool-result-block-make "toolid" "Result text" t))
         (hash (efrit-content-block-to-hash block)))
    (should (string= (gethash "type" hash) "tool_result"))
    (should (string= (gethash "tool_use_id" hash) "toolid"))
    (should (string= (gethash "content" hash) "Result text"))))

;;; Validator Tests

(ert-deftest efrit-content-block-valid-p-rejects-wrong-type ()
  "Content block validation rejects non-content-block objects."
  (should-not (efrit-content-block-valid-p "not a block")))

(ert-deftest efrit-content-block-valid-p-accepts-text-block ()
  "Content block validation accepts text blocks."
  (let ((block (efrit-text-block-make "text")))
    (should (efrit-content-block-valid-p block))))

(provide 'test-content-processor)
;;; test-content-processor.el ends here
