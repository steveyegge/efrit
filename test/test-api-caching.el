;;; test-api-caching.el --- Tests for prompt caching helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the efrit-api prompt-caching helpers (ef-tgt):
;; efrit-api-cacheable-system, efrit-api-cacheable-tools, and
;; efrit-api-cacheable-messages.  These mark cache_control breakpoints
;; on copies at request-build time; the key invariants are that marks
;; land on the right (last) element, that stored structures are never
;; mutated, and that unrecognized shapes pass through unchanged.

;;; Code:

(require 'ert)
(require 'json)
(require 'efrit-api)

(defun test-api-caching--block-cache-control (block)
  "Get the cache_control value from BLOCK (alist or hash table)."
  (if (hash-table-p block)
      (gethash "cache_control" block)
    (cdr (assoc "cache_control" block))))

;;; System Prompt

(ert-deftest efrit-api-cacheable-system-wraps-string ()
  "A string system prompt becomes a one-block vector with a breakpoint."
  (let* ((efrit-api-prompt-caching t)
         (result (efrit-api-cacheable-system "You are Efrit.")))
    (should (vectorp result))
    (should (= (length result) 1))
    (let ((block (aref result 0)))
      (should (equal (cdr (assoc "type" block)) "text"))
      (should (equal (cdr (assoc "text" block)) "You are Efrit."))
      (should (equal (test-api-caching--block-cache-control block)
                     '(("type" . "ephemeral")))))))

(ert-deftest efrit-api-cacheable-system-disabled-passthrough ()
  "With caching disabled, the system prompt is returned unchanged."
  (let ((efrit-api-prompt-caching nil))
    (should (equal (efrit-api-cacheable-system "prompt") "prompt"))))

(ert-deftest efrit-api-cacheable-system-non-string-passthrough ()
  "Non-string system values pass through unchanged."
  (let ((efrit-api-prompt-caching t))
    (should (null (efrit-api-cacheable-system nil)))))

;;; Tools

(ert-deftest efrit-api-cacheable-tools-marks-last-entry-only ()
  "Only the last tool gets a breakpoint; earlier tools are untouched."
  (let* ((efrit-api-prompt-caching t)
         (tools (vector '(("name" . "tool_a")) '(("name" . "tool_b"))))
         (result (efrit-api-cacheable-tools tools)))
    (should-not (test-api-caching--block-cache-control (aref result 0)))
    (should (test-api-caching--block-cache-control (aref result 1)))
    (should (equal (cdr (assoc "name" (aref result 1))) "tool_b"))))

(ert-deftest efrit-api-cacheable-tools-does-not-mutate-original ()
  "The shared tools schema vector must never be mutated."
  (let* ((efrit-api-prompt-caching t)
         (tools (vector '(("name" . "tool_a")))))
    (efrit-api-cacheable-tools tools)
    (should-not (test-api-caching--block-cache-control (aref tools 0)))))

(ert-deftest efrit-api-cacheable-tools-empty-and-disabled ()
  "Empty vectors and disabled caching pass through unchanged."
  (let ((efrit-api-prompt-caching t))
    (should (equal (efrit-api-cacheable-tools (vector)) (vector))))
  (let ((efrit-api-prompt-caching nil)
        (tools (vector '(("name" . "tool_a")))))
    (should (eq (efrit-api-cacheable-tools tools) tools))))

;;; Messages

(ert-deftest efrit-api-cacheable-messages-string-content ()
  "String content in the last message becomes a marked text block."
  (let* ((efrit-api-prompt-caching t)
         (messages (vector '(("role" . "user") ("content" . "hello"))))
         (result (efrit-api-cacheable-messages messages))
         (content (cdr (assoc "content" (aref result 0)))))
    (should (vectorp content))
    (should (equal (cdr (assoc "text" (aref content 0))) "hello"))
    (should (test-api-caching--block-cache-control (aref content 0)))))

(ert-deftest efrit-api-cacheable-messages-marks-last-block-of-last-message ()
  "Only the final content block of the final message gets the mark."
  (let* ((efrit-api-prompt-caching t)
         (messages
          (vector
           '(("role" . "user") ("content" . "first"))
           `(("role" . "user")
             ("content" . ,(vector '((type . "tool_result")
                                     (tool_use_id . "t1")
                                     (content . "r1"))
                                   '((type . "tool_result")
                                     (tool_use_id . "t2")
                                     (content . "r2")))))))
         (result (efrit-api-cacheable-messages messages))
         (content (cdr (assoc "content" (aref result 1)))))
    ;; First message untouched (still a plain string)
    (should (stringp (cdr (assoc "content" (aref result 0)))))
    ;; Last block of last message marked; first block untouched
    (should-not (test-api-caching--block-cache-control (aref content 0)))
    (should (test-api-caching--block-cache-control (aref content 1)))
    ;; Symbol-keyed tool_result fields preserved
    (should (equal (cdr (assq 'tool_use_id (aref content 1))) "t2"))))

(ert-deftest efrit-api-cacheable-messages-hash-table-blocks ()
  "Hash-table messages/blocks (echoed API responses) are handled."
  (let* ((efrit-api-prompt-caching t)
         (block (make-hash-table :test 'equal))
         (msg (make-hash-table :test 'equal)))
    (puthash "type" "text" block)
    (puthash "text" "hi" block)
    (puthash "role" "user" msg)
    (puthash "content" (vector block) msg)
    (let* ((result (efrit-api-cacheable-messages (vector msg)))
           (marked (aref (gethash "content" (aref result 0)) 0)))
      (should (test-api-caching--block-cache-control marked))
      ;; Original block and message untouched
      (should-not (gethash "cache_control" block))
      (should (eq (aref (gethash "content" msg) 0) block)))))

(ert-deftest efrit-api-cacheable-messages-does-not-mutate-history ()
  "Stored conversation history must never accumulate breakpoints."
  (let* ((efrit-api-prompt-caching t)
         (inner '(("type" . "text") ("text" . "x")))
         (messages (vector `(("role" . "user") ("content" . ,(vector inner))))))
    (efrit-api-cacheable-messages messages)
    (should-not (test-api-caching--block-cache-control inner))
    (should-not (test-api-caching--block-cache-control
                 (aref (cdr (assoc "content" (aref messages 0))) 0)))))

(ert-deftest efrit-api-cacheable-messages-edge-cases ()
  "Empty, disabled, and unrecognized shapes pass through unchanged."
  (let ((efrit-api-prompt-caching t))
    (should (equal (efrit-api-cacheable-messages (vector)) (vector)))
    ;; Message with empty content vector: unrecognized, unchanged
    (let ((messages (vector '(("role" . "user") ("content" . [])))))
      (should (eq (efrit-api-cacheable-messages messages) messages))))
  (let ((efrit-api-prompt-caching nil)
        (messages (vector '(("role" . "user") ("content" . "hi")))))
    (should (eq (efrit-api-cacheable-messages messages) messages))))

;;; JSON Encoding

(ert-deftest efrit-api-caching-encodes-to-valid-json ()
  "A fully marked request encodes to JSON with cache_control objects."
  (let* ((efrit-api-prompt-caching t)
         (request
          `(("model" . "claude-test")
            ("max_tokens" . 8192)
            ("messages" . ,(efrit-api-cacheable-messages
                            (vector '(("role" . "user") ("content" . "go")))))
            ("system" . ,(efrit-api-cacheable-system "sys"))
            ("tools" . ,(efrit-api-cacheable-tools
                         (vector '(("name" . "eval_sexp")))))))
         (parsed (json-parse-string (json-encode request)
                                    :object-type 'hash-table)))
    (should (equal (gethash "type"
                            (gethash "cache_control"
                                     (aref (gethash "system" parsed) 0)))
                   "ephemeral"))
    (let ((tool (aref (gethash "tools" parsed) 0)))
      (should (gethash "cache_control" tool))
      (should (equal (gethash "name" tool) "eval_sexp")))
    (let* ((msg (aref (gethash "messages" parsed) 0))
           (block (aref (gethash "content" msg) 0)))
      (should (equal (gethash "type" (gethash "cache_control" block))
                     "ephemeral")))))

(provide 'test-api-caching)
;;; test-api-caching.el ends here
