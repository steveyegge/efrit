;;; test-api-stream.el --- streaming transport, end to end over a fake curl -*- lexical-binding: t; -*-

;;; Commentary:
;; Binds `efrit-api-stream-curl-program' to a script that emits a canned
;; Anthropic SSE stream (minuet's trick).  Real subprocess, real filter,
;; real reassembly; zero network and no stubbed elisp.

;;; Code:

(require 'ert)
(require 'efrit-api-stream)

(defconst test-stream--script
  (expand-file-name "scripts/mock-anthropic-stream.py"
                    (file-name-directory (or load-file-name buffer-file-name))))

(defun test-stream--wait (pred &optional secs)
  (let ((deadline (+ (float-time) (or secs 5))))
    (while (and (not (funcall pred)) (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (funcall pred)))

(defmacro test-stream--request (user-text &rest body)
  "Run a streaming request whose user message is USER-TEXT; bind
`resp', `err', `deltas' (list of on-text strings, in order) for BODY."
  (declare (indent 1))
  `(let ((efrit-api-stream-curl-program test-stream--script)
         (efrit-api-key "sk-test-key-1234567890abcdefghij")
         (efrit-api-auth-scheme 'x-api-key)
         (efrit-api-streaming t)
         (efrit-api-prompt-caching nil)
         resp err done (deltas nil))
     (skip-unless (executable-find "python3"))
     (let ((st (efrit-api-stream-request
                '(("model" . "mock") ("max_tokens" . 10)
                  ("messages" . [(("role" . "user") ("content" . ,user-text))]))
                (lambda (r e) (setq resp r err e done t))
                (lambda (text) (push text deltas)))))
       (ignore st)
       (should (test-stream--wait (lambda () done) 10))
       (setq deltas (nreverse deltas))
       ,@body)))

(ert-deftest test-stream-text-response-assembled-and-streamed ()
  (test-stream--request "please text"
    (should-not err)
    (should (equal deltas '("Hello, " "world.")))
    (should (equal (gethash "stop_reason" resp) "end_turn"))
    (let ((content (gethash "content" resp)))
      (should (= (length content) 1))
      (should (equal (gethash "type" (aref content 0)) "text"))
      (should (equal (gethash "text" (aref content 0)) "Hello, world.")))
    ;; usage merged from message_start + message_delta
    (should (= (gethash "input_tokens" (gethash "usage" resp)) 12))
    (should (= (gethash "output_tokens" (gethash "usage" resp)) 7))
    (should-not (gethash "efrit_partial" resp))))

(ert-deftest test-stream-tool-use-input-reassembled-from-json-deltas ()
  (test-stream--request "use a tool"
    (should-not err)
    (should (equal (gethash "stop_reason" resp) "tool_use"))
    (let* ((content (gethash "content" resp))
           (tool (aref content 1)))
      (should (= (length content) 2))
      (should (equal (gethash "type" tool) "tool_use"))
      (should (equal (gethash "name" tool) "eval_sexp"))
      (should (equal (gethash "id" tool) "toolu_mock1"))
      (should (hash-table-p (gethash "input" tool)))
      (should (equal (gethash "expr" (gethash "input" tool)) "(+ 1 2)")))
    ;; downstream helpers understand the assembled shape
    (should (equal (nth 1 (efrit-content-item-as-tool-use (aref (gethash "content" resp) 1)))
                   "eval_sexp"))))

(ert-deftest test-stream-error-event-becomes-api-error ()
  (test-stream--request "trigger error"
    (should-not resp)
    (should (string-match-p "API Error (invalid_request_error): no keys found" err))))

(ert-deftest test-stream-http-failure-body-surfaces ()
  (test-stream--request "http401"
    (should-not resp)
    (should (string-match-p "authentication_error\\|invalid x-api-key" err))))

(ert-deftest test-stream-timeout-delivers-partial ()
  "curl exit 28 after content arrived = degraded success, marked partial."
  (test-stream--request "truncate"
    (should-not err)
    (should (gethash "efrit_partial" resp))
    (should (equal (gethash "text" (aref (gethash "content" resp) 0)) "partial answer"))
    (should (equal (gethash "stop_reason" resp) "end_turn"))))

(ert-deftest test-stream-cancel-interrupts ()
  (let ((efrit-api-stream-curl-program test-stream--script)
        (efrit-api-key "sk-test-key-1234567890abcdefghij")
        (efrit-api-auth-scheme 'x-api-key)
        (efrit-api-prompt-caching nil)
        resp err done first)
    (skip-unless (executable-find "python3"))
    (let ((st (efrit-api-stream-request
               '(("model" . "mock") ("max_tokens" . 10)
                 ("messages" . [(("role" . "user") ("content" . "cancel me"))]))
               (lambda (r e) (setq resp r err e done t))
               (lambda (_text) (setq first t)))))
      (should (test-stream--wait (lambda () first) 5))
      (should (memq st efrit-api-stream--active))
      (efrit-api-stream-cancel st)
      (should (test-stream--wait (lambda () done) 5))
      (should-not resp)
      (should (equal err "interrupted"))
      (should-not (memq st efrit-api-stream--active)))))

(ert-deftest test-stream-config-file-holds-headers-not-argv ()
  "Credentials go in a 0600 config file that is removed afterwards; argv has none."
  (let ((efrit-api-stream-curl-program test-stream--script)
        (efrit-api-key "sk-test-key-1234567890abcdefghij")
        (efrit-api-auth-scheme 'x-api-key)
        (efrit-api-prompt-caching nil)
        done cfg cmd)
    (skip-unless (executable-find "python3"))
    (let ((st (efrit-api-stream-request
               '(("model" . "mock") ("messages" . [(("role" . "user") ("content" . "text"))]))
               (lambda (&rest _) (setq done t)))))
      (setq cfg (efrit-api-stream-config-file st)
            cmd (process-command (efrit-api-stream-process st)))
      (should (file-exists-p cfg))
      (should (= (logand (file-modes cfg) #o077) 0))
      (with-temp-buffer (insert-file-contents cfg)
                        (should (string-match-p "x-api-key: sk-test" (buffer-string))))
      (should-not (cl-some (lambda (a) (string-match-p "sk-test" a)) cmd))
      (should (test-stream--wait (lambda () done) 10))
      (should-not (file-exists-p cfg)))))

(ert-deftest test-stream-sse-parser-handles-split-and-keepalive ()
  "Direct parser test: events split across chunks, comments, CRLF."
  (let ((st (efrit-api-stream--make :callback #'ignore)))
    (efrit-api-stream--consume st "event: message_start\r\ndata: {\"type\":\"message_start\",\"message\":{\"usage\":{\"input_tokens\":1}}}\r\n\r\n: keepalive\r\n\r\nevent: content_block_start\ndata: {\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"text\",\"text\":\"\"}}\n\nevent: content_block_delta\ndata: {\"type\":\"content_block_delta\",\"in")
    (should (equal (efrit-api-stream-buffer st)
                   "event: content_block_delta\ndata: {\"type\":\"content_block_delta\",\"in"))
    (efrit-api-stream--consume st "dex\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"hi\"}}\n\n")
    (should (equal (efrit-api-stream-buffer st) ""))
    (should (equal (gethash "text" (cdr (assoc 0 (efrit-api-stream-blocks st)))) "hi"))))

(provide 'test-api-stream)
;;; test-api-stream.el ends here
