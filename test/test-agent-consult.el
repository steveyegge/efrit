;;; test-agent-consult.el --- list_sessions and ask_session -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'efrit-agent)
(require 'efrit-agent-consult)
(require 'efrit-repl-session)
(require 'efrit-tool-registry)

(defun test-consult--buffer (name)
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t)) (erase-buffer))
      (efrit-agent-mode)
      (efrit-agent--init-regions)
      (efrit-agent--setup-regions)
      (setq default-directory temporary-file-directory)
      (setq efrit-agent--repl-session (efrit-repl-session-create temporary-file-directory))
      (setf (efrit-repl-session-buffer efrit-agent--repl-session) buf))
    buf))

(defun test-consult--hash (&rest kv)
  (let ((h (make-hash-table :test 'equal)))
    (while kv (puthash (pop kv) (pop kv) h))
    h))

(ert-deftest test-consult-lists-peers-and-answers-from-their-conversation ()
  (let* ((efrit-agent-auto-show nil)
         (efrit-sandbox-enabled nil)
         (a (test-consult--buffer "*efrit[alpha]*"))
         (b (test-consult--buffer "*efrit[beta:fix]*"))
         (sb (buffer-local-value 'efrit-agent--repl-session b))
         (efrit-current-session-id (efrit-repl-session-id (buffer-local-value 'efrit-agent--repl-session a)))
         (sent nil) (notes nil))
    (unwind-protect
        (progn
          (setf (efrit-repl-session-conversation sb)
                (list (list :role 'assistant :content "Added six retry tests in test/queue; shutdown is covered by two.")
                      (list :role 'user :content "Add tests for the retry queue consumer")))
          ;; list: the asking session is not a peer of itself
          (let ((listing (efrit-tool-registry-dispatch "list_sessions" (test-consult--hash))))
            (should (string-match-p "beta:fix" listing))
            (should-not (string-match-p "alpha" listing))
            (should (string-match-p "last asked: Add tests" listing)))
          ;; ask: a side request carrying the peer's conversation; the peer is noted
          (efrit-subscribe 'note (lambda (e) (push (alist-get :text e) notes)))
          (cl-letf (((symbol-function 'efrit-api-request-sync)
                     (lambda (request &optional _timeout)
                       (setq sent request)
                       (test-consult--hash "stop_reason" "end_turn"
                                           "content" (vector (test-consult--hash "type" "text" "text" "Yes, two of six cover shutdown."))))))
            (let ((answer (efrit-tool-registry-dispatch
                           "ask_session" (test-consult--hash "session" "beta:fix" "question" "Do the tests cover shutdown?"))))
              (should (string-match-p "Yes, two of six" answer))
              (should (string-match-p "Answer about beta:fix" answer))
              (let ((prompt (alist-get "content" (aref (alist-get "messages" sent nil nil #'equal) 0) nil nil #'equal)))
                (should (string-match-p "USER: Add tests for the retry queue" prompt))
                (should (string-match-p "ASSISTANT: Added six retry tests" prompt))
                (should (string-match-p "Question from a peer agent: Do the tests cover shutdown" prompt)))
              (should (seq-some (lambda (n) (string-match-p "consulted by" n)) notes))
              ;; the peer's own state is untouched
              (should (= 2 (length (efrit-repl-session-conversation sb))))
              (should (null (efrit-repl-session-queue sb)))))
          ;; an unknown peer is an error that lists the choices
          (should (string-match-p "\\`Error: no other agent session named \"nope\""
                                  (efrit-tool-registry-dispatch "ask_session" (test-consult--hash "session" "nope" "question" "x")))))
      (kill-buffer a) (kill-buffer b))))

(provide 'test-agent-consult)

;;; test-agent-consult.el ends here
