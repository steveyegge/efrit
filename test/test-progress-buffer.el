;;; test-progress-buffer.el --- Tests for efrit-progress-buffer -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'efrit-progress-buffer)
(require 'efrit-log)

(defgroup test-progress-buffer nil
  "Tests for progress buffer infrastructure."
  :group 'efrit)

;;; Test Fixtures

(defun test-progress-buffer-cleanup ()
  "Clean up test progress buffers after each test."
  (maphash (lambda (session-id buffer)
             (when (buffer-live-p buffer)
               (kill-buffer buffer)))
           efrit-progress-buffers)
  (setq efrit-progress-buffers (make-hash-table :test 'equal)))

;;; Tests

(ert-deftest test-progress-buffer-create ()
  "Test creating a progress buffer."
  (unwind-protect
      (let ((session-id "test-session-1")
            (buffer (efrit-progress-create-buffer "test-session-1")))
        (should (buffer-live-p buffer))
        (should (string-match "efrit-progress" (buffer-name buffer)))
        (with-current-buffer buffer
          (should (> (buffer-size) 0))
          (should (string-match "Efrit Progress Buffer" (buffer-string)))))
    (test-progress-buffer-cleanup)))

(ert-deftest test-progress-buffer-get-existing ()
  "Test retrieving an existing progress buffer."
  (unwind-protect
      (let ((session-id "test-session-2"))
        (let ((buf1 (efrit-progress-create-buffer session-id)))
          (let ((buf2 (efrit-progress-get-buffer session-id)))
            (should (eq buf1 buf2))
            (should (buffer-live-p buf2)))))
    (test-progress-buffer-cleanup)))

(ert-deftest test-progress-buffer-insert-message-event ()
  "Test inserting a message event."
  (unwind-protect
      (let ((session-id "test-session-3"))
        (efrit-progress-create-buffer session-id)
        (let ((chars-inserted (efrit-progress-insert-event session-id 'message
                                '((:text . "Hello, Claude") (:role . "assistant")))))
          (should (> chars-inserted 0))
          (let ((buffer (efrit-progress-get-buffer session-id)))
            (with-current-buffer buffer
              (should (string-match "Hello, Claude" (buffer-string)))))))
    (test-progress-buffer-cleanup)))

(ert-deftest test-progress-buffer-insert-tool-started-event ()
  "Test inserting a tool_started event."
  (unwind-protect
      (let ((session-id "test-session-4"))
        (efrit-progress-create-buffer session-id)
        (let ((chars-inserted (efrit-progress-insert-event session-id 'tool_started
                                '((:tool . "eval_sexp") (:input . "(+ 1 2)")))))
          (should (> chars-inserted 0))
          (let ((buffer (efrit-progress-get-buffer session-id)))
            (with-current-buffer buffer
              (should (string-match "eval_sexp" (buffer-string)))))))
    (test-progress-buffer-cleanup)))

(ert-deftest test-progress-buffer-insert-tool-result-event ()
  "Test inserting a tool_result event."
  (unwind-protect
      (let ((session-id "test-session-5"))
        (efrit-progress-create-buffer session-id)
        (let ((chars-inserted (efrit-progress-insert-event session-id 'tool_result
                                '((:tool . "eval_sexp") (:result . "3")))))
          (should (> chars-inserted 0))
          (let ((buffer (efrit-progress-get-buffer session-id)))
            (with-current-buffer buffer
              (should (string-match "Tool result:" (buffer-string)))))))
    (test-progress-buffer-cleanup)))

(ert-deftest test-progress-buffer-insert-complete-event ()
  "Test inserting a complete event."
  (unwind-protect
      (let ((session-id "test-session-6"))
        (efrit-progress-create-buffer session-id)
        (let ((chars-inserted (efrit-progress-insert-event session-id 'complete
                                '((:result . "end_turn") (:elapsed . 2.5)))))
          (should (> chars-inserted 0))
          (let ((buffer (efrit-progress-get-buffer session-id)))
            (with-current-buffer buffer
              (should (string-match "Execution Complete" (buffer-string)))))))
    (test-progress-buffer-cleanup)))

(ert-deftest test-progress-buffer-insert-error-event ()
  "Test inserting an error event."
  (unwind-protect
      (let ((session-id "test-session-7"))
        (efrit-progress-create-buffer session-id)
        (let ((chars-inserted (efrit-progress-insert-event session-id 'error
                                '((:message . "Something went wrong") (:level . "ERROR")))))
          (should (> chars-inserted 0))
          (let ((buffer (efrit-progress-get-buffer session-id)))
            (with-current-buffer buffer
              (should (string-match "Something went wrong" (buffer-string)))))))
    (test-progress-buffer-cleanup)))

(ert-deftest test-progress-buffer-multiple-events ()
  "Test inserting multiple events in sequence."
  (unwind-protect
      (let ((session-id "test-session-8"))
        (efrit-progress-create-buffer session-id)
        
        ;; Insert multiple events
        (efrit-progress-insert-event session-id 'message
          '((:text . "Starting...") (:role . "assistant")))
        (efrit-progress-insert-event session-id 'tool_started
          '((:tool . "eval_sexp") (:input . "(+ 1 2)")))
        (efrit-progress-insert-event session-id 'tool_result
          '((:tool . "eval_sexp") (:result . "3")))
        (efrit-progress-insert-event session-id 'complete
          '((:result . "end_turn") (:elapsed . 1.0)))
        
        ;; Verify all events are in the buffer
        (let ((buffer (efrit-progress-get-buffer session-id)))
          (with-current-buffer buffer
            (let ((content (buffer-string)))
              (should (string-match "Starting..." content))
              (should (string-match "eval_sexp" content))
              (should (string-match "Execution Complete" content))))))
    (test-progress-buffer-cleanup)))

(ert-deftest test-progress-buffer-archive ()
  "Test archiving a progress buffer."
  (unwind-protect
      (let ((session-id "test-session-9"))
        (efrit-progress-create-buffer session-id)
        
        ;; Verify buffer is in active registry
        (should (efrit-progress-get-buffer session-id))
        
        ;; Archive the buffer
        (efrit-progress-archive-buffer session-id)
        
        ;; Verify buffer is no longer in active registry
        (should-not (efrit-progress-get-buffer session-id))
        
        ;; But the buffer should still exist with archived name
        (let ((buffers (buffer-list)))
          (should (cl-some (lambda (buf)
                            (string-match "efrit-progress-.*" (buffer-name buf)))
                          buffers))))
    (test-progress-buffer-cleanup)))

(ert-deftest test-progress-buffer-clear ()
  "Test clearing a progress buffer."
  (unwind-protect
      (let ((session-id "test-session-10"))
        (let ((buffer (efrit-progress-create-buffer session-id)))
          ;; Insert some events
          (efrit-progress-insert-event session-id 'message
            '((:text . "Test message") (:role . "assistant")))
          
          ;; Verify buffer has content
          (with-current-buffer buffer
            (should (string-match "Test message" (buffer-string))))
          
          ;; Clear the buffer
          (efrit-progress-clear-buffer session-id)
          
          ;; Verify buffer still exists but is cleared
          (let ((buf (efrit-progress-get-buffer session-id)))
            (should (buffer-live-p buf))
            (with-current-buffer buf
              (should (string-match "Cleared" (buffer-string)))
              (should-not (string-match "Test message" (buffer-string)))))))
    (test-progress-buffer-cleanup)))

(ert-deftest test-progress-buffer-list ()
  "Test listing all progress buffers."
  (unwind-protect
      (let ((session-ids '("test-session-11" "test-session-12" "test-session-13")))
        ;; Create multiple buffers
        (mapc #'efrit-progress-create-buffer session-ids)
        
        ;; Verify count
        (should (= (hash-table-count efrit-progress-buffers) 3))
        
        ;; List buffers should succeed
        (should (progn
                  (efrit-progress-list-buffers)
                  t)))
    (test-progress-buffer-cleanup)))

(ert-deftest test-progress-buffer-readonly ()
  "Test that progress buffer is read-only by default."
  (unwind-protect
      (let ((session-id "test-session-14"))
        (let ((buffer (efrit-progress-create-buffer session-id)))
          (with-current-buffer buffer
            ;; Buffer should be read-only
            (should buffer-read-only))))
    (test-progress-buffer-cleanup)))

(ert-deftest test-progress-buffer-timestamp-format ()
  "Test that progress buffer uses correct timestamp format."
  (unwind-protect
      (let ((session-id "test-session-15"))
        (efrit-progress-create-buffer session-id)
        (efrit-progress-insert-event session-id 'message
          '((:text . "Timestamped event") (:role . "assistant")))
        
        (let ((buffer (efrit-progress-get-buffer session-id)))
          (with-current-buffer buffer
            (let ((content (buffer-string)))
              ;; Should contain timestamp in HH:MM:SS format
              (should (string-match "\\[.*:[0-9][0-9]:[0-9][0-9]\\]" content))))))
    (test-progress-buffer-cleanup)))

(ert-deftest test-progress-buffer-non-existent-session ()
  "Test getting progress buffer for non-existent session."
  (let ((buffer (efrit-progress-get-buffer "non-existent-session")))
    (should (null buffer))))

(ert-deftest test-progress-buffer-insert-unknown-event ()
  "Test inserting an unknown event type."
  (unwind-protect
      (let ((session-id "test-session-16"))
        (efrit-progress-create-buffer session-id)
        
        ;; Insert unknown event type
        (let ((chars-inserted (efrit-progress-insert-event session-id 'unknown-type
                                '((:data . "test")))))
          ;; Should still insert something
          (should (> chars-inserted 0))
          
          (let ((buffer (efrit-progress-get-buffer session-id)))
            (with-current-buffer buffer
              (should (string-match "Unknown event type" (buffer-string)))))))
    (test-progress-buffer-cleanup)))

(provide 'test-progress-buffer)

;;; test-progress-buffer.el ends here
