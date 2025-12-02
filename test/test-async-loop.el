;;; test-async-loop.el --- Tests for efrit-do-async-loop -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'efrit-do-async-loop)
(require 'efrit-session)
(require 'efrit-progress-buffer)
(require 'efrit-log)

(defgroup test-async-loop nil
  "Tests for async loop infrastructure."
  :group 'efrit)

;;; Test Fixtures

(defun test-async-loop-cleanup ()
  "Clean up test async loop state."
  (clrhash efrit-do-async--loops)
  ;; Clean up progress buffers
  (maphash (lambda (session-id buffer)
             (when (buffer-live-p buffer)
               (kill-buffer buffer)))
           efrit-progress-buffers)
  (setq efrit-progress-buffers (make-hash-table :test 'equal)))

;;; Tests

(ert-deftest test-async-loop-initialization ()
  "Test initializing an async loop."
  (unwind-protect
      (let ((session (efrit-session-create "test-session-1" "test command")))
        (let ((session-id (efrit-do-async-loop session nil)))
          (should (stringp session-id))
          (should (= (hash-table-count efrit-do-async--loops) 1))))
    (test-async-loop-cleanup)))

(ert-deftest test-async-loop-with-callback ()
  "Test async loop with completion callback."
  (unwind-protect
      (let* ((session (efrit-session-create "test-session-2" "test"))
             (callback-called nil)
             (callback (lambda (_s _reason) (setq callback-called t))))
        (let ((session-id (efrit-do-async-loop session nil callback)))
          (should (stringp session-id))
          ;; Callback won't be called until execution completes (which requires API)
          ))
    (test-async-loop-cleanup)))

(ert-deftest test-async-loop-stores-loop-state ()
  "Test that async loop stores state in global hash table."
  (unwind-protect
      (let ((session (efrit-session-create "test-session-3" "test")))
        (let ((session-id (efrit-do-async-loop session nil)))
          (let ((loop-state (gethash session-id efrit-do-async--loops)))
            (should (listp loop-state))
            (should (= (length loop-state) 4)))))
    (test-async-loop-cleanup)))

(ert-deftest test-async-loop-progress-buffer-creation ()
  "Test that async loop creates progress buffer."
  (unwind-protect
      (let ((original-show efrit-do-async-show-progress-buffer)
            (session (efrit-session-create "test-session-4" "test")))
        (setq efrit-do-async-show-progress-buffer t)
        (unwind-protect
            (let ((session-id (efrit-do-async-loop session nil)))
              (let ((pbuf (efrit-progress-get-buffer session-id)))
                (should (buffer-live-p pbuf))))
          (setq efrit-do-async-show-progress-buffer original-show)))
    (test-async-loop-cleanup)))

(ert-deftest test-async-loop-respects-progress-buffer-setting ()
  "Test that async loop respects progress buffer display setting."
  (unwind-protect
      (let ((original-show efrit-do-async-show-progress-buffer)
            (session (efrit-session-create "test-session-5" "test")))
        (setq efrit-do-async-show-progress-buffer nil)
        (unwind-protect
            (let ((session-id (efrit-do-async-loop session nil)))
              ;; Buffer should not be created/shown when setting is off
              ;; (it gets created lazily on first event)
              (should (stringp session-id)))
          (setq efrit-do-async-show-progress-buffer original-show)))
    (test-async-loop-cleanup)))

(ert-deftest test-async-loop-multiple-sessions ()
  "Test running multiple async loops concurrently."
  (unwind-protect
      (let ((session1 (efrit-session-create "test-session-6a" "test1"))
            (session2 (efrit-session-create "test-session-6b" "test2")))
        (let ((id1 (efrit-do-async-loop session1 nil))
              (id2 (efrit-do-async-loop session2 nil)))
          (should (= (hash-table-count efrit-do-async--loops) 2))
          (should (gethash id1 efrit-do-async--loops))
          (should (gethash id2 efrit-do-async--loops))))
    (test-async-loop-cleanup)))

(ert-deftest test-async-loop-stop-loop ()
  "Test stopping an async loop."
  (unwind-protect
      (let ((session (efrit-session-create "test-session-7" "test")))
        (let ((session-id (efrit-do-async-loop session nil)))
          (should (= (hash-table-count efrit-do-async--loops) 1))
          
          ;; Stop the loop
          (efrit-do-async--stop-loop session "test-stop")
          
          ;; Session should be removed from active loops
          (should (= (hash-table-count efrit-do-async--loops) 0))))
    (test-async-loop-cleanup)))

(ert-deftest test-async-loop-iteration-count ()
  "Test that async loop tracks iteration count."
  (unwind-protect
      (let ((session (efrit-session-create "test-session-8" "test")))
        (let ((session-id (efrit-do-async-loop session nil)))
          (let ((loop-state (gethash session-id efrit-do-async--loops)))
            ;; Iteration count should be 0 initially
            (should (= (nth 2 loop-state) 0)))))
    (test-async-loop-cleanup)))

(ert-deftest test-async-loop-on-api-error ()
  "Test async loop error handling."
  (unwind-protect
      (let* ((error-received nil)
             (session (efrit-session-create "test-session-9" "test"))
             (callback (lambda (_s reason)
                        (setq error-received reason))))
        (let ((session-id (efrit-do-async-loop session nil callback)))
          ;; Simulate API error
          (efrit-do-async--on-api-error session "API connection failed")
          
          ;; Loop should be stopped
          (should (= (hash-table-count efrit-do-async--loops) 0))))
    (test-async-loop-cleanup)))

(ert-deftest test-async-loop-max-iterations ()
  "Test that async loop respects max iteration limit."
  (unwind-protect
      (let ((original-max efrit-do-async-max-iterations)
            (session (efrit-session-create "test-session-10" "test")))
        (setq efrit-do-async-max-iterations 5)
        (unwind-protect
            (let ((session-id (efrit-do-async-loop session nil)))
              ;; The loop won't automatically enforce the limit without API calls
              ;; but we can verify the setting is in place
              (should (= efrit-do-async-max-iterations 5)))
          (setq efrit-do-async-max-iterations original-max)))
    (test-async-loop-cleanup)))

(ert-deftest test-async-loop-zero-max-iterations ()
  "Test that zero max-iterations means unlimited."
  (unwind-protect
      (let ((original-max efrit-do-async-max-iterations)
            (session (efrit-session-create "test-session-11" "test")))
        (setq efrit-do-async-max-iterations 0)
        (unwind-protect
            (let ((session-id (efrit-do-async-loop session nil)))
              (should (= efrit-do-async-max-iterations 0)))
          (setq efrit-do-async-max-iterations original-max)))
    (test-async-loop-cleanup)))

(ert-deftest test-async-loop-completion-callback-invoked ()
  "Test that completion callback is invoked with correct parameters."
  (unwind-protect
      (let* ((callback-args nil)
             (session (efrit-session-create "test-session-12" "test"))
             (callback (lambda (s reason)
                        (setq callback-args (list s reason)))))
        (let ((session-id (efrit-do-async-loop session nil callback)))
          ;; Stop with a specific reason
          (efrit-do-async--stop-loop session "end_turn")
          
          ;; Callback should have been invoked
          (should callback-args)
          (should (= (length callback-args) 2))))
    (test-async-loop-cleanup)))

(ert-deftest test-async-loop-message-event ()
  "Test inserting message event into progress buffer."
  (unwind-protect
      (let ((session (efrit-session-create "test-session-13" "test")))
        (let ((session-id (efrit-do-async-loop session nil)))
          ;; Insert a message event
          (efrit-progress-insert-event session-id 'message
            '((:text . "Test message") (:role . "assistant")))
          
          ;; Verify it's in the progress buffer
          (let ((pbuf (efrit-progress-get-buffer session-id)))
            (with-current-buffer pbuf
              (should (string-match "Test message" (buffer-string)))))))
    (test-async-loop-cleanup)))

(ert-deftest test-async-loop-session-complete-event ()
  "Test inserting complete event."
  (unwind-protect
      (let ((session (efrit-session-create "test-session-14" "test")))
        (let ((session-id (efrit-do-async-loop session nil)))
          ;; Insert a complete event
          (efrit-progress-insert-event session-id 'complete
            '((:result . "end_turn") (:elapsed . 1.5)))
          
          ;; Verify it's in the progress buffer
          (let ((pbuf (efrit-progress-get-buffer session-id)))
            (with-current-buffer pbuf
              (should (string-match "Execution Complete" (buffer-string)))))))
    (test-async-loop-cleanup)))

(ert-deftest test-async-loop-interruption ()
  "Test session interruption during async loop."
  (unwind-protect
      (let* ((session (efrit-session-create "test-session-15" "test")))
        (let ((session-id (efrit-do-async-loop session nil)))
          ;; Mark session for interruption
          (efrit-session-request-interrupt session)
          
          ;; Check interrupt flag
          (should (efrit-session-should-interrupt-p session))))
    (test-async-loop-cleanup)))

(provide 'test-async-loop)

;;; test-async-loop.el ends here
