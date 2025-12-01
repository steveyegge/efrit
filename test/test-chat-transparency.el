;;; test-chat-transparency.el --- Tests for chat transparency features -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the new transparency features in efrit-chat:
;; - Tool call visibility
;; - Incremental response display
;; - Tool result display
;; - Visual hierarchy with different faces

;;; Code:

(require 'ert)
(require 'efrit-chat-transparency)
(require 'efrit-chat-buffer)

;;; Test Fixtures

(defun test-chat-transparency--create-test-buffer ()
  "Create a clean test buffer for chat testing."
  (let ((buffer (get-buffer-create "*test-chat*")))
    (with-current-buffer buffer
      (erase-buffer)
      (setq-local efrit--conversation-marker (make-marker))
      (set-marker efrit--conversation-marker (point-min))
      (setq-local efrit--input-marker (make-marker))
      (set-marker efrit--input-marker (point-min)))
    buffer))

(defun test-chat-transparency--mock-setup-buffer (test-buffer)
  "Mock efrit--setup-buffer to return TEST-BUFFER."
  (let ((original-func (symbol-function 'efrit--setup-buffer)))
    (setf (symbol-function 'efrit--setup-buffer)
          (lambda () test-buffer))
    original-func))

(defun test-chat-transparency--restore-setup-buffer (original-func)
  "Restore original efrit--setup-buffer function."
  (setf (symbol-function 'efrit--setup-buffer) original-func))

;;; Tests for Tool Call Display

(ert-deftest test-chat-transparency-tool-call-display ()
  "Test that tool calls are displayed correctly."
  (let ((test-buffer (test-chat-transparency--create-test-buffer))
        (original-func (test-chat-transparency--mock-setup-buffer
                       (test-chat-transparency--create-test-buffer))))
    (unwind-protect
        (progn
          (setq efrit-show-tool-calls t)
          (efrit-transparency--display-tool-call "eval_sexp" '((expr . "(+ 1 2)")))
          (with-current-buffer test-buffer
            (let ((content (buffer-substring-no-properties (point-min) (point-max))))
              (should (string-match-p "Calling tool" content))
              (should (string-match-p "eval_sexp" content))
              (should (string-match-p "Input" content)))))
      (test-chat-transparency--restore-setup-buffer original-func))))

(ert-deftest test-chat-transparency-tool-call-hidden-when-disabled ()
  "Test that tool calls are not displayed when disabled."
  (let ((test-buffer (test-chat-transparency--create-test-buffer))
        (original-func (test-chat-transparency--mock-setup-buffer
                       (test-chat-transparency--create-test-buffer))))
    (unwind-protect
        (progn
          (setq efrit-show-tool-calls nil)
          (with-current-buffer test-buffer
            (let ((initial-size (buffer-size)))
              (efrit-transparency--display-tool-call "eval_sexp" '((expr . "(+ 1 2)")))
              ;; Buffer should not have changed
              (should (= initial-size (buffer-size))))))
      (test-chat-transparency--restore-setup-buffer original-func))))

;;; Tests for Tool Result Display

(ert-deftest test-chat-transparency-tool-result-display ()
  "Test that tool results are displayed correctly."
  (let ((test-buffer (test-chat-transparency--create-test-buffer))
        (original-func (test-chat-transparency--mock-setup-buffer
                       (test-chat-transparency--create-test-buffer))))
    (unwind-protect
        (progn
          (setq efrit-show-tool-results t)
          (efrit-transparency--display-tool-result "eval_sexp" "3")
          (with-current-buffer test-buffer
            (let ((content (buffer-substring-no-properties (point-min) (point-max))))
              (should (string-match-p "Result" content))
              (should (string-match-p "3" content)))))
      (test-chat-transparency--restore-setup-buffer original-func))))

(ert-deftest test-chat-transparency-tool-result-hidden-when-disabled ()
  "Test that tool results are not displayed when disabled."
  (let ((test-buffer (test-chat-transparency--create-test-buffer))
        (original-func (test-chat-transparency--mock-setup-buffer
                       (test-chat-transparency--create-test-buffer))))
    (unwind-protect
        (progn
          (setq efrit-show-tool-results nil)
          (with-current-buffer test-buffer
            (let ((initial-size (buffer-size)))
              (efrit-transparency--display-tool-result "eval_sexp" "3")
              ;; Buffer should not have changed
              (should (= initial-size (buffer-size))))))
      (test-chat-transparency--restore-setup-buffer original-func))))

;;; Tests for Incremental Response Display

(ert-deftest test-chat-transparency-incremental-display-enabled ()
  "Test that responses are displayed incrementally when enabled."
  (let ((test-buffer (test-chat-transparency--create-test-buffer))
        (original-func (test-chat-transparency--mock-setup-buffer
                       (test-chat-transparency--create-test-buffer))))
    (unwind-protect
        (progn
          (setq efrit-incremental-responses t)
          (setq efrit-incremental-delay 0)  ;; No delay in tests
          (with-current-buffer test-buffer
            ;; Mock redisplay for batch mode
            (cl-letf (((symbol-function 'redisplay) (lambda (&rest _) nil)))
              (efrit-transparency--display-incremental "Hello world")
              (let ((content (buffer-substring-no-properties (point-min) (point-max))))
                (should (string-match-p "Hello world" content))
                (should (string-match-p "Assistant" content))))))
      (test-chat-transparency--restore-setup-buffer original-func))))

(ert-deftest test-chat-transparency-incremental-display-disabled ()
  "Test that responses are displayed all at once when incremental disabled."
  (let ((test-buffer (test-chat-transparency--create-test-buffer))
        (original-func (test-chat-transparency--mock-setup-buffer
                       (test-chat-transparency--create-test-buffer))))
    (unwind-protect
        (progn
          (setq efrit-incremental-responses nil)
          (with-current-buffer test-buffer
            (efrit-transparency--display-incremental "Hello world")
            (let ((content (buffer-substring-no-properties (point-min) (point-max))))
              (should (string-match-p "Hello world" content))
              (should (string-match-p "Assistant" content)))))
      (test-chat-transparency--restore-setup-buffer original-func))))

;;; Tests for Customization

(ert-deftest test-chat-transparency-customization-defaults ()
  "Test that customization variables have correct defaults."
  (should (eq efrit-show-tool-calls t))
  (should (eq efrit-show-tool-results t))
  (should (eq efrit-incremental-responses t))
  (should (= efrit-response-chunk-size 50))
  (should (= efrit-incremental-delay 0.01)))

;;; Tests for Face Definitions

(ert-deftest test-chat-transparency-face-definitions ()
  "Test that faces are properly defined."
  (should (facep 'efrit-tool-call-face))
  (should (facep 'efrit-tool-result-face))
  (should (facep 'efrit-thinking-face)))

;;; Tests for Thinking Display

(ert-deftest test-chat-transparency-thinking-display ()
  "Test that thinking is displayed when enabled."
  (let ((test-buffer (test-chat-transparency--create-test-buffer))
        (original-func (test-chat-transparency--mock-setup-buffer
                       (test-chat-transparency--create-test-buffer))))
    (unwind-protect
        (progn
          (setq efrit-show-thinking t)
          (efrit-transparency--display-thinking "This is my reasoning")
          (with-current-buffer test-buffer
            (let ((content (buffer-substring-no-properties (point-min) (point-max))))
              (should (string-match-p "Thinking" content))
              (should (string-match-p "reasoning" content)))))
      (test-chat-transparency--restore-setup-buffer original-func))))

(ert-deftest test-chat-transparency-thinking-hidden-when-disabled ()
  "Test that thinking is not displayed when disabled."
  (let ((test-buffer (test-chat-transparency--create-test-buffer))
        (original-func (test-chat-transparency--mock-setup-buffer
                       (test-chat-transparency--create-test-buffer))))
    (unwind-protect
        (progn
          (setq efrit-show-thinking nil)
          (with-current-buffer test-buffer
            (let ((initial-size (buffer-size)))
              (efrit-transparency--display-thinking "This is my reasoning")
              ;; Buffer should not have changed
              (should (= initial-size (buffer-size))))))
      (test-chat-transparency--restore-setup-buffer original-func))))

(provide 'test-chat-transparency)

;;; test-chat-transparency.el ends here
