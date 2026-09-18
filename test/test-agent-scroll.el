;;; test-agent-scroll.el --- following-windows scrolling -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'efrit-test-sandbox-helpers)
(require 'efrit-agent)

(ert-deftest test-agent-scroll-only-following-windows-move ()
  "A window scrolled up to read stays put; one at the end follows."
  (skip-unless (not noninteractive))
  (with-temp-buffer
    (efrit-agent-mode)
    (efrit-agent--setup-regions)
    (dotimes (i 200) (efrit-agent--append-to-conversation (format "line %d\n" i) nil))
    (let* ((reader (split-window))
           (follower (selected-window)))
      (set-window-buffer reader (current-buffer))
      (set-window-buffer follower (current-buffer))
      (set-window-point reader 10)
      (set-window-point follower (point-max))
      (unwind-protect
          (progn
            (efrit-agent--append-to-conversation "new tail\n" nil)
            (should (= (window-point reader) 10))
            (should (>= (window-point follower) (- (point-max) 20))))
        (delete-window reader)))))

(ert-deftest test-agent-scroll-following-windows-predicate ()
  "Pure check of the follower predicate in batch mode."
  (with-temp-buffer
    (efrit-agent-mode)
    (efrit-agent--setup-regions)
    (dotimes (i 50) (efrit-agent--append-to-conversation (format "l%d\n" i) nil))
    (let ((w (selected-window)))
      (set-window-buffer w (current-buffer))
      (set-window-point w (point-min))
      (should-not (memq w (efrit-agent--following-windows)))
      (set-window-point w (marker-position efrit-agent--conversation-end))
      (should (memq w (efrit-agent--following-windows))))))

;;; Conversation integrity across a streamed turn

(defconst test-agent-scroll--dir
  (file-name-directory (or load-file-name buffer-file-name
                           (locate-library "test-agent-scroll")))
  "Directory of this test file, resolved at load time.")

(ert-deftest test-agent-user-turn-survives-streamed-answer ()
  "The user's line must still be in the buffer after the model answers."
  (skip-unless (executable-find "python3"))
  (require 'efrit-api-stream)
  (let ((efrit-api-key "sk-test-key-1234567890abcdefghij")
        (efrit-api-auth-scheme 'x-api-key)
        (efrit-api-streaming t) (efrit-permission-policy nil)
        (efrit-api-stream-curl-program
         (expand-file-name "scripts/mock-anthropic-stream.py" test-agent-scroll--dir))
        (efrit-agent-buffer-name "*efrit-agent-test*"))
    (unwind-protect
        (progn
          (efrit-agent-open)
          (with-current-buffer efrit-agent-buffer-name
            (goto-char (point-max)) (insert "please text")
            (efrit-agent-input-send)
            (let ((deadline (+ (float-time) 8)))
              (while (and (< (float-time) deadline) (not (eq efrit-agent--status 'idle)))
                (accept-process-output nil 0.05)))
            (let ((text (buffer-substring-no-properties (point-min) (point-max))))
              (should (string-match-p "❯ please text" text))
              (should (string-match-p "Hello, world\\." text))
              ;; the user turn precedes the answer
              (should (< (string-match "please text" text) (string-match "Hello" text)))
              ;; no raw protocol markers leak into the transcript
              (should-not (string-match-p "SESSION-COMPLETE\\|#s(hash-table" text)))))
      (when (get-buffer efrit-agent-buffer-name) (kill-buffer efrit-agent-buffer-name)))))

(ert-deftest test-agent-session-complete-renders-as-answer ()
  (let ((efrit-agent-buffer-name "*efrit-agent-test2*"))
    (unwind-protect
        (progn
          (efrit-agent-open)
          (with-current-buffer efrit-agent-buffer-name
            (let ((id (efrit-agent-show-tool-start "session_complete" nil)))
              (efrit-agent-show-tool-result id "[SESSION-COMPLETE: The answer is 42.]" t 0.0))
            (let ((text (buffer-substring-no-properties (point-min) (point-max))))
              (should (string-match-p "^The answer is 42\\.$" text))
              (should-not (string-match-p "session_complete\\|SESSION-COMPLETE\\|Result:" text)))))
      (when (get-buffer efrit-agent-buffer-name) (kill-buffer efrit-agent-buffer-name)))))

(ert-deftest test-agent-tool-row-is-one-line-with-readable-input ()
  (let ((efrit-agent-buffer-name "*efrit-agent-test3*") (efrit-agent-display-mode 'verbose))
    (unwind-protect
        (progn
          (efrit-agent-open)
          (with-current-buffer efrit-agent-buffer-name
            (let* ((h (make-hash-table :test 'equal))
                   (_ (puthash "expr" "(+ 1 2)" h))
                   (id (efrit-agent-show-tool-start "eval_sexp" h)))
              (efrit-agent-show-tool-result id "3" t 0.2))
            (let ((text (buffer-substring-no-properties (point-min) (point-max))))
              ;; elapsed sits before the summary so the summary is the
              ;; part chopped at the right edge
              (should (string-match-p "✓ eval_sexp  0\\.2s · 3$" text))
              (should (string-match-p "^       λ  (\\+ 1 2)$" text))   ; bare expr, not #s(hash-table
              (should-not (string-match-p "hash-table" text)))))
      (when (get-buffer efrit-agent-buffer-name) (kill-buffer efrit-agent-buffer-name)))))

(ert-deftest test-agent-long-summary-is-chopped-not-wrapped ()
  "A summary wider than the row is cut at the window width and marked at
the right edge; the row stays one line and the full result lives in
the expansion."
  (let ((efrit-agent-buffer-name "*efrit-agent-test-chop*") (efrit-agent-display-mode 'compact))
    (unwind-protect
        (progn
          (efrit-agent-open)
          (with-current-buffer efrit-agent-buffer-name
            (let* ((long (mapconcat (lambda (i) (format "word%d" i)) (number-sequence 1 80) " "))
                   (id (efrit-agent-show-tool-start "shell_exec" nil)))
              (efrit-agent-show-tool-result id long t 0.0)
              (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
                     (row (seq-find (lambda (l) (string-match-p "shell_exec" l))
                                    (split-string text "\n"))))
                (should row)
                ;; chopped: not the whole thing on the row, marker present
                (should-not (string-match-p "word80" row))
                (should (string-suffix-p efrit-agent--chop-marker row))
                ;; the marker is pinned right via a display space
                (goto-char (point-min))
                (search-forward efrit-agent--chop-marker)
                (should (get-text-property (- (point) 2) 'display))
                ;; row width respects the buffer's row width
                (should (<= (string-width row) (+ 2 (efrit-agent--row-width))))))))
      (when (get-buffer efrit-agent-buffer-name) (kill-buffer efrit-agent-buffer-name)))))

(ert-deftest test-agent-denied-row-is-quiet ()
  "A sandbox denial renders as a dim ⊘ row: no Failed, no Error:, no Retry bar."
  (require 'efrit-sandbox)
  (let ((efrit-agent-buffer-name "*efrit-agent-test4*") (efrit-agent-display-mode 'verbose))
    (unwind-protect
        (progn
          (efrit-agent-open)
          (with-current-buffer efrit-agent-buffer-name
            (let* ((h (make-hash-table :test 'equal))
                   (_ (puthash "command" "cat ~/x" h))
                   (id (efrit-agent-show-tool-start "shell_exec" h)))
              (efrit-agent-show-tool-result
               id (concat efrit-sandbox-denied-prefix "run shell commands. The user declined. Continue without it.")
               nil 1.0))
            (let ((text (buffer-substring-no-properties (point-min) (point-max))))
              (should (string-match-p "⊘ shell_exec: cat ~/x  1\\.0s · denied$" text))
              (should (string-match-p "^       \\$  cat ~/x$" text))
              (should-not (string-match-p "Failed\\|Error:\\|\\[Retry\\]\\|\\[Skip\\]" text)))))
      (when (get-buffer efrit-agent-buffer-name) (kill-buffer efrit-agent-buffer-name)))))

(ert-deftest test-agent-failed-row-shows-gist ()
  "A real failure: first sentence of the error on the row, message in the body, no buttons."
  (let ((efrit-agent-buffer-name "*efrit-agent-test5*") (efrit-agent-display-mode 'verbose))
    (unwind-protect
        (progn
          (efrit-agent-open)
          (with-current-buffer efrit-agent-buffer-name
            (let* ((h (make-hash-table :test 'equal))
                   (_ (puthash "path" "nope.el" h))
                   (id (efrit-agent-show-tool-start "read_file" h)))
              (efrit-agent-show-tool-result id "Error: file not found: nope.el. Check the path." nil 0.1))
            (let ((text (buffer-substring-no-properties (point-min) (point-max))))
              (should (string-match-p "✗ read_file: nope.el  0\\.1s · file not found: nope.el$" text))
              (should (string-match-p "file not found: nope.el. Check the path." text))
              (should-not (string-match-p "Failed\\|Error: $\\|\\[Retry\\]" text)))))
      (when (get-buffer efrit-agent-buffer-name) (kill-buffer efrit-agent-buffer-name)))))

(ert-deftest test-agent-completion-message-shown-only-without-streamed-answer ()
  "session_complete's message is the answer only when nothing was streamed this turn.
After streamed text it is the end-of-turn signal: only the rule is drawn."
  (let ((efrit-agent-buffer-name "*efrit-agent-test-dedupe*"))
    (unwind-protect
        (progn
          (efrit-agent-open)
          (with-current-buffer efrit-agent-buffer-name
            ;; turn 1: text streamed, then a completion that says something
            ;; different -- still not shown; the text is the answer
            (efrit-agent--add-user-message "run git status in /")
            (efrit-agent-stream-content "`/` is not a git repository.")
            (efrit-agent-stream-end)
            (let ((id (efrit-agent-show-tool-start "session_complete" nil)))
              (efrit-agent-show-tool-result id "[SESSION-COMPLETE: Some other wording entirely.]" t 0.0))
            (let ((text (buffer-substring-no-properties (point-min) (point-max))))
              (should (string-match-p "is not a git repository" text))
              (should-not (string-match-p "Some other wording" text))
              (should (string-match-p "^···$" text)))
            ;; turn 2: tool rows but no text, then completion -- it IS the answer
            (efrit-agent--add-user-message "what is 6 times 7?")
            (let ((id (efrit-agent-show-tool-start "eval_sexp" nil)))
              (efrit-agent-show-tool-result id "42" t 0.0))
            (let ((id (efrit-agent-show-tool-start "session_complete" nil)))
              (efrit-agent-show-tool-result id "[SESSION-COMPLETE: 6 times 7 is 42.]" t 0.0))
            (should (string-match-p "6 times 7 is 42" (buffer-string)))
            ;; turn 3: text from a PREVIOUS turn must not suppress this turn's message
            (efrit-agent--add-user-message "and 7 times 8?")
            (let ((id (efrit-agent-show-tool-start "session_complete" nil)))
              (efrit-agent-show-tool-result id "[SESSION-COMPLETE: 56.]" t 0.0))
            (should (string-match-p "^56\\.$" (buffer-string)))))
      (when (get-buffer efrit-agent-buffer-name) (kill-buffer efrit-agent-buffer-name)))))

(provide 'test-agent-scroll)
;;; test-agent-scroll.el ends here
