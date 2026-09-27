;;; test-transcript.el --- The per-session Markdown transcript -*- lexical-binding: t; -*-

;;; Commentary:
;; The transcript writer listens to events and appends to one file per
;; session.  These tests publish the events themselves.

;;; Code:

(require 'ert)
(require 'efrit-transcript)

(defmacro test-transcript--with (&rest body)
  "Run BODY with a temp transcript directory and a fresh session bound to SESSION and ID."
  (declare (indent 0))
  `(let* ((dir (make-temp-file "efrit-transcript-" t))
          (efrit-transcript-directory dir)
          (efrit-transcript-enabled t)
          (efrit-transcript-result-max-chars 40)
          (session (efrit-repl-session-create dir))
          (id (efrit-repl-session-id session)))
     (ignore id)
     (unwind-protect
         (progn ,@body)
       (remhash id efrit-transcript--started)
       (remhash id efrit-transcript--answers)
       (delete-directory dir t))))

(defun test-transcript--text (session)
  "The transcript file's content."
  (with-temp-buffer
    (insert-file-contents (efrit-transcript-file session))
    (buffer-string)))

(ert-deftest test-transcript-writes-a-turn-as-it-goes ()
  "Input, tool call with clipped result, streamed answer, and an abnormal end."
  (test-transcript--with
    (efrit-publish 'turn-start `((:session-id . ,id) (:input . "count the files")))
    (should (file-exists-p (efrit-transcript-file session)))
    (efrit-publish 'text-delta `((:session-id . ,id) (:text . "I will ")))
    (efrit-publish 'text-delta `((:session-id . ,id) (:text . "look.")))
    (efrit-publish 'tool-start `((:session-id . ,id) (:tool-id . "t1") (:tool . "shell_exec")
                                 (:input . (("command" . "ls")))))
    (efrit-publish 'tool-result `((:session-id . ,id) (:tool-id . "t1") (:tool . "shell_exec")
                                  (:result . ,(make-string 100 ?x)) (:success . t) (:elapsed . 0.5)))
    (efrit-publish 'text-delta `((:session-id . ,id) (:text . "Three files.")))
    (efrit-publish 'text-end `((:session-id . ,id)))
    (efrit-publish 'turn-complete `((:session-id . ,id) (:stop-reason . "max_tokens")))
    (let ((text (test-transcript--text session)))
      (should (string-match-p (concat "^# efrit session " (regexp-quote id)) text))
      (should (string-match-p "^## [0-9:]+ You\n\ncount the files$" text))
      ;; the text before the tool call was flushed before it
      (should (< (string-search "I will look." text) (string-search "### tool `shell_exec`" text)))
      (should (string-match-p "\"command\":\"ls\"" text))
      (should (string-match-p "ok in 0.5s" text))
      (should (string-match-p "… 60 more characters" text))
      (should (string-match-p "### efrit\n\nThree files\\." text))
      (should (string-match-p "_turn ended: max_tokens_" text)))))

(ert-deftest test-transcript-steer-error-and-off-switch ()
  "Steering text and errors are recorded; nothing is written when disabled."
  (test-transcript--with
    (efrit-publish 'turn-start `((:session-id . ,id) (:input . "go")))
    (efrit-publish 'steer `((:session-id . ,id) (:text . "faster")))
    (efrit-publish 'error `((:session-id . ,id) (:message . "boom")))
    (let ((text (test-transcript--text session)))
      (should (string-match-p "^> \\*\\*steer\\*\\* faster$" text))
      (should (string-match-p "\\*\\*error:\\*\\* boom" text)))
    (let ((efrit-transcript-enabled nil)
          (other (efrit-repl-session-create dir)))
      (efrit-publish 'turn-start `((:session-id . ,(efrit-repl-session-id other)) (:input . "x")))
      (should-not (efrit-transcript-file other))
      (should (= 1 (length (directory-files dir nil "\\.md\\'")))))))

(ert-deftest test-transcript-fence-outgrows-backticks ()
  (should (string-prefix-p "````\n" (efrit-transcript--fence "a ``` b")))
  (should (string-prefix-p "```\n" (efrit-transcript--fence "plain"))))

(provide 'test-transcript)
;;; test-transcript.el ends here
