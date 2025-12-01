;;; test-efrit-result-struct.el --- Tests for efrit-result-struct module -*- lexical-binding: t -*-

(require 'ert)

;; Add load paths
(add-to-list 'load-path (expand-file-name "../lisp" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../lisp/core" (file-name-directory load-file-name)))

(require 'efrit-result-struct)

;;; Tool Result Tests

(ert-deftest efrit-tool-result-make-basic ()
  "Test creating a basic tool result."
  (let ((result (efrit-tool-result-make "output")))
    (should (efrit-tool-result-p result))
    (should (equal (efrit-tool-result-get-result result) "output"))
    (should (not (efrit-tool-result-is-error result)))
    (should (equal (efrit-tool-result-get-tool-id result) ""))))

(ert-deftest efrit-tool-result-make-full ()
  "Test creating a tool result with all fields."
  (let* ((work-log-entry '("output" "input json"))
         (result (efrit-tool-result-make "output"
                                         t
                                         "read-file"
                                         t
                                         "All done!"
                                         work-log-entry)))
    (should (equal (efrit-tool-result-get-result result) "output"))
    (should (efrit-tool-result-is-error result))
    (should (equal (efrit-tool-result-get-tool-id result) "read-file"))
    (should (efrit-tool-result-session-complete-p result))
    (should (equal (efrit-tool-result-get-completion-message result) "All done!"))
    (should (equal (efrit-tool-result-get-work-log-entry result) work-log-entry))))

;;; Tool Result Accessor Tests

(ert-deftest efrit-tool-result-get-result ()
  "Test accessing result field."
  (let ((result (efrit-tool-result-make "test output")))
    (should (equal (efrit-tool-result-get-result result) "test output"))))

(ert-deftest efrit-tool-result-is-error ()
  "Test accessing error status."
  (let ((success (efrit-tool-result-make "output" nil))
        (error (efrit-tool-result-make "error" t)))
    (should (not (efrit-tool-result-is-error success)))
    (should (efrit-tool-result-is-error error))))

(ert-deftest efrit-tool-result-get-tool-id ()
  "Test accessing tool ID."
  (let ((result (efrit-tool-result-make "output" nil "write-file")))
    (should (equal (efrit-tool-result-get-tool-id result) "write-file"))))

(ert-deftest efrit-tool-result-session-complete ()
  "Test session completion status."
  (let ((incomplete (efrit-tool-result-make "output" nil "tool" nil))
        (complete (efrit-tool-result-make "output" nil "tool" t)))
    (should (not (efrit-tool-result-session-complete-p incomplete)))
    (should (efrit-tool-result-session-complete-p complete))))

(ert-deftest efrit-tool-result-completion-message ()
  "Test accessing completion message."
  (let ((result (efrit-tool-result-make "output" nil "tool" t "Done!")))
    (should (equal (efrit-tool-result-get-completion-message result) "Done!"))))

(ert-deftest efrit-tool-result-work-log-entry ()
  "Test accessing work log entry."
  (let* ((entry '("result" "input"))
         (result (efrit-tool-result-make "output" nil "tool" nil nil entry)))
    (should (equal (efrit-tool-result-get-work-log-entry result) entry))))

;;; Tool Result Conversion Tests

(ert-deftest efrit-tool-result-to-plist ()
  "Test converting tool result to plist."
  (let* ((result (efrit-tool-result-make "output" t "tool-id" t "Done!"))
         (plist (efrit-tool-result-to-plist result)))
    (should (equal (plist-get plist :result) "output"))
    (should (plist-get plist :is-error))
    (should (equal (plist-get plist :tool-id) "tool-id"))
    (should (plist-get plist :session-complete))
    (should (equal (plist-get plist :completion-message) "Done!"))))

(ert-deftest efrit-tool-result-from-plist ()
  "Test creating tool result from plist."
  (let* ((plist '(:result "output" :is-error t :tool-id "read-file" :session-complete nil))
         (result (efrit-tool-result-from-plist plist)))
    (should (equal (efrit-tool-result-get-result result) "output"))
    (should (efrit-tool-result-is-error result))
    (should (equal (efrit-tool-result-get-tool-id result) "read-file"))
    (should (not (efrit-tool-result-session-complete-p result)))))

(ert-deftest efrit-tool-result-round-trip ()
  "Test round-trip conversion: object -> plist -> object."
  (let* ((original (efrit-tool-result-make "output" t "tool-id" t "Done!" '("r" "i")))
         (plist (efrit-tool-result-to-plist original))
         (reconstructed (efrit-tool-result-from-plist plist)))
    (should (equal (efrit-tool-result-get-result original)
                   (efrit-tool-result-get-result reconstructed)))
    (should (equal (efrit-tool-result-is-error original)
                   (efrit-tool-result-is-error reconstructed)))
    (should (equal (efrit-tool-result-get-tool-id original)
                   (efrit-tool-result-get-tool-id reconstructed)))
    (should (equal (efrit-tool-result-session-complete-p original)
                   (efrit-tool-result-session-complete-p reconstructed)))
    (should (equal (efrit-tool-result-get-completion-message original)
                   (efrit-tool-result-get-completion-message reconstructed)))
    (should (equal (efrit-tool-result-get-work-log-entry original)
                   (efrit-tool-result-get-work-log-entry reconstructed)))))

;;; Content Result Tests

(ert-deftest efrit-content-result-make-basic ()
  "Test creating a basic content result."
  (let ((result (efrit-content-result-make "text result")))
    (should (efrit-content-result-p result))
    (should (equal (efrit-content-result-get-result-text result) "text result"))
    (should (not (efrit-content-result-session-complete-p result)))))

(ert-deftest efrit-content-result-make-full ()
  "Test creating a content result with all fields."
  (let* ((tool-blocks '(("tool_result" . "id-1")))
         (work-log '(("result" "input")))
         (result (efrit-content-result-make "text"
                                            tool-blocks
                                            work-log
                                            "Complete"
                                            t)))
    (should (equal (efrit-content-result-get-result-text result) "text"))
    (should (equal (efrit-content-result-get-tool-result-blocks result) tool-blocks))
    (should (equal (efrit-content-result-get-work-log-results result) work-log))
    (should (equal (efrit-content-result-get-completion-message result) "Complete"))
    (should (efrit-content-result-session-complete-p result))))

;;; Content Result Accessor Tests

(ert-deftest efrit-content-result-get-result-text ()
  "Test accessing result text."
  (let ((result (efrit-content-result-make "accumulated text")))
    (should (equal (efrit-content-result-get-result-text result) "accumulated text"))))

(ert-deftest efrit-content-result-get-tool-result-blocks ()
  "Test accessing tool result blocks."
  (let* ((blocks '(("tool_result")))
         (result (efrit-content-result-make "text" blocks)))
    (should (equal (efrit-content-result-get-tool-result-blocks result) blocks))))

(ert-deftest efrit-content-result-get-work-log-results ()
  "Test accessing work log results."
  (let* ((log '(("r" "i")))
         (result (efrit-content-result-make "text" nil log)))
    (should (equal (efrit-content-result-get-work-log-results result) log))))

(ert-deftest efrit-content-result-session-complete ()
  "Test session completion status."
  (let ((incomplete (efrit-content-result-make "text" nil nil nil nil))
        (complete (efrit-content-result-make "text" nil nil nil t)))
    (should (not (efrit-content-result-session-complete-p incomplete)))
    (should (efrit-content-result-session-complete-p complete))))

(ert-deftest efrit-content-result-completion-message ()
  "Test accessing completion message."
  (let ((result (efrit-content-result-make "text" nil nil "Session complete!" t)))
    (should (equal (efrit-content-result-get-completion-message result) "Session complete!"))))

;;; Content Result Conversion Tests

(ert-deftest efrit-content-result-to-plist ()
  "Test converting content result to plist."
  (let* ((blocks '(("tool_result")))
         (log '(("r" "i")))
         (result (efrit-content-result-make "text" blocks log "Done" t))
         (plist (efrit-content-result-to-plist result)))
    (should (equal (plist-get plist :result-text) "text"))
    (should (equal (plist-get plist :tool-result-blocks) blocks))
    (should (equal (plist-get plist :tool-results-for-work-log) log))
    (should (equal (plist-get plist :completion-message) "Done"))
    (should (plist-get plist :session-complete))))

(ert-deftest efrit-content-result-from-plist ()
  "Test creating content result from plist."
  (let* ((plist '(:result-text "text" :tool-result-blocks (("tool")) :session-complete t))
         (result (efrit-content-result-from-plist plist)))
    (should (equal (efrit-content-result-get-result-text result) "text"))
    (should (equal (efrit-content-result-get-tool-result-blocks result) '(("tool"))))
    (should (efrit-content-result-session-complete-p result))))

(ert-deftest efrit-content-result-round-trip ()
  "Test round-trip conversion: object -> plist -> object."
  (let* ((original (efrit-content-result-make "text" '(("block")) '(("r" "i")) "Done" t))
         (plist (efrit-content-result-to-plist original))
         (reconstructed (efrit-content-result-from-plist plist)))
    (should (equal (efrit-content-result-get-result-text original)
                   (efrit-content-result-get-result-text reconstructed)))
    (should (equal (efrit-content-result-get-tool-result-blocks original)
                   (efrit-content-result-get-tool-result-blocks reconstructed)))
    (should (equal (efrit-content-result-get-work-log-results original)
                   (efrit-content-result-get-work-log-results reconstructed)))
    (should (equal (efrit-content-result-get-completion-message original)
                   (efrit-content-result-get-completion-message reconstructed)))
    (should (equal (efrit-content-result-session-complete-p original)
                   (efrit-content-result-session-complete-p reconstructed)))))

;;; Edge Cases

(ert-deftest efrit-tool-result-empty-strings ()
  "Test tool result with empty strings."
  (let ((result (efrit-tool-result-make "")))
    (should (equal (efrit-tool-result-get-result result) ""))))

(ert-deftest efrit-content-result-empty-text ()
  "Test content result with empty text."
  (let ((result (efrit-content-result-make "")))
    (should (equal (efrit-content-result-get-result-text result) ""))))

(ert-deftest efrit-tool-result-nil-completion-message ()
  "Test tool result with nil completion message."
  (let ((result (efrit-tool-result-make "output" nil "tool" nil nil)))
    (should (equal (efrit-tool-result-get-completion-message result) nil))))

(ert-deftest efrit-content-result-nil-completion-message ()
  "Test content result with nil completion message."
  (let ((result (efrit-content-result-make "text" nil nil nil nil)))
    (should (equal (efrit-content-result-get-completion-message result) nil))))

(provide 'test-efrit-result-struct)

;;; test-efrit-result-struct.el ends here
