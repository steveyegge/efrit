;;; efrit-test-runner.el --- Automated test runner for Efrit -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, testing

;;; Commentary:

;; This module provides an automated test runner for Efrit that:
;; - Defines tests as declarative specs (prompt, validators, timeout)
;; - Executes tests with real API calls (burns tokens!)
;; - Validates results using configurable validators
;; - Outputs JSONL for machine-readable results
;;
;; Test specs follow this structure:
;;   (efrit-test-spec
;;     :id "test-id"
;;     :name "Human-readable name"
;;     :tier 1  ; 1-6 for manual test tiers, 7+ for automated
;;     :prompt "The prompt to send to Claude"
;;     :validators ((buffer-contains "*scratch*" "expected text")
;;                  (elisp-returns "(+ 1 2)" 3)
;;                  (function-defined 'my-func))
;;     :timeout 60)
;;
;; Following Zero Client-Side Intelligence: this runner only EXECUTES
;; tests and REPORTS results. It does not interpret or act on them.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'efrit-config)
(require 'efrit-chat)

;;; Customization

(defgroup efrit-test-runner nil
  "Automated test runner for Efrit."
  :group 'efrit
  :prefix "efrit-test-")

(defcustom efrit-test-default-timeout 120
  "Default timeout in seconds for test execution."
  :type 'integer
  :group 'efrit-test-runner)

(defcustom efrit-test-poll-interval 0.5
  "Seconds between polling for response completion."
  :type 'number
  :group 'efrit-test-runner)

(defcustom efrit-test-results-dir nil
  "Directory for test results. If nil, uses ~/.emacs.d/.efrit/test-results/."
  :type '(choice (const nil) directory)
  :group 'efrit-test-runner)

(defcustom efrit-test-verbose t
  "Whether to show verbose output during test execution."
  :type 'boolean
  :group 'efrit-test-runner)

;;; Test Spec Structure

(cl-defstruct (efrit-test-spec (:constructor efrit-test-spec--create))
  "Structure for a test specification."
  id          ; Unique identifier
  name        ; Human-readable name
  tier        ; Test tier (1-6 manual, 7+ automated)
  category    ; Category (smoke, error-recovery, workflow, etc.)
  prompt      ; The prompt to send to Claude
  setup       ; Optional setup function to run before test
  teardown    ; Optional teardown function to run after test
  validators  ; List of validator specs
  timeout     ; Timeout in seconds
  tags)       ; Optional tags for filtering

(cl-defun efrit-test-spec (&key id name tier category prompt
                                setup teardown validators timeout tags)
  "Create a test spec with the given parameters."
  (efrit-test-spec--create
   :id (or id (format "test-%s" (random 10000)))
   :name (or name "Unnamed test")
   :tier (or tier 1)
   :category (or category 'general)
   :prompt prompt
   :setup setup
   :teardown teardown
   :validators (or validators '())
   :timeout (or timeout efrit-test-default-timeout)
   :tags (or tags '())))

;;; Test Result Structure

(cl-defstruct (efrit-test-result (:constructor efrit-test-result--create))
  "Structure for a test result."
  test-id           ; ID from spec
  test-name         ; Name from spec
  tier              ; Tier from spec
  status            ; 'pass, 'fail, 'error, 'timeout, 'skipped
  start-time        ; When test started
  end-time          ; When test ended
  duration          ; Duration in seconds
  prompt            ; Prompt sent
  response          ; Claude's response text
  validator-results ; List of (validator-name pass-p message)
  error-message     ; Error message if status is 'error
  chat-buffer)      ; Contents of chat buffer for debugging

;;; Validator Functions

(defun efrit-test--validate-buffer-contains (buffer-name expected)
  "Validate that BUFFER-NAME contains EXPECTED text.
Returns (pass-p . message)."
  (let ((buffer (get-buffer buffer-name)))
    (if (not buffer)
        (cons nil (format "Buffer '%s' does not exist" buffer-name))
      (with-current-buffer buffer
        (let ((content (buffer-string)))
          (if (string-match-p (regexp-quote expected) content)
              (cons t (format "Buffer '%s' contains expected text" buffer-name))
            (cons nil (format "Buffer '%s' does not contain '%s'. Content: %s"
                             buffer-name
                             (truncate-string-to-width expected 50)
                             (truncate-string-to-width content 200)))))))))

(defun efrit-test--validate-buffer-matches (buffer-name regexp)
  "Validate that BUFFER-NAME content matches REGEXP.
Returns (pass-p . message)."
  (let ((buffer (get-buffer buffer-name)))
    (if (not buffer)
        (cons nil (format "Buffer '%s' does not exist" buffer-name))
      (with-current-buffer buffer
        (let ((content (buffer-string)))
          (if (string-match-p regexp content)
              (cons t (format "Buffer '%s' matches pattern" buffer-name))
            (cons nil (format "Buffer '%s' does not match '%s'" buffer-name regexp))))))))

(defun efrit-test--validate-elisp-returns (expr expected)
  "Validate that evaluating EXPR returns EXPECTED.
Returns (pass-p . message)."
  (condition-case err
      (let ((result (eval (read expr) t)))
        (if (equal result expected)
            (cons t (format "%s => %S (expected)" expr result))
          (cons nil (format "%s => %S (expected %S)" expr result expected))))
    (error
     (cons nil (format "Error evaluating %s: %s" expr (error-message-string err))))))

(defun efrit-test--validate-function-defined (func-name)
  "Validate that FUNC-NAME is a defined function.
Returns (pass-p . message)."
  (if (fboundp func-name)
      (cons t (format "Function '%s' is defined" func-name))
    (cons nil (format "Function '%s' is not defined" func-name))))

(defun efrit-test--validate-response-contains (response expected)
  "Validate that RESPONSE contains EXPECTED text.
Returns (pass-p . message)."
  (if (and response (string-match-p (regexp-quote expected) response))
      (cons t (format "Response contains '%s'"
                     (truncate-string-to-width expected 50)))
    (cons nil (format "Response does not contain '%s'"
                     (truncate-string-to-width expected 50)))))

(defun efrit-test--validate-response-matches (response regexp)
  "Validate that RESPONSE matches REGEXP.
Returns (pass-p . message)."
  (if (and response (string-match-p regexp response))
      (cons t (format "Response matches pattern"))
    (cons nil (format "Response does not match '%s'" regexp))))

(defun efrit-test--validate-no-error (response)
  "Validate that RESPONSE does not indicate an error.
Returns (pass-p . message)."
  (if (or (null response)
          (string-match-p "\\(?:error\\|Error\\|ERROR\\|failed\\|Failed\\)" response))
      (cons nil "Response contains error indication")
    (cons t "No error indication in response")))

(defun efrit-test--validate-file-exists (path)
  "Validate that file at PATH exists.
Returns (pass-p . message)."
  (if (file-exists-p path)
      (cons t (format "File '%s' exists" path))
    (cons nil (format "File '%s' does not exist" path))))

(defun efrit-test--validate-file-contains (path expected)
  "Validate that file at PATH contains EXPECTED text.
Returns (pass-p . message)."
  (if (not (file-exists-p path))
      (cons nil (format "File '%s' does not exist" path))
    (with-temp-buffer
      (insert-file-contents path)
      (let ((content (buffer-string)))
        (if (string-match-p (regexp-quote expected) content)
            (cons t (format "File '%s' contains expected text" path))
          (cons nil (format "File '%s' does not contain '%s'" path expected)))))))

(defun efrit-test--validate-custom (func &rest args)
  "Run custom validator FUNC with ARGS.
FUNC should return (pass-p . message)."
  (apply func args))

;;; Validator Dispatcher

(defun efrit-test--run-validator (validator-spec response)
  "Run VALIDATOR-SPEC with RESPONSE context.
Returns (validator-name pass-p message)."
  (let* ((validator-type (car validator-spec))
         (args (cdr validator-spec))
         (result
          (pcase validator-type
            ('buffer-contains
             (apply #'efrit-test--validate-buffer-contains args))
            ('buffer-matches
             (apply #'efrit-test--validate-buffer-matches args))
            ('elisp-returns
             (apply #'efrit-test--validate-elisp-returns args))
            ('function-defined
             (efrit-test--validate-function-defined (car args)))
            ('response-contains
             (efrit-test--validate-response-contains response (car args)))
            ('response-matches
             (efrit-test--validate-response-matches response (car args)))
            ('no-error
             (efrit-test--validate-no-error response))
            ('file-exists
             (efrit-test--validate-file-exists (car args)))
            ('file-contains
             (apply #'efrit-test--validate-file-contains args))
            ('custom
             (apply #'efrit-test--validate-custom args))
            (_
             (cons nil (format "Unknown validator type: %s" validator-type))))))
    (list validator-type (car result) (cdr result))))

;;; Test Execution

(defvar efrit-test--current-response nil
  "Stores the response text from the current test.")

(defun efrit-test--wait-for-response (timeout)
  "Wait for efrit-chat response to complete, up to TIMEOUT seconds.
Returns t if response completed, nil if timeout."
  (let ((start-time (current-time))
        (completed nil))
    (while (and (not completed)
                (< (float-time (time-since start-time)) timeout))
      (sleep-for efrit-test-poll-interval)
      ;; Check if response is still in progress
      (when (get-buffer "*efrit-chat*")
        (with-current-buffer "*efrit-chat*"
          (when (and (boundp 'efrit--response-in-progress)
                     (not efrit--response-in-progress))
            (setq completed t)))))
    completed))

(defun efrit-test--extract-response ()
  "Extract Claude's response from the chat buffer."
  (when (get-buffer "*efrit-chat*")
    (with-current-buffer "*efrit-chat*"
      ;; Get buffer contents and extract last assistant response
      (let ((content (buffer-string)))
        ;; Simple extraction - get content after last "Claude:" or similar
        (if (string-match "\\(?:Claude\\|Assistant\\):[[:space:]]*\\(\\(?:.\\|\n\\)*\\)$" content)
            (string-trim (match-string 1 content))
          content)))))

(defun efrit-test--run-single (spec)
  "Run a single test SPEC and return an efrit-test-result."
  (let* ((test-id (efrit-test-spec-id spec))
         (test-name (efrit-test-spec-name spec))
         (tier (efrit-test-spec-tier spec))
         (prompt (efrit-test-spec-prompt spec))
         (timeout (efrit-test-spec-timeout spec))
         (validators (efrit-test-spec-validators spec))
         (setup (efrit-test-spec-setup spec))
         (teardown (efrit-test-spec-teardown spec))
         (start-time (current-time))
         (status 'pass)
         (error-msg nil)
         (response nil)
         (validator-results '())
         (chat-contents nil))

    (when efrit-test-verbose
      (message "[TEST] Running: %s - %s" test-id test-name))

    ;; Run setup if provided
    (when setup
      (condition-case err
          (funcall setup)
        (error
         (setq status 'error)
         (setq error-msg (format "Setup failed: %s" (error-message-string err))))))

    ;; Execute test if setup succeeded
    (when (eq status 'pass)
      (condition-case err
          (progn
            ;; Start fresh chat
            (efrit-chat)

            ;; Send the prompt
            (efrit-send-message prompt)

            ;; Wait for response
            (if (efrit-test--wait-for-response timeout)
                (progn
                  (setq response (efrit-test--extract-response))
                  (setq chat-contents (when (get-buffer "*efrit-chat*")
                                       (with-current-buffer "*efrit-chat*"
                                         (buffer-string))))

                  ;; Run validators
                  (dolist (validator validators)
                    (let ((result (efrit-test--run-validator validator response)))
                      (push result validator-results)
                      (unless (cadr result)  ; pass-p is second element
                        (setq status 'fail)))))

              ;; Timeout
              (setq status 'timeout)
              (setq error-msg (format "Test timed out after %d seconds" timeout))))

        (error
         (setq status 'error)
         (setq error-msg (format "Test execution error: %s"
                                (error-message-string err))))))

    ;; Run teardown if provided
    (when teardown
      (condition-case err
          (funcall teardown)
        (error
         (when efrit-test-verbose
           (message "[TEST] Teardown error: %s" (error-message-string err))))))

    (let ((end-time (current-time)))
      (efrit-test-result--create
       :test-id test-id
       :test-name test-name
       :tier tier
       :status status
       :start-time (format-time-string "%Y-%m-%dT%H:%M:%S%z" start-time)
       :end-time (format-time-string "%Y-%m-%dT%H:%M:%S%z" end-time)
       :duration (float-time (time-subtract end-time start-time))
       :prompt prompt
       :response response
       :validator-results (nreverse validator-results)
       :error-message error-msg
       :chat-buffer chat-contents))))

;;; JSONL Output

(defun efrit-test--results-dir ()
  "Return the test results directory, creating if needed."
  (let ((dir (or efrit-test-results-dir
                (efrit-config-data-file "" "test-results"))))
    (unless (file-directory-p dir)
      (make-directory dir t))
    dir))

(defun efrit-test--result-to-json (result)
  "Convert RESULT to a JSON-encodable alist."
  `((test_id . ,(efrit-test-result-test-id result))
    (test_name . ,(efrit-test-result-test-name result))
    (tier . ,(efrit-test-result-tier result))
    (status . ,(symbol-name (efrit-test-result-status result)))
    (start_time . ,(efrit-test-result-start-time result))
    (end_time . ,(efrit-test-result-end-time result))
    (duration . ,(efrit-test-result-duration result))
    (prompt . ,(efrit-test-result-prompt result))
    (response . ,(or (efrit-test-result-response result) ""))
    (validator_results . ,(vconcat
                          (mapcar (lambda (v)
                                   `((validator . ,(symbol-name (car v)))
                                     (passed . ,(if (cadr v) t :json-false))
                                     (message . ,(caddr v))))
                                 (efrit-test-result-validator-results result))))
    (error_message . ,(or (efrit-test-result-error-message result) :json-null))))

(defun efrit-test--write-result-jsonl (result filename)
  "Append RESULT as JSONL to FILENAME."
  (let ((json-str (json-encode (efrit-test--result-to-json result))))
    (with-temp-buffer
      (insert json-str "\n")
      (append-to-file (point-min) (point-max) filename))))

;;; Test Suite Runner

(defvar efrit-test--registry '()
  "Registry of test specs.")

(defun efrit-test-register (spec)
  "Register a test SPEC in the registry."
  (let ((id (efrit-test-spec-id spec)))
    (setq efrit-test--registry
          (cons spec (cl-remove-if (lambda (s) (equal (efrit-test-spec-id s) id))
                                   efrit-test--registry)))))

(defun efrit-test-clear-registry ()
  "Clear the test registry."
  (setq efrit-test--registry '()))

(defun efrit-test-get-by-id (id)
  "Get test spec by ID from registry."
  (cl-find-if (lambda (s) (equal (efrit-test-spec-id s) id))
              efrit-test--registry))

(defun efrit-test-get-by-tier (tier)
  "Get all test specs for TIER."
  (cl-remove-if-not (lambda (s) (= (efrit-test-spec-tier s) tier))
                    efrit-test--registry))

(defun efrit-test-get-by-category (category)
  "Get all test specs for CATEGORY."
  (cl-remove-if-not (lambda (s) (eq (efrit-test-spec-category s) category))
                    efrit-test--registry))

(defun efrit-test-run-suite (specs &optional output-file)
  "Run a suite of test SPECS, optionally writing results to OUTPUT-FILE.
Returns a list of test results."
  (let* ((results '())
         (output-file (or output-file
                         (expand-file-name
                          (format "results-%s.jsonl"
                                 (format-time-string "%Y%m%d-%H%M%S"))
                          (efrit-test--results-dir))))
         (total (length specs))
         (passed 0)
         (failed 0)
         (errors 0)
         (timeouts 0))

    (message "")
    (message "========================================")
    (message "EFRIT TEST SUITE")
    (message "Running %d tests" total)
    (message "Results: %s" output-file)
    (message "========================================")
    (message "")

    (dolist (spec specs)
      (let ((result (efrit-test--run-single spec)))
        (push result results)
        (efrit-test--write-result-jsonl result output-file)

        ;; Update counts
        (pcase (efrit-test-result-status result)
          ('pass (cl-incf passed))
          ('fail (cl-incf failed))
          ('error (cl-incf errors))
          ('timeout (cl-incf timeouts)))

        ;; Show progress
        (let ((status-icon (pcase (efrit-test-result-status result)
                            ('pass "✅")
                            ('fail "❌")
                            ('error "💥")
                            ('timeout "⏱️")
                            (_ "❓"))))
          (message "%s [%s] %s (%.1fs)"
                   status-icon
                   (efrit-test-result-test-id result)
                   (efrit-test-result-test-name result)
                   (efrit-test-result-duration result)))))

    (message "")
    (message "========================================")
    (message "RESULTS SUMMARY")
    (message "========================================")
    (message "Total:    %d" total)
    (message "Passed:   %d" passed)
    (message "Failed:   %d" failed)
    (message "Errors:   %d" errors)
    (message "Timeouts: %d" timeouts)
    (message "")
    (message "Results written to: %s" output-file)
    (message "========================================")

    (nreverse results)))

(defun efrit-test-run-tier (tier)
  "Run all tests for TIER."
  (interactive "nTier number: ")
  (let ((specs (efrit-test-get-by-tier tier)))
    (if specs
        (efrit-test-run-suite specs)
      (message "No tests registered for tier %d" tier))))

(defun efrit-test-run-all ()
  "Run all registered tests."
  (interactive)
  (if efrit-test--registry
      (efrit-test-run-suite efrit-test--registry)
    (message "No tests registered")))

(defun efrit-test-run-one (id)
  "Run a single test by ID."
  (interactive "sTest ID: ")
  (if-let* ((spec (efrit-test-get-by-id id)))
      (car (efrit-test-run-suite (list spec)))
    (message "No test found with ID: %s" id)))

;;; Interactive Commands

;;;###autoload
(defun efrit-test-list ()
  "List all registered tests."
  (interactive)
  (if (null efrit-test--registry)
      (message "No tests registered")
    (with-current-buffer (get-buffer-create "*Efrit Tests*")
      (erase-buffer)
      (insert "Registered Efrit Tests\n")
      (insert "======================\n\n")
      (let ((by-tier (make-hash-table)))
        ;; Group by tier
        (dolist (spec efrit-test--registry)
          (let ((tier (efrit-test-spec-tier spec)))
            (puthash tier
                     (cons spec (gethash tier by-tier))
                     by-tier)))
        ;; Display by tier
        (maphash
         (lambda (tier specs)
           (insert (format "Tier %d\n" tier))
           (insert (make-string 40 ?-) "\n")
           (dolist (spec (reverse specs))
             (insert (format "  [%s] %s\n"
                            (efrit-test-spec-id spec)
                            (efrit-test-spec-name spec))))
           (insert "\n"))
         by-tier))
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

;;; Sample Tests (Tier 1 - Smoke Tests)

(defun efrit-test-register-tier1-samples ()
  "Register sample Tier 1 smoke tests."
  (interactive)

  ;; Simple arithmetic
  (efrit-test-register
   (efrit-test-spec
    :id "t1-arithmetic"
    :name "Simple arithmetic evaluation"
    :tier 1
    :category 'smoke
    :prompt "Evaluate (+ 2 3) and tell me the result"
    :validators '((response-contains "5")
                  (no-error))
    :timeout 60))

  ;; Buffer creation
  (efrit-test-register
   (efrit-test-spec
    :id "t1-buffer-create"
    :name "Create a buffer"
    :tier 1
    :category 'smoke
    :prompt "Create a buffer called *test-buffer* and insert 'Hello Test'"
    :setup (lambda () (when (get-buffer "*test-buffer*")
                       (kill-buffer "*test-buffer*")))
    :teardown (lambda () (when (get-buffer "*test-buffer*")
                          (kill-buffer "*test-buffer*")))
    :validators '((buffer-contains "*test-buffer*" "Hello"))
    :timeout 60))

  ;; Function definition
  (efrit-test-register
   (efrit-test-spec
    :id "t1-defun"
    :name "Define a simple function"
    :tier 1
    :category 'smoke
    :prompt "Define a function called test-add-numbers that adds two numbers and returns the result. Then evaluate it."
    :setup (lambda () (fmakunbound 'test-add-numbers))
    :teardown (lambda () (fmakunbound 'test-add-numbers))
    :validators '((function-defined test-add-numbers)
                  (no-error))
    :timeout 90))

  (message "Registered 3 Tier 1 sample tests"))

(provide 'efrit-test-runner)

;;; efrit-test-runner.el ends here
