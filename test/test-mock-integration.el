;;; test-mock-integration.el --- Integration tests with mocked Claude responses -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Keywords: test

;;; Commentary:
;; Integration tests for efrit-do with mocked Claude API responses.
;; Tests full retry cycles with realistic response patterns.

;;; Code:

;; Add the lisp directory to load path
(let ((test-dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "../lisp" test-dir)))
(require 'ert)
(require 'efrit-do)
(require 'json)

;; Mock state variables
(defvar test-mock--api-responses nil
  "Queue of mock API responses to return in sequence.")

(defvar test-mock--api-call-count 0
  "Counter for number of API calls made.")

(defvar test-mock--last-request-data nil
  "Last request data sent to mocked API.")

;; Mock API response generation
(defun test-mock--create-api-response (text-content tool-calls)
  "Create a mock API response with TEXT-CONTENT and TOOL-CALLS.
TOOL-CALLS should be a list of (name input) pairs."
  (let ((content-items (list `(("type" . "text") ("text" . ,text-content)))))
    (dolist (tool-call tool-calls)
      (let ((tool-name (car tool-call))
            (tool-input (cadr tool-call)))
        (push `(("type" . "tool_use")
                ("name" . ,tool-name)
                ("input" . (("expr" . ,tool-input))))
              content-items)))
    `(("content" . ,(vconcat (reverse content-items))))))

(defun test-mock--create-error-response (error-type error-message)
  "Create a mock API error response."
  `(("error" . (("type" . ,error-type) ("message" . ,error-message)))))

;; Mock API function
(defun test-mock--api-call (url &optional _unused1 _unused2 _unused3)
  "Mock version of url-retrieve-synchronously that returns predetermined responses.
This function mimics the signature of url-retrieve-synchronously."
  (setq test-mock--api-call-count (1+ test-mock--api-call-count))
  ;; Capture the request data from the global variable that url-retrieve-synchronously uses
  (setq test-mock--last-request-data url-request-data)
  
  (if test-mock--api-responses
      (let* ((response (pop test-mock--api-responses))
             (buffer-name (format "*mock-response-%d*" test-mock--api-call-count))
             (response-buffer (get-buffer-create buffer-name)))
        ;; Create a real buffer with the mock response
        (with-current-buffer response-buffer
          (erase-buffer)
          (insert "HTTP/1.1 200 OK\n")
          (insert "Content-Type: application/json\n")
          (insert "\n")
          (insert (json-encode response)))

        response-buffer)
    ;; Return a default success response when no more responses available  
    (let* ((buffer-name (format "*mock-response-default-%d*" test-mock--api-call-count))
           (response-buffer (get-buffer-create buffer-name)))
      (with-current-buffer response-buffer
        (erase-buffer)
        (insert "HTTP/1.1 200 OK\n")
        (insert "Content-Type: application/json\n")
        (insert "\n")
        (insert (json-encode (test-mock--create-api-response "Default response" '(("eval_sexp" "(message \"default\")")))))
        response-buffer))))

;; Test setup/teardown
(defun test-mock--setup ()
  "Set up mock testing environment."
  (setq test-mock--api-responses nil)
  (setq test-mock--api-call-count 0)
  (setq test-mock--last-request-data nil)
  (setq efrit-do-debug t)
  (setq efrit-do-show-results nil)
  (setq efrit-do-retry-on-errors t)
  (setq efrit-do-max-retries 2))

(defun test-mock--cleanup ()
  "Clean up after mock tests."
  (setq efrit-do-debug nil)
  (setq efrit-do-show-results t)
  (setq efrit-do-retry-on-errors t)
  (setq efrit-do-max-retries 3)
  ;; Clean up any remaining mock response buffers
  (dolist (buffer (buffer-list))
    (when (string-match "\\*mock-response.*\\*" (buffer-name buffer))
      (kill-buffer buffer))))

;; Integration test cases

(ert-deftest test-mock-successful-first-attempt ()
  "Test successful execution on first attempt."
  (test-mock--setup)
  
  ;; Mock response: Claude returns valid elisp that works
  (setq test-mock--api-responses
        (list (test-mock--create-api-response 
               "I'll help you create a test buffer."
               '(("eval_sexp" "(with-current-buffer (get-buffer-create \"*test*\") (insert \"success\") (buffer-name))")))))
  
  ;; Mock the URL retrieval function
  (let ((original-fn (symbol-function 'url-retrieve-synchronously)))
    (unwind-protect
        (progn
          (fset 'url-retrieve-synchronously #'test-mock--api-call)
          
          ;; Execute command
          (efrit-do "create a test buffer")
          (let ((result efrit-do--last-result))
            (should (stringp result))
            (should (string-match "success" result))
            (should (= test-mock--api-call-count 1))))
      
      ;; Restore original function
      (fset 'url-retrieve-synchronously original-fn)))
  
  (test-mock--cleanup))

(ert-deftest test-mock-syntax-error-then-success ()
  "Test retry cycle: syntax error followed by successful fix."
  (test-mock--setup)
  
  ;; Mock responses:
  ;; 1. First attempt: Claude returns invalid elisp
  ;; 2. Retry attempt: Claude fixes the syntax error
  (setq test-mock--api-responses
        (list 
         ;; First response: syntax error
         (test-mock--create-api-response 
          "I'll create a buffer for you."
          '(("eval_sexp" "(with-current-buffer (get-buffer-create \"*test*\" (insert \"broken\")")))) ; Missing closing paren
         
         ;; Second response: fixed elisp
         (test-mock--create-api-response 
          "Let me fix that syntax error."
          '(("eval_sexp" "(with-current-buffer (get-buffer-create \"*test*\") (insert \"fixed\") (buffer-name))")))))
  
  ;; Mock the URL retrieval function
  (let ((original-fn (symbol-function 'url-retrieve-synchronously)))
    (unwind-protect
        (progn
          (fset 'url-retrieve-synchronously #'test-mock--api-call)
          
          ;; Execute command - should succeed after retry
          (let ((result (efrit-do--execute-command "create a test buffer")))
            (should (stringp result))
            (should (string-match "fixed" result))
            (should (= test-mock--api-call-count 2))))
      
      ;; Restore original function
      (fset 'url-retrieve-synchronously original-fn)))
  
  (test-mock--cleanup))

(ert-deftest test-mock-runtime-error-then-success ()
  "Test retry cycle: runtime error followed by successful fix."
  (test-mock--setup)
  
  ;; Mock responses:
  ;; 1. First attempt: undefined function
  ;; 2. Retry attempt: fixed function call
  (setq test-mock--api-responses
        (list 
         ;; First response: runtime error
         (test-mock--create-api-response 
          "I'll call that function."
          '(("eval_sexp" "(nonexistent-function \"test\")")))
         
         ;; Second response: fixed function call
         (test-mock--create-api-response 
          "Let me use a valid function instead."
          '(("eval_sexp" "(message \"test\")")))))
  
  ;; Mock the URL retrieval function
  (let ((original-fn (symbol-function 'url-retrieve-synchronously)))
    (unwind-protect
        (progn
          (fset 'url-retrieve-synchronously #'test-mock--api-call)
          
          ;; Execute command - should succeed after retry
          (let ((result (efrit-do--execute-command "call a test function")))
            (should (stringp result))
            (should (string-match "Executed.*message" result))
            (should (= test-mock--api-call-count 2))))
      
      ;; Restore original function
      (fset 'url-retrieve-synchronously original-fn)))
  
  (test-mock--cleanup))

(ert-deftest test-mock-exhausted-retries ()
  "Test behavior when all retry attempts are exhausted."
  (test-mock--setup)
  
  ;; Mock responses: all attempts return broken elisp
  (setq test-mock--api-responses
        (list 
         ;; First attempt: syntax error
         (test-mock--create-api-response 
          "First try."
          '(("eval_sexp" "(broken-syntax-1")))
         
         ;; First retry: still broken
         (test-mock--create-api-response 
          "Let me fix that."
          '(("eval_sexp" "(broken-syntax-2")))
         
         ;; Second retry: still broken
         (test-mock--create-api-response 
          "Another attempt."
          '(("eval_sexp" "(broken-syntax-3")))))
  
  ;; Mock the URL retrieval function
  (let ((original-fn (symbol-function 'url-retrieve-synchronously)))
    (unwind-protect
        (progn
          (fset 'url-retrieve-synchronously #'test-mock--api-call)
          
          ;; Execute command - should fail after exhausting retries
          (let ((result (efrit-do--execute-command "do something")))
            (should (stringp result))
            (should (string-match "Syntax Error" result))
            (should (= test-mock--api-call-count 3)))) ; 1 + 2 retries
      
      ;; Restore original function
      (fset 'url-retrieve-synchronously original-fn)))
  
  (test-mock--cleanup))

;; Test runner
(defun test-mock-run-all ()
  "Run all mock integration tests."
  (interactive)
  (ert-run-tests-batch "^test-mock.*"))

(provide 'test-mock-integration)

;;; test-mock-integration.el ends here
