;;; test-agent-mock.el --- Mock-based agentic mode tests -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;;; Commentary:

;; Comprehensive test suite for efrit's agentic mode using mock API responses.
;; Tests run without burning API tokens by simulating Claude's responses.
;;
;; Tests cover:
;; - Simple single-step workflows
;; - Multi-step tool call chains
;; - Error handling and recovery
;; - Session lifecycle (start, interrupt, complete, fail)
;; - Agent buffer UI rendering
;; - Status transitions
;; - Tool execution and result handling
;;
;; The mock framework intercepts API calls and returns pre-configured responses,
;; allowing deterministic testing of the agentic loop.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'efrit-session)
(require 'efrit-agent-core)
(require 'efrit-agent)
(require 'efrit-do-async-loop)
(require 'efrit-chat-response)

;;; Mock Framework

(defvar test-agent-mock--responses nil
  "Queue of mock responses to return from API calls.
Each element is a response object or alist that will be returned
by the next API call.")

(defvar test-agent-mock--api-calls nil
  "List of captured API calls for assertion.
Each element is (messages session-id) from each call.")

(defvar test-agent-mock--tool-results nil
  "Alist mapping tool-name to mock result to return.
Used to mock tool execution results.")

(defun test-agent-mock--make-response (content stop-reason &optional error-msg)
  "Create a mock API response with CONTENT and STOP-REASON.
CONTENT should be a vector of content blocks.
If ERROR-MSG is non-nil, create an error response."
  (let ((response (make-hash-table :test 'equal)))
    (if error-msg
        (let ((error-obj (make-hash-table :test 'equal)))
          (puthash "type" "error" error-obj)
          (puthash "message" error-msg error-obj)
          (puthash "error" error-obj response))
      (progn
        (puthash "content" content response)
        (puthash "stop_reason" stop-reason response)))
    response))

(defun test-agent-mock--make-text-content (text)
  "Create a text content block with TEXT."
  (let ((ht (make-hash-table :test 'equal)))
    (puthash "type" "text" ht)
    (puthash "text" text ht)
    ht))

(defun test-agent-mock--make-tool-use (id name input)
  "Create a tool_use content block with ID, NAME, and INPUT."
  (let ((ht (make-hash-table :test 'equal)))
    (puthash "type" "tool_use" ht)
    (puthash "id" id ht)
    (puthash "name" name ht)
    (puthash "input" input ht)
    ht))

(defun test-agent-mock--api-call-mock (session messages callback)
  "Mock API call handler for testing.
Pops from `test-agent-mock--responses' and calls CALLBACK.
Correctly routes error responses through the error path."
  (push (list messages (efrit-session-id session)) test-agent-mock--api-calls)
  (let ((delay 0.01))
    (if test-agent-mock--responses
        (let* ((response (pop test-agent-mock--responses))
               (error-obj (and (hash-table-p response)
                               (gethash "error" response)))
               (error-msg (and error-obj (gethash "message" error-obj))))
          ;; Route error responses through the error callback path
          (if error-msg
              (run-at-time delay nil callback nil error-msg)
            (run-at-time delay nil callback response nil)))
      (run-at-time delay nil callback nil "No mock response configured"))))

(defun test-agent-mock--execute-tools-mock (session content)
  "Mock tool execution that extracts tools, runs mock results, continues loop.
SESSION is the session object, CONTENT is the content vector."
  (let ((session-id (efrit-session-id session))
        (results nil))
    ;; Store Claude's response before processing
    (efrit-session-add-assistant-response session content)
    
    ;; Process each content item
    (dotimes (i (length content))
      (let* ((item (aref content i))
             (item-type (when (hash-table-p item) (gethash "type" item))))
        (when (equal item-type "tool_use")
          (let* ((tool-id (gethash "id" item))
                 (tool-name (gethash "name" item))
                 (input (gethash "input" item))
                 ;; Get mock result
                 (mock-result (alist-get tool-name test-agent-mock--tool-results nil nil 'equal))
                 (result (cond
                          ((functionp mock-result)
                           (condition-case err
                               (funcall mock-result tool-name input)
                             (error (format "Error executing %s: %s" tool-name (error-message-string err)))))
                          (mock-result mock-result)
                          (t (format "Mock result for %s" tool-name))))
                 (is-error (string-prefix-p "Error " result)))
            ;; Add activity to agent buffer for tracking tests
            (when (fboundp 'efrit-agent-add-activity)
              (efrit-agent-add-activity
               (list :id tool-id
                     :type 'tool
                     :tool tool-name
                     :input (format "%S" input)
                     :result result
                     :success (not is-error)
                     :timestamp (current-time))))
            ;; Collect result
            (push (efrit-session-build-tool-result tool-id result is-error) results)))))
    
    ;; Add results to session
    (when results
      (efrit-session-add-tool-results session (nreverse results)))
    
    ;; Continue iteration
    (efrit-do-async--continue-iteration session)))

(defun test-agent-mock--stream-content-noop (_text)
  "No-op mock for streaming content to agent buffer.")

(defun test-agent-mock--stream-end-noop ()
  "No-op mock for ending content stream.")

(defmacro with-agent-mock (&rest body)
  "Execute BODY with mock API and tool handlers active.
Cleans up after execution."
  (declare (indent 0) (debug t))
  `(let ((test-agent-mock--responses nil)
         (test-agent-mock--api-calls nil)
         (test-agent-mock--tool-results nil)
         (efrit-agent-auto-show nil)
         (efrit-do-async-show-progress-buffer nil))
     (unwind-protect
         (progn
           ;; Install mock API call handler
           (advice-add 'efrit-do-async--api-call :override
                       #'test-agent-mock--api-call-mock)
           ;; Install mock tool executor (replaces entire execute-tools function)
           (advice-add 'efrit-do-async--execute-tools :override
                       #'test-agent-mock--execute-tools-mock)
           ;; Mock streaming functions to avoid buffer rendering issues
           (when (fboundp 'efrit-agent-stream-content)
             (advice-add 'efrit-agent-stream-content :override
                         #'test-agent-mock--stream-content-noop))
           (when (fboundp 'efrit-agent-stream-end)
             (advice-add 'efrit-agent-stream-end :override
                         #'test-agent-mock--stream-end-noop))
           ,@body)
       ;; Cleanup
       (advice-remove 'efrit-do-async--api-call #'test-agent-mock--api-call-mock)
       (advice-remove 'efrit-do-async--execute-tools #'test-agent-mock--execute-tools-mock)
       (when (fboundp 'efrit-agent-stream-content)
         (advice-remove 'efrit-agent-stream-content #'test-agent-mock--stream-content-noop))
       (when (fboundp 'efrit-agent-stream-end)
         (advice-remove 'efrit-agent-stream-end #'test-agent-mock--stream-end-noop))
       ;; Clean up any test buffers
       (when (get-buffer "*efrit-agent*")
         (kill-buffer "*efrit-agent*"))
       (maphash (lambda (session-id _)
                  (let ((pbuf (efrit-progress-get-buffer session-id)))
                    (when (buffer-live-p pbuf)
                      (kill-buffer pbuf))))
                efrit-do-async--loops)
       (clrhash efrit-do-async--loops))))

(defun test-agent-mock--queue-response (response)
  "Add RESPONSE to the mock response queue."
  (setq test-agent-mock--responses
        (append test-agent-mock--responses (list response))))

(defun test-agent-mock--wait-for-completion (session-id &optional timeout)
  "Wait for session SESSION-ID to complete, up to TIMEOUT seconds.
Returns t if completed, nil if timeout."
  (let ((timeout (or timeout 5))
        (start (current-time))
        (completed nil))
    (while (and (not completed)
                (< (float-time (time-since start)) timeout))
      (unless (gethash session-id efrit-do-async--loops)
        (setq completed t))
      (unless completed
        (sleep-for 0.1)))
    completed))

;;; Test Helpers

(defun test-agent-mock--create-test-session (command)
  "Create a test session with COMMAND."
  (efrit-session-create (format "test-%s" (random 10000)) command))

;;; Tests: Simple Single-Step Workflows

(ert-deftest test-agent-mock-simple-text-response ()
  "Test simple command with text-only response (no tools)."
  (with-agent-mock
    ;; Queue a simple text response with end_turn
    (test-agent-mock--queue-response
     (test-agent-mock--make-response
      (vector (test-agent-mock--make-text-content "Hello! I can help you with that."))
      "end_turn"))
    
    (let* ((session (test-agent-mock--create-test-session "Say hello"))
           (session-id (efrit-do-async-loop session nil)))
      
      ;; Wait for completion
      (should (test-agent-mock--wait-for-completion session-id))
      
      ;; Verify API was called
      (should (= 1 (length test-agent-mock--api-calls)))
      
      ;; Verify session completed
      (should-not (gethash session-id efrit-do-async--loops)))))

(ert-deftest test-agent-mock-single-tool-call ()
  "Test command with single tool call."
  (with-agent-mock
    ;; Queue tool_use response followed by end_turn
    (test-agent-mock--queue-response
     (test-agent-mock--make-response
      (vector (test-agent-mock--make-text-content "Let me check that file.")
              (test-agent-mock--make-tool-use "tool-1" "read_file"
                                               '(("path" . "/test/file.txt"))))
      "tool_use"))
    
    (test-agent-mock--queue-response
     (test-agent-mock--make-response
      (vector (test-agent-mock--make-text-content "The file contains test data."))
      "end_turn"))
    
    ;; Mock tool result
    (push '("read_file" . "file contents here") test-agent-mock--tool-results)
    
    (let* ((session (test-agent-mock--create-test-session "Read the file"))
           (session-id (efrit-do-async-loop session nil)))
      
      ;; Wait for completion
      (should (test-agent-mock--wait-for-completion session-id 10))
      
      ;; Verify two API calls were made (initial + after tool result)
      (should (= 2 (length test-agent-mock--api-calls))))))

;;; Tests: Multi-Step Tool Call Chains

(ert-deftest test-agent-mock-multi-tool-chain ()
  "Test multi-step workflow with multiple sequential tool calls."
  (with-agent-mock
    ;; First response: read a file
    (test-agent-mock--queue-response
     (test-agent-mock--make-response
      (vector (test-agent-mock--make-tool-use "tool-1" "read_file"
                                               '(("path" . "/test/config.json"))))
      "tool_use"))
    
    ;; Second response: search in file based on first result
    (test-agent-mock--queue-response
     (test-agent-mock--make-response
      (vector (test-agent-mock--make-text-content "Found config, now searching...")
              (test-agent-mock--make-tool-use "tool-2" "search_content"
                                               '(("pattern" . "api_key"))))
      "tool_use"))
    
    ;; Third response: complete
    (test-agent-mock--queue-response
     (test-agent-mock--make-response
      (vector (test-agent-mock--make-text-content "Found the API key in the config."))
      "end_turn"))
    
    ;; Mock tool results
    (push '("read_file" . "{\"api_key\": \"secret123\"}") test-agent-mock--tool-results)
    (push '("search_content" . "config.json:1: api_key") test-agent-mock--tool-results)
    
    (let* ((session (test-agent-mock--create-test-session "Find API key"))
           (session-id (efrit-do-async-loop session nil)))
      
      ;; Wait for completion
      (should (test-agent-mock--wait-for-completion session-id 10))
      
      ;; Verify three API calls were made
      (should (= 3 (length test-agent-mock--api-calls))))))

(ert-deftest test-agent-mock-parallel-tools ()
  "Test multiple tool calls in a single response."
  (with-agent-mock
    ;; Response with multiple tool calls
    (test-agent-mock--queue-response
     (test-agent-mock--make-response
      (vector (test-agent-mock--make-text-content "Let me check both files.")
              (test-agent-mock--make-tool-use "tool-1" "read_file"
                                               '(("path" . "/file1.txt")))
              (test-agent-mock--make-tool-use "tool-2" "read_file"
                                               '(("path" . "/file2.txt"))))
      "tool_use"))
    
    ;; Complete after parallel tools
    (test-agent-mock--queue-response
     (test-agent-mock--make-response
      (vector (test-agent-mock--make-text-content "Both files processed."))
      "end_turn"))
    
    ;; Mock tool results (both use same handler)
    (push '("read_file" . "file content") test-agent-mock--tool-results)
    
    (let* ((session (test-agent-mock--create-test-session "Read both files"))
           (session-id (efrit-do-async-loop session nil)))
      
      (should (test-agent-mock--wait-for-completion session-id 10))
      (should (= 2 (length test-agent-mock--api-calls))))))

;;; Tests: Error Handling and Recovery

(ert-deftest test-agent-mock-api-error ()
  "Test handling of API errors."
  (with-agent-mock
    ;; Queue an error response
    (test-agent-mock--queue-response
     (test-agent-mock--make-response nil nil "Rate limit exceeded"))
    
    (let* ((session (test-agent-mock--create-test-session "Test error"))
           (session-id (efrit-do-async-loop session nil)))
      
      ;; Wait for completion
      (should (test-agent-mock--wait-for-completion session-id))
      
      ;; Verify session stopped
      (should-not (gethash session-id efrit-do-async--loops)))))

(ert-deftest test-agent-mock-tool-failure ()
  "Test handling of tool execution failure."
  (with-agent-mock
    ;; Queue tool call
    (test-agent-mock--queue-response
     (test-agent-mock--make-response
      (vector (test-agent-mock--make-tool-use "tool-1" "read_file"
                                               '(("path" . "/nonexistent"))))
      "tool_use"))
    
    ;; Claude handles the error
    (test-agent-mock--queue-response
     (test-agent-mock--make-response
      (vector (test-agent-mock--make-text-content "File not found, trying alternative."))
      "end_turn"))
    
    ;; Mock tool to return error
    (push (cons "read_file" (lambda (_name _input)
                              (signal 'file-error '("File not found"))))
          test-agent-mock--tool-results)
    
    (let* ((session (test-agent-mock--create-test-session "Read missing file"))
           (session-id (efrit-do-async-loop session nil)))
      
      (should (test-agent-mock--wait-for-completion session-id 10)))))

;;; Tests: Session Lifecycle

(ert-deftest test-agent-mock-session-interrupt ()
  "Test session interruption during execution."
  (with-agent-mock
    ;; Queue a tool call that would be interrupted
    (test-agent-mock--queue-response
     (test-agent-mock--make-response
      (vector (test-agent-mock--make-tool-use "tool-1" "slow_operation"
                                               '(("seconds" . 60))))
      "tool_use"))
    
    (let* ((stop-reason nil)
           (session (test-agent-mock--create-test-session "Long operation"))
           (session-id (efrit-do-async-loop
                        session nil
                        (lambda (_s reason)
                          (setq stop-reason reason)))))
      
      ;; Request interrupt immediately before any timer fires
      (efrit-session-request-interrupt session)
      
      ;; Session should stop due to interrupt
      (should (test-agent-mock--wait-for-completion session-id 5))
      ;; Verify it was actually interrupted, not just error
      (should (equal stop-reason "interrupted")))))

(ert-deftest test-agent-mock-iteration-limit ()
  "Test that iteration limit is respected."
  (with-agent-mock
    (let ((efrit-do-async-max-iterations 2))
      ;; Queue responses that would loop forever
      (dotimes (_ 5)
        (test-agent-mock--queue-response
         (test-agent-mock--make-response
          (vector (test-agent-mock--make-tool-use "tool-1" "echo" '(("msg" . "loop"))))
          "tool_use")))
      
      (push '("echo" . "echoed") test-agent-mock--tool-results)
      
      (let* ((session (test-agent-mock--create-test-session "Infinite loop"))
             (session-id (efrit-do-async-loop session nil)))
        
        ;; Should stop at iteration limit
        (should (test-agent-mock--wait-for-completion session-id 10))
        
        ;; Should have made 2 iterations (the limit)
        (should (= 2 (length test-agent-mock--api-calls)))))))

;;; Tests: Agent Buffer UI

(ert-deftest test-agent-mock-buffer-status-transitions ()
  "Test agent buffer status updates during execution."
  (with-agent-mock
    ;; Keep auto-show nil to avoid display issues in batch mode
    (let ((efrit-agent-auto-show nil))
      (test-agent-mock--queue-response
       (test-agent-mock--make-response
        (vector (test-agent-mock--make-text-content "Working on it."))
        "end_turn"))
      
      (let* ((session (test-agent-mock--create-test-session "Status test"))
             (session-id (efrit-do-async-loop session nil)))
        
        ;; Check initial status
        (with-current-buffer (efrit-agent--get-buffer)
          (should (eq efrit-agent--status 'working)))
        
        ;; Wait for completion
        (test-agent-mock--wait-for-completion session-id)
        
        ;; Check final status
        (with-current-buffer (efrit-agent--get-buffer)
          (should (eq efrit-agent--status 'complete)))))))

(ert-deftest test-agent-mock-buffer-activity-tracking ()
  "Test that activities are tracked in agent buffer."
  (with-agent-mock
    (test-agent-mock--queue-response
     (test-agent-mock--make-response
      (vector (test-agent-mock--make-tool-use "tool-1" "read_file"
                                               '(("path" . "/test.txt"))))
      "tool_use"))
    
    (test-agent-mock--queue-response
     (test-agent-mock--make-response
      (vector (test-agent-mock--make-text-content "Done."))
      "end_turn"))
    
    (push '("read_file" . "test content") test-agent-mock--tool-results)
    
    (let* ((session (test-agent-mock--create-test-session "Activity test"))
           (session-id (efrit-do-async-loop session nil)))
      
      (test-agent-mock--wait-for-completion session-id 10)
      
      ;; Check that agent buffer has activity entries
      (with-current-buffer (efrit-agent--get-buffer)
        ;; Activities should have been recorded (at least 1 for tool call)
        (should (> (length efrit-agent--activities) 0))))))

;;; Tests: Completion Callback

(ert-deftest test-agent-mock-completion-callback ()
  "Test that completion callback is invoked correctly."
  (with-agent-mock
    (let ((callback-invoked nil)
          (callback-session nil)
          (callback-reason nil))
      
      (test-agent-mock--queue-response
       (test-agent-mock--make-response
        (vector (test-agent-mock--make-text-content "Done!"))
        "end_turn"))
      
      (let* ((session (test-agent-mock--create-test-session "Callback test"))
             (callback (lambda (s reason)
                        (setq callback-invoked t
                              callback-session s
                              callback-reason reason)))
             (session-id (efrit-do-async-loop session nil callback)))
        
        (test-agent-mock--wait-for-completion session-id)
        
        ;; Callback should have been invoked
        (should callback-invoked)
        (should callback-session)
        (should (equal callback-reason "end_turn"))))))

;;; Tests: Message Accumulation

(ert-deftest test-agent-mock-message-history ()
  "Test that messages are accumulated in session history."
  (with-agent-mock
    (test-agent-mock--queue-response
     (test-agent-mock--make-response
      (vector (test-agent-mock--make-tool-use "tool-1" "echo"
                                               '(("msg" . "test"))))
      "tool_use"))
    
    (test-agent-mock--queue-response
     (test-agent-mock--make-response
      (vector (test-agent-mock--make-text-content "All done."))
      "end_turn"))
    
    (push '("echo" . "test echoed") test-agent-mock--tool-results)
    
    (let* ((session (test-agent-mock--create-test-session "History test"))
           (session-id (efrit-do-async-loop session nil)))
      
      (test-agent-mock--wait-for-completion session-id 10)
      
      ;; Verify messages accumulated in session
      (let ((messages (efrit-session-get-api-messages-for-continuation session)))
        ;; Should have user message + assistant responses + tool results
        (should (> (length messages) 1))))))

;;; Tests: Unknown Stop Reason

(ert-deftest test-agent-mock-unknown-stop-reason ()
  "Test handling of unknown stop_reason from API."
  (with-agent-mock
    (test-agent-mock--queue-response
     (test-agent-mock--make-response
      (vector (test-agent-mock--make-text-content "Something unexpected."))
      "unknown_reason"))
    
    (let* ((session (test-agent-mock--create-test-session "Unknown stop"))
           (session-id (efrit-do-async-loop session nil)))
      
      ;; Should complete (stops on unknown reason)
      (should (test-agent-mock--wait-for-completion session-id)))))

(provide 'test-agent-mock)

;;; test-agent-mock.el ends here
