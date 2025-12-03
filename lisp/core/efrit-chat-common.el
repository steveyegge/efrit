;;; efrit-chat-common.el --- Shared chat logic for Efrit -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai
;; URL: https://github.com/steveyegge/efrit

;;; Commentary:
;; Shared variables, customizations, circuit breaker, and helper functions
;; used by both classic and streamlined chat modes.

;;; Code:

(require 'cl-lib)
(require 'efrit-config)
(require 'efrit-api)
(require 'efrit-common)
(require 'efrit-chat-transparency)
(require 'efrit-do-schema)

;; Forward declarations
(declare-function efrit-log-debug "efrit-log")
(declare-function efrit-log-error "efrit-log")
(declare-function efrit-tools-system-prompt "efrit-tools")
(declare-function efrit-tools-get-context "efrit-tools")
(declare-function efrit-tools-eval-sexp "efrit-tools")
(declare-function efrit-tool-read-image "efrit-tool-read-image")
(declare-function efrit-tool-create-buffer "efrit-tool-edit-buffer")
(declare-function efrit-tool-edit-buffer "efrit-tool-edit-buffer")
(declare-function efrit-tool-read-buffer "efrit-tool-edit-buffer")
(declare-function efrit-tool-buffer-info "efrit-tool-edit-buffer")
(declare-function efrit-do--execute-tool "efrit-do-dispatch")

;;; Shared Variables

(defvar efrit-model nil
  "Model to use for API calls.
When nil, uses `efrit-default-model' from efrit-config.")

(defvar efrit-max-tokens 4096
  "Maximum tokens in API response.")

(defvar efrit-default-model nil
  "Default model for API calls.")

;;; Customization

(defcustom efrit-temperature 0.1
  "Temperature setting for response generation (0.0-1.0)."
  :type 'float
  :group 'efrit)

(defcustom efrit-enable-tools t
  "Whether to enable tools for the assistant."
  :type 'boolean
  :group 'efrit)

;;; Circuit Breaker - Tool Call Tracking

(defvar-local efrit-chat--tool-call-count 0
  "Count of tool calls in current chat session.
Reset when user sends a new message.")

(defvar-local efrit-chat--consecutive-errors 0
  "Count of consecutive tool execution errors.
Reset on successful tool execution.")

(defcustom efrit-chat-max-tool-calls-per-turn 50
  "Maximum tool calls allowed per user turn in chat mode.
This is a safety limit to prevent runaway tool loops.
Lower than efrit-do since chat should be more conversational."
  :type 'integer
  :group 'efrit)

(defcustom efrit-chat-max-consecutive-errors 5
  "Maximum consecutive tool errors before stopping.
Prevents burning tokens retrying the same failing operation."
  :type 'integer
  :group 'efrit)

(defun efrit-chat--reset-circuit-breaker ()
  "Reset circuit breaker counters for new user turn."
  (setq-local efrit-chat--tool-call-count 0)
  (setq-local efrit-chat--consecutive-errors 0))

(defun efrit-chat--circuit-breaker-check ()
  "Check if circuit breaker limits exceeded.
Returns nil if OK to proceed, or an error message string if limits exceeded."
  (cond
   ((>= efrit-chat--tool-call-count efrit-chat-max-tool-calls-per-turn)
    (format "Circuit breaker: Tool call limit reached (%d/%d). Session stopped to prevent runaway."
            efrit-chat--tool-call-count efrit-chat-max-tool-calls-per-turn))
   ((>= efrit-chat--consecutive-errors efrit-chat-max-consecutive-errors)
    (format "Circuit breaker: Too many consecutive errors (%d). Session stopped."
            efrit-chat--consecutive-errors))
   (t nil)))

(defun efrit-chat--record-tool-call (success-p)
  "Record a tool call. SUCCESS-P indicates if it succeeded."
  (cl-incf efrit-chat--tool-call-count)
  (if success-p
      (setq-local efrit-chat--consecutive-errors 0)
    (cl-incf efrit-chat--consecutive-errors)))

;;; Helper Functions

(defun efrit--get-api-key ()
  "Get the Anthropic API key.
Supports multiple sources: environment variable, authinfo, or config file."
  (require 'efrit-common)
  (efrit-common-get-api-key))

;;; Error Classification

(defun efrit--classify-error (error-details)
  "Classify an error and return (type description recommendation).
Analyzes error details to determine if it's rate-limiting, auth, network, etc."
  (let ((error-str (format "%s" error-details)))
    (cond
     ;; Rate limiting errors
     ((or (string-match-p "429" error-str)
          (string-match-p "rate" error-str)
          (string-match-p "too.*many.*request" error-str))
      '("rate-limit" 
        "Rate limit exceeded" 
        "Please wait before retrying. Use M-x efrit-retry-last-message after a short delay."))
     ;; Authentication errors
     ((or (string-match-p "401" error-str)
          (string-match-p "unauthorized" error-str)
          (string-match-p "API.*key" error-str)
          (string-match-p "authentication" error-str))
      '("auth-error"
        "Authentication failed" 
        "Check your ANTHROPIC_API_KEY environment variable or Emacs keyring."))
     ;; Permission/quota errors
     ((or (string-match-p "403" error-str)
          (string-match-p "forbidden" error-str)
          (string-match-p "quota" error-str))
      '("permission-error"
        "Permission or quota exceeded"
        "Check your API account status or quota limits."))
     ;; Bad request errors
     ((or (string-match-p "400" error-str)
          (string-match-p "bad.*request" error-str)
          (string-match-p "invalid" error-str))
      '("bad-request"
        "Invalid request"
        "There may be an issue with how the request was formatted. Try sending a simpler message."))
     ;; Server errors
     ((or (string-match-p "500" error-str)
          (string-match-p "502" error-str)
          (string-match-p "503" error-str)
          (string-match-p "internal.*error" error-str))
      '("server-error"
        "API server error"
        "The API service is temporarily unavailable. Try again in a moment."))
     ;; Network/connection errors
     ((or (string-match-p "nodename.*provided" error-str)
          (string-match-p "connection.*refused" error-str)
          (string-match-p "timeout" error-str)
          (string-match-p "network" error-str))
      '("network-error"
        "Network connection error"
        "Check your internet connection and firewall settings."))
     ;; Default unknown error
     (t
      '("unknown-error"
        "Unknown error"
        "Unexpected error occurred. Check the logs for more details.")))))

;;; Safe Error Message Helper

(defun efrit--safe-error-message (err)
  "Extract a safe error message string from ERR.
ERR can be an error object, string, or other type."
  (cond
   ((stringp err) err)
   ((and (listp err) (symbolp (car err)))
    (error-message-string err))
   (t (format "%s" err))))

;;; Shared Tool Execution

(defun efrit-chat--execute-builtin-tool (tool-name input)
  "Execute a built-in tool TOOL-NAME with INPUT hash-table.
Returns the result string, or an error message.
For tools not handled here, returns nil to indicate delegation is needed."
  (condition-case err
      (cond
       ;; Handle eval_sexp tool call
       ((string= tool-name "eval_sexp")
        (let ((expr (gethash "expr" input)))
          (efrit-log-debug "Executing elisp: %s" expr)
          (if expr
              (let ((result (efrit-tools-eval-sexp expr)))
                (efrit-log-debug "Elisp result: %s" result)
                result)
            "Error: No expression provided")))
       
       ;; Handle get_context tool call
       ((string= tool-name "get_context")
        (efrit-tools-get-context))
       
       ;; Handle read_image tool call
       ((string= tool-name "read_image")
        (require 'efrit-tool-read-image)
        (let ((path (gethash "path" input)))
          (efrit-log-debug "Reading image: %s" path)
          (efrit-tool-read-image `((path . ,path)))))
       
       ;; Handle create_buffer tool call
       ((string= tool-name "create_buffer")
        (let* ((name (gethash "name" input))
               (content (gethash "content" input ""))
               (mode (gethash "mode" input nil))
               (read-only (gethash "read-only" input nil))
               (args `((name . ,name)
                       ,@(when content `((content . ,content)))
                       ,@(when mode `((mode . ,mode)))
                       ,@(when read-only `((read-only . ,read-only))))))
          (efrit-log-debug "Creating buffer: %s" name)
          (efrit-tool-create-buffer args)))
       
       ;; Handle edit_buffer tool call
       ((string= tool-name "edit_buffer")
        (let* ((buffer (gethash "buffer" input))
               (text (gethash "text" input ""))
               (position (gethash "position" input "end"))
               (replace (gethash "replace" input nil))
               (from-pos (gethash "from-pos" input nil))
               (to-pos (gethash "to-pos" input nil))
               (args `((buffer . ,buffer)
                       (text . ,text)
                       ,@(when position `((position . ,(if (stringp position)
                                                            (intern position)
                                                          position))))
                       ,@(when replace `((replace . ,replace)))
                       ,@(when from-pos `((from-pos . ,from-pos)))
                       ,@(when to-pos `((to-pos . ,to-pos))))))
          (efrit-log-debug "Editing buffer: %s" buffer)
          (efrit-tool-edit-buffer args)))
       
       ;; Handle read_buffer tool call
       ((string= tool-name "read_buffer")
        (let* ((buffer (gethash "buffer" input))
               (start (gethash "start" input nil))
               (end (gethash "end" input nil))
               (args `((buffer . ,buffer)
                       ,@(when start `((start . ,start)))
                       ,@(when end `((end . ,end))))))
          (efrit-log-debug "Reading buffer: %s" buffer)
          (efrit-tool-read-buffer args)))
       
       ;; Handle buffer_info tool call
       ((string= tool-name "buffer_info")
        (let* ((buffer (gethash "buffer" input))
               (args `((buffer . ,buffer))))
          (efrit-log-debug "Getting buffer info: %s" buffer)
          (efrit-tool-buffer-info args)))
       
       ;; Not a built-in tool
       (t nil))
    (error
     (format "Error executing %s: %s" tool-name (efrit--safe-error-message err)))))

(defun efrit-chat--delegate-to-dispatcher (tool-name tool-id input)
  "Delegate TOOL-NAME execution to efrit-do dispatcher.
TOOL-ID is the tool use ID, INPUT is the input hash-table.
Returns the result string."
  (require 'efrit-do)
  (efrit-log-debug "Delegating tool '%s' to efrit-do dispatcher" tool-name)
  (condition-case err
      (let ((tool-item (make-hash-table :test 'equal)))
        (puthash "id" tool-id tool-item)
        (puthash "name" tool-name tool-item)
        (puthash "input" input tool-item)
        (efrit-do--execute-tool tool-item))
    (error
     (format "Error executing %s: %s" tool-name (efrit--safe-error-message err)))))

(provide 'efrit-chat-common)

;;; efrit-chat-common.el ends here
