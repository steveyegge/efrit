;;; efrit-chat-api.el --- Claude API communication for Efrit chat -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai
;; URL: https://github.com/steveyegge/efrit

;;; Commentary:
;; API communication functions for both classic and streamlined chat modes.
;; Handles request building, response parsing, tool execution, and multi-turn conversations.

;;; Code:

(require 'json)
(require 'url)
(require 'cl-lib)
(require 'auth-source)
(require 'efrit-tools)
(require 'efrit-config)
(require 'efrit-chat-buffer)
(require 'efrit-common)
(require 'efrit-tool-edit-buffer)
(require 'efrit-chat-transparency)

;; Declare functions from other modules to avoid warnings
(declare-function efrit-common-get-api-key "efrit-common")
(declare-function efrit-do "efrit-do" (command))
(declare-function efrit-tools-system-prompt "efrit-tools")
(declare-function efrit-tools-get-context "efrit-tools")
(declare-function efrit-common-get-api-url "efrit-common")
(declare-function efrit-common-escape-json-unicode "efrit-common")
(declare-function efrit-tool-read-image "efrit-tool-read-image")
(declare-function efrit-tools-eval-sexp "efrit-tools")
(declare-function efrit-tool-create-buffer "efrit-tool-edit-buffer")
(declare-function efrit-tool-edit-buffer "efrit-tool-edit-buffer")
(declare-function efrit-tool-read-buffer "efrit-tool-edit-buffer")
(declare-function efrit-tool-buffer-info "efrit-tool-edit-buffer")
(declare-function efrit-unified-context-add-message "efrit-session")
(declare-function efrit-chat--on-message-sent "efrit-chat-persistence")

;; Declare variables from other modules
(defvar efrit-model nil "Model to use for API calls. When nil, uses efrit-default-model from efrit-config")
(defvar efrit-max-tokens 4096 "Maximum tokens in API response")
(defvar efrit-default-model nil "Default model for API calls")

;;; Internal variables - Classic Chat Mode

(defvar-local efrit--message-history nil
  "History of message exchanges with the API.")

(defvar-local efrit--current-conversation nil
  "Current multi-turn conversation state, if active.")

(defvar-local efrit--last-user-message nil
  "Last message sent by the user, for retry purposes.")

;;; Internal variables - Streamlined Mode

(defvar efrit-streamlined--current-messages nil
  "Messages for the current conversation turn.")

(defvar efrit-streamlined--turn-count 0
  "Current turn count for the conversation.")

;;; Customization

(defcustom efrit-temperature 0.1
  "Temperature setting for response generation (0.0-1.0)."
  :type 'float
  :group 'efrit)

(defcustom efrit-enable-tools t
  "Whether to enable tools for the assistant."
  :type 'boolean
  :group 'efrit)

(defcustom efrit-max-retries 3
  "Maximum number of retry attempts when tool execution fails in chat mode."
  :type 'integer
  :group 'efrit)

(defcustom efrit-retry-on-errors t
  "Whether to automatically retry failed tool executions in chat mode."
  :type 'boolean
  :group 'efrit)

(defcustom efrit-custom-headers nil
  "Alist of custom headers to add to API requests.
Each element should be a cons cell of (HEADER-NAME . HEADER-VALUE).
Example: \\='((\"authorization\" . \"Bearer your-token\")
             (\"custom-header\" . \"custom-value\"))"
  :type '(alist :key-type string :value-type string)
  :group 'efrit)

(defcustom efrit-excluded-headers nil
  "List of default header names to exclude from API requests.
Example: \\='(\"anthropic-version\" \"anthropic-beta\")"
  :type '(repeat string)
  :group 'efrit)

;; efrit-api-url is defined in efrit-common.el (legacy, deprecated)

(defcustom efrit-max-turns 2
  "Maximum number of turns in a conversation for streamlined interface."
  :type 'integer
  :group 'efrit)

;;; Helper Functions

(defun efrit--get-api-key ()
  "Get the Anthropic API key.
Supports multiple sources: environment variable, authinfo, or config file."
  (require 'efrit-common)
  (efrit-common-get-api-key))

(defun efrit--build-tool-result (tool-id result)
  "Build a tool_result content block for TOOL-ID with RESULT.
Returns an alist in the format required by the Anthropic API.

RESULT can be:
- A string: returned as simple text content
- An alist with an `image' key: returned as an image content block
- Anything else: converted to string via `format'

For image results, the content is an array containing the image block:
  ((type . \"tool_result\")
   (tool_use_id . TOOL-ID)
   (content . [((type . \"image\") (source . ...))]))

For text results:
  ((type . \"tool_result\")
   (tool_use_id . TOOL-ID)
   (content . RESULT-STRING))"
  (let ((content
         (cond
          ;; Check for image response format from efrit-tool-read-image
          ((and (listp result)
                (alist-get 'image result))
           ;; Return as array containing the image block
           (vector (alist-get 'image result)))
          ;; String result - use as-is
          ((stringp result)
           result)
          ;; Everything else - convert to string
          (t
           (format "%s" result)))))
    `((type . "tool_result")
      (tool_use_id . ,tool-id)
      (content . ,content))))

;;; Header customization

(defun efrit--build-headers (api-key)
  "Build HTTP headers for API requests, respecting customization options."
  (let ((default-headers `(("x-api-key" . ,api-key)
                          ("anthropic-version" . "2023-06-01")
                          ("anthropic-beta" . "max-tokens-3-5-sonnet-2024-07-15")
                          ("content-type" . "application/json"))))
    ;; Remove excluded headers
    (when efrit-excluded-headers
      (setq default-headers
            (cl-remove-if (lambda (header)
                           (member (car header) efrit-excluded-headers))
                         default-headers)))
    ;; Add custom headers (custom headers override defaults)
    (append efrit-custom-headers default-headers)))

;;; System Prompts

(defun efrit-streamlined--system-prompt ()
  "Generate enhanced system prompt for streamlined chat experience."
  (concat
   (if (fboundp 'efrit-tools-system-prompt)
       (efrit-tools-system-prompt)
     "You are Efrit, an Emacs assistant.")
   "\n\n### Response Guidelines\n"
   "Determine whether the user's request is primarily an ACTION (do something) or INFORMATION (explain/show something):\n\n"

   "**For ACTION requests** (write, create, make, add, fix, etc.):\n"
   "- Execute the requested action using Elisp code\n"
   "- Provide minimal chat output (e.g., \"Done.\" or \"Created haiku in *vim-haiku* buffer.\")\n"
   "- Complete simple tasks in a single turn\n"
   "- Don't break straightforward operations into multiple steps\n\n"

   "**For INFORMATION requests** (how, what, explain, show, etc.):\n"
   "- Provide comprehensive, well-formatted responses\n"
   "- Include examples, explanations, and details as appropriate\n"
   "- Structure information clearly with headers and formatting\n\n"

   "### Single vs Multi-turn\n"
   "- Complete simple requests in ONE turn whenever possible\n"
   "- Only use multiple turns for genuinely complex multi-step operations\n"
   "- Don't artificially break down straightforward tasks\n"))

;;; API functions - Classic Chat Mode

(defun efrit--send-api-request (messages)
  "Send MESSAGES to the Claude API and handle the response.
In batch mode (non-interactive), uses synchronous request with timeout.
In interactive mode, uses async request with callbacks."
  (let* ((api-key (efrit--get-api-key))
         (url-request-method "POST")
         (url-request-extra-headers (efrit--build-headers api-key))
         (system-prompt (when efrit-enable-tools (efrit-tools-system-prompt)))
         (model (or efrit-model efrit-default-model))
         (request-data
         `(("model" . ,model)
         ("max_tokens" . ,efrit-max-tokens)
         ("temperature" . ,efrit-temperature)
         ,@(when system-prompt
         `(("system" . ,system-prompt)))
         ("messages" . ,(vconcat
         ;; Add the conversation history
         (mapcar (lambda (msg)
               `(("role" . ,(alist-get 'role msg))
                      ("content" . ,(alist-get 'content msg))))
                  messages)))
              ,@(when efrit-enable-tools
                  '(("tools" . [
                                   ;; Primary tool: Elisp evaluation
                                   (("name" . "eval_sexp")
                                   ("description" . "Evaluate a Lisp expression and return the result. This is the primary tool for interacting with Emacs.")
                                   ("input_schema" . (("type" . "object")
                                                       ("properties" . (("expr" . (("type" . "string")
                                                                                   ("description" . "The Elisp expression to evaluate")))))
                                                       ("required" . ["expr"]))))

                                   ;; Context gathering
                                   (("name" . "get_context")
                                   ("description" . "Get comprehensive context information about the Emacs environment")
                                   ("input_schema" . (("type" . "object")
                                                       ("properties" . (("request" . (("type" . "string")
                                                                                      ("description" . "Optional context request")))))
                                                       ("required" . []))))

                                   ;; Path resolution (useful helper)
                                   (("name" . "resolve_path")
                                   ("description" . "Resolve a path from natural language description")
                                   ("input_schema" . (("type" . "object")
                                                       ("properties" . (("path_description" . (("type" . "string")
                                                                                              ("description" . "Natural language path description")))))
                                                       ("required" . ["path_description"]))))

                                   ;; Image reading - THIS IS HOW CLAUDE SEES IMAGES
                                   (("name" . "read_image")
                                   ("description" . "View an image file so that YOU (Claude) can see its visual contents. This is the ONLY way you can see images - opening a file in Emacs with find-file does NOT give you vision access, it only displays the image to the user. When a user asks you to look at, describe, analyze, or examine an image, you MUST use this tool. Supports PNG, JPEG, GIF, and WebP formats.")
                                   ("input_schema" . (("type" . "object")
                                                       ("properties" . (("path" . (("type" . "string")
                                                                                   ("description" . "Path to the image file")))))
                                                       ("required" . ["path"]))))

                                   ;; Buffer creation and editing tools
                                   (("name" . "create_buffer")
                                   ("description" . "Create a new buffer with optional initial content and major mode. High-level tool for common buffer creation tasks.")
                                   ("input_schema" . (("type" . "object")
                                                       ("properties" . (("name" . (("type" . "string")
                                                                                   ("description" . "Buffer name (required)")))
                                                                       ("content" . (("type" . "string")
                                                                                    ("description" . "Initial content to insert (optional)")))
                                                                       ("mode" . (("type" . "string")
                                                                                 ("description" . "Major mode to enable, e.g. 'org-mode' or 'markdown-mode' (optional)")))
                                                                       ("read-only" . (("type" . "boolean")
                                                                                      ("description" . "Whether buffer should be read-only (optional, default false)")))))
                                                       ("required" . ["name"]))))

                                   (("name" . "edit_buffer")
                                   ("description" . "Insert or replace text in an existing buffer. High-level tool for buffer editing without needing complex Elisp.")
                                   ("input_schema" . (("type" . "object")
                                                       ("properties" . (("buffer" . (("type" . "string")
                                                                                     ("description" . "Buffer name or buffer object (required)")))
                                                                       ("text" . (("type" . "string")
                                                                                 ("description" . "Text to insert or replace (required)")))
                                                                       ("position" . (("type" . "string")
                                                                                     ("description" . "Where to insert ('start', 'end', 'point', or line number, default 'end')")))
                                                                       ("replace" . (("type" . "boolean")
                                                                                    ("description" . "If true, replace region between from-pos and to-pos (optional)")))
                                                                       ("from-pos" . (("type" . "integer")
                                                                                     ("description" . "Start position for replacement (required if replace=true)")))
                                                                       ("to-pos" . (("type" . "integer")
                                                                                   ("description" . "End position for replacement (required if replace=true)")))))
                                                       ("required" . ["buffer" "text"]))))

                                   (("name" . "read_buffer")
                                   ("description" . "Read contents of a buffer, optionally between specific positions.")
                                   ("input_schema" . (("type" . "object")
                                                       ("properties" . (("buffer" . (("type" . "string")
                                                                                     ("description" . "Buffer name or buffer object (required)")))
                                                                       ("start" . (("type" . "integer")
                                                                                  ("description" . "Start position (optional, default beginning)")))
                                                                       ("end" . (("type" . "integer")
                                                                                ("description" . "End position (optional, default end)")))))
                                                       ("required" . ["buffer"]))))

                                   (("name" . "buffer_info")
                                   ("description" . "Get information about a buffer (name, size, mode, modified status, etc.)")
                                   ("input_schema" . (("type" . "object")
                                                       ("properties" . (("buffer" . (("type" . "string")
                                                                                     ("description" . "Buffer name or buffer object (required)")))))
                                                       ("required" . ["buffer"]))))

                                   ])))
                                   ))
         (json-string (json-encode request-data))
         ;; Convert unicode characters to JSON escape sequences to prevent multibyte HTTP errors
         (escaped-json (efrit-common-escape-json-unicode json-string))
         (url-request-data (encode-coding-string escaped-json 'utf-8)))
     ;; In batch mode, use synchronous request (async callbacks don't work)
     ;; In interactive mode, use async request
     (if noninteractive
         ;; Batch mode: synchronous request with immediate response handling
         (condition-case err
             (let ((response-buffer (url-retrieve-synchronously (or efrit-api-url (efrit-common-get-api-url)) t t 30)))
               (if response-buffer
                   (unwind-protect
                       (with-current-buffer response-buffer
                         ;; The status is passed as nil for sync requests when successful
                         ;; Simulate the url-retrieve callback with success status
                         (efrit--handle-api-response nil))
                     ;; Clean up the response buffer after handling
                     (when (buffer-live-p response-buffer)
                       (kill-buffer response-buffer)))
                 (with-current-buffer (efrit--setup-buffer)
                   (setq buffer-read-only nil)
                   (let ((inhibit-read-only t))
                     (setq-local efrit--response-in-progress nil)
                     (efrit--display-message "Connection timeout: Could not reach API" 'system)
                     (efrit--insert-prompt)))))
           (error
            (with-current-buffer (efrit--setup-buffer)
              (setq buffer-read-only nil)
              (let ((inhibit-read-only t))
                (setq-local efrit--response-in-progress nil)
                (efrit--display-message 
                 (format "API connection error: %s" (error-message-string err))
                 'system)
                (efrit--insert-prompt)))))
       ;; Interactive mode: async request with callback
       (url-retrieve (or efrit-api-url (efrit-common-get-api-url)) 'efrit--handle-api-response nil t t))))

(defun efrit--parse-api-response ()
  "Parse JSON response from current buffer and return content, or signal API error.
Returns the content hash-table from the API response.
Throws an error if the response contains an API error object."
  (goto-char (point-min))
  (when (search-forward-regexp "^$" nil t)
    (let* ((json-object-type 'hash-table)
           (json-array-type 'vector)
           (json-key-type 'string)
           ;; Ensure proper UTF-8 decoding
           (coding-system-for-read 'utf-8)
           (raw-response (decode-coding-region (point) (point-max) 'utf-8 t)))
      (condition-case parse-err
          (let ((response (json-read-from-string raw-response)))
            ;; Check if the API returned an error object
            (if-let* ((error-obj (gethash "error" response)))
                (let ((error-type (gethash "type" error-obj))
                      (error-msg (gethash "message" error-obj)))
                  (efrit-log-error "API error response: type=%s message=%s" error-type error-msg)
                  ;; Throw an error so it gets caught and displayed properly
                  (error "API Error (%s): %s" error-type error-msg))
              ;; Success - return the content
              (gethash "content" response)))
        (error
         (efrit-log-error "Failed to parse API response JSON: %s" parse-err)
         (efrit-log-debug "Raw response was: %s" (substring raw-response 0 (min 500 (length raw-response))))
         (error "Failed to parse API response: %s" (error-message-string parse-err)))))))


(defun efrit--process-http-status (status)
  "Process HTTP status and return non-nil if there's an error.
If there's an error, handle it and clean up the buffer."
  (when (plist-get status :error)
    (efrit--handle-http-error (plist-get status :error))
    ;; Clean up response buffer
    (when (buffer-live-p (current-buffer))
      (kill-buffer (current-buffer)))
    t)) ; Return t to indicate error was handled

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

(defun efrit--handle-http-error (error-details)
  "Handle HTTP error, providing classification and guidance.
ERROR-DETAILS contains the error information."
  (require 'efrit-log)
  (efrit-log-error "[efrit-chat] API Error: %s" error-details)
  
  (let ((error-info (efrit--classify-error error-details)))
    (let ((error-type (nth 0 error-info))
          (error-description (nth 1 error-info))
          (recommendation (nth 2 error-info)))
      
      (efrit-log-error "[efrit-chat] Error type: %s" error-type)
      
      (with-current-buffer (efrit--setup-buffer)
        (setq buffer-read-only nil)
        (let ((inhibit-read-only t))
          ;; Remove "thinking" indicator if present
          (when efrit--response-in-progress
            (save-excursion
              (goto-char (point-max))
              (when (search-backward "System: Thinking..." nil t)
                (let ((start (match-beginning 0)))
                  (when (search-forward "Thinking..." nil t)
                    (delete-region start (point)))))))

          ;; Clear in-progress flag
          (setq-local efrit--response-in-progress nil)

          ;; Display detailed error message with classification and recommendations
          (efrit--display-message
           (concat
            (format "⚠️ %s (%s)\n" error-description error-type)
            (format "\nDetails: %s\n" error-details)
            (format "\nWhat to do: %s\n" recommendation)
            "\nOptions: M-x efrit-retry-last-message (retry)  |  M-x efrit-chat-clear (start fresh)")
           'system)

          ;; Insert prompt for next message
          (efrit--insert-prompt))))))


(defun efrit--detect-incomplete-task (_content _message-text)
  "Detect if CONTENT represents an incomplete multi-step task.
Returns non-nil if the task appears incomplete and needs delegation.

DEPRECATED: This function violated the Pure Executor principle by using
pattern matching to make decisions. Multi-turn completion should be
handled by asking Claude explicitly via efrit-multi-turn.el, not by
parsing response text. Currently disabled - always returns nil."
  ;; Always return nil - let Claude make decisions explicitly
  nil)

(defun efrit--extract-content-and-tools (content)
  "Extract text content and execute tool calls from CONTENT array.
Returns (list message-text tool-results should-delegate) where tool-results
is a list of tool_result blocks for sending back to Claude."
  (let ((message-text "")
        (tool-results '())
        (should-delegate nil))
    (efrit-log-debug "Processing content with %d items" (length content))
    (when content
      (dotimes (i (length content))
        (let* ((item (aref content i))
               (type (gethash "type" item)))

          ;; Handle thinking content (from extended thinking models)
          (when (string= type "thinking")
            (let ((thinking-text (gethash "thinking" item)))
              (when thinking-text
                (efrit-log-debug "Processing thinking block: %d chars" (length thinking-text))
                (efrit-transparency--display-thinking thinking-text))))

          ;; Handle text content
           (when (string= type "text")
            (let ((text (gethash "text" item)))
              (when text

                (setq message-text (concat message-text text)))))

          ;; Handle tool calls
           (when (string= type "tool_use")
             (let* ((tool-name (gethash "name" item))
                    (tool-id (gethash "id" item))  ; Captured for use in tool_result messages (ef-5af)
                    (input (gethash "input" item)))

               ;; Display the tool call for transparency
               (efrit-transparency--display-tool-call tool-name input)

               ;; Execute tool call
               (let ((result (condition-case tool-err
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
                                  (let* ((path (gethash "path" input))
                                         (args `((path . ,path))))
                                    (efrit-log-debug "Reading image: %s" path)
                                    (efrit-tool-read-image args)))
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
                                 ;; Unknown tool
                                 (t
                                  (format "Error: Unknown tool '%s'" tool-name)))
                              (error
                               (format "Error executing tool %s: %s"
                                      tool-name (if tool-err
                                                   (efrit--safe-error-message tool-err)
                                                 "Unknown error"))))))

                ;; Display the tool result for transparency
                 (efrit-transparency--display-tool-result tool-name result)

                 ;; Collect ALL tool results for sending back to Claude
                 (push (efrit--build-tool-result tool-id result) tool-results)

                 ;; In chat mode, don't display tool results inline
                 ;; Only show errors (buffer objects and nil results are suppressed)
                 ;; For image results (alists), just skip display
                 (when (and (stringp result)
                            (string-match-p "^Error:" result))
                   (setq message-text (concat message-text "\n" result))))))))

      ;; Check if we should delegate to efrit-do
      (when (efrit--detect-incomplete-task content message-text)
        (efrit-log-debug "Detected incomplete task - will delegate")
        (setq should-delegate t)))

    ;; Return message text, tool results, and delegation flag
    (list message-text (nreverse tool-results) should-delegate)))

(defun efrit--update-ui-with-response (message-text)
  "Update the UI with MESSAGE-TEXT response."
  (with-current-buffer (efrit--setup-buffer)
    (setq buffer-read-only nil)
    (let ((inhibit-read-only t))
      ;; Remove "thinking" indicator (but only if it's actually at the end)
      (when efrit--response-in-progress
        (save-excursion
          (goto-char (point-max))
          (when (search-backward "System: Thinking..." nil t)
            (let ((start (match-beginning 0)))
              ;; Only delete if this is at the very end of the buffer to avoid deleting user content
              (when (and (search-forward "Thinking..." nil t)
                        (= (point) (point-max)))
                (delete-region start (point-max)))))))

      ;; Clear progress flag
      (setq-local efrit--response-in-progress nil)

      ;; Process tool calls and display response text
      (when (and message-text (not (string-empty-p message-text)))
        (if efrit-enable-tools
            ;; Tools already executed in efrit--extract-content-and-tools, just display the result
            (condition-case-unless-debug process-err
                (let ((highlighted-text (efrit--sanitize-chat-text message-text)))
                  ;; Display the message with incremental/transparency features
                  (efrit-transparency--display-incremental highlighted-text)

                  ;; Add to conversation history
                  (push `((role . "assistant")
                         (content . ,message-text))
                        efrit--message-history)
                  ;; Also add to unified context
                  (require 'efrit-session)
                  (efrit-unified-context-add-message 'assistant message-text 'chat)
                  ;; Auto-save session
                  (require 'efrit-chat-persistence)
                  (efrit-chat--on-message-sent))
              (error
               ;; Fallback to displaying the raw message if processing fails
               (message "Error processing response: %s"
                       (efrit--safe-error-message process-err))
               (efrit-transparency--display-incremental
                (concat message-text
                       "\n\n[Error processing response: "
                       (efrit--safe-error-message process-err) "]"))

               ;; Add original message to history
               (push `((role . "assistant")
                      (content . ,message-text))
                     efrit--message-history)
               ;; Also add to unified context
               (require 'efrit-session)
               (efrit-unified-context-add-message 'assistant message-text 'chat)
               ;; Auto-save session
               (require 'efrit-chat-persistence)
               (efrit-chat--on-message-sent)))

          ;; Just display the text directly if tools are disabled
          (progn
            (efrit-transparency--display-incremental message-text)

            ;; Add to conversation history
            (push `((role . "assistant")
                   (content . ,message-text))
                  efrit--message-history)
            ;; Also add to unified context
            (require 'efrit-session)
            (efrit-unified-context-add-message 'assistant message-text 'chat)
            ;; Auto-save session
            (require 'efrit-chat-persistence)
            (efrit-chat--on-message-sent))))

      ;; Don't auto-continue in chat mode - insert prompt for next user input
      (efrit--insert-prompt))))

(defun efrit--handle-parse-error ()
  "Handle parsing errors by displaying error message and resetting UI."
  (with-current-buffer (efrit--setup-buffer)
    (setq buffer-read-only nil)
    (let ((inhibit-read-only t))
      ;; Remove "thinking" indicator if present
      (when efrit--response-in-progress
        (save-excursion
          (goto-char (point-max))
          (when (search-backward "System: Thinking..." nil t)
            (let ((start (match-beginning 0)))
              (when (search-forward "Thinking..." nil t)
                (delete-region start (point)))))))

      ;; Clear progress flag
      (setq-local efrit--response-in-progress nil)

      ;; Display error message
      (efrit--display-message
       "Error parsing API response" 'system)

      ;; Insert prompt for next message
      (efrit--insert-prompt))))

(defun efrit--handle-api-response (status)
  "Handle the API response with STATUS."


  (unless (efrit--process-http-status status)
    ;; Only proceed if there was no HTTP error
    (condition-case api-err
        (let ((content (efrit--parse-api-response)))
          (if content
              ;; Execute tool processing in the chat buffer context
              (with-current-buffer (efrit--setup-buffer)
                (let* ((result (efrit--extract-content-and-tools content))
                       (message-text (nth 0 result))
                       (tool-results (nth 1 result))
                       (should-delegate (nth 2 result)))
                  (cond
                   ;; If we have tool results, send them back to Claude
                   ((and tool-results (> (length tool-results) 0))
                    (efrit-log-debug "Tool results collected: %d, sending back to Claude" (length tool-results))
                    ;; Add assistant's message with tool_use to history
                    (push `((role . "assistant")
                            (content . ,content))
                          efrit--message-history)
                    ;; Build user message with tool_result blocks
                    (push `((role . "user")
                            (content . ,(vconcat tool-results)))
                          efrit--message-history)
                    ;; Continue conversation - response will come back to this handler
                    (efrit--send-api-request (reverse efrit--message-history)))

                   ;; Delegate to efrit-do for multi-step completion
                   (should-delegate
                    (efrit-log-debug "Delegating to efrit-do")
                    (efrit--delegate-to-do))

                   ;; Normal response handling (no tools, just text)
                   (t
                    (efrit--update-ui-with-response message-text)))))
             ;; Handle case where content is nil
             (efrit--handle-parse-error)))
       ;; Handle parsing errors or API error responses
       (error
        (let ((error-msg (error-message-string api-err)))
          (if (string-match-p "^API Error" error-msg)
              ;; This is an API error response (e.g., authentication failure)
              ;; Treat it like an HTTP error with detailed classification
              (efrit--handle-http-error error-msg)
            ;; This is a JSON parsing error
            (efrit--handle-parse-error)))))
    ;; Note: Let url-retrieve handle its own buffer cleanup
    ;; Manual cleanup here causes format-message(nil) errors
    ))

(defun efrit--delegate-to-do ()
  "Delegate current request to efrit-do for multi-step completion."
  (let ((original-request (when efrit--message-history
                            (let ((first-msg (car (last efrit--message-history))))
                              (when (eq (alist-get 'role first-msg) 'user)
                                (alist-get 'content first-msg))))))
    (when original-request
      ;; Update chat UI to indicate delegation
      (with-current-buffer (efrit--setup-buffer)
        (setq buffer-read-only nil)
        (let ((inhibit-read-only t))
          ;; Remove "thinking" indicator
          (when efrit--response-in-progress
            (save-excursion
              (goto-char (point-max))
              (when (search-backward "System: Thinking..." nil t)
                (let ((start (match-beginning 0)))
                  (when (search-forward "Thinking..." nil t)
                    (delete-region start (point)))))))

          ;; Clear in-progress flag
          (setq-local efrit--response-in-progress nil)

          ;; Show delegation message
          (efrit--display-message
           "[Delegating to efrit-do for multi-step task completion...]"
           'system)

          ;; Insert prompt and let user know
          (efrit--insert-prompt)

          ;; Execute via efrit-do asynchronously
          (run-at-time 0.1 nil
                       (lambda ()
                         (require 'efrit-do)
                         (message "Executing via efrit-do: %s" original-request)
                         (efrit-do original-request))))))))

;;; API functions - Streamlined Mode

(defun efrit-streamlined--send-request (messages)
  "Send MESSAGES with enhanced streamlined prompt."
  (efrit-streamlined--log-to-work
   (format "Sending request with %d messages" (length messages)))

  ;; Use enhanced system prompt
  (let* ((api-key (efrit--get-api-key))
         (url-request-method "POST")
         (url-request-extra-headers (efrit--build-headers api-key))
         (system-prompt (efrit-streamlined--system-prompt))
         (cleaned-messages (mapcar (lambda (msg)
                                     (let ((content (alist-get 'content msg)))
                                       `(("role" . ,(alist-get 'role msg))
                                         ("content" . ,(if (stringp content)
                                                           (substring-no-properties content)
                                                         content)))))
                                   messages))
         (model (or efrit-model (require 'efrit-config) efrit-default-model))
         (request-data
          `(("model" . ,model)
            ("max_tokens" . ,efrit-max-tokens)
            ("temperature" . ,efrit-temperature)
            ("system" . ,system-prompt)
            ("messages" . ,(vconcat cleaned-messages))
            ,@(when efrit-enable-tools
                '(("tools" . [
                              (("name" . "eval_sexp")
                               ("description" . "Evaluate Elisp expression")
                               ("input_schema" . (("type" . "object")
                                                  ("properties" . (("expr" . (("type" . "string")
                                                                              ("description" . "Elisp expression")))))
                                                  ("required" . ["expr"]))))
                              (("name" . "get_context")
                               ("description" . "Get Emacs environment context")
                               ("input_schema" . (("type" . "object")
                                                  ("properties" . (("request" . (("type" . "string")
                                                                                 ("description" . "Context request")))))
                                                  ("required" . []))))
                              (("name" . "read_image")
                               ("description" . "View an image file so that YOU (Claude) can see its visual contents. This is the ONLY way you can see images - opening a file in Emacs does NOT give you vision access. When asked to look at, describe, or analyze an image, you MUST use this tool. Supports PNG, JPEG, GIF, WebP.")
                               ("input_schema" . (("type" . "object")
                                                  ("properties" . (("path" . (("type" . "string")
                                                                              ("description" . "Path to the image file")))))
                                                  ("required" . ["path"]))))
                              
                              (("name" . "create_buffer")
                               ("description" . "Create a new buffer with optional initial content and major mode.")
                               ("input_schema" . (("type" . "object")
                                                  ("properties" . (("name" . (("type" . "string")
                                                                              ("description" . "Buffer name (required)")))
                                                                  ("content" . (("type" . "string")
                                                                               ("description" . "Initial content (optional)")))
                                                                  ("mode" . (("type" . "string")
                                                                            ("description" . "Major mode like 'org-mode' (optional)")))
                                                                  ("read-only" . (("type" . "boolean")
                                                                                 ("description" . "Read-only flag (optional)")))))
                                                  ("required" . ["name"]))))
                              
                              (("name" . "edit_buffer")
                               ("description" . "Insert or replace text in an existing buffer.")
                               ("input_schema" . (("type" . "object")
                                                  ("properties" . (("buffer" . (("type" . "string")
                                                                                ("description" . "Buffer name (required)")))
                                                                  ("text" . (("type" . "string")
                                                                            ("description" . "Text to insert (required)")))
                                                                  ("position" . (("type" . "string")
                                                                                ("description" . "'start', 'end', 'point', or line number")))
                                                                  ("replace" . (("type" . "boolean")
                                                                               ("description" . "Replace mode (optional)")))
                                                                  ("from-pos" . (("type" . "integer")
                                                                                ("description" . "Start of replacement (if replace=true)")))
                                                                  ("to-pos" . (("type" . "integer")
                                                                              ("description" . "End of replacement (if replace=true)")))))
                                                  ("required" . ["buffer" "text"]))))
                              
                              (("name" . "read_buffer")
                               ("description" . "Read contents of a buffer.")
                               ("input_schema" . (("type" . "object")
                                                  ("properties" . (("buffer" . (("type" . "string")
                                                                                ("description" . "Buffer name (required)")))
                                                                  ("start" . (("type" . "integer")
                                                                             ("description" . "Start position (optional)")))
                                                                  ("end" . (("type" . "integer")
                                                                           ("description" . "End position (optional)")))))
                                                  ("required" . ["buffer"]))))
                              
                              (("name" . "buffer_info")
                               ("description" . "Get information about a buffer.")
                               ("input_schema" . (("type" . "object")
                                                  ("properties" . (("buffer" . (("type" . "string")
                                                                                ("description" . "Buffer name (required)")))))
                                                  ("required" . ["buffer"]))))
                              ])))
            ))
            (json-string (json-encode request-data))
            ;; Convert unicode characters to JSON escape sequences to prevent multibyte HTTP errors
            (escaped-json (efrit-common-escape-json-unicode json-string))
            (url-request-data (encode-coding-string escaped-json 'utf-8)))

            ;; Log request details to work buffer
    (efrit-streamlined--log-to-work
     (format "Request: %s characters, %s tools enabled"
             (length url-request-data)
             (if efrit-enable-tools "tools" "no tools")))

    ;; Send request
    (url-retrieve (or efrit-api-url (efrit-common-get-api-url)) 'efrit-streamlined--handle-response nil t t)))

(defun efrit-streamlined--handle-response (_status)
  "Handle API response with work buffer logging."
  (efrit-streamlined--log-to-work "Received response")

  ;; Parse response and handle it
  (goto-char (point-min))
  (search-forward "\n\n")
  (let* ((response-text (buffer-substring-no-properties (point) (point-max)))
         (response-data (condition-case err
                           (json-read-from-string response-text)
                         (error
                          (efrit-streamlined--log-to-work
                           (format "JSON parse error: %s" err))
                          nil))))

    (if (not response-data)
        (efrit-streamlined--log-to-work "Failed to parse API response")

      ;; Check for API errors
      (if-let* ((error-obj (alist-get 'error response-data)))
          (let ((error-type (alist-get 'type error-obj))
                (error-message (alist-get 'message error-obj)))
            (efrit-streamlined--log-to-work
             (format "API ERROR: %s - %s" error-type error-message))
            (message "Efrit: API Error - %s" error-message)
            (efrit-streamlined--display-response
             (format "API Error (%s): %s" error-type error-message)))

        ;; Extract content
        (let* ((content-array (alist-get 'content response-data))
               (text-content "")
               (tool-uses (efrit-streamlined--extract-tool-uses response-data))
               (has-text-content nil))

          ;; Collect all text content
          (when (and content-array (vectorp content-array))
            (dotimes (i (length content-array))
              (let* ((content-item (aref content-array i))
                     (type (alist-get 'type content-item)))
                (when (string-equal type "text")
                  (when-let* ((text (alist-get 'text content-item)))
                    (setq text-content (concat text-content text))
                    (setq has-text-content t))))))

          ;; Log to work buffer
          (efrit-streamlined--log-to-work
           (format "Response: %s chars text, %d tools"
                   (length text-content)
                   (length tool-uses)))

          ;; Execute tools and collect results
          (let ((tool-results '()))
            (when tool-uses
              (setq tool-results (efrit-streamlined--execute-tools tool-uses)))

            ;; Check if Claude wants to continue
            (let* ((at-turn-limit (>= efrit-streamlined--turn-count efrit-max-turns))
            (claude-signals-done (or (string-match-p "\\bdone\\b\\|\\bcomplete\\b\\|\\bfinished\\b\\|\\bfinal\\b"
                                                         (downcase (or text-content "")))
                                         (and tool-uses (not has-text-content))))
                 (should-continue (and tool-uses
                                      (not claude-signals-done)
                                     (not at-turn-limit))))

            (efrit-streamlined--log-to-work
            (format "Turn %d/%d: %s"
            efrit-streamlined--turn-count efrit-max-turns
            (if should-continue "continuing" "finishing")))

               (when at-turn-limit
                 (message "Efrit: Turn limit reached (%d)" efrit-max-turns))

              (if should-continue
                  ;; Continue conversation with tool results
                  (progn
                    (setq efrit-streamlined--turn-count (1+ efrit-streamlined--turn-count))
                    (efrit-streamlined--log-to-work "Continuing conversation with tool results")
                    (efrit-streamlined--continue-with-results tool-results content-array))

                ;; Final response - display it
                (progn
                  (message "Efrit: Complete")
                  (when has-text-content
                    (efrit-streamlined--display-response text-content)))))))))))

(defun efrit-streamlined--continue-with-results (tool-results assistant-content)
  "Continue conversation with TOOL-RESULTS from executed tools.
TOOL-RESULTS is a list of tool_result blocks from `efrit--build-tool-result'.
ASSISTANT-CONTENT is the original content array from the assistant response."
  (let ((updated-messages efrit-streamlined--current-messages))

    ;; CRITICAL: Add messages in correct chronological order!
    ;; Messages list should be: [user, assistant-with-tool_use, user-with-tool_result]
    ;; The API requires tool_result to immediately follow a message with tool_use

    ;; First: Add assistant's response WITH tool_use blocks
    (when assistant-content
      (setq updated-messages
            (append updated-messages
                    (list `((role . "assistant")
                            (content . ,assistant-content))))))

    ;; Second: Add user message with tool_result blocks
    (when (and tool-results (> (length tool-results) 0))
      (setq updated-messages
            (append updated-messages
                    (list `((role . "user")
                            (content . ,(vconcat tool-results)))))))

    ;; Continue conversation with results
    (when (> (length tool-results) 0)
      (setq efrit-streamlined--current-messages updated-messages)
      (efrit-streamlined--send-request updated-messages))))

(defun efrit-streamlined--extract-tool-uses (response-data)
  "Extract tool_use blocks from RESPONSE-DATA."
  (condition-case err
      (let* ((content-array (alist-get 'content response-data))
             (tool-uses '()))
        (when (and content-array (vectorp content-array))
          (dotimes (i (length content-array))
            (let* ((content-item (aref content-array i))
                   (type (alist-get 'type content-item)))
              (when (and type (string-equal type "tool_use"))
                (push content-item tool-uses)))))
        (nreverse tool-uses))
    (error
     (efrit-streamlined--log-to-work
      (format "Tool extraction error: %s" (error-message-string err)))
     '())))

(defun efrit-streamlined--execute-tools (tool-uses)
  "Execute TOOL-USES and return list of tool_result blocks for continuation."
  (efrit-streamlined--log-to-work
   (format "Executing %d tools" (length tool-uses)))

  (let ((results '()))
    (dolist (tool-use tool-uses)
      (let* ((tool-name (alist-get 'name tool-use))
             (tool-input (alist-get 'input tool-use))
             (tool-id (alist-get 'id tool-use)))

        (efrit-streamlined--log-to-work
         (format "Tool: %s (id: %s)" tool-name tool-id))

        (let ((result-content
               (cond
                ((string-equal tool-name "eval_sexp")
                 (let ((expr (alist-get 'expr tool-input)))
                   (if expr
                       (progn
                         (efrit-streamlined--log-to-work (format "Evaluating: %s" expr))
                         (condition-case err
                             (let ((result (eval (read expr))))
                               (let ((result-str (if (stringp result)
                                                     (substring-no-properties result)
                                                   (prin1-to-string result))))
                                 (efrit-streamlined--log-to-work
                                  (format "Result: %s" (substring result-str 0 (min 200 (length result-str)))))
                                 result-str))
                           (error
                            (let ((error-msg (error-message-string err)))
                              (efrit-streamlined--log-to-work
                               (format "Error: %s" error-msg))
                              (format "Error: %s" error-msg)))))
                     "Error: No expression provided")))

                ((string-equal tool-name "get_context")
                 (efrit-streamlined--log-to-work "Getting context...")
                 (let ((context (if (fboundp 'efrit-tools-get-context)
                                    (efrit-tools-get-context)
                                  "Context not available - efrit-tools not loaded")))
                   (efrit-streamlined--log-to-work
                    (format "Context: %d chars" (length context)))
                   context))

                ((string-equal tool-name "read_image")
                 (require 'efrit-tool-read-image)
                 (let ((path (alist-get 'path tool-input)))
                   (efrit-streamlined--log-to-work (format "Reading image: %s" path))
                   (condition-case err
                       (let ((result (efrit-tool-read-image `((path . ,path)))))
                         (efrit-streamlined--log-to-work "Image read successfully")
                         result)
                     (error
                      (let ((error-msg (error-message-string err)))
                        (efrit-streamlined--log-to-work (format "Error: %s" error-msg))
                        (format "Error reading image: %s" error-msg))))))

                ((string-equal tool-name "create_buffer")
                 (let ((name (alist-get 'name tool-input))
                       (content (alist-get 'content tool-input ""))
                       (mode (alist-get 'mode tool-input nil))
                       (read-only (alist-get 'read-only tool-input nil)))
                   (efrit-streamlined--log-to-work (format "Creating buffer: %s" name))
                   (condition-case err
                       (let ((result (efrit-tool-create-buffer
                                     `((name . ,name)
                                       ,@(when content `((content . ,content)))
                                       ,@(when mode `((mode . ,mode)))
                                       ,@(when read-only `((read-only . ,read-only)))))))
                         (efrit-streamlined--log-to-work "Buffer created successfully")
                         result)
                     (error
                      (let ((error-msg (error-message-string err)))
                        (efrit-streamlined--log-to-work (format "Error creating buffer: %s" error-msg))
                        (format "Error creating buffer: %s" error-msg))))))

                ((string-equal tool-name "edit_buffer")
                 (let ((buffer (alist-get 'buffer tool-input))
                       (text (alist-get 'text tool-input ""))
                       (position (alist-get 'position tool-input "end"))
                       (replace (alist-get 'replace tool-input nil))
                       (from-pos (alist-get 'from-pos tool-input nil))
                       (to-pos (alist-get 'to-pos tool-input nil)))
                   (efrit-streamlined--log-to-work (format "Editing buffer: %s" buffer))
                   (condition-case err
                       (let ((result (efrit-tool-edit-buffer
                                     `((buffer . ,buffer)
                                       (text . ,text)
                                       ,@(when position `((position . ,(if (stringp position)
                                                                           (intern position)
                                                                         position))))
                                       ,@(when replace `((replace . ,replace)))
                                       ,@(when from-pos `((from-pos . ,from-pos)))
                                       ,@(when to-pos `((to-pos . ,to-pos)))))))
                         (efrit-streamlined--log-to-work "Buffer edited successfully")
                         result)
                     (error
                      (let ((error-msg (error-message-string err)))
                        (efrit-streamlined--log-to-work (format "Error editing buffer: %s" error-msg))
                        (format "Error editing buffer: %s" error-msg))))))

                ((string-equal tool-name "read_buffer")
                 (let ((buffer (alist-get 'buffer tool-input))
                       (start (alist-get 'start tool-input nil))
                       (end (alist-get 'end tool-input nil)))
                   (efrit-streamlined--log-to-work (format "Reading buffer: %s" buffer))
                   (condition-case err
                       (let ((result (efrit-tool-read-buffer
                                     `((buffer . ,buffer)
                                       ,@(when start `((start . ,start)))
                                       ,@(when end `((end . ,end)))))))
                         (efrit-streamlined--log-to-work (format "Buffer read: %d chars" (length result)))
                         result)
                     (error
                      (let ((error-msg (error-message-string err)))
                        (efrit-streamlined--log-to-work (format "Error reading buffer: %s" error-msg))
                        (format "Error reading buffer: %s" error-msg))))))

                ((string-equal tool-name "buffer_info")
                 (let ((buffer (alist-get 'buffer tool-input)))
                   (efrit-streamlined--log-to-work (format "Getting buffer info: %s" buffer))
                   (condition-case err
                       (let ((result (efrit-tool-buffer-info
                                     `((buffer . ,buffer)))))
                         (efrit-streamlined--log-to-work "Buffer info retrieved")
                         (format "%S" result))
                     (error
                      (let ((error-msg (error-message-string err)))
                        (efrit-streamlined--log-to-work (format "Error getting buffer info: %s" error-msg))
                        (format "Error getting buffer info: %s" error-msg))))))

                (t
                 (efrit-streamlined--log-to-work
                  (format "Unknown tool: %s" tool-name))
                 (format "Error: Unknown tool %s" tool-name)))))

          ;; Build proper tool_result block using helper
          (push (efrit--build-tool-result tool-id result-content) results))))

    (reverse results)))

(provide 'efrit-chat-api)

;;; efrit-chat-api.el ends here
