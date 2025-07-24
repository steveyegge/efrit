;;; efrit-agent.el --- Agent loop pattern for Efrit -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Keywords: ai, tools, convenience, wp
;; Version: 1.0.0
;; Package-Requires: ((emacs "28.1") (cl-lib "0.5"))

;;; Commentary:

;; This file implements the agent loop pattern for the Efrit assistant,
;; based on the agent architecture described by Thorsten Ball
;; (https://ampcode.com/how-to-build-an-agent).  The agent loop pattern
;; enables Efrit to handle complex, multi-step tasks by breaking them down
;; into smaller steps, executing them iteratively, and adjusting the plan
;; based on feedback from each step's execution.
;;
;; The implementation consists of the following key components:
;;
;; 1. Planning phase: The agent uses Claude LLM to analyze the user's request
;;    and create a detailed step-by-step plan as a structured JSON object.
;;    Each step has type, description, tool specifications, and status.
;;
;; 2. Execution loop: The agent executes each step in the plan sequentially,
;;    using appropriate Emacs tools for each action.  Steps can include
;;    analysis, information gathering, execution, navigation, and more.
;;
;; 3. Tool delegation: The agent leverages Emacs functions and environment
;;    via the efrit-tools system to interact with buffers, files, and evaluate
;;    Lisp expressions in a safe manner.
;;
;; 4. Feedback processing: After each step execution, the LLM processes
;;    the results and updates the plan accordingly, which might include
;;    adding new steps, modifying existing ones, or skipping steps
;;    that are no longer necessary.
;;
;; 5. State tracking: The agent maintains state throughout the execution
;;    to ensure continuity and to provide context for future steps and
;;    the final summary.
;;
;; 6. Error handling: Robust error handling ensures that the agent can
;;    recover from errors or at least fail gracefully with informative
;;    messages about what went wrong.
;;
;; 7. Summarization: After completing all steps (or reaching the maximum
;;    number of iterations), the agent generates a concise summary of
;;    what was accomplished.
;;
;; Usage:
;;
;;   (efrit-agent-run "Your request here")
;;
;; or interactively with M-x efrit-agent-run
;;
;; The agent will break down the request, create a plan, execute it
;; step by step, and provide progress updates and a final summary.
;; 
;; The agent's operations are logged to the *efrit-agent-log* buffer
;; for debugging and review.

;;; Code:

;; Core requirements
(require 'json)
(require 'cl-lib)
(require 'url)
(require 'auth-source)

;; Declare functions from other efrit modules to avoid compiler warnings
(declare-function efrit-tools-dispatch "efrit-tools" (tool-name parameters))
(declare-function efrit-tools-system-prompt "efrit-tools" ())

;; Variables from other efrit modules
(defvar efrit-max-tokens 8192
  "Maximum number of tokens in responses. Defined in efrit-chat.el.")

;; Module requirements (with error handling for compilation)
(condition-case nil
(progn
(require 'efrit-tools)
(require 'project nil t))
(file-missing 
 (message "Note: efrit-tools not found during compilation.
This is expected if you're byte-compiling standalone files.")))

;; Function declarations to suppress byte-compiler warnings
(declare-function project-files "project" (project &optional dirs))
(declare-function project-roots "project" (project))
(declare-function efrit-tools-get-context "efrit-tools" ())
(declare-function efrit-tools-eval-sexp "efrit-tools" (sexp-string))

;;; Customization Options

(defgroup efrit-agent nil
  "Agent loop implementation for the Efrit assistant.
This group contains configuration options for the agent
architecture that handles complex multi-step tasks."
  :group 'efrit
  :prefix "efrit-agent-")

(defcustom efrit-agent-log-buffer-name "*efrit-agent-log*"
  "Name of the buffer for logging agent activity.
All agent operations, including API calls, step execution, errors,
and other important events are logged to this buffer for debugging
and auditing purposes."
  :type 'string
  :group 'efrit-agent)

(defcustom efrit-agent-max-iterations 10
  "Maximum number of iterations in the agent loop.
This acts as a safety limit to prevent infinite loops or excessive
operations. If the agent reaches this many iterations without
completing its plan, it will stop and report that the iteration
limit was reached."
  :type 'integer
  :group 'efrit-agent)

(defcustom efrit-agent-model "claude-3-7-sonnet-20250219"
  "LLM model to use for the agent.
This should be a valid Anthropic Claude model identifier.
The agent uses this model for planning, feedback processing,
and summarization."
  :type 'string
  :group 'efrit-agent)

(defcustom efrit-agent-debug nil
  "Whether to enable detailed debugging output.
When non-nil, verbose debug information is logged to the agent
log buffer, including full prompts and responses. This is useful
for development and troubleshooting but can make the log buffer
very large."
  :type 'boolean
  :group 'efrit-agent)

;;; Internal State Variables
;; These buffer-local variables track the agent's state during execution.

(defvar-local efrit-agent--state nil
  "Agent state for the current session.
An alist containing key information about the agent's state, including:
- request: The original user request
- context: Buffer context information
- timestamp: When the agent was started
- completed: Whether the agent has completed all steps
- error: Any fatal error that occurred (if applicable)
- summary: Final execution summary")

(defvar-local efrit-agent--plan nil
  "Current execution plan for the agent.
A list of steps to be executed, where each step is an alist with:
- type: The step type (analysis, gather_info, execute, etc.)
- description: A brief description of the step
- details: Detailed information about the step
- tool: The tool to use (optional)
- input: The input for the tool (optional)
- completed: Whether the step has been completed")

(defvar-local efrit-agent--current-step nil
  "Current step being executed by the agent.
This is an alist representing the step currently being processed,
with the same structure as steps in `efrit-agent--plan'.")

(defvar-local efrit-agent--results nil
  "Results from previously executed steps.
This is an alist mapping steps to their execution results,
used for context in feedback processing and summarization.")

(defvar-local efrit-agent--iteration 0
  "Current iteration count in the agent loop.
This tracks how many iterations the agent has performed,
and is used to enforce the maximum iteration limit.")

(defvar-local efrit-agent--target-buffer nil
  "Buffer that the agent is operating on.
This is the buffer where the agent will perform operations
and display progress updates.")

;;; Utility Functions
;; These functions provide logging, debugging, and progress display capabilities.

(defun efrit-agent--log (format-string &rest args)
  "Log a message to the agent log buffer with FORMAT-STRING and ARGS.
This function adds a timestamp to each log entry and appends the
formatted message to the log buffer specified by `efrit-agent-log-buffer-name'.
It's used for all agent activity logging."
  (let ((log-buffer (get-buffer-create efrit-agent-log-buffer-name)))
    (with-current-buffer log-buffer
      (goto-char (point-max))
      (insert (apply #'format (concat "[%s] " format-string "\n")
                     (cons (format-time-string "%H:%M:%S") args))))))

(defun efrit-agent--debug (format-string &rest args)
  "Log a debug message with FORMAT-STRING and ARGS if debugging is enabled.
When `efrit-agent-debug' is non-nil, this function logs detailed debugging
information to the agent log buffer. These messages are prefixed with
\"DEBUG:\" to distinguish them from regular log messages."
  (when efrit-agent-debug
    (apply #'efrit-agent--log (concat "DEBUG: " format-string) args)))

(defun efrit-agent--show-progress (message &optional buffer)
  "Display a progress MESSAGE in optional BUFFER or current buffer.
This function is used to provide user-visible feedback about the agent's
progress during execution. The message is formatted with a distinct
prefix to make it clear that it's from the agent.

If BUFFER is provided, the message is displayed in that buffer;
otherwise, it's displayed in the current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char (point-max))
        (insert (format "\n[Agent progress] %s\n" message))))))

;;; LLM API Functions
;; These functions handle communication with the Anthropic Claude API.

(defun efrit-agent--get-api-key ()
  "Get the Anthropic API key from the .authinfo file.
Uses Emacs' auth-source package to securely retrieve the API key.
Expects an entry in .authinfo or .authinfo.gpg like:
machine api.anthropic.com login personal password YOUR_API_KEY

Returns the API key as a string, or nil if no key was found."
  (when-let* ((auth-info (car (auth-source-search :host "api.anthropic.com"
                                                 :user "personal"
                                                 :require '(:secret))))
              (secret (plist-get auth-info :secret)))
    (if (functionp secret)
        (funcall secret)
      secret)))

(defun efrit-agent--build-messages (prompt system-prompt)
  "Build messages array for API request with PROMPT and optional SYSTEM-PROMPT."
  (let ((messages-json []))
    ;; Add system message if provided
    (when system-prompt
      (setq messages-json 
            (vconcat messages-json 
                     `[((role . "system")
                        (content . ,system-prompt))])))
    
    ;; Add user message
    (setq messages-json 
          (vconcat messages-json 
                   `[((role . "user")
                      (content . ,prompt))]))
    messages-json))

(defun efrit-agent--check-http-status (response-buffer)
  "Check HTTP status in RESPONSE-BUFFER and error if >= 400."
  (with-current-buffer response-buffer
    (goto-char (point-min))
    (let ((status-line (buffer-substring (point-min) 
                                        (progn (end-of-line) (point)))))
      (when (string-match "HTTP/[0-9.]+ \\([0-9]+\\)" status-line)
        (let ((status-code (string-to-number (match-string 1 status-line))))
          (when (>= status-code 400)
            (error "API request failed with status code %d: %s" 
                   status-code status-line)))))))

(defun efrit-agent--extract-content-from-response (response-buffer)
  "Extract text content from API response in RESPONSE-BUFFER."
  (with-current-buffer response-buffer
    (when (search-forward-regexp "^$" nil t)
      (condition-case parse-err
          (let* ((json-object-type 'hash-table)
                 (json-array-type 'vector)
                 (json-key-type 'string)
                 (response (json-read-from-string
                            (buffer-substring-no-properties (point) (point-max)))))
            
            ;; Check for API-level error
            (when (gethash "error" response)
              (let ((error-info (gethash "error" response)))
                (error "API error: %s" 
                       (or (gethash "message" error-info) 
                           "Unknown API error"))))
            
            ;; Extract content from response
            (let ((content (gethash "content" response))
                  (message-text ""))
              
              (unless content
                (error "API response missing content"))
              
              ;; Process all content items
              (dotimes (i (length content))
                (let* ((item (aref content i))
                       (type (gethash "type" item)))
                  (when (and (string= type "text")
                             (gethash "text" item))
                    (setq message-text 
                          (concat message-text (gethash "text" item))))))
              
              (when (string-empty-p message-text)
                (error "Received empty response from API"))
              
              message-text))
        (error
         (error "Failed to parse API response: %s" 
                (error-message-string parse-err)))))))

(defun efrit-agent--call-llm (prompt &optional system-prompt temperature)
  "Send PROMPT to the Anthropic Claude API and return the response text.
Makes a synchronous HTTP request to the Claude API with the given prompt
and returns the text content from the response.

Arguments:
  PROMPT - The main user prompt to send to the LLM.
  SYSTEM-PROMPT - Optional system instructions that provide context and
                 guidance to the LLM about its role and constraints.
  TEMPERATURE - Optional temperature parameter controlling randomness
               in the response generation. Values range from 0.0-1.0,
               where 0.0 is most deterministic and 1.0 most random.
               Defaults to 0.0 if not specified.

The function uses the model specified in `efrit-agent-model` with a
maximum token count of 4096. It handles various error conditions and
returns appropriate error messages if the API call fails."
  (efrit-agent--debug "Calling LLM with prompt: %s" 
                      (substring prompt 0 (min 100 (length prompt))))
  
  (unless prompt
    (error "Cannot call LLM with empty prompt"))
  
  (let* ((api-key (or (efrit-agent--get-api-key)
                      (error "Failed to retrieve API key")))
         (api-url "https://api.anthropic.com/v1/messages")
         (messages-json (efrit-agent--build-messages prompt system-prompt))
         (response-buffer nil)
         (result nil))
    
    ;; Prepare request data
    (let* ((request-data `((model . ,efrit-agent-model)
                           (max_tokens . ,efrit-max-tokens)
                           (temperature . ,(or temperature 0.0))
                           (messages . ,messages-json)))
           (url-request-method "POST")
           (url-request-extra-headers
            `(("x-api-key" . ,api-key)
              ("anthropic-version" . "2023-06-01")
              ("anthropic-beta" . "max-tokens-3-5-sonnet-2024-07-15")
              ("content-type" . "application/json")))
           (url-request-data 
            (encode-coding-string (json-encode request-data) 'utf-8)))
      
      (condition-case err
          (progn
            ;; Make the API request
            (setq response-buffer (url-retrieve-synchronously api-url nil t 30))
            
            (unless response-buffer
              (error "Failed to connect to Anthropic API"))
            
            ;; Check HTTP status
            (efrit-agent--check-http-status response-buffer)
            
            ;; Extract content from response
            (setq result (efrit-agent--extract-content-from-response response-buffer)))
        
        (error
         (efrit-agent--log "LLM API call error: %s" (error-message-string err))
         (setq result (format "Error calling LLM API: %s" (error-message-string err))))))
    
    ;; Clean up
    (when response-buffer 
      (kill-buffer response-buffer))
    
    ;; Return result or error message
    (or result "LLM API call failed with unknown error")))

;;; Agent State Management
;; Functions for initializing, updating, and accessing the agent's state.

(defun efrit-agent--init-state (request target-buffer)
  "Initialize the agent state with the user REQUEST and TARGET-BUFFER.
Sets up all buffer-local variables needed for agent operation:
- efrit-agent--state: The main state alist
- efrit-agent--plan: The execution plan (initially empty)
- efrit-agent--current-step: The current step (initially nil)
- efrit-agent--results: Accumulated results (initially empty)
- efrit-agent--iteration: Iteration counter (reset to 0)
- efrit-agent--target-buffer: The buffer to operate on

This function should be called at the beginning of each agent session."
  (setq-local efrit-agent--state
              `((request . ,request)
                (timestamp . ,(current-time-string))
                (context . nil)
                (completed . nil)))
  (setq-local efrit-agent--plan nil)
  (setq-local efrit-agent--current-step nil)
  (setq-local efrit-agent--results nil)
  (setq-local efrit-agent--iteration 0)
  (setq-local efrit-agent--target-buffer target-buffer))

(defun efrit-agent--update-state (key value)
  "Update the agent state by setting KEY to VALUE.
If KEY already exists in the state, its value is replaced.
Otherwise, a new key-value pair is added to the state.

This is the primary method for modifying the agent's state during execution."
  (setq-local efrit-agent--state
              (cons (cons key value)
                    (assq-delete-all key efrit-agent--state))))

(defun efrit-agent--get-state (key)
  "Get the value for KEY from the agent state.
Returns the value associated with KEY in the agent state,
or nil if the key doesn't exist in the state.

This is the primary method for accessing the agent's state information."
  (alist-get key efrit-agent--state))

;;; Context Gathering Functions
;; Functions for collecting information about the environment for planning.

(defun efrit-agent--get-project-info ()
  "Get information about the current project.
Uses `project.el` if available to determine project root and type.
Returns an alist with project root, type, and additional metadata."
  (condition-case nil
      (when (fboundp 'project-current)
        (let ((project (project-current)))
          (when project
            (let* ((root (if (fboundp 'project-root) 
                             (project-root project)
                           (expand-file-name (car (project-roots project)))))
                   (project-type (when (fboundp 'project-type)
                                   (project-type project)))
                   (vc-backend (when (and (require 'vc nil t) 
                                          (fboundp 'vc-responsible-backend)
                                          root)
                                 (ignore-errors
                                   (vc-responsible-backend root)))))
              `((root . ,root)
                (type . ,(if project-type
                             (symbol-name project-type)
                           "unknown"))
                (vc-backend . ,(if vc-backend
                                   (symbol-name vc-backend)
                                 "none"))
                (vc-root . ,(when (and vc-backend root)
                              (ignore-errors
                                (vc-call-backend vc-backend 'root root))))
                (files . ,(when (and root (fboundp 'project-files))
                            (condition-case nil 
                                (let ((files (project-files project)))
                                  (let ((file-names (mapcar (lambda (file)
                                                       (file-relative-name file root))
                                                     files)))
                                    (cl-subseq file-names 0 (min 20 (length file-names)))))  ; Limit to 20 files
                              (error "Operation failed")))))))))
    (error "Operation failed")))

(defun efrit-agent--get-directory-structure (directory)
  "Get information about DIRECTORY structure.
Returns an alist with parent directory, children directories,
and important files in the directory."
  (when (and directory (file-exists-p directory))
    (condition-case nil
        (let* ((parent (file-name-directory (directory-file-name directory)))
               (parent-dirs (when parent
                              (condition-case nil
                                  (directory-files parent nil directory-files-no-dot-files-regexp t)
                                (error "Operation failed"))))
               (children (condition-case nil
                             (cl-remove-if-not #'file-directory-p
                                              (directory-files directory t directory-files-no-dot-files-regexp))
                           (error "Operation failed")))
               (files (condition-case nil
                          (cl-remove-if #'file-directory-p
                                       (directory-files directory t directory-files-no-dot-files-regexp))
                        (error "Operation failed"))))
          `((directory . ,directory)
            (parent . ,parent)
            (sibling-dirs . ,(mapcar #'file-name-nondirectory parent-dirs))
            (subdirs . ,(mapcar #'file-name-nondirectory (cl-subseq children 0 (min 10 (length children)))))
            (files . ,(mapcar #'file-name-nondirectory (cl-subseq files 0 (min 20 (length files)))))
            (readme . ,(condition-case nil
                           (car (directory-files directory t "README\\|readme" t))
                         (error "Operation failed")))))
      (error "Operation failed"))))

(defun efrit-agent--get-recent-paths ()
  "Get information about recently visited files and directories.
Uses `recentf-list` if available and `file-name-history`.
Returns an alist with recent files and directories."
  (condition-case nil
      (let ((recent-files (when (and (boundp 'recentf-list) recentf-list)
                            (cl-subseq recentf-list 0 (min 10 (length recentf-list)))))
            (file-history (when (boundp 'file-name-history)
                            (cl-subseq file-name-history 0 (min 10 (length file-name-history))))))
        `((recent-files . ,recent-files)
          (file-history . ,file-history)))
    (error "Operation failed")))

(defun efrit-agent--resolve-path (path-str &optional base-dir)
  "Resolve PATH-STR to an absolute path using various strategies.
Optional BASE-DIR provides context for relative paths.
Handles tilde expansion, environment variables, and project-relative paths.
Returns the resolved path or nil if resolution failed."
  (condition-case nil
      (cond
       ;; Already absolute path (including those starting with ~)
       ((file-name-absolute-p path-str)
        (expand-file-name path-str))
       
       ;; Project-relative path with project prefix
       ((string-match "^project:\\(.*\\)" path-str)
        (let* ((project-relative-path (match-string 1 path-str))
               (project (project-current))
               (project-root (when project 
                               (if (fboundp 'project-root)
                                   (project-root project)
                                 (car (project-roots project))))))
          (when project-root
            (expand-file-name project-relative-path project-root))))
       
       ;; Plain relative path
       (t
        (expand-file-name path-str (or base-dir default-directory))))
    (error "Operation failed")))

(defun efrit-agent--find-file-in-project (file-pattern)
  "Find files matching FILE-PATTERN in the current project.
Uses project.el's project-files if available.
Returns a list of matching files (absolute paths)."
  (condition-case nil
      (when (fboundp 'project-current)
        (let* ((project (project-current))
               (project-files (when project (project-files project)))
               (regexp (if (string-match-p "\\*" file-pattern)
                           (wildcard-to-regexp file-pattern)
                         file-pattern)))
          (cl-remove-if-not (lambda (file) 
                              (string-match-p regexp (file-name-nondirectory file)))
                            project-files)))
    (error "Operation failed")))

(defun efrit-agent--gather-context (buffer)
  "Gather context information from BUFFER for agent planning.
Collects comprehensive buffer and environment information including:
- Buffer name and associated file
- Major and minor modes  
- Buffer state (read-only, modified)
- Point and mark positions
- Region status
- Buffer size and content sample around point
- Window information
- Project information (root directory, version control)
- Directory structure information
- Path expansion capabilities  
- Recent files and directories

Returns an alist of contextual information about the buffer environment.
This information is also stored in the agent state for later reference."
  (unless (buffer-live-p buffer)
    (error "Cannot gather context: buffer %s is not live" buffer))
  
  (condition-case err
      (with-current-buffer buffer
        (let* ((file-name (buffer-file-name))
               (file-directory (when file-name (file-name-directory file-name)))
               (default-directory-path (expand-file-name default-directory))
               ;; Get project information if available
               (project-info (efrit-agent--get-project-info))
               ;; Get directory structure information
               (dir-structure (efrit-agent--get-directory-structure 
                              (or file-directory default-directory-path)))
               ;; Get recently visited files/directories
               (recent-paths (efrit-agent--get-recent-paths))
               ;; Combine all information
               (buffer-info
                `((buffer-name . ,(buffer-name))
                  (file-name . ,file-name)
                  (file-directory . ,file-directory)
                  (default-directory . ,default-directory-path)
                  (major-mode . ,(symbol-name major-mode))
                  (minor-modes . ,(mapconcat #'symbol-name
                                            (cl-remove-if-not 
                                             (lambda (mode)
                                               (and (boundp mode)
                                                    (symbol-value mode)))
                                             minor-mode-list)
                                           ", "))
                  (read-only . ,buffer-read-only)
                  (modified . ,(buffer-modified-p))
                  (point . ,(point))
                  (mark . ,(ignore-errors (mark)))
                  (region-active . ,(region-active-p))
                  (point-min . ,(point-min))
                  (point-max . ,(point-max))
                  (content-sample . ,(condition-case nil
                                        (buffer-substring-no-properties
                                         (max (- (point) 1000) (point-min))
                                         (min (+ (point) 1000) (point-max)))
                                      (error "(Unable to extract content sample)")))
                  (window-start . ,(ignore-errors (window-start)))
                  (window-end . ,(ignore-errors (window-end)))
                  (timestamp . ,(format-time-string "%Y-%m-%d %H:%M:%S"))
                  ;; Add the extended context information
                  (project . ,project-info)
                  (directory-structure . ,dir-structure)
                  (recent-paths . ,recent-paths)
                  (home-directory . ,(expand-file-name "~"))
                  (hostname . ,(system-name))
                  (username . ,(user-login-name)))))
          
          ;; Update the agent state with the gathered context
          (efrit-agent--update-state 'context buffer-info)
          
          ;; Log the context gathering
          (efrit-agent--debug "Gathered extended context from buffer: %s" (buffer-name buffer))
          
          ;; Return the buffer info
          buffer-info))
    
    (error
     (let ((error-msg (format "Error gathering context: %s" (error-message-string err))))
       (efrit-agent--log "ERROR: %s" error-msg)
       
       ;; Create a minimal fallback context with error information
       (let ((fallback-context
              `((buffer-name . ,(buffer-name buffer))
                (error . ,error-msg)
                (timestamp . ,(format-time-string "%Y-%m-%d %H:%M:%S")))))
         (efrit-agent--update-state 'context fallback-context)
         fallback-context)))))

;;; Planning Phase Functions
;; Functions for generating and managing the agent's execution plan.

(defun efrit-agent--format-context-for-planning (context)
  "Format CONTEXT as a string for use in the planning prompt.
Takes the context alist gathered by `efrit-agent--gather-context` and
formats it into a string representation suitable for inclusion in the
LLM planning prompt. Handles special formatting for content samples
to keep the prompt at a reasonable size."
  (let ((formatted-string ""))
    ;; Process all context items except content-sample
    (dolist (item context)
      (unless (equal (car item) 'content-sample)
        (setq formatted-string
              (concat formatted-string
                      (format "%s: %s\n" (car item) (cdr item))))))
    
    ;; Add a trimmed version of the content sample if it exists
    (let ((sample (alist-get 'content-sample context)))
      (when sample
        ;; Trim sample if too long
        (when (and (stringp sample) (> (length sample) 500))
          (setq sample (concat (substring sample 0 250)
                              "\n...\n"
                              (substring sample (- (length sample) 250)))))
        
        ;; Add sample to formatted string
        (when (stringp sample)
          (setq formatted-string
                (concat formatted-string
                        (format "Content sample around point:\n%s\n" sample))))))
    
    formatted-string))

(defun efrit-agent--create-initial-plan (request context)
  "Create an initial execution plan based on REQUEST and CONTEXT using the LLM.
Sends a detailed prompt to the LLM describing the user's request and the
current buffer context, asking it to generate a step-by-step plan for
fulfilling the request. The plan is returned as a structured JSON object,
which is then parsed into an alist of steps.

Each step in the plan includes:
- type: The step type (analysis, gather_info, execute, etc.)
- description: A brief description of the step
- details: More details about what the step involves
- tool (optional): Which tool to use for execution
- input (optional): What input to provide to the tool
- completed: Whether the step is completed (initially all false)

If there's an error in plan creation (e.g., LLM unavailable or JSON parsing
error), a simple fallback plan is used instead."
  (efrit-agent--log "Creating initial plan for request: %s" request)
  
  ;; Define system prompt that instructs the LLM on plan structure
  (let* ((system-prompt
          "You are an AI agent creating a step-by-step plan for automating tasks in Emacs. 
Your goal is to break down a user's request into discrete, executable steps 
that can be carried out programmatically, primarily using Elisp evaluation.

Your output must be in valid JSON format as an array of steps. Each step should have:
1. type: The type of step (analysis, gather_info, elisp_eval, summary, etc.)
2. description: A brief summary of the step
3. details: More details about what the step involves
4. elisp_code (preferred): The Elisp code to evaluate for this step
5. tool (optional, legacy): Use only if absolutely necessary and Elisp won't work
6. input (optional, legacy): Input for legacy tool if used
7. completed: Boolean indicating if the step is completed (always false initially)

Format all JSON output as a valid JSON array using this exact structure, and nothing else:
[
  {
    \"type\": \"...\",
    \"description\": \"...\",
    \"details\": \"...\",
    \"elisp_code\": \"...\",  // preferred approach
    \"tool\": \"...\",        // legacy, use only if necessary
    \"input\": \"...\",       // legacy, use only if necessary
    \"completed\": false
  },
  ...
]")
         
         ;; Tool description to include in the prompt
         (tools-description
          "PREFERRED APPROACH - DIRECT ELISP EVALUATION:
For most operations, use the 'elisp_eval' step type with 'elisp_code' containing the Elisp to execute.
Examples:
- To examine the environment: (buffer-name), (point), (buffer-file-name), etc.
- To manipulate text: (insert \"text\"), (delete-region start end), etc.
- To navigate: (goto-char position), (search-forward \"text\"), etc.
- To work with files: (find-file \"path\"), (save-buffer), etc.
- To gather rich context: (efrit-tools-get-context)

LEGACY TOOLS (use only when absolutely necessary):
1. eval_sexp: Evaluate a Lisp expression and return its result
2. get_context: Get comprehensive information about the Emacs environment
3. resolve_path: Resolve a path from natural language description")
         
         ;; Format the context for the planning prompt
         (formatted-context (efrit-agent--format-context-for-planning context))
        
        ;; Construct the planning prompt
        (planning-prompt
         (format "I need to create a step-by-step execution plan for the following request in Emacs:

USER REQUEST:
%s

CURRENT CONTEXT:
%s

%s

Create a detailed, step-by-step plan that breaks down the request into logical steps.
Primarily use direct Elisp evaluation via the 'elisp_code' field wherever possible.
Only use legacy tools as a fallback when absolutely necessary.

The plan should include:
1. Analysis step to understand the request and environment
2. Information gathering steps as needed using Elisp
3. Implementation steps using concise and effective Elisp code
4. A final summary step

For each step that manipulates the environment, include the appropriate Elisp code.
Return ONLY a JSON array of steps, nothing else."
                 request
                 formatted-context
                 tools-description))
        
        ;; Default fallback plan in case of errors
        (fallback-plan
         (list
          `((type . "analysis")
            (description . "Analyze user request")
            (details . ,request)
            (completed . nil))
          `((type . "gather_info")
            (description . "Gather context information")
            (details . "Collect current buffer state and environment")
            (completed . nil))
          `((type . "execute")
            (description . "Execute requested action")
            (details . "Perform the action requested by the user")
            (tool . "eval_sexp")
            (input . "(message \"Executed fallback action\")")
            (completed . nil))
          `((type . "summary")
            (description . "Summarize results")
            (details . "Provide summary of actions taken and results")
            (completed . nil)))))
    
    ;; Call the LLM to generate the plan
    (let ((json-response (efrit-agent--call-llm planning-prompt system-prompt)))
      (efrit-agent--debug "Received planning response: %s" 
                          (substring json-response 0 (min 200 (length json-response))))
      
      ;; Parse and validate the response
      (let ((steps 
             (condition-case err
                 ;; Try to parse the JSON response
                 (let* ((json-array-type 'list)
                        (json-object-type 'alist)
                        (json-key-type 'symbol)
                        (parsed-steps (json-read-from-string json-response)))
                   
                   ;; Validate the response is a list
                   (unless (listp parsed-steps)
                     (error "Expected a JSON array of steps, got: %s" (type-of parsed-steps)))
                   
                   ;; Validate and normalize each step
                   (mapcar (lambda (step)
                             ;; Check required fields
                             (unless (and (alist-get 'type step)
                                          (alist-get 'description step)
                                          (alist-get 'details step))
                               (error "Step missing required fields: %s" step))
                             
                             ;; Ensure completed field exists
                             (if (assq 'completed step)
                                 step
                               (cons '(completed . nil) step)))
                           parsed-steps))
               
               ;; On error, use the fallback plan
               (error
                (efrit-agent--log "Error parsing plan JSON: %s. Using fallback plan." 
                                  (error-message-string err))
                fallback-plan))))
        
        ;; Update the agent state and return the plan
        (setq-local efrit-agent--plan steps)
        steps))))

(defun efrit-agent--update-plan (results)
  "Update the execution plan based on RESULTS from previous steps using the LLM.
This function is called after each step execution to adapt the plan
based on the results. It first marks the current step as completed,
then decides whether to update the remaining plan:

- If only the summary step remains, no update is needed
- Otherwise, sends the current plan state, recent results, and original request
  to the LLM to get an updated plan

The LLM may add, modify, or remove steps based on the execution results.
If there's an error in the update process, the function falls back to
simply marking the current step as completed without other plan changes."
  (efrit-agent--log "Updating plan based on results")
  
  ;; Mark the current step as completed
  (efrit-agent--mark-current-step-completed)
  
  ;; Check if we only have the summary step remaining
  (let ((remaining-steps (cl-remove-if (lambda (step) 
                                        (alist-get 'completed step)) 
                                      efrit-agent--plan)))
    ;; If only summary step remains, no need to update plan
    (if (and (= (length remaining-steps) 1)
             (string= (alist-get 'type (car remaining-steps)) "summary"))
        (progn
          (efrit-agent--debug "Only summary step remains, not updating plan")
          efrit-agent--plan)
      ;; Otherwise, update the plan with LLM
      (efrit-agent--update-plan-with-llm results))))

(defun efrit-agent--mark-current-step-completed ()
  "Mark the current step as completed in the plan."
  (when efrit-agent--current-step
    (let ((step-index (cl-position efrit-agent--current-step 
                                   efrit-agent--plan :test #'equal)))
      (when step-index
        (setf (alist-get 'completed (nth step-index efrit-agent--plan)) t)))))

(defun efrit-agent--update-plan-with-llm (results)
  "Use LLM to update the plan based on RESULTS from the last step."
  ;; System prompt instructs the LLM on plan revision
  (let ((system-prompt
         "You are an AI agent adjusting a step-by-step plan for automating tasks in Emacs
based on results from executed steps. Your goal is to revise the remaining steps 
of the plan to ensure successful completion of the user's request.

Your output must be in valid JSON format as an array of steps. Each step should have:
1. type: The type of step (analysis, gather_info, execute, etc.)
2. description: A brief summary of the step
3. details: More details about what the step involves
4. tool (optional): The tool to use for execution (e.g., eval_sexp, insert_text)
5. input (optional): The input to pass to the tool
6. completed: Boolean indicating if the step is completed

Format all JSON output as a valid JSON array using this exact structure, and nothing else:
[
  {
    \"type\": \"...\",
    \"description\": \"...\",
    \"details\": \"...\",
    \"tool\": \"...\",  // optional
    \"input\": \"...\",  // optional
    \"completed\": false
  },
  ...
]")
        
        ;; User prompt with context on what needs to be updated
        (update-prompt
         (format "I need to update a step-by-step execution plan based on results from executed steps.

ORIGINAL REQUEST:
%s

CURRENT PLAN (with step status):
%s

MOST RECENT STEP EXECUTED:
Type: %s
Description: %s
Result: %s

Analyze the results of the most recent step and revise the remaining (incomplete) steps in the plan accordingly.
Keep any completed steps as they are, and adjust the remaining steps based on the latest results.
Return ONLY a complete JSON array including ALL steps (completed and remaining), nothing else."
                 (alist-get 'request efrit-agent--state)
                 (json-encode efrit-agent--plan)
                 (alist-get 'type efrit-agent--current-step)
                 (alist-get 'description efrit-agent--current-step)
                 results)))
    
    ;; Get the updated plan from the LLM
    (let ((json-response (efrit-agent--call-llm update-prompt system-prompt)))
      (efrit-agent--debug "Received plan update response: %s" 
                          (substring json-response 0 (min 200 (length json-response))))
      
      ;; Parse and validate the response
      (condition-case err
          (let* ((json-array-type 'list)
                 (json-object-type 'alist)
                 (json-key-type 'symbol)
                 (parsed-steps (json-read-from-string json-response)))
            
            ;; Validate that we got a proper list
            (unless (listp parsed-steps)
              (error "Expected a JSON array of steps, got: %s" (type-of parsed-steps)))
            
            ;; Update the plan
            (setq-local efrit-agent--plan parsed-steps)
            efrit-agent--plan)
        
        ;; Handle JSON parsing errors or other issues
        (error
         (efrit-agent--log "Error parsing updated plan JSON: %s. Keeping current plan." 
                           (error-message-string err))
         ;; Keep the existing plan (already marked completed by the caller)
         efrit-agent--plan)))))

;;; Execution Loop Functions
;; Functions for executing the plan steps and processing feedback.

(defun efrit-agent--execute-elisp-code (elisp-code)
  "Execute ELISP-CODE and return the result.
Evaluates the elisp code using efrit-tools-eval-sexp."
  (efrit-agent--log "Evaluating Elisp code: %s" elisp-code)
  (efrit-agent--show-progress 
   (format "  → Evaluating Elisp: %s" 
          (if (> (length elisp-code) 60)
              (concat (substring elisp-code 0 60) "...")
            elisp-code))
   efrit-agent--target-buffer)
  
  (condition-case elisp-err
      (efrit-tools-eval-sexp elisp-code)
    (error
     (let ((err-msg (format "Error evaluating Elisp: %s" 
                           (error-message-string elisp-err))))
       (efrit-agent--log "ERROR: %s" err-msg)
       (efrit-agent--show-progress 
        (format "  → %s" err-msg) 
        efrit-agent--target-buffer)
       err-msg))))

(defun efrit-agent--execute-analysis-step (step-tool step-input)
  "Execute an analysis step with optional STEP-TOOL and STEP-INPUT."
  (if (and step-tool step-input)
      (condition-case tool-err
          (efrit-tools-dispatch step-tool step-input)
        (error 
         (format "Analysis tool error: %s" (error-message-string tool-err))))
    ;; Default analysis using context gathering
    (condition-case analysis-err
        (let ((context (efrit-agent--gather-context 
                       (or efrit-agent--target-buffer (current-buffer)))))
          (format "Analysis complete. Buffer: %s, Mode: %s, Project: %s"
                 (alist-get 'buffer-name context)
                 (alist-get 'major-mode context)
                 (if-let* ((project-info (alist-get 'project context)))
                     (alist-get 'root project-info)
                   "none")))
      (error
       (format "Analysis error: %s" (error-message-string analysis-err))))))

(defun efrit-agent--execute-gather-info-step (step-tool step-input)
  "Execute a gather_info step with optional STEP-TOOL and STEP-INPUT."
  (condition-case gather-err
      (cond
       ;; Use specified tool if available
       ((and step-tool step-input)
        (efrit-tools-dispatch step-tool step-input))
       
       ;; Default: Use efrit-tools-get-context if available
       ((fboundp 'efrit-tools-get-context)
        (let ((context-result (efrit-tools-get-context)))
          (format "Context gathered: %s" 
                 (if (stringp context-result)
                     context-result
                   (prin1-to-string context-result)))))
       
       ;; Fallback: gather basic buffer information
       (t
        (let ((context (efrit-agent--gather-context 
                       (or efrit-agent--target-buffer (current-buffer)))))
          (format "Basic context: buffer=%s, point=%s, modified=%s"
                 (alist-get 'buffer-name context)
                 (alist-get 'point context)
                 (alist-get 'modified context)))))
    (error
     (format "Context gathering error: %s" (error-message-string gather-err)))))

(defun efrit-agent--execute-search-step (step-tool step-input)
  "Execute a search step with optional STEP-TOOL and STEP-INPUT."
  (cond
   ;; Use specified search tool
   ((and step-tool step-input)
    (condition-case search-err
        (efrit-tools-dispatch step-tool step-input)
      (error
       (format "Search error: %s" (error-message-string search-err)))))
   
   ;; Basic search using Elisp if input looks like a search term
   ((and step-input (stringp step-input) (not (string-empty-p step-input)))
    (condition-case search-err
        (with-current-buffer (or efrit-agent--target-buffer (current-buffer))
          (let ((case-fold-search t)
                (found-pos nil))
            (save-excursion
              (goto-char (point-min))
              (setq found-pos (search-forward step-input nil t)))
            (if found-pos
                (format "Found '%s' at position %d" step-input found-pos)
              (format "Did not find '%s' in buffer" step-input))))
      (error
       (format "Search operation error: %s" 
              (error-message-string search-err)))))
   
   ;; No search term provided
   (t
    "No search term provided")))

(defun efrit-agent--execute-legacy-step (step-type step-tool step-input _step)
  "Execute a legacy step based on STEP-TYPE with STEP-TOOL and STEP-INPUT."
  (cond
   ;; Analysis step
   ((string= step-type "analysis")
    (efrit-agent--execute-analysis-step step-tool step-input))
   
   ;; Information gathering step
   ((string= step-type "gather_info")
    (efrit-agent--execute-gather-info-step step-tool step-input))
   
   ;; Direct Elisp evaluation step
   ((string= step-type "elisp_eval")
    (if (and step-tool (string= step-tool "eval_sexp") step-input)
        (progn
          (efrit-agent--log "Evaluating Elisp via eval_sexp: %s" step-input)
          (efrit-agent--show-progress 
           (format "  → Evaluating Elisp: %s" 
                  (if (> (length step-input) 60)
                      (concat (substring step-input 0 60) "...")
                    step-input))
           efrit-agent--target-buffer)
          
          (condition-case elisp-err
              (efrit-tools-eval-sexp step-input)
            (error
             (let ((err-msg (format "Error evaluating Elisp: %s" 
                                   (error-message-string elisp-err))))
               (efrit-agent--log "ERROR: %s" err-msg)
               (efrit-agent--show-progress 
                (format "  → %s" err-msg) 
                efrit-agent--target-buffer)
               err-msg))))
      "No Elisp code provided for evaluation"))
   
   ;; Search step
   ((string= step-type "search")
    (efrit-agent--execute-search-step step-tool step-input))
   
   ;; Legacy: Execute a tool (deprecated - prefer elisp_eval)
   ((string= step-type "execute")
    (if (and step-tool step-input)
        (progn
          (efrit-agent--log "Using legacy tool %s with input: %s" step-tool step-input)
          (efrit-agent--show-progress 
           (format "  → Using legacy tool: %s" step-tool) 
           efrit-agent--target-buffer)
          (condition-case tool-err
              (efrit-tools-dispatch step-tool step-input)
            (error
             (let ((err-msg (format "Error executing tool %s: %s" 
                                  step-tool
                                  (error-message-string tool-err))))
               (efrit-agent--log "ERROR: %s" err-msg)
               err-msg))))
      "No tool or input specified for execution step."))
   
   ;; Legacy: Modify step (deprecated - prefer elisp_eval)
   ((or (string= step-type "modify") (string= step-type "transform"))
    (if (and step-tool step-input)
        (condition-case tool-err
            (progn
              (efrit-agent--log "Using legacy modify tool %s with input: %s" 
                              step-tool step-input)
              (efrit-tools-dispatch step-tool step-input))
          (error
           (let ((err-msg (format "Error with modify tool %s: %s" 
                                step-tool
                                (error-message-string tool-err))))
             (efrit-agent--log "ERROR: %s" err-msg)
             err-msg)))
      "No tool or input specified for modify step."))
   
   ;; Legacy: Navigate step (deprecated - prefer elisp_eval)
   ((string= step-type "navigate")
    (if (and step-tool step-input)
        (condition-case tool-err
            (progn
              (efrit-agent--log "Using legacy navigate tool %s with input: %s" 
                              step-tool step-input)
              (efrit-tools-dispatch step-tool step-input))
          (error
           (let ((err-msg (format "Error with navigation tool %s: %s" 
                                step-tool
                                (error-message-string tool-err))))
             (efrit-agent--log "ERROR: %s" err-msg)
             err-msg)))
      "No tool or input specified for navigation step."))
   
   ;; Summary step: special handling
   ((string= step-type "summary")
    "Ready to generate summary.")
   
   ;; Fallback: Other tool execution
   ((and step-tool step-input)
    (efrit-agent--log "Using legacy tool %s with input: %s" step-tool step-input)
    (condition-case tool-err
        (efrit-tools-dispatch step-tool step-input)
      (error
       (let ((err-msg (format "Error with tool %s: %s" 
                            step-tool
                            (error-message-string tool-err))))
         (efrit-agent--log "ERROR: %s" err-msg)
         err-msg))))
   
   ;; Unknown step type
   (t
    (format "Unknown or unsupported step type: %s" step-type))))

(defun efrit-agent--process-step-result (result)
  "Process and display the RESULT from step execution.
Logs and shows abbreviated results to the user."
  ;; Log and show result (abbreviated if very long)
  (let ((result-preview 
         (if (stringp result)
             (substring result 0 (min 100 (length result)))
           (format "%s" result))))
    (efrit-agent--log "Step result: %s%s" 
                     result-preview
                     (if (and (stringp result) (> (length result) 100)) "..." "")))
  
  ;; Show brief result to user if it's a string
  (when (stringp result)
    (let ((preview (substring result 0 (min 80 (length result)))))
      (efrit-agent--show-progress 
       (format "  → Result: %s%s" 
              preview 
              (if (> (length result) 80) "..." ""))
       efrit-agent--target-buffer)))
  
  result)

(defun efrit-agent--get-next-step ()
  "Get the next uncompleted step from the execution plan.
Searches through the agent's current plan and returns the first step
that has not yet been completed. If all steps are completed, returns nil.

This function is used by the main execution loop to determine which
step to execute next during each iteration."
  (car (cl-remove-if (lambda (step) (alist-get 'completed step)) efrit-agent--plan)))

(defun efrit-agent--execute-step (step)
  "Execute the given STEP and return the results.
Handles the actual execution of a single step in the agent's plan.
The execution behavior depends on the step's type and available fields.

Priority is given to \\='elisp_code\\=' field which directly evaluates Elisp.
If not present, then the legacy tool-based approach is used as a fallback.
The step types include:

- analysis: Performs analysis using Elisp or a specified tool
- gather_info: Gathers information about the environment
- elisp_eval: Directly evaluates Elisp code (preferred approach)
- execute, search, modify, navigate: Legacy step types that use tools
- summary: Prepares for generating the execution summary

Results of the execution are logged and shown to the user as progress updates.
If an error occurs during execution, it's handled gracefully with
appropriate error messages while allowing the agent loop to continue."
  (efrit-agent--log "Executing step: %s" (alist-get 'description step))
  (efrit-agent--show-progress (format "Executing: %s" (alist-get 'description step)) 
                             efrit-agent--target-buffer)
  (setq-local efrit-agent--current-step step)
  
  (let ((step-type (alist-get 'type step))
        (elisp-code (alist-get 'elisp_code step))
        (step-tool (alist-get 'tool step))
        (step-input (alist-get 'input step))
        (result nil))
    
    ;; Set a reasonable default result message
    (setq result (format "Executed step: %s" (alist-get 'description step)))
    
    (condition-case-unless-debug err
        (cond
         ;; Primary approach: Direct Elisp evaluation if elisp_code is provided
         (elisp-code
          (setq result (efrit-agent--execute-elisp-code elisp-code)))
         
         ;; Fallback: Legacy tool-based execution
         (t
         (setq result (efrit-agent--execute-legacy-step step-type step-tool step-input step))))
      
      ;; Error handling for the overall execution
      (error
       (let ((error-msg (format "Error executing step: %s" (error-message-string err))))
         (efrit-agent--log "FATAL ERROR: %s" error-msg)
         (efrit-agent--show-progress 
          (format "  → Fatal error: %s" error-msg) 
          efrit-agent--target-buffer)
         (setq result (format "Execution failed: %s" error-msg)))))
    
    ;; Process and return the result
    (efrit-agent--process-step-result result)))

(defun efrit-agent--process-feedback (results)
  "Process RESULTS from step execution and update the agent state.
This function is called after each step execution to:
1. Store the results of the step in the agent's accumulated results
2. Update the execution plan based on the new results using the LLM

The function pairs the current step with its results and adds this
to the agent's results history. Then it calls `efrit-agent--update-plan`
to potentially modify the remaining steps based on these results.

Arguments:
  RESULTS - The results returned from the executed step"
  (efrit-agent--log "Processing feedback from results")
  
  ;; Add results to the accumulated results
  (setq-local efrit-agent--results
              (cons (cons efrit-agent--current-step results)
                    efrit-agent--results))
  
  ;; Update the plan based on the new results
  (efrit-agent--update-plan results))

(defun efrit-agent--is-complete ()
  "Check if all steps in the agent's plan are completed.
Returns non-nil if every step in the plan has its \\='completed\\='
field set to a non-nil value, indicating that the entire plan
has been executed. This is used to determine when the main
execution loop should terminate."
  (cl-every (lambda (step) (alist-get 'completed step)) efrit-agent--plan))

(defun efrit-agent--generate-summary ()
  "Generate a summary of the agent\\'s actions and results using the LLM.
This function is called at the end of the agent's execution to provide
a concise overview of what was accomplished. It sends the original request,
the executed plan, and all collected results to the LLM, asking it to
produce a human-readable summary focusing on:

1. The original goal/request
2. The key steps that were taken
3. The outcome/results
4. Any notable issues encountered and how they were addressed

The summary is displayed to the user and stored in the agent state.
A slightly higher temperature (0.2) is used to make the summary more
conversational and less mechanical."
  (efrit-agent--log "Generating summary")
  
  (let* ((system-prompt
          "You are an AI agent summarizing the execution of a multi-step task in Emacs.
Your goal is to provide a clear, concise summary of the actions taken and results achieved.
Be specific about what was accomplished, any issues encountered, and how they were resolved.
Keep your response clear, professional, and focused on the task execution details.")
         
         (summary-prompt
          (format "Generate a summary of the following multi-step task execution:

ORIGINAL REQUEST:
%s

EXECUTED STEPS:
%s

RESULTS COLLECTED:
%s

Provide a concise, informative summary of what was accomplished, focusing on:
1. The original goal/request
2. The key steps that were taken
3. The outcome/results
4. Any notable issues encountered and how they were addressed

Keep the summary under 5-6 sentences, focusing on the most important details."
                  (alist-get 'request efrit-agent--state)
                  (json-encode efrit-agent--plan)
                  (json-encode (reverse efrit-agent--results))))
         
         (summary (efrit-agent--call-llm summary-prompt system-prompt 0.2)))
    
    ;; Display summary in the progress area
    (efrit-agent--show-progress (concat "Summary: " summary) efrit-agent--target-buffer)
    
    ;; Return the summary
    summary))

;;; Main agent loop

(defun efrit-agent--display-plan (plan)
  "Display the current execution PLAN in a user-friendly format.
Takes the agent's plan (a list of step alists) and formats it
as a checklist with checkboxes indicating completion status.
The formatted plan is displayed in the target buffer using
the progress display mechanism.

This provides the user with visibility into the current plan
and progress, allowing them to see what steps have been completed
and what remains to be done."
  (unless (or (null plan) (not (listp plan)))
    (let ((plan-text "Current plan:\n"))
      ;; Process each step in the plan
      (dolist (step plan)
        (when (and step (listp step))  ; Ensure step is a valid alist
          (let ((completed (alist-get 'completed step))
                (description (alist-get 'description step "Unnamed step")))
            (setq plan-text 
                  (concat plan-text 
                          (format "- %s %s\n" 
                                 (if completed "✓" "□")
                                 description))))))
      
      ;; Display the formatted plan
      (when efrit-agent--target-buffer
        (efrit-agent--show-progress plan-text efrit-agent--target-buffer)))))

(defun efrit-agent-run (request &optional buffer)
  "Run the LLM-powered agent loop with REQUEST in BUFFER or current buffer.
This is the main entry point for the agent system. It orchestrates the
entire agent execution process from initial context gathering through
planning, step-by-step execution, and final summarization.

The execution happens in several distinct phases:
1. Context Gathering: Collects information about the buffer environment
2. Planning: Uses the LLM to create a detailed step-by-step plan
3. Execution Loop: Iteratively executes steps and updates the plan
4. Summarization: Generates a concise summary of what was accomplished

Arguments:
  REQUEST - A string describing what the user wants the agent to do
  BUFFER - Optional buffer to operate on (defaults to current buffer)

Returns the agent's final state as an alist containing:
- request: The original user request
- context: Buffer context information
- timestamp: When the agent was started
- completed: Whether the agent completed successfully
- error: Any fatal error that occurred (if applicable)
- summary: Final execution summary
- (and other state information)

When called interactively, prompts for the request string."
  (interactive "sRequest: ")
  (unless (stringp request)
    (error "Request must be a string"))
  
  (when (string-empty-p request)
    (user-error "Cannot run agent with empty request"))
  
  (let ((target-buffer (or buffer (current-buffer)))
        (error-occurred nil))
    
    (unless (buffer-live-p target-buffer)
      (error "Target buffer is not live"))
    
    ;; Initialize state
    (efrit-agent--init-state request target-buffer)
    (efrit-agent--log "Starting agent loop for request: %s" request)
    (efrit-agent--show-progress 
     (format "Starting agent process for request: %s" request) target-buffer)
    
    (catch 'agent-error
      (unwind-protect
          (progn
            ;; Phase 1: Gather initial context
            (condition-case ctx-err
                (progn
                  (efrit-agent--show-progress "Gathering context information..." target-buffer)
                  (efrit-agent--gather-context target-buffer))
              (error
               (setq error-occurred t)
               (efrit-agent--log "ERROR in context gathering: %s" (error-message-string ctx-err))
               (efrit-agent--show-progress 
                (format "Error gathering context: %s" (error-message-string ctx-err))
                target-buffer)
               (efrit-agent--update-state 'error
                                        (format "Context error: %s" (error-message-string ctx-err)))
               (throw 'agent-error nil)))
            
            ;; Phase 2: Create initial plan
            (condition-case plan-err
                (progn
                  (efrit-agent--show-progress "Creating plan..." target-buffer)
                  (efrit-agent--create-initial-plan request (efrit-agent--get-state 'context))
                  (efrit-agent--display-plan efrit-agent--plan))
              (error
               (setq error-occurred t)
               (efrit-agent--log "ERROR in planning: %s" (error-message-string plan-err))
               (efrit-agent--show-progress 
                (format "Error creating plan: %s" (error-message-string plan-err))
                target-buffer)
               (efrit-agent--update-state 'error
                                        (format "Planning error: %s" (error-message-string plan-err)))
               (throw 'agent-error nil)))
            
            ;; Phase 3: Main execution loop
            (catch 'loop-complete
              (condition-case loop-err
                  (progn
                    (while (and (not (efrit-agent--is-complete))
                                (< efrit-agent--iteration efrit-agent-max-iterations))
                      (setq-local efrit-agent--iteration (1+ efrit-agent--iteration))
                      (efrit-agent--log "Starting iteration %d" efrit-agent--iteration)
                      (efrit-agent--show-progress 
                       (format "\nIteration %d of %d" 
                              efrit-agent--iteration efrit-agent-max-iterations) 
                       target-buffer)
                      
                      ;; Get next step
                      (let ((next-step (efrit-agent--get-next-step)))
                        (if next-step
                            (condition-case step-err
                                (progn
                                  ;; Execute step
                                  (let ((result (efrit-agent--execute-step next-step)))
                                    
                                    ;; Process feedback
                                    (efrit-agent--process-feedback result)
                                    
                                    ;; Show updated plan
                                    (efrit-agent--display-plan efrit-agent--plan)))
                              (error
                               ;; Handle step execution error but continue loop
                               (efrit-agent--log "ERROR in step execution: %s" 
                                               (error-message-string step-err))
                               (efrit-agent--show-progress 
                                (format "Error in step execution: %s" 
                                       (error-message-string step-err))
                                target-buffer)
                               ;; Mark current step as completed to avoid infinite loop
                               (when efrit-agent--current-step
                                 (let ((step-index (cl-position efrit-agent--current-step 
                                                              efrit-agent--plan :test #'equal)))
                                   (when step-index
                                     (setf (alist-get 'completed 
                                                     (nth step-index efrit-agent--plan)) t))))))
                          
                          ;; No more steps, mark as complete
                          (efrit-agent--log "No more steps to execute")
                          (efrit-agent--update-state 'completed t)
                          (throw 'loop-complete t))))
                    
                    ;; Check if we hit the iteration limit
                    (when (and (>= efrit-agent--iteration efrit-agent-max-iterations)
                               (not (efrit-agent--is-complete)))
                      (efrit-agent--log "Reached maximum iterations without completing plan")
                      (efrit-agent--show-progress 
                       (format "Reached maximum of %d iterations without completing all steps." 
                              efrit-agent-max-iterations)
                       target-buffer)
                      (efrit-agent--update-state 'iteration-limit-reached t)))
                
                ;; Handle fatal errors in the main loop
                (error
                 (setq error-occurred t)
                 (efrit-agent--log "FATAL ERROR in execution loop: %s" 
                                 (error-message-string loop-err))
                 (efrit-agent--show-progress 
                  (format "Fatal error in execution: %s" (error-message-string loop-err)) 
                  target-buffer)
                 (efrit-agent--update-state 'error 
                                          (format "Execution error: %s" 
                                                 (error-message-string loop-err)))
                 (throw 'agent-error nil))))
            
            ;; Phase 4: Generate and display summary
            (condition-case sum-err
                (progn
                  (efrit-agent--show-progress "\nGenerating summary..." target-buffer)
                  (let ((summary (efrit-agent--generate-summary)))
                    (efrit-agent--update-state 'summary summary)))
              (error
               ;; Non-fatal summary error
               (efrit-agent--log "ERROR in summary generation: %s" (error-message-string sum-err))
               (efrit-agent--show-progress 
                (format "Error generating summary: %s" (error-message-string sum-err))
                target-buffer)
               (efrit-agent--update-state 'summary 
                                        (format "Summary generation failed: %s" 
                                               (error-message-string sum-err))))))
        
        ;; Cleanup form (always executed)
        (when error-occurred
          (efrit-agent--show-progress 
           "\nAgent processing completed with errors. See log for details."
           target-buffer)
          (efrit-agent--log "Agent process completed with errors"))))
    
    ;; Return final state
    efrit-agent--state))

;;; Future extensions

;; Core agent functionality completed:
;; ✅ LLM integration for planning and feedback
;; ✅ Step-by-step execution and visualization
;; ✅ Comprehensive error handling and recovery
;; ✅ Refactored for maintainability (helper functions)
;; ✅ Zero client-side intelligence architecture

;; Potential future enhancements:
;; - State persistence between sessions
;; - Enhanced progress visualization
;; - Concurrent step execution capabilities
;; - User interaction and guidance features

(provide 'efrit-agent)
;;; efrit-agent.el ends here