;;; efrit-tools.el --- Core functions for Efrit with focus on Elisp evaluation -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.3.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai
;; URL: https://github.com/stevey/efrit
;; Package: efrit

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; This file implements core functions for the Efrit assistant, focusing on
;; Elisp evaluation as the primary mechanism for interacting with Emacs.
;; It provides:
;;
;; 1. Direct Elisp evaluation with enhanced error handling
;; 2. Rich context gathering about the Emacs environment
;; 3. Path resolution and project information
;; 4. A simplified tool dispatching system
;; 5. Support for both <elisp>...</elisp> syntax and traditional tools
;;
;; The design follows the principle that Emacs is already designed for
;; programmatic manipulation via Elisp, so Efrit leverages that power
;; directly rather than building complex abstraction layers.

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'auth-source)

;; Load efrit-log if it exists, otherwise use minimal logging
(declare-function efrit-log "efrit-log")
(condition-case nil
    (require 'efrit-log)
  (error 
   (defun efrit-log (level format-string &rest args)
     "Fallback logging function when efrit-log is not available."
     (when (memq level '(warn error))
       (message (apply #'format format-string args))))))

;; Declare functions from efrit-do.el to avoid warnings
(declare-function efrit-do-todo-item-status "efrit-do")
(declare-function efrit-do-todo-item-priority "efrit-do")
(declare-function efrit-do-todo-item-content "efrit-do")
(declare-function efrit-do-todo-item-id "efrit-do")

;; Declare functions from efrit-common.el
(declare-function efrit-common-get-api-key "efrit-common")
(declare-function efrit-common-truncate-string "efrit-common")

;; efrit-log is loaded above or defined as fallback

;;; Customization

(defgroup efrit-tools nil
  "Tools for the Efrit conversational assistant."
  :group 'efrit
  :prefix "efrit-tools-")

;;; Logging System

;; Logging is now handled by efrit-log.el - use (efrit-log 'level ...) for logging


;; Security system removed - was causing more problems than it solved

(defcustom efrit-tools-sexp-evaluation-enabled t
  "Whether to allow the assistant to evaluate Lisp expressions."
  :type 'boolean
  :group 'efrit-tools)

(defcustom efrit-tools-sexp-max-output-length 4000
  "Maximum length of output from evaluated expressions."
  :type 'integer
  :group 'efrit-tools)

(defcustom efrit-tools-require-confirmation t
  "Whether to require confirmation for potentially destructive operations."
  :type 'boolean
  :group 'efrit-tools)

(defcustom efrit-tools-max-eval-per-session 100
  "Maximum number of eval_sexp calls allowed per session.
Set to 0 to disable rate limiting."
  :type 'integer
  :group 'efrit-tools)

(defcustom efrit-tools-max-total-calls-per-session 500
  "Maximum total number of tool calls allowed per session.
Set to 0 to disable rate limiting."
  :type 'integer
  :group 'efrit-tools)

;;; Rate Limiting State

(defvar efrit-tools--eval-count 0
  "Count of eval_sexp calls in current session.")

(defvar efrit-tools--total-call-count 0
  "Count of all tool calls in current session.")

(defun efrit-tools--reset-rate-limits ()
  "Reset rate limiting counters. Called at session start."
  (setq efrit-tools--eval-count 0
        efrit-tools--total-call-count 0))

(defun efrit-tools--check-rate-limit (tool-name)
  "Check if TOOL-NAME can be called without exceeding rate limits.
Signals an error if rate limit would be exceeded."
  (when (> efrit-tools-max-total-calls-per-session 0)
    (when (>= efrit-tools--total-call-count efrit-tools-max-total-calls-per-session)
      (error "Tool call rate limit exceeded: %d calls per session (limit: %d)"
             efrit-tools--total-call-count
             efrit-tools-max-total-calls-per-session)))

  (when (and (string= tool-name "eval_sexp")
             (> efrit-tools-max-eval-per-session 0))
    (when (>= efrit-tools--eval-count efrit-tools-max-eval-per-session)
      (error "eval_sexp rate limit exceeded: %d calls per session (limit: %d)"
             efrit-tools--eval-count
             efrit-tools-max-eval-per-session))))

(defun efrit-tools--increment-rate-limit (tool-name)
  "Increment rate limit counters for TOOL-NAME."
  (setq efrit-tools--total-call-count (1+ efrit-tools--total-call-count))
  (when (string= tool-name "eval_sexp")
    (setq efrit-tools--eval-count (1+ efrit-tools--eval-count))))

;;; Utility functions

(defun efrit-tools--elisp-results-or-empty (result)
  "Return an empty string for nil or blank RESULT, otherwise format it."
  (if (or (null result) 
          (equal result "") 
          (equal result "nil")
          (and (stringp result) (string-empty-p (string-trim result))))
      ""
    (format "%S" result)))

(defun efrit--get-api-key ()
  "Get the Anthropic API key from .authinfo file."
  (require 'efrit-common)
  (efrit-common-get-api-key))

;;; Core Elisp Evaluation

(defun efrit-tools--safe-read (string)
  "Read STRING into a single sexp, signalling an error if trailing data."
  (let* ((read-data (read-from-string string))
         (sexp  (car read-data))
         (rest  (cdr read-data)))
    (when (< rest (length string))
      (let ((trailing (substring string rest)))
        (unless (string-match-p "\\`[[:space:]]*\\'" trailing)
          (error "Trailing data after expression: %s" trailing))))
    sexp))

(defun efrit-tools--parse-sexp-string (sexp-string)
  "Parse SEXP-STRING into a Lisp expression. Returns the parsed sexp."
  (with-temp-buffer
    (insert sexp-string)
    (goto-char (point-min))
    (condition-case parse-err
        (car (read-from-string sexp-string))
      (error
       (error "Invalid Lisp syntax: %s" (error-message-string parse-err))))))

(defun efrit-tools--eval-with-context (sexp)
  "Evaluate SEXP and return result data with context."
  (let* ((result (eval sexp t))
         (result-string (format "%S" result)))
    ;; Truncate if result is too long
    (when (and result-string 
               (> (length result-string) efrit-tools-sexp-max-output-length))
      (setq result-string
            (concat (substring result-string 0 efrit-tools-sexp-max-output-length) "...")))
    
    (list :success t
          :result result-string
          :input (format "%S" sexp)
          :context (condition-case _
                      (efrit-tools-get-buffer-context)
                    (error "Operation failed")))))

(defun efrit-tools--handle-eval-error (err sexp-string)
  "Handle evaluation error ERR for SEXP-STRING. Returns error data."
  (efrit-log 'error (format "Eval error in %s: %s" sexp-string (error-message-string err)))
  (list :success nil
        :error (format "%s: %s" 
                      (pcase (car err)
                        (`void-function "Function not defined")
                        (`wrong-number-of-arguments "Wrong number of arguments") 
                        (`wrong-type-argument "Wrong type of argument")
                        (`arith-error "Arithmetic error")
                        (_ "Error"))
                      (error-message-string err))
        :input sexp-string))

;;; Security Sandboxing Configuration

;; Security system completely removed

(defun efrit-tools--extract-function-calls (sexp)
  "Extract all function calls from SEXP recursively."
  (let ((functions '()))
    (cond
     ((and (listp sexp) (symbolp (car sexp)))
      (push (car sexp) functions)
      (dolist (arg (cdr sexp))
        (setq functions (append functions (efrit-tools--extract-function-calls arg)))))
     ((listp sexp)
      (dolist (item sexp)
        (setq functions (append functions (efrit-tools--extract-function-calls item))))))
    functions))

;; Security validation completely removed

(defun efrit-tools-eval-sexp (sexp-string)
  "Evaluate the Lisp expression in SEXP-STRING and return the result as a string.
Handles parsing, evaluation, error handling, and result formatting."
  ;; Check rate limits first
  (efrit-tools--check-rate-limit "eval_sexp")

  ;; Check if evaluation is enabled
  (unless efrit-tools-sexp-evaluation-enabled
    (error "Lisp expression evaluation is disabled"))

  ;; Validate input
  (when (or (not sexp-string) (string-empty-p (string-trim sexp-string)))
    (error "Cannot evaluate empty Lisp expression"))

  ;; Security check removed - trust Claude to do the right thing

  (efrit-log 'debug "Evaluating: %s" sexp-string)

  ;; Increment rate limit counter
  (efrit-tools--increment-rate-limit "eval_sexp")

  (let ((result-data (condition-case err
                         (efrit-tools--eval-with-context
                          (efrit-tools--parse-sexp-string sexp-string))
                       (error (efrit-tools--handle-eval-error err sexp-string)))))

    ;; Return formatted result
    (if (plist-get result-data :success)
        (format "%s" (plist-get result-data :result))
      (progn
        (require 'efrit-common)
        (format "Error evaluating %s: %s"
                (efrit-common-truncate-string (plist-get result-data :input) 30)
                (plist-get result-data :error))))))

;;; Safe Text Manipulation Functions

(defun efrit-tools-reverse-lines (text)
  "Safely reverse the order of lines in TEXT.
Returns the text with lines in reverse order."
  (when text
    (let ((lines (split-string text "\n" t)))
      (mapconcat 'identity (reverse lines) "\n"))))

(defun efrit-tools-reverse-words (text)
  "Safely reverse the order of words in TEXT.
Returns the text with words in reverse order."
  (when text
    (let ((words (split-string text "\\s-+" t)))
      (mapconcat 'identity (reverse words) " "))))

(defun efrit-tools-reverse-chars (text)
  "Safely reverse the characters in TEXT.
Returns the text with characters in reverse order."
  (when text
    (concat (reverse (string-to-list text)))))

(defun efrit-tools-create-buffer-with-content (buffer-name content)
  "Safely create a buffer with BUFFER-NAME and insert CONTENT.
Returns the buffer name if successful."
  (with-current-buffer (get-buffer-create buffer-name)
    (erase-buffer)
    (insert content)
    buffer-name))

;;; Context Gathering

(defun efrit-tools-get-buffer-context ()
  "Gather comprehensive contextual information about the current buffer.

This function collects detailed information about the current buffer state
including buffer identity, positions, region, window geometry, and content
surrounding point. It provides the LLM with the context needed to generate
appropriate Elisp code.

The information collected includes:
- Buffer name and file name
- Major mode
- Point, mark, and region information
- Buffer size and boundaries
- Content samples around point
- Window information
- Default directory

Return:
  A property list containing the contextual information."
  (list :buffer-name (buffer-name)
        :file-name buffer-file-name
        :major-mode (symbol-name major-mode)
        :point (point)
        :point-min (point-min)
        :point-max (point-max)
        :mark (when (mark t) (mark t))
        :region-active mark-active
        :region-text (when (and mark-active (use-region-p))
                       (buffer-substring-no-properties (region-beginning) (region-end)))
        :modified (buffer-modified-p)
        :read-only buffer-read-only
        :current-line (line-number-at-pos)
        :current-column (current-column)
        :text-before-point (when (> (point) (point-min))
                             (buffer-substring-no-properties (max (- (point) 200) (point-min)) (point)))
        :text-after-point (when (< (point) (point-max))
                            (buffer-substring-no-properties (point) (min (+ (point) 200) (point-max))))
        :default-directory default-directory
        :window-start (window-start)
        :window-end (window-end)
        :window-width (window-width)
        :window-height (window-height)))

(defun efrit-tools-get-mode-info ()
  "Gather detailed information about the current major and minor modes.

This function collects comprehensive information about the active modes
in the current buffer, providing context about the editing environment.
This helps the LLM understand the buffer's purpose and available commands.

The collected information includes:
- Major mode name and mode name string
- Derived mode relationships
- Active minor modes
- Syntax table information (partial)
- Local keymap bindings (most important mappings)

Return:
  A property list containing the mode information."
  (list :major-mode (symbol-name major-mode)
        :mode-name mode-name
        :is-derived (mapcar #'symbol-name 
                           (cl-remove-if-not 
                            (lambda (m) (and (boundp m) (derived-mode-p m)))
                            (apropos-internal "-mode$" 'boundp)))
        :minor-modes (mapcar (lambda (mode)
                              (when (and (boundp mode) (symbol-value mode))
                                (symbol-name mode)))
                            minor-mode-list)
        :syntax-table (let ((table '()))
                        (map-char-table
                         (lambda (key value)
                           (when (and (characterp key) (not (= key ?\0)) (< key 127))
                             (push (cons (char-to-string key) (format "%s" value)) table)))
                         (syntax-table))
                        (seq-take table 20))
        :local-keymap-bindings (let ((bindings '())
                                     (map (current-local-map)))
                                 (when map
                                   (map-keymap
                                    (lambda (key binding)
                                      (when (and (characterp key) 
                                                (commandp binding))
                                        (push (cons (key-description (vector key))
                                                   (symbol-name binding))
                                              bindings)))
                                    map))
                                 (seq-take bindings 20))))

(defun efrit-tools-get-project-info ()
  "Gather comprehensive information about the current project.

This function uses Emacs' built-in project.el (if available) to collect
information about the project containing the current file or directory.
The information helps the LLM understand the project context, which is
essential for many coding operations.

The function safely handles situations where:
- project.el isn't available
- No project exists for the current context
- VCS information cannot be retrieved
- Project files cannot be listed

The collected information includes:
- Project root directory
- Project type (if available)
- Version control system backend and branch
- Sample of project files (limited to 20 for performance)

Return:
  A property list with project information, or nil if no project is found."
  (condition-case nil
      (if (fboundp 'project-current)
          (when-let* ((project (project-current)))
            (let* ((root (if (fboundp 'project-root)
                             (project-root project)
                           (when (fboundp 'project-roots)
                             (expand-file-name (car (project-roots project))))))
                   (type (when (fboundp 'project-type)
                           (project-type project)))
                   (vc-backend (when (and (require 'vc nil t)
                                         (fboundp 'vc-responsible-backend))
                                 (ignore-errors
                                   (vc-responsible-backend root))))
                   (files (when (fboundp 'project-files)
                            (condition-case nil
                                (seq-take 
                                 (mapcar (lambda (f) (file-relative-name f root))
                                        (project-files project))
                                 20)
                              (error "Operation failed")))))
              (list :project-root root
                    :project-type (when type (symbol-name type))
                    :vc-backend (when vc-backend (symbol-name vc-backend))
                    :vc-branch (when vc-backend
                                 (ignore-errors
                                   (vc-call-backend vc-backend 'branch-name root)))
                    :project-files files)))
        nil)
    (error "Operation failed")))

(defun efrit-tools-get-emacs-info ()
  "Gather comprehensive information about the Emacs environment.

This function collects system-level information about Emacs and the
operating environment. This provides important context for operations
that interact with the filesystem or require knowledge of the Emacs
configuration.

The collected information includes:
- Emacs version (full string and numeric components)
- System type and name
- User information
- Key directories (home, emacs installation)
- Execution path settings
- A sample of loaded features
- User-customized variables

This information helps the LLM generate Elisp code that's appropriate
for the specific Emacs environment.

Return:
  A property list containing the Emacs environment information."
  (list :emacs-version emacs-version
        :emacs-major-version emacs-major-version
        :emacs-minor-version emacs-minor-version
        :system-type system-type
        :system-name (system-name)
        :user-login-name user-login-name
        :user-full-name user-full-name
        :home-directory (expand-file-name "~")
        :emacs-directory user-emacs-directory
        :default-directory default-directory
        :exec-path exec-path
        :features (seq-take (mapcar #'symbol-name features) 30)
        :custom-vars (let ((vars '()))
                       (mapatoms (lambda (sym)
                                  (when (and (boundp sym)
                                  (custom-variable-p sym)
                                  (if (fboundp 'user-variable-p)
                                                 (user-variable-p sym)
                                               t))
                                    (push (symbol-name sym) vars))))
                       (seq-take vars 30))))

(defun efrit-tools-get-context ()
  "Gather comprehensive context about the Emacs environment.
Returns detailed information as a JSON string that can be used by the LLM
to better understand the environment and generate appropriate Elisp code."
  ;; Check and increment rate limits
  (efrit-tools--check-rate-limit "get_context")
  (efrit-tools--increment-rate-limit "get_context")

  (condition-case err
      (let ((context-data
             (list :buffer (efrit-tools-get-buffer-context)
                   :modes (efrit-tools-get-mode-info)
                   :project (efrit-tools-get-project-info) 
                   :emacs (efrit-tools-get-emacs-info)
                   :buffers (seq-take 
                             (mapcar (lambda (buf)
                                      (condition-case buf-err
                                          (list :name (buffer-name buf)
                                                :file (buffer-file-name buf)
                                                :major-mode (with-current-buffer buf
                                                             (symbol-name major-mode))
                                                :modified (buffer-modified-p buf)
                                                :size (buffer-size buf))
                                        (error
                                         (list :name (format "Error: %s" (error-message-string buf-err))
                                               :file nil
                                               :major-mode "unknown"
                                               :modified nil
                                               :size 0))))
                                    (buffer-list))
                             20)
                   :recent-files (when (boundp 'recentf-list)
                                  (seq-take recentf-list 10)))))
        (json-encode context-data))
    (error
     (json-encode (list :error (format "Context gathering failed: %s" (error-message-string err)))))))

;;; Tool Extraction

(defun efrit-tools-extract-tools-from-response (text)
  "Extract and process tool calls from assistant response TEXT.

This function parses the response text, looking for Elisp code blocks,
executes them, and replaces them with their results. It handles:

1. Elisp evaluation: <elisp>ELISP_CODE</elisp>

The function processes all code blocks in the text, evaluates them,
and embeds their results in the response.

If any errors occur during extraction or execution, they are caught and
properly formatted in the result text.

Return:
  A cons cell (PROCESSED-TEXT . RESULTS) where:
  - PROCESSED-TEXT is the original text with tool calls replaced by results
  - RESULTS is a list of all tool execution results in order of appearance

Arguments:
  TEXT - The text containing Elisp code blocks and/or tool calls to process."
  (unless (stringp text)
    (error "Response text must be a string"))
  
  (let ((results nil)
        (processed-text (or text ""))
        (elisp-regex "<elisp>\\([\\s\\S]+?\\)</elisp>"))
    
    (condition-case-unless-debug extraction-err
        (progn
          ;; Process Elisp evaluation requests
          (while (string-match elisp-regex processed-text)
            (let* ((elisp-code (match-string 1 processed-text))
                   (call-start (match-beginning 0))
                   (call-end (match-end 0))
                   (result (condition-case eval-err
                               (progn
                                 ;; Show elisp eval in progress if available
                                 (when (fboundp 'efrit-progress-show-elisp-eval)
                                   (require 'efrit-ui))
                                 (let ((eval-result (efrit-tools-eval-sexp elisp-code)))
                                   (when (fboundp 'efrit-progress-show-elisp-eval)
                                     (efrit-progress-show-elisp-eval elisp-code eval-result))
                                   eval-result))
                             (error
                              (format "Error in Elisp evaluation: %s" 
                                     (error-message-string eval-err))))))
              
              ;; Add result to the list
              (push result results)
              
              ;; Replace the Elisp call with its result in the text
              (setq processed-text
                    (concat (substring processed-text 0 call-start)
                            (efrit-tools--elisp-results-or-empty result)
                            (substring processed-text call-end))))))
      
      ;; Handle extraction errors
      (error 
       (message "Error extracting tools: %s" (or (error-message-string extraction-err) "Unknown error"))
       (setq processed-text (concat processed-text 
                                  "\n[Error processing tool calls: " 
                                  (or (error-message-string extraction-err) "Unknown error") "]"))))
    
    ;; Return both the processed text and results
    (cons processed-text (nreverse results))))

;;; Updated System Prompt

(defun efrit-tools-system-prompt ()
  "Generate a comprehensive system prompt for the Efrit assistant.

This function returns a carefully crafted system prompt that instructs
the LLM on how to interact with Emacs through Elisp evaluation. It explains
the preferred approach of using <elisp>...</elisp> tags and provides many
examples of common operations.

The prompt covers:
- Basic Elisp evaluation approach and syntax
- Buffer navigation and management
- Text manipulation operations
- File operations
- Environment information gathering
- Multi-step operation examples
- Safety considerations
- Context awareness guidance

This is a key part of guiding the LLM to generate effective Elisp code
that leverages Emacs' capabilities directly rather than relying on 
specialized tools.

Return:
  A string containing the complete system prompt."
  (concat "You are Efrit, an AI coding assistant embedded in Emacs. "
          "You can directly manipulate the Emacs environment using Elisp evaluation.\n\n"

          "⚠️  CRITICAL RULE #1: READ BEFORE WRITE ⚠️\n"
          "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n"
          "ALWAYS read buffer contents BEFORE manipulating them.\n"
          "Use eval_sexp with (buffer-string) or (with-current-buffer NAME (buffer-string))\n"
          "Never assume buffer state. Never guess positions. Always verify first.\n"
          "This rule applies to ALL buffer manipulation requests - no exceptions.\n\n"

          "### Using Elisp Evaluation\n"
          "The primary way to interact with Emacs is through Elisp evaluation.\n"
          "Use the eval_sexp tool to evaluate Emacs Lisp code:\n"
          "  eval_sexp: (+ 1 2)\n"
          "  eval_sexp: (buffer-name)\n"
          "  eval_sexp: (find-file \"~/.emacs.d/init.el\")\n\n"

          "### The Read-Verify-Act Pattern\n"
          "For ALL buffer manipulation requests, follow this workflow:\n\n"
          "1. READ: Call eval_sexp to read buffer contents\n"
          "   eval_sexp: (buffer-string)  ; current buffer\n"
          "   eval_sexp: (with-current-buffer \"*scratch*\" (buffer-string))  ; specific buffer\n\n"
          "2. VERIFY: Analyze the returned contents to find exact positions\n"
          "   - Locate target text, functions, or forms\n"
          "   - Identify any unexpected content (comments, whitespace, other text)\n"
          "   - Determine precise positions for manipulation\n\n"
          "3. ACT: Generate precise Elisp based on verified state\n"
          "   eval_sexp: (with-current-buffer \"*scratch*\" ...precise positioning code...)\n\n"

          "### Anti-Patterns: What NOT To Do\n"
          "These patterns WILL FAIL. Learn from these mistakes:\n\n"
          "❌ WRONG: Blind position guessing\n"
          "   eval_sexp: (progn (goto-char (point-max)) (eval-last-sexp nil))\n"
          "   Why it fails: point-max might be after comments, whitespace, or garbage text\n"
          "   Result: Tries to eval \"Thinking...\" or other non-code → void-variable error\n\n"
          "❌ WRONG: Assuming buffer is empty\n"
          "   eval_sexp: (with-current-buffer \"*scratch*\" (insert \"new text\"))\n"
          "   Why it fails: Buffer might already have content\n"
          "   Result: Text appears in wrong location, corrupts existing content\n\n"
          "❌ WRONG: Assuming buffer exists\n"
          "   eval_sexp: (with-current-buffer \"*foo*\" (goto-char 100))\n"
          "   Why it fails: Buffer might not exist yet\n"
          "   Result: Error: no buffer named *foo*\n\n"
          "✅ RIGHT: Read first, then act with precision\n"
          "   Step 1: eval_sexp: (when (get-buffer \"*foo*\") (with-current-buffer \"*foo*\" (buffer-string)))\n"
          "   Step 2: Analyze output, find exact position\n"
          "   Step 3: eval_sexp: (with-current-buffer \"*foo*\" ...precise action...)\n\n"

          "### Reading Buffer Contents\n"
          "Master these reading techniques:\n\n"
          "**Read entire buffer:**\n"
          "  eval_sexp: (buffer-string)  ; current buffer\n"
          "  eval_sexp: (with-current-buffer \"*Messages*\" (buffer-string))  ; specific buffer\n\n"
          "**Read specific region:**\n"
          "  eval_sexp: (buffer-substring 100 500)  ; positions 100-500\n\n"
          "**Check if buffer exists:**\n"
          "  eval_sexp: (get-buffer \"*scratch*\")  ; returns buffer object or nil\n\n"
          "**Read without side effects (non-destructive):**\n"
          "  eval_sexp: (save-excursion (goto-char (point-max)) (thing-at-point 'sexp))\n"
          "  ; save-excursion restores point after reading\n\n"
          "**Get buffer size and properties:**\n"
          "  eval_sexp: (buffer-size)  ; character count\n"
          "  eval_sexp: (point)  ; current position\n"
          "  eval_sexp: (point-min)  ; start of buffer (usually 1)\n"
          "  eval_sexp: (point-max)  ; end of buffer\n\n"

          "### Finding Positions Safely\n"
          "After reading buffer contents, find positions precisely:\n\n"
          "**Find last complete sexp:**\n"
          "  eval_sexp: (save-excursion\n"
          "               (goto-char (point-max))\n"
          "               (skip-chars-backward \" \\t\\n\")  ; skip trailing whitespace\n"
          "               (backward-sexp)\n"
          "               (point))\n\n"
          "**Find first function definition:**\n"
          "  eval_sexp: (save-excursion\n"
          "               (goto-char (point-min))\n"
          "               (re-search-forward \"^(defun \" nil t)\n"
          "               (beginning-of-line)\n"
          "               (point))\n\n"
          "**Find specific text:**\n"
          "  eval_sexp: (save-excursion\n"
          "               (goto-char (point-min))\n"
          "               (search-forward \"target text\")\n"
          "               (match-beginning 0))  ; start of match\n\n"

          "### Complete Example: \"Eval Last Form in *scratch*\"\n"
          "User request: \"eval the last form in *scratch*\"\n\n"
          "❌ WRONG approach (blind guessing):\n"
          "   eval_sexp: (with-current-buffer \"*scratch*\"\n"
          "                (goto-char (point-max))\n"
          "                (eval-last-sexp nil))\n"
          "   ; This WILL FAIL if there's any text after the last form!\n\n"
          "✅ RIGHT approach (read-verify-act):\n\n"
          "STEP 1 - READ:\n"
          "   eval_sexp: (with-current-buffer \"*scratch*\" (buffer-string))\n"
          "   Result: \"(defun fibonacci (n)\\n  (if (<= n 1) n\\n    (+ (fibonacci (- n 1))\\n       (fibonacci (- n 2)))))\\n\\n;; Test:\\n(fibonacci 10)\\n\\n\"\n\n"
          "STEP 2 - VERIFY:\n"
          "   Analyzing result:\n"
          "   - Buffer exists ✓\n"
          "   - Contains fibonacci function definition\n"
          "   - Last complete sexp is (fibonacci 10)\n"
          "   - Located after comment \";;Test:\"\n"
          "   - Followed by newlines (safe)\n\n"
          "STEP 3 - ACT:\n"
          "   eval_sexp: (with-current-buffer \"*scratch*\"\n"
          "                (goto-char (point-min))\n"
          "                (search-forward \"(fibonacci 10)\")\n"
          "                (eval-last-sexp nil))\n"
          "   ; Now we KNOW exactly where (fibonacci 10) is located\n\n"

          "### Common Buffer Operations\n"
          "After reading and verifying, use these operations:\n\n"
          "**Buffer Navigation:**\n"
          "  eval_sexp: (switch-to-buffer \"*scratch*\")\n"
          "  eval_sexp: (get-buffer-create \"new-buffer\")\n"
          "  eval_sexp: (buffer-list)\n\n"

          "**Text Manipulation:**\n"
          "  eval_sexp: (insert \"Hello, world!\")\n"
          "  eval_sexp: (delete-region (point) (+ (point) 10))\n"
          "  eval_sexp: (goto-char (point-min))\n"
          "  eval_sexp: (search-forward \"target\")\n\n"

          "**File Operations:**\n"
          "  eval_sexp: (find-file \"~/Documents/notes.txt\")\n"
          "  eval_sexp: (write-file \"~/new-file.txt\")\n"
          "  eval_sexp: (save-buffer)\n\n"

          "**Environment Information:**\n"
          "  Use get_context tool for detailed environment info\n"
          "  eval_sexp: (buffer-file-name)  ; current file path\n"
          "  eval_sexp: (default-directory)  ; current directory\n\n"

          "### Multi-Step Operations\n"
          "For complex tasks, break into read-verify-act cycles:\n\n"
          "Example: Copy a function from init.el\n"
          "1. eval_sexp: (find-file \"~/.emacs.d/init.el\")\n"
          "2. eval_sexp: (buffer-string)  ; READ to see what's there\n"
          "3. [Analyze output to locate target function]\n"
          "4. eval_sexp: (progn\n"
          "                (goto-char (point-min))\n"
          "                (search-forward \"(defun my-function\")\n"
          "                (beginning-of-line)\n"
          "                (set-mark (point))\n"
          "                (forward-sexp)\n"
          "                (copy-region-as-kill (mark) (point)))\n\n"

          "### Safety Considerations\n"
          "When performing potentially destructive operations:\n"
          "- Use save-excursion to preserve point and mark when reading\n"
          "- Read buffer contents first to verify what will be affected\n"
          "- Back up important data before modifying it\n"
          "- Confirm before executing commands that could cause data loss\n\n"

          "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n"
          "⚠️  REMEMBER: READ BEFORE WRITE - Every Single Time ⚠️\n"
          "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n"
          "If you're about to manipulate a buffer and haven't read its contents yet,\n"
          "STOP and read it first. No exceptions. This prevents 90% of errors.\n\n"

          "Remember to use proper Elisp formatting with correct indentation and balanced parentheses."))

;;; Pure Tools for Claude to Use Explicitly

(defun efrit-tools-create-buffer (name content &optional mode)
  "Create a buffer with NAME and CONTENT, optionally setting MODE.
This is a pure executor tool - no smart decisions about formatting.
MODE can be a symbol like \\='markdown-mode, \\='org-mode, \\='text-mode, etc."
  (let ((buffer (get-buffer-create name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert content)
        (when mode
          (funcall mode))
        (goto-char (point-min))
        (setq buffer-read-only t)))
    
    ;; Display the buffer - use more aggressive display to ensure it shows
    (pop-to-buffer buffer '(display-buffer-reuse-window
                           display-buffer-pop-up-window
                           display-buffer-below-selected
                           (window-height . 0.5)))
    (format "Buffer '%s' created with %d characters" name (length content))))

(defun efrit-tools-format-file-list (content)
  "Format CONTENT as a markdown file list.
This is a pure formatter - no intelligence about what constitutes a file path."
  (if (stringp content)
      (let ((lines (split-string content "\n" t)))
        (concat "## Files:\n\n"
                (mapconcat (lambda (line)
                             (format "- `%s`" line))
                           lines "\n")
                "\n"))
    (format "%S" content)))

(defun efrit-tools-format-todo-list (todos &optional sort-by)
  "Format TODOS list with optional SORT-BY criteria.
SORT-BY can be \\='status, \\='priority, \\='created, or nil (no sort).
This provides explicit sorting control to Claude rather than hardcoded logic."
  (when todos
    (let ((sorted-todos (cond
                         ((eq sort-by 'status)
                          (sort (copy-sequence todos)
                                (lambda (a b)
                                  (let ((status-order '(in-progress todo completed)))
                                    (< (seq-position status-order (efrit-do-todo-item-status a))
                                       (seq-position status-order (efrit-do-todo-item-status b)))))))
                         ((eq sort-by 'priority)
                          (sort (copy-sequence todos)
                                (lambda (a b)
                                  (let ((priority-order '(high medium low)))
                                    (< (seq-position priority-order (efrit-do-todo-item-priority a))
                                       (seq-position priority-order (efrit-do-todo-item-priority b)))))))
                         (t todos))))  ; No sorting
      (mapconcat (lambda (todo)
                   (format "%s [%s] %s (%s)"
                           (pcase (efrit-do-todo-item-status todo)
                             ('todo "☐")
                             ('in-progress "⟳")
                             ('completed "☑"))
                           (upcase (symbol-name (efrit-do-todo-item-priority todo)))
                           (efrit-do-todo-item-content todo)
                           (efrit-do-todo-item-id todo)))
                 sorted-todos "\n"))))

(defun efrit-tools-display-in-buffer (buffer-name content &optional window-height)
  "Display CONTENT in buffer BUFFER-NAME with optional WINDOW-HEIGHT.
Pure display control tool for Claude."
  (with-current-buffer (get-buffer-create buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert content)))
  (display-buffer (get-buffer buffer-name) 
                  `(display-buffer-reuse-window
                    display-buffer-below-selected
                    (window-height . ,(or window-height 10))))
  (format "Displayed content in buffer '%s'" buffer-name))

(provide 'efrit-tools)
;;; efrit-tools.el ends here
