;;; efrit-tools.el --- Core functions for Efrit with focus on Elisp evaluation -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.2.0
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
(require 'efrit-log)

;; Declare functions from efrit-do.el to avoid warnings
(declare-function efrit-do-todo-item-status "efrit-do")
(declare-function efrit-do-todo-item-priority "efrit-do")
(declare-function efrit-do-todo-item-content "efrit-do")
(declare-function efrit-do-todo-item-id "efrit-do")

;; Declare functions from efrit-common.el
(declare-function efrit-common-get-api-key "efrit-common")
(declare-function efrit-common-truncate-string "efrit-common")

;;; Customization

(defgroup efrit-tools nil
  "Tools for the Efrit conversational assistant."
  :group 'efrit
  :prefix "efrit-tools-")

;;; Logging System

;; Logging is now handled by efrit-log.el - use (efrit-log 'level ...) for logging


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

(defun efrit-tools-eval-sexp (sexp-string)
  "Evaluate the Lisp expression in SEXP-STRING and return the result as a string.
Handles parsing, evaluation, error handling, and result formatting."
  ;; Check if evaluation is enabled
  (unless efrit-tools-sexp-evaluation-enabled
    (error "Lisp expression evaluation is disabled"))
  
  ;; Validate input
  (when (or (not sexp-string) (string-empty-p (string-trim sexp-string)))
    (error "Cannot evaluate empty Lisp expression"))
  
  (efrit-log 'debug "Evaluating: %s" sexp-string)
  
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
                                   (require 'efrit-progress))
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
          
          "### Using Elisp Evaluation\n"
          "The primary way to interact with Emacs is through Elisp evaluation.\n"
          "Use <elisp> tags to evaluate Emacs Lisp code:\n"
          "  <elisp>(+ 1 2)</elisp>\n"
          "  <elisp>(buffer-name)</elisp>\n"
          "  <elisp>(find-file \"~/.emacs.d/init.el\")</elisp>\n\n"
          
          "### Common Emacs Operations\n"
          "Here are examples of common operations using Elisp:\n\n"
          
          "**Buffer Navigation and Management**:\n"
          "  <elisp>(switch-to-buffer \"*scratch*\")</elisp>\n"
          "  <elisp>(get-buffer-create \"new-buffer\")</elisp>\n"
          "  <elisp>(buffer-list)</elisp>\n\n"
          
          "**Text Manipulation**:\n"
          "  <elisp>(insert \"Hello, world!\")</elisp>\n"
          "  <elisp>(delete-region (point) (+ (point) 10))</elisp>\n"
          "  <elisp>(goto-char (point-min))</elisp>\n"
          "  <elisp>(search-forward \"target\")</elisp>\n\n"
          
          "**File Operations**:\n"
          "  <elisp>(find-file \"~/Documents/notes.txt\")</elisp>\n"
          "  <elisp>(write-file \"~/new-file.txt\")</elisp>\n"
          "  <elisp>(save-buffer)</elisp>\n\n"
          
          "**Environment Information**:\n"
          "  <elisp>(get_context)</elisp> - Get detailed environment information\n"
          "  <elisp>(buffer-file-name)</elisp> - Get current file path\n"
          "  <elisp>(default-directory)</elisp> - Get current directory\n\n"
          
          "### Multi-step Operations\n"
          "For complex tasks, break them down into a sequence of Elisp evaluations:\n\n"
          "1. <elisp>(find-file \"~/.emacs.d/init.el\")</elisp>\n"
          "2. <elisp>(goto-char (point-min))</elisp>\n"
          "3. <elisp>(search-forward \"use-package\")</elisp>\n"
          "4. <elisp>(set-mark (point))</elisp>\n"
          "5. <elisp>(search-forward \")\")</elisp>\n"
          "6. <elisp>(copy-region-as-kill (mark) (point))</elisp>\n\n"
          
          "### Safety Considerations\n"
          "When performing potentially destructive operations:\n"
          "- Use save-excursion when appropriate to preserve point and mark\n"
          "- Back up important data before modifying it\n"
          "- Confirm before executing commands that could cause data loss\n\n"
          
          "### Using Context\n"
          "Always consider the current context when generating Elisp code:\n"
          "- What buffer is active? <elisp>(buffer-name)</elisp>\n"
          "- Where is point? <elisp>(point)</elisp>\n"
          "- What mode is active? <elisp>(symbol-name major-mode)</elisp>\n"
          "- What file is being edited? <elisp>(buffer-file-name)</elisp>\n\n"
          
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
