;;; efrit-do.el --- Execute natural language commands in Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.2.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai
;; URL: https://github.com/stevey/efrit

;;; Commentary:
;; This file provides a simple command interface for executing natural language
;; commands in Emacs using the Efrit assistant.  It allows users to issue
;; commands like "open Dired to my downloads folder" and have them executed
;; immediately.

;;; Code:

(require 'efrit-chat)
(require 'efrit-tools)
(require 'cl-lib)
(require 'seq)
(require 'ring)

;;; Customization

(defgroup efrit-do nil
  "Natural language command execution for Efrit."
  :group 'efrit
  :prefix "efrit-do-")

(defcustom efrit-do-buffer-name "*efrit-do*"
  "Name of the buffer for command execution results."
  :type 'string
  :group 'efrit-do)

(defcustom efrit-do-show-results t
  "Whether to show command results in a buffer."
  :type 'boolean
  :group 'efrit-do)

(defcustom efrit-do-show-errors-only t
  "When non-nil, only show the *efrit-do* buffer for errors.
When nil, show buffer for all results (controlled by `efrit-do-show-results')."
  :type 'boolean
  :group 'efrit-do)

(defcustom efrit-do-history-max 50
  "Maximum number of commands to keep in history."
  :type 'integer
  :group 'efrit-do)

(defcustom efrit-do-debug nil
  "Whether to show debug information during command execution."
  :type 'boolean
  :group 'efrit-do)

(defcustom efrit-do-context-size 10
  "Size of the context ring."
  :type 'integer
  :group 'efrit-do)

(defcustom efrit-do-context-file "~/.emacs.d/efrit-do-context.el"
  "File to persist context data."
  :type 'file
  :group 'efrit-do)

;;; Internal variables

(defvar efrit-do-history nil
  "History of executed commands.")

(defvar efrit-do--last-result nil
  "Result of the last executed command.")

;;; Context system

(cl-defstruct (efrit-do-context-item
            (:constructor efrit-do-context-item-create)
            (:type vector))
  "Context item structure."
  timestamp
  command
  result
  buffer
  directory
  window-config
  metadata)

(defvar efrit-do--context-ring nil
  "Ring buffer for context items.")

(defvar efrit-do--context-hooks nil
  "Hooks run when context is captured.")

;;; Context ring management

(defun efrit-do--ensure-context-ring ()
  "Ensure the context ring is initialized."
  (unless efrit-do--context-ring
    (setq efrit-do--context-ring (make-ring efrit-do-context-size))))

(defun efrit-do--capture-context (command result)
  "Capture current context after executing COMMAND with RESULT."
  (efrit-do--ensure-context-ring)
  (let ((item (efrit-do-context-item-create
               :timestamp (current-time)
               :command command
               :result result
               :buffer (buffer-name)
               :directory default-directory
               :window-config (current-window-configuration)
               :metadata (list :point (point)
                              :mark (mark)
                              :major-mode major-mode))))
    (ring-insert efrit-do--context-ring item)
    (run-hook-with-args 'efrit-do--context-hooks item)))

(defun efrit-do--get-context-items (&optional n)
  "Get N most recent context items (default all)."
  (efrit-do--ensure-context-ring)
  (let ((ring efrit-do--context-ring)
        (count (or n (ring-length efrit-do--context-ring)))
        items)
    (dotimes (i (min count (ring-length ring)))
      (push (ring-ref ring i) items))
    items))

(defun efrit-do--context-to-string (item)
  "Convert context ITEM to string representation."
  (format "[%s] %s -> %s (in %s)"
          (format-time-string "%H:%M:%S" (efrit-do-context-item-timestamp item))
          (efrit-do-context-item-command item)
          (truncate-string-to-width (or (efrit-do-context-item-result item) "") 50 nil nil t)
          (efrit-do-context-item-buffer item)))

(defun efrit-do--clear-context ()
  "Clear the context ring."
  (when efrit-do--context-ring
    (setq efrit-do--context-ring (make-ring efrit-do-context-size))))

;;; Context persistence

(defun efrit-do--save-context ()
  "Save context ring to file."
  (when efrit-do--context-ring
    (let ((file (expand-file-name efrit-do-context-file))
          (items (mapcar (lambda (item)
                          ;; Create a copy without window-config for serialization
                          (efrit-do-context-item-create
                           :timestamp (efrit-do-context-item-timestamp item)
                           :command (efrit-do-context-item-command item)
                           :result (efrit-do-context-item-result item)
                           :buffer (efrit-do-context-item-buffer item)
                           :directory (efrit-do-context-item-directory item)
                           :window-config nil  ; Skip window config
                           :metadata (efrit-do-context-item-metadata item)))
                        (efrit-do--get-context-items))))
      ;; Ensure directory exists
      (make-directory (file-name-directory file) t)
      (condition-case err
          (with-temp-file file
            (insert ";;; Efrit-do context data\n")
            (insert (format ";; Saved: %s\n" (current-time-string)))
            (insert (format "(setq efrit-do--saved-context-data\n'%S)\n" items)))
        (error
         (when efrit-do-debug
           (message "Error saving context: %s" (error-message-string err))))))))

(defun efrit-do--load-context ()
  "Load context ring from file."
  (let ((file (expand-file-name efrit-do-context-file)))
    (when (file-readable-p file)
      (condition-case err
          (progn
            (load-file file)
            (efrit-do--ensure-context-ring)
            (when (boundp 'efrit-do--saved-context-data)
              (dolist (item (reverse efrit-do--saved-context-data))
                (when (vectorp item)
                  (ring-insert efrit-do--context-ring item)))
              (makunbound 'efrit-do--saved-context-data)))
        (error
         (when efrit-do-debug
           (message "Error loading context: %s" (error-message-string err))))))))

;;; Helper functions for improved error handling

(defun efrit-do--extract-response-text (response-buffer)
  "Extract response text from RESPONSE-BUFFER with proper cleanup.
Returns the response body as a string, or nil if extraction fails.
The RESPONSE-BUFFER is automatically killed after extraction."
  (unwind-protect
      (when (buffer-live-p response-buffer)
        (with-current-buffer response-buffer
          (goto-char (point-min))
          (when (search-forward-regexp "^$" nil t)
            (buffer-substring-no-properties (point) (point-max)))))
    (when (buffer-live-p response-buffer)
      (kill-buffer response-buffer))))

(defun efrit-do--sanitize-elisp-string (str)
  "Sanitize potentially over-escaped elisp string from JSON.
This handles cases where JSON escaping has been applied multiple times."
  (when str
    ;; Try to detect and fix over-escaped strings
    (let ((cleaned str))
      ;; If we see patterns like \\\\b, it's likely over-escaped
      (when (string-match-p "\\\\\\\\\\\\b\\|\\\\\\\\\\\\(" cleaned)
        (setq cleaned (replace-regexp-in-string "\\\\\\\\\\\\b" "\\\\b" cleaned))
        (setq cleaned (replace-regexp-in-string "\\\\\\\\\\\\(" "\\\\(" cleaned))
        (setq cleaned (replace-regexp-in-string "\\\\\\\\\\\\)" "\\\\)" cleaned))
        (setq cleaned (replace-regexp-in-string "\\\\\\\\\\\\|" "\\\\|" cleaned)))
      cleaned)))

(defun efrit-do--execute-tool (tool-item)
  "Execute a tool specified by TOOL-ITEM hash table.
TOOL-ITEM should contain \\='name\\=' and \\='input\\=' keys.
Returns a formatted string with execution results or empty string on failure."
  (let* ((tool-name (gethash "name" tool-item))
         (tool-input (gethash "input" tool-item))
         (input-str (cond
                     ((stringp tool-input) tool-input)
                     ((hash-table-p tool-input) 
                      (seq-some (lambda (key) (gethash key tool-input))
                                '("expr" "expression" "code")))
                     (t (format "%S" tool-input)))))
    
    ;; Sanitize potentially over-escaped strings
    (when input-str
      (setq input-str (efrit-do--sanitize-elisp-string input-str)))
    
    (when efrit-do-debug
      (message "Tool use: %s with input: %S (extracted: %S)" 
               tool-name tool-input input-str))
    
    (if (and (string= tool-name "eval_sexp") input-str)
        (condition-case eval-err
            (let ((eval-result (efrit-tools-eval-sexp input-str)))
              (format "\n[Executed: %s]\n[Result: %s]" input-str eval-result))
          (error
           (format "\n[Error executing %s: %s]" 
                   input-str (error-message-string eval-err))))
      "")))

;;; Command execution

(defun efrit-do--command-system-prompt ()
  "Generate system prompt for command execution with optional context.
Uses previous command context if available."
  (let ((context-info (when efrit-do--last-result
                        (let ((recent-items (efrit-do--get-context-items 1)))
                          (when recent-items
                            (let ((item (car recent-items)))
                              (format "\n\nPREVIOUS CONTEXT:\nLast command: %s\nLast result: %s\n\n"
                                      (efrit-do-context-item-command item)
                                      (efrit-do-context-item-result item))))))))
    (concat "You are Efrit, an AI assistant that executes natural language commands in Emacs.\n\n"
          
          "IMPORTANT: You are in COMMAND MODE. This means:\n"
          "- Generate valid Elisp code to accomplish the user's request\n"
          "- Use the eval_sexp tool to execute the code immediately\n"
          "- DO NOT explain what you're doing unless asked\n"
          "- DO NOT ask for clarification - make reasonable assumptions\n"
          "- ONLY use documented Emacs functions - NEVER invent function names\n"
          "- If unsure about a function, use simpler approaches or multiple steps\n"
          "- For file paths, always use expand-file-name to handle ~ expansion\n"
          "- Be concise in responses\n"
          "- If user says 'that didn't work' or similar, examine the previous command/result to debug\n\n"
          
          "BUFFER OPERATIONS GUIDANCE:\n"
          "- When user says 'the text' or 'the buffer', operate on entire buffer (point-min) to (point-max)\n"
          "- Use temporary bindings (let) for settings when possible - preserve user's original settings\n"
          "- Only operate on current paragraph/region if explicitly specified\n"
          "- For buffer-wide operations, prefer whole-buffer functions\n\n"
          
          "Common tasks:\n"
          "- Font scaling: (global-text-scale-adjust 2) or (text-scale-adjust 2)\n"
          "- Buffer switching: (switch-to-buffer \"*Messages*\")\n"
          "- Window operations: (split-window-horizontally), (other-window 1)\n"
          "- Text wrapping: (let ((fill-column N)) (fill-region (point-min) (point-max)))\n"
          "- Sorting lines: (sort-lines nil (point-min) (point-max))\n"
          "- Case changes: (upcase-region (point-min) (point-max))\n\n"
          
          "Examples:\n\n"
          
          "User: open dired to my downloads folder\n"
          "Assistant: I'll open dired for your downloads folder.\n"
          "Tool call: eval_sexp with expr: \"(dired (expand-file-name \\\"~/Downloads/\\\"))\"\n\n"
          
          "User: split window and show scratch buffer\n"
          "Assistant: I'll split the window and show the scratch buffer.\n"
          "Tool call: eval_sexp with expr: \"(progn (split-window-horizontally) (other-window 1) (switch-to-buffer \\\"*scratch*\\\"))\"\n\n"
          
          "User: save all buffers\n"
          "Assistant: I'll save all modified buffers.\n"
          "Tool call: eval_sexp with expr: \"(save-some-buffers t)\"\n\n"
          
          "User: wrap the text to 2500 columns\n"
          "Assistant: I'll wrap the text to 2500 columns.\n"
          "Tool call: eval_sexp with expr: \"(let ((fill-column 2500)) (fill-region (point-min) (point-max)))\"\n\n"
          
          "Remember: Generate safe, valid Elisp and execute immediately."
          (or context-info ""))))

(defun efrit-do--format-result (command result)
  "Format COMMAND and RESULT for display."
  (with-temp-buffer
    (insert (format "Command: %s\n" command))
    (insert (make-string 60 ?-) "\n")
    (insert result)
    (buffer-string)))

(defun efrit-do--display-result (command result &optional error-p)
  "Display COMMAND and RESULT in the results buffer.
If ERROR-P is non-nil, this indicates an error result.
When `efrit-do-show-errors-only' is non-nil, only show buffer for errors."
  (when efrit-do-show-results
    (with-current-buffer (get-buffer-create efrit-do-buffer-name)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (unless (bobp)
          (insert "\n\n"))
        (insert (efrit-do--format-result command result)))
      ;; Only display buffer if not in errors-only mode, or if this is an error
      (when (or (not efrit-do-show-errors-only) error-p)
        (display-buffer (current-buffer) 
                        '(display-buffer-reuse-window
                          display-buffer-below-selected
                          (window-height . 10)))))))

(defun efrit-do--execute-command (command)
  "Execute natural language COMMAND and return the result.
Uses improved error handling."
  (condition-case api-err
      (let* ((api-key (efrit--get-api-key))
             (url-request-method "POST")
             (url-request-extra-headers
              `(("x-api-key" . ,api-key)
                ("anthropic-version" . "2023-06-01")
                ("content-type" . "application/json")))
             (system-prompt (efrit-do--command-system-prompt))
             (request-data
              `(("model" . ,efrit-model)
                ("max_tokens" . ,efrit-max-tokens)
                ("temperature" . 0.0)
                ("messages" . [(("role" . "user")
                               ("content" . ,command))])
                ("system" . ,system-prompt)
                ("tools" . [(("name" . "eval_sexp")
                            ("description" . "Evaluate a Lisp expression and return the result. This is the primary tool for interacting with Emacs.")
                            ("input_schema" . (("type" . "object")
                                              ("properties" . (("expr" . (("type" . "string")
                                                                          ("description" . "The Elisp expression to evaluate")))))
                                              ("required" . ["expr"]))))])))
             (url-request-data
              (encode-coding-string (json-encode request-data) 'utf-8)))
        
        (if-let* ((response-buffer (url-retrieve-synchronously efrit-api-url))
                  (response-text (efrit-do--extract-response-text response-buffer)))
            (efrit-do--process-api-response response-text)
          (error "Failed to get response from API")))
    (error
     (format "API error: %s" (error-message-string api-err)))))

(defun efrit-do--process-api-response (response-text)
  "Process API RESPONSE-TEXT and execute any tools."
  (condition-case json-err
      (let* ((json-object-type 'hash-table)
             (json-array-type 'vector)
             (json-key-type 'string)
             (response (json-read-from-string response-text))
             (message-text ""))
        
        (when efrit-do-debug
          (message "Raw API Response: %s" response-text)
          (message "Parsed response: %S" response))
        
        ;; Check for API errors first
        (if-let* ((error-obj (gethash "error" response)))
            (let ((error-type (gethash "type" error-obj))
                  (error-message (gethash "message" error-obj)))
              (format "API Error (%s): %s" error-type error-message))
          
          ;; Process successful response
          (let ((content (gethash "content" response)))
            (when efrit-do-debug
              (message "API Response content: %S" content))
            
            (when content
              (dotimes (i (length content))
                (let* ((item (aref content i))
                       (type (gethash "type" item)))
                  (cond
                   ;; Handle text content
                   ((string= type "text")
                    (when-let* ((text (gethash "text" item)))
                      (setq message-text (concat message-text text))))
                   
                   ;; Handle tool use
                   ((string= type "tool_use")
                    (setq message-text 
                          (concat message-text 
                                  (efrit-do--execute-tool item))))))))
            
            (or message-text "Command executed"))))
    (error
     (format "JSON parsing error: %s" (error-message-string json-err)))))

;;;###autoload
(defun efrit-do (command)
  "Execute natural language COMMAND in Emacs.
The command is sent to Claude, which translates it into Elisp
and executes it immediately.  Results are displayed in a dedicated
buffer if `efrit-do-show-results' is non-nil."
  (interactive
   (list (read-string "Command: " nil 'efrit-do-history)))
  
  ;; Add to history
  (add-to-history 'efrit-do-history command efrit-do-history-max)
  
  ;; Execute the command
  (message "Executing: %s..." command)
  (condition-case err
      (if-let* ((result (efrit-do--execute-command command)))
          (progn
            (setq efrit-do--last-result result)
            (efrit-do--capture-context command result)
            (efrit-do--display-result command result)
            (message "Command executed successfully"))
        (let ((error-msg "No result returned from command execution"))
          (setq efrit-do--last-result error-msg)
          (efrit-do--display-result command error-msg t)  ; t = error-p
          (user-error "%s" error-msg)))
    (error
     (let ((error-msg (format "Error: %s" (error-message-string err))))
       (setq efrit-do--last-result error-msg)
       (efrit-do--display-result command error-msg t)  ; t = error-p
       (user-error "%s" error-msg)))))

;;;###autoload
(defun efrit-do-repeat ()
  "Repeat the last efrit-do command."
  (interactive)
  (if efrit-do-history
      (efrit-do (car efrit-do-history))
    (message "No previous command to repeat")))

;;;###autoload
(defun efrit-do-clear-results ()
  "Clear the efrit-do results buffer."
  (interactive)
  (if-let* ((buffer (get-buffer efrit-do-buffer-name)))
      (progn
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (erase-buffer)))
        (message "Results buffer cleared"))
    (message "No results buffer to clear")))

;;; Context system user commands

;;;###autoload
(defun efrit-do-show-context ()
  "Show recent context items."
  (interactive)
  (let ((items (efrit-do--get-context-items)))
    (if items
        (with-output-to-temp-buffer "*efrit-do-context*"
          (princ "Recent efrit-do context:\n\n")
          (dolist (item items)
            (princ (efrit-do--context-to-string item))
            (princ "\n")))
      (message "No context items found"))))

;;;###autoload
(defun efrit-do-clear-context ()
  "Clear the context ring."
  (interactive)
  (efrit-do--clear-context)
  (message "Context cleared"))

;;;###autoload
(defun efrit-do-clear-history ()
  "Clear efrit-do command history and context."
  (interactive)
  (setq efrit-do-history nil)
  (efrit-do--clear-context)
  (setq efrit-do--last-result nil)
  (message "History and context cleared"))

;;;###autoload
(defun efrit-do-clear-all ()
  "Clear all efrit-do state: history, context, results buffer, and conversations."
  (interactive)
  (setq efrit-do-history nil)
  (efrit-do--clear-context)
  (setq efrit-do--last-result nil)
  
  ;; Clear results buffer if it exists
  (when-let* ((buffer (get-buffer efrit-do-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer))))
  
  ;; Clear multi-turn conversations if available
  (when (boundp 'efrit--multi-turn-conversations)
    (clrhash efrit--multi-turn-conversations))
  
  (message "All efrit-do state cleared"))

;;;###autoload
(defun efrit-do-reset ()
  "Interactive reset with options for different levels of clearing."
  (interactive)
  (let ((choice (read-char-choice 
                 "Reset efrit-do: (h)istory only, (c)ontext only, (r)esults buffer, (a)ll state, or (q)uit? "
                 '(?h ?c ?r ?a ?q))))
    (cond
     ((eq choice ?h) (setq efrit-do-history nil)
                     (setq efrit-do--last-result nil)
                     (message "Command history cleared"))
     ((eq choice ?c) (efrit-do-clear-context))
     ((eq choice ?r) (efrit-do-clear-results))
     ((eq choice ?a) (efrit-do-clear-all))
     ((eq choice ?q) (message "Reset cancelled")))))

;;;###autoload
(defun efrit-do-to-chat (&optional n)
  "Convert recent efrit-do context to efrit-chat session.
Include last N commands (default 5)."
  (interactive "P")
  (require 'efrit-chat)
  (let* ((count (or n 5))
         (items (efrit-do--get-context-items count))
         (buffer (efrit--setup-buffer)))
    
    (if (not items)
        (message "No efrit-do context to convert")
      
      (with-current-buffer buffer
        (setq buffer-read-only nil)
        (let ((inhibit-read-only t))
          ;; Clear existing content and history
          (erase-buffer)
          (setq-local efrit--message-history nil)
          
          ;; Add context summary
          (efrit--display-message 
           (format "Chat session from %d recent efrit-do commands:" (length items))
           'system)
          
          ;; Convert each context item to conversation
          (dolist (item items) ; Show oldest first (items already in oldest-first order)
            (let ((command (efrit-do-context-item-command item))
                  (result (efrit-do-context-item-result item))
                  (timestamp (efrit-do-context-item-timestamp item)))
              
              ;; Display user command
              (efrit--display-message 
               (format "[%s] %s" 
                       (format-time-string "%H:%M:%S" timestamp)
                       command)
               'user)
              
              ;; Display result (truncated if too long)
              (efrit--display-message 
               (truncate-string-to-width result 500 nil nil t)
               'assistant)))
          
          ;; Build history in correct order for efrit-chat (newest first)
          ;; Process items in reverse order so newest ends up first
          (dolist (item items)
            (let ((command (efrit-do-context-item-command item))
                  (result (efrit-do-context-item-result item)))
              (push `((role . "assistant") (content . ,result)) efrit--message-history)
              (push `((role . "user") (content . ,command)) efrit--message-history)))
          
          ;; Insert prompt for new input
          (efrit--insert-prompt)))
      
      ;; Switch to the chat buffer
      (switch-to-buffer buffer)
      (message "Converted %d efrit-do commands to chat session" (length items)))))

;;; Initialization

(defun efrit-do--initialize ()
  "Initialize the efrit-do context system."
  (efrit-do--load-context)
  (add-hook 'kill-emacs-hook #'efrit-do--save-context))

(defun efrit-do--uninitialize ()
  "Uninitialize the efrit-do context system."
  (efrit-do--save-context)
  (remove-hook 'kill-emacs-hook #'efrit-do--save-context))

(add-hook 'after-init-hook #'efrit-do--initialize)

(provide 'efrit-do)

;;; efrit-do.el ends here
