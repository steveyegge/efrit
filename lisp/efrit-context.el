;;; efrit-context.el --- Context management utilities for Efrit -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.2.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; This module provides context management utilities for Efrit sessions.
;; It handles work log compression, context extraction, and state management
;; to support multi-step command execution with minimal token usage.
;;
;; Key features:
;; - Smart work log compression for Claude context
;; - Buffer state tracking and restoration
;; - Context accumulation across steps
;; - Efficient representation of execution history

;;; Code:

(require 'cl-lib)
(require 'efrit-log)
(require 'efrit-common)

;;; Customization

(defgroup efrit-context nil
  "Context management for Efrit."
  :group 'efrit
  :prefix "efrit-context-")

(defcustom efrit-context-max-result-length 200
  "Maximum length of result strings in compressed logs."
  :type 'integer
  :group 'efrit-context)

(defcustom efrit-context-include-buffer-state t
  "Whether to include buffer state in context."
  :type 'boolean
  :group 'efrit-context)

(defcustom efrit-context-compression-level 'smart
  "Level of context compression.
- minimal: Only essential information
- smart: Balanced compression (default)
- full: Include all available context"
  :type '(choice (const :tag "Minimal" minimal)
                 (const :tag "Smart" smart)
                 (const :tag "Full" full))
  :group 'efrit-context)

;;; Context Data Structures

(cl-defstruct efrit-context-state
  "State snapshot for context tracking."
  buffer-name
  buffer-mode
  point
  mark
  window-config
  directory
  timestamp
  metadata)

(cl-defstruct efrit-context-entry
  "Single context entry in work log."
  code          ; Executed elisp code
  result        ; Result of execution
  state         ; Context state before execution
  duration      ; Execution duration
  error-p)      ; Whether an error occurred

;;; Work Log Compression

(defun efrit-context-compress-work-log (work-log &optional level)
  "Compress WORK-LOG for efficient Claude context usage.
LEVEL determines compression aggressiveness (defaults to customization)."
  (let ((level (or level efrit-context-compression-level)))
    (cl-case level
      (minimal (efrit-context--minimal-compression work-log))
      (smart (efrit-context--smart-compression work-log))
      (full (efrit-context--full-compression work-log))
      (t (efrit-context--smart-compression work-log)))))

(defun efrit-context--minimal-compression (work-log)
  "Minimal compression - only essential information."
  (let ((total-steps (length work-log))
        (last-result (when work-log (caar work-log))))
    (json-encode
     `((steps . ,total-steps)
       (last_result . ,(if last-result
                          (efrit-common-truncate-string 
                           (format "%s" last-result) 
                           efrit-context-max-result-length)
                        "none"))))))

(defun efrit-context--smart-compression (work-log)
  "Smart compression - balanced information preservation."
  (let* ((total-steps (length work-log))
         (recent-entries (seq-take work-log 5))
         (compressed-entries
          (mapcar (lambda (entry)
                   (let ((result (car entry))
                         (code (cdr entry)))
                     `((code . ,(efrit-context--compress-code code))
                       (result . ,(efrit-context--compress-result result)))))
                 recent-entries)))
    (json-encode
     `((total_steps . ,total-steps)
       (recent . ,compressed-entries)
       (buffer_modified . ,(buffer-modified-p))))))

(defun efrit-context--full-compression (work-log)
  "Full compression - include all context with smart truncation."
  (let ((compressed-entries
         (mapcar (lambda (entry)
                  (let ((result (car entry))
                        (code (cdr entry)))
                    `((code . ,code)
                      (result . ,(efrit-context--compress-result result))
                      (type . ,(efrit-context--classify-code code)))))
                work-log)))
    (json-encode
     `((entries . ,compressed-entries)
       (context . ,(efrit-context--get-current-context))))))

;;; Compression Helpers

(defun efrit-context--compress-code (code)
  "Compress CODE string for context."
  (cond
   ;; Keep short code as-is
   ((< (length code) 80) code)
   ;; Compress long buffer operations
   ((string-match "\\(buffer-substring[^[:space:]]*\\|insert\\|delete-region\\)" code)
    (concat "(" (match-string 1 code) " ...)"))
   ;; Truncate very long code
   (t (efrit-common-truncate-string code 97))))

(defun efrit-context--compress-result (result)
  "Compress RESULT for context usage."
  (let ((result-str (format "%s" result)))
    (cond
     ;; Handle nil/empty
     ((or (null result) (string-empty-p result-str)) "nil")
     ;; Handle buffer names
     ((string-match "^#<buffer \\(.+\\)>$" result-str)
      (format "buffer:%s" (match-string 1 result-str)))
     ;; Handle long strings
     ((> (length result-str) efrit-context-max-result-length)
      (concat (efrit-common-truncate-string 
               result-str 
               (- efrit-context-max-result-length 3))
              "..."))
     ;; Keep short results as-is
     (t result-str))))

(defun efrit-context--classify-code (code)
  "Classify CODE into operation type for context."
  (cond
   ((string-match "eval-sexp\\|funcall\\|apply" code) 'evaluation)
   ((string-match "insert\\|delete\\|replace" code) 'text-modification)
   ((string-match "find-file\\|switch-to-buffer" code) 'navigation)
   ((string-match "shell-command\\|call-process" code) 'external-command)
   ((string-match "create-buffer\\|generate-new-buffer" code) 'buffer-creation)
   (t 'other)))

;;; Context State Management

(defun efrit-context-capture-state ()
  "Capture current Emacs state for context."
  (make-efrit-context-state
   :buffer-name (buffer-name)
   :buffer-mode major-mode
   :point (point)
   :mark (mark t)
   :window-config (current-window-configuration)
   :directory default-directory
   :timestamp (current-time)
   :metadata (list :read-only buffer-read-only
                   :modified (buffer-modified-p)
                   :size (buffer-size))))

(defun efrit-context-restore-state (state)
  "Restore Emacs state from STATE object."
  (when (efrit-context-state-p state)
    ;; Restore window configuration
    (when (efrit-context-state-window-config state)
      (set-window-configuration (efrit-context-state-window-config state)))
    
    ;; Restore buffer if it exists
    (when-let* ((buffer-name (efrit-context-state-buffer-name state))
                (buffer (get-buffer buffer-name)))
      (switch-to-buffer buffer)
      
      ;; Restore point and mark
      (goto-char (or (efrit-context-state-point state) (point)))
      (when (efrit-context-state-mark state)
        (set-mark (efrit-context-state-mark state))))))

;;; Context Information Extraction

(defun efrit-context--get-current-context ()
  "Get current context information for Claude."
  (let ((context '()))
    ;; Buffer information
    (when efrit-context-include-buffer-state
      (push `(buffer . ((name . ,(buffer-name))
                       (mode . ,(symbol-name major-mode))
                       (size . ,(buffer-size))
                       (modified . ,(buffer-modified-p))))
            context))
    
    ;; Selection information
    (when (use-region-p)
      (push `(selection . ((start . ,(region-beginning))
                          (end . ,(region-end))
                          (length . ,(- (region-end) (region-beginning)))))
            context))
    
    ;; Directory information
    (push `(directory . ,default-directory) context)
    
    ;; Recent buffers - optimized to avoid full buffer list generation
    (let ((recent-buffers '())
          (count 0))
      (catch 'done
        (dolist (buf (buffer-list))
          (push (buffer-name buf) recent-buffers)
          (setq count (1+ count))
          (when (>= count 5)
            (throw 'done nil))))
      (push `(recent-buffers . ,(nreverse recent-buffers))
            context))
    
    context))

;;; Session Context API

(defun efrit-context-create-entry (code result &optional error-p)
  "Create a context entry for CODE and RESULT.
If ERROR-P is non-nil, marks this as an error result."
  (let ((start-time (current-time)))
    (make-efrit-context-entry
     :code code
     :result result
     :state (efrit-context-capture-state)
     :duration (float-time (time-since start-time))
     :error-p error-p)))

(defun efrit-context-format-for-claude (entries)
  "Format context ENTRIES for Claude consumption.
Returns a string suitable for inclusion in system prompts."
  (let ((compressed (efrit-context-compress-work-log entries)))
    (format "Work Log Context:\n%s\n\nCurrent State:\n%s"
            compressed
            (json-encode (efrit-context--get-current-context)))))

;;; Integration Helpers

(defun efrit-context-should-continue-p (work-log command)
  "Determine if session should continue based on WORK-LOG and original COMMAND.
This is a heuristic that can help Claude decide, but Claude has final say."
  (cond
   ;; No work done yet - definitely continue
   ((null work-log) t)
   
   ;; Check for explicit completion markers
   ((seq-some (lambda (entry)
               (string-match "SESSION-COMPLETE\\|task.*complete" 
                           (format "%s" (car entry))))
             work-log)
    nil)
   
   ;; Check for reasonable stopping points
   ((and (> (length work-log) 10)
         (not (string-match "step\\|phase\\|continue" command)))
    nil)
   
   ;; Default to continue
   (t t)))

;;; Command History Ring

(defcustom efrit-context-ring-size 10
  "Size of the command history ring."
  :type 'integer
  :group 'efrit-context)

(defcustom efrit-context-persist-file nil
  "File to persist context data.
If nil, uses the default location in the efrit data directory."
  :type '(choice (const :tag "Default location" nil)
                 (file :tag "Custom file"))
  :group 'efrit-context)

(cl-defstruct (efrit-context-item
            (:constructor efrit-context-item-create)
            (:type vector))
  "Context item structure for command history."
  timestamp
  command
  result
  buffer
  directory
  window-config
  metadata)

(defvar efrit-context--ring nil
  "Ring buffer for context items.")

(defvar efrit-context--hooks nil
  "Hooks run when context is captured.")

(defun efrit-context-ring-init ()
  "Initialize or reinitialize the context ring."
  (unless efrit-context--ring
    (setq efrit-context--ring (make-ring efrit-context-ring-size))))

(defun efrit-context-ring-add (command result &optional metadata)
  "Add a new context item for COMMAND with RESULT and optional METADATA."
  (efrit-context-ring-init)
  (let ((item (efrit-context-item-create
               :timestamp (current-time)
               :command command
               :result result
               :buffer (buffer-name)
               :directory default-directory
               :window-config (current-window-configuration)
               :metadata metadata)))
    (ring-insert efrit-context--ring item)
    (run-hook-with-args 'efrit-context--hooks item)
    item))

(defun efrit-context-ring-get-recent (&optional n)
  "Get N most recent context items (default all)."
  (when efrit-context--ring
    (let ((ring efrit-context--ring)
          (count (or n (ring-length efrit-context--ring)))
          (items '()))
      (dotimes (i (min count (ring-length ring)))
        (push (ring-ref ring i) items))
      (nreverse items))))

(defun efrit-context-item-to-string (item)
  "Convert context ITEM to string representation."
  (format "[%s] %s -> %s (in %s)"
          (format-time-string "%H:%M:%S" (efrit-context-item-timestamp item))
          (efrit-context-item-command item)
          (truncate-string-to-width (or (efrit-context-item-result item) "") 50 nil nil t)
          (efrit-context-item-buffer item)))

(defun efrit-context-ring-clear ()
  "Clear the context ring."
  (when efrit-context--ring
    (setq efrit-context--ring (make-ring efrit-context-ring-size))))

(defun efrit-context-ring-persist ()
  "Persist context ring to file."
  (when efrit-context--ring
    (require 'efrit-config)
    (let ((file (or efrit-context-persist-file 
                   (efrit-config-context-file "efrit-context-ring.el")))
          (items (efrit-context-ring-get-recent)))
      (when items
        (with-temp-file file
          (insert ";; Efrit context ring data\n")
          (insert ";; Saved: " (current-time-string) "\n\n")
          (prin1 items (current-buffer)))
        (efrit-log 'debug "Persisted %d context items to %s" (length items) file)))))

(defun efrit-context-ring-restore ()
  "Restore context ring from file."
  (require 'efrit-config)
  (let ((file (or efrit-context-persist-file
                 (efrit-config-context-file "efrit-context-ring.el"))))
    (when (file-exists-p file)
      (condition-case err
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (search-forward "\n\n" nil t)
            (let ((items (read (current-buffer))))
              (efrit-context-ring-init)
              (dolist (item (reverse items))
                (ring-insert efrit-context--ring item))
              (efrit-log 'debug "Restored %d context items from %s" 
                        (length items) file)))
        (error
         (efrit-log 'warn "Failed to restore context: %s" 
                   (efrit-common-safe-error-message err)))))))

;;; Public API

(defun efrit-context-init ()
  "Initialize context system."
  (efrit-context-ring-init)
  (efrit-context-ring-restore)
  (efrit-log 'debug "Context system initialized"))

(defun efrit-context-reset ()
  "Reset context system state."
  (efrit-context-ring-clear)
  (efrit-log 'debug "Context system reset"))

(provide 'efrit-context)
;;; efrit-context.el ends here