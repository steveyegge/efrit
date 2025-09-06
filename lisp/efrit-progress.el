;;; efrit-progress.el --- Progress display for Efrit operations -*- lexical-binding: t -*-

;; Copyright (C) 2025 Steven Yegge

;; Author: Steven Yegge
;; Version: 0.1
;; Package-Requires: ((emacs "28.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This module provides real-time progress display for Efrit operations,
;; showing tool invocations, Claude's messages, file changes, and command
;; outputs in a dedicated buffer similar to Claude Code's output.

;;; Code:

(require 'efrit-common)
(require 'efrit-log)
(require 'ansi-color)

;;; Customization

(defgroup efrit-progress nil
  "Progress display for Efrit."
  :group 'efrit)

(defcustom efrit-progress-buffer-name "*Efrit Progress*"
  "Name of the buffer for displaying progress."
  :type 'string
  :group 'efrit-progress)

(defcustom efrit-progress-auto-show t
  "Whether to automatically show progress buffer when operations start."
  :type 'boolean
  :group 'efrit-progress)

(defcustom efrit-progress-verbosity 'normal
  "Verbosity level for progress display.
- minimal: Only show major operations
- normal: Show operations and key results  
- verbose: Show all details including raw responses"
  :type '(choice (const :tag "Minimal" minimal)
                 (const :tag "Normal" normal)
                 (const :tag "Verbose" verbose))
  :group 'efrit-progress)

(defcustom efrit-progress-timestamp-format "%H:%M:%S"
  "Format for timestamps in progress display."
  :type 'string
  :group 'efrit-progress)

(defcustom efrit-progress-auto-shrink t
  "When non-nil, progress buffer automatically shrinks to fit content."
  :type 'boolean
  :group 'efrit-progress)

;;; Faces

(defface efrit-progress-timestamp
  '((t :foreground "gray60"))
  "Face for timestamps in progress display."
  :group 'efrit-progress)

(defface efrit-progress-section-header
  '((t :weight bold :foreground "DeepSkyBlue"))
  "Face for section headers in progress display."
  :group 'efrit-progress)

(defface efrit-progress-tool-name
  '((t :weight bold :foreground "DarkOrange"))
  "Face for tool names in progress display."
  :group 'efrit-progress)

(defface efrit-progress-success
  '((t :foreground "green3"))
  "Face for success messages in progress display."
  :group 'efrit-progress)

(defface efrit-progress-error
  '((t :foreground "red3"))
  "Face for error messages in progress display."
  :group 'efrit-progress)

(defface efrit-progress-claude-message
  '((t :foreground "RoyalBlue"))
  "Face for Claude's messages in progress display."
  :group 'efrit-progress)

(defface efrit-progress-diff-added
  '((t :foreground "green4" :background "honeydew1"))
  "Face for added lines in diffs."
  :group 'efrit-progress)

(defface efrit-progress-diff-removed
  '((t :foreground "red4" :background "mistyrose1"))
  "Face for removed lines in diffs."
  :group 'efrit-progress)

;;; Progress Display Functions

(defvar efrit-progress--current-session nil
  "Current session being displayed.")

(defun efrit-progress--get-buffer ()
  "Get or create the progress buffer."
  (let ((buffer (get-buffer-create efrit-progress-buffer-name)))
    (with-current-buffer buffer
      (unless (eq major-mode 'efrit-progress-mode)
        (efrit-progress-mode)))
    buffer))

(defun efrit-progress--timestamp ()
  "Return formatted timestamp."
  (propertize (format-time-string efrit-progress-timestamp-format)
              'face 'efrit-progress-timestamp))

(defun efrit-progress--append (text &optional face)
  "Append TEXT to progress buffer with optional FACE."
  (let ((buffer (efrit-progress--get-buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)
            (was-at-end (= (point) (point-max))))
        (goto-char (point-max))
        (insert (efrit-progress--timestamp) " ")
        (insert (if face (propertize text 'face face) text))
        (insert "\n")
        (when was-at-end
          (goto-char (point-max))
          (when (get-buffer-window buffer)
            (set-window-point (get-buffer-window buffer) (point))))))))

(defun efrit-progress--append-section (header content)
  "Append a section with HEADER and CONTENT."
  (efrit-progress--append header 'efrit-progress-section-header)
  (efrit-progress--append content))

;;; Public API

(defun efrit-progress-start-session (session-id command)
  "Start progress tracking for SESSION-ID with COMMAND."
  (setq efrit-progress--current-session session-id)
  (let ((buffer (efrit-progress--get-buffer)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (efrit-progress--append 
     (format "═══ Starting Efrit Session: %s ═══" session-id)
     'efrit-progress-section-header)
    (efrit-progress--append (format "Command: %s\n" command))
    (when efrit-progress-auto-show
      (display-buffer buffer))))

(defun efrit-progress-show-message (message &optional type)
  "Show MESSAGE in progress buffer with optional TYPE.
TYPE can be 'claude, 'error, 'success, or nil."
  (let ((face (pcase type
                ('claude 'efrit-progress-claude-message)
                ('error 'efrit-progress-error)
                ('success 'efrit-progress-success)
                (_ nil))))
    (efrit-progress--append message face)))

(defun efrit-progress-show-tool-start (tool-name input)
  "Show the start of TOOL-NAME execution with INPUT."
  (when (memq efrit-progress-verbosity '(normal verbose))
    (efrit-progress--append 
     (format "▶ Executing tool: %s" tool-name)
     'efrit-progress-tool-name)
    ;; Show meaningful info for TODO tools
    (cond
     ((and (string= tool-name "todo_add") input)
      (let ((content (if (hash-table-p input)
                       (gethash "content" input)
                     input)))
        (when content
          (efrit-progress--append 
           (format "\n  → Adding: %s" content)
           'font-lock-comment-face))))
     ((and (string= tool-name "todo_update") input)
      (let ((id (when (hash-table-p input) (gethash "id" input)))
            (status (when (hash-table-p input) (gethash "status" input))))
        (when (and id status)
          (efrit-progress--append
           (format "\n  → Marking %s as %s" id status)
           'font-lock-comment-face))))
     ((eq efrit-progress-verbosity 'verbose)
      (efrit-progress--append 
       (format "  Input: %s" (efrit-common-truncate-string 
                              (format "%S" input) 200)))))))

(defun efrit-progress-show-tool-result (tool-name result success-p)
  "Show the RESULT of TOOL-NAME execution.
SUCCESS-P indicates if the execution was successful."
  (when (memq efrit-progress-verbosity '(normal verbose))
    (efrit-progress--append 
     (format "◀ %s: %s" tool-name (if success-p "Success" "Failed"))
     (if success-p 'efrit-progress-success 'efrit-progress-error))
    (when (or (not success-p) (eq efrit-progress-verbosity 'verbose))
      (efrit-progress--append 
       (format "  Result: %s" (efrit-common-truncate-string 
                                (format "%S" result) 300))))))

(defun efrit-progress-show-elisp-eval (code result)
  "Show Elisp CODE evaluation and RESULT."
  (when (memq efrit-progress-verbosity '(normal verbose))
    (efrit-progress--append "▶ Evaluating Elisp:" 'efrit-progress-tool-name)
    (efrit-progress--append (format "  %s" code))
    (efrit-progress--append 
     (format "  → %s" (efrit-common-truncate-string 
                       (format "%S" result) 200)))))

(defun efrit-progress-show-file-edit (file old-content new-content)
  "Show FILE edit with OLD-CONTENT and NEW-CONTENT."
  (when (memq efrit-progress-verbosity '(normal verbose))
    (efrit-progress--append 
     (format "📝 Editing file: %s" file) 'efrit-progress-tool-name)
    (when (eq efrit-progress-verbosity 'verbose)
      ;; Simple diff display
      (let ((old-lines (split-string old-content "\n"))
            (new-lines (split-string new-content "\n")))
        (efrit-progress--append "  Changes:")
        (dotimes (i (max (length old-lines) (length new-lines)))
          (let ((old-line (nth i old-lines))
                (new-line (nth i new-lines)))
            (cond
             ((and old-line new-line (not (string= old-line new-line)))
              (efrit-progress--append 
               (format "  - %s" old-line) 'efrit-progress-diff-removed)
              (efrit-progress--append 
               (format "  + %s" new-line) 'efrit-progress-diff-added))
             ((and new-line (not old-line))
              (efrit-progress--append 
               (format "  + %s" new-line) 'efrit-progress-diff-added))
             ((and old-line (not new-line))
              (efrit-progress--append 
               (format "  - %s" old-line) 'efrit-progress-diff-removed)))))))))

(defun efrit-progress-show-command-output (command output exit-code)
  "Show COMMAND execution with OUTPUT and EXIT-CODE."
  (efrit-progress--append 
   (format "$ %s" command) 'efrit-progress-tool-name)
  (when (not (string-empty-p output))
    ;; Apply ANSI color codes if present
    (let ((colored-output (ansi-color-apply output)))
      (dolist (line (split-string colored-output "\n"))
        (unless (string-empty-p line)
          (efrit-progress--append (format "  %s" line))))))
  (when (not (zerop exit-code))
    (efrit-progress--append 
     (format "  Exit code: %d" exit-code) 'efrit-progress-error)))

(defun efrit-progress-end-session (session-id success-p)
  "End progress tracking for SESSION-ID with SUCCESS-P status."
  (efrit-progress--append 
   (format "═══ Session %s: %s ═══" 
           session-id 
           (if success-p "Completed" "Failed"))
   (if success-p 'efrit-progress-success 'efrit-progress-error))
  (setq efrit-progress--current-session nil))

;;; Progress Mode

(defvar efrit-progress-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "q" 'quit-window)
    (define-key map "c" 'efrit-progress-clear)
    (define-key map "v" 'efrit-progress-cycle-verbosity)
    map)
  "Keymap for efrit-progress-mode.")

(define-derived-mode efrit-progress-mode special-mode "Efrit-Progress"
  "Major mode for viewing Efrit operation progress.
\\{efrit-progress-mode-map}"
  (setq-local truncate-lines nil)
  (setq-local word-wrap t))

(defun efrit-progress-clear ()
  "Clear the progress buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defun efrit-progress-cycle-verbosity ()
  "Cycle through verbosity levels."
  (interactive)
  (setq efrit-progress-verbosity
        (pcase efrit-progress-verbosity
          ('minimal 'normal)
          ('normal 'verbose)
          ('verbose 'minimal)))
  (message "Progress verbosity: %s" efrit-progress-verbosity))

(defun efrit-progress-show ()
  "Show the progress buffer with shrink-to-fit."
  (interactive)
  (let* ((buffer (efrit-progress--get-buffer))
         (window (display-buffer buffer
                                (if efrit-progress-auto-shrink
                                    '((display-buffer-reuse-window
                                       display-buffer-below-selected)
                                      (window-height . fit-window-to-buffer)
                                      (window-parameters . ((no-delete-other-windows . t))))
                                  '(display-buffer-reuse-window
                                    display-buffer-below-selected)))))
    (when (and window efrit-progress-auto-shrink)
      (fit-window-to-buffer window nil nil 20 nil))))

(defun efrit-progress-show-todos ()
  "Display current TODOs in progress buffer."
  (require 'efrit-do)
  (when (bound-and-true-p efrit-do--current-todos)
    (let ((buffer (efrit-progress--get-buffer)))
      (with-current-buffer buffer
        (goto-char (point-max))
        (efrit-progress--append "\n━━━ TODO List ━━━\n" 'font-lock-function-name-face)
        (if (null efrit-do--current-todos)
            (efrit-progress--append "No TODOs\n")
          (let ((total (length efrit-do--current-todos))
                (completed (seq-count (lambda (todo)
                                       (eq (efrit-do-todo-item-status todo) 'completed))
                                     efrit-do--current-todos))
                (in-progress (seq-count (lambda (todo)
                                         (eq (efrit-do-todo-item-status todo) 'in-progress))
                                       efrit-do--current-todos)))
            (efrit-progress--append (format "Progress: %d/%d completed, %d in progress\n\n" 
                                          completed total in-progress)
                                  'font-lock-comment-face)
            (dolist (todo efrit-do--current-todos)
              (let* ((status (efrit-do-todo-item-status todo))
                     (icon (pcase status
                             ('todo "☐")
                             ('in-progress "⟳")
                             ('completed "☑")))
                     (face (pcase status
                             ('todo 'default)
                             ('in-progress 'font-lock-warning-face)
                             ('completed 'font-lock-comment-face))))
                (efrit-progress--append (format "%s %s\n" icon 
                                              (efrit-do-todo-item-content todo))
                                      face)))))))))

(defun efrit-progress-update-todo (todo-id new-status)
  "Update TODO display when TODO-ID changes to NEW-STATUS."
  (efrit-progress-show-message 
   (format "TODO %s → %s" todo-id new-status)
   (if (eq new-status 'completed) 'success 'info)))

(provide 'efrit-progress)

;;; efrit-progress.el ends here