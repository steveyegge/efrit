;;; efrit-chat-transparency.el --- Transparency features for Efrit chat -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.4.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai
;; URL: https://github.com/steveyegge/efrit

;;; Commentary:
;; Transparency features for Efrit chat:
;; - Show tool calls as Claude makes them
;; - Display incremental responses (streaming-like effect)
;; - Show reasoning/thinking process
;; - Better visual hierarchy for different content types

;;; Code:

(require 'efrit-config)

;; Declare functions to avoid warnings
(declare-function efrit--setup-buffer "efrit-chat-buffer")

;; Faces for different content types

(defface efrit-tool-call-face
  '((t :inherit font-lock-function-name-face :weight bold))
  "Face for tool call names."
  :group 'efrit)

(defface efrit-tool-args-face
  '((t :inherit font-lock-variable-name-face))
  "Face for tool arguments."
  :group 'efrit)

(defface efrit-tool-result-face
  '((t :inherit font-lock-doc-face))
  "Face for tool results."
  :group 'efrit)

(defface efrit-thinking-face
  '((t :inherit font-lock-comment-face :slant italic))
  "Face for thinking/reasoning content."
  :group 'efrit)

;; Customization

(defcustom efrit-show-tool-calls t
  "Whether to show tool calls as Claude executes them."
  :type 'boolean
  :group 'efrit)

(defcustom efrit-show-tool-results t
  "Whether to show tool execution results in chat."
  :type 'boolean
  :group 'efrit)

(defcustom efrit-incremental-responses t
  "Whether to display responses incrementally (streaming-like).
When nil, responses appear all at once as before."
  :type 'boolean
  :group 'efrit)

(defcustom efrit-response-chunk-size 50
  "Number of characters to display at once in incremental responses.
Smaller values feel more like streaming, larger values are faster."
  :type 'integer
  :group 'efrit)

(defcustom efrit-incremental-delay 0.01
  "Delay in seconds between response chunks (default 10ms).
Set to 0 for instant display with incremental splitting."
  :type 'number
  :group 'efrit)

;;; Display Functions

(defun efrit-transparency--display-tool-call (tool-name tool-input)
  "Display a tool call that Claude is about to make.
TOOL-NAME is the name of the tool.
TOOL-INPUT is the input parameters (as alist)."
  (when efrit-show-tool-calls
    (require 'efrit-chat-buffer)
    (let ((input-str (format "%S" tool-input)))
      (with-current-buffer (efrit--setup-buffer)
        (setq buffer-read-only nil)
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (unless (bobp) (insert "\n"))
          
          ;; Display tool call header
          (insert (format "  → Calling tool: "))
          (let ((name-start (point)))
            (insert tool-name)
            (add-text-properties name-start (point) '(face efrit-tool-call-face)))
          (insert "\n")
          
          ;; Display input parameters (truncated if too long)
          (let* ((max-input-display 200)
                 (display-input (if (> (length input-str) max-input-display)
                                   (concat (substring input-str 0 max-input-display) "...")
                                 input-str)))
            (insert (format "    Input: %s\n" display-input)))
          
          ;; Update conversation marker
          (require 'efrit-chat-buffer)
          (when (boundp 'efrit--conversation-marker)
            (set-marker efrit--conversation-marker (point))))))))

(defun efrit-transparency--display-tool-result (_tool-name result)
  "Display the result of a tool execution.
_TOOL-NAME is the name of the tool that was executed (unused).
RESULT is the result returned by the tool."
  (when efrit-show-tool-results
    (require 'efrit-chat-buffer)
    (with-current-buffer (efrit--setup-buffer)
      (setq buffer-read-only nil)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        
        ;; Format result for display
        (let* ((result-str (if (stringp result)
                             result
                           (format "%S" result)))
               (max-result-display 300)
               (display-result (if (> (length result-str) max-result-display)
                                 (concat (substring result-str 0 max-result-display) "\n    [... truncated ...]")
                               result-str)))
          
          ;; Display result
          (insert (format "    Result: "))
          (let ((content-start (point)))
            (insert display-result)
            (add-text-properties content-start (point) '(face efrit-tool-result-face)))
          (insert "\n"))
        
        ;; Update conversation marker
        (when (boundp 'efrit--conversation-marker)
          (set-marker efrit--conversation-marker (point)))))))

(defun efrit-transparency--display-incremental (text)
  "Display TEXT response incrementally (streaming-like).
If `efrit-incremental-responses' is nil, displays all at once."
  (if (not efrit-incremental-responses)
      ;; Display all at once (legacy behavior)
      (progn
        (require 'efrit-chat-buffer)
        (with-current-buffer (efrit--setup-buffer)
          (setq buffer-read-only nil)
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (unless (bobp) (insert "\n"))
            
            (let ((start (point)))
              (insert (format "Assistant: %s\n" text))
              (add-text-properties start (- (point) 1) '(face efrit-assistant-face)))
            
            (when (boundp 'efrit--conversation-marker)
              (set-marker efrit--conversation-marker (point))))))
    
    ;; Incremental display (new streaming-like behavior)
    (require 'efrit-chat-buffer)
    (let ((pos 0)
          (total-len (length text)))
      (with-current-buffer (efrit--setup-buffer)
        (setq buffer-read-only nil)
        (let ((inhibit-read-only t))
          ;; Insert initial "Assistant:" prefix once
          (goto-char (point-max))
          (unless (bobp) (insert "\n"))
          
          (let ((prefix-start (point)))
            (insert "Assistant: ")
            (add-text-properties prefix-start (point) '(face efrit-assistant-face)))
          
          (let ((content-start (point)))
            ;; Display text in chunks
            (while (< pos total-len)
              (let* ((chunk-end (min (+ pos efrit-response-chunk-size) total-len))
                     (chunk (substring text pos chunk-end)))
                (insert chunk)
                (setq pos chunk-end)
                
                ;; Redisplay to show incremental progress
                (redisplay t)
                
                ;; Add delay between chunks (if configured)
                (when (> efrit-incremental-delay 0)
                  (sleep-for efrit-incremental-delay)))
              
              ;; Apply face to all displayed content
              (add-text-properties content-start (point) '(face efrit-assistant-face)))
            
            ;; Final newline
            (insert "\n")
            
            ;; Update conversation marker
            (when (boundp 'efrit--conversation-marker)
              (set-marker efrit--conversation-marker (point)))))))))

(defun efrit-transparency--display-thinking (thinking-text)
  "Display THINKING-TEXT as visible reasoning/thinking process."
  (require 'efrit-chat-buffer)
  (with-current-buffer (efrit--setup-buffer)
    (setq buffer-read-only nil)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (unless (bobp) (insert "\n"))
      
      (let ((start (point)))
        (insert (format "💭 Thinking: %s\n" thinking-text))
        (add-text-properties start (- (point) 1) '(face efrit-thinking-face)))
      
      (when (boundp 'efrit--conversation-marker)
        (set-marker efrit--conversation-marker (point))))))

(provide 'efrit-chat-transparency)

;;; efrit-chat-transparency.el ends here
