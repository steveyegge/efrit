;;; efrit-agent-input.el --- Input handling for efrit-agent -*- lexical-binding: t -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.4.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; Input handling module for efrit-agent providing:
;; - Input minor mode for the editable input region
;; - Question display with option buttons
;; - Input prompt management
;; - History navigation (placeholder)

;;; Code:

(require 'cl-lib)
(require 'efrit-agent-core)
(require 'efrit-agent-render)

;; Forward declarations
(declare-function efrit-executor-respond "efrit-executor")

;;; Question Display

(defun efrit-agent--add-question (question &optional options)
  "Add a QUESTION from Claude to the conversation region.
OPTIONS is an optional list of choices the user can select.
Returns the question ID for tracking responses."
  ;; End any streaming Claude message first
  (efrit-agent--stream-end-message)
  (let* ((q-id (format "question-%d" (cl-incf efrit-agent--message-counter)))
         (formatted-text
          (concat
           ;; Question indicator
           (propertize (format "%s " (efrit-agent--char 'status-waiting))
                       'face 'efrit-agent-timestamp)
           ;; Question text
           (propertize question 'face 'efrit-agent-question)
           "\n"
           ;; Options (if provided)
           (when options
             (concat
              "   "
              (mapconcat
               (lambda (opt-pair)
                 (let* ((idx (car opt-pair))
                        (opt (cdr opt-pair)))
                   (efrit-agent--make-option-button opt idx)))
               (cl-loop for opt in options
                        for idx from 1
                        collect (cons idx opt))
               " ")
              "\n"))
           ;; Hint for keyboard selection
           (when options
             (propertize (format "   Press %s or type custom response\n"
                                 (mapconcat #'number-to-string
                                            (number-sequence 1 (min 4 (length options)))
                                            "/"))
                         'face 'efrit-agent-timestamp))
           "\n")))
    (efrit-agent--append-to-conversation
     formatted-text
     (list 'efrit-type 'question
           'efrit-id q-id
           'efrit-question question
           'efrit-options options))
    ;; Update input prompt to indicate we're waiting
    (efrit-agent--update-input-prompt question options)
    q-id))

(defun efrit-agent--update-input-prompt (_question options)
  "Update the input region prompt for question with OPTIONS.
Shows context about what input is expected."
  (when (marker-position efrit-agent--input-start)
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char efrit-agent--input-start)
        ;; Clear any existing prompt marker
        (when (looking-at "^.*\n")
          (delete-region (point) (match-end 0)))
        ;; Insert new context-aware prompt
        (insert
         (propertize
          (if options
              (format "Answer (or %s): "
                      (mapconcat #'number-to-string
                                 (number-sequence 1 (min 4 (length options)))
                                 "/"))
            "Answer: ")
          'face 'efrit-agent-input-prompt))))))

(defun efrit-agent--reset-input-prompt ()
  "Reset the input region prompt to the default state.
Called after responding to a question."
  (when (marker-position efrit-agent--input-start)
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char efrit-agent--input-start)
        ;; Clear any existing prompt text on this line
        (when (looking-at "^[^\n]*")
          (delete-region (point) (match-end 0)))
        ;; Insert default prompt
        (insert (propertize "> " 'face 'efrit-agent-input-prompt))))))

;;; Option Selection

(defun efrit-agent--select-option (n)
  "Select option N (1-indexed) from pending question options.
Returns nil if no options or N is out of range."
  (when (eq efrit-agent--status 'waiting)
    (let* ((options (cadr efrit-agent--pending-question))
           (option (and options (nth (1- n) options))))
      (when option
        (efrit-executor-respond option)
        (setq efrit-agent--status 'working)
        t))))

;;; Input Minor Mode
;;
;; A minor mode that activates when point is in the input region.
;; Provides a separate keymap for editing input (RET sends, etc.)

(defvar efrit-agent-input-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Sending input
    (define-key map (kbd "RET") #'efrit-agent-input-send-or-newline)
    (define-key map (kbd "S-<return>") #'newline)
    (define-key map (kbd "C-c C-c") #'efrit-agent-input-send)
    (define-key map (kbd "C-c C-s") #'efrit-agent-input-send)
    (define-key map (kbd "C-c C-k") #'efrit-agent-input-clear)
    ;; History navigation (placeholder for future)
    (define-key map (kbd "M-p") #'efrit-agent-input-history-prev)
    (define-key map (kbd "M-n") #'efrit-agent-input-history-next)
    map)
  "Keymap for `efrit-agent-input-mode'.")

(define-minor-mode efrit-agent-input-mode
  "Minor mode for editing input in the efrit-agent buffer.
Activates when point is in the input region, providing a separate
keymap for input editing.

\\{efrit-agent-input-mode-map}"
  :lighter " Input"
  :keymap efrit-agent-input-mode-map)

(defun efrit-agent-input-send-or-newline ()
  "Send input if on a single line, otherwise insert newline.
Use S-RET to always insert a newline."
  (interactive)
  (let ((input (efrit-agent--get-input)))
    (if (and input (not (string-match-p "\n" input)))
        ;; Single line - send it
        (efrit-agent-input-send)
      ;; Multi-line or empty - insert newline
      (newline))))

(defun efrit-agent-input-send ()
  "Send the current input."
  (interactive)
  (let ((input (efrit-agent--get-input)))
    (if (or (null input) (string-empty-p (string-trim input)))
        (message "Nothing to send")
      ;; Add user message to conversation with proper formatting
      (efrit-agent--add-user-message input)
      ;; Clear the input
      (efrit-agent--clear-input)
      ;; Move point to input area
      (goto-char efrit-agent--input-start)
      ;; TODO: Actually send to the executor when wired up
      (message "Input sent: %s" (truncate-string-to-width input 50)))))

(defun efrit-agent-input-clear ()
  "Clear the current input."
  (interactive)
  (efrit-agent--clear-input)
  (goto-char efrit-agent--input-start)
  (message "Input cleared"))

(defun efrit-agent-input-history-prev ()
  "Navigate to previous input in history."
  (interactive)
  (message "Input history not yet implemented"))

(defun efrit-agent-input-history-next ()
  "Navigate to next input in history."
  (interactive)
  (message "Input history not yet implemented"))

(defun efrit-agent--maybe-enable-input-mode ()
  "Enable or disable input mode based on point position.
Called from `post-command-hook'."
  (if (efrit-agent--in-input-region-p)
      (unless efrit-agent-input-mode
        (efrit-agent-input-mode 1))
    (when efrit-agent-input-mode
      (efrit-agent-input-mode -1))))

(provide 'efrit-agent-input)

;;; efrit-agent-input.el ends here
