;;; efrit-agent-input.el --- Input handling for efrit-agent -*- lexical-binding: t -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.4.1
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
(require 'efrit-log)
(require 'efrit-agent-core)
(require 'efrit-agent-render)
(require 'efrit-session)
(require 'efrit-session-worklog)
(require 'efrit-session-persist)
(require 'efrit-repl-session)
(require 'efrit-repl-loop)

;; Forward declarations
(declare-function efrit-executor-respond "efrit-executor")
(declare-function efrit-agent-set-status "efrit-agent")
(declare-function efrit-do--start-async-session "efrit-do")
(declare-function efrit-session-id "efrit-session")
(declare-function efrit-agent--begin-session "efrit-agent-core")

;;; REPL Session State
;;
;; Each agent buffer has a persistent REPL session that accumulates
;; conversation context across multiple inputs.

(defvar-local efrit-agent--repl-session nil
  "The persistent REPL session for this agent buffer.
Unlike efrit-session which completes after each command, this session
persists and accumulates conversation context.")

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
    ;; History navigation
    (define-key map (kbd "M-p") #'efrit-agent-input-history-prev)
    (define-key map (kbd "M-n") #'efrit-agent-input-history-next)
    ;; Completion
    (define-key map (kbd "TAB") #'completion-at-point)
    map)
  "Keymap for `efrit-agent-input-mode'.")

(define-minor-mode efrit-agent-input-mode
  "Minor mode for editing input in the efrit-agent buffer.
Activates when point is in the input region, providing a separate
keymap for input editing.

Key bindings:
\\{efrit-agent-input-mode-map}"
  :lighter " Input"
  :keymap efrit-agent-input-mode-map
  (when efrit-agent-input-mode
    ;; Set up completion when mode is enabled
    (efrit-agent--setup-completion)))

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
  "Send the current input using the persistent REPL session model.
Conversation context accumulates across inputs - Claude remembers
what was discussed previously.

If the REPL session is idle, continues with the new input.
If the REPL session is working, queues the input (not yet implemented).
If no REPL session exists, creates one automatically."
  (interactive)
  (let ((input (efrit-agent--get-input)))
    (if (or (null input) (string-empty-p (string-trim input)))
        (message "Nothing to send")
      ;; Add to history before clearing
      (efrit-agent--add-to-history input)
      ;; Reset history navigation state
      (efrit-agent--reset-history-navigation)
      ;; Add user message to conversation display
      (efrit-agent--add-user-message input)
      ;; Clear the input
      (efrit-agent--clear-input)
      ;; Move point to input area
      (when (and efrit-agent--input-start
                 (marker-position efrit-agent--input-start))
        (goto-char efrit-agent--input-start))

      ;; Use REPL session model
      (efrit-agent--repl-send input))))

(defun efrit-agent--repl-send (input)
  "Send INPUT to the REPL session.
Creates a new REPL session if needed, otherwise continues the existing one."
  ;; Ensure we have a REPL session
  (unless efrit-agent--repl-session
    (setq efrit-agent--repl-session (efrit-repl-session-create default-directory))
    (setf (efrit-repl-session-buffer efrit-agent--repl-session) (current-buffer)))

  (let ((session efrit-agent--repl-session))
    (pcase (efrit-repl-session-status session)
      ;; Idle - continue with new input
      ('idle
       (efrit-repl-continue session input
                            #'efrit-agent--on-turn-complete)
       (message "Efrit: continuing conversation"))

      ;; Paused - resume and continue
      ('paused
       (efrit-repl-session-resume session)
       (efrit-repl-continue session input
                            #'efrit-agent--on-turn-complete)
       (message "Efrit: resumed and continuing"))

      ;; Working - can't send right now
      ('working
       (message "Efrit: session is busy, please wait")
       ;; TODO: Queue input for later
       )

      ;; Waiting for specific input (question)
      ('waiting
       ;; Handle question response
       (when (efrit-repl-session-pending-question session)
         (setf (efrit-repl-session-pending-question session) nil))
       (efrit-repl-continue session input
                            #'efrit-agent--on-turn-complete)
       (message "Efrit: response sent"))

      ;; Unknown state
      (_
       (message "Efrit: unknown session state, resetting")
       (efrit-repl-session-reset session)
       (efrit-repl-continue session input
                            #'efrit-agent--on-turn-complete)))))

(defun efrit-agent--on-turn-complete (session stop-reason)
  "Callback when a REPL turn completes.
SESSION is the REPL session, STOP-REASON indicates why the turn ended."
  (efrit-log 'debug "REPL turn complete: %s" stop-reason)
  ;; Update agent buffer status based on stop reason
  (when (fboundp 'efrit-agent-set-status)
    (efrit-agent-set-status
     (pcase stop-reason
       ("end_turn" 'idle)
       ("session-complete" 'idle)
       ("paused" 'paused)
       (_ 'failed))))
  ;; Reset prompt if we were waiting
  (efrit-agent--reset-input-prompt)
  ;; Auto-save session if enabled
  (when (and efrit-session-persist-auto-save session)
    (efrit-agent--auto-save-session session)))

(defun efrit-agent--auto-save-session (session)
  "Auto-save SESSION to disk.
Saves asynchronously to avoid blocking the UI."
  (condition-case err
      (when (efrit-session-persist-save session)
        (efrit-log 'debug "Auto-saved session %s" (efrit-repl-session-id session)))
    (error
     (efrit-log 'error "Auto-save failed: %s" (error-message-string err)))))

(defun efrit-agent-input-clear ()
  "Clear the current input."
  (interactive)
  (efrit-agent--clear-input)
  (when (and efrit-agent--input-start
             (marker-position efrit-agent--input-start))
    (goto-char efrit-agent--input-start))
  (message "Input cleared"))

;;; REPL Session Commands

(defun efrit-agent-new-conversation ()
  "Start a fresh conversation, clearing the REPL session context.
The conversation display is cleared and a new REPL session is created."
  (interactive)
  (when efrit-agent--repl-session
    (efrit-repl-session-reset efrit-agent--repl-session))
  ;; Clear conversation display
  (let ((inhibit-read-only t))
    (when (and efrit-agent--conversation-end
               (marker-position efrit-agent--conversation-end))
      (delete-region (point-min) efrit-agent--conversation-end)
      (goto-char (point-min))
      (insert "\n")
      (set-marker efrit-agent--conversation-end (point))))
  ;; Reset status
  (setq efrit-agent--status 'idle)
  (when (fboundp 'efrit-agent-set-status)
    (efrit-agent-set-status 'idle))
  (message "Efrit: started new conversation"))

(defun efrit-agent-pause ()
  "Pause the current REPL session."
  (interactive)
  (when efrit-agent--repl-session
    (efrit-repl-session-pause efrit-agent--repl-session)
    (message "Efrit: session paused")))

(defun efrit-agent-session-info ()
  "Display information about the current REPL session."
  (interactive)
  (if (null efrit-agent--repl-session)
      (message "No active REPL session")
    (let ((session efrit-agent--repl-session))
      (message "REPL Session: %s | Status: %s | Turns: %d | Elapsed: %s"
               (efrit-repl-session-id session)
               (efrit-repl-session-status session)
               (efrit-repl-session-turn-count session)
               (efrit-repl-session--format-elapsed session)))))

;;; Input History
;;
;; Input history supports M-p/M-n navigation through previous inputs.
;; History is maintained both per-session and globally (persisted across restarts).

(defun efrit-agent--combined-history ()
  "Return combined history list (session + global, deduplicated).
Session history takes precedence (appears first)."
  (let ((seen (make-hash-table :test 'equal))
        (result nil))
    ;; Add session history first
    (dolist (item efrit-agent--input-history)
      (unless (gethash item seen)
        (puthash item t seen)
        (push item result)))
    ;; Add global history (items not already seen)
    (dolist (item efrit-agent--global-history)
      (unless (gethash item seen)
        (puthash item t seen)
        (push item result)))
    (nreverse result)))

(defun efrit-agent--set-input (text)
  "Set the input region to TEXT."
  (when (and efrit-agent--input-start
             (marker-position efrit-agent--input-start))
    (let ((inhibit-read-only t))
      (efrit-agent--clear-input)
      (save-excursion
        (goto-char efrit-agent--input-start)
        (insert text))
      (goto-char (point-max)))))

(defun efrit-agent-input-history-prev ()
  "Navigate to previous input in history.
First M-p saves current input and shows most recent history.
Subsequent M-p moves further back in history."
  (interactive)
  (let ((history (efrit-agent--combined-history)))
    (if (null history)
        (message "No input history")
      (let ((max-idx (1- (length history))))
        ;; If starting navigation, save current input
        (when (= efrit-agent--history-index -1)
          (setq efrit-agent--history-temp (efrit-agent--get-input)))
        ;; Move to previous (older) entry
        (if (>= efrit-agent--history-index max-idx)
            (message "End of history")
          (cl-incf efrit-agent--history-index)
          (efrit-agent--set-input (nth efrit-agent--history-index history))
          (message "History: %d/%d" (1+ efrit-agent--history-index) (length history)))))))

(defun efrit-agent-input-history-next ()
  "Navigate to next (more recent) input in history.
When at the most recent entry, restores what user was typing."
  (interactive)
  (if (< efrit-agent--history-index 0)
      (message "No more history")
    (let ((history (efrit-agent--combined-history)))
      (cl-decf efrit-agent--history-index)
      (if (< efrit-agent--history-index 0)
          ;; Restore the original input the user was typing
          (progn
            (efrit-agent--set-input (or efrit-agent--history-temp ""))
            (setq efrit-agent--history-temp nil)
            (message "End of history (restored input)"))
        (efrit-agent--set-input (nth efrit-agent--history-index history))
        (message "History: %d/%d" (1+ efrit-agent--history-index) (length history))))))

(defun efrit-agent--add-to-history (input)
  "Add INPUT to both session and global history.
Deduplicates by removing any existing identical entry."
  (let ((trimmed (string-trim input)))
    (unless (string-empty-p trimmed)
      ;; Add to session history (remove duplicates first)
      (setq efrit-agent--input-history
            (cons trimmed (delete trimmed efrit-agent--input-history)))
      ;; Trim session history to max size
      (when (> (length efrit-agent--input-history) efrit-agent-history-max-size)
        (setq efrit-agent--input-history
              (seq-take efrit-agent--input-history efrit-agent-history-max-size)))
      ;; Add to global history (remove duplicates first)
      (setq efrit-agent--global-history
            (cons trimmed (delete trimmed efrit-agent--global-history)))
      ;; Trim global history to max size
      (when (> (length efrit-agent--global-history) efrit-agent-history-max-size)
        (setq efrit-agent--global-history
              (seq-take efrit-agent--global-history efrit-agent-history-max-size)))
      ;; Save global history to file
      (efrit-agent--save-history))))

(defun efrit-agent--reset-history-navigation ()
  "Reset history navigation state.
Called after sending input to exit history navigation mode."
  (setq efrit-agent--history-index -1)
  (setq efrit-agent--history-temp nil))

;;; History Persistence

(defun efrit-agent--save-history ()
  "Save global history to `efrit-agent-history-file'."
  (when (and efrit-agent-history-file efrit-agent--global-history)
    (condition-case err
        (with-temp-file efrit-agent-history-file
          (insert ";; Efrit Agent Input History\n")
          (insert ";; Do not edit manually\n")
          (pp efrit-agent--global-history (current-buffer)))
      (error
       (message "Failed to save efrit-agent history: %s" (error-message-string err))))))

(defun efrit-agent--load-history ()
  "Load global history from `efrit-agent-history-file'."
  (when (and efrit-agent-history-file
             (file-exists-p efrit-agent-history-file))
    (condition-case err
        (with-temp-buffer
          (insert-file-contents efrit-agent-history-file)
          (goto-char (point-min))
          ;; Skip comment lines
          (while (looking-at "^;")
            (forward-line 1))
          (setq efrit-agent--global-history (read (current-buffer))))
      (error
       (message "Failed to load efrit-agent history: %s" (error-message-string err))
       (setq efrit-agent--global-history nil)))))

(defun efrit-agent--maybe-enable-input-mode ()
  "Enable or disable input mode based on point position.
Called from `post-command-hook'."
  (if (efrit-agent--in-input-region-p)
      (unless efrit-agent-input-mode
        (efrit-agent-input-mode 1))
    (when efrit-agent-input-mode
      (efrit-agent-input-mode -1))))

;;; Context-Aware Completion
;;
;; Provides intelligent completion in the input region based on conversation context:
;; - File paths mentioned in the conversation
;; - Options from pending questions (1, 2, 3, 4)
;; - Common response patterns

(defvar-local efrit-agent--context-files nil
  "List of file paths extracted from the conversation.
Updated when new tool calls reference files.")

(defun efrit-agent--extract-path-from-input (input)
  "Extract file path from tool INPUT if present.
INPUT can be a plist or alist with :path, path, :file_path, or file_path keys."
  (when input
    (or (plist-get input :path)
        (plist-get input :file_path)
        (and (listp input)
             (or (cdr (assoc 'path input))
                 (cdr (assoc :path input))
                 (cdr (assoc 'file_path input))
                 (cdr (assoc :file_path input)))))))

(defun efrit-agent--extract-file-paths ()
  "Extract file paths mentioned in the conversation.
Returns a list of file path strings found in tool calls."
  (let ((files nil)
        (seen-ids (make-hash-table :test 'equal)))
    ;; Scan buffer for tool-call text properties with input containing paths
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
        (let ((id (get-text-property (point) 'efrit-id))
              (type (get-text-property (point) 'efrit-type))
              (input (get-text-property (point) 'efrit-tool-input)))
          ;; Only process each unique ID once
          (when (and id (not (gethash id seen-ids)))
            (puthash id t seen-ids)
            (when (and (eq type 'tool-call) input)
              (when-let* ((path (efrit-agent--extract-path-from-input input)))
                (push path files)))))
        ;; Move to next property change (by ID to catch all tool calls)
        (goto-char (or (next-single-property-change (point) 'efrit-id)
                       (point-max)))))
    ;; Remove duplicates, return most recent first
    (delete-dups (nreverse files))))

(defun efrit-agent--get-completion-candidates ()
  "Get all completion candidates based on current context.
Returns a list of strings for completion."
  (let ((candidates nil))
    ;; Option numbers if waiting for question response
    (when (and (eq efrit-agent--status 'waiting)
               efrit-agent--pending-question)
      (let ((options (cadr efrit-agent--pending-question)))
        (when options
          ;; Add option numbers
          (dotimes (i (min 4 (length options)))
            (push (number-to-string (1+ i)) candidates))
          ;; Add actual option text
          (dolist (opt options)
            (push opt candidates)))))
    ;; File paths from conversation
    (dolist (path (efrit-agent--extract-file-paths))
      (push path candidates))
    ;; Common response patterns
    (push "yes" candidates)
    (push "no" candidates)
    (push "continue" candidates)
    (push "cancel" candidates)
    (push "skip" candidates)
    ;; Return unique candidates
    (delete-dups candidates)))

(defun efrit-agent--completion-at-point ()
  "Completion-at-point function for efrit-agent input.
Provides context-aware completions based on conversation."
  (when (efrit-agent--in-input-region-p)
    (let* ((end (point))
           (start (save-excursion
                    (skip-chars-backward "^ \t\n")
                    (point)))
           (prefix (buffer-substring-no-properties start end))
           (candidates (efrit-agent--get-completion-candidates)))
      (when (and candidates (>= (length prefix) 0))
        (list start end
              (completion-table-dynamic
               (lambda (_)
                 (cl-remove-if-not
                  (lambda (c) (string-prefix-p prefix c t))
                  candidates)))
              :exclusive 'no)))))

(defun efrit-agent--setup-completion ()
  "Set up completion for the input region."
  (add-hook 'completion-at-point-functions
            #'efrit-agent--completion-at-point
            nil t))

(provide 'efrit-agent-input)

;;; efrit-agent-input.el ends here
