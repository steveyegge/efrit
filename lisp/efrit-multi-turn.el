;;; efrit-multi-turn.el --- Multi-turn conversation management with Claude completion assessment -*- lexical-binding: t -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 2.0.0
;; Package-Requires: ((emacs "28.1") (cl-lib "0.5"))
;; Keywords: ai, tools, multi-turn, conversation
;; URL: https://github.com/stevey/efrit

;;; Commentary:

;; Multi-turn conversation management for Efrit coding agent.
;; 
;; This module enables automatic conversation continuation for multi-step
;; requests by asking Claude directly whether tasks are complete. This approach
;; is more reliable and language-agnostic than text pattern matching.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'url)

;; Forward declarations
(declare-function efrit--get-api-key "efrit-chat")
(declare-function efrit-log-debug "efrit-debug" (format-string &rest args))

;;; Configuration

(defcustom efrit-multi-turn-max-turns 10
  "Maximum number of turns allowed in a multi-turn conversation."
  :type 'integer
  :group 'efrit)

(defcustom efrit-multi-turn-timeout 300
  "Timeout in seconds for multi-turn conversations."
  :type 'integer
  :group 'efrit)

(defcustom efrit-multi-turn-enabled t
  "Whether multi-turn conversations are enabled."
  :type 'boolean
  :group 'efrit)

;; Use centralized completion model configuration
(require 'efrit-config)
(defvaralias 'efrit-multi-turn-completion-model 'efrit-completion-model)

(defcustom efrit-multi-turn-completion-max-tokens 150
  "Maximum tokens for completion check responses."
  :type 'integer
  :group 'efrit)

(defcustom efrit-multi-turn-api-timeout 10
  "Timeout in seconds for completion check API calls."
  :type 'integer
  :group 'efrit)

(defcustom efrit-multi-turn-simple-max-turns 3
  "Maximum turns for simple heuristic continuation logic."
  :type 'integer
  :group 'efrit)

;;; State Management

(defvar efrit--multi-turn-conversations (make-hash-table :test 'equal)
  "Hash table tracking active multi-turn conversations.")

(defvar efrit--conversation-counter 0
  "Counter for generating unique conversation IDs.")

(cl-defstruct efrit-conversation
  "Structure representing a multi-turn conversation state."
  id                    ; Unique conversation identifier
  original-request      ; The original user request that started this conversation
  current-turn          ; Current turn number (starts at 1)
  max-turns             ; Maximum allowed turns for this conversation
  start-time            ; When this conversation started
  last-activity         ; Timestamp of last activity
  termination-reason    ; Why the conversation ended (if it has)
  context-history       ; History of requests and responses for context
  user-buffer)          ; The buffer where user interaction happens

(defun efrit--generate-conversation-id ()
  "Generate a unique conversation ID."
  (format "conv-%d-%d" 
          (floor (float-time))
          (cl-incf efrit--conversation-counter)))

(defun efrit--create-conversation (original-request &optional user-buffer)
  "Create a new multi-turn conversation state."
  (let* ((conv-id (efrit--generate-conversation-id))
         (conversation (make-efrit-conversation
                       :id conv-id
                       :original-request original-request
                       :current-turn 1
                       :max-turns efrit-multi-turn-max-turns
                       :start-time (current-time)
                       :last-activity (current-time)
                       :termination-reason nil
                       :context-history nil
                       :user-buffer user-buffer)))
                       (puthash conv-id conversation efrit--multi-turn-conversations)
                       conversation))

(defun efrit--update-conversation (conversation)
  "Update conversation state in the hash table."
  (puthash (efrit-conversation-id conversation) 
           conversation 
           efrit--multi-turn-conversations)
  conversation)

(defun efrit--conversation-expired-p (conversation)
  "Check if CONVERSATION has exceeded timeout limits."
  (let ((elapsed (float-time (time-subtract (current-time) 
                                           (efrit-conversation-last-activity conversation)))))
    (require 'efrit-debug)
    (efrit-log-debug "Expiry check: elapsed=%.1fs, timeout=%ds" elapsed efrit-multi-turn-timeout)
    (> elapsed efrit-multi-turn-timeout)))

(defun efrit--conversation-at-max-turns-p (conversation)
  "Check if CONVERSATION has reached maximum turn limit."
  (>= (efrit-conversation-current-turn conversation)
      (efrit-conversation-max-turns conversation)))

;;; Claude-Based Completion Assessment

(defun efrit--ask-claude-about-completion (conversation)
  "Ask Claude whether CONVERSATION should continue.
Returns \\='continue, \\='complete, or \\='error."
  (require 'efrit-debug)
  (efrit-log-debug "Asking Claude about completion...")
  (let* ((original-request (efrit-conversation-original-request conversation))
         (history (efrit-conversation-context-history conversation))
         (context-summary (efrit--summarize-conversation-context history))
         (completion-prompt
          (format "COMPLETION CHECK REQUEST

Original user request: \"%s\"

Conversation so far:
%s

Question: Based on the original request and what has been accomplished so far, should I continue working on this request or is it complete?

Respond with exactly this format:
COMPLETION: [0-100]%%
CONTINUE: [YES/NO]
REASON: [brief explanation]"
                  original-request
                  context-summary)))
    

    
    ;; Make lightweight API call to Claude
    (condition-case nil
        (efrit--send-completion-check-request completion-prompt conversation)
      (error 'error))))

(defun efrit--summarize-conversation-context (history)
  "Create a summary of conversation HISTORY for completion checking."
  (if (null history)
      "No previous turns."
    (mapconcat
     (lambda (entry)
       (format "Turn %d: %s -> %s"
               (plist-get entry :turn)
               (truncate-string-to-width (plist-get entry :request) 80 nil nil "...")
               (truncate-string-to-width (plist-get entry :response) 80 nil nil "...")))
     history
     "\n")))

(defun efrit--send-completion-check-request (prompt conversation)
  "Send lightweight completion check request to Claude."
  (let* ((api-key (efrit--get-api-key))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("x-api-key" . ,api-key)
            ("anthropic-version" . "2023-06-01")
            ("content-type" . "application/json")))
         (request-data
           `(("model" . ,efrit-multi-turn-completion-model)
             ("max_tokens" . ,efrit-multi-turn-completion-max-tokens)
             ("temperature" . 0.1)
             ("messages" . [
                           (("role" . "user")
                            ("content" . ,prompt))])))
          (url-request-data
           (encode-coding-string (json-encode request-data) 'utf-8)))
    
    ;; Send synchronous request for completion check
    (require 'efrit-debug)
    (efrit-log-debug "Sending completion check to Claude...")
    (condition-case err
        (with-current-buffer 
            (url-retrieve-synchronously "https://api.anthropic.com/v1/messages" t nil efrit-multi-turn-api-timeout)
          (goto-char (point-min))
          (re-search-forward "\n\n" nil t) ; Skip headers
          (let* ((response-json (buffer-substring (point) (point-max)))
                 (response-data (json-read-from-string response-json))
                 (content (cdr (assoc 'content response-data))))
            (efrit-log-debug "Got completion check response")
            (if (and content (vectorp content) (> (length content) 0))
                (let ((text (cdr (assoc 'text (aref content 0)))))
                  (efrit--parse-completion-response text conversation))
              (progn 
                (efrit-log-debug "Invalid response structure from completion API")
                'error))))
      (error 
       (efrit-log-debug "Error in completion check: %s" (error-message-string err))
       'error))))

(defun efrit--parse-completion-response (response-text _conversation)
  "Parse Claude's completion check RESPONSE-TEXT."
  (require 'efrit-debug)
  (efrit-log-debug "Claude completion response: %s" response-text)
  (if (string-match "CONTINUE:\\s-*\\(YES\\|NO\\)" response-text)
      (let ((should-continue (string= (match-string 1 response-text) "YES")))
        (efrit-log-debug "Claude decision: %s" (if should-continue "CONTINUE" "COMPLETE"))
        (if should-continue 'continue 'complete))
    (progn
      (efrit-log-debug "Failed to parse Claude response - defaulting to error")
      'error)))

;;; Continuation Logic

(defun efrit--should-continue-conversation-p (conversation _response)
  "Determine if CONVERSATION should continue using Claude-based assessment."
  (require 'efrit-debug)
  (cond
   ;; No conversation or multi-turn disabled
   ((or (not efrit-multi-turn-enabled) (not conversation))
    (efrit-log-debug "Not continuing: multi-turn disabled (%s) or no conversation (%s)" 
                     efrit-multi-turn-enabled (not (null conversation)))
    nil)
   
   ;; Conversation expired
   ((efrit--conversation-expired-p conversation)
    (efrit-log-debug "Not continuing: conversation expired")
    (efrit--terminate-conversation conversation "expired")
    nil)
   
   ;; Max turns reached
   ((efrit--conversation-at-max-turns-p conversation)
    (efrit-log-debug "Not continuing: max turns reached (%d)" 
                     (efrit-conversation-current-turn conversation))
    (efrit--terminate-conversation conversation "max-turns")
    nil)
   
   ;; First turn: always continue (give Claude a chance to work)
   ((= (efrit-conversation-current-turn conversation) 1)
    (efrit-log-debug "Continuing: first turn always continues")
    t)
   
   ;; For subsequent turns: simple heuristic (continue until max turns)
   (t
    (efrit-log-debug "Turn %d: using simple continuation heuristic" 
                     (efrit-conversation-current-turn conversation))
    (if (<= (efrit-conversation-current-turn conversation) efrit-multi-turn-simple-max-turns)
        (progn
          (efrit-log-debug "Continuing: turn %d <= %d" 
                           (efrit-conversation-current-turn conversation)
                           efrit-multi-turn-simple-max-turns)
          t)
      (progn
        (efrit-log-debug "Stopping: turn %d > %d" 
                         (efrit-conversation-current-turn conversation)
                         efrit-multi-turn-simple-max-turns)
        (efrit--terminate-conversation conversation "max-simple-turns")
        nil)))))

(defun efrit--terminate-conversation (conversation reason)
  "Terminate CONVERSATION with the given REASON."
  (setf (efrit-conversation-termination-reason conversation) reason)
  (efrit--update-conversation conversation))

;;; Multi-Turn Detection

(defun efrit--request-might-need-multiple-turns-p (_request)
  "Always returns t - let Claude decide if it needs multiple turns.
This removes client-side heuristics per Zero Client-Side Intelligence principle."
  ;; Always start multi-turn, let Claude manage when to stop
  t)

;;; Conversation Management

(defun efrit--advance-conversation-turn (conversation)
  "Advance CONVERSATION to the next turn and update state."
  (setf (efrit-conversation-current-turn conversation)
        (1+ (efrit-conversation-current-turn conversation)))
  (setf (efrit-conversation-last-activity conversation)
        (current-time))
  (efrit--update-conversation conversation)
  conversation)

(defun efrit--add-to-conversation-history (conversation request response)
  "Add REQUEST and RESPONSE to CONVERSATION's context history."
  (let ((history-entry (list :turn (efrit-conversation-current-turn conversation)
                            :timestamp (current-time)
                            :request request
                            :response response)))
    (setf (efrit-conversation-context-history conversation)
          (append (efrit-conversation-context-history conversation)
                  (list history-entry))))
  (efrit--update-conversation conversation))

(defun efrit--generate-continuation-prompt (conversation)
  "Generate a generic continuation prompt for CONVERSATION.
Removes client-side pattern matching per Zero Client-Side Intelligence
principle."
  (let ((original-request (efrit-conversation-original-request conversation))
        (current-turn (efrit-conversation-current-turn conversation)))
    (format "Continue with the next step of this request: \"%s\"\n\nThis is turn %d. Focus on any remaining tasks that haven't been completed yet. Don't repeat work that was already done in previous turns."
            original-request current-turn)))

;;; Integration Points

;;;###autoload
(defun efrit--start-multi-turn-if-needed (original-request user-buffer)
  "Start a multi-turn conversation if the request appears to need multiple steps."
  (when (and efrit-multi-turn-enabled
             (efrit--request-might-need-multiple-turns-p original-request))
    (efrit--create-conversation original-request user-buffer)))



(provide 'efrit-multi-turn)

;;; efrit-multi-turn.el ends here
