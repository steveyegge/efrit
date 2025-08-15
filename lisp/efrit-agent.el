;;; efrit-agent.el --- Autonomous problem-solving agent -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (json "1.4"))
;; Keywords: tools, convenience, ai, agent
;; URL: https://github.com/stevey/efrit

;;; Commentary:
;; Autonomous problem-solving mode for Efrit. Takes high-level goals
;; and works until completion using self-directed AI consultation.

;;; Code:

(require 'json)
(require 'efrit-tools)

;; Conditional requires
(declare-function efrit-streamlined-send "efrit-chat-streamlined")

;;; Customization

(defgroup efrit-agent nil
  "Autonomous problem-solving agent for Efrit."
  :group 'efrit
  :prefix "efrit-agent-")

(defcustom efrit-agent-backend "claude-3.5-sonnet"
  "Default model backend for agent mode."
  :type '(choice (const "claude-3.5-sonnet")
                 (const "gpt-4") 
                 (const "local-llama")
                 (string :tag "Custom API endpoint"))
  :group 'efrit-agent)

(defcustom efrit-agent-max-iterations 50
  "Maximum iterations before forcing user consultation."
  :type 'integer
  :group 'efrit-agent)

;;; Session Management

(defvar efrit-agent--current-session nil
  "Current agent session data.")

(defun efrit-agent--create-session (goal &optional context session-id)
  "Create a new agent session for GOAL."
  (let ((session (make-hash-table :test 'equal)))
    (puthash "id" (or session-id (format "agent_%d" (floor (float-time)))) session)
    (puthash "goal" goal session)
    (puthash "context" (or context "") session)
    (puthash "status" "planning" session)
    (puthash "todos" (list) session)
    (puthash "actions" (list) session)
    (puthash "iteration" 0 session)
    (puthash "created_at" (current-time) session)
    session))

(defun efrit-agent--session-complete-p (session)
  "Check if SESSION is complete."
  (equal (gethash "status" session) "complete"))

(defun efrit-agent--update-session (session action result reflection)
  "Update SESSION with ACTION, RESULT, and REFLECTION."
  (let ((actions (gethash "actions" session)))
    (push (list :action action :result result :reflection reflection :timestamp (current-time)) actions)
    (puthash "actions" actions session)
    (puthash "iteration" (1+ (gethash "iteration" session)) session)))

(defun efrit-agent--session-summary (session)
  "Generate a summary of SESSION for display."
  (format "Session %s: %s (iteration %d, status: %s)"
          (gethash "id" session)
          (gethash "goal" session)
          (gethash "iteration" session)
          (gethash "status" session)))

;;; TODO Management

(defun efrit-agent--update-todos (session todos)
  "Update the TODO list for SESSION."
  (puthash "todos" todos session))

(defun efrit-agent--format-todos (todos)
  "Format TODOS for prompt display."
  (if (null todos)
      "(none)"
    (mapconcat (lambda (todo)
                 (format "- [%s] %s"
                         (plist-get todo :status)
                         (plist-get todo :content)))
               todos "\n")))

(defun efrit-agent--format-actions (actions)
  "Format ACTIONS for prompt display."
  (if (null actions)
      "(none)"
    (mapconcat (lambda (action)
                 (let ((output (or (plist-get (plist-get action :result) :output) "no-output")))
                   (format "- %s: %s"
                           (plist-get (plist-get action :action) :type)
                           (if (stringp output) 
                               (substring output 0 (min 100 (length output)))
                             (prin1-to-string output)))))
               (reverse (last actions 3)) "\n")))

;;; LLM Consultation

(defun efrit-agent--build-prompt (session)
  "Build prompt for LLM consultation based on SESSION."
  (let ((goal (gethash "goal" session))
        (context (gethash "context" session))
        (todos (gethash "todos" session))
        (actions (gethash "actions" session))
        (iteration (gethash "iteration" session)))
    (format "You are Efrit, an autonomous Emacs agent. Work systematically toward goals.\n\nGOAL: %s\nCONTEXT: %s\nITERATION: %d\n\nCURRENT TODOS:\n%s\n\nACTIONS TAKEN:\n%s\n\nRespond with JSON containing your next action."
            goal context iteration
            (efrit-agent--format-todos todos)
            (efrit-agent--format-actions actions))))

(defun efrit-agent--mock-llm-response (prompt)
  "Generate a mock LLM response for testing."
  (if (string-match-p "Create a test file" prompt)
      "{\"status\": \"executing\", \"todos\": [{\"id\": \"create-file\", \"status\": \"in-progress\", \"content\": \"Create hello.txt file\"}], \"next_action\": {\"type\": \"eval\", \"content\": \"(with-temp-file \\\"hello.txt\\\" (insert \\\"Hello from Efrit Agent\\\"))\"}, \"rationale\": \"I need to create the requested file with the specified content\", \"self_assessment\": \"Simple file creation task\"}"
    "{\"status\": \"complete\", \"rationale\": \"Task completed\"}"))

(defun efrit-agent--consult-llm (prompt)
  "Send PROMPT to LLM backend and return response."
  ;; Use mock for now
  (efrit-agent--mock-llm-response prompt))

(defun efrit-agent--parse-signal (response)
  "Parse LLM RESPONSE into signal plist."
  (condition-case err
      (let* ((json-object-type 'hash-table)
             (json-array-type 'list)
             (json-key-type 'string)
             (parsed (json-read-from-string response)))
        (list :status (intern (gethash "status" parsed))
              :todos (gethash "todos" parsed)
              :next_action (gethash "next_action" parsed)
              :rationale (gethash "rationale" parsed)))
    (error
     (list :status 'error :error (error-message-string err)))))

;;; Action Execution

(defun efrit-agent--execute-action (action)
  "Execute ACTION and return result hash."
  (let ((action-type (plist-get action :type))
        (content (plist-get action :content))
        (result (make-hash-table :test 'equal)))
    (puthash "timestamp" (current-time) result)
    (puthash "action_type" action-type result)
    (condition-case err
        (pcase action-type
          ('eval
           (let ((eval-result (efrit-tools-eval-sexp content)))
             (puthash "success" t result)
             (puthash "output" (prin1-to-string eval-result) result)))
          ('shell
           (let ((shell-result (shell-command-to-string content)))
             (puthash "success" t result)
             (puthash "output" shell-result result)))
          ('user_input
           (let ((user-response (read-string (format "Efrit needs input: %s " content))))
             (puthash "success" t result)
             (puthash "output" user-response result)))
          (_
           (puthash "success" nil result)
           (puthash "error" (format "Unknown action type: %s" action-type) result)))
      (error
       (puthash "success" nil result)
       (puthash "error" (error-message-string err) result)))
    result))

;;; Main Agent Loop

;;;###autoload
(defun efrit-agent-solve (goal &optional context session-id)
  "Autonomous problem-solving mode for GOAL."
  (interactive "sGoal: ")
  (let ((session (efrit-agent--create-session goal context session-id)))
    (setq efrit-agent--current-session session)
    (message "Efrit Agent starting: %s" goal)
    
    (condition-case err
        (while (and (not (efrit-agent--session-complete-p session))
                    (< (gethash "iteration" session) efrit-agent-max-iterations))
          
          ;; Build prompt and consult LLM
          (let* ((prompt (efrit-agent--build-prompt session))
                 (response (efrit-agent--consult-llm prompt))
                 (signal (efrit-agent--parse-signal response)))
            
            ;; Update session status
            (puthash "status" (symbol-name (plist-get signal :status)) session)
            
            ;; Handle signal status
            (pcase (plist-get signal :status)
              ('stuck
               (message "Agent stuck, requesting user input")
               (let ((user-input (read-string "Efrit is stuck. Help: ")))
                 (puthash "context" (concat (gethash "context" session) "\n\nUser input: " user-input) session)))
              ('complete
               (message "Agent completed goal: %s" goal))
              (_
               ;; Execute next action
               (let ((next-action (plist-get signal :next_action)))
                 (when next-action
                   (let ((result (efrit-agent--execute-action next-action)))
                     (efrit-agent--update-session session next-action result signal))))))))
      (error
       (message "Agent error: %s" (error-message-string err))))
    
    (efrit-agent--session-summary session)))

(provide 'efrit-agent)
;;; efrit-agent.el ends here
