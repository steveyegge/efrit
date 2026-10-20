;;; efrit-agent-consult.el --- One agent asks another: list_sessions and ask_session -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.12.12
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, ai

;;; Commentary:

;; With several agents on related work, one needs what another knows
;; ("do the new retry tests cover shutdown?").  M-x Machina routes
;; such a request through a socket to the other agent's input and
;; waits for its reply; the recipient must be idle and the user sees
;; a turn they did not start.  efrit has both agents in one Emacs, so
;; it does neither (tzz, 2026-10-14: "ask_session seems good!"):
;;
;; `list_sessions' tells the model which agent buffers exist: name,
;; project, branch, status, what they were last asked.
;;
;; `ask_session' answers a question ABOUT a peer session, from its
;; conversation: a side request carrying the peer's recent turns (as
;; much as `efrit-agent-consult-max-chars' allows, newest kept) and
;; the question, answered by the model in one shot.  The peer's turn,
;; queue and transcript are untouched; nothing is sent as the peer;
;; no tool of the peer runs.  The peer's user sees a note that it was
;; consulted.  The call is synchronous (tools return strings) through
;; `efrit-api-request-sync', bounded by `efrit-agent-consult-timeout'.
;;
;; Permission class `read': the peer's conversation may hold things
;; the asking session was not shown, so the sandbox asks once per
;; session per peer unless granted.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'efrit-config)
(require 'efrit-log)
(require 'efrit-api)
(require 'efrit-chat-response)
(require 'efrit-events)
(require 'efrit-tool-registry)

(declare-function efrit-agent-buffers "efrit-agent-core")
(declare-function efrit-agent-ring "efrit-agent-ring")
(declare-function efrit-agent-attention-state "efrit-agent-ring")
(declare-function efrit-repl-session-id "efrit-repl-session")
(declare-function efrit-repl-session-status "efrit-repl-session")
(declare-function efrit-repl-session-project-root "efrit-repl-session")
(declare-function efrit-repl-session-conversation "efrit-repl-session")
(declare-function efrit-repl-session-meta-get "efrit-repl-session")
(declare-function efrit-vcs-branch "efrit-vcs")
(declare-function efrit-vcs-root "efrit-vcs")
(declare-function efrit-sandbox-check "efrit-sandbox")
(declare-function efrit-ask-response-text "efrit-ask")
(defvar efrit-agent--repl-session)
(defvar efrit-current-session-id)
(defvar efrit-default-model)
(defvar efrit-sandbox-enabled)

(defgroup efrit-agent-consult nil
  "Agents consulting each other."
  :group 'efrit)

(defcustom efrit-agent-consult-max-chars 24000
  "Most characters of a peer's conversation sent with a question, newest kept."
  :type 'integer
  :group 'efrit-agent-consult)

(defcustom efrit-agent-consult-timeout 90
  "Seconds one ask_session may take."
  :type 'integer
  :group 'efrit-agent-consult)

(defcustom efrit-agent-consult-model nil
  "Model that answers ask_session, or nil for `efrit-default-model'."
  :type '(choice (const nil) string)
  :group 'efrit-agent-consult)

;;;; Peers

(defun efrit-agent-consult--label (buffer)
  (let ((name (buffer-name buffer)))
    (if (string-match "\\`\\*efrit\\[\\(.*\\)\\]\\*\\'" name) (match-string 1 name) name)))

(defun efrit-agent-consult-peers ()
  "The agent buffers other than the asking session's, in ring order."
  (seq-remove (lambda (b)
                (let ((s (buffer-local-value 'efrit-agent--repl-session b)))
                  (and s efrit-current-session-id
                       (equal (efrit-repl-session-id s) efrit-current-session-id))))
              (if (fboundp 'efrit-agent-ring) (efrit-agent-ring) (efrit-agent-buffers))))

(defun efrit-agent-consult-find (name)
  "The peer buffer NAME names: its label, buffer name or session id; nil if none."
  (when (stringp name)
    (let ((name (string-trim name)))
      (seq-find (lambda (b)
                  (let ((s (buffer-local-value 'efrit-agent--repl-session b)))
                    (or (equal name (efrit-agent-consult--label b))
                        (equal name (buffer-name b))
                        (and s (equal name (efrit-repl-session-id s))))))
                (efrit-agent-consult-peers)))))

(defun efrit-agent-consult--last-ask (session)
  (let ((conv (efrit-repl-session-conversation session)))
    (when-let* ((user (seq-find (lambda (e) (eq (plist-get e :role) 'user)) conv)))
      (truncate-string-to-width (replace-regexp-in-string "[\n\r]+" " " (or (plist-get user :content) ""))
                                80 nil nil "…"))))

(defun efrit-agent-consult-list ()
  "One line per peer: label, project, branch, status, attention, last ask."
  (let ((peers (efrit-agent-consult-peers)))
    (if (null peers) "No other agent sessions."
      (mapconcat
       (lambda (b)
         (with-current-buffer b
           (let* ((s efrit-agent--repl-session)
                  (root (or (and s (efrit-repl-session-project-root s)) default-directory))
                  (branch (ignore-errors (require 'efrit-vcs) (efrit-vcs-branch (efrit-vcs-root root)))))
             (format "- %s: project %s%s, status %s%s%s"
                     (efrit-agent-consult--label b)
                     (abbreviate-file-name (directory-file-name root))
                     (if branch (format " (branch %s)" branch) "")
                     (if s (efrit-repl-session-status s) "none")
                     (if (fboundp 'efrit-agent-attention-state)
                         (pcase (efrit-agent-attention-state b)
                           ('waiting ", waiting for the user") ('unread ", reply unread") (_ ""))
                       "")
                     (let ((ask (and s (efrit-agent-consult--last-ask s))))
                       (if ask (format ", last asked: %s" ask) ""))))))
       peers "\n"))))

;;;; Asking

(defun efrit-agent-consult--transcript (session)
  "SESSION's conversation as text, newest `efrit-agent-consult-max-chars' kept."
  (let* ((entries (reverse (efrit-repl-session-conversation session)))
         (text (mapconcat (lambda (e)
                            (format "%s: %s" (upcase (symbol-name (plist-get e :role)))
                                    (or (plist-get e :content) "")))
                          entries "\n\n")))
    (if (> (length text) efrit-agent-consult-max-chars)
        (concat "[… earlier turns omitted]\n\n"
                (substring text (- (length text) efrit-agent-consult-max-chars)))
      text)))

(defconst efrit-agent-consult--system
  "You answer a question about another coding agent's conversation, on behalf of that agent.  You are given that conversation (its user's requests and the agent's replies) and one question from a peer agent working on related code.  Answer from the conversation only: what was asked, decided, changed, tested, left open.  Quote file names and identifiers exactly.  If the conversation does not say, say so; do not guess.  Be brief.")

(defun efrit-agent-consult-ask (buffer question)
  "Answer QUESTION about the session in BUFFER from its conversation.
Synchronous; returns the answer text or an error string starting with
\"Error\".  The peer is notified with a note in its transcript."
  (require 'efrit-ask)
  (let* ((session (buffer-local-value 'efrit-agent--repl-session buffer))
         (label (efrit-agent-consult--label buffer)))
    (cond
     ((null session) (format "Error: %s has no session" label))
     ((not (stringp question)) "Error: question is required")
     (t
      (let* ((transcript (efrit-agent-consult--transcript session))
             (prompt (format "Conversation of agent %s (project %s):\n\n%s\n\nQuestion from a peer agent: %s"
                             label (abbreviate-file-name (directory-file-name (or (efrit-repl-session-project-root session) "")))
                             (if (string-empty-p transcript) "(empty so far)" transcript)
                             question))
             (request `(("model" . ,(or efrit-agent-consult-model efrit-default-model))
                        ("max_tokens" . 1500)
                        ("system" . ,(efrit-api-cacheable-system efrit-agent-consult--system))
                        ("messages" . [(("role" . "user") ("content" . ,prompt))])))
             (efrit-api-request-purpose (format "ask_session about %s" label)))
        (efrit-publish 'note (list (cons :session-id (efrit-repl-session-id session))
                                   (cons :text (format "⇄ consulted by %s: %s"
                                                       (or efrit-current-session-id "another session")
                                                       (truncate-string-to-width question 80 nil nil "…")))
                                   (cons :face 'shadow) (cons :kind 'consult)))
        (condition-case err
            (let ((response (efrit-api-request-sync request efrit-agent-consult-timeout)))
              (cond
               ((null response) "Error: no response")
               ((efrit-response-error response)
                (format "Error: %s" (efrit-error-message (efrit-response-error response))))
               (t (let ((text (efrit-ask-response-text response)))
                    (if (string-empty-p (string-trim text)) "Error: empty answer"
                      (format "Answer about %s (from its conversation, %d chars seen):\n%s"
                              label (length transcript) text))))))
          (error (format "Error: %s" (error-message-string err)))))))))

;;;; Tools

(defun efrit-agent-consult--tool-list (_input)
  (efrit-agent-consult-list))

(defun efrit-agent-consult--tool-ask (input)
  (let* ((name (alist-get "session" input nil nil #'equal))
         (question (alist-get "question" input nil nil #'equal))
         (buffer (efrit-agent-consult-find name)))
    (cond
     ((null buffer)
      (format "Error: no other agent session named %S. Sessions:\n%s" name (efrit-agent-consult-list)))
     ((and (bound-and-true-p efrit-sandbox-enabled) (fboundp 'efrit-sandbox-check)
           (not (condition-case nil
                    (efrit-sandbox-check 'buffer (cons 'buffer (buffer-name buffer)) "ask_session"
                                         (format "read the conversation of %s to answer: %s"
                                                 (efrit-agent-consult--label buffer)
                                                 (truncate-string-to-width (or question "") 60 nil nil "…")))
                  (error nil))))
      (format "Error: reading %s's conversation was not allowed" (efrit-agent-consult--label buffer)))
     (t (efrit-agent-consult-ask buffer question)))))

(defun efrit-agent-consult-register-tools ()
  "Offer list_sessions and ask_session to the model."
  (efrit-register-tool
   "list_sessions"
   :description "The other efrit agent sessions in this Emacs: name, project, branch, status, what each was last asked. Use it before ask_session."
   :input-schema '(("type" . "object") ("properties" . ()) ("required" . []))
   :function #'efrit-agent-consult--tool-list :class 'read :package 'efrit-agent-consult)
  (efrit-register-tool
   "ask_session"
   :description "Ask a question about another agent session's work, answered from that session's conversation (what it was asked, decided, changed, tested, left open). It does not interrupt that session or run anything there. session: the name from list_sessions."
   :input-schema '(("type" . "object")
                   ("properties" . (("session" . (("type" . "string") ("description" . "Name from list_sessions, e.g. proj or proj:fix")))
                                    ("question" . (("type" . "string")))))
                   ("required" . ["session" "question"]))
   :function #'efrit-agent-consult--tool-ask :class 'read :package 'efrit-agent-consult))

(efrit-agent-consult-register-tools)

(provide 'efrit-agent-consult)

;;; efrit-agent-consult.el ends here
