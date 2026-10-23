;;; efrit-questions.el --- Keep the model's questions honest -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.13.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, ai

;;; Commentary:

;; Two ways a question turn went wrong on 2026-10-20, both fixed here
;; mechanically rather than by asking the model to behave:
;;
;; 1. The user answered a request_user_input with a question of their
;;    own ("which of the two is missing those features?").  The model
;;    replied with another request_user_input, same options, no text:
;;    the user's question was never answered and the menu came back
;;    as if nothing had been typed.  `efrit-questions-answer-first-p'
;;    spots that shape (the user's last message asks something, the
;;    response is a bare request_user_input) and the loop refuses the
;;    tool call with an instruction to answer in text first.
;;
;; 2. The model wrote its choices as prose in a final message ("Three
;;    options: 1. ... 2. ... 3. ...  Which one?") and ended the turn
;;    without request_user_input.  No menu, no 1/2/3 keys; the user
;;    typed "1" RET, which started a new turn.
;;    `efrit-questions-parse-prose-choices' finds that shape in the
;;    final text and the agent registers a pending question from it,
;;    so the keys and the menu work exactly as if the tool had been
;;    called.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'efrit-chat-response)

(defconst efrit-questions-user-question-regexp
  "\\(?:\\?\\s-*\\'\\|\\`\\s-*\\(?:what\\|which\\|why\\|how\\|where\\|when\\|who\\|is\\|are\\|does\\|do\\|can\\|could\\|should\\|would\\)\\b\\)"
  "A user message that asks something: ends in ? or starts with a question word.")

(defun efrit-questions-user-asked-p (text)
  "Non-nil when TEXT, the user's message, asks the model something."
  (and (stringp text)
       (let ((case-fold-search t)
             (trimmed (string-trim text)))
         (and (not (string-empty-p trimmed))
              (string-match-p efrit-questions-user-question-regexp trimmed)))))

(defun efrit-questions--last-user-text (messages)
  "The text of the last user message in MESSAGES (API shape), or nil.
A user message whose content is tool results only does not count:
the user did not type it."
  (let ((text nil))
    (cl-loop for m across (vconcat messages)
             when (equal (efrit-questions--get m "role") "user")
             do (let ((content (efrit-questions--get m "content")))
                  (cond
                   ((stringp content) (setq text content))
                   ((or (vectorp content) (listp content))
                    (let ((parts (cl-loop for block across (vconcat content)
                                          when (equal (efrit-questions--get block "type") "text")
                                          collect (efrit-questions--get block "text"))))
                      (when parts (setq text (string-join parts "\n"))))))))
    text))

(defun efrit-questions--get (block key)
  (cond ((hash-table-p block) (gethash key block))
        ((listp block) (or (cdr (assoc key block)) (cdr (assq (intern key) block))))))

(defun efrit-questions--content-text (content)
  "The text blocks of CONTENT, a response content vector, joined."
  (string-trim
   (mapconcat (lambda (item) (or (efrit-content-item-text item) ""))
              (append content nil) "")))

(defconst efrit-questions-answer-first-message
  "Error: request_user_input refused."
  "Prefix of the tool_result the loop returns for a refused re-ask.
`efrit-questions-answer-first-result' builds the whole text.")

(defun efrit-questions-answer-first-result (messages)
  "The tool_result text for a refused re-ask, quoting the user's question.
The model re-asked 35 times on 2026-10-21 with a shorter version of
this text: it needs the question in front of it and the exact shape
of the response that will be accepted."
  (format "%s The user did not pick an option; they asked you: \"%s\". Your next response must be plain text that answers that question (facts, comparison, your recommendation and why). Do not call any tool in that response. End the turn after the answer; ask again with request_user_input only in a later turn, and only if a choice still remains."
          efrit-questions-answer-first-message
          (string-trim (or (efrit-questions--last-user-text messages) ""))))

(defun efrit-questions--already-refused-p (messages)
  "Non-nil when a refusal was already returned since the user's last text.
The refusal is a tool_result; the user message that carries it holds
no text of the user's, so the question before it still reads as the
last user text.  Without this bound the same refusal repeated for as
long as the model re-asked: 19 rows on 2026-10-21."
  (let ((refused nil))
    (cl-loop for m across (vconcat messages)
             when (equal (efrit-questions--get m "role") "user")
             do (let ((content (efrit-questions--get m "content")))
                  (cond
                   ((stringp content) (setq refused nil))
                   ((or (vectorp content) (listp content))
                    (cl-loop for block across (vconcat content)
                             for type = (efrit-questions--get block "type")
                             do (cond
                                 ((equal type "text") (setq refused nil))
                                 ((and (equal type "tool_result")
                                       (let ((c (efrit-questions--get block "content")))
                                         (and (stringp c)
                                              (string-prefix-p efrit-questions-answer-first-message c))))
                                  (setq refused t))))))))
    refused))

(defun efrit-questions-answer-first-p (messages content)
  "Non-nil when CONTENT asks the user again without answering what they asked.
MESSAGES is the conversation so far; CONTENT the new response.  True
when the user's last message asks a question and CONTENT is a
`request_user_input' with no text before it.  Refused at most once
per user message: if the model re-asks after the refusal, the
question goes through (its prompt may hold the answer) rather than
loop."
  (let ((uses (cl-loop for item across content
                       for use = (efrit-content-item-as-tool-use item)
                       when use collect use)))
    (and uses
         (cl-every (lambda (u) (equal (nth 1 u) "request_user_input")) uses)
         (string-empty-p (efrit-questions--content-text content))
         (efrit-questions-user-asked-p (efrit-questions--last-user-text messages))
         (not (efrit-questions--already-refused-p messages)))))

;;;; Choices written as prose

(defconst efrit-questions--choice-line-regexp
  "^[ \t]*\\(?:\\([0-9]+\\)[.)]\\|(\\([0-9]+\\))\\|\\([A-Za-z]\\)[.)]\\)[ \t]+\\(.+\\)$"
  "A numbered or lettered list line.  Group 4 is the item text.")

(defconst efrit-questions-prose-question-regexp
  "\\(?:which\\|what\\|pick\\|choose\\|prefer\\|option\\|want\\|go with\\|should i\\|shall i\\)"
  "Words in the closing line that make a list a choice.")

(defconst efrit-questions--yes-no-regexp
  "(y/n)\\|(yes/no)\\|\\by or n\\b\\|\\byes or no\\b"
  "A closing line that asks for y/n in words.")

(defun efrit-questions-parse-prose-choices (text)
  "The (QUESTION . OPTIONS) a final TEXT offers as a prose list, or nil.
TEXT must end (within its last three non-empty lines) in a line with
a question mark that reads like a choice, and hold a numbered or
lettered list of 2 to 9 items above it.  Items are cut at the first
sentence end so a long rationale does not become the label.  A
closing line that asks for y/n in words (\"pending (y/n).\") yields
\(LINE \"yes\" \"no\") (2026-10-21)."
  (when (stringp text)
    (let* ((lines (cl-remove-if #'string-empty-p (mapcar #'string-trim (split-string text "\n"))))
           (tail (last lines 3))
           (yes-no (cl-find-if (lambda (l) (let ((case-fold-search t)) (string-match-p efrit-questions--yes-no-regexp l)))
                               (reverse tail)))
           (closing (cl-find-if (lambda (l)
                                  (and (string-suffix-p "?" l)
                                       (let ((case-fold-search t))
                                         (string-match-p efrit-questions-prose-question-regexp l))))
                                (reverse tail))))
      (cond
       ((and yes-no (not closing)) (list yes-no "yes" "no"))
       (closing
        (let ((items nil) (last-num 0))
          (dolist (l lines)
            (when (string-match efrit-questions--choice-line-regexp l)
              (let ((n (or (and (match-string 1 l) (string-to-number (match-string 1 l)))
                           (and (match-string 2 l) (string-to-number (match-string 2 l)))
                           (1+ last-num)))
                    (body (match-string 4 l)))
                ;; a list restarts: keep the last one
                (when (= n 1) (setq items nil))
                (setq last-num n)
                (push (efrit-questions--option-label body) items))))
          (setq items (nreverse items))
          (when (<= 2 (length items) 9)
            (cons closing items))))))))

(defun efrit-questions--option-label (body)
  "BODY cut to its first sentence, bold markers removed, at most 80 columns."
  (let* ((plain (replace-regexp-in-string "\\*\\*\\|__" "" body))
         (sentence (if (string-match "\\`\\(.*?[.;:]\\)\\(?:\\s-\\|\\'\\)" plain)
                       (match-string 1 plain)
                     plain))
         (clean (string-trim-right sentence "[.;:]")))
    (truncate-string-to-width clean 80 nil nil "…")))

(provide 'efrit-questions)

;;; efrit-questions.el ends here
