;;; efrit-brief.el --- Structured briefs, prompt suffixes, verbatim text -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.9.2
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, ai

;;; Commentary:

;; What goes out with a turn, shaped (after ai-code-interface,
;; 2026-09-28):
;;
;; - `efrit-brief': a prompt made of labelled sections (Goal, Scope,
;;   Context, Boundaries, Agent responsibilities, Verification
;;   evidence, Instruction); blank sections drop out.  The `question'
;;   kind says "answer only, no changes" and, through
;;   `efrit-brief-question-turn', makes the sandbox deny writes, shell
;;   and eval for that one turn: the boundary is enforced, not asked.
;;
;; - `efrit-prompt-suffix-functions': an abnormal hook over every
;;   outgoing input.  Each function gets an `efrit-prompt-context'
;;   (the text, the command that produced it, the buffer, a memo
;;   table shared by all providers of one send) and returns a string
;;   to append, or nil.  A provider that signals aborts the send.
;;
;; - `efrit-verbatim': a text property.  Text carrying it (a pasted
;;   diff, a quoted error) is left alone by mention expansion and by
;;   the suffix providers' view of the text; it is data, not
;;   instructions.  `efrit-mark-verbatim' sets it on a region.
;;
;; - `efrit-grill-me': the standing "ask first" instruction for the
;;   next send.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;;;; Briefs

(defconst efrit-brief-default-boundaries
  "Make only the requested change. Do not do unrelated cleanup, renames or reformatting. Keep the project's conventions."
  "Boundaries every code-change brief carries unless told otherwise.")

(defconst efrit-brief-default-responsibilities
  "Inspect the relevant files before editing. Plan briefly. After the change run the project's verification (tests, byte-compile, linters) and fix what fails."
  "What the agent is responsible for in a code-change brief.")

(defconst efrit-brief-default-evidence
  "Report the exact verification command(s), their result, and any remaining risk."
  "The evidence a code-change brief asks for at the end.")

(defconst efrit-brief-question-boundaries
  "Answer the question only. Do not make code changes, do not run commands that change anything."
  "Boundaries of a question brief.")

(cl-defun efrit-brief (&key goal scope context clipboard boundaries responsibilities
                            evidence instruction (kind 'change))
  "A prompt of labelled sections; blank ones are left out.
KIND is `change' (the default sections above apply) or `question'
\(answer-only boundaries, no responsibilities or evidence).  Pass an
explicit empty string to drop a default section."
  (let* ((question (eq kind 'question))
         (sections
          `(("Goal" . ,goal)
            ("Scope" . ,scope)
            ("Context" . ,context)
            ("Clipboard context" . ,clipboard)
            ("Boundaries" . ,(or boundaries (if question efrit-brief-question-boundaries
                                                efrit-brief-default-boundaries)))
            ("Agent responsibilities" . ,(or responsibilities (and (not question) efrit-brief-default-responsibilities)))
            ("Verification evidence" . ,(or evidence (and (not question) efrit-brief-default-evidence)))
            ("Instruction" . ,instruction))))
    (mapconcat (lambda (s) (format "%s:\n%s" (car s) (string-trim (cdr s))))
               (cl-remove-if (lambda (s) (or (null (cdr s)) (string-empty-p (string-trim (cdr s))))) sections)
               "\n\n")))

;;;; A question turn is a read-only turn

(declare-function efrit-sandbox--turn-set "efrit-sandbox")
(defvar efrit-sandbox--question-turn nil
  "Non-nil while the running turn was sent as a question: writes, shell
and eval are denied without asking.  Set through `efrit-brief-question-turn'.")

(defun efrit-brief-question-turn (&optional on)
  "Mark the next turn (ON non-nil) as a question: no writes, shell or eval.
The sandbox reads `efrit-sandbox-question-turn-p'; the loop clears the
mark when the turn ends."
  (setq efrit-sandbox--question-turn (and on t)))

(defun efrit-sandbox-question-turn-p (cap)
  "Non-nil when CAP is refused because the running turn is a question."
  (and efrit-sandbox--question-turn (memq cap '(write shell elisp))))

;;;; Prompt suffix providers

(cl-defstruct (efrit-prompt-context (:constructor efrit-prompt-context-create))
  text            ; the outgoing text, verbatim spans included
  command         ; the command that produced it (a symbol), or nil
  buffer          ; the buffer the user sent from
  (memo (make-hash-table :test 'equal)))   ; shared by all providers of one send

(defvar efrit-prompt-suffix-functions nil
  "Abnormal hook: functions called with an `efrit-prompt-context'.
Each returns a string to append to the outgoing text, or nil.  They run
in hook order (use the DEPTH argument of `add-hook' to order them);
one that signals aborts the send with its message.  Use the context's
memo (`efrit-prompt-context-memoize') to share a decision between
providers, for example whether the user accepted a mode this send.")

(defun efrit-prompt-context-memoize (ctx key thunk)
  "The value of KEY in CTX's memo, computed by THUNK the first time."
  (let ((memo (efrit-prompt-context-memo ctx)))
    (if (eq (gethash key memo 'efrit--unset) 'efrit--unset)
        (puthash key (funcall thunk) memo)
      (gethash key memo))))

(defun efrit-prompt-apply-suffixes (text &optional command buffer)
  "TEXT with every suffix of `efrit-prompt-suffix-functions' appended.
COMMAND names the producing command, BUFFER the sending buffer.
Each suffix is separated by a blank line; a signalling provider
propagates (the send does not happen)."
  (if (null efrit-prompt-suffix-functions)
      text
    (let* ((ctx (efrit-prompt-context-create :text text :command command
                                             :buffer (or buffer (current-buffer))))
           (parts (delq nil (mapcar (lambda (fn) (let ((s (funcall fn ctx)))
                                                   (and (stringp s) (not (string-empty-p (string-trim s))) s)))
                                    efrit-prompt-suffix-functions))))
      (if parts
          (concat text "\n\n" (mapconcat #'string-trim parts "\n\n"))
        text))))

;;;; Verbatim text

(defun efrit-mark-verbatim (start end)
  "Mark START..END as verbatim: data the model reads, not instructions.
Mention expansion skips it and providers see it unchanged."
  (interactive "r")
  (add-text-properties start end '(efrit-verbatim t))
  (when (called-interactively-p 'interactive)
    (message "Marked as verbatim: %d chars" (- end start))))

(defun efrit-verbatim-spans (text)
  "The (START . END) spans of TEXT carrying `efrit-verbatim'."
  (let ((pos 0) (out nil) (len (length text)))
    (while (< pos len)
      (let ((next (or (next-single-property-change pos 'efrit-verbatim text) len)))
        (when (get-text-property pos 'efrit-verbatim text)
          (push (cons pos next) out))
        (setq pos next)))
    (nreverse out)))

(defun efrit-verbatim-p (text pos)
  "Non-nil when POS of TEXT is inside a verbatim span."
  (and (< pos (length text)) (get-text-property pos 'efrit-verbatim text)))

;;;; Grill me

(defconst efrit-grill-me-text
  "Before doing anything: ask me the clarifying questions you need answered (use request_user_input, one question at a time if several). Do not act, edit or run anything until I have answered."
  "The instruction appended when the user asks to be grilled first.")

(defvar efrit-grill-me nil
  "Non-nil: the next send asks the model to clarify first, then clears.")

(defun efrit-grill-me ()
  "Have the model ask its clarifying questions before acting, on the next send.
A second call before sending cancels it."
  (interactive)
  (setq efrit-grill-me (not efrit-grill-me))
  (message (if efrit-grill-me
               "efrit: the next send asks the model to clarify first (C-c g again cancels)"
             "efrit: grill-me cancelled")))

(defun efrit-grill-me-suffix (_ctx)
  "Suffix provider: the grill-me instruction once, when armed."
  (when efrit-grill-me
    (setq efrit-grill-me nil)
    efrit-grill-me-text))

(add-hook 'efrit-prompt-suffix-functions #'efrit-grill-me-suffix 90)

(provide 'efrit-brief)

;;; efrit-brief.el ends here
