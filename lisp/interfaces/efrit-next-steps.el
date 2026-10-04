;;; efrit-next-steps.el --- The answer's numbered next steps, as buttons -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.9.2
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, ai, convenience

;;; Commentary:

;; When an answer ends with a numbered list under a "Next steps"
;; heading, each item becomes a button: RET or mouse-1 on it sends
;; "Do step N: <text>"; `M-1'..`M-4' in the agent buffer send the Nth
;; step of the last answer without moving there.  The "(Recommended)"
;; mark gets its own face.  ai-code-interface asks the user to reply
;; with a bare number; here the UI does it (2026-09-28).
;;
;; `efrit-next-steps-ask' (a suffix provider, off by default) asks the
;; model to end a non-code-change answer with such a list.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'efrit-brief)

(defgroup efrit-next-steps nil
  "Numbered next steps in answers, as buttons."
  :group 'efrit-agent)

(defcustom efrit-next-steps-ask nil
  "Non-nil asks the model to end answers (not code changes) with 3-4 numbered next steps."
  :type 'boolean
  :group 'efrit-next-steps)

(defface efrit-next-step
  '((t :inherit button))
  "A next-step item you can pick."
  :group 'efrit-next-steps)

(defface efrit-next-step-recommended
  '((t :inherit success :weight bold))
  "The (Recommended) mark."
  :group 'efrit-next-steps)

(defconst efrit-next-steps--heading-regexp
  "^[ \t]*\\(?:#+[ \t]*\\)?\\(?:\\*\\*\\)?\\(?:Suggested \\|Possible \\|Recommended \\)?[Nn]ext [Ss]teps?\\(?:\\*\\*\\)?:?[ \t]*$"
  "A line that introduces the next-steps list.")

(defconst efrit-next-steps--item-regexp
  "^[ \t]*\\([0-9]\\)[.)][ \t]+\\(.+\\)$"
  "A numbered item: group 1 the number, group 2 the text.")

(defvar efrit-next-steps-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'efrit-next-step-at-point)
    (define-key map [mouse-1] #'efrit-next-step-at-point)
    map)
  "Keymap on a next-step item.")

(defun efrit-next-steps-find (start end)
  "The next-step items in START..END as ((N TEXT ITEM-START ITEM-END) ...), or nil.
Only a list that follows a next-steps heading and runs to the end of
the region (modulo blank lines) counts."
  (save-excursion
    (goto-char end)
    (skip-chars-backward " \t\n")
    (let ((limit (point)) (items nil))
      (goto-char start)
      (when (re-search-forward efrit-next-steps--heading-regexp limit t)
        (forward-line 1)
        (while (and (< (point) limit) (looking-at efrit-next-steps--item-regexp))
          (push (list (string-to-number (match-string 1))
                      (string-trim (match-string-no-properties 2))
                      (match-beginning 2) (match-end 2))
                items)
          (forward-line 1)
          ;; a continuation line (indented, not numbered) belongs to the item
          (while (and (< (point) limit) (looking-at "^[ \t]+[^0-9 \t]")) (forward-line 1)))
        (skip-chars-forward " \t\n")
        (when (and items (>= (point) limit))
          (nreverse items))))))

(defun efrit-next-steps-mark (start end)
  "Turn the next-step items in START..END into buttons; return how many."
  (let ((items (efrit-next-steps-find start end)) (inhibit-read-only t))
    (dolist (it items)
      (pcase-let ((`(,n ,text ,s ,e) it))
        (add-text-properties s e (list 'efrit-next-step (cons n text)
                                       'keymap efrit-next-steps-map
                                       'mouse-face 'highlight
                                       'help-echo (format "RET or M-%d: do this step" n)))
        (add-face-text-property s e 'efrit-next-step)
        (save-excursion
          (goto-char s)
          (when (re-search-forward "(\\(?:Recommended\\|recommended\\))" e t)
            (add-face-text-property (match-beginning 0) (match-end 0) 'efrit-next-step-recommended)))))
    (length items)))

(defun efrit-next-steps-of-last-answer ()
  "The next-step items of the last answer in this buffer, as ((N . TEXT) ...)."
  (let ((pos (point-min)) (out nil))
    (while (setq pos (text-property-not-all pos (point-max) 'efrit-next-step nil))
      (let ((v (get-text-property pos 'efrit-next-step)))
        ;; one item can be several runs: the final render pass marks
        ;; the last line again with a fresh cons (live run 2026-09-28
        ;; 17:28 listed step 3 twice); same number and text is one item
        (unless (equal v (car out)) (push v out))
        (setq pos (or (next-single-property-change pos 'efrit-next-step) (point-max)))))
    ;; the last answer's list is the last run of items; keep only the
    ;; tail whose numbers restart at 1
    (let ((items (nreverse out)) (last nil))
      (dolist (it items)
        (when (= 1 (car it)) (setq last nil))
        (push it last))
      (nreverse last))))

(declare-function efrit-submit "efrit-agent-input")

(defun efrit-next-step-send (n text)
  "Send step N, TEXT, as the next turn."
  (require 'efrit-agent-input)
  (unless (efrit-submit (format "do step %d: %s" n (truncate-string-to-width text 60 nil nil "…"))
                        (format "Do next step %d from your last answer: %s" n text))
    (user-error "efrit is busy; try again when the turn ends")))

(defun efrit-next-step-at-point (&optional event)
  "Send the next step at point (or under EVENT)."
  (interactive (list last-nonmenu-event))
  (let* ((pos (if (and event (listp event) (eventp event)) (posn-point (event-end event)) (point)))
         (step (get-text-property pos 'efrit-next-step)))
    (unless step (user-error "No next step here"))
    (efrit-next-step-send (car step) (cdr step))))

(defun efrit-next-step (n)
  "Send step N of the last answer's next steps (M-1 .. M-4 in the agent buffer)."
  (interactive "p")
  (let ((step (assq n (efrit-next-steps-of-last-answer))))
    (unless step (user-error "The last answer has no step %d" n))
    (efrit-next-step-send n (cdr step))))

(defun efrit-next-step-1 () (interactive) (efrit-next-step 1))
(defun efrit-next-step-2 () (interactive) (efrit-next-step 2))
(defun efrit-next-step-3 () (interactive) (efrit-next-step 3))
(defun efrit-next-step-4 () (interactive) (efrit-next-step 4))

;;;; Asking the model for the list

(defconst efrit-next-steps--suffix
  "Unless this is a code change, end your answer with a heading `Next steps` and 3-4 numbered options, one marked (Recommended). The user picks one by number."
  "Appended to the prompt when `efrit-next-steps-ask' is on.")

(defun efrit-next-steps--code-change-p (text)
  "A cheap guess whether TEXT asks for a code change."
  (string-match-p "\\b\\(fix\\|change\\|edit\\|refactor\\|rename\\|implement\\|add\\|remove\\|write\\|update\\|apply\\)\\b"
                  (downcase text)))

(defun efrit-next-steps-suffix (ctx)
  "Suffix provider: ask for next steps on non-code-change prompts."
  (when (and efrit-next-steps-ask
             (not (efrit-next-steps--code-change-p (efrit-prompt-context-text ctx))))
    efrit-next-steps--suffix))

(add-hook 'efrit-prompt-suffix-functions #'efrit-next-steps-suffix 50)

(provide 'efrit-next-steps)

;;; efrit-next-steps.el ends here
