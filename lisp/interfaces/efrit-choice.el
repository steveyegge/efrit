;;; efrit-choice.el --- One Emacs-native chooser for efrit's questions -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.13.6
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, ai

;;; Commentary:

;; Every question efrit puts to the user is one shape: a heading, some
;; detail, and a small set of choices answered with one key.  Three
;; places asked in three ways: the sandbox has a transient; the agent
;; buffer's request_user_input has another; confirm_action used a bare
;; `y-or-n-p' in the echo area, and a list of twenty toggles was prose
;; with "type the numbers to flip" (tzz, 2026-10-21: "the confirmation
;; dialog here could have been nicer"; "presenting long lists of
;; options with type number to flip is awkward when the native elisp
;; UI is available").
;;
;; `efrit-choice-read' is that one shape, built on transient:
;;
;;   (efrit-choice-read "Install kirigami from GitHub?"
;;                      '(("y" "yes" yes) ("n" "no" no))
;;                      :detail "package-vc, writes ~/.emacs.d/elpa/kirigami/")
;;
;;   (efrit-choice-read "Which of these to adopt?"
;;                      '(("16" "package-load-indicator-mode" 16 t)
;;                        ("18" "package-autosuggest-mode" 18 t) …)
;;                      :multi t)
;;
;; Single choice: a key answers.  Multi: keys toggle, RET submits, with
;; the model's picks preselected.  Both allow a typed answer (t) and
;; C-g.  The menu waits in a `recursive-edit' the way the sandbox menu
;; does, reopens if it closes unanswered (another transient, a focus
;; change), and never times out: a prompt is answered by the user or
;; not at all.  Without a display (batch, a terminal with no transient)
;; it falls back to `read-multiple-choice' / `completing-read-multiple'.
;;
;; Keys are assigned from the labels when not given: digits 1-9, then
;; letters a-z skipping efrit's own (t, q, RET).  More than 35 choices
;; is a list, not a menu: the caller gets `completing-read-multiple'.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'efrit-log)
(require 'efrit-events)
(require 'eieio)

;; transient is loaded on demand (`efrit-choice--menu-available-p');
;; the compiler needs the names it would otherwise learn from it
(defvar transient--prefix)
(declare-function transient-quit-all "transient" ())
(declare-function transient-define-prefix "transient")
(eieio-declare-slots command)

(defgroup efrit-choice nil
  "efrit's chooser for questions to the user."
  :group 'efrit)

(defface efrit-choice-heading
  '((t :inherit bold))
  "Face of the question in the chooser."
  :group 'efrit-choice)

(defface efrit-choice-detail
  '((t :inherit shadow))
  "Face of the detail lines under the question."
  :group 'efrit-choice)

(defface efrit-choice-selected
  '((t :inherit success :weight bold))
  "Face of a toggled-on choice in a multi-select chooser."
  :group 'efrit-choice)

(defcustom efrit-choice-menu-max 35
  "Most choices shown as a transient menu; beyond that, completion is used."
  :type 'integer
  :group 'efrit-choice)

;;;; State of the open chooser

(defvar efrit-choice--heading nil)
(defvar efrit-choice--detail nil)
(defvar efrit-choice--items nil
  "The open chooser's items: (KEY LABEL VALUE SELECTED).")
(defvar efrit-choice--multi nil)
(defvar efrit-choice--answer 'pending
  "`pending', `typed', `cancelled', or the chosen value(s).")
(defvar efrit-choice--depth nil)
(defvar efrit-choice--allow-typed t)

(defconst efrit-choice--reserved-keys '("t" "q" "RET" "C-g")
  "Keys the chooser itself uses; never assigned to an item.")

(defun efrit-choice--assign-keys (items)
  "ITEMS with a key for each: (KEY LABEL VALUE SELECTED), keys unique.
An item is (KEY LABEL VALUE [SELECTED]) or (LABEL VALUE [SELECTED]);
a nil KEY is filled from 1-9 then a-z."
  (let ((used nil) (pool (append (mapcar #'number-to-string (number-sequence 1 9))
                                 (mapcar #'char-to-string (number-sequence ?a ?z))))
        (out nil))
    (dolist (it items)
      (let* ((full (if (and (stringp (car it)) (>= (length it) 3)
                            (or (null (nth 1 it)) (stringp (nth 1 it))))
                       it
                     (cons nil it)))
             (key (nth 0 full)) (label (nth 1 full)) (value (nth 2 full)) (sel (nth 3 full)))
        (when (or (null key) (member key used) (member key efrit-choice--reserved-keys))
          (setq key (cl-find-if (lambda (k) (not (or (member k used) (member k efrit-choice--reserved-keys)))) pool)))
        (push key used)
        (push (list key label value (and sel t)) out)))
    (nreverse out)))

;;;; The transient

(defun efrit-choice--description ()
  (concat (propertize (or efrit-choice--heading "Choose") 'face 'efrit-choice-heading)
          (when (and efrit-choice--detail (not (string-empty-p efrit-choice--detail)))
            (concat "\n" (propertize (replace-regexp-in-string "^" "  " efrit-choice--detail) 'face 'efrit-choice-detail)))
          (when efrit-choice--multi
            (propertize "\n  keys toggle · RET submits" 'face 'efrit-choice-detail))
          "\n"))

(defun efrit-choice--item-description (item)
  (let ((label (truncate-string-to-width (nth 1 item) (max 30 (- (frame-width) 14)) nil nil "…")))
    (if efrit-choice--multi
        (if (nth 3 item)
            (propertize (concat "[x] " label) 'face 'efrit-choice-selected)
          (concat "[ ] " label))
      label)))

(defun efrit-choice--pick (item)
  "The suffix action for ITEM: answer (single) or toggle (multi)."
  (if efrit-choice--multi
      (setf (nth 3 item) (not (nth 3 item)))
    (setq efrit-choice--answer (nth 2 item))))

(defun efrit-choice--submit ()
  "Submit the toggled set of a multi chooser."
  (interactive)
  (setq efrit-choice--answer (mapcar (lambda (it) (nth 2 it)) (cl-remove-if-not (lambda (it) (nth 3 it)) efrit-choice--items))))

(defun efrit-choice--type-answer ()
  "Close the menu; the caller reads a typed answer."
  (interactive)
  (setq efrit-choice--answer 'typed))

(defun efrit-choice--cancel ()
  "Close the menu with no answer (like C-g)."
  (interactive)
  (setq efrit-choice--answer 'cancelled))

(defun efrit-choice--suffixes ()
  "Transient suffix specs for the open items, in columns of 12."
  (let ((specs (mapcar (lambda (item)
                         (list (nth 0 item)
                               (lambda () (interactive) (efrit-choice--pick item))
                               :description (lambda () (efrit-choice--item-description item))
                               :transient (and efrit-choice--multi t)))
                       efrit-choice--items)))
    (let (cols)
      (while specs
        (push (vconcat (seq-take specs 12)) cols)
        (setq specs (nthcdr 12 specs)))
      (nreverse cols))))

;; The prefix is (re)defined at runtime by `efrit-choice--define-menu';
;; the compiler cannot see it (snapshot Emacs: "not known to be defined")
(declare-function efrit-choice-menu "efrit-choice" (&optional arg))

(defun efrit-choice--define-menu ()
  "Define `efrit-choice-menu' for the open items.  Redefined per question:
the suffixes are the items."
  (when (require 'transient nil t)
    (eval
     `(transient-define-prefix efrit-choice-menu ()
        "Answer efrit's question."
        [:description efrit-choice--description
         ,@(efrit-choice--suffixes)
         [,@(when efrit-choice--multi '(("RET" "submit the toggled set" efrit-choice--submit)))
          ,@(when efrit-choice--allow-typed '(("t" "type an answer" efrit-choice--type-answer)))
          ("q" "cancel" efrit-choice--cancel)]])
     t)
    t))

(defun efrit-choice--exit-recursive-edit ()
  "Leave the recursive edit once answered; reopen the menu otherwise."
  (when (and efrit-choice--depth (= (recursion-depth) efrit-choice--depth))
    (if (eq efrit-choice--answer 'pending)
        (run-at-time 0.2 nil
                     (lambda ()
                       (when (and (eq efrit-choice--answer 'pending) efrit-choice--depth
                                  (= (recursion-depth) efrit-choice--depth))
                         (efrit-log 'info "choice: menu closed without an answer; reopening")
                         (condition-case err (call-interactively #'efrit-choice-menu)
                           (error (efrit-log 'warn "choice: could not reopen: %s" (error-message-string err)))))))
      (exit-recursive-edit))))

(defun efrit-choice--menu-available-p ()
  (and (not noninteractive) (display-graphic-p) (require 'transient nil t)))

;;;; Fallbacks without a menu

(defun efrit-choice--read-plain (heading items multi)
  "Ask with the minibuffer: `read-multiple-choice' or `completing-read-multiple'."
  (if multi
      (let* ((labels (mapcar (lambda (it) (nth 1 it)) items))
             (initial (mapconcat (lambda (it) (nth 1 it)) (cl-remove-if-not (lambda (it) (nth 3 it)) items) ","))
             (picked (completing-read-multiple (concat heading " (toggle by name): ") labels nil t initial)))
        (mapcar (lambda (it) (nth 2 it)) (cl-remove-if-not (lambda (it) (member (nth 1 it) picked)) items)))
    (let* ((choices (mapcar (lambda (it) (list (string-to-char (nth 0 it)) (nth 1 it))) items))
           (got (read-multiple-choice heading choices)))
      (nth 2 (cl-find-if (lambda (it) (= (string-to-char (nth 0 it)) (car got))) items)))))

;;;; Entry point

(cl-defun efrit-choice-read (heading items &key detail multi (allow-typed t) label)
  "Ask the user HEADING with ITEMS; return the chosen value.
ITEMS: (KEY LABEL VALUE [SELECTED]) or (LABEL VALUE [SELECTED]).
Single choice returns one VALUE; MULTI returns the list of VALUEs
toggled on at RET.  A typed answer (t, when ALLOW-TYPED) returns
\(:typed . STRING).  Cancel (q, C-g) returns `cancelled'.  DETAIL is
shown under the heading.  LABEL names the prompt for the prompt turn
and notifications.  Waits as long as the user takes."
  (let* ((items (efrit-choice--assign-keys items))
         (label (or label heading)))
    (efrit-with-prompt-turn label nil
      (efrit-with-user-waiting
        (cond
         ((or (not (efrit-choice--menu-available-p))
              (> (length items) efrit-choice-menu-max))
          (condition-case nil
              (efrit-choice--read-plain heading items multi)
            (quit 'cancelled)))
         (t
          (setq efrit-choice--heading heading
                efrit-choice--detail detail
                efrit-choice--items items
                efrit-choice--multi multi
                efrit-choice--allow-typed allow-typed
                efrit-choice--answer 'pending)
          (efrit-choice--define-menu)
          (let ((efrit-choice--depth (1+ (recursion-depth))))
            (unwind-protect
                (progn
                  (add-hook 'transient-post-exit-hook #'efrit-choice--exit-recursive-edit)
                  (run-at-time 0 nil (lambda () (call-interactively #'efrit-choice-menu)))
                  (condition-case nil
                      (let ((inhibit-quit nil)) (recursive-edit))
                    (quit (setq efrit-choice--answer 'cancelled))))
              (remove-hook 'transient-post-exit-hook #'efrit-choice--exit-recursive-edit)))
          (pcase efrit-choice--answer
            ('pending 'cancelled)
            ('typed (condition-case nil
                        (cons :typed (read-string (concat heading " ")))
                      (quit 'cancelled)))
            (answer answer))))))))

;;;; Non-blocking use: the agent buffer's questions

(defvar efrit-choice--on-answer nil
  "Callback of a non-blocking chooser, called with the answer once.")

(defun efrit-choice--deliver ()
  "After the menu closes in non-blocking mode: hand the answer to the callback."
  (when (and efrit-choice--on-answer (not (eq efrit-choice--answer 'pending)))
    (remove-hook 'transient-post-exit-hook #'efrit-choice--deliver)
    (let ((fn efrit-choice--on-answer) (answer efrit-choice--answer))
      (setq efrit-choice--on-answer nil)
      (funcall fn answer))))

(cl-defun efrit-choice-open (heading items on-answer &key detail multi (allow-typed t))
  "Open the chooser for HEADING and ITEMS without waiting; call ON-ANSWER later.
ON-ANSWER gets the same values `efrit-choice-read' returns, or
`typed' when the user chose to type (the caller then reads from its
own input).  A menu closed unanswered is not reopened here: the agent
buffer keeps the question in its prompt and 1-9 keys, so the user can
answer there.  Returns non-nil when a menu was opened."
  (when (and (efrit-choice--menu-available-p) (<= (length items) efrit-choice-menu-max))
    (setq efrit-choice--heading heading
          efrit-choice--detail detail
          efrit-choice--items (efrit-choice--assign-keys items)
          efrit-choice--multi multi
          efrit-choice--allow-typed allow-typed
          efrit-choice--answer 'pending
          efrit-choice--depth nil
          efrit-choice--on-answer on-answer)
    (efrit-choice--define-menu)
    (add-hook 'transient-post-exit-hook #'efrit-choice--deliver)
    (run-at-time 0 nil (lambda () (condition-case err (call-interactively #'efrit-choice-menu)
                                    (error (efrit-log 'warn "choice: %s" (error-message-string err))))))
    t))

(defun efrit-choice-close ()
  "Close an open non-blocking chooser without an answer (the question was answered elsewhere)."
  (when efrit-choice--on-answer
    (setq efrit-choice--on-answer nil)
    (remove-hook 'transient-post-exit-hook #'efrit-choice--deliver)
    (when (and (bound-and-true-p transient--prefix)
               (eq (oref transient--prefix command) 'efrit-choice-menu))
      (transient-quit-all))))

(provide 'efrit-choice)

;;; efrit-choice.el ends here
