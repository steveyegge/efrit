;;; efrit-agent-fold.el --- Fold a finished turn's work under one line -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.13.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, ai

;;; Commentary:

;; A finished turn is a user line, forty rows of tool calls, reviews
;; and sandbox notes, and the answer.  Once the turn is over the rows
;; are history: they cost a screen of scrolling to get from one
;; answer to the previous one (tzz, 2026-10-20: "collapsed into one
;; dropdown grouped with the answer, expanding them grows upwards").
;;
;; When a turn ends, its work rows (everything between the user line
;; and the answer that is not prose) become `invisible' under one
;; header line placed where the first of them was:
;;
;;   ❯ fix the spinner
;;   ▶ 14 steps · 3 reviews · 1 denied          <- the fold
;;   The spinner is gone.  …                     <- the answer, visible
;;
;; RET or mouse-1 on the header shows the rows again, between the
;; header and the answer, so the answer stays where it was and the
;; detail opens above it.  The running turn is never folded; a turn
;; the user unfolded stays unfolded until they fold it again.  Tool
;; bodies keep their own folds inside.  isearch opens a folded turn
;; the same way it opens a tool body.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'efrit-agent-core)

(declare-function efrit-agent--set-tool-expanded "efrit-agent-tools")

(defgroup efrit-agent-fold nil
  "Folding of finished turns in the agent buffer."
  :group 'efrit-agent)

(defcustom efrit-agent-fold-past-turns t
  "When non-nil, a turn's tool and review rows fold under one line when it ends."
  :type 'boolean
  :group 'efrit-agent-fold)

(defface efrit-agent-fold-header
  '((t :inherit shadow))
  "Face of the one-line summary of a folded turn's work."
  :group 'efrit-agent-fold)

(defconst efrit-agent-fold--work-types
  '(tool-call question todos-block error-message review-note steer-note
    sandbox-note limits-note question-note interrupted-note system-note)
  "`efrit-type' values that are a turn's work, not its answer.
A `*-note' type not listed here is work too (see `efrit-agent-fold--work-p').")

(defun efrit-agent-fold--work-p (type)
  "Non-nil when TYPE marks a row that folds."
  (and type
       (or (memq type efrit-agent-fold--work-types)
           (string-suffix-p "-note" (symbol-name type)))))

(defvar-local efrit-agent-fold--counter 0)

(defun efrit-agent-fold--ensure-spec ()
  "Make `efrit-turn-work' invisible in this buffer.
`efrit-agent-mode' adds it for new buffers; a buffer alive across a
reload did not have it, so folded rows stayed on screen (2026-10-21:
\"I see all the history when I scroll back\")."
  (unless (and (listp buffer-invisibility-spec)
               (memq 'efrit-turn-work buffer-invisibility-spec))
    (add-to-invisibility-spec 'efrit-turn-work)))

;;;; Finding the turn

(defun efrit-agent-fold--runs (start end)
  "The (TYPE BEG END) runs of `efrit-type' between START and END."
  (let ((runs nil) (pos start))
    (while (< pos end)
      (let* ((next (min end (or (next-single-property-change pos 'efrit-type nil end) end)))
             (type (get-text-property pos 'efrit-type)))
        (push (list type pos next) runs)
        (setq pos next)))
    (nreverse runs)))

(defun efrit-agent-fold--turn-starts ()
  "Positions where user turns start, in order, up to the conversation end."
  (when (and efrit-agent--conversation-end (marker-position efrit-agent--conversation-end))
    (let ((end (marker-position efrit-agent--conversation-end))
          (pos (point-min)) (starts nil))
      (while (setq pos (text-property-any pos end 'efrit-type 'user-message))
        (push pos starts)
        (setq pos (or (next-single-property-change pos 'efrit-id nil end) end)))
      (nreverse starts))))

(defun efrit-agent-fold--turn-bounds (start next)
  "(USER-END . TURN-END) for the turn whose user line starts at START; NEXT is the next turn's start or nil."
  (let ((end (or next (marker-position efrit-agent--conversation-end))))
    (cons (or (next-single-property-change start 'efrit-type nil end) end) end)))

(defun efrit-agent-fold--last-turn-bounds ()
  "(USER-END . CONVERSATION-END) of the last turn, or nil when there is none."
  (when-let* ((starts (efrit-agent-fold--turn-starts)))
    (efrit-agent-fold--turn-bounds (car (last starts)) nil)))

(defun efrit-agent-fold--work-span (user-end conv-end)
  "The (FIRST-WORK-BEG . LAST-WORK-END) inside USER-END..CONV-END, or nil.
The answer is the last prose run; work before it folds, prose between
work runs (narration) folds with it.  Rows after the answer stay out:
a question the agent asked at the end of its text (the prose-choice
fallback appends one) was folded with the answer on 2026-10-21, so
the turn showed only its summary line and the prompt was hidden."
  (let* ((runs (efrit-agent-fold--runs user-end conv-end))
         (answer (cl-find 'claude-message (reverse runs) :key #'car))
         (before (if answer
                     (cl-subseq runs 0 (cl-position answer runs))
                   runs))
         (work (cl-remove-if-not (lambda (r) (efrit-agent-fold--work-p (car r))) before)))
    (when work
      (cons (nth 1 (car work)) (nth 2 (car (last work)))))))

(defun efrit-agent-fold--summary (beg end)
  "A short account of the rows in BEG..END: steps, reviews, denials."
  (let ((steps 0) (reviews 0) (rejected 0) (denied 0) (questions 0) (seen nil))
    (dolist (r (efrit-agent-fold--runs beg end))
      (pcase-let ((`(,type ,b ,_e) r))
        (pcase type
          ('tool-call
           (let ((id (get-text-property b 'efrit-id))
                 (name (get-text-property b 'efrit-tool-name)))
             (unless (member id seen)
               (push id seen)
               (cond
                ((equal name "review")
                 (cl-incf reviews)
                 (unless (get-text-property b 'efrit-tool-success) (cl-incf rejected)))
                ((equal name "session_complete") nil)
                (t (cl-incf steps)
                   (unless (get-text-property b 'efrit-tool-success) (cl-incf denied)))))))
          ('question (cl-incf questions)))))
    (string-join
     (delq nil
           (list (format "%d step%s" steps (if (= steps 1) "" "s"))
                 (and (> reviews 0) (format "%d review%s" reviews (if (= reviews 1) "" "s")))
                 (and (> rejected 0) (format "%d rejected" rejected))
                 (and (> denied 0) (format "%d failed or denied" denied))
                 (and (> questions 0) (format "%d question%s" questions (if (= questions 1) "" "s")))))
     " · ")))

;;;; Folding and unfolding

(defvar efrit-agent-fold-header-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'efrit-agent-fold-toggle)
    (define-key map (kbd "TAB") #'efrit-agent-fold-toggle)
    (define-key map [mouse-1] #'efrit-agent-fold-toggle-at-mouse)
    map)
  "Keys on a folded turn's header line.")

(defun efrit-agent-fold--header-text (fold-id summary open)
  (propertize (concat "  " (efrit-agent--char (if open 'expand-expanded 'expand-collapsed))
                      " " summary "\n")
              'face 'efrit-agent-fold-header
              'font-lock-face 'efrit-agent-fold-header
              'efrit-type 'turn-fold
              'efrit-id fold-id
              'efrit-fold-open open
              'efrit-fold-summary summary
              'keymap efrit-agent-fold-header-map
              'mouse-face 'highlight
              'help-echo "RET, mouse-1: show / hide this turn's steps"
              'read-only t 'field 'output 'fontified t))

(defun efrit-agent-fold--fold-bounds (bounds)
  "Fold the work inside BOUNDS, (USER-END . TURN-END); return the fold id or nil."
  (when-let* ((span (efrit-agent-fold--work-span (car bounds) (cdr bounds))))
    (unless (text-property-any (car bounds) (cdr bounds) 'efrit-type 'turn-fold)
      (efrit-agent-fold--ensure-spec)
      (let* ((beg (car span)) (end (cdr span))
             (fold-id (format "fold-%d" (cl-incf efrit-agent-fold--counter)))
             (summary (efrit-agent-fold--summary beg end)))
        (efrit-agent--with-render
          (let ((inhibit-read-only t))
            ;; the rows: hidden, and tagged so the header finds them
            (add-text-properties beg end (list 'efrit-fold fold-id))
            (put-text-property beg end 'invisible 'efrit-turn-work)
            (goto-char beg)
            (insert (efrit-agent-fold--header-text fold-id summary nil))))
        fold-id))))

(defun efrit-agent-fold-last-turn ()
  "Fold the work of the last turn under one header line.
Does nothing when there is no work, or the turn is already folded.
Returns the fold id when a fold was made."
  (when-let* ((bounds (efrit-agent-fold--last-turn-bounds)))
    (efrit-agent-fold--fold-bounds bounds)))

(defun efrit-agent-fold-history (&optional include-last)
  "Fold every finished turn in this buffer that is not folded yet.
The last turn is left alone unless INCLUDE-LAST (it may be running).
For transcripts that predate the fold, or buffers alive across a
reload.  Returns how many folds were made."
  (interactive "P")
  (let* ((starts (efrit-agent-fold--turn-starts))
         (pairs (cl-loop for (start next) on starts collect (cons start next)))
         (n 0))
    (unless include-last (setq pairs (butlast pairs)))
    ;; fold from the end so earlier positions stay valid
    (dolist (pair (reverse pairs))
      (when (efrit-agent-fold--fold-bounds (efrit-agent-fold--turn-bounds (car pair) (cdr pair)))
        (cl-incf n)))
    (when (called-interactively-p 'any)
      (message "efrit: folded %d turn%s" n (if (= n 1) "" "s")))
    n))

(defun efrit-agent-fold--header-bounds (fold-id)
  "(BEG . END) of FOLD-ID's header line."
  (save-excursion
    (goto-char (point-min))
    (let ((pos (point-min)) (found nil))
      (while (and (not found) (setq pos (text-property-any pos (point-max) 'efrit-type 'turn-fold)))
        (if (equal (get-text-property pos 'efrit-id) fold-id)
            (setq found (cons pos (or (next-single-property-change pos 'efrit-id) (point-max))))
          (setq pos (or (next-single-property-change pos 'efrit-id) (point-max)))))
      found)))

(defun efrit-agent-fold--work-bounds (fold-id)
  "(BEG . END) of the rows FOLD-ID hides, or nil."
  (let ((beg (text-property-any (point-min) (point-max) 'efrit-fold fold-id)))
    (when beg
      (let ((end beg) (pos beg))
        (while (and pos (equal (get-text-property pos 'efrit-fold) fold-id))
          (setq end (or (next-single-property-change pos 'efrit-fold) (point-max)))
          (setq pos (and (< end (point-max))
                         (equal (get-text-property end 'efrit-fold) fold-id)
                         end)))
        (cons beg end)))))

(defun efrit-agent-fold-set (fold-id open)
  "Show (OPEN non-nil) or hide the rows under FOLD-ID; redraw its header."
  (when-let* ((header (efrit-agent-fold--header-bounds fold-id))
              (work (efrit-agent-fold--work-bounds fold-id)))
    (efrit-agent-fold--ensure-spec)
    (let ((summary (get-text-property (car header) 'efrit-fold-summary)))
      (efrit-agent--with-render
        (let ((inhibit-read-only t))
          (if open
              ;; tool bodies inside keep their own `efrit-tool-body'
              ;; invisibility: only lift ours
              (let ((pos (car work)))
                (while (< pos (cdr work))
                  (let ((next (or (next-single-property-change pos 'invisible nil (cdr work)) (cdr work))))
                    (when (eq (get-text-property pos 'invisible) 'efrit-turn-work)
                      (remove-list-of-text-properties pos next '(invisible)))
                    (setq pos next))))
            (let ((pos (car work)))
              (while (< pos (cdr work))
                (let ((next (or (next-single-property-change pos 'invisible nil (cdr work)) (cdr work))))
                  (unless (get-text-property pos 'invisible)
                    (put-text-property pos next 'invisible 'efrit-turn-work))
                  (setq pos next)))))
          (save-excursion
            (goto-char (car header))
            (delete-region (car header) (cdr header))
            (insert (efrit-agent-fold--header-text fold-id summary open)))))
      t)))

(defun efrit-agent-fold-toggle ()
  "Show or hide the steps of the folded turn whose header is at point."
  (interactive)
  (let ((id (and (eq (get-text-property (point) 'efrit-type) 'turn-fold)
                 (get-text-property (point) 'efrit-id))))
    (if id
        (efrit-agent-fold-set id (not (get-text-property (point) 'efrit-fold-open)))
      (user-error "No folded turn here"))))

(defun efrit-agent-fold-toggle-at-mouse (event)
  "Toggle the folded turn under the mouse EVENT."
  (interactive "e")
  (when-let* ((pos (posn-point (event-end event))))
    (save-excursion (goto-char pos) (efrit-agent-fold-toggle))))

(defun efrit-agent-fold-open-containing (pos)
  "Open the turn fold that hides POS, if any.  Returns the fold id."
  (when-let* ((id (get-text-property pos 'efrit-fold)))
    (when (eq (get-text-property pos 'invisible) 'efrit-turn-work)
      (efrit-agent-fold-set id t)
      id)))

(defun efrit-agent-fold-all (open)
  "Open (OPEN non-nil) or close every turn fold in the buffer."
  (interactive (list current-prefix-arg))
  (let ((pos (point-min)) (ids nil))
    (while (setq pos (text-property-any pos (point-max) 'efrit-type 'turn-fold))
      (push (get-text-property pos 'efrit-id) ids)
      (setq pos (or (next-single-property-change pos 'efrit-id) (point-max))))
    (dolist (id (nreverse ids)) (efrit-agent-fold-set id open))
    (message "efrit: %d turn fold%s %s" (length ids) (if (= 1 (length ids)) "" "s")
             (if open "opened" "closed"))))

;; Buffers alive across a reload: the invisibility spec, and the
;; history folded (the turns that ended before this code loaded)
(dolist (b (buffer-list))
  (when (and (buffer-live-p b) (eq (buffer-local-value 'major-mode b) 'efrit-agent-mode))
    (with-current-buffer b
      (efrit-agent-fold--ensure-spec)
      (when efrit-agent-fold-past-turns
        (ignore-errors (efrit-agent-fold-history))))))

(provide 'efrit-agent-fold)

;;; efrit-agent-fold.el ends here
