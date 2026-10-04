;;; efrit-candidates.el --- Pick one of several answers -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.9.2
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, ai

;;; Commentary:

;; When the model is asked for several candidates (commit messages,
;; rewrites, names), the user picks one.  `efrit-candidates-choose'
;; shows them in a buffer, one section each, numbered; `n'/`p' move,
;; `1'..`9' and RET pick, `e` edits the one at point before picking,
;; `q' gives up.  The raw text of each candidate is kept in a table
;; keyed by its hash and never re-read from the display, so display
;; decoration cannot leak into what is inserted (copilot's panel
;; trick, 2026-09-28).  ON-CHOOSE receives the chosen string; it is
;; called from the command loop after the panel is gone.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defface efrit-candidates-number
  '((t :inherit font-lock-keyword-face :weight bold))
  "The number before a candidate."
  :group 'efrit)

(defface efrit-candidates-current
  '((t :inherit highlight :extend t))
  "The candidate at point."
  :group 'efrit)

(defvar-local efrit-candidates--table nil
  "Hash of candidate key -> raw text.")
(defvar-local efrit-candidates--on-choose nil)
(defvar-local efrit-candidates--overlay nil)

(defvar efrit-candidates-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'efrit-candidates-next)
    (define-key map (kbd "p") #'efrit-candidates-previous)
    (define-key map (kbd "TAB") #'efrit-candidates-next)
    (define-key map (kbd "<backtab>") #'efrit-candidates-previous)
    (define-key map (kbd "RET") #'efrit-candidates-pick)
    (define-key map (kbd "e") #'efrit-candidates-edit-and-pick)
    (define-key map (kbd "q") #'efrit-candidates-quit)
    (dotimes (i 9)
      (define-key map (kbd (number-to-string (1+ i)))
                  (lambda () (interactive) (efrit-candidates-pick-number (1+ i)))))
    map))

(define-derived-mode efrit-candidates-mode special-mode "Efrit-Pick"
  "Choose one of the model's candidates.
\\{efrit-candidates-mode-map}"
  (setq-local truncate-lines nil)
  (setq header-line-format
        (concat " " (propertize "Pick a candidate" 'face 'bold)
                (propertize "   n/p move   RET or 1-9 pick   e edit then pick   q none" 'face 'shadow))))

(defun efrit-candidates--key (text)
  (secure-hash 'sha1 text))

(defun efrit-candidates--bounds-at (pos)
  "The (START . END) of the candidate section at POS, or nil."
  (when-let* ((key (get-text-property pos 'efrit-candidate)))
    (cons (or (previous-single-property-change (1+ pos) 'efrit-candidate) (point-min))
          (or (next-single-property-change pos 'efrit-candidate) (point-max)))))

(defun efrit-candidates--highlight ()
  (let ((b (efrit-candidates--bounds-at (point))))
    (if (null b)
        (when efrit-candidates--overlay (delete-overlay efrit-candidates--overlay))
      (unless efrit-candidates--overlay
        (setq efrit-candidates--overlay (make-overlay (car b) (cdr b)))
        (overlay-put efrit-candidates--overlay 'face 'efrit-candidates-current))
      (move-overlay efrit-candidates--overlay (car b) (cdr b)))))

(defun efrit-candidates--goto (n)
  "Move to candidate N (1-based); nil if there is none."
  (goto-char (point-min))
  (let ((i 0) (found nil))
    (while (and (not found)
                (setq found (text-property-not-all (point) (point-max) 'efrit-candidate nil)))
      (goto-char found)
      (cl-incf i)
      (if (= i n)
          (progn (efrit-candidates--highlight) (setq found t))
        (goto-char (or (next-single-property-change (point) 'efrit-candidate) (point-max)))
        (setq found nil)
        (when (eobp) (setq found 'none))))
    (eq found t)))

(defun efrit-candidates-next ()
  "Move to the next candidate."
  (interactive)
  (when-let* ((b (efrit-candidates--bounds-at (point))))
    (goto-char (cdr b)))
  (when-let* ((next (text-property-not-all (point) (point-max) 'efrit-candidate nil)))
    (goto-char next))
  (efrit-candidates--highlight))

(defun efrit-candidates-previous ()
  "Move to the previous candidate."
  (interactive)
  (let ((here (or (car (efrit-candidates--bounds-at (point))) (point))))
    ;; back over this candidate and the gap before it, onto the previous one
    (let ((pos (1- here)))
      (while (and (> pos (point-min)) (not (get-text-property pos 'efrit-candidate)))
        (cl-decf pos))
      (when (get-text-property pos 'efrit-candidate)
        (goto-char (car (efrit-candidates--bounds-at pos))))))
  (efrit-candidates--highlight))

(defun efrit-candidates--finish (text)
  "Close the panel and hand TEXT to the chooser."
  (let ((on-choose efrit-candidates--on-choose))
    (quit-window t)
    (when (and on-choose text)
      (run-at-time 0 nil on-choose text))))

(defun efrit-candidates-pick ()
  "Pick the candidate at point."
  (interactive)
  (let ((key (get-text-property (point) 'efrit-candidate)))
    (unless key (user-error "Not on a candidate"))
    (efrit-candidates--finish (gethash key efrit-candidates--table))))

(defun efrit-candidates-pick-number (n)
  "Pick candidate N."
  (interactive "p")
  (if (efrit-candidates--goto n)
      (efrit-candidates-pick)
    (user-error "No candidate %d" n)))

(defun efrit-candidates-edit-and-pick ()
  "Edit the candidate at point, then pick the edited text."
  (interactive)
  (require 'efrit-ui-helpers)
  (let ((key (get-text-property (point) 'efrit-candidate)))
    (unless key (user-error "Not on a candidate"))
    (let ((edited (condition-case nil
                      (efrit-edit-in-buffer (gethash key efrit-candidates--table) "the candidate")
                    (quit nil))))
      (if edited
          (efrit-candidates--finish (string-trim-right edited))
        (message "Edit cancelled")))))

(defun efrit-candidates-quit ()
  "Close the panel without choosing."
  (interactive)
  (efrit-candidates--finish nil))

(declare-function efrit-edit-in-buffer "efrit-ui-helpers")

;;;###autoload
(defun efrit-candidates-choose (candidates on-choose &optional title)
  "Show CANDIDATES (strings) in a panel; call ON-CHOOSE with the pick.
TITLE names the buffer.  One candidate is chosen without a panel."
  (cond
   ((null candidates) (message "efrit: no candidates"))
   ((null (cdr candidates)) (funcall on-choose (car candidates)))
   (t
    (let ((buf (get-buffer-create (format "*efrit pick: %s*" (or title "candidates")))))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (efrit-candidates-mode)
          (setq efrit-candidates--table (make-hash-table :test 'equal)
                efrit-candidates--on-choose on-choose)
          (cl-loop for text in candidates for i from 1 do
                   (let ((key (efrit-candidates--key text)))
                     (puthash key text efrit-candidates--table)
                     (let ((start (point)))
                       (insert (propertize (format "%d. " i) 'face 'efrit-candidates-number)
                               text "\n\n")
                       (put-text-property start (point) 'efrit-candidate key))))
          (goto-char (point-min))
          (efrit-candidates--highlight)))
      (pop-to-buffer buf '((display-buffer-reuse-window display-buffer-at-bottom)
                           (window-height . fit-window-to-buffer)))
      buf))))

(provide 'efrit-candidates)

;;; efrit-candidates.el ends here
