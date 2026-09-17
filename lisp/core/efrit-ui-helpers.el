;;; efrit-ui-helpers.el --- Small blocking UI primitives -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; Two primitives used by the permission prompt and elsewhere:
;;
;; - `efrit-edit-in-buffer': edit arbitrary text in a real buffer with
;;   a mode, blocking via `recursive-edit', returning the text or
;;   signalling `quit' on cancel.  (copilot-chat's edit-in-buffer.)
;; - `efrit-show-preview': show a -/+ or plain text preview in a
;;   pop-up window fitted to content, returning the window so the
;;   caller can delete it after a prompt.
;;
;; And a string helper: `efrit-fence-for' returns a code fence longer
;; than any backtick run in the text, so model output can be embedded
;; in a prompt without breaking out of it.

;;; Code:

(require 'subr-x)

(defvar efrit-edit-in-buffer-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'exit-recursive-edit)
    (define-key map (kbd "C-c C-k") #'abort-recursive-edit)
    map)
  "Keys active in `efrit-edit-in-buffer'.")

(defun efrit-edit-in-buffer (text description &optional mode)
  "Let the user edit TEXT in a buffer described by DESCRIPTION; return the result.
MODE, if non-nil, is a major-mode function to enable (e.g.
`emacs-lisp-mode').  Blocks in a recursive edit: \\`C-c C-c' returns
the buffer contents, \\`C-c C-k' signals `quit'.  The buffer and its
window are cleaned up either way."
  (let ((buf (generate-new-buffer (format "*efrit edit: %s*" description))))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (insert (or text ""))
            (goto-char (point-min))
            (when (and mode (fboundp mode))
              (condition-case nil (funcall mode) (error nil)))
            (use-local-map (make-composed-keymap efrit-edit-in-buffer-map
                                                 (current-local-map)))
            (setq header-line-format
                  (substitute-command-keys
                   (format "Edit %s, then \\[exit-recursive-edit] to use it, \\[abort-recursive-edit] to cancel"
                           description))))
          (pop-to-buffer buf)
          (recursive-edit)
          (with-current-buffer buf (buffer-string)))
      (when-let* ((w (get-buffer-window buf t)))
        (ignore-errors (quit-window nil w)))
      (kill-buffer buf))))

(defun efrit-show-preview (name text &optional mode)
  "Display TEXT in a preview buffer NAME in a window fitted to its size.
MODE is an optional major mode (e.g. `diff-mode').  Returns the window.
The buffer is read-only and reused across calls."
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert text)
        (goto-char (point-min)))
      (when (and mode (fboundp mode) (not (eq major-mode mode)))
        (condition-case nil (funcall mode) (error nil)))
      (setq buffer-read-only t))
    (let ((win (display-buffer buf '((display-buffer-at-bottom)
                                     (window-height . fit-window-to-buffer)))))
      (when (window-live-p win)
        (fit-window-to-buffer win (/ (frame-height) 2) 4))
      win)))

(defun efrit-fence-for (text)
  "Return a backtick fence longer than any run of backticks in TEXT.
Minimum three.  Use as both the opening and closing fence when
embedding TEXT in markdown so it cannot terminate the block early."
  (let ((longest 0) (start 0))
    (while (string-match "`+" (or text "") start)
      (setq longest (max longest (- (match-end 0) (match-beginning 0)))
            start (match-end 0)))
    (make-string (max 3 (1+ longest)) ?`)))

(provide 'efrit-ui-helpers)

;;; efrit-ui-helpers.el ends here
