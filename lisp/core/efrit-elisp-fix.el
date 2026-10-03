;;; efrit-elisp-fix.el --- Repair unbalanced Lisp the model wrote -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.8.5
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, lisp

;;; Commentary:

;; A model that writes Lisp sometimes drops a closing paren or adds
;; one too many.  Sending that to `eval_sexp' costs a failed read, a
;; tool-result round trip and a review call before the model tries
;; again.  This file fixes the common cases before the read, the way
;; copilot-balancer.el does for completions (2026-09-28):
;;
;; - trailing closers beyond what is open are dropped;
;; - missing closers are appended, in the right order, computed by
;;   `parse-partial-sexp' with the Emacs Lisp syntax table (so
;;   strings, comments and char literals are understood);
;; - an unterminated string is closed.
;;
;; `efrit-elisp-fix' returns the fixed text and what it did, or nil
;; when the text was fine.  `eval_sexp' applies it and tells the
;; model in the result ("(2 closing parens added)"), so it learns.
;; Text that does not read after the fix is left to the reader's own
;; error.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defun efrit-elisp-fix--state (text)
  "The `parse-partial-sexp' state at the end of TEXT, in emacs-lisp syntax."
  (with-temp-buffer
    (set-syntax-table emacs-lisp-mode-syntax-table)
    (insert text)
    (parse-partial-sexp (point-min) (point-max))))

(defun efrit-elisp-fix--strip-extra-closers (text)
  "TEXT without closing delimiters at its end that close nothing.
Only unescaped, outside-string closers at the very end are removed,
one at a time, while the text stays over-closed."
  (let ((s (string-trim-right text)))
    (while (and (> (length s) 0)
                (memq (aref s (1- (length s))) '(?\) ?\]))
                (not (and (>= (length s) 2) (= (aref s (- (length s) 2)) ?\\)))
                ;; over-closed: the depth at the end is negative
                (< (nth 0 (efrit-elisp-fix--state s)) 0))
      (setq s (string-trim-right (substring s 0 (1- (length s))))))
    s))

(defun efrit-elisp-fix (text)
  "Balance the Lisp in TEXT.
Returns (FIXED . NOTE) when something was changed, NOTE a short
human phrase (\"2 closing parens added\"), or nil when TEXT was
already balanced (or too broken to fix mechanically)."
  (when (and (stringp text) (not (string-empty-p (string-trim text))))
    (condition-case nil
        (let* ((stripped (efrit-elisp-fix--strip-extra-closers text))
               (removed (- (length (string-trim-right text)) (length stripped)))
               (state (efrit-elisp-fix--state stripped))
               (in-string (nth 3 state))
               (opens (nth 9 state))
               (closers (mapconcat (lambda (pos)
                                     (let ((c (with-temp-buffer
                                                (set-syntax-table emacs-lisp-mode-syntax-table)
                                                (insert stripped)
                                                (matching-paren (char-after pos)))))
                                       (if c (string c) "")))
                                   (reverse opens) ""))
               (fixed (concat stripped
                              (if (characterp in-string) (string in-string) (if in-string "\"" ""))
                              closers))
               (notes (delq nil (list (and (> removed 0)
                                           (format "%d stray closing paren%s removed"
                                                   removed (if (= removed 1) "" "s")))
                                      (and in-string "unterminated string closed")
                                      (and (> (length closers) 0)
                                           (format "%d closing paren%s added"
                                                   (length closers) (if (= (length closers) 1) "" "s")))))))
          (when notes
            ;; only offer a fix that reads
            (condition-case nil
                (progn (read-from-string fixed)
                       (cons fixed (mapconcat #'identity notes ", ")))
              (error nil))))
      (error nil))))

(provide 'efrit-elisp-fix)

;;; efrit-elisp-fix.el ends here
