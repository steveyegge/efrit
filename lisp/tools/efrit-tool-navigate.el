;;; efrit-tool-navigate.el --- Semantic navigation through xref, imenu, tree-sitter -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.8.5
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, ai

;;; Commentary:

;; The model had search, read and VCS tools but no way to ask Emacs
;; what it knows about code.  These tools run Emacs's own machinery
;; in a buffer visiting the file, so whatever backend the user runs
;; (eglot, lsp-mode, etags, the elisp backend) answers, with no hard
;; dependency on any of them (after claude-code-ide's emacs-tools,
;; 2026-09-28).
;;
;;   xref_references  where is SYMBOL used: file:line: summary
;;   xref_apropos     definitions matching a pattern
;;   imenu_symbols    the structure of a file, flattened
;;   treesit_info     the syntax node at a position, ancestors, children
;;   show_location    open a file for the user at a text anchor
;;
;; Lines are 1-based, columns 0-based, as everywhere in efrit's tools.
;; Every file is checked as a read with the sandbox.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'xref)
(require 'imenu)
(require 'project)
(require 'apropos)   ; the elisp xref backend's apropos needs apropos-parse-pattern
(require 'efrit-tool-utils)

(defmacro efrit-tool-navigate--no-prompts (&rest body)
  "Run BODY with project.el's \"Select project\" prompt turned into an error.
A backend that searches the project (the elisp one) asks for one when
the file is outside any; from a tool that would hang the turn."
  (declare (indent 0))
  `(let ((project-prompter (lambda (&rest _)
                             (signal 'user-error (list "the file is not in a project; the backend needs one to search")))))
     ,@body))

(defcustom efrit-tool-navigate-max-results 200
  "Most items a navigation tool returns; the rest is counted."
  :type 'integer
  :group 'efrit-tool-utils)

;;;; Helpers

(defun efrit-tool-navigate--buffer (file tool)
  "A buffer visiting FILE (checked as a read for TOOL); nil path means the current buffer."
  (if (or (null file) (string-empty-p file))
      (or (and (require 'efrit-context-sources nil t)
               (fboundp 'efrit-context-target-buffer)
               (efrit-context-target-buffer))
          (current-buffer))
    (let ((path (efrit-resolve-path-simple file 'read tool)))
      (unless (file-exists-p path)
        (signal 'user-error (list (format "No such file: %s" file))))
      (find-file-noselect path))))

(declare-function efrit-context-target-buffer "efrit-context-sources")

(defun efrit-tool-navigate--goto (line column)
  "Move point to LINE (1-based) and COLUMN (0-based) in the current buffer."
  (goto-char (point-min))
  (forward-line (1- (max 1 (or line 1))))
  (move-to-column (max 0 (or column 0))))

(defun efrit-tool-navigate--xref-item (item)
  "ITEM (an xref) as an alist for the result."
  (let* ((loc (xref-item-location item))
         (marker (ignore-errors (xref-location-marker loc)))
         (file (or (ignore-errors (xref-location-group loc)) ""))
         (line (or (ignore-errors (xref-location-line loc))
                   (and marker (with-current-buffer (marker-buffer marker)
                                 (line-number-at-pos marker)))))
         (summary (substring-no-properties (or (xref-item-summary item) ""))))
    `((file . ,file) (line . ,line) (summary . ,(string-trim summary))
      (text . ,(format "%s:%s: %s" file (or line "?") (string-trim summary))))))

(defun efrit-tool-navigate--cap (items)
  "ITEMS cut to the max, with the count of what was left out."
  (let ((n (length items)))
    (cons (seq-take items efrit-tool-navigate-max-results)
          (max 0 (- n efrit-tool-navigate-max-results)))))

;;;; xref_references

(defun efrit-tool-xref-references (args)
  "References to a symbol, through the file's xref backend.
ARGS: symbol (required); file (default the current buffer); line and
column to place point first, so the backend resolves the symbol in
its scope."
  (efrit-tool-execute xref_references args
    (let* ((symbol (alist-get 'symbol args))
           (file (alist-get 'file args))
           (buf (efrit-tool-navigate--buffer file "xref_references")))
      (unless (and symbol (not (string-empty-p symbol)))
        (signal 'user-error (list "symbol is required")))
      (with-current-buffer buf
        (save-excursion
          (when (alist-get 'line args)
            (efrit-tool-navigate--goto (alist-get 'line args) (alist-get 'column args)))
          (let* ((backend (xref-find-backend))
                 (refs (condition-case err
                           (if (and (eq backend 'etags) (not (bound-and-true-p tags-file-name))
                                    (not (bound-and-true-p tags-table-list)))
                               (signal 'user-error (list "the etags backend has no tags table; run visit-tags-table or use eglot/lsp"))
                             (efrit-tool-navigate--no-prompts
                               (xref-backend-references backend symbol)))
                         (user-error (signal (car err) (cdr err)))
                         (error (signal 'user-error (list (format "%s backend: %s" backend (error-message-string err)))))))
                 (capped (efrit-tool-navigate--cap (mapcar #'efrit-tool-navigate--xref-item refs))))
            (efrit-tool-success
             `((backend . ,(format "%s" backend))
               (symbol . ,symbol)
               (count . ,(length refs))
               (omitted . ,(cdr capped))
               (references . ,(vconcat (car capped)))))))))))

;;;; xref_apropos

(defun efrit-tool-xref-apropos (args)
  "Definitions whose names match a pattern, through the xref backend.
ARGS: pattern (required, the backend's syntax: words for elisp and
LSP, a regexp for etags); file (default the current buffer)."
  (efrit-tool-execute xref_apropos args
    (let* ((pattern (alist-get 'pattern args))
           (buf (efrit-tool-navigate--buffer (alist-get 'file args) "xref_apropos")))
      (unless (and pattern (not (string-empty-p pattern)))
        (signal 'user-error (list "pattern is required")))
      (with-current-buffer buf
        (let* ((backend (xref-find-backend))
               (defs (condition-case err
                         (efrit-tool-navigate--no-prompts (xref-backend-apropos backend pattern))
                       (error (signal 'user-error (list (format "%s backend: %s" backend (error-message-string err)))))))
               (capped (efrit-tool-navigate--cap (mapcar #'efrit-tool-navigate--xref-item defs))))
          (efrit-tool-success
           `((backend . ,(format "%s" backend))
             (pattern . ,pattern)
             (count . ,(length defs))
             (omitted . ,(cdr capped))
             (definitions . ,(vconcat (car capped))))))))))

;;;; imenu_symbols

(defun efrit-tool-navigate--flatten-imenu (index &optional prefix)
  "INDEX (from `imenu--make-index-alist') as a flat list of (NAME KIND POS)."
  (let (out)
    (dolist (entry index)
      (let ((name (car entry)) (val (cdr entry)))
        (cond
         ((or (equal entry imenu--rescan-item) (equal name (car-safe imenu--rescan-item))) nil)
         ((imenu--subalist-p entry)
          (setq out (nconc out (efrit-tool-navigate--flatten-imenu val name))))
         ((or (number-or-marker-p val) (overlayp val))
          (push (list name prefix (if (overlayp val) (overlay-start val) val)) out))
         ((and (consp val) (number-or-marker-p (car val)))
          (push (list name prefix (car val)) out)))))
    (nreverse out)))

(defun efrit-tool-imenu-symbols (args)
  "The definitions of a file as imenu sees them, flattened, with lines.
ARGS: file (default the current buffer)."
  (efrit-tool-execute imenu_symbols args
    (let ((buf (efrit-tool-navigate--buffer (alist-get 'file args) "imenu_symbols")))
      (with-current-buffer buf
        (let* ((index (condition-case err
                          (progn (setq imenu--index-alist nil)
                                 (imenu--make-index-alist t))
                        (error (signal 'user-error (list (format "imenu: %s" (error-message-string err)))))))
               (flat (efrit-tool-navigate--flatten-imenu index))
               (items (mapcar (lambda (e)
                                (pcase-let ((`(,name ,kind ,pos) e))
                                  (let ((line (save-excursion (goto-char pos) (line-number-at-pos))))
                                    `((name . ,(substring-no-properties name))
                                      (kind . ,(or kind "")) (line . ,line)
                                      (text . ,(format "%s%s  line %d" (if kind (concat kind ": ") "") name line))))))
                              (sort flat (lambda (a b) (< (nth 2 a) (nth 2 b))))))
               (capped (efrit-tool-navigate--cap items)))
          (efrit-tool-success
           `((file . ,(or buffer-file-name (buffer-name)))
             (mode . ,(symbol-name major-mode))
             (count . ,(length items))
             (omitted . ,(cdr capped))
             (symbols . ,(vconcat (car capped))))))))))

;;;; treesit_info

(declare-function treesit-node-at "treesit")
(declare-function treesit-node-type "treesit")
(declare-function treesit-node-start "treesit")
(declare-function treesit-node-end "treesit")
(declare-function treesit-node-parent "treesit")
(declare-function treesit-node-children "treesit")
(declare-function treesit-node-text "treesit")
(declare-function treesit-node-field-name "treesit")
(declare-function treesit-parser-list "treesit")
(declare-function treesit-buffer-root-node "treesit")
(declare-function treesit-available-p "treesit")

(defun efrit-tool-navigate--node-alist (node &optional with-text)
  (let ((start (treesit-node-start node)) (end (treesit-node-end node)))
    `((type . ,(treesit-node-type node))
      ,@(when-let* ((f (treesit-node-field-name node))) `((field . ,f)))
      (start_line . ,(line-number-at-pos start))
      (end_line . ,(line-number-at-pos end))
      ,@(when with-text
          `((text . ,(truncate-string-to-width (treesit-node-text node t) 200 nil nil "…")))))))

(defun efrit-tool-navigate--tree (node depth)
  "NODE and its named children to DEPTH as nested alists."
  (append (efrit-tool-navigate--node-alist node)
          (when (> depth 0)
            `((children . ,(vconcat (mapcar (lambda (c) (efrit-tool-navigate--tree c (1- depth)))
                                            (treesit-node-children node t))))))))

(defun efrit-tool-treesit-info (args)
  "The tree-sitter node at a position: its type, range, ancestors, children.
ARGS: file (default the current buffer); line and column (default
point); ancestors (default 3): how many parents to list; children
(default t): list the named children; whole_tree (default nil): the
whole tree to depth 20 instead."
  (efrit-tool-execute treesit_info args
    (unless (and (fboundp 'treesit-available-p) (treesit-available-p))
      (signal 'user-error (list "this Emacs has no tree-sitter")))
    (let ((buf (efrit-tool-navigate--buffer (alist-get 'file args) "treesit_info")))
      (with-current-buffer buf
        (unless (treesit-parser-list)
          (signal 'user-error (list (format "no tree-sitter parser in %s (%s); the buffer needs a *-ts-mode"
                                            (buffer-name) major-mode))))
        (if (alist-get 'whole_tree args)
            (efrit-tool-success
             `((file . ,(or buffer-file-name (buffer-name)))
               (tree . ,(efrit-tool-navigate--tree (treesit-buffer-root-node) 20))))
          (save-excursion
            (when (alist-get 'line args)
              (efrit-tool-navigate--goto (alist-get 'line args) (alist-get 'column args)))
            (let* ((node (treesit-node-at (point)))
                   (up (or (alist-get 'ancestors args) 3))
                   (ancestors (cl-loop for n = (treesit-node-parent node) then (treesit-node-parent n)
                                       for i from 0 while (and n (< i up))
                                       collect (efrit-tool-navigate--node-alist n)))
                   (children (unless (eq (alist-get 'children args) :json-false)
                               (mapcar (lambda (c) (efrit-tool-navigate--node-alist c t))
                                       (treesit-node-children node t)))))
              (efrit-tool-success
               `((file . ,(or buffer-file-name (buffer-name)))
                 (node . ,(efrit-tool-navigate--node-alist node t))
                 (ancestors . ,(vconcat ancestors))
                 (children . ,(vconcat children)))))))))))

;;;; show_location

(defun efrit-tool-show-location (args)
  "Open a file for the user and highlight a place in it.
ARGS: file (required); start_text and end_text (preferred: the range
is found by text, so it survives line drift); else line and end_line.
The window is shown without taking focus from the agent buffer."
  (efrit-tool-execute show_location args
    (let* ((file (alist-get 'file args))
           (buf (efrit-tool-navigate--buffer file "show_location"))
           (start-text (alist-get 'start_text args))
           (end-text (alist-get 'end_text args))
           (line (alist-get 'line args))
           (end-line (alist-get 'end_line args))
           start end how)
      (unless (and file (not (string-empty-p file)))
        (signal 'user-error (list "file is required")))
      (with-current-buffer buf
        (save-excursion
          (goto-char (point-min))
          (cond
           ((and start-text (search-forward start-text nil t))
            (setq start (match-beginning 0) how "text")
            (setq end (if (and end-text (search-forward end-text nil t)) (match-end 0) (match-end 0))))
           (line
            (efrit-tool-navigate--goto line 0)
            (setq start (point) how "line")
            (when end-line (efrit-tool-navigate--goto end-line 0) (end-of-line))
            (setq end (if end-line (point) (line-end-position))))
           (t (setq start (point-min) end (point-min) how "top")))))
      (let ((win (display-buffer buf '((display-buffer-reuse-window display-buffer-pop-up-window)
                                       (inhibit-same-window . t)))))
        (when (window-live-p win)
          (set-window-point win start)
          (with-selected-window win
            (goto-char start)
            (recenter)
            (when (and end (> end start))
              (let ((ov (make-overlay start end)))
                (overlay-put ov 'face 'highlight)
                (run-at-time 3 nil (lambda () (delete-overlay ov)))))))
        (efrit-tool-success
         `((file . ,(buffer-file-name buf))
           (found_by . ,how)
           (line . ,(with-current-buffer buf (line-number-at-pos start)))
           (shown . ,(if (window-live-p win) t :json-false))))))))

(provide 'efrit-tool-navigate)

;;; efrit-tool-navigate.el ends here
