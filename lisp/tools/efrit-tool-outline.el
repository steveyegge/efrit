;;; efrit-tool-outline.el --- Whole-file reads with a budget; the project's outline -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.12.33
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, ai

;;; Commentary:

;; The model read one 1100-line file in five 60-to-260-line windows
;; and a 1600-line one in four, thirteen round trips for what two
;; whole-file reads would have given (tzz, 2026-10-20: "it would be
;; much more efficient to read the .el files in the project all at
;; once").  All of them at once is the wrong unit: this tree is some
;; 60k lines, far past the context window, and the context is paid
;; again on every iteration of the turn.  The right unit is "the
;; files this task touches, whole, in one call".
;;
;; Two tools here.
;;
;; `read_file' with PATHS: several files, whole, in one result, under
;; a token budget.  A file that would not fit comes back as its
;; OUTLINE instead (imenu entries with line numbers), so the model can
;; range-read the part it wants.
;;
;; `project_outline': every top-level definition in the project's
;; source files, by file, with line numbers.  A few thousand tokens
;; for a tree this size; cached by file modification time, so the
;; second call in a session is free.  "Where is tool-result
;; rendering?" becomes one call that names the five functions and
;; their files.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'imenu)
(require 'efrit-tool-utils)
(require 'efrit-tool-read-file)
(require 'efrit-tool-navigate)

(declare-function efrit-tool-project-files--project-files "efrit-tool-project-files")
(declare-function efrit-tool-project-files--dir-recursive "efrit-tool-project-files")

(defgroup efrit-tool-outline nil
  "Whole-file reads and the project outline."
  :group 'efrit-tool-utils)

(defcustom efrit-tool-read-file-batch-budget 40000
  "Tokens one multi-file `read_file' may return, all files together.
Roughly 3.5 characters per token.  A file over what is left of the
budget comes back as its outline; the model reads a range of it next."
  :type 'integer
  :group 'efrit-tool-outline)

(defcustom efrit-tool-outline-max-files 400
  "Most files `project_outline' walks; beyond that the result says so."
  :type 'integer
  :group 'efrit-tool-outline)

(defcustom efrit-tool-outline-file-regexp
  "\\.\\(?:el\\|py\\|js\\|ts\\|tsx\\|jsx\\|go\\|rs\\|c\\|h\\|cc\\|cpp\\|hpp\\|java\\|rb\\|sh\\|lua\\|org\\|md\\)\\'"
  "Files `project_outline' includes: sources imenu knows how to index."
  :type 'regexp
  :group 'efrit-tool-outline)

(defconst efrit-tool-outline--chars-per-token 3.5)

;;;; Outline of one file

(defvar efrit-tool-outline--cache (make-hash-table :test 'equal)
  "Canonical path -> (MTIME . OUTLINE), OUTLINE a list of (NAME KIND LINE).")

(defun efrit-tool-outline--mode-for (path)
  "The major mode `auto-mode-alist' gives PATH, or nil."
  (let ((mode (assoc-default path auto-mode-alist #'string-match)))
    (and (symbolp mode) (fboundp mode) mode)))

(defun efrit-tool-outline-of-file (path)
  "The outline of PATH: a list of (NAME KIND LINE), by imenu, cached by mtime.
A live buffer visiting PATH is used as is; otherwise a temporary
buffer in the file's mode, so no buffer is left behind for every
file of the project."
  (let* ((path (expand-file-name path))
         (attrs (file-attributes path))
         (mtime (and attrs (file-attribute-modification-time attrs)))
         (cached (gethash path efrit-tool-outline--cache)))
    (if (and cached (equal (car cached) mtime))
        (cdr cached)
      (let ((outline
             (condition-case nil
                 (let ((buf (find-buffer-visiting path)))
                   (if buf
                       (with-current-buffer buf (efrit-tool-outline--imenu-here))
                     (with-temp-buffer
                       (insert-file-contents path)
                       (setq buffer-file-name path)
                       (delay-mode-hooks
                         (let ((mode (efrit-tool-outline--mode-for path)))
                           (when mode (funcall mode))))
                       (prog1 (efrit-tool-outline--imenu-here)
                         (setq buffer-file-name nil)
                         (set-buffer-modified-p nil)))))
               (error nil))))
        (puthash path (cons mtime outline) efrit-tool-outline--cache)
        outline))))

(defun efrit-tool-outline--imenu-here ()
  "The current buffer's imenu entries as (NAME KIND LINE), in file order."
  (let* ((imenu-auto-rescan t)
         (index (progn (setq imenu--index-alist nil) (imenu--make-index-alist t)))
         (flat (efrit-tool-navigate--flatten-imenu index)))
    (mapcar (lambda (e)
              (pcase-let ((`(,name ,kind ,pos) e))
                (list (substring-no-properties name) (or kind "")
                      (save-excursion (goto-char pos) (line-number-at-pos)))))
            (sort flat (lambda (a b) (< (nth 2 a) (nth 2 b)))))))

(defun efrit-tool-outline--text (outline)
  "OUTLINE as lines the model reads: `  123 kind: name'."
  (mapconcat (lambda (e)
               (pcase-let ((`(,name ,kind ,line) e))
                 (format "%5d %s%s" line (if (string-empty-p kind) "" (concat kind ": ")) name)))
             outline "\n"))

;;;; read_file with several paths

(defun efrit-tool-read-files (args)
  "Read the files in ARGS's `paths' whole, in one result, under a budget.
ARGS: paths (a list or vector of paths), budget (tokens, optional).
Each entry of the result is the file's content, or its outline when
the content would not fit in what remains of the budget."
  (efrit-tool-execute read_file args
    (let* ((paths (let ((p (alist-get 'paths args))) (if (vectorp p) (append p nil) p)))
           (budget (or (alist-get 'budget args) efrit-tool-read-file-batch-budget))
           (remaining (* budget efrit-tool-outline--chars-per-token))
           (files nil) (warnings nil))
      (unless (and paths (cl-every #'stringp paths))
        (signal 'user-error (list "paths must be a list of file paths")))
      (dolist (p paths)
        (condition-case err
            (let* ((info (efrit-resolve-path p 'read "read_file"))
                   (path (plist-get info :path))
                   (rel (plist-get info :path-relative)))
              (cond
               ((not (file-exists-p path))
                (push `((path . ,p) (error . "not found")) files))
               ((file-directory-p path)
                (push `((path . ,p) (error . "a directory")) files))
               ((efrit-tool-binary-file-p path 8192)
                (push `((path . ,path) (path_relative . ,rel) (is_binary . t)) files))
               (t
                (let* ((size (file-attribute-size (file-attributes path)))
                       (buf (find-buffer-visiting path))
                       (content (if buf
                                    (with-current-buffer buf
                                      (buffer-substring-no-properties (point-min) (point-max)))
                                  (with-temp-buffer (insert-file-contents path) (buffer-string))))
                       (len (length content)))
                  (if (<= len remaining)
                      (progn
                        (cl-decf remaining len)
                        (push `((path . ,path) (path_relative . ,rel) (size . ,size)
                                (total_lines . ,(with-temp-buffer (insert content) (count-lines (point-min) (point-max))))
                                (content . ,content)
                                (from_live_buffer . ,(if buf t :json-false)))
                              files))
                    ;; over budget: the outline, and a line count, so
                    ;; the model picks a range next
                    (let ((outline (efrit-tool-outline-of-file path)))
                      (push `((path . ,path) (path_relative . ,rel) (size . ,size)
                              (total_lines . ,(with-temp-buffer (insert content) (count-lines (point-min) (point-max))))
                              (outline . ,(efrit-tool-outline--text outline))
                              (note . "over the budget for this call: outline only; read_file with start_line/end_line for a part, or this path alone"))
                            files)
                      (push (format "%s (%d KB) did not fit; its outline is given instead" (or rel path) (/ size 1000))
                            warnings)))))))
          (efrit-sandbox-denied (signal (car err) (cdr err)))
          (error (push `((path . ,p) (error . ,(error-message-string err))) files))))
      (efrit-tool-success
       `((files . ,(vconcat (nreverse files)))
         (budget_tokens . ,budget)
         (budget_left_tokens . ,(floor (/ remaining efrit-tool-outline--chars-per-token))))
       (nreverse warnings)))))

;;;; project_outline

(defun efrit-tool-outline--project-files (root pattern)
  "The project's source files under ROOT (relative), filtered by PATTERN."
  (require 'efrit-tool-project-files)
  (let ((all (or (ignore-errors (efrit-tool-project-files--project-files root pattern nil))
                 (ignore-errors (efrit-tool-project-files--dir-recursive root pattern 8 nil)))))
    (cl-remove-if-not (lambda (f) (string-match-p efrit-tool-outline-file-regexp f))
                      ;; the project.el path gives relative names, the
                      ;; directory walk absolute ones: one shape
                      (mapcar (lambda (f) (if (file-name-absolute-p f) (file-relative-name f root) f)) all))))

(defun efrit-tool-project-outline (args)
  "Every top-level definition in the project's source files, by file.
ARGS: path (a directory, default the project root), pattern (a glob
to keep only some files), max_files.  Cached per file by mtime."
  (efrit-tool-execute project_outline args
    (let* ((root-input (alist-get 'path args))
           (root (file-name-as-directory
                  (if (and root-input (not (string-empty-p root-input)))
                      (efrit-resolve-path-simple root-input 'read "project_outline")
                    (efrit-tool--get-project-root))))
           (pattern (alist-get 'pattern args))
           (max-files (or (alist-get 'max_files args) efrit-tool-outline-max-files))
           (files (efrit-tool-outline--project-files root pattern))
           (shown (seq-take files max-files))
           (entries nil) (definitions 0))
      (dolist (rel shown)
        (let* ((path (expand-file-name rel root))
               (outline (efrit-tool-outline-of-file path)))
          (cl-incf definitions (length outline))
          (push `((file . ,rel) (count . ,(length outline))
                  (outline . ,(efrit-tool-outline--text outline)))
                entries)))
      (efrit-tool-success
       `((root . ,(abbreviate-file-name root))
         (files . ,(length shown))
         (definitions . ,definitions)
         (entries . ,(vconcat (nreverse entries))))
       (and (> (length files) max-files)
            (list (format "%d files not shown (max_files %d); pass pattern to narrow"
                          (- (length files) max-files) max-files)))))))

(provide 'efrit-tool-outline)

;;; efrit-tool-outline.el ends here
