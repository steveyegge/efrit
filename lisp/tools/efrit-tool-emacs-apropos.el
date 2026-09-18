;;; efrit-tool-emacs-apropos.el --- Discover Emacs commands and variables -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; The emacs_apropos tool.  `elisp_docs' answers "what does X do" when
;; the model already knows X.  This tool answers "what is there for
;; this job": it takes plain words ("recent files", "open remote
;; directory", "revert buffer") and returns the matching commands,
;; functions and variables that exist in THIS Emacs, ranked the way
;; `apropos' ranks them, with the first line of each docstring and the
;; feature the symbol comes from.  Package-provided symbols (magit,
;; consult, ...) show up when they are loaded or autoloaded.
;;
;; It also reports the live value of matching variables when they are
;; small (recentf-list, dired-listing-switches, vc-handled-backends),
;; because the value is usually what the model needs next.
;;
;; No buffers are shown and nothing is evaluated: this is `apropos'
;; internals plus `symbol-value', so it is a read-only tool that needs
;; no sandbox grant.

;;; Code:

(require 'cl-lib)
(require 'apropos)
(require 'help-fns)
(require 'efrit-tool-utils)

(defcustom efrit-tool-emacs-apropos-max-results 25
  "Most symbols returned per call."
  :type 'integer
  :group 'efrit-tool-utils)

(defcustom efrit-tool-emacs-apropos-max-value-length 400
  "Variable values longer than this many characters are truncated."
  :type 'integer
  :group 'efrit-tool-utils)

(defconst efrit-tool-emacs-apropos-kinds '("command" "function" "variable" "all")
  "Accepted values of the KIND input.")

;;; Matching

(defun efrit-tool-emacs-apropos--pattern (query)
  "Turn QUERY into the word list `apropos-parse-pattern' wants.
Several words match symbols containing at least two of them (or one
word when only one is given); a query with regexp characters is used
as a regexp."
  (if (string-match-p "[][*+?^$\\\\|]" query)
      query
    (let ((words (split-string query "[ \t,_-]+" t)))
      (if (= (length words) 1) (car words) words))))

(defun efrit-tool-emacs-apropos--kind-p (symbol kind)
  "Non-nil if SYMBOL is of KIND (\"command\" \"function\" \"variable\" \"all\")."
  (pcase kind
    ("command" (commandp symbol))
    ("function" (fboundp symbol))
    ("variable" (and (boundp symbol) (not (keywordp symbol))))
    (_ (or (fboundp symbol) (and (boundp symbol) (not (keywordp symbol)))))))

(defun efrit-tool-emacs-apropos--internal-p (symbol)
  "Non-nil for symbols with `--' in their name: private, skip by default."
  (string-match-p "--" (symbol-name symbol)))

(defun efrit-tool-emacs-apropos--matches (query kind include-internal)
  "Symbols matching QUERY of KIND, scored, best first."
  (apropos-parse-pattern (efrit-tool-emacs-apropos--pattern query))
  (let ((scored nil))
    (mapatoms
     (lambda (sym)
       (when (and (string-match-p apropos-regexp (symbol-name sym))
                  (efrit-tool-emacs-apropos--kind-p sym kind)
                  (or include-internal
                      (not (efrit-tool-emacs-apropos--internal-p sym))))
         (let ((score (apropos-score-symbol sym))
               (doc (efrit-tool-emacs-apropos--doc-first-line sym)))
           ;; A word from the query in the first doc line counts too:
           ;; that is how "recent files" finds `recentf-open-files'.
           (when (and doc apropos-all-words-regexp
                      (let ((case-fold-search t))
                        (string-match-p apropos-all-words-regexp doc)))
             (setq score (+ score 40)))
           ;; Interactive commands are what the user would reach for
           (when (commandp sym) (setq score (+ score 20)))
           (push (cons score sym) scored)))))
    ;; Doc-only matches: symbols whose name misses but whose first doc
    ;; line has every word.  Cheap enough to scan once for commands.
    (when (and (listp apropos-words) (> (length apropos-words) 1)
               (member kind '("command" "all")))
      (let ((seen (make-hash-table :test #'eq)))
        (dolist (c scored) (puthash (cdr c) t seen))
        (mapatoms
         (lambda (sym)
           (when (and (commandp sym)
                      (not (gethash sym seen))
                      (not (efrit-tool-emacs-apropos--internal-p sym)))
           (let ((doc (efrit-tool-emacs-apropos--doc-first-line sym)))
             (when (and doc
                        (let ((case-fold-search t))
                          (cl-every (lambda (w) (string-match-p (regexp-quote w) doc))
                                    apropos-words)))
               (push (cons 30 sym) scored))))))))
    (mapcar #'cdr (sort scored (lambda (a b) (> (car a) (car b)))))))

;;; Describing

(defun efrit-tool-emacs-apropos--doc-first-line (symbol &optional prefer-variable)
  "First line of SYMBOL's function docstring (variable's with PREFER-VARIABLE), or nil."
  (let ((doc (condition-case nil
                 (let ((fdoc (and (fboundp symbol) (documentation symbol t)))
                       (vdoc (and (boundp symbol)
                                  (documentation-property symbol 'variable-documentation t))))
                   (if prefer-variable (or vdoc fdoc) (or fdoc vdoc)))
               (error nil))))
    (when (and (stringp doc) (not (string-empty-p doc)))
      (car (split-string doc "\n")))))

(defun efrit-tool-emacs-apropos--feature (symbol)
  "The library SYMBOL was loaded from, or would be autoloaded from."
  (let ((f (symbol-function symbol)))
    (cond
     ((and f (autoloadp f) (stringp (cadr f))) (file-name-nondirectory (cadr f)))
     (t (when-let* ((file (symbol-file symbol)))
          (file-name-sans-extension (file-name-nondirectory file)))))))

(defun efrit-tool-emacs-apropos--value (symbol)
  "SYMBOL's current value as a short string, or nil if unbound or huge."
  (when (boundp symbol)
    (let ((s (condition-case nil (prin1-to-string (symbol-value symbol)) (error nil))))
      (when s
        (if (> (length s) efrit-tool-emacs-apropos-max-value-length)
            (concat (substring s 0 efrit-tool-emacs-apropos-max-value-length) "…")
          s)))))

(defun efrit-tool-emacs-apropos--describe (symbol kind include-values)
  "The alist for one SYMBOL found under the requested KIND.
A symbol can be both a command and a variable (recentf-edit-list); the
entry describes the facet that was asked for."
  (let* ((as-variable (or (equal kind "variable") (not (fboundp symbol))))
         (entry `((symbol . ,(symbol-name symbol))
                  (kind . ,(cond (as-variable "variable")
                                 ((commandp symbol) "command")
                                 ((macrop symbol) "macro")
                                 (t "function"))))))
    (when (and (fboundp symbol) (not as-variable))
      (when-let* ((args (condition-case nil (help-function-arglist symbol t) (error t))))
        (unless (eq args t)
          (setq entry (append entry `((signature . ,(format "%S" args))))))))
    (when-let* ((doc (efrit-tool-emacs-apropos--doc-first-line symbol as-variable)))
      (setq entry (append entry `((doc . ,doc)))))
    (when-let* ((feature (efrit-tool-emacs-apropos--feature symbol)))
      (setq entry (append entry `((from . ,feature)))))
    (when (and (fboundp symbol) (not as-variable) (autoloadp (symbol-function symbol)))
      (setq entry (append entry '((autoload . t)))))
    (when (and include-values as-variable (boundp symbol))
      (when-let* ((v (efrit-tool-emacs-apropos--value symbol)))
        (setq entry (append entry `((value . ,v))))))
    entry))

;;; Entry point

(defun efrit-tool-emacs-apropos (args)
  "Find Emacs commands, functions and variables for a job.

ARGS is an alist with:
  query            - words describing the job, or a regexp (required)
  kind             - command | function | variable | all (default command)
  include_values   - report current values of matching variables (default t)
  include_internal - include symbols with -- in the name (default nil)
  max              - cap on results (default `efrit-tool-emacs-apropos-max-results')"
  (efrit-tool-execute emacs_apropos args
    (let* ((query (alist-get 'query args))
           (kind (or (alist-get 'kind args) "command"))
           (include-values (let ((v (alist-get 'include_values args 'unset)))
                             (if (eq v 'unset) t (and v (not (eq v :json-false))))))
           (include-internal (let ((v (alist-get 'include_internal args)))
                               (and v (not (eq v :json-false)))))
           (max (or (alist-get 'max args) efrit-tool-emacs-apropos-max-results))
           (warnings nil))
      (unless (and (stringp query) (not (string-empty-p (string-trim query))))
        (signal 'user-error (list "query is required")))
      (unless (member kind efrit-tool-emacs-apropos-kinds)
        (signal 'user-error
                (list (format "kind must be one of %s" efrit-tool-emacs-apropos-kinds))))
      (let* ((all (efrit-tool-emacs-apropos--matches query kind include-internal))
             (shown (seq-take all max)))
        (when (> (length all) (length shown))
          (push (format "%d matches; showing the %d best. Narrow the query or raise max."
                        (length all) (length shown))
                warnings))
        (when (null all)
          (push "No match. Try fewer or different words, or kind=\"all\"." warnings))
        (efrit-tool-success
         `((query . ,query)
           (kind . ,kind)
           (results . ,(vconcat (mapcar (lambda (s) (efrit-tool-emacs-apropos--describe s kind include-values))
                                        shown)))
           (total . ,(length all)))
         warnings)))))

(provide 'efrit-tool-emacs-apropos)

;;; efrit-tool-emacs-apropos.el ends here
