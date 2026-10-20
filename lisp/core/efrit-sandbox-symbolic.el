;;; efrit-sandbox-symbolic.el --- What a path means, not only where it is -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.12.19
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, ai

;;; Commentary:

;; The sandbox needs the literal path; the reviewer needs its meaning.
;; `~/work/2026-10-16/claude.org' is, to the user, "today's work log",
;; and their CLAUDE.md says so: `~/work/YYYY-MM-DD/claude.org'.  Until
;; 0.12.19 the reviewer saw the literal, the grant history was keyed
;; on the literal directory, and every new day asked again (tzz,
;; 2026-10-16: "communicating symbolically instead of literally is
;; where a lot of the value of the review model comes from").
;;
;; Two things live here.
;;
;; TEMPLATES from the user's instruction files.  Any path-like token
;; in those files with a placeholder in it -- YYYY, MM, DD, %F, %Y,
;; <date>, {date}, * -- is a template; a literal path that matches one
;; is "named in your instructions".  That is the strongest fact the
;; sandbox can have about a path: the user asked for it in writing
;; before the session began.  Such a request is expected (no prompt),
;; and the reviewer sees the fact.  The templates come from the
;; user's files only; the model cannot add one.
;;
;; SYMBOLIC RENDERING of any path.  Components that are generated --
;; dates, times, ISO weeks, long hex or numeric ids, efrit session
;; ids -- are shown as placeholders: `~/work/<date>/claude.org'.  The
;; reviewer reads the pattern; the grant history is keyed on the
;; pattern, so answers accumulate across days.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function efrit-instructions-locate "efrit-instructions")
(declare-function efrit-instructions--read-file "efrit-instructions")
(declare-function efrit-sandbox-canonical "efrit-sandbox")
(declare-function efrit-sandbox-abbreviate "efrit-sandbox")
(declare-function efrit-tool--get-project-root "efrit-tool-utils")

(defgroup efrit-sandbox-symbolic nil
  "Paths as patterns: instruction templates and generated components."
  :group 'efrit-sandbox)

;;;; Placeholders

(defconst efrit-sandbox-symbolic-placeholders
  '(("<session>" "repl-[0-9]\\{8\\}-[0-9]\\{6\\}-[0-9]\\{4\\}" "<session-id>" "{session}")
    ("<id>" "\\(?:[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{12\\}\\|[0-9a-f]*[a-f][0-9a-f]*[0-9][0-9a-f]\\{5,\\}\\|[0-9a-f]*[0-9][0-9a-f]*[a-f][0-9a-f]\\{5,\\}\\|[0-9]\\{9,\\}\\)"
     "{id}" "<hash>" "<pid>" "<uuid>")
    ("<date>" "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" "YYYY-MM-DD" "%F" "%Y-%m-%d" "{date}" "<today>" "$(date +%F)")
    ("<date>" "[0-9]\\{8\\}" "YYYYMMDD" "%Y%m%d")
    ("<week>" "[0-9]\\{4\\}-W[0-9]\\{2\\}" "YYYY-Www" "%G-W%V")
    ("<month>" "[0-9]\\{4\\}-[0-9]\\{2\\}" "YYYY-MM" "%Y-%m")
    ("<time>" "[0-9]\\{2\\}:?[0-9]\\{2\\}\\(?::?[0-9]\\{2\\}\\)?" "HHMMSS" "HH:MM:SS" "%H%M%S" "%T" "HHMM" "HH:MM" "%H%M" "%H:%M")
    ("<year>" "[0-9]\\{4\\}" "YYYY" "%Y")
    ("<mm>" "[0-9]\\{2\\}" "MM" "%m")
    ("<dd>" "[0-9]\\{2\\}" "DD" "%d")
    ("*" "[^/]*" "*"))
  "(PLACEHOLDER REGEXP SPELLING...): a slot, what fills it, how templates spell it.
PLACEHOLDER is the canonical spelling shown to the reviewer; REGEXP
matches one component's worth of literal text; the SPELLINGs are what
instruction files write.  Order matters twice: the first entry whose
regexp matches a whole component names it (a session id before the
date inside it), and when rendering partly generated components the
entries are tried in this order (a date before the year inside it).")

(defun efrit-sandbox-symbolic--entry-regexp (entry) (nth 1 entry))
(defun efrit-sandbox-symbolic--entry-spellings (entry) (nthcdr 2 entry))

(defun efrit-sandbox-symbolic--component-placeholder (component)
  "The placeholder for a generated path COMPONENT, or nil for a plain name."
  (cl-some (lambda (entry)
             (let ((re (concat "\\`" (efrit-sandbox-symbolic--entry-regexp entry) "\\'")))
               (and (not (string= (car entry) "*"))
                    (string-match-p re component)
                    (car entry))))
           efrit-sandbox-symbolic-placeholders))

(defun efrit-sandbox-symbolic-path (path)
  "PATH with its generated components shown as placeholders.
\"~/work/2026-10-16/claude.org\" becomes \"~/work/<date>/claude.org\";
a component that is partly generated (\"log-2026-10-16.txt\") keeps
its fixed part: \"log-<date>.txt\".  A path with nothing generated
comes back unchanged.  The home directory is abbreviated."
  (let* ((abbrev (abbreviate-file-name path))
         (dir-p (directory-name-p abbrev))
         (parts (split-string abbrev "/"))
         (out (mapcar #'efrit-sandbox-symbolic--component parts)))
    (let ((s (string-join out "/")))
      (if (and dir-p (not (string-suffix-p "/" s))) (concat s "/") s))))

(defun efrit-sandbox-symbolic--component (component)
  "COMPONENT with each generated run replaced by its placeholder."
  (or (and (not (string-empty-p component))
           (efrit-sandbox-symbolic--component-placeholder component))
      ;; partly generated: replace inner runs, longest patterns first
      (let ((s component))
        (dolist (entry efrit-sandbox-symbolic-placeholders)
          (unless (string= (car entry) "*")
            (let ((re (concat "\\(?:\\`\\|[-_.]\\)\\(" (efrit-sandbox-symbolic--entry-regexp entry) "\\)\\(?:[-_.]\\|\\'\\)")))
              (while (string-match re s)
                (setq s (concat (substring s 0 (match-beginning 1))
                                (car entry)
                                (substring s (match-end 1))))))))
        s)))

;;;; Templates from the instruction files

(defvar efrit-sandbox-symbolic--templates-cache nil
  "(KEY . TEMPLATES): the templates of the instruction files last read.
KEY is the list of (PATH . MTIME) the templates came from.")

(defconst efrit-sandbox-symbolic--path-token
  "\\(?:~/\\|\\$HOME/\\|/\\)[^][:space:]\"'`(),;<>|]*[^][:space:]\"'`(),;<>|.:]"
  "A path-like token in prose: starts at ~/, $HOME/ or /, runs to a delimiter.")

(defun efrit-sandbox-symbolic--placeholder-regexp (spelling)
  "The regexp for one placeholder SPELLING, or nil when it is not one."
  (cl-some (lambda (entry) (and (member spelling (efrit-sandbox-symbolic--entry-spellings entry))
                                (efrit-sandbox-symbolic--entry-regexp entry)))
           efrit-sandbox-symbolic-placeholders))

(defun efrit-sandbox-symbolic--spellings ()
  "Every placeholder spelling, longest first, so `YYYY-MM-DD' wins over `YYYY'."
  (sort (cl-remove-duplicates
         (cl-loop for entry in efrit-sandbox-symbolic-placeholders
                  append (efrit-sandbox-symbolic--entry-spellings entry))
         :test #'equal)
        (lambda (a b) (> (length a) (length b)))))

(defun efrit-sandbox-symbolic-template-regexp (template)
  "The regexp a literal canonical path must match to instantiate TEMPLATE, or nil.
Nil when TEMPLATE has no placeholder: a literal path in the
instructions is a mention, handled elsewhere.  The regexp matches the
template itself and anything under it when the template is a directory."
  (let ((spellings (efrit-sandbox-symbolic--spellings))
        (has-slot nil)
        (pos 0) (out "\\`")
        (text (expand-file-name (replace-regexp-in-string "\\`\\$HOME/" "~/" template))))
    (while (< pos (length text))
      (let ((hit (cl-some (lambda (sp)
                            (and (string-prefix-p sp (substring text pos))
                                 (not (and (string= sp "*") (string-prefix-p "**" (substring text pos))))
                                 sp))
                          spellings)))
        (cond
         ((and (string-prefix-p "**" (substring text pos)))
          (setq has-slot t out (concat out ".*") pos (+ pos 2)))
         (hit
          (setq has-slot t
                out (concat out (efrit-sandbox-symbolic--placeholder-regexp hit))
                pos (+ pos (length hit))))
         (t (setq out (concat out (regexp-quote (substring text pos (1+ pos))))
                  pos (1+ pos))))))
    (and has-slot
         (concat out (if (directory-name-p text) ".*" "\\(?:/.*\\)?") "\\'"))))

(defun efrit-sandbox-symbolic--tokens (text)
  "Path-like tokens in TEXT that carry a placeholder."
  (let ((out nil) (start 0))
    (while (string-match efrit-sandbox-symbolic--path-token text start)
      (let ((tok (match-string 0 text)))
        (when (efrit-sandbox-symbolic-template-regexp tok)
          (cl-pushnew tok out :test #'equal)))
      (setq start (match-end 0)))
    (nreverse out)))

(defun efrit-sandbox-symbolic-templates (&optional root)
  "The path templates the instruction files for ROOT name: ((TEMPLATE REGEXP FILE)...).
Read once per set of files and modification times."
  (when (require 'efrit-instructions nil t)
    (let* ((located (ignore-errors (efrit-instructions-locate root)))
           (key (mapcar (lambda (e) (cons (car e) (file-attribute-modification-time
                                                   (file-attributes (car e)))))
                        located)))
      (if (equal key (car efrit-sandbox-symbolic--templates-cache))
          (cdr efrit-sandbox-symbolic--templates-cache)
        (let ((templates nil))
          (dolist (e located)
            (when-let* ((body (ignore-errors (efrit-instructions--read-file (car e)))))
              (dolist (tok (efrit-sandbox-symbolic--tokens body))
                (push (list tok (efrit-sandbox-symbolic-template-regexp tok) (car e)) templates))))
          (setq templates (nreverse templates))
          (setq efrit-sandbox-symbolic--templates-cache (cons key templates))
          templates)))))

(defun efrit-sandbox-symbolic-instructed (path &optional root)
  "The (TEMPLATE . FILE) from the instructions that PATH instantiates, or nil."
  (when (and (stringp path) (not (file-remote-p path)))
    ;; the template expands without resolving links; the sandbox hands
    ;; over canonical (link-resolved) paths.  Try both forms.
    (let ((forms (delete-dups
                  (list (expand-file-name path)
                        (and (fboundp 'efrit-sandbox-canonical) (efrit-sandbox-canonical path))
                        (file-truename (expand-file-name path))))))
      (cl-some (lambda (entry)
                 (and (cl-some (lambda (f) (and f (string-match-p (nth 1 entry) f))) forms)
                      (cons (nth 0 entry) (nth 2 entry))))
               (efrit-sandbox-symbolic-templates root)))))

(defun efrit-sandbox-symbolic-forget ()
  "Drop the template cache (tests, or after editing an instruction file in place)."
  (setq efrit-sandbox-symbolic--templates-cache nil))

(provide 'efrit-sandbox-symbolic)

;;; efrit-sandbox-symbolic.el ends here
