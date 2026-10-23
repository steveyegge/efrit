;;; efrit-tool-introspect.el --- One-call answers about the live Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.13.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, ai

;;; Commentary:

;; Four read-only tools for questions the model used to answer with a
;; chain of search_content, read_file ranges and reviewed eval_sexp
;; probes (2026-10-20, the spinner session: eleven calls and four
;; reviews to learn where one mode-line construct came from).  Each
;; is one call, classed `read', so no reviewer runs; the shapes are
;; fixed and the sandbox's file and buffer checks still apply.
;;
;; `symbol_info'      everything about one symbol: kind, signature,
;;                    docstring, file and line, the definition's source
;;                    text, the current value, advice, who calls it.
;; `live_state'       the current values of named variables and the
;;                    bound/fbound state of names, in a chosen buffer.
;;                    No forms are evaluated: names only.
;; `mode_line_report' every window's mode line, header line and the
;;                    `global-mode-string', rendered to text and taken
;;                    apart construct by construct, so "which spinner
;;                    is that" is answered by name.
;; `buffer_scan'      regexp matches with context over one buffer or
;;                    every buffer whose name matches, under the
;;                    sandbox's buffer check.  Replaces hand-rolled
;;                    `re-search-backward' loops in eval_sexp.
;; `buffer_list'      the live buffers with name, file, size, mode,
;;                    modified, shown, and whether the file is REMOTE.
;;                    Nothing is entered.  The user's rule for remote
;;                    buffers (2026-10-20): look at local buffers
;;                    first, then at the NAMES of remote ones, and only
;;                    when one is very likely useful ask to use it.  A
;;                    walk that `set-buffer's into seven TRAMP buffers
;;                    to search them asked for a grant on each.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'help-fns)
(require 'find-func)
(require 'nadvice)
(require 'efrit-tool-utils)
(require 'efrit-sandbox)

(declare-function efrit-tool-xref-references "efrit-tool-navigate")
(declare-function efrit-context-target-buffer "efrit-context-sources")

(defgroup efrit-tool-introspect nil
  "One-call answers about the live Emacs."
  :group 'efrit-tool-utils)

(defcustom efrit-tool-introspect-value-max-chars 2000
  "Longest printed value `symbol_info' and `live_state' return before cutting."
  :type 'integer
  :group 'efrit-tool-introspect)

(defcustom efrit-tool-introspect-source-max-chars 12000
  "Longest definition text `symbol_info' returns before cutting."
  :type 'integer
  :group 'efrit-tool-introspect)

(defcustom efrit-tool-buffer-scan-max-matches 200
  "Most matches one `buffer_scan' returns, over all buffers scanned."
  :type 'integer
  :group 'efrit-tool-introspect)

(defcustom efrit-tool-buffer-scan-max-buffers 50
  "Most buffers one `buffer_scan' walks when `buffer' is a pattern."
  :type 'integer
  :group 'efrit-tool-introspect)

;;;; Shared helpers

(defun efrit-tool-introspect--print (value max)
  "VALUE printed, cut to MAX characters with a note."
  (let ((text (condition-case err
                  (let ((print-length 200) (print-level 8))
                    (prin1-to-string value))
                (error (format "#<unprintable: %s>" (error-message-string err))))))
    (if (<= (length text) max)
        text
      (format "%s … (+%d chars)" (substring text 0 max) (- (length text) max)))))

(defun efrit-tool-introspect--buffer (name tool)
  "The buffer NAME names (checked for TOOL), the target buffer when nil."
  (let ((buf (cond
              ((or (null name) (string-empty-p name))
               (or (and (require 'efrit-context-sources nil t)
                        (fboundp 'efrit-context-target-buffer)
                        (efrit-context-target-buffer))
                   (current-buffer)))
              ((get-buffer name))
              (t (signal 'user-error (list (format "No buffer named %s" name)))))))
    (efrit-sandbox-check-buffer buf tool)
    buf))

(defun efrit-tool-introspect--file-line (symbol kind)
  "(FILE . LINE) of SYMBOL's definition of KIND, or nil."
  (let* ((type (pcase kind ('variable 'defvar) ('face 'defface) (_ nil)))
         (file (ignore-errors (symbol-file symbol (pcase kind ('function 'defun) ('variable 'defvar) ('face 'defface) (_ nil)))))
         (source (and file (ignore-errors (find-library-name (file-name-sans-extension (file-name-nondirectory file)))))))
    (when-let* ((path (or source file)))
      (condition-case nil
          (let ((location (save-excursion
                            (find-function-search-for-symbol symbol type path))))
            (when (and (car location) (cdr location))
              (with-current-buffer (car location)
                (cons (buffer-file-name) (line-number-at-pos (cdr location))))))
        (error (cons path nil))))))

(defun efrit-tool-introspect--definition-text (file line)
  "The top-level form starting at LINE of FILE, or nil."
  (when (and file line (file-readable-p file))
    (let ((buf (find-buffer-visiting file)))
      (with-current-buffer (or buf (find-file-noselect file t))
        (prog1
            (save-excursion
              (save-restriction
                (widen)
                (goto-char (point-min))
                (forward-line (1- line))
                (let ((start (point)))
                  (condition-case nil
                      (progn (forward-sexp)
                             (let ((text (buffer-substring-no-properties start (point))))
                               (if (<= (length text) efrit-tool-introspect-source-max-chars)
                                   text
                                 (concat (substring text 0 efrit-tool-introspect-source-max-chars)
                                         (format "\n;; … +%d chars" (- (length text) efrit-tool-introspect-source-max-chars))))))
                    (error nil)))))
          (unless buf (kill-buffer)))))))

;;;; symbol_info

(defun efrit-tool-introspect--advice (symbol)
  "Names of the advice on SYMBOL's function, as strings."
  (let (pieces)
    (when (fboundp symbol)
      (advice-mapc (lambda (f props)
                     (push (format "%s%s" (if (symbolp f) f "#<lambda>")
                                   (if-let* ((name (alist-get 'name props))) (format " (%s)" name) ""))
                           pieces))
                   symbol))
    (nreverse pieces)))

(defun efrit-tool-introspect--callers (symbol)
  "Callers of SYMBOL via xref, best effort: a list of \"file:line\"."
  (when (and (fboundp symbol) (require 'efrit-tool-navigate nil t))
    (let* ((r (ignore-errors (efrit-tool-xref-references `((symbol . ,(symbol-name symbol))))))
           (refs (and (eq t (alist-get 'success r))
                      (append (alist-get 'references (alist-get 'result r)) nil))))
      (mapcar (lambda (ref) (format "%s:%s" (alist-get 'file ref) (alist-get 'line ref))) refs))))

(defun efrit-tool-symbol-info (args)
  "Everything about the symbol ARGS's `symbol' names, in one result.
ARGS: symbol (required); buffer (where the variable's value is read;
default the target buffer); include_source (default t); include_callers
\(default nil: an xref walk of the project)."
  (efrit-tool-execute symbol_info args
    (let* ((name (or (alist-get 'symbol args) ""))
           (symbol (intern-soft name))
           (include-source (let ((v (alist-get 'include_source args))) (if (null v) t (not (eq v :json-false)))))
           (include-callers (let ((v (alist-get 'include_callers args))) (and v (not (eq v :json-false)))))
           (buf (efrit-tool-introspect--buffer (alist-get 'buffer args) "symbol_info")))
      (when (string-empty-p name) (signal 'user-error (list "symbol is required")))
      (unless symbol
        (signal 'user-error (list (format "%s is not interned in this Emacs: nothing defines it (yet)" name))))
      (let* ((is-fn (fboundp symbol))
             (is-var (boundp symbol))
             (is-face (facep symbol))
             (fn-def (and is-fn (indirect-function symbol)))
             (fn-loc (and is-fn (efrit-tool-introspect--file-line symbol 'function)))
             (var-loc (and is-var (efrit-tool-introspect--file-line symbol 'variable)))
             (kinds (delq nil (list (and is-fn (cond ((macrop symbol) "macro")
                                                     ((special-form-p symbol) "special-form")
                                                     ((commandp symbol) "command")
                                                     ((autoloadp fn-def) "autoload")
                                                     (t "function")))
                                    (and is-var (if (custom-variable-p symbol) "user-option" "variable"))
                                    (and is-face "face")))))
        (unless kinds
          (signal 'user-error (list (format "%s is interned but neither a function, a variable nor a face" name))))
        (efrit-tool-success
         `((symbol . ,name)
           (kinds . ,(vconcat kinds))
           ,@(when is-fn
               `((function
                  . ((signature . ,(ignore-errors (format "%s" (help-function-arglist symbol t))))
                     (docstring . ,(ignore-errors (documentation symbol t)))
                     (file . ,(car fn-loc))
                     (line . ,(cdr fn-loc))
                     (interactive . ,(if (commandp symbol) t :json-false))
                     (advice . ,(vconcat (efrit-tool-introspect--advice symbol)))
                     (compiled . ,(if (or (byte-code-function-p fn-def) (subrp fn-def)
                                          (and (fboundp 'native-comp-function-p) (native-comp-function-p fn-def)))
                                      t :json-false))
                     ,@(when include-source
                         `((source . ,(efrit-tool-introspect--definition-text (car fn-loc) (cdr fn-loc)))))
                     ,@(when include-callers
                         `((callers . ,(vconcat (efrit-tool-introspect--callers symbol)))))))))
           ,@(when is-var
               `((variable
                  . ((docstring . ,(ignore-errors (documentation-property symbol 'variable-documentation t)))
                     (file . ,(car var-loc))
                     (line . ,(cdr var-loc))
                     (buffer_local . ,(if (local-variable-if-set-p symbol buf) t :json-false))
                     (value_in . ,(buffer-name buf))
                     (value . ,(efrit-tool-introspect--print (buffer-local-value symbol buf)
                                                             efrit-tool-introspect-value-max-chars))
                     ,@(when (and (local-variable-p symbol buf) (default-boundp symbol))
                         `((default_value . ,(efrit-tool-introspect--print (default-value symbol)
                                                                           efrit-tool-introspect-value-max-chars))))
                     ,@(when (custom-variable-p symbol)
                         `((standard_value . ,(efrit-tool-introspect--print
                                               (ignore-errors (eval (car (get symbol 'standard-value)) t))
                                               efrit-tool-introspect-value-max-chars))))
                     ,@(when include-source
                         `((source . ,(efrit-tool-introspect--definition-text (car var-loc) (cdr var-loc)))))))))
           ,@(when is-face
               `((face . ((docstring . ,(ignore-errors (face-documentation symbol)))
                          (attributes . ,(efrit-tool-introspect--print
                                          (ignore-errors (face-all-attributes symbol (selected-frame)))
                                          efrit-tool-introspect-value-max-chars))))))))))))

;;;; live_state

(defun efrit-tool-live-state (args)
  "Current values of named variables and the state of names, in one buffer.
ARGS: names (a list of symbol names, required); buffer (default the
target buffer).  For each name: whether it is bound and fbound, its
value when bound (printed, cut), whether that value is buffer-local.
No form is evaluated: a name is looked up, nothing is called."
  (efrit-tool-execute live_state args
    (let* ((names (let ((n (alist-get 'names args))) (if (vectorp n) (append n nil) n)))
           (buf (efrit-tool-introspect--buffer (alist-get 'buffer args) "live_state")))
      (unless (and names (cl-every #'stringp names))
        (signal 'user-error (list "names must be a list of symbol names")))
      (efrit-tool-success
       `((buffer . ,(buffer-name buf))
         (names
          . ,(vconcat
              (mapcar
               (lambda (name)
                 (let ((sym (intern-soft name)))
                   (if (null sym)
                       `((name . ,name) (interned . :json-false))
                     `((name . ,name)
                       (interned . t)
                       (bound . ,(if (boundp sym) t :json-false))
                       (fbound . ,(if (fboundp sym) t :json-false))
                       ,@(when (boundp sym)
                           `((value . ,(efrit-tool-introspect--print (buffer-local-value sym buf)
                                                                     efrit-tool-introspect-value-max-chars))
                             (buffer_local . ,(if (local-variable-p sym buf) t :json-false))))
                       ,@(when (fboundp sym)
                           `((function_kind . ,(cond ((macrop sym) "macro") ((commandp sym) "command") (t "function")))
                             (advised . ,(if (efrit-tool-introspect--advice sym) t :json-false))))))))
               names))))))))

;;;; mode_line_report

(defun efrit-tool-introspect--construct-pieces (construct)
  "CONSTRUCT (a mode-line format) flattened to (SOURCE . TEXT) pairs.
SOURCE names what produced TEXT: a variable, a function in an :eval,
a conditional's switch, or \"literal\".  TEXT is `format-mode-line' of
just that piece in the current buffer and window; a piece whose text
is empty is kept, since \"this contributes nothing now\" is an answer
too.  Unbound or nil variables are skipped."
  (let (pieces)
    (cl-labels
        ((render (c) (condition-case nil (format-mode-line c) (error "#<error>")))
         (walk (c)
           (cond
            ((null c) nil)
            ((stringp c)
             (unless (string-empty-p c) (push (cons "literal" (render c)) pieces)))
            ((symbolp c)
             (when (and (boundp c) (symbol-value c))
               (push (cons (symbol-name c) (render c)) pieces)))
            ((and (consp c) (eq (car c) :eval))
             (let ((form (cadr c)))
               (push (cons (format "(:eval %s)" (if (consp form) (format "(%s …)" (car form)) form))
                           (render c))
                     pieces)))
            ((and (consp c) (eq (car c) :propertize))
             (walk (cadr c)))
            ((and (consp c) (symbolp (car c)) (not (keywordp (car c))))
             ;; (SYMBOL THEN ELSE): one piece, named after its switch
             (push (cons (format "(if %s …)" (car c)) (render c)) pieces))
            ((and (consp c) (integerp (car c)))
             (walk (cdr c)))
            ((consp c) (mapc #'walk c))
            (t nil))))
      (walk construct))
    (nreverse pieces)))

(defun efrit-tool-introspect--pieces-alist (pieces)
  (vconcat (mapcar (lambda (p) `((source . ,(car p)) (text . ,(substring-no-properties (cdr p))))) pieces)))

(defun efrit-tool-mode-line-report (args)
  "Every window's mode line, header line and tab line, by construct.
ARGS: frame (\"selected\" default, or \"all\"); buffer (only windows
showing it).  For each window: the rendered text of each line and the
pieces it is made of, with the variable or :eval function each piece
came from.  Also the `global-mode-string' entries, which every mode
line shares."
  (efrit-tool-execute mode_line_report args
    (let* ((all-frames (equal (alist-get 'frame args) "all"))
           (only (alist-get 'buffer args))
           (windows (cl-remove-if-not
                     (lambda (w) (or (null only) (equal (buffer-name (window-buffer w)) only)))
                     (if all-frames (window-list-1 nil 'nomini t) (window-list nil 'nomini))))
           (entries
            (mapcar
             (lambda (w)
               (with-selected-window w
                 (with-current-buffer (window-buffer w)
                   `((buffer . ,(buffer-name))
                     (selected . ,(if (eq w (selected-window)) t :json-false))
                     (pixel_edges . ,(vconcat (window-pixel-edges w)))
                     (mode_line
                      . ((text . ,(substring-no-properties (format-mode-line mode-line-format)))
                         (mode_name . ,(substring-no-properties (format-mode-line mode-name)))
                         (pieces . ,(efrit-tool-introspect--pieces-alist
                                     (efrit-tool-introspect--construct-pieces mode-line-format)))))
                     (header_line
                      . ,(if header-line-format
                             `((text . ,(substring-no-properties (format-mode-line header-line-format)))
                               (pieces . ,(efrit-tool-introspect--pieces-alist
                                           (efrit-tool-introspect--construct-pieces header-line-format))))
                           "none"))
                     (tab_line
                      . ,(if tab-line-format
                             `((text . ,(substring-no-properties (format-mode-line tab-line-format))))
                           "none"))))))
             windows)))
      (efrit-tool-success
       `((windows . ,(vconcat entries))
         (global_mode_string
          . ,(efrit-tool-introspect--pieces-alist
              (with-current-buffer (window-buffer (selected-window))
                (efrit-tool-introspect--construct-pieces global-mode-string))))
         (mode_line_misc_info
          . ,(efrit-tool-introspect--pieces-alist
              (with-current-buffer (window-buffer (selected-window))
                (efrit-tool-introspect--construct-pieces mode-line-misc-info)))))))))

;;;; buffer_list

(defun efrit-tool-introspect--buffer-remote-p (buf)
  "Non-nil when BUF visits a remote file or its directory is remote."
  (let ((file (buffer-file-name buf))
        (dir (buffer-local-value 'default-directory buf)))
    (or (and file (file-remote-p file))
        (and (not file) dir (file-remote-p dir)))))

(defun efrit-tool-buffer-list (args)
  "The live buffers, described, without entering any of them.
ARGS: pattern (regexp on buffer names and file names, optional);
include_hidden (names starting with a space; default false);
max (default 200).  Each entry: name, file, remote (the TRAMP host
part, or false), size, major_mode, modified, shown (in some window).
Remote buffers are listed by name and file only; reading them is a
separate decision (see the description).  Local entries come first."
  (efrit-tool-execute buffer_list args
    (let* ((pattern (alist-get 'pattern args))
           (hidden (let ((v (alist-get 'include_hidden args))) (and v (not (eq v :json-false)))))
           (max (or (alist-get 'max args) 200))
           (entries nil))
      (dolist (b (buffer-list))
        (let* ((name (buffer-name b))
               (file (buffer-file-name b)))
          (when (and (or hidden (not (string-prefix-p " " name)))
                     (or (null pattern) (string-empty-p pattern)
                         (string-match-p pattern name)
                         (and file (string-match-p pattern file))))
            (let ((remote (efrit-tool-introspect--buffer-remote-p b)))
              (push `((name . ,name)
                      (file . ,(and file (abbreviate-file-name file)))
                      (remote . ,(or remote :json-false))
                      (size . ,(buffer-size b))
                      (major_mode . ,(symbol-name (buffer-local-value 'major-mode b)))
                      (modified . ,(if (buffer-modified-p b) t :json-false))
                      (shown . ,(if (get-buffer-window b t) t :json-false)))
                    entries)))))
      (setq entries (nreverse entries))
      ;; local first, then remote: the order the user wants them considered
      (setq entries (append (cl-remove-if (lambda (e) (not (eq (alist-get 'remote e) :json-false))) entries)
                            (cl-remove-if (lambda (e) (eq (alist-get 'remote e) :json-false)) entries)))
      (let ((shown (seq-take entries max)))
        (efrit-tool-success
         `((count . ,(length entries))
           (local . ,(cl-count-if (lambda (e) (eq (alist-get 'remote e) :json-false)) entries))
           (remote . ,(cl-count-if (lambda (e) (not (eq (alist-get 'remote e) :json-false))) entries))
           (buffers . ,(vconcat shown)))
         (and (> (length entries) max)
              (list (format "%d more not shown (max %d); pass pattern to narrow" (- (length entries) max) max))))))))

;;;; buffer_scan

(defun efrit-tool-introspect--scan-one (buf regexp context max from-end)
  "Matches of REGEXP in BUF as alists, newest first when FROM-END, at most MAX."
  (with-current-buffer buf
    (save-excursion
      (save-restriction
        (widen)
        (let ((case-fold-search nil) (found nil) (n 0))
          (goto-char (if from-end (point-max) (point-min)))
          (while (and (< n max)
                      (if from-end (re-search-backward regexp nil t) (re-search-forward regexp nil t)))
            (cl-incf n)
            (let* ((line (line-number-at-pos (match-beginning 0)))
                   (bol (save-excursion (goto-char (match-beginning 0)) (line-beginning-position)))
                   (before (save-excursion (goto-char bol) (forward-line (- context))
                                           (buffer-substring-no-properties (point) bol)))
                   (after (save-excursion (goto-char (match-beginning 0)) (forward-line 1)
                                          (let ((s (point))) (forward-line context)
                                               (buffer-substring-no-properties s (point))))))
              (push `((line . ,line)
                      (column . ,(- (match-beginning 0) bol))
                      (match . ,(buffer-substring-no-properties (match-beginning 0) (match-end 0)))
                      (text . ,(buffer-substring-no-properties bol (save-excursion (goto-char bol) (line-end-position))))
                      ,@(when (> context 0)
                          `((context_before . ,(string-trim-right before "\n"))
                            (context_after . ,(string-trim-right after "\n")))))
                    found))
            ;; an empty match must still advance
            (when (= (match-beginning 0) (match-end 0))
              (if from-end (backward-char 1) (forward-char 1))))
          (nreverse found))))))

(defun efrit-tool-buffer-scan (args)
  "Regexp matches with context in one buffer, or every buffer matching a pattern.
ARGS: pattern (regexp, required); buffer (a name, default the target
buffer); buffer_regexp (scan every live buffer whose name matches,
instead of `buffer'); context (lines each side, default 2); max
\(matches in all, default `efrit-tool-buffer-scan-max-matches'); from_end
\(newest first: the last matches of a log buffer); include_remote
\(under buffer_regexp, also scan buffers visiting remote files;
default false: they are listed as skipped, by name, so the model can
ask about one).  Hidden buffers (names starting with a space) are
skipped under a pattern.  Each buffer passes the sandbox's buffer check."
  (efrit-tool-execute buffer_scan args
    (let* ((regexp (or (alist-get 'pattern args) ""))
           (context (or (alist-get 'context args) 2))
           (max (min (or (alist-get 'max args) efrit-tool-buffer-scan-max-matches)
                     efrit-tool-buffer-scan-max-matches))
           (from-end (let ((v (alist-get 'from_end args))) (and v (not (eq v :json-false)))))
           (include-remote (let ((v (alist-get 'include_remote args))) (and v (not (eq v :json-false)))))
           (name-re (alist-get 'buffer_regexp args))
           (remote-skipped nil)
           (buffers (if (and name-re (not (string-empty-p name-re)))
                        (seq-take (cl-remove-if-not
                                   (lambda (b)
                                     (and (not (string-prefix-p " " (buffer-name b)))
                                          (string-match-p name-re (buffer-name b))
                                          (or include-remote
                                              (not (efrit-tool-introspect--buffer-remote-p b))
                                              (progn (push (buffer-name b) remote-skipped) nil))))
                                   (buffer-list))
                                  efrit-tool-buffer-scan-max-buffers)
                      (list (efrit-tool-introspect--buffer (alist-get 'buffer args) "buffer_scan"))))
           (left max) (results nil) (skipped nil))
      (when (string-empty-p regexp) (signal 'user-error (list "pattern is required")))
      (condition-case err (string-match-p regexp "")
        (invalid-regexp (signal 'user-error (list (format "bad regexp: %s" (error-message-string err))))))
      (dolist (buf buffers)
        (when (> left 0)
          (if (condition-case nil (progn (efrit-sandbox-check-buffer buf "buffer_scan") t)
                (efrit-sandbox-denied nil))
              (let ((matches (efrit-tool-introspect--scan-one buf regexp context left from-end)))
                (cl-decf left (length matches))
                (when matches
                  (push `((buffer . ,(buffer-name buf))
                          (file . ,(buffer-file-name buf))
                          (count . ,(length matches))
                          (matches . ,(vconcat matches)))
                        results)))
            (push (buffer-name buf) skipped))))
      (efrit-tool-success
       `((pattern . ,regexp)
         (buffers_scanned . ,(- (length buffers) (length skipped)))
         (total_matches . ,(- max left))
         (truncated . ,(if (<= left 0) t :json-false))
         (results . ,(vconcat (nreverse results))))
       (delq nil
             (list (and skipped (format "not scanned (no buffer grant): %s" (string-join (nreverse skipped) ", ")))
                   (and remote-skipped
                        (format "remote buffers not scanned (name them with buffer= or pass include_remote=true if one is clearly the right place): %s"
                                (string-join (nreverse remote-skipped) ", ")))))))))

(provide 'efrit-tool-introspect)

;;; efrit-tool-introspect.el ends here
