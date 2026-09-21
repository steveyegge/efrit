;;; efrit-package-review.el --- Model-assisted review of incoming packages -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; Emacs 31 can stop before installing a package and let you read the
;; source, diff it against the previous installation, and read the
;; change log (`package-review-policy', `package-review').  Reading a
;; whole package before every upgrade is more than most people do,
;; which is what makes the review a formality.  This module has a
;; model read it and report, so the human decision is made with the
;; risky forms already pointed at.
;;
;; `efrit-package-review-mode' wraps `package-review'.  Before
;; package.el asks "Install?", the reviewer sees the same material the
;; user could open (the sources, the diff when there was a previous
;; install, the changelog) and returns a verdict with findings: each
;; a severity, a file and line, and one sentence.  The report opens in
;; a popup beside the prompt (q closes it), and the prompt itself is
;; prefixed with the verdict.  What happens next is
;; `efrit-package-review-action':
;;
;;   annotate            show the report; you answer the prompt (default)
;;   auto-approve-clean  a clean verdict answers yes for you; anything
;;                       flagged, or a failed review, falls back to
;;                       the prompt with the report shown
;;
;; `M-x efrit-review-package' runs the same review on demand for an
;; installed or an archive package, on any Emacs version.
;;
;; The reviewer starts from the diff and the change log (or the file
;; list, for a new install) and opens files itself with one tool,
;; read_package_file, confined to the package directory.  A whole
;; package sent up front was refused by the API; this is also how a
;; person reads an upgrade.  The report lists what was opened.  The
;; rubric: network use, evaluation or loading of foreign code, writes
;; outside the package directory, external programs, redefinition of
;; built-ins, encoded data, credentials, and diff-level changes of
;; maintainer, archive, dependencies or load-time code.
;;
;; The model is `efrit-package-review-model' when set, else the
;; per-turn reviewer's model, else the default: a rarer and higher
;; stakes review is where a stronger model earns its cost.
;;
;; Nothing here runs package code.  The reads are efrit's own, made
;; on the user's request, so they are not sandbox prompts (the sandbox
;; gates what the model does); the API call carries a purpose so a
;; failure names it.  None of this is reachable from eval_sexp: the
;; efrit-package-review prefix is on the refused list.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'package)
(require 'efrit-log)
(require 'efrit-api)
(require 'efrit-chat-response)
(require 'efrit-review)          ; efrit-review-model, efrit-review-parse-verdict
(require 'efrit-ui-helpers)

(defvar efrit-default-model)
(declare-function package-find-news-file "package")
(declare-function package-desc-dir "package")
(declare-function package--get-activatable-pkg "package")

(defgroup efrit-package-review nil
  "Model-assisted review of packages before they are installed."
  :group 'efrit-review
  :prefix "efrit-package-review-")

(defcustom efrit-package-review-model nil
  "Model that reviews packages, or nil for `efrit-review-model' / the default.
A package review is rare and the cost of a miss is high; name a
stronger model here than the one that reviews every turn."
  :type '(choice (const :tag "Same as the turn reviewer" nil) string)
  :group 'efrit-package-review)

(defcustom efrit-package-review-action 'annotate
  "What the review does with its verdict at install time.
`annotate' shows the report and leaves the answer to you.
`auto-approve-clean' answers yes when the verdict is clean; a
flagged package, or a review that failed, still asks you."
  :type '(choice (const annotate) (const auto-approve-clean))
  :group 'efrit-package-review)

(defcustom efrit-package-review-max-file-chars 60000
  "Most characters of one source file shown to the reviewer.
Longer files are cut and the cut is reported to the reviewer and in
the verdict."
  :type 'integer
  :group 'efrit-package-review)

(defcustom efrit-package-review-max-total-chars 120000
  "Most characters of the diff sent in the first request.
A longer diff is cut with a note; the reviewer opens the files."
  :type 'integer
  :group 'efrit-package-review)

(defcustom efrit-package-review-timeout 120
  "Seconds to wait for the reviewer at install time.
`package-review' is synchronous, so the review must be too."
  :type 'integer
  :group 'efrit-package-review)

(defconst efrit-package-review-severities '(high medium low info)
  "Severities a finding may carry, most serious first.")

(defconst efrit-package-review--buffer "*efrit-package-review*")

;;; Gathering what the reviewer sees first
;;
;; The first request is small: package metadata, the change log, the
;; diff from the installed version (or, for a new install, the file
;; list with sizes), and one tool the reviewer may call to read any
;; file of the package by name, whole or by line range.  The reviewer
;; decides what to open.  This keeps the request within what the API
;; accepts (a whole package up front was refused outright) and mirrors
;; how a person reviews an upgrade: read the diff, open what it
;; touches.

(defun efrit-package-review--source-files (dir)
  "The reviewable files under DIR, relative names, never .elc or images."
  (mapcar (lambda (f) (file-relative-name f dir))
          (cl-remove-if
           (lambda (f)
             (or (string-match-p "\\.\\(elc\\|eln\\|png\\|jpg\\|gif\\|svg\\|info\\|gz\\)\\'" f)
                 (string-match-p "/\\.git/" f)))
           (directory-files-recursively dir "" nil))))

(defun efrit-package-review--news-file (pkg-desc pkg-dir)
  "The change log file for PKG-DESC unpacked in PKG-DIR, or nil.
package.el looks in the desc's own dir, which at review time may not
be PKG-DIR yet; look in PKG-DIR first, with the usual names."
  (or (cl-some (lambda (name)
                 (let ((f (expand-file-name name pkg-dir)))
                   (and (file-regular-p f) (file-readable-p f) f)))
               '("NEWS-elpa" "NEWS" "news" "CHANGELOG" "CHANGELOG.md" "ChangeLog" "NEWS.md" "NEWS.org"))
      (and (fboundp 'package-find-news-file)
           (ignore-errors (package-find-news-file pkg-desc)))))

(defun efrit-package-review--diff (old-dir new-dir)
  "Unified diff from OLD-DIR to NEW-DIR as a string, or nil when git is missing.
Paths are shown relative to each tree, so the reviewer can name a file
to open without the temp directory prefix."
  (when (executable-find "git")
    (with-temp-buffer
      (call-process "git" nil t nil "diff" "--no-index" "--no-color"
                    "--diff-filter=d" "--minimal" old-dir new-dir)
      (let ((text (buffer-string)))
        (dolist (dir (list old-dir new-dir))
          (setq text (replace-regexp-in-string (regexp-quote (directory-file-name dir)) "" text t t)))
        ;; --no-index takes no pathspec: drop the sections for compiled
        ;; and binary files here, they are noise in a review
        (mapconcat #'identity
                   (cl-remove-if (lambda (section)
                                   (string-match-p "\\`--git a/.*\\.\\(elc\\|eln\\|info\\|png\\|jpg\\|gif\\|gz\\) " section))
                                 (split-string text "^diff " t))
                   "diff ")))))

(defun efrit-package-review--cut (text limit label)
  "TEXT cut at LIMIT characters with a note naming LABEL, or nil for nil."
  (cond ((null text) nil)
        ((<= (length text) limit) text)
        (t (concat (substring text 0 limit)
                   (format "\n[... %s cut here: %d more characters; open the file to see the rest ...]\n"
                           label (- (length text) limit))))))

(defun efrit-package-review-gather (pkg-desc pkg-dir old-desc)
  "What the first request carries for PKG-DESC unpacked in PKG-DIR.
OLD-DESC is the installed version or nil.  Returns a plist
\(:name :version :archive :maintainers :old-version :dir :files
:diff :news :cut).  :files is the relative file list with sizes;
sources themselves are read on the reviewer's request."
  (let* ((news-file (efrit-package-review--news-file pkg-desc pkg-dir))
         (old-dir (and old-desc (package-desc-dir old-desc)))
         (raw-diff (and old-dir (file-directory-p old-dir)
                        (efrit-package-review--diff old-dir pkg-dir)))
         (diff (efrit-package-review--cut raw-diff efrit-package-review-max-total-chars "diff"))
         (news (and news-file
                    (efrit-package-review--cut
                     (with-temp-buffer (insert-file-contents news-file) (buffer-string))
                     efrit-package-review-max-file-chars "changelog"))))
    (list :name (symbol-name (package-desc-name pkg-desc))
          :version (package-version-join (package-desc-version pkg-desc))
          :archive (package-desc-archive pkg-desc)
          ;; a string ("Name <mail>", comma-separated), not a list:
          ;; walking it as a sequence printed char codes, and a line of
          ;; comma-separated numbers under "Maintainers:" got every
          ;; request refused by the API as encoded data
          :maintainers (ignore-errors (package-maintainers pkg-desc t))
          :old-version (and old-desc (package-version-join (package-desc-version old-desc)))
          :dir pkg-dir
          :files (mapcar (lambda (rel)
                           (cons rel (or (nth 7 (file-attributes (expand-file-name rel pkg-dir))) 0)))
                         (efrit-package-review--source-files pkg-dir))
          :diff diff
          :news news
          :cut (or (and raw-diff (> (length raw-diff) (length diff)))
                   (and news (string-match-p "cut here" news))))))

;;; The reviewer's one tool

(defconst efrit-package-review--tool-name "read_package_file")

(defconst efrit-package-review--tool-schema
  `(("name" . ,efrit-package-review--tool-name)
    ("description" . "Read a file of the package under review, whole or a line range.  Paths are relative to the package directory, as listed in the file list and the diff.  Use it on the files the diff touches and on anything the changelog or the file list makes you want to see.")
    ("input_schema" . (("type" . "object")
                       ("properties" . (("path" . (("type" . "string")
                                                   ("description" . "Relative path inside the package")))
                                        ("start_line" . (("type" . "integer")
                                                         ("description" . "First line to return, 1-based (default 1)")))
                                        ("end_line" . (("type" . "integer")
                                                       ("description" . "Last line to return (default: end of file)")))))
                       ("required" . ["path"]))))
  "The tool schema, in the alist shape `efrit-api' encodes.")

(defun efrit-package-review--read-tool (dir input)
  "Run the read tool: INPUT's path under DIR, optional line range.
Never leaves DIR: a path that resolves outside it is refused, as is
one the file list excludes.  Returns the text, or an error string."
  (let* ((rel (gethash "path" input))
         (start (or (gethash "start_line" input) 1))
         (end (gethash "end_line" input))
         (file (and (stringp rel) (expand-file-name rel dir)))
         (root (file-name-as-directory (file-truename dir))))
    (cond
     ((not (stringp rel)) "Error: path is required")
     ((not (string-prefix-p root (file-truename file)))
      (format "Error: %s is outside the package directory" rel))
     ((not (file-regular-p file)) (format "Error: no such file in the package: %s" rel))
     ((not (member rel (efrit-package-review--source-files dir)))
      (format "Error: %s is not a reviewable source file" rel))
     (t
      (with-temp-buffer
        (insert-file-contents file)
        (let* ((total (count-lines (point-min) (point-max)))
               (end (min total (or end total)))
               (start (max 1 (min start end))))
          (goto-char (point-min)) (forward-line (1- start))
          (let ((from (point)))
            (forward-line (1+ (- end start)))
            (let ((text (buffer-substring from (point))))
              (concat (format "%s lines %d-%d of %d:\n" rel start end total)
                      (efrit-package-review--cut text efrit-package-review-max-file-chars rel))))))))))

;;; The request

(defconst efrit-package-review--system-prompt
  "You are helping an Emacs user decide whether to install or upgrade an Emacs Lisp package from a public package archive.  This is an ordinary pre-install code review of open-source software, the same reading a careful maintainer does before pressing yes.

You are given the package's metadata, its change log, and either a unified diff from the version already installed or, for a new install, the list of files.  You have one tool, read_package_file, to open any file of the package by name, whole or by line range.  Start from the diff: open the files it touches where the diff alone does not show what the new code does.  For a new install, open the main file and anything whose name or size stands out.  Do not read every file; read what the decision needs.

Report, briefly and specifically, what the user should look at before installing.  In order of importance:
1. Network use, and what is done with anything received.
2. Evaluation or loading of code that is not part of the package's own files.
3. Files written or modified outside the package's own directory (user init files, dotfiles, system paths).
4. External programs run, and which.
5. Redefinition of, or advice on, built-in Emacs functions.
6. Encoded or hard-to-read data in the source, and what it is for.
7. Configuration or credentials read, and where they are sent.
8. In the diff: changes of maintainer, archive or repository URL; new dependencies; new code that runs at load time.

Behaviour that is the package's stated purpose is not a finding; mention it once as info.

When you have read enough, answer with exactly one JSON object and nothing else:
{\"verdict\": \"approve\" | \"reject\",
 \"summary\": one or two sentences for the user,
 \"findings\": [{\"severity\": \"high\"|\"medium\"|\"low\"|\"info\", \"file\": \"relative/path.el\", \"line\": N or null, \"note\": one sentence}],
 \"files_read\": [\"relative/path.el\", ...],
 \"saw_everything\": true | false}
Use reject when a high finding exists, or a medium one is not explained by the package's purpose.  Set saw_everything false if you could not open something you needed."
  "System prompt for the package reviewer.
Diff first, sources on request.  A whole package sent up front (57k
tokens of code beside a review rubric) was refused by the API before
generation; the small first request and the reviewer's own reads keep
each request modest and match how an upgrade is read by a person.")

(defun efrit-package-review--user-message (info)
  "The first user message for INFO (see `efrit-package-review-gather')."
  (concat
   (format "Package: %s %s%s\nArchive: %s\nMaintainers: %s\n%s\n"
           (plist-get info :name) (plist-get info :version)
           (if (plist-get info :old-version)
               (format " (upgrading from %s)" (plist-get info :old-version))
             " (new install)")
           (or (plist-get info :archive) "unknown")
           (let ((m (plist-get info :maintainers)))
             (cond ((and (stringp m) (not (string-empty-p m))) m)
                   ((consp m) (mapconcat (lambda (x) (format "%s" (if (consp x) (car x) x))) m ", "))
                   (t "unknown")))
           (if (plist-get info :cut)
               "NOTE: the diff or changelog below was cut for size; open files to see the rest."
             ""))
   (format "\n=== FILES (%d) ===\n%s\n" (length (plist-get info :files))
           (mapconcat (lambda (f) (format "%s  (%d bytes)" (car f) (cdr f)))
                      (plist-get info :files) "\n"))
   (when-let* ((news (plist-get info :news)))
     (concat "\n=== CHANGELOG ===\n" news "\n"))
   (if-let* ((diff (plist-get info :diff)))
       (concat "\n=== DIFF FROM INSTALLED VERSION ===\n" diff "\n")
     "\n(New install: no previous version to diff against.  Open the main file and what stands out.)\n")))

(defun efrit-package-review-model ()
  "The model that reviews packages."
  (or efrit-package-review-model efrit-review-model efrit-default-model))

(defun efrit-package-review--request (messages)
  "A request carrying MESSAGES (the growing review conversation)."
  `(("model" . ,(efrit-package-review-model))
    ("max_tokens" . 3000)
    ("system" . ,(efrit-api-cacheable-system efrit-package-review--system-prompt))
    ("tools" . ,(vector efrit-package-review--tool-schema))
    ("messages" . ,(vconcat messages))))

(defun efrit-package-review--json-span (text)
  "The first balanced {...} object in TEXT, or nil.
A greedy match to the last brace fails when the answer has prose
with a brace after the object; a fence around the object is fine."
  (when (and (stringp text) (string-match "{" text))
    (let ((start (match-beginning 0)) (depth 0) (i (match-beginning 0)) (in-string nil) (end nil))
      (while (and (< i (length text)) (not end))
        (let ((c (aref text i)))
          (cond
           (in-string (cond ((eq c ?\\) (cl-incf i)) ((eq c ?\") (setq in-string nil))))
           ((eq c ?\") (setq in-string t))
           ((eq c ?{) (cl-incf depth))
           ((eq c ?}) (cl-decf depth) (when (zerop depth) (setq end (1+ i))))))
        (cl-incf i))
      (and end (substring text start end)))))

(defun efrit-package-review-parse (text)
  "Parse the reviewer's TEXT into a plist, or nil if malformed.
\(:verdict SYM :summary STR :findings ((:severity SYM :file STR :line N :note STR)...)
:files-read LIST :saw-everything BOOL)."
  (when-let* ((span (efrit-package-review--json-span text)))
    (condition-case nil
        (let* ((obj (json-parse-string span
                                       :object-type 'alist :array-type 'list
                                       :null-object nil :false-object nil))
               (verdict (intern (downcase (format "%s" (alist-get 'verdict obj))))))
          (when (memq verdict efrit-review-verdicts)
            (list :verdict verdict
                  :summary (or (alist-get 'summary obj) "")
                  :saw-everything (eq (alist-get 'saw_everything obj) t)
                  :files-read (cl-remove-if-not #'stringp (alist-get 'files_read obj))
                  :findings
                  (delq nil
                        (mapcar (lambda (f)
                                  (let ((sev (intern (downcase (format "%s" (alist-get 'severity f))))))
                                    (when (memq sev efrit-package-review-severities)
                                      (list :severity sev
                                            :file (alist-get 'file f)
                                            :line (alist-get 'line f)
                                            :note (or (alist-get 'note f) "")))))
                                (alist-get 'findings obj))))))
      (error nil))))

(defcustom efrit-package-review-max-reads 40
  "Most read_package_file calls one review may make before it must answer."
  :type 'integer
  :group 'efrit-package-review)

(cl-defstruct (efrit-package-review-state (:constructor efrit-package-review-state-create)
                                          (:copier nil))
  "One review in progress: the conversation so far and the files opened."
  info
  messages
  (reads nil))

(defun efrit-package-review--begin (info)
  "A fresh state for reviewing INFO."
  (efrit-package-review-state-create
   :info info
   :messages (list `((role . "user") (content . ,(efrit-package-review--user-message info))))))

(defun efrit-package-review--purpose (info)
  "The `efrit-api-request-purpose' for a review of INFO."
  (format "reviewing package %s %s" (plist-get info :name) (plist-get info :version)))

(defun efrit-package-review--finish (state verdict)
  "VERDICT with STATE's reads attached: what every driver returns."
  (append (list :reads (reverse (efrit-package-review-state-reads state))) verdict))

(defun efrit-package-review--answer-tools (state uses)
  "Run the reviewer's tool calls USES, extend STATE's conversation with them."
  (let* ((info (efrit-package-review-state-info state))
         (dir (plist-get info :dir))
         (results nil))
    (dolist (use uses)
      (let* ((input (nth 2 use))
             (rel (and (hash-table-p input) (gethash "path" input)))
             (out (if (equal (nth 1 use) efrit-package-review--tool-name)
                      (efrit-package-review--read-tool dir input)
                    (format "Error: unknown tool %s" (nth 1 use)))))
        (efrit-log 'debug "package review %s: read %s (%d chars)" (plist-get info :name) rel (length out))
        (push rel (efrit-package-review-state-reads state))
        (push (efrit-api-build-tool-result (nth 0 use) out (string-prefix-p "Error" out)) results)))
    (setf (efrit-package-review-state-messages state)
          (append (efrit-package-review-state-messages state)
                  (list `((role . "user") (content . ,(vconcat (nreverse results)))))))))

(defun efrit-package-review--step (state response)
  "Advance STATE with the reviewer's RESPONSE.
Returns (done . VERDICT) when the review is over, or (continue) when
the reviewer asked to read files and STATE's conversation now carries
the answers for the next request."
  (let ((info (efrit-package-review-state-info state)))
    (cond
     ((and response (efrit-response-error response))
      (cons 'done (list :verdict 'error
                        :summary (efrit-error-message (efrit-response-error response)))))
     ;; The API declined before generating.  Not a finding.
     ((equal (efrit-response-stop-reason response) "refusal")
      (cons 'done
            (list :verdict 'error :refused t
                  :summary (format "the API refused to process this request (stop reason refusal, %s tokens in). Nothing was judged. This is a classifier decision about the request, not a finding about %s; try another model (efrit-package-review-model)."
                                   (let ((u (efrit-response-usage response)))
                                     (or (and u (gethash "input_tokens" u)) "?"))
                                   (plist-get info :name)))))
     (t
      (let* ((content (efrit-response-content response))
             (uses (delq nil (mapcar #'efrit-content-item-as-tool-use (append content nil))))
             (text (efrit-package-review--response-text response)))
        (cond
         ;; tool calls: answer them and go round again
         ((and uses (< (length (efrit-package-review-state-reads state)) efrit-package-review-max-reads))
          (setf (efrit-package-review-state-messages state)
                (append (efrit-package-review-state-messages state)
                        (list `((role . "assistant") (content . ,content)))))
          (efrit-package-review--answer-tools state uses)
          (list 'continue))
         (uses
          (cons 'done (list :verdict 'error
                            :summary (format "the reviewer asked to read more than %d files without answering"
                                             efrit-package-review-max-reads))))
         (t
          (efrit-log 'debug "package review %s: reviewer said: %s" (plist-get info :name)
                     (truncate-string-to-width text 600 nil nil "…"))
          (cons 'done
                (or (efrit-package-review-parse text)
                    (list :verdict 'error
                          :summary (format "the reviewer's answer was not a verdict (%d chars, stop reason %s)"
                                           (length text) (or (efrit-response-stop-reason response) "?"))
                          :raw text))))))))))

(defun efrit-package-review-run (info)
  "Review INFO: a synchronous tool loop until the reviewer answers.
Returns the parsed verdict plist with :reads (the files opened), or
\(:verdict error :summary WHY [:refused t] [:raw TEXT]) when the call,
the parse, or the read budget failed.  Blocks: for `package-review',
which is synchronous itself.  `efrit-package-review-run-async' is the
same review without blocking."
  (let* ((state (efrit-package-review--begin info))
         (result nil)
         (efrit-api-request-purpose (efrit-package-review--purpose info)))
    (condition-case err
        (while (not result)
          (let ((step (efrit-package-review--step
                       state
                       (efrit-api-request-sync
                        (efrit-package-review--request (efrit-package-review-state-messages state))
                        efrit-package-review-timeout))))
            (when (eq (car step) 'done) (setq result (cdr step)))))
      (error (setq result (list :verdict 'error :summary (error-message-string err)))))
    (efrit-package-review--finish state result)))

(defun efrit-package-review-run-async (info callback)
  "Review INFO without blocking; call CALLBACK with the verdict when done.
The verdict has the shape `efrit-package-review-run' returns.  Returns
the state; `efrit-package-review-cancel' on it stops the loop after
the request in flight."
  (let ((state (efrit-package-review--begin info)))
    (efrit-package-review--send state callback)
    state))

(defvar efrit-package-review--cancelled (make-hash-table :test 'eq :weakness 'key)
  "States whose async review was cancelled: the loop stops at the next step.")

(defun efrit-package-review-cancel (state)
  "Stop the async review STATE after the request in flight."
  (puthash state t efrit-package-review--cancelled))

(defun efrit-package-review--send (state callback)
  "Send STATE's next request; on the answer, step and send again or finish."
  (let ((efrit-api-request-purpose (efrit-package-review--purpose (efrit-package-review-state-info state))))
    (efrit-api-request-async
     (efrit-package-review--request (efrit-package-review-state-messages state))
     (lambda (response)
       (let ((step (condition-case err
                       (efrit-package-review--step state response)
                     (error (cons 'done (list :verdict 'error :summary (error-message-string err)))))))
         (cond
          ((eq (car step) 'done)
           (funcall callback (efrit-package-review--finish state (cdr step))))
          ((gethash state efrit-package-review--cancelled)
           (funcall callback (efrit-package-review--finish
                              state (list :verdict 'error :cancelled t :summary "cancelled"))))
          (t (efrit-package-review--send state callback)))))
     (lambda (message)
       (funcall callback (efrit-package-review--finish
                          state (list :verdict 'error :summary message)))))))

(defun efrit-package-review--response-text (response)
  (let ((content (efrit-response-content response)) (texts nil))
    (when content
      (dotimes (i (length content))
        (let ((item (aref content i)))
          (when (and (hash-table-p item) (equal (gethash "type" item) "text"))
            (push (gethash "text" item) texts)))))
    (string-join (nreverse texts) "")))

;;; The report

(defun efrit-package-review-clean-p (verdict)
  "Non-nil if VERDICT approves with nothing above `info' and saw everything."
  (and (eq (plist-get verdict :verdict) 'approve)
       (plist-get verdict :saw-everything)
       (cl-every (lambda (f) (eq (plist-get f :severity) 'info))
                 (plist-get verdict :findings))))

(defun efrit-package-review-verdict-line (verdict)
  "One line: the verdict and the count of findings by severity."
  (pcase (plist-get verdict :verdict)
    ('error (format "efrit review failed: %s" (plist-get verdict :summary)))
    (v (format "efrit: %s · %s%s"
               (if (eq v 'approve) "approve" "REJECT")
               (efrit-package-review--counts-text verdict)
               (if (plist-get verdict :saw-everything) "" " · did not see everything")))))

(defgroup efrit-package-review-faces nil
  "Faces of the package review report."
  :group 'efrit-package-review)

(defface efrit-package-review-title
  '((t :inherit bold :height 1.2))
  "The package name and version at the top of a report."
  :group 'efrit-package-review-faces)

(defface efrit-package-review-approve
  '((((background dark)) :foreground "#1b1b1b" :background "#8fbc8f")
    (t :foreground "white" :background "#2e8b57"))
  "Badge of an approving verdict."
  :group 'efrit-package-review-faces)

(defface efrit-package-review-reject
  '((((background dark)) :foreground "#1b1b1b" :background "#e57373")
    (t :foreground "white" :background "#b22222"))
  "Badge of a rejecting verdict."
  :group 'efrit-package-review-faces)

(defface efrit-package-review-failed
  '((((background dark)) :foreground "#1b1b1b" :background "#b0b0b0")
    (t :foreground "white" :background "#6e6e6e"))
  "Badge of a review that did not produce a verdict."
  :group 'efrit-package-review-faces)

(defface efrit-package-review-high
  '((t :inherit efrit-package-review-reject))
  "Badge of a high severity finding."
  :group 'efrit-package-review-faces)

(defface efrit-package-review-medium
  '((((background dark)) :foreground "#1b1b1b" :background "#e6b422")
    (t :foreground "#1b1b1b" :background "#f0c040"))
  "Badge of a medium severity finding."
  :group 'efrit-package-review-faces)

(defface efrit-package-review-low
  '((((background dark)) :foreground "#1b1b1b" :background "#8ab4f8")
    (t :foreground "white" :background "#3b6ea5"))
  "Badge of a low severity finding."
  :group 'efrit-package-review-faces)

(defface efrit-package-review-info
  '((t :inherit efrit-package-review-failed))
  "Badge of an informational finding."
  :group 'efrit-package-review-faces)

(defface efrit-package-review-note
  '((t :inherit default))
  "The text of a finding."
  :group 'efrit-package-review-faces)

(defface efrit-package-review-meta
  '((t :inherit shadow))
  "The footer: reviewer model, what it was given, what it opened."
  :group 'efrit-package-review-faces)

(defcustom efrit-package-review-fill-column 78
  "Width the report's prose is filled to, bounded by the window width."
  :type 'integer
  :group 'efrit-package-review)

(defun efrit-package-review--verdict-face (verdict)
  "The badge face for VERDICT's verdict."
  (pcase (plist-get verdict :verdict)
    ('approve 'efrit-package-review-approve)
    ('error 'efrit-package-review-failed)
    (_ 'efrit-package-review-reject)))

(defun efrit-package-review--verdict-word (verdict)
  "The word on VERDICT's badge."
  (pcase (plist-get verdict :verdict)
    ('approve "APPROVE")
    ('error "NO VERDICT")
    (_ "REJECT")))

(defun efrit-package-review--severity-face (severity)
  "The badge face for SEVERITY."
  (intern (format "efrit-package-review-%s" severity)))

(defun efrit-package-review--sorted-findings (verdict)
  "VERDICT's findings, most serious first, order within a severity kept."
  (sort (copy-sequence (plist-get verdict :findings))
        (lambda (a b) (< (cl-position (plist-get a :severity) efrit-package-review-severities)
                         (cl-position (plist-get b :severity) efrit-package-review-severities)))))

(defun efrit-package-review--counts-text (verdict)
  "\"2 high, 1 low\" for VERDICT, or \"no findings\"."
  (let ((parts (delq nil
                     (mapcar (lambda (sev)
                               (let ((n (cl-count sev (plist-get verdict :findings)
                                                  :key (lambda (f) (plist-get f :severity)))))
                                 (and (> n 0) (format "%d %s" n sev))))
                             efrit-package-review-severities))))
    (if parts (mapconcat #'identity parts ", ") "no findings")))

(defun efrit-package-review--footer (info verdict)
  "The footer line: reviewer, what it was given, what it opened."
  (format "Reviewer: %s.  Given: %d file(s) listed%s%s.  Opened: %s."
          (efrit-package-review-model)
          (length (plist-get info :files))
          (if (plist-get info :diff) ", diff against installed" "")
          (if (plist-get info :news) ", changelog" "")
          (let ((reads (delete-dups (copy-sequence (plist-get verdict :reads)))))
            (if reads (mapconcat #'identity reads ", ") "nothing"))))

(defun efrit-package-review--location (finding)
  "\"file.el:12\" for FINDING, or nil when it names no file."
  (when-let* ((file (plist-get finding :file)))
    (if (plist-get finding :line)
        (format "%s:%s" file (plist-get finding :line))
      file)))

(defun efrit-package-review-report (info verdict)
  "The report for INFO and VERDICT as plain text: logs, tests, the kill ring.
`efrit-package-review-render' is the same report with faces and
badges, inserted into a buffer."
  (with-temp-buffer
    (efrit-package-review-render info verdict t)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun efrit-package-review--fill-paragraphs (start end fill-width)
  "Fill each paragraph between START and END to FILL-WIDTH.
Indentation of the first line is kept for every line of the paragraph."
  ;; END is a marker: filling changes the text length, and an integer
  ;; end past `point-max' made the loop below spin forever (the first
  ;; live report with a long note hung Emacs).
  (let ((fill-column fill-width) (adaptive-fill-mode nil)
        (end (copy-marker end t)))
    (save-excursion
      (goto-char start)
      (while (< (point) end)
        (skip-chars-forward " \t\n" end)
        (when (< (point) end)
          (let* ((indent (current-indentation))
                 (fill-prefix (make-string indent ?\s))
                 (para-start (line-beginning-position))
                 (para-end (save-excursion (forward-paragraph) (min (point) end))))
            (fill-region-as-paragraph para-start para-end nil t)
            ;; always move: a paragraph that did not advance point ends
            ;; the pass rather than the session
            (let ((next (save-excursion (forward-paragraph) (point))))
              (goto-char (if (> next (point)) (min next end) end)))))))
    (set-marker end nil)))

(defun efrit-package-review--visit (info location)
  "Open LOCATION (\"file[:line]\") of the package described by INFO."
  (let* ((parts (split-string location ":"))
         (file (expand-file-name (car parts) (plist-get info :dir)))
         (line (and (cadr parts) (string-to-number (cadr parts)))))
    (if (not (file-exists-p file))
        (message "%s is not there any more (the package directory was %s)" (car parts) (plist-get info :dir))
      (pop-to-buffer (find-file-noselect file))
      (when (and line (> line 0))
        (goto-char (point-min)) (forward-line (1- line))))))

(defun efrit-package-review-render (info verdict &optional plain)
  "Insert the report for INFO and VERDICT at point.
Badges and buttons are used when the display can show them; PLAIN
forces text only (the same words, no images or faces).  The prose is
filled to `efrit-package-review-fill-column' or the window, whichever
is narrower."
  (let* ((badge (if plain
                    (lambda (text _face) text)
                  #'efrit-ui-badge))
         (width (min efrit-package-review-fill-column
                     (max 40 (- (window-body-width) 2))))
         (start (point)))
    ;; title line
    (insert (funcall badge (efrit-package-review--verdict-word verdict)
                     (efrit-package-review--verdict-face verdict))
            "  "
            (propertize (format "%s %s" (plist-get info :name) (plist-get info :version))
                        'face (unless plain 'efrit-package-review-title))
            (if (plist-get info :old-version)
                (propertize (format "  (from %s)" (plist-get info :old-version))
                            'face (unless plain 'efrit-package-review-meta))
              "")
            "\n")
    (insert (propertize
             (concat (efrit-package-review--counts-text verdict)
                     (if (or (plist-get verdict :saw-everything)
                             (eq (plist-get verdict :verdict) 'error))
                         ""
                       " · the reviewer did not see everything"))
             'face (unless plain 'efrit-package-review-meta))
            "\n\n")
    ;; summary, filled
    (let ((from (point)))
      (insert (string-trim (or (plist-get verdict :summary) "")) "\n")
      (efrit-package-review--fill-paragraphs from (point) width))
    ;; findings: badge, location button, then the note as an indented paragraph
    (when-let* ((findings (efrit-package-review--sorted-findings verdict)))
      (dolist (f findings)
        (insert "\n")
        (let ((sev (plist-get f :severity))
              (location (efrit-package-review--location f)))
          (insert (funcall badge (upcase (symbol-name sev))
                           (efrit-package-review--severity-face sev)))
          (when location
            (insert " ")
            (if plain
                (insert location)
              (insert-text-button location
                                  'action (lambda (_b) (efrit-package-review--visit info location))
                                  'follow-link t
                                  'help-echo "Open this file at the line the reviewer named")))
          (insert "\n")
          (let ((from (point)))
            (insert "    " (string-trim (plist-get f :note)) "\n")
            (efrit-package-review--fill-paragraphs from (point) width)
            (unless plain
              (add-face-text-property from (point) 'efrit-package-review-note))))))
    ;; a non-verdict answer, verbatim
    (when-let* ((raw (plist-get verdict :raw)))
      (insert "\nThe reviewer's answer, verbatim:\n\n")
      (let ((from (point)))
        (insert (mapconcat (lambda (l) (concat "  | " l)) (split-string raw "\n") "\n") "\n")
        (unless plain (add-face-text-property from (point) 'font-lock-comment-face))))
    ;; footer
    (let ((from (point)))
      (insert "\n" (efrit-package-review--footer info verdict) "\n")
      (efrit-package-review--fill-paragraphs from (point) width)
      (unless plain (add-face-text-property from (point) 'efrit-package-review-meta)))
    (buffer-substring start (point))))

(defun efrit-package-review-show (info verdict &optional select)
  "Show the report in a popup; return its plain text.
With SELECT the popup takes focus, so `q' closes it at once; without,
it stays beside the prompt that is still reading keys (install time)."
  (funcall (if select #'efrit-show-popup #'efrit-show-preview)
           efrit-package-review--buffer
           (lambda () (efrit-package-review-render info verdict)))
  (efrit-package-review-report info verdict))

;;; Install-time hook (Emacs 31's package-review)

(defun efrit-package-review--around (orig pkg-desc pkg-dir old-desc)
  "Review PKG-DESC in PKG-DIR against OLD-DESC before ORIG asks the user."
  (let* ((info (efrit-package-review-gather pkg-desc pkg-dir old-desc))
         (_ (efrit-log 'info "package review %s %s: %d file(s) listed%s%s%s"
                       (plist-get info :name) (plist-get info :version)
                       (length (plist-get info :files))
                       (if (plist-get info :diff) (format ", diff %d chars" (length (plist-get info :diff))) "")
                       (if (plist-get info :news) ", changelog" "")
                       (if (plist-get info :cut) ", CUT" "")))
         (verdict (efrit-package-review-run info)))
    (when (plist-get verdict :raw)
      (efrit-log 'warn "package review %s: not a verdict: %s" (plist-get info :name)
                 (truncate-string-to-width (plist-get verdict :raw) 600 nil nil "…")))
    (efrit-log 'info "package review %s %s: %s" (plist-get info :name)
               (plist-get info :version) (efrit-package-review-verdict-line verdict))
    (if (and (eq efrit-package-review-action 'auto-approve-clean)
             (efrit-package-review-clean-p verdict))
        (progn
          (message "%s: %s (installed without asking; efrit-package-review-action)"
                   (plist-get info :name) (efrit-package-review-verdict-line verdict))
          nil)
      (efrit-package-review-show info verdict)
      ;; package.el's prompt is read-multiple-choice; prefix its question
      ;; with the verdict so the answer is made with it in view
      (cl-letf* ((rmc (symbol-function 'read-multiple-choice))
                 ((symbol-function 'read-multiple-choice)
                  (lambda (prompt choices &rest rest)
                    (apply rmc (concat (efrit-package-review-verdict-line verdict) "\n" prompt)
                           choices rest))))
        (funcall orig pkg-desc pkg-dir old-desc)))))

;;;###autoload
(define-minor-mode efrit-package-review-mode
  "Have efrit review packages before Emacs 31's `package-review' asks.
Needs `package-review-policy' set to something other than nil; the
mode turns it on (t) when it is nil and says so."
  :global t
  :group 'efrit-package-review
  (cond
   ((not (fboundp 'package-review))
    (setq efrit-package-review-mode nil)
    (user-error "package-review needs Emacs 31; use M-x efrit-review-package instead"))
   (efrit-package-review-mode
    (advice-add 'package-review :around #'efrit-package-review--around)
    (when (and (boundp 'package-review-policy) (null package-review-policy))
      (setq package-review-policy t)
      (message "efrit: package-review-policy set to t so reviews happen")))
   (t (advice-remove 'package-review #'efrit-package-review--around))))

;;; Finding out what the API refuses
;;
;; A refusal (stop_reason "refusal", no output) is decided before the
;; model runs and gives no reason.  When a review is refused, the
;; probe sends the same package in shrinking variants and logs which
;; pass, so the trigger is measured instead of guessed.  The msg-*
;; variants bisect the user message itself, section by section.

(defconst efrit-package-review--probe-variants
  '((full          "the review request as sent")
    (no-tool       "same, without the read tool")
    (no-rubric     "same message, system prompt is one neutral sentence")
    (system-hello  "system prompt is one neutral sentence, user says hello")
    (inline-system "no system field; the rubric is the first paragraph of the user message")
    (plain         "no system prompt, one sentence asking to summarise the diff")
    (tiny          "no system prompt, the first 2000 chars of the diff, summarise")
    (msg-meta      "summarise: metadata lines only")
    (msg-files     "summarise: metadata + file list")
    (msg-news      "summarise: metadata + file list + changelog")
    (msg-diff      "summarise: the whole user message"))
  "Probe variants, most like the real request first.")

(defun efrit-package-review--message-part (info upto)
  "The user message for INFO built only UPTO a section: meta, files, news, diff."
  (let ((full (efrit-package-review--user-message info)))
    (pcase upto
      ('meta (car (split-string full "\n=== FILES")))
      ('files (car (split-string full "\n=== CHANGELOG\\|\n=== DIFF\\|\n(New install")))
      ('news (car (split-string full "\n=== DIFF\\|\n(New install")))
      (_ full))))

(defun efrit-package-review--probe-request (variant info)
  "The request for probe VARIANT of INFO."
  (let* ((msg (efrit-package-review--user-message info))
         (diff (or (plist-get info :diff) ""))
         (base `(("model" . ,(efrit-package-review-model)) ("max_tokens" . 300)))
         (summarise (lambda (text)
                      (append base
                              `(("messages" . [(("role" . "user")
                                                ("content" . ,(concat "Summarise the following in two sentences.\n\n" text)))]))))))
    (pcase variant
      ('full (efrit-package-review--request
              (list `((role . "user") (content . ,msg)))))
      ('no-tool (append base
                        `(("system" . ,efrit-package-review--system-prompt)
                          ("messages" . [(("role" . "user") ("content" . ,msg))]))))
      ('no-rubric (append base
                          `(("system" . "You help an Emacs user read a package update before installing it.")
                            ("messages" . [(("role" . "user") ("content" . ,msg))]))))
      ('system-hello (append base
                             `(("system" . "You are a helpful assistant.")
                               ("messages" . [(("role" . "user") ("content" . "Say hello."))]))))
      ('inline-system (append base
                              `(("messages" . [(("role" . "user")
                                                ("content" . ,(concat efrit-package-review--system-prompt
                                                                      "\n\n---\n\n" msg)))]))))
      ('plain (funcall summarise diff))
      ('tiny (funcall summarise (substring diff 0 (min 2000 (length diff)))))
      ('msg-meta (funcall summarise (efrit-package-review--message-part info 'meta)))
      ('msg-files (funcall summarise (efrit-package-review--message-part info 'files)))
      ('msg-news (funcall summarise (efrit-package-review--message-part info 'news)))
      ('msg-diff (funcall summarise msg)))))

;;;###autoload
(defun efrit-package-review-probe (name)
  "Send shrinking variants of the review request for installed package NAME.
Reports, per variant, whether the API answered or refused, in a
buffer and the log, and names the message section whose addition
first drew a refusal.  For working out what a refusal reacts to."
  (interactive
   (list (intern (completing-read "Probe package: "
                                  (mapcar (lambda (p) (symbol-name (car p))) package-alist) nil t))))
  (let* ((desc (or (cadr (assq name package-alist)) (user-error "%s is not installed" name)))
         (info (efrit-package-review-gather desc (package-desc-dir desc) nil))
         (efrit-api-inline-system-on-refusal nil)
         (lines nil) (first-refused-stage nil))
    (dolist (v efrit-package-review--probe-variants)
      (let* ((efrit-api-request-purpose (format "probe %s for %s" (car v) name))
             (outcome
              (condition-case err
                  (let* ((r (efrit-api-request-sync (efrit-package-review--probe-request (car v) info)
                                                    efrit-package-review-timeout))
                         (stop (efrit-response-stop-reason r))
                         (u (efrit-response-usage r)))
                    (format "%s (in=%s out=%s)" (or stop "?")
                            (and u (gethash "input_tokens" u)) (and u (gethash "output_tokens" u))))
                (error (format "error: %s" (error-message-string err))))))
        (when (and (not first-refused-stage)
                   (memq (car v) '(msg-meta msg-files msg-news msg-diff))
                   (string-prefix-p "refusal" outcome))
          (setq first-refused-stage (car v)))
        (efrit-log 'info "probe %s %s: %s" name (car v) outcome)
        (push (format "  %-14s %-52s %s" (car v) (cadr v) outcome) lines)))
    (setq lines (nreverse lines))
    (let ((culprit
           (pcase first-refused-stage
             ('msg-meta (efrit-package-review--message-part info 'meta))
             ('msg-files (string-remove-prefix (efrit-package-review--message-part info 'meta)
                                               (efrit-package-review--message-part info 'files)))
             ('msg-news (string-remove-prefix (efrit-package-review--message-part info 'files)
                                              (efrit-package-review--message-part info 'news)))
             ('msg-diff (string-remove-prefix (efrit-package-review--message-part info 'news)
                                              (efrit-package-review--user-message info))))))
      (efrit-show-popup "*efrit-package-review-probe*"
                          (concat (format "Refusal probe for %s with %s\n\n" name (efrit-package-review-model))
                                  (mapconcat #'identity lines "\n")
                                  "\n\nThe first variant that is not refused names the trigger: what the variant above it still had."
                                  (if culprit
                                      (format "\n\nFirst refused message stage: %s.  The text that stage added:\n\n%s"
                                              first-refused-stage culprit)
                                    "\n\nNo message stage was refused."))))
    lines))

;;; On demand

;;;###autoload
(defun efrit-review-package (name)
  "Review the installed package NAME with efrit and show the report.
With an archive copy available that is newer, the installed one is
still what is reviewed: this command judges what is on disk."
  (interactive
   (list (intern (completing-read "Review package: "
                                  (mapcar (lambda (p) (symbol-name (car p))) package-alist)
                                  nil t))))
  (let* ((desc (or (cadr (assq name package-alist))
                   (user-error "%s is not installed" name)))
         (dir (package-desc-dir desc))
         (info (efrit-package-review-gather desc dir nil))
         (verdict (efrit-package-review-run info)))
    (efrit-package-review-show info verdict t)
    (message "%s" (efrit-package-review-verdict-line verdict))
    verdict))

(provide 'efrit-package-review)

;;; efrit-package-review.el ends here
