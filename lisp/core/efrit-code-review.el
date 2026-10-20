;;; efrit-code-review.el --- Local code review of a change set, findings as patches -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.12.12
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, vc, ai

;;; Commentary:

;; A review of what is about to be committed (or pushed, or merged),
;; done locally, before a person sees it.  The model reads the diff of
;; a SCOPE (the staged changes, the commits not yet pushed, or the
;; branch against its base), may read files at the scope's revision
;; and search the tree, and ends with one `submit_review' call: a list
;; of FINDINGS.  A finding is a comment on a place, a suggestion with
;; an exact replacement, or an all-clear for a file.
;;
;; Suggestions arrive as SEARCH/REPLACE text (`old_lines'/`new_lines'),
;; never as a diff the model authored: models copy the text they read
;; far more reliably than they count lines (after magit-hutch, Akshay
;; Gupta, 2026-10-05; tzz, 2026-10-09: "study this article and see how
;; much of that we can use in efrit").  The GATES then check every
;; finding against the real tree: a file that does not exist drops
;; the finding; an `old_lines' that is missing or ambiguous turns the
;; suggestion into a comment; a unique match becomes a unified diff
;; through `efrit-vcs-diff-strings'.  So what the user queues and
;; applies is always a patch efrit made from the file it has.
;;
;; `efrit-code-review-run' is the driver: a side conversation like the
;; package review's, with tools, outside any agent session.  The
;; buffer that shows the findings and applies the queued ones is
;; `efrit-code-review-ui'.  A finished review is saved under the
;; scope's diff hash in `efrit-data-directory', so the same change set
;; opens again without another request.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)
(require 'efrit-config)
(require 'efrit-log)
(require 'efrit-api)
(require 'efrit-chat-response)
(require 'efrit-vcs)

(declare-function efrit-tool-search-content "efrit-tool-search-content")
(declare-function efrit-tool-surrounding-context "efrit-tool-navigate")
(declare-function efrit-sandbox-canonical "efrit-sandbox")
(declare-function vc-git--run-command-string "vc-git")
(declare-function vc-git-command "vc-git")
(declare-function vc-git-mergebase "vc-git")

(defgroup efrit-code-review nil
  "Local code review of staged, unpushed or branch changes."
  :group 'efrit)

(defcustom efrit-code-review-model nil
  "Model that reviews the change set, or nil for `efrit-default-model'."
  :type '(choice (const nil) string)
  :group 'efrit-code-review)

(defcustom efrit-code-review-max-rounds 40
  "Most tool rounds one review may take before it must submit."
  :type 'integer
  :group 'efrit-code-review)

(defcustom efrit-code-review-soft-round-ratio 0.65
  "Fraction of `efrit-code-review-max-rounds' the prompt names as the soft target."
  :type 'float
  :group 'efrit-code-review)

(defcustom efrit-code-review-timeout 180
  "Seconds one request of the review may take."
  :type 'integer
  :group 'efrit-code-review)

(defcustom efrit-code-review-max-diff-chars 60000
  "Longest per-file diff returned to the model in full."
  :type 'integer
  :group 'efrit-code-review)

(defcustom efrit-code-review-max-title-chars 80
  "A finding's title is cut to this."
  :type 'integer
  :group 'efrit-code-review)

(defcustom efrit-code-review-max-description-chars 1000
  "A finding's description is cut to this."
  :type 'integer
  :group 'efrit-code-review)

(defcustom efrit-code-review-default-base "main"
  "Branch the `branch' scope compares against when no upstream is known."
  :type 'string
  :group 'efrit-code-review)

(defcustom efrit-code-review-persist t
  "When non-nil, finished reviews are saved under their scope hash and reopened."
  :type 'boolean
  :group 'efrit-code-review)

(defconst efrit-code-review-kinds '(staged unpushed branch)
  "The change sets a review may cover.")

(defconst efrit-code-review-finding-types '(comment suggestion lgtm))

;; pending -> queued | dismissed ; queued -> pending | applied | invalid
(defconst efrit-code-review-finding-states '(pending queued applied dismissed invalid))

(defconst efrit-code-review--transitions
  '((pending . (queued dismissed))
    (queued . (pending applied invalid))
    (applied . ())
    (invalid . ())
    (dismissed . ())))

(define-error 'efrit-code-review-error "Code review error")

;;;; Scope

(cl-defstruct (efrit-code-review-scope (:constructor efrit-code-review-scope-create)
                                       (:copier nil))
  "One change set: KIND, the ROOT it is in, and the revisions it spans.
For `staged' BASE and HEAD are nil and the diff is index against HEAD.
For `unpushed' and `branch' BASE is the merge base and HEAD is the
tip.  HASH identifies the content reviewed; FILES is the list of
\(PATH ADDED DELETED)."
  kind root base head hash files)

(defun efrit-code-review--git (root &rest args)
  "The output of git ARGS in ROOT through VC, as a trimmed string, or nil."
  (require 'vc-git)
  (let ((default-directory root))
    (when-let* ((out (apply #'vc-git--run-command-string nil args)))
      (string-trim-right out))))

(defun efrit-code-review--numstat (root base head)
  "The (PATH ADDED DELETED) list of the diff BASE..HEAD (nil nil: staged)."
  (let* ((out (if head
                  (efrit-code-review--git root "diff" "--numstat" (format "%s..%s" base head))
                (efrit-code-review--git root "diff" "--cached" "--numstat")))
         (files nil))
    (dolist (line (split-string (or out "") "\n" t))
      (pcase (split-string line "\t")
        (`(,a ,d ,path)
         (push (list path (string-to-number a) (string-to-number d)) files))))
    (nreverse files)))

(defun efrit-code-review--hash (root base head)
  "A hash of the diff's content for BASE..HEAD (staged when HEAD is nil)."
  (let ((diff (if head
                  (efrit-code-review--git root "diff" (format "%s..%s" base head))
                (efrit-code-review--git root "diff" "--cached"))))
    (secure-hash 'sha256 (or diff ""))))

(defun efrit-code-review--base-for (root kind)
  "The (BASE . HEAD) revisions of KIND in ROOT, or (nil . nil) for staged."
  (pcase kind
    ('staged (cons nil nil))
    ('unpushed
     (let ((upstream (or (efrit-vcs-upstream root)
                         (signal 'efrit-code-review-error
                                 (list "the current branch has no upstream to compare against")))))
       (cons (efrit-code-review--git root "merge-base" upstream "HEAD")
             (efrit-code-review--git root "rev-parse" "HEAD"))))
    ('branch
     (let ((base-branch (if (efrit-code-review--git root "rev-parse" "--verify" "--quiet" efrit-code-review-default-base)
                            efrit-code-review-default-base
                          (signal 'efrit-code-review-error
                                  (list (format "no branch %s to compare against (efrit-code-review-default-base)"
                                                efrit-code-review-default-base))))))
       (cons (efrit-code-review--git root "merge-base" base-branch "HEAD")
             (efrit-code-review--git root "rev-parse" "HEAD"))))
    (_ (signal 'efrit-code-review-error (list (format "unknown scope kind %s" kind))))))

(defun efrit-code-review-scope (kind &optional dir)
  "The scope of KIND (one of `efrit-code-review-kinds') for DIR's repository.
Signals `efrit-code-review-error' when the tree is not Git or the
scope has no changes."
  (pcase-let* ((`(,backend . ,root) (efrit-vcs-require dir)))
    (unless (eq backend 'Git)
      (signal 'efrit-code-review-error (list "code review needs a Git tree")))
    (pcase-let* ((`(,base . ,head) (efrit-code-review--base-for root kind))
                 (files (efrit-code-review--numstat root base head)))
      (unless files
        (signal 'efrit-code-review-error
                (list (pcase kind
                        ('staged "nothing is staged")
                        ('unpushed "nothing to push")
                        (_ (format "no difference from %s" efrit-code-review-default-base))))))
      (efrit-code-review-scope-create
       :kind kind :root root :base base :head head
       :hash (efrit-code-review--hash root base head)
       :files files))))

(defun efrit-code-review-scope-current-p (scope)
  "Non-nil when SCOPE's change set is still what it was when made."
  (equal (efrit-code-review-scope-hash scope)
         (efrit-code-review--hash (efrit-code-review-scope-root scope)
                                  (efrit-code-review-scope-base scope)
                                  (efrit-code-review-scope-head scope))))

(defun efrit-code-review-scope-label (scope)
  "One line naming SCOPE for the user."
  (format "%s in %s (%d file%s)"
          (pcase (efrit-code-review-scope-kind scope)
            ('staged "staged changes")
            ('unpushed "unpushed commits")
            (_ (format "branch against %s" efrit-code-review-default-base)))
          (abbreviate-file-name (efrit-code-review-scope-root scope))
          (length (efrit-code-review-scope-files scope))
          (if (= 1 (length (efrit-code-review-scope-files scope))) "" "s")))

(defun efrit-code-review--safe-relative (scope path)
  "PATH as a path relative to SCOPE's root, or nil when it leaves the root."
  (let* ((root (efrit-code-review-scope-root scope))
         (full (expand-file-name path root))
         (rel (file-relative-name full root)))
    (and (not (string-prefix-p ".." rel)) (not (file-name-absolute-p rel)) rel)))

(defun efrit-code-review-file-at-scope (scope path)
  "The text of PATH at SCOPE's reviewed revision, or nil.
Staged: the index copy.  Unpushed and branch: at the tip commit."
  (when-let* ((rel (efrit-code-review--safe-relative scope path)))
    (let ((root (efrit-code-review-scope-root scope))
          (head (efrit-code-review-scope-head scope)))
      (require 'vc-git)
      (let ((default-directory root))
        (with-temp-buffer
          (let ((status (condition-case nil
                            (vc-git-command (current-buffer) 1 nil "show"
                                            (format "%s:%s" (or head "") rel))
                          (error 1))))
            (and (eql status 0) (buffer-string))))))))

(defun efrit-code-review-diff-for (scope &optional path)
  "The diff of SCOPE, for PATH alone when given, as a string."
  (let* ((root (efrit-code-review-scope-root scope))
         (base (efrit-code-review-scope-base scope))
         (head (efrit-code-review-scope-head scope))
         (rel (and path (efrit-code-review--safe-relative scope path)))
         (args (append (if head (list "diff" (format "%s..%s" base head)) (list "diff" "--cached"))
                       (and rel (list "--" rel)))))
    (or (apply #'efrit-code-review--git root args) "")))

;;;; Findings

(defvar efrit-code-review--finding-counter 0)

(defun efrit-code-review-finding (type file &rest kv)
  "A finding plist of TYPE on FILE with the keys in KV.
Keys: :lines (string), :title, :description, :patch, :old-lines,
:new-lines, :state (default `pending' for a suggestion, `applied' for
the rest: a comment has nothing to apply)."
  (unless (memq type efrit-code-review-finding-types)
    (error "efrit-code-review: not a finding type: %S" type))
  (let ((f (append (list :id (format "f%d" (cl-incf efrit-code-review--finding-counter))
                         :type type :file file)
                   kv)))
    (unless (plist-member f :state)
      (setq f (plist-put f :state (if (eq type 'suggestion) 'pending 'applied))))
    f))

(defun efrit-code-review-finding-transition (finding state)
  "FINDING moved to STATE when `efrit-code-review--transitions' allows it.
Returns the finding, changed or not."
  (unless (memq state efrit-code-review-finding-states)
    (error "efrit-code-review: not a finding state: %S" state))
  (when (memq state (alist-get (plist-get finding :state) efrit-code-review--transitions))
    (plist-put finding :state state))
  finding)

(defun efrit-code-review--cut (text max)
  (let ((text (if (stringp text) text "")))
    (if (> (length text) max) (concat (substring text 0 (1- max)) "…") text)))

(defun efrit-code-review-normalize (raw)
  "The finding plist for RAW, one object of the model's findings array.
RAW is a hash table.  Shape only; the gates do the checking."
  (let* ((get (lambda (k) (let ((v (gethash k raw))) (and (not (eq v :null)) v))))
         (file (or (funcall get "file") "unknown"))
         (old (funcall get "old_lines"))
         (new (funcall get "new_lines"))
         (lines (funcall get "lines"))
         (title (efrit-code-review--cut (or (funcall get "title") "Issue") efrit-code-review-max-title-chars))
         (desc (efrit-code-review--cut (or (funcall get "description") "") efrit-code-review-max-description-chars)))
    (cond
     ((eq (funcall get "lgtm") t)
      (efrit-code-review-finding 'lgtm file))
     ((and (stringp old) (stringp new) (not (string-empty-p old)))
      (efrit-code-review-finding 'suggestion file :lines "?" :title title :description desc
                                 :old-lines old :new-lines new))
     (t
      (efrit-code-review-finding 'comment file
                                 :lines (if (stringp lines) lines (if lines (format "%s" lines) "?"))
                                 :title title :description desc)))))

;;;; Gates

(defun efrit-code-review-occurrences (haystack needle)
  "The 1-based line numbers where NEEDLE starts in HAYSTACK.
Nil for an empty NEEDLE."
  (unless (or (null needle) (string-empty-p needle))
    (let ((pos 0) (lines nil))
      (while (setq pos (string-search needle haystack pos))
        (push (1+ (cl-count ?\n haystack :end pos)) lines)
        (setq pos (1+ pos)))
      (nreverse lines))))

(defun efrit-code-review--gate-file (scope findings)
  "FINDINGS without those on a file SCOPE does not have."
  (let ((kept (seq-filter
               (lambda (f)
                 (or (efrit-code-review-file-at-scope scope (plist-get f :file))
                     (progn (efrit-log 'debug "code review: dropped finding on missing file %s"
                                       (plist-get f :file))
                            nil)))
               findings)))
    (efrit-log 'debug "code review: file gate %d -> %d" (length findings) (length kept))
    kept))

(defun efrit-code-review--downgrade (f why)
  "F as a comment, its suggestion dropped for WHY."
  (efrit-log 'debug "code review: %s: %s, suggestion downgraded" (plist-get f :file) why)
  (efrit-code-review-finding 'comment (plist-get f :file)
                             :lines "?" :title (plist-get f :title)
                             :description (plist-get f :description)
                             :downgraded why))

(defun efrit-code-review--gate-patch (scope findings)
  "FINDINGS with every suggestion resolved against SCOPE's files.
A unique match of :old-lines sets :patch (a unified diff) and
:lines; a missing or repeated match makes the finding a comment."
  (mapcar
   (lambda (f)
     (if (not (eq (plist-get f :type) 'suggestion))
         f
       (let* ((file (plist-get f :file))
              (old (plist-get f :old-lines))
              (new (plist-get f :new-lines))
              (source (efrit-code-review-file-at-scope scope file))
              (hits (and source (efrit-code-review-occurrences source old))))
         (cond
          ((null source) (efrit-code-review--downgrade f "file missing at the reviewed revision"))
          ((null hits) (efrit-code-review--downgrade f "old_lines not found"))
          ((cdr hits) (efrit-code-review--downgrade f (format "old_lines matches %d times" (length hits))))
          (t
           (let* ((start (car hits))
                  (end (+ start (1- (length (split-string old "\n"))))))
             (plist-put f :patch (efrit-vcs-diff-strings source (string-replace old new source)
                                                         (concat "a/" file) (concat "b/" file)))
             (plist-put f :lines (if (= start end) (number-to-string start) (format "%d-%d" start end)))
             f))))))
   findings))

(defun efrit-code-review-gate (scope findings)
  "FINDINGS after every gate for SCOPE."
  (efrit-code-review--gate-patch scope (efrit-code-review--gate-file scope findings)))

;;;; Applying

(defun efrit-code-review-parse-lines (lines)
  "The first line number in LINES (\"12\", \"12-20\", \"12, 40\"), or 0."
  (if (and (stringp lines) (string-match "[0-9]+" lines))
      (string-to-number (match-string 0 lines))
    0))

(defun efrit-code-review-apply-order (findings)
  "The queued FINDINGS, grouped by file, bottom-most first within a file.
Applying from the bottom keeps the earlier matches' positions intact."
  (sort (seq-filter (lambda (f) (eq (plist-get f :state) 'queued)) findings)
        (lambda (a b)
          (let ((fa (plist-get a :file)) (fb (plist-get b :file)))
            (if (string= fa fb)
                (> (efrit-code-review-parse-lines (plist-get a :lines))
                   (efrit-code-review-parse-lines (plist-get b :lines)))
              (string< fa fb))))))

(defun efrit-code-review--apply-one (scope f)
  "Apply suggestion F to its file in SCOPE's work tree.
The replacement is done by text in the file as it is now, not by the
patch's line numbers: a unique `old_lines' is replaced, a missing or
repeated one fails.  A buffer visiting the file is edited and saved so
the user sees the change.  Returns (OK . NOTE)."
  (let* ((root (efrit-code-review-scope-root scope))
         (file (expand-file-name (plist-get f :file) root))
         (old (plist-get f :old-lines))
         (new (plist-get f :new-lines))
         (buf (find-buffer-visiting file)))
    (cond
     ((not (file-exists-p file)) (cons nil "file does not exist in the work tree"))
     ((and buf (buffer-modified-p buf)) (cons nil "the file's buffer has unsaved changes"))
     (t
      (let* ((text (if buf (with-current-buffer buf (buffer-substring-no-properties (point-min) (point-max)))
                     (with-temp-buffer (insert-file-contents file) (buffer-string))))
             (hits (efrit-code-review-occurrences text old)))
        (cond
         ((null hits) (cons nil "old_lines no longer in the file"))
         ((cdr hits) (cons nil (format "old_lines now matches %d times" (length hits))))
         (t
          (if buf
              (with-current-buffer buf
                (save-excursion
                  (goto-char (point-min))
                  (search-forward old)
                  (replace-match new t t))
                (save-buffer))
            (with-temp-file file (insert (string-replace old new text))))
          (cons t (format "line %d" (car hits))))))))))

(defun efrit-code-review-apply-queued (scope findings)
  "Apply every queued suggestion in FINDINGS to SCOPE's work tree.
Each moves to `applied' or `invalid' (with :error); one failure does
not stop the rest.  Returns the list of (FINDING . NOTE) attempted."
  (let ((done nil))
    (dolist (f (efrit-code-review-apply-order findings))
      (pcase-let ((`(,ok . ,note) (efrit-code-review--apply-one scope f)))
        (efrit-code-review-finding-transition f (if ok 'applied 'invalid))
        (unless ok (plist-put f :error note))
        (push (cons f note) done)))
    (nreverse done)))

;;;; The review in flight

(cl-defstruct (efrit-code-review-state (:constructor efrit-code-review-state-create)
                                       (:copier nil))
  "One review in flight: the SCOPE, the MESSAGES so far, the ROUNDS used,
the raw SUBMITTED findings once submit_review ran, and the tool CALLS log
\((NAME . SECONDS) ...)."
  scope messages (rounds 0) submitted (calls nil) cancelled)

;;;; Tools

(defconst efrit-code-review--tools
  `((("name" . "read_diff")
     ("description" . "The diff of one file in the change set under review (or of the whole set without a path).  Read a file's diff before saying anything about it.")
     ("input_schema" . (("type" . "object")
                        ("properties" . (("path" . (("type" . "string") ("description" . "Path from the file list, relative to the repository root")))))
                        ("required" . []))))
    (("name" . "read_file")
     ("description" . "The text of a file at the revision under review (the index for staged changes, the tip commit otherwise), whole or a line range.  Use it when the diff alone does not show what the changed code does.")
     ("input_schema" . (("type" . "object")
                        ("properties" . (("path" . (("type" . "string")))
                                         ("start_line" . (("type" . "integer") ("description" . "1-based (default 1)")))
                                         ("end_line" . (("type" . "integer") ("description" . "inclusive (default: end)")))))
                        ("required" . ["path"]))))
    (("name" . "surrounding_context")
     ("description" . "The definition(s) enclosing a line of a file, from the syntax tree: the function, and with depth 2 its class or module too.  Cheaper than read_file for 'what is around this change'.")
     ("input_schema" . (("type" . "object")
                        ("properties" . (("path" . (("type" . "string")))
                                         ("line" . (("type" . "integer") ("description" . "1-based")))
                                         ("depth" . (("type" . "integer") ("description" . "How many enclosing definitions (default 1, at most 3)")))))
                        ("required" . ["path" "line"]))))
    (("name" . "search_codebase")
     ("description" . "Search the repository's files for a pattern (callers, references, other uses of a name).  Returns matching lines with file and line number.")
     ("input_schema" . (("type" . "object")
                        ("properties" . (("pattern" . (("type" . "string")))
                                         ("is_regex" . (("type" . "boolean")))
                                         ("file_pattern" . (("type" . "string") ("description" . "Glob such as *.el")))))
                        ("required" . ["pattern"]))))
    (("name" . "verify_block")
     ("description" . "Check that old_lines occurs exactly once in a file at the reviewed revision.  Answers OK with the line, NOT_FOUND, or AMBIGUOUS with the count.  Call it before putting old_lines in a suggestion.")
     ("input_schema" . (("type" . "object")
                        ("properties" . (("file" . (("type" . "string")))
                                         ("old_lines" . (("type" . "string") ("description" . "Exact text copied from the file")))))
                        ("required" . ["file" "old_lines"]))))
    (("name" . "submit_review")
     ("description" . "Submit the findings.  Call it exactly once, at the end.  A suggestion carries old_lines (exact, unique text from the file) and new_lines (its replacement); a comment carries lines instead; a clean file is {file, lgtm: true}.")
     ("input_schema" . (("type" . "object")
                        ("properties" . (("findings" . (("type" . "array")
                                                        ("items" . (("type" . "object")
                                                                    ("properties" . (("file" . (("type" . "string")))
                                                                                     ("title" . (("type" . "string") ("description" . "At most 80 characters")))
                                                                                     ("description" . (("type" . "string") ("description" . "At most 1000 characters")))
                                                                                     ("old_lines" . (("type" . "string") ("description" . "Exact text to replace, unique in the file; omit for a comment")))
                                                                                     ("new_lines" . (("type" . "string") ("description" . "Replacement; omit for a comment")))
                                                                                     ("lines" . (("type" . "string") ("description" . "Where a comment points: \"42\" or \"42-50\"")))
                                                                                     ("lgtm" . (("type" . "boolean") ("description" . "true: the file is fine, nothing else needed")))))
                                                                    ("required" . ["file"])))))))
                        ("required" . ["findings"]))))))

(defun efrit-code-review--clip (text max)
  (if (> (length text) max)
      (concat (substring text 0 max) (format "\n[… %d more characters omitted]" (- (length text) max)))
    text))

(defun efrit-code-review--tool-read-diff (scope input)
  (let ((diff (efrit-code-review-diff-for scope (gethash "path" input))))
    (if (string-empty-p diff) "No diff for that path in this change set."
      (efrit-code-review--clip diff efrit-code-review-max-diff-chars))))

(defun efrit-code-review--tool-read-file (scope input)
  (let* ((path (gethash "path" input))
         (text (and (stringp path) (efrit-code-review-file-at-scope scope path))))
    (if (null text)
        (format "Error: no file %s at the reviewed revision" path)
      (let* ((lines (split-string text "\n"))
             (start (max 1 (or (gethash "start_line" input) 1)))
             (end (min (length lines) (or (gethash "end_line" input) (length lines)))))
        (efrit-code-review--clip (string-join (seq-subseq lines (1- start) end) "\n")
                                 efrit-code-review-max-diff-chars)))))

(defun efrit-code-review--tool-verify-block (scope input)
  (let* ((file (gethash "file" input))
         (old (gethash "old_lines" input))
         (text (and (stringp file) (efrit-code-review-file-at-scope scope file)))
         (hits (and text (stringp old) (efrit-code-review-occurrences text old))))
    (cond
     ((null text) (format "Error: no file %s at the reviewed revision" file))
     ((null hits) (format "NOT_FOUND in %s. Re-read the file and copy the text exactly, whitespace included." file))
     ((cdr hits) (format "AMBIGUOUS in %s: matches %d times (lines %s). Add surrounding lines." file (length hits)
                         (mapconcat #'number-to-string hits ", ")))
     (t (format "OK: unique at line %d in %s" (car hits) file)))))

(defun efrit-code-review--tool-search (scope input)
  (require 'efrit-tool-search-content)
  (let* ((root (efrit-code-review-scope-root scope))
         (result (efrit-tool-search-content
                  `((pattern . ,(gethash "pattern" input))
                    (is_regex . ,(gethash "is_regex" input))
                    (file_pattern . ,(gethash "file_pattern" input))
                    (path . ,root)
                    (context_lines . 0)
                    (max_results . 80))))
         (matches (append (alist-get 'matches (alist-get 'result result)) nil)))
    (cond
     ((not (eq (alist-get 'success result) t))
      (format "Error: %s" (or (alist-get 'message (alist-get 'error result)) "search failed")))
     ((null matches) "No matches.")
     (t (mapconcat (lambda (m)
                     (format "%s:%s: %s"
                             (file-relative-name (alist-get 'file m) root)
                             (alist-get 'line m)
                             (string-trim (or (alist-get 'text m) (alist-get 'content m) ""))))
                   matches "\n")))))

(defun efrit-code-review--tool-surrounding (scope input)
  (require 'efrit-tool-navigate)
  (let* ((path (gethash "path" input))
         (text (and (stringp path) (efrit-code-review-file-at-scope scope path))))
    (if (null text)
        (format "Error: no file %s at the reviewed revision" path)
      (let ((result (efrit-tool-surrounding-context
                     `((file . ,(expand-file-name path (efrit-code-review-scope-root scope)))
                       (text . ,text)
                       (line . ,(gethash "line" input))
                       (depth . ,(min 3 (or (gethash "depth" input) 1)))))))
        (if (eq (alist-get 'success result) t)
            (let ((defs (append (alist-get 'definitions (alist-get 'result result)) nil)))
              (if defs
                  (mapconcat (lambda (d) (format "Lines %s-%s:\n%s" (alist-get 'start_line d) (alist-get 'end_line d) (alist-get 'text d)))
                             defs "\n\n")
                "No enclosing definition found."))
          (format "Error: %s" (or (alist-get 'message (alist-get 'error result)) "no syntax tree")))))))

(defun efrit-code-review--run-tool (state use)
  "Run tool USE for STATE; returns the result text.  Submit sets the findings."
  (pcase-let ((`(,_id ,name ,input) use))
    (let ((scope (efrit-code-review-state-scope state)))
      (pcase name
        ("read_diff" (efrit-code-review--tool-read-diff scope input))
        ("read_file" (efrit-code-review--tool-read-file scope input))
        ("verify_block" (efrit-code-review--tool-verify-block scope input))
        ("search_codebase" (efrit-code-review--tool-search scope input))
        ("surrounding_context" (efrit-code-review--tool-surrounding scope input))
        ("submit_review"
         (let ((raw (append (gethash "findings" input) nil)))
           (setf (efrit-code-review-state-submitted state)
                 (mapcar #'efrit-code-review-normalize (seq-filter #'hash-table-p raw)))
           (format "Review of %d finding(s) received." (length raw))))
        (_ (format "Error: unknown tool %s" name))))))

;;;; The conversation

(defconst efrit-code-review--system-prompt
  "You review a change set for its author, before anyone else sees it.  Find the substantive problems: bugs, logic errors, unhandled cases, races, security holes, broken contracts with callers.  Style is not a finding unless it hides a bug.

Look at the change from at least three angles before you submit (correctness, error handling, edge cases, security, concurrency, API contracts, tests).  Aim for two to five findings on a substantive change.  For a trivial change (documentation, formatting, renames, dependency bumps) submit {file, lgtm: true} per file; do not invent findings.

Round budget: at most %d tool rounds; by round %d prefer comments over more reading.  Once you have read a file or diff you have it; do not read it again.

Suggestion or comment: a small, certain fix (typo, wrong operator, off-by-one, swapped argument, missing guard) MUST be a suggestion with old_lines and new_lines.  A judgement call (cross-file impact, several possible fixes, unknown caller expectations) is a comment with lines.

old_lines rules: exact text copied from the file, every space and tab; unique in the file (add a line above or below until it is); three to ten lines usually.  Call verify_block before you rely on a block.  If it says NOT_FOUND or AMBIGUOUS, fix the block or make the finding a comment.

End with exactly one submit_review call.  Never answer in prose."
  "System prompt; %d slots are the hard and soft round limits.")

(defun efrit-code-review--system ()
  (format efrit-code-review--system-prompt efrit-code-review-max-rounds
          (round (* efrit-code-review-soft-round-ratio efrit-code-review-max-rounds))))

(defun efrit-code-review--first-message (scope)
  (format "Review these %s, then call submit_review.\n\nChanged files (added/deleted lines):\n%s\n\nUse read_diff on each file worth looking at; skip pure noise (whitespace, comment-only)."
          (pcase (efrit-code-review-scope-kind scope)
            ('staged "staged changes")
            ('unpushed "unpushed commits")
            (_ "branch changes"))
          (mapconcat (lambda (f) (format "  %s  +%d/-%d" (nth 0 f) (nth 1 f) (nth 2 f)))
                     (efrit-code-review-scope-files scope) "\n")))

(defun efrit-code-review-model ()
  (or efrit-code-review-model efrit-default-model))

(defun efrit-code-review--request (state)
  `(("model" . ,(efrit-code-review-model))
    ("max_tokens" . 4000)
    ("system" . ,(efrit-api-cacheable-system (efrit-code-review--system)))
    ("tools" . ,(vconcat efrit-code-review--tools))
    ("messages" . ,(vconcat (efrit-code-review-state-messages state)))))

(defun efrit-code-review--answer-tools (state uses)
  "Run USES, append the assistant turn's results to STATE's messages."
  (let ((results nil))
    (dolist (use uses)
      (let* ((t0 (float-time))
             (out (condition-case err
                      (efrit-code-review--run-tool state use)
                    (error (format "Error: %s" (error-message-string err)))))
             (secs (- (float-time) t0)))
        (push (cons (nth 1 use) secs) (efrit-code-review-state-calls state))
        (efrit-log 'debug "code review: %s %.2fs -> %d chars" (nth 1 use) secs (length out))
        (push (efrit-api-build-tool-result (nth 0 use) out (string-prefix-p "Error" out)) results)))
    (setf (efrit-code-review-state-messages state)
          (append (efrit-code-review-state-messages state)
                  (list `((role . "user") (content . ,(vconcat (nreverse results)))))))))

(defun efrit-code-review--step (state response)
  "Advance STATE by RESPONSE: (done . RESULT) or (continue)."
  (cond
   ((and response (efrit-response-error response))
    (cons 'done (list :status 'error :message (efrit-error-message (efrit-response-error response)))))
   ((equal (efrit-response-stop-reason response) "refusal")
    (cons 'done (list :status 'error :message "the API refused the request (stop reason refusal)")))
   (t
    (let* ((content (efrit-response-content response))
           (uses (delq nil (mapcar #'efrit-content-item-as-tool-use (append content nil)))))
      (cl-incf (efrit-code-review-state-rounds state))
      (cond
       ((null uses)
        (cons 'done (list :status 'error
                          :message (format "the reviewer answered in prose instead of submit_review (round %d)"
                                           (efrit-code-review-state-rounds state)))))
       (t
        (setf (efrit-code-review-state-messages state)
              (append (efrit-code-review-state-messages state)
                      (list `((role . "assistant") (content . ,content)))))
        (efrit-code-review--answer-tools state uses)
        (cond
         ((efrit-code-review-state-submitted state)
          (cons 'done (list :status 'ok)))
         ((>= (efrit-code-review-state-rounds state) efrit-code-review-max-rounds)
          (cons 'done (list :status 'error
                            :message (format "no submit_review after %d rounds" efrit-code-review-max-rounds))))
         (t (list 'continue)))))))))

(defun efrit-code-review--finish (state result)
  "The review RESULT for STATE: gated findings attached, saved when ok."
  (let* ((scope (efrit-code-review-state-scope state))
         (findings (and (eq (plist-get result :status) 'ok)
                        (efrit-code-review-gate scope (efrit-code-review-state-submitted state))))
         (full (append result
                       (list :scope scope
                             :findings findings
                             :rounds (efrit-code-review-state-rounds state)
                             :calls (reverse (efrit-code-review-state-calls state))
                             :model (efrit-code-review-model)
                             :at (format-time-string "%FT%T%z")))))
    (when (and efrit-code-review-persist (eq (plist-get result :status) 'ok))
      (efrit-code-review-save full))
    full))

(defun efrit-code-review--purpose (scope)
  (format "code review of %s" (efrit-code-review-scope-label scope)))

(defun efrit-code-review-run (scope callback &optional progress)
  "Review SCOPE without blocking; CALLBACK gets the result plist.
The result has :status (`ok' or `error'), :findings (gated), :scope,
:rounds, :calls, :model, :at, and :message on error.  PROGRESS, when
given, is called with (ROUND TOOL-NAMES) after each round.  Returns
the state; `efrit-code-review-cancel' stops it after the request in
flight."
  (let ((state (efrit-code-review-state-create
                :scope scope
                :messages (list `((role . "user") (content . ,(efrit-code-review--first-message scope)))))))
    (efrit-code-review--send state callback progress)
    state))

(defun efrit-code-review-cancel (state)
  (setf (efrit-code-review-state-cancelled state) t))

(defun efrit-code-review--send (state callback progress)
  (let ((efrit-api-request-purpose (efrit-code-review--purpose (efrit-code-review-state-scope state))))
    (efrit-api-request-async
     (efrit-code-review--request state)
     (lambda (response)
       (let ((step (condition-case err
                       (efrit-code-review--step state response)
                     (error (cons 'done (list :status 'error :message (error-message-string err)))))))
         (when progress
           (funcall progress (efrit-code-review-state-rounds state)
                    (mapcar #'car (seq-take (efrit-code-review-state-calls state) 6))))
         (cond
          ((eq (car step) 'done) (funcall callback (efrit-code-review--finish state (cdr step))))
          ((efrit-code-review-state-cancelled state)
           (funcall callback (efrit-code-review--finish state (list :status 'error :cancelled t :message "cancelled"))))
          (t (efrit-code-review--send state callback progress)))))
     (lambda (message)
       (funcall callback (efrit-code-review--finish state (list :status 'error :message message)))))))

(defun efrit-code-review-run-sync (scope)
  "Review SCOPE, blocking: the result plist.  For tests and batch use."
  (let ((state (efrit-code-review-state-create
                :scope scope
                :messages (list `((role . "user") (content . ,(efrit-code-review--first-message scope))))))
        (result nil)
        (efrit-api-request-purpose (efrit-code-review--purpose scope)))
    (condition-case err
        (while (not result)
          (let ((step (efrit-code-review--step
                       state (efrit-api-request-sync (efrit-code-review--request state) efrit-code-review-timeout))))
            (when (eq (car step) 'done) (setq result (cdr step)))))
      (error (setq result (list :status 'error :message (error-message-string err)))))
    (efrit-code-review--finish state result)))

;;;; Saved reviews

;; Two stores, chosen per repository (tzz, 2026-10-11: "do the notes
;; storage.  Support JSON as well, the choice is per repo"):
;;
;; json   one file per scope hash under efrit-data-directory/code-reviews/.
;;        Private to this machine; nothing enters the repository.
;; notes  a git note in refs/notes/<efrit-code-review-notes-ref>,
;;        attached to a blob whose id is the hash of the key string
;;        "efrit-code-review <scope hash>" (so a staged review has an
;;        object to hang on before any commit exists), and copied onto
;;        the commit when one lands with exactly the reviewed content
;;        (`efrit-code-review-promote-after-commit' on `vc-checkin-hook'
;;        and `magit-post-commit-hook').  Travels with the repository
;;        when pushed on purpose: git push origin refs/notes/efrit-reviews.
;;
;; The repository's choice lives in .efrit/settings.json, section
;; "code-review", key "store"; absent, `efrit-code-review-store'.

(require 'efrit-settings)

(defcustom efrit-code-review-store 'json
  "Where finished reviews go when a repository does not say.
`json': files under `efrit-data-directory'.  `notes': git notes in the
repository (see the Saved reviews commentary)."
  :type '(choice (const json) (const notes))
  :group 'efrit-code-review)

(defcustom efrit-code-review-notes-ref "efrit-reviews"
  "The notes ref (under refs/notes/) reviews are written to with the `notes' store."
  :type 'string
  :group 'efrit-code-review)

(defconst efrit-code-review-settings-section "code-review")

(defun efrit-code-review-store-for (root)
  "The store ROOT uses: its settings override, else `efrit-code-review-store'."
  (let* ((section (efrit-settings-get (file-name-as-directory root) efrit-code-review-settings-section))
         (value (and (hash-table-p section) (gethash "store" section))))
    (if (member value '("json" "notes")) (intern value) efrit-code-review-store)))

(defun efrit-code-review-set-project-store (store &optional root)
  "Write STORE (`json', `notes', or `unset') as ROOT's review store.
A value equal to `efrit-code-review-store' removes the override, as
the other project setters do."
  (interactive
   (list (intern (completing-read "Review store for this repository: " '("json" "notes" "unset") nil t))
         nil))
  (let ((root (or root (efrit-vcs-root) (efrit-settings-project-root))))
    (efrit-settings-put root efrit-code-review-settings-section
                        (unless (or (eq store 'unset) (eq store efrit-code-review-store))
                          (let ((h (make-hash-table :test 'equal)))
                            (puthash "store" (symbol-name store) h)
                            h)))
    (when (called-interactively-p 'interactive)
      (message "efrit: reviews of %s go to %s" (abbreviate-file-name root) (efrit-code-review-store-for root)))))

(defun efrit-code-review--json-file (hash)
  (efrit-config-data-file (concat hash ".json") "code-reviews"))

(defun efrit-code-review--notes-key-object (root hash &optional write)
  "The blob id the note for HASH hangs on in ROOT; WRITE creates the blob."
  (let ((key (make-temp-file "efrit-review-key-")))
    (unwind-protect
        (progn
          (with-temp-file key (insert "efrit-code-review " hash "\n"))
          (apply #'efrit-code-review--git root "hash-object" (append (and write '("-w")) (list "--" key))))
      (delete-file key))))

(defun efrit-code-review--notes-add (root object text)
  "Attach TEXT as the review note of OBJECT in ROOT; non-nil on success."
  (let ((file (make-temp-file "efrit-review-note-")))
    (unwind-protect
        (progn
          (with-temp-file file (insert text))
          (require 'vc-git)
          (let ((default-directory root))
            (condition-case err
                (progn (vc-git-command nil 0 nil "notes" "--ref" efrit-code-review-notes-ref
                                       "add" "-f" "-F" file object)
                       t)
              (error (efrit-log 'warn "code review: could not write the note on %s: %s"
                                object (error-message-string err))
                     nil))))
      (delete-file file))))

(defun efrit-code-review--notes-show (root object)
  "The review note of OBJECT in ROOT, or nil."
  (when object
    (efrit-code-review--git root "notes" "--ref" efrit-code-review-notes-ref "show" object)))

(defun efrit-code-review--write-text (root hash text)
  "Keep TEXT as the review of HASH in ROOT's store; non-nil on success."
  (pcase (efrit-code-review-store-for root)
    ('notes
     (when-let* ((object (efrit-code-review--notes-key-object root hash t)))
       (efrit-code-review--notes-add root object text)))
    (_
     (let ((file (efrit-code-review--json-file hash)))
       (condition-case err
           (progn (make-directory (file-name-directory file) t)
                  (with-temp-file file (insert text))
                  t)
         (error (efrit-log 'warn "code review: could not save %s: %s" file (error-message-string err))
                nil))))))

(defun efrit-code-review--read-text (root hash)
  "The saved review text of HASH in ROOT's store, or nil."
  (pcase (efrit-code-review-store-for root)
    ('notes (efrit-code-review--notes-show root (efrit-code-review--notes-key-object root hash)))
    (_ (let ((file (efrit-code-review--json-file hash)))
         (and (file-readable-p file)
              (with-temp-buffer (insert-file-contents file) (buffer-string)))))))

(defun efrit-code-review--finding-json (f)
  `((id . ,(plist-get f :id)) (type . ,(symbol-name (plist-get f :type)))
    (file . ,(plist-get f :file)) (lines . ,(plist-get f :lines))
    (title . ,(plist-get f :title)) (description . ,(plist-get f :description))
    (patch . ,(plist-get f :patch)) (state . ,(symbol-name (plist-get f :state)))
    (old_lines . ,(plist-get f :old-lines)) (new_lines . ,(plist-get f :new-lines))
    (downgraded . ,(plist-get f :downgraded))))

(defun efrit-code-review--result-json (result)
  "RESULT as the JSON text both stores keep."
  (let ((scope (plist-get result :scope)))
    (json-encode
     `((version . 1)
       (kind . ,(symbol-name (efrit-code-review-scope-kind scope)))
       (base . ,(efrit-code-review-scope-base scope))
       (head . ,(efrit-code-review-scope-head scope))
       (hash . ,(efrit-code-review-scope-hash scope))
       (files . ,(vconcat (mapcar #'vconcat (efrit-code-review-scope-files scope))))
       (model . ,(plist-get result :model))
       (at . ,(plist-get result :at))
       (rounds . ,(plist-get result :rounds))
       (findings . ,(vconcat (mapcar #'efrit-code-review--finding-json (plist-get result :findings))))))))

(defvar efrit-code-review--last-staged nil
  "Alist (ROOT . (HASH . JSON-TEXT)): the last saved staged review per repository.
Read by `efrit-code-review-promote-after-commit'.")

(defun efrit-code-review-save (result)
  "Keep RESULT in its repository's store.  Quiet on failure."
  (let* ((scope (plist-get result :scope))
         (root (efrit-code-review-scope-root scope))
         (text (efrit-code-review--result-json result)))
    (when (efrit-code-review--write-text root (efrit-code-review-scope-hash scope) text)
      (when (eq (efrit-code-review-scope-kind scope) 'staged)
        (setf (alist-get root efrit-code-review--last-staged nil nil #'equal)
              (cons (efrit-code-review-scope-hash scope) text)))
      t)))

(defun efrit-code-review--parse-saved (scope text)
  "The result plist for SCOPE from saved TEXT, or nil when it does not parse."
  (condition-case err
      (let* ((data (json-parse-string text :object-type 'alist :array-type 'list :null-object nil))
             (findings
              (mapcar (lambda (j)
                        (efrit-code-review-finding
                         (intern (alist-get 'type j)) (alist-get 'file j)
                         :lines (alist-get 'lines j) :title (alist-get 'title j)
                         :description (alist-get 'description j) :patch (alist-get 'patch j)
                         :old-lines (alist-get 'old_lines j) :new-lines (alist-get 'new_lines j)
                         :downgraded (alist-get 'downgraded j)
                         ;; what was applied stays applied; a queue does not survive
                         :state (if (equal (alist-get 'state j) "applied") 'applied
                                  (if (equal (alist-get 'type j) "suggestion") 'pending 'applied))))
                      (alist-get 'findings data))))
        (list :status 'ok :scope scope :findings findings :saved t
              :model (alist-get 'model data) :at (alist-get 'at data) :rounds (alist-get 'rounds data)))
    (error (efrit-log 'warn "code review: saved review for %s does not parse: %s"
                      (efrit-code-review-scope-hash scope) (error-message-string err))
           nil)))

(defun efrit-code-review-load (scope)
  "The saved result for SCOPE from its repository's store, or nil."
  (when-let* ((text (efrit-code-review--read-text (efrit-code-review-scope-root scope)
                                                  (efrit-code-review-scope-hash scope))))
    (efrit-code-review--parse-saved scope text)))

;;;; After a commit: the note moves onto the commit

(defun efrit-code-review--commit-hash (root)
  "The diff hash of HEAD against its first parent (or the empty tree), or nil."
  (when-let* ((head (efrit-code-review--git root "rev-parse" "--verify" "--quiet" "HEAD")))
    (let ((parent (or (efrit-code-review--git root "rev-parse" "--verify" "--quiet" "HEAD^")
                      (efrit-code-review--git root "hash-object" "-t" "tree" "/dev/null"))))
      (and parent (efrit-code-review--hash root parent head)))))

(defun efrit-code-review-promote-after-commit ()
  "Put the last staged review's note on HEAD when HEAD is what was reviewed.
For the `notes' store only; the JSON store already keeps the review
by hash.  A commit that differs from the review (one more staged line,
an amend) gets no note, and a message says so."
  (when-let* ((root (ignore-errors (efrit-vcs-root)))
              (entry (alist-get root efrit-code-review--last-staged nil nil #'equal)))
    (when (eq (efrit-code-review-store-for root) 'notes)
      (let ((commit-hash (efrit-code-review--commit-hash root)))
        (cond
         ((null commit-hash)
          (efrit-log 'debug "code review: no HEAD diff hash in %s; note not promoted" root))
         ((equal commit-hash (car entry))
          (setq efrit-code-review--last-staged
                (assoc-delete-all root efrit-code-review--last-staged #'equal))
          (if (efrit-code-review--notes-add root "HEAD" (cdr entry))
              (message "efrit: the code review is now a note on %s"
                       (substring (or (efrit-code-review--git root "rev-parse" "HEAD") "?") 0 10))
            (message "efrit: could not attach the review note to HEAD")))
         (t
          ;; keep the entry: the user may amend into the reviewed content
          (message "efrit: the commit differs from the reviewed staged changes; review note not attached")))))))

(add-hook 'vc-checkin-hook #'efrit-code-review-promote-after-commit)
(with-eval-after-load 'magit
  (when (boundp 'magit-post-commit-hook)
    (add-hook 'magit-post-commit-hook #'efrit-code-review-promote-after-commit)))

(provide 'efrit-code-review)

;;; efrit-code-review.el ends here
