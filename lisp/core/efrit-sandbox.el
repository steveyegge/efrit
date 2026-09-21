;;; efrit-sandbox.el --- Scope-based capability sandbox -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; efrit may not read, write, or execute anything the user has not
;; granted.  Grants are *scopes*, decided per project:
;;
;;   read   PREFIX   read files/dirs under PREFIX (search, vcs, read_file)
;;   write  PREFIX   create/edit/delete files under PREFIX, save buffers
;;   exec   elisp    evaluate Lisp (eval_sexp)
;;   exec   shell    run shell commands (one grant; see below)
;;   exec   net      fetch URLs / web search
;;   buffer TARGET   touch a live buffer (read or edit its text)
;;
;; A PREFIX is a directory (or file) path.  It includes the Tramp
;; remote identity, so /ssh:host:/proj is a different scope from
;; /proj.  The project root gets `read' by default and nothing else.
;;
;; The `buffer' capability closes a hole the file checks cannot see: a
;; live buffer already visiting a file outside the project exposes that
;; file's contents through buffer operations (buffer-string, insert,
;; save) that resolve no file name, so the file-name handler never
;; fires.  Reading or editing such a buffer needs a `buffer' grant.
;; The buffer the user works in, efrit's own UI buffers, and buffers
;; visiting a file inside the project are allowed without asking (see
;; `efrit-sandbox-buffer-allowed-p').  A grant's TARGET is the visited
;; file's path when the buffer has one, else the cons (buffer . NAME).
;;
;; When a tool needs more than the current scope allows, it does not
;; run.  `efrit-sandbox-check' signals `efrit-sandbox-denied' carrying
;; a *request* -- the minimal grant that would have let it proceed.
;; The UI (efrit-sandbox-ui) turns that into a prompt offering the
;; grant once / for this session / for this project.  Project grants
;; are persisted as JSON in .efrit/sandbox.json (efrit-sandbox-store);
;; `.efrit/' itself is a hard write deny for every path, including
;; eval_sexp.
;;
;; A shell grant names commands (git, make, ...), not a blanket
;; permission; see "Shell commands" below.  That is a boundary
;; against accidents, not against a hostile model: any command can
;; escalate to any other.  Most of what an agent wants from a shell is
;; available through Emacs primitives, which *are* checked.
;;
;; Pure Executor: this is security filtering, which ARCHITECTURE.md
;; allows.  No task logic lives here.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'efrit-log)
(require 'efrit-tool-utils)   ; efrit-project-root, efrit-tool--get-project-root
(require 'efrit-settings)

(declare-function efrit-sandbox-store-save "efrit-sandbox-store")
(declare-function efrit-sandbox-store-ensure-loaded "efrit-sandbox-store")

(defgroup efrit-sandbox nil
  "Scope-based sandbox for tool access."
  :group 'efrit
  :prefix "efrit-sandbox-")

(defcustom efrit-sandbox-enabled t
  "When non-nil, every tool access is checked against the granted scope.
nil restores the pre-0.5 prefix check only (efrit-project-sandbox)."
  :type 'boolean
  :group 'efrit-sandbox)

(defcustom efrit-sandbox-default-project-grants '(read)
  "Capabilities granted on the project root without asking.
A list drawn from `read', `write', `elisp', `shell', `net', `buffer'.
The default lets the model read the project it was invoked in;
anything else asks."
  :type '(set (const read) (const write) (const elisp) (const shell)
              (const net) (const buffer))
  :group 'efrit-sandbox)

(defcustom efrit-sandbox-always-deny
  '("\\.efrit/" "\\.ssh/" "\\.gnupg/" "\\.authinfo" "\\.netrc" "\\.password-store/"
    "id_rsa" "id_ed25519" "id_ecdsa" "\\.pem\\'" "\\.key\\'")
  "Regexps matched against any path; a match is denied regardless of grants.
`.efrit/' holds the sandbox's own persisted state and must never be
model-writable."
  :type '(repeat regexp)
  :group 'efrit-sandbox)

(defconst efrit-sandbox-capabilities '(read write elisp shell net buffer)
  "Every capability the sandbox knows, in display order.")

;;; Shell commands
;;
;; A shell grant names commands, not a blanket permission.  The
;; grant's target is one of
;;
;;   t                      any command (the old single grant; still
;;                          offered, behind a separate key)
;;   (shell "git" "sed")    these command names, in any pipeline
;;   (command . "LINE")     this exact command line; never persisted,
;;                          the shape of a once-grant on a dangerous line
;;
;; A request for "git log | sed s/x//" needs git and sed both covered.
;; The command names are found by a shell-agnostic split on |, ||, &&,
;; ;, &, newlines and command substitution; wrappers (env, xargs,
;; nohup, time, ...) expose the command they run.  The split errs
;; toward finding *more* command words, so an unusual construct asks
;; rather than slips through.
;;
;; Any command can still write a shell script and run it, so this is
;; not a security boundary against a hostile model.  It is a boundary
;; against accidents: the model that was granted "git" does not get
;; "rm -rf" for free, and the user sees what is being run under which
;; grant.  `efrit-sandbox-shell-always-ask' handles the lines where an
;; accident is expensive: they are asked every time, once only, and
;; no standing grant covers them.

(defcustom efrit-sandbox-shell-always-ask
  '("\\brm\\s-+-[a-zA-Z]*\\(?:r[a-zA-Z]*f\\|f[a-zA-Z]*r\\)"   ; rm -rf, rm -fr
    "\\bsudo\\b" "\\bdoas\\b" "\\bsu\\b"
    "\\bgit\\s-+push\\b.*\\(?:--force\\|-f\\b\\|\\+[a-zA-Z]\\)"
    "\\bgit\\s-+reset\\s-+--hard"
    "\\bgit\\s-+clean\\s-+-[a-zA-Z]*[fdx]"
    "\\bgit\\s-+branch\\s-+-D\\b"
    "\\bgit\\s-+checkout\\s-+\\(?:--\\|\\.\\)"
    "\\bdd\\s-+.*of="
    "\\bmkfs\\b" "\\bfdisk\\b" "\\bparted\\b"
    "\\bchmod\\s-+\\(?:-R\\s-+\\)?[0-7]*777\\b"
    "\\bchown\\s-+-R\\b"
    "\\(?:curl\\|wget\\)\\b.*|\\s-*\\(?:ba\\|z\\)?sh\\b"
    ">\\s-*/dev/\\(?:sd\\|nvme\\|disk\\)"
    "\\bshutdown\\b" "\\breboot\\b" "\\bhalt\\b" "\\bkill\\s-+-9\\s-+-1\\b"
    ":()\\s-*{")
  "Regexps for shell lines that are asked about every time they run.
A match is never covered by a standing grant (not even a project-wide
\"any command\" grant or a `shell' default grant): the prompt offers
only \"once\", and asks for confirmation.  The list is about the
cost of an accident, not about malice; add the commands that would
ruin your day."
  :type '(repeat regexp)
  :group 'efrit-sandbox)

(defconst efrit-sandbox-shell--separators
  "\\(?:||\\|&&\\||&?\\|;\\|&\\|\n\\|`\\)"
  "Where one command ends and the next may begin, outside substitutions.")

(defun efrit-sandbox-shell--lift-substitutions (line)
  "LINE with every $(...) and (...) group moved out into its own segment.
The inner text is a command line of its own.  The group is removed
from the outer word, since the rest of that word is a path fragment,
not a command: ls dir/$(date)/f is ls and date, nothing else.
Nested groups are handled by repeating until none is left; an
unbalanced paren leaves the rest as is."
  (let ((out line) (lifted nil) (guard 0))
    (while (and (< guard 20)
                (string-match "\\$?(\\([^()]*\\))" out))
      (cl-incf guard)
      (push (match-string 1 out) lifted)
      (setq out (concat (substring out 0 (match-beginning 0))
                        (substring out (match-end 0)))))
    (mapconcat #'identity (cons out (nreverse lifted)) " ; ")))

(defun efrit-sandbox-shell--strip-noise (line)
  "LINE without quoted strings and redirections, which are not commands.
Quoted text may hold separators (git commit -m \"a; b\"); `2>&1' holds
an ampersand.  Escaped quotes are not understood: a line that still
confuses the split asks for more than it needs, never less."
  (let ((s line))
    (setq s (replace-regexp-in-string "\"[^\"]*\"" "\"\"" s))
    (setq s (replace-regexp-in-string "'[^']*'" "''" s))
    (setq s (replace-regexp-in-string "[0-9]*>&[0-9-]+" "" s))
    (setq s (replace-regexp-in-string "[0-9]*[<>]\\{1,2\\}[ \t]*[^ \t|;&]+" "" s))
    s))

(defconst efrit-sandbox-shell--wrappers
  '("env" "nohup" "time" "timeout" "nice" "ionice" "exec" "command" "builtin"
    "xargs" "watch" "sudo" "doas" "strace" "ltrace" "caffeinate" "stdbuf" "unbuffer")
  "Commands whose first non-option argument is itself a command to run.
Both names are reported: the wrapper needs a grant too.")

(defun efrit-sandbox-shell--word-command (word)
  "WORD stripped of quotes and directory, or nil if it is not a command word."
  (let ((w (replace-regexp-in-string "\\`[\"']+\\|[\"']+\\'" "" word)))
    (cond
     ((string-empty-p w) nil)
     ((string-match-p "\\`[A-Za-z_][A-Za-z0-9_]*=" w) nil)   ; VAR=value
     ((string-match-p "\\`[!\\[]\\'" w) nil)                 ; ! and [ are syntax
     (t (file-name-nondirectory w)))))

(defun efrit-sandbox-shell--segment-commands (segment)
  "Command names in one pipeline SEGMENT (no separators inside)."
  (let ((words (split-string segment "[ \t]+" t)) (out nil) (want-command t))
    (while (and words want-command)
      (let* ((word (pop words))
             (name (efrit-sandbox-shell--word-command word)))
        (cond
         ((null name))                              ; assignment/redirect: keep looking
         ((string-prefix-p "-" name)                ; an option of a wrapper: skip
          nil)
         (t
          (push name out)
          (setq want-command (member name efrit-sandbox-shell--wrappers))
          ;; `timeout 5 cmd' and `nice -n 5 cmd': skip the numeric argument
          (when (and want-command words (string-match-p "\\`[0-9]+[smhd]?\\'" (car words)))
            (pop words))))))
    (nreverse out)))

(defun efrit-sandbox-shell-commands (line)
  "The distinct command names a shell LINE would run, in order.
Nil for an empty line."
  (let ((out nil))
    (dolist (segment (split-string (efrit-sandbox-shell--lift-substitutions
                                    (efrit-sandbox-shell--strip-noise line))
                                   efrit-sandbox-shell--separators t))
      (dolist (name (efrit-sandbox-shell--segment-commands segment))
        (unless (member name out) (push name out))))
    (nreverse out)))

(defun efrit-sandbox-shell-always-ask-match (line)
  "The first regexp in `efrit-sandbox-shell-always-ask' that matches LINE, or nil."
  (cl-some (lambda (re) (and (string-match-p re line) re)) efrit-sandbox-shell-always-ask))

(defun efrit-sandbox-shell-target-p (target)
  "Non-nil if TARGET is a command-list shell target (shell NAME...)."
  (and (consp target) (eq (car target) 'shell)
       (listp (cdr target)) (cl-every #'stringp (cdr target))))

(defun efrit-sandbox-shell-target-commands (target)
  "The command names of a shell TARGET, or nil for t / exact-line targets."
  (and (efrit-sandbox-shell-target-p target) (cdr target)))

;;; Per-project default grants (the "sandbox" section of .efrit/settings.json)
;;
;;   "sandbox": {"default-grants": ["read", "write"]}
;;
;; Overrides `efrit-sandbox-default-project-grants' for that project.
;; An empty list is a valid override (nothing by default, not even
;; read), so presence of the key decides, not its truthiness.

(defconst efrit-sandbox-settings-section "sandbox"
  "The section of the project settings file this module owns.")

(defun efrit-sandbox-project-default-grants (&optional root)
  "ROOT's own default-grants override: a list of capabilities, or `unset'.
An invalid list in the file counts as unset."
  (let ((section (efrit-settings-get (or root (efrit-sandbox-project-root))
                                     efrit-sandbox-settings-section)))
    (if (and (hash-table-p section) (listp (gethash "default-grants" section 'unset))
             (not (eq (gethash "default-grants" section 'unset) 'unset)))
        (let ((raw (gethash "default-grants" section)))
          (if (null raw) nil
            (or (efrit-settings-symbol-list raw efrit-sandbox-capabilities) 'unset)))
      'unset)))

(defun efrit-sandbox-effective-default-grants (&optional root)
  "Capabilities granted on ROOT without asking: the project override, else the option."
  (let ((project (efrit-sandbox-project-default-grants root)))
    (if (eq project 'unset) efrit-sandbox-default-project-grants project)))

(defun efrit-sandbox-set-project-default-grants (grants &optional root)
  "Write GRANTS (a list of capabilities, possibly empty) as ROOT's default grants.
GRANTS `unset' removes the override."
  (efrit-settings-put (or root (efrit-sandbox-project-root)) efrit-sandbox-settings-section
                      (unless (eq grants 'unset)
                        (let ((h (make-hash-table :test 'equal)))
                          (puthash "default-grants" (mapcar #'symbol-name grants) h)
                          h))))

;;; Errors

(define-error 'efrit-sandbox-denied "Sandbox: access denied")

;;; Grant representation
;;
;; A grant is a plist (:cap CAP :target TARGET :scope SCOPE) where
;;   CAP    ∈ read write elisp shell net buffer
;;   TARGET a canonical directory/file path for read/write, or t;
;;          for buffer, a canonical file path or the cons (buffer . NAME)
;;   SCOPE  ∈ once session project
;; Session and once grants live in memory; project grants are what
;; efrit-sandbox-store persists.  Lookup: a request (cap target) is satisfied by
;; any grant with the same cap whose target is t or a prefix of the
;; request target.

(cl-defstruct (efrit-sandbox-request (:constructor efrit-sandbox-request-create))
  cap target tool detail)

(defvar efrit-sandbox--session-grants (make-hash-table :test 'equal)
  "Project root -> list of grant plists valid for this Emacs session.")

(defvar efrit-sandbox--once-grant nil
  "A grant that applies to the very next check only.")

(defvar efrit-sandbox--project-grants (make-hash-table :test 'equal)
  "Project root -> list of grant plists loaded from the project's store.")

(defvar efrit-sandbox-request-function nil
  "Function called with an `efrit-sandbox-request' when a check fails.
It must return a scope symbol (`once' `session' `project') to grant
the request at that scope, or nil to deny.  efrit-sandbox-ui sets
this to an interactive prompt; nil means every failure is a denial.")

;;; Buffer identity (injected by efrit-context-sources to avoid a
;;; dependency cycle; safe fallbacks when it is not loaded).

(defvar efrit-sandbox-target-buffer-function nil
  "Function of no args returning the buffer the user works in, or nil.
Set from efrit-context-sources.  That buffer is allowed without a
`buffer' grant.")

(defvar efrit-sandbox-agent-buffer-p-function nil
  "Predicate on a buffer: non-nil if it is one of efrit's own UI buffers.
Set from efrit-context-sources.  efrit's buffers never need a grant.")

(defun efrit-sandbox--own-buffer-p (buffer)
  "Non-nil if BUFFER is efrit's own UI buffer, or a hidden/temp buffer.
Hidden buffers (name starts with a space) are Emacs scratch space the
model creates itself; they carry no user file, so they are exempt."
  (or (string-prefix-p " " (buffer-name buffer))
      (and efrit-sandbox-agent-buffer-p-function
           (condition-case nil
               (funcall efrit-sandbox-agent-buffer-p-function buffer)
             (error nil)))))

(defun efrit-sandbox-buffer-target (buffer)
  "The grant target that identifies BUFFER.
Its visited file's canonical path when it has one, else (buffer . NAME)."
  (let ((file (buffer-local-value 'buffer-file-name buffer)))
    (if (and (stringp file) (not (string-empty-p file)))
        (efrit-sandbox-canonical file)
      (cons 'buffer (buffer-name buffer)))))

;;; Canonical paths

(defun efrit-sandbox-canonical (path)
  "Canonical form of PATH for prefix comparison.
Expanded, symlinks resolved where the path exists, directories with a
trailing slash.  Remote identity is preserved."
  (let* ((expanded (expand-file-name path))
         (resolved (if (file-exists-p expanded)
                       (condition-case nil (file-truename expanded) (error expanded))
                     ;; resolve the deepest existing parent so a new
                     ;; file under a symlinked dir canonicalizes right
                     (let* ((dir (file-name-directory expanded))
                            (base (file-name-nondirectory expanded)))
                       (if (and dir (not (equal dir expanded)) (file-exists-p dir))
                           (concat (file-name-as-directory
                                    (condition-case nil (file-truename dir) (error dir)))
                                   base)
                         expanded)))))
    (if (file-directory-p resolved)
        (file-name-as-directory resolved)
      resolved)))

(defun efrit-sandbox--under-p (path prefix)
  "Non-nil if canonical PATH is PREFIX or below it, on the same host."
  (and (equal (file-remote-p path) (file-remote-p prefix))
       (or (string-prefix-p (file-name-as-directory prefix) path)
           (string= path prefix)
           (string= (file-name-as-directory path) (file-name-as-directory prefix)))))

(defun efrit-sandbox-project-root ()
  "Canonical project root the sandbox is keyed on."
  (efrit-sandbox-canonical (efrit-tool--get-project-root)))

;;; Grant queries

(defun efrit-sandbox-grants (&optional root)
  "All grants in force for ROOT (default current project): project then session."
  (let ((root (or root (efrit-sandbox-project-root))))
    (append (gethash root efrit-sandbox--project-grants)
            (gethash root efrit-sandbox--session-grants))))

(defun efrit-sandbox--shell-grant-covers-p (gt line)
  "Non-nil if shell grant target GT covers the command LINE.
An always-ask LINE is covered only by an exact (command . LINE) grant."
  (let ((always (and (stringp line) (efrit-sandbox-shell-always-ask-match line))))
    (cond
     ((and (consp gt) (eq (car gt) 'command)) (equal (cdr gt) line))
     (always nil)
     ((eq gt t) t)
     ((eq line t) nil)                  ; a blanket request needs a blanket grant
     ((efrit-sandbox-shell-target-p gt)
      (let ((names (efrit-sandbox-shell-commands line)))
        (and names (cl-subsetp names (cdr gt) :test #'equal))))
     (t nil))))

(defun efrit-sandbox--grant-covers-p (grant cap target)
  (and (eq (plist-get grant :cap) cap)
       (let ((gt (plist-get grant :target)))
         (if (eq cap 'shell)
             (efrit-sandbox--shell-grant-covers-p gt target)
           (or (eq gt t)
               ;; a fileless-buffer target: (buffer . NAME), matched exactly
               (and (consp target) (consp gt) (equal target gt))
               (and (stringp target) (stringp gt)
                    (efrit-sandbox--under-p target gt)))))))

(defun efrit-sandbox--always-denied-p (target)
  (and (stringp target)
       (cl-some (lambda (re) (string-match-p re target)) efrit-sandbox-always-deny)))

(defun efrit-sandbox--canonical-target (cap target)
  "TARGET in the form grants are matched against: paths canonical, shell lines trimmed."
  (cond
   ((eq cap 'shell) (if (stringp target) (string-trim target) target))
   ((stringp target) (efrit-sandbox-canonical target))
   (t target)))

(defun efrit-sandbox-allowed-p (cap &optional target root)
  "Non-nil if CAP on TARGET is covered by the scope for ROOT, without asking.
For `shell', TARGET is the command line (or t for \"any command\")."
  (let* ((root (or root (efrit-sandbox-project-root)))
         (target (efrit-sandbox--canonical-target cap target)))
    (cond
     ((and (not (eq cap 'shell)) (efrit-sandbox--always-denied-p target)) nil)
     ;; an always-ask shell line: only its own once-grant applies
     ((and (eq cap 'shell) (stringp target) (efrit-sandbox-shell-always-ask-match target))
      (when (and efrit-sandbox--once-grant
                 (efrit-sandbox--grant-covers-p efrit-sandbox--once-grant cap target))
        (setq efrit-sandbox--once-grant nil)
        t))
     ;; default project grants
     ((and (memq cap (efrit-sandbox-effective-default-grants root))
           (or (memq cap '(elisp shell net))
               (and (stringp target) (efrit-sandbox--under-p target root))))
      t)
     ;; explicit grants
     ((cl-some (lambda (g) (efrit-sandbox--grant-covers-p g cap target))
               (efrit-sandbox-grants root))
      t)
     ;; the one-shot grant
     ((and efrit-sandbox--once-grant
           (efrit-sandbox--grant-covers-p efrit-sandbox--once-grant cap target))
      (setq efrit-sandbox--once-grant nil)
      t)
     (t nil))))

(defun efrit-sandbox-buffer-allowed-p (buffer &optional root)
  "Non-nil if efrit may touch BUFFER without a `buffer' grant.
Allowed without asking: efrit's own and hidden buffers, the user's
target buffer, and buffers visiting a file inside ROOT.  A buffer
visiting a file outside ROOT, or a fileless non-target buffer, needs
an explicit grant (checked by `efrit-sandbox-check-buffer')."
  (let ((root (or root (efrit-sandbox-project-root))))
    (or
     (efrit-sandbox--own-buffer-p buffer)
     ;; the buffer the user is working in
     (and efrit-sandbox-target-buffer-function
          (eq buffer (condition-case nil
                         (funcall efrit-sandbox-target-buffer-function)
                       (error nil))))
     ;; a buffer visiting a file inside the project root
     (let ((target (efrit-sandbox-buffer-target buffer)))
       (and (stringp target)
            (not (efrit-sandbox--always-denied-p target))
            (efrit-sandbox--under-p target root)))
     ;; an explicit buffer grant covering it
     (efrit-sandbox-allowed-p 'buffer (efrit-sandbox-buffer-target buffer) root))))

;;; Granting

(defun efrit-sandbox--suggest-target (cap target root)
  "The target a grant should carry for CAP on TARGET: narrow, not wide.
For a path outside the root, suggest its directory (so the next file
alongside is covered) but never anything above the user's home for
write."
  (cond
   ((memq cap '(elisp net)) t)
   ;; shell: the commands on the line; an always-ask line is granted
   ;; exactly, once (see `efrit-sandbox-shell-always-ask')
   ((eq cap 'shell)
    (cond
     ((not (stringp target)) t)
     ((efrit-sandbox-shell-always-ask-match target) (cons 'command target))
     (t (let ((names (efrit-sandbox-shell-commands target)))
          (if names (cons 'shell names) (cons 'command target))))))
   ;; a fileless buffer: grant exactly that buffer, never wider
   ((and (eq cap 'buffer) (consp target)) target)
   ((not (stringp target)) t)
   ;; a buffer grant on a file names that file, not its directory: the
   ;; user allowed *this* out-of-project file, not everything beside it
   ((eq cap 'buffer) target)
   ((efrit-sandbox--under-p target root) root)
   (t (let* ((dir (if (directory-name-p target) target (file-name-directory target)))
             (home (efrit-sandbox-canonical "~")))
        (cond
         ;; a lone file in $HOME or /: just that file
         ((and (eq cap 'write) (or (string= dir home) (string= dir "/"))) target)
         ;; a read inside a git work tree: the whole tree.  Reading one
         ;; directory of a checkout is never what the user meant, and
         ;; asking again for each subdirectory trained them to say yes
         ;; without looking.  Never above $HOME, never for write.
         ((eq cap 'read)
          (let ((top (efrit-sandbox--git-toplevel dir)))
            (if (and top (not (string= top home)) (not (string= top "/"))
                     (efrit-sandbox--under-p top home))
                top
              dir)))
         (t dir))))))

(defvar efrit-sandbox--git-toplevel-cache (make-hash-table :test 'equal)
  "Directory -> its git top-level (or `none'), for `efrit-sandbox--git-toplevel'.")

(defun efrit-sandbox--git-toplevel (dir)
  "The canonical git work-tree root containing DIR, or nil.
Cached per directory; a repository created after the first lookup is
seen after `efrit-sandbox-forget-git-toplevels'."
  (let ((cached (gethash dir efrit-sandbox--git-toplevel-cache)))
    (cond
     ((eq cached 'none) nil)
     (cached cached)
     (t
      (let* ((default-directory dir)
             (top (and (file-directory-p dir)
                       (efrit-tool-executable-find "git" dir)
                       (with-temp-buffer
                         (when (eq 0 (ignore-errors
                                       (efrit-tool-call-process "git" nil t nil
                                                                "rev-parse" "--show-toplevel")))
                           (let ((out (string-trim (buffer-string))))
                             (and (not (string-empty-p out))
                                  (efrit-sandbox-canonical
                                   (concat (file-remote-p dir) out)))))))))
        (puthash dir (or top 'none) efrit-sandbox--git-toplevel-cache)
        top)))))

(defun efrit-sandbox-forget-git-toplevels ()
  "Drop the git top-level cache."
  (clrhash efrit-sandbox--git-toplevel-cache))

(defun efrit-sandbox-grant (cap target scope &optional root)
  "Record a grant of CAP on TARGET at SCOPE for ROOT.
SCOPE is `once', `session' or `project'.  Project grants are also
persisted via `efrit-sandbox-store-save'."
  (let* ((root (or root (efrit-sandbox-project-root)))
         (grant (list :cap cap :target target :scope scope)))
    (pcase scope
      ('once (setq efrit-sandbox--once-grant grant))
      ('session (puthash root (efrit-sandbox--absorb grant (gethash root efrit-sandbox--session-grants))
                         efrit-sandbox--session-grants))
      ('project
       (puthash root (efrit-sandbox--absorb grant (gethash root efrit-sandbox--project-grants))
                efrit-sandbox--project-grants)
       (efrit-sandbox-store-save root))
      (_ (error "Unknown grant scope %S" scope)))
    (efrit-log 'info "sandbox: granted %s %s (%s) for %s" cap target scope root)
    (when (fboundp 'efrit-publish)
      (efrit-publish 'sandbox-grant `((:cap . ,cap) (:target . ,target)
                                      (:scope . ,scope) (:root . ,root))))
    grant))

(defun efrit-sandbox--absorb (grant grants)
  "GRANTS with GRANT added, minus the path grants GRANT now covers.
A read on the repository root makes the earlier read on lisp/ redundant;
keeping both only clutters the editor.  Only same-capability path
prefixes are absorbed; shell lists, buffers and t are left alone."
  (let ((cap (plist-get grant :cap)) (tg (plist-get grant :target)))
    (cons grant
          (cl-remove-if (lambda (g)
                          (and (eq (plist-get g :cap) cap)
                               (stringp tg) (stringp (plist-get g :target))
                               (efrit-sandbox--under-p (plist-get g :target) tg)))
                        grants))))

(defun efrit-sandbox-revoke (cap target &optional root)
  "Remove every session and project grant matching CAP and TARGET for ROOT."
  (let ((root (or root (efrit-sandbox-project-root))))
    (dolist (table (list efrit-sandbox--session-grants efrit-sandbox--project-grants))
      (puthash root
               (cl-remove-if (lambda (g) (and (eq (plist-get g :cap) cap)
                                              (equal (plist-get g :target) target)))
                             (gethash root table))
               table))
    (efrit-sandbox-store-save root)))

(defun efrit-sandbox-reset-session (&optional root)
  "Forget session grants for ROOT, or all projects when nil."
  (interactive)
  (if root
      (remhash root efrit-sandbox--session-grants)
    (clrhash efrit-sandbox--session-grants))
  (setq efrit-sandbox--once-grant nil))

;;; The check

(defvar efrit-sandbox--turn-answer nil
  "A standing answer for the rest of the turn: `deny-all' or `abort', or nil.
Set by the prompt's N (deny every further request this turn) and q
\(abort the turn); cleared by `efrit-sandbox-begin-turn'.")

(defun efrit-sandbox-begin-turn ()
  "Forget the standing answer of the previous turn.  Loops call this per turn."
  (setq efrit-sandbox--turn-answer nil))

(defun efrit-sandbox-deny-rest-of-turn ()
  "Answer no to this request and to every further request this turn.
The model keeps running; each denied tool gets the usual result."
  (setq efrit-sandbox--turn-answer 'deny-all))

(defun efrit-sandbox-abort-turn ()
  "Answer no and stop the turn: the tool is interrupted as C-g would.
The loop records an interrupted tool result and ends the turn; the
conversation stays and the next input continues it."
  (setq efrit-sandbox--turn-answer 'abort))

(defun efrit-sandbox--ask-without-clock (req)
  "Call `efrit-sandbox-request-function' on REQ with tool timeouts paused.
The check runs inside the tool's `with-timeout'; the time the user
spends reading the prompt must not count against the tool, or an
out-of-project read that waits 30 s for an answer times out the
moment it is granted.  `with-timeout-suspend' is what the debugger
uses for the same reason.  Returns the chosen scope or nil."
  (let ((suspended (with-timeout-suspend)))
    (unwind-protect
        (condition-case err
            (funcall efrit-sandbox-request-function req)
          (quit nil)
          (error
           (efrit-log 'warn "sandbox request function: %s" (error-message-string err))
           nil))
      (with-timeout-unsuspend suspended))))

(defun efrit-sandbox-check (cap target &optional tool detail)
  "Ensure CAP on TARGET is allowed, asking to widen the scope if not.
TOOL and DETAIL describe the caller for the prompt.  Returns t when
allowed.  Signals `efrit-sandbox-denied' with the request when the
user declines or no prompt function is installed; the data is the
`efrit-sandbox-request' struct.

With `efrit-sandbox-enabled' nil this is a no-op that returns t."
  (if (not efrit-sandbox-enabled)
      t
    (let* ((root (efrit-sandbox-project-root))
           (ctarget (efrit-sandbox--canonical-target cap target)))
      (efrit-sandbox-store-ensure-loaded root)
      (cond
       ((and (not (eq cap 'shell)) (efrit-sandbox--always-denied-p ctarget))
        (efrit-log 'warn "sandbox: %s on %s is always denied" cap ctarget)
        (signal 'efrit-sandbox-denied
                (list (efrit-sandbox-request-create
                       :cap cap :target ctarget :tool tool
                       :detail (format "%s is protected and can never be granted" ctarget)))))
       ((efrit-sandbox-allowed-p cap ctarget root)
        (efrit-log 'debug "sandbox: allowed %s %s (%s)" cap
                   (if (stringp ctarget) (abbreviate-file-name ctarget) ctarget) tool)
        t)
       (t
        (efrit-log 'debug "sandbox: %s %s not covered for %s; asking (%s)" cap
                   (if (stringp ctarget) (abbreviate-file-name ctarget) ctarget)
                   (abbreviate-file-name root) tool)
        (let* ((req (efrit-sandbox-request-create
                     :cap cap
                     :target (efrit-sandbox--suggest-target cap ctarget root)
                     :tool tool :detail detail))
               (scope (and efrit-sandbox-request-function
                           ;; a standing N from earlier this turn: no prompt
                           (not (eq efrit-sandbox--turn-answer 'deny-all))
                           (efrit-sandbox--ask-without-clock req))))
          ;; q in the prompt: this tool is interrupted, the loop ends
          ;; the turn the way it does for C-g
          (when (eq efrit-sandbox--turn-answer 'abort)
            ;; one quit ends the turn; do not keep quitting into the next
            (setq efrit-sandbox--turn-answer nil)
            (efrit-log 'info "sandbox: turn aborted by the user at %s %s (%s)" cap ctarget tool)
            (when (fboundp 'efrit-publish)
              (efrit-publish 'sandbox-denied `((:cap . ,cap) (:target . ,ctarget) (:tool . ,tool)
                                               (:abort . t))))
            (signal 'quit nil))
          ;; an exact-line shell grant is never standing: whatever the
          ;; prompt returned, it applies to this run only
          (when (and (memq scope '(session project))
                     (efrit-sandbox-request-once-only-p req))
            (setq scope 'once))
          (if (memq scope '(once session project))
              (progn
                (efrit-sandbox-grant cap (efrit-sandbox-request-target req) scope root)
                ;; The grant carries the suggested (possibly wider)
                ;; target, so the re-check passes; a once grant is
                ;; consumed by this re-check.
                (or (efrit-sandbox-allowed-p cap ctarget root)
                    (signal 'efrit-sandbox-denied (list req))))
            (efrit-log 'info "sandbox: denied %s %s (%s)" cap ctarget tool)
            (when (fboundp 'efrit-publish)
              (efrit-publish 'sandbox-denied `((:cap . ,cap) (:target . ,ctarget) (:tool . ,tool))))
            (signal 'efrit-sandbox-denied (list req)))))))))

(defun efrit-sandbox-request-once-only-p (req)
  "Non-nil if REQ can only ever be granted once: an always-ask shell line."
  (let ((target (efrit-sandbox-request-target req)))
    (and (eq (efrit-sandbox-request-cap req) 'shell)
         (consp target) (eq (car target) 'command))))

(defun efrit-sandbox--target-label (target)
  "A short human label for a grant TARGET (path, (buffer . NAME), or a shell target)."
  (cond ((and (consp target) (eq (car target) 'buffer))
         (format "buffer %s" (cdr target)))
        ((efrit-sandbox-shell-target-p target)
         (mapconcat #'identity (cdr target) ", "))
        ((and (consp target) (eq (car target) 'command))
         (format "exactly: %s" (cdr target)))
        ((stringp target) (abbreviate-file-name target))
        ((eq target t) "any")
        (t (format "%s" target))))

(defun efrit-sandbox-check-buffer (buffer &optional tool detail)
  "Ensure efrit may touch BUFFER, asking for a `buffer' grant if not.
Returns t when allowed.  Signals `efrit-sandbox-denied' when refused,
exactly like `efrit-sandbox-check', so callers propagate it the same
way.  A no-op returning t when `efrit-sandbox-enabled' is nil.

The user's target buffer, efrit's own buffers, hidden buffers, and
buffers visiting a file inside the project are allowed without asking."
  (if (not efrit-sandbox-enabled)
      t
    (let ((buffer (get-buffer buffer)))
      (cond
       ((null buffer) t)                ; nothing to protect
       ((efrit-sandbox-buffer-allowed-p buffer) t)
       (t
        (efrit-sandbox-check
         'buffer (efrit-sandbox-buffer-target buffer)
         (or tool "buffer")
         (or detail
             (format "the buffer %s visits a file outside the project"
                     (buffer-name buffer)))))))))

(defun efrit-sandbox-describe-request (req)
  "One-line human description of REQ for prompts and tool results."
  (let ((cap (efrit-sandbox-request-cap req))
        (target (efrit-sandbox-request-target req)))
    (pcase cap
      ('read (format "read %s" target))
      ('write (format "write under %s" target))
      ('elisp "evaluate Emacs Lisp")
      ('shell (cond ((efrit-sandbox-shell-target-p target)
                     (format "run %s" (efrit-sandbox--target-label target)))
                    ((and (consp target) (eq (car target) 'command))
                     (format "run the command %s" (cdr target)))
                    (t "run shell commands")))
      ('net "access the network")
      ('buffer (format "touch %s" (efrit-sandbox--target-label target)))
      (_ (format "%s %s" cap target)))))

(defconst efrit-sandbox-denied-prefix "Error sandbox denied: "
  "Prefix of a denied tool result.  The agent buffer recognises it to
render the row as a denial rather than a tool failure.")

(defun efrit-sandbox-denied-tool-result (req)
  "The tool_result text (after `efrit-sandbox-denied-prefix') for a denied REQ.
Says what was refused.  The turn continues: the model is told to go
on without that access, not to retry it or route around it."
  (format "%s%s. The user declined. Do not retry this or work around it (no other tool, no other path). Continue the task without it if you can; otherwise say what access you need and why, and stop."
          (efrit-sandbox-describe-request req)
          (if (efrit-sandbox-request-detail req)
              (format " (%s)" (efrit-sandbox-request-detail req))
            "")))

(provide 'efrit-sandbox)

;; The store needs the tables above; load it after providing so its
;; (require 'efrit-sandbox) is satisfied without recursion.
(require 'efrit-sandbox-store)

;;; efrit-sandbox.el ends here
