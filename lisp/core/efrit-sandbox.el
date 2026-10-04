;;; efrit-sandbox.el --- Scope-based capability sandbox -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.9.2
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
(require 'efrit-events)
(require 'vc)

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

;;; Remote hosts
;;
;; A path with a TRAMP prefix is on another machine.  The local
;; project's default grants never apply to it, even when the project
;; root itself is remote: what the model may read or write there is
;; decided per host by `efrit-sandbox-remote-hosts', falling back to
;; `efrit-sandbox-remote-default'.  Every read and write the sandbox
;; sees goes through this: the file tools (`efrit-resolve-path'), the
;; eval file-name handler, buffers visiting remote files, and shell
;; commands run in a remote root.

(defconst efrit-sandbox-remote-policies '(allow ask once deny)
  "What a remote host policy may say for `:read' or `:write'.
allow: no prompt, the whole host.  ask: prompt, all scopes offered.
once: prompt, only a one-time grant offered.  deny: refused without a
prompt.")

(defcustom efrit-sandbox-remote-hosts nil
  "Per-host sandbox policy for remote (TRAMP) paths.
An alist of (HOST . (:read POLICY :write POLICY)).  HOST is matched
against the remote identity of the path (\"/ssh:user@box:\") and
against its host part (\"box\") with `string-match-p', first match
wins; a plain host name matches itself and its subdomains.  POLICY
is one of `efrit-sandbox-remote-policies'; a missing key falls back
to `efrit-sandbox-remote-default'.

  ((\"build-box\" . (:read allow :write ask))
   (\"prod-\" . (:read once :write deny))
   (\"/sudo:\" . (:read deny :write deny)))"
  :type '(alist :key-type (string :tag "Host or regexp")
                :value-type (plist :key-type (choice (const :read) (const :write))
                                   :value-type (choice (const allow) (const ask)
                                                       (const once) (const deny))))
  :group 'efrit-sandbox)

(defcustom efrit-sandbox-remote-default '(:read ask :write once)
  "Policy for remote hosts not in `efrit-sandbox-remote-hosts'.
The default asks for reads (any scope) and allows writes one at a
time only: a remote machine is somebody's box, not a scratch tree."
  :type '(plist :key-type (choice (const :read) (const :write))
                :value-type (choice (const allow) (const ask) (const once) (const deny)))
  :group 'efrit-sandbox)

(defun efrit-sandbox-abbreviate (path)
  "`abbreviate-file-name' that never touches a remote host.
On a remote name Emacs's version asks TRAMP whether the file system is
case-insensitive, which opens the connection; the sandbox must not
connect just to display or log a path.  A remote PATH comes back as
written."
  (if (and (stringp path) (file-remote-p path))
      path
    (abbreviate-file-name path)))

(defun efrit-sandbox-remote-host (path)
  "The host part of remote PATH (\"box\" of \"/ssh:me@box:/x\"), or nil."
  (when (stringp path)
    (or (file-remote-p path 'host) nil)))

(defun efrit-sandbox-remote-policy (path cap)
  "The policy symbol for CAP (`read' or `write') on remote PATH.
nil when PATH is not remote or CAP is not a file capability."
  (when (and (memq cap '(read write)) (stringp path) (file-remote-p path))
    (let* ((identity (file-remote-p path))
           (host (or (efrit-sandbox-remote-host path) ""))
           (key (if (eq cap 'read) :read :write))
           (entry (cl-find-if
                   (lambda (e)
                     (let ((pat (car e)))
                       (or (string-match-p pat identity)
                           (string-match-p pat host)
                           (efrit-sandbox-host-under-p host pat))))
                   efrit-sandbox-remote-hosts)))
      (or (and entry (plist-get (cdr entry) key))
          (plist-get efrit-sandbox-remote-default key)
          'ask))))

(defun efrit-sandbox-remote-p (target)
  "Non-nil if grant TARGET names a remote file (a string with a TRAMP prefix)."
  (and (stringp target) (file-remote-p target) t))

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
  cap target tool detail
  ;; set by the prompt when the user edited the shell line or form
  ;; before allowing it; the tool runs the edited text instead
  edited)

;;; Per-turn state, per session
;;
;; The once grant, the edited input and the standing answer (N / q)
;; belong to one turn of one session.  With several sessions (and
;; prompts that nest: a sentinel runs another session's tools inside
;; this session's `recursive-edit'), a global would let session B
;; take A's once grant or inherit A's deny-all.  They are kept per
;; `efrit-current-session-id'; code outside any session uses the nil
;; key (multi-session audit, 2026-09-28).

(defvar efrit-sandbox--turn-state (make-hash-table :test 'equal)
  "Session id (or nil) -> plist (:once GRANT :edited (TOOL . TEXT) :answer SYMBOL).")

(defvar efrit-current-session-id)

(defun efrit-sandbox--turn-get (key)
  (plist-get (gethash (bound-and-true-p efrit-current-session-id) efrit-sandbox--turn-state) key))

(defun efrit-sandbox--turn-set (key value)
  (let ((id (bound-and-true-p efrit-current-session-id)))
    (puthash id (plist-put (gethash id efrit-sandbox--turn-state) key value)
             efrit-sandbox--turn-state)
    value))

(defun efrit-sandbox-take-edited-input (tool)
  "The edited input the user supplied at TOOL's prompt just now, or nil.
Consumed: a second call returns nil.  Callers: shell_exec runs the
edited command line, eval_sexp reads the edited form."
  (let ((edited (efrit-sandbox--turn-get :edited)))
    (when (and edited (equal (car edited) tool))
      (prog1 (cdr edited)
        (efrit-sandbox--turn-set :edited nil)))))

(defun efrit-sandbox-request-editable-p (req)
  "Non-nil if REQ is one whose input the user can edit before allowing:
a shell command line or an elisp form."
  (and (memq (efrit-sandbox-request-cap req) '(shell elisp))
       (stringp (efrit-sandbox-request-detail req))
       (not (string-empty-p (efrit-sandbox-request-detail req)))
       (member (efrit-sandbox-request-tool req) '("shell_exec" "eval_sexp"))))

(defvar efrit-sandbox--session-grants (make-hash-table :test 'equal)
  "Project root -> list of grant plists valid for this Emacs session.")

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
trailing slash.  Remote identity is preserved.

A remote PATH is canonicalized lexically only: deciding whether to
ask must not open a TRAMP connection (that made the sandbox itself
connect to a host, or hang on one that is down, 2026-09-28), and the
policy is per host anyway, so symlinks there do not change the
answer.  The remote identity is kept as written."
  (if (file-remote-p path)
      (let* ((remote (file-remote-p path))
             (local (file-remote-p path 'localname))
             (clean (if (and local (file-name-absolute-p local))
                        (let ((file-name-handler-alist nil))
                          (expand-file-name local "/"))
                      (or local "/"))))
        (concat remote (if (and (string-suffix-p "/" (or local "")) (not (string= clean "/")))
                           (file-name-as-directory clean)
                         clean)))
    (efrit-sandbox--canonical-local path)))

(defun efrit-sandbox--canonical-local (path)
  "`efrit-sandbox-canonical' for a local PATH (may touch the filesystem)."
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

(defun efrit-sandbox-repo-of (target)
  "The canonical VC work-tree root that contains path TARGET, or nil.
Local paths only.  $HOME or / as a work tree (a dotfiles checkout) is
not a repository for this purpose: a grant on it would be a grant on
everything."
  (when (and (stringp target) (not (efrit-sandbox-remote-p target)))
    (let* ((dir (if (directory-name-p target) target (file-name-directory target)))
           ;; a file the model is about to create sits in a directory
           ;; that may not exist yet: the nearest existing ancestor
           ;; says which checkout it will belong to
           (dir (let ((d dir))
                  (while (and d (not (file-directory-p d)) (not (string= d "/")))
                    (setq d (file-name-directory (directory-file-name d))))
                  d))
           (top (and dir (efrit-sandbox--git-toplevel dir)))
           (home (efrit-sandbox-canonical "~")))
      (and top (not (string= top home)) (not (string= top "/"))
           top))))

(defun efrit-sandbox-grants-for (cap target root)
  "The grants that may cover CAP on TARGET: ROOT's, and the target's repo's.
A grant made \"for this project\" is keyed on the repository the file
belongs to, so it holds from any agent buffer (tzz, 2026-10-01: one
turn asked six times with the root at ~/).  Repos only add their own
grants when TARGET lies inside them."
  (let ((repo (and (memq cap '(read write buffer)) (efrit-sandbox-repo-of target))))
    (append (efrit-sandbox-grants root)
            (and repo (not (equal repo root))
                 (progn (efrit-sandbox-store-ensure-loaded repo)
                        (efrit-sandbox-grants repo))))))

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
               ;; a network host: (host . NAME) covers NAME and its subdomains
               (and (efrit-sandbox-host-target-p target) (efrit-sandbox-host-target-p gt)
                    (efrit-sandbox-host-under-p (cdr target) (cdr gt)))
               (and (stringp target) (stringp gt)
                    (efrit-sandbox--under-p target gt)))))))

(defun efrit-sandbox-host-target-p (target)
  "Non-nil if TARGET is a network host grant target, (host . NAME)."
  (and (consp target) (eq (car target) 'host) (stringp (cdr target))))

(defun efrit-sandbox-host-under-p (host domain)
  "Non-nil if HOST is DOMAIN or a subdomain of it."
  (let ((host (downcase host)) (domain (downcase domain)))
    (or (string= host domain)
        (string-suffix-p (concat "." domain) host))))

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
     ;; a remote file: the host policy first.  `allow' needs no grant,
     ;; `deny' accepts none; `ask' and `once' fall through to the
     ;; explicit grants below (never to the project defaults)
     ((and (memq cap '(read write)) (efrit-sandbox-remote-p target)
           (memq (efrit-sandbox-remote-policy target cap) '(allow deny)))
      (eq (efrit-sandbox-remote-policy target cap) 'allow))
     ;; an always-ask shell line: only its own once-grant applies
     ((and (eq cap 'shell) (stringp target) (efrit-sandbox-shell-always-ask-match target))
      (let ((once (efrit-sandbox--turn-get :once)))
        (when (and once (efrit-sandbox--grant-covers-p once cap target))
          (efrit-sandbox--turn-set :once nil)
          t)))
     ;; default project grants (never for a remote file, see above;
     ;; tested first so a remote target does not read the project's
     ;; settings on the host just to be told no)
     ((and (or (memq cap '(elisp shell net))
               (and (stringp target)
                    (not (efrit-sandbox-remote-p target))
                    (efrit-sandbox--under-p target root)))
           (memq cap (efrit-sandbox-effective-default-grants root)))
      t)
     ;; explicit grants: the root's and, for a file, its repository's
     ((cl-some (lambda (g) (efrit-sandbox--grant-covers-p g cap target))
               (efrit-sandbox-grants-for cap target root))
      t)
     ;; the one-shot grant.  For a host or a read it holds for the rest
     ;; of the turn: fetching an article means the page and then its
     ;; API, and reading a file means reading it again after an edit;
     ;; asking for each was noise (tzz, 2026-10-01).  A write, a shell
     ;; line or an eval stays one operation.
     ((let ((once (efrit-sandbox--turn-get :once)))
        (and once (efrit-sandbox--grant-covers-p once cap target)))
      (unless (memq cap '(net read buffer))
        (efrit-sandbox--turn-set :once nil))
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
     ;; a buffer visiting a file inside the project root.  A remote
     ;; file is not "inside" for this purpose: its host policy decides
     ;; (allow = free, anything else = a buffer grant is asked for)
     (let ((target (efrit-sandbox-buffer-target buffer)))
       (and (stringp target)
            (not (efrit-sandbox--always-denied-p target))
            (if (efrit-sandbox-remote-p target)
                (eq (efrit-sandbox-remote-policy target 'read) 'allow)
              (efrit-sandbox--under-p target root))))
     ;; an explicit buffer grant covering it
     (efrit-sandbox-allowed-p 'buffer (efrit-sandbox-buffer-target buffer) root))))

;;; Granting

(defun efrit-sandbox--suggest-target (cap target root)
  "The target a grant should carry for CAP on TARGET: narrow, not wide.
For a path outside the root, suggest its directory (so the next file
alongside is covered) but never anything above the user's home for
write."
  (cond
   ((eq cap 'elisp) t)
   ;; net: the host asked for, so a grant covers that site and its
   ;; subdomains, not the whole internet; t stays t
   ((eq cap 'net) target)
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
   ;; a remote file: its directory, no wider.  The project root does
   ;; not stand in for it even when the root is on that host (the
   ;; host policy governs remote files), and no git probe over TRAMP.
   ((efrit-sandbox-remote-p target)
    (if (directory-name-p target) target (file-name-directory target)))
   ((efrit-sandbox--under-p target root) root)
   (t (let* ((dir (if (directory-name-p target) target (file-name-directory target)))
             (home (efrit-sandbox-canonical "~")))
        (cond
         ;; a lone file in $HOME or /: just that file
         ((and (eq cap 'write) (or (string= dir home) (string= dir "/"))) target)
         ;; inside a git work tree: the whole tree, for read and write
         ;; alike.  One directory of a checkout is never what the user
         ;; meant, and asking per subdirectory trained them to say yes
         ;; without looking.  Never above $HOME.
         ((efrit-sandbox-repo-of target))
         (t dir))))))

(declare-function efrit-review-confidence-take-grant "efrit-review-confidence")
(declare-function efrit-grant-history-record "efrit-grant-history")

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
      ;; VC finds the work tree without running git (the backend walks
      ;; up for its marker directory); a remote DIR is never probed
      (let* ((top (and (not (file-remote-p dir))
                       (file-directory-p dir)
                       (let* ((backend (ignore-errors (vc-responsible-backend dir)))
                              (root (and backend (ignore-errors (vc-call-backend backend 'root dir)))))
                         (and root (efrit-sandbox-canonical (expand-file-name root)))))))
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
         ;; a grant on a path inside some repository is that repo's,
         ;; whatever buffer asked (see `efrit-sandbox-grants-for')
         (root (or (and (memq scope '(session project))
                        (memq cap '(read write buffer))
                        (efrit-sandbox-repo-of target))
                   root))
         (grant (list :cap cap :target target :scope scope)))
    (when (eq scope 'project) (efrit-sandbox-store-ensure-loaded root))
    (pcase scope
      ('once (efrit-sandbox--turn-set :once grant))
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
  (efrit-sandbox--turn-set :once nil))

;;; The check

(defun efrit-sandbox-turn-answer ()
  "The standing answer for the rest of this session's turn: `deny-all', `abort', or nil.
Set by the prompt's N (deny every further request this turn) and q
\(abort the turn); cleared by `efrit-sandbox-begin-turn'."
  (efrit-sandbox--turn-get :answer))

(defun efrit-sandbox-begin-turn (&optional user-text)
  "Forget the previous turn's standing answer, once grant and edited input.
Loops call this per turn, as the session's code.  The question mark
\(`efrit-brief-question-turn') is per turn too: it is armed by the
send and cleared at the next turn's start.

USER-TEXT is what the user sent (the API text, with any article or
snapshot efrit prepended): the hosts and local paths it names are the
work itself, and requests for them are expected this turn (tzz,
2026-10-01: an article's own URL should not need a grant)."
  (puthash (bound-and-true-p efrit-current-session-id) nil efrit-sandbox--turn-state)
  (when (stringp user-text)
    (efrit-sandbox--turn-set :mentioned (efrit-sandbox-mentions user-text))))

(defun efrit-sandbox-mentions (text)
  "The hosts and local paths TEXT names: (:hosts (H…) :paths (P…)).
URLs give their host; absolute, ~/ and ./ paths give the canonical
path.  Trailing punctuation is dropped."
  (let ((hosts nil) (paths nil) (start 0))
    (while (string-match "\\bhttps?://\\([^/[:space:]\"'<>()]+\\)" text start)
      (cl-pushnew (downcase (replace-regexp-in-string ":[0-9]+\\'" "" (match-string 1 text))) hosts :test #'equal)
      (setq start (match-end 0)))
    (setq start 0)
    (while (string-match "\\(?:^\\|[[:space:]\"'(=,]\\)\\(\\(?:~/\\|/\\|\\./\\)[^[:space:]\"'<>(),;]+\\)" text start)
      (let ((p (replace-regexp-in-string "[.:]+\\'" "" (match-string 1 text))))
        (unless (string-match-p "\\`/\\(?:ssh\\|rpc\\|scp\\|sudo\\|docker\\):" p)
          (cl-pushnew (efrit-sandbox-canonical (expand-file-name p)) paths :test #'equal)))
      (setq start (match-end 0)))
    (list :hosts (nreverse hosts) :paths (nreverse paths))))

(defun efrit-sandbox--mentioned-p (cap target)
  "Non-nil when TARGET of CAP was named in this turn's user text."
  (let ((m (efrit-sandbox--turn-get :mentioned)))
    (and m
         (pcase cap
           ('net (and (efrit-sandbox-host-target-p target)
                      (cl-some (lambda (h) (efrit-sandbox-host-under-p (cdr target) h))
                               (plist-get m :hosts))))
           ((or 'read 'write 'buffer)
            (and (stringp target)
                 (cl-some (lambda (p) (or (string= target p) (efrit-sandbox--under-p target p)
                                          (efrit-sandbox--under-p p target)))
                          (plist-get m :paths))))
           (_ nil)))))

(defvar efrit-sandbox--question-turn)
(defun efrit-sandbox-end-turn ()
  "Drop the per-turn question mark.  Loops call this when a turn ends."
  (when (boundp 'efrit-sandbox--question-turn)
    (setq efrit-sandbox--question-turn nil)))

(defun efrit-sandbox-deny-rest-of-turn ()
  "Answer no to this request and to every further request this turn.
The model keeps running; each denied tool gets the usual result."
  (efrit-sandbox--turn-set :answer 'deny-all))

(defun efrit-sandbox-abort-turn ()
  "Answer no and stop the turn: the tool is interrupted as C-g would.
The loop records an interrupted tool result and ends the turn; the
conversation stays and the next input continues it."
  (efrit-sandbox--turn-set :answer 'abort))

(defun efrit-sandbox--ask-without-clock (req)
  "Call `efrit-sandbox-request-function' on REQ with efrit's clocks paused.
The check runs inside the tool's `with-timeout' and the turn's wall
clock; the time the user spends reading the prompt must not count
against either (`efrit-with-user-waiting').  Returns the chosen scope
or nil."
  ;; Tools run inside the API's process callback, where Emacs binds
  ;; `inhibit-quit' to t for every sentinel and filter.  A prompt
  ;; needs C-g as a way out, so quits are re-enabled here for its
  ;; duration; C-g then lands in the `quit' handler as a denial.
  ;; (From 2026-09-28 to 2026-09-30 this function refused to prompt
  ;; at all when quits were inhibited: every live sandbox request was
  ;; denied without a menu.  The prompt that wedged Emacs on
  ;; 2026-09-27 came from the eval handler owning `load', fixed at the
  ;; source in efrit-sandbox-eval, not from quits.)
  (let ((inhibit-quit nil))
    (efrit-with-user-waiting
      (condition-case err
          (funcall (efrit-sandbox--request-function) req)
        (quit nil)
        (error
         (efrit-log 'warn "sandbox request function: %s" (error-message-string err))
         nil)))))

(defun efrit-sandbox--request-function ()
  "The prompt to ask with: the variable, else the UI prompt when it is loaded.
The variable was found nil in a live Emacs on 2026-09-30 (every
request denied silently, the tour saw no menu); until the cause is
known, a loaded `efrit-sandbox-ui' is the answer.  Batch tests bind
the variable to nil to mean \"deny without asking\": no fallback there."
  (or efrit-sandbox-request-function
      (and (not noninteractive)
           (fboundp 'efrit-sandbox-ui-prompt)
           #'efrit-sandbox-ui-prompt)))

(declare-function efrit-sandbox-ui-prompt "efrit-sandbox-ui")

;;; Expected requests
;;
;; Most prompts are not decisions: a read under the Emacs installation,
;; a scratch write under `temporary-file-directory', a read-only shell
;; command, a path inside a repository the user already granted this
;; session.  Those are *expected*: granted for the session with a
;; transcript note, no menu.  Everything else is *unusual* and asks as
;; before, with the menu saying why.  Expected grants are per session,
;; never saved (tzz, 2026-10-01).

(defcustom efrit-sandbox-expected-read-roots
  (list (lambda () data-directory)
        (lambda () (file-name-directory (directory-file-name data-directory)))
        (lambda () (bound-and-true-p package-user-dir))
        (lambda () (and (boundp 'user-emacs-directory) (expand-file-name "elpa" user-emacs-directory)))
        (lambda () temporary-file-directory))
  "Directories whose files the model may read without asking.
Each element is a directory, or a function of no arguments returning
one (or nil).  The Emacs installation, installed packages and the
temporary directory by default."
  :type '(repeat (choice directory function))
  :group 'efrit-sandbox)

(defcustom efrit-sandbox-expected-write-roots
  (list (lambda () temporary-file-directory))
  "Directories the model may write under without asking.
Same shape as `efrit-sandbox-expected-read-roots'.  Scratch files
under `temporary-file-directory' by default: a `write-region' to /tmp
inside an eval is transport, not intent."
  :type '(repeat (choice directory function))
  :group 'efrit-sandbox)

(defcustom efrit-sandbox-expected-shell-commands
  '("ls" "cat" "head" "tail" "wc" "grep" "rg" "find" "fd" "diff" "sort" "uniq" "cut" "tr"
    "awk" "column" "nl" "tac" "rev" "basename" "dirname" "realpath" "readlink" "env" "printf"
    "echo" "date" "pwd" "which" "file" "stat" "du" "df" "cd" "true" "false" "test"
    "git status" "git diff" "git log" "git show" "git branch" "git rev-parse" "git blame"
    "git ls-files" "git remote" "git stash list")
  "Shell commands (or command + first word) that run without asking.
A line is expected when every command on it is listed, no redirection
or substitution appears, and no `efrit-sandbox-shell-always-ask' rule
matches.  Read-only tools by default."
  :type '(repeat string)
  :group 'efrit-sandbox)

(defun efrit-sandbox--expected-roots (option)
  "The directories OPTION (a list of dirs or thunks) names, canonical."
  (delq nil (mapcar (lambda (x)
                      (let ((d (if (functionp x) (ignore-errors (funcall x)) x)))
                        (and (stringp d) (file-name-as-directory (efrit-sandbox-canonical d)))))
                    option)))

(defun efrit-sandbox--expected-shell-line-p (line)
  "Non-nil when every command on shell LINE is in `efrit-sandbox-expected-shell-commands'."
  (and (stringp line)
       (not (efrit-sandbox-shell-always-ask-match line))
       ;; redirections, substitutions and background jobs change what a
       ;; read-only command can do; `&&' and `||' only sequence.  Sending
       ;; stderr to /dev/null changes nothing (2026-10-03: `ls ... 2>/dev/null'
       ;; asked for a grant over seven read-only commands)
       ;; Inside single quotes nothing expands: an awk program's `$3' is
       ;; text, not a substitution
       (not (string-match-p "[<>`$]"
                            (replace-regexp-in-string
                             "'[^']*'" ""
                             (replace-regexp-in-string "2>/dev/null" "" line t t) t t)))
       (not (string-match-p "\\(?:^\\|[^&]\\)&\\(?:[^&]\\|$\\)" line))
       (let ((names (efrit-sandbox-shell-commands line)))
         (and names
              (cl-every
               (lambda (name)
                 (or (member name efrit-sandbox-expected-shell-commands)
                     ;; "git status" style entries: command + its first word
                     (and (string-match (concat "\\(?:^\\|[;|&]\\s-*\\)" (regexp-quote name) "\\s-+\\([a-z-]+\\)") line)
                          (member (concat name " " (match-string 1 line))
                                  efrit-sandbox-expected-shell-commands))))
               names)))))

(defun efrit-sandbox-expected-p (cap target root)
  "Why CAP on TARGET is an expected request for ROOT, or nil when it is unusual.
The reason is a short string for the transcript note."
  (let ((target (efrit-sandbox--canonical-target cap target)))
    (cond
     ((and (stringp target) (efrit-sandbox--always-denied-p target)) nil)
     ((and (stringp target) (efrit-sandbox-remote-p target)) nil)
     ;; the user named it in this turn's input: it is the work
     ((efrit-sandbox--mentioned-p cap target) "named in your request")
     ;; the project's own files are the project's business, never scratch
     ((and (memq cap '(read write)) (stringp target) (efrit-sandbox--under-p target root)) nil)
     ((and (eq cap 'read) (stringp target)
           (cl-some (lambda (d) (efrit-sandbox--under-p target d))
                    (efrit-sandbox--expected-roots efrit-sandbox-expected-read-roots)))
      "a read under the Emacs installation or the temporary directory")
     ;; a scratch write: under the temporary directory, but not inside a
     ;; repository that happens to live there (a checkout under /tmp is
     ;; still a project)
     ((and (eq cap 'write) (stringp target)
           (not (efrit-sandbox-repo-of target))
           (cl-some (lambda (d) (efrit-sandbox--under-p target d))
                    (efrit-sandbox--expected-roots efrit-sandbox-expected-write-roots)))
      "a scratch write under the temporary directory")
     ;; a file in a repository the user already granted this session
     ;; (any capability on it): the repo is in play
     ((and (memq cap '(read write buffer)) (stringp target))
      (when-let* ((repo (efrit-sandbox-repo-of target)))
        (and (cl-some (lambda (g) (and (stringp (plist-get g :target))
                                       (equal (plist-get g :target) repo)))
                      (append (gethash repo efrit-sandbox--session-grants)
                              (gethash root efrit-sandbox--session-grants)))
             (format "inside %s, granted earlier this session" (efrit-sandbox-abbreviate repo)))))
     ((and (eq cap 'shell) (efrit-sandbox--expected-shell-line-p target))
      "read-only shell commands")
     (t nil))))

(defun efrit-sandbox--grant-expected (cap target root reason tool)
  "Grant CAP on TARGET for the session as an expected request; say so.
A scratch write is granted on the file alone: a grant on the whole
temporary directory would also cover any checkout living under it."
  (let ((grant-target (if (and (eq cap 'write) (stringp target)
                               (cl-some (lambda (d) (efrit-sandbox--under-p target d))
                                        (efrit-sandbox--expected-roots efrit-sandbox-expected-write-roots)))
                          target
                        (efrit-sandbox--suggest-target cap target root))))
    (efrit-sandbox-grant cap grant-target 'session root)
    (efrit-log 'info "sandbox: %s %s expected (%s); granted for the session (%s)"
               cap (if (stringp target) (efrit-sandbox-abbreviate target) target) reason tool)
    (when (fboundp 'efrit-publish)
      (efrit-publish 'note `((:text . ,(format "⛨ %s: %s, allowed without asking (%s)"
                                                (or tool "a tool")
                                                (efrit-sandbox-describe-request
                                                 (efrit-sandbox-request-create :cap cap :target grant-target))
                                                reason))
                             (:face . shadow) (:kind . sandbox))))
    t))

(defun efrit-sandbox-shell-starts-emacs-p (line)
  "Non-nil when shell LINE runs an emacs binary (not emacsclient).
A second Emacs is sometimes the right tool (a clean-Emacs test, a
byte-compile of a tree) and often the wrong one (testing code that
could run here, with the user's libraries stubbed).  The sandbox
does not decide; the reviewer sees the line flagged and the agent's
reason for it (tzz, 2026-10-01: \"not an absolute rule, a strong
recommendation, and the model should have to justify it\")."
  (and (stringp line)
       (cl-some (lambda (name) (string-match-p "\\`\\(?:.*/\\)?emacs\\(?:-[0-9.]+\\)?\\'" name))
                (efrit-sandbox-shell-commands line))))

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
       ;; the user sent this turn as a question: no writes, shell or
       ;; eval, without a prompt (efrit-brief)
       ((and (fboundp 'efrit-sandbox-question-turn-p) (efrit-sandbox-question-turn-p cap))
        (efrit-log 'info "sandbox: %s refused, this turn is a question" cap)
        (when (fboundp 'efrit-publish)
          (efrit-publish 'sandbox-denied `((:cap . ,cap) (:target . ,ctarget) (:tool . ,tool))))
        (signal 'efrit-sandbox-denied
                (list (efrit-sandbox-request-create
                       :cap cap :target ctarget :tool tool
                       :detail "the user asked a question this turn: answer it; do not change, run or evaluate anything"))))
       ((and (not (eq cap 'shell)) (efrit-sandbox--always-denied-p ctarget))
        (efrit-log 'warn "sandbox: %s on %s is always denied" cap ctarget)
        (signal 'efrit-sandbox-denied
                (list (efrit-sandbox-request-create
                       :cap cap :target ctarget :tool tool
                       :detail (format "%s is protected and can never be granted" ctarget)))))
       ;; a shell command in a remote root runs on that host and can
       ;; touch anything there: the host's write policy governs it
       ((and (eq cap 'shell) (file-remote-p root)
             (eq (efrit-sandbox-remote-policy root 'write) 'deny))
        (efrit-log 'warn "sandbox: shell on host %s denied by policy" (efrit-sandbox-remote-host root))
        (when (fboundp 'efrit-publish)
          (efrit-publish 'sandbox-denied `((:cap . ,cap) (:target . ,ctarget) (:tool . ,tool))))
        (signal 'efrit-sandbox-denied
                (list (efrit-sandbox-request-create
                       :cap cap :target ctarget :tool tool
                       :detail (format "shell commands on host %s are denied by efrit-sandbox-remote-hosts (write policy)"
                                       (efrit-sandbox-remote-host root))))))
       ;; a remote host whose policy for this capability is deny
       ((and (memq cap '(read write)) (efrit-sandbox-remote-p ctarget)
             (eq (efrit-sandbox-remote-policy ctarget cap) 'deny))
        (efrit-log 'warn "sandbox: %s on %s denied by the policy for host %s"
                   cap ctarget (efrit-sandbox-remote-host ctarget))
        (when (fboundp 'efrit-publish)
          (efrit-publish 'sandbox-denied `((:cap . ,cap) (:target . ,ctarget) (:tool . ,tool))))
        (signal 'efrit-sandbox-denied
                (list (efrit-sandbox-request-create
                       :cap cap :target ctarget :tool tool
                       :detail (format "%s access to host %s is denied by efrit-sandbox-remote-hosts"
                                       cap (efrit-sandbox-remote-host ctarget))))))
       ((efrit-sandbox-allowed-p cap ctarget root)
        (efrit-log 'debug "sandbox: allowed %s %s (%s)" cap
                   (if (stringp ctarget) (efrit-sandbox-abbreviate ctarget) ctarget) tool)
        t)
       ;; expected: a grant for the session and a note, no menu
       ((let ((reason (efrit-sandbox-expected-p cap ctarget root)))
          (and reason (efrit-sandbox--grant-expected cap ctarget root reason tool))))
       ;; the reviewer vouched for this request at or above the user's
       ;; threshold (efrit-review-confidence): its word stands for the
       ;; user's `s' or `o', and the transcript says so
       ((when-let* ((vouch (and (fboundp 'efrit-review-confidence-take-grant)
                                (efrit-review-confidence-take-grant cap ctarget))))
          (let ((scope (plist-get vouch :scope)))
            (efrit-sandbox-grant cap (efrit-sandbox--suggest-target cap ctarget root) scope root)
            (efrit-log 'info "sandbox: %s %s granted by the reviewer (%.2f: %s) for the %s (%s)"
                       cap (if (stringp ctarget) (efrit-sandbox-abbreviate ctarget) ctarget)
                       (plist-get vouch :confidence) (plist-get vouch :why) scope tool)
            (when (fboundp 'efrit-publish)
              (efrit-publish 'note `((:text . ,(format "⛨ %s: %s, allowed by the reviewer (%.2f: %s)"
                                                        (or tool "a tool")
                                                        (efrit-sandbox-describe-request
                                                         (efrit-sandbox-request-create :cap cap :target ctarget))
                                                        (plist-get vouch :confidence) (plist-get vouch :why)))
                                     (:face . shadow) (:kind . sandbox)))
              (efrit-publish 'sandbox-reviewer-grant `((:cap . ,cap) (:target . ,ctarget)
                                                        (:confidence . ,(plist-get vouch :confidence)))))
            (efrit-sandbox-allowed-p cap ctarget root))))
       (t
        (efrit-log 'debug "sandbox: %s %s not covered for %s; asking (%s)" cap
                   (if (stringp ctarget) (efrit-sandbox-abbreviate ctarget) ctarget)
                   (efrit-sandbox-abbreviate root) tool)
        (let* ((req (efrit-sandbox-request-create
                     :cap cap
                     :target (efrit-sandbox--suggest-target cap ctarget root)
                     :tool tool :detail detail))
               (scope (and (efrit-sandbox--request-function)
                           ;; a standing N from earlier this turn: no prompt
                           (not (eq (efrit-sandbox-turn-answer) 'deny-all))
                           (efrit-sandbox--ask-without-clock req))))
          ;; what the user answered feeds the reviewer's confidence next time
          (when (and (fboundp 'efrit-grant-history-record)
                     (not (eq (efrit-sandbox-turn-answer) 'abort)))
            (ignore-errors (efrit-grant-history-record cap ctarget scope)))
          ;; q in the prompt: this tool is interrupted, the loop ends
          ;; the turn the way it does for C-g
          (when (eq (efrit-sandbox-turn-answer) 'abort)
            ;; one quit ends the turn; do not keep quitting into the next
            (efrit-sandbox--turn-set :answer nil)
            (efrit-log 'info "sandbox: turn aborted by the user at %s %s (%s)" cap ctarget tool)
            (when (fboundp 'efrit-publish)
              (efrit-publish 'sandbox-denied `((:cap . ,cap) (:target . ,ctarget) (:tool . ,tool)
                                               (:abort . t))))
            (signal 'quit nil))
          ;; an exact-line shell grant is never standing: whatever the
          ;; prompt returned, it applies to this run only.  Same for a
          ;; remote host whose policy is `once'.
          (when (and (memq scope '(session project))
                     (efrit-sandbox-request-once-only-p req))
            (setq scope 'once))
          ;; An edited input applies to this run only, whatever scope
          ;; was chosen: the grant would cover the original line
          (when (efrit-sandbox-request-edited req)
            (efrit-sandbox--turn-set :edited (cons tool (efrit-sandbox-request-edited req)))
            (when (memq scope '(session project)) (setq scope 'once)))
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

(defun efrit-sandbox-request-exact-line-p (req)
  "Non-nil if REQ is an always-ask shell line (granted exactly, once,
after the line is confirmed)."
  (let ((target (efrit-sandbox-request-target req)))
    (and (eq (efrit-sandbox-request-cap req) 'shell)
         (consp target) (eq (car target) 'command))))

(defun efrit-sandbox-request-once-only-p (req)
  "Non-nil if REQ can only ever be granted once: an always-ask shell
line, or a remote file whose host policy is `once'."
  (let ((target (efrit-sandbox-request-target req))
        (cap (efrit-sandbox-request-cap req)))
    (or (efrit-sandbox-request-exact-line-p req)
        (and (memq cap '(read write)) (efrit-sandbox-remote-p target)
             (eq (efrit-sandbox-remote-policy target cap) 'once))
        ;; a shell grant in a remote root whose write policy is once
        (and (eq cap 'shell)
             (let ((root (efrit-sandbox-project-root)))
               (and (file-remote-p root)
                    (eq (efrit-sandbox-remote-policy root 'write) 'once)))))))

(defun efrit-sandbox--target-label (target)
  "A short human label for a grant TARGET (path, (buffer . NAME), or a shell target)."
  (cond ((and (consp target) (eq (car target) 'buffer))
         (format "buffer %s" (cdr target)))
        ((efrit-sandbox-shell-target-p target)
         (mapconcat #'identity (cdr target) ", "))
        ((and (consp target) (eq (car target) 'command))
         (format "exactly: %s" (cdr target)))
        ((efrit-sandbox-host-target-p target) (cdr target))
        ((stringp target) (efrit-sandbox-abbreviate target))
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
