;;; efrit-review.el --- Second-model review of proposed tool calls -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.8.5
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; External verification, in the shape that pays for itself: the model
;; that does the work (the proposer) does not get to be the only judge
;; of whether the work matches what the user asked.  Before a turn's
;; mutating tool calls run, a second call (the reviewer) sees the
;; user's request, the proposer's own words from this turn, and the
;; exact tool calls with their inputs, and answers approve or reject
;; with a reason.  A rejection is fed back to the proposer as a failed
;; tool_result, in the same shape as a sandbox denial, and the turn
;; continues: the proposer revises or explains.
;;
;; What the reviewer does NOT see, on purpose: tool outputs and the
;; rest of the conversation.  Tool outputs are the prompt-injection
;; surface (a file the proposer read may carry text aimed at whoever
;; reads it next), and the reviewer's judgement is only worth having
;; if it is formed from the user's intent and the proposed actions
;; alone.  This also keeps the review request small (a few thousand
;; tokens), so the added cost per turn is a fraction of the main call.
;;
;; What the reviewer is NOT: consent.  The sandbox prompt remains the
;; human approval for scope; a reviewer's "approve" grants nothing.
;; And it is not a third party: with the default configuration it is
;; the same model family as the proposer, which catches slips and
;; misread intent, not shared misjudgement.  `efrit-review-model' can
;; point at a different model; independence is a configuration choice,
;; not something this file can manufacture.
;;
;; Only tool calls in the classes `efrit-review-classes' (default:
;; write and exec, per `efrit-permission-tool-classes') are reviewed.
;; A turn with none of them is not reviewed.  Reads and efrit's own
;; control tools never are.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'json)
(require 'efrit-log)
(require 'efrit-api)
(require 'efrit-chat-response)
(require 'efrit-permissions)   ; efrit-permission-tool-class
(require 'efrit-events)
(require 'efrit-settings)

(defvar efrit-default-model)

(defgroup efrit-review nil
  "Second-model review of proposed tool calls."
  :group 'efrit
  :prefix "efrit-review-")

(defcustom efrit-review-enabled t
  "When non-nil, mutating tool calls are reviewed by a second model call
before they run.  On by default: the cost is one small request per
mutating turn, and it catches edits that drift from what was asked.
Set to nil for throwaway work where speed matters more."
  :type 'boolean
  :group 'efrit-review)

(defcustom efrit-review-model nil
  "Model the reviewer uses, or nil for `efrit-default-model'.
A different model than the proposer gives a more independent second
opinion; the same model still catches slips and misread intent."
  :type '(choice (const :tag "Same as proposer" nil) string)
  :group 'efrit-review)

(defcustom efrit-review-classes '(write exec net)
  "Permission classes of tool calls that are reviewed.
Drawn from `efrit-permission-tool-classes'.  Tools of other classes
\(reads, efrit's control tools) run without review; the transcript
says so for each turn, so silence never means \"forgot\"."
  :type '(set (const write) (const exec) (const net) (const read))
  :group 'efrit-review)

(defun efrit-review-skip-reason (content)
  "Why CONTENT is not reviewed: a short phrase, or nil when nothing needs saying.
Published as `review-skipped' by the loop so the transcript can show it.
Nil for a reviewed turn, a turn with no tools, and a turn of only
efrit's control tools (session_complete, todo_write): those never
carry an action worth judging, and a note there is noise."
  (let* ((uses (efrit-review--tool-uses content))
         (judgeable (cl-remove-if
                     (lambda (u) (eq (efrit-permission-tool-class (nth 1 u)) 'control))
                     uses)))
    (cond
     ((null judgeable) nil)
     ((not (efrit-review-enabled-p)) "review off")
     ((not (efrit-review-applies-p content))
      (format "read-only turn (%s)"
              (mapconcat #'identity
                         (delete-dups (mapcar (lambda (u) (nth 1 u)) judgeable))
                         ", ")))
     (t nil))))

(defcustom efrit-review-max-rejections 3
  "Rejected batches in one turn after which the turn is handed to the user.
Counts the *same* proposal coming back: a batch is a repeat when it
names the same tools and files as a batch already rejected this turn.
A rejection of something else (an ancillary edit beside the main
work) is a denial the model reads and moves on from, like a sandbox
denial; it does not end the turn.  The count starts afresh with each
turn (2026-10-01: two rejections in two different turns ended the
second turn at its first tool)."
  :type 'integer
  :group 'efrit-review)

(defcustom efrit-review-on-failure '((exec . reject) (t . approve))
  "What to do when the review call itself fails (network, timeout, malformed).
An alist from permission class to `approve' or `reject'; the entry
for t is the fallback.  A bare symbol applies to every class.

`approve' lets the turn run and logs the failure; `reject' feeds the
failure back to the proposer as a rejection.  The default fails
closed for `exec' (a shell command or eval that nobody reviewed is
the one place an outage can do damage) and open for the rest, so a
review outage does not stall edits the sandbox already gates."
  :type '(choice (const approve) (const reject)
                 (alist :key-type (choice (const write) (const exec) (const net)
                                          (const read) (const t))
                        :value-type (choice (const approve) (const reject))))
  :group 'efrit-review)

(defun efrit-review-failure-policy (classes)
  "The failure verdict for a batch whose tool calls have CLASSES.
`reject' if any class maps to reject in `efrit-review-on-failure'."
  (let ((policy efrit-review-on-failure))
    (if (symbolp policy)
        policy
      (if (cl-some (lambda (class)
                     (eq 'reject (alist-get class policy (alist-get t policy 'approve))))
                   (or classes '(t)))
          'reject
        'approve))))

(defcustom efrit-review-max-input-chars 4000
  "Longest single tool input shown to the reviewer; longer ones are cut.
The reviewer needs the shape of an action, not a whole file."
  :type 'integer
  :group 'efrit-review)

(defconst efrit-review-verdicts '(approve reject)
  "Verdicts the reviewer may return.")

(defconst efrit-review-rejected-prefix "Error review rejected: "
  "Prefix of a tool_result for a rejected tool call.  Parallel to
`efrit-sandbox-denied-prefix', so the agent buffer can render it as a
review outcome rather than a tool failure.")

;;; Deciding whether a turn is reviewed

(defun efrit-review--tool-uses (content)
  "The (tool-id tool-name input) triples in CONTENT, a content vector."
  (let ((uses nil))
    (dotimes (i (length content))
      (when-let* ((use (efrit-content-item-as-tool-use (aref content i))))
        (push use uses)))
    (nreverse uses)))

;;; Per-project overrides (the "review" section of .efrit/settings.json)
;;
;;   "review": {"enabled": false, "classes": ["write", "exec"]}
;;
;; Either key may be absent; the customization value applies then.

(defconst efrit-review-settings-section "review"
  "The section of the project settings file this module owns.")

(defconst efrit-review-all-classes '(write exec net read)
  "Classes a project may put in its review list.")

(defun efrit-review--project-section (&optional root)
  (efrit-settings-get (or root (efrit-settings-project-root)) efrit-review-settings-section))

(defun efrit-review-enabled-p (&optional root)
  "Whether review is on for ROOT: the project override, else `efrit-review-enabled'."
  (let ((flag (plist-get (efrit-review-project-override root) :enabled)))
    (if (eq flag 'unset) efrit-review-enabled flag)))

(defun efrit-review-effective-classes (&optional root)
  "Classes reviewed for ROOT: the project override, else `efrit-review-classes'."
  (or (plist-get (efrit-review-project-override root) :classes)
      efrit-review-classes))

(defun efrit-review-project-override (&optional root)
  "The project override for ROOT as a plist (:enabled BOOL-OR-unset :classes LIST-OR-nil)."
  (let ((section (efrit-review--project-section root)))
    (list :enabled (if (hash-table-p section)
                       (efrit-settings-json-bool (gethash "enabled" section 'unset))
                     'unset)
          :classes (and (hash-table-p section)
                        (efrit-settings-symbol-list (gethash "classes" section)
                                                    efrit-review-all-classes)))))

(defun efrit-review-set-project-override (enabled classes &optional root)
  "Write ROOT's review override: ENABLED is t, nil or `unset'; CLASSES a list or nil.
Both unset removes the section."
  (let ((root (or root (efrit-settings-project-root)))
        (h (make-hash-table :test 'equal)))
    (unless (eq enabled 'unset) (puthash "enabled" (if enabled t :false) h))
    (when classes (puthash "classes" (mapcar #'symbol-name classes) h))
    (efrit-settings-put root efrit-review-settings-section
                        (and (> (hash-table-count h) 0) h))))

(defun efrit-review--reviewable-p (tool-name)
  "Non-nil if a call to TOOL-NAME is in a reviewed class for the current project."
  (memq (efrit-permission-tool-class tool-name) (efrit-review-effective-classes)))

(defvar efrit-review--user-allowed (make-hash-table :test 'equal)
  "Session id -> list of batch signatures the user allowed past the reviewer.
Set when the user answers a review escalation with \"allow\"; the
batch then runs without review, once (see `efrit-review-user-allow').")

(defun efrit-review-user-allow (session-id signature)
  "Let SIGNATURE's batch run unreviewed the next time SESSION-ID proposes it."
  (push signature (gethash session-id efrit-review--user-allowed)))

(defun efrit-review--user-allowed-p (session-id content)
  "Non-nil (and consumed) when the user allowed CONTENT's batch for SESSION-ID."
  (let* ((sig (efrit-review-batch-signature content))
         (allowed (gethash session-id efrit-review--user-allowed)))
    (when (member sig allowed)
      (puthash session-id (cl-remove sig allowed :test #'equal :count 1) efrit-review--user-allowed)
      t)))

(defun efrit-review-answer-escalation (session-id meta answer)
  "Turn the user's ANSWER to a review escalation into the model's next input.
META is the plist the loop stored with the question (:signature
:count :reason).  \"Allow\" lets the same batch run unreviewed once
and tells the model to go ahead; \"drop\" tells it to continue the
task without that action; \"stop\" tells it to stop and report; any
other text is the user's own steer, passed through."
  (let ((a (downcase (string-trim answer))))
    (cond
     ((string-prefix-p "allow" a)
      (efrit-review-user-allow session-id (plist-get meta :signature))
      "The user allows the action the reviewer rejected. Do it now, once, then continue the task.")
     ((string-prefix-p "drop" a)
      "The user says: drop that action and continue the task without it. Do not propose it again this turn.")
     ((string-prefix-p "stop" a)
      "The user says: stop here. Summarise what was done and what was not, then end the turn.")
     (t answer))))

(defun efrit-review-applies-p (content &optional session-id)
  "Non-nil if CONTENT (a response content vector) has a reviewable tool call.
With SESSION-ID, a batch the user allowed after an escalation passes
without review, once."
  (and (efrit-review-enabled-p)
       (cl-some (lambda (use) (efrit-review--reviewable-p (nth 1 use)))
                (efrit-review--tool-uses content))
       (not (and session-id (efrit-review--user-allowed-p session-id content)))))

;;; What the reviewer sees

(defun efrit-review--message-text (message)
  "Plain text of a user MESSAGE (an alist), or nil if it is tool results.
Content is a string, or a vector of blocks; only text blocks count."
  (let ((content (alist-get 'content message)))
    (cond
     ((stringp content) content)
     ((vectorp content)
      (let ((texts nil))
        (dotimes (i (length content))
          (let ((block (aref content i)))
            (cond
             ((and (hash-table-p block) (equal (gethash "type" block) "text"))
              (push (gethash "text" block) texts))
             ((and (listp block) (equal (alist-get 'type block) "text"))
              (push (alist-get 'text block) texts)))))
        (and texts (string-join (nreverse texts) "\n"))))
     (t nil))))

(defun efrit-review-user-intent (messages)
  "The user's most recent request text in MESSAGES (a vector or list), or nil.
Walks backwards past tool_result messages, which are also role user
but carry no request.  A context block (<editor-context>) prepended
by the REPL is stripped so the reviewer reads what the user typed."
  (let ((list (append messages nil))
        (found nil))
    (dolist (m (reverse list))
      (when (and (not found)
                 (equal (alist-get 'role m) "user"))
        (when-let* ((text (efrit-review--message-text m)))
          (setq found
                (if (string-match "</editor-context>\n*" text)
                    (substring text (match-end 0))
                  text)))))
    (and found (not (string-empty-p (string-trim found))) found)))

(defun efrit-review--proposer-text (content)
  "The proposer's own text blocks in CONTENT joined, or nil."
  (let ((texts nil))
    (dotimes (i (length content))
      (let ((item (aref content i)))
        (when (and (hash-table-p item) (equal (gethash "type" item) "text"))
          (push (gethash "text" item) texts))))
    (and texts (string-join (nreverse texts) "\n"))))

(defun efrit-review--input-string (input)
  "INPUT (a hash table) as compact JSON, cut to `efrit-review-max-input-chars'."
  (let ((s (condition-case nil
               (json-encode input)
             (error (format "%S" input)))))
    (if (> (length s) efrit-review-max-input-chars)
        (concat (substring s 0 efrit-review-max-input-chars)
                (format " …[%d more chars]" (- (length s) efrit-review-max-input-chars)))
      s)))

(declare-function efrit-sandbox-shell-starts-emacs-p "efrit-sandbox")
(declare-function efrit-review-flags-for-use "efrit-review-flags")

(defun efrit-review--flags (use)
  "FLAG lines the reviewer should weigh for tool USE, joined, or nil.
A shell line that starts another Emacs needs the agent's reason; a
Lisp edit or eval is flagged for each effect it adds (keys, hooks,
advice, core variables, `:vc' blocks, shadowed names, loads, side
effects), see efrit-review-flags."
  (let* ((input (nth 2 use))
         (lines
          (append
           (when (and (equal (nth 1 use) "shell_exec") (hash-table-p input)
                      (fboundp 'efrit-sandbox-shell-starts-emacs-p)
                      (efrit-sandbox-shell-starts-emacs-p (gethash "command" input)))
             (list "[FLAG shell: starts another Emacs from the shell. The agent must have said why this cannot be done in the running Emacs with eval_sexp; stubbing the user's own macros or libraries to make the batch run is not a reason. Without a stated reason, reject and say so.]"))
           (when (require 'efrit-review-flags nil t)
             (ignore-errors (efrit-review-flags-for-use use))))))
    (and lines (mapconcat #'identity lines "\n   "))))

(defun efrit-review-describe-batch (content)
  "The reviewable tool calls in CONTENT as numbered text for the reviewer.
Tool outputs are never included: see the commentary.  Calls the
reviewer should look at twice carry a FLAG line."
  (let ((n 0))
    (mapconcat
     (lambda (use)
       (cl-incf n)
       (concat (format "%d. %s %s" n (nth 1 use) (efrit-review--input-string (nth 2 use)))
               (when-let* ((flag (efrit-review--flags use))) (concat "\n   " flag))))
     (cl-remove-if-not (lambda (use) (efrit-review--reviewable-p (nth 1 use)))
                       (efrit-review--tool-uses content))
     "\n")))

;;; The review request

(defconst efrit-review--system-prompt
  "You review actions an AI coding agent proposes inside a user's Emacs, before they run.
You see: the user's standing instructions (their AGENTS.md / CLAUDE.md files, when any),
the user's request, the agent's own words this turn, and the exact tool calls.
You do not see tool outputs or earlier turns. Judge only from what is shown.

The standing instructions are the user's own and apply to every turn: an action they
call for (keeping a work log, a clock file, a notes file, a checkpoint) is asked for
even when the request of the moment does not mention it.

Lines marked [FLAG kind: …] under a call are facts about what the Lisp it writes or
evaluates does, read statically: key = binds a key; hook / advice = changes what other
code runs; core-var = sets a variable that changes how Emacs behaves; vc = a use-package
:vc block (a remote repository, branch or revision the config will fetch and run); shadow =
defines a name that belongs to another library; load = loads code; effect = deletes,
writes, or starts a process; shell = starts another Emacs.  A flag is not a verdict: weigh
it against the request.  A change the user asked for that rebinds a key is fine; a :vc
block pointing at a new remote, a shadowed macro, or an advice on a core function that the
request did not call for is not, and a flagged batch-Emacs line needs the agent's stated
reason (in AGENT SAID THIS TURN).  Say which flag decided it.

Reject a batch when any call:
- does something the user did not ask for -- in the request or the standing instructions --
  or would plausibly object to (deleting, overwriting unrelated files, changing configuration,
  sending data out, running commands with effects beyond the task);
- contradicts the agent's own stated plan;
- is destructive with no evident reason;
- touches secrets, credentials, or another project.
Approve otherwise. Ordinary imperfection is not a reason to reject; the user reviews results.

Answer with one JSON object and nothing else -- no prose before it, no code fence:
{\"verdict\": \"approve\"} or {\"verdict\": \"reject\", \"reason\": \"<one or two sentences, addressed to the agent, saying what to change>\"}"
  "System prompt for the reviewer.")

(defvar efrit-review--last-refused-request nil
  "The last review request the endpoint refused, for `efrit-review-probe-refusal'.")

(defcustom efrit-review-instructions-max-chars 4000
  "Longest standing-instructions block shown to the reviewer; the head is kept."
  :type 'integer
  :group 'efrit-review)

(declare-function efrit-instructions-text "efrit-instructions")

(defun efrit-review--standing-instructions ()
  "The user's instruction files as the reviewer should see them, or nil.
The same block the agent gets (`efrit-instructions-text'), cut to
`efrit-review-instructions-max-chars'.  On 2026-10-01 the reviewer
rejected an edit to the work log that tzz's CLAUDE.md requires on
every turn, because it only saw the request of the moment."
  (when (require 'efrit-instructions nil t)
    (when-let* ((text (ignore-errors (efrit-instructions-text))))
      (if (> (length text) efrit-review-instructions-max-chars)
          (concat (substring text 0 efrit-review-instructions-max-chars)
                  (format "\n… [%d more chars]" (- (length text) efrit-review-instructions-max-chars)))
        text))))

(defun efrit-review--user-message (intent proposer-text batch)
  "Assemble the reviewer's single user message."
  (concat
   (when-let* ((standing (efrit-review--standing-instructions)))
     (concat "USER'S STANDING INSTRUCTIONS:\n" standing "\n\n"))
   "USER REQUEST:\n" (or intent "(not available)")
   "\n\nAGENT SAID THIS TURN:\n" (or proposer-text "(nothing)")
   "\n\nPROPOSED TOOL CALLS:\n" batch))

(defun efrit-review--request-data (intent proposer-text batch)
  "The API request for one review.  No tools: the reviewer only answers."
  `(("model" . ,(or efrit-review-model efrit-default-model))
    ("max_tokens" . 1024)
    ("system" . ,(efrit-api-cacheable-system efrit-review--system-prompt))
    ("messages" . [(("role" . "user")
                    ("content" . ,(efrit-review--user-message intent proposer-text batch)))])))

(defun efrit-review-parse-verdict (text)
  "Parse the reviewer's TEXT into (VERDICT . REASON), or nil if malformed.
VERDICT is a symbol from `efrit-review-verdicts'.  Tolerates prose
around the object by taking the first {...} span."
  (when (stringp text)
    ;; The first balanced {...}: the greedy span to the LAST brace broke
    ;; when the reviewer added a second object or an example after its
    ;; verdict, and every review then failed as malformed (2026-09-25).
    (let ((start (string-search "{" text)) (obj nil))
      (while (and start (not obj))
        (let ((depth 0) (i start) (end nil) (in-string nil) (escaped nil))
          (while (and (< i (length text)) (not end))
            (let ((c (aref text i)))
              (cond
               (escaped (setq escaped nil))
               ((and in-string (eq c ?\\)) (setq escaped t))
               ((eq c ?\") (setq in-string (not in-string)))
               ((and (not in-string) (eq c ?{)) (cl-incf depth))
               ((and (not in-string) (eq c ?}))
                (cl-decf depth)
                (when (zerop depth) (setq end (1+ i))))))
            (cl-incf i))
          (when end
            (condition-case nil
                (let* ((parsed (json-parse-string (substring text start end) :object-type 'alist))
                       (verdict (alist-get 'verdict parsed))
                       (reason (alist-get 'reason parsed))
                       (sym (and (stringp verdict) (intern (downcase verdict)))))
                  (when (memq sym efrit-review-verdicts)
                    (setq obj (cons sym (and (stringp reason) reason)))))
              (error nil)))
          (setq start (and (not obj) (string-search "{" text (1+ start))))))
      obj)))

(defun efrit-review--response-text (response)
  "The concatenated text of RESPONSE's content blocks."
  (let ((content (efrit-response-content response)) (texts nil))
    (when content
      (dotimes (i (length content))
        (let ((item (aref content i)))
          (when (and (hash-table-p item) (equal (gethash "type" item) "text"))
            (push (gethash "text" item) texts)))))
    (string-join (nreverse texts) "")))

(defun efrit-review--failure-verdict (why &optional classes)
  "The verdict used when the review call fails, per `efrit-review-on-failure'.
CLASSES are the permission classes of the batch under review."
  (let ((policy (efrit-review-failure-policy classes)))
    (efrit-log 'warn "review: call failed (%s); policy %s for %s" why policy classes)
    (if (eq policy 'reject)
        (cons 'reject (format "the review could not be completed (%s); nothing was run" why))
      (cons 'approve nil))))

(defun efrit-review--batch-classes (content)
  "The distinct reviewable permission classes of the tool calls in CONTENT."
  (delete-dups
   (delq nil (mapcar (lambda (use)
                       (let ((class (efrit-permission-tool-class (nth 1 use))))
                         (and (memq class (efrit-review-effective-classes)) class)))
                     (efrit-review--tool-uses content)))))

(defun efrit-review-turn (session-id messages content callback)
  "Review the reviewable tool calls in CONTENT for SESSION-ID, asynchronously.
MESSAGES is the conversation so far (used only to find the user's
request).  CALLBACK is called with (VERDICT . REASON), VERDICT being
`approve' or `reject'.  It is called exactly once, on the main loop."
  (let* ((intent (efrit-review-user-intent messages))
         (proposer (efrit-review--proposer-text content))
         (batch (efrit-review-describe-batch content))
         (classes (efrit-review--batch-classes content))
         (request (efrit-review--request-data intent proposer batch))
         (done nil)
         (prompt (efrit-review--user-message intent proposer batch))
         (finish (lambda (verdict)
                   (unless done
                     (setq done t)
                     (efrit-log 'info "review %s: %s%s" session-id (car verdict)
                                (if (cdr verdict) (format " (%s)" (cdr verdict)) ""))
                     (efrit-publish 'review-verdict
                                    `((:session-id . ,session-id)
                                      (:verdict . ,(car verdict))
                                      (:reason . ,(cdr verdict))
                                      (:prompt . ,prompt)))
                     (funcall callback verdict)))))
    (efrit-publish 'review-start `((:session-id . ,session-id)
                                   (:calls . ,(length (split-string batch "\n" t)))
                                   (:model . ,(or efrit-review-model efrit-default-model))
                                   (:prompt . ,prompt)))
    (condition-case err
        (let ((efrit-api-request-purpose
               (format "reviewing %d proposed tool call(s) before they run"
                       (length (split-string batch "\n" t)))))
          (efrit-api-request-async
           request
         (lambda (response)
           (funcall finish
                    (cond
                     ((null response) (efrit-review--failure-verdict "no response" classes))
                     ((efrit-response-error response)
                      (efrit-review--failure-verdict
                       (efrit-error-message (efrit-response-error response)) classes))
                     ;; The endpoint refused the review request itself
                     ;; (stop_reason refusal, no text).  That is an
                     ;; outage of the reviewer, not a judgement of the
                     ;; calls; say so, and keep the request for a probe.
                     ((efrit-api--refused-p response)
                      (setq efrit-review--last-refused-request request)
                      (efrit-log 'warn "review %s: the endpoint refused the review request; M-x efrit-review-probe-refusal bisects it" session-id)
                      (efrit-review--failure-verdict "the endpoint refused the review request" classes))
                     (t (let ((text (efrit-review--response-text response)))
                          (efrit-log 'debug "review %s: reviewer said: %s" session-id
                                     (truncate-string-to-width text 400 nil nil "…"))
                          (or (efrit-review-parse-verdict text)
                              (progn
                                (efrit-log 'warn "review %s: not a verdict: %s" session-id
                                           (truncate-string-to-width text 400 nil nil "…"))
                                (efrit-review--failure-verdict "malformed verdict" classes))))))))
         (lambda (error-msg)
           (funcall finish (efrit-review--failure-verdict error-msg classes)))))
      (error
       (funcall finish (efrit-review--failure-verdict (error-message-string err) classes))))))

;;; Refusal probe
;;
;; A route that pre-filters requests can refuse the review call while
;; the proposer's own calls go through.  On 2026-09-20 the trigger was
;; a line of comma-separated char codes; on 2026-09-25 every review of
;; an `eval_sexp' batch was refused.  Guessing at the cause was wrong
;; every time; bisecting the message is not.


(defun efrit-review--probe-send (message)
  "Send MESSAGE as the reviewer's user message; return `refused', `ok' or an error string."
  (condition-case err
      (let* ((req `(("model" . ,(or efrit-review-model efrit-default-model))
                    ("max_tokens" . 16)
                    ("messages" . [(("role" . "user") ("content" . ,message))])))
             (efrit-api-request-purpose "review refusal probe")
             (r (efrit-api-request-sync req 60)))
        (if (efrit-api--refused-p r) 'refused 'ok))
    (error (error-message-string err))))

(defun efrit-review-probe-refusal ()
  "Bisect the last refused review request down to the lines that trigger it.
Sends the reviewer's user message without a system prompt, then
halves, until one line (or an inseparable pair) remains.  Costs a few
tiny requests.  Shows the result in a popup."
  (interactive)
  (unless efrit-review--last-refused-request
    (user-error "No refused review request recorded yet"))
  (require 'efrit-ui-helpers)
  (let* ((msg (alist-get "content" (aref (alist-get "messages" efrit-review--last-refused-request nil nil #'equal) 0)
                         nil nil #'equal))
         (lines (split-string msg "\n"))
         (log nil)
         (note (lambda (fmt &rest args) (push (apply #'format fmt args) log))))
    (funcall note "whole message (%d lines): %s" (length lines) (efrit-review--probe-send msg))
    (funcall note "system prompt alone: %s" (efrit-review--probe-send efrit-review--system-prompt))
    (when (eq 'refused (efrit-review--probe-send msg))
      (let ((suspect lines))
        (while (> (length suspect) 1)
          (let* ((half (/ (length suspect) 2))
                 (a (seq-take suspect half)) (b (seq-drop suspect half))
                 (ra (efrit-review--probe-send (string-join a "\n")))
                 (rb (efrit-review--probe-send (string-join b "\n"))))
            (funcall note "%d lines: first half %s, second half %s" (length suspect) ra rb)
            (setq suspect (cond ((eq ra 'refused) a)
                                ((eq rb 'refused) b)
                                (t (funcall note "neither half alone is refused: the trigger needs both") nil)))))
        (when suspect
          (funcall note "TRIGGER: %S" (car suspect)))))
    (efrit-show-popup "*efrit-review-probe*"
                      (concat "Review refusal probe\n\n" (string-join (nreverse log) "\n") "\n\n--- message ---\n" msg))))

;;; Consecutive rejections

(defvar efrit-review--rejections (make-hash-table :test 'equal)
  "Session ID -> number of consecutive rejected turns.")

(defun efrit-review-batch-signature (content)
  "What a batch in CONTENT proposes, as a comparable key: tool names with their files.
Two batches with the same signature are the model re-proposing the
same thing after a rejection."
  (sort (mapcar (lambda (use)
                  (let* ((input (nth 2 use))
                         (path (and (hash-table-p input)
                                    (or (gethash "path" input) (gethash "file" input)
                                        (gethash "command" input) (gethash "expr" input)))))
                    (format "%s %s" (nth 1 use) (or path ""))))
                (efrit-review--tool-uses content))
        #'string<))

(defun efrit-review-note-verdict (session-id verdict &optional signature)
  "Record VERDICT for SESSION-ID's turn; return how many times SIGNATURE was rejected.
An approval of anything clears the turn's record.  Without SIGNATURE
(old callers) every rejection counts as the same proposal."
  (let ((key (or signature '(same))))
    (if (eq verdict 'reject)
        (let* ((table (or (gethash session-id efrit-review--rejections)
                          (puthash session-id (make-hash-table :test 'equal) efrit-review--rejections)))
               (n (1+ (gethash key table 0))))
          (puthash key n table)
          n)
      (remhash session-id efrit-review--rejections)
      0)))

(defun efrit-review-forget-session (session-id)
  "Drop SESSION-ID's rejection record.  Called when a turn begins or ends."
  (remhash session-id efrit-review--rejections))

;;; What the proposer is told

(defun efrit-review-rejected-tool-result (reason)
  "The tool_result text for a call rejected by the reviewer for REASON.
Like a sandbox denial, it says what happened and that the turn goes on."
  (format "%s%s If this call was ancillary to the user's request, carry on with the request without it; otherwise revise the approach, or explain to the user why the action is needed and stop."
          efrit-review-rejected-prefix
          (or reason "the reviewer rejected this action.")))

(provide 'efrit-review)

;;; efrit-review.el ends here
