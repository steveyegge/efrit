;;; efrit-review-confidence.el --- How sure the reviewer may be that the user would say yes -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.9.2
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, ai

;;; Commentary:

;; Two gates stand before a tool call: the reviewer (is this sensible
;; for the task?) and the sandbox (is this path, host or command
;; granted?).  Until 0.9.2 the second gate always asked the user when
;; nothing covered the request, even after the reviewer had approved
;; and even when the user had granted the same thing in session after
;; session.  tzz, 2026-10-03: "Give the reviewer a certainty threshold
;; that I would be OK with them approving something.  If it's over 95%
;; likely then just approve it.  This can be derived from past
;; interactions and from some basic rules."
;;
;; So, for each proposed call that the sandbox would prompt for, this
;; file computes the request it would make and a SCORE from facts:
;; the user's past answers to the same pattern (`efrit-grant-history'),
;; whether the target is named in the request, whether the command set
;; is read-only, whether the path lies in a repository the user has
;; granted before, whether the host was fetched before.  The facts and
;; the score go to the reviewer, who returns a confidence it may LOWER
;; but not raise, and a grant scope.  At or above
;; `efrit-review-auto-grant-threshold' the sandbox takes the reviewer's
;; word as the user's `s' (or `o'), with a note saying why.
;;
;; Hard limits that no score overrides: never a `project' (saved)
;; grant; never an always-ask shell line (rm -rf, sudo, git push…);
;; never a write outside every repository the user has ever granted;
;; never elisp that `efrit-review-flags' marks as shadowing, advising
;; or pulling a :vc; never a remote path; never after the user denied
;; the pattern more often than granted it.

;;; Code:

(require 'cl-lib)
(require 'efrit-grant-history)
(require 'efrit-log)

(declare-function efrit-sandbox-allowed-p "efrit-sandbox")
(declare-function efrit-sandbox-project-root "efrit-sandbox")
(declare-function efrit-sandbox--canonical-target "efrit-sandbox")
(declare-function efrit-sandbox--mentioned-p "efrit-sandbox")
(declare-function efrit-sandbox--expected-shell-line-p "efrit-sandbox")
(declare-function efrit-sandbox-shell-always-ask-match "efrit-sandbox")
(declare-function efrit-sandbox-shell-commands "efrit-sandbox")
(declare-function efrit-sandbox-remote-p "efrit-sandbox")
(declare-function efrit-sandbox-repo-of "efrit-sandbox")
(declare-function efrit-sandbox-abbreviate "efrit-sandbox")
(declare-function efrit-sandbox--suggest-target "efrit-sandbox")
(declare-function efrit-sandbox-canonical "efrit-sandbox")
(declare-function efrit-sandbox--turn-get "efrit-sandbox")
(declare-function efrit-sandbox--turn-set "efrit-sandbox")
(declare-function efrit-sandbox-host-under-p "efrit-sandbox")
(declare-function efrit-review-flags-for-use "efrit-review-flags")
(declare-function efrit-resolve-path-simple "efrit-tool-utils")
(declare-function efrit-publish "efrit-events")
(defvar efrit-sandbox-enabled)

(defgroup efrit-review-confidence nil
  "Letting the reviewer stand in for the user when it is sure enough."
  :group 'efrit-review)

(defcustom efrit-review-auto-grant-threshold 0.95
  "Reviewer confidence at or above which its approval also satisfies the sandbox.
nil turns the feature off: the reviewer judges, the sandbox asks as before."
  :type '(choice (const :tag "Off" nil) (number :tag "Threshold 0..1"))
  :group 'efrit-review-confidence)

(defcustom efrit-review-confidence-strong-history 6.0
  "Net history strength (see `efrit-grant-history-strength') that counts as strong.
With the default scope weights: three session grants, or two saved
grants, or a saved grant plus a session and a once, or six one-time
yeses -- all recent.  Sixty days later each is worth half."
  :type 'number
  :group 'efrit-review-confidence)

(defcustom efrit-review-confidence-weights
  '((history-strong . 0.60)     ; decayed, scope-weighted net >= efrit-review-confidence-strong-history
    (history-some . 0.35)       ; net > 0, below that
    (history-mixed . 0.10)      ; granted more than denied, but denied too
    (mentioned . 0.45)          ; the request names the target
    (read-only-shell . 0.35)    ; every command is in the read-only set
    (read-cap . 0.15)           ; a read, not a write / shell / net
    (granted-repo . 0.25)       ; inside a repository the user has granted before
    (known-host . 0.30)         ; a host fetched before
    (write-cap . -0.25)         ; a write starts lower
    (net-cap . -0.10)
    (shell-cap . -0.10)
    (elisp-cap . -0.40))
  "What each fact adds to the computed score.  The sum is clipped to 0..1.
Tuned so that one strong fact is never enough: a read granted three
times before in a repository granted before reaches the 0.95 line
\(0.60 + 0.25 + 0.15); a read-only shell line scores 0.25 on its own,
0.85 with a strong history, 0.95 only when the request also names
its target or the history is strong AND the line is named."
  :type '(alist :key-type symbol :value-type number)
  :group 'efrit-review-confidence)

;;;; The request a tool call would make

(defconst efrit-review-confidence--path-tools
  '(("read_file" read "path") ("file_info" read "paths") ("search_content" read "path")
    ("project_files" read "path") ("edit_file" write "path") ("create_file" write "path")
    ("format_file" write "path") ("undo_edit" write "path") ("show_location" read "file")
    ("imenu_symbols" read "file") ("xref_references" read "file") ("xref_apropos" read "file")
    ("treesit_info" read "file") ("get_diagnostics" read "path") ("vcs_diff" read "path")
    ("vcs_log" read "path") ("vcs_blame" read "path") ("vcs_status" read "path"))
  "(TOOL CAP INPUT-KEY) for tools whose sandbox request is a path.")

(defun efrit-review-confidence--url-host (url)
  (when (and (stringp url) (string-match "\\`https?://\\([^/:?#]+\\)" url))
    (downcase (match-string 1 url))))

(defun efrit-review-confidence-requests (use)
  "The (CAP . TARGET) requests tool USE, a (ID NAME INPUT) triple, would put to the sandbox.
Canonical targets as the sandbox sees them.  Empty for tools that
never prompt (todo_write, request_user_input…) and for eval_sexp, whose
requests come from running it."
  (let* ((name (nth 1 use)) (input (nth 2 use))
         (get (lambda (k) (and (hash-table-p input) (gethash k input)))))
    (cond
     ((equal name "shell_exec")
      (let ((line (funcall get "command")))
        (and (stringp line) (list (cons 'shell (string-trim line))))))
     ((member name '("fetch_url" "web_search"))
      (when-let* ((host (efrit-review-confidence--url-host (or (funcall get "url") ""))))
        (list (cons 'net (cons 'host host)))))
     ((member name '("edit_buffer" "editor_state")) nil)
     ((equal name "eval_sexp") nil)
     (t
      (when-let* ((spec (assoc name efrit-review-confidence--path-tools)))
        (let* ((cap (nth 1 spec)) (key (nth 2 spec)) (v (funcall get key))
               (paths (cond ((stringp v) (list v))
                            ((vectorp v) (append v nil))
                            ((listp v) v))))
          (delq nil
                (mapcar (lambda (p)
                          (when (stringp p)
                            (let ((abs (condition-case nil
                                           (if (file-name-absolute-p (expand-file-name p))
                                               (expand-file-name p (efrit-sandbox-project-root))
                                             p)
                                         (error p))))
                              (cons cap (efrit-sandbox--canonical-target cap abs)))))
                        paths))))))))

;;;; Facts and score

(defvar efrit-review-confidence--granted-repos-cache nil)

(defun efrit-review-confidence--granted-repo-p (path)
  "Non-nil when PATH lies inside a repository the user granted in any past session."
  (when-let* ((repo (efrit-sandbox-repo-of path)))
    (let ((st (or (efrit-grant-history-strength 'read repo)
                  (efrit-grant-history-strength 'write repo))))
      (and st (> (plist-get st :yes) 0) (= 0.0 (plist-get st :no))))))

(defun efrit-review-confidence-facts (cap target &optional use)
  "The facts about CAP on TARGET as (FACT . DETAIL) pairs, plus hard blockers.
A fact named `blocked' means no score may grant this."
  (let* ((facts nil)
         (st (efrit-grant-history-strength cap target))
         (yes (if st (plist-get st :yes) 0.0))
         (no (if st (plist-get st :no) 0.0))
         (net (- yes no)))
    (cl-flet ((fact (k &optional d) (push (cons k d) facts)))
      ;; hard blockers
      (when (and (stringp target) (efrit-sandbox-remote-p target))
        (fact 'blocked "remote path: the host policy decides"))
      (when (and (eq cap 'shell) (stringp target) (efrit-sandbox-shell-always-ask-match target))
        (fact 'blocked "always-ask shell line"))
      (when (> no yes)
        (fact 'blocked (format "the user's refusals outweigh their grants (%.1f against %.1f, decayed)" no yes)))
      (when (and (eq cap 'write) (stringp target)
                 (not (efrit-review-confidence--granted-repo-p target)))
        (fact 'blocked "a write outside every repository the user has granted before"))
      (when (and use (equal (nth 1 use) "eval_sexp"))
        (fact 'blocked "eval_sexp requests arise while it runs"))
      (when (and use (member (nth 1 use) '("edit_file" "create_file" "edit_buffer")))
        (let ((flags (ignore-errors (efrit-review-flags-for-use use))))
          (when (cl-some (lambda (f) (string-match-p "\\[FLAG \\(?:shadow\\|advice\\|vc\\|core-var\\|effect\\):" f)) flags)
            (fact 'blocked "the Lisp it writes shadows, advises, pulls a :vc or sets a core variable"))))
      ;; positive facts
      ;; a hair of decay since the last answer must not drop a fresh
      ;; 6.0 under the line
      (cond ((and (>= (+ net 0.01) efrit-review-confidence-strong-history) (= no 0.0))
             (fact 'history-strong (efrit-review-confidence--history-words st)))
            ((and (> net 0) (= no 0.0))
             (fact 'history-some (efrit-review-confidence--history-words st)))
            ((and (> net 0) (> no 0.0))
             (fact 'history-mixed (efrit-review-confidence--history-words st))))
      (when (ignore-errors (efrit-sandbox--mentioned-p cap target))
        (fact 'mentioned "named in the user's request"))
      (when (and (eq cap 'shell) (ignore-errors (efrit-sandbox--expected-shell-line-p target)))
        (fact 'read-only-shell (format "only %s" (string-join (efrit-sandbox-shell-commands target) ", "))))
      (when (and (memq cap '(read write)) (stringp target)
                 (efrit-review-confidence--granted-repo-p target))
        (fact 'granted-repo (format "inside %s, which the user granted before"
                                    (efrit-sandbox-abbreviate (efrit-sandbox-repo-of target)))))
      (when (and (eq cap 'net) (consp target))
        (let ((st (efrit-grant-history-strength 'net target)))
          (when (and st (> (plist-get st :yes) 0))
            (fact 'known-host (format "fetched from %s before" (cdr target))))))
      (pcase cap
        ('read (fact 'read-cap))
        ('write (fact 'write-cap))
        ('net (fact 'net-cap))
        ('shell (fact 'shell-cap))
        ('elisp (fact 'elisp-cap))))
    (nreverse facts)))

(defun efrit-review-confidence--history-words (st)
  "ST (from `efrit-grant-history-strength') in words for the reviewer and the note."
  (let* ((events (plist-get st :count))
         (last (plist-get st :last))
         (age (and last (ignore-errors (/ (float-time (time-since (date-to-time last))) 86400.0)))))
    (format "%d past answer(s), strength %.1f for / %.1f against%s"
            events (plist-get st :yes) (plist-get st :no)
            (cond ((null age) "")
                  ((< age 1) ", last today")
                  (t (format ", last %d day(s) ago" (round age)))))))

(defun efrit-review-confidence-score (facts)
  "The computed confidence 0..1 for FACTS; 0 when any fact is `blocked'."
  (if (assq 'blocked facts)
      0.0
    (let ((sum 0.0))
      (dolist (f facts)
        (cl-incf sum (or (alist-get (car f) efrit-review-confidence-weights) 0.0)))
      (min 1.0 (max 0.0 sum)))))

;;;; What the reviewer sees

(defun efrit-review-confidence-describe (use)
  "Lines for the reviewer about the sandbox requests USE would make, or nil.
Only requests nothing covers are described: covered ones never prompt."
  (when (and efrit-review-auto-grant-threshold (bound-and-true-p efrit-sandbox-enabled))
    (let ((lines nil))
      (dolist (req (ignore-errors (efrit-review-confidence-requests use)))
        (let ((cap (car req)) (target (cdr req)))
          (unless (ignore-errors (efrit-sandbox-allowed-p cap target))
            (let* ((facts (efrit-review-confidence-facts cap target use))
                   (score (efrit-review-confidence-score facts)))
              (push (format "[SANDBOX would ask: %s %s | computed confidence %.2f | %s]"
                            cap
                            (cond ((stringp target) (efrit-sandbox-abbreviate target))
                                  ((consp target) (cdr target))
                                  (t target))
                            score
                            (if facts
                                (mapconcat (lambda (f) (if (cdr f) (format "%s: %s" (car f) (cdr f)) (symbol-name (car f))))
                                           facts "; ")
                              "no facts"))
                    lines)))))
      (nreverse lines))))

(defun efrit-review-confidence-would-ask-p (use)
  "Non-nil when some sandbox request of USE has no grant and would prompt."
  (when (bound-and-true-p efrit-sandbox-enabled)
    (cl-some (lambda (req) (not (ignore-errors (efrit-sandbox-allowed-p (car req) (cdr req)))))
             (ignore-errors (efrit-review-confidence-requests use)))))

(defun efrit-review-confidence-ceiling (use)
  "The highest confidence the reviewer may claim for USE: the lowest computed
score over its would-ask requests, or nil when nothing would ask."
  (when (and efrit-review-auto-grant-threshold (bound-and-true-p efrit-sandbox-enabled))
    (let ((scores nil))
      (dolist (req (ignore-errors (efrit-review-confidence-requests use)))
        (unless (ignore-errors (efrit-sandbox-allowed-p (car req) (cdr req)))
          (push (efrit-review-confidence-score (efrit-review-confidence-facts (car req) (cdr req) use)) scores)))
      (and scores (apply #'min scores)))))

;;;; The reviewer's grant, consulted by the sandbox

(defun efrit-review-confidence-remember-grant (use confidence scope)
  "Record that the reviewer vouched for USE's requests at CONFIDENCE with SCOPE.
SCOPE is `once' or `session'; `project' is lowered to `session'.
The sandbox finds these in the turn state before it prompts."
  (when (and efrit-review-auto-grant-threshold
             (numberp confidence)
             (>= confidence efrit-review-auto-grant-threshold))
    (let ((scope (if (memq scope '(once session)) scope 'session))
          (ceiling (efrit-review-confidence-ceiling use)))
      (when (and ceiling (>= ceiling efrit-review-auto-grant-threshold))
        (dolist (req (ignore-errors (efrit-review-confidence-requests use)))
          (unless (ignore-errors (efrit-sandbox-allowed-p (car req) (cdr req)))
            (let ((facts (efrit-review-confidence-facts (car req) (cdr req) use)))
              (unless (assq 'blocked facts)
                (efrit-sandbox--turn-set
                 :reviewer-grants
                 (cons (list :cap (car req) :target (cdr req) :scope scope
                             :confidence (min confidence ceiling)
                             :why (mapconcat (lambda (f) (or (cdr f) (symbol-name (car f))))
                                             (cl-remove-if (lambda (f) (memq (car f) '(read-cap write-cap net-cap shell-cap elisp-cap))) facts)
                                             ", "))
                       (efrit-sandbox--turn-get :reviewer-grants)))))))))))

(defun efrit-review-confidence-take-grant (cap target)
  "The reviewer's vouching for CAP on TARGET this turn, as a plist, or nil.
Consumed: a second request for the same target asks again unless the
session grant made then already covers it."
  (let* ((grants (efrit-sandbox--turn-get :reviewer-grants))
         (hit (cl-find-if (lambda (g) (and (eq (plist-get g :cap) cap)
                                           (equal (plist-get g :target) target)))
                          grants)))
    (when hit
      (efrit-sandbox--turn-set :reviewer-grants (delq hit grants))
      hit)))

(provide 'efrit-review-confidence)

;;; efrit-review-confidence.el ends here
