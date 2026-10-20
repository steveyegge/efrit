;;; test-sandbox-symbolic.el --- Paths as patterns -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'efrit-sandbox)
(require 'efrit-sandbox-symbolic)
(require 'efrit-grant-history)
(require 'efrit-review-confidence)
(require 'efrit-review)
(require 'efrit-instructions)

(ert-deftest test-symbolic-path-shows-generated-components-as-placeholders ()
  (let ((home (file-name-as-directory (expand-file-name "~"))))
    (should (equal "~/work/<date>/claude.org"
                   (efrit-sandbox-symbolic-path (concat home "work/2026-10-16/claude.org"))))
    (should (equal "~/work/<date>/" (efrit-sandbox-symbolic-path (concat home "work/20261016/"))))
    (should (equal "~/logs/<week>/<month>/x" (efrit-sandbox-symbolic-path (concat home "logs/2026-W42/2026-10/x"))))
    (should (equal "~/notes/log-<date>.txt" (efrit-sandbox-symbolic-path (concat home "notes/log-2026-10-16.txt"))))
    (should (equal "~/.efrit/sessions/<session>.json"
                   (efrit-sandbox-symbolic-path (concat home ".efrit/sessions/repl-20261014-212357-0002.json"))))
    (should (equal "/tmp/efrit-<id>/out" (efrit-sandbox-symbolic-path "/tmp/efrit-3f2a9c1d/out")))
    ;; nothing generated: unchanged
    (should (equal "~/autodist/files/tmux.conf" (efrit-sandbox-symbolic-path (concat home "autodist/files/tmux.conf"))))
    ;; a short number is a name, not an id
    (should (equal "~/p/v2/a.el" (efrit-sandbox-symbolic-path (concat home "p/v2/a.el"))))))

(ert-deftest test-symbolic-template-regexp-and-match ()
  (let ((home (file-name-as-directory (expand-file-name "~"))))
    (should-not (efrit-sandbox-symbolic-template-regexp "~/work/claude.org"))
    (let ((re (efrit-sandbox-symbolic-template-regexp "~/work/YYYY-MM-DD/claude.org")))
      (should (string-match-p re (concat home "work/2026-10-16/claude.org")))
      (should-not (string-match-p re (concat home "work/2026-10-16/other.org")))
      (should-not (string-match-p re (concat home "work/notes/claude.org"))))
    ;; a directory template covers what is under it; %F and $HOME spellings
    (let ((re (efrit-sandbox-symbolic-template-regexp "$HOME/work/%F/")))
      (should (string-match-p re (concat home "work/2026-10-16/anything/deep.txt"))))
    ;; * is one component, ** any depth
    (should (string-match-p (efrit-sandbox-symbolic-template-regexp "~/src/*/TODO.org") (concat home "src/efrit/TODO.org")))
    (should-not (string-match-p (efrit-sandbox-symbolic-template-regexp "~/src/*/TODO.org") (concat home "src/a/b/TODO.org")))
    (should (string-match-p (efrit-sandbox-symbolic-template-regexp "~/src/**/TODO.org") (concat home "src/a/b/TODO.org")))))

(defmacro test-symbolic--with-instructions (text &rest body)
  "BODY with a throwaway user instruction file holding TEXT and a throwaway root."
  (declare (indent 1))
  `(let* ((dir (file-name-as-directory (make-temp-file "efrit-sym-" t)))
          (file (expand-file-name "CLAUDE.md" dir))
          (root (file-name-as-directory (expand-file-name "proj" dir)))
          (efrit-instructions-user-files (list file))
          (efrit-instructions-ancestors nil)
          (efrit-project-root root)
          (efrit-sandbox-enabled t)
          (efrit-sandbox-default-project-grants '(read))
          (efrit-sandbox-request-function nil)
          (efrit-sandbox--session-grants (make-hash-table :test 'equal))
          (efrit-sandbox--project-grants (make-hash-table :test 'equal))
          (efrit-sandbox--turn-state (make-hash-table :test 'equal))
          (efrit-sandbox-store--loaded (make-hash-table :test 'equal))
          (efrit-sandbox-expected-read-roots nil)
          (efrit-sandbox-expected-write-roots nil)
          (efrit-data-directory (file-name-as-directory (expand-file-name "data" dir)))
          (efrit-grant-history--table nil)
          (efrit-review-auto-grant-threshold 0.95)
          (efrit-current-session-id "sym-test"))
     (make-directory root)
     (with-temp-file file (insert ,text))
     (efrit-sandbox-symbolic-forget)
     (unwind-protect (progn ,@body)
       (efrit-sandbox-symbolic-forget)
       (ignore-errors (delete-directory dir t)))))

(ert-deftest test-symbolic-instruction-template-is-expected-and-a-reviewer-fact ()
  "A write to a path the user's CLAUDE.md names as a template is expected
\(no prompt), carries the `instructed' fact at the threshold, is not
blocked as a write outside granted repos, and the SANDBOX line shows the
pattern.  A path outside the template still asks."
  (test-symbolic--with-instructions
      "## Work log\nRead `~/work/YYYY-MM-DD/claude.org` or create it. Notes under ~/work/YYYY-MM-DD/.\n"
    (let* ((home (file-name-as-directory (expand-file-name "~")))
           (log (concat home "work/2026-10-16/claude.org"))
           (asked nil)
           (efrit-sandbox-request-function (lambda (req) (push (efrit-sandbox-request-target req) asked) nil)))
      (let ((templates (efrit-sandbox-symbolic-templates root)))
        (should (= 2 (length templates)))
        (should (equal "~/work/YYYY-MM-DD/claude.org" (car (nth 0 templates)))))
      (should (equal "~/work/YYYY-MM-DD/claude.org" (car (efrit-sandbox-symbolic-instructed log root))))
      (should (efrit-sandbox-expected-p 'write log root))
      (should (string-match-p "in your instructions" (efrit-sandbox-expected-p 'write log root)))
      ;; the check grants without a prompt
      (should (efrit-sandbox-check 'write log "edit_file"))
      (should (null asked))
      ;; the reviewer's facts
      (let ((facts (efrit-review-confidence-facts 'write log)))
        (should (assq 'instructed facts))
        (should-not (assq 'blocked facts))
        (should (>= (efrit-review-confidence-score facts) 0.95)))
      ;; a sibling the instructions do not name asks, and is blocked for the reviewer
      (let ((other (concat home "work/2026-10-16/../other/x.txt")))
        (should-not (efrit-sandbox-expected-p 'write (expand-file-name other) root))
        (should (assq 'blocked (efrit-review-confidence-facts 'write (expand-file-name other)))))
      ;; the SANDBOX line carries the pattern
      (clrhash efrit-sandbox--session-grants)
      (let* ((input (make-hash-table :test 'equal)))
        (puthash "path" (concat home "work/2026-10-16/scratch.txt") input)
        (puthash "old_str" "a" input) (puthash "new_str" "b" input)
        (let ((lines (efrit-review-confidence-describe (list "id" "edit_file" input))))
          (should lines)
          (should (string-match-p "(pattern ~/work/<date>/scratch.txt)" (car lines)))))
      ;; history is keyed on the pattern: today's answer counts tomorrow
      (efrit-grant-history-record 'write (concat home "work/2026-10-16/scratch.txt") 'session)
      (should (efrit-grant-history-lookup 'write (concat home "work/2026-10-17/scratch.txt")))
      (should (string-match-p "<date>" (efrit-grant-history-pattern 'write (concat home "work/2026-10-17/x")))))))

(ert-deftest test-symbolic-templates-follow-the-file ()
  "Editing the instruction file refreshes the templates; a file with no
placeholders yields none."
  (test-symbolic--with-instructions "nothing here but ~/notes/fixed.org\n"
    (should (null (efrit-sandbox-symbolic-templates root)))
    (sleep-for 1.1)
    (with-temp-file file (insert "log at ~/journal/YYYY/MM/DD.org\n"))
    (let ((templates (efrit-sandbox-symbolic-templates root)))
      (should (= 1 (length templates)))
      (should (efrit-sandbox-symbolic-instructed (expand-file-name "~/journal/2026/10/16.org") root))
      (should-not (efrit-sandbox-symbolic-instructed (expand-file-name "~/journal/2026/10/16.txt") root)))))

(provide 'test-sandbox-symbolic)

;;; test-sandbox-symbolic.el ends here
