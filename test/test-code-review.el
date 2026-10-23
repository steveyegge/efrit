;;; test-code-review.el --- Local code review: scope, gates, loop, apply, UI -*- lexical-binding: t; -*-

;;; Commentary:

;; A throwaway Git repository the test makes (never the user's) with
;; one commit and one staged change; a fake API that plays the
;; reviewer's turns (read_diff, verify_block, submit_review).

;;; Code:

(require 'ert)
(require 'efrit-code-review)
(require 'efrit-code-review-ui)

(defun test-cr--git (&rest args)
  (apply #'call-process "git" nil nil nil args))

(defun test-cr--repo ()
  "A repository with a.el committed and a staged edit to it.  Returns the root."
  (let ((root (file-name-as-directory (make-temp-file "efrit-cr-" t))))
    (let ((default-directory root))
      (test-cr--git "init" "-q")
      (test-cr--git "config" "user.email" "t@example.invalid")
      (test-cr--git "config" "user.name" "t")
      (with-temp-file "a.el" (insert "(defun add (a b)\n  (+ a b))\n\n(defun sub (a b)\n  (- a b))\n"))
      (test-cr--git "add" "a.el")
      (test-cr--git "commit" "-q" "-m" "first")
      (with-temp-file "a.el" (insert "(defun add (a b)\n  (+ a b))\n\n(defun sub (a b)\n  (+ a b))\n"))
      (test-cr--git "add" "a.el"))
    (file-name-as-directory (file-truename root))))

(defmacro test-cr--with-repo (var &rest body)
  (declare (indent 1))
  `(let* ((,var (test-cr--repo))
          (efrit-data-directory (file-name-as-directory (make-temp-file "efrit-cr-data-" t)))
          (efrit-code-review-persist t))
     (unwind-protect (progn ,@body)
       (ignore-errors (delete-directory ,var t))
       (ignore-errors (delete-directory efrit-data-directory t)))))

(defun test-cr--hash (&rest kv)
  (let ((h (make-hash-table :test 'equal)))
    (while kv (puthash (pop kv) (pop kv) h))
    h))

(defun test-cr--tool-use (id name &rest kv)
  (test-cr--hash "type" "tool_use" "id" id "name" name "input" (apply #'test-cr--hash kv)))

(defun test-cr--response (&rest items)
  (test-cr--hash "stop_reason" "tool_use" "content" (vconcat items)))

(ert-deftest test-cr-scope-staged-has-files-and-a-stable-hash ()
  (skip-unless (executable-find "git"))
  (test-cr--with-repo root
    (let ((scope (efrit-code-review-scope 'staged root)))
      (should (eq 'staged (efrit-code-review-scope-kind scope)))
      (should (equal '(("a.el" 1 1)) (efrit-code-review-scope-files scope)))
      (should (efrit-code-review-scope-current-p scope))
      ;; the index copy, not the work tree: edit the work tree and it is unchanged
      (with-temp-file (expand-file-name "a.el" root) (insert "changed on disk\n"))
      (should (string-suffix-p "(+ a b))\n" (efrit-code-review-file-at-scope scope "a.el")))
      (should (efrit-code-review-scope-current-p scope))
      ;; staging the edit moves the scope
      (let ((default-directory root)) (test-cr--git "add" "a.el"))
      (should-not (efrit-code-review-scope-current-p scope))
      (should-not (efrit-code-review-file-at-scope scope "../outside"))
      (should (string-match-p "^-  (- a b))" (efrit-code-review-diff-for scope "a.el")))
      ;; nothing staged: an error naming it
      (let ((default-directory root)) (test-cr--git "reset" "-q" "HEAD" "a.el"))
      (should-error (efrit-code-review-scope 'staged root) :type 'efrit-code-review-error))))

(ert-deftest test-cr-gates-make-patches-and-downgrade ()
  "A unique old_lines becomes a patch with lines; missing or repeated
text becomes a comment; a finding on an unknown file is dropped."
  (skip-unless (executable-find "git"))
  (test-cr--with-repo root
    (let* ((scope (efrit-code-review-scope 'staged root))
           (raw (list (test-cr--hash "file" "a.el" "title" "sub adds" "description" "wrong operator"
                                     "old_lines" "(defun sub (a b)\n  (+ a b))" "new_lines" "(defun sub (a b)\n  (- a b))")
                      (test-cr--hash "file" "a.el" "title" "ambiguous" "old_lines" "  (+ a b))" "new_lines" "x")
                      (test-cr--hash "file" "a.el" "title" "gone" "old_lines" "not in file" "new_lines" "x")
                      (test-cr--hash "file" "nope.el" "title" "ghost" "lines" "3")
                      (test-cr--hash "file" "a.el" "lgtm" t)))
           (findings (efrit-code-review-gate scope (mapcar #'efrit-code-review-normalize raw))))
      (should (= 4 (length findings)))
      (let ((good (nth 0 findings)) (amb (nth 1 findings)) (gone (nth 2 findings)) (ok (nth 3 findings)))
        (should (eq 'suggestion (plist-get good :type)))
        (should (equal "4-5" (plist-get good :lines)))
        (should (string-match-p "^-  (\\+ a b))\n\\+  (- a b))" (plist-get good :patch)))
        (should (string-match-p "^--- a/a.el" (plist-get good :patch)))
        (should (eq 'pending (plist-get good :state)))
        (should (eq 'comment (plist-get amb :type)))
        (should (string-match-p "2 times" (plist-get amb :downgraded)))
        (should (eq 'comment (plist-get gone :type)))
        (should (eq 'lgtm (plist-get ok :type)))
        (should (eq 'applied (plist-get ok :state)))))))

(ert-deftest test-cr-finding-states ()
  (let ((f (efrit-code-review-finding 'suggestion "a.el" :old-lines "x" :new-lines "y")))
    (should (eq 'pending (plist-get f :state)))
    (efrit-code-review-finding-transition f 'applied)
    (should (eq 'pending (plist-get f :state)))
    (efrit-code-review-finding-transition f 'queued)
    (efrit-code-review-finding-transition f 'dismissed)
    (should (eq 'queued (plist-get f :state)))
    (efrit-code-review-finding-transition f 'invalid)
    (should (eq 'invalid (plist-get f :state)))
    (should-error (efrit-code-review-finding-transition f 'bogus))
    (should (equal '(3) (efrit-code-review-occurrences "a\nb\nxy\n" "xy")))
    (should (equal '(1 3) (efrit-code-review-occurrences "ab\nc\nab\n" "ab")))
    (should (null (efrit-code-review-occurrences "ab" "")))
    (should (= 42 (efrit-code-review-parse-lines "42-50")))))

(ert-deftest test-cr-loop-reads-verifies-submits-and-saves ()
  "The fake reviewer reads the diff, verifies a block, submits; the
result has gated findings, is saved by hash, and loads back."
  (skip-unless (executable-find "git"))
  (test-cr--with-repo root
    (let* ((scope (efrit-code-review-scope 'staged root))
           (turn 0) (seen nil)
           (efrit-code-review-model "fake-reviewer"))
      (cl-letf (((symbol-function 'efrit-api-request-sync)
                 (lambda (request &optional _timeout)
                   (let* ((messages (append (alist-get "messages" request nil nil #'equal) nil))
                          (last (car (last messages))))
                     (push (alist-get 'role last) seen)
                     (cl-incf turn)
                     (pcase turn
                       (1 (test-cr--response (test-cr--tool-use "t1" "read_diff" "path" "a.el")
                                             (test-cr--tool-use "t2" "verify_block" "file" "a.el"
                                                                "old_lines" "(defun sub (a b)\n  (+ a b))")))
                       (2
                        ;; the tool results of turn 1 came back as text
                        (let* ((content (append (alist-get 'content last) nil))
                               (texts (mapcar (lambda (c) (alist-get 'content c)) content)))
                          (should (seq-some (lambda (s) (and (stringp s) (string-match-p "OK: unique at line 4" s))) texts))
                          (should (seq-some (lambda (s) (and (stringp s) (string-match-p "^-  (- a b))" s))) texts)))
                        (test-cr--response
                         (test-cr--tool-use "t3" "submit_review" "findings"
                                            (vector (test-cr--hash "file" "a.el" "title" "sub adds instead of subtracting"
                                                                   "description" "The staged change turns sub into add."
                                                                   "old_lines" "(defun sub (a b)\n  (+ a b))"
                                                                   "new_lines" "(defun sub (a b)\n  (- a b))")))))
                       (_ (error "the loop did not stop after submit")))))))
        (let ((result (efrit-code-review-run-sync scope)))
          (should (eq 'ok (plist-get result :status)))
          (should (= 2 (plist-get result :rounds)))
          (should (equal '("read_diff" "verify_block" "submit_review") (mapcar #'car (plist-get result :calls))))
          (should (= 1 (length (plist-get result :findings))))
          (should (plist-get (car (plist-get result :findings)) :patch))
          ;; saved and loadable under the same hash
          (let ((loaded (efrit-code-review-load scope)))
            (should loaded)
            (should (plist-get loaded :saved))
            (should (equal "fake-reviewer" (plist-get loaded :model)))
            (should (equal (plist-get (car (plist-get loaded :findings)) :patch)
                           (plist-get (car (plist-get result :findings)) :patch)))
            (should (eq 'pending (plist-get (car (plist-get loaded :findings)) :state)))))))))

(ert-deftest test-cr-loop-prose-and-budget-are-errors ()
  (skip-unless (executable-find "git"))
  (test-cr--with-repo root
    (let ((scope (efrit-code-review-scope 'staged root)))
      (cl-letf (((symbol-function 'efrit-api-request-sync)
                 (lambda (&rest _) (test-cr--hash "stop_reason" "end_turn"
                                                  "content" (vector (test-cr--hash "type" "text" "text" "Looks fine."))))))
        (let ((r (efrit-code-review-run-sync scope)))
          (should (eq 'error (plist-get r :status)))
          (should (string-match-p "prose" (plist-get r :message)))
          (should-not (efrit-code-review-load scope))))
      (let ((efrit-code-review-max-rounds 3))
        (cl-letf (((symbol-function 'efrit-api-request-sync)
                   (lambda (&rest _) (test-cr--response (test-cr--tool-use "t" "read_diff" "path" "a.el")))))
          (let ((r (efrit-code-review-run-sync scope)))
            (should (eq 'error (plist-get r :status)))
            (should (string-match-p "3 rounds" (plist-get r :message)))))))))

(ert-deftest test-cr-apply-queued-bottom-up-and-marks-failures ()
  "Two queued suggestions in one file land bottom-up; one whose text
moved is marked invalid; the others still apply."
  (skip-unless (executable-find "git"))
  (test-cr--with-repo root
    (let* ((scope (efrit-code-review-scope 'staged root))
           (findings
            (efrit-code-review-gate
             scope
             (list (efrit-code-review-finding 'suggestion "a.el" :title "top"
                                              :old-lines "(defun add (a b)" :new-lines "(defun add (a b) ; sum")
                   (efrit-code-review-finding 'suggestion "a.el" :title "bottom"
                                              :old-lines "(defun sub (a b)\n  (+ a b))" :new-lines "(defun sub (a b)\n  (- a b))")
                   (efrit-code-review-finding 'suggestion "a.el" :title "stale"
                                              :old-lines "(defun add (a b)\n  (+ a b))" :new-lines "x")))))
      (should (= 3 (length findings)))
      (dolist (f findings) (efrit-code-review-finding-transition f 'queued))
      (should (equal '("bottom" "top" "stale")
                     (mapcar (lambda (f) (plist-get f :title)) (efrit-code-review-apply-order findings))))
      ;; "stale" is third because its first line is the same as "top"'s (line 1): after "top"
      ;; applies, its text is gone and it must fail without breaking the rest
      (let ((done (efrit-code-review-apply-queued scope findings)))
        (should (= 3 (length done)))
        (should (equal '(applied applied invalid) (mapcar (lambda (d) (plist-get (car d) :state)) done)))
        (should (string-match-p "no longer" (plist-get (car (nth 2 done)) :error))))
      (with-temp-buffer
        (insert-file-contents (expand-file-name "a.el" root))
        (should (equal "(defun add (a b) ; sum\n  (+ a b))\n\n(defun sub (a b)\n  (- a b))\n" (buffer-string)))))))

(ert-deftest test-cr-ui-renders-queues-and-refuses-a-moved-scope ()
  (skip-unless (executable-find "git"))
  (test-cr--with-repo root
    (let* ((scope (efrit-code-review-scope 'staged root))
           (findings (efrit-code-review-gate
                      scope
                      (list (efrit-code-review-finding 'suggestion "a.el" :title "fix sub" :description "wrong op"
                                                       :old-lines "(defun sub (a b)\n  (+ a b))" :new-lines "(defun sub (a b)\n  (- a b))")
                            (efrit-code-review-finding 'comment "a.el" :lines "1" :title "naming" :description "add is vague")
                            (efrit-code-review-finding 'lgtm "a.el"))))
           (buf (efrit-code-review-ui--buffer)))
      (unwind-protect
          (with-current-buffer buf
            (setq efrit-code-review-ui--result
                  (list :status 'ok :scope scope :findings findings :model "m" :at "now" :rounds 2))
            (efrit-code-review-ui--render)
            (should (derived-mode-p 'efrit-code-review-ui-mode))
            (should (string-match-p "1 suggestion, 1 comment, 1 clean file" (buffer-string)))
            (should (string-match-p "SUGGESTION.*a.el:4-5" (buffer-string)))
            ;; point is on the first finding (the suggestion); m queues it
            (should (eq 'suggestion (plist-get (efrit-code-review-ui--finding-at) :type)))
            (efrit-code-review-ui-queue)
            (should (eq 'queued (plist-get (car findings) :state)))
            (should (string-match-p "queued" (buffer-string)))
            ;; after m point moved to the comment: m there is refused
            (should (eq 'comment (plist-get (efrit-code-review-ui--finding-at) :type)))
            (should-error (efrit-code-review-ui-queue) :type 'user-error)
            (efrit-code-review-ui-previous)
            (should (eq 'suggestion (plist-get (efrit-code-review-ui--finding-at) :type)))
            ;; the staged set moves: A refuses
            (let ((default-directory root))
              (with-temp-file "a.el" (insert "(defun add (a b)\n  (+ a b))\n\n(defun sub (a b)\n  (+ a b))\n;; more\n"))
              (test-cr--git "add" "a.el"))
            (should-error (efrit-code-review-ui-apply) :type 'user-error)
            (should (eq 'queued (plist-get (car findings) :state)))
            ;; back to the reviewed set: A applies, the file changes, the finding is applied
            (let ((default-directory root))
              (with-temp-file "a.el" (insert "(defun add (a b)\n  (+ a b))\n\n(defun sub (a b)\n  (+ a b))\n"))
              (test-cr--git "add" "a.el"))
            (efrit-code-review-ui-apply)
            (should (eq 'applied (plist-get (car findings) :state)))
            (with-temp-buffer
              (insert-file-contents (expand-file-name "a.el" root))
              (should (string-match-p "(- a b)" (buffer-string))))
            (should (string-match-p "applied" (buffer-string))))
        (kill-buffer buf)))))

(ert-deftest test-cr-command-shows-the-saved-review-without-a-request ()
  (skip-unless (executable-find "git"))
  (test-cr--with-repo root
    (let* ((scope (efrit-code-review-scope 'staged root))
           (asked 0))
      (efrit-code-review-save (list :status 'ok :scope scope :model "m" :at "t" :rounds 1
                                    :findings (list (efrit-code-review-finding 'comment "a.el" :lines "4" :title "saved one"))))
      (cl-letf (((symbol-function 'efrit-api-request-async) (lambda (&rest _) (cl-incf asked)))
                ((symbol-function 'pop-to-buffer) (lambda (b &rest _) (set-buffer b))))
        (let ((buf (efrit-code-review 'staged root)))
          (unwind-protect
              (with-current-buffer buf
                (should (= 0 asked))
                (should (string-match-p "saved one" (buffer-string)))
                (should (string-match-p "from the saved review" (buffer-string)))
                ;; g asks anew
                (efrit-code-review-ui-rerun)
                (should (= 1 asked))
                (should efrit-code-review-ui--state))
            (with-current-buffer buf (setq efrit-code-review-ui--state nil))
            (kill-buffer buf)))))))

(ert-deftest test-cr-surrounding-context-tool-reads-given-text ()
  "With tree-sitter and an elisp grammar the tool returns the enclosing
defun from the text it was given, not the file on disk."
  (require 'efrit-tool-navigate)
  (skip-unless (and (fboundp 'treesit-available-p) (treesit-available-p)
                    (treesit-language-available-p 'elisp)))
  (let ((r (efrit-tool-surrounding-context
            `((file . "/nonexistent/x.el") (text . "(defun one ()\n  1)\n\n(defun two ()\n  (let ((a 2))\n    a))\n")
              (line . 5) (depth . 1)))))
    (should (eq t (alist-get 'success r)))
    (let ((defs (append (alist-get 'definitions (alist-get 'result r)) nil)))
      (should (= 1 (length defs)))
      (should (string-prefix-p "(defun two" (alist-get 'text (car defs)))))))

(ert-deftest test-cr-notes-store-round-trips-and-promotes-on-commit ()
  "With the repository set to the notes store the review is a git note
on a key blob; nothing lands in efrit-data-directory; after a commit of
exactly the reviewed content the note is on HEAD; a different commit
gets none."
  (skip-unless (executable-find "git"))
  (test-cr--with-repo root
    (let* ((efrit-code-review-store 'json)
           (efrit-settings--cache (make-hash-table :test 'equal))
           (scope (efrit-code-review-scope 'staged root))
           (result (list :status 'ok :scope scope :model "m" :at "t" :rounds 1
                         :findings (list (efrit-code-review-finding 'comment "a.el" :lines "4" :title "noted")))))
      (efrit-settings-forget root)
      (should (eq 'json (efrit-code-review-store-for root)))
      (efrit-code-review-set-project-store 'notes root)
      (should (eq 'notes (efrit-code-review-store-for root)))
      (should (file-exists-p (expand-file-name ".efrit/settings.json" root)))
      ;; the default again removes the override and the file
      (efrit-code-review-set-project-store 'json root)
      (should-not (file-exists-p (expand-file-name ".efrit/settings.json" root)))
      (efrit-code-review-set-project-store 'notes root)
      (should (efrit-code-review-save result))
      (should-not (file-exists-p (efrit-code-review--json-file (efrit-code-review-scope-hash scope))))
      (let ((loaded (efrit-code-review-load scope)))
        (should loaded)
        (should (equal "noted" (plist-get (car (plist-get loaded :findings)) :title))))
      (let ((default-directory root))
        (should (string-match-p "[0-9a-f]\\{40\\}" (or (efrit-code-review--git root "notes" "--ref" "efrit-reviews" "list") "")))
        ;; a commit with one more line than reviewed: no note on it
        (with-temp-file "a.el" (insert "(defun add (a b)\n  (+ a b))\n\n(defun sub (a b)\n  (+ a b))\n;; extra\n"))
        (test-cr--git "add" "a.el")
        (test-cr--git "commit" "-q" "-m" "different")
        (efrit-code-review-promote-after-commit)
        (should-not (efrit-code-review--notes-show root "HEAD"))
        ;; amend back to the reviewed content: promoted
        (with-temp-file "a.el" (insert "(defun add (a b)\n  (+ a b))\n\n(defun sub (a b)\n  (+ a b))\n"))
        (test-cr--git "add" "a.el")
        (test-cr--git "commit" "-q" "--amend" "-m" "reviewed")
        (efrit-code-review-promote-after-commit)
        (let ((note (efrit-code-review--notes-show root "HEAD")))
          (should note)
          (should (string-match-p "\"title\":\"noted\"" note)))
        ;; consumed: a second call does nothing
        (should-not (alist-get root efrit-code-review--last-staged nil nil #'equal))))))

(provide 'test-code-review)

;;; test-code-review.el ends here
