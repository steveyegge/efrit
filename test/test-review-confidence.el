;;; test-review-confidence.el --- The reviewer vouching for the user -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'efrit-sandbox)
(require 'efrit-grant-history)
(require 'efrit-review-confidence)
(require 'efrit-review)

(defmacro test-rc--isolated (&rest body)
  "BODY with a throwaway data directory, project root and repo; clean tables."
  (declare (indent 0))
  `(let* ((efrit-data-directory (file-name-as-directory (make-temp-file "efrit-rc-data-" t)))
          (root (file-name-as-directory (make-temp-file "efrit-rc-root-" t)))
          (repo (let ((d (file-name-as-directory (make-temp-file "efrit-rc-repo-" t))))
                  (make-directory (expand-file-name ".git" d))
                  (efrit-sandbox-forget-git-toplevels)
                  (file-name-as-directory (efrit-sandbox-canonical d))))
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
          (efrit-grant-history--table nil)
          (efrit-review-auto-grant-threshold 0.95)
          (efrit-current-session-id "rc-test"))
     (unwind-protect (progn ,@body)
       (ignore-errors (delete-directory efrit-data-directory t))
       (ignore-errors (delete-directory root t))
       (ignore-errors (delete-directory repo t)))))

(defun test-rc--use (name &rest kv)
  (let ((input (make-hash-table :test 'equal)))
    (while kv (puthash (pop kv) (pop kv) input))
    (list "id" name input)))

(ert-deftest test-rc-history-is-recorded-per-pattern-and-persists ()
  "Answers are counted per (cap . repo-or-dir) and survive a reload of the table."
  (test-rc--isolated
    (let ((file (expand-file-name "src/a.el" repo)))
      (efrit-grant-history-record 'read file 'session)
      (efrit-grant-history-record 'read (expand-file-name "README" repo) 'once)
      (efrit-grant-history-record 'read file nil)
      (let ((rec (efrit-grant-history-lookup 'read (expand-file-name "other/b.el" repo))))
        (should (= 2 (plist-get rec :granted)))
        (should (= 1 (plist-get rec :denied))))
      (setq efrit-grant-history--table nil)
      (should (= 2 (plist-get (efrit-grant-history-lookup 'read file) :granted)))
      ;; strength: session 2 + once 1 for, denied 3 against, all today
      (let ((st (efrit-grant-history-strength 'read file)))
        (should (< (abs (- 3.0 (plist-get st :yes))) 0.01))
        (should (< (abs (- 3.0 (plist-get st :no))) 0.01)))
      ;; shell: the sorted command set; net: the host
      (efrit-grant-history-record 'shell "rg foo | sort" 'session)
      (should (efrit-grant-history-lookup 'shell "sort -r | rg bar"))
      (efrit-grant-history-record 'net '(host . "example.invalid") 'session)
      (should (efrit-grant-history-lookup 'net '(host . "example.invalid")))
      ;; remote paths have no pattern
      (should-not (efrit-grant-history-pattern 'read "/ssh:h:/etc/x")))))

(ert-deftest test-rc-scope-and-age-weigh-the-answers ()
  "A saved grant counts 3, a session 2, a once 1; an answer from one half-life
ago counts half; with the half-life off nothing fades."
  (test-rc--isolated
    (let* ((efrit-grant-history-half-life-days 60)
           (p (expand-file-name "a" repo))
           (ago (lambda (days) (format-time-string "%FT%T%z" (time-subtract (current-time) (* days 86400))))))
      (efrit-grant-history-record 'read p 'project)
      (should (< (abs (- 3.0 (plist-get (efrit-grant-history-strength 'read p) :yes))) 0.01))
      (efrit-grant-history-forget)
      (efrit-grant-history-record 'read p 'session (funcall ago 60))
      (should (< (abs (- 1.0 (plist-get (efrit-grant-history-strength 'read p) :yes))) 0.02))
      (efrit-grant-history-record 'read p 'once (funcall ago 120))
      (should (< (abs (- 1.25 (plist-get (efrit-grant-history-strength 'read p) :yes))) 0.03))
      (let ((efrit-grant-history-half-life-days nil))
        (should (< (abs (- 3.0 (plist-get (efrit-grant-history-strength 'read p) :yes))) 0.01)))
      ;; strong history: two fresh saved grants reach 6; the same two
      ;; from 60 days back do not, and the fact is only history-some
      (efrit-grant-history-forget)
      (efrit-grant-history-record 'read p 'project)
      (efrit-grant-history-record 'read p 'project)
      (should (assq 'history-strong (efrit-review-confidence-facts 'read (efrit-sandbox-canonical p))))
      (efrit-grant-history-forget)
      (efrit-grant-history-record 'read p 'project (funcall ago 60))
      (efrit-grant-history-record 'read p 'project (funcall ago 60))
      (let ((facts (efrit-review-confidence-facts 'read (efrit-sandbox-canonical p))))
        (should (assq 'history-some facts))
        (should-not (assq 'history-strong facts)))
      ;; one fresh denial (3) against two old saved grants (3) blocks: no > yes by a hair
      (efrit-grant-history-record 'read p nil)
      (should (assq 'blocked (efrit-review-confidence-facts 'read (efrit-sandbox-canonical p)))))))

(ert-deftest test-rc-score-from-facts-and-hard-blockers ()
  "A read in a repo granted three times before and named in the request scores
above the threshold; a write outside any granted repo, an always-ask line and a
denied pattern score 0 whatever else is true."
  (test-rc--isolated
    (let ((file (expand-file-name "src/a.el" repo)))
      (dotimes (_ 3) (efrit-grant-history-record 'read file 'session))
      (efrit-sandbox-begin-turn (format "look at %s please" file))
      (let* ((facts (efrit-review-confidence-facts 'read (efrit-sandbox-canonical file)))
             (score (efrit-review-confidence-score facts)))
        (should (assq 'history-strong facts))
        (should (assq 'mentioned facts))
        (should (assq 'granted-repo facts))
        (should (>= score 0.95)))
      ;; a first-time read in an unknown place is far below
      (should (< (efrit-review-confidence-score
                  (efrit-review-confidence-facts 'read (expand-file-name "x.txt" root)))
                 0.5))
      ;; blockers
      (should (= 0.0 (efrit-review-confidence-score
                      (efrit-review-confidence-facts 'write (expand-file-name "w.txt" root)))))
      (should (= 0.0 (efrit-review-confidence-score
                      (efrit-review-confidence-facts 'shell "rm -rf /tmp/x"))))
      (should (= 0.0 (efrit-review-confidence-score
                      (efrit-review-confidence-facts 'read "/ssh:h:/etc/passwd"))))
      (efrit-grant-history-record 'net '(host . "nope.invalid") nil)
      (efrit-grant-history-record 'net '(host . "nope.invalid") nil)
      (efrit-grant-history-record 'net '(host . "nope.invalid") 'once)
      (should (= 0.0 (efrit-review-confidence-score
                      (efrit-review-confidence-facts 'net '(host . "nope.invalid")))))
      ;; a read-only shell line granted before scores 0.85: history alone
      ;; is not enough; with the target named in the request it crosses
      (dotimes (_ 3) (efrit-grant-history-record 'shell "cd /x && rg a *.el | sort" 'session))
      (let ((score (efrit-review-confidence-score
                    (efrit-review-confidence-facts 'shell "cd /y && rg b *.el | sort"))))
        (should (< 0.8 score 0.95))))))

(ert-deftest test-rc-reviewer-sees-the-would-ask-line-and-its-vouch-replaces-the-prompt ()
  "The batch text carries a SANDBOX line with the computed score; after an
approve with confidence >= threshold, the sandbox grants for the session with a
note instead of calling the prompt function.  Below the threshold, or when the
reviewer claims more than the computed ceiling allows, the prompt is called."
  (test-rc--isolated
    (let* ((file (expand-file-name "src/a.el" repo))
           (asked 0)
           (efrit-sandbox-request-function (lambda (_req) (cl-incf asked) nil))
           (notes nil)
           (sub (lambda (e) (push (or (alist-get :text e) "") notes))))
      ;; the request does not name the file (a named one is expected and
      ;; never reaches the reviewer path); a fourth grant brings the
      ;; computed score to the line without it
      (dotimes (_ 3) (efrit-grant-history-record 'read file 'session))
      (efrit-sandbox-begin-turn "summarize the main file")
      (let* ((use (test-rc--use "read_file" "path" file))
             (batch (let ((u (make-hash-table :test 'equal)))
                      (puthash "type" "tool_use" u) (puthash "id" "id" u)
                      (puthash "name" "read_file" u) (puthash "input" (nth 2 use) u)
                      (efrit-review-describe-batch (vector u)))))
        (should (string-match-p "\\[SANDBOX would ask: read .* | computed confidence \\(?:0\\.9[5-9]\\|1\\.00\\)" batch))
        (should (string-match-p "history-strong" batch))
        ;; the reviewer vouches
        (efrit-subscribe 'note sub)
        (unwind-protect
            (progn
              (efrit-review-confidence-remember-grant use 0.97 'session)
              (should (efrit-sandbox-check 'read file "read_file"))
              (should (= 0 asked))
              (should (cl-some (lambda (n) (string-match-p "allowed by the reviewer (0.97" n)) notes))
              ;; the session grant now covers the sibling without a vouch
              (should (efrit-sandbox-allowed-p 'read (expand-file-name "src/b.el" repo) root)))
          (efrit-unsubscribe 'note sub)))
      ;; below the threshold: the user is asked
      (clrhash efrit-sandbox--session-grants)
      (let ((use (test-rc--use "read_file" "path" file)))
        (efrit-review-confidence-remember-grant use 0.80 'session)
        (should-error (efrit-sandbox-check 'read file "read_file") :type 'efrit-sandbox-denied)
        (should (= 1 asked)))
      ;; a confident reviewer cannot lift a request the facts block
      (let ((use (test-rc--use "create_file" "path" (expand-file-name "new.txt" root) "content" "x")))
        (efrit-review-confidence-remember-grant use 0.99 'session)
        (should-not (efrit-sandbox--turn-get :reviewer-grants))
        (should-error (efrit-sandbox-check 'write (expand-file-name "new.txt" root) "create_file")
                      :type 'efrit-sandbox-denied)
        (should (= 2 asked)))
      ;; the read refusal above was recorded (the write is another key):
      ;; 3 granted, 1 denied; the user's "no" counts against the
      ;; reviewer next time.  Reset for the last case.
      (should (= 1 (plist-get (efrit-grant-history-lookup 'read file) :denied)))
      (efrit-grant-history-forget)
      (dotimes (_ 3) (efrit-grant-history-record 'read file 'session))
      ;; project scope from the reviewer becomes session: nothing is saved
      (let ((use (test-rc--use "read_file" "path" file)))
        (clrhash efrit-sandbox--session-grants)
        (efrit-review-confidence-remember-grant use 0.98 'project)
        (should (eq 'session (plist-get (car (efrit-sandbox--turn-get :reviewer-grants)) :scope)))))))

(ert-deftest test-rc-parse-verdict-keeps-the-vouch ()
  "Confidence and grant ride along with the approve; a reject carries none."
  (should (equal '(approve) (efrit-review-parse-verdict "{\"verdict\": \"approve\", \"confidence\": 0.97, \"grant\": \"session\"}")))
  (should (equal 0.97 (plist-get efrit-review--last-vouch :confidence)))
  (should (eq 'session (plist-get efrit-review--last-vouch :grant)))
  (efrit-review-parse-verdict "{\"verdict\": \"reject\", \"reason\": \"no\"}")
  (should-not efrit-review--last-vouch))

(provide 'test-review-confidence)
;;; test-review-confidence.el ends here
