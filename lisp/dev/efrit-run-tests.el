;;; efrit-run-tests.el --- Run each ERT file in its own Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.6.2
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, maint

;;; Commentary:

;; `make test-unit' used to load every test/test-*.el into one Emacs.
;; State from one file leaked into the next (a struct redefined by a
;; "mock" broke three todo_write tests for days, 2026-09-30), and the
;; files that are live scripts rather than suites were excluded by a
;; comment, not by a rule.
;;
;; This runner selects the files that contain an `ert-deftest' (or the
;; ones named on the command line), starts a fresh batch Emacs for
;; each, and prints one line per file plus a total.  A file that
;; exits non-zero, or whose output has no "Ran N tests" line, counts
;; as failed.  The exit status is the number of failed files (capped
;; at 125).
;;
;;   emacs -Q --batch -l lisp/dev/efrit-run-tests.el            all suites
;;   emacs -Q --batch -l lisp/dev/efrit-run-tests.el test/test-api.el ...
;;
;; Environment: EFRIT_TEST_EMACS names the Emacs to run the suites
;; with (default: the one running this script); EFRIT_TEST_JOBS is
;; how many run at once (default 4).

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defconst efrit-run-tests--root
  (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name)))
  "The lisp/ directory; the repository root is one up.")

(defconst efrit-run-tests--repo (expand-file-name ".." efrit-run-tests--root))

(defconst efrit-run-tests--load-path
  '("lisp" "lisp/core" "lisp/interfaces" "lisp/support" "lisp/tools" "lisp/dev" "test")
  "Directories under the repository root put on `load-path' for a suite.")

(defconst efrit-run-tests--status-ok 0)
(defconst efrit-run-tests--status-cap 125
  "Highest exit status used: above it the shell reads a signal.")

(defun efrit-run-tests--ert-file-p (file)
  "Non-nil when FILE defines at least one ERT test."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (re-search-forward "^(ert-deftest " nil t)))

(defun efrit-run-tests--files (args)
  "The suite files: ARGS when given, else every ERT file under test/."
  (or (mapcar #'expand-file-name args)
      (cl-remove-if-not #'efrit-run-tests--ert-file-p
                        (directory-files (expand-file-name "test" efrit-run-tests--repo)
                                         t "\\`test-.*\\.el\\'"))))

(defun efrit-run-tests--command (file)
  "The command line that runs FILE's suite in a fresh Emacs."
  (append (list (or (getenv "EFRIT_TEST_EMACS")
                    (expand-file-name invocation-name invocation-directory))
                "-Q" "--batch")
          (cl-mapcan (lambda (d) (list "-L" (expand-file-name d efrit-run-tests--repo)))
                     efrit-run-tests--load-path)
          (list "-l" "ert" "-l" file "-f" "ert-run-tests-batch-and-exit")))

(defun efrit-run-tests--summary-line (output)
  "The \"Ran N tests …\" line of OUTPUT, or nil."
  (and (string-match "^Ran [0-9]+ tests?.*$" output) (match-string 0 output)))

(defun efrit-run-tests--failed-names (output)
  "The names ERT listed as FAILED in OUTPUT."
  (let ((names nil) (start 0))
    (while (string-match "^ +FAILED +\\(?:[0-9]+/[0-9]+ +\\)?\\([^ \n]+\\)" output start)
      (cl-pushnew (match-string 1 output) names :test #'equal)
      (setq start (match-end 0)))
    (nreverse names)))

(defun efrit-run-tests--start (file done)
  "Run FILE's suite; call DONE with (FILE EXIT-STATUS OUTPUT) when it ends."
  (let* ((buf (generate-new-buffer (format " *efrit-test %s*" (file-name-nondirectory file))))
         (command (efrit-run-tests--command file))
         (default-directory efrit-run-tests--repo))
    (make-process
     :name (file-name-nondirectory file)
     :buffer buf
     :command command
     :noquery t
     :sentinel (lambda (proc _event)
                 (unless (process-live-p proc)
                   (let ((output (with-current-buffer buf (buffer-string))))
                     (kill-buffer buf)
                     (funcall done file (process-exit-status proc) output)))))))

(defun efrit-run-tests-main ()
  "Run the suites named in `command-line-args-left' (or all) and exit."
  (let* ((files (efrit-run-tests--files command-line-args-left))
         (jobs (max 1 (string-to-number (or (getenv "EFRIT_TEST_JOBS") "4"))))
         (pending (copy-sequence files))
         (running 0)
         (results nil)
         (started (float-time)))
    (setq command-line-args-left nil)
    (message "efrit tests: %d suite(s), %d at a time" (length files) jobs)
    (cl-labels ((launch ()
                  (while (and pending (< running jobs))
                    (cl-incf running)
                    (efrit-run-tests--start (pop pending) #'finished)))
                (finished (file status output)
                  (cl-decf running)
                  (let* ((summary (efrit-run-tests--summary-line output))
                         (ok (and summary (zerop status) (string-match-p ", 0 unexpected" summary)))
                         (failed (efrit-run-tests--failed-names output)))
                    (push (list file ok summary failed status) results)
                    (message "%s %s: %s%s"
                             (if ok "ok  " "FAIL")
                             (file-relative-name file efrit-run-tests--repo)
                             (or summary (format "no ERT summary (exit %d)" status))
                             (if failed (format "  [%s]" (mapconcat #'identity failed " ")) "")))
                  (launch)))
      (launch)
      (while (or pending (> running 0))
        (accept-process-output nil 0.2)))
    (let* ((bad (cl-remove-if #'cadr results))
           (tests (cl-loop for r in results
                           for s = (nth 2 r)
                           when (and s (string-match "Ran \\([0-9]+\\)" s))
                           sum (string-to-number (match-string 1 s)))))
      (message "\n%d suite(s), %d test(s), %d suite(s) failed, %.0fs"
               (length results) tests (length bad) (- (float-time) started))
      (dolist (r (reverse bad))
        (message "  %s: %s" (file-relative-name (car r) efrit-run-tests--repo)
                 (or (nth 2 r) (format "exit %d, no summary; tail:\n%s" (nth 4 r) ""))))
      (kill-emacs (min efrit-run-tests--status-cap (length bad))))))

(efrit-run-tests-main)

;;; efrit-run-tests.el ends here
