;;; efrit-tool-lisp-dev.el --- Load project Lisp and run ERT in this Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.12.33
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, ai

;;; Commentary:

;; After editing a Lisp file the model wants to load it and run its
;; tests.  `load' is refused inside eval_sexp (it is the way to run
;; arbitrary code past the static inspection), and a batch Emacs is
;; refused too, so the model had no route left (2026-10-20: three
;; rejected `emacs --batch' attempts for three new ERT tests).  Two
;; tools give it the in-process route, which is also the one tzz
;; wants: "everything it needs is in the running Emacs".
;;
;; `load_project_file' loads one Lisp file that lives under the
;; project root into this Emacs: `load' with the file name handler
;; and the sandbox's read check, the compiled file ignored so the
;; edit is what runs.  Reviewed as a write (it changes the live
;; session) and flagged by efrit-review-flags like an eval of the
;; file's contents.
;;
;; `run_ert' runs ERT tests by selector in this Emacs and returns the
;; counts and the failing tests' messages, nothing more.  It never
;; prompts; `ert-run-tests-batch' writes to a temporary buffer.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'ert)
(require 'efrit-tool-utils)

(declare-function efrit-review-flags--text-effects "efrit-review-flags")

(defgroup efrit-tool-lisp-dev nil
  "Loading project Lisp and running ERT from the agent."
  :group 'efrit-tool-utils)

(defcustom efrit-tool-run-ert-max-failures 10
  "Most failing tests whose messages `run_ert' returns."
  :type 'integer
  :group 'efrit-tool-lisp-dev)

(defcustom efrit-tool-run-ert-timeout 120
  "Seconds `run_ert' lets a selector run before it is stopped."
  :type 'integer
  :group 'efrit-tool-lisp-dev)

;;;; load_project_file

(defun efrit-tool-load-project-file (args)
  "Load the Lisp file ARGS's `path' names, under the project root, into this Emacs.
The source is loaded, never a stale .elc.  Returns the features the
load provided and the top-level definitions the file holds."
  (efrit-tool-execute load_project_file args
    (let* ((path-input (or (alist-get 'path args) ""))
           (info (efrit-resolve-path path-input 'read "load_project_file"))
           (path (plist-get info :path))
           (root (file-name-as-directory (efrit-tool--get-project-root))))
      (when (string-empty-p path-input) (signal 'user-error (list "path is required")))
      (unless (file-exists-p path) (signal 'file-error (list "File not found" path)))
      (unless (string-match-p "\\.el\\'" path)
        (signal 'user-error (list "Only Emacs Lisp source files (.el) can be loaded" path)))
      (unless (string-prefix-p root (expand-file-name path))
        (signal 'user-error (list (format "%s is outside the project (%s); load_project_file loads project files only"
                                          (plist-get info :path-relative) (abbreviate-file-name root)))))
      (let ((before (copy-sequence features))
            (load-prefer-newer t)
            (messages nil))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest a) (when fmt (push (apply #'format fmt a) messages)) nil)))
          (load path nil t t))
        (efrit-tool-success
         `((path . ,path)
           (path_relative . ,(plist-get info :path-relative))
           (features_provided . ,(vconcat (mapcar #'symbol-name (cl-set-difference features before))))
           (messages . ,(vconcat (nreverse messages)))))))))

;;;; run_ert

(defun efrit-tool-run-ert--selector (raw)
  "The ERT selector for RAW: a string is a regexp, \"t\" is everything, a vector is a list of names."
  (cond
   ((null raw) t)
   ((equal raw "t") t)
   ((vectorp raw) (cons 'member (mapcar #'intern (append raw nil))))
   ((stringp raw) raw)
   (t (signal 'user-error (list "selector must be a regexp string or a list of test names")))))

(defun efrit-tool-run-ert (args)
  "Run ERT tests matching ARGS's `selector' in this Emacs; return counts and failures.
`selector' is a regexp on test names (default: every test), or a list
of test names.  The tests run under `ert-run-tests-batch' with its
output captured; failures come back with their messages, capped at
`efrit-tool-run-ert-max-failures'."
  (efrit-tool-execute run_ert args
    (let* ((selector (efrit-tool-run-ert--selector (alist-get 'selector args)))
           (matching (ert-select-tests selector t))
           (start (float-time))
           (failures nil)
           (stats nil))
      (when (null matching)
        (signal 'user-error (list (format "No ERT tests match %S; load the test file first (load_project_file)"
                                          (alist-get 'selector args)))))
      (with-temp-buffer
        (let ((standard-output (current-buffer))
              (inhibit-message t))
          (with-timeout (efrit-tool-run-ert-timeout
                         (signal 'user-error (list (format "run_ert stopped after %ds" efrit-tool-run-ert-timeout))))
            (setq stats (ert-run-tests-batch selector)))))
      (dotimes (i (length (ert--stats-tests stats)))
        (let* ((test (aref (ert--stats-tests stats) i))
               (result (aref (ert--stats-test-results stats) i)))
          (when (and result (not (ert-test-result-expected-p test result)))
            (push `((name . ,(symbol-name (ert-test-name test)))
                    (status . ,(if (ert-test-result-type-p result :failed) "failed"
                                 (if (ert-test-result-type-p result :passed) "unexpected-pass" "error")))
                    (message . ,(if (ert-test-result-with-condition-p result)
                                    (truncate-string-to-width
                                     (format "%S" (ert-test-result-with-condition-condition result))
                                     2000 nil nil "…")
                                  "")))
                  failures))))
      (setq failures (nreverse failures))
      (efrit-tool-success
       `((selector . ,(format "%S" selector))
         (total . ,(ert-stats-total stats))
         (passed . ,(ert-stats-completed-expected stats))
         (failed . ,(ert-stats-completed-unexpected stats))
         (skipped . ,(ert-stats-skipped stats))
         (seconds . ,(/ (round (* 100 (- (float-time) start))) 100.0))
         (failures . ,(vconcat (seq-take failures efrit-tool-run-ert-max-failures))))
       (and (> (length failures) efrit-tool-run-ert-max-failures)
            (list (format "%d more failure(s) not shown" (- (length failures) efrit-tool-run-ert-max-failures))))))))

(provide 'efrit-tool-lisp-dev)

;;; efrit-tool-lisp-dev.el ends here
