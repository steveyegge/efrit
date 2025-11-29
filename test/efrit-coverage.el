;;; efrit-coverage.el --- Coverage reporting for Efrit -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, testing

;;; Commentary:

;; This module provides code coverage tracking and reporting for Efrit.
;; It uses Emacs's built-in testcover.el for instrumentation and adds:
;; - Per-module coverage tracking
;; - Multiple report formats (text, LCOV)
;; - Integration with efrit-test-runner
;; - Coverage thresholds for CI
;;
;; Usage:
;;   ;; Start coverage tracking
;;   (efrit-coverage-start)
;;
;;   ;; Run your tests...
;;   (require 'efrit-test-runner)
;;   (efrit-test-run-all)
;;
;;   ;; Generate reports
;;   (efrit-coverage-report)           ; Text report
;;   (efrit-coverage-report-lcov)      ; LCOV format for CI
;;
;;   ;; Check threshold
;;   (efrit-coverage-check-threshold 80.0)  ; Fail if < 80%
;;
;; Following Zero Client-Side Intelligence: this module only TRACKS
;; coverage data and REPORTS it. It does not interpret results or
;; make decisions.

;;; Code:

(require 'cl-lib)
(require 'testcover)
(require 'json)

;; Declare functions from efrit-test-runner to avoid warnings
(declare-function efrit-test-register-all-tiers "efrit-test-specs")
(declare-function efrit-test-run-all "efrit-test-runner")

;;; Customization

(defgroup efrit-coverage nil
  "Coverage reporting for Efrit."
  :group 'efrit
  :prefix "efrit-coverage-")

(defcustom efrit-coverage-threshold 0.0
  "Minimum coverage percentage required.
Set to 0 to disable threshold checking."
  :type 'number
  :group 'efrit-coverage)

(defcustom efrit-coverage-output-dir nil
  "Directory for coverage reports.
If nil, uses ~/.emacs.d/.efrit/coverage/."
  :type '(choice (const nil) directory)
  :group 'efrit-coverage)

(defcustom efrit-coverage-source-dirs '("lisp" "lisp/core" "lisp/support"
                                         "lisp/interfaces" "lisp/tools")
  "Directories containing source files to track."
  :type '(repeat string)
  :group 'efrit-coverage)

(defcustom efrit-coverage-exclude-patterns '("test-" "efrit-autonomous-")
  "Patterns to exclude from coverage tracking."
  :type '(repeat string)
  :group 'efrit-coverage)

;;; Internal Variables

(defvar efrit-coverage--data (make-hash-table :test 'equal)
  "Hash table mapping file paths to coverage data.
Each entry is an alist with:
  - total-forms: total instrumentable forms
  - covered-forms: forms that were executed
  - form-data: list of (line . covered-p) for each form")

(defvar efrit-coverage--instrumented-files nil
  "List of files that have been instrumented for coverage.")

(defvar efrit-coverage--session-active nil
  "Non-nil if coverage tracking is active.")

(defvar efrit-coverage--start-time nil
  "Time when coverage session started.")

;;; Output Directory

(defun efrit-coverage--output-dir ()
  "Return the coverage output directory, creating it if needed."
  (let ((dir (or efrit-coverage-output-dir
                 (expand-file-name "coverage"
                                   (expand-file-name ".efrit" "~/.emacs.d")))))
    (unless (file-directory-p dir)
      (make-directory dir t))
    dir))

;;; File Discovery

(defun efrit-coverage--find-source-files ()
  "Find all elisp source files to track for coverage."
  (let ((files '())
        (project-root (locate-dominating-file default-directory "Makefile")))
    (when project-root
      (dolist (subdir efrit-coverage-source-dirs)
        (let ((dir (expand-file-name subdir project-root)))
          (when (file-directory-p dir)
            (dolist (file (directory-files dir t "\\.el$"))
              (let ((name (file-name-nondirectory file)))
                (unless (or (string-prefix-p "." name)
                            (cl-some (lambda (pat)
                                      (string-match-p pat name))
                                    efrit-coverage-exclude-patterns))
                  (push file files))))))))
    (nreverse files)))

;;; Instrumentation

(defun efrit-coverage--instrument-file (file)
  "Instrument FILE for coverage tracking using testcover."
  (condition-case err
      (progn
        (testcover-start file)
        (push file efrit-coverage--instrumented-files)
        (message "[Coverage] Instrumented: %s" (file-name-nondirectory file))
        t)
    (error
     (message "[Coverage] Warning: Could not instrument %s: %s"
              (file-name-nondirectory file)
              (error-message-string err))
     nil)))

(defun efrit-coverage--collect-data-for-file (file)
  "Collect coverage data for FILE after tests have run."
  (let* ((buffer (find-buffer-visiting file))
         (data '())
         (total 0)
         (covered 0))
    (when buffer
      (with-current-buffer buffer
        ;; testcover stores data in buffer-local variables
        ;; Walk through instrumented points using overlays
        (save-excursion
          (goto-char (point-min))
          ;; Count forms and check which have edebug-coverage marks
          (while (not (eobp))
            (let ((ov (car (overlays-at (point)))))
              (when ov
                (let ((covered-p (not (overlay-get ov 'testcover-thing))))
                  (cl-incf total)
                  (when covered-p (cl-incf covered))
                  (push (cons (line-number-at-pos) covered-p) data))))
            (forward-char 1)))))

    ;; If no overlays yet, try to get count from testcover's internal data
    (when (zerop total)
      (setq total (efrit-coverage--count-forms file))
      (setq covered (efrit-coverage--count-covered-forms file)))

    (list (cons 'total-forms total)
          (cons 'covered-forms covered)
          (cons 'form-data (nreverse data)))))

(defun efrit-coverage--count-forms (file)
  "Count total instrumentable forms in FILE."
  (let ((count 0))
    (with-temp-buffer
      (insert-file-contents file)
      (emacs-lisp-mode)
      (goto-char (point-min))
      (condition-case nil
          (while t
            (forward-sexp)
            (cl-incf count))
        (scan-error nil)))
    count))

(defun efrit-coverage--count-covered-forms (file)
  "Count forms executed in FILE based on testcover data."
  ;; This is a heuristic - testcover doesn't expose this directly
  ;; We count forms that were evaluated by checking if the function
  ;; symbols have been called
  (let ((count 0)
        (buffer (find-buffer-visiting file)))
    (when buffer
      (with-current-buffer buffer
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward "^(defun \\([^ ]+\\)" nil t)
            (let ((fn (intern (match-string 1))))
              (when (and (fboundp fn)
                         (symbol-function fn))
                (cl-incf count)))))))
    count))

;;; Public API

;;;###autoload
(defun efrit-coverage-start ()
  "Start coverage tracking for all Efrit source files."
  (interactive)
  (setq efrit-coverage--data (make-hash-table :test 'equal))
  (setq efrit-coverage--instrumented-files nil)
  (setq efrit-coverage--start-time (current-time))
  (setq efrit-coverage--session-active t)

  (let ((files (efrit-coverage--find-source-files))
        (success 0)
        (failed 0))
    (message "")
    (message "========================================")
    (message "EFRIT COVERAGE: Starting Instrumentation")
    (message "========================================")
    (message "Found %d source files" (length files))

    (dolist (file files)
      (if (efrit-coverage--instrument-file file)
          (cl-incf success)
        (cl-incf failed)))

    (message "")
    (message "Instrumented: %d files" success)
    (when (> failed 0)
      (message "Skipped: %d files" failed))
    (message "========================================")
    (message ""))

  (message "[Coverage] Session started at %s"
           (format-time-string "%Y-%m-%d %H:%M:%S" efrit-coverage--start-time)))

;;;###autoload
(defun efrit-coverage-stop ()
  "Stop coverage tracking and collect data."
  (interactive)
  (unless efrit-coverage--session-active
    (error "No coverage session active"))

  (message "")
  (message "========================================")
  (message "EFRIT COVERAGE: Collecting Data")
  (message "========================================")

  ;; Collect data from all instrumented files
  (dolist (file efrit-coverage--instrumented-files)
    (let ((data (efrit-coverage--collect-data-for-file file)))
      (puthash file data efrit-coverage--data)
      (message "[Coverage] Collected: %s (%d/%d forms)"
               (file-name-nondirectory file)
               (alist-get 'covered-forms data)
               (alist-get 'total-forms data))))

  ;; Mark all uncovered forms in buffers
  (dolist (file efrit-coverage--instrumented-files)
    (let ((buffer (find-buffer-visiting file)))
      (when buffer
        (testcover-mark-all buffer))))

  (setq efrit-coverage--session-active nil)

  (message "")
  (message "Coverage data collected for %d files"
           (hash-table-count efrit-coverage--data))
  (message "========================================")
  (message ""))

;;;###autoload
(defun efrit-coverage-report ()
  "Generate and display a text coverage report."
  (interactive)
  (let ((total-forms 0)
        (total-covered 0)
        (report-lines '()))

    ;; Header
    (push "========================================" report-lines)
    (push "EFRIT COVERAGE REPORT" report-lines)
    (push (format "Generated: %s" (format-time-string "%Y-%m-%d %H:%M:%S")) report-lines)
    (push "========================================" report-lines)
    (push "" report-lines)

    ;; Per-module breakdown
    (push "Module Coverage:" report-lines)
    (push "----------------" report-lines)

    (maphash
     (lambda (file data)
       (let* ((forms (alist-get 'total-forms data 0))
              (covered (alist-get 'covered-forms data 0))
              (pct (if (> forms 0) (* 100.0 (/ (float covered) forms)) 0.0))
              (name (file-name-nondirectory file)))
         (cl-incf total-forms forms)
         (cl-incf total-covered covered)
         (push (format "  %-40s %5d/%5d (%5.1f%%)"
                      name covered forms pct)
               report-lines)))
     efrit-coverage--data)

    ;; Summary
    (push "" report-lines)
    (push "----------------------------------------" report-lines)
    (let ((total-pct (if (> total-forms 0)
                         (* 100.0 (/ (float total-covered) total-forms))
                       0.0)))
      (push (format "TOTAL: %d/%d forms covered (%.1f%%)"
                   total-covered total-forms total-pct)
            report-lines)

      ;; Threshold check
      (when (> efrit-coverage-threshold 0)
        (push "" report-lines)
        (if (>= total-pct efrit-coverage-threshold)
            (push (format "Threshold: PASSED (%.1f%% >= %.1f%%)"
                         total-pct efrit-coverage-threshold)
                  report-lines)
          (push (format "Threshold: FAILED (%.1f%% < %.1f%%)"
                       total-pct efrit-coverage-threshold)
                report-lines))))

    (push "========================================" report-lines)

    ;; Display report
    (let ((report (string-join (nreverse report-lines) "\n")))
      (with-current-buffer (get-buffer-create "*Efrit Coverage*")
        (erase-buffer)
        (insert report)
        (goto-char (point-min))
        (display-buffer (current-buffer)))
      (message "%s" report))))

;;;###autoload
(defun efrit-coverage-report-lcov (&optional output-file)
  "Generate LCOV format coverage report.
OUTPUT-FILE defaults to coverage/lcov.info in the coverage directory."
  (interactive)
  (let* ((output-file (or output-file
                          (expand-file-name "lcov.info"
                                           (efrit-coverage--output-dir))))
         (lcov-lines '()))

    (maphash
     (lambda (file data)
       (let ((form-data (alist-get 'form-data data)))
         ;; LCOV format:
         ;; TN: test name (optional)
         ;; SF: source file
         ;; DA: line,execution_count
         ;; LF: lines found
         ;; LH: lines hit
         ;; end_of_record
         (push (format "TN:efrit-tests") lcov-lines)
         (push (format "SF:%s" file) lcov-lines)

         ;; Add line data
         (let ((line-hits (make-hash-table :test 'eql)))
           (dolist (entry form-data)
             (let ((line (car entry))
                   (covered (cdr entry)))
               (puthash line (if covered 1 0) line-hits)))

           (maphash
            (lambda (line hits)
              (push (format "DA:%d,%d" line hits) lcov-lines))
            line-hits)

           ;; Summary for this file
           (push (format "LF:%d" (alist-get 'total-forms data 0)) lcov-lines)
           (push (format "LH:%d" (alist-get 'covered-forms data 0)) lcov-lines))

         (push "end_of_record" lcov-lines)))
     efrit-coverage--data)

    ;; Write to file
    (with-temp-file output-file
      (insert (string-join (nreverse lcov-lines) "\n")))

    (message "[Coverage] LCOV report written to: %s" output-file)
    output-file))

;;;###autoload
(defun efrit-coverage-report-json (&optional output-file)
  "Generate JSON format coverage report.
OUTPUT-FILE defaults to coverage/coverage.json in the coverage directory."
  (interactive)
  (let* ((output-file (or output-file
                          (expand-file-name "coverage.json"
                                           (efrit-coverage--output-dir))))
         (report-data '())
         (total-forms 0)
         (total-covered 0))

    (maphash
     (lambda (file data)
       (let ((forms (alist-get 'total-forms data 0))
             (covered (alist-get 'covered-forms data 0)))
         (cl-incf total-forms forms)
         (cl-incf total-covered covered)
         (push `((file . ,file)
                 (total_forms . ,forms)
                 (covered_forms . ,covered)
                 (coverage_pct . ,(if (> forms 0)
                                     (* 100.0 (/ (float covered) forms))
                                   0.0)))
               report-data)))
     efrit-coverage--data)

    (let* ((total-pct (if (> total-forms 0)
                          (* 100.0 (/ (float total-covered) total-forms))
                        0.0))
           (json-data `((generated_at . ,(format-time-string "%Y-%m-%dT%H:%M:%S%z"))
                        (total_forms . ,total-forms)
                        (covered_forms . ,total-covered)
                        (coverage_pct . ,total-pct)
                        (threshold . ,efrit-coverage-threshold)
                        (threshold_passed . ,(>= total-pct efrit-coverage-threshold))
                        (modules . ,(vconcat (nreverse report-data))))))

      (with-temp-file output-file
        (insert (json-encode json-data)))

      (message "[Coverage] JSON report written to: %s" output-file)
      output-file)))

;;;###autoload
(defun efrit-coverage-check-threshold (&optional threshold)
  "Check if coverage meets THRESHOLD percentage.
Returns t if coverage >= threshold, nil otherwise.
Signals an error in batch mode if threshold not met."
  (interactive "nCoverage threshold (%): ")
  (let ((threshold (or threshold efrit-coverage-threshold))
        (total-forms 0)
        (total-covered 0))

    (maphash
     (lambda (_file data)
       (cl-incf total-forms (alist-get 'total-forms data 0))
       (cl-incf total-covered (alist-get 'covered-forms data 0)))
     efrit-coverage--data)

    (let ((actual-pct (if (> total-forms 0)
                          (* 100.0 (/ (float total-covered) total-forms))
                        0.0)))
      (if (>= actual-pct threshold)
          (progn
            (message "[Coverage] PASSED: %.1f%% >= %.1f%%" actual-pct threshold)
            t)
        (progn
          (message "[Coverage] FAILED: %.1f%% < %.1f%%" actual-pct threshold)
          (when noninteractive
            (kill-emacs 1))
          nil)))))

;;; Integration with Test Runner

;;;###autoload
(defun efrit-coverage-run-tests-with-coverage ()
  "Run all tests with coverage tracking enabled."
  (interactive)
  (require 'efrit-test-runner)

  ;; Start coverage
  (efrit-coverage-start)

  ;; Run tests
  (unwind-protect
      (progn
        (efrit-test-register-all-tiers)
        (efrit-test-run-all))

    ;; Always stop coverage and generate report
    (efrit-coverage-stop)
    (efrit-coverage-report)
    (efrit-coverage-report-lcov)
    (efrit-coverage-report-json)))

;;; Simple Function Coverage (Alternative to testcover)

;; testcover.el can be complex. Here's a simpler approach that tracks
;; which functions were called during tests.

(defvar efrit-coverage--function-calls (make-hash-table :test 'eq)
  "Hash table tracking function call counts.")

(defvar efrit-coverage--advised-functions nil
  "Alist of (function . tracker-fn) that have been advised for coverage tracking.")

(defun efrit-coverage-simple-start ()
  "Start simple function-level coverage tracking.
This is lighter weight than testcover and just tracks which
functions get called.

NOTE: Source files must be loaded BEFORE calling this function
for their functions to be tracked.  Typical usage:

  (require 'efrit)
  (require 'efrit-do)
  ;; ... load all modules you want coverage for ...
  (efrit-coverage-simple-start)
  ;; ... run tests ...
  (efrit-coverage-simple-report)
  (efrit-coverage-simple-stop)"
  (interactive)
  (setq efrit-coverage--function-calls (make-hash-table :test 'eq))
  (setq efrit-coverage--advised-functions nil)
  (setq efrit-coverage--session-active t)
  (setq efrit-coverage--start-time (current-time))

  (let ((files (efrit-coverage--find-source-files))
        (fn-count 0)
        (total-functions 0))
    (message "")
    (message "========================================")
    (message "EFRIT COVERAGE: Simple Function Tracking")
    (message "========================================")

    (dolist (file files)
      (with-temp-buffer
        (insert-file-contents file)
        (emacs-lisp-mode)
        (goto-char (point-min))
        (while (re-search-forward "^(defun \\([^ \n(]+\\)" nil t)
          (let* ((name (match-string 1))
                 (fn (intern-soft name)))
            (cl-incf total-functions)
            (when (and fn (fboundp fn))
              (let ((tracker (lambda (&rest _args)
                              (cl-incf (gethash fn efrit-coverage--function-calls 0)))))
                (advice-add fn :before tracker)
                (push (cons fn tracker) efrit-coverage--advised-functions)
                (cl-incf fn-count)))))))

    (message "Tracking %d/%d functions across %d files"
             fn-count total-functions (length files))
    (when (< fn-count total-functions)
      (message "Note: %d functions not loaded (require modules before starting coverage)"
               (- total-functions fn-count)))
    (message "========================================")
    (message "")))

(defun efrit-coverage-simple-stop ()
  "Stop simple coverage tracking and remove advice."
  (interactive)
  (dolist (entry efrit-coverage--advised-functions)
    (advice-remove (car entry) (cdr entry)))
  (setq efrit-coverage--advised-functions nil)
  (setq efrit-coverage--session-active nil))

(defun efrit-coverage-simple-report ()
  "Generate a simple function coverage report."
  (interactive)
  (let ((total (length efrit-coverage--advised-functions))
        (called 0)
        (uncalled '()))

    ;; Count called vs uncalled - entries are now (fn . tracker) cons cells
    (dolist (entry efrit-coverage--advised-functions)
      (let ((fn (car entry)))
        (if (> (gethash fn efrit-coverage--function-calls 0) 0)
            (cl-incf called)
          (push fn uncalled))))

    ;; Print summary to messages
    (message "")
    (message "========================================")
    (message "EFRIT SIMPLE COVERAGE REPORT")
    (message "Generated: %s" (format-time-string "%Y-%m-%d %H:%M:%S"))
    (message "========================================")
    (message "")
    (message "Functions Called: %d/%d (%.1f%%)"
             called total
             (if (> total 0) (* 100.0 (/ (float called) total)) 0.0))
    (when uncalled
      (message "")
      (message "Uncalled Functions: %d" (length uncalled)))
    (message "========================================")
    (message "")

    ;; Also create buffer for interactive use
    (with-current-buffer (get-buffer-create "*Efrit Coverage*")
      (erase-buffer)
      (insert "========================================\n")
      (insert "EFRIT SIMPLE COVERAGE REPORT\n")
      (insert (format "Generated: %s\n"
                     (format-time-string "%Y-%m-%d %H:%M:%S")))
      (insert "========================================\n\n")

      (insert (format "Functions Called: %d/%d (%.1f%%)\n\n"
                     called total
                     (if (> total 0) (* 100.0 (/ (float called) total)) 0.0)))

      (when uncalled
        (insert "Uncalled Functions:\n")
        (insert "-------------------\n")
        (dolist (fn (sort uncalled (lambda (a b)
                                     (string< (symbol-name a)
                                             (symbol-name b)))))
          (insert (format "  %s\n" fn))))

      (goto-char (point-min))
      (unless noninteractive
        (display-buffer (current-buffer))))))

(provide 'efrit-coverage)

;;; efrit-coverage.el ends here
