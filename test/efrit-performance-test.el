;;; efrit-performance-test.el --- Performance profiling for Efrit -*- lexical-binding: t -*-

;;; Commentary:
;; Performance tests to identify bottlenecks and measure optimization impact

;;; Code:

(require 'efrit)
(require 'benchmark)
(require 'profiler)

(defvar efrit-perf-results nil
  "Collection of performance test results.")

(defmacro efrit-perf-measure (name &rest body)
  "Measure performance of BODY and store results under NAME."
  (declare (indent 1))
  `(let* ((start-time (float-time))
          (start-mem (garbage-collect))
          (result (progn ,@body))
          (end-time (float-time))
          (elapsed (- end-time start-time)))
     (push (list ,name 
                 :elapsed elapsed
                 :timestamp (current-time))
           efrit-perf-results)
     (message "%s: %.3fs" ,name elapsed)
     result))

(defun efrit-perf-test-api-roundtrip ()
  "Test basic API roundtrip time."
  (efrit-perf-measure "API roundtrip"
    (efrit-do "What is 2+2?")))

(defun efrit-perf-test-context-capture ()
  "Test context capture performance with various buffer sizes."
  ;; Small buffer
  (with-temp-buffer
    (insert (make-string 100 ?x))
    (efrit-perf-measure "Context capture (100 chars)"
      (efrit-context-capture-state)))
  
  ;; Medium buffer
  (with-temp-buffer
    (insert (make-string 10000 ?x))
    (efrit-perf-measure "Context capture (10K chars)"
      (efrit-context-capture-state)))
  
  ;; Large buffer
  (with-temp-buffer
    (insert (make-string 100000 ?x))
    (efrit-perf-measure "Context capture (100K chars)"
      (efrit-context-capture-state))))

(defun efrit-perf-test-work-log-compression ()
  "Test work log compression performance."
  ;; Create sample work logs of different sizes
  (let ((small-log (cl-loop repeat 10
                           collect (cons "(message \"test\")" "test")))
        (medium-log (cl-loop repeat 100
                            collect (cons "(message \"test\")" "test")))
        (large-log (cl-loop repeat 1000
                           collect (cons "(message \"test\")" "test"))))
    
    (efrit-perf-measure "Compress small work log (10 items)"
      (efrit-context-compress-work-log small-log))
    
    (efrit-perf-measure "Compress medium work log (100 items)"
      (efrit-context-compress-work-log medium-log))
    
    (efrit-perf-measure "Compress large work log (1000 items)"
      (efrit-context-compress-work-log large-log))))

(defun efrit-perf-test-async-queue ()
  "Test async queue processing performance."
  (let ((commands (cl-loop for i from 1 to 5
                          collect (format "What is %d + %d?" i i))))
    
    (efrit-perf-measure "Queue 5 commands"
      (dolist (cmd commands)
        (efrit-async-execute-command cmd #'ignore))
      (length efrit-async--session-queue))))

(defun efrit-perf-test-json-parsing ()
  "Test JSON parsing performance for typical responses."
  (let* ((small-response (json-encode '((result . "test") (status . "complete"))))
         (medium-response (json-encode `((result . ,(make-string 10000 ?x)) 
                                       (status . "complete"))))
         (large-response (json-encode `((result . ,(make-string 100000 ?x))
                                      (tools . ,(cl-loop repeat 10
                                                        collect '((name . "test")
                                                                 (input . "data"))))
                                      (status . "complete")))))
    
    (efrit-perf-measure "Parse small JSON"
      (json-read-from-string small-response))
    
    (efrit-perf-measure "Parse medium JSON"
      (json-read-from-string medium-response))
    
    (efrit-perf-measure "Parse large JSON"
      (json-read-from-string large-response))))

(defun efrit-perf-run-all ()
  "Run all performance tests."
  (interactive)
  (setq efrit-perf-results nil)
  (message "\n=== Efrit Performance Tests ===")
  
  ;; Non-API tests first
  (message "\n-- Local Performance Tests --")
  (efrit-perf-test-context-capture)
  (efrit-perf-test-work-log-compression)
  (efrit-perf-test-json-parsing)
  
  ;; API tests
  (when (y-or-n-p "\nRun API performance tests? (will consume tokens) ")
    (message "\n-- API Performance Tests --")
    (efrit-perf-test-api-roundtrip)
    (efrit-perf-test-async-queue))
  
  ;; Summary
  (message "\n-- Performance Summary --")
  (dolist (result (reverse efrit-perf-results))
    (let ((name (car result))
          (elapsed (plist-get (cdr result) :elapsed)))
      (message "%-35s: %.3fs" name elapsed))))

(defun efrit-perf-profile-command (command)
  "Profile execution of COMMAND using Emacs profiler."
  (interactive "sCommand to profile: ")
  (profiler-start 'cpu+mem)
  (unwind-protect
      (efrit-do command)
    (profiler-stop)
    (profiler-report)))

(provide 'efrit-performance-test)
;;; efrit-performance-test.el ends here