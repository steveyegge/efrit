;;; test-efrit-metrics.el --- Tests for efrit-metrics module -*- lexical-binding: t -*-

(require 'ert)

;; Add load paths
(add-to-list 'load-path (expand-file-name "../lisp" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../lisp/core" (file-name-directory load-file-name)))

(require 'efrit-metrics)

;;; Creation Tests

(ert-deftest efrit-metrics-make-default ()
  "Test creating metrics with defaults."
  (let ((metrics (efrit-metrics-make)))
    (should (efrit-metrics-p metrics))
    (should (equal (efrit-metrics-get-commands-executed metrics) 0))
    (should (equal (efrit-metrics-get-api-calls metrics) 0))
    (should (equal (efrit-metrics-get-todos-created metrics) 0))))

(ert-deftest efrit-metrics-make-full ()
  "Test creating metrics with all fields."
  (let* ((tools '(("read-file" . 3) ("write-file" . 2)))
         (files '("/tmp/test.el" "/tmp/test2.el"))
         (buffers '("*test1*" "*test2*"))
         (metrics (efrit-metrics-make 10 5 3 20 2 1
                                      buffers files tools
                                      "session-123" (current-time) 150.5)))
    (should (equal (efrit-metrics-get-commands-executed metrics) 10))
    (should (equal (efrit-metrics-get-todos-created metrics) 5))
    (should (equal (efrit-metrics-get-todos-completed metrics) 3))
    (should (equal (efrit-metrics-get-api-calls metrics) 20))
    (should (equal (efrit-metrics-get-api-errors metrics) 2))
    (should (equal (efrit-metrics-get-errors metrics) 1))
    (should (equal (length (efrit-metrics-get-files-modified metrics)) 2))
    (should (equal (length (efrit-metrics-get-buffers-created metrics)) 2))))

;;; Accessor Tests

(ert-deftest efrit-metrics-get-todos-active ()
  "Test calculating active TODO count."
  (let ((metrics (efrit-metrics-make 0 10 7)))
    (should (equal (efrit-metrics-get-todos-active metrics) 3))))

(ert-deftest efrit-metrics-get-todos-active-zero ()
  "Test active TODOs when all completed."
  (let ((metrics (efrit-metrics-make 0 5 5)))
    (should (equal (efrit-metrics-get-todos-active metrics) 0))))

(ert-deftest efrit-metrics-get-todos-active-negative ()
  "Test active TODOs prevents negative numbers."
  (let ((metrics (efrit-metrics-make 0 3 10)))
    (should (equal (efrit-metrics-get-todos-active metrics) 0))))

;;; Alist Conversion Tests

(ert-deftest efrit-metrics-from-alist ()
  "Test creating metrics from alist."
  (let* ((alist '((commands-executed . 5)
                  (todos-created . 3)
                  (todos-completed . 1)
                  (api-calls . 10)))
         (metrics (efrit-metrics-from-alist alist)))
    (should (equal (efrit-metrics-get-commands-executed metrics) 5))
    (should (equal (efrit-metrics-get-todos-created metrics) 3))
    (should (equal (efrit-metrics-get-todos-completed metrics) 1))
    (should (equal (efrit-metrics-get-api-calls metrics) 10))))

(ert-deftest efrit-metrics-from-alist-with-lists ()
  "Test creating metrics from alist with list fields."
  (let* ((alist '((commands-executed . 5)
                  (files-modified . ("/tmp/test1.el" "/tmp/test2.el"))
                  (buffers-created . ("*test1*" "*test2*"))
                  (tools-used . (("read-file" . 3)))))
         (metrics (efrit-metrics-from-alist alist)))
    (should (equal (length (efrit-metrics-get-files-modified metrics)) 2))
    (should (equal (length (efrit-metrics-get-buffers-created metrics)) 2))
    (should (equal (length (efrit-metrics-get-tools-used metrics)) 1))))

(ert-deftest efrit-metrics-to-alist ()
  "Test converting metrics to alist."
  (let* ((metrics (efrit-metrics-make 5 3 1 10 0 0 nil nil nil))
         (alist (efrit-metrics-to-alist metrics)))
    (should (equal (alist-get 'commands-executed alist) 5))
    (should (equal (alist-get 'todos-created alist) 3))
    (should (equal (alist-get 'todos-completed alist) 1))
    (should (equal (alist-get 'api-calls alist) 10))))

(ert-deftest efrit-metrics-round-trip ()
  "Test round-trip conversion: metrics -> alist -> metrics."
  (let* ((original (efrit-metrics-make 5 3 1 10 2 1
                                       '("*test*") '("/tmp/test.el")
                                       '(("tool" . 3))))
         (alist (efrit-metrics-to-alist original))
         (restored (efrit-metrics-from-alist alist)))
    (should (equal (efrit-metrics-get-commands-executed original)
                   (efrit-metrics-get-commands-executed restored)))
    (should (equal (efrit-metrics-get-todos-created original)
                   (efrit-metrics-get-todos-created restored)))
    (should (equal (efrit-metrics-get-todos-completed original)
                   (efrit-metrics-get-todos-completed restored)))
    (should (equal (efrit-metrics-get-api-calls original)
                   (efrit-metrics-get-api-calls restored)))
    (should (equal (efrit-metrics-get-buffers-created original)
                   (efrit-metrics-get-buffers-created restored)))
    (should (equal (efrit-metrics-get-files-modified original)
                   (efrit-metrics-get-files-modified restored)))))

;;; Display Tests

(ert-deftest efrit-metrics-format-for-dashboard ()
  "Test formatting metrics for display."
  (let ((metrics (efrit-metrics-make 5 10 7 20 0 0)))
    (let ((formatted (efrit-metrics-format-for-dashboard metrics)))
      (should (stringp formatted))
      (should (string-match "Commands: 5" formatted))
      (should (string-match "TODOs: 10 created" formatted))
      (should (string-match "3 active" formatted)))))

;;; Statistics Tests

(ert-deftest efrit-metrics-total-actions ()
  "Test calculating total actions."
  (let ((metrics (efrit-metrics-make 5 10 0 20)))
    (should (equal (efrit-metrics-total-actions metrics) 35))))

(ert-deftest efrit-metrics-error-rate-zero ()
  "Test error rate with no errors."
  (let ((metrics (efrit-metrics-make 5 0 0 20 0 0)))
    (should (= (efrit-metrics-error-rate metrics) 0))))

(ert-deftest efrit-metrics-error-rate-nonzero ()
  "Test error rate with some errors."
  (let ((metrics (efrit-metrics-make 50 0 0 0 0 10)))
    (should (= (efrit-metrics-error-rate metrics) 20.0))))

(ert-deftest efrit-metrics-error-rate-zero-actions ()
  "Test error rate with no actions."
  (let ((metrics (efrit-metrics-make 0 0 0 0 0 0)))
    (should (= (efrit-metrics-error-rate metrics) 0))))

(ert-deftest efrit-metrics-unique-files-modified ()
  "Test counting unique files modified."
  (let* ((files '("/tmp/test1.el" "/tmp/test2.el" "/tmp/test3.el"))
         (metrics (efrit-metrics-make 0 0 0 0 0 0 nil files)))
    (should (equal (efrit-metrics-unique-files-modified metrics) 3))))

(ert-deftest efrit-metrics-unique-buffers-created ()
  "Test counting unique buffers created."
  (let* ((buffers '("*test1*" "*test2*"))
         (metrics (efrit-metrics-make 0 0 0 0 0 0 buffers)))
    (should (equal (efrit-metrics-unique-buffers-created metrics) 2))))

(ert-deftest efrit-metrics-unique-tools-used ()
  "Test counting unique tools used."
  (let* ((tools '(("read-file" . 3) ("write-file" . 2) ("search" . 5)))
         (metrics (efrit-metrics-make 0 0 0 0 0 0 nil nil tools)))
    (should (equal (efrit-metrics-unique-tools-used metrics) 3))))

(ert-deftest efrit-metrics-most-used-tool ()
  "Test finding the most used tool."
  (let* ((tools '(("read-file" . 3) ("write-file" . 2) ("search" . 5)))
         (metrics (efrit-metrics-make 0 0 0 0 0 0 nil nil tools)))
    (should (equal (efrit-metrics-most-used-tool metrics) "search"))))

(ert-deftest efrit-metrics-most-used-tool-none ()
  "Test most used tool when no tools used."
  (let ((metrics (efrit-metrics-make 0 0 0 0 0 0)))
    (should (equal (efrit-metrics-most-used-tool metrics) nil))))

;;; Edge Cases

(ert-deftest efrit-metrics-with-nil-lists ()
  "Test metrics with nil for list fields."
  (let ((metrics (efrit-metrics-make 5 3 1 10 0 0 nil nil nil)))
    (should (equal (efrit-metrics-get-buffers-created metrics) nil))
    (should (equal (efrit-metrics-get-files-modified metrics) nil))
    (should (equal (efrit-metrics-unique-buffers-created metrics) 0))
    (should (equal (efrit-metrics-unique-files-modified metrics) 0))))

(ert-deftest efrit-metrics-all-zeros ()
  "Test metrics with all zero values."
  (let ((metrics (efrit-metrics-make)))
    (should (equal (efrit-metrics-total-actions metrics) 0))
    (should (equal (efrit-metrics-error-rate metrics) 0))
    (should (equal (efrit-metrics-get-todos-active metrics) 0))))

(ert-deftest efrit-metrics-large-values ()
  "Test metrics with large values."
  (let ((metrics (efrit-metrics-make 1000000 500000 400000 2000000)))
    (should (equal (efrit-metrics-get-commands-executed metrics) 1000000))
    (should (equal (efrit-metrics-get-todos-active metrics) 100000))
    (should (equal (efrit-metrics-total-actions metrics) 3500000))))

(provide 'test-efrit-metrics)

;;; test-efrit-metrics.el ends here
