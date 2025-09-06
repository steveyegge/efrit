;;; efrit-performance-batch.el --- Non-interactive performance tests -*- lexical-binding: t -*-

;;; Code:

(require 'efrit-performance-test)

;; Run only non-API tests
(setq efrit-perf-results nil)
(message "\n=== Efrit Performance Tests (Batch Mode) ===")

(message "\n-- Local Performance Tests --")
(efrit-perf-test-context-capture)
(efrit-perf-test-work-log-compression)
(efrit-perf-test-json-parsing)

;; Summary
(message "\n-- Performance Summary --")
(dolist (result (reverse efrit-perf-results))
  (let ((name (car result))
        (elapsed (plist-get (cdr result) :elapsed)))
    (message "%-35s: %.3fs" name elapsed)))

(provide 'efrit-performance-batch)
;;; efrit-performance-batch.el ends here