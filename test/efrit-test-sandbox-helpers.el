;;; efrit-test-sandbox-helpers.el --- run tool tests under a permissive sandbox -*- lexical-binding: t; -*-

;;; Commentary:
;; Tool tests exercise the tool, not the sandbox.  Loading this file
;; grants every capability for the duration of the test run so those
;; suites behave as before.  test-sandbox.el binds its own tables and
;; is unaffected.  Individual tests can still `let'-bind
;; `efrit-sandbox-request-function' to nil to observe a denial.

;;; Code:

(require 'efrit-sandbox)

(setq efrit-sandbox-request-function
      (lambda (req)
        ;; grant whatever is asked, for the session, in whatever root
        (ignore req)
        'session))

(provide 'efrit-test-sandbox-helpers)
;;; efrit-test-sandbox-helpers.el ends here
