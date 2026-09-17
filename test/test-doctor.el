;;; test-doctor.el --- Tests for efrit-doctor -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'efrit-doctor)

(defun test-doctor--levels (&rest checks)
  "Run CHECKS (functions) on a fresh findings list; return (LEVEL . TITLE) list."
  (setq efrit-doctor--findings nil)
  (dolist (c checks) (funcall c))
  (mapcar (lambda (f) (cons (nth 0 f) (nth 1 f))) (reverse efrit-doctor--findings)))

(defun test-doctor--has (levels level regexp)
  (cl-some (lambda (l) (and (eq (car l) level) (string-match-p regexp (cdr l)))) levels))

(defun test-doctor--fix-for (regexp)
  "Return the fix function of the first finding whose title matches REGEXP."
  (cl-some (lambda (f) (and (string-match-p regexp (nth 1 f)) (nth 4 f)))
           efrit-doctor--findings))

;;; Variables

(ert-deftest test-doctor-flags-obsolete-variables-with-fix ()
  (defvar efrit-model)
  (let ((efrit-model "old-model")
        (efrit-default-model "new-model")
        (efrit-api-auth-scheme 'x-api-key))
    (let ((levels (test-doctor--levels #'efrit-doctor--check-variables)))
      (should (test-doctor--has levels 'warn "efrit-model is set but obsolete")))
    (funcall (test-doctor--fix-for "efrit-model is set"))
    (should (equal efrit-default-model "old-model"))))

(ert-deftest test-doctor-bad-auth-scheme ()
  (let ((efrit-api-auth-scheme 'basic) (efrit-default-model "m"))
    (let ((levels (test-doctor--levels #'efrit-doctor--check-variables)))
      (should (test-doctor--has levels 'fail "bad value")))
    (funcall (test-doctor--fix-for "bad value"))
    (should (eq efrit-api-auth-scheme 'x-api-key))))

;;; Credentials

(ert-deftest test-doctor-credentials-format-mismatch-offers-bearer ()
  (let ((efrit-api-key "gateway-token-not-sk-format-1234")
        (efrit-api-auth-scheme 'x-api-key))
    (let ((levels (test-doctor--levels #'efrit-doctor--check-credentials)))
      (should (test-doctor--has levels 'fail "does not look like an Anthropic key")))
    (funcall (test-doctor--fix-for "does not look like"))
    (should (eq efrit-api-auth-scheme 'bearer))
    (should (test-doctor--has (test-doctor--levels #'efrit-doctor--check-credentials)
                              'ok "API key resolves"))))

(ert-deftest test-doctor-credentials-missing ()
  (let ((efrit-api-key nil) (process-environment (cons "ANTHROPIC_API_KEY" process-environment)))
    (cl-letf (((symbol-function 'auth-source-search) (lambda (&rest _) nil)))
      (should (test-doctor--has (test-doctor--levels #'efrit-doctor--check-credentials)
                                'fail "No API key found")))))

(ert-deftest test-doctor-anthropic-key-with-bearer-warns ()
  (let ((efrit-api-key "sk-ant-api03-something-long-enough-here")
        (efrit-api-auth-scheme 'bearer))
    (should (test-doctor--has (test-doctor--levels #'efrit-doctor--check-credentials)
                              'warn "Bearer scheme with an Anthropic key"))))

;;; Endpoint (no network: stub the probe)

(defmacro test-doctor--with-probe (result &rest body)
  "Make the TCP probe succeed (RESULT non-nil) or fail."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'make-network-process)
              (lambda (&rest _) (if ,result 'fake-proc (error "Connection refused"))))
             ((symbol-function 'delete-process) #'ignore))
     ,@body))

(ert-deftest test-doctor-endpoint-double-suffix ()
  (let ((efrit-api-base-url "https://gw.example.com/v1/messages")
        (efrit-api-url nil) (efrit-api-auth-scheme 'bearer))
    (test-doctor--with-probe t
      (should (test-doctor--has (test-doctor--levels #'efrit-doctor--check-endpoint)
                                'fail "Double /v1/messages")))
    (funcall (test-doctor--fix-for "Double"))
    (should (equal efrit-api-base-url "https://gw.example.com"))))

(ert-deftest test-doctor-endpoint-scheme-host-mismatch ()
  (let ((efrit-api-url nil))
    (let ((efrit-api-base-url "https://gw.example.com/anthropic") (efrit-api-auth-scheme 'x-api-key))
      (test-doctor--with-probe t
        (should (test-doctor--has (test-doctor--levels #'efrit-doctor--check-endpoint)
                                  'warn "Non-Anthropic host with x-api-key"))))
    (let ((efrit-api-base-url "https://api.anthropic.com") (efrit-api-auth-scheme 'bearer))
      (test-doctor--with-probe t
        (should (test-doctor--has (test-doctor--levels #'efrit-doctor--check-endpoint)
                                  'fail "api.anthropic.com with bearer"))))))

(ert-deftest test-doctor-endpoint-unreachable ()
  (let ((efrit-api-base-url "https://api.anthropic.com") (efrit-api-url nil)
        (efrit-api-auth-scheme 'x-api-key))
    (test-doctor--with-probe nil
      (should (test-doctor--has (test-doctor--levels #'efrit-doctor--check-endpoint)
                                'fail "Cannot connect")))
    (test-doctor--with-probe t
      (should (test-doctor--has (test-doctor--levels #'efrit-doctor--check-endpoint)
                                'ok "TCP\\(/TLS\\)? to")))))

(ert-deftest test-doctor-redacts-urls ()
  (should (equal (efrit-doctor--redact-url "https://u:p@gw.example.com/secret/path?k=v")
                 "https://gw.example.com"))
  (should (equal (efrit-doctor--redact-url "http://h.example.com:8080/x")
                 "http://h.example.com:8080")))

;;; Live check (stubbed transport)

(defmacro test-doctor--with-api (responder &rest body)
  "Stub efrit-api-request-async; RESPONDER gets (req ok-cb err-cb)."
  (declare (indent 1))
  `(progn
     (require 'efrit-api)
     (cl-letf (((symbol-function 'efrit-api-request-async) ,responder))
       ,@body)))

(defun test-doctor--pong ()
  (let ((h (make-hash-table :test 'equal)) (blk (make-hash-table :test 'equal)))
    (puthash "text" "pong" blk) (puthash "content" (vector blk) h) h))

(ert-deftest test-doctor-live-ok-and-caching-ok ()
  (let ((efrit-api-prompt-caching t) (efrit-default-model "m"))
    (test-doctor--with-api (lambda (_req ok _err) (funcall ok (test-doctor--pong)))
      (let ((levels (test-doctor--levels #'efrit-doctor--check-live)))
        (should (test-doctor--has levels 'ok "answered"))
        (should (test-doctor--has levels 'ok "accepts cache_control"))))))

(ert-deftest test-doctor-live-caching-rejected-offers-fix ()
  (let ((efrit-api-prompt-caching t) (efrit-default-model "m"))
    (test-doctor--with-api
        (lambda (req ok err)
          (if (vectorp (cdr (assoc "system" req)))   ; cache_control form
              (funcall err "API Error (invalid_request_error): cache_control not supported")
            (funcall ok (test-doctor--pong))))
      (let ((levels (test-doctor--levels #'efrit-doctor--check-live)))
        (should (test-doctor--has levels 'ok "answered"))
        (should (test-doctor--has levels 'warn "rejects cache_control")))
      (funcall (test-doctor--fix-for "rejects cache_control"))
      (should-not efrit-api-prompt-caching))))

(ert-deftest test-doctor-live-classifies-http-errors ()
  (let ((efrit-api-prompt-caching nil) (efrit-default-model "m"))
    (dolist (case '(("HTTP error: (error http 401)" . "401")
                    ("API Error (not_found_error): model: m not found" . "not available at this endpoint")
                    ("API Error (api_error): no keys found that support model: claude-sonnet-4-5" . "not available at this endpoint")
                    ("HTTP error: (error http 404)" . "404 from endpoint")
                    ("HTTP error: (error http 403)" . "403")))
      (test-doctor--with-api (lambda (_req _ok err) (funcall err (car case)))
        (should (test-doctor--has (test-doctor--levels #'efrit-doctor--check-live)
                                  'fail (cdr case)))))))

;;; Permissions / context / whole run

(ert-deftest test-doctor-model-error-offers-select-model ()
  (let ((efrit-api-prompt-caching nil) (efrit-default-model "m") (called nil))
    (test-doctor--with-api
        (lambda (_req _ok err) (funcall err "API Error (api_error): no keys found that support model: m"))
      (test-doctor--levels #'efrit-doctor--check-live)
      (cl-letf (((symbol-function 'efrit-select-model) (lambda (&optional p) (setq called p))))
        (funcall (test-doctor--fix-for "not available")))
      (should (eq called t)))))

(ert-deftest test-doctor-permissions ()
  (require 'efrit-permissions)
  (let ((efrit-permission-policy nil) (efrit-permission-responder-function nil))
    (should (test-doctor--has (test-doctor--levels #'efrit-doctor--check-permissions)
                              'warn "policy is nil"))
    (funcall (test-doctor--fix-for "policy is nil"))
    (should (equal efrit-permission-policy '(write exec))))
  (let ((efrit-permission-policy '(write exec))
        (efrit-permission-responder-function (lambda (_) 'bogus)))
    (should (test-doctor--has (test-doctor--levels #'efrit-doctor--check-permissions)
                              'fail "unexpected")))
  (let ((efrit-permission-policy '(write exec))
        (efrit-permission-responder-function (lambda (_) (error "x"))))
    (should (test-doctor--has (test-doctor--levels #'efrit-doctor--check-permissions)
                              'fail "signalled"))))

(ert-deftest test-doctor-credentials-named-function ()
  (defun test-doctor--key () "gateway-token-xyz")
  (let ((efrit-api-key #'test-doctor--key) (efrit-api-auth-scheme 'bearer))
    (let ((levels (test-doctor--levels #'efrit-doctor--check-credentials)))
      (should (test-doctor--has levels 'ok "via function test-doctor--key")))))

(ert-deftest test-doctor-context-and-hooks ()
  (require 'efrit-context-sources)
  (let ((efrit-context-sources '(buffer bogus-source))
        (efrit-system-prompt-functions (list (lambda (_) 42))))
    (let ((levels (test-doctor--levels #'efrit-doctor--check-context)))
      (should (test-doctor--has levels 'warn "Unknown context sources"))
      (should (test-doctor--has levels 'ok "Editor context renders"))
      (should (test-doctor--has levels 'warn "returned integer")))))

(ert-deftest test-doctor-full-run-produces-buffer-and-never-signals ()
  "A whole static run on a deliberately broken config must complete."
  (let ((efrit-api-key "not-a-key") (efrit-api-auth-scheme 'x-api-key)
        (efrit-api-base-url "https://api.anthropic.com") (efrit-api-url nil)
        (efrit-doctor-live-checks nil))
    (test-doctor--with-probe nil
      (cl-letf (((symbol-function 'display-buffer) #'ignore))
        (should-not (efrit-doctor))            ; returns nil: there are failures
        (with-current-buffer "*efrit-doctor*"
          (should (string-match-p "problem(s)" (buffer-string)))
          (should (string-match-p "✗" (buffer-string)))
          ;; never leak the key value
          (should-not (string-match-p "not-a-key" (buffer-string))))))))

(provide 'test-doctor)
;;; test-doctor.el ends here
