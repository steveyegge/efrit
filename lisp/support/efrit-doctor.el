;;; efrit-doctor.el --- Verify every layer of the configuration -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; `M-x efrit-doctor' walks the configuration from the bottom up and
;; reports each layer as OK / WARN / FAIL with a concrete suggestion,
;; and where a fix is mechanical, a button that applies it:
;;
;;   1. Installation   load-path, shadowed copies, byte-compiled staleness
;;   2. Variables      obsolete names still set, type mismatches
;;   3. Credentials    key resolves, format matches the auth scheme
;;   4. Endpoint       URL shape, TCP/TLS reachability
;;   4b. Transport     streaming on/off, curl present and new enough
;;   5. Model          a real one-token request round-trips (opt-in)
;;   6. Caching        cache_control accepted by the endpoint (with 5)
;;   7. Sandbox        project root, remote host, data directory
;;   8. Tramp          remote root has git/rg/sh on the remote PATH
;;   9. Permissions    policy sane, responder callable
;;  10. Context        sources resolve, snapshot renders
;;  11. UI             agent buffer mode, SVG header support
;;
;; Layers 5 and 6 spend a few tokens; they run only with a prefix
;; argument (C-u M-x efrit-doctor) or `efrit-doctor-live-checks'.
;;
;; The report is written so that pasting it into a bug report is
;; safe: URLs are shown as scheme://host only, keys are never shown.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url-parse)
(require 'efrit-config)
(require 'efrit-common)
(require 'efrit-models)
;; Modules whose variables the layers inspect.  Loaded eagerly: a
;; lazy (require ...) inside a layer that `let'-binds one of these
;; variables would define it as special while it is lexically bound.
(require 'efrit-api-stream)
(require 'efrit-context-sources)
(require 'efrit-do-prompt)
(require 'efrit-permissions)
(require 'efrit-tool-utils)

(declare-function efrit-api-request-async "efrit-api")
(declare-function efrit-api-build-headers "efrit-api")
(declare-function efrit-api-cacheable-system "efrit-api")
(declare-function efrit-tool--get-project-root "efrit-tool-utils")
(declare-function efrit-tool-executable-find "efrit-tool-utils")
(declare-function efrit-context-snapshot "efrit-context-sources")
(declare-function efrit-context-target-buffer "efrit-context-sources")
(declare-function efrit-permission-tool-class "efrit-permissions")
(declare-function efrit-permission-summarize "efrit-permissions")
(declare-function efrit-config--ensure-directories "efrit-config")

(defvar efrit-api-auth-scheme)
(defvar efrit-api-base-url)
(defvar efrit-api-url)
(defvar efrit-api-key)
(defvar efrit-api-custom-headers)
(defvar efrit-api-excluded-headers)
(defvar efrit-api-prompt-caching)
(defvar efrit-default-model)
(defvar efrit-agent-header-style)

(defgroup efrit-doctor nil
  "Configuration verifier."
  :group 'efrit
  :prefix "efrit-doctor-")

(defcustom efrit-doctor-live-checks nil
  "When non-nil, `efrit-doctor' always makes a real API request.
Otherwise the live model/caching checks run only with a prefix arg."
  :type 'boolean
  :group 'efrit-doctor)

(defcustom efrit-doctor-timeout 20
  "Seconds to wait for the live API check."
  :type 'integer
  :group 'efrit-doctor)

;;; Report plumbing

(defvar efrit-doctor--findings nil
  "List of (LEVEL TITLE DETAIL FIX-LABEL FIX-FN) in reverse order.")

(defun efrit-doctor--add (level title &optional detail fix-label fix-fn)
  "Record a finding.  LEVEL is `ok', `warn', `fail' or `info'."
  (push (list level title detail fix-label fix-fn) efrit-doctor--findings))

(defun efrit-doctor--ok (title &optional detail) (efrit-doctor--add 'ok title detail))
(defun efrit-doctor--info (title &optional detail) (efrit-doctor--add 'info title detail))
(defun efrit-doctor--warn (title detail &optional fix-label fix-fn)
  (efrit-doctor--add 'warn title detail fix-label fix-fn))
(defun efrit-doctor--fail (title detail &optional fix-label fix-fn)
  (efrit-doctor--add 'fail title detail fix-label fix-fn))

(defun efrit-doctor--redact-url (url)
  "Return URL reduced to scheme://host[:port] for safe display."
  (condition-case nil
      (let ((u (url-generic-parse-url url)))
        (format "%s://%s%s" (url-type u) (url-host u)
                (if-let* ((p (url-port-if-non-default u))) (format ":%d" p) "")))
    (error "<unparsable>")))

(defmacro efrit-doctor--layer (name &rest body)
  "Run BODY as layer NAME; an unexpected error becomes a FAIL, not a crash."
  (declare (indent 1))
  `(condition-case err
       (progn ,@body)
     (error (efrit-doctor--fail ,(format "%s: check itself failed" name)
                                (error-message-string err)))))

;;; 1. Installation

(defun efrit-doctor--check-install ()
  (efrit-doctor--layer "Installation"
    (let* ((lib (locate-library "efrit"))
           (dir (and lib (file-name-directory lib))))
      (if (not lib)
          (efrit-doctor--fail "efrit.el not on load-path"
                              "Add the lisp/ directory of your checkout to load-path.")
        (efrit-doctor--ok "efrit.el found" (abbreviate-file-name dir))
        ;; Subdirectories reachable?
        (dolist (sub '("core" "interfaces" "support" "tools"))
          (let ((d (expand-file-name sub dir)))
            (cond
             ((not (file-directory-p d))
              (efrit-doctor--fail (format "lisp/%s/ missing" sub)
                                  "This is not a complete checkout, or you installed with :files (\"lisp/*.el\") which drops the subdirectories.  Use :load-path to the checkout's lisp/ dir, or :files with lisp/*/*.el as well."))
             ((not (member (directory-file-name d)
                           (mapcar #'directory-file-name load-path)))
              (efrit-doctor--warn (format "lisp/%s/ not on load-path" sub)
                                  "efrit.el adds it when loaded; if you see void-function errors, (require 'efrit) earlier."
                                  "Add to load-path now"
                                  (lambda () (add-to-list 'load-path d)))))))
        ;; Shadowed copies: another efrit-*.el elsewhere on load-path?
        (let ((others (cl-remove-if
                       (lambda (p) (string-prefix-p (file-truename dir) (file-truename p)))
                       (delete-dups
                        (delq nil (mapcar (lambda (p)
                                            (let ((f (expand-file-name "efrit-common.el" p)))
                                              (and (file-exists-p f) p)))
                                          load-path))))))
          (when others
            (efrit-doctor--warn "Another efrit copy is on load-path"
                                (format "%s\nWhichever comes first wins; an old package.el/:vc install can shadow this checkout.  M-x package-delete efrit, or put :load-path first."
                                        (mapconcat #'abbreviate-file-name others ", ")))))
        ;; Stale .elc next to newer .el
        (let ((stale (cl-loop for f in (directory-files-recursively dir "\\.el\\'")
                              for c = (concat f "c")
                              when (and (file-exists-p c) (file-newer-than-file-p f c))
                              collect f)))
          (when stale
            (efrit-doctor--warn (format "%d stale .elc file(s)" (length stale))
                                "Byte-compiled files are older than their sources; Emacs will load the stale code."
                                "Delete stale .elc"
                                (lambda () (dolist (f stale) (delete-file (concat f "c"))))))))
      (if (version<= "28.1" emacs-version)
          (efrit-doctor--ok (format "Emacs %s" emacs-version))
        (efrit-doctor--fail (format "Emacs %s too old" emacs-version) "28.1 or later is required.")))))

;;; 2. Variables

(defconst efrit-doctor--obsolete-vars
  '((efrit-custom-headers . efrit-api-custom-headers)
    (efrit-excluded-headers . efrit-api-excluded-headers)
    (efrit-model . efrit-default-model)
    (efrit-api-url . efrit-api-base-url)
    (efrit-max-tokens . efrit-default-max-tokens))
  "Old variable names people still have in their init files.")

(defun efrit-doctor--check-variables ()
  (efrit-doctor--layer "Variables"
    (dolist (pair efrit-doctor--obsolete-vars)
      (let ((old (car pair)) (new (cdr pair)))
        (when (and (boundp old) (symbol-value old)
                   ;; an alias IS the new variable; nothing to migrate
                   (not (eq (indirect-variable old) new))
                   ;; efrit-api-url is still honoured; only nag if the new one is also set
                   (not (and (eq old 'efrit-api-url)
                             (equal efrit-api-base-url "https://api.anthropic.com"))))
          (efrit-doctor--warn (format "%s is set but obsolete" old)
                              (format "Use %s instead.%s" new
                                      (if (eq old 'efrit-api-url)
                                          "  efrit-api-url is a full URL; efrit-api-base-url takes the base only (no /v1/messages)."
                                        "  The old name is ignored."))
                              (unless (eq old 'efrit-api-url)
                                (format "Copy value to %s" new))
                              (unless (eq old 'efrit-api-url)
                                (lambda () (set new (symbol-value old))))))))
    (when (and (boundp 'efrit-api-excluded-headers) efrit-api-excluded-headers)
      (efrit-doctor--info "efrit-api-excluded-headers is set"
                          "Usually unnecessary now: with efrit-api-auth-scheme 'bearer no x-api-key header is sent at all."))
    (unless (memq efrit-api-auth-scheme '(x-api-key bearer))
      (efrit-doctor--fail (format "efrit-api-auth-scheme has bad value %S" efrit-api-auth-scheme)
                          "Must be 'x-api-key (Anthropic) or 'bearer (proxies)."
                          "Set to x-api-key" (lambda () (setq efrit-api-auth-scheme 'x-api-key))))
    (if (and (stringp efrit-default-model) (not (string-empty-p efrit-default-model)))
        (efrit-doctor--ok (format "Model: %s" efrit-default-model))
      (efrit-doctor--fail "efrit-default-model is not a string" ""))))

;;; 3. Credentials

(defun efrit-doctor--check-credentials ()
  (efrit-doctor--layer "Credentials"
    (let ((source (cond ((stringp efrit-api-key) "literal string in efrit-api-key (not recommended)")
                        ((functionp efrit-api-key)
                         (format "function %s in efrit-api-key"
                                 (if (symbolp efrit-api-key) efrit-api-key "(lambda)")))
                        ((and efrit-api-key (symbolp efrit-api-key))
                         (format "environment variable %s" efrit-api-key))
                        ((getenv "ANTHROPIC_API_KEY") "ANTHROPIC_API_KEY")
                        (t (format "auth-source (machine %s login %s)"
                                   efrit-api-auth-source-host efrit-api-auth-source-user)))))
      (condition-case err
          (let ((key (efrit-common-get-api-key)))
            (efrit-doctor--ok (format "API key resolves (%d chars) via %s" (length key) source))
            (when (and (eq efrit-api-auth-scheme 'bearer) (string-prefix-p "sk-ant-" key))
              (efrit-doctor--warn "Bearer scheme with an Anthropic key"
                                  "sk-ant- keys are for api.anthropic.com, which expects x-api-key.  If you are not going through a proxy, set efrit-api-auth-scheme to 'x-api-key."
                                  "Switch to x-api-key" (lambda () (setq efrit-api-auth-scheme 'x-api-key)))))
        (error
         (let ((msg (error-message-string err)))
           (if (string-match-p "Invalid API key format" msg)
               (efrit-doctor--fail "Key does not look like an Anthropic key"
                                   (format "Source: %s.  If this is a proxy/gateway token, set efrit-api-auth-scheme to 'bearer; the sk- format check then no longer applies." source)
                                   "Switch to bearer" (lambda () (setq efrit-api-auth-scheme 'bearer)))
             (efrit-doctor--fail "No API key found"
                                 (format "%s\nTried: %s.  Options: put it in ~/.authinfo.gpg as\n  machine %s login %s password KEY\nor set ANTHROPIC_API_KEY, or set efrit-api-key to a function returning it."
                                         msg source efrit-api-auth-source-host efrit-api-auth-source-user)))))))))

;;; 4. Endpoint

(defun efrit-doctor--check-endpoint ()
  (efrit-doctor--layer "Endpoint"
    (let* ((url (efrit-common-get-api-url))
           (u (url-generic-parse-url url))
           (host (url-host u))
           (port (or (url-port-if-non-default u)
                     (url-scheme-get-property (url-type u) 'default-port))))
      (efrit-doctor--info (format "Messages endpoint: %s/…/v1/messages" (efrit-doctor--redact-url url)))
      (cond
       ((not (member (url-type u) '("https" "http")))
        (efrit-doctor--fail "Endpoint is not http(s)" url))
       ((string-match-p "/v1/messages/v1/messages\\'" url)
        (efrit-doctor--fail "Double /v1/messages in endpoint"
                            "efrit-api-base-url must be the base only; efrit appends /v1/messages."
                            "Strip suffix from efrit-api-base-url"
                            (lambda () (setq efrit-api-base-url
                                             (string-remove-suffix "/v1/messages" efrit-api-base-url)))))
       ((equal (url-type u) "http")
        (efrit-doctor--warn "Endpoint is plain http" "Your key travels unencrypted.")))
      (when (and (eq efrit-api-auth-scheme 'x-api-key)
                 (not (equal host "api.anthropic.com")))
        (efrit-doctor--warn "Non-Anthropic host with x-api-key scheme"
                            (format "%s is not api.anthropic.com.  Most gateways want Authorization: Bearer; set efrit-api-auth-scheme to 'bearer." host)
                            "Switch to bearer" (lambda () (setq efrit-api-auth-scheme 'bearer))))
      (when (and (eq efrit-api-auth-scheme 'bearer) (equal host "api.anthropic.com"))
        (efrit-doctor--fail "api.anthropic.com with bearer scheme"
                            "Anthropic's API requires x-api-key."
                            "Switch to x-api-key" (lambda () (setq efrit-api-auth-scheme 'x-api-key))))
      ;; Reachability: TCP+TLS handshake only, no request
      (when host
        (condition-case err
            (let* ((tls (and (equal (url-type u) "https")
                             (fboundp 'gnutls-available-p) (gnutls-available-p)
                             (cons 'gnutls-x509pki
                                   (gnutls-boot-parameters :type 'gnutls-x509pki
                                                           :hostname host))))
                   (proc (make-network-process
                          :name "efrit-doctor-probe" :host host :service port
                          :nowait nil :tls-parameters tls)))
              (delete-process proc)
              (efrit-doctor--ok (format "%s to %s:%d succeeds" (if tls "TCP/TLS" "TCP")
                                        host port))
              (when (and (equal (url-type u) "https") (not tls))
                (efrit-doctor--warn "This Emacs has no GnuTLS"
                                    "https requests will fall back to an external tls program or fail; check (gnutls-available-p).")))
          (error
           (efrit-doctor--fail (format "Cannot connect to %s:%d" host port)
                               (format "%s\nCheck VPN/proxy (url-proxy-services, HTTPS_PROXY) and that the host name is right."
                                       (error-message-string err)))))))))

;;; 5+6. Live request

(defun efrit-doctor--live-request (with-cache callback)
  "POST a one-token request; CALLBACK gets (STATUS . DETAIL).
STATUS is `ok', `http', or `net'.  With WITH-CACHE the system prompt
carries a cache_control block, which is the caching probe."
  (require 'efrit-api)
  (let* ((efrit-api-prompt-caching with-cache)
         (system "You are a connectivity probe. Reply with the single word: pong")
         (req `(("model" . ,efrit-default-model)
                ("max_tokens" . 5)
                ("system" . ,(efrit-api-cacheable-system system))
                ("messages" . [(("role" . "user") ("content" . "ping"))])))
         (done nil))
    (efrit-api-request-async
     req
     (lambda (resp)
       (setq done t)
       (funcall callback (cons 'ok (or (ignore-errors
                                         (gethash "text" (aref (gethash "content" resp) 0)))
                                       "(no text)"))))
     (lambda (msg)
       (setq done t)
       (funcall callback (cons (if (string-match-p "HTTP\\|API Error" msg) 'http 'net) msg))))
    (with-timeout (efrit-doctor-timeout
                   (unless done (funcall callback (cons 'net "timed out"))))
      (while (not done) (accept-process-output nil 0.1)))))

(defun efrit-doctor--check-live ()
  (efrit-doctor--layer "Live request"
    (let (result)
      (efrit-doctor--live-request nil (lambda (r) (setq result r)))
      (pcase (car result)
        ('ok (efrit-doctor--ok (format "Model %s answered: %S" efrit-default-model
                                       (string-trim (cdr result)))))
        ('http
         (let ((m (cdr result)))
           (cond
            ((string-match-p "401\\|authentication\\|invalid x-api-key\\|Unauthorized" m)
             (efrit-doctor--fail "Endpoint rejected the credentials (401)"
                                 (format "%s\nKey resolves locally but the server refuses it: expired/rotated key, wrong auth scheme (%s), or wrong environment." m efrit-api-auth-scheme)))
            ((efrit-models-model-error-p m)
             (efrit-doctor--fail (format "Model %s not available at this endpoint" efrit-default-model)
                                 (format "%s\nGateways expose their own model ids and per-key entitlements.  Pick one that answers here." m)
                                 "Select a working model"
                                 (lambda () (efrit-select-model t))))
            ((string-match-p "404" m)
             (efrit-doctor--fail "404 from endpoint"
                                 (format "%s\nThe base URL is probably wrong (does the gateway need a path prefix?)." m)))
            ((string-match-p "403" m)
             (efrit-doctor--fail "403 Forbidden" (format "%s\nKey valid but not entitled to this model/route." m)))
            ;; Unclassified but names a model: still offer the picker,
            ;; since gateways phrase entitlement errors many ways
            ((string-match-p "model" m)
             (efrit-doctor--fail "API request failed (mentions the model)" m
                                 "Select a working model"
                                 (lambda () (efrit-select-model t))))
            (t (efrit-doctor--fail "API request failed" m)))))
        (_ (efrit-doctor--fail "No response" (format "%s\nNetwork-level failure after the TLS probe succeeded: proxy in the middle, or the endpoint hung." (cdr result))))))
    ;; Caching probe only if the plain request worked and caching is on
    (when (and efrit-api-prompt-caching
               (eq (car (car efrit-doctor--findings)) 'ok))
      (let (result)
        (efrit-doctor--live-request t (lambda (r) (setq result r)))
        (if (eq (car result) 'ok)
            (efrit-doctor--ok "Endpoint accepts cache_control (prompt caching works)")
          (efrit-doctor--warn "Endpoint rejects cache_control"
                              (format "%s\nefrit-api-prompt-caching is on; every real request will fail like this." (cdr result))
                              "Disable prompt caching" (lambda () (setq efrit-api-prompt-caching nil))))))))

;;; 7. Sandbox and data

(defun efrit-doctor--check-sandbox ()
  (efrit-doctor--layer "Sandbox"
    (let* ((root (efrit-tool--get-project-root))
           (remote (file-remote-p root)))
      (efrit-doctor--info (format "Project root: %s%s" (abbreviate-file-name root)
                                  (cond (efrit-project-root " (efrit-project-root)")
                                        ((project-current) " (project.el)")
                                        (t " (default-directory fallback)"))))
      (unless (file-directory-p root)
        (efrit-doctor--fail "Project root does not exist" root))
      (when (and (not efrit-project-root) (not (project-current)))
        (efrit-doctor--info "No project detected here"
                            "Tools will be sandboxed to default-directory.  Run efrit from inside a project, or set efrit-project-root."))
      (if efrit-project-sandbox
          (efrit-doctor--ok "Path sandbox enabled")
        (efrit-doctor--warn "efrit-project-sandbox is nil"
                            "File tools may touch anything on disk." "Enable sandbox"
                            (lambda () (setq efrit-project-sandbox t))))
      (when remote (efrit-doctor--info (format "Root is remote via Tramp: %s" remote))))
    (let ((d (expand-file-name efrit-data-directory)))
      (cond ((not (file-directory-p d))
             (efrit-doctor--warn "Data directory missing" d "Create it"
                                 (lambda () (efrit-config--ensure-directories))))
            ((not (file-writable-p d))
             (efrit-doctor--fail "Data directory not writable" d))
            (t (efrit-doctor--ok (format "Data directory: %s" (abbreviate-file-name d))))))))

;;; 8. Tramp

(defun efrit-doctor--check-tramp ()
  (efrit-doctor--layer "Tramp"
    (let* ((root (efrit-tool--get-project-root))
           (remote (file-remote-p root)))
      (if (not remote)
          (efrit-doctor--info "Project root is local; Tramp checks skipped"
                              "Open a /ssh: file and rerun to verify remote tooling.")
        (dolist (prog '("sh" "git" "rg"))
          (if (efrit-tool-executable-find prog root)
              (efrit-doctor--ok (format "%s found on %s" prog remote))
            (if (equal prog "rg")
                (efrit-doctor--warn (format "rg not on remote PATH (%s)" remote)
                                    "search_content will use the slow elisp fallback over Tramp.  Install ripgrep remotely, or extend tramp-remote-path.")
              (efrit-doctor--fail (format "%s not on remote PATH (%s)" prog remote)
                                  "Remote tools cannot run.  Check tramp-remote-path (try adding 'tramp-own-remote-path)."))))
        (let ((default-directory root))
          (condition-case err
              (with-temp-buffer
                (if (eq 0 (process-file "sh" nil t nil "-c" "echo efrit-ok"))
                    (efrit-doctor--ok "process-file runs on the remote host")
                  (efrit-doctor--fail "process-file returned non-zero" (buffer-string))))
            (error (efrit-doctor--fail "process-file failed" (error-message-string err)))))))))

;;; 9. Permissions

(defun efrit-doctor--check-permissions ()
  (efrit-doctor--layer "Permissions"
    (cond
     ((null efrit-permission-policy)
      (efrit-doctor--warn "Permission policy is nil"
                          "Every tool, including eval_sexp and shell_exec, runs without asking."
                          "Use default '(write exec)"
                          (lambda () (setq efrit-permission-policy '(write exec)))))
     ((cl-every (lambda (c) (memq c '(write exec))) efrit-permission-policy)
      (efrit-doctor--ok (format "Permission policy: %S" efrit-permission-policy)))
     (t (efrit-doctor--fail (format "Bad efrit-permission-policy %S" efrit-permission-policy)
                            "Only 'write and 'exec are meaningful.")))
    (when efrit-permission-responder-function
      (if (functionp efrit-permission-responder-function)
          (condition-case err
              (let ((r (funcall efrit-permission-responder-function
                                `((:tool . "eval_sexp") (:class . exec)
                                  (:input . ,(make-hash-table :test 'equal))
                                  (:summary . "(doctor probe)")
                                  (:project-root . ,default-directory)
                                  (:session-id . "doctor")))))
                (if (memq r '(nil allow deny allow-tool allow-all))
                    (efrit-doctor--ok (format "Responder callable; probe returned %S" r))
                  (efrit-doctor--fail (format "Responder returned unexpected %S" r)
                                      "Must return allow/deny/allow-tool/allow-all or nil.")))
            (error (efrit-doctor--fail "Responder signalled on a probe" (error-message-string err))))
        (efrit-doctor--fail "efrit-permission-responder-function is not a function" "")))))

;;; 10. Context and prompt hooks

(defun efrit-doctor--check-context ()
  (efrit-doctor--layer "Context"
    (let ((bad (cl-remove-if (lambda (s) (or (functionp s)
                                             (memq s '(buffer position region diagnostic project
                                                       visible-buffers recent-files))))
                             efrit-context-sources)))
      (when bad (efrit-doctor--warn (format "Unknown context sources: %S" bad)
                                    "They are silently skipped.")))
    (if (null efrit-context-sources)
        (efrit-doctor--info "Proactive editor context disabled" "efrit-context-sources is nil.")
      (let ((snap (efrit-context-snapshot (efrit-context-target-buffer))))
        (if snap
            (efrit-doctor--ok (format "Editor context renders (%d chars)" (length snap)))
          (efrit-doctor--warn "Editor context produced nothing" "All sources returned nil for the current buffer."))))
    (dolist (fn efrit-system-prompt-functions)
      (condition-case err
          (let ((r (funcall fn "doctor")))
            (unless (or (null r) (stringp r))
              (efrit-doctor--warn (format "Prompt hook %S returned %s" fn (type-of r)) "Must return a string or nil.")))
        (error (efrit-doctor--warn (format "Prompt hook %S signalled" fn) (error-message-string err)))))
    (when efrit-system-prompt-functions
      (efrit-doctor--ok (format "%d system-prompt hook(s) callable" (length efrit-system-prompt-functions))))))

;;; 11. UI

(defun efrit-doctor--check-transport ()
  (efrit-doctor--layer "Transport"
    (if (not (bound-and-true-p efrit-api-streaming))
        (efrit-doctor--info "Streaming transport off; using url-retrieve"
                            "Responses render only when complete and cannot be cancelled mid-flight.  Set efrit-api-streaming to t (needs curl).")
      (let ((curl (executable-find (symbol-value 'efrit-api-stream-curl-program))))
        (if (not curl)
            (efrit-doctor--fail (format "efrit-api-streaming is on but %s is not found"
                                        (symbol-value 'efrit-api-stream-curl-program))
                                "Install curl or set efrit-api-streaming to nil."
                                "Disable streaming" (lambda () (setq efrit-api-streaming nil)))
          (let ((ver (with-temp-buffer
                       (call-process curl nil t nil "--version")
                       (buffer-substring (point-min) (line-end-position)))))
            (efrit-doctor--ok (format "Streaming via %s" ver))
            ;; --fail-with-body needs curl 7.76+
            (when (and (string-match "curl \\([0-9]+\\)\\.\\([0-9]+\\)" ver)
                       (version< (format "%s.%s" (match-string 1 ver) (match-string 2 ver)) "7.76"))
              (efrit-doctor--fail "curl too old for --fail-with-body (needs 7.76+)"
                                  "Upgrade curl or disable streaming."
                                  "Disable streaming" (lambda () (setq efrit-api-streaming nil))))))))))

(defun efrit-doctor--check-ui ()
  (efrit-doctor--layer "UI"
    (if (fboundp 'efrit-agent-mode)
        (efrit-doctor--ok "efrit-agent-mode available (M-x efrit)")
      (efrit-doctor--warn "efrit-agent not loaded" "(require 'efrit) or M-x efrit will autoload it."))
    (when (boundp 'efrit-agent-header-style)
      (pcase efrit-agent-header-style
        ('graphical
         (cond ((not (display-graphic-p))
                (efrit-doctor--info "Graphical header requested on a text terminal; text header will be used"))
               ((not (image-type-available-p 'svg))
                (efrit-doctor--warn "Graphical header requested but this Emacs has no SVG support"
                                    "Falls back to the text header.  Rebuild with librsvg, or set efrit-agent-header-style to 'text."
                                    "Use text header" (lambda () (setq efrit-agent-header-style 'text))))
               (t (efrit-doctor--ok "SVG header supported"))))
        (s (efrit-doctor--ok (format "Header style: %s" s)))))
    (when (and (fboundp 'efrit-setup-keybindings) (boundp 'efrit-enable-global-keymap)
               (not (symbol-value 'efrit-enable-global-keymap)))
      (efrit-doctor--info "Global keymap disabled" "Set efrit-enable-global-keymap or bind M-x efrit yourself."))))

;;; Report

(defun efrit-doctor--insert-report (live)
  (let* ((findings (reverse efrit-doctor--findings))
         (fails (cl-count 'fail findings :key #'car))
         (warns (cl-count 'warn findings :key #'car)))
    (insert (propertize "Efrit doctor\n" 'face 'bold))
    (insert (format "%s  •  Emacs %s  •  %s\n"
                    (format-time-string "%Y-%m-%d %H:%M") emacs-version
                    (if live "with live API check" "static checks only (C-u for live check)")))
    ;; Which doctor is this?  Stale .elc and half-reloaded sessions are
    ;; the most common reason a fix "isn't there".
    (let* ((src (or (locate-library "efrit-doctor") "?"))
           (el (and (string-suffix-p ".elc" src) (substring src 0 -1))))
      (insert (propertize
               (format "doctor source: %s (%s)%s\n\n"
                       (abbreviate-file-name src)
                       (format-time-string "%Y-%m-%d %H:%M"
                                           (file-attribute-modification-time (file-attributes src)))
                       (if (and el (file-exists-p el) (file-newer-than-file-p el src))
                           "  STALE .elc -- the .el is newer; M-x load-file it or make compile"
                         ""))
               'face 'shadow)))
    (dolist (f findings)
      (pcase-let ((`(,level ,title ,detail ,fix-label ,fix-fn) f))
        (insert (propertize (pcase level ('ok "  ✓ ") ('warn "  ⚠ ") ('fail "  ✗ ") (_ "  · "))
                            'face (pcase level ('ok 'success) ('warn 'warning) ('fail 'error) (_ 'shadow))))
        (insert title)
        (when (and fix-label fix-fn)
          (insert "  ")
          (insert-text-button (format "[%s]" fix-label)
                              'action (lambda (_)
                                        (funcall fix-fn)
                                        (message "Applied: %s — rerunning doctor" fix-label)
                                        (efrit-doctor live))
                              'follow-link t
                              'help-echo "Apply this fix and rerun"))
        (insert "\n")
        (when (and detail (not (string-empty-p detail)))
          (dolist (line (split-string detail "\n"))
            (insert (propertize (concat "      " line "\n") 'face 'shadow))))))
    (insert "\n")
    (insert (cond ((> fails 0) (propertize (format "%d problem(s), %d warning(s).  Fix the ✗ items first.\n" fails warns) 'face 'error))
                  ((> warns 0) (propertize (format "No blockers; %d warning(s) worth a look.\n" warns) 'face 'warning))
                  (t (propertize "Everything checks out.  M-x efrit to start.\n" 'face 'success))))
    (insert (propertize "\nFix buttons only change the running Emacs; put the corresponding setq in your init file to keep them.\n" 'face 'shadow))))

;;;###autoload
(defun efrit-doctor (&optional live)
  "Verify every layer of the efrit configuration and suggest fixes.
With a prefix argument LIVE (or when `efrit-doctor-live-checks' is
non-nil), also make a real one-token API request to confirm the key,
endpoint, model and prompt-caching setting work end to end."
  (interactive "P")
  (setq live (or live efrit-doctor-live-checks))
  (setq efrit-doctor--findings nil)
  (efrit-doctor--check-install)
  (efrit-doctor--check-variables)
  (efrit-doctor--check-credentials)
  (efrit-doctor--check-endpoint)
  (when live
    ;; Only worth the tokens if nothing above already failed
    (if (cl-some (lambda (f) (eq (car f) 'fail)) efrit-doctor--findings)
        (efrit-doctor--info "Live request skipped: fix the failures above first")
      (efrit-doctor--check-live)))
  (efrit-doctor--check-transport)
  (efrit-doctor--check-sandbox)
  (efrit-doctor--check-tramp)
  (efrit-doctor--check-permissions)
  (efrit-doctor--check-context)
  (efrit-doctor--check-ui)
  (with-current-buffer (get-buffer-create "*efrit-doctor*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (efrit-doctor--insert-report live)
      (goto-char (point-min))
      (special-mode))
    (display-buffer (current-buffer)))
  (let ((fails (cl-count 'fail efrit-doctor--findings :key #'car)))
    (message "efrit-doctor: %s" (if (zerop fails) "OK" (format "%d problem(s)" fails)))
    (zerop fails)))

(provide 'efrit-doctor)

;;; efrit-doctor.el ends here
