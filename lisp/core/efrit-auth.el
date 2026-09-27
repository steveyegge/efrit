;;; efrit-auth.el --- Credentials and authenticated HTTP for efrit's sources -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; Document sources (Google Drive, Confluence, ...) need to talk to a
;; web API as the user.  This file gives them one way to do it that
;; starts and ends in auth-source:
;;
;;   (efrit-auth-request "drive" "GET" "https://www.googleapis.com/drive/v3/files"
;;                       :params '(("q" . "name contains 'recap'")))
;;
;; "drive" is an auth-source host.  The entry for it decides how to
;; authenticate:
;;
;;   - OAuth2 (Google, Atlassian cloud): the entry carries
;;     :client-id :client-secret :auth-url :token-url, plus :scope and
;;     optionally :redirect-uri :state :use-pkce.  The token comes from
;;     oauth2.el (`oauth2-auth-and-store', so the consent flow, the
;;     plstore and the refresh are the same as for Gnus's xoauth2
;;     plugin and nngmail; with oauth2-loopback loaded the code is
;;     caught on localhost).  The id of the stored token is computed
;;     from auth-url, token-url, scope, client-id and user, so an entry
;;     with a wider scope gets its own consent once and is then cached.
;;   - Bearer token / API token (Confluence with a personal access
;;     token, most self-hosted services): the entry has only :user and
;;     :secret (the password).  It is sent as "Authorization: Bearer".
;;     An entry with :auth-type "basic" sends HTTP Basic instead
;;     (Atlassian cloud API tokens want user:token in Basic).
;;
;; Lookups bypass auth-source's cache and the xoauth2 plugin's advice
;; (both bit nngmail: a negative cache and the plugin running its own
;; token dance while we only wanted the client id).
;;
;; `efrit-auth-request' returns the parsed JSON (or the raw body with
;; :raw t) and signals `efrit-auth-http-error' with (STATUS MESSAGE URL)
;; on 4xx/5xx after one token refresh on 401 and short retries on
;; transport errors and 5xx.  There is no quota policy here: sources
;; that need pacing (Gmail does) keep their own.
;;
;; `M-x efrit-auth-reauthorize HOST' drops the token held in memory and
;; runs the consent flow again -- for after the entry's scope changed.
;; `M-x efrit-auth-check HOST' says what the entry looks like and
;; whether a token can be obtained, without a request.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'auth-source)
(require 'url)
(require 'url-http)
(require 'json)
(require 'efrit-log)

(declare-function oauth2-auth-and-store "oauth2")
(declare-function oauth2-refresh-access "oauth2")
(declare-function oauth2-token-access-token "oauth2")
(declare-function oauth2-token-request-cache "oauth2")

(defgroup efrit-auth nil
  "Credentials and authenticated HTTP for efrit's document sources."
  :group 'efrit
  :prefix "efrit-auth-")

(defcustom efrit-auth-request-timeout 60
  "Seconds one HTTP request may take before it counts as a transport error."
  :type 'integer)

(defcustom efrit-auth-max-attempts 4
  "Attempts per request: the first plus retries on transport errors, 5xx and one 401."
  :type 'integer)

(define-error 'efrit-auth-error "efrit-auth error")
(define-error 'efrit-auth-http-error "efrit-auth HTTP error" 'efrit-auth-error)
(define-error 'efrit-auth-no-credentials "efrit-auth: no credentials" 'efrit-auth-error)

(defconst efrit-auth--oauth2-keys '(:client-id :client-secret :auth-url :token-url)
  "An auth-source entry with all of these is an OAuth2 client.")

(defvar efrit-auth--credentials (make-hash-table :test #'equal)
  "(HOST . USER) -> the auth-source entry found, for this session.")

(defvar efrit-auth--tokens (make-hash-table :test #'equal)
  "(HOST . USER) -> the `oauth2-token' in use, for this session.")

;;;; auth-source

(defun efrit-auth--search (host user)
  "The auth-source entries for HOST (and USER when given), uncached.
The xoauth2 plugin's advice on `auth-source-search-backends' is
disabled for the duration: it would run a token flow, or drop entries
whose user differs from `smtpmail-smtp-user'."
  (let ((auth-source-do-cache nil)
        (spec (append (list :host host :max 20) (and user (list :user user)))))
    (if (and (fboundp 'auth-source-xoauth2-plugin--search-backends)
             (advice-member-p 'auth-source-xoauth2-plugin--search-backends
                              'auth-source-search-backends))
        (unwind-protect
            (progn
              (advice-remove 'auth-source-search-backends
                             'auth-source-xoauth2-plugin--search-backends)
              (apply #'auth-source-search spec))
          (advice-add 'auth-source-search-backends :around
                      'auth-source-xoauth2-plugin--search-backends))
      (apply #'auth-source-search spec))))

(defun efrit-auth-get (entry key)
  "KEY of auth-source ENTRY, with secret functions resolved."
  (let ((v (plist-get entry key)))
    (if (functionp v) (funcall v) v)))

(defun efrit-auth-oauth2-p (entry)
  "Non-nil when auth-source ENTRY describes an OAuth2 client."
  (cl-every (lambda (k) (plist-get entry k)) efrit-auth--oauth2-keys))

(defun efrit-auth-credentials (host &optional user)
  "The auth-source entry for HOST (and USER), or signal `efrit-auth-no-credentials'.
An OAuth2 entry is preferred over a plain one; the first match
otherwise.  Found entries are kept for the session; `efrit-auth-forget'
drops them."
  (let ((key (cons host user)))
    (or (gethash key efrit-auth--credentials)
        (let* ((entries (efrit-auth--search host user))
               (entry (or (seq-find #'efrit-auth-oauth2-p entries)
                          (seq-find (lambda (e) (plist-get e :secret)) entries))))
          (unless entry
            (signal 'efrit-auth-no-credentials
                    (list (format "no auth-source entry for host %S%s with OAuth2 fields %s or a secret"
                                  host (if user (format " user %S" user) "")
                                  efrit-auth--oauth2-keys))))
          (puthash key entry efrit-auth--credentials)))))

(defvar efrit-auth-host-functions nil
  "Functions returning extra auth-source hosts worth trying for Google entries.
Each is called with no arguments and returns a list of host strings.
A mail backend that already holds a Google OAuth entry under its own
server name adds one, so sources need not guess the name.")

(defun efrit-auth-candidate-hosts (&rest hosts)
  "HOSTS (strings or lists of strings) plus those from `efrit-auth-host-functions', deduplicated."
  (delete-dups
   (delq nil (append (flatten-tree hosts)
                     (flatten-tree (mapcar (lambda (fn) (ignore-errors (funcall fn)))
                                           efrit-auth-host-functions))))))

;;;###autoload
(defun efrit-auth-describe (host &optional user)
  "Show what auth-source holds for HOST (and USER): every entry, secrets hidden.
For finding out why a source cannot see the entry you mean: the host
name, the user, whether the OAuth2 fields are there, the scope."
  (interactive (list (read-string "auth-source host: ") nil))
  (let ((entries (efrit-auth--search host user)))
    (if (null entries)
        (message "efrit-auth: no auth-source entry has host %S%s (auth-sources: %S)"
                 host (if user (format " user %S" user) "") auth-sources)
      (with-output-to-temp-buffer "*efrit-auth*"
        (princ (format "auth-source entries for host %S%s:\n\n" host (if user (format " user %S" user) "")))
        (dolist (e entries)
          (princ (format "- user %S  oauth2 %s  scope %S\n  keys: %s\n"
                         (plist-get e :user)
                         (if (efrit-auth-oauth2-p e) "yes" "no")
                         (efrit-auth-get e :scope)
                         (mapconcat (lambda (k) (substring (symbol-name k) 1))
                                    (seq-filter #'keywordp e) " "))))))))

(defun efrit-auth-forget (host &optional user)
  "Drop the credentials and token held in memory for HOST (and USER)."
  (let ((key (cons host user)))
    (remhash key efrit-auth--credentials)
    (remhash key efrit-auth--tokens)))

;;;; OAuth2

(defun efrit-auth--true-p (value)
  "Whether an auth-source string VALUE means yes."
  (and value (not (member (downcase (format "%s" value)) '("false" "nil" "no" "0" "")))))

(defun efrit-auth--oauth2-token (host user entry)
  "The `oauth2-token' for ENTRY of HOST/USER, obtaining or refreshing it."
  (require 'oauth2)
  (let* ((key (cons host user))
         (user (or user (efrit-auth-get entry :user)))
         (scope (efrit-auth-get entry :scope))
         (token (or (gethash key efrit-auth--tokens)
                    (puthash key
                             (oauth2-auth-and-store
                              (efrit-auth-get entry :auth-url)
                              (efrit-auth-get entry :token-url)
                              scope
                              (efrit-auth-get entry :client-id)
                              (efrit-auth-get entry :client-secret)
                              (efrit-auth-get entry :redirect-uri)
                              (efrit-auth-get entry :state)
                              user
                              host
                              (efrit-auth--true-p (efrit-auth-get entry :use-pkce)))
                             efrit-auth--tokens)))
         (refreshed (oauth2-refresh-access token host)))
    (puthash key refreshed efrit-auth--tokens)
    refreshed))

(defun efrit-auth--force-refresh (host user)
  "Make the next token lookup for HOST/USER refresh the access token."
  (when-let* ((token (gethash (cons host user) efrit-auth--tokens)))
    ;; An empty request cache makes `oauth2-refresh-access' refresh.
    (eval `(setf (oauth2-token-request-cache ,token) nil) t)))

(defun efrit-auth-authorization (host &optional user)
  "The value of the Authorization header for HOST (and USER).
OAuth2 entries give \"Bearer ACCESS-TOKEN\" (consent flow on first use);
plain entries give \"Bearer SECRET\", or HTTP Basic when the entry has
:auth-type basic."
  (let ((entry (efrit-auth-credentials host user)))
    (cond
     ((efrit-auth-oauth2-p entry)
      (concat "Bearer " (oauth2-token-access-token (efrit-auth--oauth2-token host user entry))))
     ((equal (format "%s" (or (efrit-auth-get entry :auth-type) "")) "basic")
      (concat "Basic " (base64-encode-string
                        (encode-coding-string
                         (format "%s:%s" (efrit-auth-get entry :user) (efrit-auth-get entry :secret))
                         'utf-8)
                        t)))
     (t (concat "Bearer " (efrit-auth-get entry :secret))))))

;;;###autoload
(defun efrit-auth-reauthorize (host &optional user)
  "Forget HOST's token and obtain one afresh (consent in the browser if needed).
For after the auth-source entry changed: a new scope, a new client."
  (interactive (list (read-string "auth-source host: ")))
  (efrit-auth-forget host user)
  (let ((entry (efrit-auth-credentials host user)))
    (if (efrit-auth-oauth2-p entry)
        (progn
          (efrit-auth--oauth2-token host user entry)
          (message "efrit-auth: %s authorized (scope %s)" host (or (efrit-auth-get entry :scope) "none")))
      (message "efrit-auth: %s uses a stored secret; nothing to authorize" host))))

;;;###autoload
(defun efrit-auth-check (host &optional user)
  "Describe what auth-source has for HOST and whether a token can be obtained."
  (interactive (list (read-string "auth-source host: ")))
  (condition-case err
      (let ((entry (efrit-auth-credentials host user)))
        (if (efrit-auth-oauth2-p entry)
            (let ((token (efrit-auth--oauth2-token host user entry)))
              (message "efrit-auth: %s is OAuth2 (user %s, scope %s); access token %s"
                       host (efrit-auth-get entry :user) (or (efrit-auth-get entry :scope) "none")
                       (if (oauth2-token-access-token token) "present" "missing")))
          (message "efrit-auth: %s has a stored secret for user %s (%s)"
                   host (efrit-auth-get entry :user)
                   (if (equal (format "%s" (efrit-auth-get entry :auth-type)) "basic") "HTTP Basic" "Bearer"))))
    (efrit-auth-error (message "efrit-auth: %s" (error-message-string err)))))

;;;; HTTP

(defun efrit-auth--url (url params)
  "URL with PARAMS ((KEY . VALUE) ...) appended as a query string."
  (if (null params)
      url
    (concat url (if (string-match-p "\\?" url) "&" "?")
            (mapconcat (lambda (p)
                         (concat (url-hexify-string (format "%s" (car p))) "="
                                 (url-hexify-string (format "%s" (cdr p)))))
                       params "&"))))

(defun efrit-auth--http (method url headers body)
  "Perform METHOD on URL with HEADERS and BODY through url.el.
Returns (STATUS HEADERS-ALIST BODY-BYTES); STATUS 0 with a message in
the body means no response at all."
  (let ((url-request-method method)
        (url-request-extra-headers headers)
        (url-request-data (and body (encode-coding-string body 'utf-8)))
        (url-show-status nil)
        (url-mime-charset-string nil)
        (url-http-attempt-keepalives nil))
    (let ((buf (condition-case err
                   (url-retrieve-synchronously url t t efrit-auth-request-timeout)
                 (error (list 0 nil (error-message-string err))))))
      (cond
       ((listp buf) buf)
       ((null buf) (list 0 nil "no response"))
       (t
        (with-current-buffer buf
          (unwind-protect
              (let (status hdrs)
                (goto-char (point-min))
                (when (re-search-forward "\\`HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
                  (setq status (string-to-number (match-string 1))))
                (forward-line 1)
                (while (looking-at "\\([^:\r\n]+\\): *\\(.*?\\)\r?$")
                  (push (cons (downcase (match-string 1)) (match-string 2)) hdrs)
                  (forward-line 1))
                (re-search-forward "^\r?$" nil t)
                (forward-line 1)
                (list (or status 0) hdrs
                      (buffer-substring-no-properties (point) (point-max))))
            (kill-buffer buf))))))))

(defun efrit-auth--error-message (text)
  "A one-line reason from an API error body TEXT (Google and Atlassian shapes), else TEXT."
  (or (ignore-errors
        (let* ((obj (json-parse-string text :object-type 'alist :array-type 'list))
               (err (alist-get 'error obj)))
          (cond
           ((and (listp err) (alist-get 'message err)) (alist-get 'message err))
           ((stringp err) err)
           ((alist-get 'message obj) (alist-get 'message obj))
           ((alist-get 'errorMessages obj) (string-join (alist-get 'errorMessages obj) "; ")))))
      (truncate-string-to-width (string-trim (or text "")) 200 nil nil "…")))

(cl-defun efrit-auth-request (host method url &key user params body content-type raw accept)
  "Do METHOD on URL as the user of auth-source HOST; return the parsed JSON.
PARAMS are query parameters ((KEY . VALUE) ...).  BODY is a string sent
as is, or a Lisp object encoded as JSON; CONTENT-TYPE defaults to
application/json.  With RAW non-nil the body comes back as a unibyte
string (an export, a binary).  ACCEPT is the Accept header; default
application/json, or */* with RAW.

A 401 refreshes the token and retries once; transport errors and 5xx
retry with a short backoff up to `efrit-auth-max-attempts'.  Other
failures signal `efrit-auth-http-error' with (STATUS MESSAGE URL)."
  (let ((full-url (efrit-auth--url url params))
        (body-string (cond ((null body) nil)
                           ((stringp body) body)
                           (t (json-encode body))))
        (attempt 0)
        (result nil) (done nil))
    (while (not done)
      (cl-incf attempt)
      (let* ((headers `(("Authorization" . ,(efrit-auth-authorization host user))
                        ("Accept" . ,(or accept (if raw "*/*" "application/json")))
                        ,@(when body-string
                            `(("Content-Type" . ,(or content-type "application/json"))))))
             (started (float-time)))
        (pcase-let ((`(,status ,_hdrs ,bytes) (efrit-auth--http method full-url headers body-string)))
          (efrit-log 'debug "auth: %s %s -> %s in %.1fs" method
                     (truncate-string-to-width full-url 80 nil nil "…") status (- (float-time) started))
          (cond
           ((and (>= status 200) (< status 300))
            (setq result (if raw bytes
                           (let ((text (decode-coding-string bytes 'utf-8)))
                             (if (string-empty-p (string-trim text)) nil
                               (json-parse-string text :object-type 'alist :array-type 'list
                                                  :null-object nil :false-object nil))))
                  done t))
           ((and (= status 401) (< attempt efrit-auth-max-attempts))
            (efrit-log 'info "auth: 401 from %s, refreshing the token" host)
            (efrit-auth--force-refresh host user))
           ((and (or (= status 0) (memq status '(500 502 503 504)))
                 (< attempt efrit-auth-max-attempts))
            (let ((wait (min 10 (* 2 attempt))))
              (efrit-log 'warn "auth: %s from %s (%s); retry in %ds"
                         (if (= status 0) "no response" status) host
                         (efrit-auth--error-message (decode-coding-string bytes 'utf-8)) wait)
              (sleep-for wait)))
           (t
            (signal 'efrit-auth-http-error
                    (list status (efrit-auth--error-message (decode-coding-string bytes 'utf-8))
                          full-url)))))))
    result))

(defun efrit-auth-explain (err)
  "A readable one-line explanation of ERR from `efrit-auth-request'."
  (pcase err
    (`(efrit-auth-http-error 401 ,msg . ,_) (format "not authorized (401): %s; try M-x efrit-auth-reauthorize" msg))
    (`(efrit-auth-http-error 403 ,msg . ,_)
     (cond
      ((string-match-p "insufficient\\|scope\\|ACCESS_TOKEN_SCOPE" msg)
       (format "the token lacks a scope (403): %s; add it to the auth-source entry and run M-x efrit-auth-reauthorize" msg))
      ((string-match-p "has not been used in project\\|is disabled\\|SERVICE_DISABLED" msg)
       (format "the API is not enabled for the OAuth client's Cloud project (403): %s; enable it under APIs & Services > Library in the Google Cloud console" msg))
      ((string-match-p "admin_policy_enforced\\|access_denied" msg)
       (format "blocked by the Workspace administrator's API controls (403): %s; the client must be allowed those scopes in the Admin console" msg))
      (t (format "access refused (403): %s" msg))))
    (`(efrit-auth-http-error 404 ,msg . ,_) (format "not found (404): %s" msg))
    (`(efrit-auth-http-error ,status ,msg . ,_) (format "HTTP %s: %s" status msg))
    (_ (error-message-string err))))

(provide 'efrit-auth)

;;; efrit-auth.el ends here
