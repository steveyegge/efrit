;;; test-efrit-documents.el --- Tests for efrit-auth, efrit-documents and the Drive source -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'efrit-auth)
(require 'efrit-documents)
(require 'efrit-documents-gdrive)
(require 'efrit-documents-confluence)
(require 'efrit-documents-gcalendar)

;;;; efrit-auth

(defmacro test-auth--with-entries (entries &rest body)
  "Run BODY with auth-source returning ENTRIES (a form) for any search."
  (declare (indent 1))
  `(let ((efrit-auth--credentials (make-hash-table :test #'equal))
         (efrit-auth--tokens (make-hash-table :test #'equal)))
     (cl-letf (((symbol-function 'auth-source-search)
                (lambda (&rest spec)
                  (let ((host (plist-get spec :host)))
                    (seq-filter (lambda (e) (equal (plist-get e :host) host)) ,entries)))))
       ,@body)))

(defconst test-auth--oauth-entry
  '(:host "gdrive" :user "me@example.com" :client-id "cid" :client-secret "sec"
    :auth-url "https://a" :token-url "https://t" :scope "https://www.googleapis.com/auth/drive.readonly"
    :redirect-uri "http://localhost:8999"))

(ert-deftest test-efrit-auth-credentials-and-authorization ()
  "OAuth2 entries win over plain ones; plain ones give Bearer or Basic;
missing entries signal with the host named."
  (test-auth--with-entries
      (list '(:host "wiki" :user "me" :secret "tok")
            '(:host "cloud" :user "me@x" :secret "apitoken" :auth-type "basic")
            '(:host "gdrive" :user "old" :secret "plain")
            test-auth--oauth-entry)
    (should (equal "Bearer tok" (efrit-auth-authorization "wiki")))
    (should (equal (concat "Basic " (base64-encode-string "me@x:apitoken" t))
                   (efrit-auth-authorization "cloud")))
    (should (efrit-auth-oauth2-p (efrit-auth-credentials "gdrive")))
    (let ((calls nil))
      (cl-letf (((symbol-function 'oauth2-auth-and-store)
                 (lambda (&rest args) (push args calls) 'token-object))
                ((symbol-function 'oauth2-refresh-access) (lambda (tok _host) tok))
                ((symbol-function 'oauth2-token-access-token) (lambda (_tok) "ACCESS"))
                ((symbol-function 'require) (lambda (&rest _) t)))
        (should (equal "Bearer ACCESS" (efrit-auth-authorization "gdrive")))
        ;; Called with the ELPA oauth2 signature: auth token scope id secret redirect state user host pkce.
        (should (equal '("https://a" "https://t" "https://www.googleapis.com/auth/drive.readonly" "cid" "sec"
                         "http://localhost:8999" nil "me@example.com" "gdrive" nil)
                       (car calls)))
        ;; The token is kept: a second call does not run the flow again.
        (efrit-auth-authorization "gdrive")
        (should (= 1 (length calls)))))
    (should-error (efrit-auth-credentials "nowhere") :type 'efrit-auth-no-credentials)
    (should (string-match-p "nowhere" (cadr (should-error (efrit-auth-credentials "nowhere")))))))

(ert-deftest test-efrit-auth-search-disables-xoauth2-advice-and-cache ()
  "The lookup runs with the cache off and, when the xoauth2 plugin's
advice is installed, without it."
  (let ((seen-cache 'unset) (advised-during nil))
    (cl-letf (((symbol-function 'auth-source-xoauth2-plugin--search-backends) (lambda (fn &rest a) (apply fn a)))
              ((symbol-function 'auth-source-search)
               (lambda (&rest _)
                 (setq seen-cache auth-source-do-cache
                       advised-during (advice-member-p 'auth-source-xoauth2-plugin--search-backends
                                                       'auth-source-search-backends))
                 nil)))
      (advice-add 'auth-source-search-backends :around 'auth-source-xoauth2-plugin--search-backends)
      (unwind-protect
          (progn
            (efrit-auth--search "h" nil)
            (should-not seen-cache)
            (should-not advised-during)
            ;; Put back afterwards.
            (should (advice-member-p 'auth-source-xoauth2-plugin--search-backends 'auth-source-search-backends)))
        (advice-remove 'auth-source-search-backends 'auth-source-xoauth2-plugin--search-backends)))))

(ert-deftest test-efrit-auth-candidate-hosts-and-describe ()
  "Hosts from `efrit-auth-host-functions' follow the given ones, deduplicated;
`efrit-auth-describe' names the entries without secrets."
  (let ((efrit-auth-host-functions (list (lambda () '("work-gmail" "gmail")) (lambda () (error "broken")))))
    (should (equal '("gcalendar" "gdrive" "gmail" "work-gmail")
                   (efrit-auth-candidate-hosts "gcalendar" '("gdrive" "gmail")))))
  (test-auth--with-entries (list test-auth--oauth-entry)
    (efrit-auth-describe "gdrive")
    (with-current-buffer "*efrit-auth*"
      (should (string-match-p "user \"me@example.com\"  oauth2 yes  scope \"https://www.googleapis.com/auth/drive.readonly\"" (buffer-string)))
      (should-not (string-match-p "\\bsec\\b" (buffer-string))))
    (kill-buffer "*efrit-auth*")))

(defmacro test-auth--with-http (responses &rest body)
  "Run BODY with `efrit-auth--http' returning RESPONSES in turn and recording requests in REQUESTS."
  (declare (indent 1))
  `(let ((responses ,responses) (requests nil))
     (cl-letf (((symbol-function 'efrit-auth--http)
                (lambda (method url headers body)
                  (push (list method url headers body) requests)
                  (or (pop responses) (list 0 nil "queue empty"))))
               ((symbol-function 'efrit-auth-authorization) (lambda (&rest _) "Bearer X"))
               ((symbol-function 'sleep-for) #'ignore))
       ,@body)))

(ert-deftest test-efrit-auth-request-parses-retries-and-errors ()
  "JSON comes back parsed, params are encoded, a 401 refreshes and retries,
5xx and no-response retry, other failures signal with status and message."
  (let ((efrit-auth--tokens (make-hash-table :test #'equal)))
    (test-auth--with-http (list (list 200 nil "{\"files\":[{\"id\":\"1\",\"name\":\"A\"}]}"))
      (let ((r (efrit-auth-request "h" "GET" "https://x/files" :params '(("q" . "name contains 'a b'")))))
        (should (equal "A" (alist-get 'name (car (alist-get 'files r)))))
        (should (equal "https://x/files?q=name%20contains%20%27a%20b%27" (nth 1 (car requests))))
        (should (equal "Bearer X" (cdr (assoc "Authorization" (nth 2 (car requests))))))))
    (test-auth--with-http (list (list 401 nil "{\"error\":{\"message\":\"expired\"}}")
                                (list 200 nil "{\"ok\":true}"))
      (let ((refreshed nil))
        (cl-letf (((symbol-function 'efrit-auth--force-refresh) (lambda (&rest _) (setq refreshed t))))
          (should (equal '((ok . t)) (efrit-auth-request "h" "GET" "https://x")))
          (should refreshed)
          (should (= 2 (length requests))))))
    (test-auth--with-http (list (list 0 nil "connection reset") (list 503 nil "busy") (list 200 nil "[1,2]"))
      (should (equal '(1 2) (efrit-auth-request "h" "GET" "https://x")))
      (should (= 3 (length requests))))
    (test-auth--with-http (list (list 403 nil "{\"error\":{\"message\":\"Request had insufficient authentication scopes.\"}}"))
      (let ((err (should-error (efrit-auth-request "h" "GET" "https://x") :type 'efrit-auth-http-error)))
        (should (= 403 (nth 1 err)))
        (should (string-match-p "insufficient" (nth 2 err)))
        (should (string-match-p "lacks a scope" (efrit-auth-explain err)))))
    (should (string-match-p "not enabled for the OAuth client's Cloud project"
                            (efrit-auth-explain '(efrit-auth-http-error 403 "Google Calendar API has not been used in project 123 before or it is disabled." "u"))))
    (should (string-match-p "Workspace administrator"
                            (efrit-auth-explain '(efrit-auth-http-error 403 "admin_policy_enforced" "u"))))
    (test-auth--with-http (list (list 200 nil "raw bytes here"))
      (should (equal "raw bytes here" (efrit-auth-request "h" "GET" "https://x" :raw t)))
      (should (equal "*/*" (cdr (assoc "Accept" (nth 2 (car requests)))))))
    (test-auth--with-http (list (list 201 nil ""))
      (should-not (efrit-auth-request "h" "POST" "https://x" :body '((a . 1))))
      (should (equal "{\"a\":1}" (nth 3 (car requests))))
      (should (equal "application/json" (cdr (assoc "Content-Type" (nth 2 (car requests)))))))))

;;;; efrit-documents with a fake source

(defclass test-doc-source (efrit-document-source)
  ((docs :initarg :docs :initform nil)
   (fetches :initform 0 :accessor test-doc-source-fetches)
   (meta :initarg :meta :initform t :documentation "Whether metadata is answered.")))

(cl-defmethod efrit-documents-source-match ((_s test-doc-source) url)
  (and (string-match "\\`https://fake/\\([a-z0-9]+\\)" url) (match-string 1 url)))

(cl-defmethod efrit-documents-source-metadata ((s test-doc-source) id)
  (and (oref s meta)
       (let ((d (cdr (assoc id (oref s docs)))))
         (and d (list :source "fake" :id id :title (plist-get d :title) :modified (plist-get d :modified))))))

(cl-defmethod efrit-documents-source-fetch ((s test-doc-source) id format)
  (cl-incf (test-doc-source-fetches s))
  (let ((d (or (cdr (assoc id (oref s docs))) (signal 'efrit-documents-error (list "no such doc")))))
    (list :source "fake" :id id :title (plist-get d :title) :modified (plist-get d :modified)
          :url (concat "https://fake/" id) :text (plist-get d :text) :format format)))

(cl-defmethod efrit-documents-source-search ((s test-doc-source) query)
  (let ((words (or (plist-get query :title) (plist-get query :text))))
    (delq nil (mapcar (lambda (e)
                        (let ((d (cdr e)))
                          (and (seq-some (lambda (w) (string-match-p (regexp-quote w) (downcase (plist-get d :title)))) words)
                               (list :source "fake" :id (car e) :title (plist-get d :title)
                                     :modified (plist-get d :modified) :url (concat "https://fake/" (car e))))))
                      (oref s docs)))))

(defmacro test-docs--with-source (docs &rest body)
  (declare (indent 1))
  `(let ((efrit-documents--sources nil)
         (efrit-documents--cache (make-hash-table :test #'equal))
         (efrit-documents-related-functions nil)
         (efrit-documents-max-chars 40000))
     (let ((source (efrit-documents-register (test-doc-source :name "fake" :description "a fake" :docs ,docs))))
       (ignore source)
       ,@body)))

(ert-deftest test-efrit-documents-resolve-fetch-cache ()
  "URLs and SOURCE:ID refs resolve; a fetch is cached and reused while
:modified is unchanged; a changed doc is fetched again; text is clipped."
  (test-docs--with-source
      (list (cons "abc" (list :title "Plan" :modified "2026-09-01T00:00:00Z" :text "the plan"))
            (cons "big" (list :title "Big" :modified "2026-09-02T00:00:00Z" :text (make-string 100 ?x))))
    (should (equal "abc" (cdr (efrit-documents-resolve "https://fake/abc?x=1"))))
    (should (equal "abc" (cdr (efrit-documents-resolve "fake:abc"))))
    (should-not (efrit-documents-resolve "https://elsewhere/abc"))
    (should-error (efrit-documents-fetch "https://elsewhere/abc") :type 'efrit-documents-not-found)
    (let ((doc (efrit-documents-fetch "https://fake/abc")))
      (should (equal "the plan" (plist-get doc :text)))
      (should (equal "fake:abc" (efrit-documents-ref doc))))
    (efrit-documents-fetch "fake:abc")
    (should (= 1 (test-doc-source-fetches source)))
    ;; The doc changed: fetched again.
    (setf (plist-get (cdr (assoc "abc" (oref source docs))) :modified) "2026-09-03T00:00:00Z")
    (efrit-documents-fetch "fake:abc")
    (should (= 2 (test-doc-source-fetches source)))
    ;; Clipping.
    (let ((efrit-documents-max-chars 10))
      (should (string-match-p "\\[\\.\\.\\. 90 more characters" (plist-get (efrit-documents-fetch "fake:big") :text))))
    ;; A source without metadata: cached once per session.
    (oset source meta nil)
    (efrit-documents-fetch "fake:abc") (efrit-documents-fetch "fake:abc")
    (should (= 3 (test-doc-source-fetches source)))
    ;; expand-url: text or nil, failures as a note.
    (should (string-match-p "Title: Plan\n" (efrit-documents-expand-url "https://fake/abc")))
    (should-not (efrit-documents-expand-url "https://nothing/here"))
    (should (string-match-p "\\[document not fetched: no such doc\\]" (efrit-documents-expand-url "https://fake/zzz")))))

(ert-deftest test-efrit-documents-related-and-search ()
  "With the title search opted in, related documents are found by the
title's words near the date, without the ones already linked; the tools
list and fetch.  Off by default: providers only."
  (test-docs--with-source
      (list (cons "r1" (list :title "Widget bringup prep - recap" :modified "2026-09-15T10:00:00Z" :text "recap"))
            (cons "n1" (list :title "Notes: Widget bringup prep" :modified "2026-09-15T11:00:00Z" :text "notes"))
            (cons "w1" (list :title "Widget budget" :modified "2026-09-16T11:00:00Z" :text "one word"))
            (cons "o1" (list :title "Other" :modified "2026-09-15T11:00:00Z" :text "other")))
    (should (equal '("widget" "bringup" "prep")
                   (efrit-documents-title-words "Recap: Widget bringup prep 2026-09-15")))
    ;; Default: no providers, no search, nothing.
    (should-not (efrit-documents-related '(:title "Recap: Widget bringup prep" :date "2026-09-15T12:00:00Z")))
    ;; Sources match any one word; the full match ranks above the
    ;; one-word match even though the latter is newer.
    (let* ((efrit-documents-related-search t)
           (related (efrit-documents-related '(:title "Recap: Widget bringup prep" :date "2026-09-15T12:00:00Z"
                                                     :urls ("https://fake/r1")))))
      (should (equal '("n1" "w1") (mapcar (lambda (d) (plist-get d :id)) related))))
    ;; The title "Recap: 2026-09-21 - Platform weekly" reduces to
    ;; one word, and "Notes - Platform weekly" shares it.
    (should (equal '("platform") (efrit-documents-title-words "Recap: 2026-09-21 - Platform weekly")))
    (should (= 1 (efrit-documents--overlap '("platform") "Notes - Platform weekly")))
    (let ((efrit-documents-related-search t))
      (should (string-match-p "Related document (fake): https://fake/n1 ---\nTitle: Notes"
                              (efrit-documents-related-text '(:title "Widget bringup prep" :date "2026-09-15")))))
    (should-not (efrit-documents-related '(:title "the of and" :date nil)))
    ;; Tools.
    (should (string-match-p "2 documents.*\n- fake:r1\\|- fake:n1"
                            (efrit-documents--tool-search '(("query" . "widget")))))
    (should (string-match-p "No documents match" (efrit-documents--tool-search '(("query" . "zzz")))))
    (should (string-match-p "Title: Notes: Widget" (efrit-documents--tool-fetch '(("ref" . "fake:n1")))))
    (should (string-match-p "Could not fetch fake:zz: no such doc" (efrit-documents--tool-fetch '(("ref" . "fake:zz")))))
    (should (string-match-p "- fake: a fake" (efrit-documents--tool-sources nil)))
    ;; An unavailable source is listed but not searched.
    (setf (efrit-documents-source-unavailable source) "no credentials")
    (should-not (efrit-documents-sources))
    (should (string-match-p "unavailable: no credentials" (efrit-documents--tool-sources nil)))
    (should-not (efrit-documents-search '(:text ("widget"))))))

;;;; The Drive source

(ert-deftest test-efrit-documents-gdrive-match-and-q ()
  "Docs, Sheets, Slides and Drive URLs give their id; the q= expression
carries words, dates and the type filter."
  (let ((s (efrit-document-source-gdrive :name "gdrive")))
    (should (equal "1aB_c-9" (efrit-documents-source-match s "https://docs.google.com/document/d/1aB_c-9/edit?usp=sharing")))
    (should (equal "SHEET" (efrit-documents-source-match s "https://docs.google.com/spreadsheets/u/0/d/SHEET/edit")))
    (should (equal "FILE" (efrit-documents-source-match s "https://drive.google.com/file/d/FILE/view")))
    (should (equal "OPEN" (efrit-documents-source-match s "https://drive.google.com/open?id=OPEN")))
    (should-not (efrit-documents-source-match s "https://example.com/d/x"))
    (let ((q (efrit-documents-gdrive--q '(:title ("widget" "o'brien") :since "2026-09-12T00:00:00Z" :until "2026-09-18T00:00:00Z"))))
      (should (string-match-p (regexp-quote "(name contains 'widget' or name contains 'o\\'brien')") q))
      (should (string-search "((modifiedTime >= '2026-09-12T00:00:00Z' and modifiedTime <= '2026-09-18T00:00:00Z') or (createdTime >= '2026-09-12T00:00:00Z' and createdTime <= '2026-09-18T00:00:00Z'))" q))
      (should (string-match-p "trashed = false" q)))))

(ert-deftest test-efrit-documents-gdrive-fetch-and-search ()
  "Fetch = metadata + export through efrit-auth on the found host; search
maps files.list; auth errors become document errors with the reason."
  (let ((calls nil) (efrit-documents--cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'efrit-auth-credentials)
               (lambda (host &optional _user)
                 (if (equal host "gmail")
                     '(:host "gmail" :scope "https://mail.google.com/ https://www.googleapis.com/auth/drive.readonly")
                   (signal 'efrit-auth-no-credentials (list "none")))))
              ((symbol-function 'efrit-auth-request)
               (cl-function
                (lambda (host method url &key user params body content-type raw accept)
                  (ignore user body content-type accept)
                  (push (list host method url params raw) calls)
                  (cond
                   ((string-suffix-p "/export" url) "# Title\n\nbody\r\n")
                   ((string-match-p "/files/D1\\'" url)
                    '((id . "D1") (name . "Widget notes") (mimeType . "application/vnd.google-apps.document")
                      (modifiedTime . "2026-09-15T10:00:00Z") (webViewLink . "https://docs.google.com/document/d/D1/edit")))
                   ((string-match-p "/files/GONE\\'" url)
                    (signal 'efrit-auth-http-error (list 404 "File not found: GONE." url)))
                   ((string-suffix-p "/files" url)
                    '((files . (((id . "D1") (name . "Widget notes") (mimeType . "application/vnd.google-apps.document")
                                 (modifiedTime . "2026-09-15T10:00:00Z"))
                                ((id . "S1") (name . "Widget sheet") (mimeType . "application/vnd.google-apps.spreadsheet")
                                 (modifiedTime . "2026-09-14T10:00:00Z"))))))
                   (t (error "unexpected %s" url)))))))
      (let ((s (efrit-document-source-gdrive :name "gdrive")))
        ;; The gdrive host has no entry; the scoped gmail entry is used.
        (should (equal "gmail" (efrit-documents-gdrive--find-host s)))
        (let ((doc (efrit-documents-source-fetch s "D1" 'markdown)))
          (should (equal "Widget notes" (plist-get doc :title)))
          (should (equal "# Title\n\nbody" (plist-get doc :text)))
          (should (eq 'markdown (plist-get doc :format)))
          (should (equal "doc" (plist-get doc :kind))))
        (let ((export (seq-find (lambda (c) (string-suffix-p "/export" (nth 2 c))) calls)))
          (should (equal "text/markdown" (cdr (assoc "mimeType" (nth 3 export)))))
          (should (nth 4 export)))
        (should (equal "text/html" (cdr (assoc "mimeType"
                                              (nth 3 (progn (efrit-documents-source-fetch s "D1" 'html)
                                                            (seq-find (lambda (c) (string-suffix-p "/export" (nth 2 c))) calls)))))))
        (let ((found (efrit-documents-source-search s '(:title ("widget") :limit 5))))
          (should (equal '("D1" "S1") (mapcar (lambda (d) (plist-get d :id)) found)))
          (should (equal "sheet" (plist-get (cadr found) :kind))))
        (let ((err (should-error (efrit-documents-source-fetch s "GONE" 'markdown) :type 'efrit-documents-error)))
          (should (string-match-p "not found (404): File not found: GONE" (cadr err))))))))

(ert-deftest test-efrit-documents-gdrive-export-falls-back-when-too-large ()
  "Drive refuses exports over 10 MB; HTML falls back to Markdown, then text,
and the document says which format it got; refused everywhere is an error."
  (let ((asked nil) (efrit-documents--cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'efrit-documents-gdrive--find-host) (lambda (_s) "gmail"))
              ((symbol-function 'efrit-auth-request)
               (cl-function
                (lambda (_host _method url &key params &allow-other-keys)
                  (cond
                   ((string-suffix-p "/export" url)
                    (let ((mime (cdr (assoc "mimeType" params))))
                      (push mime asked)
                      (if (and (equal mime "text/plain") (string-match-p "/BIG/" url))
                          "Plain text of the big doc"
                        (signal 'efrit-auth-http-error (list 403 "This file is too large to be exported." url)))))
                   (t '((id . "BIG") (name . "Big") (mimeType . "application/vnd.google-apps.document")
                        (modifiedTime . "2026-09-21T10:00:00Z"))))))))
      (let* ((s (efrit-document-source-gdrive :name "gdrive"))
             (doc (efrit-documents-source-fetch s "BIG" 'html)))
        (should (equal "Plain text of the big doc" (plist-get doc :text)))
        (should (eq 'text (plist-get doc :format)))
        (should (equal '("text/html" "text/markdown" "text/plain") (reverse asked)))
        (let ((err (should-error (efrit-documents-source-fetch s "HUGE" 'markdown) :type 'efrit-documents-error)))
          (should (string-match-p "any text format" (cadr err))))))))

;;;; The Confluence source

(defun test-confluence--source (base)
  (efrit-document-source-confluence :name "confluence" :base-url base :auth-host "wiki.example.com"))

(ert-deftest test-efrit-documents-confluence-match ()
  "Cloud and self-hosted page URLs give the id or a marker; other hosts do not match."
  (let ((cloud (test-confluence--source "https://example.atlassian.net/wiki"))
        (dc (test-confluence--source "https://wiki.example.com")))
    (should (efrit-documents-confluence-cloud-p cloud))
    (should-not (efrit-documents-confluence-cloud-p dc))
    (should (equal "123456" (efrit-documents-source-match cloud "https://example.atlassian.net/wiki/spaces/ENG/pages/123456/Design+Notes")))
    (should (equal "123456" (efrit-documents-source-match cloud "https://example.atlassian.net/wiki/spaces/ENG/pages/123456")))
    (should (equal "tiny:AbCd1" (efrit-documents-source-match cloud "https://example.atlassian.net/wiki/x/AbCd1")))
    (should (equal "777" (efrit-documents-source-match dc "https://wiki.example.com/pages/viewpage.action?pageId=777&focused=1")))
    (should (equal "title:ENG:Design Notes" (efrit-documents-source-match dc "https://wiki.example.com/display/ENG/Design+Notes")))
    (should (equal "42" (efrit-documents-source-match dc "https://wiki.example.com/spaces/ENG/pages/42/New+DC+shape")))
    (should-not (efrit-documents-source-match dc "https://other.example.com/pages/viewpage.action?pageId=1"))
    (should-not (efrit-documents-source-match cloud "https://docs.google.com/document/d/x"))))

(ert-deftest test-efrit-documents-confluence-fetch-search-cql ()
  "Fetch reads body.storage and version through efrit-auth; storage XHTML
becomes text with macros unwrapped; search is CQL with title words and
date bounds; title: markers resolve by a lookup."
  (let ((calls nil) (efrit-documents--cache (make-hash-table :test #'equal)))
    (cl-letf (((symbol-function 'efrit-auth-request)
               (cl-function
                (lambda (host _method url &key params &allow-other-keys)
                  (push (list host url params) calls)
                  (cond
                   ((string-suffix-p "/rest/api/content/555" url)
                    `((id . "555") (type . "page") (title . "Design Notes")
                      (space . ((key . "ENG")))
                      (version . ((when . "2026-09-15T10:00:00.000Z") (by . ((displayName . "Ann")))))
                      (_links . ((webui . "/spaces/ENG/pages/555/Design+Notes")))
                      ,@(when (string-match-p "body.storage" (cdr (assoc "expand" params)))
                          '((body . ((storage . ((value . "<h1>Plan</h1><p>Ship <b>it</b>.</p><ac:structured-macro ac:name=\"code\"><ac:plain-text-body><![CDATA[make all]]></ac:plain-text-body></ac:structured-macro>")))))))))
                   ((string-suffix-p "/rest/api/content/search" url)
                    '((results . (((id . "555") (type . "page") (title . "Design Notes")
                                   (version . ((when . "2026-09-15T10:00:00.000Z")))
                                   (_links . ((webui . "/spaces/ENG/pages/555/Design+Notes"))))
                                  ((id . "556") (type . "blogpost") (title . "Design retro")
                                   (version . ((when . "2026-09-14T10:00:00.000Z"))))))))
                   ((string-suffix-p "/rest/api/content" url)
                    '((results . (((id . "555"))))))
                   (t (error "unexpected %s" url)))))))
      (let ((s (test-confluence--source "https://wiki.example.com")))
        (let ((doc (efrit-documents-source-fetch s "555" 'markdown)))
          (should (equal "wiki.example.com" (car (car calls))))
          (should (equal "Design Notes" (plist-get doc :title)))
          (should (equal "https://wiki.example.com/spaces/ENG/pages/555/Design+Notes" (plist-get doc :url)))
          (should (equal "2026-09-15T10:00:00.000Z" (plist-get doc :modified)))
          (should (equal "ENG" (plist-get doc :space)))
          (should (eq 'text (plist-get doc :format)))
          (should (string-match-p "Plan" (plist-get doc :text)))
          (should (string-match-p "Ship it\\." (plist-get doc :text)))
          (should (string-match-p "make all" (plist-get doc :text)))
          (should-not (string-match-p "ac:" (plist-get doc :text))))
        (should (string-match-p "<h1>Plan</h1>" (plist-get (efrit-documents-source-fetch s "555" 'html) :text)))
        (should (equal "2026-09-15T10:00:00.000Z" (plist-get (efrit-documents-source-metadata s "555") :modified)))
        ;; A display/ URL resolves through a title lookup, then fetches.
        (setq calls nil)
        (should (equal "Design Notes" (plist-get (efrit-documents-source-fetch s "title:ENG:Design Notes" 'text) :title)))
        (let ((lookup (seq-find (lambda (c) (string-suffix-p "/rest/api/content" (nth 1 c))) calls)))
          (should (equal "ENG" (cdr (assoc "spaceKey" (nth 2 lookup)))))
          (should (equal "Design Notes" (cdr (assoc "title" (nth 2 lookup))))))
        ;; Search and its CQL.
        (let ((found (efrit-documents-source-search s '(:title ("design" "o\"k") :since "2026-09-12T00:00:00Z" :limit 5))))
          (should (equal '("555" "556") (mapcar (lambda (d) (plist-get d :id)) found)))
          (should (equal "blogpost" (plist-get (cadr found) :kind)))
          (let ((cql (cdr (assoc "cql" (nth 2 (car calls))))))
            (should (string-match-p "type in (\"page\",\"blogpost\")" cql))
            (should (string-match-p "(title ~ \"design\" or title ~ \"o\\\\\"k\")" cql))
            (should (string-match-p "lastmodified >= \"2026-09-12 00:00\"" cql))
            (should (string-suffix-p "order by lastmodified desc" cql))))))))

(ert-deftest test-efrit-documents-confluence-sites-register ()
  "Each configured site is a source, named confluence, confluence-2 or as given."
  (let ((efrit-documents--sources nil)
        (efrit-documents-confluence--sources nil)
        (efrit-documents-confluence-sites
         '(("https://example.atlassian.net/wiki/" . "example.atlassian.net")
           ("https://wiki.example.com" "wiki.example.com" "intranet"))))
    (efrit-documents-confluence-register)
    (should (equal '("confluence" "intranet") (mapcar #'efrit-documents-source-name (efrit-documents-sources))))
    (should (equal "https://example.atlassian.net/wiki"
                   (efrit-documents-confluence-base-url (efrit-documents-source "confluence"))))
    (should (string-match-p "Atlassian Cloud" (efrit-documents-source-description (efrit-documents-source "confluence"))))
    ;; Re-registering replaces, not duplicates.
    (efrit-documents-confluence-register)
    (should (= 2 (length (efrit-documents-sources))))
    (let ((efrit-documents-confluence-sites nil))
      (efrit-documents-confluence-register)
      (should-not (efrit-documents-sources)))))

;;;; The Calendar provider

(ert-deftest test-efrit-documents-related-functions-run-first ()
  "A provider's documents come before the title search and are not duplicated by it."
  (test-docs--with-source
      (list (cons "n1" (list :title "Notes: Widget sync" :modified "2026-09-15T11:00:00Z" :text "notes")))
    (let ((efrit-documents-related-search t)
          (efrit-documents-related-functions
           (list (lambda (item)
                   (should (equal "Widget sync" (plist-get item :title)))
                   (list (list :source "fake" :id "n1" :title "old name")
                         (list :source "fake" :id "extra" :title "From the calendar")))
                 (lambda (_item) (error "broken provider")))))
      (let ((docs (efrit-documents-related '(:title "Widget sync" :date "2026-09-15T12:00:00Z"))))
        (should (equal '("n1" "extra") (mapcar (lambda (d) (plist-get d :id)) docs)))
        ;; The provider's title wins; the search did not add n1 again.
        (should (equal "old name" (plist-get (car docs) :title)))))))

(ert-deftest test-efrit-documents-gcalendar-exact-rule ()
  "The day comes from the title's date; that day's events are listed; only
the event whose title the item's title contains gives its attachments.
No word scoring: a same-day event with a shared word does not count."
  (let ((calls nil) (efrit-documents-gcalendar--host nil) (efrit-documents--sources nil)
        (efrit-documents-gcalendar-calendars '("primary")))
    (efrit-documents-gdrive-register)
    (cl-letf (((symbol-function 'efrit-auth-credentials)
               (lambda (host &optional _user)
                 (if (equal host "gmail")
                     '(:host "gmail" :scope "https://mail.google.com/ https://www.googleapis.com/auth/calendar.readonly")
                   (signal 'efrit-auth-no-credentials (list "none")))))
              ((symbol-function 'efrit-auth-request)
               (cl-function
                (lambda (host _method url &key params &allow-other-keys)
                  (push (list host url params) calls)
                  (should (string-match-p "/calendars/primary/events\\'" url))
                  '((items . (((id . "e1") (summary . "Platform weekly")
                               (htmlLink . "https://calendar.google.com/event?eid=e1")
                               (start . ((dateTime . "2026-09-21T14:00:00Z")))
                               (attachments . (((fileId . "NOTES1") (title . "Notes - Platform weekly")
                                                (fileUrl . "https://docs.google.com/document/d/NOTES1/edit")
                                                (mimeType . "application/vnd.google-apps.document"))
                                               ((fileUrl . "https://example.com/deck.pdf") (title . "deck")))))
                              ((id . "e2") (summary . "Platform budget review")
                               (start . ((dateTime . "2026-09-21T12:00:00Z")))
                               (attachments . (((fileId . "BUDGET") (title . "budget")))))
                              ((id . "e3") (summary . "Lunch") (start . ((dateTime . "2026-09-21T13:00:00Z")))))))))))
      ;; The mail's Date is the next morning; the title's date wins.
      (let ((docs (efrit-documents-gcalendar-related
                   '(:title "Recap: 2026-09-21 - Platform weekly" :date "2026-09-22T08:00:00Z"))))
        (should (equal "gmail" (car (car calls))))
        (let ((min (cdr (assoc "timeMin" (nth 2 (car calls)))))
              (max (cdr (assoc "timeMax" (nth 2 (car calls))))))
          ;; One calendar day around 2026-09-21 local noon.
          (should (< (float-time (date-to-time min)) (float-time (date-to-time "2026-09-21T12:00:00"))))
          (should (> (float-time (date-to-time max)) (float-time (date-to-time "2026-09-21T12:00:00"))))
          (should (= (* 24 3600) (- (float-time (date-to-time max)) (float-time (date-to-time min))))))
        (should (equal "true" (cdr (assoc "singleEvents" (nth 2 (car calls))))))
        ;; Only the Drive attachment of the matching event; the budget
        ;; meeting shares a word and is on the same day, and does not count.
        (should (equal '("NOTES1") (mapcar (lambda (d) (plist-get d :id)) docs)))
        (should (equal "gdrive" (plist-get (car docs) :source)))
        (should (equal "Platform weekly" (plist-get (car docs) :event))))
      ;; The default match rule.
      (should (efrit-documents-gcalendar-title-matches-p "Notes: Platform weekly" "Platform weekly"))
      (should (efrit-documents-gcalendar-title-matches-p "platform weekly" "Platform weekly!"))
      (should-not (efrit-documents-gcalendar-title-matches-p "Platform budget" "Platform weekly"))
      ;; Whole-word containment, either way round is not: the item must contain the event.
      (should (efrit-documents-gcalendar-title-matches-p "Platform weekly" "weekly"))
      (should-not (efrit-documents-gcalendar-title-matches-p "weekly" "Platform weekly"))
      (should-not (efrit-documents-gcalendar-title-matches-p "x" ""))
      ;; The primitive with an exact rule of one's own: the day and an
      ;; equality test, nothing inferred.
      (should (equal '("NOTES1")
                     (mapcar (lambda (d) (plist-get d :id))
                             (efrit-documents-gcalendar-attachments
                              (date-to-time "2026-09-21T12:00:00")
                              (lambda (event-title) (equal event-title "Platform weekly"))))))
      (should-not (efrit-documents-gcalendar-attachments
                   (date-to-time "2026-09-21T12:00:00") (lambda (e) (equal e "Lunch"))))
      ;; No date anywhere: nothing to look at, no request.
      (setq calls nil)
      (should-not (efrit-documents-gcalendar-related '(:title "Widget")))
      (should-not calls))))

;;;; The Jira source (needs jira.el on the load path)

(ert-deftest test-efrit-documents-jira-source ()
  "Keys and browse URLs resolve; fetch renders the attribute block plus comments;
search is JQL; keys mentioned in an item are related documents; without
jira.el the source is registered unavailable."
  (skip-unless (and (locate-library "jira-api") (locate-library "jira-doc")))
  (require 'efrit-documents-jira)
  (let ((efrit-documents--sources nil)
        (efrit-documents--cache (make-hash-table :test #'equal))
        (efrit-documents-related-functions nil)
        (jira-base-url "https://jira.example.com")
        (jira-current-url nil)
        (calls nil))
    (cl-letf (((symbol-function 'jira-api-call)
               (cl-function
                (lambda (verb endpoint &key params &allow-other-keys)
                  (push (list verb endpoint params) calls)
                  (cond
                   ((equal endpoint "issue/INFRA-1")
                    (make-request-response
                     :status-code 200
                     :data '((id . "10") (key . "INFRA-1")
                             (fields . ((summary . "Salt master OOM") (updated . "2026-09-21T10:00:00.000+0000")
                                        (status . ((name . "Open") (statusCategory . ((name . "To Do")))))
                                        (issuetype . ((name . "Bug")))
                                        (description . "It ran out of memory.")
                                        (comment . ((comments . [((id . "1") (author . ((displayName . "Cara")))
                                                                  (created . "2026-09-21T11:00:00.000+0000")
                                                                  (body . "Bumped to 64G."))]))))))))
                   ((equal endpoint "myself")
                    (make-request-response :status-code 200 :data '((displayName . "Me"))))
                   (t (make-request-response :status-code 404 :error-thrown "nope"))))))
              ((symbol-function 'jira-api-search)
               (cl-function
                (lambda (&key params &allow-other-keys)
                  (push (list 'search params) calls)
                  (make-request-response
                   :status-code 200
                   :data '((issues . [((id . "10") (key . "INFRA-1")
                                       (fields . ((summary . "Salt master OOM") (updated . "2026-09-21T10:00:00.000+0000")
                                                  (status . ((name . "Open"))) (issuetype . ((name . "Bug"))))))])))))))
      (let ((source (efrit-documents-jira-register)))
        (should-not (efrit-documents-source-unavailable source))
        (should (memq #'efrit-documents-jira-related efrit-documents-related-functions))
        (should (equal "INFRA-1" (efrit-documents-source-match source "https://jira.example.com/browse/INFRA-1")))
        (should (equal "INFRA-1" (efrit-documents-source-match source "https://jira.example.com/jira/software/c/projects/INFRA/boards/3?selectedIssue=INFRA-1")))
        (should (equal "INFRA-1" (efrit-documents-source-match source "INFRA-1")))
        (should-not (efrit-documents-source-match source "https://other.example.com/browse/INFRA-1"))
        (should-not (efrit-documents-source-match source "https://jira.example.com/secure/Dashboard.jspa"))
        (let ((doc (efrit-documents-fetch "jira:INFRA-1")))
          (should (equal "INFRA-1: Salt master OOM" (plist-get doc :title)))
          (should (equal "https://jira.example.com/browse/INFRA-1" (plist-get doc :url)))
          (should (string-prefix-p "INFRA-1 | Bug | Open\nUpdated      2026-09-21T10:00:00.000+0000\nURL          https://jira.example.com/browse/INFRA-1\n" (plist-get doc :text)))
          (should (string-match-p "It ran out of memory\\." (plist-get doc :text)))
          (should (string-search "--- Comments ---\nCara (2026-09-21T11:00:00.000+0000):\nBumped to 64G." (plist-get doc :text))))
        (let ((found (efrit-documents-source-search source '(:text ("salt" "oom") :since "2026-09-01" :limit 5))))
          (should (equal '("INFRA-1") (mapcar (lambda (d) (plist-get d :id)) found)))
          (let ((jql (cdr (assoc "jql" (cadr (assq 'search calls))))))
            (should (string-match-p "(text ~ \"salt\" OR text ~ \"oom\") AND updated >= \"2026-09-01\" ORDER BY updated DESC" jql))))
        ;; Related: keys in the item.
        (should (equal '("INFRA-1" "OPS-22") (efrit-documents-jira-keys-in "Re: INFRA-1 and OPS-22, not infra-3 or X-1-2 twice INFRA-1")))
        (let ((related (efrit-documents-related '(:title "About INFRA-1" :body "see also NOPE-9"))))
          (should (equal '("INFRA-1") (mapcar (lambda (d) (plist-get d :id)) related))))
        ;; The tool.
        (should (string-search "\n- INFRA-1 | Open | 2026-09-21" (efrit-documents-jira--tool-search '(("jql" . "project = INFRA")))))))
    ;; Not configured: unavailable with the reason.
    (let ((jira-base-url ""))
      (should (equal "jira-base-url is not set"
                     (efrit-documents-source-unavailable (efrit-documents-jira-register)))))))

(provide 'test-efrit-documents)
;;; test-efrit-documents.el ends here
