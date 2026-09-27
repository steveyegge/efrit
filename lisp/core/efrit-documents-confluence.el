;;; efrit-documents-confluence.el --- Confluence as a document source -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; Confluence pages and blog posts for `efrit-documents', from Atlassian
;; Cloud sites and self-hosted (Server / Data Center) installations
;; alike.  Both speak the same REST API (`/rest/api/content'); they
;; differ in the URL prefix (`/wiki' on Cloud), the page URL shapes, and
;; how you authenticate.
;;
;; Setup is `efrit-documents-confluence-sites': one entry per site,
;;
;;   (("https://example.atlassian.net/wiki" . "example.atlassian.net")
;;    ("https://wiki.example.com" . "wiki.example.com"))
;;
;; the base URL of the Confluence web root (with `/wiki' on Cloud) and
;; the auth-source host that holds the credentials.  Each site is its
;; own source, named `confluence' for the first and `confluence-2',
;; ... for the rest, or by the third element of the entry.
;;
;; Credentials, in auth-source under that host:
;;   - Cloud: an API token from id.atlassian.com, sent as HTTP Basic
;;     with your e-mail as the user.  The entry needs `auth-type basic':
;;       machine example.atlassian.net login you@example.com
;;         password <api token> auth-type basic
;;   - Server / Data Center 7.9+: a personal access token, sent as
;;     Bearer (the default for a plain entry):
;;       machine wiki.example.com login you password <PAT>
;;     An older server without PATs takes your password with
;;     `auth-type basic'.
;;   - An OAuth2 entry (client-id, client-secret, auth-url, token-url,
;;     scope) works too, through `efrit-auth'; Atlassian's 3LO flow is
;;     more setup than a token and gives the same access.
;;
;; A page is fetched with `body.storage' (Confluence's XHTML) and the
;; version, which gives the modified time for the cache.  For the
;; model the XHTML is rendered to text with shr when libxml is
;; available (macros such as code blocks and tables come out readable),
;; else tags are stripped; `html' format hands the XHTML over as is.
;; Search is CQL: title words, or text words, within `lastmodified'
;; bounds, pages and blog posts only, newest first.
;;
;; URLs recognised: `.../spaces/KEY/pages/ID/Title' (Cloud and new
;; DC), `.../pages/viewpage.action?pageId=ID', `.../x/SHORT' tiny links
;; (Cloud) and `.../display/KEY/Title' (resolved by a CQL title lookup,
;; one request).  `M-x efrit-documents-confluence-check' tries each
;; configured site.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'url-parse)
(require 'url-util)
(require 'efrit-documents)
(require 'efrit-auth)

(declare-function shr-insert-document "shr")
(declare-function libxml-parse-html-region "xml.c")
(defvar shr-width) (defvar shr-use-fonts) (defvar shr-inhibit-images)

(defgroup efrit-documents-confluence nil
  "Confluence as an efrit document source."
  :group 'efrit-documents
  :prefix "efrit-documents-confluence-")

(defcustom efrit-documents-confluence-sites nil
  "Confluence sites: (BASE-URL AUTH-HOST [NAME]) or (BASE-URL . AUTH-HOST).
BASE-URL is the web root, with /wiki for Atlassian Cloud
\(https://example.atlassian.net/wiki) and without for a self-hosted
site (https://wiki.example.com).  AUTH-HOST names the auth-source
entry; see the Commentary for its shape.  NAME is the source name,
default confluence, confluence-2, ...  Each site registers when this
file loads and again on `efrit-documents-confluence-register'."
  :type '(repeat (choice (cons string string) (list string string string)))
  :set (lambda (sym value)
         (set-default sym value)
         (when (fboundp 'efrit-documents-confluence-register)
           (efrit-documents-confluence-register))))

(defcustom efrit-documents-confluence-user nil
  "The auth-source user to look up, or nil for any."
  :type '(choice (const nil) string))

(defcustom efrit-documents-confluence-content-types '("page" "blogpost")
  "Content types searched and fetched."
  :type '(repeat string))

(defclass efrit-document-source-confluence (efrit-document-source)
  ((base-url :initarg :base-url :reader efrit-documents-confluence-base-url
             :documentation "Web root, no trailing slash: https://x.atlassian.net/wiki or https://wiki.x.com.")
   (auth-host :initarg :auth-host :reader efrit-documents-confluence-auth-host))
  "One Confluence site.")

(defun efrit-documents-confluence-cloud-p (source)
  "Whether SOURCE is an Atlassian Cloud site (decided by its base URL)."
  (string-match-p "\\.atlassian\\.net\\(/wiki\\)?\\'\\|/wiki\\'" (efrit-documents-confluence-base-url source)))

;;;; Requests

(defun efrit-documents-confluence--request (source path &optional params)
  "GET PATH (under /rest/api) of SOURCE with PARAMS; a parsed JSON alist."
  (condition-case err
      (efrit-auth-request (efrit-documents-confluence-auth-host source) "GET"
                          (concat (efrit-documents-confluence-base-url source) "/rest/api" path)
                          :user efrit-documents-confluence-user :params params)
    (efrit-auth-http-error
     (signal 'efrit-documents-error (list (efrit-auth-explain err))))))

;;;; URLs

(defun efrit-documents-confluence--site-path (source url)
  "URL's path and query relative to SOURCE's web root, or nil when URL is elsewhere."
  (let* ((base (efrit-documents-confluence-base-url source))
         (parsed (ignore-errors (url-generic-parse-url url)))
         (base-parsed (url-generic-parse-url base)))
    (when (and parsed (url-host parsed)
               (equal (downcase (url-host parsed)) (downcase (url-host base-parsed))))
      (let ((prefix (url-filename base-parsed))
            (path (url-filename parsed)))
        (cond
         ((string-empty-p prefix) path)
         ((string-prefix-p prefix path) (substring path (length prefix)))
         ;; A Cloud link written without /wiki still belongs to the site.
         (t path))))))

(cl-defmethod efrit-documents-source-match ((source efrit-document-source-confluence) url)
  "A page id, or a title:SPACE:TITLE / tiny:KEY marker the fetch resolves."
  (when-let* ((path (and (stringp url) (efrit-documents-confluence--site-path source url))))
    (cond
     ((string-match "/pages/\\([0-9]+\\)\\(?:/\\|\\'\\|\\?\\)" path) (match-string 1 path))
     ((string-match "viewpage\\.action\\?.*\\bpageId=\\([0-9]+\\)" path) (match-string 1 path))
     ((string-match "/display/\\([^/?#]+\\)/\\([^?#]+\\)" path)
      (format "title:%s:%s" (match-string 1 path)
              (url-unhex-string (string-replace "+" " " (match-string 2 path)))))
     ((string-match "/x/\\([A-Za-z0-9_-]+\\)\\'" path) (format "tiny:%s" (match-string 1 path)))
     (t nil))))

(defun efrit-documents-confluence--resolve-id (source id)
  "A numeric content id for ID, which may be a title: or tiny: marker."
  (cond
   ((string-match "\\`title:\\([^:]+\\):\\(.+\\)\\'" id)
    (let* ((space (match-string 1 id)) (title (match-string 2 id))
           (result (efrit-documents-confluence--request
                    source "/content"
                    `(("spaceKey" . ,space) ("title" . ,title) ("limit" . "1")
                      ("expand" . "version"))))
           (hit (car (alist-get 'results result))))
      (or (alist-get 'id hit)
          (signal 'efrit-documents-error (list (format "no page titled %S in space %s" title space))))))
   ((string-match "\\`tiny:\\(.+\\)\\'" id)
    ;; Tiny links redirect to the page; the API has no lookup for them,
    ;; but the redirect target carries pageId or /pages/ID.
    (let* ((url (format "%s/x/%s" (efrit-documents-confluence-base-url source) (match-string 1 id)))
           (target (efrit-documents-confluence--redirect-target source url)))
      (or (and target (let ((resolved (efrit-documents-source-match source target)))
                        (and resolved (not (string-match-p "\\`\\(title\\|tiny\\):" resolved)) resolved)))
          (signal 'efrit-documents-error (list (format "cannot resolve tiny link %s" url))))))
   (t id)))

(defun efrit-documents-confluence--redirect-target (source url)
  "The Location a HEAD of URL redirects to, authenticated, or nil."
  (let ((url-request-method "HEAD")
        (url-request-extra-headers
         `(("Authorization" . ,(efrit-auth-authorization (efrit-documents-confluence-auth-host source)
                                                         efrit-documents-confluence-user))))
        (url-max-redirections 0))
    (when-let* ((buf (ignore-errors (url-retrieve-synchronously url t t efrit-auth-request-timeout))))
      (unwind-protect
          (with-current-buffer buf
            (goto-char (point-min))
            (when (re-search-forward "^[Ll]ocation: *\\(.*?\\)\r?$" nil t)
              (let ((loc (match-string 1)))
                (if (string-prefix-p "/" loc)
                    (concat (let ((b (url-generic-parse-url (efrit-documents-confluence-base-url source))))
                              (format "%s://%s" (url-type b) (url-host b)))
                            loc)
                  loc))))
        (kill-buffer buf)))))

;;;; Documents

(defun efrit-documents-confluence--doc (source content)
  "A document plist from a content result CONTENT (an alist)."
  (let* ((links (alist-get '_links content))
         (webui (alist-get 'webui links))
         (version (alist-get 'version content))
         (space (alist-get 'space content)))
    (list :source (efrit-documents-source-name source)
          :id (format "%s" (alist-get 'id content))
          :title (alist-get 'title content)
          :url (if webui
                   (concat (efrit-documents-confluence-base-url source) webui)
                 (format "%s/pages/viewpage.action?pageId=%s"
                         (efrit-documents-confluence-base-url source) (alist-get 'id content)))
          :modified (alist-get 'when version)
          :kind (or (alist-get 'type content) "page")
          :space (alist-get 'key space)
          :author (alist-get 'displayName (alist-get 'by version)))))

(defconst efrit-documents-confluence--expand "version,space")

(cl-defmethod efrit-documents-source-metadata ((source efrit-document-source-confluence) id)
  (let ((id (efrit-documents-confluence--resolve-id source id)))
    (efrit-documents-confluence--doc
     source (efrit-documents-confluence--request
             source (concat "/content/" id)
             `(("expand" . ,efrit-documents-confluence--expand))))))

(defun efrit-documents-confluence-storage-to-text (xhtml)
  "Confluence storage-format XHTML as readable text.
Structured macros are unwrapped so their bodies (code, notes, panels)
stay; shr renders the rest when libxml is available, else tags go."
  (with-temp-buffer
    (insert xhtml)
    (goto-char (point-min))
    ;; Code and plain-text macro bodies are CDATA; keep their content.
    (while (re-search-forward "<ac:plain-text-body><!\\[CDATA\\[\\(\\(?:.\\|\n\\)*?\\)\\]\\]></ac:plain-text-body>" nil t)
      (replace-match "<pre>\\1</pre>" t))
    (goto-char (point-min))
    (while (re-search-forward "</?ac:[a-z-]+[^>]*>" nil t) (replace-match "" t t))
    (goto-char (point-min))
    (while (re-search-forward "<ri:[a-z-]+[^>]*/>" nil t) (replace-match "" t t))
    (if (fboundp 'libxml-parse-html-region)
        (progn
          (require 'shr)
          (let ((dom (libxml-parse-html-region (point-min) (point-max))))
            (erase-buffer)
            (when dom
              (let ((shr-width 80) (shr-use-fonts nil) (shr-inhibit-images t))
                (shr-insert-document dom)))))
      ;; No libxml: block tags become line breaks, the rest go.
      (goto-char (point-min))
      (while (re-search-forward "</?\\(p\\|div\\|h[1-6]\\|li\\|tr\\|pre\\|br\\|table\\)\\b[^>]*>" nil t)
        (replace-match "\n" t t))
      (goto-char (point-min))
      (while (re-search-forward "<[^>]+>" nil t) (replace-match "" t t))
      (goto-char (point-min))
      (while (re-search-forward "[ \t]+" nil t) (replace-match " " t t)))
    (string-trim (replace-regexp-in-string "\n\\{3,\\}" "\n\n" (buffer-string) t t))))

(cl-defmethod efrit-documents-source-fetch ((source efrit-document-source-confluence) id format)
  (let* ((id (efrit-documents-confluence--resolve-id source id))
         (content (efrit-documents-confluence--request
                   source (concat "/content/" id)
                   `(("expand" . ,(concat "body.storage," efrit-documents-confluence--expand)))))
         (xhtml (or (alist-get 'value (alist-get 'storage (alist-get 'body content))) ""))
         (doc (efrit-documents-confluence--doc source content)))
    (append doc
            (if (eq format 'html)
                (list :text xhtml :format 'html)
              (list :text (efrit-documents-confluence-storage-to-text xhtml) :format 'text)))))

;;;; Search

(defun efrit-documents-confluence--cql-quote (string)
  "STRING as a CQL double-quoted literal."
  (concat "\"" (replace-regexp-in-string "[\"\\\\]" "\\\\\\&" string) "\""))

(defun efrit-documents-confluence--cql (query)
  "The CQL for QUERY."
  (let ((parts (list (format "type in (%s)" (mapconcat (lambda (s) (concat "\"" s "\""))
                                                        efrit-documents-confluence-content-types ",")))))
    ;; Any title word (ranked by overlap in `efrit-documents-search').
    (when-let* ((words (plist-get query :title)))
      (push (concat "(" (mapconcat (lambda (w) (format "title ~ %s" (efrit-documents-confluence--cql-quote w)))
                                   words " or ")
                    ")")
            parts))
    (dolist (w (plist-get query :text))
      (push (format "text ~ %s" (efrit-documents-confluence--cql-quote w)) parts))
    (when-let* ((since (efrit-documents--time (plist-get query :since))))
      (push (format "lastmodified >= %s" (efrit-documents-confluence--cql-quote
                                          (format-time-string "%F %R" since t)))
            parts))
    (when-let* ((until (efrit-documents--time (plist-get query :until))))
      (push (format "lastmodified <= %s" (efrit-documents-confluence--cql-quote
                                          (format-time-string "%F %R" until t)))
            parts))
    (concat (string-join (nreverse parts) " and ") " order by lastmodified desc")))

(cl-defmethod efrit-documents-source-search ((source efrit-document-source-confluence) query)
  (let ((result (efrit-documents-confluence--request
                 source "/content/search"
                 `(("cql" . ,(efrit-documents-confluence--cql query))
                   ("limit" . ,(number-to-string (min 100 (or (plist-get query :limit) 10))))
                   ("expand" . ,efrit-documents-confluence--expand)))))
    (mapcar (lambda (c) (efrit-documents-confluence--doc source c)) (alist-get 'results result))))

;;;; Registration and checks

(defvar efrit-documents-confluence--sources nil
  "The registered site sources.")

(defun efrit-documents-confluence--site (entry n)
  "The source for site ENTRY, the Nth configured (from 1)."
  (pcase-let* ((`(,base ,host ,name) (if (consp (cdr entry)) entry (list (car entry) (cdr entry) nil)))
               (base (string-remove-suffix "/" base))
               (name (or name (if (= n 1) "confluence" (format "confluence-%d" n)))))
    (efrit-document-source-confluence
     :name name
     :description (format "Confluence at %s: pages and blog posts%s" base
                          (if (string-match-p "atlassian\\.net" base) " (Atlassian Cloud)" ""))
     :base-url base :auth-host host)))

(defun efrit-documents-confluence-register ()
  "Register one source per entry of `efrit-documents-confluence-sites'."
  (interactive)
  (dolist (old efrit-documents-confluence--sources)
    (efrit-documents-unregister (efrit-documents-source-name old)))
  (setq efrit-documents-confluence--sources nil)
  (let ((n 0))
    (dolist (entry efrit-documents-confluence-sites)
      (push (efrit-documents-register (efrit-documents-confluence--site entry (cl-incf n)))
            efrit-documents-confluence--sources)))
  (setq efrit-documents-confluence--sources (nreverse efrit-documents-confluence--sources)))

(efrit-documents-confluence-register)

;;;###autoload
(defun efrit-documents-confluence-check ()
  "Try each configured Confluence site: who the credentials log in as."
  (interactive)
  (efrit-documents-confluence-register)
  (if (null efrit-documents-confluence--sources)
      (message "efrit-documents: no Confluence sites; set `efrit-documents-confluence-sites'")
    (dolist (source efrit-documents-confluence--sources)
      (condition-case-unless-debug err
          (let ((me (efrit-documents-confluence--request source "/user/current")))
            (setf (efrit-documents-source-unavailable source) nil)
            (message "efrit-documents: %s (%s) works as %s"
                     (efrit-documents-source-name source) (efrit-documents-confluence-base-url source)
                     (or (alist-get 'displayName me) (alist-get 'username me) (alist-get 'email me) "?")))
        (error
         (setf (efrit-documents-source-unavailable source) (efrit-documents-explain err))
         (message "efrit-documents: %s (%s) is not usable: %s"
                  (efrit-documents-source-name source) (efrit-documents-confluence-base-url source)
                  (efrit-documents-explain err)))))))

(provide 'efrit-documents-confluence)

;;; efrit-documents-confluence.el ends here
