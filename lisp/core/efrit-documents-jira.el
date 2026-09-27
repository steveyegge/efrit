;;; efrit-documents-jira.el --- Jira issues as documents, through jira.el -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; Jira issues for `efrit-documents', on top of the jira.el package
;; (https://github.com/unmonoqueteclea/jira.el).  Nothing is configured here: the connection is jira.el's
;; (`jira-base-url' and the auth-source entry for that host).  Without
;; jira.el the source registers as unavailable and says so.
;;
;; Fetch: an issue by key or by any URL on the Jira host that names
;; one (`/browse/KEY', `selectedIssue=KEY', `/issues/KEY').  The text
;; is an attribute block (status, type, priority, assignee, sprint,
;; components, ...) then the description, then every comment with its
;; author and date -- so a model sees the whole
;; conversation, not the description alone.
;;
;; Search: JQL.  `efrit-documents-search' words become `text ~ "w"'
;; clauses (title words: `summary ~'), dates bound `updated'; the
;; model's `jira_search' tool takes raw JQL.
;;
;; Related: an article that mentions an issue key (in its subject or
;; body) gets that issue as a related document; exact, no guessing.
;; That is the `efrit-documents-related-functions' provider here.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'efrit-documents)

(declare-function jira-api-call "jira-api")
(declare-function jira-api-search "jira-api")
(declare-function jira-api--get-current-url "jira-api")
(declare-function request-response-data "request")
(declare-function request-response-status-code "request")
(declare-function jira-doc-format "jira-doc")
(declare-function efrit-register-tool "efrit-tool-registry")
(defvar jira-base-url)

(defgroup efrit-documents-jira nil
  "Jira issues as an efrit document source."
  :group 'efrit-documents
  :prefix "efrit-documents-jira-")

(defcustom efrit-documents-jira-key-regexp "\\b\\([A-Z][A-Z0-9_]+-[0-9]+\\)\\b"
  "Matches an issue key in text; group 1 is the key.
Keys mentioned in an article make the issue a related document."
  :type 'regexp)

(defcustom efrit-documents-jira-max-related 5
  "Most issues taken from the keys an article mentions."
  :type 'integer)

(defcustom efrit-documents-jira-issue-fields
  '("summary" "status" "issuetype" "priority" "assignee" "reporter" "created" "updated"
    "resolution" "labels" "components" "fixVersions" "parent" "issuelinks" "description"
    "comment" "sprint" "customfield_10020")
  "Fields requested for an issue.  customfield_10020 is Cloud's sprint field."
  :type '(repeat string))

(defclass efrit-document-source-jira (efrit-document-source) ()
  "Jira, through jira.el.")

;;;; jira.el

(defun efrit-documents-jira--ready-p ()
  "Non-nil when jira.el is loaded and points at a site."
  (and (require 'jira-api nil t) (require 'jira-doc nil t)
       (boundp 'jira-base-url) (stringp jira-base-url) (not (string-empty-p jira-base-url))))

(defun efrit-documents-jira--why-not ()
  "Why the source cannot work, or nil."
  (cond ((not (and (require 'jira-api nil t) (require 'jira-doc nil t)))
         "the jira.el package is not installed")
        ((or (not (boundp 'jira-base-url)) (not (stringp jira-base-url)) (string-empty-p jira-base-url))
         "jira-base-url is not set")
        (t nil)))

(defun efrit-documents-jira--data (response what)
  "The data of jira.el RESPONSE, or signal `efrit-documents-error' about WHAT."
  (let ((status (and response (request-response-status-code response))))
    (cond
     ((null response) (signal 'efrit-documents-error (list (format "Jira: %s: no response" what))))
     ((and status (>= status 400))
      (signal 'efrit-documents-error
              (list (format "Jira: %s: HTTP %s%s" what status
                            (pcase status
                              (401 "; the token in auth-source is not accepted")
                              (403 "; not permitted for this account")
                              (404 "; no such issue")
                              (_ ""))))))
     (t (request-response-data response)))))

(defun efrit-documents-jira--url ()
  "The Jira site URL, without a trailing slash."
  (string-remove-suffix "/" (or (jira-api--get-current-url) jira-base-url)))

(defun efrit-documents-jira--host ()
  "The Jira host, for URLs and references."
  (replace-regexp-in-string "\\`https?://\\|/.*\\'" "" (efrit-documents-jira--url)))

(defun efrit-documents-jira-issue-url (key)
  "The browse URL of issue KEY."
  (concat (efrit-documents-jira--url) "/browse/" key))

;;;; Reading an issue

(defun efrit-documents-jira--field (issue &rest path)
  "The value at PATH under ISSUE's fields."
  (let ((v (alist-get 'fields issue)))
    (dolist (k path v)
      (setq v (and (listp v) (alist-get k v))))))

(defun efrit-documents-jira--person (person)
  "A display name for PERSON (an alist), or nil."
  (and person (or (alist-get 'displayName person) (alist-get 'name person) (alist-get 'emailAddress person))))

(defun efrit-documents-jira--names (values)
  "The `name' of each of VALUES (a vector or list of alists), or the strings."
  (delq nil (mapcar (lambda (v) (if (stringp v) v (alist-get 'name v)))
                    (if (vectorp values) (append values nil) values))))

(defun efrit-documents-jira--sprints (issue)
  "Names of ISSUE's sprints, from either field shape."
  (let ((raw (or (efrit-documents-jira--field issue 'sprint)
                 (efrit-documents-jira--field issue 'customfield_10020))))
    (delq nil (mapcar (lambda (s)
                        (cond ((stringp s)
                               (and (string-match "name=\\([^],]+\\)" s) (match-string 1 s)))
                              ((listp s) (alist-get 'name s))))
                      (if (vectorp raw) (append raw nil) (if (listp raw) raw (list raw)))))))

(defun efrit-documents-jira--comments (issue)
  "ISSUE's comments, oldest first."
  (append (alist-get 'comments (alist-get 'comment (alist-get 'fields issue))) nil))

(defun efrit-documents-jira--links (issue)
  "ISSUE's links as \"relation KEY: summary\" lines."
  (delq nil
        (mapcar (lambda (link)
                  (let* ((out (alist-get 'outwardIssue link))
                         (in (alist-get 'inwardIssue link))
                         (type (alist-get 'type link))
                         (other (or out in))
                         (relation (if out (alist-get 'outward type) (alist-get 'inward type))))
                    (and other
                         (format "  %s %s: %s" relation (alist-get 'key other)
                                 (or (alist-get 'summary (alist-get 'fields other)) "")))))
                (append (efrit-documents-jira--field issue 'issuelinks) nil))))

(defun efrit-documents-jira--doc-text (doc)
  "DOC (ADF alist, wiki string, or nil) as plain text through jira.el."
  (cond ((null doc) "")
        (t (condition-case nil
               (substring-no-properties (jira-doc-format doc))
             (error (if (stringp doc) doc (format "%S" doc)))))))

(defun efrit-documents-jira--attribute-line (name values)
  "One line: NAME then VALUES (strings), or nil when there are none."
  (when values
    (concat (format "%-13s" name) (string-join values ", ") "\n")))

(defun efrit-documents-jira-issue-header-block (issue)
  "ISSUE's attributes as text: key, type, status, priority, people, sprint, links, URL."
  (let* ((key (alist-get 'key issue))
         (field (lambda (&rest path) (apply #'efrit-documents-jira--field issue path)))
         (parent (funcall field 'parent)))
    (concat
     (string-join (delq nil (list key (funcall field 'issuetype 'name) (funcall field 'status 'name)
                                  (funcall field 'priority 'name) (funcall field 'resolution 'name)))
                  " | ")
     "\n"
     (efrit-documents-jira--attribute-line
      "Assignee" (and (funcall field 'assignee) (list (efrit-documents-jira--person (funcall field 'assignee)))))
     (efrit-documents-jira--attribute-line
      "Reporter" (and (funcall field 'reporter) (list (efrit-documents-jira--person (funcall field 'reporter)))))
     (efrit-documents-jira--attribute-line "Sprint" (efrit-documents-jira--sprints issue))
     (efrit-documents-jira--attribute-line "Components" (efrit-documents-jira--names (funcall field 'components)))
     (efrit-documents-jira--attribute-line "Fix versions" (efrit-documents-jira--names (funcall field 'fixVersions)))
     (efrit-documents-jira--attribute-line "Labels" (efrit-documents-jira--names (funcall field 'labels)))
     (efrit-documents-jira--attribute-line "Created" (and (funcall field 'created) (list (funcall field 'created))))
     (efrit-documents-jira--attribute-line "Updated" (and (funcall field 'updated) (list (funcall field 'updated))))
     (when parent
       (efrit-documents-jira--attribute-line
        "Parent" (list (format "%s %s" (alist-get 'key parent)
                               (or (efrit-documents-jira--field parent 'summary) "")))))
     (when-let* ((links (efrit-documents-jira--links issue)))
       (concat "Links\n" (string-join links "\n") "\n"))
     (format "%-13s" "URL") (efrit-documents-jira-issue-url key) "\n")))

;;;; Documents

(defun efrit-documents-jira--doc (issue)
  "A document plist for ISSUE (an alist from the API), without :text."
  (let ((key (alist-get 'key issue)))
    (list :source "jira"
          :id key
          :title (format "%s: %s" key (or (efrit-documents-jira--field issue 'summary) ""))
          :url (efrit-documents-jira-issue-url key)
          :modified (efrit-documents-jira--field issue 'updated)
          :kind (downcase (or (efrit-documents-jira--field issue 'issuetype 'name) "issue"))
          :status (efrit-documents-jira--field issue 'status 'name))))

(defun efrit-documents-jira-issue-text (issue)
  "ISSUE as text for a model: the attribute block, the description, then the comments."
  (concat
   (efrit-documents-jira-issue-header-block issue)
   "\n" (efrit-documents-jira--doc-text (efrit-documents-jira--field issue 'description))
   (let ((comments (efrit-documents-jira--comments issue)))
     (when comments
       (concat "\n\n--- Comments ---\n"
               (mapconcat (lambda (c)
                            (format "%s (%s):\n%s"
                                    (or (efrit-documents-jira--person (alist-get 'author c)) "?")
                                    (or (alist-get 'created c) "")
                                    (efrit-documents-jira--doc-text (alist-get 'body c))))
                          comments "\n\n"))))))

;;;; The protocol

(cl-defmethod efrit-documents-source-match ((_source efrit-document-source-jira) url)
  "The issue key URL names on the Jira host, or a bare key."
  (when (stringp url)
    (cond
     ((let ((case-fold-search nil))
        (string-match (concat "\\`" (substring efrit-documents-jira-key-regexp 2) "\\'") url))
      (match-string 1 url))
     ((and (efrit-documents-jira--ready-p)
           (string-match-p (concat "\\`https?://" (regexp-quote (efrit-documents-jira--host))) url)
           (string-match "\\(?:/browse/\\|selectedIssue=\\|/issues/\\)\\([A-Z][A-Z0-9_]+-[0-9]+\\)" url))
      (match-string 1 url)))))

(cl-defmethod efrit-documents-source-metadata ((_source efrit-document-source-jira) id)
  (efrit-documents-jira--doc
   (efrit-documents-jira--data
    (jira-api-call "GET" (concat "issue/" id) :params '(("fields" . "summary,updated,status,issuetype"))
                   :sync t :error #'ignore)
    (concat "issue " id))))

(cl-defmethod efrit-documents-source-fetch ((_source efrit-document-source-jira) id _format)
  (let ((issue (efrit-documents-jira--data
                (jira-api-call "GET" (concat "issue/" id)
                               :params `(("fields" . ,(string-join efrit-documents-jira-issue-fields ",")))
                               :sync t :error #'ignore)
                (concat "issue " id))))
    (append (efrit-documents-jira--doc issue)
            (list :text (efrit-documents-jira-issue-text issue) :format 'text))))

(defun efrit-documents-jira--jql (query)
  "The JQL for QUERY (see `efrit-documents-search'), or QUERY's :jql as given."
  (or (plist-get query :jql)
      (let ((parts nil)
            (quote (lambda (w) (concat "\"" (replace-regexp-in-string "\"" "\\\\\"" w) "\""))))
        (when-let* ((words (plist-get query :title)))
          (push (concat "(" (mapconcat (lambda (w) (format "summary ~ %s" (funcall quote w))) words " OR ") ")") parts))
        (when-let* ((words (plist-get query :text)))
          (push (concat "(" (mapconcat (lambda (w) (format "text ~ %s" (funcall quote w))) words " OR ") ")") parts))
        (when-let* ((since (efrit-documents--time (plist-get query :since))))
          (push (format "updated >= \"%s\"" (format-time-string "%F" since)) parts))
        (when-let* ((until (efrit-documents--time (plist-get query :until))))
          (push (format "updated <= \"%s\"" (format-time-string "%F" until)) parts))
        (concat (string-join (nreverse parts) " AND ") " ORDER BY updated DESC"))))

(cl-defmethod efrit-documents-source-search ((_source efrit-document-source-jira) query)
  (let* ((jql (efrit-documents-jira--jql query))
         (data (efrit-documents-jira--data
                (jira-api-search :params `(("jql" . ,jql)
                                           ("maxResults" . ,(or (plist-get query :limit) 10))
                                           ("fields" . "summary,updated,status,issuetype"))
                                 ;; jira.el's error callback retries `search' when
                                 ;; `search/jql' 404s; do not replace it.
                                 :sync t)
                (format "search %s" jql))))
    (efrit-log 'debug "documents: jira jql=%s" jql)
    (mapcar #'efrit-documents-jira--doc (append (alist-get 'issues data) nil))))

;;;; Related: issue keys mentioned in an item

(defun efrit-documents-jira-keys-in (text)
  "The distinct issue keys mentioned in TEXT, in order."
  (let ((out nil) (start 0)
        (case-fold-search nil))          ; keys are upper case; "infra-3" is not one
    (while (and (stringp text) (string-match efrit-documents-jira-key-regexp text start))
      (let ((key (match-string 1 text)))
        (unless (member key out) (push key out)))
      (setq start (match-end 0)))
    (nreverse out)))

(defun efrit-documents-jira-related (item)
  "For `efrit-documents-related-functions': the issues ITEM's title or :body mentions.
Only keys that name an existing issue count; a key that 404s is
skipped."
  (when (efrit-documents-jira--ready-p)
    (let ((keys (seq-take (efrit-documents-jira-keys-in
                           (concat (or (plist-get item :title) "") "\n" (or (plist-get item :body) "")))
                          efrit-documents-jira-max-related))
          (out nil))
      (dolist (key keys)
        (condition-case err
            (push (efrit-documents-source-metadata (efrit-documents-source "jira") key) out)
          (efrit-documents-error
           (efrit-log 'debug "documents: jira key %s mentioned but not fetched: %s" key (cadr err)))))
      (nreverse out))))

;;;; The tool

(defun efrit-documents-jira--tool-search (input)
  "jira_search: issues matching a JQL query."
  (let* ((jql (alist-get "jql" input nil nil #'equal))
         (limit (min 50 (or (alist-get "limit" input nil nil #'equal) 20)))
         (docs (efrit-documents-source-search (efrit-documents-source "jira")
                                              (list :jql jql :limit limit))))
    (if (null docs)
        (format "No issues match %s." jql)
      (concat (format "%d issue%s. Use doc_fetch with the key (or jira:KEY) for the full text and comments.\n"
                      (length docs) (if (= 1 (length docs)) "" "s"))
              (mapconcat (lambda (d) (format "- %s | %s | %s | %s" (plist-get d :id)
                                             (or (plist-get d :status) "?") (or (plist-get d :modified) "?")
                                             (plist-get d :title)))
                         docs "\n")))))

(defconst efrit-documents-jira--tools
  `(("jira_search"
     "Search the user's Jira with a JQL query (for example: project = INFRA AND resolution = Unresolved ORDER BY updated DESC). Returns keys, status, dates and titles. Read-only. Follow up with doc_fetch KEY for the description and comments."
     (("type" . "object")
      ("properties" . (("jql" . (("type" . "string") ("description" . "A JQL query")))
                       ("limit" . (("type" . "integer") ("description" . "Most results, default 20, max 50")))))
      ("required" . ["jql"]))
     ,#'efrit-documents-jira--tool-search))
  "The tool offered to efrit: (NAME DESCRIPTION INPUT-SCHEMA FUNCTION).")

(defun efrit-documents-jira-ensure-tools ()
  "Register `jira_search' with efrit when the source works."
  (when (and (efrit-documents-jira--ready-p) (require 'efrit-tool-registry nil t))
    (pcase-dolist (`(,name ,description ,schema ,fn) efrit-documents-jira--tools)
      (efrit-register-tool name :description description :input-schema schema
                           :function fn :class 'read :package 'efrit-documents))))

;;;; Registration

(defun efrit-documents-jira-register ()
  "Register the Jira source; unavailable, with the reason, when jira.el is not ready."
  (let ((source (efrit-documents-register
                 (efrit-document-source-jira
                  :name "jira"
                  :description "Jira issues (description and comments) through jira.el"
                  :unavailable (efrit-documents-jira--why-not)))))
    (unless (efrit-documents-source-unavailable source)
      (add-hook 'efrit-documents-related-functions #'efrit-documents-jira-related)
      (efrit-documents-jira-ensure-tools))
    source))

(efrit-documents-jira-register)

;;;###autoload
(defun efrit-documents-jira-check ()
  "Say whether Jira is reachable through jira.el, and as whom."
  (interactive)
  (let ((source (efrit-documents-jira-register)))
    (if-let* ((why (efrit-documents-source-unavailable source)))
        (message "efrit-documents: Jira is not usable: %s" why)
      (condition-case-unless-debug err
          (let ((me (efrit-documents-jira--data (jira-api-call "GET" "myself" :sync t :error #'ignore) "myself")))
            (message "efrit-documents: Jira at %s works as %s" (efrit-documents-jira--host)
                     (or (alist-get 'displayName me) (alist-get 'emailAddress me) (alist-get 'name me) "?")))
        (error
         (setf (efrit-documents-source-unavailable source) (efrit-documents-explain err))
         (message "efrit-documents: Jira is not usable: %s" (efrit-documents-explain err)))))))

(provide 'efrit-documents-jira)

;;; efrit-documents-jira.el ends here
