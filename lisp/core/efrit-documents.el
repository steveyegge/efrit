;;; efrit-documents.el --- Documents from external sources, as text -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; A meeting recap in mail is a link to a Google Doc; the notes Calendar
;; took for the same meeting are a second Doc nobody linked; the design
;; behind both is a Confluence page.  This file is the one place efrit
;; asks for such documents, whatever holds them.
;;
;; A SOURCE is an object of a subclass of `efrit-document-source' that
;; implements three generics:
;;
;;   (efrit-documents-source-match SOURCE URL)       -> id or nil
;;   (efrit-documents-source-fetch SOURCE ID FORMAT) -> document plist
;;   (efrit-documents-source-search SOURCE QUERY)    -> list of document plists,
;;                                                      without :text
;;
;; and optionally `efrit-documents-source-metadata' (SOURCE ID) -> the
;; plist without :text, cheaply; with it, an unchanged document (same
;; :modified) is served from the session cache without a fetch.
;;
;; A document plist has :source (the source's name), :id, :title,
;; :url, :modified (an ISO 8601 string or nil), :kind (a short word:
;; "doc", "page", "sheet"), :text (when fetched) and :format (the
;; format of :text: `markdown', `html' or `text').  QUERY is a plist:
;; :text (words to match in title or body), :title (words in the title
;; only), :since and :until (ISO dates or Emacs times), :limit.
;;
;; Sources register with `efrit-documents-register'; a source that
;; cannot work (no credentials) still registers, so the user sees it
;; listed with the reason.  Callers use:
;;
;;   `efrit-documents-fetch' URL-or-ID   the document, from cache when
;;                                       its :modified is unchanged
;;   `efrit-documents-expand-url' URL    the text for an analysis, or nil
;;   `efrit-documents-search' QUERY      across every source, or one
;;   `efrit-documents-related' PLIST     documents that belong with an
;;                                       item: the calendar event's
;;                                       attachments (and, opted in, a
;;                                       title search near the date)
;;
;; and the model gets `doc_fetch' and `doc_search' tools.  Everything
;; is read-only.  Text is capped at `efrit-documents-max-chars'.
;;
;; efrit-gnus feeds `efrit-documents-expand-url' with the links in an
;; article and `efrit-documents-related' with its subject and date, so
;; an analysis of a recap carries the recap doc and the meeting notes.
;; `efrit-documents-gdrive.el' is the Google Drive source,
;; `efrit-documents-confluence.el' the Confluence one (Cloud and
;; self-hosted).

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'eieio)
(require 'efrit-log)

(declare-function efrit-register-tool "efrit-tool-registry")

(defgroup efrit-documents nil
  "Documents from external sources for efrit's analyses."
  :group 'efrit
  :prefix "efrit-documents-")

(defcustom efrit-documents-sources-libraries '(efrit-documents-gdrive efrit-documents-gcalendar efrit-documents-confluence efrit-documents-jira)
  "Libraries loaded by `efrit-documents-ensure-tools' so their sources register.
Google Drive, Google Calendar (a related-documents provider, not a
source), Confluence and Jira (through the jira.el package) come with efrit (Confluence registers nothing
until `efrit-documents-confluence-sites' is set); add yours, or remove
one you never use."
  :type '(repeat symbol))

(defcustom efrit-documents-max-chars 40000
  "Characters kept of one document's text; the rest is cut and marked."
  :type 'integer)

(defcustom efrit-documents-related-limit 3
  "Most documents the title search in `efrit-documents-related' adds."
  :type 'integer)

(defcustom efrit-documents-related-search nil
  "Whether `efrit-documents-related' also searches sources by title words.
nil (the default): only `efrit-documents-related-functions' answer, and
they answer from facts (a calendar event's attachments).  t adds a
search of every source for the title's words within
`efrit-documents-related-days' of the date -- more finds, and false
positives with them."
  :type 'boolean)

(defcustom efrit-documents-related-days 3
  "Days either side of an item's date within which a document counts as related."
  :type 'integer)

(defcustom efrit-documents-related-stopwords
  '("the" "a" "an" "of" "for" "and" "or" "to" "in" "on" "with" "re" "fw" "fwd"
    "notes" "recap" "recaps" "bot" "summary" "minutes" "meeting" "sync"
    "weekly" "monthly" "daily" "call" "invitation" "invite" "reminder"
    "updated" "accepted" "declined" "canceled" "cancelled" "am" "pm")
  "Words dropped from a title before searching for related documents."
  :type '(repeat string))

(defcustom efrit-documents-related-functions nil
  "Functions that know which documents belong with an item, beyond title search.
Each is called with the ITEM plist (:title :date :urls :body ...) and returns a
list of document plists (with :source and :id at least, no :text), or
nil.  They run before the title search in `efrit-documents-related';
what they return comes first and is not searched for again.  The
Google Calendar provider (`efrit-documents-gcalendar') adds one: it
finds the event the item is about by date and returns the event's
attachments, whatever they are called now.  Errors are logged and
skipped."
  :type 'hook)

(define-error 'efrit-documents-error "efrit-documents error")
(define-error 'efrit-documents-not-found "efrit-documents: no source for this" 'efrit-documents-error)

;;;; Sources

(defclass efrit-document-source ()
  ((name :initarg :name :reader efrit-documents-source-name
         :documentation "Short unique name: \"gdrive\", \"confluence\".")
   (description :initarg :description :initform "" :reader efrit-documents-source-description)
   (unavailable :initarg :unavailable :initform nil :accessor efrit-documents-source-unavailable
                :documentation "nil, or a string saying why the source cannot work now."))
  "A place documents come from.  Subclasses implement the generics below."
  :abstract t)

(cl-defgeneric efrit-documents-source-match (source url)
  "The id of the document URL names in SOURCE, or nil when URL is not SOURCE's.")

(cl-defgeneric efrit-documents-source-fetch (source id format)
  "The document ID from SOURCE as a plist with :text in FORMAT (`markdown', `html' or `text').
Signal `efrit-documents-error' (or an `efrit-auth-error') when it cannot.")

(cl-defgeneric efrit-documents-source-metadata (source id)
  "The document ID of SOURCE without :text (title, :modified, :url), or nil when unknown.
Used to decide whether the cached text is current.  Default: nil, so
the document is fetched once per session.")

(cl-defmethod efrit-documents-source-metadata ((_source efrit-document-source) _id)
  nil)

(cl-defgeneric efrit-documents-source-search (source query)
  "Documents in SOURCE matching QUERY (a plist, see the Commentary), newest first, no :text.")

(cl-defmethod efrit-documents-source-search ((_source efrit-document-source) _query)
  "A source with no search returns nothing."
  nil)

(defvar efrit-documents--sources nil
  "Registered sources, in registration order.")

(defun efrit-documents-register (source)
  "Register SOURCE, replacing one of the same name."
  (setq efrit-documents--sources
        (append (cl-remove (efrit-documents-source-name source) efrit-documents--sources
                           :key #'efrit-documents-source-name :test #'equal)
                (list source)))
  (efrit-log 'debug "documents: source %s registered%s" (efrit-documents-source-name source)
             (if (efrit-documents-source-unavailable source)
                 (format " (unavailable: %s)" (efrit-documents-source-unavailable source))
               ""))
  source)

(defun efrit-documents-unregister (name)
  "Forget the source NAME."
  (setq efrit-documents--sources
        (cl-remove name efrit-documents--sources :key #'efrit-documents-source-name :test #'equal)))

(defun efrit-documents-sources (&optional include-unavailable)
  "The registered sources that can work, or all with INCLUDE-UNAVAILABLE."
  (if include-unavailable
      efrit-documents--sources
    (seq-remove #'efrit-documents-source-unavailable efrit-documents--sources)))

(defun efrit-documents-source (name)
  "The source called NAME, or nil."
  (seq-find (lambda (s) (equal (efrit-documents-source-name s) name)) efrit-documents--sources))

(defun efrit-documents-resolve (url-or-ref)
  "The (SOURCE . ID) that URL-OR-REF names, or nil.
A ref is SOURCE:ID, as tool listings show; a URL is matched by each source."
  (or (and (string-match "\\`\\([a-z][a-z0-9-]*\\):\\([^/].*\\)\\'" url-or-ref)
           (let ((source (efrit-documents-source (match-string 1 url-or-ref))))
             (and source (cons source (match-string 2 url-or-ref)))))
      (catch 'found
        (dolist (source (efrit-documents-sources))
          (when-let* ((id (efrit-documents-source-match source url-or-ref)))
            (throw 'found (cons source id))))
        nil)))

(defun efrit-documents-ref (doc)
  "The SOURCE:ID reference of document plist DOC."
  (format "%s:%s" (plist-get doc :source) (plist-get doc :id)))

;;;; Cache

(defvar efrit-documents--cache (make-hash-table :test #'equal)
  "(SOURCE-NAME ID FORMAT) -> document plist, for this Emacs session.
A hit is used only when the source reports the same :modified; a
source that cannot tell (no metadata call) sets :modified nil and its
documents are fetched once per session.")

(defun efrit-documents-clear-cache ()
  "Forget every cached document."
  (interactive)
  (clrhash efrit-documents--cache)
  (message "efrit-documents: cache cleared"))

(defun efrit-documents--clip (text)
  "TEXT cut to `efrit-documents-max-chars' with a marker."
  (if (<= (length text) efrit-documents-max-chars)
      text
    (concat (substring text 0 efrit-documents-max-chars)
            (format "\n[... %d more characters not shown ...]"
                    (- (length text) efrit-documents-max-chars)))))

;;;; Fetching

(defun efrit-documents-fetch (url-or-ref &optional format)
  "The document URL-OR-REF names, fetched or from cache; a plist with :text.
FORMAT is `markdown' (default), `html' or `text'.  HTML is not clipped
\(a renderer wants the whole tree); the others are.  A cached copy is
used when the source's metadata reports the same :modified, or when
the source has no metadata call.  Signals `efrit-documents-not-found'
when no source claims URL-OR-REF."
  (pcase-let* ((format (or format 'markdown))
               (`(,source . ,id) (or (efrit-documents-resolve url-or-ref)
                                     (signal 'efrit-documents-not-found (list url-or-ref))))
               (key (list (efrit-documents-source-name source) id format))
               (cached (gethash key efrit-documents--cache)))
    (if (and cached
             (let ((meta (efrit-documents-source-metadata source id)))
               (or (null meta)
                   (equal (plist-get meta :modified) (plist-get cached :modified)))))
        cached
      (efrit-log 'debug "documents: fetch %s:%s as %s%s" (efrit-documents-source-name source) id format
                 (if cached " (cache stale)" ""))
      (let* ((doc (efrit-documents-source-fetch source id format))
             (doc (if (eq format 'html)
                      doc
                    (plist-put (copy-sequence doc) :text
                               (efrit-documents--clip (or (plist-get doc :text) ""))))))
        (puthash key doc efrit-documents--cache)
        doc))))

(defun efrit-documents-cached (url-or-ref &optional format)
  "The cached document for URL-OR-REF in FORMAT, or nil.  No request."
  (when-let* ((resolved (efrit-documents-resolve url-or-ref)))
    (gethash (list (efrit-documents-source-name (car resolved)) (cdr resolved) (or format 'markdown))
             efrit-documents--cache)))

(defun efrit-documents-as-text (doc)
  "DOC's title and text as one block for a model."
  (concat (format "Title: %s\n" (or (plist-get doc :title) (plist-get doc :id)))
          (when (plist-get doc :modified) (format "Modified: %s\n" (plist-get doc :modified)))
          (when (plist-get doc :url) (format "Source: %s\n" (plist-get doc :url)))
          "\n" (or (plist-get doc :text) "")))

(defun efrit-documents-expand-url (url)
  "The text of the document URL points to, for an analysis; nil when no source knows URL.
A fetch failure is a one-line note, so the analysis still runs."
  (when (efrit-documents-resolve url)
    (condition-case err
        (efrit-documents-as-text (efrit-documents-fetch url))
      (error (format "[document not fetched: %s]" (efrit-documents-explain err))))))

(defun efrit-documents-explain (err)
  "A readable one-line reason for ERR from a fetch or search."
  (pcase (car-safe err)
    ((or 'efrit-auth-http-error 'efrit-auth-no-credentials 'efrit-auth-error)
     (if (fboundp 'efrit-auth-explain) (funcall 'efrit-auth-explain err) (error-message-string err)))
    ('efrit-documents-not-found (format "no document source handles %s" (cadr err)))
    ('efrit-documents-error (if (stringp (cadr err)) (cadr err) (error-message-string err)))
    (_ (error-message-string err))))

;;;; Searching

(defvar efrit-documents-related-problems nil
  "Problems met by the last `efrit-documents-related' call: strings, newest last.
A provider or source that failed is named here with the reason, so a
caller that shows nothing can say why.")

(defun efrit-documents-search (query &optional source-name)
  "Documents matching QUERY across the working sources, or in SOURCE-NAME only.
Newest first.  A source that fails contributes nothing and a warning in
the log; the others still answer."
  (let ((sources (if source-name
                     (list (or (efrit-documents-source source-name)
                               (signal 'efrit-documents-error (list (format "no source %s" source-name)))))
                   (efrit-documents-sources)))
        (out nil))
    (dolist (source sources)
      (condition-case err
          (let ((found (efrit-documents-source-search source query)))
            (efrit-log 'debug "documents: %s search title=%S text=%S since=%s until=%s -> %d: %s"
                       (efrit-documents-source-name source)
                       (plist-get query :title) (plist-get query :text)
                       (efrit-documents--iso (plist-get query :since)) (efrit-documents--iso (plist-get query :until))
                       (length found)
                       (mapconcat (lambda (d) (format "%S" (plist-get d :title))) (seq-take found 5) ", "))
            (setq out (append out found)))
        (error
         (let ((why (format "%s: %s" (efrit-documents-source-name source) (efrit-documents-explain err))))
           (push why efrit-documents-related-problems)
           (efrit-log 'warn "documents: search failed: %s" why)))))
    (let ((words (plist-get query :title)))
      (sort out (lambda (a b)
                  ;; Most title words shared first (the sources match any
                  ;; one word), then newest.
                  (let ((oa (efrit-documents--overlap words (plist-get a :title)))
                        (ob (efrit-documents--overlap words (plist-get b :title))))
                    (if (= oa ob)
                        (string> (or (plist-get a :modified) "") (or (plist-get b :modified) ""))
                      (> oa ob))))))))

(defun efrit-documents--provider-name (fn)
  "A short name for provider FN: its feature-ish prefix, or the symbol."
  (if (symbolp fn)
      (let ((name (symbol-name fn)))
        (if (string-match "\\`efrit-documents-\\([a-z0-9]+\\)" name) (match-string 1 name) name))
    "provider"))

(defun efrit-documents--overlap (words title)
  "How many of WORDS appear among TITLE's words."
  (if (null words) 0
    (length (cl-intersection words (efrit-documents-title-words title) :test #'equal))))

(defun efrit-documents-title-words (title)
  "The words of TITLE worth searching for: no stopwords, no dates, no one-letter words."
  (let ((words (split-string (downcase (or title "")) "[^[:alnum:]]+" t)))
    (seq-remove (lambda (w)
                  (or (< (length w) 2)
                      (string-match-p "\\`[0-9]+\\'" w)
                      (member w efrit-documents-related-stopwords)))
                words)))

(defun efrit-documents--iso (value)
  "VALUE (a time or date string or nil) as an ISO string for the log."
  (if-let* ((time (efrit-documents--time value)))
      (format-time-string "%FT%TZ" time t)
    "-"))

(defun efrit-documents--time (value)
  "VALUE (an Emacs time, an ISO/RFC date string, or nil) as an Emacs time or nil."
  (cond ((null value) nil)
        ((stringp value) (ignore-errors (date-to-time value)))
        (t value)))

(defun efrit-documents-related (item)
  "Documents that belong with ITEM: a plist with :title, :date and optionally :urls.
Asks `efrit-documents-related-functions' (a calendar knows the
meeting's attachments by date and title); with
`efrit-documents-related-search' also searches each source for the
title's words within `efrit-documents-related-days' of :date.
Documents already among :urls, and duplicates, are left out.  Returns
document plists without :text, providers' answers first."
  (let* ((words (efrit-documents-title-words (plist-get item :title)))
         (date (efrit-documents--time (plist-get item :date)))
         (linked (delq nil (mapcar (lambda (u) (ignore-errors (efrit-documents-resolve u)))
                                   (plist-get item :urls))))
         (known (mapcar (lambda (l) (cons (efrit-documents-source-name (car l)) (cdr l))) linked))
         (out nil))
    (setq efrit-documents-related-problems nil)
    (efrit-log 'debug "documents: related for title=%S -> words=%S date=%s from=%S linked=%S providers=%S sources=%S"
               (plist-get item :title) words (efrit-documents--iso date) (plist-get item :from)
               known efrit-documents-related-functions
               (mapcar #'efrit-documents-source-name (efrit-documents-sources)))
    (cl-flet ((keep (docs)
                (dolist (doc docs)
                  (let ((key (cons (plist-get doc :source) (plist-get doc :id))))
                    (unless (member key known)
                      (push key known)
                      (push doc out))))))
      (dolist (fn efrit-documents-related-functions)
        (condition-case err
            (let ((docs (funcall fn item)))
              (efrit-log 'debug "documents: provider %s -> %d: %s" (efrit-documents--provider-name fn)
                         (length docs) (mapconcat (lambda (d) (format "%S" (plist-get d :title))) docs ", "))
              (keep docs))
          (error
           (let ((why (format "%s: %s" (efrit-documents--provider-name fn) (efrit-documents-explain err))))
             (push why efrit-documents-related-problems)
             (efrit-log 'warn "documents: related provider failed: %s" why)))))
      (when (and words efrit-documents-related-search)
        (let ((window (* efrit-documents-related-days 24 3600)))
          (keep (seq-take
                 (seq-filter (lambda (d) (> (efrit-documents--overlap words (plist-get d :title)) 0))
                             (efrit-documents-search
                              (list :title words
                                    :since (and date (time-subtract date window))
                                    :until (and date (time-add date window))
                                    :limit (* 4 efrit-documents-related-limit))))
                 efrit-documents-related-limit)))))
    (efrit-log 'debug "documents: related result %d: %s" (length out)
               (mapconcat (lambda (d) (format "%s:%s %S" (plist-get d :source) (plist-get d :id) (plist-get d :title)))
                          (reverse out) ", "))
    (nreverse out)))

(defun efrit-documents-related-text (item)
  "The related documents of ITEM fetched and rendered as blocks for a model, or nil."
  (efrit-documents-related-docs-text
   (condition-case err
       (efrit-documents-related item)
     (error (efrit-log 'warn "documents: related lookup failed: %s" (efrit-documents-explain err)) nil))))

(defun efrit-documents-related-docs-text (docs)
  "DOCS (document plists) fetched and rendered as blocks for a model, or nil."
  (let ((blocks nil))
    (dolist (doc docs)
      (condition-case err
          (push (format "--- Related document (%s): %s ---\n%s"
                        (plist-get doc :source) (or (plist-get doc :url) (efrit-documents-ref doc))
                        (efrit-documents-as-text (efrit-documents-fetch (efrit-documents-ref doc))))
                blocks)
        (error (push (format "--- Related document (%s): %s ---\n[not fetched: %s]"
                             (plist-get doc :source) (or (plist-get doc :url) (efrit-documents-ref doc))
                             (efrit-documents-explain err))
                     blocks))))
    (and blocks (string-join (nreverse blocks) "\n\n"))))

;;;; Tools for the model

(defun efrit-documents--listing (docs)
  "DOCS as lines a model can pick from."
  (mapconcat (lambda (d)
               (format "- %s | %s | %s | %s"
                       (efrit-documents-ref d) (or (plist-get d :modified) "?")
                       (or (plist-get d :kind) "doc") (or (plist-get d :title) "(untitled)")))
             docs "\n"))

(defun efrit-documents--tool-fetch (input)
  "doc_fetch: a document's text by URL or SOURCE:ID reference."
  (let ((key (alist-get "ref" input nil nil #'equal)))
    (condition-case err
        (efrit-documents-as-text (efrit-documents-fetch key))
      (error (format "Could not fetch %s: %s" key (efrit-documents-explain err))))))

(defun efrit-documents--tool-search (input)
  "doc_search: documents matching words, optionally within dates and one source."
  (let* ((text (alist-get "query" input nil nil #'equal))
         (source (alist-get "source" input nil nil #'equal))
         (limit (min 50 (or (alist-get "limit" input nil nil #'equal) 10)))
         (query (list :text (and text (split-string text "[[:space:]]+" t))
                      :since (alist-get "since" input nil nil #'equal)
                      :until (alist-get "until" input nil nil #'equal)
                      :limit limit))
         (docs (condition-case err
                   (efrit-documents-search query source)
                 (error (signal (car err) (cdr err))))))
    (if (null docs)
        (format "No documents match %S%s. Sources: %s." text
                (if source (format " in %s" source) "")
                (mapconcat #'efrit-documents-source-name (efrit-documents-sources) ", "))
      (concat (format "%d document%s. Use doc_fetch with the reference (SOURCE:ID) for the text.\n"
                      (length docs) (if (= 1 (length docs)) "" "s"))
              (efrit-documents--listing (seq-take docs limit))))))

(defun efrit-documents--tool-sources (_input)
  "doc_sources: what sources exist and whether they work."
  (if (null efrit-documents--sources)
      "No document sources are configured."
    (mapconcat (lambda (s)
                 (format "- %s: %s%s" (efrit-documents-source-name s)
                         (efrit-documents-source-description s)
                         (if-let* ((why (efrit-documents-source-unavailable s)))
                             (format " (unavailable: %s)" why) "")))
               efrit-documents--sources "\n")))

(defconst efrit-documents--tools
  `(("doc_fetch"
     "The text of a document the user can read, by URL (Google Docs, Confluence...) or by a SOURCE:ID reference from doc_search. Read-only. Long documents are cut with a marker."
     (("type" . "object")
      ("properties" . (("ref" . (("type" . "string") ("description" . "A document URL or a SOURCE:ID reference")))))
      ("required" . ["ref"]))
     ,#'efrit-documents--tool-fetch)
    ("doc_search"
     "Search the user's documents (Google Drive, Confluence... whatever sources are configured; doc_sources lists them) by words in the title or body, optionally within dates. Returns references, dates and titles. Read-only. Follow up with doc_fetch."
     (("type" . "object")
      ("properties" . (("query" . (("type" . "string") ("description" . "Words to match")))
                       ("source" . (("type" . "string") ("description" . "Only this source (name from doc_sources)")))
                       ("since" . (("type" . "string") ("description" . "Only documents modified on or after this ISO date")))
                       ("until" . (("type" . "string") ("description" . "Only documents modified on or before this ISO date")))
                       ("limit" . (("type" . "integer") ("description" . "Most results, default 10, max 50")))))
      ("required" . ["query"]))
     ,#'efrit-documents--tool-search)
    ("doc_sources"
     "The document sources configured for the user and whether each is working. Read-only."
     (("type" . "object") ("properties" . ()))
     ,#'efrit-documents--tool-sources))
  "The tools offered to efrit: (NAME DESCRIPTION INPUT-SCHEMA FUNCTION).")

;;;###autoload
(defun efrit-documents-ensure-tools ()
  "Register the document tools with efrit.  Idempotent."
  (interactive)
  (require 'efrit-tool-registry)
  (dolist (lib efrit-documents-sources-libraries)
    (condition-case err
        (require lib)
      (error (efrit-log 'warn "documents: cannot load source library %s: %s" lib (error-message-string err)))))
  (pcase-dolist (`(,name ,description ,schema ,fn) efrit-documents--tools)
    (efrit-register-tool name :description description :input-schema schema
                         :function fn :class 'read :package 'efrit-documents))
  (when (called-interactively-p 'any)
    (message "efrit-documents: %d tools registered" (length efrit-documents--tools))))

;;;###autoload
(defun efrit-documents-list-sources ()
  "Say which document sources are registered and whether they work."
  (interactive)
  (message "%s" (efrit-documents--tool-sources nil)))

(provide 'efrit-documents)

;;; efrit-documents.el ends here
