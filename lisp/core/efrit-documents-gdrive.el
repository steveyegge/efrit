;;; efrit-documents-gdrive.el --- Google Drive as a document source -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; Google Docs, Sheets and Slides (and files Drive can export as text)
;; for `efrit-documents'.  Setup is one auth-source entry named by
;; `efrit-documents-gdrive-auth-host' (default "gdrive"), or -- when
;; that is absent -- the first entry among `efrit-documents-gdrive-auth-hosts'
;; that carries a Drive scope, so the Gmail entry nngmail already uses
;; serves, with no second consent, once its :scope includes
;; https://www.googleapis.com/auth/drive.readonly.
;;
;;   machine gdrive login you@example.com
;;     client-id ... client-secret ...
;;     auth-url https://accounts.google.com/o/oauth2/auth
;;     token-url https://oauth2.googleapis.com/token
;;     scope "https://www.googleapis.com/auth/drive.readonly"
;;     redirect-uri http://localhost:8999
;;
;; Fetch: the metadata call gives the title and modifiedTime (so an
;; unchanged doc is served from cache), then Drive's export endpoint
;; gives Markdown (for the model), HTML (for a renderer) or plain text.
;; Search: files.list with a q= built from the query's words and
;; dates, over documents the account can read.  Calendar's "take
;; meeting notes" docs are ordinary Docs in Drive named after the
;; meeting, so `efrit-documents-related' finds them by title and date
;; with no Calendar API.
;;
;; The Cloud project that owns the OAuth client must have the Google
;; Drive API enabled (console: APIs & Services > Library > Google Drive
;; API) and the scope added on its consent screen; then re-consent once
;; with `efrit-auth-reauthorize'.  docs/DOCUMENTS.md walks through it.
;;
;; The source registers itself when this file loads.  Whether it works
;; is decided at first use (credentials are looked up then); `M-x
;; efrit-documents-gdrive-check' tells you now.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'efrit-documents)
(require 'efrit-auth)

(defgroup efrit-documents-gdrive nil
  "Google Drive as an efrit document source."
  :group 'efrit-documents
  :prefix "efrit-documents-gdrive-")

(defcustom efrit-documents-gdrive-auth-host "gdrive"
  "The auth-source host of the entry used for Drive."
  :type 'string)

(defcustom efrit-documents-gdrive-auth-hosts '("gmail" "gmail.com" "imap.gmail.com")
  "Hosts tried, in order, when `efrit-documents-gdrive-auth-host' has no entry.
An entry counts only if its :scope names a Drive scope.  Hosts from
`efrit-auth-host-functions' (a mail backend's Google entries) follow."
  :type '(repeat string))

(defcustom efrit-documents-gdrive-user nil
  "The auth-source user to look up, or nil for any."
  :type '(choice (const nil) string))

(defconst efrit-documents-gdrive--api "https://www.googleapis.com/drive/v3/files"
  "Drive v3 files endpoint.")

(defconst efrit-documents-gdrive--scope "https://www.googleapis.com/auth/drive.readonly"
  "The scope the token needs.")

(defconst efrit-documents-gdrive--url-regexp
  "https://docs\\.google\\.com/\\(document\\|spreadsheets\\|presentation\\)/\\(?:u/[0-9]+/\\)?d/\\([-_a-zA-Z0-9]+\\)\\|https://drive\\.google\\.com/\\(?:file/d/\\|open\\?id=\\)\\([-_a-zA-Z0-9]+\\)"
  "Matches Docs, Sheets, Slides and Drive file URLs; the id is group 2 or 3.")

(defconst efrit-documents-gdrive--kinds
  '(("application/vnd.google-apps.document" . "doc")
    ("application/vnd.google-apps.spreadsheet" . "sheet")
    ("application/vnd.google-apps.presentation" . "slides"))
  "Google MIME types and the short kind word shown in listings.")

(defconst efrit-documents-gdrive--exports
  '(("application/vnd.google-apps.document"
     (markdown . "text/markdown") (html . "text/html") (text . "text/plain"))
    ("application/vnd.google-apps.spreadsheet"
     (markdown . "text/csv") (html . "text/html") (text . "text/csv"))
    ("application/vnd.google-apps.presentation"
     (markdown . "text/plain") (html . "text/plain") (text . "text/plain")))
  "Export MIME type per Google type and requested format.")

(defclass efrit-document-source-gdrive (efrit-document-source)
  ((host :initarg :host :initform nil :accessor efrit-documents-gdrive-host
         :documentation "The auth-source host found to work, once looked up."))
  "Google Drive.")

;;;; Credentials

(defun efrit-documents-gdrive--scoped-p (entry)
  "Whether auth-source ENTRY's scope includes a Drive scope."
  (let ((scope (or (efrit-auth-get entry :scope) "")))
    (string-match-p "googleapis\\.com/auth/drive" scope)))

(defun efrit-documents-gdrive--find-host (source)
  "The auth-source host SOURCE uses, finding and remembering it on first call.
Signals `efrit-auth-no-credentials' naming what was tried."
  (or (efrit-documents-gdrive-host source)
      (let ((tried nil) (found nil))
        (dolist (host (efrit-auth-candidate-hosts efrit-documents-gdrive-auth-host efrit-documents-gdrive-auth-hosts))
          (unless found
            (push host tried)
            (condition-case nil
                (let ((entry (efrit-auth-credentials host efrit-documents-gdrive-user)))
                  (when (or (equal host efrit-documents-gdrive-auth-host)
                            (efrit-documents-gdrive--scoped-p entry))
                    (setq found host)))
              (efrit-auth-no-credentials nil))))
        (unless found
          (signal 'efrit-auth-no-credentials
                  (list (format "no auth-source entry for Drive: tried hosts %s; add one named %S with scope %s, or add that scope to your Gmail entry"
                                (string-join (nreverse tried) ", ")
                                efrit-documents-gdrive-auth-host efrit-documents-gdrive--scope))))
        (setf (efrit-documents-gdrive-host source) found))))

(defun efrit-documents-gdrive--request (source path &optional params raw)
  "GET PATH under the files endpoint (or a full URL) with PARAMS."
  (efrit-auth-request (efrit-documents-gdrive--find-host source) "GET"
                      (if (string-prefix-p "http" path) path
                        (concat efrit-documents-gdrive--api path))
                      :user efrit-documents-gdrive-user :params params :raw raw
                      :accept (if raw "*/*" "application/json")))

;;;; The protocol

(cl-defmethod efrit-documents-source-match ((_source efrit-document-source-gdrive) url)
  (when (and (stringp url) (string-match efrit-documents-gdrive--url-regexp url))
    (or (match-string 2 url) (match-string 3 url))))

(defun efrit-documents-gdrive--doc (file)
  "A document plist from Drive file metadata FILE (an alist)."
  (let ((mime (alist-get 'mimeType file)))
    (list :source "gdrive"
          :id (alist-get 'id file)
          :title (alist-get 'name file)
          :url (or (alist-get 'webViewLink file)
                   (format "https://docs.google.com/document/d/%s" (alist-get 'id file)))
          :modified (alist-get 'modifiedTime file)
          :kind (or (cdr (assoc mime efrit-documents-gdrive--kinds)) "file")
          :mime mime)))

(defconst efrit-documents-gdrive--fields "id,name,mimeType,modifiedTime,webViewLink")

(cl-defmethod efrit-documents-source-metadata ((source efrit-document-source-gdrive) id)
  (condition-case err
      (efrit-documents-gdrive--doc
       (efrit-documents-gdrive--request source (concat "/" id)
                                        `(("fields" . ,efrit-documents-gdrive--fields))))
    (efrit-auth-http-error
     (signal 'efrit-documents-error (list (efrit-auth-explain err))))))

(defun efrit-documents-gdrive--export-mime (mime format)
  "The export MIME type for a Google MIME type and FORMAT, or nil for a plain file."
  (cdr (assq format (cdr (assoc mime efrit-documents-gdrive--exports)))))

(defconst efrit-documents-gdrive--fallbacks
  '(("text/html" "text/markdown" "text/plain")
    ("text/markdown" "text/plain")
    ("text/csv" "text/plain"))
  "Export MIME types to try, in order, when the first is too large.
Drive refuses exports over 10 MB with \"This file is too large to be
exported\"; a Doc with embedded images exceeds that as HTML while its
text is a few kilobytes.  The text formats drop the images.")

(defun efrit-documents-gdrive--too-large-p (err)
  "Whether ERR is Drive's export size refusal."
  (and (eq (car-safe err) 'efrit-auth-http-error)
       (string-match-p "too large to be exported\\|exportSizeLimitExceeded" (or (nth 2 err) ""))))

(defun efrit-documents-gdrive--export (source id export)
  "Export doc ID as EXPORT, falling back to smaller formats when Drive refuses.
Returns (MIME . BYTES).  Signals `efrit-documents-error' when every
format is refused, naming the last reason."
  (let ((chain (or (assoc export efrit-documents-gdrive--fallbacks) (list export)))
        (result nil) (last-error nil))
    (while (and chain (null result))
      (let ((mime (pop chain)))
        (condition-case err
            (setq result (cons mime (efrit-documents-gdrive--request source (format "/%s/export" id)
                                                                     `(("mimeType" . ,mime)) t)))
          (efrit-auth-http-error
           (setq last-error err)
           (if (and chain (efrit-documents-gdrive--too-large-p err))
               (efrit-log 'info "documents: gdrive %s too large as %s; trying %s" id mime (car chain))
             (setq chain nil))))))
    (or result
        (signal 'efrit-documents-error
                (list (if (efrit-documents-gdrive--too-large-p last-error)
                          (format "Drive refuses to export %s in any text format (over its 10 MB export limit)" id)
                        (efrit-auth-explain last-error)))))))

(cl-defmethod efrit-documents-source-fetch ((source efrit-document-source-gdrive) id format)
  (let* ((doc (efrit-documents-source-metadata source id))
         (export (efrit-documents-gdrive--export-mime (plist-get doc :mime) format))
         (got (if export
                  (efrit-documents-gdrive--export source id export)
                ;; A plain file (text, markdown, csv uploaded to Drive).
                (condition-case err
                    (cons nil (efrit-documents-gdrive--request source (format "/%s" id) '(("alt" . "media")) t))
                  (efrit-auth-http-error
                   (signal 'efrit-documents-error (list (efrit-auth-explain err)))))))
         (mime (car got))
         (text (string-trim (replace-regexp-in-string
                             "\r\n" "\n" (decode-coding-string (or (cdr got) "") 'utf-8) t t))))
    (append doc (list :text text
                      :format (cond ((equal mime "text/html") 'html)
                                    ((equal mime "text/markdown") 'markdown)
                                    (t 'text))))))

(defun efrit-documents-gdrive--q (query)
  "Drive's q= expression for QUERY."
  (let ((parts (list "trashed = false"))
        (quote (lambda (w) (string-replace "'" "\\'" w))))
    ;; Any title word, not all: the document was named by someone else
    ;; ("Notes - Platform weekly" for a mail titled "Recap:
    ;; 2026-09-21 - Platform weekly").  `efrit-documents-search' ranks
    ;; the results by how many words they share.
    (when-let* ((words (plist-get query :title)))
      (push (concat "(" (mapconcat (lambda (w) (format "name contains '%s'" (funcall quote w)))
                                   words " or ")
                    ")")
            parts))
    (dolist (w (plist-get query :text))
      (push (format "fullText contains '%s'" (funcall quote w)) parts))
    ;; A document counts as in the window if it was modified in it, or
    ;; created in it (notes made at the meeting and never touched since).
    (let ((since (efrit-documents--time (plist-get query :since)))
          (until (efrit-documents--time (plist-get query :until))))
      (when (or since until)
        (let ((bounds (lambda (field)
                        (string-join
                         (delq nil (list (and since (format "%s >= '%s'" field (format-time-string "%FT%TZ" since t)))
                                         (and until (format "%s <= '%s'" field (format-time-string "%FT%TZ" until t)))))
                         " and "))))
          (push (format "((%s) or (%s))" (funcall bounds "modifiedTime") (funcall bounds "createdTime")) parts))))
    (push "(mimeType contains 'application/vnd.google-apps.document' or mimeType contains 'spreadsheet' or mimeType contains 'presentation' or mimeType contains 'text/')" parts)
    (string-join (nreverse parts) " and ")))

(cl-defmethod efrit-documents-source-search ((source efrit-document-source-gdrive) query)
  (condition-case err
      (let* ((q (efrit-documents-gdrive--q query))
             (_ (efrit-log 'debug "documents: gdrive q=%s" q))
             (result (efrit-documents-gdrive--request
                     source ""
                     `(("q" . ,q)
                       ("orderBy" . "modifiedTime desc")
                       ("pageSize" . ,(number-to-string (min 100 (or (plist-get query :limit) 10))))
                       ("fields" . ,(concat "files(" efrit-documents-gdrive--fields ")"))
                       ("supportsAllDrives" . "true")
                       ("includeItemsFromAllDrives" . "true")))))
        (mapcar #'efrit-documents-gdrive--doc (alist-get 'files result)))
    (efrit-auth-http-error
     (signal 'efrit-documents-error (list (efrit-auth-explain err))))))

;;;; Registration and checks

(defvar efrit-documents-gdrive--source nil)

(defun efrit-documents-gdrive-register ()
  "Register (or re-register) the Drive source."
  (setq efrit-documents-gdrive--source
        (efrit-documents-register
         (efrit-document-source-gdrive
          :name "gdrive"
          :description "Google Drive: Docs, Sheets, Slides and text files the account can read"))))

(efrit-documents-gdrive-register)

;;;###autoload
(defun efrit-documents-gdrive-check ()
  "Say which auth-source entry Drive would use and whether it answers."
  (interactive)
  (let ((source (or efrit-documents-gdrive--source (efrit-documents-gdrive-register))))
    (setf (efrit-documents-gdrive-host source) nil)
    (condition-case-unless-debug err
        (let* ((host (efrit-documents-gdrive--find-host source))
               (about (efrit-auth-request host "GET" "https://www.googleapis.com/drive/v3/about"
                                          :user efrit-documents-gdrive-user
                                          :params '(("fields" . "user(emailAddress)")))))
          (setf (efrit-documents-source-unavailable source) nil)
          (message "efrit-documents: Drive works through auth-source host %S as %s"
                   host (alist-get 'emailAddress (alist-get 'user about))))
      (error
       (setf (efrit-documents-source-unavailable source) (efrit-documents-explain err))
       (message "efrit-documents: Drive is not usable: %s" (efrit-documents-explain err))))))

(provide 'efrit-documents-gdrive)

;;; efrit-documents-gdrive.el ends here
