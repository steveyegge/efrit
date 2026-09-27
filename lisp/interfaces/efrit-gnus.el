;;; efrit-gnus.el --- Ask efrit about mail and news in Gnus -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, convenience, ai, mail, gnus

;;; Commentary:

;; Gnus shows the mail; efrit reads it for you.  This file does two
;; things, for any Gnus backend (nnimap, nnml, nntp, nngmail...):
;;
;; 1. Commands that hand efrit a set of articles and a question.  In a
;;    summary buffer, `efrit-gnus-analyze' takes the articles you mean
;;    (the active region's lines, else the process-marked ones, else
;;    the one at point), a prompt chosen by name from
;;    the prompt library or typed, and starts an efrit turn with the
;;    articles inlined as text.  `efrit-gnus-analyze-thread' takes the
;;    thread at point, `efrit-gnus-analyze-unread' every unread article
;;    of a group, `efrit-gnus-analyze-group' the newest N of a group,
;;    and `efrit-gnus-analyze-search' the results of a Gnus search
;;    (`gnus-search', so the query syntax is your server's).  The
;;    prompts are efrit-prompts' library (`e' in the chooser or
;;    `M-x efrit-prompts-manage' edits them).  The turn
;;    shows in the efrit buffer as one line ("analyze 12 articles from
;;    unread in INBOX: triage"); the model receives the prompt and the
;;    text.
;;
;; 2. Tools the model can call on its own in any turn, registered with
;;    `efrit-register-tool': `gnus_groups' (subscribed groups with
;;    unread counts), `gnus_search' (a `gnus-search' query on a
;;    server; returns a listing with article references), and
;;    `gnus_articles' (full text of articles by reference).  With these
;;    efrit can follow an analysis up -- "find her reply", "what else
;;    came from that list this week" -- without leaving the
;;    conversation.  They are read-only.  Nothing here marks, moves or
;;    edits an article.
;;
;; An article reference is GROUP#NUMBER, the full Gnus group name and
;; the article number in it; that is what the listings show and what
;; `gnus_articles' takes.
;;
;; Articles are rendered through Gnus's own fetch (`gnus-request-article')
;; and MIME layer: headers, the text/plain part, else text/html through
;; shr, attachment names and sizes.  Each article is capped at
;; `efrit-gnus-article-max-chars' and the whole payload at
;; `efrit-gnus-max-chars'; what does not fit is listed by header with a
;; note, so the model knows what it did not read.
;;
;; Reading an article in Gnus marks it read.  These commands do not go
;; through the summary's article display, so they leave marks alone.
;;
;; Documents: a link in the article that an efrit document source
;; (`efrit-documents': Google Drive, ...) can fetch is followed by the
;; document's text, and documents related to the article -- same title
;; words, near the same date, such as the notes Calendar took for the
;; meeting a recap describes -- follow that.  A backend can add its own
;; expander to `efrit-gnus-expand-link-functions' for links no source
;; handles.  The document tools (doc_fetch, doc_search) are registered
;; with the Gnus ones.  `gnus-treat-related-documents' (off by default)
;; is a washing treatment that appends the same related documents as a
;; footnote to the article you read, with URLs; an analysis of that
;; article then fetches them from the footnote instead of searching.
;;
;; Privacy: article text goes to the model efrit is configured for, as
;; anything you ask efrit does.  Nothing asks first; you chose the
;; articles and the prompt.  `efrit-gnus-confirm' can make it ask.
;;
;; Size: a selection larger than `efrit-gnus-max-chars' goes out as
;; consecutive turns.  Each prompt in the library has two
;; parts: one asked of every batch, one asked once at the end over the
;; whole selection.  By default (`efrit-gnus-isolate-batches') each
;; batch is a turn of its own: the conversation is taken back to where
;; the selection started before every batch, so the model reads one
;; batch at a time, and the closing turn gets the per-batch answers
;; stitched together instead of the articles.  `efrit-gnus-cancel'
;; stops the batches still queued.
;;
;; Keys: `efrit-gnus-map' is bound under `efrit-gnus-summary-prefix-key'
;; (default `C-c g') in every Gnus summary and group buffer, and the analyze
;; commands are on the Gnus summary menu.  Backends may bind the map
;; elsewhere too (nngmail puts it under its `L A').

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'gnus)
(require 'gnus-sum)
(require 'gnus-group)
(require 'gnus-int)
(require 'gnus-range)
(require 'gnus-art)
(require 'mm-decode)
(require 'mm-view)
(require 'nnheader)
(require 'message)
(require 'efrit-events)
(require 'efrit-prompts)
(require 'efrit-documents)

(declare-function efrit-register-tool "efrit-tool-registry")
(declare-function efrit-unregister-package-tools "efrit-tool-registry")
(declare-function efrit-submit "efrit-agent-input")
(declare-function efrit-agent-repl-session "efrit-agent-input")
(declare-function efrit-repl-session-history-mark "efrit-repl-session")
(declare-function efrit-repl-session-rewind "efrit-repl-session")
(declare-function efrit-repl-session-last-answer "efrit-repl-session")
(declare-function efrit-reload "efrit-reload")
(declare-function nngmail-reload "nngmail-reload")
(declare-function shr-insert-document "shr")
(declare-function libxml-parse-html-region "xml.c")
(declare-function gnus-search-server-to-engine "gnus-search")
(declare-function gnus-search-run-search "gnus-search")
(defvar shr-width) (defvar shr-use-fonts) (defvar shr-inhibit-images)
(defvar gnus-summary-article-menu)
(defvar gnus-treatment-function-alist)
(defvar gnus-summary-wash-map)
(defvar gnus-treat-condition)
(defvar gnus-article-current)

(defgroup efrit-gnus nil
  "Ask efrit about articles in Gnus."
  :group 'efrit
  :prefix "efrit-gnus-")

;;;; Prompts
;; The named prompts live in efrit-prompts (a library with an editor,
;; `M-x efrit-prompts-manage'); these are the built-in ones.  A
;; summarizing prompt has a per-batch part and an over-everything part;
;; a single prompt ("draft replies") has the per-batch part only.  Prompts may mention
;; the group, the count and the query with the placeholders {group},
;; {count} and {query}.

(efrit-prompts-define
 "summarize"
 "Summarize each of these messages in two or three lines: what it says, who wants what from whom, anything decided or due."
 "Now, across everything you were sent: what are the main threads and themes, what changed over time, what recurs, and what stands out as important or unresolved. One consolidated summary, longest section first.")
(efrit-prompts-define
 "action items"
 "List every action item, request or deadline in these messages, each with who asked, who must act, and by when. Say which are addressed to me. Omit anything already done."
 "Across all of them: one consolidated action list, deduplicated, grouped by owner, mine first, dated where dates exist. Flag items that appear more than once or seem overdue.")
(efrit-prompts-define
 "draft replies"
 "For each message that expects a reply from me, draft one in my voice: brief, plain, specific. Show the draft under the message's subject. Say which messages need no reply and why."
 nil "One task per message; nothing to consolidate." 'single)
(efrit-prompts-define
 "who and what"
 "Who are the people in these messages and what does each one want or report? One line per person per message."
 "Across all of them: one entry per person, merged: what they are working on, asking for, or blocked by over the whole period.")
(efrit-prompts-define
 "timeline"
 "Put the events, decisions and commitments in these messages on a dated timeline, oldest first, with the message each comes from."
 "Merge into one timeline over the whole selection, oldest first. Then name the turning points: where a plan changed, a decision was reversed, or a deadline moved.")
(efrit-prompts-define
 "triage"
 "Sort these messages into: needs my reply today, needs my reply this week, read only, ignore. One line each with the reason. Be decisive."
 "Across all of them: the short list of what I must do today, then this week. Nothing else.")
(efrit-prompts-define
 "extract"
 "Extract every concrete fact from these messages -- names, dates, amounts, links, identifiers, decisions -- as a table with the message it came from."
 "One merged table over everything, duplicates collapsed, sorted by date. Then note any facts that conflict between messages.")
(efrit-prompts-define
 "tone check"
 "Read these messages for tone: is anyone upset, blocked, or waiting on me? Quote the line that tells you and say what a good response would be."
 "Across all of them: who is unhappy or waiting, and for how long; whether it is getting better or worse over the period.")
(efrit-prompts-define
 "trends, accomplishments, concerns"
 "For each message, note in one line: the topics it covers, anything finished or decided, and anything worrying or blocked."
 "Now, over the whole selection, three sections. Trends: the recurring topics and how each moved over the period, with dates. Accomplishments: what got done or decided, by whom. Concerns: what is blocked, slipping, contested, or keeps coming back unresolved, and since when. Write it as a briefing someone who read none of these could act on; name people and dates.")

(defcustom efrit-gnus-default-prompt "summarize"
  "Name of the prompt offered first, when none was used yet this session."
  :type 'string)

(defcustom efrit-gnus-article-max-chars 12000
  "Characters kept of one article's body; the rest is cut and marked."
  :type 'integer)

(defcustom efrit-gnus-max-chars 150000
  "Characters of article text sent in one analysis, all articles together.
Roughly 40k tokens.  Articles past this are listed by header only, with
a note, so the model knows what it did not read."
  :type 'integer)

(defcustom efrit-gnus-tool-max-articles 25
  "Most articles the `gnus_articles' tool returns in full at once."
  :type 'integer)

(defcustom efrit-gnus-tool-max-chars 40000
  "Characters of article text one `gnus_articles' call returns, in total.
Separate from `efrit-gnus-max-chars', which sizes the batches an
analysis sends: a tool result stays in the conversation for every
later request, and the model tends to call the tool many times in one
turn (52 calls in a 115-article run on 2026-09-24, which is what
overran the context window).  Articles past this are listed by
reference for a second call."
  :type 'integer)

(defcustom efrit-gnus-search-limit 50
  "Most results an analysis or the `gnus_search' tool takes from a search."
  :type 'integer)

(defcustom efrit-gnus-confirm nil
  "Whether to confirm before sending article text to the model.
nil (the default) never asks: you chose the articles and the prompt.
`once' asks the first time in an Emacs session, `always' every time;
the prompt states how many articles and characters go out."
  :type '(choice (const nil) (const once) (const always)))

(defcustom efrit-gnus-html-renderer 'shr
  "How text/html bodies are turned into text: `shr', or nil to skip them."
  :type '(choice (const shr) (const nil)))

(defcustom efrit-gnus-headers '("From" "To" "Cc" "Newsgroups" "Date" "Subject" "Message-ID")
  "Headers included above each article body, in this order."
  :type '(repeat string))

(defcustom efrit-gnus-expand-link-functions nil
  "Functions that turn a URL found in an article into text for the model.
Each is called with one argument, the URL, and returns a string (the
linked document's text, already trimmed to a sensible size) or nil to
pass.  The first non-nil result is appended to the article under a
\"Linked document: URL\" heading.  Errors are caught: a failing
expander contributes a one-line note instead, so the analysis still
runs.  Backends add expanders for documents they can fetch; nngmail
adds one for Google Docs.  Only URLs matching
`efrit-gnus-expand-link-regexp' are offered."
  :type 'hook)

(defcustom efrit-gnus-expand-link-regexp "https?://[^][<>\"'()\\ \t\n]+"
  "URLs in an article body that are offered to `efrit-gnus-expand-link-functions'."
  :type 'regexp)

(defcustom efrit-gnus-expand-links-max 5
  "Most links expanded per article; the rest are listed as URLs only."
  :type 'integer)

(defcustom efrit-gnus-related-documents t
  "Whether an analysis also carries documents related to each article.
`efrit-gnus-related-documents-function' decides what is related.  nil
turns it off."
  :type 'boolean)

(defcustom efrit-gnus-related-documents-function #'efrit-gnus-related-documents-default
  "Function that finds the documents related to the article in the current buffer.
Called with no arguments in a buffer holding the article as delivered:
headers, a blank line, the decoded body.  Returns a list of
`efrit-documents' document plists (:source :id :title :url, no :text
needed) -- the notes taken at the meeting a recap describes, the page
a ticket points to.  The default asks `efrit-documents-related' with
the Subject, Date, From/To/Cc and the body's links, which the Calendar
provider answers by date and title.  Mail with a fixed subject format
\(a recap bot) deserves an exact rule in your configuration: parse the
Subject and call `efrit-documents-gcalendar-attachments' with the day
and an equality test."
  :type 'function)

(defcustom efrit-gnus-summary-prefix-key "C-c g"
  "Prefix under which `efrit-gnus-map' is bound in Gnus summary and group buffers.
nil installs no binding.  Set before this file loads.  Not `C-c e',
which efrit's own global map uses for `efrit-do'."
  :type '(choice (const nil) key-sequence))

(defvar efrit-gnus--confirmed nil
  "Non-nil once the user agreed to send article text this session.")

;;;; Loading efrit's own pieces

(defun efrit-gnus--require (feature)
  "Load efrit FEATURE, loading efrit itself first.
efrit's libraries live in subdirectories that efrit.el adds to
`load-path' when it loads; with a deferred `use-package' that has not
happened yet when a summary command runs."
  (unless (featurep 'efrit)
    (unless (locate-library "efrit")
      (user-error "efrit-gnus: efrit is not on your load-path"))
    (require 'efrit))
  (require feature))

;;;; Article references

(defun efrit-gnus--ref (group number)
  "The reference string for article NUMBER of GROUP."
  (format "%s#%d" group number))

(defun efrit-gnus--parse-ref (ref)
  "Parse REF (\"GROUP#NUMBER\") into (GROUP . NUMBER), or nil."
  (when (and (stringp ref) (string-match "\\`\\(.+\\)#\\([0-9]+\\)\\'" ref))
    (cons (match-string 1 ref) (string-to-number (match-string 2 ref)))))

;;;; Rendering an article as text

(defun efrit-gnus--decoded-part (handle)
  "The text of leaf HANDLE, transfer- and charset-decoded, as a multibyte string."
  (let* ((charset (or (mail-content-type-get (mm-handle-type handle) 'charset) "utf-8"))
         (coding (or (mm-charset-to-coding-system charset nil t) 'utf-8))
         (bytes (mm-get-part handle)))
    (if (multibyte-string-p bytes) bytes (decode-coding-string bytes coding))))

(defun efrit-gnus--part-text (handle)
  "Text of leaf MIME HANDLE, or nil if it is not text we can show."
  (let ((type (mm-handle-media-type handle)))
    (cond
     ((equal type "text/plain")
      (efrit-gnus--decoded-part handle))
     ((and (equal type "text/html") (eq efrit-gnus-html-renderer 'shr)
           (fboundp 'libxml-parse-html-region))
      (require 'shr)
      (with-temp-buffer
        (insert (efrit-gnus--decoded-part handle))
        (let ((dom (libxml-parse-html-region (point-min) (point-max))))
          (erase-buffer)
          (when dom
            (let ((shr-width 80) (shr-use-fonts nil) (shr-inhibit-images t))
              (shr-insert-document dom))))
        (buffer-substring-no-properties (point-min) (point-max))))
     ((equal type "text/html")
      ;; No libxml: strip tags rather than drop the body.
      (with-temp-buffer
        (insert (efrit-gnus--decoded-part handle))
        (goto-char (point-min))
        (while (re-search-forward "<[^>]+>" nil t) (replace-match " " t t))
        (replace-regexp-in-string "[ \t]+" " " (buffer-string) t t)))
     (t nil))))

(defun efrit-gnus--walk-parts (handle texts attachments)
  "Collect text parts and attachment names from HANDLE into TEXTS and ATTACHMENTS.
Both are cons cells whose car is a list, pushed onto in order.  For a
multipart/alternative only the best part is kept (plain over html)."
  (cond
   ((and (listp handle) (stringp (car handle)))
    (let ((type (car handle))
          (children (cdr handle)))
      (if (equal type "multipart/alternative")
          (let* ((leaves (seq-remove (lambda (h) (and (listp h) (stringp (car h)))) children))
                 (best (or (seq-find (lambda (h) (equal (mm-handle-media-type h) "text/plain")) leaves)
                           (seq-find (lambda (h) (equal (mm-handle-media-type h) "text/html")) leaves)
                           (car children))))
            (when best (efrit-gnus--walk-parts best texts attachments)))
        (dolist (child children)
          (efrit-gnus--walk-parts child texts attachments)))))
   ((and (listp handle) (bufferp (car handle)))
    (let* ((disposition (mm-handle-disposition handle))
           (filename (or (mail-content-type-get disposition 'filename)
                         (mail-content-type-get (mm-handle-type handle) 'name)))
           (text (and (not (equal (car disposition) "attachment"))
                      (efrit-gnus--part-text handle))))
      (if text
          (push text (car texts))
        (push (format "%s (%s, %s)"
                      (or filename "unnamed")
                      (mm-handle-media-type handle)
                      (file-size-human-readable
                       (with-current-buffer (mm-handle-buffer handle) (buffer-size))))
              (car attachments)))))))

(defun efrit-gnus--clip (text limit)
  "TEXT cut to LIMIT characters with a visible marker when cut."
  (if (<= (length text) limit)
      text
    (concat (substring text 0 limit)
            (format "\n[... %d more characters not shown ...]" (- (length text) limit)))))

(defun efrit-gnus--fetch-raw (group number)
  "The raw article NUMBER of GROUP as a unibyte string, or nil.
Through `gnus-request-article', so every backend works and no summary
mark changes."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (when (ignore-errors (gnus-request-article number group (current-buffer)))
      (buffer-string))))

(defun efrit-gnus--article-links (body)
  "Distinct URLs in BODY in order of first appearance.
Trailing punctuation is dropped."
  (let (out (start 0))
    (while (string-match efrit-gnus-expand-link-regexp body start)
      (let ((url (string-trim-right (match-string 0 body) "[.,;:!?)]+")))
        (unless (member url out) (push url out)))
      (setq start (match-end 0)))
    (nreverse out)))

(defun efrit-gnus--expand-link (url)
  "Text for URL: from a document source, else `efrit-gnus-expand-link-functions'.
An expander that signals contributes a note naming the error."
  (catch 'done
    (when-let* ((text (efrit-documents-expand-url url)))
      (throw 'done text))
    (dolist (fn efrit-gnus-expand-link-functions)
      (condition-case err
          (when-let* ((text (funcall fn url)))
            (throw 'done text))
        (error
         (throw 'done (format "[could not fetch: %s]" (error-message-string err))))))
    nil))

(defconst efrit-gnus-related-heading "Related documents:"
  "The line that opens a related-documents footnote in an article.
`gnus-article-show-related-documents' writes it; `efrit-gnus--render'
sees it in the body and expands the URLs under it instead of searching
again.")

(defun efrit-gnus--displayed-footnote (group number)
  "The related-documents footnote shown for article NUMBER of GROUP, or nil.
The article buffer keeps only the displayed article; when that is this
one and the washing treatment wrote a footnote, its text is returned so
the render can use it instead of searching again."
  (when (and (gnus-buffer-live-p gnus-article-buffer)
             (equal (car-safe (buffer-local-value 'gnus-article-current (get-buffer gnus-article-buffer))) group)
             (eql (cdr-safe (buffer-local-value 'gnus-article-current (get-buffer gnus-article-buffer))) number))
    (with-current-buffer gnus-article-buffer
      (when-let* ((start (text-property-any (point-min) (point-max) 'efrit-gnus-related t)))
        (let ((end (or (text-property-not-all start (point-max) 'efrit-gnus-related t) (point-max))))
          (buffer-substring-no-properties start end))))))

(defun efrit-gnus--article-item ()
  "The item plist `efrit-documents-related' takes, from the article in the current buffer."
  (save-excursion
    (save-restriction
      (widen)
      (let* ((header (lambda (name)
                       (save-restriction
                         (message-narrow-to-head)
                         (when-let* ((v (message-fetch-field name)))
                           (mail-decode-encoded-word-string v)))))
             (body (progn (goto-char (point-min))
                          (if (re-search-forward "^$" nil t)
                              (buffer-substring-no-properties (point) (point-max))
                            ""))))
        (list :title (funcall header "Subject")
              :date (funcall header "Date")
              :from (funcall header "From")
              :participants (delq nil (list (funcall header "To") (funcall header "Cc")))
              :urls (efrit-gnus--article-links body)
              :body body)))))

(defun efrit-gnus-related-documents-default ()
  "The default `efrit-gnus-related-documents-function': ask `efrit-documents-related'."
  (efrit-documents-related (efrit-gnus--article-item)))

(defun efrit-gnus--related-documents-in-buffer ()
  "Run `efrit-gnus-related-documents-function' on the current buffer.
Returns document plists; a failure is recorded in
`efrit-documents-related-problems' and returns nil."
  (condition-case err
      (funcall efrit-gnus-related-documents-function)
    (error
     (push (efrit-documents-explain err) efrit-documents-related-problems)
     (efrit-log 'warn "efrit-gnus: related-documents function failed: %s" (efrit-documents-explain err))
     nil)))

(defun efrit-gnus--related-documents (headers body &optional footnote)
  "Documents related to the article in the current buffer, as text, or nil.
HEADERS (an alist) and BODY are the rendered article; the current
buffer holds the article with headers and decoded body.  With FOOTNOTE
\(the related-documents footnote from the article buffer) or when BODY
carries one, no lookup runs: the footnote's URLs are expanded as links
instead and FOOTNOTE itself is returned so the model sees the list."
  (cond
   ((string-match-p (concat "^" (regexp-quote efrit-gnus-related-heading)) body)
    (efrit-log 'debug "efrit-gnus: %S carries a related-documents footnote in its body; no lookup"
               (cdr (assoc "Subject" headers)))
    nil)
   (footnote
    (efrit-log 'debug "efrit-gnus: %S: using the displayed footnote (%d chars); no lookup"
               (cdr (assoc "Subject" headers)) (length footnote))
    (when-let* ((linked (efrit-gnus--expand-links footnote)))
      (concat footnote "\n" linked)))
   ((and efrit-gnus-related-documents (efrit-documents-sources))
    (efrit-documents-related-docs-text (efrit-gnus--related-documents-in-buffer)))))

(defun efrit-gnus--expand-links (body)
  "Linked documents of BODY as text blocks to append, or nil.
At most `efrit-gnus-expand-links-max' links are fetched."
  (when (or efrit-gnus-expand-link-functions (efrit-documents-sources))
    (let ((n 0) blocks)
      (dolist (url (efrit-gnus--article-links body))
        (when (< n efrit-gnus-expand-links-max)
          (when-let* ((text (efrit-gnus--expand-link url)))
            (cl-incf n)
            (push (format "--- Linked document: %s ---\n%s" url text) blocks))))
      (and blocks (string-join (nreverse blocks) "\n\n")))))

(defun efrit-gnus--render (group number)
  "Article NUMBER of GROUP as plain text: headers, body, attachment list.
Documents linked from the body that a document source or an
`efrit-gnus-expand-link-functions' entry can fetch follow the body,
then documents related to it (see `efrit-gnus-related-documents').
Returns nil when the article cannot be fetched."
  (when-let* ((raw (efrit-gnus--fetch-raw group number)))
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert raw)
      (goto-char (point-min))
      (while (search-forward "\r\n" nil t) (replace-match "\n" t t))
      (let* ((headers
              (save-restriction
                (message-narrow-to-head)
                (delq nil
                      (mapcar (lambda (h)
                                (when-let* ((v (message-fetch-field h)))
                                  (cons h (replace-regexp-in-string
                                           "[\n\r\t]+" " " (mail-decode-encoded-word-string v) t t))))
                              efrit-gnus-headers))))
             (handles (mm-dissect-buffer t))
             (texts (list nil))
             (attachments (list nil)))
        (unwind-protect
            (progn
              (when handles (efrit-gnus--walk-parts handles texts attachments))
              (let* ((body (string-join (nreverse (car texts)) "\n\n"))
                     (body (replace-regexp-in-string "\n\\{3,\\}" "\n\n" (string-trim body) t t)))
                (concat
                 (mapconcat (lambda (h) (format "%s: %s" (car h) (cdr h))) headers "\n")
                 (format "\nArticle: %s" (efrit-gnus--ref group number))
                 (and (car attachments)
                      (format "\nAttachments: %s" (string-join (nreverse (car attachments)) "; ")))
                 "\n\n"
                 (if (string-empty-p body)
                     "[no text body]"
                   (efrit-gnus--clip body efrit-gnus-article-max-chars))
                 (when-let* ((linked (efrit-gnus--expand-links body)))
                   (concat "\n\n" linked))
                 (when-let* ((related
                              (efrit-gnus--with-article-text
                               raw body
                               (lambda ()
                                 (efrit-gnus--related-documents
                                  headers body (efrit-gnus--displayed-footnote group number))))))
                   (concat "\n\n" related)))))
          (when handles (mm-destroy-parts handles)))))))

(defun efrit-gnus--header-line (group number)
  "One line describing article NUMBER of GROUP for listings, from its head."
  (let ((head (with-temp-buffer
                (when (ignore-errors (gnus-request-head number group))
                  (with-current-buffer nntp-server-buffer
                    (goto-char (point-min))
                    (nnheader-parse-head t))))))
    (if head
        (format "- %s | %s | %s | %s"
                (efrit-gnus--ref group number)
                (mail-header-date head) (mail-header-from head) (mail-header-subject head))
      (format "- %s | (header unavailable)" (efrit-gnus--ref group number)))))

(defun efrit-gnus--progress (done total)
  "Show rendering progress DONE of TOTAL in the echo area, throttled."
  (when (and (> total 3) (or (= done total) (zerop (% done 5))))
    (message "efrit-gnus: rendering %d/%d" done total)))

(defun efrit-gnus--payload (refs &optional batch max-chars)
  "The articles REFS ((GROUP . NUMBER) ...) rendered, within MAX-CHARS.
MAX-CHARS defaults to `efrit-gnus-max-chars'.
Returns (TEXT SENT-COUNT TOTAL-CHARS REST): REST is the tail of REFS
that did not fit, in order, for the caller to send as the next batch;
nil when everything fit.  Articles are numbered within the whole
selection: BATCH is (OFFSET . TOTAL) when this is a later batch.  An
article larger than the whole budget goes out alone, cut by
`efrit-gnus-article-max-chars' as always."
  (let ((budget (or max-chars efrit-gnus-max-chars))
        (parts nil) (sent 0) (total 0) (rest nil)
        (i (or (car batch) 0))
        (n (or (cdr batch) (length refs)))
        (pending refs))
    (while (and pending (null rest))
      (pcase-let ((`(,group . ,number) (car pending)))
        (efrit-gnus--progress (1+ i) n)
        (let ((text (or (efrit-gnus--render group number)
                        (format "%s\n\n[article text unavailable]"
                                (efrit-gnus--header-line group number)))))
          (if (and (> (length text) budget) (> sent 0))
              ;; Does not fit after what is already in: next batch.
              (setq rest pending)
            (cl-incf i)
            (cl-decf budget (length text))
            (cl-incf total (length text))
            (cl-incf sent)
            (push (format "=== Article %d of %d ===\n%s" i n text) parts)
            (setq pending (cdr pending))))))
    (list (string-join (nreverse parts) "\n\n") sent total rest)))

;;;; Prompts and submission

(defvar efrit-gnus-query-history nil)

(defun efrit-gnus--read-prompt (&optional default purpose)
  "Ask for a prompt through `efrit-prompts-read'.
Returns (ITEM-PROMPT . SUMMARY-PROMPT); for typed text the summary is
nil and `efrit-gnus--prompt-pair' derives one.  DEFAULT names the
prompt offered first, PURPOSE heads the chooser."
  (efrit-prompts-read (or purpose "Ask efrit about the articles")
                      (or default efrit-prompts-last efrit-gnus-default-prompt)))

(defun efrit-gnus--prompt-pair (prompt)
  "PROMPT as (ITEM . SUMMARY).
A pair or plist passes through, a name is looked up; a single-task
prompt has a nil SUMMARY and no closing turn.  A typed question (a
string that names no prompt) gets a derived summary: the user asked
one thing of the whole selection, so it is asked once more over all
the answers."
  (let ((pair (efrit-prompts-pair prompt)))
    (if (and (null (cdr pair))
             (not (efrit-prompts-name-of (car pair))))
        (cons (car pair)
              (format "Now the same, over the whole selection as one: %s Consolidate; do not repeat the per-message answers."
                      (car pair)))
      pair)))

(defun efrit-gnus--fill (prompt group count query)
  "PROMPT with {group}, {count} and {query} filled in."
  (let ((s prompt))
    (setq s (replace-regexp-in-string "{group}" (or group "") s t t))
    (setq s (replace-regexp-in-string "{count}" (number-to-string count) s t t))
    (setq s (replace-regexp-in-string "{query}" (or query "") s t t))
    s))

(defun efrit-gnus--prompt-name (prompt)
  "The short name of PROMPT (an item prompt string) for the conversation line."
  (or (efrit-prompts-name-of prompt)
      (truncate-string-to-width prompt 40 nil nil "…")))

(defun efrit-gnus--confirm (count chars)
  "Ask, per `efrit-gnus-confirm', before sending COUNT articles of CHARS."
  (or (null efrit-gnus-confirm)
      (and (eq efrit-gnus-confirm 'once) efrit-gnus--confirmed)
      (when (y-or-n-p (format "Send %d article%s (%d characters) to efrit's model? "
                              count (if (= count 1) "" "s") chars))
        (setq efrit-gnus--confirmed t))))

(defcustom efrit-gnus-isolate-batches t
  "Whether each batch of a selection is analyzed in a turn of its own.
When non-nil, the model sees one batch at a time: before every batch
the conversation is taken back to where it stood when the selection
started, so no batch reads the ones before it.  The per-batch answers
are collected, and the closing turn receives them all, stitched, with
the summary prompt -- not the articles.  With 115 articles in five
batches this keeps every request near one batch in size, where the
accumulating history overran the model's context window (1.06M tokens
on 2026-09-24).  nil keeps every batch and its answer in the
conversation, so the closing summary can also see the articles."
  :type 'boolean)

(cl-defstruct (efrit-gnus-run (:constructor efrit-gnus-run--make))
  "One selection in flight: its batches, answers, and closing prompt."
  (queue nil)      ; batches waiting: (REFS PROMPT WHERE OFFSET TOTAL), oldest last
  (closing nil)    ; the summary prompt, sent after the last batch
  (mark nil)       ; history mark of the session when the run started
  (session nil)    ; the REPL session the run uses
  (answers nil)    ; (LABEL . ANSWER) per finished batch, newest first
  (batches 0)      ; batches sent so far
  (where nil)      ; how the selection is named in the conversation
  (total 0))       ; articles in the selection

(defvar efrit-gnus--run nil
  "The `efrit-gnus-run' in progress, or nil.
A selection larger than `efrit-gnus-max-chars' is sent as consecutive
turns; the first goes now, the rest as each turn ends, then the
closing summary.")

(defvar efrit-gnus--sending nil
  "Non-nil while a queued batch is being rendered and submitted.
Rendering runs the event loop (network fetches for linked documents),
so a second idle event can arrive meanwhile; this keeps it from
starting anything.")

(defun efrit-gnus--api-text (prompt text offset sent total &optional summarizing)
  "The message the model receives: PROMPT, guidance, the batch TEXT.
SUMMARIZING says a closing turn over everything will follow."
  (concat
   prompt
   (if (> total sent)
       (format "\n\nThis is part of a selection of %d articles, sent in batches because of size: articles %d-%d here%s. Answer for these now%s."
               total (1+ offset) (+ offset sent)
               (if efrit-gnus-isolate-batches
                   ", each batch in a conversation of its own"
                 ", the rest follow in later messages")
               (if summarizing
                   ", briefly per article; a final message will ask for the view over the whole selection"
                 ", per article"))
     (if summarizing
         "\n\nAnswer per article, briefly; a follow-up message will ask for the view over the whole selection."
       "\n\nAnswer per article."))
   "\n\nThe articles follow, oldest first, each with its reference (GROUP#NUMBER). "
   "Refer to them by subject and sender, not by number. "
   "If you need other articles, the rest of a conversation, or the full text of one "
   "that was cut, the gnus_search and gnus_articles tools give it to you.\n\n"
   text))

(defun efrit-gnus--closing-text (run prompt)
  "The closing message of RUN: PROMPT over the stitched per-batch answers.
With `efrit-gnus-isolate-batches' the model has not seen the batches
this turn, so their answers are inlined, oldest first."
  (let ((answers (reverse (efrit-gnus-run-answers run))))
    (if (and efrit-gnus-isolate-batches answers)
        (concat
         (format "That was the whole selection: %d articles from %s, analyzed in %d batch%s. The analysis of each batch follows; the articles themselves are not in this conversation (the gnus_search and gnus_articles tools have them if you need one).\n\n"
                 (efrit-gnus-run-total run) (or (efrit-gnus-run-where run) "the selection")
                 (length answers) (if (= 1 (length answers)) "" "es"))
         (mapconcat (lambda (a) (format "=== %s ===\n%s" (car a) (cdr a))) answers "\n\n")
         "\n\n" prompt)
      (concat "That was the whole selection. " prompt))))

(defun efrit-gnus--rewind-for-turn (run)
  "Take RUN's session back to the history mark, when batches are isolated."
  (when (and efrit-gnus-isolate-batches (efrit-gnus-run-session run) (efrit-gnus-run-mark run))
    (efrit-repl-session-rewind (efrit-gnus-run-session run) (efrit-gnus-run-mark run))))

(defun efrit-gnus--collect-answer (run)
  "Store the answer of the turn that just ended in RUN, labeled by batch."
  (when-let* ((session (efrit-gnus-run-session run))
              (answer (efrit-repl-session-last-answer session (efrit-gnus-run-mark run))))
    (push (cons (format "Batch %d" (efrit-gnus-run-batches run)) answer)
          (efrit-gnus-run-answers run))))

(defun efrit-gnus--submit-batch (run refs prompt where offset total)
  "Render and submit for RUN the batch starting REFS; queue the remainder.
OFFSET is how many of TOTAL were sent before.  Returns non-nil if the
turn started."
  (pcase-let* ((`(,text ,sent ,chars ,rest) (efrit-gnus--payload refs (cons offset total))))
    (unless (efrit-gnus--confirm sent chars)
      (user-error "efrit-gnus: not sent"))
    (let* ((last (+ offset sent))
           (shown (if (> total sent)
                      (format "analyze articles %d-%d of %d from %s: %s"
                              (1+ offset) last total (or where "the selection")
                              (efrit-gnus--prompt-name prompt))
                    (format "analyze %d article%s from %s: %s"
                            total (if (= 1 total) "" "s")
                            (or where "the selection") (efrit-gnus--prompt-name prompt))))
           (api (efrit-gnus--api-text prompt text offset sent total
                                      (and (efrit-gnus-run-closing run) t))))
      (when rest
        (push (list rest prompt where last total) (efrit-gnus-run-queue run)))
      (cl-incf (efrit-gnus-run-batches run))
      (efrit-gnus--rewind-for-turn run)
      (unless (efrit-submit shown api)
        (setq efrit-gnus--run nil)
        (user-error "efrit is busy with another turn; try again when it is idle"))
      t)))

(defun efrit-gnus-cancel ()
  "Stop sending the rest of the current selection and its closing summary.
The turn in flight finishes on its own."
  (interactive)
  (if efrit-gnus--run
      (efrit-gnus--stop-batches "cancelled")
    (message "efrit-gnus: nothing queued")))

(defun efrit-gnus--stop-batches (why)
  "Drop the run in progress, saying WHY."
  (setq efrit-gnus--run nil efrit-gnus--sending nil)
  (message "efrit-gnus: batch stopped: %s" why))

(defun efrit-gnus--send-next-batch ()
  "Send the oldest queued batch; the closing prompt when none is left."
  (setq efrit-gnus--sending t)
  (unwind-protect
      (condition-case err
          (when-let* ((run efrit-gnus--run))
            (efrit-gnus--collect-answer run)
            (if (efrit-gnus-run-queue run)
                (let ((batch (car (last (efrit-gnus-run-queue run)))))
                  (setf (efrit-gnus-run-queue run) (butlast (efrit-gnus-run-queue run)))
                  (pcase-let ((`(,refs ,prompt ,where ,offset ,total) batch))
                    (efrit-gnus--submit-batch run refs prompt where offset total)))
              (let ((prompt (efrit-gnus-run-closing run)))
                ;; The run is over once the closing turn starts: its
                ;; idle event must not send anything more.
                (setq efrit-gnus--run nil)
                (when prompt
                  (efrit-gnus--rewind-for-turn run)
                  (unless (efrit-submit "over the whole selection"
                                        (efrit-gnus--closing-text run prompt))
                    (user-error "efrit is busy; the closing summary was not sent"))))))
        (error (efrit-gnus--stop-batches (error-message-string err))))
    (setq efrit-gnus--sending nil)))

(defun efrit-gnus--on-status (event)
  "On the agent going idle, continue the selection: next batch, then the closing.
One handler decides in order, so the closing prompt can never overtake
a batch that is still rendering (two handlers racing did exactly that:
the summary went out after batch one and the second batch found the
session busy).  A failed turn ends the run: its answer is missing and
the next batch would start the whole chain over."
  (pcase (alist-get :status event)
    ('idle
     (when (and efrit-gnus--run (not efrit-gnus--sending))
       ;; Not from inside the event: let the turn finish tearing down.
       (run-at-time 0.1 nil #'efrit-gnus--send-next-batch)))
    ('failed
     (when efrit-gnus--run
       (efrit-gnus--stop-batches "the turn failed")))))

(defun efrit-gnus--watch-for-idle ()
  "Subscribe to the agent's status events.
`efrit-subscribe' adds a function once, so this is safe to call at load
and before every selection.  Subscribers from an earlier version of
this file are removed: a reload keeps `efrit-events--subscribers' and
the old function definitions, and two generations of handlers racing
sent the closing summary before the second batch (2026-09-22)."
  (dolist (old '(efrit-gnus--send-next efrit-gnus--send-closing))
    (efrit-unsubscribe 'status old))
  (efrit-subscribe 'status #'efrit-gnus--on-status))

(efrit-gnus--watch-for-idle)

(defun efrit-gnus--submit (refs prompt &optional where group query)
  "Render REFS and run PROMPT over them: per batch, then over everything.
PROMPT is (ITEM . SUMMARY), a prompt name, a plist from `efrit-prompts',
or a question as a string.
WHERE names the selection in the conversation line (\"unread in
INBOX\"); GROUP and QUERY fill the placeholders.  The articles go out
in as many turns as `efrit-gnus-max-chars' requires, each answered per
article; when the last turn ends, the SUMMARY prompt is sent for one
answer across the whole selection.  So 113 recaps become 113 short
takes and then one briefing on trends.  See `efrit-gnus-isolate-batches'
for what each turn can see."
  (efrit-gnus--require 'efrit-agent-input)
  (efrit-gnus-ensure-tools)
  (let* ((pair (efrit-gnus--prompt-pair prompt))
         (n (length refs))
         (item (efrit-gnus--fill (car pair) group n query))
         (summary (and (cdr pair) (efrit-gnus--fill (cdr pair) group n query)))
         (session (efrit-agent-repl-session))
         (run (efrit-gnus-run--make
               :closing summary :session session
               :mark (efrit-repl-session-history-mark session)
               :where where :total n)))
    (setq efrit-gnus--run run)
    (efrit-gnus--watch-for-idle)
    (efrit-gnus--submit-batch run refs item where 0 n)))

;;;; Washing: a related-documents footnote in the article buffer
;;
;; A Gnus article treatment (like buttonizing): under the control of
;; `gnus-treat-related-documents' it appends, below the article, a
;; footnote listing the documents `efrit-documents-related' finds for
;; the subject and date -- the notes Calendar took for the meeting a
;; recap describes, the Confluence page with the same title.  The
;; footnote is text with URLs, so Gnus buttonizes it for you to follow,
;; and when efrit analyzes the article it reads the same footnote and
;; fetches those documents as links, without searching again.

(defcustom gnus-treat-related-documents nil
  "Append a footnote of related documents (from efrit's sources) to the article.
Valid values are nil, t, a list of group-name regexps, an integer, a
`typep' form, or and/or/not of those; see Info node `(gnus)Customizing
Articles'.  Off by default: each article costs one search per document
source (results are not cached across articles).  A list such as
\='(\"recaps\") turns it on where the mail is a pointer to a meeting.
`W R' in the summary applies it to the current article regardless."
  :group 'gnus-article-treat
  :group 'efrit-gnus
  :link '(custom-manual "(gnus)Customizing Articles")
  :type gnus-article-treat-custom)

(defface efrit-gnus-related-heading
  '((t :inherit gnus-header-subject :weight bold))
  "Face of the heading of the related-documents footnote.")

(defun efrit-gnus--with-article-text (raw body function)
  "Call FUNCTION in a buffer holding RAW's headers, a blank line, and BODY.
This is the buffer `efrit-gnus-related-documents-function' is promised:
the article as delivered, with the MIME body already turned into text."
  (with-temp-buffer
    (insert raw)
    (goto-char (point-min))
    (while (search-forward "\r\n" nil t) (replace-match "\n" t t))
    (goto-char (point-min))
    (let ((head-end (progn (re-search-forward "^$" nil 'move) (point))))
      (delete-region head-end (point-max))
      (goto-char (point-max))
      (insert "\n" body))
    (goto-char (point-min))
    (funcall function)))

(defun efrit-gnus--with-original-article (function)
  "Call FUNCTION as `efrit-gnus--with-article-text' does, for the displayed article.
The original article buffer has the raw message; its MIME body is
decoded here."
  (when (gnus-buffer-live-p gnus-original-article-buffer)
    (let* ((raw (with-current-buffer gnus-original-article-buffer
                  (buffer-substring-no-properties (point-min) (point-max))))
           (body (with-temp-buffer
                   (insert raw)
                   (let ((handles (mm-dissect-buffer t))
                         (texts (list nil)) (attachments (list nil)))
                     (unwind-protect
                         (progn (when handles (efrit-gnus--walk-parts handles texts attachments))
                                (string-join (nreverse (car texts)) "\n\n"))
                       (when handles (mm-destroy-parts handles)))))))
      (efrit-gnus--with-article-text raw body function))))

(defun efrit-gnus--article-header (name)
  "Header NAME of the article being washed, from the original article buffer."
  (when (gnus-buffer-live-p gnus-original-article-buffer)
    (with-current-buffer gnus-original-article-buffer
      (save-restriction
        (article-narrow-to-head)
        (message-fetch-field name)))))

(defun efrit-gnus--related-footnote (docs)
  "The footnote text for DOCS: a heading and one line per document."
  (concat efrit-gnus-related-heading "\n"
          (mapconcat (lambda (d)
                       (format "- %s (%s%s) %s"
                               (or (plist-get d :title) (plist-get d :id))
                               (plist-get d :source)
                               (if-let* ((m (plist-get d :modified)))
                                   (concat ", " (substring m 0 (min 10 (length m))))
                                 "")
                               (or (plist-get d :url) (efrit-documents-ref d))))
                     docs "\n")
          "\n"))

(defun gnus-article-show-related-documents ()
  "Append a footnote listing documents related to this article.
A Gnus washing function (`gnus-treat-related-documents'); also
\\<gnus-summary-mode-map>\\[gnus-article-show-related-documents] in the
summary.  Related means: found in an efrit document source by the
subject's words within a few days of the Date.  Idempotent: an article
with the footnote is left alone.  Nothing is shown when nothing is
found; a failing source is a one-line note."
  (interactive nil gnus-article-mode gnus-summary-mode)
  (efrit-documents-ensure-tools)
  (gnus-with-article-buffer
    (save-restriction
      (unless (bound-and-true-p gnus-treat-condition)
        (article-goto-body)
        (narrow-to-region (point) (point-max)))
      (unless (text-property-any (point-min) (point-max) 'efrit-gnus-related t)
        (let* ((title (efrit-gnus--article-header "Subject"))
               (docs (and (efrit-documents-sources)
                          (progn (setq efrit-documents-related-problems nil)
                                 (efrit-gnus--with-original-article
                                  #'efrit-gnus--related-documents-in-buffer))))
               (interactive-p (not (bound-and-true-p gnus-treat-condition))))
          (efrit-log 'debug "efrit-gnus: related footnote for %S (%s): %d document(s)%s"
                     title (if interactive-p "by hand" "treatment") (length docs)
                     (if efrit-documents-related-problems
                         (format "; problems: %s" (string-join (reverse efrit-documents-related-problems) "; "))
                       ""))
          (cond
           (docs
            (let ((inhibit-read-only t) (start nil))
              (goto-char (point-max))
              (unless (bolp) (insert "\n"))
              (insert "\n")
              (setq start (point))
              (insert (efrit-gnus--related-footnote docs))
              (add-text-properties start (point) '(efrit-gnus-related t))
              (put-text-property start (+ start (length efrit-gnus-related-heading))
                                 'face 'efrit-gnus-related-heading)
              ;; Make the URLs followable, as the buttonize pass would
              ;; have had they been in the original text.
              (save-restriction
                (narrow-to-region start (point))
                (gnus-article-add-buttons))
              (when (and interactive-p efrit-documents-related-problems)
                (message "efrit-gnus: %d related document(s); also: %s" (length docs)
                         (string-join (reverse efrit-documents-related-problems) "; ")))))
           ;; Nothing to show.  As a treatment stay quiet; by hand, say why.
           (interactive-p
            (message "efrit-gnus: no related documents for %S%s"
                     (string-join (efrit-documents-title-words title) " ")
                     (cond
                      ((null (efrit-documents-sources)) " (no document sources are working; M-x efrit-documents-list-sources)")
                      (efrit-documents-related-problems
                       (concat " (" (string-join (reverse efrit-documents-related-problems) "; ") ")"))
                      (t ""))))))))))

(defun efrit-gnus--install-treatment ()
  "Put the footnote treatment after buttonizing in `gnus-treatment-function-alist'."
  (unless (assq 'gnus-treat-related-documents gnus-treatment-function-alist)
    (let ((cell (memq (assq 'gnus-treat-buttonize gnus-treatment-function-alist)
                      gnus-treatment-function-alist)))
      (if cell
          (setcdr cell (cons '(gnus-treat-related-documents gnus-article-show-related-documents) (cdr cell)))
        (setq gnus-treatment-function-alist
              (append gnus-treatment-function-alist
                      '((gnus-treat-related-documents gnus-article-show-related-documents))))))))

(efrit-gnus--install-treatment)

;;;; Choosing articles

(defun efrit-gnus--summary-refs ()
  "References of the articles the user means in this summary, oldest first.
The active region's lines, else the process-marked articles, else the
article at point."
  (unless (derived-mode-p 'gnus-summary-mode)
    (user-error "efrit-gnus: not in a summary buffer"))
  (let ((numbers (if (use-region-p)
                     (save-excursion
                       (let ((end (region-end)) out)
                         (goto-char (region-beginning))
                         (while (< (point) end)
                           (when-let* ((a (gnus-summary-article-number)))
                             (push a out))
                           (forward-line 1))
                         (nreverse out)))
                   (gnus-summary-work-articles nil))))
    (when (null numbers) (user-error "efrit-gnus: no article here"))
    (mapcar (lambda (n) (cons gnus-newsgroup-name n)) (sort (copy-sequence numbers) #'<))))

(defun efrit-gnus--group-for-command ()
  "The group a command applies to: the summary's, the group line's, or asked."
  (or (and (derived-mode-p 'gnus-summary-mode) gnus-newsgroup-name)
      (and (derived-mode-p 'gnus-group-mode) (gnus-group-group-name))
      (gnus-group-completing-read "Group: ")))

(defun efrit-gnus--group-article-numbers (group)
  "All article numbers of GROUP, ascending, from its active range less `unexist'."
  (let ((active (or (gnus-active group) (gnus-activate-group group))))
    (unless active (user-error "efrit-gnus: cannot activate %s" group))
    (range-list-difference
     (range-uncompress active)
     (cdr (assq 'unexist (gnus-info-marks (gnus-get-info group)))))))

;;;; Commands

;;;###autoload
(defun efrit-gnus-analyze (prompt)
  "Ask efrit PROMPT about the selected articles in this summary.
The selection is the active region's articles, else the process-marked
ones, else the article at point.  Interactively PROMPT is picked from
the prompt library or typed."
  (interactive (list (efrit-gnus--read-prompt
                      nil (format "%d selected article(s)" (length (efrit-gnus--summary-refs))))))
  (let ((refs (efrit-gnus--summary-refs)))
    (efrit-gnus--submit refs prompt (format "group %s" (gnus-group-real-name gnus-newsgroup-name))
                        (gnus-group-real-name gnus-newsgroup-name))))

;;;###autoload
(defun efrit-gnus-analyze-thread (prompt)
  "Ask efrit PROMPT about the thread of the article at point, as shown in this summary."
  (interactive (list (efrit-gnus--read-prompt nil "This thread")))
  (unless (derived-mode-p 'gnus-summary-mode)
    (user-error "efrit-gnus: not in a summary buffer"))
  (let ((numbers (gnus-summary-articles-in-thread)))
    (when (null numbers) (user-error "efrit-gnus: no thread here"))
    (efrit-gnus--submit (mapcar (lambda (n) (cons gnus-newsgroup-name n)) (sort numbers #'<))
                        prompt "this thread" (gnus-group-real-name gnus-newsgroup-name))))

;;;###autoload
(defun efrit-gnus-analyze-unread (group prompt)
  "Ask efrit PROMPT about every unread article of GROUP.
Interactively GROUP is the current one in a summary or on a group line.
Nothing is marked read by this.  Defaults to the `triage' prompt."
  (interactive (let ((g (efrit-gnus--group-for-command)))
                 (list g (efrit-gnus--read-prompt
                          "triage" (format "%d unread in %s" (length (gnus-list-of-unread-articles g))
                                           (gnus-group-real-name g))))))
  (let ((numbers (gnus-list-of-unread-articles group)))
    (when (null numbers) (user-error "efrit-gnus: nothing unread in %s" (gnus-group-real-name group)))
    (efrit-gnus--submit (mapcar (lambda (n) (cons group n)) numbers)
                        prompt (format "unread in %s" (gnus-group-real-name group))
                        (gnus-group-real-name group))))

;;;###autoload
(defun efrit-gnus-analyze-group (group count prompt)
  "Ask efrit PROMPT about the newest COUNT articles of GROUP.
Interactively COUNT is the prefix argument or is asked for."
  (interactive
   (let* ((g (efrit-gnus--group-for-command))
          (n (if current-prefix-arg (prefix-numeric-value current-prefix-arg)
               (read-number (format "Newest how many of %s? " (gnus-group-real-name g)) 20))))
     (list g n (efrit-gnus--read-prompt nil (format "Newest %d of %s" n (gnus-group-real-name g))))))
  (let* ((numbers (efrit-gnus--group-article-numbers group))
         (newest (last numbers count)))
    (when (null newest) (user-error "efrit-gnus: %s is empty" (gnus-group-real-name group)))
    (efrit-gnus--submit (mapcar (lambda (n) (cons group n)) newest)
                        prompt (format "newest %d of %s" (length newest) (gnus-group-real-name group))
                        (gnus-group-real-name group))))

;;;###autoload
(defun efrit-gnus-analyze-search (server query prompt)
  "Ask efrit PROMPT about the results of a Gnus search for QUERY on SERVER.
QUERY is in the syntax of SERVER's search engine (`gnus-search'); it is
passed raw.  At most `efrit-gnus-search-limit' results are used."
  (interactive
   (list (gnus-completing-read "Server" (mapcar #'car gnus-server-alist) t)
         (read-string "Search query: " nil 'efrit-gnus-query-history)
         (efrit-gnus--read-prompt nil "Search results")))
  (let ((refs (efrit-gnus--search server query efrit-gnus-search-limit)))
    (when (null refs) (user-error "efrit-gnus: no results for %S" query))
    (efrit-gnus--submit refs prompt (format "search %S on %s" query server) nil query)))

(defun efrit-gnus--search (server query limit)
  "Run QUERY on SERVER through `gnus-search'; return up to LIMIT (GROUP . NUMBER)."
  (require 'gnus-search)
  (let* ((engine (or (gnus-search-server-to-engine server)
                     (user-error "efrit-gnus: no search engine for server %s" server)))
         (results (gnus-search-run-search engine server `((query . ,query) (raw . t)) nil))
         (refs nil))
    (dotimes (i (length results))
      (let ((r (aref results i)))
        ;; A result vector is [GROUP NUMBER SCORE]; the group comes back
        ;; as the backend's short name or a full one.
        (let ((group (aref r 0)) (number (aref r 1)))
          (push (cons (if (gnus-group-prefixed-p group) group
                        (gnus-group-full-name group (gnus-server-to-method server)))
                      number)
                refs))))
    (seq-take (nreverse refs) limit)))

;;;; Tools for the model

(defun efrit-gnus--tool-groups (_input)
  "gnus_groups: subscribed groups with their unread counts."
  (let (rows)
    (dolist (info (cdr gnus-newsrc-alist))
      (when (<= (gnus-info-level info) gnus-level-subscribed)
        (let* ((group (gnus-info-group info))
               (unread (gnus-group-unread group)))
          (push (format "- %s: %s unread" group (if (numberp unread) unread "?")) rows))))
    (if rows
        (concat "Subscribed Gnus groups (full names, use them in gnus_search and article references):\n"
                (string-join (nreverse rows) "\n"))
      "No subscribed groups (is Gnus running?).")))

(defun efrit-gnus--tool-search (input)
  "gnus_search: a search on a Gnus server; a listing with references."
  (let* ((server (or (alist-get "server" input nil nil #'equal)
                     (car (car gnus-server-alist))))
         (query (alist-get "query" input nil nil #'equal))
         (limit (min 100 (or (alist-get "limit" input nil nil #'equal) 20)))
         (refs (efrit-gnus--search server query limit)))
    (if (null refs)
        (format "No articles match %S on %s." query server)
      (concat (format "%d article%s for %S on %s. Use gnus_articles with the references for the text.\n"
                      (length refs) (if (= 1 (length refs)) "" "s") query server)
              (mapconcat (lambda (r) (efrit-gnus--header-line (car r) (cdr r))) refs "\n")))))

(defun efrit-gnus--tool-articles (input)
  "gnus_articles: full text of the articles with the given references."
  (let* ((raw (alist-get "refs" input nil nil #'equal))
         (raw (if (vectorp raw) (append raw nil) raw))
         (refs (delq nil (mapcar #'efrit-gnus--parse-ref (seq-take raw efrit-gnus-tool-max-articles)))))
    (if (null refs)
        "No valid references; a reference is GROUP#NUMBER as shown by gnus_search."
      (pcase-let ((`(,text ,_sent ,_chars ,rest)
                   (efrit-gnus--payload refs nil efrit-gnus-tool-max-chars)))
        (if rest
            (concat text (format "\n\n[%d more article%s not included, over the size limit; ask for them in a second call: %s]"
                                 (length rest) (if (= 1 (length rest)) "" "s")
                                 (mapconcat (lambda (r) (efrit-gnus--ref (car r) (cdr r))) rest ", ")))
          text)))))

(defconst efrit-gnus--tools
  `(("gnus_groups"
     "The user's subscribed Gnus mail and news groups with unread counts. Read-only. Group names from here go into gnus_search and article references."
     (("type" . "object") ("properties" . ()))
     ,#'efrit-gnus--tool-groups)
    ("gnus_search"
     "Search the user's mail or news through Gnus on a server (IMAP SEARCH, Gmail, notmuch... whatever the server's engine is; the query is passed raw). Returns a listing with article references (GROUP#NUMBER), dates, senders and subjects. Read-only. Follow up with gnus_articles for the text."
     (("type" . "object")
      ("properties" . (("query" . (("type" . "string") ("description" . "Search query in the server's syntax")))
                       ("server" . (("type" . "string") ("description" . "Gnus server name; default the first configured")))
                       ("limit" . (("type" . "integer") ("description" . "Most results, default 20, max 100")))))
      ("required" . ["query"]))
     ,#'efrit-gnus--tool-search)
    ("gnus_articles"
     "Full text of Gnus articles by reference (GROUP#NUMBER, from gnus_search or from the conversation): headers, body as text, attachment names. Read-only; does not mark anything read. Long bodies are cut with a marker."
     (("type" . "object")
      ("properties" . (("refs" . (("type" . "array") ("items" . (("type" . "string"))) ("description" . "Article references GROUP#NUMBER")))))
      ("required" . ["refs"]))
     ,#'efrit-gnus--tool-articles))
  "The tools offered to efrit: (NAME DESCRIPTION INPUT-SCHEMA FUNCTION).")

;;;###autoload
(defun efrit-gnus-ensure-tools ()
  "Register the Gnus tools with efrit.  Idempotent; the commands call it."
  (interactive)
  (efrit-gnus--require 'efrit-tool-registry)
  (pcase-dolist (`(,name ,description ,schema ,fn) efrit-gnus--tools)
    (efrit-register-tool name :description description :input-schema schema
                         :function fn :class 'read :package 'efrit-gnus))
  (efrit-documents-ensure-tools)
  (when (called-interactively-p 'any)
    (message "efrit-gnus: %d tools registered" (length efrit-gnus--tools))))

(defun efrit-gnus-remove-tools ()
  "Withdraw the Gnus tools from efrit."
  (interactive)
  (when (featurep 'efrit-tool-registry)
    (efrit-unregister-package-tools 'efrit-gnus)))

;;;; Reloading

;;;###autoload
(defun efrit-gnus-reload (&optional verbose)
  "Reload efrit from source, then any nngmail, then re-register the tools.
For working on efrit and a Gnus backend at once.  With VERBOSE list
each file as it loads."
  (interactive "P")
  (let ((t0 (float-time)) (efrit-count 0) (other-count 0))
    (efrit-gnus--require 'efrit-reload)
    (setq efrit-count (or (efrit-reload verbose) 0))
    (when (and (featurep 'nngmail) (locate-library "nngmail-reload"))
      (require 'nngmail-reload)
      (setq other-count (or (funcall 'nngmail-reload verbose) 0)))
    (funcall 'efrit-gnus-ensure-tools)
    (funcall 'efrit-gnus--install-keys)
    (message "efrit-gnus: reloaded %d efrit and %d nngmail libraries in %.1fs; %d tools registered"
             efrit-count other-count (- (float-time) t0) (length efrit-gnus--tools))))

;;;; Keys and menu

(defvar efrit-gnus-map
  (let ((map (make-sparse-keymap)))
    (define-key map "a" #'efrit-gnus-analyze)
    (define-key map "t" #'efrit-gnus-analyze-thread)
    (define-key map "u" #'efrit-gnus-analyze-unread)
    (define-key map "g" #'efrit-gnus-analyze-group)
    (define-key map "s" #'efrit-gnus-analyze-search)
    (define-key map "e" #'efrit-prompts-manage)
    (define-key map "k" #'efrit-gnus-cancel)
    map)
  "Keymap of the efrit commands for Gnus.")

(defun efrit-gnus--install-keys ()
  "Bind `efrit-gnus-map' under `efrit-gnus-summary-prefix-key' in Gnus summaries.
Called at load and again by the reloaders (the map is a `defvar' that a
reload rebuilds)."
  (when efrit-gnus-summary-prefix-key
    (define-key gnus-summary-mode-map (kbd efrit-gnus-summary-prefix-key) efrit-gnus-map)
    (define-key gnus-group-mode-map (kbd efrit-gnus-summary-prefix-key) efrit-gnus-map))
  (define-key gnus-summary-wash-map "R" #'gnus-article-show-related-documents))

(efrit-gnus--install-keys)

(defconst efrit-gnus--menu
  '("Ask efrit"
    ["About the selection..." efrit-gnus-analyze t]
    ["About this thread..." efrit-gnus-analyze-thread t]
    ["About all unread here..." efrit-gnus-analyze-unread t]
    ["About the newest N here..." efrit-gnus-analyze-group t]
    ["About a search..." efrit-gnus-analyze-search t]
    "---"
    ["Show related documents (footnote)" gnus-article-show-related-documents t]
    ["Edit the prompts..." efrit-prompts-manage t]
    ["Stop the batches still queued" efrit-gnus-cancel t])
  "The submenu added to Gnus's Article menu.")

(defun efrit-gnus--install-menu ()
  "Add the \"Ask efrit\" submenu to the summary's Article menu.
Gnus builds that menu lazily, in `gnus-summary-make-menu-bar' when the
first summary opens; adding to the mode map before then made a second
\"Article\" menu.  So this runs from `gnus-summary-menu-hook', which
Gnus calls once the menus exist."
  (when (boundp 'gnus-summary-article-menu)
    (easy-menu-add-item gnus-summary-article-menu nil efrit-gnus--menu)))

(add-hook 'gnus-summary-menu-hook #'efrit-gnus--install-menu)
;; A summary already open (this file loaded late, or reloaded) has its
;; menu built; add to it now.
(efrit-gnus--install-menu)

(provide 'efrit-gnus)

;;; efrit-gnus.el ends here
