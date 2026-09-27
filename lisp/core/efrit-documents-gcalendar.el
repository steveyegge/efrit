;;; efrit-documents-gcalendar.el --- Meeting attachments from Google Calendar -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; A related-documents provider for `efrit-documents': given an item
;; with a title and a date (a meeting recap's subject and Date), find
;; the calendar event it is about and return the event's attachments.
;; Calendar's "take meeting notes" Doc is such an attachment.
;;
;; Two layers:
;;
;;   `efrit-documents-gcalendar-attachments' DAY PREDICATE is the
;;   primitive: list every event of DAY on
;;   `efrit-documents-gcalendar-calendars', keep those whose title
;;   satisfies PREDICATE, return their Drive attachments as documents
;;   of the Drive source.  No scoring, no nearest-in-time.
;;
;;   `efrit-documents-gcalendar-related' is the default provider on
;;   `efrit-documents-related-functions': the day is a YYYY-MM-DD in the
;;   item's title, else its :date; an event matches when the item's
;;   title contains the event's title whole.  A mail source whose
;;   subjects follow a fixed format (a recap bot) should not rely on
;;   that guess: give efrit-gnus its own
;;   `efrit-gnus-related-documents-function' that parses the subject
;;   and calls the primitive with the exact day and an equality test.
;;
;; Credentials: an auth-source entry named
;; `efrit-documents-gcalendar-auth-host' (default "gcalendar"), else the
;; Drive/Gmail entry when its :scope carries a Calendar scope.  Add
;; https://www.googleapis.com/auth/calendar.events.readonly (or
;; calendar.readonly) to the scope, enable the Google Calendar API for
;; the Cloud project that owns the OAuth client (console: APIs &
;; Services > Library > Google Calendar API) and add the scope on the
;; consent screen, then re-consent once (`efrit-auth-reauthorize').
;; `M-x efrit-documents-gcalendar-check' lists the calendars the token
;; can read.  docs/DOCUMENTS.md has the console walk-through.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'efrit-documents)
(require 'efrit-documents-gdrive)
(require 'efrit-auth)

(defgroup efrit-documents-gcalendar nil
  "Google Calendar as a provider of documents related to a meeting."
  :group 'efrit-documents
  :prefix "efrit-documents-gcalendar-")

(defcustom efrit-documents-gcalendar-auth-host "gcalendar"
  "The auth-source host of the entry used for Calendar."
  :type 'string)

(defcustom efrit-documents-gcalendar-auth-hosts '("gdrive" "gmail" "gmail.com" "imap.gmail.com")
  "Hosts tried, in order, when `efrit-documents-gcalendar-auth-host' has no entry.
An entry counts only if its :scope names a Calendar scope.  Hosts from
`efrit-auth-host-functions' (a mail backend's Google entries) follow."
  :type '(repeat string))

(defcustom efrit-documents-gcalendar-user nil
  "The auth-source user to look up, or nil for any."
  :type '(choice (const nil) string))

(defcustom efrit-documents-gcalendar-calendars '("primary")
  "Calendar ids searched: \"primary\", an address, or a shared calendar's id."
  :type '(repeat string))

(defconst efrit-documents-gcalendar--api "https://www.googleapis.com/calendar/v3"
  "Calendar v3 endpoint.")

(defconst efrit-documents-gcalendar--scope "https://www.googleapis.com/auth/calendar.events.readonly"
  "The scope the token needs (calendar.readonly also works).")

(defvar efrit-documents-gcalendar--host nil
  "The auth-source host found to work, once looked up.")

;;;; Credentials

(defun efrit-documents-gcalendar--scoped-p (entry)
  "Whether auth-source ENTRY's scope includes a Calendar scope."
  (string-match-p "googleapis\\.com/auth/calendar" (or (efrit-auth-get entry :scope) "")))

(defun efrit-documents-gcalendar--find-host ()
  "The auth-source host to use, finding and remembering it on first call."
  (or efrit-documents-gcalendar--host
      (let ((tried nil) (found nil))
        (dolist (host (efrit-auth-candidate-hosts efrit-documents-gcalendar-auth-host efrit-documents-gcalendar-auth-hosts))
          (unless found
            (push host tried)
            (condition-case nil
                (let ((entry (efrit-auth-credentials host efrit-documents-gcalendar-user)))
                  (when (or (equal host efrit-documents-gcalendar-auth-host)
                            (efrit-documents-gcalendar--scoped-p entry))
                    (setq found host)))
              (efrit-auth-no-credentials nil))))
        (unless found
          (signal 'efrit-auth-no-credentials
                  (list (format "no auth-source entry for Calendar: tried hosts %s; add %s to the scope of your Google entry and run M-x efrit-auth-reauthorize"
                                (string-join (nreverse tried) ", ") efrit-documents-gcalendar--scope))))
        (setq efrit-documents-gcalendar--host found))))

(defun efrit-documents-gcalendar--request (path &optional params)
  "GET PATH under the Calendar endpoint with PARAMS."
  (condition-case err
      (efrit-auth-request (efrit-documents-gcalendar--find-host) "GET"
                          (concat efrit-documents-gcalendar--api path)
                          :user efrit-documents-gcalendar-user :params params)
    (efrit-auth-http-error
     (signal 'efrit-documents-error (list (efrit-auth-explain err))))))

;;;; The day and the title

(defun efrit-documents-gcalendar-item-date (item)
  "The day ITEM is about: a YYYY-MM-DD in its title, else its :date, as a time.
A recap is sent after the meeting, sometimes the next morning; the
date the bot wrote in the subject is the meeting's."
  (let ((title (or (plist-get item :title) "")))
    (or (and (string-match "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)" title)
             (efrit-documents--time (concat (match-string 1 title) "T12:00:00")))
        (efrit-documents--time (plist-get item :date)))))

(defun efrit-documents-gcalendar--day-bounds (time)
  "The start and end of TIME's calendar day, local zone, as (START . END)."
  (let* ((decoded (decode-time time))
         (start (encode-time (list 0 0 0 (decoded-time-day decoded) (decoded-time-month decoded)
                                   (decoded-time-year decoded) nil -1 (decoded-time-zone decoded)))))
    (cons start (time-add start (* 24 3600)))))

(defun efrit-documents-gcalendar--normalize (title)
  "TITLE lower-cased with runs of space and punctuation collapsed to one space."
  (string-trim (replace-regexp-in-string "[[:space:][:punct:]]+" " " (downcase (or title "")))))

(defun efrit-documents-gcalendar-title-matches-p (item-title event-title)
  "Whether an event called EVENT-TITLE is the meeting ITEM-TITLE is about.
Equal, or ITEM-TITLE contains EVENT-TITLE whole (bounded by spaces or
the ends), case and punctuation aside.  \"Notes: Platform weekly\" matches
the event \"Platform weekly\"; \"Platform budget\" does not."
  (let ((want (efrit-documents-gcalendar--normalize item-title))
        (have (efrit-documents-gcalendar--normalize event-title)))
    (and (not (string-empty-p have))
         (not (string-empty-p want))
         (or (equal want have)
             (string-match-p (concat "\\(?:\\`\\| \\)" (regexp-quote have) "\\(?:\\'\\| \\)") want))
         t)))

;;;; Events

(defun efrit-documents-gcalendar--events (since until)
  "Event instances on the configured calendars between SINCE and UNTIL.
The log line marks the ones with attachments with a star."
  (let ((out nil))
    (dolist (calendar efrit-documents-gcalendar-calendars)
      (let ((result (efrit-documents-gcalendar--request
                     (format "/calendars/%s/events" (url-hexify-string calendar))
                     `(("timeMin" . ,(format-time-string "%FT%TZ" since t))
                       ("timeMax" . ,(format-time-string "%FT%TZ" until t))
                       ("singleEvents" . "true")
                       ("orderBy" . "startTime")
                       ("maxResults" . "250")
                       ("fields" . "items(id,summary,htmlLink,start,attachments)")))))
        (efrit-log 'debug "documents: gcalendar %s %s..%s -> %d events: %s"
                   calendar (format-time-string "%FT%TZ" since t) (format-time-string "%FT%TZ" until t)
                   (length (alist-get 'items result))
                   (mapconcat (lambda (e) (format "%S%s" (alist-get 'summary e)
                                                  (if (alist-get 'attachments e) "*" "")))
                              (alist-get 'items result) ", "))
        (setq out (append out (alist-get 'items result)))))
    out))

(defun efrit-documents-gcalendar--attachment-doc (attachment event)
  "A document plist for ATTACHMENT of EVENT: a Drive file by id when it is one."
  (let* ((file-id (alist-get 'fileId attachment))
         (url (alist-get 'fileUrl attachment))
         (drive-id (or file-id (and url (efrit-documents-source-match
                                          (or (efrit-documents-source "gdrive")
                                              (efrit-documents-gdrive-register))
                                          url)))))
    (when drive-id
      (list :source "gdrive"
            :id drive-id
            :title (or (alist-get 'title attachment) drive-id)
            :url (or url (format "https://docs.google.com/document/d/%s" drive-id))
            :modified (or (alist-get 'dateTime (alist-get 'start event)) (alist-get 'date (alist-get 'start event)))
            :kind (if (equal (alist-get 'mimeType attachment) "application/vnd.google-apps.document") "doc" "file")
            :event (alist-get 'summary event)
            :event-url (alist-get 'htmlLink event)))))

(defun efrit-documents-gcalendar-attachments (day predicate)
  "The Drive attachments of DAY's events whose title satisfies PREDICATE.
DAY is a time; every event of that calendar day on
`efrit-documents-gcalendar-calendars' is listed and PREDICATE is
called with its title.  Returns document plists of the Drive source
\(no :text), in event order.  This is the primitive a mail-specific
rule builds on: decide the day and the title test yourself, and let
this do the calendar."
  (pcase-let* ((`(,since . ,until) (efrit-documents-gcalendar--day-bounds day))
               (out nil))
    (dolist (event (efrit-documents-gcalendar--events since until))
      (let ((matches (funcall predicate (alist-get 'summary event))))
        (efrit-log 'debug "documents: gcalendar event %S at %s: %s, %d attachment(s)"
                   (alist-get 'summary event)
                   (or (alist-get 'dateTime (alist-get 'start event)) (alist-get 'date (alist-get 'start event)))
                   (if matches "matches" "no match")
                   (length (alist-get 'attachments event)))
        (when matches
          (dolist (attachment (alist-get 'attachments event))
            (if-let* ((doc (efrit-documents-gcalendar--attachment-doc attachment event)))
                (push doc out)
              (efrit-log 'debug "documents: gcalendar attachment %S of %S is not a Drive file (%s); skipped"
                         (alist-get 'title attachment) (alist-get 'summary event) (alist-get 'fileUrl attachment)))))))
    (nreverse out)))

(defun efrit-documents-gcalendar-related (item)
  "For `efrit-documents-related-functions': the attachments of ITEM's meeting.
The day is a YYYY-MM-DD in ITEM's title, else its :date
\(`efrit-documents-gcalendar-item-date'); an event matches when the
title contains its title whole
\(`efrit-documents-gcalendar-title-matches-p').  A mail source with a
fixed subject format does better with its own rule on top of
`efrit-documents-gcalendar-attachments'."
  (when-let* ((day (efrit-documents-gcalendar-item-date item)))
    (efrit-documents-gcalendar-attachments
     day (lambda (event-title)
           (efrit-documents-gcalendar-title-matches-p (plist-get item :title) event-title)))))

(add-hook 'efrit-documents-related-functions #'efrit-documents-gcalendar-related)

;;;###autoload
(defun efrit-documents-gcalendar-check ()
  "Say which auth-source entry Calendar uses and list the calendars it can read."
  (interactive)
  (setq efrit-documents-gcalendar--host nil)
  (condition-case-unless-debug err
      (let* ((host (efrit-documents-gcalendar--find-host))
             (list (efrit-documents-gcalendar--request "/users/me/calendarList"
                                                       '(("fields" . "items(id,summary,primary)")))))
        (message "efrit-documents: Calendar works through auth-source host %S; calendars: %s"
                 host (mapconcat (lambda (c) (format "%s%s" (alist-get 'summary c)
                                                     (if (eq (alist-get 'primary c) t) " (primary)" "")))
                                 (alist-get 'items list) ", ")))
    (error (message "efrit-documents: Calendar is not usable: %s" (efrit-documents-explain err)))))

(provide 'efrit-documents-gcalendar)

;;; efrit-documents-gcalendar.el ends here
