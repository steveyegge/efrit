;;; test-efrit-gnus.el --- Tests for efrit-gnus -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'efrit-gnus)
(require 'efrit-tool-registry)
(require 'efrit-repl-session)

(defvar test-gnus--articles nil
  "Alist ((GROUP . NUMBER) . RAW) served by the mocked `gnus-request-article'.")

(defun test-gnus--raw (subject body &optional html attachment)
  "A raw RFC 822 message with SUBJECT and text BODY; optional HTML alternative and ATTACHMENT."
  (let ((boundary "b1"))
    (concat "From: Ann <ann@example.com>\r\nTo: me@example.com\r\nDate: Mon, 01 Sep 2026 10:00:00 +0000\r\n"
            "Subject: " subject "\r\nMessage-ID: <" subject "@x>\r\nMIME-Version: 1.0\r\n"
            (if (or html attachment)
                (concat "Content-Type: multipart/mixed; boundary=\"" boundary "\"\r\n\r\n"
                        "--" boundary "\r\nContent-Type: text/plain; charset=utf-8\r\n\r\n" body "\r\n"
                        (when html (concat "--" boundary "\r\nContent-Type: text/html\r\n\r\n" html "\r\n"))
                        (when attachment
                          (concat "--" boundary "\r\nContent-Type: application/pdf; name=\"" attachment
                                  "\"\r\nContent-Disposition: attachment; filename=\"" attachment
                                  "\"\r\n\r\n%PDF-1.4 fake\r\n"))
                        "--" boundary "--\r\n")
              (concat "Content-Type: text/plain; charset=utf-8\r\n\r\n" body "\r\n")))))

(defmacro test-gnus--with-articles (articles &rest body)
  "Run BODY with `gnus-request-article' serving ARTICLES ((GROUP . NUMBER) . RAW)."
  (declare (indent 1))
  `(let ((test-gnus--articles ,articles))
     (cl-letf (((symbol-function 'gnus-request-article)
                (lambda (number group &optional buffer)
                  (when-let* ((raw (cdr (assoc (cons group number) test-gnus--articles))))
                    (with-current-buffer (or buffer (current-buffer))
                      (insert raw))
                    t)))
               ((symbol-function 'gnus-request-head) (lambda (&rest _) nil)))
       ,@body)))

(ert-deftest test-efrit-gnus-render-article ()
  "An article renders as headers, reference, attachments and decoded body;
long bodies are cut with a marker."
  (test-gnus--with-articles
      `((("nnml:mail" . 1) . ,(encode-coding-string
                               (test-gnus--raw "Plan" "Körper ✓ line one.\n\nline two." nil "deck.pdf")
                               'utf-8))
        (("nnml:mail" . 2) . ,(test-gnus--raw "Long" (make-string 500 ?x))))
    (let ((text (efrit-gnus--render "nnml:mail" 1)))
      (should (string-match-p "^From: Ann <ann@example.com>$" text))
      (should (string-match-p "^Subject: Plan$" text))
      (should (string-match-p "^Article: nnml:mail#1$" text))
      (should (string-match-p "^Attachments: deck.pdf (application/pdf" text))
      (should (string-match-p "Körper ✓ line one\\.\n\nline two\\." text)))
    (let* ((efrit-gnus-article-max-chars 100)
           (text (efrit-gnus--render "nnml:mail" 2)))
      (should (string-match-p "\\[\\.\\.\\. 400 more characters not shown \\.\\.\\.\\]" text)))
    (should-not (efrit-gnus--render "nnml:mail" 99))))

(ert-deftest test-efrit-gnus-payload-budget ()
  "The payload keeps whole articles up to the budget and hands back the rest
as the next batch; numbering runs over the whole selection."
  (test-gnus--with-articles
      (mapcar (lambda (n) (cons (cons "nnml:mail" n) (test-gnus--raw (format "m%d" n) (make-string 300 ?y))))
              '(1 2 3))
    (pcase-let* ((efrit-gnus-max-chars 900)
                 (`(,text ,sent ,chars ,rest)
                  (efrit-gnus--payload '(("nnml:mail" . 1) ("nnml:mail" . 2) ("nnml:mail" . 3)))))
      (should (= 2 sent))
      (should (<= chars 900))
      (should (string-match-p "=== Article 1 of 3 ===" text))
      (should (string-match-p "=== Article 2 of 3 ===" text))
      (should-not (string-match-p "Article 3 of 3" text))
      (should (equal '(("nnml:mail" . 3)) rest))
      ;; The next batch continues the numbering.
      (pcase-let ((`(,text2 ,sent2 ,_ ,rest2) (efrit-gnus--payload rest '(2 . 3))))
        (should (= 1 sent2))
        (should (string-match-p "=== Article 3 of 3 ===" text2))
        (should-not rest2)))
    ;; An article bigger than the budget still goes out, alone.
    (pcase-let* ((efrit-gnus-max-chars 100)
                 (`(,text ,sent ,_ ,rest) (efrit-gnus--payload '(("nnml:mail" . 1) ("nnml:mail" . 2)))))
      (should (= 1 sent))
      (should (string-match-p "Article 1 of 2" text))
      (should (equal '(("nnml:mail" . 2)) rest)))))

(defun test-gnus--answer (session text)
  "Record TEXT as the assistant's answer of the turn in SESSION, after its input."
  (efrit-repl-session-add-assistant-message
   session (vector `((type . "text") (text . ,text)))))

(ert-deftest test-efrit-gnus-batches-over-turns ()
  "A selection over the budget is sent as consecutive turns: the first at
once, each next one when the agent reports idle, then a closing message.
One idle event schedules one step; a second idle during a step does not.
Batches are isolated: every turn starts from the history mark the
selection began at, and the closing turn carries the stitched answers,
not the articles."
  (test-gnus--with-articles
      (mapcar (lambda (n) (cons (cons "nnml:mail" n) (test-gnus--raw (format "m%d" n) (make-string 300 ?y))))
              '(1 2 3 4 5))
    (let* ((session (efrit-repl-session-create))
           (submitted nil) (subscribed nil) (timers nil))
      ;; Two earlier turns in the conversation, which every batch must keep
      (efrit-repl-session-add-user-message session "earlier question")
      (test-gnus--answer session "earlier answer")
      (cl-letf (((symbol-function 'efrit-submit)
                 (lambda (shown api)
                   (push (cons shown api) submitted)
                   ;; What the real submit does to the history
                   (efrit-repl-session-add-user-message session shown api)
                   t))
                ((symbol-function 'efrit-agent-repl-session) (lambda () session))
                ((symbol-function 'efrit-gnus-ensure-tools) #'ignore)
                ((symbol-function 'efrit-gnus--require) (lambda (&rest _) t))
                ((symbol-function 'efrit-subscribe) (lambda (type fn) (push (cons type fn) subscribed) fn))
                ((symbol-function 'efrit-unsubscribe) #'ignore)
                ((symbol-function 'run-at-time) (lambda (_s _r fn &rest _) (push fn timers) nil)))
        (let ((efrit-gnus-max-chars 900) (efrit-gnus-confirm nil)
              (efrit-gnus-isolate-batches t)
              (efrit-gnus--run nil) (efrit-gnus--sending nil))
          (efrit-gnus--submit (mapcar (lambda (n) (cons "nnml:mail" n)) '(1 2 3 4 5))
                              '("Triage." . "Overall triage.") "unread in mail")
          ;; First batch went out: articles 1-2 of 5, and it says so.
          (should (= 1 (length submitted)))
          (should (equal "analyze articles 1-2 of 5 from unread in mail: Triage." (car (car submitted))))
          (should (string-match-p "articles 1-2 here, each batch in a conversation of its own" (cdr (car submitted))))
          (should (= 1 (length (efrit-gnus-run-queue efrit-gnus--run))))
          (should (= 2 (efrit-gnus-run-mark efrit-gnus--run)))
          (should (equal '(status . efrit-gnus--on-status) (car subscribed)))
          (test-gnus--answer session "answer one")
          (should (= 4 (length (efrit-repl-session-api-messages session))))
          ;; Agent goes idle: the next batch is scheduled and sent.
          (efrit-gnus--on-status '((:status . working)))
          (should-not timers)
          (efrit-gnus--on-status '((:status . idle)))
          (should (= 1 (length timers)))
          ;; While the step runs, another idle must not start a second one.
          (let ((efrit-gnus--sending t))
            (efrit-gnus--on-status '((:status . idle)))
            (should (= 1 (length timers))))
          (funcall (pop timers))
          (should (= 2 (length submitted)))
          (should (equal "analyze articles 3-4 of 5 from unread in mail: Triage." (car (car submitted))))
          ;; The first batch's answer was collected and its turn rewound:
          ;; the history is the two earlier messages plus this batch's input.
          (should (equal '(("Batch 1" . "answer one")) (efrit-gnus-run-answers efrit-gnus--run)))
          (should (= 3 (length (efrit-repl-session-api-messages session))))
          (should (equal "earlier answer"
                         (efrit-repl-session-last-answer session)))
          (test-gnus--answer session "answer two")
          (efrit-gnus--on-status '((:status . idle)))
          (funcall (pop timers))
          (should (= 3 (length submitted)))
          (should (equal "analyze articles 5-5 of 5 from unread in mail: Triage." (car (car submitted))))
          (should-not (efrit-gnus-run-queue efrit-gnus--run))
          (should (equal "Overall triage." (efrit-gnus-run-closing efrit-gnus--run)))
          (test-gnus--answer session "answer three")
          ;; The closing goes out on the next idle, after the last batch,
          ;; with the three answers stitched and no article text.
          (efrit-gnus--on-status '((:status . idle)))
          (funcall (pop timers))
          (should (= 4 (length submitted)))
          (should (equal "over the whole selection" (car (car submitted))))
          (let ((closing (cdr (car submitted))))
            (should (string-match-p "5 articles from unread in mail, analyzed in 3 batches" closing))
            (should (string-match-p "=== Batch 1 ===\nanswer one\n\n=== Batch 2 ===\nanswer two\n\n=== Batch 3 ===\nanswer three\n\nOverall triage\\." closing))
            (should-not (string-match-p "Subject: m1" closing)))
          (should-not efrit-gnus--run)
          (should (= 3 (length (efrit-repl-session-api-messages session))))
          ;; Nothing left: idle schedules nothing.
          (efrit-gnus--on-status '((:status . idle)))
          (should-not timers)
          ;; Cancel drops what is queued.
          (setq efrit-gnus--run (efrit-gnus-run--make :closing "c"))
          (efrit-gnus-cancel)
          (should-not efrit-gnus--run)
          ;; A failed turn ends the run.
          (setq efrit-gnus--run (efrit-gnus-run--make :closing "c"))
          (efrit-gnus--on-status '((:status . failed)))
          (should-not efrit-gnus--run))))))

(ert-deftest test-efrit-gnus-batches-not-isolated ()
  "With `efrit-gnus-isolate-batches' nil nothing is rewound and the closing
message carries the prompt only."
  (test-gnus--with-articles
      (mapcar (lambda (n) (cons (cons "nnml:mail" n) (test-gnus--raw (format "m%d" n) (make-string 300 ?y))))
              '(1 2 3))
    (let* ((session (efrit-repl-session-create))
           (submitted nil) (timers nil))
      (cl-letf (((symbol-function 'efrit-submit)
                 (lambda (shown api)
                   (push (cons shown api) submitted)
                   (efrit-repl-session-add-user-message session shown api)
                   t))
                ((symbol-function 'efrit-agent-repl-session) (lambda () session))
                ((symbol-function 'efrit-gnus-ensure-tools) #'ignore)
                ((symbol-function 'efrit-gnus--require) (lambda (&rest _) t))
                ((symbol-function 'efrit-subscribe) (lambda (_type fn) fn))
                ((symbol-function 'efrit-unsubscribe) #'ignore)
                ((symbol-function 'run-at-time) (lambda (_s _r fn &rest _) (push fn timers) nil)))
        (let ((efrit-gnus-max-chars 900) (efrit-gnus-confirm nil)
              (efrit-gnus-isolate-batches nil)
              (efrit-gnus--run nil) (efrit-gnus--sending nil))
          (efrit-gnus--submit '(("nnml:mail" . 1) ("nnml:mail" . 2) ("nnml:mail" . 3))
                              '("Triage." . "Overall triage.") "mail")
          (should (string-match-p "the rest follow in later messages" (cdr (car submitted))))
          (test-gnus--answer session "a1")
          (efrit-gnus--on-status '((:status . idle)))
          (funcall (pop timers))
          (test-gnus--answer session "a2")
          (efrit-gnus--on-status '((:status . idle)))
          (funcall (pop timers))
          (should (= 3 (length submitted)))
          (should (equal "That was the whole selection. Overall triage." (cdr (car submitted))))
          ;; Every turn stayed: 2 batches + 2 answers + closing input
          (should (= 5 (length (efrit-repl-session-api-messages session)))))))))

(ert-deftest test-efrit-gnus-reload-drops-stale-handlers ()
  "After a reload the previous generation's status handlers must be gone
from the bus, or they race the new one.  Watching is done against the
real event bus here."
  (require 'efrit-events)
  (let ((efrit-events--subscribers nil))
    (defalias 'efrit-gnus--send-next #'ignore)
    (defalias 'efrit-gnus--send-closing #'ignore)
    (unwind-protect
        (progn
          (efrit-subscribe 'status #'efrit-gnus--send-next)
          (efrit-subscribe 'status #'efrit-gnus--send-closing)
          (efrit-gnus--watch-for-idle)
          (efrit-gnus--watch-for-idle)
          (should (equal '(efrit-gnus--on-status)
                         (cdr (assq 'status efrit-events--subscribers)))))
      (fmakunbound 'efrit-gnus--send-next)
      (fmakunbound 'efrit-gnus--send-closing))))

(ert-deftest test-efrit-gnus-documents-in-render ()
  "A linked Google Doc comes through efrit-documents' Drive source, and a
related document (same title words, near the date) follows it."
  (require 'efrit-documents-gdrive)
  (let ((efrit-documents--cache (make-hash-table :test #'equal))
        (efrit-documents-related-functions nil)
        (efrit-documents-related-search t)
        (efrit-gnus-related-documents t)
        (efrit-gnus-expand-link-functions nil))
    (cl-letf (((symbol-function 'efrit-documents-gdrive--find-host) (lambda (_s) "gmail"))
              ((symbol-function 'efrit-auth-request)
               (cl-function
                (lambda (_host _method url &key params raw &allow-other-keys)
                  (cond
                   ((string-suffix-p "/export" url)
                    (if (string-match-p "NOTES" url) "Notes taken by Calendar" "The recap text"))
                   ((string-match-p "/files/RECAP1\\'" url)
                    '((id . "RECAP1") (name . "Widget bringup prep - recap") (mimeType . "application/vnd.google-apps.document")
                      (modifiedTime . "2026-09-15T10:00:00Z")))
                   ((string-match-p "/files/NOTES1\\'" url)
                    '((id . "NOTES1") (name . "Notes: Widget bringup prep") (mimeType . "application/vnd.google-apps.document")
                      (modifiedTime . "2026-09-15T11:00:00Z")))
                   ((string-suffix-p "/files" url)
                    (should (string-match-p "name contains 'widget'" (cdr (assoc "q" params))))
                    ;; The article is dated 2026-09-01 (see `test-gnus--raw').
                    (should (string-match-p "modifiedTime >= '2026-08-29" (cdr (assoc "q" params))))
                    (should (string-match-p "createdTime <= '2026-09-04" (cdr (assoc "q" params))))
                    '((files . (((id . "RECAP1") (name . "Widget bringup prep - recap")
                                 (mimeType . "application/vnd.google-apps.document") (modifiedTime . "2026-09-15T10:00:00Z"))
                                ((id . "NOTES1") (name . "Notes: Widget bringup prep")
                                 (mimeType . "application/vnd.google-apps.document") (modifiedTime . "2026-09-15T11:00:00Z"))))))
                   (t (error "unexpected %s (raw %s)" url raw)))))))
      (test-gnus--with-articles
          `((("nnml:recaps" . 1) . ,(test-gnus--raw "Recap: Widget bringup prep"
                                                    "Recap: https://docs.google.com/document/d/RECAP1/edit")))
        (let ((text (efrit-gnus--render "nnml:recaps" 1)))
          (should (string-match-p "Linked document: https://docs.google.com/document/d/RECAP1/edit ---\nTitle: Widget bringup prep - recap" text))
          (should (string-match-p "The recap text" text))
          (should (string-match-p "Related document (gdrive): .*NOTES1.*\nTitle: Notes: Widget bringup prep" text))
          (should (string-match-p "Notes taken by Calendar" text))
          ;; The linked recap is not repeated as a related document.
          (should-not (string-match-p "Related document (gdrive): [^\n]*RECAP1" text)))))))

(ert-deftest test-efrit-gnus-related-footnote-and-render ()
  "The washing function appends a footnote of related documents once;
a render of an article carrying the footnote expands those URLs as
links and does not search again."
  (require 'efrit-documents-gdrive)
  (let ((efrit-documents--cache (make-hash-table :test #'equal))
        (efrit-documents-related-functions nil)
        (efrit-documents-related-search t)
        (efrit-gnus-related-documents t)
        (efrit-gnus-expand-link-functions nil)
        (searches 0))
    (cl-letf (((symbol-function 'efrit-documents-gdrive--find-host) (lambda (_s) "gmail"))
              ((symbol-function 'efrit-documents-ensure-tools) #'ignore)
              ((symbol-function 'efrit-auth-request)
               (cl-function
                (lambda (_host _method url &key raw &allow-other-keys)
                  (ignore raw)
                  (cond
                   ((string-suffix-p "/export" url) "Notes taken by Calendar")
                   ((string-match-p "/files/NOTES1\\'" url)
                    '((id . "NOTES1") (name . "Notes: Widget bringup prep") (mimeType . "application/vnd.google-apps.document")
                      (modifiedTime . "2026-09-15T11:00:00Z") (webViewLink . "https://docs.google.com/document/d/NOTES1/edit")))
                   ((string-suffix-p "/files" url)
                    (cl-incf searches)
                    '((files . (((id . "NOTES1") (name . "Notes: Widget bringup prep")
                                 (mimeType . "application/vnd.google-apps.document") (modifiedTime . "2026-09-15T11:00:00Z")
                                 (webViewLink . "https://docs.google.com/document/d/NOTES1/edit"))))))
                   (t (error "unexpected %s" url)))))))
      ;; The article buffer as Gnus leaves it after the other treatments.
      (let ((gnus-article-buffer (generate-new-buffer "*test article*"))
            (gnus-original-article-buffer (generate-new-buffer "*test original*")))
        (unwind-protect
            (progn
              (with-current-buffer gnus-original-article-buffer
                (insert "From: a@example.com\nSubject: Recap: Widget bringup prep\nDate: Mon, 15 Sep 2026 10:00:00 +0000\n\nbody\n"))
              (with-current-buffer gnus-article-buffer
                (insert "From: a@example.com\nSubject: Recap: Widget bringup prep\nDate: Mon, 15 Sep 2026 10:00:00 +0000\n\nRecap body.\n")
                (gnus-article-show-related-documents)
                (let ((text (buffer-string)))
                  (should (string-match-p "\n\nRelated documents:\n- Notes: Widget bringup prep (gdrive, 2026-09-15) https://docs.google.com/document/d/NOTES1/edit\n" text))
                  (should (eq 'efrit-gnus-related-heading
                              (get-text-property (string-match "Related documents:" text) 'face text))))
                (should (= 1 searches))
                ;; Idempotent.
                (gnus-article-show-related-documents)
                (should (= 1 searches))
                (should (= 1 (cl-count "Related documents:" (split-string (buffer-string) "\n") :test #'equal)))))
          (kill-buffer gnus-article-buffer) (kill-buffer gnus-original-article-buffer)))
      ;; A render of the article shown in the article buffer with the
      ;; footnote: the footnote's documents are fetched, nothing searched.
      (setq searches 0)
      (let ((gnus-article-buffer (generate-new-buffer "*test article*")))
        (unwind-protect
            (progn
              (with-current-buffer gnus-article-buffer
                (setq-local gnus-article-current (cons "nnml:recaps" 1))
                (insert "Recap body.\n\n")
                (let ((start (point)))
                  (insert "Related documents:\n- Notes: Widget bringup prep (gdrive, 2026-09-15) https://docs.google.com/document/d/NOTES1/edit\n")
                  (add-text-properties start (point) '(efrit-gnus-related t))))
              (test-gnus--with-articles
                  `((("nnml:recaps" . 1) . ,(test-gnus--raw "Recap: Widget bringup prep" "Recap body.")))
                (let ((text (efrit-gnus--render "nnml:recaps" 1)))
                  (should (string-match-p "\n\nRelated documents:\n- Notes: Widget bringup prep" text))
                  (should (string-match-p "Linked document: https://docs.google.com/document/d/NOTES1/edit ---\nTitle: Notes" text))
                  (should (string-match-p "Notes taken by Calendar" text))
                  (should (= 0 searches)))
                ;; Another article: the footnote is not this one's.
                (with-current-buffer gnus-article-buffer (setq-local gnus-article-current (cons "nnml:recaps" 2)))
                (efrit-gnus--render "nnml:recaps" 1)
                (should (= 1 searches))))
          (kill-buffer gnus-article-buffer)))
      ;; A render of an article whose body already has the footnote.
      (setq searches 0)
      (test-gnus--with-articles
          `((("nnml:recaps" . 1) . ,(test-gnus--raw "Recap: Widget bringup prep"
                                                    "Recap body.\n\nRelated documents:\n- Notes: Widget bringup prep (gdrive, 2026-09-15) https://docs.google.com/document/d/NOTES1/edit\n")))
        (let ((text (efrit-gnus--render "nnml:recaps" 1)))
          (should (string-match-p "Linked document: https://docs.google.com/document/d/NOTES1/edit ---\nTitle: Notes: Widget bringup prep" text))
          (should (string-match-p "Notes taken by Calendar" text))
          (should-not (string-match-p "Related document (gdrive)" text))
          (should (= 0 searches)))))))

(ert-deftest test-efrit-gnus-related-documents-function ()
  "A custom `efrit-gnus-related-documents-function' runs in a buffer with
the headers and the decoded body, in both the render and the wash; its
documents are fetched.  A failing one is a recorded problem, not an error."
  (require 'efrit-documents-gdrive)
  (let* ((seen nil)
         (efrit-documents--cache (make-hash-table :test #'equal))
         (efrit-documents-related-functions nil)
         (efrit-gnus-related-documents t)
         (efrit-gnus-expand-link-functions nil)
         (efrit-gnus-related-documents-function
          (lambda ()
            (push (buffer-substring-no-properties (point-min) (point-max)) seen)
            (save-excursion
              (goto-char (point-min))
              (when (re-search-forward "^Subject: Recap: \\([0-9-]+\\) - \\(.+\\)$" nil t)
                (list (list :source "gdrive" :id "NOTES1" :title (concat "Notes - " (match-string 2))
                            :url "https://docs.google.com/document/d/NOTES1/edit")))))))
    (cl-letf (((symbol-function 'efrit-documents-gdrive--find-host) (lambda (_s) "gmail"))
              ((symbol-function 'efrit-documents-ensure-tools) #'ignore)
              ((symbol-function 'efrit-auth-request)
               (cl-function
                (lambda (_host _method url &key &allow-other-keys)
                  (cond
                   ((string-suffix-p "/export" url) "Notes body")
                   ((string-match-p "/files/NOTES1\\'" url)
                    '((id . "NOTES1") (name . "Notes - Platform weekly") (mimeType . "application/vnd.google-apps.document")
                      (modifiedTime . "2026-09-21T15:00:00Z")))
                   (t (error "unexpected %s" url)))))))
      ;; Render: the function sees headers + decoded body (HTML part rendered).
      (test-gnus--with-articles
          `((("nnml:recaps" . 1) . ,(test-gnus--raw "Recap: 2026-09-21 - Platform weekly"
                                                    "Plain recap." "<p>HTML <b>recap</b>.</p>")))
        (let ((text (efrit-gnus--render "nnml:recaps" 1)))
          (should (string-match-p "Related document (gdrive): https://docs.google.com/document/d/NOTES1/edit ---\nTitle: Notes - Platform weekly" text))
          (should (string-match-p "Notes body" text))
          (should (string-match-p "^Subject: Recap: 2026-09-21 - Platform weekly$" (car seen)))
          (should (string-match-p "\n\nPlain recap\." (car seen)))
          ;; The MIME body is gone; the text of both parts is there.
          (should-not (string-match-p "^--b1" (car seen)))
          (should (string-match-p "HTML recap" (car seen)))))
      ;; Wash: same buffer shape from the original article buffer.
      (setq seen nil)
      (let ((gnus-article-buffer (generate-new-buffer "*test article*"))
            (gnus-original-article-buffer (generate-new-buffer "*test original*")))
        (unwind-protect
            (progn
              (with-current-buffer gnus-original-article-buffer
                (insert (test-gnus--raw "Recap: 2026-09-21 - Platform weekly" "Plain recap.")))
              (with-current-buffer gnus-article-buffer
                (insert "Subject: Recap: 2026-09-21 - Platform weekly\n\nPlain recap.\n")
                (gnus-article-show-related-documents)
                (should (string-match-p "Related documents:\n- Notes - Platform weekly (gdrive) https://docs.google.com/document/d/NOTES1/edit" (buffer-string)))
                (should (string-match-p "^Subject: Recap: " (car seen)))))
          (kill-buffer gnus-article-buffer) (kill-buffer gnus-original-article-buffer)))
      ;; A failing function is a problem, not a crash.
      (let ((efrit-gnus-related-documents-function (lambda () (error "no calendar today"))))
        (test-gnus--with-articles
            `((("nnml:recaps" . 2) . ,(test-gnus--raw "Other" "body")))
          (should (string-match-p "Article: nnml:recaps#2" (efrit-gnus--render "nnml:recaps" 2)))
          (should (member "no calendar today" efrit-documents-related-problems)))))))

(ert-deftest test-efrit-gnus-prompts-are-pairs ()
  "Every built-in prompt has a per-batch part; summarizing ones an
over-everything part too.  A name or a plist resolves to that pair, a
single prompt to an item with no closing, a typed question to the
question plus a derived closing."
  (should (>= (length efrit-prompts-builtin) 9))
  (dolist (p efrit-prompts-builtin)
    (should (stringp (plist-get p :name)))
    (should (stringp (plist-get p :item)))
    (should (memq (efrit-prompts-kind p) '(single summarizing)))
    (when (efrit-prompts-summarizing-p p)
      (should (not (string-empty-p (plist-get p :summary))))))
  (let ((efrit-prompts--loaded t) (efrit-prompts--user nil))
    (should (efrit-prompts-summarizing-p (efrit-prompts-get "trends, accomplishments, concerns")))
    (should-not (efrit-prompts-summarizing-p (efrit-prompts-get "draft replies")))
    (should (equal (efrit-gnus--prompt-pair "triage")
                   (efrit-gnus--prompt-pair (efrit-prompts-get "triage"))))
    (should (cdr (efrit-gnus--prompt-pair "triage")))
    (should-not (cdr (efrit-gnus--prompt-pair "draft replies")))
    (should (equal "Now the same, over the whole selection as one: Which mention budgets? Consolidate; do not repeat the per-message answers."
                   (cdr (efrit-gnus--prompt-pair "Which mention budgets?"))))))

(ert-deftest test-efrit-gnus-submit-builds-turn ()
  "The submission fills the prompt, confirms once, and hands efrit a short
shown line plus the full API text; a busy efrit is a user error."
  (test-gnus--with-articles
      (mapcar (lambda (n) (cons (cons "nnml:mail" n) (test-gnus--raw (format "m%d" n) "hello"))) '(1 2))
    (let ((session (efrit-repl-session-create)) submitted)
      (cl-letf (((symbol-function 'efrit-submit)
                 (lambda (shown api) (setq submitted (list shown api)) t))
                ((symbol-function 'efrit-agent-repl-session) (lambda () session))
                ((symbol-function 'efrit-gnus-ensure-tools) #'ignore)
                ((symbol-function 'efrit-gnus--require) (lambda (&rest _) t))
                ((symbol-function 'y-or-n-p) (lambda (_) t)))
        (let ((efrit-gnus-confirm 'once) (efrit-gnus--confirmed nil))
          (efrit-gnus--submit '(("nnml:mail" . 1) ("nnml:mail" . 2))
                              "Summarize {count} articles of {group}." "group mail" "mail")
          (should efrit-gnus--confirmed)
          (should (equal "analyze 2 articles from group mail: Summarize 2 articles of mail."
                         (car submitted)))
          (should (string-prefix-p "Summarize 2 articles of mail." (cadr submitted)))
          (should (string-match-p "gnus_search and gnus_articles" (cadr submitted)))
          (should (string-match-p "=== Article 1 of 2 ===" (cadr submitted)))
          (should (string-match-p "Subject: m2" (cadr submitted)))
          (efrit-gnus--submit '(("nnml:mail" . 1)) "summarize")
          (should (equal "analyze 1 article from the selection: summarize" (car submitted)))
          ;; The summary prompt is armed for after the turn.
          (should (string-match-p "main threads and themes" (efrit-gnus-run-closing efrit-gnus--run)))
          ;; A typed prompt gets a derived summary.
          (efrit-gnus--submit '(("nnml:mail" . 1)) "Which ones mention budgets?")
          (should (string-match-p "over the whole selection as one: Which ones mention budgets\\?"
                                  (efrit-gnus-run-closing efrit-gnus--run)))))
      (cl-letf (((symbol-function 'efrit-submit) (lambda (&rest _) nil))
                ((symbol-function 'efrit-agent-repl-session) (lambda () session))
                ((symbol-function 'efrit-gnus-ensure-tools) #'ignore)
                ((symbol-function 'efrit-gnus--require) (lambda (&rest _) t)))
        (let ((efrit-gnus-confirm nil))
          (should-error (efrit-gnus--submit '(("nnml:mail" . 1)) "x") :type 'user-error)
          (should-not efrit-gnus--run)))
      ;; Default: no confirmation prompt at all.
      (should-not efrit-gnus-confirm))))

(ert-deftest test-efrit-gnus-tools ()
  "gnus_articles takes GROUP#NUMBER references; bad ones are reported; the
tools register as read-only under package efrit-gnus."
  (test-gnus--with-articles
      `((("nnml:mail" . 7) . ,(test-gnus--raw "seven" "body seven")))
    (let ((text (efrit-gnus--tool-articles '(("refs" . ["nnml:mail#7"])))))
      (should (string-match-p "Article: nnml:mail#7" text))
      (should (string-match-p "body seven" text)))
    (should (string-match-p "No valid references" (efrit-gnus--tool-articles '(("refs" . ["nonsense"]))))))
  ;; The tool has its own, smaller budget: past it, references for a second call.
  (test-gnus--with-articles
      (mapcar (lambda (n) (cons (cons "nnml:mail" n) (test-gnus--raw (format "m%d" n) (make-string 300 ?y))))
              '(1 2 3))
    (let* ((efrit-gnus-max-chars 150000) (efrit-gnus-tool-max-chars 900)
           (text (efrit-gnus--tool-articles '(("refs" . ["nnml:mail#1" "nnml:mail#2" "nnml:mail#3"])))))
      (should (string-match-p "Subject: m2" text))
      (should-not (string-match-p "Subject: m3" text))
      (should (string-match-p "1 more article not included, over the size limit; ask for them in a second call: nnml:mail#3" text))))
  (test-gnus--with-articles
      `((("nnml:mail" . 7) . ,(test-gnus--raw "seven" "body seven")))
    (should (equal '("nnml:mail" . 7) (efrit-gnus--parse-ref "nnml:mail#7")))
    (should (equal '("nnimap+work:a#b/c" . 12) (efrit-gnus--parse-ref "nnimap+work:a#b/c#12")))
    (should-not (efrit-gnus--parse-ref "no-number")))
  (let ((efrit-tool-registry nil))
    (cl-letf (((symbol-function 'efrit-gnus--require) (lambda (&rest _) t)))
      (efrit-gnus-ensure-tools)
      ;; The document tools ride along.
      (should (equal '("doc_fetch" "doc_search" "doc_sources" "gnus_articles" "gnus_groups" "gnus_search")
                     (sort (mapcar #'car efrit-tool-registry) #'string<)))
      (should (cl-every (lambda (e) (eq 'read (efrit-registered-tool-class (cdr e)))) efrit-tool-registry))
      (should (equal '("gnus_articles" "gnus_groups" "gnus_search")
                     (sort (efrit-unregister-package-tools 'efrit-gnus) #'string<))))))

(ert-deftest test-efrit-gnus-keys ()
  "The command map is under the prefix in summary and group mode maps."
  (should (keymapp efrit-gnus-map))
  (should (eq 'efrit-gnus-analyze (lookup-key efrit-gnus-map "a")))
  (should (eq 'efrit-gnus-analyze-unread (lookup-key efrit-gnus-map "u")))
  (when efrit-gnus-summary-prefix-key
    (should (eq efrit-gnus-map (lookup-key gnus-summary-mode-map (kbd efrit-gnus-summary-prefix-key))))))


(ert-deftest test-efrit-gnus-expand-links ()
  "Links in the body are offered to `efrit-gnus-expand-link-functions';
the first non-nil result follows the article; a failing expander leaves
a note; at most `efrit-gnus-expand-links-max' are expanded."
  (test-gnus--with-articles
      `((("nnml:mail" . 1)
         . ,(test-gnus--raw "recap" "Notes at https://docs.example/d/AAA. Also https://other.example/x and https://docs.example/d/BBB")))
    (let ((efrit-gnus-expand-link-functions
           (list (lambda (url) (when (string-match-p "docs\\.example/d/AAA" url) "Doc A text"))
                 (lambda (url) (when (string-match-p "docs\\.example/d/BBB" url) (error "no access")))))
          (efrit-gnus-expand-links-max 5))
      (let ((text (efrit-gnus--render "nnml:mail" 1)))
        (should (string-match-p "--- Linked document: https://docs.example/d/AAA ---\nDoc A text" text))
        (should (string-match-p "--- Linked document: https://docs.example/d/BBB ---\n\\[could not fetch: no access\\]" text))
        (should-not (string-match-p "Linked document: https://other.example" text)))
      ;; The link cap counts expansions, not URLs seen.
      (let ((efrit-gnus-expand-links-max 1))
        (let ((text (efrit-gnus--render "nnml:mail" 1)))
          (should (string-match-p "Doc A text" text))
          (should-not (string-match-p "could not fetch" text)))))
    ;; No expanders: nothing appended.
    (let ((efrit-gnus-expand-link-functions nil))
      (should-not (string-match-p "Linked document" (efrit-gnus--render "nnml:mail" 1))))
    (should (equal '("https://a.example/p" "https://b.example/q")
                   (efrit-gnus--article-links "see https://a.example/p, and (https://b.example/q).")))))

(provide 'test-efrit-gnus)
;;; test-efrit-gnus.el ends here
