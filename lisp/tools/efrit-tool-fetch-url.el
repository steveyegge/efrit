;;; efrit-tool-fetch-url.el --- Fetch URL content tool -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Keywords: ai, tools, web
;; Version: 0.4.1

;;; Commentary:
;;
;; Tool for fetching content from specific URLs.
;;
;; Use cases:
;; - Fetch documentation pages found via web_search
;; - Read README from GitHub repos
;; - Access API documentation
;;
;; Security:
;; - Domain allowlist by default
;; - Optional per-request confirmation
;; - Configurable security level

;;; Code:

(require 'efrit-tool-utils)
(require 'url)
(require 'url-http)
(require 'shr)
(require 'dom)
(require 'cl-lib)

;; Declare url-http-target-url which is set during URL fetching
(defvar url-http-target-url nil
  "The URL being fetched in url-http.")

;;; Customization

(defcustom efrit-fetch-url-security-level 'allowlist
  "Security level for URL fetching.
Options:
  allowlist - Only fetch from approved domains (default)
  confirm - Ask user before each fetch
  open - Fetch any URL (not recommended)"
  :type '(choice (const :tag "Allowlist only" allowlist)
                 (const :tag "Confirm each" confirm)
                 (const :tag "Open (not recommended)" open))
  :group 'efrit-tool-utils)

(defcustom efrit-fetch-url-allowed-domains
  '("gnu.org" "emacs.org" "www.gnu.org"
    "stackoverflow.com" "emacs.stackexchange.com" "stackexchange.com"
    "github.com" "raw.githubusercontent.com" "gist.github.com"
    "gitlab.com"
    "docs.python.org" "developer.mozilla.org" "mdn.io"
    "en.wikipedia.org"
    "melpa.org" "stable.melpa.org"
    "orgmode.org"
    "www.emacswiki.org" "emacswiki.org")
  "List of allowed domains for URL fetching.
Only used when `efrit-fetch-url-security-level' is \\='allowlist."
  :type '(repeat string)
  :group 'efrit-tool-utils)

(defcustom efrit-fetch-url-max-length 50000
  "Maximum content length in characters."
  :type 'integer
  :group 'efrit-tool-utils)

(defcustom efrit-fetch-url-timeout 15
  "Timeout for URL fetches in seconds."
  :type 'integer
  :group 'efrit-tool-utils)

;;; URL Validation

(defun efrit-fetch-url--extract-domain (url)
  "Extract the domain from URL."
  (when (string-match "https?://\\([^/]+\\)" url)
    (match-string 1 url)))

(defun efrit-fetch-url--domain-allowed-p (domain)
  "Check if DOMAIN is in the allowlist."
  (cl-some (lambda (allowed)
             (or (string= domain allowed)
                 (string-suffix-p (concat "." allowed) domain)))
           efrit-fetch-url-allowed-domains))

(defun efrit-fetch-url--check-permission (url)
  "Check if fetching URL is permitted. Return t if allowed, nil otherwise.
May prompt user depending on security level."
  (let ((domain (efrit-fetch-url--extract-domain url)))
    (pcase efrit-fetch-url-security-level
      ('open t)
      ('confirm
       (y-or-n-p (format "Fetch content from %s? " domain)))
      ('allowlist
       (if (efrit-fetch-url--domain-allowed-p domain)
           t
         (user-error "Domain %s not in allowlist. Add to efrit-fetch-url-allowed-domains or change security level" domain)))
      (_ nil))))

;;; Content Fetching

(defun efrit-fetch-url--fetch (url)
  "Fetch URL and return plist with :success, :content, :content-type, :url."
  (let* ((start-time (float-time))
         (url-request-extra-headers
          '(("User-Agent" . "Mozilla/5.0 (compatible; Emacs Efrit)")))
         (buffer (url-retrieve-synchronously url t nil efrit-fetch-url-timeout)))
    (if (not buffer)
        (list :success nil :error "Failed to fetch URL" :fetch-time 0)
      (unwind-protect
          (with-current-buffer buffer
            (goto-char (point-min))
            ;; Parse HTTP status
            (if (not (looking-at "HTTP/[0-9.]+ \\([0-9]+\\)"))
                (list :success nil :error "Invalid HTTP response"
                      :fetch-time (- (float-time) start-time))
              (let ((status (string-to-number (match-string 1))))
                (if (not (and (>= status 200) (< status 300)))
                    (list :success nil
                          :error (format "HTTP error: %d" status)
                          :fetch-time (- (float-time) start-time))
                  ;; Extract content-type
                  (let ((content-type "text/html")
                        (final-url url))
                    (when (re-search-forward "^Content-Type: \\([^\r\n;]+\\)" nil t)
                      (setq content-type (match-string 1)))
                    ;; Get final URL after redirects
                    (when (boundp 'url-http-end-of-headers)
                      (setq final-url (or (url-recreate-url url-http-target-url) url)))
                    ;; Move to body
                    (goto-char (point-min))
                    (if (not (re-search-forward "\r?\n\r?\n" nil t))
                        (list :success nil :error "No content body"
                              :fetch-time (- (float-time) start-time))
                      (list :success t
                            :content (buffer-substring (point) (point-max))
                            :content-type content-type
                            :url final-url
                            :fetch-time (- (float-time) start-time))))))))
        (kill-buffer buffer)))))

;;; Content Conversion

(defun efrit-fetch-url--html-to-text (html)
  "Convert HTML to plain text using shr."
  (with-temp-buffer
    (insert html)
    (let ((shr-inhibit-images t)
          (shr-use-fonts nil)
          (shr-width 80))
      (shr-render-region (point-min) (point-max)))
    (buffer-string)))

(defun efrit-fetch-url--html-to-markdown (html)
  "Convert HTML to markdown.
Uses pandoc if available, otherwise falls back to simple conversion."
  (if (executable-find "pandoc")
      (with-temp-buffer
        (insert html)
        (if (zerop (call-process-region (point-min) (point-max)
                                        "pandoc" t t nil
                                        "-f" "html" "-t" "markdown"
                                        "--wrap=none"))
            (buffer-string)
          ;; Fallback if pandoc fails
          (efrit-fetch-url--html-to-text html)))
    ;; No pandoc - use simple conversion
    (efrit-fetch-url--simple-html-to-markdown html)))

(defun efrit-fetch-url--simple-html-to-markdown (html)
  "Simple HTML to markdown conversion without pandoc."
  (with-temp-buffer
    (insert html)
    ;; Remove scripts and styles
    (goto-char (point-min))
    (while (re-search-forward "<\\(script\\|style\\)[^>]*>.*?</\\1>" nil t)
      (replace-match ""))
    ;; Convert headers
    (goto-char (point-min))
    (while (re-search-forward "<h\\([1-6]\\)[^>]*>\\(.*?\\)</h[1-6]>" nil t)
      (let ((level (string-to-number (match-string 1)))
            (text (match-string 2)))
        (replace-match (concat "\n" (make-string level ?#) " "
                               (efrit-fetch-url--strip-tags text) "\n"))))
    ;; Convert links
    (goto-char (point-min))
    (while (re-search-forward "<a[^>]*href=\"\\([^\"]+\\)\"[^>]*>\\(.*?\\)</a>" nil t)
      (let ((url (match-string 1))
            (text (match-string 2)))
        (replace-match (format "[%s](%s)" (efrit-fetch-url--strip-tags text) url))))
    ;; Convert paragraphs
    (goto-char (point-min))
    (while (re-search-forward "<p[^>]*>\\(.*?\\)</p>" nil t)
      (replace-match (concat "\n" (efrit-fetch-url--strip-tags (match-string 1)) "\n")))
    ;; Convert code blocks
    (goto-char (point-min))
    (while (re-search-forward "<pre[^>]*>\\(.*?\\)</pre>" nil t)
      (replace-match (concat "\n```\n" (efrit-fetch-url--strip-tags (match-string 1)) "\n```\n")))
    ;; Convert inline code
    (goto-char (point-min))
    (while (re-search-forward "<code[^>]*>\\(.*?\\)</code>" nil t)
      (replace-match (concat "`" (efrit-fetch-url--strip-tags (match-string 1)) "`")))
    ;; Convert lists
    (goto-char (point-min))
    (while (re-search-forward "<li[^>]*>\\(.*?\\)</li>" nil t)
      (replace-match (concat "\n- " (efrit-fetch-url--strip-tags (match-string 1)))))
    ;; Remove remaining tags
    (goto-char (point-min))
    (while (re-search-forward "<[^>]+>" nil t)
      (replace-match ""))
    ;; Decode entities
    (goto-char (point-min))
    (while (re-search-forward "&\\([a-z]+\\);" nil t)
      (let ((entity (match-string 1)))
        (replace-match
         (pcase entity
           ("amp" "&") ("lt" "<") ("gt" ">")
           ("quot" "\"") ("apos" "'") ("nbsp" " ")
           (_ " ")))))
    ;; Clean up whitespace
    (goto-char (point-min))
    (while (re-search-forward "\n\n\n+" nil t)
      (replace-match "\n\n"))
    (string-trim (buffer-string))))

(defun efrit-fetch-url--strip-tags (text)
  "Remove HTML tags from TEXT."
  (replace-regexp-in-string "<[^>]+>" "" text))

(defun efrit-fetch-url--extract-title (html)
  "Extract title from HTML."
  (when (string-match "<title[^>]*>\\([^<]+\\)</title>" html)
    (string-trim (match-string 1 html))))

(defun efrit-fetch-url--apply-selector (html selector)
  "Extract content matching CSS SELECTOR from HTML.
Currently supports simple selectors: tag, #id, .class"
  (with-temp-buffer
    (insert html)
    (let ((dom (libxml-parse-html-region (point-min) (point-max))))
      (when dom
        (let ((nodes (cond
                      ;; ID selector
                      ((string-prefix-p "#" selector)
                       (dom-by-id dom (substring selector 1)))
                      ;; Class selector
                      ((string-prefix-p "." selector)
                       (dom-by-class dom (substring selector 1)))
                      ;; Tag selector
                      (t (dom-by-tag dom (intern selector))))))
          (when nodes
            (mapconcat (lambda (node) (dom-texts node))
                       (if (listp nodes) nodes (list nodes))
                       "\n\n")))))))

;;; Main Tool Function

(defun efrit-tool-fetch-url (args)
  "Retrieve content from a specific URL.

ARGS is an alist with:
   url - page to fetch (required)
   selector - optional CSS selector to extract specific content
   format - \\='text\\=', \\='markdown\\=', or \\='html\\=' (default: markdown)
   max_length - truncate if longer (default: 50000)

Returns standard tool response with:
  content - the page content in requested format
  title - page title
  url - final URL (after redirects)
  content_type - MIME type
  truncated - whether content was truncated
  fetch_time - how long it took"
  (efrit-tool-execute fetch_url args
    (let* ((url (alist-get 'url args))
           (selector (alist-get 'selector args))
           (format-type (or (alist-get 'format args) "markdown"))
           (max-length (or (alist-get 'max_length args) efrit-fetch-url-max-length)))

      ;; Validate URL
      (unless url
        (signal 'user-error (list "url is required")))

      (unless (string-match-p "^https?://" url)
        (signal 'user-error (list "url must start with http:// or https://")))

      ;; Check permission
      (unless (efrit-fetch-url--check-permission url)
        (efrit-tool-error 'permission_denied
                          "URL fetch not permitted"
                          `((url . ,url))))

      ;; Fetch the URL
      (let ((fetch-result (efrit-fetch-url--fetch url)))
        (if (not (plist-get fetch-result :success))
            (efrit-tool-error 'fetch_failed
                              (plist-get fetch-result :error)
                              `((url . ,url)))

          ;; Process content
          (let* ((html (plist-get fetch-result :content))
                 (title (efrit-fetch-url--extract-title html))
                 ;; Apply selector if provided
                 (selected-html (if selector
                                    (or (efrit-fetch-url--apply-selector html selector)
                                        html)
                                  html))
                 ;; Convert to requested format
                 (content (pcase format-type
                            ("html" selected-html)
                            ("text" (efrit-fetch-url--html-to-text selected-html))
                            (_ (efrit-fetch-url--html-to-markdown selected-html))))
                 ;; Check truncation
                 (truncated (> (length content) max-length))
                 (final-content (if truncated
                                    (substring content 0 max-length)
                                  content)))

            (efrit-tool-success
             `((content . ,final-content)
               (title . ,(or title ""))
               (url . ,(plist-get fetch-result :url))
               (content_type . ,(plist-get fetch-result :content-type))
               (truncated . ,(if truncated t :json-false))
               (fetch_time . ,(plist-get fetch-result :fetch-time))
               (format . ,format-type)
               ,@(when selector `((selector . ,selector)))))))))))

(provide 'efrit-tool-fetch-url)

;;; efrit-tool-fetch-url.el ends here
