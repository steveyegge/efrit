;;; efrit-tool-web-search.el --- Web search tool -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Keywords: ai, tools, web
;; Version: 0.4.1

;;; Commentary:
;;
;; Tool for searching the web for documentation, solutions, and examples.
;;
;; Provides web search capability for Efrit with:
;; - User consent flow (asks before first use)
;; - Rate limiting per session
;; - Result caching
;; - Multiple search backends (DuckDuckGo HTML by default)
;;
;; Note: Search queries are sent to external services.

;;; Code:

(require 'efrit-tool-utils)
(require 'url)
(require 'url-http)
(require 'dom)
(require 'cl-lib)

;;; Customization

(defcustom efrit-enable-web-access nil
  "When non-nil, allow web search without asking.
When nil, user will be prompted for consent on first search."
  :type 'boolean
  :group 'efrit-tool-utils)

(defcustom efrit-web-search-max-per-session 10
  "Maximum number of web searches per session.
Set to 0 to disable rate limiting."
  :type 'integer
  :group 'efrit-tool-utils)

(defcustom efrit-web-search-cache-seconds 3600
  "How long to cache search results (default 1 hour).
Set to 0 to disable caching."
  :type 'integer
  :group 'efrit-tool-utils)

(defcustom efrit-web-search-backend 'duckduckgo
  "Search backend to use.
Options:
  duckduckgo - DuckDuckGo HTML scraping (free, no API key)
  brave - Brave Search API (requires API key)"
  :type '(choice (const :tag "DuckDuckGo (free)" duckduckgo)
                 (const :tag "Brave Search API" brave))
  :group 'efrit-tool-utils)

(defcustom efrit-brave-search-api-key nil
  "API key for Brave Search. Get one free at https://brave.com/search/api/"
  :type '(choice (const nil) string)
  :group 'efrit-tool-utils)

;;; Session state

(defvar efrit-web-search--session-count 0
  "Number of web searches in current session.")

(defvar efrit-web-search--session-enabled nil
  "Whether web access has been enabled for this session.")

(defvar efrit-web-search--cache (make-hash-table :test 'equal)
  "Cache of recent search results. Key is query, value is (timestamp . results).")

(defun efrit-web-search--reset-session ()
  "Reset web search session state."
  (setq efrit-web-search--session-count 0)
  (setq efrit-web-search--session-enabled nil))

;;; Cache management

(defun efrit-web-search--cache-key (query site)
  "Generate cache key for QUERY with optional SITE restriction."
  (format "%s|%s" (downcase query) (or site "")))

(defun efrit-web-search--cache-get (query site)
  "Get cached results for QUERY with SITE, or nil if not cached/expired."
  (when (> efrit-web-search-cache-seconds 0)
    (let* ((key (efrit-web-search--cache-key query site))
           (entry (gethash key efrit-web-search--cache)))
      (when entry
        (let ((timestamp (car entry))
              (results (cdr entry)))
          (if (< (- (float-time) timestamp) efrit-web-search-cache-seconds)
              results
            ;; Expired - remove from cache
            (remhash key efrit-web-search--cache)
            nil))))))

(defun efrit-web-search--cache-put (query site results)
  "Cache RESULTS for QUERY with SITE."
  (when (> efrit-web-search-cache-seconds 0)
    (let ((key (efrit-web-search--cache-key query site)))
      (puthash key (cons (float-time) results) efrit-web-search--cache))))

;;; DuckDuckGo HTML search

(defun efrit-web-search--duckduckgo-url (query site)
  "Build DuckDuckGo search URL for QUERY with optional SITE restriction."
  (let ((full-query (if site
                        (format "site:%s %s" site query)
                      query)))
    (format "https://html.duckduckgo.com/html/?q=%s"
            (url-hexify-string full-query))))

(defun efrit-web-search--parse-duckduckgo (html)
  "Parse DuckDuckGo HTML results and return list of result alists."
  (with-temp-buffer
    (insert html)
    (let* ((dom (libxml-parse-html-region (point-min) (point-max)))
           (results '()))
      ;; Find result divs - DuckDuckGo uses class="result"
      (dolist (result-node (dom-by-class dom "result"))
        (let* ((link-node (car (dom-by-class result-node "result__a")))
               (snippet-node (car (dom-by-class result-node "result__snippet")))
               (url (when link-node (dom-attr link-node 'href)))
               (title (when link-node (dom-texts link-node)))
               (snippet (when snippet-node (dom-texts snippet-node))))
          ;; DuckDuckGo uses redirect URLs, extract actual URL
          (when (and url (string-match "uddg=\\([^&]+\\)" url))
            (setq url (url-unhex-string (match-string 1 url))))
          (when (and url title)
            (push `((title . ,(string-trim (or title "")))
                    (url . ,url)
                    (snippet . ,(string-trim (or snippet "")))
                    (source . ,(when url
                                 (when (string-match "https?://\\([^/]+\\)" url)
                                   (match-string 1 url)))))
                  results))))
      (nreverse results))))

(defun efrit-web-search--duckduckgo (query site max-results)
  "Search DuckDuckGo for QUERY with optional SITE, returning MAX-RESULTS."
  (let* ((url (efrit-web-search--duckduckgo-url query site))
         (url-request-extra-headers
          '(("User-Agent" . "Mozilla/5.0 (compatible; Emacs Efrit)")))
         (buffer (url-retrieve-synchronously url t nil 10)))
    (if (not buffer)
        (list :success nil :error "Failed to connect to DuckDuckGo")
      (unwind-protect
          (with-current-buffer buffer
            (goto-char (point-min))
            (if (not (re-search-forward "\r?\n\r?\n" nil t))
                (list :success nil :error "Invalid HTTP response")
              (let* ((html (buffer-substring (point) (point-max)))
                     (results (efrit-web-search--parse-duckduckgo html))
                     (limited (seq-take results max-results)))
                (list :success t
                      :results limited
                      :total (length results)
                      :returned (length limited)))))
        (kill-buffer buffer)))))

;;; Brave Search API

(defun efrit-web-search--brave (query site max-results)
  "Search Brave for QUERY with optional SITE, returning MAX-RESULTS."
  (unless efrit-brave-search-api-key
    (user-error "Brave Search requires API key. Set efrit-brave-search-api-key"))
  (let* ((full-query (if site (format "site:%s %s" site query) query))
         (url (format "https://api.search.brave.com/res/v1/web/search?q=%s&count=%d"
                      (url-hexify-string full-query) max-results))
         (url-request-extra-headers
          `(("Accept" . "application/json")
            ("X-Subscription-Token" . ,efrit-brave-search-api-key)))
         (buffer (url-retrieve-synchronously url t nil 10)))
    (if (not buffer)
        (list :success nil :error "Failed to connect to Brave Search API")
      (unwind-protect
          (with-current-buffer buffer
            (goto-char (point-min))
            (if (not (re-search-forward "\r?\n\r?\n" nil t))
                (list :success nil :error "Invalid HTTP response")
              (condition-case err
                  (let* ((json-data (json-read))
                         (web-results (alist-get 'web json-data))
                         (results-list (alist-get 'results web-results))
                         (formatted
                          (mapcar (lambda (r)
                                    `((title . ,(alist-get 'title r))
                                      (url . ,(alist-get 'url r))
                                      (snippet . ,(alist-get 'description r))
                                      (source . ,(alist-get 'site_name r))))
                                  results-list)))
                    (list :success t
                          :results formatted
                          :total (length formatted)
                          :returned (length formatted)))
                (error
                 (list :success nil :error (format "JSON parse error: %s" err))))))
        (kill-buffer buffer)))))

;;; Consent flow

(defun efrit-web-search--check-consent (query)
  "Check if web search is allowed, prompting user if needed.
Returns t if allowed, nil if denied.
In batch/noninteractive mode, auto-approves to avoid blocking."
  (cond
   ;; Permanent enable
   (efrit-enable-web-access t)
   ;; Already enabled for session
   (efrit-web-search--session-enabled t)
   ;; Batch/noninteractive mode - auto-approve (can't prompt)
   (noninteractive
    (setq efrit-web-search--session-enabled t)
    t)
   ;; Need to ask user
   (t
    (let ((response (y-or-n-p
                     (format "Enable web search for this session?\nQuery: %s\nThis will send the query to %s. "
                             query
                             (symbol-name efrit-web-search-backend)))))
      (when response
        (setq efrit-web-search--session-enabled t))
      response))))

;;; Main tool function

(defun efrit-tool-web-search (args)
  "Search the web for documentation, solutions, examples.

ARGS is an alist with:
  query - search terms (required)
  site - site restriction (e.g., 'emacs.stackexchange.com')
  max_results - limit (default: 5)
  type - 'general', 'docs', or 'code' (currently ignored)

Returns standard tool response with search results.

Note: Search queries are sent to external services.
Do not include sensitive user data in queries."
  (efrit-tool-execute web_search args
    (let* ((query (alist-get 'query args))
           (site (alist-get 'site args))
           (max-results (or (alist-get 'max_results args) 5)))

      ;; Validate
      (unless query
        (signal 'user-error (list "query is required")))

      (when (string-empty-p (string-trim query))
        (signal 'user-error (list "query cannot be empty")))

      ;; Check consent
      (unless (efrit-web-search--check-consent query)
        (efrit-tool-error 'permission_denied
                          "Web search not enabled for this session"
                          `((query . ,query))))

      ;; Check rate limit
      (when (and (> efrit-web-search-max-per-session 0)
                 (>= efrit-web-search--session-count efrit-web-search-max-per-session))
        (efrit-tool-error 'rate_limited
                          (format "Web search limit reached (%d per session)"
                                  efrit-web-search-max-per-session)
                          `((count . ,efrit-web-search--session-count)
                            (limit . ,efrit-web-search-max-per-session))))

      ;; Check cache
      (let ((cached (efrit-web-search--cache-get query site)))
        (if cached
            (efrit-tool-success
             `((query_used . ,query)
               (results . ,(vconcat cached))
               (from_cache . t)
               (returned . ,(length cached))))

          ;; Perform search
          (setq efrit-web-search--session-count (1+ efrit-web-search--session-count))
          (let ((search-result
                 (pcase efrit-web-search-backend
                   ('duckduckgo (efrit-web-search--duckduckgo query site max-results))
                   ('brave (efrit-web-search--brave query site max-results))
                   (_ (efrit-web-search--duckduckgo query site max-results)))))

            (if (not (plist-get search-result :success))
                (efrit-tool-error 'search_failed
                                  (plist-get search-result :error)
                                  `((query . ,query)
                                    (backend . ,efrit-web-search-backend)))

              ;; Cache and return results
              (let ((results (plist-get search-result :results)))
                (efrit-web-search--cache-put query site results)
                (efrit-tool-success
                 `((query_used . ,query)
                   ,@(when site `((site_filter . ,site)))
                   (results . ,(vconcat results))
                   (total_estimated . ,(plist-get search-result :total))
                   (returned . ,(plist-get search-result :returned))
                   (from_cache . :json-false)))))))))))

(provide 'efrit-tool-web-search)

;;; efrit-tool-web-search.el ends here
