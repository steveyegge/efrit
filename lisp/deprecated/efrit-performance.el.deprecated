;;; efrit-performance.el --- Performance optimizations for Efrit -*- lexical-binding: t -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;; Performance optimizations including request caching, memory management,
;; and response streaming capabilities.

;;; Code:

(require 'efrit-log)
(require 'efrit-common)

(declare-function efrit-context-state-buffer-name "efrit-context")
(declare-function efrit-context-state-buffer-mode "efrit-context") 
(declare-function efrit-context-state-directory "efrit-context")

;;; Memory Management

(defcustom efrit-performance-max-sessions 10
  "Maximum number of sessions to keep in memory."
  :type 'integer
  :group 'efrit)

(defcustom efrit-performance-session-ttl 3600
  "Time to live for inactive sessions in seconds."
  :type 'integer
  :group 'efrit)

(defvar efrit-performance--session-timestamps (make-hash-table :test 'equal)
  "Track last access time for sessions.")

(defun efrit-performance-cleanup-old-sessions ()
  "Clean up sessions older than TTL."
  (let ((cutoff-time (- (float-time) efrit-performance-session-ttl))
        (sessions-to-remove nil))
    ;; Find expired sessions
    (maphash (lambda (session-id timestamp)
               (when (< timestamp cutoff-time)
                 (push session-id sessions-to-remove)))
             efrit-performance--session-timestamps)
    ;; Remove them
    (dolist (session-id sessions-to-remove)
      (efrit-log 'debug "Cleaning up expired session: %s" session-id)
      (efrit-performance--remove-session session-id))))

(defun efrit-performance--remove-session (session-id)
  "Remove SESSION-ID from all tracking structures."
  ;; Remove from async module
  (when (boundp 'efrit-async--sessions)
    (remhash session-id efrit-async--sessions))
  ;; Remove from timestamps
  (remhash session-id efrit-performance--session-timestamps)
  ;; Clean up associated buffers
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (boundp 'efrit-session-id)
                 (equal efrit-session-id session-id))
        (kill-buffer buffer)))))

(defun efrit-performance-touch-session (session-id)
  "Update last access time for SESSION-ID."
  (puthash session-id (float-time) efrit-performance--session-timestamps))

;;; Request Caching

(defvar efrit-performance--request-cache (make-hash-table :test 'equal)
  "Cache for recent API requests.")

(defcustom efrit-performance-cache-ttl 300
  "Cache time to live in seconds."
  :type 'integer
  :group 'efrit)

(cl-defstruct efrit-cache-entry
  "Cache entry for API responses."
  response
  timestamp
  hit-count)

(defun efrit-performance-cache-key (command context)
  "Generate cache key from COMMAND and CONTEXT."
  (secure-hash 'sha256 (format "%s:%s" command 
                                (if context 
                                    (efrit-common-truncate-string 
                                     (json-encode 
                                     (if (and context (fboundp 'efrit-context-state-buffer-name))
                                     ;; Convert struct to alist for JSON encoding
                                     (list (cons 'buffer-name (efrit-context-state-buffer-name context))
                                     (cons 'buffer-mode (format "%s" (efrit-context-state-buffer-mode context)))
                                     (cons 'directory (efrit-context-state-directory context)))
                                     context))
                                     1000)
                                  ""))))

(defun efrit-performance-get-cached (key)
  "Get cached response for KEY if still valid."
  (let ((entry (gethash key efrit-performance--request-cache)))
    (if (and entry
             (< (- (float-time) (efrit-cache-entry-timestamp entry))
                efrit-performance-cache-ttl))
        (progn
          (cl-incf (efrit-cache-entry-hit-count entry))
          (efrit-log 'debug "Cache hit for key %s (hits: %d)"
                     (substring key 0 8)
                     (efrit-cache-entry-hit-count entry))
          (efrit--record-metric 'cache-hits 1)
          (efrit-cache-entry-response entry))
      ;; Cache miss
      (when entry  ; Key exists but expired
        (efrit--record-metric 'cache-misses 1))
      nil)))

(defun efrit-performance-cache-put (key response)
  "Store RESPONSE in cache under KEY."
  (puthash key 
           (make-efrit-cache-entry
            :response response
            :timestamp (float-time)
            :hit-count 0)
           efrit-performance--request-cache))

(defun efrit-performance-clear-cache ()
  "Clear the request cache."
  (interactive)
  (clrhash efrit-performance--request-cache)
  (message "Efrit request cache cleared"))

;;; Response Streaming

(defvar efrit-performance--streaming-enabled nil
  "Whether to enable response streaming.")

(defun efrit-performance-parse-streaming-chunk (chunk)
  "Parse a streaming CHUNK from the API."
  ;; This would parse SSE (Server-Sent Events) format
  ;; For now, just a placeholder
  (when (string-match "^data: \\(.+\\)$" chunk)
    (condition-case nil
        (json-read-from-string (match-string 1 chunk))
      (error nil))))

;;; Optimized JSON Handling

(defun efrit-performance-json-read-optimized (string)
  "Optimized JSON reading for large responses."
  ;; Use native JSON if available (Emacs 27+)
  (if (fboundp 'json-parse-string)
      (json-parse-string string
                         :object-type 'alist
                         :array-type 'list
                         :null-object nil
                         :false-object nil)
    ;; Fall back to traditional parser
    (json-read-from-string string)))

;;; Background Cleanup Timer

(defvar efrit-performance--cleanup-timer nil
  "Timer for periodic cleanup tasks.")

(defun efrit-performance-start-cleanup-timer ()
  "Start periodic cleanup timer."
  (when efrit-performance--cleanup-timer
    (cancel-timer efrit-performance--cleanup-timer))
  (setq efrit-performance--cleanup-timer
        (run-with-timer 300 300 #'efrit-performance-run-cleanup)))

(defun efrit-performance-stop-cleanup-timer ()
  "Stop the cleanup timer."
  (when efrit-performance--cleanup-timer
    (cancel-timer efrit-performance--cleanup-timer)
    (setq efrit-performance--cleanup-timer nil)))

(defun efrit-performance-run-cleanup ()
  "Run all cleanup tasks."
  (efrit-log 'debug "Running performance cleanup")
  (efrit-performance-cleanup-old-sessions)
  (efrit-performance-cleanup-cache))

(defun efrit-performance-cleanup-cache ()
  "Remove expired cache entries."
  (let ((cutoff-time (- (float-time) efrit-performance-cache-ttl))
        (keys-to-remove nil))
    (maphash (lambda (key entry)
               (when (< (efrit-cache-entry-timestamp entry) cutoff-time)
                 (push key keys-to-remove)))
             efrit-performance--request-cache)
    (dolist (key keys-to-remove)
      (remhash key efrit-performance--request-cache))))

;;; Performance Monitoring

(defvar efrit-performance--api-times nil
  "List of recent API call times for monitoring.")

(defvar efrit--metrics (make-hash-table :test 'equal)
  "Hash table tracking various performance metrics.
Keys:
  api-call-times - List of API call durations
  token-usage - List of token counts (input/output)
  cache-hits - Total cache hit count
  cache-misses - Total cache miss count
  total-requests - Total API requests made
  errors - List of error occurrences")

(defun efrit--record-metric (metric-key value)
  "Record VALUE for METRIC-KEY in the metrics system.
Metric types:
  - Counters: total-requests, cache-hits, cache-misses, errors
  - Lists: api-call-times, token-usage (kept to last 100 entries)
  - Values: Any other key stores the raw value"
  (let ((current (gethash metric-key efrit--metrics)))
    (cond
     ;; Counter metrics - increment
     ((memq metric-key '(total-requests cache-hits cache-misses errors))
      (puthash metric-key (1+ (or current 0)) efrit--metrics))

     ;; List metrics - append and limit to 100
     ((memq metric-key '(api-call-times token-usage))
      (let ((updated-list (cons value (or current nil))))
        (when (> (length updated-list) 100)
          (setq updated-list (cl-subseq updated-list 0 100)))
        (puthash metric-key updated-list efrit--metrics)))

     ;; Other metrics - store value directly
     (t
      (puthash metric-key value efrit--metrics)))))

(defun efrit--get-metric (metric-key)
  "Get the value of METRIC-KEY from the metrics system."
  (gethash metric-key efrit--metrics))

(defun efrit--clear-metrics ()
  "Clear all metrics data."
  (interactive)
  (clrhash efrit--metrics)
  (message "Efrit metrics cleared"))

(defun efrit-performance-record-api-time (elapsed-time)
  "Record ELAPSED-TIME for performance monitoring."
  (push elapsed-time efrit-performance--api-times)
  ;; Keep only last 100 times
  (when (> (length efrit-performance--api-times) 100)
    (setq efrit-performance--api-times
          (cl-subseq efrit-performance--api-times 0 100)))
  ;; Also record in new metrics system
  (efrit--record-metric 'api-call-times elapsed-time)
  (efrit--record-metric 'total-requests 1))

(defun efrit-performance-get-stats ()
  "Get performance statistics."
  (when efrit-performance--api-times
    (let* ((times (sort (copy-sequence efrit-performance--api-times) #'<))
           (count (length times))
           (median (if (cl-oddp count)
                       (nth (/ count 2) times)
                     (/ (+ (nth (1- (/ count 2)) times)
                           (nth (/ count 2) times))
                        2.0)))
           (mean (/ (cl-reduce #'+ times) (float count))))
      (list :count count
            :mean mean
            :median median
            :min (car times)
            :max (car (last times))))))

(defun efrit-performance-show-stats ()
  "Display performance statistics."
  (interactive)
  (let ((stats (efrit-performance-get-stats)))
    (if stats
        (message "API Performance - Calls: %d, Mean: %.2fs, Median: %.2fs, Range: %.2f-%.2fs"
                 (plist-get stats :count)
                 (plist-get stats :mean)
                 (plist-get stats :median)
                 (plist-get stats :min)
                 (plist-get stats :max))
      (message "No performance data collected yet"))))

;;; Metrics Dashboard

(defun efrit-show-metrics ()
  "Display comprehensive metrics dashboard in a buffer."
  (interactive)
  (with-current-buffer (get-buffer-create "*Efrit Metrics*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert "Efrit Performance Metrics Dashboard\n")
      (insert "====================================\n\n")

      ;; API Call Statistics
      (insert "API Calls\n")
      (insert "---------\n")
      (let ((total-requests (or (efrit--get-metric 'total-requests) 0))
            (api-times (efrit--get-metric 'api-call-times)))
        (insert (format "Total requests: %d\n" total-requests))
        (when api-times
          (let* ((times (sort (copy-sequence api-times) #'<))
                 (count (length times))
                 (mean (/ (cl-reduce #'+ times) (float count)))
                 (median (if (cl-oddp count)
                            (nth (/ count 2) times)
                          (/ (+ (nth (1- (/ count 2)) times)
                                (nth (/ count 2) times))
                             2.0))))
            (insert (format "Recent calls analyzed: %d\n" count))
            (insert (format "Mean response time: %.2fs\n" mean))
            (insert (format "Median response time: %.2fs\n" median))
            (insert (format "Min response time: %.2fs\n" (car times)))
            (insert (format "Max response time: %.2fs\n" (car (last times)))))))
      (insert "\n")

      ;; Cache Statistics
      (insert "Cache Performance\n")
      (insert "-----------------\n")
      (let ((hits (or (efrit--get-metric 'cache-hits) 0))
            (misses (or (efrit--get-metric 'cache-misses) 0)))
        (insert (format "Cache hits: %d\n" hits))
        (insert (format "Cache misses: %d\n" misses))
        (when (> (+ hits misses) 0)
          (let ((hit-rate (* 100.0 (/ (float hits) (+ hits misses)))))
            (insert (format "Hit rate: %.1f%%\n" hit-rate)))))
      (let ((cache-size (hash-table-count efrit-performance--request-cache)))
        (insert (format "Cached entries: %d\n" cache-size)))
      (insert "\n")

      ;; Token Usage
      (insert "Token Usage\n")
      (insert "-----------\n")
      (let ((token-data (efrit--get-metric 'token-usage)))
        (if token-data
            (let ((total-input 0)
                  (total-output 0))
              (dolist (entry token-data)
                (when (consp entry)
                  (cl-incf total-input (or (alist-get 'input entry) 0))
                  (cl-incf total-output (or (alist-get 'output entry) 0))))
              (insert (format "Total input tokens: %d\n" total-input))
              (insert (format "Total output tokens: %d\n" total-output))
              (insert (format "Total tokens: %d\n" (+ total-input total-output))))
          (insert "No token usage data recorded yet\n")))
      (insert "\n")

      ;; Session Statistics
      (insert "Session Management\n")
      (insert "------------------\n")
      (let ((active-sessions (hash-table-count efrit-performance--session-timestamps)))
        (insert (format "Active sessions: %d\n" active-sessions))
        (insert (format "Max sessions: %d\n" efrit-performance-max-sessions))
        (insert (format "Session TTL: %d seconds\n" efrit-performance-session-ttl)))
      (insert "\n")

      ;; Error Statistics
      (insert "Errors\n")
      (insert "------\n")
      (let ((error-count (or (efrit--get-metric 'errors) 0)))
        (insert (format "Total errors: %d\n" error-count)))
      (insert "\n")

      ;; Actions
      (insert "====================================\n")
      (insert "Actions:\n")
      (insert "  C-c r - Refresh dashboard\n")
      (insert "  C-c c - Clear metrics\n")
      (insert "  C-c C - Clear cache\n")
      (insert "  q     - Quit dashboard\n"))

    ;; Set up key bindings
    (local-set-key (kbd "C-c r") #'efrit-show-metrics)
    (local-set-key (kbd "C-c c") #'efrit--clear-metrics)
    (local-set-key (kbd "C-c C") #'efrit-performance-clear-cache)
    (local-set-key (kbd "q") #'quit-window)

    (setq buffer-read-only t)
    (goto-char (point-min))
    (display-buffer (current-buffer))))

;;; Initialize
(efrit-performance-start-cleanup-timer)

(provide 'efrit-performance)
;;; efrit-performance.el ends here