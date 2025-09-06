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
                                (if context (efrit-common--truncate-string 
                                           (json-encode context) 1000)
                                  ""))))

(defun efrit-performance-get-cached (key)
  "Get cached response for KEY if still valid."
  (let ((entry (gethash key efrit-performance--request-cache)))
    (when (and entry
               (< (- (float-time) (efrit-cache-entry-timestamp entry))
                  efrit-performance-cache-ttl))
      (cl-incf (efrit-cache-entry-hit-count entry))
      (efrit-log 'debug "Cache hit for key %s (hits: %d)" 
                 (substring key 0 8)
                 (efrit-cache-entry-hit-count entry))
      (efrit-cache-entry-response entry))))

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

(defun efrit-performance-record-api-time (elapsed-time)
  "Record ELAPSED-TIME for performance monitoring."
  (push elapsed-time efrit-performance--api-times)
  ;; Keep only last 100 times
  (when (> (length efrit-performance--api-times) 100)
    (setq efrit-performance--api-times 
          (cl-subseq efrit-performance--api-times 0 100))))

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

;;; Initialize
(efrit-performance-start-cleanup-timer)

(provide 'efrit-performance)
;;; efrit-performance.el ends here