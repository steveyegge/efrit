;;; efrit-ui-dashboard.el --- Dashboard UI for Efrit -*- lexical-binding: t -*-

;; Copyright (C) 2025 Steven Yegge

;; Author: Steven Yegge
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Dashboard interface for session and TODO management, plus metrics visualization.

;;; Code:

(require 'efrit-ui-performance)
(require 'json)
(require 'seq)
(require 'cl-lib)

(defvar efrit-data-directory)

(defgroup efrit-dashboard nil
  "Dashboard interface for Efrit AI assistant."
  :group 'efrit
  :prefix "efrit-dashboard-")

(defconst efrit-dashboard-buffer-name "*efrit-dashboard*"
  "Name of the efrit dashboard buffer.")

(defcustom efrit-dashboard-auto-refresh-interval 5
  "Seconds between dashboard auto-refreshes."
  :type 'number
  :group 'efrit-dashboard)

(defcustom efrit-dashboard-use-unicode-symbols t
  "Whether to use Unicode symbols in dashboard headings.
When nil, uses ASCII alternatives for better TTY compatibility."
  :type 'boolean
  :group 'efrit-dashboard)

(defvar efrit-dashboard-refresh-timer nil
  "Timer for auto-refreshing the dashboard.")

(defvar efrit-dashboard-session-state
  '((start-time . nil)
    (commands-executed . 0)
    (todos-completed . 0)
    (todos-active . 0)
    (buffers-created . ())
    (files-modified . ())
    (api-calls . 0)
    (last-activity . nil))
  "Current session state for dashboard display.")

;;; Dashboard Mode

(defvar efrit-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") 'efrit-dashboard-refresh)
    (define-key map (kbd "q") 'efrit-dashboard-quit)
    (define-key map (kbd "c") 'efrit-dashboard-clear-todos)
    (define-key map (kbd "l") 'efrit-dashboard-show-log)
    (define-key map (kbd "s") 'efrit-dashboard-save-session)
    (define-key map (kbd "TAB") 'efrit-dashboard-next-section)
    (define-key map (kbd "S-TAB") 'efrit-dashboard-prev-section)
    (define-key map (kbd "RET") 'efrit-dashboard-action-at-point)
    map)
  "Keymap for efrit dashboard mode.")

(define-derived-mode efrit-dashboard-mode special-mode "Efrit-Dashboard"
  "Major mode for the Efrit Dashboard.

\\{efrit-dashboard-mode-map}"
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (hl-line-mode 1))

;;; Dashboard Core Functions

;;;###autoload
(defun efrit-dashboard ()
  "Open or refresh the Efrit Dashboard."
  (interactive)
  (let ((buffer (get-buffer-create efrit-dashboard-buffer-name)))
    (with-current-buffer buffer
      (efrit-dashboard-mode)
      (efrit-dashboard-refresh)
      (efrit-dashboard-setup-auto-refresh))
    (display-buffer buffer)
    (select-window (get-buffer-window buffer))))

(defun efrit-dashboard-refresh ()
  "Refresh the dashboard content."
  (interactive)
  (with-current-buffer (get-buffer-create efrit-dashboard-buffer-name)
    (let ((inhibit-read-only t)
          (point-pos (point)))
      (erase-buffer)
      (efrit-dashboard-insert-header)
      (efrit-dashboard-insert-session-state)
      (efrit-dashboard-insert-todo-panel)
      (efrit-dashboard-insert-queue-status)
      (efrit-dashboard-insert-recent-activity)
      (efrit-dashboard-insert-quick-actions)
      (goto-char (min point-pos (point-max))))))

(defun efrit-dashboard-insert-header ()
  "Insert the dashboard header."
  (insert (propertize "EFRIT DASHBOARD" 'face 'bold 'font-lock-face 'bold) "\n")
  (insert (format "Session started: %s\n"
                  (or (cdr (assoc 'start-time efrit-dashboard-session-state))
                      (format-time-string "%Y-%m-%d %H:%M:%S"))))
  (insert (make-string 50 ?═) "\n\n"))

(defun efrit-dashboard--insert-section-header (title icon)
  "Insert section header with TITLE and optional ICON."
  (let ((header-text (if (and efrit-dashboard-use-unicode-symbols icon)
                         (format "%s %s" icon title)
                       title)))
    (insert (propertize (format "### %s" header-text)
                        'face 'bold
                        'efrit-section-header t)
            "\n")))

(defun efrit-dashboard-insert-session-state ()
  "Insert session state panel."
  (efrit-dashboard--insert-section-header "SESSION STATE" "📊")

  ;; Try to get data from session tracker if available
  (let ((session-data (when (fboundp 'efrit-session-get-summary)
                        (efrit-session-get-summary)))
        (dashboard-state efrit-dashboard-session-state))

    (if session-data
        ;; Use session tracker data
        (let ((metrics (plist-get session-data :metrics)))
          (insert (format "• Commands executed: %d\n"
                          (or (cdr (assoc 'commands-executed metrics)) 0)))
          (insert (format "• TODOs created: %d\n"
                          (or (cdr (assoc 'todos-created metrics)) 0)))
          (insert (format "• TODOs completed: %d\n"
                          (or (cdr (assoc 'todos-completed metrics)) 0)))
          (insert (format "• API calls made: %d\n"
                          (or (cdr (assoc 'api-calls metrics)) 0)))
          (insert (format "• Session duration: %.1fs\n"
                          (or (plist-get session-data :duration) 0))))

      ;; Fall back to dashboard state
      (insert (format "• Commands executed: %d\n"
                      (cdr (assoc 'commands-executed dashboard-state))))
      (insert (format "• TODOs completed: %d\n"
                      (cdr (assoc 'todos-completed dashboard-state))))
      (insert (format "• TODOs active: %d\n"
                      (cdr (assoc 'todos-active dashboard-state))))
      (insert (format "• API calls made: %d\n"
                      (cdr (assoc 'api-calls dashboard-state))))
      (insert (format "• Last activity: %s\n"
                      (or (cdr (assoc 'last-activity dashboard-state)) "None")))))
  (insert "\n"))

(defun efrit-dashboard-insert-todo-panel ()
  "Insert TODO management panel."
  (efrit-dashboard--insert-section-header "TODO MANAGEMENT" "📋")

  ;; Get TODOs from current session if available
  (let* ((todos-file (expand-file-name "todos.json"
                                       (expand-file-name "context" efrit-data-directory)))
         (todos (when (file-exists-p todos-file)
                  (efrit-dashboard-read-json-file todos-file))))

    (if todos
        (progn
          ;; Active TODOs
          (insert (propertize "Active TODOs:" 'face 'font-lock-keyword-face) "\n")
          (let ((active-count 0))
            (dolist (todo todos)
              (let ((status (cdr (assoc 'status todo)))
                    (priority (cdr (assoc 'priority todo)))
                    (content (cdr (assoc 'content todo))))
                (when (not (string= status "completed"))
                  (setq active-count (1+ active-count))
                  (insert (format "  %s [%s] %s\n"
                                  (if (string= status "in-progress") "⟳" "☐")
                                  (upcase priority)
                                  content)))))
            (when (= active-count 0)
              (insert "  (No active TODOs)\n")))

          ;; Completed TODOs (last 3)
          (insert "\n" (propertize "Recently Completed:" 'face 'font-lock-keyword-face) "\n")
          (let ((completed-todos (seq-filter (lambda (todo)
                                               (string= (cdr (assoc 'status todo)) "completed"))
                                             todos))
                (shown-count 0))
            (if completed-todos
                (dolist (todo (seq-take completed-todos 3))
                  (insert (format "  ☑ %s\n" (cdr (assoc 'content todo))))
                  (setq shown-count (1+ shown-count)))
              (insert "  (No completed TODOs)\n"))))

      (insert "  (No TODO data available)\n")))
  (insert "\n"))

(defun efrit-dashboard-insert-queue-status ()
  "Insert AI communication queue status."
  (efrit-dashboard--insert-section-header "QUEUE STATUS" "🔗")

  (let* ((queues-dir (expand-file-name "queues" efrit-data-directory))
         (requests-dir (expand-file-name "requests" queues-dir))
         (processing-dir (expand-file-name "processing" queues-dir))
         (responses-dir (expand-file-name "responses" queues-dir)))

    (insert (format "• Pending requests: %d\n"
                    (if (file-directory-p requests-dir)
                        (length (directory-files requests-dir nil "^[^.]"))
                        0)))
    (insert (format "• Processing: %d\n"
                    (if (file-directory-p processing-dir)
                        (length (directory-files processing-dir nil "^[^.]"))
                        0)))
    (insert (format "• Completed responses: %d\n"
                    (if (file-directory-p responses-dir)
                        (length (directory-files responses-dir nil "^[^.]"))
                        0))))
  (insert "\n"))

(defun efrit-dashboard-insert-recent-activity ()
  "Insert recent activity log."
  (efrit-dashboard--insert-section-header "RECENT ACTIVITY" "🔍")

  (let* ((log-file (expand-file-name "efrit.log"
                                     (expand-file-name "logs" efrit-data-directory))))
    (if (file-exists-p log-file)
        (with-temp-buffer
          (insert-file-contents log-file nil nil nil t)
          (goto-char (point-max))
          (forward-line -3) ; Show last 3 lines
          (let ((recent-log (buffer-substring-no-properties (point) (point-max))))
            (if (string-blank-p recent-log)
                (insert "  (No recent activity)\n")
              (insert "  " (replace-regexp-in-string "\n" "\n  " recent-log)))))
      (insert "  (No log file found)\n")))
  (insert "\n"))

(defun efrit-dashboard-insert-quick-actions ()
  "Insert quick action buttons."
  (efrit-dashboard--insert-section-header "QUICK ACTIONS" "⚡")
  (insert "  g - Refresh dashboard\n")
  (insert "  c - Clear completed TODOs\n")
  (insert "  l - Show full log\n")
  (insert "  s - Save session state\n")
  (insert "  q - Quit dashboard\n"))

;;; Dashboard Helper Functions

(defun efrit-dashboard-read-json-file (file)
  "Read and parse JSON from FILE.
Returns parsed data structure on success, nil if file doesn't exist,
or :malformed if JSON parsing fails."
  (cond
   ((not (file-exists-p file)) nil)
   (t
    (with-temp-buffer
      (insert-file-contents file)
      (condition-case err
          (let ((json-array-type 'list)      ; Parse arrays as lists
                (json-object-type 'alist))   ; Parse objects as alists
            (json-read-from-string (buffer-string)))
        (error
         (message "Warning: Malformed JSON in %s: %s" file (error-message-string err))
         :malformed))))))

(defun efrit-dashboard-update-session-state (key value)
  "Update session state KEY with VALUE."
  (setf (alist-get key efrit-dashboard-session-state) value)
  (setf (alist-get 'last-activity efrit-dashboard-session-state)
        (format-time-string "%H:%M:%S")))

(defun efrit-dashboard-increment-counter (key)
  "Increment session state counter KEY."
  (cl-incf (alist-get key efrit-dashboard-session-state 0))
  (setf (alist-get 'last-activity efrit-dashboard-session-state)
        (format-time-string "%H:%M:%S")))

;;; Dashboard Auto-refresh

(defun efrit-dashboard-setup-auto-refresh ()
  "Setup auto-refresh timer for the dashboard."
  (when efrit-dashboard-refresh-timer
    (cancel-timer efrit-dashboard-refresh-timer))
  (setq efrit-dashboard-refresh-timer
        (run-with-timer efrit-dashboard-auto-refresh-interval
                        efrit-dashboard-auto-refresh-interval
                        'efrit-dashboard-refresh-if-visible)))

(defun efrit-dashboard-refresh-if-visible ()
  "Refresh dashboard only if it's currently visible."
  (when (and (buffer-live-p (get-buffer efrit-dashboard-buffer-name))
             (get-buffer-window efrit-dashboard-buffer-name))
    (efrit-dashboard-refresh)))

;;; Dashboard Interactive Commands

(defun efrit-dashboard-quit ()
  "Quit the dashboard and clean up."
  (interactive)
  (when efrit-dashboard-refresh-timer
    (cancel-timer efrit-dashboard-refresh-timer)
    (setq efrit-dashboard-refresh-timer nil))
  (quit-window t))

(defun efrit-dashboard-clear-todos ()
  "Clear completed TODOs."
  (interactive)
  (let ((todos-file (expand-file-name "todos.json"
                                     (expand-file-name "context" efrit-data-directory))))
    (when (file-exists-p todos-file)
      (let* ((todos (efrit-dashboard-read-json-file todos-file))
             (active-todos (seq-filter (lambda (todo)
                                        (not (string= (cdr (assoc 'status todo)) "completed")))
                                      todos)))
        (with-temp-file todos-file
          (insert (json-encode active-todos)))))
    (efrit-dashboard-refresh)
    (message "Cleared completed TODOs")))

(defun efrit-dashboard-show-log ()
  "Show the full efrit log."
  (interactive)
  (let ((log-file (expand-file-name "efrit.log"
                                   (expand-file-name "logs" efrit-data-directory))))
    (if (file-exists-p log-file)
        (find-file-other-window log-file)
      (message "No log file found"))))

(defun efrit-dashboard-save-session ()
  "Save current session state."
  (interactive)
  (let ((session-file (expand-file-name "current-session.json"
                                       (expand-file-name "sessions" efrit-data-directory))))
    (make-directory (file-name-directory session-file) t)
    (with-temp-file session-file
      (insert (json-encode efrit-dashboard-session-state)))
    (message "Session state saved")))

(defun efrit-dashboard-next-section ()
  "Navigate to next section."
  (interactive)
  (let ((pos (next-single-property-change (point) 'efrit-section-header)))
    (when pos
      (goto-char pos))))

(defun efrit-dashboard-prev-section ()
  "Navigate to previous section."
  (interactive)
  (let ((pos (previous-single-property-change (point) 'efrit-section-header)))
    (when pos
      (goto-char pos))))

(defun efrit-dashboard-action-at-point ()
  "Perform action at current point."
  (interactive)
  (message "Action at point not implemented yet"))

;;; Dashboard Integration Hooks

(defun efrit-dashboard-track-command ()
  "Track command execution for dashboard."
  (efrit-dashboard-increment-counter 'commands-executed))

(defun efrit-dashboard-track-todo-completion ()
  "Track TODO completion for dashboard."
  (efrit-dashboard-increment-counter 'todos-completed))

(defun efrit-dashboard-track-api-call ()
  "Track API call for dashboard."
  (efrit-dashboard-increment-counter 'api-calls))

;;; ========================================================================
;;; Metrics Dashboard
;;; Comprehensive performance metrics visualization
;;; ========================================================================

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

;;; Initialization

;; Initialize session state
(unless (cdr (assoc 'start-time efrit-dashboard-session-state))
  (efrit-dashboard-update-session-state 'start-time
                                       (format-time-string "%Y-%m-%d %H:%M:%S")))

(provide 'efrit-ui-dashboard)

;;; efrit-ui-dashboard.el ends here
