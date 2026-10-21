;;; efrit-tool-confirm-action.el --- User confirmation for destructive actions -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Keywords: ai, tools
;; Version: 0.12.12

;;; Commentary:
;;
;; Tool for explicit user confirmation before destructive operations.
;;
;; Provides a `confirm_action` tool that:
;; - Pauses the session to wait for user confirmation
;; - Supports severity levels: info, warning, danger
;; - Danger mode requires typing 'yes' (not just y/n)
;; - Logs all confirmations to audit trail
;; - Waits for the user; no timeout
;;
;; Difference from request_user_input:
;; - Structured for confirmation workflow (not open questions)
;; - Severity levels with visual treatment
;; - Built-in audit logging
;; - Timeout handling

;;; Code:

(require 'efrit-tool-utils)
(require 'cl-lib)
(require 'json)
(require 'efrit-events)

;;; Customization

;; There is no timeout on a confirmation.  A prompt is not a hung
;; request: answering after five minutes must not be a rejection
;; (2026-09-27), and the model's own timeout_seconds (it kept passing
;; 300) turned a six-minute absence into a "timeout" it read as the
;; install having failed (2026-10-20).  `efrit-with-user-waiting'
;; already suspends every enclosing `with-timeout' for the prompt's
;; duration; nothing here starts another.

(defcustom efrit-confirm-log-file
  (expand-file-name "logs/confirmations.log" efrit-data-directory)
  "File path for confirmation audit log."
  :type 'file
  :group 'efrit-tool-utils)

(defcustom efrit-confirm-enable-logging t
  "When non-nil, log all confirmation requests and responses."
  :type 'boolean
  :group 'efrit-tool-utils)

;;; Internal State

(defvar efrit-confirm--pending nil
  "When non-nil, holds the pending confirmation request.
Plist with :action, :details, :severity, :options, :timeout, :start-time, :request-id.")

(defvar efrit-confirm--last-response nil
  "Last confirmation response, for debugging.")

;;; Audit Logging

(defun efrit-confirm--log-to-file (entry)
  "Write ENTRY to the confirmation audit log file."
  (when efrit-confirm-enable-logging
    (let ((file (expand-file-name efrit-confirm-log-file)))
      (make-directory (file-name-directory file) t)
      (with-temp-buffer
        (insert (format "%s | %s | severity=%s | choice=%s | response_time=%.2fs | action=%s\n"
                        (plist-get entry :timestamp)
                        (if (plist-get entry :confirmed) "CONFIRMED" "REJECTED")
                        (plist-get entry :severity)
                        (plist-get entry :choice)
                        (or (plist-get entry :response_time_seconds) 0)
                        (plist-get entry :action)))
        (write-region (point-min) (point-max) file t 'quiet)))))

(defun efrit-confirm--log (action severity confirmed choice response-time &optional reason)
  "Log a confirmation event to audit trail.
ACTION is what was being confirmed.
SEVERITY is 'info, 'warning, or 'danger.
CONFIRMED is t if user confirmed, nil if rejected.
CHOICE is the user's choice string.
RESPONSE-TIME is seconds taken to respond.
REASON is optional reason for rejection (e.g., 'timeout, 'user_rejected)."
  (let ((entry (list :timestamp (efrit-tool-format-time nil)
                     :action action
                     :severity (symbol-name severity)
                     :confirmed confirmed
                     :choice choice
                     :response_time_seconds response-time
                     :reason reason)))
    ;; Log to file
    (efrit-confirm--log-to-file entry)
    ;; Also use standard tool audit if available
    (when (fboundp 'efrit-tool-audit)
      (efrit-tool-audit 'confirm_action
                        `((action . ,action) (severity . ,severity))
                        (if confirmed :success :blocked)
                        response-time))))

;;; UI Helpers

(defun efrit-confirm--format-details (details)
  "Format DETAILS list for display."
  (when (and details (> (length details) 0))
    (let ((items (if (> (length details) 10)
                     (append (seq-take details 10)
                             (list (format "... %d more ..." (- (length details) 10))))
                   details)))
      (mapconcat (lambda (item) (format "  - %s" item))
                 items
                 "\n"))))

(declare-function efrit-choice-read "efrit-choice")

(defun efrit-confirm--prompt-user (action details severity options)
  "Ask the user with efrit's chooser; wait as long as they take.
Returns response plist with :confirmed, :choice, :response_time_seconds, :reason.
One menu for every severity (2026-10-21: the info prompt was a bare
`y-or-n-p' in the echo area).  info/warning: y and n, or the caller's
OPTIONS as keys; danger: the yes is spelled out and the key is Y, so
a reflex y does not confirm it."
  (require 'efrit-choice)
  (let* ((start-time (current-time))
         (options-list (or options '("Yes" "No")))
         (items (cond
                 ((eq severity 'danger)
                  '(("Y" "YES, do it (destructive)" "Yes") ("n" "no" "No")))
                 ((equal options-list '("Yes" "No"))
                  '(("y" "yes" "Yes") ("n" "no" "No")))
                 (t (mapcar (lambda (o) (list nil o o)) options-list))))
         (detail (concat (pcase severity ('danger "DANGER: destructive, not undoable\n") ('warning "warning\n") (_ ""))
                         (or (efrit-confirm--format-details details) "")))
         (got (efrit-choice-read action items :detail (string-trim detail) :allow-typed nil
                                 :label "a confirmation"))
         (choice (pcase got
                   ('cancelled nil)
                   (`(:typed . ,s) s)
                   (s s))))
    (list :confirmed (and (stringp choice) (string= (downcase choice) "yes"))
          :choice choice
          :reason (and (null choice) "user_cancelled")
          :response_time_seconds (float-time (time-subtract (current-time) start-time)))))

;;; Main Tool Implementation

(defun efrit-tool-confirm-action (args)
  "Request explicit user confirmation before a destructive operation.

ARGS is an alist with:
  action - short description of what will happen (required)
  details - list of specific items affected (optional)
  severity - 'info' | 'warning' | 'danger' (default: 'warning')
  options - custom choices (default: ['Yes', 'No'])
  timeout_seconds - accepted and ignored: the prompt waits for the user

Returns a standard tool response with confirmation result."
  (efrit-tool-execute confirm_action args
    (let* ((action (alist-get 'action args))
           (details (alist-get 'details args))
           (severity-str (or (alist-get 'severity args) "warning"))
           (severity (intern severity-str))
           (options (alist-get 'options args)))

      ;; Validate required fields
      (unless action
        (signal 'user-error (list "action is required")))

      ;; Validate severity
      (unless (memq severity '(info warning danger))
        (signal 'user-error
                (list (format "Invalid severity '%s'. Must be: info, warning, danger"
                              severity-str))))

      ;; Convert details vector to list if needed
      (when (vectorp details)
        (setq details (append details nil)))

      ;; Convert options vector to list if needed
      (when (vectorp options)
        (setq options (append options nil)))

      ;; Prompt the user
      (let ((response (efrit-confirm--prompt-user action details severity options)))
        ;; Log the confirmation
        (efrit-confirm--log action severity
                           (plist-get response :confirmed)
                           (plist-get response :choice)
                           (plist-get response :response_time_seconds)
                           (plist-get response :reason))

        ;; Store for debugging
        (setq efrit-confirm--last-response response)

        ;; Build result
        (efrit-tool-success
         (if (plist-get response :confirmed)
             ;; Confirmed
             `((confirmed . t)
               (choice . ,(plist-get response :choice))
               (response_time_seconds . ,(plist-get response :response_time_seconds)))
           ;; Rejected or cancelled; say so in words
           `((confirmed . :json-false)
             (choice . ,(or (plist-get response :choice) :null))
             (reason . ,(or (plist-get response :reason) "user_rejected"))
             (note . ,(if (equal (plist-get response :reason) "user_cancelled")
                          "The user cancelled the prompt (C-g); nothing was run."
                        "The user declined; nothing was run.")))))))))

;;; Interactive Testing

(defun efrit-confirm-action-test ()
  "Interactive test for confirm_action tool."
  (interactive)
  (let ((result (efrit-tool-confirm-action
                 '((action . "Delete 15 test files")
                   (details . ("file1.txt" "file2.txt" "file3.txt"))
                   (severity . "warning")))))
    (message "Result: %S" result)))

(defun efrit-confirm-action-test-danger ()
  "Interactive test for confirm_action with danger severity."
  (interactive)
  (let ((result (efrit-tool-confirm-action
                 '((action . "This will permanently delete your entire project")
                   (details . ("src/" "test/" "docs/" "README.md"))
                   (severity . "danger")))))
    (message "Result: %S" result)))

(provide 'efrit-tool-confirm-action)

;;; efrit-tool-confirm-action.el ends here
