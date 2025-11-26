;;; efrit-tool-confirm-action.el --- User confirmation for destructive actions -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Keywords: ai, tools
;; Version: 0.3.0

;;; Commentary:
;;
;; Tool for explicit user confirmation before destructive operations.
;;
;; Provides a `confirm_action` tool that:
;; - Pauses the session to wait for user confirmation
;; - Supports severity levels: info, warning, danger
;; - Danger mode requires typing 'yes' (not just y/n)
;; - Logs all confirmations to audit trail
;; - Supports timeout handling
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

;;; Customization

(defcustom efrit-confirm-default-timeout 300
  "Default timeout in seconds for confirmation prompts (5 minutes)."
  :type 'integer
  :group 'efrit-tool-utils)

(defcustom efrit-confirm-log-file "~/.emacs.d/.efrit/logs/confirmations.log"
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
        (append-to-file (point-min) (point-max) file)))))

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

(defun efrit-confirm--build-prompt (action details severity options)
  "Build the confirmation prompt string.
ACTION is the description of what will happen.
DETAILS is optional list of items affected.
SEVERITY is 'info, 'warning, or 'danger.
OPTIONS is list of choices (default [\"Yes\", \"No\"])."
  (let ((severity-prefix
         (pcase severity
           ('danger "⚠️  DANGER: ")
           ('warning "⚠️  ")
           (_ "")))
        (details-text (efrit-confirm--format-details details))
        (options-list (or options '("Yes" "No"))))
    (concat
     severity-prefix
     action
     (when details-text
       (concat "\n\n" details-text))
     "\n\n"
     (if (eq severity 'danger)
         "Type 'yes' to confirm: "
       (format "(%s) " (mapconcat #'identity options-list "/"))))))

;;; Confirmation Functions

(defun efrit-confirm--get-response-info (action severity)
  "Prompt user for confirmation for INFO severity.
ACTION is the description. Uses simple y-or-n-p."
  (let ((start-time (current-time))
        (confirmed (y-or-n-p (format "%s " action))))
    (list :confirmed confirmed
          :choice (if confirmed "Yes" "No")
          :response_time_seconds (float-time (time-subtract (current-time) start-time)))))

(defun efrit-confirm--get-response-warning (action details options)
  "Prompt user for confirmation for WARNING severity.
ACTION is the description, DETAILS is list of items, OPTIONS is choices."
  (let* ((start-time (current-time))
         (prompt (efrit-confirm--build-prompt action details 'warning options))
         (options-list (or options '("Yes" "No")))
         (choice (completing-read prompt options-list nil t)))
    (list :confirmed (string= (downcase choice) "yes")
          :choice choice
          :response_time_seconds (float-time (time-subtract (current-time) start-time)))))

(defun efrit-confirm--get-response-danger (action details)
  "Prompt user for confirmation for DANGER severity.
ACTION is the description, DETAILS is list of items.
Requires typing 'yes' explicitly."
  (let* ((start-time (current-time))
         (prompt (efrit-confirm--build-prompt action details 'danger nil))
         (response (read-string prompt)))
    (list :confirmed (string= (downcase response) "yes")
          :choice response
          :response_time_seconds (float-time (time-subtract (current-time) start-time)))))

(defun efrit-confirm--prompt-user (action details severity options timeout)
  "Prompt user for confirmation with timeout handling.
Returns response plist with :confirmed, :choice, :response_time_seconds, :reason."
  (let ((timeout-seconds (or timeout efrit-confirm-default-timeout)))
    (condition-case nil
        (with-timeout (timeout-seconds
                       (list :confirmed nil
                             :choice nil
                             :reason "timeout"
                             :response_time_seconds timeout-seconds))
          (pcase severity
            ('danger (efrit-confirm--get-response-danger action details))
            ('warning (efrit-confirm--get-response-warning action details options))
            (_ (efrit-confirm--get-response-info action severity))))
      (quit
       ;; User pressed C-g
       (list :confirmed nil
             :choice nil
             :reason "user_cancelled"
             :response_time_seconds 0)))))

;;; Main Tool Implementation

(defun efrit-tool-confirm-action (args)
  "Request explicit user confirmation before a destructive operation.

ARGS is an alist with:
  action - short description of what will happen (required)
  details - list of specific items affected (optional)
  severity - 'info' | 'warning' | 'danger' (default: 'warning')
  options - custom choices (default: ['Yes', 'No'])
  timeout_seconds - how long to wait (default: 300)

Returns a standard tool response with confirmation result."
  (efrit-tool-execute confirm_action args
    (let* ((action (alist-get 'action args))
           (details (alist-get 'details args))
           (severity-str (or (alist-get 'severity args) "warning"))
           (severity (intern severity-str))
           (options (alist-get 'options args))
           (timeout (alist-get 'timeout_seconds args)))

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
      (let ((response (efrit-confirm--prompt-user action details severity options timeout)))
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
           ;; Rejected or timeout
           `((confirmed . :json-false)
             (choice . ,(or (plist-get response :choice) :null))
             (reason . ,(or (plist-get response :reason) "user_rejected")))))))))

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
