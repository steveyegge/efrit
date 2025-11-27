;;; efrit-tool-show-diff-preview.el --- Show diff preview for proposed changes -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Keywords: ai, tools, diff
;; Version: 0.3.1

;;; Commentary:
;;
;; Tool for showing proposed changes to the user before applying them.
;;
;; This enables a human-in-the-loop workflow where Claude can propose
;; changes and the user reviews them in a diff view before approval.
;;
;; Features:
;; - Unified diff display with diff-mode highlighting
;; - Support for new files, deletions, and modifications
;; - All-or-nothing or selective approval modes
;; - User can edit proposed changes before applying

;;; Code:

(require 'efrit-tool-utils)
(require 'diff-mode)
(require 'cl-lib)

;;; Customization

(defcustom efrit-diff-preview-buffer-name "*efrit-diff-preview*"
  "Name of the buffer for diff preview."
  :type 'string
  :group 'efrit-tool-utils)

(defcustom efrit-diff-preview-timeout-seconds 300
  "Timeout for user response in seconds (default 5 minutes)."
  :type 'integer
  :group 'efrit-tool-utils)

;;; Internal variables

(defvar efrit-diff-preview--changes nil
  "Current list of proposed changes being reviewed.")

(defvar efrit-diff-preview--result nil
  "Result of the diff preview interaction.")

(defvar efrit-diff-preview--waiting nil
  "Non-nil when waiting for user response.")

(defvar efrit-diff-preview--selected-indices nil
  "Indices of changes selected for application in selective mode.")

(defvar efrit-diff-preview--apply-mode nil
  "Current apply mode: 'all_or_nothing or 'selective.")

;;; Diff Generation

(defun efrit-diff-preview--generate-unified-diff (old-content new-content filename)
  "Generate a unified diff between OLD-CONTENT and NEW-CONTENT for FILENAME.
Returns the diff as a string."
  (let ((old-temp (make-temp-file "efrit-old-"))
        (new-temp (make-temp-file "efrit-new-")))
    (unwind-protect
        (progn
          (with-temp-file old-temp
            (insert (or old-content "")))
          (with-temp-file new-temp
            (insert (or new-content "")))
          ;; Use diff with unified format
          (with-temp-buffer
            (let ((exit-code (call-process "diff" nil t nil
                                           "-u"
                                           "--label" (format "a/%s" filename)
                                           "--label" (format "b/%s" filename)
                                           old-temp new-temp)))
              ;; diff returns 0 for identical, 1 for differences, 2 for error
              (if (>= exit-code 2)
                  (format "--- a/%s\n+++ b/%s\n@@ Error generating diff @@\n" filename filename)
                (buffer-string)))))
      ;; Clean up temp files
      (ignore-errors (delete-file old-temp))
      (ignore-errors (delete-file new-temp)))))

(defun efrit-diff-preview--format-change (change index)
  "Format a single CHANGE for display at INDEX."
  (let* ((file (alist-get 'file change))
         (old-content (alist-get 'old_content change))
         (new-content (alist-get 'new_content change))
         (change-type (cond
                       ((null old-content) "NEW FILE")
                       ((null new-content) "DELETED")
                       (t "MODIFIED"))))
    (concat
     (format "\n%s=== Change %d: %s (%s) %s\n"
             (make-string 60 ?=)
             (1+ index)
             file
             change-type
             (make-string (max 0 (- 60 (+ 15 (length file) (length change-type)))) ?=))
     (cond
      ;; New file - show all as additions
      ((null old-content)
       (concat "--- /dev/null\n"
               (format "+++ b/%s\n" file)
               "@@ -0,0 +1,"
               (format "%d" (1+ (cl-count ?\n new-content)))
               " @@\n"
               (mapconcat (lambda (line) (concat "+" line))
                          (split-string new-content "\n")
                          "\n")
               "\n"))
      ;; Deleted file - show all as deletions
      ((null new-content)
       (concat (format "--- a/%s\n" file)
               "+++ /dev/null\n"
               "@@ -1,"
               (format "%d" (1+ (cl-count ?\n old-content)))
               " +0,0 @@\n"
               (mapconcat (lambda (line) (concat "-" line))
                          (split-string old-content "\n")
                          "\n")
               "\n"))
      ;; Modified - generate proper diff
      (t
       (efrit-diff-preview--generate-unified-diff old-content new-content file))))))

;;; UI Mode

(defvar efrit-diff-preview-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") #'efrit-diff-preview-approve)
    (define-key map (kbd "y") #'efrit-diff-preview-approve)
    (define-key map (kbd "r") #'efrit-diff-preview-reject)
    (define-key map (kbd "n") #'efrit-diff-preview-reject)
    (define-key map (kbd "q") #'efrit-diff-preview-reject)
    (define-key map (kbd "SPC") #'efrit-diff-preview-toggle-change)
    (define-key map (kbd "RET") #'efrit-diff-preview-toggle-change)
    (define-key map (kbd "s") #'efrit-diff-preview-apply-selected)
    map)
  "Keymap for `efrit-diff-preview-mode'.")

(define-derived-mode efrit-diff-preview-mode diff-mode "Efrit-Diff"
  "Major mode for reviewing proposed changes from Efrit.

\\{efrit-diff-preview-mode-map}"
  (setq buffer-read-only t)
  (setq-local header-line-format
              '(:eval (efrit-diff-preview--header-line))))

(defun efrit-diff-preview--header-line ()
  "Generate header line for diff preview buffer."
  (let ((mode-str (if (eq efrit-diff-preview--apply-mode 'selective)
                      (format "SELECTIVE [%d/%d selected]"
                              (length efrit-diff-preview--selected-indices)
                              (length efrit-diff-preview--changes))
                    "ALL-OR-NOTHING")))
    (concat " Efrit Diff Preview | " mode-str
            " | [a]pprove [r]eject"
            (when (eq efrit-diff-preview--apply-mode 'selective)
              " [SPC]toggle [s]apply-selected"))))

;;; Interactive Commands

(defun efrit-diff-preview-approve ()
  "Approve the proposed changes."
  (interactive)
  (setq efrit-diff-preview--result
        `((approved . t)
          (selected_changes . ,(if (eq efrit-diff-preview--apply-mode 'selective)
                                   (vconcat efrit-diff-preview--selected-indices)
                                 (vconcat (number-sequence 0 (1- (length efrit-diff-preview--changes))))))
          (user_edits . :json-false)))
  (setq efrit-diff-preview--waiting nil)
  (message "Changes approved."))

(defun efrit-diff-preview-reject ()
  "Reject the proposed changes."
  (interactive)
  (setq efrit-diff-preview--result
        `((approved . :json-false)
          (selected_changes . [])
          (reason . "user_rejected")))
  (setq efrit-diff-preview--waiting nil)
  (message "Changes rejected."))

(defun efrit-diff-preview-toggle-change ()
  "Toggle selection of the change at point (in selective mode)."
  (interactive)
  (unless (eq efrit-diff-preview--apply-mode 'selective)
    (user-error "Toggle only available in selective mode"))
  (let ((index (efrit-diff-preview--change-at-point)))
    (if index
        (progn
          (if (memq index efrit-diff-preview--selected-indices)
              (setq efrit-diff-preview--selected-indices
                    (delq index efrit-diff-preview--selected-indices))
            (push index efrit-diff-preview--selected-indices))
          (force-mode-line-update)
          (message "Change %d %s" (1+ index)
                   (if (memq index efrit-diff-preview--selected-indices)
                       "selected" "deselected")))
      (message "No change at point"))))

(defun efrit-diff-preview-apply-selected ()
  "Apply only selected changes (in selective mode)."
  (interactive)
  (unless (eq efrit-diff-preview--apply-mode 'selective)
    (user-error "Apply-selected only available in selective mode"))
  (if (null efrit-diff-preview--selected-indices)
      (message "No changes selected. Use SPC to select changes.")
    (setq efrit-diff-preview--result
          `((approved . t)
            (selected_changes . ,(vconcat (sort efrit-diff-preview--selected-indices #'<)))
            (user_edits . :json-false)))
    (setq efrit-diff-preview--waiting nil)
    (message "Applied %d selected changes." (length efrit-diff-preview--selected-indices))))

(defun efrit-diff-preview--change-at-point ()
  "Return the index of the change at point, or nil if not on a change."
  (save-excursion
    (when (re-search-backward "^=+ Change \\([0-9]+\\):" nil t)
      (1- (string-to-number (match-string 1))))))

;;; Display Functions

(defun efrit-diff-preview--display (changes description apply-mode)
  "Display CHANGES with DESCRIPTION in preview buffer using APPLY-MODE."
  (let ((buffer (get-buffer-create efrit-diff-preview-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        ;; Header
        (insert "Efrit Proposed Changes\n")
        (insert (make-string 60 ?=) "\n\n")
        (insert (format "Description: %s\n" (or description "No description provided")))
        (insert (format "Mode: %s\n" (if (eq apply-mode 'selective)
                                         "Selective (choose which changes to apply)"
                                       "All-or-nothing")))
        (insert (format "Number of changes: %d\n" (length changes)))
        (insert "\nPress 'a' or 'y' to approve all, 'r' or 'n' to reject")
        (when (eq apply-mode 'selective)
          (insert "\nPress SPC to toggle selection, 's' to apply selected"))
        (insert "\n")
        ;; Each change
        (cl-loop for change in changes
                 for i from 0
                 do (insert (efrit-diff-preview--format-change change i)))
        ;; Footer
        (insert "\n" (make-string 60 ?=) "\n")
        (insert "END OF PROPOSED CHANGES\n"))
      (efrit-diff-preview-mode)
      (goto-char (point-min)))
    ;; Display buffer
    (pop-to-buffer buffer '((display-buffer-reuse-window
                             display-buffer-pop-up-window)
                            (window-height . 0.6)))
    buffer))

;;; Main Tool Function

(defun efrit-tool-show-diff-preview (args)
  "Show user proposed changes before applying them.

ARGS is an alist with:
  changes - list of proposed changes (required)
    Each change has: file, old_content, new_content
  description - what these changes accomplish
  apply_mode - 'all_or_nothing or 'selective (default: all_or_nothing)

Returns a standard tool response with:
  approved - boolean
  selected_changes - indices of changes to apply
  user_edits - any modifications made (currently not supported)"
  (efrit-tool-execute show_diff_preview args
    (let* ((changes (alist-get 'changes args))
           (description (alist-get 'description args))
           (apply-mode (or (alist-get 'apply_mode args) "all_or_nothing"))
           (apply-mode-sym (intern apply-mode)))

      ;; Validate
      (unless changes
        (signal 'user-error (list "changes list is required")))

      (when (zerop (length changes))
        (signal 'user-error (list "changes list cannot be empty")))

      ;; Convert vector to list if needed
      (when (vectorp changes)
        (setq changes (append changes nil)))

      ;; Initialize state
      (setq efrit-diff-preview--changes changes)
      (setq efrit-diff-preview--result nil)
      (setq efrit-diff-preview--waiting t)
      (setq efrit-diff-preview--apply-mode apply-mode-sym)
      (setq efrit-diff-preview--selected-indices
            (when (eq apply-mode-sym 'selective)
              ;; In selective mode, start with all selected
              (number-sequence 0 (1- (length changes)))))

      ;; Display the diff
      (efrit-diff-preview--display changes description apply-mode-sym)

      ;; Wait for user response (with timeout)
      (let ((start-time (float-time))
            (timeout efrit-diff-preview-timeout-seconds))
        (while (and efrit-diff-preview--waiting
                    (< (- (float-time) start-time) timeout))
          (sit-for 0.1)
          (redisplay))

        ;; Handle timeout
        (when efrit-diff-preview--waiting
          (setq efrit-diff-preview--result
                `((approved . :json-false)
                  (selected_changes . [])
                  (reason . "timeout")))
          (setq efrit-diff-preview--waiting nil)))

      ;; Clean up and return result
      (when-let* ((buffer (get-buffer efrit-diff-preview-buffer-name)))
        (kill-buffer buffer))

      (efrit-tool-success efrit-diff-preview--result))))

(provide 'efrit-tool-show-diff-preview)

;;; efrit-tool-show-diff-preview.el ends here
