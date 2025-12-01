;;; efrit-metrics.el --- Session metrics EIEIO class -*- lexical-binding: t -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (eieio "1.4"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; This module provides an EIEIO class for session metrics, replacing the
;; ad-hoc alist-based metrics structure in efrit-session-metrics.el.
;;
;; Key features:
;; - Type-safe metric storage with slot validation
;; - Clear, self-documenting slot names
;; - Accessor methods with sensible defaults
;; - Easy conversion to/from alists for backward compatibility
;; - Display helpers for UI integration

;;; Code:

(require 'eieio)

;;; Session Metrics Class

(defclass efrit-metrics ()
  ((commands-executed :initarg :commands-executed :type integer :initform 0
                     :documentation "Number of commands executed")
   (todos-created :initarg :todos-created :type integer :initform 0
                 :documentation "Number of TODOs created")
   (todos-completed :initarg :todos-completed :type integer :initform 0
                   :documentation "Number of TODOs completed")
   (api-calls :initarg :api-calls :type integer :initform 0
             :documentation "Number of API calls made")
   (api-errors :initarg :api-errors :type integer :initform 0
              :documentation "Number of API errors encountered")
   (errors-encountered :initarg :errors-encountered :type integer :initform 0
                      :documentation "Total errors encountered")
   (buffers-created :initarg :buffers-created :type list :initform nil
                   :documentation "List of buffer names created")
   (files-modified :initarg :files-modified :type list :initform nil
                  :documentation "List of file paths modified")
   (tools-used :initarg :tools-used :type list :initform nil
              :documentation "Alist of (tool-name . count) pairs")
   (session-id :initarg :session-id :type (or string null) :initform nil
              :documentation "Session identifier")
   (start-time :initarg :start-time :type (or t null) :initform nil
              :documentation "Session start time")
   (duration :initarg :duration :type number :initform 0
            :documentation "Session duration in seconds"))
  :documentation "Represents metrics from an Efrit session.")

;;; Metric Accessors

(cl-defmethod efrit-metrics-get-commands-executed ((metrics efrit-metrics))
  "Get count of commands executed."
  (slot-value metrics 'commands-executed))

(cl-defmethod efrit-metrics-get-todos-created ((metrics efrit-metrics))
  "Get count of TODOs created."
  (slot-value metrics 'todos-created))

(cl-defmethod efrit-metrics-get-todos-completed ((metrics efrit-metrics))
  "Get count of TODOs completed."
  (slot-value metrics 'todos-completed))

(cl-defmethod efrit-metrics-get-todos-active ((metrics efrit-metrics))
  "Get count of active TODOs (created - completed)."
  (max 0 (- (slot-value metrics 'todos-created)
            (slot-value metrics 'todos-completed))))

(cl-defmethod efrit-metrics-get-api-calls ((metrics efrit-metrics))
  "Get count of API calls made."
  (slot-value metrics 'api-calls))

(cl-defmethod efrit-metrics-get-api-errors ((metrics efrit-metrics))
  "Get count of API errors."
  (slot-value metrics 'api-errors))

(cl-defmethod efrit-metrics-get-errors ((metrics efrit-metrics))
  "Get total error count."
  (slot-value metrics 'errors-encountered))

(cl-defmethod efrit-metrics-get-buffers-created ((metrics efrit-metrics))
  "Get list of buffers created."
  (slot-value metrics 'buffers-created))

(cl-defmethod efrit-metrics-get-files-modified ((metrics efrit-metrics))
  "Get list of files modified."
  (slot-value metrics 'files-modified))

(cl-defmethod efrit-metrics-get-tools-used ((metrics efrit-metrics))
  "Get alist of tools used with counts."
  (slot-value metrics 'tools-used))

(cl-defmethod efrit-metrics-get-duration ((metrics efrit-metrics))
  "Get session duration in seconds."
  (slot-value metrics 'duration))

(cl-defmethod efrit-metrics-get-session-id ((metrics efrit-metrics))
  "Get the session ID."
  (slot-value metrics 'session-id))

;;; Builder Functions

(defun efrit-metrics-make (&optional commands-executed todos-created todos-completed
                                     api-calls api-errors errors-encountered
                                     buffers-created files-modified tools-used
                                     session-id start-time duration)
  "Create an efrit-metrics object with the given values.
All parameters are optional with sensible defaults."
  (make-instance 'efrit-metrics
    :commands-executed (or commands-executed 0)
    :todos-created (or todos-created 0)
    :todos-completed (or todos-completed 0)
    :api-calls (or api-calls 0)
    :api-errors (or api-errors 0)
    :errors-encountered (or errors-encountered 0)
    :buffers-created (or buffers-created nil)
    :files-modified (or files-modified nil)
    :tools-used (or tools-used nil)
    :session-id session-id
    :start-time start-time
    :duration (or duration 0)))

(defun efrit-metrics-from-alist (alist)
  "Create an efrit-metrics from an alist (from legacy metrics)."
  (make-instance 'efrit-metrics
    :commands-executed (or (alist-get 'commands-executed alist) 0)
    :todos-created (or (alist-get 'todos-created alist) 0)
    :todos-completed (or (alist-get 'todos-completed alist) 0)
    :api-calls (or (alist-get 'api-calls alist) 0)
    :api-errors (or (alist-get 'api-errors alist) 0)
    :errors-encountered (or (alist-get 'errors-encountered alist) 0)
    :buffers-created (or (alist-get 'buffers-created alist) nil)
    :files-modified (or (alist-get 'files-modified alist) nil)
    :tools-used (or (alist-get 'tools-used alist) nil)))

;;; Conversion to Alist (for backward compatibility)

(defun efrit-metrics-to-alist (metrics)
  "Convert an efrit-metrics to an alist for backward compatibility."
  (list (cons 'commands-executed (efrit-metrics-get-commands-executed metrics))
        (cons 'todos-created (efrit-metrics-get-todos-created metrics))
        (cons 'todos-completed (efrit-metrics-get-todos-completed metrics))
        (cons 'api-calls (efrit-metrics-get-api-calls metrics))
        (cons 'api-errors (efrit-metrics-get-api-errors metrics))
        (cons 'errors-encountered (efrit-metrics-get-errors metrics))
        (cons 'buffers-created (efrit-metrics-get-buffers-created metrics))
        (cons 'files-modified (efrit-metrics-get-files-modified metrics))
        (cons 'tools-used (efrit-metrics-get-tools-used metrics))))

;;; Display Helpers

(defun efrit-metrics-format-for-dashboard (metrics)
  "Format METRICS for display in the dashboard."
  (let* ((commands (efrit-metrics-get-commands-executed metrics))
         (todos-created (efrit-metrics-get-todos-created metrics))
         (todos-completed (efrit-metrics-get-todos-completed metrics))
         (todos-active (efrit-metrics-get-todos-active metrics))
         (api-calls (efrit-metrics-get-api-calls metrics))
         (duration (efrit-metrics-get-duration metrics)))
    (format (concat "Commands: %d | TODOs: %d created, %d completed, %d active | "
                   "API calls: %d | Duration: %.1fs")
           commands
           todos-created
           todos-completed
           todos-active
           api-calls
           duration)))

;;; Statistics

(cl-defmethod efrit-metrics-total-actions ((metrics efrit-metrics))
  "Calculate total number of actions taken."
  (+ (efrit-metrics-get-commands-executed metrics)
     (efrit-metrics-get-api-calls metrics)
     (efrit-metrics-get-todos-created metrics)))

(cl-defmethod efrit-metrics-error-rate ((metrics efrit-metrics))
  "Calculate error rate as a percentage of total actions."
  (let* ((total (efrit-metrics-total-actions metrics))
         (errors (efrit-metrics-get-errors metrics)))
    (if (> total 0)
        (/ (* errors 100.0) total)
      0)))

(cl-defmethod efrit-metrics-unique-files-modified ((metrics efrit-metrics))
  "Get count of unique files modified."
  (length (efrit-metrics-get-files-modified metrics)))

(cl-defmethod efrit-metrics-unique-buffers-created ((metrics efrit-metrics))
  "Get count of unique buffers created."
  (length (efrit-metrics-get-buffers-created metrics)))

(cl-defmethod efrit-metrics-unique-tools-used ((metrics efrit-metrics))
  "Get count of unique tools used."
  (length (efrit-metrics-get-tools-used metrics)))

(cl-defmethod efrit-metrics-most-used-tool ((metrics efrit-metrics))
  "Get the most frequently used tool, or nil if no tools used."
  (let* ((tools (efrit-metrics-get-tools-used metrics))
         (sorted (sort (copy-sequence tools)
                      (lambda (a b) (> (cdr a) (cdr b))))))
    (when sorted (caar sorted))))

(provide 'efrit-metrics)

;;; efrit-metrics.el ends here
