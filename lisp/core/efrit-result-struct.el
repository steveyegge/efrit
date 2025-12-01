;;; efrit-result-struct.el --- Result structure classes for Efrit -*- lexical-binding: t -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (eieio "1.4"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; This module provides EIEIO classes for result structures that are built
;; and extracted throughout the executor. It replaces magic plist keys with
;; type-safe, documented classes.
;;
;; Two main result types:
;; - efrit-tool-result: Result from a single tool execution
;; - efrit-content-result: Accumulated result from processing content items
;;
;; These classes provide:
;; - Type safety (slots are validated)
;; - Clear semantics (slot names are self-documenting)
;; - Easy conversion to/from plists for backward compatibility
;; - Builder functions for convenient construction

;;; Code:

(require 'eieio)

;;; Tool Result Class

(defclass efrit-tool-result ()
  ((result :initarg :result :type string :documentation "The result/output from the tool")
   (is-error :initarg :is-error :type boolean :initform nil :documentation "Whether the result is an error")
   (tool-id :initarg :tool-id :type string :documentation "ID of the tool that was called")
   (session-complete :initarg :session-complete :type boolean :initform nil :documentation "Whether session should end")
   (completion-message :initarg :completion-message :type (or string null) :initform nil :documentation "Message if session is completing")
   (work-log-entry :initarg :work-log-entry :type (or list null) :initform nil :documentation "Entry for work log (result input pair)"))
  :documentation "Represents the result from executing a single tool.")

;;; Tool Result Accessors

(cl-defmethod efrit-tool-result-get-result ((result efrit-tool-result))
  "Get the result/output from RESULT."
  (slot-value result 'result))

(cl-defmethod efrit-tool-result-is-error ((result efrit-tool-result))
  "Check if RESULT represents an error."
  (slot-value result 'is-error))

(cl-defmethod efrit-tool-result-get-tool-id ((result efrit-tool-result))
  "Get the tool ID from RESULT."
  (slot-value result 'tool-id))

(cl-defmethod efrit-tool-result-session-complete-p ((result efrit-tool-result))
  "Check if this RESULT indicates session should complete."
  (slot-value result 'session-complete))

(cl-defmethod efrit-tool-result-get-completion-message ((result efrit-tool-result))
  "Get the completion message from RESULT, or nil."
  (slot-value result 'completion-message))

(cl-defmethod efrit-tool-result-get-work-log-entry ((result efrit-tool-result))
  "Get the work log entry from RESULT, or nil."
  (slot-value result 'work-log-entry))

;;; Tool Result Builders

(defun efrit-tool-result-make (result &optional is-error tool-id session-complete completion-message work-log-entry)
  "Create an efrit-tool-result with the given fields.
RESULT is required; all others are optional with sensible defaults."
  (make-instance 'efrit-tool-result
    :result result
    :is-error (or is-error nil)
    :tool-id (or tool-id "")
    :session-complete (or session-complete nil)
    :completion-message completion-message
    :work-log-entry work-log-entry))

;;; Tool Result Conversion

(defun efrit-tool-result-to-plist (result)
  "Convert an efrit-tool-result to a plist for backward compatibility."
  (list :result (efrit-tool-result-get-result result)
        :is-error (efrit-tool-result-is-error result)
        :tool-id (efrit-tool-result-get-tool-id result)
        :session-complete (efrit-tool-result-session-complete-p result)
        :completion-message (efrit-tool-result-get-completion-message result)
        :work-log-entry (efrit-tool-result-get-work-log-entry result)))

(defun efrit-tool-result-from-plist (plist)
  "Create an efrit-tool-result from a plist (reverse of to-plist)."
  (make-instance 'efrit-tool-result
    :result (or (plist-get plist :result) "")
    :is-error (plist-get plist :is-error)
    :tool-id (or (plist-get plist :tool-id) "")
    :session-complete (plist-get plist :session-complete)
    :completion-message (plist-get plist :completion-message)
    :work-log-entry (plist-get plist :work-log-entry)))

;;; Content Result Class

(defclass efrit-content-result ()
  ((result-text :initarg :result-text :type string :initform "" :documentation "Accumulated text result")
   (tool-result-blocks :initarg :tool-result-blocks :type list :initform nil :documentation "List of tool_result blocks for API")
   (tool-results-for-work-log :initarg :tool-results-for-work-log :type list :initform nil :documentation "Simplified results for work log")
   (completion-message :initarg :completion-message :type (or string null) :initform nil :documentation "Completion message if session ending")
   (session-complete :initarg :session-complete :type boolean :initform nil :documentation "Whether session should complete"))
  :documentation "Represents accumulated results from processing content items.")

;;; Content Result Accessors

(cl-defmethod efrit-content-result-get-result-text ((result efrit-content-result))
  "Get the accumulated text result from RESULT."
  (slot-value result 'result-text))

(cl-defmethod efrit-content-result-get-tool-result-blocks ((result efrit-content-result))
  "Get the list of tool_result blocks from RESULT."
  (slot-value result 'tool-result-blocks))

(cl-defmethod efrit-content-result-get-work-log-results ((result efrit-content-result))
  "Get the simplified results for work log from RESULT."
  (slot-value result 'tool-results-for-work-log))

(cl-defmethod efrit-content-result-get-completion-message ((result efrit-content-result))
  "Get the completion message from RESULT, or nil."
  (slot-value result 'completion-message))

(cl-defmethod efrit-content-result-session-complete-p ((result efrit-content-result))
  "Check if this RESULT indicates session should complete."
  (slot-value result 'session-complete))

;;; Content Result Builders

(defun efrit-content-result-make (result-text &optional tool-result-blocks work-log-results completion-message session-complete)
  "Create an efrit-content-result with the given fields.
RESULT-TEXT is required; all others are optional."
  (make-instance 'efrit-content-result
    :result-text (or result-text "")
    :tool-result-blocks (or tool-result-blocks nil)
    :tool-results-for-work-log (or work-log-results nil)
    :completion-message completion-message
    :session-complete (or session-complete nil)))

;;; Content Result Conversion

(defun efrit-content-result-to-plist (result)
  "Convert an efrit-content-result to a plist for backward compatibility."
  (list :result-text (efrit-content-result-get-result-text result)
        :tool-result-blocks (efrit-content-result-get-tool-result-blocks result)
        :tool-results-for-work-log (efrit-content-result-get-work-log-results result)
        :completion-message (efrit-content-result-get-completion-message result)
        :session-complete (efrit-content-result-session-complete-p result)))

(defun efrit-content-result-from-plist (plist)
  "Create an efrit-content-result from a plist (reverse of to-plist)."
  (make-instance 'efrit-content-result
    :result-text (or (plist-get plist :result-text) "")
    :tool-result-blocks (plist-get plist :tool-result-blocks)
    :tool-results-for-work-log (plist-get plist :tool-results-for-work-log)
    :completion-message (plist-get plist :completion-message)
    :session-complete (plist-get plist :session-complete)))

(provide 'efrit-result-struct)

;;; efrit-result-struct.el ends here
