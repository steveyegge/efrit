;;; efrit-content-processor.el --- Content block processing abstraction -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; Abstracts the processing of API response content blocks using EIEIO classes.
;;
;; API responses contain vectors of content blocks (text, tool_use, etc).
;; This module provides:
;; - EIEIO classes for typed content blocks
;; - Factory function to parse raw hash-table items into typed objects
;; - Validators for type-safe handling
;; - Helper functions for converting to/from hash-tables

;;; Code:

(require 'cl-lib)
(require 'eieio)
(require 'efrit-chat-response)

;;; Content Block Base Class

(defclass efrit-content-block ()
  ((type :initarg :type :accessor efrit-content-block-type
         :type string
         :documentation "Block type: `text' or `tool_use'.")
   (timestamp :initarg :timestamp :accessor efrit-content-block-timestamp
              :initform nil
              :documentation "When this block was created (unused)."))
  :abstract t
  :documentation "Base class for content blocks from API responses.")

;;; Text Block

(defclass efrit-text-block (efrit-content-block)
  ((text :initarg :text :accessor efrit-text-block-text
         :type string
         :documentation "Text content."))
  :documentation "A text content block from Claude.")

(defun efrit-text-block-make (text)
  "Create a text block with TEXT."
  (efrit-text-block :type "text" :text text))

;;; Tool Use Block

(defclass efrit-tool-use-block (efrit-content-block)
  ((tool-id :initarg :tool-id :accessor efrit-tool-use-block-id
            :type string
            :documentation "Unique ID for this tool call.")
   (tool-name :initarg :tool-name :accessor efrit-tool-use-block-name
              :type string
              :documentation "Name of the tool being called.")
   (tool-input :initarg :tool-input :accessor efrit-tool-use-block-input
               :type hash-table
               :documentation "Input arguments for the tool."))
  :documentation "A tool_use content block from Claude.")

(defun efrit-tool-use-block-make (tool-id tool-name tool-input)
  "Create a tool use block with TOOL-ID, TOOL-NAME, and TOOL-INPUT."
  (efrit-tool-use-block :type "tool_use"
                        :tool-id tool-id
                        :tool-name tool-name
                        :tool-input tool-input))

;;; Tool Result Block

(defclass efrit-tool-result-block (efrit-content-block)
  ((tool-use-id :initarg :tool-use-id :accessor efrit-tool-result-block-tool-id
                :type string
                :documentation "ID of the tool_use block this result is for.")
   (content :initarg :content :accessor efrit-tool-result-block-content
            :type string
            :documentation "Result content from tool execution.")
   (is-error :initarg :is-error :accessor efrit-tool-result-block-is-error
             :type boolean
             :initform nil
             :documentation "Whether this is an error result."))
  :documentation "A tool_result block for sending back to the API.")

(defun efrit-tool-result-block-make (tool-use-id content &optional is-error)
  "Create a tool result block.
TOOL-USE-ID links back to the original tool_use.
CONTENT is the result string.
IS-ERROR indicates if this is an error result."
  (efrit-tool-result-block :type "tool_result"
                           :tool-use-id tool-use-id
                           :content content
                           :is-error (or is-error nil)))

;;; Factory Function

(defun efrit-content-block-from-hash (item)
  "Parse ITEM hash-table from API response into typed block object.
Returns: efrit-content-block subclass or nil if invalid."
  (when (hash-table-p item)
    (let ((type (efrit-content-item-type item)))
      (cond
       ((string= type "text")
        (when-let* ((text (efrit-content-item-text item)))
          (efrit-text-block-make text)))
       
       ((string= type "tool_use")
        (when-let* ((result (efrit-content-item-as-tool-use item)))
          (apply #'efrit-tool-use-block-make result)))
       
       (t nil)))))

;;; Validators

(defun efrit-content-block-valid-p (block)
  "Check if BLOCK is a valid content block."
  (and (eieio-object-p block)
       (object-of-class-p block 'efrit-content-block)))

(defun efrit-text-block-valid-p (block)
  "Check if BLOCK is a valid text block."
  (and (eieio-object-p block)
       (object-of-class-p block 'efrit-text-block)
       (stringp (efrit-text-block-text block))
       (not (string-empty-p (efrit-text-block-text block)))))

(defun efrit-tool-use-block-valid-p (block)
  "Check if BLOCK is a valid tool_use block."
  (and (eieio-object-p block)
       (object-of-class-p block 'efrit-tool-use-block)
       (stringp (efrit-tool-use-block-id block))
       (not (string-empty-p (efrit-tool-use-block-id block)))
       (stringp (efrit-tool-use-block-name block))
       (not (string-empty-p (efrit-tool-use-block-name block)))
       (hash-table-p (efrit-tool-use-block-input block))))

(defun efrit-tool-result-block-valid-p (block)
  "Check if BLOCK is a valid tool_result block."
  (and (eieio-object-p block)
       (object-of-class-p block 'efrit-tool-result-block)
       (stringp (efrit-tool-result-block-tool-id block))
       (not (string-empty-p (efrit-tool-result-block-tool-id block)))
       (stringp (efrit-tool-result-block-content block))))

;;; Conversion to/from Hash-Table

(defun efrit-content-block-to-hash (block)
  "Convert typed BLOCK back to hash-table for API compatibility."
  (let ((hash (make-hash-table :test 'equal)))
    (puthash "type" (efrit-content-block-type block) hash)
    (cond
     ((efrit-text-block-p block)
      (puthash "text" (efrit-text-block-text block) hash))
     ((efrit-tool-use-block-p block)
      (puthash "id" (efrit-tool-use-block-id block) hash)
      (puthash "name" (efrit-tool-use-block-name block) hash)
      (puthash "input" (efrit-tool-use-block-input block) hash))
     ((efrit-tool-result-block-p block)
      (puthash "tool_use_id" (efrit-tool-result-block-tool-id block) hash)
      (puthash "content" (efrit-tool-result-block-content block) hash)))
    hash))

(provide 'efrit-content-processor)
;;; efrit-content-processor.el ends here
