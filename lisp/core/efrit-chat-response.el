;;; efrit-chat-response.el --- API response accessors for Claude API -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; Centralizes access patterns for Claude API response structures.
;; 
;; The Anthropic API returns deeply nested hash tables. Rather than
;; repeatedly using gethash patterns throughout the executor, this module
;; provides typed accessors that:
;; - Document the API contract in one place
;; - Handle missing fields gracefully with sensible defaults
;; - Make response handling testable
;; - Reduce mental friction when reading code

;;; Code:

(require 'cl-lib)

;;; API Error Handling

(defun efrit-response-error (response)
  "Extract error object from API RESPONSE, or nil if successful.
Returns: error object hash-table or nil."
  (when (hash-table-p response)
    (gethash "error" response)))

(defun efrit-error-type (error-obj)
  "Get the type string from ERROR-OBJ.
Common types: `invalid_request_error', `authentication_error', `rate_limit_error'.
Returns type string or `unknown'."
  (if (hash-table-p error-obj)
      (or (gethash "type" error-obj) "unknown")
    "unknown"))

(defun efrit-error-message (error-obj)
  "Get the message string from ERROR-OBJ.
Returns message string or `unknown error'."
  (if (hash-table-p error-obj)
      (or (gethash "message" error-obj) "unknown error")
    "unknown error"))

;;; Response Content

(defun efrit-response-content (response)
  "Extract content vector from API RESPONSE.
Content is a vector of content blocks (text, tool_use, etc.)
Returns: vector or nil if missing."
  (when (hash-table-p response)
    (gethash "content" response)))

(defun efrit-response-stop-reason (response)
  "Extract stop_reason from API RESPONSE.
Common values: `end_turn' (finished normally), `max_tokens' (token limit).
Returns string or nil."
  (when (hash-table-p response)
    (gethash "stop_reason" response)))

(defun efrit-response-usage (response)
  "Extract usage object from API RESPONSE containing token counts.
Returns: usage object hash-table or nil."
  (when (hash-table-p response)
    (gethash "usage" response)))

(defun efrit-usage-input-tokens (usage-obj)
  "Get input token count from USAGE-OBJ.
Returns: positive integer or 0 if missing."
  (if (hash-table-p usage-obj)
      (or (gethash "input_tokens" usage-obj) 0)
    0))

(defun efrit-usage-output-tokens (usage-obj)
  "Get output token count from USAGE-OBJ.
Returns: positive integer or 0 if missing."
  (if (hash-table-p usage-obj)
      (or (gethash "output_tokens" usage-obj) 0)
    0))

;;; Content Block Accessors

(defun efrit-content-item-type (item)
  "Get the type of content ITEM.
Types: `text', `tool_use'.
Returns type string or nil."
  (when (hash-table-p item)
    (gethash "type" item)))

(defun efrit-content-item-text (item)
  "Extract text content from ITEM.
Only present for text content blocks.
Returns: string or nil."
  (when (and (hash-table-p item)
             (string= (efrit-content-item-type item) "text"))
    (gethash "text" item)))

(defun efrit-content-item-as-tool-use (item)
  "Parse ITEM as a tool_use block.
Returns: (tool-id tool-name input-hash) or nil if not a tool_use block."
  (when (and (hash-table-p item)
             (string= (efrit-content-item-type item) "tool_use"))
    (list (gethash "id" item)
          (gethash "name" item)
          (gethash "input" item))))

;;; Tool Use Accessors

(defun efrit-tool-use-id (tool-use-item)
  "Get the ID of a tool_use ITEM.
This ID is used in tool_result blocks to link results back.
Returns: string or nil."
  (when (hash-table-p tool-use-item)
    (gethash "id" tool-use-item)))

(defun efrit-tool-use-name (tool-use-item)
  "Get the tool name from a tool_use ITEM.
Returns: string or nil."
  (when (hash-table-p tool-use-item)
    (gethash "name" tool-use-item)))

(defun efrit-tool-use-input (tool-use-item)
  "Get the input object from a tool_use ITEM.
Returns: hash-table or nil."
  (when (hash-table-p tool-use-item)
    (gethash "input" tool-use-item)))

;;; Helper for API Error Classification

(defun efrit-classify-error (error-obj)
  "Classify an ERROR-OBJ for user-friendly error handling.
Returns: one of (:rate-limit :auth :invalid :server :unknown)"
  (let ((type (efrit-error-type error-obj)))
    (cond
     ((string-match-p "rate_limit" type) :rate-limit)
     ((string-match-p "auth" type) :auth)
     ((string-match-p "invalid" type) :invalid)
     ((string-match-p "api_error" type) :server)
     (t :unknown))))

(provide 'efrit-chat-response)
;;; efrit-chat-response.el ends here
