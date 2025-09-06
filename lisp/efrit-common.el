;;; efrit-common.el --- Shared utilities and API functions for efrit -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Keywords: ai, tools, common
;; Version: 0.3.0

;;; Commentary:
;; Common utilities shared across efrit modules to eliminate code duplication.
;; Includes API key management, HTTP utilities, and shared constants.

;;; Code:

(require 'auth-source)
;; efrit-log will be required by modules that need it

;;; Core Group Definition

(defgroup efrit nil
  "Efrit AI coding assistant for Emacs."
  :group 'tools
  :prefix "efrit-")

;;; API Configuration

(defun efrit-common-get-api-key ()
  "Get the Anthropic API key from .authinfo file.
Checks efrit-api-channel to determine which key to use.
Throws error if not found."
  (let* ((channel (and (boundp 'efrit-api-channel) efrit-api-channel))
         (host (if (equal channel "ai-efrit")
                   "api.anthropic.com/sourcegraph"
                 "api.anthropic.com"))
         (user (if (equal channel "ai-efrit")
                   "apikey"
                 "personal"))
         (auth-info (car (auth-source-search :host host
                                            :user user
                                            :require '(:secret))))
         (secret (when auth-info (plist-get auth-info :secret))))
    (if (and secret (functionp secret))
        (funcall secret)
      (error "No API key found. Add to ~/.authinfo: machine %s login %s password YOUR_KEY" host user))))

(defconst efrit-common-api-url "https://api.anthropic.com/v1/messages"
  "Anthropic API endpoint for all efrit modules.")

(defconst efrit-common-api-version "2023-06-01" 
  "Anthropic API version for all requests.")

(defun efrit-common-build-headers (api-key)
  "Build standard HTTP headers using API-KEY."
  `(("Content-Type" . "application/json")
    ("anthropic-version" . ,efrit-common-api-version)
    ("x-api-key" . ,api-key)
    ("anthropic-beta" . "max-tokens-3-5-sonnet-2024-07-15")
    ("x-channel" . ,(or (and (boundp 'efrit-api-channel) efrit-api-channel) "default"))))

;;; Error Handling

(defun efrit-common-safe-error-message (err)
  "Extract safe error message from ERR object."
  (cond
   ((stringp err) err)
   ((and (consp err) (stringp (cadr err))) (cadr err))
   ((error-message-string err))
   (t "Unknown error")))

;;; Utilities

(defun efrit-common-truncate-string (str max-length)
  "Truncate STR to MAX-LENGTH characters with ellipsis if needed."
  (if (> (length str) max-length)
      (concat (substring str 0 max-length) "...")
    str))

(defun efrit-common-escape-json-unicode (json-string)
  "Escape unicode characters in JSON-STRING for HTTP transmission.
This prevents multibyte encoding errors when sending to APIs."
  (replace-regexp-in-string 
   "[^\x00-\x7F]" 
   (lambda (char)
     (format "\\\\u%04X" (string-to-char char)))
   json-string))

(provide 'efrit-common)

;;; efrit-common.el ends here
