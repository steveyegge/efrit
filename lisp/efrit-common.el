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

(defcustom efrit-api-key nil
  "Anthropic API key for Efrit.
This can be:
- nil: Use auth-source lookup (recommended)
- A string: The API key directly (not recommended for security)
- A symbol naming an environment variable (e.g. 'ANTHROPIC_API_KEY)
- A function that returns the API key"
  :type '(choice (const :tag "Use auth-source" nil)
                 (string :tag "API key string")
                 (symbol :tag "Environment variable name")
                 (function :tag "Function returning API key"))
  :group 'efrit)

(defcustom efrit-api-auth-source-host "api.anthropic.com"
  "Host to use for auth-source lookup of API key."
  :type 'string
  :group 'efrit)

(defcustom efrit-api-auth-source-user "personal"
  "User to use for auth-source lookup of API key."
  :type 'string
  :group 'efrit)

(defun efrit-common-get-api-key ()
  "Get the Anthropic API key using BYOK (Bring Your Own Key) system.
Tries in order:
1. `efrit-api-key' if set (direct string, env var, or function)
2. Environment variable ANTHROPIC_API_KEY
3. Auth-source lookup using `efrit-api-auth-source-host' and `efrit-api-auth-source-user'
Throws error if not found."
  (cond
   ;; Direct string API key
   ((stringp efrit-api-key)
    efrit-api-key)
   
   ;; Symbol naming an environment variable (but not nil)
   ((and (symbolp efrit-api-key) efrit-api-key)
    (or (getenv (symbol-name efrit-api-key))
        (error "Environment variable %s not set" efrit-api-key)))
   
   ;; Function that returns API key
   ((functionp efrit-api-key)
    (or (funcall efrit-api-key)
        (error "API key function returned nil")))
   
   ;; Try ANTHROPIC_API_KEY environment variable
   ((getenv "ANTHROPIC_API_KEY"))
   
   ;; Fall back to auth-source
   (t
    (let* ((auth-info (car (auth-source-search :host efrit-api-auth-source-host
                                               :user efrit-api-auth-source-user
                                               :require '(:secret))))
           (secret (when auth-info (plist-get auth-info :secret))))
      (if (and secret (functionp secret))
          (funcall secret)
        (error "No API key found. Try one of:
1. Set efrit-api-key variable
2. Set ANTHROPIC_API_KEY environment variable  
3. Add to ~/.authinfo: machine %s login %s password YOUR_KEY"
               efrit-api-auth-source-host efrit-api-auth-source-user))))))

(defconst efrit-common-api-url "https://api.anthropic.com/v1/messages"
  "Anthropic API endpoint for all efrit modules.")

(defconst efrit-common-api-version "2023-06-01" 
  "Anthropic API version for all requests.")

(defun efrit-common-build-headers (api-key)
  "Build standard HTTP headers using API-KEY."
  `(("Content-Type" . "application/json")
    ("anthropic-version" . ,efrit-common-api-version)
    ("x-api-key" . ,api-key)
    ("anthropic-beta" . "max-tokens-3-5-sonnet-2024-07-15")))

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
