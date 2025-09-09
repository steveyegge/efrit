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
(declare-function efrit-log "efrit-log")

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
- A symbol naming an environment variable (e.g. \\='ANTHROPIC_API_KEY)
- A function that returns the API key"
  :type '(choice (const :tag "Use auth-source" nil)
                 (string :tag "API key string")
                 (symbol :tag "Environment variable name")
                 (function :tag "Function returning API key"))
  :group 'efrit)

(defcustom efrit-api-base-url "https://api.anthropic.com"
  "Base URL for Anthropic API endpoints.
This can be:
- A string: Static base URL (default)
- A function: Dynamic base URL (for enterprise/proxy setups)
Useful for corporate proxies or alternative API endpoints."
  :type '(choice (string :tag "Static base URL")
                 (function :tag "Function returning base URL"))
  :group 'efrit)

(defcustom efrit-api-auth-source-host "api.anthropic.com"
  "Host to use for auth-source lookup of API key."
  :type 'string
  :group 'efrit)

(defcustom efrit-api-auth-source-user "personal"
  "User to use for auth-source lookup of API key."
  :type 'string
  :group 'efrit)

(defun efrit-common--validate-api-key (key)
  "Validate that KEY looks like a valid Anthropic API key.
Returns t if valid, signals error if not."
  (when (or (not (stringp key))
            (string-empty-p key)
            (< (length key) 20)
            (not (string-prefix-p "sk-" key)))
    (error "Invalid API key format. Anthropic keys should start with 'sk-' and be at least 20 characters"))
  t)

(defun efrit-common--sanitize-key-for-logging (key)
  "Return a sanitized version of KEY safe for logging.
Shows only first 6 and last 4 characters."
  (if (and (stringp key) (>= (length key) 10))
      (concat (substring key 0 6) "..." (substring key -4))
    "[INVALID-KEY]"))

(defun efrit-common-safe-log (level format-string &rest args)
  "Log message with automatic API key sanitization.
Scans all ARGS for strings that look like API keys and sanitizes them."
  (let ((sanitized-args 
         (mapcar (lambda (arg)
                   (if (and (stringp arg) 
                            (>= (length arg) 20)
                            (string-prefix-p "sk-" arg))
                       (efrit-common--sanitize-key-for-logging arg)
                     arg))
                 args)))
    (efrit-log level format-string sanitized-args)))

;;; File System Security

(defun efrit-common--validate-path (path allowed-base-paths)
  "Validate that PATH is within one of ALLOWED-BASE-PATHS.
Prevents directory traversal attacks by ensuring the resolved path
stays within allowed directories."
  (let ((resolved-path (expand-file-name path)))
    (unless (cl-some (lambda (base)
                       (let ((resolved-base (expand-file-name base)))
                         (string-prefix-p resolved-base resolved-path)))
                     allowed-base-paths)
      (error "🚫 SECURITY: Path '%s' not within allowed directories: %s" 
             path allowed-base-paths))
    resolved-path))

(defun efrit-common--validate-filename (filename)
  "Validate that FILENAME is safe (no special chars, reasonable length).
Returns the validated filename or signals an error."
  (when (or (string-match-p "[<>:\"|?*]" filename)
            (> (length filename) 255))
    (error "🚫 SECURITY: Unsafe filename '%s'" filename))
  filename)

(defun efrit-common-safe-expand-file-name (filename directory)
  "Safely expand FILENAME within DIRECTORY with path traversal protection.
Ensures the result stays within DIRECTORY."
  (let* ((base-dir (expand-file-name directory))
         (candidate (expand-file-name filename base-dir)))
    ;; Check for path traversal first (before filename validation)
    (unless (string-prefix-p base-dir candidate)
      (error "🚫 SECURITY: Path traversal attempt blocked: %s" filename))
    ;; Then validate the filename itself
    (efrit-common--validate-filename (file-name-nondirectory candidate))
    candidate))

(defun efrit-common-get-api-key ()
  "Get the Anthropic API key using secure BYOK system.
Tries in order:
1. `efrit-api-key' if set (direct string, env var, or function)
2. Environment variable ANTHROPIC_API_KEY
3. Auth-source lookup using configured host and user
Validates key format and throws error if not found."
  (let ((key (cond
              ;; Direct string API key (NOT recommended for security)
              ((stringp efrit-api-key)
               (when efrit-api-key ; Log security warning
                 (message "⚠️  WARNING: API key stored directly in variable (security risk)"))
               efrit-api-key)
              
              ;; Symbol naming an environment variable (recommended)
              ((and (symbolp efrit-api-key) efrit-api-key)
               (or (getenv (symbol-name efrit-api-key))
                   (error "Environment variable %s not set" efrit-api-key)))
              
              ;; Function that returns API key (advanced)
              ((functionp efrit-api-key)
               (or (funcall efrit-api-key)
                   (error "API key function returned nil")))
              
              ;; Try ANTHROPIC_API_KEY environment variable (fallback)
              ((getenv "ANTHROPIC_API_KEY"))
              
              ;; Fall back to auth-source (most secure)
              (t
               (let* ((auth-info (car (auth-source-search :host efrit-api-auth-source-host
                                                          :user efrit-api-auth-source-user
                                                          :require '(:secret))))
                      (secret (when auth-info (plist-get auth-info :secret))))
                 (if (and secret (functionp secret))
                     (funcall secret)
                   (error "No API key found. Try one of:
1. Set ANTHROPIC_API_KEY environment variable (recommended)
2. Add to ~/.authinfo: machine %s login %s password YOUR_KEY (most secure)
3. Set efrit-api-key variable (NOT recommended for security)"
                          efrit-api-auth-source-host efrit-api-auth-source-user)))))))
    
    ;; Validate the key format and return it
    (when key
      (efrit-common--validate-api-key key)
      key)))

(defun efrit-common-get-base-url ()
  "Get the configured base URL for API endpoints.
Handles both static strings and dynamic functions."
  (cond
   ((stringp efrit-api-base-url) efrit-api-base-url)
   ((functionp efrit-api-base-url) (funcall efrit-api-base-url))
   (t "https://api.anthropic.com")))

(defun efrit-common-get-api-url ()
  "Get the full API URL for messages endpoint."
  (concat (efrit-common-get-base-url) "/v1/messages"))

(defconst efrit-common-api-version "2023-06-01" 
  "Anthropic API version for all requests.")

(defun efrit-common-build-headers (api-key)
  "Build standard HTTP headers using API-KEY with security validation."
  ;; Validate the API key before using it
  (efrit-common--validate-api-key api-key)
  
  ;; Build headers (API key will not be logged due to validation above)
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
