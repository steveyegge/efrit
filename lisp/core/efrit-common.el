;;; efrit-common.el --- Shared utilities and API functions for efrit -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Keywords: ai, tools, common
;; Version: 0.4.1

;;; Commentary:
;; Common utilities shared across efrit modules to eliminate code duplication.
;; Includes API key management, HTTP utilities, and shared constants.

;;; Code:

(require 'auth-source)
;; efrit-log will be required by modules that need it
(declare-function efrit-log "efrit-log")
(declare-function efrit-log-debug "efrit-log")
(declare-function efrit-log-error "efrit-log")
;; efrit-config for data directory in health check
(defvar efrit-data-directory)

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

    ;;; Error Message Formatting

    ;; All error messages returned to Claude should use these helpers for consistency.
    ;; This ensures Claude can reliably parse and understand errors across all tools.
    ;; Format: [Error: CATEGORY] message or 🚫 SECURITY: message for security issues.

    (defun efrit-format-error (category message &rest args)
    "Format an error message with consistent structure.
    CATEGORY is a brief category name (e.g., 'API', 'Security', 'Validation').
    MESSAGE is the error message (can contain format specifiers).
    ARGS are format arguments for MESSAGE.

    Returns a formatted string suitable for returning to Claude as a tool result."
    (let ((formatted-msg (if args (apply #'format message args) message)))
    (format "\n[Error: %s] %s" category formatted-msg)))

    (defun efrit-format-validation-error (field-name &optional details)
    "Format a validation error for missing or invalid field.
    FIELD-NAME is the name of the field that failed validation.
    DETAILS is optional additional context."
    (if details
     (format "\n[Error: Validation] Field '%s' is invalid: %s" field-name details)
    (format "\n[Error: Validation] Field '%s' is required" field-name)))

    (defun efrit-format-security-error (message &rest args)
    "Format a security error with warning marker.
    MESSAGE is the error message (can contain format specifiers).
    ARGS are format arguments for MESSAGE."
    (let ((formatted-msg (if args (apply #'format message args) message)))
    (format "🚫 SECURITY: %s" formatted-msg)))

    (defun efrit-format-tool-error (tool-name message)
    "Format an error from tool execution.
    TOOL-NAME is the name of the tool that failed.
    MESSAGE is the error message."
    (format "\n[Error: %s] %s" tool-name message))

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
      (error "%s" (efrit-format-security-error "Path '%s' not within allowed directories: %s"
                                               path allowed-base-paths)))
    resolved-path))

(defun efrit-common--validate-filename (filename)
  "Validate that FILENAME is safe (no special chars, reasonable length).
Returns the validated filename or signals an error."
  (when (or (string-match-p "[<>:\"|?*]" filename)
            (> (length filename) 255))
    (error "%s" (efrit-format-security-error "Unsafe filename '%s'" filename)))
  filename)

(defun efrit-common-safe-expand-file-name (filename directory)
  "Safely expand FILENAME within DIRECTORY with path traversal protection.
Ensures the result stays within DIRECTORY."
  (let* ((base-dir (expand-file-name directory))
         (candidate (expand-file-name filename base-dir)))
    ;; Check for path traversal first (before filename validation)
    (unless (string-prefix-p base-dir candidate)
      (error "%s" (efrit-format-security-error "Path traversal attempt blocked: %s" filename)))
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

(defun efrit-common-truncate-string (str max-length &optional ellipsis-in-max)
  "Truncate STR to MAX-LENGTH characters with ellipsis if needed.
When ELLIPSIS-IN-MAX is non-nil, the returned string including
ellipsis will be at most MAX-LENGTH characters."
  (if (> (length str) max-length)
      (let ((cut-at (if ellipsis-in-max (- max-length 3) max-length)))
        (concat (substring str 0 (max 0 cut-at)) "..."))
    str))

(defun efrit-common-escape-json-unicode (json-string)
  "Escape unicode characters in JSON-STRING for HTTP transmission.
This prevents multibyte encoding errors when sending to APIs.
Converts non-ASCII characters to JSON unicode escapes (\\uXXXX)."
  (replace-regexp-in-string
   "[^\x00-\x7F]"
   (lambda (char)
     (format "\\u%04X" (string-to-char char)))
   json-string
   nil    ; FIXEDCASE - preserve case
   t))    ; LITERAL - don't interpret \& and \N in replacement

;;; Error Recovery

(defun efrit--safe-execute (func &optional context recovery-hint)
  "Execute FUNC with comprehensive error handling.

CONTEXT is an optional string describing what operation is being performed,
used in error messages and logging.

RECOVERY-HINT is an optional string providing guidance on how to recover
from errors, displayed to the user.

Returns a cons cell (SUCCESS . RESULT):
- If successful: (t . return-value-of-func)
- If error: (nil . error-message-string)

Example usage:
  (let ((result (efrit--safe-execute
                 (lambda () (some-risky-operation))
                 \"loading configuration\"
                 \"Check that ~/.efrit/config.el exists and is readable\")))
    (if (car result)
        (message \"Success: %s\" (cdr result))
      (message \"Failed: %s\" (cdr result))))"
  (require 'efrit-log)
  (let ((ctx (or context "operation"))
        (start-time (current-time)))
    (condition-case err
        (progn
          (efrit-log-debug "Starting %s" ctx)
          (let ((result (funcall func)))
            (efrit-log-debug "Completed %s in %.2fs"
                           ctx
                           (float-time (time-subtract (current-time) start-time)))
            (cons t result)))

      ;; File errors
      (file-error
       (let* ((error-msg (efrit-common-safe-error-message err))
              (msg (if recovery-hint
                       (format "File error during %s: %s\nRecovery: %s" ctx error-msg recovery-hint)
                     (format "File error during %s: %s" ctx error-msg))))
         (efrit-log-error "%s" msg)
         (when recovery-hint
           (message "Error: File error during %s: %s\nRecovery: %s" ctx error-msg recovery-hint))
         (cons nil (format "File error during %s: %s" ctx error-msg))))

      ;; Buffer errors
      (buffer-read-only
       (let ((msg (format "Buffer read-only during %s" ctx)))
         (efrit-log-error "%s" msg)
         (when recovery-hint
           (message "Error: %s\nRecovery: %s" msg recovery-hint))
         (cons nil msg)))

      ;; API/network errors
      (error
       (let* ((error-string (efrit-common-safe-error-message err))
              (msg (cond
                    ;; API key errors
                    ((string-match-p "\\(api[- ]?key\\|authentication\\|401\\)"
                                   (downcase error-string))
                     (format "API authentication failed during %s: %s\nCheck your API key configuration (M-x efrit-config-api-key)"
                            ctx error-string))

                    ;; Network errors
                    ((string-match-p "\\(network\\|connection\\|timeout\\|dns\\)"
                                   (downcase error-string))
                     (format "Network error during %s: %s\nCheck your internet connection"
                            ctx error-string))

                    ;; Rate limiting
                    ((string-match-p "\\(rate\\|limit\\|429\\)"
                                   (downcase error-string))
                     (format "Rate limit exceeded during %s: %s\nWait a moment before retrying"
                            ctx error-string))

                    ;; Generic error
                    (t
                     (format "Error during %s: %s" ctx error-string))))
              (log-msg (if recovery-hint
                          (format "%s\nRecovery: %s" msg recovery-hint)
                        msg)))
         (efrit-log-error "%s" log-msg)
         (when recovery-hint
           (message "%s\nRecovery: %s" msg recovery-hint))
         (cons nil msg))))))

;;; Health Check

;;;###autoload
(defun efrit-doctor ()
  "Run health checks on Efrit configuration and environment.
Validates: API key, data directories, module loading, and basic connectivity."
  (interactive)
  (require 'efrit-config)
  (let ((results '())
        (all-ok t))

    ;; Check 1: API key
    (message "Checking API key...")
    (let ((key-check (efrit--safe-execute
                      (lambda () (efrit-common-get-api-key))
                      "API key check")))
      (if (car key-check)
          (push "✓ API key is configured and accessible" results)
        (setq all-ok nil)
        (push (format "✗ API key check failed: %s" (cdr key-check)) results)))

    ;; Check 2: Data directory
    (message "Checking data directory...")
    (let ((dir efrit-data-directory))
      (if (and dir (file-directory-p (expand-file-name dir)))
          (progn
            (push (format "✓ Data directory exists: %s" dir) results)
            ;; Check write permissions
            (if (file-writable-p (expand-file-name dir))
                (push "✓ Data directory is writable" results)
              (setq all-ok nil)
              (push (format "✗ Data directory not writable: %s" dir) results)))
        (setq all-ok nil)
        (push (format "✗ Data directory missing: %s" dir) results)))

    ;; Check 3: Required modules
    (message "Checking required modules...")
    (let ((required-modules '(efrit-config efrit-tools efrit-session)))
      (dolist (mod required-modules)
        (if (featurep mod)
            (push (format "✓ Module loaded: %s" mod) results)
          (setq all-ok nil)
          (push (format "✗ Module not loaded: %s" mod) results))))

    ;; Check 4: API endpoint
    (message "Checking API endpoint...")
    (let ((endpoint (efrit-common-get-api-url)))
      (if endpoint
          (push (format "✓ API endpoint configured: %s" endpoint) results)
        (setq all-ok nil)
        (push "✗ API endpoint not configured" results)))

    ;; Display results
    (with-current-buffer (get-buffer-create "*efrit-doctor*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Efrit Health Check\n")
        (insert "==================\n\n")
        (dolist (result (reverse results))
          (insert result "\n"))
        (insert "\n")
        (if all-ok
            (insert "Overall Status: ✓ All checks passed\n")
          (insert "Overall Status: ✗ Some checks failed\n")
          (insert "\nRecommended actions:\n")
          (insert "- Set API key via ANTHROPIC_API_KEY environment variable\n")
          (insert "- Or configure efrit-api-key in your init file\n")
          (insert "- Run M-x efrit-config--ensure-directories to create directories\n"))
        (goto-char (point-min))
        (view-mode))
      (display-buffer (current-buffer)))

    (if all-ok
        (message "Efrit health check: All OK")
      (message "Efrit health check: Some issues found (see *efrit-doctor* buffer)"))))

(provide 'efrit-common)

;;; efrit-common.el ends here
