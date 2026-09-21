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

(defcustom efrit-api-url nil
  "DEPRECATED: Legacy full API URL override.
Use `efrit-api-base-url' instead.

When non-nil, this takes precedence over `efrit-api-base-url'.
This is kept for backwards compatibility with existing configurations.
Set to nil (the default) to use the centralized configuration."
  :type '(choice (const :tag "Use efrit-api-base-url" nil)
                 (string :tag "Legacy full URL override"))
  :group 'efrit)

(defcustom efrit-api-auth-scheme 'x-api-key
  "How the API key is presented to the endpoint.

- `x-api-key': Anthropic's native scheme; the key is sent in an
  `x-api-key' header and must look like an Anthropic key (`sk-...').
- `bearer': send `Authorization: Bearer KEY' and skip Anthropic key
  format validation.  Use this for LLM proxies (LiteLLM, Bifrost,
  OpenRouter, ...) that expose the Anthropic Messages API behind
  their own token scheme.  Point `efrit-api-base-url' at the proxy's
  Anthropic-compatible base.

Custom headers from `efrit-api-custom-headers' still override either."
  :type '(choice (const :tag "x-api-key (Anthropic)" x-api-key)
                 (const :tag "Authorization: Bearer (proxies)" bearer))
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
  "Validate that KEY is usable under `efrit-api-auth-scheme'.
Returns t if valid, signals error if not.  Under `x-api-key' the key
must look like an Anthropic key; under `bearer' any non-empty string
is accepted, since proxy tokens have arbitrary formats."
  (when (or (not (stringp key))
            (string-empty-p key))
    (error "API key is empty or not a string"))
  (when (and (eq efrit-api-auth-scheme 'x-api-key)
             (or (< (length key) 20)
                 (not (string-prefix-p "sk-" key))))
    (error "Invalid API key format. Anthropic keys should start with 'sk-' and be at least 20 characters (set `efrit-api-auth-scheme' to `bearer' for proxy tokens)"))
  t)

(defun efrit-common--sanitize-key-for-logging (key)
  "Return a sanitized version of KEY safe for logging.
Shows only first 6 and last 4 characters."
  (if (and (stringp key) (>= (length key) 10))
      (concat (substring key 0 6) "..." (substring key -4))
    "[INVALID-KEY]"))

(define-obsolete-function-alias 'efrit-common-safe-log 'efrit-log-safe "0.4.2"
  "Use efrit-log-safe from efrit-log.el instead.")

;;; Error Message Formatting

;; All error messages returned to Claude should use these helpers for consistency.
;; This ensures Claude can reliably parse and understand errors across all tools.
;; Format: [Error: CATEGORY] message or 🚫 SECURITY: message for security issues.

(defun efrit-format-error (category message &rest args)
  "Format an error message with consistent structure.
CATEGORY is a brief category name (e.g., `API', `Security', `Validation').
MESSAGE is the error message (can contain format specifiers).
ARGS are format arguments for MESSAGE.

Returns a formatted string for returning to Claude as a tool result."
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
              
              ;; Function that returns the key.  Checked BEFORE the
              ;; env-var case: a named function like #\='my-key-fn is
              ;; also a symbol, and used to be looked up as an
              ;; environment variable.
              ((functionp efrit-api-key)
               (or (funcall efrit-api-key)
                   (error "API key function %s returned nil"
                          (if (symbolp efrit-api-key) efrit-api-key "(lambda)"))))

              ;; Symbol naming an environment variable
              ((and (symbolp efrit-api-key) efrit-api-key)
               (or (getenv (symbol-name efrit-api-key))
                   (error "Environment variable %s not set%s" efrit-api-key
                          (if (fboundp efrit-api-key) ""
                            " (and it is not a function either)"))))
              
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
  ;; Proxies often hand out bases with a trailing slash
  ;; (e.g. "https://gw.example.com/anthropic/"); strip it so the
  ;; "/v1/messages" suffix doesn't produce "//".
  (string-remove-suffix
   "/"
   (cond
    ((stringp efrit-api-base-url) efrit-api-base-url)
    ((functionp efrit-api-base-url) (funcall efrit-api-base-url))
    (t "https://api.anthropic.com"))))

(defun efrit-common-get-api-url ()
  "Get the full API URL for messages endpoint."
  (concat (efrit-common-get-base-url) "/v1/messages"))

(defconst efrit-common-api-version "2023-06-01"
  "Anthropic API version for all requests.")

(defun efrit-common-auth-header (api-key)
  "Return the (NAME . VALUE) auth header for API-KEY per `efrit-api-auth-scheme'."
  (pcase efrit-api-auth-scheme
    ('bearer (cons "authorization" (concat "Bearer " api-key)))
    (_ (cons "x-api-key" api-key))))

(defun efrit-common-build-headers (api-key)
  "Build standard HTTP headers using API-KEY with security validation."
  ;; Validate the API key before using it
  (efrit-common--validate-api-key api-key)
  `(("Content-Type" . "application/json")
    ("anthropic-version" . ,efrit-common-api-version)
    ,(efrit-common-auth-header api-key)))

;;; Error Handling

(defun efrit-common-safe-error-message (err)
  "Extract safe error message from ERR object."
  (cond
   ((stringp err) err)
   ((and (consp err) (stringp (cadr err))) (cadr err))
   ((error-message-string err))
   (t "Unknown error")))

;;; Utilities

(defun efrit-truncate-string (str max-length &optional mode)
  "Truncate STR to display within MAX-LENGTH.
MODE determines truncation strategy:
- \\='chars\\=' (default): Truncate by character count
- \\='width\\=': Truncate by display width (for variable-width fonts)
- \\='by-words\\=': Truncate to nearest word boundary

Returns truncated string with ellipsis (...) if needed."
  (setq mode (or mode 'chars))
  (pcase mode
      ('chars
       (if (> (length str) max-length)
           (concat (substring str 0 (- max-length 3)) "...")
         str))
      ('width
       ;; Use builtin width truncation for display purposes
       (truncate-string-to-width str max-length nil nil t))
      ('by-words
       ;; TODO: Implement word-boundary truncation
       (efrit-truncate-string str max-length 'chars))
      (_
       ;; Default to character truncation
       (if (> (length str) max-length)
           (concat (substring str 0 (- max-length 3)) "...")
         str))))



(defun efrit-common-count-words (text)
  "Count words in TEXT.
Returns the number of words, where a word is defined as a sequence
of non-whitespace characters separated by whitespace.

Example:
  (efrit-common-count-words \"Hello world\") => 2
  (efrit-common-count-words \"  foo   bar  baz  \") => 3"
  (if (or (null text) (string-empty-p text))
      0
    (let ((count 0)
          (in-word nil))
      (dotimes (i (length text))
        (let ((char (aref text i)))
          (if (memq char '(?  ?\t ?\n ?\r))
              (setq in-word nil)
            (unless in-word
              (setq count (1+ count))
              (setq in-word t)))))
      count)))

(defconst efrit-common--replacement-char #xFFFD
  "U+FFFD, written for a character JSON cannot carry.")

(defun efrit-common--json-escape-char (char)
  "CHAR (a code point) as JSON \\uXXXX escapes.
A JSON escape holds exactly four hex digits, so a character above the
Basic Multilingual Plane is written as a UTF-16 surrogate pair: one
escape of six digits (\\u1F600 for an emoji) made the whole request
invalid JSON, and a diff of a Unicode-heavy package tripped it.  A raw
undecoded byte (Emacs code points #x3FFF80 and up), a lone surrogate,
or a code point past U+10FFFF has no JSON form and becomes U+FFFD."
  (cond
   ((or (> char #x10FFFF) (<= #xD800 char #xDFFF))
    (format "\\u%04X" efrit-common--replacement-char))
   ((> char #xFFFF)
    (let ((v (- char #x10000)))
      (format "\\u%04X\\u%04X"
              (+ #xD800 (ash v -10))
              (+ #xDC00 (logand v #x3FF)))))
   (t (format "\\u%04X" char))))

(defun efrit-common-escape-json-unicode (json-string)
  "Escape non-ASCII characters in JSON-STRING as JSON \\u escapes.
The result is pure ASCII, so no transport can mis-encode it.  See
`efrit-common--json-escape-char' for the cases."
  (replace-regexp-in-string
   "[^\x00-\x7F]"
   (lambda (char)
     (efrit-common--json-escape-char (string-to-char char)))
   json-string
   ;; FIXEDCASE must be t: with nil, a match that is an upper-case
   ;; letter (an O with a stroke, say) makes `replace-match' upcase
   ;; the replacement, and \u00D8 goes out as \U00D8, which no JSON
   ;; parser accepts.  Found by a package whose docstrings hold one.
   t
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

;;; Output truncation, warn-once, templates, syntax check

(defun efrit-truncate-output (text max-chars &optional keep)
  "Truncate TEXT to about MAX-CHARS characters for a tool result.
KEEP is `tail' (default), `head', or `both'.  Tool output usually
ends with the interesting part (the failing test, the error), so the
default keeps the tail.  A marker says how much was dropped."
  (let ((len (length text)))
    (if (<= len max-chars)
        text
      (pcase (or keep 'tail)
        ('head (concat (substring text 0 max-chars)
                       (format "\n... [%d more chars truncated]" (- len max-chars))))
        ('both (let ((half (/ max-chars 2)))
                 (concat (substring text 0 half)
                         (format "\n... [%d chars omitted] ...\n" (- len max-chars))
                         (substring text (- len half)))))
        (_ (concat (format "[first %d chars omitted] ...\n" (- len max-chars))
                   (substring text (- len max-chars))))))))

(defvar efrit--warned (make-hash-table :test 'equal)
  "Keys already warned about, for `efrit-warn-once'.")

(defun efrit-warn-once (key format-string &rest args)
  "Display a warning once per KEY (and message text) per session.
Re-warns only if the message changes, so a flapping condition doesn't
spam but a *different* failure is still shown.  Returns non-nil if a
warning was displayed."
  (let* ((msg (apply #'format format-string args))
         (prev (gethash key efrit--warned)))
    (unless (equal prev msg)
      (puthash key msg efrit--warned)
      (display-warning 'efrit msg :warning)
      t)))

(defun efrit-expand-template (template lookup)
  "Expand {{{:key}}} placeholders in TEMPLATE using LOOKUP.
LOOKUP is a function from the keyword (e.g. :key) to a string, a
symbol whose value is a string, a function returning a string, or
nil.  A nil slot expands to the empty string.  Single pass with
`string-search': replacement text is inserted verbatim, never
rescanned, and never passed through a regexp replacement, so `\\1',
`\\&' and backslashes in file contents cannot corrupt the result.
\(minuet's expander.)"
  (let ((out nil) (pos 0) (len (length template)))
    (while (< pos len)
      (let ((start (string-search "{{{" template pos)))
        (if (not start)
            (progn (push (substring template pos) out) (setq pos len))
          (let ((end (string-search "}}}" template (+ start 3))))
            (if (not end)
                (progn (push (substring template pos) out) (setq pos len))
              (push (substring template pos start) out)
              (let* ((name (substring template (+ start 3) end))
                     (key (intern (if (string-prefix-p ":" name) name (concat ":" name))))
                     (raw (funcall lookup key))
                     (val (cond ((null raw) "")
                                ((stringp raw) raw)
                                ((functionp raw) (funcall raw))
                                ((and (symbolp raw) (boundp raw)) (symbol-value raw))
                                (t raw))))
                (unless (stringp val)
                  (signal 'wrong-type-argument (list 'stringp val key)))
                (push val out))
              (setq pos (+ end 3)))))))
    (apply #'concat (nreverse out))))

(defun efrit-lisp-syntax-problem (text &optional mode)
  "Return a description of an unbalanced-paren/string problem in TEXT, or nil.
Checks in a temp buffer under MODE (default `emacs-lisp-mode') using
the mode's syntax table, so quotes and comments are respected.
Use after writing Lisp so the model gets \"you produced unbalanced
code at line N\" instead of a later load error."
  (with-temp-buffer
    (insert text)
    (condition-case nil (funcall (or mode #'emacs-lisp-mode)) (error nil))
    (condition-case err
        (progn (check-parens) nil)
      (user-error
       ;; check-parens leaves point at the problem
       (format "%s at line %d, column %d"
               (error-message-string err)
               (line-number-at-pos) (current-column)))
      (error (error-message-string err)))))

;;; Health check lives in efrit-doctor.el (M-x efrit-doctor)

(provide 'efrit-common)

;;; efrit-common.el ends here
