;;; efrit-tool-utils.el --- Standardized utilities for efrit tools -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Keywords: ai, tools
;; Version: 0.3.1

;;; Commentary:
;;
;; This module provides standardized infrastructure for efrit tools:
;;
;; 1. PATH HANDLING
;;    - Project root detection via project.el
;;    - Resolution of absolute and project-relative paths
;;    - Sandbox enforcement (paths must stay within project root)
;;
;; 2. RESPONSE FORMAT
;;    - Consistent success/error response structure
;;    - Standardized error types
;;    - Warning accumulation
;;
;; 3. LARGE RESULT HANDLING
;;    - Truncation indicators
;;    - Pagination support (offset/limit)
;;
;; All new tools should use these utilities to ensure consistency.

;;; Code:

(require 'project)
(require 'json)
(require 'cl-lib)
(require 'url-parse)

;;; Customization

(defgroup efrit-tool-utils nil
  "Utilities for efrit tools."
  :group 'efrit
  :prefix "efrit-tool-")

(defcustom efrit-project-sandbox t
  "When non-nil, restrict file tools to project root.
This prevents accidental access to files outside the project."
  :type 'boolean
  :group 'efrit-tool-utils)

(defvar efrit-project-root nil
  "Explicitly set project root directory.
When non-nil, this takes precedence over `project-current' detection.
This is useful for daemon mode where `default-directory' may be `~/'
and there's no file visiting to provide project context.

Can be set via:
- The `set_project_root' tool
- Programmatically via `(setq efrit-project-root \"/path/to/project\")'
- Automatically from file-visiting buffers")

(defcustom efrit-sensitive-file-patterns
  '("\\.env\\'" "\\.env\\.[^/]*\\'"
    "credentials" "\\.pem\\'" "\\.key\\'"
    "\\.authinfo\\'" "\\.netrc\\'"
    "id_rsa" "id_ed25519" "id_ecdsa"
    "\\.gnupg/" "password" "secret")
  "Patterns for files that require extra caution.
These patterns are matched against file paths.
Matched files will trigger warnings or require confirmation."
  :type '(repeat string)
  :group 'efrit-tool-utils)

;;; Error Types

(defconst efrit-tool-error-types
  '(not_found
    permission_denied
    timeout
    invalid_input
    sandbox_violation
    sensitive_file
    parse_error
    execution_error
    rate_limited)
  "Standard error types for tool responses.")

;;; Path Handling

(defun efrit-tool--get-project-root ()
  "Get the project root directory.

Priority order:
1. `efrit-project-root' if explicitly set
2. `project-current' via project.el
3. `default-directory' as fallback

Returns the expanded absolute path."
  (expand-file-name
   (cond
    ;; Explicit project root takes precedence
    ((and efrit-project-root
          (file-directory-p efrit-project-root))
     efrit-project-root)
    ;; Try project.el detection
    ((when-let* ((proj (project-current)))
       (project-root proj)))
    ;; Fallback to default-directory
    (t default-directory))))

(defun efrit-set-project-root (path)
  "Set `efrit-project-root' to PATH.
Returns the normalized path, or nil if PATH is invalid."
  (let ((expanded (expand-file-name path)))
    (if (file-directory-p expanded)
        (progn
          (setq efrit-project-root expanded)
          (message "Efrit project root set to: %s" expanded)
          expanded)
      (message "Warning: Path does not exist: %s" path)
      nil)))

(defun efrit-clear-project-root ()
  "Clear explicit project root, returning to auto-detection."
  (interactive)
  (setq efrit-project-root nil)
  (message "Efrit project root cleared, using auto-detection"))

(defun efrit-tool--path-in-directory-p (path directory)
  "Check if PATH is within DIRECTORY.
Both paths are expanded before comparison.
Handles the case where PATH equals DIRECTORY (same directory)."
  (let ((expanded-path (expand-file-name path))
        (expanded-dir (file-name-as-directory (expand-file-name directory))))
    ;; Either path is a prefix match, or they're the same directory
    (or (string-prefix-p expanded-dir expanded-path)
        (file-equal-p expanded-path (directory-file-name expanded-dir)))))

(defun efrit-tool--is-sensitive-file (path)
  "Check if PATH matches any sensitive file pattern."
  (let ((path-str (expand-file-name path)))
    (cl-some (lambda (pattern)
               (string-match-p pattern path-str))
             efrit-sensitive-file-patterns)))

(defun efrit-resolve-path (path &optional allow-outside)
  "Resolve PATH relative to project root with sandbox enforcement.

If PATH is absolute, use as-is (with sandbox check).
If PATH is relative, resolve against project root.
If PATH is nil or empty, return project root.

When `efrit-project-sandbox' is non-nil and ALLOW-OUTSIDE is nil,
signals an error if the resolved path is outside project root.

Returns a plist with:
  :path - the absolute resolved path
  :path-relative - path relative to project root (or nil if outside)
  :project-root - the project root directory
  :is-sensitive - t if path matches sensitive file patterns
  :outside-project - t if path is outside project root"
  (let* ((project-root (efrit-tool--get-project-root))
         (resolved (cond
                    ;; Empty or nil path -> project root
                    ((or (null path) (string-empty-p path))
                     project-root)
                    ;; Absolute path -> use as-is
                    ((file-name-absolute-p path)
                     (expand-file-name path))
                    ;; Relative path -> resolve against project root
                    (t
                     (expand-file-name path project-root))))
         (in-project (efrit-tool--path-in-directory-p resolved project-root))
         (relative-path (when in-project
                          (file-relative-name resolved project-root))))

    ;; Sandbox enforcement
    (when (and efrit-project-sandbox
               (not allow-outside)
               (not in-project))
      (signal 'efrit-sandbox-violation
              (list (format "Path '%s' is outside project root '%s'"
                           path project-root))))

    (list :path resolved
          :path-relative relative-path
          :project-root project-root
          :is-sensitive (efrit-tool--is-sensitive-file resolved)
          :outside-project (not in-project))))

;; Define the error type
(define-error 'efrit-sandbox-violation "Sandbox violation")

(defun efrit-resolve-path-simple (path &optional allow-outside)
  "Simplified path resolution - returns just the absolute path string.
See `efrit-resolve-path' for full details."
  (plist-get (efrit-resolve-path path allow-outside) :path))

;;; Response Building

(defun efrit-tool-success (result &optional warnings)
  "Build a standard success response.

RESULT is the tool-specific result data (can be any type).
WARNINGS is an optional list of warning strings.

Returns an alist suitable for JSON encoding:
  ((success . t)
   (result . RESULT)
   (warnings . WARNINGS))"
  `((success . t)
    (result . ,result)
    ,@(when warnings `((warnings . ,(vconcat warnings))))))

(defun efrit-tool-error (error-type message &optional details)
  "Build a standard error response.

ERROR-TYPE should be one of `efrit-tool-error-types'.
MESSAGE is a human-readable error message.
DETAILS is an optional alist of additional error context.

Returns an alist suitable for JSON encoding:
  ((success . :json-false)
   (error . ((type . ERROR-TYPE)
             (message . MESSAGE)
             (details . DETAILS))))"
  (let ((error-obj `((type . ,(symbol-name error-type))
                     (message . ,message))))
    (when details
      (setq error-obj (append error-obj `((details . ,details)))))
    `((success . :json-false)
      (error . ,error-obj))))

(defun efrit-tool-response-to-json (response)
  "Encode RESPONSE alist to JSON string."
  (json-encode response))

;;; Truncation and Pagination

(defun efrit-tool-truncation-info (total-available returned &optional offset)
  "Build truncation metadata for large results.

TOTAL-AVAILABLE is the total count of items available.
RETURNED is the count of items actually returned.
OFFSET is the starting offset (default 0).

Returns an alist with truncation info if truncated, nil otherwise."
  (let ((offset (or offset 0)))
    (when (> total-available returned)
      `((truncated . t)
        (total_available . ,total-available)
        (returned . ,returned)
        (offset . ,offset)
        (continuation_hint . ,(format "Use offset=%d to get more"
                                      (+ offset returned)))))))

(defun efrit-tool-paginate (items offset limit)
  "Paginate ITEMS list using OFFSET and LIMIT.

Returns a plist with:
  :items - the paginated items
  :total - total count before pagination
  :offset - the offset used
  :limit - the limit used
  :truncated - t if more items available"
  (let* ((total (length items))
         (offset (or offset 0))
         (limit (or limit 50))
         (end (min (+ offset limit) total))
         (paginated (cl-subseq items offset end)))
    (list :items paginated
          :total total
          :offset offset
          :limit limit
          :truncated (< end total))))

;;; Timeout Support

(defcustom efrit-tool-default-timeout 30
  "Default timeout in seconds for tool execution."
  :type 'integer
  :group 'efrit-tool-utils)

(defcustom efrit-tool-timeouts
  '((search_content . 60)
    (project_files . 30)
    (fetch_url . 30)
    (vcs_blame . 45)
    (vcs_log . 30)
    (read_file . 15))
  "Timeout in seconds for specific tools.
Tools not listed use `efrit-tool-default-timeout'."
  :type '(alist :key-type symbol :value-type integer)
  :group 'efrit-tool-utils)

(defun efrit-tool-get-timeout (tool-name)
  "Get the timeout in seconds for TOOL-NAME."
  (or (alist-get tool-name efrit-tool-timeouts)
      efrit-tool-default-timeout))

(defmacro efrit-with-tool-timeout (tool-name &rest body)
  "Execute BODY with timeout appropriate for TOOL-NAME.
Returns the result of BODY, or a timeout error response if timeout occurs."
  (declare (indent 1))
  `(let ((timeout (efrit-tool-get-timeout ,tool-name)))
     (with-timeout (timeout
                    (efrit-tool-error 'timeout
                                      (format "Tool execution timed out after %d seconds"
                                              timeout)
                                      `((tool . ,(symbol-name ,tool-name))
                                        (timeout_seconds . ,timeout))))
       ,@body)))

;;; Project Type Detection

(defun efrit-tool-detect-project-type ()
  "Detect the type of project in the current directory.
Returns a symbol: \\='git, \\='npm, \\='cargo, \\='python, \\='make, or \\='unknown."
  (let ((root (efrit-tool--get-project-root)))
    (cond
     ((file-exists-p (expand-file-name ".git" root)) 'git)
     ((file-exists-p (expand-file-name "package.json" root)) 'npm)
     ((file-exists-p (expand-file-name "Cargo.toml" root)) 'cargo)
     ((or (file-exists-p (expand-file-name "setup.py" root))
          (file-exists-p (expand-file-name "pyproject.toml" root))) 'python)
     ((file-exists-p (expand-file-name "Makefile" root)) 'make)
     (t 'unknown))))

;;; Git Utilities (for vcs tools)

(defun efrit-tool-git-available-p ()
  "Check if git is available and we're in a git repository."
  (and (executable-find "git")
       (eq (efrit-tool-detect-project-type) 'git)))

(defun efrit-tool-run-git (args &optional timeout)
  "Run git command with ARGS and return output.
ARGS should be a list of command-line arguments.
TIMEOUT defaults to 30 seconds.

Returns a plist with:
  :success - t if command succeeded (exit code 0)
  :output - stdout as string
  :error - stderr as string (if failed)
  :exit-code - the exit code"
  (let* ((timeout (or timeout 30))
         (default-directory (efrit-tool--get-project-root))
         (output-buffer (generate-new-buffer " *efrit-git-output*"))
         (stderr-file (make-temp-file "efrit-git-stderr"))
         exit-code stdout stderr result)
    (unwind-protect
        (progn
          (with-timeout (timeout
                         (setq result (list :success nil
                                            :output nil
                                            :error "Git command timed out"
                                            :exit-code -1)))
            (setq exit-code
                  (apply #'call-process "git" nil
                         (list output-buffer stderr-file) nil args))
            (setq stdout (with-current-buffer output-buffer
                          (buffer-string)))
            (setq stderr (when (file-exists-p stderr-file)
                          (with-temp-buffer
                            (insert-file-contents stderr-file)
                            (buffer-string))))
            (setq result (list :success (= exit-code 0)
                               :output stdout
                               :error (unless (= exit-code 0) stderr)
                               :exit-code exit-code)))
          result)
      (kill-buffer output-buffer)
      (when (file-exists-p stderr-file)
        (delete-file stderr-file)))))

;;; Binary File Detection

(defconst efrit-tool-binary-extensions
  '("png" "jpg" "jpeg" "gif" "bmp" "ico" "webp" "svg"  ; Images
    "pdf" "doc" "docx" "xls" "xlsx" "ppt" "pptx"      ; Documents
    "zip" "tar" "gz" "bz2" "xz" "7z" "rar"            ; Archives
    "exe" "dll" "so" "dylib" "o" "a"                  ; Binaries
    "mp3" "mp4" "wav" "avi" "mkv" "mov" "flac"        ; Media
    "ttf" "otf" "woff" "woff2" "eot"                  ; Fonts
    "sqlite" "db" "pyc" "elc" "class")                ; Other
  "File extensions that are definitely binary.")

(defun efrit-tool-binary-extension-p (path)
  "Check if PATH has a binary file extension."
  (let ((ext (file-name-extension path)))
    (and ext (member (downcase ext) efrit-tool-binary-extensions))))

(defun efrit-tool-binary-file-p (path &optional check-bytes)
  "Check if PATH is a binary file.
If CHECK-BYTES is non-nil (default 8192), also check for null bytes
in the first CHECK-BYTES of the file."
  (or (efrit-tool-binary-extension-p path)
      (when (and check-bytes (file-exists-p path))
        (with-temp-buffer
          (set-buffer-multibyte nil)
          (insert-file-contents-literally path nil 0 (or check-bytes 8192))
          (goto-char (point-min))
          (search-forward "\0" nil t)))))

;;; Timestamp Formatting

(defun efrit-tool-format-time (time)
  "Format TIME as ISO 8601 string.
TIME can be a time value or nil (uses current time)."
  (format-time-string "%Y-%m-%dT%H:%M:%SZ" (or time (current-time)) t))

(defun efrit-tool-file-mtime (path)
  "Get the modification time of PATH as ISO 8601 string."
  (when (file-exists-p path)
    (efrit-tool-format-time (file-attribute-modification-time
                             (file-attributes path)))))

;;; Network Access Controls

(defcustom efrit-enable-web-access nil
  "When non-nil, enable web search and URL fetching tools.
This is disabled by default for security.  Enable when you need
Claude to access external documentation or search the web."
  :type 'boolean
  :group 'efrit-tool-utils)

(defcustom efrit-allowed-domains
  '("gnu.org" "emacs.org" "www.gnu.org"
    "stackoverflow.com" "emacs.stackexchange.com"
    "github.com" "raw.githubusercontent.com"
    "gitlab.com"
    "docs.python.org" "developer.mozilla.org"
    "devdocs.io" "api.anthropic.com")
  "Domains allowed for web fetching when web access is enabled.
Add domains you trust to this list.  Subdomains are automatically allowed."
  :type '(repeat string)
  :group 'efrit-tool-utils)

(defcustom efrit-confirm-each-fetch t
  "When non-nil, confirm before each URL fetch.
Even with web access enabled, this provides per-request control."
  :type 'boolean
  :group 'efrit-tool-utils)

(defun efrit-tool-domain-allowed-p (url)
  "Check if URL's domain is in the allowed list."
  (when-let* ((parsed (url-generic-parse-url url))
              (host (url-host parsed)))
    (cl-some (lambda (allowed)
               (or (string= host allowed)
                   (string-suffix-p (concat "." allowed) host)))
             efrit-allowed-domains)))

(defun efrit-tool-check-web-access (url)
  "Check if web access to URL is permitted.
Returns nil if allowed, or an error response if blocked."
  (cond
   ((not efrit-enable-web-access)
    (efrit-tool-error 'permission_denied
                      "Web access is disabled. Set efrit-enable-web-access to t to enable."
                      `((url . ,url))))
   ((not (efrit-tool-domain-allowed-p url))
    (efrit-tool-error 'permission_denied
                      (format "Domain not in allowed list: %s" url)
                      `((url . ,url)
                        (allowed_domains . ,(vconcat efrit-allowed-domains)))))
   (t nil)))

;;; Audit Logging

(defvar efrit-tool-audit-log nil
  "In-memory audit log of tool invocations.
Each entry is a plist with :timestamp, :tool, :inputs, :result, :duration.")

(defcustom efrit-tool-audit-max-entries 1000
  "Maximum number of entries to keep in the audit log."
  :type 'integer
  :group 'efrit-tool-utils)

(defcustom efrit-tool-audit-to-file nil
  "When non-nil, also write audit log entries to a file.
The file path is determined by `efrit-tool-audit-file'."
  :type 'boolean
  :group 'efrit-tool-utils)

(defcustom efrit-tool-audit-file "~/.emacs.d/.efrit/logs/tool-audit.log"
  "File path for tool audit log when `efrit-tool-audit-to-file' is non-nil."
  :type 'file
  :group 'efrit-tool-utils)

(defun efrit-tool-audit (tool-name inputs result &optional duration)
  "Record an audit log entry for tool invocation.
TOOL-NAME is the tool that was called.
INPUTS is the input parameters (will be sanitized).
RESULT is :success, :error, or :blocked.
DURATION is the execution time in seconds (optional)."
  (let ((entry (list :timestamp (efrit-tool-format-time nil)
                     :tool tool-name
                     :inputs (efrit-tool--sanitize-audit-inputs inputs)
                     :result result
                     :duration duration)))
    ;; Add to in-memory log
    (push entry efrit-tool-audit-log)
    ;; Trim if too large
    (when (> (length efrit-tool-audit-log) efrit-tool-audit-max-entries)
      (setq efrit-tool-audit-log
            (seq-take efrit-tool-audit-log efrit-tool-audit-max-entries)))
    ;; Optionally write to file
    (when efrit-tool-audit-to-file
      (efrit-tool--write-audit-to-file entry))
    ;; Also log via efrit-log
    (efrit-tool-log 'info tool-name "result=%s duration=%.2fs"
                    result (or duration 0))))

(defun efrit-tool--sanitize-audit-inputs (inputs)
  "Sanitize INPUTS for audit logging.
Removes or truncates sensitive data."
  (cond
   ((stringp inputs)
    (if (> (length inputs) 200)
        (concat (substring inputs 0 200) "...")
      inputs))
   ((listp inputs)
    (mapcar (lambda (item)
              (if (and (consp item)
                       (memq (car item) '(password secret key token)))
                  (cons (car item) "[REDACTED]")
                item))
            inputs))
   (t inputs)))

(defun efrit-tool--write-audit-to-file (entry)
  "Write ENTRY to the audit log file."
  (let ((file (expand-file-name efrit-tool-audit-file)))
    (make-directory (file-name-directory file) t)
    (with-temp-buffer
      (insert (format "%s [%s] %s inputs=%S duration=%s\n"
                      (plist-get entry :timestamp)
                      (plist-get entry :tool)
                      (plist-get entry :result)
                      (plist-get entry :inputs)
                      (or (plist-get entry :duration) "N/A")))
      (append-to-file (point-min) (point-max) file))))

(defun efrit-tool-get-audit-log (&optional limit)
  "Get recent audit log entries.
LIMIT defaults to 50."
  (seq-take efrit-tool-audit-log (or limit 50)))

(defun efrit-tool-clear-audit-log ()
  "Clear the in-memory audit log."
  (interactive)
  (setq efrit-tool-audit-log nil)
  (message "Audit log cleared"))

;;; Tool Execution Wrapper

(defmacro efrit-tool-execute (tool-name inputs &rest body)
  "Execute BODY as TOOL-NAME with audit logging.
INPUTS are the tool inputs (for audit log).
Handles errors, timeouts, and audit logging automatically.

Returns the result of BODY, which should be a tool response."
  (declare (indent 2))
  `(let ((start-time (current-time))
         result)
     (condition-case err
         (progn
           (setq result
                 (efrit-with-tool-timeout ',tool-name
                   ,@body))
           (efrit-tool-audit ',tool-name ,inputs
                             (if (eq (cdr (assoc 'success result)) t)
                                 :success :error)
                             (float-time (time-subtract (current-time) start-time)))
           result)
       (efrit-sandbox-violation
        (efrit-tool-audit ',tool-name ,inputs :blocked)
        (efrit-tool-error 'sandbox_violation
                          (cadr err)
                          `((tool . ,(symbol-name ',tool-name)))))
       (error
        (efrit-tool-audit ',tool-name ,inputs :error
                          (float-time (time-subtract (current-time) start-time)))
        (efrit-tool-error 'execution_error
                          (error-message-string err)
                          `((tool . ,(symbol-name ',tool-name))))))))

;;; Logging Integration

(declare-function efrit-log "efrit-log")

(defun efrit-tool-log (level tool-name message &rest args)
  "Log MESSAGE for TOOL-NAME at LEVEL.
ARGS are passed to `format'."
  (when (fboundp 'efrit-log)
    (efrit-log level (format "[%s] %s" tool-name (apply #'format message args)))))

(provide 'efrit-tool-utils)

;;; efrit-tool-utils.el ends here
