;;; efrit-tool-format-file.el --- Auto-format files tool -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Keywords: ai, tools
;; Version: 0.4.1

;;; Commentary:
;;
;; This tool provides Claude with the ability to auto-format files using
;; project formatters (for non-elisp) or Emacs' built-in formatting (for elisp).
;;
;; Key features:
;; - Elisp files: Uses emacs-lisp-mode indentation
;; - Other files: Calls external formatters (prettier, gofmt, rustfmt, etc.)
;; - Returns diff showing formatting changes
;; - Sandbox enforcement (paths must stay within project root)

;;; Code:

(require 'efrit-tool-utils)
(require 'cl-lib)

;;; Customization

(defcustom efrit-tool-format-file-formatters
  '(("\\.jsx?$" . ("prettier" "--write"))
    ("\\.tsx?$" . ("prettier" "--write"))
    ("\\.json$" . ("prettier" "--write"))
    ("\\.css$" . ("prettier" "--write"))
    ("\\.scss$" . ("prettier" "--write"))
    ("\\.html?$" . ("prettier" "--write"))
    ("\\.ya?ml$" . ("prettier" "--write"))
    ("\\.md$" . ("prettier" "--write"))
    ("\\.go$" . ("gofmt" "-w"))
    ("\\.rs$" . ("rustfmt"))
    ("\\.py$" . ("black"))
    ("\\.rb$" . ("rubocop" "-a"))
    ("\\.sh$" . ("shfmt" "-w"))
    ("\\.c$" . ("clang-format" "-i"))
    ("\\.cpp$" . ("clang-format" "-i"))
    ("\\.h$" . ("clang-format" "-i"))
    ("\\.hpp$" . ("clang-format" "-i")))
  "Alist mapping file patterns to formatter commands.
Each entry is (REGEX . (COMMAND ARGS...)).
The file path is appended to the command."
  :type '(alist :key-type regexp :value-type (repeat string))
  :group 'efrit-tool-utils)

(defcustom efrit-tool-format-file-max-size 1000000
  "Maximum file size in bytes for formatting (default: 1MB)."
  :type 'integer
  :group 'efrit-tool-utils)

;;; Diff Generation

(defalias 'efrit-tool-format-file--generate-diff #'efrit-tool-unified-diff
  "Obsolete: use `efrit-tool-unified-diff'.")

;;; Elisp Formatting

(defun efrit-tool-format-file--format-elisp (content)
  "Format CONTENT as Emacs Lisp.
Returns the formatted content string."
  (with-temp-buffer
    (insert content)
    (emacs-lisp-mode)
    (indent-region (point-min) (point-max))
    (buffer-string)))

;;; External Formatter

(defun efrit-tool-format-file--run-external (file-path formatter)
  "Run external FORMATTER on FILE-PATH.
FORMATTER is (COMMAND . ARGS).
Returns (success . error-message)."
  (let* ((cmd (car formatter))
         ;; Run on the file's host: a remote file gets the remote
         ;; formatter, with the host-local path as its argument.
         (default-directory (file-name-directory file-path))
         (args (append (cdr formatter)
                       (list (efrit-tool-local-name file-path))))
         (exit-code nil)
         (output nil))
    ;; Check if formatter is available
    (if (not (efrit-tool-executable-find cmd))
        (cons nil (format "Formatter '%s' not found in PATH%s" cmd
                          (if (file-remote-p default-directory)
                              (format " on %s" (file-remote-p default-directory))
                            "")))
      ;; Run the formatter
      (with-temp-buffer
        (setq exit-code (apply #'efrit-tool-call-process cmd nil t nil args))
        (setq output (buffer-string)))
      (if (= exit-code 0)
          (cons t nil)
        (cons nil (format "Formatter failed (exit %d): %s" exit-code output))))))

;;; Main Implementation

(defun efrit-tool-format-file (args)
  "Format a file using appropriate formatter.

ARGS is an alist with:
  path - file to format (required, must be absolute)

Returns a standard tool response with diff showing changes."
  (efrit-tool-execute format_file args
    (let* ((path-input (or (alist-get 'path args) ""))
           (warnings '()))

      ;; Validate required inputs
      (when (string-empty-p path-input)
        (signal 'user-error (list "Path is required")))

      ;; Resolve path with sandbox check
      (let* ((path-info (efrit-resolve-path path-input))
             (path (plist-get path-info :path))
             (path-relative (plist-get path-info :path-relative)))

        ;; Check file exists
        (unless (file-exists-p path)
          (signal 'file-error (list "File not found" path)))

        ;; Check it's not a directory
        (when (file-directory-p path)
          (signal 'user-error (list "Path is a directory, not a file" path)))

        ;; Check for sensitive file
        (when (plist-get path-info :is-sensitive)
          (push (format "Formatting sensitive file: %s" path-relative) warnings))

        ;; Check file size
        (let ((size (file-attribute-size (file-attributes path))))
          (when (> size efrit-tool-format-file-max-size)
            (signal 'user-error
                    (list (format "File too large (%d bytes, max %d)"
                                  size efrit-tool-format-file-max-size)))))

        ;; Check if binary
        (when (efrit-tool-binary-file-p path 8192)
          (signal 'user-error (list "Cannot format binary file" path)))

        ;; Read original content
        (let ((original-content (with-temp-buffer
                                  (insert-file-contents path)
                                  (buffer-string)))
              (new-content nil)
              (formatter-used nil))

          ;; Format based on file type
          (cond
           ;; Elisp files - use built-in formatting
           ((efrit-tool-format-file--elisp-file-p path)
            (setq new-content (efrit-tool-format-file--format-elisp original-content))
            (setq formatter-used "emacs-lisp-mode indent"))

           ;; Other files - try external formatter
           ((efrit-tool-format-file--find-formatter path)
            (let* ((formatter (efrit-tool-format-file--find-formatter path))
                   (result (efrit-tool-format-file--run-external path formatter)))
              (if (car result)
                  (progn
                    ;; Re-read file after formatter modified it
                    (setq new-content (with-temp-buffer
                                        (insert-file-contents path)
                                        (buffer-string)))
                    (setq formatter-used (format "%s" (car formatter))))
                ;; Formatter failed
                (signal 'user-error (list (cdr result))))))

           ;; No formatter available
           (t
            (signal 'user-error
                    (list (format "No formatter configured for file type: %s"
                                  (or (file-name-extension path) "no extension"))))))

          ;; Check if anything changed
          (if (string= original-content new-content)
              ;; No changes
              (efrit-tool-success
               `((path . ,path)
                 (path_relative . ,path-relative)
                 (formatter . ,formatter-used)
                 (changed . :json-false)
                 (message . "File already formatted correctly"))
               warnings)

            ;; For elisp, we need to write the changes ourselves
            (when (efrit-tool-format-file--elisp-file-p path)
              (with-temp-file path
                (insert new-content)))

            ;; Generate and return diff
            (let ((diff-output (efrit-tool-format-file--generate-diff
                                original-content new-content path)))
              (efrit-tool-success
               `((path . ,path)
                 (path_relative . ,path-relative)
                 (formatter . ,formatter-used)
                 (changed . t)
                 (diff . ,diff-output)
                 (old_size . ,(length original-content))
                 (new_size . ,(length new-content)))
               warnings))))))))

(provide 'efrit-tool-format-file)

;;; efrit-tool-format-file.el ends here
