;;; efrit-tool-read-file.el --- File reading tool -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Keywords: ai, tools
;; Version: 0.4.0

;;; Commentary:
;;
;; This tool provides Claude with the ability to read arbitrary files,
;; not just those open in Emacs buffers.
;;
;; Key features:
;; - Line range support (read specific sections)
;; - Binary file detection
;; - Large file truncation
;; - Sensitive file detection
;; - Encoding detection

;;; Code:

(require 'efrit-tool-utils)
(require 'cl-lib)

;;; Customization

(defcustom efrit-tool-read-file-max-size 100000
  "Default maximum bytes to read from a file."
  :type 'integer
  :group 'efrit-tool-utils)

(defcustom efrit-tool-read-file-max-line-length 2000
  "Maximum line length before truncating individual lines."
  :type 'integer
  :group 'efrit-tool-utils)

;;; MIME Type Detection

(defconst efrit-tool-read-file--mime-types
  '(("png" . "image/png")
    ("jpg" . "image/jpeg")
    ("jpeg" . "image/jpeg")
    ("gif" . "image/gif")
    ("webp" . "image/webp")
    ("svg" . "image/svg+xml")
    ("pdf" . "application/pdf")
    ("zip" . "application/zip")
    ("gz" . "application/gzip")
    ("tar" . "application/x-tar")
    ("mp3" . "audio/mpeg")
    ("mp4" . "video/mp4")
    ("wav" . "audio/wav")
    ("json" . "application/json")
    ("xml" . "application/xml"))
  "Common file extensions and their MIME types.")

(defun efrit-tool-read-file--mime-type (path)
  "Get MIME type for file at PATH based on extension."
  (let ((ext (file-name-extension path)))
    (when ext
      (cdr (assoc (downcase ext) efrit-tool-read-file--mime-types)))))

;;; Encoding Detection

(defun efrit-tool-read-file--detect-encoding (path)
  "Detect encoding of file at PATH.
Returns the coding system to use."
  (with-temp-buffer
    (insert-file-contents-literally path nil 0 1024)
    (goto-char (point-min))
    (cond
     ;; Check for Emacs-style coding cookie
     ((re-search-forward "-\\*-.*coding: *\\([^ \t;]+\\)" nil t)
      (intern (match-string 1)))
     ;; Check UTF-8 BOM
     ((looking-at "\xef\xbb\xbf")
      'utf-8-with-signature)
     ;; Check UTF-16 BOM
     ((looking-at "\xff\xfe")
      'utf-16-le)
     ((looking-at "\xfe\xff")
      'utf-16-be)
     ;; Default to utf-8
     (t 'utf-8))))

;;; Main Implementation

(defun efrit-tool-read-file--read-lines (path start end encoding max-size)
  "Read lines START to END from file at PATH.
ENCODING is the coding system to use.
MAX-SIZE limits total bytes read.
Returns a plist with :content, :lines-read, :total-lines, :truncated."
  (let ((coding-system-for-read encoding)
        content lines-read total-lines truncated)
    (with-temp-buffer
      (condition-case err
          (progn
            (if (and start end)
                ;; Line range specified - read efficiently
                (progn
                  (insert-file-contents path nil nil nil)
                  (setq total-lines (count-lines (point-min) (point-max)))
                  (goto-char (point-min))
                  (forward-line (1- start))
                  (let ((range-start (point)))
                    (forward-line (- end start -1))
                    (setq content (buffer-substring-no-properties range-start (point)))
                    (setq lines-read (format "%d-%d" start (min end total-lines)))))
              ;; No range - read with size limit
              (if (> (file-attribute-size (file-attributes path)) max-size)
                  (progn
                    (insert-file-contents path nil 0 max-size)
                    (setq truncated t)
                    ;; Count total lines in file
                    (setq total-lines
                          (with-temp-buffer
                            (insert-file-contents path)
                            (count-lines (point-min) (point-max)))))
                (insert-file-contents path)
                (setq total-lines (count-lines (point-min) (point-max))))
              (setq content (buffer-string))
              (setq lines-read (format "1-%d" (count-lines (point-min) (point-max))))))
        (error
         (signal 'file-error (list "Error reading file" (error-message-string err)))))
      (list :content content
            :lines-read lines-read
            :total-lines total-lines
            :truncated truncated))))

(defun efrit-tool-read-file (args)
  "Read a file and return its contents with metadata.

ARGS is an alist with:
  path       - file to read (required)
  start_line - line to start from (1-indexed, optional)
  end_line   - line to end at (inclusive, optional)
  encoding   - encoding override (optional)
  max_size   - max bytes to read (default: 100000)

Returns a standard tool response with file contents."
  (efrit-tool-execute read_file args
    (let* ((path-input (or (alist-get 'path args) ""))
           (start-line (alist-get 'start_line args))
           (end-line (alist-get 'end_line args))
           (encoding-override (alist-get 'encoding args))
           (max-size (or (alist-get 'max_size args)
                         efrit-tool-read-file-max-size))
           (warnings '()))

      ;; Validate path is provided
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
          (push (format "Sensitive file: %s" path-relative) warnings))

        ;; Check if binary
        (if (efrit-tool-binary-file-p path 8192)
            ;; Binary file - return metadata only
            (let ((attrs (file-attributes path)))
              (efrit-tool-success
               `((path . ,path)
                 (path_relative . ,path-relative)
                 (is_binary . t)
                 (size . ,(file-attribute-size attrs))
                 (mtime . ,(efrit-tool-format-time
                            (file-attribute-modification-time attrs)))
                 (mime_type . ,(or (efrit-tool-read-file--mime-type path)
                                   "application/octet-stream"))
                 (content . :json-null))
               (cons "Binary file detected, content not returned" warnings)))

          ;; Text file - read contents
          (let* ((encoding (if encoding-override
                               (intern encoding-override)
                             (efrit-tool-read-file--detect-encoding path)))
                 (read-result (efrit-tool-read-file--read-lines
                               path start-line end-line encoding max-size))
                 (attrs (file-attributes path)))

            (when (plist-get read-result :truncated)
              (push (format "File truncated at %dKB. Use start_line/end_line for specific sections."
                            (/ max-size 1000))
                    warnings))

            (efrit-tool-success
             `((path . ,path)
               (path_relative . ,path-relative)
               (content . ,(plist-get read-result :content))
               (encoding . ,(symbol-name encoding))
               (size . ,(file-attribute-size attrs))
               (mtime . ,(efrit-tool-format-time
                          (file-attribute-modification-time attrs)))
               (total_lines . ,(plist-get read-result :total-lines))
               (lines_returned . ,(plist-get read-result :lines-read))
               (truncated . ,(if (plist-get read-result :truncated) t :json-false)))
             warnings)))))))

(provide 'efrit-tool-read-file)

;;; efrit-tool-read-file.el ends here
