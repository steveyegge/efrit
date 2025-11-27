;;; efrit-tool-read-image.el --- Image reading tool for multimodal support -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Keywords: ai, tools, images
;; Version: 0.3.0

;;; Commentary:
;;
;; This tool provides Claude with the ability to read and analyze image files.
;; Images are returned as base64-encoded content blocks that Claude can
;; process visually.
;;
;; Supported formats:
;; - PNG (image/png)
;; - JPEG (image/jpeg)
;; - GIF (image/gif)
;; - WebP (image/webp)
;;
;; Size limits:
;; - Maximum 5MB per image (Claude API recommendation)
;; - Warning at 1MB

;;; Code:

(require 'efrit-tool-utils)
(require 'cl-lib)

;;; Customization

(defcustom efrit-tool-read-image-max-size (* 5 1024 1024)
  "Maximum image size in bytes (default 5MB).
Claude API recommends images under 5MB for best performance."
  :type 'integer
  :group 'efrit-tool-utils)

(defcustom efrit-tool-read-image-warn-size (* 1 1024 1024)
  "Image size in bytes at which to emit a warning (default 1MB)."
  :type 'integer
  :group 'efrit-tool-utils)

;;; Supported Image Types

(defconst efrit-tool-read-image--supported-types
  '(("png" . "image/png")
    ("jpg" . "image/jpeg")
    ("jpeg" . "image/jpeg")
    ("gif" . "image/gif")
    ("webp" . "image/webp"))
  "Supported image file extensions and their MIME types.
These are the formats supported by Claude's vision capabilities.")

(defun efrit-tool-read-image--get-mime-type (path)
  "Get MIME type for image file at PATH.
Returns nil if the file extension is not a supported image type."
  (let ((ext (file-name-extension path)))
    (when ext
      (cdr (assoc (downcase ext) efrit-tool-read-image--supported-types)))))

(defun efrit-tool-read-image--supported-p (path)
  "Return non-nil if PATH is a supported image file."
  (efrit-tool-read-image--get-mime-type path))

;;; Base64 Encoding

(defun efrit-tool-read-image--encode-base64 (path)
  "Read file at PATH and return base64-encoded string."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path)
    (base64-encode-region (point-min) (point-max) t)
    (buffer-string)))

;;; Main Implementation

(defun efrit-tool-read-image (args)
  "Read an image file and return it for visual analysis by Claude.

ARGS is an alist with:
  path - path to the image file (required)

Returns either:
- A standard tool error response if the file can't be read
- A special image response that will be converted to an image content block

The image is returned as base64-encoded data with the appropriate MIME type."
  (efrit-tool-execute read_image args
    (let* ((path-input (or (alist-get 'path args) ""))
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

        ;; Check it's a supported image type
        (let ((mime-type (efrit-tool-read-image--get-mime-type path)))
          (unless mime-type
            (signal 'user-error
                    (list (format "Unsupported image type: %s. Supported: png, jpg, jpeg, gif, webp"
                                  (or (file-name-extension path) "unknown")))))

          ;; Check file size
          (let ((size (file-attribute-size (file-attributes path))))
            (when (> size efrit-tool-read-image-max-size)
              (signal 'user-error
                      (list (format "Image too large: %s bytes (max %s bytes)"
                                    size efrit-tool-read-image-max-size))))

            (when (> size efrit-tool-read-image-warn-size)
              (push (format "Large image: %s bytes (may slow processing)"
                            size)
                    warnings))

            ;; Check for sensitive file
            (when (plist-get path-info :is-sensitive)
              (push (format "Sensitive file: %s" path-relative) warnings))

            ;; Read and encode the image
            (let ((base64-data (efrit-tool-read-image--encode-base64 path)))

              ;; Return special image response format
              ;; This will be detected and converted to an image content block
              ;; by efrit-chat.el
              `((success . t)
                (image . ((type . "image")
                          (source . ((type . "base64")
                                     (media_type . ,mime-type)
                                     (data . ,base64-data)))))
                (metadata . ((path . ,path)
                             (path_relative . ,path-relative)
                             (size . ,size)
                             (mime_type . ,mime-type)))
                ,@(when warnings
                    `((warnings . ,(vconcat warnings))))))))))))

(provide 'efrit-tool-read-image)

;;; efrit-tool-read-image.el ends here
