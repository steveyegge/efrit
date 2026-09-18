;;; efrit-tool-edit-buffer.el --- Buffer creation and editing tools -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai
;; URL: https://github.com/steveyegge/efrit

;;; Commentary:
;; High-level tools for creating and editing buffers.
;; These tools provide a more natural interface than eval_sexp for common
;; buffer operations like creating new buffers with content or editing files.

;;; Code:

(require 'cl-lib)
(require 'efrit-sandbox)

(declare-function efrit-log "efrit-log")

;; Fallback logging if efrit-log not available
(condition-case nil
    (require 'efrit-log)
  (error
   (defun efrit-log (level format-string &rest args)
     "Fallback logging function."
     (when (memq level '(warn error))
       (message (apply #'format format-string args))))))

;;; Buffer Creation Tool

(defvar efrit-tool-report-keys-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keys layered onto every buffer `buffer_create' makes.")

(defun efrit-tool--make-report-quittable ()
  "Give the current buffer a `q' that closes its window, whatever its mode.
Composed under the mode map, so a mode that already binds q wins."
  (unless (and (current-local-map)
               (eq (lookup-key (current-local-map) (kbd "q")) #'quit-window))
    (use-local-map (make-composed-keymap (current-local-map) efrit-tool-report-keys-map)))
  (setq-local efrit-tool-report-buffer t))

(defvar-local efrit-tool-report-buffer nil
  "Non-nil in buffers created by `buffer_create'.")

(defun efrit-tool-create-buffer (args)
  "Create a new buffer with optional initial content and mode.

ARGS is an alist with the following keys:
  - name (required): Buffer name (string)
  - content (optional): Initial content to insert
  - mode (optional): Major mode to enable (symbol or string)
  - read-only (optional): Whether buffer should be read-only (boolean)

Returns a string describing what was created."
  (condition-case err
      (let* ((name (alist-get 'name args))
             (content (alist-get 'content args ""))
             (mode (alist-get 'mode args))
             (read-only (alist-get 'read-only args nil)))
        
        ;; Validate inputs
        (unless (stringp name)
          (error "Buffer name must be a string, got %s" (type-of name)))
        (when content
          (unless (stringp content)
            (error "Content must be a string, got %s" (type-of content))))
        
        ;; An existing buffer of this name would be erased below; if it
        ;; visits a file outside the project, that is a destructive
        ;; touch and needs a grant.  A brand-new buffer needs none.
        (when-let* ((existing (get-buffer name)))
          (efrit-sandbox-check-buffer existing "create_buffer"))

        ;; Create buffer
        (let ((buffer (get-buffer-create name)))
          (with-current-buffer buffer
            (let ((inhibit-read-only t))
              ;; Clear any existing content
              (erase-buffer)
              ;; Insert content
              (when content
                (insert content))
              ;; Set major mode if provided
              (when mode
                (let ((mode-symbol (if (stringp mode)
                                       (intern (format "%s" mode))
                                     mode)))
                  (when (fboundp mode-symbol)
                    (funcall mode-symbol))))
              ;; A report buffer behaves like a popup: `q' closes it.
              ;; The major mode keeps its own keys; only q is added.
              (efrit-tool--make-report-quittable)
              ;; Set read-only if requested
              (when read-only
                (setq buffer-read-only t))))
          
          ;; Return success message
          (format "Created buffer '%s' with %d characters"
                  name (length (or content "")))))
    (efrit-sandbox-denied (signal (car err) (cdr err)))
    (error
     (format "Error creating buffer: %s" (error-message-string err)))))

;;; Buffer Editing Tool

(defun efrit-tool-edit-buffer (args)
  "Edit an existing buffer by inserting or replacing text.

ARGS is an alist with the following keys:
  - buffer (required): Buffer name or buffer object
  - text (required): Text to insert
  - position (optional): Where to insert ('start, 'end, 'point, or line number, default 'end)
  - replace (optional): If non-nil, replace region. Requires 'from-pos' and 'to-pos'
  - from-pos (optional): Start position for replacement
  - to-pos (optional): End position for replacement

Returns a string describing the edit operation."
  (condition-case err
      (let* ((buffer-arg (alist-get 'buffer args))
             (text (alist-get 'text args ""))
             (position (alist-get 'position args 'end))
             (replace (alist-get 'replace args nil))
             (from-pos (alist-get 'from-pos args))
             (to-pos (alist-get 'to-pos args))
             (buffer (cond
                      ((bufferp buffer-arg) buffer-arg)
                      ((stringp buffer-arg) (get-buffer buffer-arg))
                      (t (error "Invalid buffer argument")))))
        
        ;; Validate inputs
        (unless text
          (error "Text to insert/replace is required"))
        (unless (stringp text)
          (error "Text must be a string"))
        (unless buffer
          (error "Buffer %s not found" buffer-arg))
        (efrit-sandbox-check-buffer buffer "edit_buffer")

        ;; Edit the buffer
        (with-current-buffer buffer
          (let ((inhibit-read-only t))
            (if replace
                ;; Replace mode: delete region and insert text
                (progn
                  (unless (and from-pos to-pos)
                    (error "Replace mode requires 'from-pos' and 'to-pos'"))
                  (unless (and (integerp from-pos) (integerp to-pos))
                    (error "Positions must be integers"))
                  (delete-region from-pos to-pos)
                  (goto-char from-pos)
                  (insert text)
                  (format "Replaced text in buffer '%s' at positions %d-%d (%d chars)"
                          (buffer-name) from-pos to-pos (length text)))
              
              ;; Insert mode: insert text at specified position
              (progn
                (goto-char
                 (cond
                  ((eq position 'start) (point-min))
                  ((eq position 'end) (point-max))
                  ((eq position 'point) (point))
                  ((integerp position) position)
                  (t (error "Invalid position: %s" position))))
                (insert text)
                (format "Inserted %d characters into buffer '%s' at position %s"
                        (length text) (buffer-name) position))))))
    (efrit-sandbox-denied (signal (car err) (cdr err)))
    (error
     (format "Error editing buffer: %s" (error-message-string err)))))

;;; Buffer Reading Tool

(defun efrit-tool-read-buffer (args)
  "Read contents of a buffer.

ARGS is an alist with the following keys:
  - buffer (required): Buffer name or buffer object
  - start (optional): Start position (default 1/point-min)
  - end (optional): End position (default point-max)

Returns the buffer contents as a string."
  (condition-case err
      (let* ((buffer-arg (alist-get 'buffer args))
             (start (alist-get 'start args))
             (end (alist-get 'end args))
             (buffer (cond
                      ((bufferp buffer-arg) buffer-arg)
                      ((stringp buffer-arg) (get-buffer buffer-arg))
                      (t (error "Invalid buffer argument")))))
        
        (unless buffer
          (error "Buffer %s not found" buffer-arg))
        (efrit-sandbox-check-buffer buffer "read_buffer")

        (with-current-buffer buffer
          (let ((min (if (integerp start) start (point-min)))
                (max (if (integerp end) end (point-max))))
            (buffer-substring-no-properties min max))))
    (efrit-sandbox-denied (signal (car err) (cdr err)))
    (error
     (format "Error reading buffer: %s" (error-message-string err)))))

;;; Buffer Info Tool

(defun efrit-tool-buffer-info (args)
  "Get information about a buffer.

ARGS is an alist with key:
  - buffer (required): Buffer name or buffer object

Returns an alist with buffer properties."
  (condition-case err
      (let* ((buffer-arg (alist-get 'buffer args))
             (buffer (cond
                      ((bufferp buffer-arg) buffer-arg)
                      ((stringp buffer-arg) (get-buffer buffer-arg))
                      (t (error "Invalid buffer argument")))))
        
        (unless buffer
          (error "Buffer %s not found" buffer-arg))
        (efrit-sandbox-check-buffer buffer "buffer_info")

        (with-current-buffer buffer
          `((name . ,(buffer-name))
            (file . ,buffer-file-name)
            (modified . ,(buffer-modified-p))
            (read-only . ,buffer-read-only)
            (size . ,(buffer-size))
            (major-mode . ,(symbol-name major-mode))
            (point . ,(point))
            (point-min . ,(point-min))
            (point-max . ,(point-max)))))
    (efrit-sandbox-denied (signal (car err) (cdr err)))
    (error
     (format "Error getting buffer info: %s" (error-message-string err)))))

(provide 'efrit-tool-edit-buffer)

;;; efrit-tool-edit-buffer.el ends here
