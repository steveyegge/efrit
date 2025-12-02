;;; efrit-tool-get-diagnostics.el --- Diagnostics retrieval tool -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Keywords: ai, tools
;; Version: 0.4.1

;;; Commentary:
;;
;; This tool retrieves compiler/linter errors from Emacs diagnostic systems.
;; Supports:
;; - Flymake (built-in since Emacs 26)
;; - Flycheck (popular third-party checker)
;; - LSP-mode diagnostics
;; - Compilation buffer errors
;;
;; Returns structured data suitable for the edit-compile-fix loop.

;;; Code:

(require 'efrit-tool-utils)
(require 'cl-lib)

;; Forward declarations for optional packages
(defvar compilation-error-regexp-alist-alist)
(declare-function compilation-next-error "compile")
(declare-function compilation--message->loc "compile")
(declare-function compilation--loc->file-struct "compile")
(declare-function compilation--loc->line "compile")
(declare-function compilation--loc->col "compile")

;;; Customization

(defcustom efrit-tool-get-diagnostics-max-results 100
  "Maximum number of diagnostics to return."
  :type 'integer
  :group 'efrit-tool-utils)

;;; Flymake Support

(defun efrit-tool-get-diagnostics--flymake-available-p ()
  "Check if Flymake is available and active."
  (and (featurep 'flymake)
       (bound-and-true-p flymake-mode)))

(defun efrit-tool-get-diagnostics--flymake-severity (type)
  "Convert Flymake TYPE to severity string."
  (pcase type
    (:error "error")
    (:warning "warning")
    (:note "info")
    (_ "info")))

(defun efrit-tool-get-diagnostics--from-flymake (buffer)
  "Get diagnostics from Flymake for BUFFER.
Returns a list of diagnostic alists."
  (when (and (featurep 'flymake)
             (buffer-live-p buffer))
    (with-current-buffer buffer
      (when (bound-and-true-p flymake-mode)
        (let ((diagnostics '()))
          (when (fboundp 'flymake-diagnostics)
            (dolist (diag (flymake-diagnostics))
              (when (fboundp 'flymake-diagnostic-beg)
                (let* ((beg (flymake-diagnostic-beg diag))
                       (end (when (fboundp 'flymake-diagnostic-end)
                              (flymake-diagnostic-end diag)))
                       (type (when (fboundp 'flymake-diagnostic-type)
                               (flymake-diagnostic-type diag)))
                       (text (when (fboundp 'flymake-diagnostic-text)
                               (flymake-diagnostic-text diag))))
                  (push `((source . "flymake")
                          (severity . ,(efrit-tool-get-diagnostics--flymake-severity type))
                          (message . ,text)
                          (line . ,(line-number-at-pos beg))
                          (column . ,(save-excursion
                                       (goto-char beg)
                                       (current-column)))
                          (end_line . ,(when end (line-number-at-pos end)))
                          (end_column . ,(when end
                                           (save-excursion
                                             (goto-char end)
                                             (current-column)))))
                        diagnostics)))))
          (nreverse diagnostics))))))

;;; Flycheck Support

(defun efrit-tool-get-diagnostics--flycheck-available-p ()
  "Check if Flycheck is available and active."
  (and (featurep 'flycheck)
       (bound-and-true-p flycheck-mode)))

(defun efrit-tool-get-diagnostics--flycheck-severity (level)
  "Convert Flycheck LEVEL to severity string."
  (pcase level
    ('error "error")
    ('warning "warning")
    ('info "info")
    (_ "info")))

(defun efrit-tool-get-diagnostics--from-flycheck (buffer)
  "Get diagnostics from Flycheck for BUFFER.
Returns a list of diagnostic alists."
  (when (and (featurep 'flycheck)
             (buffer-live-p buffer))
    (with-current-buffer buffer
      (when (bound-and-true-p flycheck-mode)
        (let ((diagnostics '()))
          (when (bound-and-true-p flycheck-current-errors)
            (dolist (err flycheck-current-errors)
              (when (fboundp 'flycheck-error-line)
                (push `((source . ,(or (when (fboundp 'flycheck-error-checker)
                                         (symbol-name (flycheck-error-checker err)))
                                       "flycheck"))
                        (severity . ,(efrit-tool-get-diagnostics--flycheck-severity
                                      (when (fboundp 'flycheck-error-level)
                                        (flycheck-error-level err))))
                        (message . ,(when (fboundp 'flycheck-error-message)
                                      (flycheck-error-message err)))
                        (line . ,(flycheck-error-line err))
                        (column . ,(when (fboundp 'flycheck-error-column)
                                     (flycheck-error-column err)))
                        (end_line . ,(when (fboundp 'flycheck-error-end-line)
                                       (flycheck-error-end-line err)))
                        (end_column . ,(when (fboundp 'flycheck-error-end-column)
                                         (flycheck-error-end-column err))))
                      diagnostics))))
          (nreverse diagnostics))))))

;;; LSP-mode Support

(defun efrit-tool-get-diagnostics--lsp-available-p ()
  "Check if LSP-mode is available and active."
  (and (featurep 'lsp-mode)
       (bound-and-true-p lsp-mode)))

(defun efrit-tool-get-diagnostics--lsp-severity (severity)
  "Convert LSP SEVERITY number to string.
LSP spec: 1=Error, 2=Warning, 3=Information, 4=Hint."
  (pcase severity
    (1 "error")
    (2 "warning")
    (3 "info")
    (4 "hint")
    (_ "info")))

(defun efrit-tool-get-diagnostics--from-lsp (buffer)
  "Get diagnostics from LSP-mode for BUFFER.
Returns a list of diagnostic alists."
  (when (and (featurep 'lsp-mode)
             (buffer-live-p buffer))
    (with-current-buffer buffer
      (when (bound-and-true-p lsp-mode)
        (let ((diagnostics '()))
          (when (fboundp 'lsp-diagnostics)
            (let* ((file (buffer-file-name))
                   (uri (when (and file (fboundp 'lsp--path-to-uri))
                          (lsp--path-to-uri file)))
                   (diags (when uri
                            (gethash uri (lsp-diagnostics)))))
              (dolist (diag diags)
                (let* ((range (when (fboundp 'lsp:diagnostic-range)
                                (lsp:diagnostic-range diag)))
                       (start (when (and range (fboundp 'lsp:range-start))
                                (lsp:range-start range)))
                       (end (when (and range (fboundp 'lsp:range-end))
                              (lsp:range-end range))))
                  (push `((source . ,(or (when (fboundp 'lsp:diagnostic-source)
                                           (lsp:diagnostic-source diag))
                                         "lsp"))
                          (severity . ,(efrit-tool-get-diagnostics--lsp-severity
                                        (when (fboundp 'lsp:diagnostic-severity)
                                          (lsp:diagnostic-severity diag))))
                          (message . ,(when (fboundp 'lsp:diagnostic-message)
                                        (lsp:diagnostic-message diag)))
                          (line . ,(when (and start (fboundp 'lsp:position-line))
                                     (1+ (lsp:position-line start))))
                          (column . ,(when (and start (fboundp 'lsp:position-character))
                                       (lsp:position-character start)))
                          (end_line . ,(when (and end (fboundp 'lsp:position-line))
                                         (1+ (lsp:position-line end))))
                          (end_column . ,(when (and end (fboundp 'lsp:position-character))
                                           (lsp:position-character end))))
                        diagnostics)))))
          (nreverse diagnostics))))))

;;; Compilation Buffer Support

(defun efrit-tool-get-diagnostics--from-compilation ()
  "Get errors from the most recent compilation buffer.
Returns a list of diagnostic alists."
  (let ((diagnostics '())
        (comp-buffer (get-buffer "*compilation*")))
    (when (and comp-buffer (buffer-live-p comp-buffer))
      (with-current-buffer comp-buffer
        (save-excursion
          (goto-char (point-min))
          (while (and (< (length diagnostics) efrit-tool-get-diagnostics-max-results)
                      (re-search-forward compilation-error-regexp-alist-alist nil t))
            (when-let* ((msg (compilation-next-error 1 nil (point))))
              (let* ((loc (compilation--message->loc msg))
                     (file (when loc (car (compilation--loc->file-struct loc))))
                     (line (when loc (compilation--loc->line loc)))
                     (col (when loc (compilation--loc->col loc))))
                (when file
                  (push `((source . "compilation")
                          (severity . "error")
                          (message . ,(buffer-substring-no-properties
                                       (line-beginning-position)
                                       (line-end-position)))
                          (file . ,file)
                          (line . ,line)
                          (column . ,col))
                        diagnostics))))))))
    (nreverse diagnostics)))

;;; Main Entry Point

(defun efrit-tool-get-diagnostics (args)
  "Get diagnostics from IDE systems (Flymake, Flycheck, LSP, compilation).

ARGS is an alist with:
  path - optional file path to get diagnostics for
         If nil, uses current buffer
  sources - optional list of sources to query
            Options: \"flymake\", \"flycheck\", \"lsp\", \"compilation\", \"all\"
            Default: \"all\"
  severity - optional minimum severity filter
             Options: \"error\", \"warning\", \"info\"
             Default: include all

Returns a standard tool response with diagnostics data."
  (efrit-tool-execute get_diagnostics args
    (let* ((path (alist-get 'path args))
           (sources-input (alist-get 'sources args))
           (sources (cond
                     ((null sources-input) '(all))
                     ((stringp sources-input) (list (intern sources-input)))
                     ((vectorp sources-input) (mapcar #'intern (append sources-input nil)))
                     ((listp sources-input) (mapcar #'intern sources-input))
                     (t '(all))))
           (severity-filter (alist-get 'severity args))
           (all-sources (memq 'all sources))
           (buffer (if path
                       (find-buffer-visiting (expand-file-name path))
                     (current-buffer)))
           (all-diagnostics '())
           (warnings '()))

      ;; If path specified but buffer not found, try to find file
      (when (and path (not buffer))
        (let ((file-path (efrit-resolve-path-simple path)))
          (when (file-exists-p file-path)
            (setq buffer (find-file-noselect file-path)))))

      ;; Collect diagnostics from each source
      (when buffer
        ;; Flymake
        (when (or all-sources (memq 'flymake sources))
          (let ((flymake-diags (efrit-tool-get-diagnostics--from-flymake buffer)))
            (setq all-diagnostics (append all-diagnostics flymake-diags))))

        ;; Flycheck
        (when (or all-sources (memq 'flycheck sources))
          (let ((flycheck-diags (efrit-tool-get-diagnostics--from-flycheck buffer)))
            (setq all-diagnostics (append all-diagnostics flycheck-diags))))

        ;; LSP
        (when (or all-sources (memq 'lsp sources))
          (let ((lsp-diags (efrit-tool-get-diagnostics--from-lsp buffer)))
            (setq all-diagnostics (append all-diagnostics lsp-diags)))))

      ;; Compilation buffer (not buffer-specific)
      (when (or all-sources (memq 'compilation sources))
        (let ((comp-diags (efrit-tool-get-diagnostics--from-compilation)))
          (setq all-diagnostics (append all-diagnostics comp-diags))))

      ;; Apply severity filter
      (when severity-filter
        (let ((min-severity (pcase severity-filter
                              ("error" 0)
                              ("warning" 1)
                              ("info" 2)
                              (_ 2))))
          (setq all-diagnostics
                (cl-remove-if-not
                 (lambda (diag)
                   (let ((sev (alist-get 'severity diag)))
                     (<= (pcase sev
                           ("error" 0)
                           ("warning" 1)
                           (_ 2))
                         min-severity)))
                 all-diagnostics))))

      ;; Truncate if needed
      (when (> (length all-diagnostics) efrit-tool-get-diagnostics-max-results)
        (push (format "Truncated from %d to %d diagnostics"
                      (length all-diagnostics)
                      efrit-tool-get-diagnostics-max-results)
              warnings)
        (setq all-diagnostics
              (seq-take all-diagnostics efrit-tool-get-diagnostics-max-results)))

      ;; Report active diagnostic systems
      (let ((active-systems '()))
        (when (efrit-tool-get-diagnostics--flymake-available-p)
          (push "flymake" active-systems))
        (when (efrit-tool-get-diagnostics--flycheck-available-p)
          (push "flycheck" active-systems))
        (when (efrit-tool-get-diagnostics--lsp-available-p)
          (push "lsp" active-systems))
        (when (get-buffer "*compilation*")
          (push "compilation" active-systems))

        (efrit-tool-success
         `((diagnostics . ,(vconcat all-diagnostics))
           (count . ,(length all-diagnostics))
           (buffer . ,(when buffer (buffer-name buffer)))
           (file . ,(when buffer (buffer-file-name buffer)))
           (active_systems . ,(vconcat active-systems)))
         warnings)))))

(provide 'efrit-tool-get-diagnostics)

;;; efrit-tool-get-diagnostics.el ends here
