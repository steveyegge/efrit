;;; efrit-tool-elisp-docs.el --- Emacs Lisp documentation lookup tool -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Keywords: ai, tools, help
;; Version: 0.3.0

;;; Commentary:
;;
;; This tool provides structured Emacs Lisp documentation lookup.
;; Unlike `describe-function' and friends, this returns structured
;; data that Claude can reliably parse, without side effects like
;; opening help buffers.
;;
;; Features:
;; - Automatic type detection (function, variable, face)
;; - Source file and line number lookup
;; - Related symbols via apropos
;; - Batch multiple lookups in one call

;;; Code:

(require 'efrit-tool-utils)
(require 'cl-lib)
(require 'help-fns)
(require 'find-func)

;;; Customization

(defcustom efrit-tool-elisp-docs-max-symbols 20
  "Maximum number of symbols to query in one request."
  :type 'integer
  :group 'efrit-tool-utils)

(defcustom efrit-tool-elisp-docs-max-related 10
  "Maximum number of related symbols to return from apropos."
  :type 'integer
  :group 'efrit-tool-utils)

;;; Implementation

(defun efrit-tool-elisp-docs--detect-type (symbol)
  "Detect the type of SYMBOL.
Returns \\='function, \\='variable, \\='face, or nil if unknown."
  (cond
   ((functionp symbol) 'function)
   ((facep symbol) 'face)
   ((boundp symbol) 'variable)
   ;; Check if it's a function defined in autoload
   ((and (fboundp symbol)
         (autoloadp (symbol-function symbol)))
    'function)
   ;; Check if it's a special form
   ((special-form-p symbol) 'function)
   ;; Check if it's a macro
   ((macrop symbol) 'function)
   (t nil)))

(defun efrit-tool-elisp-docs--get-function-signature (symbol)
  "Get the argument signature for function SYMBOL.
Returns a string like \"(ARG1 ARG2 &optional ARG3)\"."
  (condition-case nil
      (let* ((func (symbol-function symbol))
             (args (help-function-arglist symbol t)))
        (if (eq args t)
            nil  ; No arglist available
          (format "%S" args)))
    (error nil)))

(defun efrit-tool-elisp-docs--get-docstring (symbol type)
  "Get the docstring for SYMBOL of TYPE."
  (condition-case nil
      (pcase type
        ('function (documentation symbol t))
        ('variable (documentation-property symbol 'variable-documentation t))
        ('face (documentation-property symbol 'face-documentation t)))
    (error nil)))

(defun efrit-tool-elisp-docs--get-source-location (symbol type)
  "Get the source file and line for SYMBOL of TYPE.
Returns a plist (:file FILE :line LINE) or nil."
  (condition-case nil
      (let ((location
             (pcase type
               ('function (find-function-noselect symbol t))
               ('variable (find-variable-noselect symbol))
               ('face (find-face-definition symbol)))))
        (when (and location (consp location))
          (let* ((buffer (car location))
                 (pos (cdr location))
                 file line)
            (when buffer
              (with-current-buffer buffer
                (setq file (buffer-file-name))
                (when (and pos (numberp pos))
                  (save-excursion
                    (goto-char pos)
                    (setq line (line-number-at-pos))))))
            (when file
              (list :file file :line line)))))
    (error nil)))

(defun efrit-tool-elisp-docs--get-function-type (symbol)
  "Get the specific type of function SYMBOL.
Returns a string like \"function\", \"macro\", \"special-form\", \"autoload\"."
  (condition-case nil
      (cond
       ((special-form-p symbol) "special-form")
       ((macrop symbol) "macro")
       ((and (fboundp symbol) (autoloadp (symbol-function symbol))) "autoload")
       ((subrp (symbol-function symbol)) "primitive")
       ((commandp symbol) "command")
       ((functionp symbol) "function")
       (t "function"))
    (error "function")))

(defun efrit-tool-elisp-docs--get-variable-type (symbol)
  "Get additional info about variable SYMBOL.
Returns a string like \"user-option\", \"constant\", \"buffer-local\"."
  (cond
   ((custom-variable-p symbol) "user-option")
   ((get symbol 'constant) "constant")
   ((local-variable-if-set-p symbol) "buffer-local")
   (t "variable")))

(defun efrit-tool-elisp-docs--get-related (symbol)
  "Get related symbols using apropos.
Returns a list of symbol names."
  (condition-case nil
      (let* ((pattern (symbol-name symbol))
             ;; Run apropos but suppress output
             (apropos-do-all nil)
             (matches (apropos-internal pattern)))
        ;; Filter and limit
        (seq-take
         (cl-remove-if-not
          (lambda (s)
            (and (not (eq s symbol))
                 (or (fboundp s) (boundp s) (facep s))))
          matches)
         efrit-tool-elisp-docs-max-related))
    (error nil)))

(defun efrit-tool-elisp-docs--lookup-single (symbol-name type include-source include-related)
  "Look up documentation for SYMBOL-NAME.
TYPE is \\='function, \\='variable, \\='face, or \\='auto.
INCLUDE-SOURCE and INCLUDE-RELATED control optional fields."
  (let* ((symbol (intern-soft symbol-name))
         (detected-type (when symbol (efrit-tool-elisp-docs--detect-type symbol)))
         (actual-type (if (eq type 'auto) detected-type type)))

    (cond
     ;; Symbol doesn't exist
     ((not symbol)
      `((symbol . ,symbol-name)
        (found . :json-false)
        (error . "Symbol not found")))

     ;; Type mismatch - requested specific type but symbol isn't that
     ((and (not (eq type 'auto))
           (not (eq detected-type type)))
      `((symbol . ,symbol-name)
        (found . :json-false)
        (error . ,(format "Symbol is a %s, not a %s" detected-type type))))

     ;; Symbol doesn't match any type
     ((not detected-type)
      `((symbol . ,symbol-name)
        (found . :json-false)
        (error . "Symbol is not a function, variable, or face")))

     ;; Success - build result
     (t
      (let ((result `((symbol . ,symbol-name)
                      (found . t)
                      (type . ,(symbol-name actual-type)))))
        ;; Add type-specific fields
        (pcase actual-type
          ('function
           (setq result
                 (append result
                         `((function_type . ,(efrit-tool-elisp-docs--get-function-type symbol))
                           ,@(when-let* ((sig (efrit-tool-elisp-docs--get-function-signature symbol)))
                               `((signature . ,sig)))))))
          ('variable
           (setq result
                 (append result
                         `((variable_type . ,(efrit-tool-elisp-docs--get-variable-type symbol))))))
          ('face
           ;; Add face attributes
           (when-let* ((attrs (face-all-attributes symbol (selected-frame))))
             (setq result
                   (append result
                           `((face_attributes . ,(efrit-tool-elisp-docs--format-face-attrs attrs))))))))

        ;; Add docstring
        (when-let* ((docstring (efrit-tool-elisp-docs--get-docstring symbol actual-type)))
          (setq result (append result `((docstring . ,docstring)))))

        ;; Add source location if requested
        (when include-source
          (when-let* ((loc (efrit-tool-elisp-docs--get-source-location symbol actual-type)))
            (setq result
                  (append result
                          `((source_file . ,(plist-get loc :file))
                            ,@(when (plist-get loc :line)
                                `((source_line . ,(plist-get loc :line)))))))))

        ;; Add related symbols if requested
        (when include-related
          (when-let* ((related (efrit-tool-elisp-docs--get-related symbol)))
            (setq result
                  (append result
                          `((related_symbols . ,(vconcat (mapcar #'symbol-name related))))))))

        result)))))

(defun efrit-tool-elisp-docs--format-face-attrs (attrs)
  "Format face ATTRS alist for JSON output."
  (let ((formatted '()))
    (dolist (attr attrs)
      (when (and (cdr attr) (not (eq (cdr attr) 'unspecified)))
        (push `(,(car attr) . ,(format "%s" (cdr attr))) formatted)))
    (nreverse formatted)))

(defun efrit-tool-elisp-docs (args)
  "Look up Emacs Lisp documentation.

ARGS is an alist with:
  symbol - single symbol name or list of symbol names (required)
  type - \\='function, \\='variable, \\='face, or \\='auto (default: \\='auto)
  include_source - whether to include source location (default: false)
  related - whether to include related symbols via apropos (default: false)

Returns a standard tool response with documentation for each symbol."
  (efrit-tool-execute elisp_docs args
    (let* ((symbol-input (alist-get 'symbol args))
           ;; Normalize to list
           (symbols (cond
                     ((stringp symbol-input) (list symbol-input))
                     ((symbolp symbol-input) (list (symbol-name symbol-input)))
                     ((listp symbol-input) (mapcar (lambda (s)
                                                     (if (symbolp s)
                                                         (symbol-name s)
                                                       s))
                                                   symbol-input))
                     ((vectorp symbol-input)
                      (mapcar (lambda (s)
                                (if (symbolp s)
                                    (symbol-name s)
                                  s))
                              (append symbol-input nil)))
                     (t (signal 'user-error (list "symbol must be a string or list")))))
           (type-input (alist-get 'type args 'auto))
           (type (cond
                  ((eq type-input 'auto) 'auto)
                  ((stringp type-input) (intern type-input))
                  ((symbolp type-input) type-input)
                  (t 'auto)))
           (include-source (alist-get 'include_source args))
           (include-related (alist-get 'related args))
           (warnings '()))

      ;; Validate
      (unless symbols
        (signal 'user-error (list "symbol is required")))

      ;; Validate type
      (unless (memq type '(auto function variable face))
        (signal 'user-error
                (list (format "type must be 'function', 'variable', 'face', or 'auto', got: %s"
                              type))))

      ;; Limit symbols
      (when (> (length symbols) efrit-tool-elisp-docs-max-symbols)
        (push (format "Only querying first %d of %d symbols"
                      efrit-tool-elisp-docs-max-symbols (length symbols))
              warnings)
        (setq symbols (seq-take symbols efrit-tool-elisp-docs-max-symbols)))

      ;; Look up each symbol
      (let ((results (mapcar (lambda (sym)
                               (efrit-tool-elisp-docs--lookup-single
                                sym type include-source include-related))
                             symbols)))
        (efrit-tool-success
         `((results . ,(vconcat results))
           (queried . ,(length symbols)))
         warnings)))))

(provide 'efrit-tool-elisp-docs)

;;; efrit-tool-elisp-docs.el ends here
