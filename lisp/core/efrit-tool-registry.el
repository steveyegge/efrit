;;; efrit-tool-registry.el --- Tools contributed by other packages -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; efrit's built-in tools live in three tables: the schema sent to the
;; model (`efrit-do--tools-schema'), the dispatch table that maps a
;; tool name to its handler (`efrit-do--tool-dispatch-table'), and the
;; permission class of each tool (`efrit-permission-tool-classes').
;; All three are constants, so a package outside efrit had no way to
;; offer the model a tool of its own without patching efrit.
;;
;; This module is that way.  `efrit-register-tool' takes one
;; description of a tool -- its name, what the model should know
;; about it, its input schema, the function that runs it, and its
;; permission class -- and the three tables consult the registry:
;; the schema getter appends registered tools, the dispatcher falls
;; back to the registry for names it does not know, and the class
;; lookup does the same.  Registration is idempotent and can be
;; undone.
;;
;; A registered tool's function receives the tool input as an alist
;; with string keys (the JSON object the model sent, converted from
;; the hash table efrit uses internally) and returns a string, which
;; becomes the tool result.  Errors are caught and returned as an
;; error result, so a buggy tool cannot end the turn.
;;
;; Nothing here decides anything: the model chooses when to call a
;; tool, and consent stays with the sandbox (the tool function is
;; expected to call `efrit-sandbox-check' as efrit's own tools do,
;; with the capability the tool needs; a `read' tool that only reads
;; the package's own data needs none).

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function efrit-log "efrit-log")

(cl-defstruct (efrit-registered-tool (:constructor efrit-registered-tool--create))
  "A tool offered to the model by a package outside efrit."
  name          ; string, the tool name the model calls
  description   ; string shown to the model
  input-schema  ; alist, JSON schema of the tool input (\"type\" \"object\" ...)
  function      ; (lambda (input-alist)) -> string
  class         ; permission class: read, write, exec, net, control
  package)      ; symbol naming the registering package, for listings

(defvar efrit-tool-registry nil
  "Alist of (NAME . `efrit-registered-tool') contributed by other packages.
Maintained by `efrit-register-tool' and `efrit-unregister-tool'; the
schema, dispatch and permission tables read it.")

(cl-defun efrit-register-tool (name &key description input-schema function
                                    (class 'read) package)
  "Offer the model a tool NAME implemented by FUNCTION.
DESCRIPTION is what the model reads to decide when to use it.
INPUT-SCHEMA is the JSON schema of the input as an alist in the shape
efrit uses for its own tools, for example

  \\='((\"type\" . \"object\")
    (\"properties\" . ((\"query\" . ((\"type\" . \"string\")
                                 (\"description\" . \"Gmail search\")))))
    (\"required\" . [\"query\"]))

FUNCTION takes one argument, the input as an alist with string keys,
and returns a string.  CLASS is the permission class (`read' by
default; `write', `exec', `net' or `control'), which decides whether
external review looks at the call.  PACKAGE names the registering
package for `efrit-list-registered-tools'.

Registering a NAME again replaces the earlier registration.  A NAME
that collides with one of efrit's own tools is refused: those come
first in dispatch and the registration would never be reached."
  (unless (and (stringp name)
               (let ((case-fold-search nil))
                 (string-match-p "\\`[a-z][a-z0-9_]*\\'" name)))
    (error "efrit-register-tool: NAME must be a lowercase identifier, got %S" name))
  (unless (functionp function)
    (error "efrit-register-tool: %s needs a FUNCTION" name))
  (unless (memq class '(read write exec net control))
    (error "efrit-register-tool: %s: unknown class %S" name class))
  (when (efrit-tool-registry--builtin-p name)
    (error "efrit-register-tool: %s is one of efrit's own tools" name))
  (let ((tool (efrit-registered-tool--create
               :name name :description (or description "")
               :input-schema (or input-schema '(("type" . "object") ("properties" . ())))
               :function function :class class :package package)))
    (setf (alist-get name efrit-tool-registry nil nil #'equal) tool)
    (when (fboundp 'efrit-log)
      (efrit-log 'debug "registered tool %s (%s) from %s" name class package))
    tool))

(defun efrit-unregister-tool (name)
  "Withdraw tool NAME.  Return non-nil if it was registered."
  (prog1 (assoc name efrit-tool-registry)
    (setf (alist-get name efrit-tool-registry nil 'remove #'equal) nil)))

(defun efrit-unregister-package-tools (package)
  "Withdraw every tool PACKAGE registered.  Return the names withdrawn."
  (let (names)
    (dolist (entry efrit-tool-registry)
      (when (eq (efrit-registered-tool-package (cdr entry)) package)
        (push (car entry) names)))
    (dolist (n names) (efrit-unregister-tool n))
    names))

(defun efrit-registered-tool-get (name)
  "The `efrit-registered-tool' called NAME, or nil."
  (cdr (assoc name efrit-tool-registry)))

(defun efrit-tool-registry--builtin-p (name)
  "Non-nil if NAME is one of efrit's own tools."
  (and (boundp 'efrit-do--tool-dispatch-table)
       (assoc name (symbol-value 'efrit-do--tool-dispatch-table))
       t))

(defun efrit-tool-registry-schema ()
  "The registered tools as schema entries, in the shape of `efrit-do--tools-schema'.
A list, for appending to that vector."
  (mapcar (lambda (entry)
            (let ((tool (cdr entry)))
              `(("name" . ,(efrit-registered-tool-name tool))
                ("description" . ,(efrit-registered-tool-description tool))
                ("input_schema" . ,(efrit-registered-tool-input-schema tool)))))
          (reverse efrit-tool-registry)))

(defun efrit-tool-registry-input->alist (input)
  "INPUT (a hash table, alist, string or nil) as an alist with string keys."
  (cond
   ((hash-table-p input)
    (let (out)
      (maphash (lambda (k v) (push (cons (format "%s" k) v) out)) input)
      (nreverse out)))
   ((stringp input) (list (cons "input" input)))
   ((listp input) input)
   (t nil)))

(defun efrit-tool-registry-dispatch (name input)
  "Run registered tool NAME on INPUT; return its result string, or nil if unknown.
Errors in the tool become an error result: the model is told and the
turn continues."
  (when-let* ((tool (efrit-registered-tool-get name)))
    (condition-case err
        (let ((result (funcall (efrit-registered-tool-function tool)
                               (efrit-tool-registry-input->alist input))))
          (cond ((stringp result) result)
                ((null result) "")
                (t (format "%S" result))))
      ((debug error)
       (when (fboundp 'efrit-log)
         (efrit-log 'warn "registered tool %s failed: %s" name (error-message-string err)))
       (format "\n[Error: %s failed: %s]" name (error-message-string err))))))

(defun efrit-tool-registry-class (name)
  "Permission class of registered tool NAME, or nil if not registered."
  (when-let* ((tool (efrit-registered-tool-get name)))
    (efrit-registered-tool-class tool)))

;;;###autoload
(defun efrit-list-registered-tools ()
  "Show the tools other packages have registered with efrit."
  (interactive)
  (if (null efrit-tool-registry)
      (message "efrit: no registered tools")
    (with-current-buffer (get-buffer-create "*efrit registered tools*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "%-28s %-8s %-16s %s\n" "tool" "class" "package" "description"))
        (dolist (entry (reverse efrit-tool-registry))
          (let ((tool (cdr entry)))
            (insert (format "%-28s %-8s %-16s %s\n"
                            (efrit-registered-tool-name tool)
                            (efrit-registered-tool-class tool)
                            (or (efrit-registered-tool-package tool) "")
                            (car (split-string (efrit-registered-tool-description tool) "\n"))))))
        (goto-char (point-min))
        (special-mode))
      (pop-to-buffer (current-buffer)))))

(provide 'efrit-tool-registry)

;;; efrit-tool-registry.el ends here
