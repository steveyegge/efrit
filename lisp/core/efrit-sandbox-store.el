;;; efrit-sandbox-store.el --- Persist project grants as JSON -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; Project-scoped grants live in <project>/.efrit/sandbox.json:
;;
;;   {"version": 1,
;;    "grants": [{"cap": "write", "target": "/home/u/proj/", "scope": "project"},
;;               {"cap": "elisp", "target": true,          "scope": "project"}]}
;;
;; JSON on purpose: it cannot carry code.  An earlier design used a
;; custom theme, which is an elisp file that `load-theme' evaluates;
;; keeping that safe meant validating the file's shape on every load
;; and hoping nobody ever pointed `load-theme' at it.  A JSON file has
;; no evaluation path -- `json-parse-string' returns data or signals.
;;
;; The file is still validated field by field (unknown caps, relative
;; paths, non-project scopes are dropped, not trusted) and written
;; 0600.  `.efrit/' is in `efrit-sandbox-always-deny', so no tool,
;; including eval_sexp, can write it; only the user's explicit grant
;; through the prompt does.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'efrit-sandbox)
(require 'efrit-log)

(defconst efrit-sandbox-store--file-name "sandbox.json")
(defconst efrit-sandbox-store--dir ".efrit")
(defconst efrit-sandbox-store--version 1)

(defun efrit-sandbox-store-file (root)
  "Path of the grants file for project ROOT."
  (expand-file-name efrit-sandbox-store--file-name
                    (expand-file-name efrit-sandbox-store--dir root)))

;;; Validation

(defconst efrit-sandbox-store--buffer-target-prefix "buffer:"
  "A JSON target string with this prefix is a fileless-buffer grant.
The remainder is the buffer name; it deserializes to (buffer . NAME).
Chosen because it cannot be confused with an absolute file path.")

(defconst efrit-sandbox-store--shell-target-prefix "shell:"
  "A JSON target string with this prefix is a command-list shell grant.
The remainder is the command names separated by spaces; it
deserializes to (shell NAME...).  Command names cannot contain spaces.")

(defun efrit-sandbox-store--target-to-json (target)
  "Serialize grant TARGET (t, a path, (buffer . NAME), or (shell NAME...)) for JSON."
  (cond
   ((and (consp target) (eq (car target) 'buffer))
    (concat efrit-sandbox-store--buffer-target-prefix (cdr target)))
   ((efrit-sandbox-shell-target-p target)
    (concat efrit-sandbox-store--shell-target-prefix (mapconcat #'identity (cdr target) " ")))
   (t target)))

(defun efrit-sandbox-store--target-from-json (target)
  "Deserialize a JSON TARGET back to t, a path, (buffer . NAME), or (shell NAME...)."
  (cond
   ((and (stringp target)
         (string-prefix-p efrit-sandbox-store--buffer-target-prefix target))
    (cons 'buffer (substring target (length efrit-sandbox-store--buffer-target-prefix))))
   ((and (stringp target)
         (string-prefix-p efrit-sandbox-store--shell-target-prefix target))
    (cons 'shell (split-string (substring target (length efrit-sandbox-store--shell-target-prefix))
                               " " t)))
   (t target)))

(defun efrit-sandbox-store--valid-grant (g)
  "Return a grant plist for JSON object G (a hash table), or nil if invalid."
  (when (hash-table-p g)
    (let* ((cap (gethash "cap" g))
           (raw-target (gethash "target" g))
           (scope (gethash "scope" g))
           (cap-sym (and (stringp cap) (intern cap)))
           (target (efrit-sandbox-store--target-from-json raw-target)))
      (when (and (memq cap-sym '(read write elisp shell net buffer))
                 (equal scope "project")
                 (or (eq target t)
                     (and (stringp target) (file-name-absolute-p target))
                     ;; a fileless-buffer target
                     (and (eq cap-sym 'buffer) (consp target)
                          (stringp (cdr target)) (not (string-empty-p (cdr target))))
                     ;; a command-list shell target with at least one name
                     (and (eq cap-sym 'shell) (efrit-sandbox-shell-target-p target)
                          (cdr target))))
        (list :cap cap-sym :target target :scope 'project)))))

(defun efrit-sandbox-store--parse (file)
  "Read FILE; return its valid grants (possibly nil).  Never signals."
  (condition-case err
      (let* ((obj (with-temp-buffer
                    (insert-file-contents file)
                    (json-parse-buffer :object-type 'hash-table :array-type 'list)))
             (grants (and (hash-table-p obj) (gethash "grants" obj))))
        (unless (hash-table-p obj)
          (efrit-log 'warn "sandbox store %s: not an object, ignoring" file))
        (delq nil (mapcar #'efrit-sandbox-store--valid-grant
                          (if (listp grants) grants nil))))
    (error
     (efrit-log 'warn "sandbox store %s: unreadable (%s), ignoring"
                file (error-message-string err))
     nil)))

;;; Load / save

(defun efrit-sandbox-store-load (root)
  "Load ROOT's project grants from disk into the sandbox; return them."
  (let* ((file (efrit-sandbox-store-file root))
         (grants (and (file-readable-p file) (efrit-sandbox-store--parse file))))
    (puthash root grants efrit-sandbox--project-grants)
    grants))

(defun efrit-sandbox-store-save (root)
  "Write ROOT's project grants to disk (mode 0600).
Bypasses the sandbox deliberately: this is efrit persisting its own
state on the user's explicit instruction, never a tool acting."
  (let* ((file (efrit-sandbox-store-file root))
         (grants (gethash root efrit-sandbox--project-grants))
         (json (json-encode
                `((version . ,efrit-sandbox-store--version)
                  (grants . ,(vconcat
                              (mapcar (lambda (g)
                                        `((cap . ,(symbol-name (plist-get g :cap)))
                                          (target . ,(efrit-sandbox-store--target-to-json
                                                      (plist-get g :target)))
                                          (scope . "project")))
                                      grants)))))))
    (make-directory (file-name-directory file) t)
    (with-file-modes #o600
      (with-temp-file file
        (insert json "\n")))
    (efrit-log 'info "sandbox: saved %d project grant(s) to %s" (length grants) file)
    file))

;;; Auto-load per project

(defvar efrit-sandbox-store--loaded (make-hash-table :test 'equal))

(defun efrit-sandbox-store-ensure-loaded (&optional root)
  "Load ROOT's grants once per session (idempotent)."
  (let ((root (or root (efrit-sandbox-project-root))))
    (unless (gethash root efrit-sandbox-store--loaded)
      (puthash root t efrit-sandbox-store--loaded)
      (efrit-sandbox-store-load root))))

(defun efrit-sandbox-store-forget (&optional root)
  "Drop the loaded flag so the next check re-reads ROOT's file."
  (if root (remhash root efrit-sandbox-store--loaded)
    (clrhash efrit-sandbox-store--loaded)))

(provide 'efrit-sandbox-store)

;;; efrit-sandbox-store.el ends here
