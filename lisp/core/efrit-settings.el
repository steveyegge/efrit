;;; efrit-settings.el --- Per-project settings file and project registry -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; One file per project, <project>/.efrit/settings.json, holds the
;; user's per-project choices: loop limits, review policy, default
;; sandbox grants.  Each subsystem owns one top-level section and
;; validates its own values; this file only reads, writes and caches
;; the JSON.  Sections it does not know are kept across a save, so an
;; older efrit never drops what a newer one wrote.
;;
;;   {"version": 1,
;;    "limits":  {"max-iterations": 200},
;;    "review":  {"enabled": false, "classes": ["write", "exec"]},
;;    "sandbox": {"default-grants": ["read", "write"]}}
;;
;; Like the sandbox store: data, not code, validated on load, written
;; 0600, and never model-writable (`.efrit/' is in
;; `efrit-sandbox-always-deny').
;;
;; The registry, <efrit-data-directory>/projects.json, lists every
;; project root that ever got a grant or a setting, so the permissions
;; editor can show all projects rather than only the current one.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'efrit-log)
(require 'efrit-config)       ; efrit-data-directory
(require 'efrit-tool-utils)   ; efrit-tool--get-project-root

(defconst efrit-settings-file-name "settings.json")
(defconst efrit-settings-dir ".efrit")
(defconst efrit-settings-version 1)
(defconst efrit-settings-registry-file-name "projects.json")

(defun efrit-settings-project-root ()
  "The project root settings are keyed on: expanded, with a trailing slash."
  (file-name-as-directory (expand-file-name (efrit-tool--get-project-root))))

(defun efrit-settings-file (root)
  "Path of ROOT's settings file."
  (expand-file-name efrit-settings-file-name (expand-file-name efrit-settings-dir root)))

;;; Cache: root -> hash table of section name -> parsed JSON value

(defvar efrit-settings--cache (make-hash-table :test 'equal)
  "Project root -> hash table (string section -> value), as read from disk.")

(defun efrit-settings--read (file)
  "Parse FILE into a hash table of sections, or an empty one.  Never signals."
  (or (and (file-readable-p file)
           (condition-case err
               (let ((obj (with-temp-buffer
                            (insert-file-contents file)
                            (json-parse-buffer :object-type 'hash-table :array-type 'list))))
                 (if (hash-table-p obj)
                     ;; the version is file metadata, not a section
                     (progn (remhash "version" obj) obj)
                   (efrit-log 'warn "settings %s: not an object, ignoring" file)
                   nil))
             (error
              (efrit-log 'warn "settings %s: unreadable (%s), ignoring" file (error-message-string err))
              nil)))
      (make-hash-table :test 'equal)))

(defun efrit-settings-load (root)
  "Read ROOT's settings file into the cache; return the sections table."
  (let ((root (file-name-as-directory root)))
    (puthash root (efrit-settings--read (efrit-settings-file root)) efrit-settings--cache)))

(defun efrit-settings--sections (root)
  "The cached sections table for ROOT, loading it on first use."
  (let ((root (file-name-as-directory root)))
    (or (gethash root efrit-settings--cache)
        (efrit-settings-load root))))

(defun efrit-settings-forget (&optional root)
  "Drop the cache for ROOT (all projects when nil) so the next read hits disk."
  (if root (remhash (file-name-as-directory root) efrit-settings--cache)
    (clrhash efrit-settings--cache)))

;;; Sections

(defun efrit-settings-get (root section)
  "The parsed JSON value of SECTION (a string) in ROOT's settings, or nil.
Objects are hash tables with string keys, arrays are lists."
  (gethash section (efrit-settings--sections root)))

(defun efrit-settings-put (root section value)
  "Set SECTION of ROOT's settings to VALUE and write the file.
VALUE is anything `json-encode' accepts; nil removes the section.
A file with no sections left is deleted.  ROOT is added to the
project registry."
  (let* ((root (file-name-as-directory root))
         (sections (efrit-settings--sections root)))
    (if value
        (puthash section value sections)
      (remhash section sections))
    (efrit-settings--write root sections)
    (efrit-settings-register-project root)
    value))

(defun efrit-settings--serializable (value)
  "VALUE with lists turned into vectors, recursively, for `json-serialize'.
The parser gives arrays back as lists; `json-serialize' wants vectors
and rejects lists, but it does write `:false' and `:null' the way the
parser reads them (json.el's `json-encode' would write \"false\")."
  (cond
   ((hash-table-p value)
    (let ((out (make-hash-table :test 'equal)))
      (maphash (lambda (k v) (puthash k (efrit-settings--serializable v) out)) value)
      out))
   ((vectorp value) (vconcat (mapcar #'efrit-settings--serializable value)))
   ((and (listp value) value) (vconcat (mapcar #'efrit-settings--serializable value)))
   ((null value) [])
   (t value)))

(defun efrit-settings-write-json (file obj)
  "Write OBJ as JSON to FILE (mode 0600), UTF-8, no prompt.
`json-serialize' returns a unibyte string of UTF-8 bytes; inserted
into an ordinary (multibyte) buffer those bytes become raw-byte
characters that no coding system can encode, and `with-temp-file'
stops to ask which one to use.  The first verdict with an em dash in
it hit that prompt.  So the buffer is unibyte and written as is."
  (make-directory (file-name-directory file) t)
  (with-file-modes #o600
    (with-temp-file file
      (set-buffer-multibyte nil)
      (setq buffer-file-coding-system 'no-conversion)
      (insert (json-serialize obj) "\n")))
  file)

(defun efrit-settings--write (root sections)
  "Write SECTIONS for ROOT (mode 0600); delete the file when empty.
Bypasses the sandbox deliberately: this is efrit persisting the
user's own choice, never a tool acting."
  (let ((file (efrit-settings-file root)))
    (if (zerop (hash-table-count sections))
        (when (file-exists-p file) (delete-file file))
      (let ((obj (make-hash-table :test 'equal)))
        (puthash "version" efrit-settings-version obj)
        (maphash (lambda (k v) (puthash k (efrit-settings--serializable v) obj)) sections)
        (efrit-settings-write-json file obj)))
    (efrit-log 'info "settings: saved %s" file)
    file))

;;; Helpers for section owners

(defun efrit-settings-symbol-list (value allowed)
  "VALUE (a JSON array of strings) as a list of symbols, each in ALLOWED.
Nil when VALUE is not a list or holds anything else, so a hand-edited
file with a typo falls back to the default instead of half-applying."
  (when (and (listp value)
             (cl-every (lambda (s) (and (stringp s) (memq (intern s) allowed))) value))
    (mapcar #'intern value)))

(defun efrit-settings-json-bool (value)
  "VALUE (JSON true/false as parsed) as t or nil; `unset' when it is neither."
  (cond ((eq value t) t)
        ((memq value '(:false nil)) nil)
        (t 'unset)))

;;; Project registry

(defun efrit-settings-registry-file ()
  "Path of the project registry."
  (expand-file-name efrit-settings-registry-file-name efrit-data-directory))

(defun efrit-settings-known-projects ()
  "Project roots recorded in the registry, most recent first.
Roots whose directory no longer exists are dropped from the result
but kept in the file until the next write."
  (let ((file (efrit-settings-registry-file)))
    (cl-remove-if-not
     (lambda (r) (and (stringp r) (file-directory-p r)))
     (and (file-readable-p file)
          (condition-case err
              (let ((obj (with-temp-buffer
                           (insert-file-contents file)
                           (json-parse-buffer :object-type 'hash-table :array-type 'list))))
                (and (hash-table-p obj) (gethash "projects" obj)))
            (error
             (efrit-log 'warn "settings registry %s: unreadable (%s)" file (error-message-string err))
             nil))))))

(defun efrit-settings-register-project (root)
  "Record ROOT in the project registry (moved to the front if present)."
  (let* ((root (file-name-as-directory (expand-file-name root)))
         (file (efrit-settings-registry-file))
         (known (cons root (delete root (efrit-settings-known-projects)))))
    (make-directory (file-name-directory file) t)
    (with-file-modes #o600
      (with-temp-file file
        (insert (json-encode `((version . ,efrit-settings-version)
                               (projects . ,(vconcat known))))
                "\n")))
    known))

(provide 'efrit-settings)

;;; efrit-settings.el ends here
