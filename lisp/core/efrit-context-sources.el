;;; efrit-context-sources.el --- Proactive editor context for prompts -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; Without this module the model starts every turn blind: it has to
;; spend tool calls discovering which buffer the user is in, whether a
;; region is active, and what the project is.  This module snapshots
;; that state when the user submits input and renders a short
;; EDITOR CONTEXT block that the REPL prepends to the user's message.
;;
;; This is context gathering, which the Pure Executor principle
;; explicitly allows: nothing here interprets the user's request or
;; decides anything; it reports facts about the editor.
;;
;; The snapshot is taken from the *target* buffer -- the buffer the
;; user was working in, not the agent buffer they typed into.  See
;; `efrit-context-target-buffer'.
;;
;; `efrit-context-sources' is a list of symbols and/or functions, in
;; the spirit of agent-shell's `agent-shell-context-sources': each
;; contributes zero or more "Key: value" lines.  Users add their own
;; sources by pushing a function that takes the target buffer and
;; returns a string or nil.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'project)

(defvar efrit-sandbox-target-buffer-function)
(defvar efrit-sandbox-agent-buffer-p-function)

(declare-function flymake-diagnostics "flymake")
(declare-function flymake-diagnostic-text "flymake")
(declare-function flymake-diagnostic-type "flymake")
(declare-function flycheck-overlay-errors-at "flycheck")
(declare-function flycheck-error-message "flycheck")
(declare-function flycheck-error-level "flycheck")
(declare-function which-function "which-func")
(declare-function vc-git--symbolic-ref "vc-git")
(declare-function efrit-tool--get-project-root "efrit-tool-utils")

(defgroup efrit-context nil
  "Proactive editor context sent with each turn."
  :group 'efrit
  :prefix "efrit-context-")

(defcustom efrit-context-sources
  '(buffer position region diagnostic project visible-buffers)
  "Sources of editor context prepended to each REPL turn.

Each element is either a symbol naming a built-in source or a
function.  Built-ins:

  buffer          target buffer name, file, major mode, modified/narrowed/read-only
  position        line:column, point, buffer size, enclosing defun (which-function)
  region          active region as line/col range plus its text (truncated)
  diagnostic      flymake/flycheck diagnostics on the current line
  project         project root (and remote host if Tramp), git branch
  visible-buffers other buffers shown in the frame's windows
  recent-files    a few entries from `recentf-list'

A function element is called with one argument, the target buffer,
inside `with-current-buffer', and returns a string (one or more
lines) or nil.  Errors in any source are caught and logged; that
source is skipped.

Set to nil to send no proactive context."
  :type '(repeat (choice (const buffer) (const position) (const region)
                         (const diagnostic) (const project)
                         (const visible-buffers) (const recent-files)
                         function))
  :group 'efrit-context)

(defcustom efrit-context-region-max-chars 4000
  "Longest active region included verbatim; longer ones are truncated."
  :type 'integer
  :group 'efrit-context)

(defcustom efrit-context-max-visible-buffers 8
  "Cap on entries in the visible-buffers source."
  :type 'integer
  :group 'efrit-context)

;;; Target buffer

(defvar efrit-context--agent-buffer-p-function
  (lambda (buf) (with-current-buffer buf (derived-mode-p 'efrit-agent-mode)))
  "Predicate: is BUF one of efrit's own UI buffers?  Overridable for tests.")

(defun efrit-context-target-buffer (&optional from-buffer)
  "Return the buffer the user is working in.
If FROM-BUFFER (default the current buffer) is not an efrit UI buffer,
it is the target.  Otherwise pick the most recently selected window
in the frame that shows a non-efrit buffer, falling back to the most
recent buffer in `buffer-list' that is not efrit's and not hidden."
  (let ((from (or from-buffer (current-buffer))))
    (if (not (funcall efrit-context--agent-buffer-p-function from))
        from
      (or
       ;; Most recently used other window in this frame
       (let ((win (get-mru-window nil nil t)))
         (and win
              (not (funcall efrit-context--agent-buffer-p-function
                            (window-buffer win)))
              (window-buffer win)))
       ;; Any other visible window
       (cl-loop for win in (window-list nil 'no-minibuf)
                for buf = (window-buffer win)
                unless (funcall efrit-context--agent-buffer-p-function buf)
                return buf)
       ;; Most recent non-hidden buffer
       (cl-loop for buf in (buffer-list)
                unless (or (funcall efrit-context--agent-buffer-p-function buf)
                           (string-prefix-p " " (buffer-name buf)))
                return buf)))))

;;; Built-in sources

(defun efrit-context--source-buffer (_buf)
  (let ((file (buffer-file-name)))
    (concat
     (format "Buffer: %s" (buffer-name))
     (when file (format "\nFile: %s" (abbreviate-file-name file)))
     (format "\nMode: %s" major-mode)
     (let ((flags (delq nil (list (and (buffer-modified-p) "modified")
                                  (and buffer-read-only "read-only")
                                  (and (buffer-narrowed-p) "narrowed")))))
       (when flags (format "\nState: %s" (string-join flags ", "))))
     (unless file
       (format "\nDirectory: %s" (abbreviate-file-name default-directory))))))

(defun efrit-context--source-position (_buf)
  (let ((defun-name (and (fboundp 'which-function)
                         (ignore-errors (which-function)))))
    (concat
     (format "Point: line %d, column %d (char %d of %d)"
             (line-number-at-pos) (current-column) (point) (buffer-size))
     (when defun-name (format "\nEnclosing definition: %s" defun-name)))))

(defun efrit-context--source-region (_buf)
  (when (use-region-p)
    (let* ((beg (region-beginning))
           (end (region-end))
           (text (buffer-substring-no-properties beg end))
           (truncated (> (length text) efrit-context-region-max-chars)))
      (format "Active region: line %d col %d to line %d col %d (%d chars)%s\n<<<REGION\n%s%s\n>>>"
              (line-number-at-pos beg) (save-excursion (goto-char beg) (current-column))
              (line-number-at-pos end) (save-excursion (goto-char end) (current-column))
              (length text)
              (if truncated " [truncated]" "")
              (if truncated (substring text 0 efrit-context-region-max-chars) text)
              (if truncated "\n..." "")))))

(defun efrit-context--source-diagnostic (_buf)
  (let ((items nil))
    (when (and (bound-and-true-p flymake-mode) (fboundp 'flymake-diagnostics))
      (dolist (d (flymake-diagnostics (line-beginning-position) (line-end-position)))
        (push (format "%s: %s" (flymake-diagnostic-type d)
                      (flymake-diagnostic-text d))
              items)))
    (when (and (bound-and-true-p flycheck-mode) (fboundp 'flycheck-overlay-errors-at))
      (dolist (e (flycheck-overlay-errors-at (point)))
        (push (format "%s: %s" (flycheck-error-level e) (flycheck-error-message e))
              items)))
    (when items
      (concat "Diagnostics on current line:\n  "
              (string-join (nreverse items) "\n  ")))))

(defun efrit-context--git-branch (dir)
  "Current git branch of DIR, or nil.  Cheap: reads .git/HEAD, no process."
  (when-let* ((git (locate-dominating-file dir ".git"))
              (head (expand-file-name ".git/HEAD" git)))
    (when (file-readable-p head)
      (with-temp-buffer
        (insert-file-contents head)
        (goto-char (point-min))
        (if (looking-at "ref: refs/heads/\\(.*\\)$")
            (match-string 1)
          (buffer-substring (point-min) (min (point-max) 12)))))))

(defun efrit-context--source-project (_buf)
  (let* ((root (condition-case nil
                   (if (fboundp 'efrit-tool--get-project-root)
                       (efrit-tool--get-project-root)
                     (when-let* ((p (project-current))) (project-root p)))
                 (error nil)))
         (remote (and root (file-remote-p root)))
         (branch (and root (ignore-errors (efrit-context--git-branch root)))))
    (when root
      (concat (format "Project root: %s" (abbreviate-file-name root))
              (when remote (format "\nRemote host: %s (Tramp)" remote))
              (when branch (format "\nGit branch: %s" branch))))))

(defun efrit-context--source-visible-buffers (buf)
  (let ((others (cl-loop for win in (window-list nil 'no-minibuf)
                         for b = (window-buffer win)
                         unless (or (eq b buf)
                                    (funcall efrit-context--agent-buffer-p-function b))
                         collect (with-current-buffer b
                                   (if (buffer-file-name)
                                       (abbreviate-file-name (buffer-file-name))
                                     (buffer-name))))))
    (when others
      (format "Other visible buffers: %s"
              (string-join (seq-take (delete-dups others)
                                     efrit-context-max-visible-buffers)
                           ", ")))))

(defun efrit-context--source-recent-files (_buf)
  (when (bound-and-true-p recentf-list)
    (format "Recent files: %s"
            (string-join (mapcar #'abbreviate-file-name
                                 (seq-take recentf-list 5))
                         ", "))))

(defconst efrit-context--builtin-sources
  '((buffer . efrit-context--source-buffer)
    (position . efrit-context--source-position)
    (region . efrit-context--source-region)
    (diagnostic . efrit-context--source-diagnostic)
    (project . efrit-context--source-project)
    (visible-buffers . efrit-context--source-visible-buffers)
    (recent-files . efrit-context--source-recent-files)))

;;; Snapshot and rendering

(defun efrit-context-snapshot (&optional target)
  "Return the EDITOR CONTEXT block for TARGET buffer as a string, or nil.
TARGET defaults to `efrit-context-target-buffer'.  Runs every entry
of `efrit-context-sources' inside TARGET; a source that signals is
logged and skipped.  Returns nil when there are no sources or none
produced output."
  (when efrit-context-sources
    (let ((buf (or target (efrit-context-target-buffer))))
      (when (buffer-live-p buf)
        (let ((parts nil))
          (with-current-buffer buf
            (dolist (src efrit-context-sources)
              ;; Builtin names first: `position' is also a function
              ;; (the cl alias of `cl-position'), so functionp alone
              ;; would call it with one argument and fail.
              (let ((fn (or (alist-get src efrit-context--builtin-sources)
                            (and (functionp src) src))))
                (when fn
                  (condition-case err
                      (let ((text (save-excursion
                                    (save-restriction
                                      (funcall fn buf)))))
                        (when (and (stringp text) (not (string-empty-p text)))
                          (push text parts)))
                    (error
                     (when (fboundp 'efrit-log)
                       (efrit-log 'warn "efrit-context source %S failed: %s"
                                  src (error-message-string err)))))))))
          (when parts
            (concat "<editor-context>\n"
                    (string-join (nreverse parts) "\n")
                    "\n</editor-context>")))))))

(defun efrit-context-wrap-user-input (input &optional target)
  "Return INPUT with the editor-context block prepended, if any.
This is what the REPL sends to the API as the user message; the
plain INPUT is what is shown in the conversation."
  (let ((ctx (efrit-context-snapshot target)))
    (if ctx
        (concat ctx "\n\n" input)
      input)))

;;; Sandbox integration
;;
;; The sandbox exempts the user's target buffer and efrit's own UI
;; buffers from the `buffer' capability check.  It cannot depend on
;; this file (that would be a cycle), so it reads two injected
;; functions; set them here, where both are defined.

(with-eval-after-load 'efrit-sandbox
  (setq efrit-sandbox-target-buffer-function #'efrit-context-target-buffer
        efrit-sandbox-agent-buffer-p-function
        (lambda (buf) (funcall efrit-context--agent-buffer-p-function buf))))

(provide 'efrit-context-sources)

;;; efrit-context-sources.el ends here
