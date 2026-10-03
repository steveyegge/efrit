;;; efrit-context-sources.el --- Proactive editor context for prompts -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.8.5
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
(require 'efrit-text-window)
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
  '(buffer position region diagnostic project visible-buffers edit-history pins)
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
  edit-history    the target buffer's recent edit bursts as diffs
                  (only while `efrit-edit-history-mode' is on there)
  pins            the project's pinned references (`efrit-context-pin')

A function element is called with one argument, the target buffer,
inside `with-current-buffer', and returns a string (one or more
lines) or nil.  Errors in any source are caught and logged; that
source is skipped.

Set to nil to send no proactive context."
  :type '(repeat (choice (const buffer) (const position) (const region)
                         (const diagnostic) (const project)
                         (const visible-buffers) (const recent-files)
                         (const edit-history) (const pins)
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

(declare-function treesit-node-at "treesit")
(declare-function treesit-node-parent "treesit")
(declare-function treesit-node-type "treesit")
(declare-function treesit-node-start "treesit")
(declare-function treesit-node-end "treesit")
(declare-function treesit-node-child-by-field-name "treesit")
(declare-function treesit-node-children "treesit")
(declare-function treesit-node-text "treesit")
(declare-function treesit-parser-list "treesit")

(defconst efrit-context--function-node-regexp
  "function\\|method\\|defun\\|procedure\\|lambda\\|arrow_function\\|func_literal"
  "Tree-sitter node types that are a function-like definition.")

(defconst efrit-context--class-node-regexp
  "class\\|struct\\|interface\\|impl_item\\|trait\\|module\\|namespace\\|enum"
  "Tree-sitter node types that are a type-like container.")

(defun efrit-context--node-header (node)
  "The header of NODE: its text up to (not including) its body field, one line."
  (let* ((body (treesit-node-child-by-field-name node "body"))
         (end (if body (treesit-node-start body) (treesit-node-end node)))
         (text (buffer-substring-no-properties (treesit-node-start node) end)))
    (string-trim (replace-regexp-in-string "[ \t\n]+" " " text))))

(defun efrit-context--enclosing (node regexp)
  "The nearest ancestor of NODE (itself included) whose type matches REGEXP."
  (let ((n node))
    (while (and n (not (string-match-p regexp (treesit-node-type n))))
      (setq n (treesit-node-parent n)))
    n))

(defun efrit-context-scope-block (&optional pos)
  "What encloses POS (default point): class and function headers and ranges, or nil.
From tree-sitter when the buffer has a parser: the enclosing type-like
node and function-like node, each as its header (up to the body) and
its line range.  Else `which-function'.  A non-blank line is resolved
from its first non-blank character, so indentation does not pick the
enclosing node (after ai-code-interface, 2026-09-28)."
  (save-excursion
    (when pos (goto-char pos))
    (cond
     ((and (fboundp 'treesit-parser-list) (ignore-errors (treesit-parser-list)))
      (back-to-indentation)
      (let* ((node (treesit-node-at (point)))
             (fn (and node (efrit-context--enclosing node efrit-context--function-node-regexp)))
             (cls (and node (efrit-context--enclosing (or (and fn (treesit-node-parent fn)) node)
                                                     efrit-context--class-node-regexp)))
             (parts nil))
        (when cls
          (push (format "Enclosing type: %s (lines %d-%d)"
                        (truncate-string-to-width (efrit-context--node-header cls) 120 nil nil "…")
                        (line-number-at-pos (treesit-node-start cls))
                        (line-number-at-pos (treesit-node-end cls)))
                parts))
        (when fn
          (push (format "Enclosing function: %s (lines %d-%d)"
                        (truncate-string-to-width (efrit-context--node-header fn) 160 nil nil "…")
                        (line-number-at-pos (treesit-node-start fn))
                        (line-number-at-pos (treesit-node-end fn)))
                parts))
        (and parts (mapconcat #'identity (nreverse parts) "\n"))))
     ((and (fboundp 'which-function) (ignore-errors (which-function)))
      (format "Enclosing definition: %s" (which-function))))))

(defun efrit-context--source-position (_buf)
  (concat
   (format "Point: line %d, column %d (char %d of %d)"
           (line-number-at-pos) (current-column) (point) (buffer-size))
   (when-let* ((scope (ignore-errors (efrit-context-scope-block))))
     (concat "\n" scope))))

(defun efrit-context--source-region (_buf)
  (when (use-region-p)
    (let* ((beg (region-beginning))
           (end (region-end))
           (len (- end beg))
           (truncated (> len efrit-context-region-max-chars))
           ;; a long region is cut on whole lines around point, not at
           ;; a character count from its start
           (text (if truncated
                     (let ((w (save-restriction
                                (narrow-to-region beg end)
                                (efrit-text-window :start (min (max (point) beg) end)
                                                   :chars efrit-context-region-max-chars))))
                       (concat (when (plist-get w :before-cut) "...\n")
                               (plist-get w :before) (plist-get w :after)
                               (when (plist-get w :after-cut) "\n...")))
                   (buffer-substring-no-properties beg end))))
      (format "Active region: line %d col %d to line %d col %d (%d chars)%s\n<<<REGION\n%s\n>>>"
              (line-number-at-pos beg) (save-excursion (goto-char beg) (current-column))
              (line-number-at-pos end) (save-excursion (goto-char end) (current-column))
              len
              (if truncated " [truncated around point]" "")
              text))))

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
    (recent-files . efrit-context--source-recent-files)
    (edit-history . efrit-context--source-edit-history)
    (pins . efrit-context--source-pins)))

(declare-function efrit-edit-history-text "efrit-edit-history")
(defvar efrit-edit-history-mode)

(defun efrit-context--source-edit-history (buf)
  (when (and (featurep 'efrit-edit-history)
             (buffer-local-value 'efrit-edit-history-mode buf))
    (efrit-edit-history-text buf)))

;;; Dismissing the file context, and the live indicator
;;
;; The snapshot is computed at send time, so until now the user could
;; not see what would go along.  `efrit-context-describe' is the
;; short label the agent buffer's header shows ("⧉ foo.el:120, 3
;; lines").  `efrit-context-dismiss' drops the file-bound sources
;; (buffer, position, region, diagnostic, edit-history) for the
;; current target: they come back when the user moves to another file
;; or selects a region (after claude-code-ide's dismissed-file
;; semantics, 2026-09-28).

(defvar efrit-context--dismissed nil
  "The buffer whose file context the user dismissed, or nil.")

(defconst efrit-context--file-bound-sources '(buffer position region diagnostic edit-history)
  "Sources that describe the target buffer; the ones a dismiss removes.")

(defun efrit-context-dismissed-p (&optional target)
  "Non-nil while the file context of TARGET (default the target buffer) is dismissed.
A selected region undoes the dismissal: the user pointed at something."
  (let ((buf (or target (efrit-context-target-buffer))))
    (and efrit-context--dismissed
         (eq efrit-context--dismissed buf)
         (not (with-current-buffer buf (use-region-p))))))

;;;###autoload
(defun efrit-context-dismiss ()
  "Stop sending the current file's context with the next turns.
Moving to another file or selecting a region turns it back on;
`efrit-context-restore' does so at once."
  (interactive)
  (let ((buf (efrit-context-target-buffer)))
    (setq efrit-context--dismissed buf)
    (message "efrit: context of %s dismissed until you move to another file or select something"
             (buffer-name buf))
    (force-mode-line-update t)))

(defun efrit-context-restore ()
  "Send the current file's context again."
  (interactive)
  (setq efrit-context--dismissed nil)
  (force-mode-line-update t))

(defun efrit-context-active-sources (&optional target)
  "`efrit-context-sources' minus the file-bound ones while dismissed."
  (if (efrit-context-dismissed-p target)
      (cl-remove-if (lambda (s) (memq s efrit-context--file-bound-sources)) efrit-context-sources)
    efrit-context-sources))

(defvar efrit-context--last-label nil
  "The context label last shown, to redraw agent headers only when it changes.")

(defun efrit-context--refresh-headers ()
  "After each command: when the context label changed, redraw the agent headers.
A header-line is re-evaluated only when its own window redisplays;
moving point in the file next to it does not do that (tour
2026-09-29: the label kept its first line number)."
  (when (and (fboundp 'efrit-agent-buffers)
             (not (derived-mode-p 'efrit-agent-mode)))
    (let ((label (ignore-errors (efrit-context-describe))))
      (unless (equal label efrit-context--last-label)
        (setq efrit-context--last-label label)
        (force-mode-line-update t)))))

(add-hook 'post-command-hook #'efrit-context--refresh-headers)
(declare-function efrit-agent-buffers "efrit-agent-core")

(defun efrit-context-describe (&optional target)
  "A short label of what the next turn's context will carry, or nil.
For example \"⧉ foo.el:120\", \"⧉ foo.el:120, 3 lines\", or
\"⧉ foo.el (dismissed)\"."
  (when-let* ((buf (or target (efrit-context-target-buffer))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let* ((name (if buffer-file-name (file-name-nondirectory buffer-file-name) (buffer-name)))
               (pins (length (efrit-context-pins)))
               (pin-note (if (> pins 0) (format " +%d pin%s" pins (if (= pins 1) "" "s")) ""))
               (base (cond
                      ((efrit-context-dismissed-p buf) (format "⧉ %s (dismissed)" name))
                      ((not (cl-intersection efrit-context--file-bound-sources efrit-context-sources)) nil)
                      ((use-region-p)
                       (format "⧉ %s:%d, %d lines" name (line-number-at-pos (region-beginning))
                               (count-lines (region-beginning) (region-end))))
                      (t (format "⧉ %s:%d" name (line-number-at-pos))))))
          (cond (base (concat base pin-note))
                ((> pins 0) (concat "⧉" pin-note))))))))

;;; Pinned context: references sent with every turn until cleared
;;
;; `@mentions' are one-off and the automatic sources follow the
;; cursor.  A pin is a `path#L10-L20' (or a whole file) the user wants
;; the model to keep seeing: kept per project root, listed in the
;; header, sent as the `pins' source (after ai-code-interface's
;; curated context list, 2026-09-28).

(defvar efrit-context--pins (make-hash-table :test 'equal)
  "Project root -> list of pins, newest last.  A pin is a mention string.")

(defcustom efrit-context-pin-max-chars 6000
  "Longest text one pin contributes; more is cut with a note."
  :type 'integer
  :group 'efrit-context)

(defun efrit-context--pin-root ()
  (file-name-as-directory
   (or (ignore-errors (funcall 'efrit-tool--get-project-root)) default-directory)))

(defun efrit-context-pins (&optional root)
  "The pins of ROOT (default the current project)."
  (gethash (or root (efrit-context--pin-root)) efrit-context--pins))

;;;###autoload
(defun efrit-context-pin (start end)
  "Pin the region's lines of this file (or the whole file without a region).
Sent with every turn of this project until `efrit-context-unpin' or
`efrit-context-clear-pins'."
  (interactive (if (use-region-p) (list (region-beginning) (region-end)) (list nil nil)))
  (unless buffer-file-name (user-error "This buffer visits no file"))
  (let* ((root (efrit-context--pin-root))
         (path (if (string-prefix-p root buffer-file-name)
                   (file-relative-name buffer-file-name root)
                 buffer-file-name))
         (pin (if start
                  (format "%s#L%d-L%d" path (line-number-at-pos start)
                          (line-number-at-pos (max start (1- end))))
                path))
         (pins (efrit-context-pins root)))
    (deactivate-mark)
    (unless (member pin pins)
      (puthash root (append pins (list pin)) efrit-context--pins))
    (message "Pinned %s (%d pin%s for %s)" pin (length (efrit-context-pins root))
             (if (= 1 (length (efrit-context-pins root))) "" "s") (abbreviate-file-name root))
    (force-mode-line-update t)))

(defun efrit-context-unpin (pin)
  "Remove PIN from this project's pins."
  (interactive (list (completing-read "Unpin: " (efrit-context-pins) nil t)))
  (let ((root (efrit-context--pin-root)))
    (puthash root (delete pin (efrit-context-pins root)) efrit-context--pins)
    (force-mode-line-update t)))

(defun efrit-context-clear-pins ()
  "Forget this project's pins."
  (interactive)
  (remhash (efrit-context--pin-root) efrit-context--pins)
  (message "Pins cleared")
  (force-mode-line-update t))

(declare-function efrit-agent-mention-split "efrit-agent-mentions")
(declare-function efrit-agent-mention-range-text "efrit-agent-mentions")
(declare-function efrit-fence-for "efrit-ui-helpers")

(defun efrit-context--source-pins (_buf)
  (when-let* ((pins (efrit-context-pins)))
    (require 'efrit-agent-mentions)
    (require 'efrit-ui-helpers)
    (let ((root (efrit-context--pin-root)))
      (concat
       "Pinned by the user, keep these in mind for every answer:\n"
       (mapconcat
        (lambda (pin)
          (pcase-let* ((`(,path ,start ,end) (efrit-agent-mention-split pin))
                       (file (expand-file-name path root)))
            (if (not (file-readable-p file))
                (format "- %s (not readable)" pin)
              (let* ((r (if start (efrit-agent-mention-range-text file start end)
                          (cons "whole file"
                                (with-temp-buffer (insert-file-contents file)
                                                  (string-trim-right (buffer-string))))))
                     (text (if (> (length (cdr r)) efrit-context-pin-max-chars)
                               (concat (substring (cdr r) 0 efrit-context-pin-max-chars) "\n[… cut]")
                             (cdr r))))
                (format "- %s (%s)\n%s\n%s\n%s" pin (car r)
                        (efrit-fence-for text) text (efrit-fence-for text))))))
        pins "\n")))))

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
            (dolist (src (efrit-context-active-sources buf))
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
    (cond
     ((null ctx) input)
     ;; Content blocks (an input with images): the context goes into
     ;; the text block, which is the last one
     ((vectorp input)
      (let* ((blocks (append input nil))
             (last (car (last blocks))))
        (if (equal (alist-get 'type last) "text")
            (vconcat (butlast blocks)
                     (list `((type . "text")
                             (text . ,(concat ctx "\n\n" (alist-get 'text last))))))
          (vconcat input (list `((type . "text") (text . ,ctx)))))))
     (t (concat ctx "\n\n" input)))))

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
