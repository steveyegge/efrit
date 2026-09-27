;;; efrit-agent-mentions.el --- @file mentions, /commands, drag and drop -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; Three things the input region of the agent buffer understands
;; besides plain text:
;;
;; `@path' mentions.  Typing `@' at the start of a word completes over
;; the project's files (git ls-files, else a directory walk, cached
;; per completion).  When the input is sent, each mention that names a
;; readable file is expanded for the model: the text keeps the
;; mention, and the file's contents follow in a fenced block, cut at
;; `efrit-agent-mention-max-chars'.  Paths with spaces are written
;; `@"the path"'.  Images are not inlined as text; they become image
;; content blocks (see `efrit-agent-mentions-content-blocks').
;;
;; `/command' at the very start of the input.  These are efrit's own
;; commands, not the model's: `/new', `/model', `/mode', `/help' and so
;; on, from `efrit-agent-slash-commands'.  A slash command runs at
;; once and sends nothing.  `/' anywhere else is text.  Completion
;; offers the commands, with their descriptions, only at the input
;; start (a `/' after other text is a path or a division).
;;
;; Drag and drop.  A file dropped on the buffer from a file manager is
;; inserted as a mention; an image gets a small preview in the input.
;; Files from a temporary directory (a screenshot the OS offers as a
;; drag) are copied under `<project>/.efrit/dropped/` first, so the
;; mention still resolves after the OS deletes the original.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'dnd)
(require 'efrit-log)
(require 'efrit-tool-utils)

(declare-function efrit-agent--in-input-region-p "efrit-agent-core")
(declare-function efrit-agent--get-input "efrit-agent-core")
(declare-function efrit-agent--clear-input "efrit-agent-core")
(declare-function efrit-agent-new-conversation "efrit-agent-input")
(declare-function efrit-agent-restart "efrit-agent-input")
(declare-function efrit-agent-queue-show "efrit-agent-input")
(declare-function efrit-agent-copy-last-output "efrit-agent-input")
(declare-function efrit-agent-help "efrit-agent")
(declare-function efrit-agent-cycle-display-mode "efrit-agent")
(declare-function efrit-agent-cycle-verbosity "efrit-agent")
(declare-function efrit-agent-cancel "efrit-agent")
(declare-function efrit-agent-expand-all "efrit-agent")
(declare-function efrit-agent-collapse-all "efrit-agent")
(declare-function efrit-select-model "efrit-models")
(declare-function efrit-menu "efrit-menu")
(declare-function efrit-prompts-manage "efrit-prompts")
(declare-function efrit-permissions "efrit-permissions-ui")
(declare-function efrit-resume "efrit-agent")
(declare-function efrit-doctor "efrit-doctor")
(defvar efrit-agent--input-start)

(defgroup efrit-agent-mentions nil
  "@file mentions, /commands and drag and drop in the agent buffer."
  :group 'efrit-agent
  :prefix "efrit-agent-mention")

(defcustom efrit-agent-mention-max-chars 40000
  "Characters of one mentioned file inlined for the model; the rest is cut with a note."
  :type 'integer)

(defcustom efrit-agent-mention-max-files 2000
  "Most project files offered by `@' completion; larger projects complete by prefix only."
  :type 'integer)

(defcustom efrit-agent-mention-image-max-width 200
  "Pixel width of the preview an image mention gets in the input."
  :type 'integer)

(defconst efrit-agent-mention-regexp
  "\\(?:^\\|[^[:alnum:]_.@]\\)@\\(?:\"\\([^\"\n]+\\)\"\\|\\([^[:space:]\"\n]+\\)\\)"
  "A mention: `@path' or `@\"path with spaces\"', at a word boundary.
Group 1 is a quoted path, group 2 a bare one.  An email address does
not match: the character before the @ must not be a word character.")

;;;; Mentions

(defun efrit-agent-mention-text (path)
  "The mention for PATH: quoted when it has whitespace."
  (if (string-match-p "[[:space:]]" path)
      (format "@\"%s\"" path)
    (concat "@" path)))

(defun efrit-agent-mentions-in (text)
  "The paths mentioned in TEXT, in order, duplicates removed."
  (let ((out nil) (start 0))
    (while (string-match efrit-agent-mention-regexp text start)
      (let ((path (or (match-string 1 text) (match-string 2 text))))
        ;; Trailing punctuation of the sentence is not part of a bare path
        (setq path (string-trim-right path "[.,;:!?)]+"))
        (unless (member path out) (push path out)))
      (setq start (match-end 0)))
    (nreverse out)))

(defun efrit-agent-mention--resolve (path)
  "PATH as an absolute file name, relative to the project root when relative."
  (expand-file-name path (ignore-errors (efrit-tool--get-project-root))))

(defun efrit-agent-mention--image-p (file)
  "Non-nil when FILE is an image Emacs can show."
  (and (fboundp 'image-supported-file-p)
       (image-supported-file-p file)))

(defun efrit-agent-mention--clip (text)
  "TEXT cut to `efrit-agent-mention-max-chars', with a note when cut."
  (if (<= (length text) efrit-agent-mention-max-chars)
      text
    (concat (substring text 0 efrit-agent-mention-max-chars)
            (format "\n[... %d more characters not shown ...]"
                    (- (length text) efrit-agent-mention-max-chars)))))

(defun efrit-agent-mentions-expand (text)
  "TEXT with the contents of every mentioned, readable, non-image file appended.
Each file follows as a fenced block headed by its mention.  Mentions
of files that do not exist stay as written: the model can ask.
Images are left to `efrit-agent-mentions-content-blocks'."
  (let ((blocks nil))
    (dolist (path (efrit-agent-mentions-in text))
      (let ((file (efrit-agent-mention--resolve path)))
        (when (and (file-regular-p file) (file-readable-p file)
                   (not (efrit-agent-mention--image-p file)))
          (condition-case err
              (push (format "%s\n```%s\n%s\n```"
                            (efrit-agent-mention-text path)
                            (or (file-name-extension file) "")
                            (efrit-agent-mention--clip
                             (string-trim-right
                              (with-temp-buffer
                                (insert-file-contents file)
                                (buffer-string)))))
                    blocks)
            (error
             (efrit-log 'warn "mention: cannot read %s: %s" file (error-message-string err)))))))
    (if blocks
        (concat text "\n\nThe files mentioned above:\n\n" (string-join (nreverse blocks) "\n\n"))
      text)))

(defun efrit-agent-mentions-content-blocks (text)
  "Image content blocks for the images mentioned in TEXT, for the API.
Each is ((type . \"image\") (source . ((type . \"base64\") (media_type . M) (data . D))))."
  (let (blocks)
    (dolist (path (efrit-agent-mentions-in text))
      (let ((file (efrit-agent-mention--resolve path)))
        (when (and (file-regular-p file) (efrit-agent-mention--image-p file))
          (let ((type (pcase (downcase (or (file-name-extension file) ""))
                        ("jpg" "image/jpeg") ("jpeg" "image/jpeg") ("png" "image/png")
                        ("gif" "image/gif") ("webp" "image/webp") (_ nil))))
            (when type
              (push `((type . "image")
                      (source . ((type . "base64")
                                 (media_type . ,type)
                                 (data . ,(with-temp-buffer
                                            (set-buffer-multibyte nil)
                                            (insert-file-contents-literally file)
                                            (base64-encode-string (buffer-string) t))))))
                    blocks))))))
    (nreverse blocks)))

;;;; @ completion over project files

(defvar efrit-agent-mention--files-cache nil
  "(ROOT . FILES) of the last listing, cleared when completion ends.")

(defun efrit-agent-mention--project-files ()
  "Relative paths of the project's files, cached for this completion."
  (let ((root (ignore-errors (efrit-tool--get-project-root))))
    (when root
      (if (equal root (car efrit-agent-mention--files-cache))
          (cdr efrit-agent-mention--files-cache)
        (let* ((default-directory root)
               (git (and (file-directory-p (expand-file-name ".git" root))
                         (executable-find "git")
                         (with-temp-buffer
                           (when (zerop (call-process "git" nil t nil "ls-files" "--cached"
                                                      "--others" "--exclude-standard"))
                             (split-string (buffer-string) "\n" t)))))
               (files (or git
                          (let ((all (directory-files-recursively root "." nil
                                                                  (lambda (d) (not (string-match-p "/\\.\\|/node_modules\\'" d))))))
                            (mapcar (lambda (f) (file-relative-name f root))
                                    (seq-take all efrit-agent-mention-max-files))))))
          (setq efrit-agent-mention--files-cache (cons root files))
          files)))))

(defun efrit-agent-mention--forget-files ()
  "Drop the file listing when completion is over."
  (unless completion-in-region-mode
    (setq efrit-agent-mention--files-cache nil)))

(add-hook 'completion-in-region-mode-hook #'efrit-agent-mention--forget-files)

(defun efrit-agent-mention--at-word-start-p (pos)
  "Non-nil when POS is at the start of a word: input start, or after whitespace."
  (or (<= pos (or (and (boundp 'efrit-agent--input-start)
                       (marker-position efrit-agent--input-start))
                  (point-min)))
      (memq (char-before pos) '(?\s ?\t ?\n))))

(defun efrit-agent-mention-completion-at-point ()
  "Complete `@path' from the project's files."
  (when (efrit-agent--in-input-region-p)
    (save-excursion
      (let ((end (point)))
        (when (re-search-backward "@\\(\"[^\"\n]*\\|[^[:space:]\"\n]*\\)\\=" (line-beginning-position) t)
          (let ((at (match-beginning 0)))
            (when (efrit-agent-mention--at-word-start-p at)
              (let ((start (1+ at))
                    (quoted (eq (char-after (1+ at)) ?\")))
                (list (if quoted (1+ start) start) end
                      (completion-table-dynamic
                       (lambda (_) (efrit-agent-mention--project-files)))
                      :exclusive 'no
                      :exit-function
                      (lambda (path status)
                        (when (eq status 'finished)
                          ;; Rewrite as a quoted mention when the path needs it
                          (let ((mention (efrit-agent-mention-text path)))
                            (unless (equal mention (concat "@" path))
                              (delete-region at (point))
                              (insert mention)))
                          (insert " "))))))))))))

;;;; Slash commands

(defvar efrit-agent-slash-commands nil
  "The `/commands' of the input: (NAME DESCRIPTION FUNCTION).
FUNCTION is called with the rest of the line (a string, maybe empty).
Add with `efrit-agent-define-slash-command'.")

(defun efrit-agent-define-slash-command (name description function)
  "Define the slash command NAME (no slash) as FUNCTION with DESCRIPTION.
FUNCTION takes the argument text after the command."
  (setq efrit-agent-slash-commands
        (cons (list name description function)
              (cl-remove name efrit-agent-slash-commands :key #'car :test #'equal)))
  name)

(defun efrit-agent-slash-parse (input)
  "The (NAME . ARGS) of INPUT when it is a slash command, else nil.
Only at the very start of the input, so a `/' later in a sentence is text."
  (when (string-match "\\`/\\([[:alnum:]_-]+\\)\\(?:[[:space:]]+\\(.*\\)\\)?[[:space:]]*\\'" input)
    (cons (match-string 1 input) (or (match-string 2 input) ""))))

(defun efrit-agent-slash-run (input)
  "Run INPUT as a slash command.  Returns non-nil when it was one.
An unknown command is reported and not sent to the model either: a
typo should not cost a turn."
  (when-let* ((parsed (efrit-agent-slash-parse input)))
    (let ((entry (assoc (car parsed) efrit-agent-slash-commands)))
      (if entry
          (progn
            (efrit-agent--clear-input)
            (funcall (nth 2 entry) (cdr parsed)))
        (message "Efrit: no command /%s; /help lists them" (car parsed)))
      t)))

(defun efrit-agent-slash-completion-at-point ()
  "Complete `/command' at the start of the input."
  (when (efrit-agent--in-input-region-p)
    (let* ((input-start (and (boundp 'efrit-agent--input-start)
                             (marker-position efrit-agent--input-start)))
           (slash (and input-start
                       (save-excursion
                         (goto-char input-start)
                         (skip-chars-forward " \t\n")
                         (and (eq (char-after) ?/) (point))))))
      (when slash
        (let ((word-end (save-excursion (goto-char slash) (skip-chars-forward "/[:alnum:]_-") (point))))
          (when (and (<= (point) word-end) (> (point) slash))
            (list (1+ slash) word-end
                  (mapcar #'car efrit-agent-slash-commands)
                  :exclusive 'no
                  :annotation-function
                  (lambda (name) (concat "  " (nth 1 (assoc name efrit-agent-slash-commands))))
                  :exit-function (lambda (_ status) (when (eq status 'finished) (insert " "))))))))))

(defun efrit-agent-slash-help (&rest _)
  "List the slash commands."
  (interactive)
  (let ((width (apply #'max 4 (mapcar (lambda (c) (length (car c))) efrit-agent-slash-commands))))
    (with-help-window "*efrit slash commands*"
      (princ "Slash commands, typed at the start of the input:\n\n")
      (dolist (c (sort (copy-sequence efrit-agent-slash-commands)
                       (lambda (a b) (string< (car a) (car b)))))
        (princ (format (format "  /%%-%ds  %%s\n" width) (car c) (nth 1 c)))))))

(defun efrit-agent-slash--define-builtins ()
  "The built-in slash commands.  Called at load; safe to call again."
  (efrit-agent-define-slash-command "help" "List these commands" #'efrit-agent-slash-help)
  (efrit-agent-define-slash-command "keys" "The agent buffer's keys" (lambda (_) (efrit-agent-help)))
  (efrit-agent-define-slash-command "new" "Fresh conversation in this buffer" (lambda (_) (efrit-agent-new-conversation)))
  (efrit-agent-define-slash-command "restart" "Fresh session, same windows" (lambda (_) (efrit-agent-restart)))
  (efrit-agent-define-slash-command "resume" "Pick a saved session" (lambda (_) (efrit-resume)))
  (efrit-agent-define-slash-command "cancel" "Cancel the running turn" (lambda (_) (efrit-agent-cancel)))
  (efrit-agent-define-slash-command "queue" "Show or drop queued inputs" (lambda (_) (efrit-agent-queue-show)))
  (efrit-agent-define-slash-command "copy" "Copy the last answer" (lambda (_) (efrit-agent-copy-last-output)))
  (efrit-agent-define-slash-command "model" "Choose the model (/model NAME sets it)"
                                    (lambda (args)
                                      (if (string-empty-p args)
                                          (efrit-select-model)
                                        (setq efrit-default-model args)
                                        (message "Model: %s" args))))
  (efrit-agent-define-slash-command "mode" "Cycle the tool display mode" (lambda (_) (efrit-agent-cycle-display-mode)))
  (efrit-agent-define-slash-command "verbosity" "Cycle the verbosity" (lambda (_) (efrit-agent-cycle-verbosity)))
  (efrit-agent-define-slash-command "expand" "Expand every tool row" (lambda (_) (efrit-agent-expand-all)))
  (efrit-agent-define-slash-command "collapse" "Collapse every tool row" (lambda (_) (efrit-agent-collapse-all)))
  (efrit-agent-define-slash-command "prompts" "The prompt library" (lambda (_) (efrit-prompts-manage)))
  (efrit-agent-define-slash-command "permissions" "Sandbox grants, review, limits" (lambda (_) (efrit-permissions)))
  (efrit-agent-define-slash-command "menu" "The efrit menu" (lambda (_) (efrit-menu)))
  (efrit-agent-define-slash-command "doctor" "Check the setup" (lambda (_) (efrit-doctor))))

(efrit-agent-slash--define-builtins)

;;;; Drag and drop

(defun efrit-agent-dnd--local-file (url)
  "The local file URL names, or a user error."
  (let ((file (dnd-get-local-file-name url t)))
    (unless (and file (file-regular-p file))
      (user-error "Not a local file: %s" url))
    file))

(defun efrit-agent-dnd--keep (file)
  "FILE, or a copy under the project when FILE is in a temporary directory.
The OS deletes a dragged screenshot soon after the drop."
  (let ((root (ignore-errors (efrit-tool--get-project-root)))
        (tmp (file-name-as-directory (expand-file-name temporary-file-directory))))
    (if (and root (string-prefix-p tmp (expand-file-name file))
             (not (string-prefix-p (file-name-as-directory root) (expand-file-name file))))
        (let* ((dir (expand-file-name ".efrit/dropped/" root))
               (dest (expand-file-name
                      (format "%s-%s.%s" (format-time-string "%Y%m%d-%H%M%S")
                              (substring (md5 file) 0 6)
                              (or (file-name-extension file) "bin"))
                      dir)))
          (make-directory dir t)
          (copy-file file dest t)
          dest)
      file)))

(defun efrit-agent-dnd--insert-mention (file)
  "Insert a mention of FILE in the input; an image gets a preview."
  (let* ((root (ignore-errors (efrit-tool--get-project-root)))
         (path (if (and root (string-prefix-p (file-name-as-directory root) file))
                   (file-relative-name file root)
                 (abbreviate-file-name file)))
         (mention (efrit-agent-mention-text path)))
    (goto-char (point-max))
    (unless (or (bobp) (memq (char-before) '(?\s ?\n))) (insert " "))
    (if (and (efrit-agent-mention--image-p file) (display-images-p))
        (let ((image (ignore-errors
                       (create-image file nil nil :max-width efrit-agent-mention-image-max-width))))
          (insert (if image
                      (propertize mention 'display image 'efrit-mention-image t
                                  'help-echo mention)
                    mention)))
      (insert mention))
    (insert " ")))

(defun efrit-agent-dnd-handle (urls _action)
  "Handle files dropped on the agent buffer: each becomes a mention."
  (let ((files (mapcar #'efrit-agent-dnd--local-file (ensure-list urls))))
    (dolist (file files)
      (efrit-agent-dnd--insert-mention (efrit-agent-dnd--keep file)))
    'private))

(put 'efrit-agent-dnd-handle 'dnd-multiple-handler t)

(defun efrit-agent-dnd-enable ()
  "Route file drops on this buffer to `efrit-agent-dnd-handle'."
  (setq-local dnd-protocol-alist
              (append '(("^file:///" . efrit-agent-dnd-handle)
                        ("^file:/[^/]" . efrit-agent-dnd-handle)
                        ("^file:[^/]" . efrit-agent-dnd-handle))
                      (cl-remove 'efrit-agent-dnd-handle dnd-protocol-alist :key #'cdr))))

;;;; Wiring

(defun efrit-agent-mentions-setup ()
  "Install the completion functions and drop handling in the agent buffer."
  (add-hook 'completion-at-point-functions #'efrit-agent-slash-completion-at-point -10 t)
  (add-hook 'completion-at-point-functions #'efrit-agent-mention-completion-at-point -5 t)
  (efrit-agent-dnd-enable))

(provide 'efrit-agent-mentions)

;;; efrit-agent-mentions.el ends here
