;;; efrit-tool-show-diff-preview.el --- Show diff preview for proposed changes -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Keywords: ai, tools, diff
;; Version: 0.5.3

;;; Commentary:
;;
;; Tool for showing proposed changes to the user before applying them.
;;
;; This enables a human-in-the-loop workflow where Claude can propose
;; changes and the user reviews them in a diff view before approval.
;;
;; Features:
;; - Unified diff display with diff-mode highlighting
;; - Support for new files, deletions, and modifications
;; - All-or-nothing or selective approval modes
;; - User can edit proposed changes before applying

;;; Code:

(require 'efrit-tool-utils)
(require 'diff-mode)
(require 'cl-lib)
(require 'efrit-events)

;;; Customization

(defcustom efrit-diff-preview-buffer-name "*efrit-diff-preview*"
  "Name of the buffer for diff preview."
  :type 'string
  :group 'efrit-tool-utils)

(defcustom efrit-diff-preview-timeout-seconds nil
  "Seconds the diff preview waits for a decision before it counts as rejected.
nil (the default) waits as long as the user takes: reviewing a diff
for ten minutes is not a rejection (2026-09-27)."
  :type '(choice (const :tag "Wait for the user" nil) integer)
  :group 'efrit-tool-utils)

;;; Internal variables

(defvar efrit-diff-preview--changes nil
  "Current list of proposed changes being reviewed.")

(defvar efrit-diff-preview--result nil
  "Result of the diff preview interaction.")

(defvar efrit-diff-preview--waiting nil
  "Non-nil when waiting for user response.")

(defvar efrit-diff-preview--selected-indices nil
  "Indices of changes selected for application in selective mode.")

(defvar efrit-diff-preview--edited nil
  "Indices of changes the user edited in ediff this preview.")

(defvar efrit-diff-preview--apply-mode nil
  "Current apply mode: 'all_or_nothing or 'selective.")

;;; Diff Generation

(defun efrit-diff-preview--generate-unified-diff (old-content new-content filename)
  "Generate a unified diff between OLD-CONTENT and NEW-CONTENT for FILENAME.
Returns the diff as a string."
  (let ((old-temp (make-temp-file "efrit-old-"))
        (new-temp (make-temp-file "efrit-new-")))
    (unwind-protect
        (progn
          (with-temp-file old-temp
            (insert (or old-content "")))
          (with-temp-file new-temp
            (insert (or new-content "")))
          ;; Use diff with unified format
          (with-temp-buffer
            (let ((exit-code (call-process "diff" nil t nil
                                           "-u"
                                           "--label" (format "a/%s" filename)
                                           "--label" (format "b/%s" filename)
                                           old-temp new-temp)))
              ;; diff returns 0 for identical, 1 for differences, 2 for error
              (if (>= exit-code 2)
                  (format "--- a/%s\n+++ b/%s\n@@ Error generating diff @@\n" filename filename)
                (buffer-string)))))
      ;; Clean up temp files
      (ignore-errors (delete-file old-temp))
      (ignore-errors (delete-file new-temp)))))

(defun efrit-diff-preview--display-name (file)
  "FILE relative to the preview's root when it is inside it.
The a/ b/ diff headers read better with `notes.txt' than with the
absolute path a tool passed."
  (let ((root (efrit-diff-preview--root)))
    (if (and (stringp file) (file-name-absolute-p file) root
             (string-prefix-p (file-name-as-directory root) file))
        (file-relative-name file root)
      file)))

(defun efrit-diff-preview--format-change (change index)
  "Format a single CHANGE for display at INDEX."
  (let* ((file (efrit-diff-preview--display-name (alist-get 'file change)))
         (old-content (alist-get 'old_content change))
         (new-content (alist-get 'new_content change))
         (change-type (cond
                       ((null old-content) "NEW FILE")
                       ((null new-content) "DELETED")
                       (t "MODIFIED"))))
    (concat
     (format "\n%s=== Change %d: %s (%s) %s\n"
             (make-string 60 ?=)
             (1+ index)
             file
             change-type
             (make-string (max 0 (- 60 (+ 15 (length file) (length change-type)))) ?=))
     (cond
      ;; New file - show all as additions
      ((null old-content)
       (concat "--- /dev/null\n"
               (format "+++ b/%s\n" file)
               "@@ -0,0 +1,"
               (format "%d" (1+ (cl-count ?\n new-content)))
               " @@\n"
               (mapconcat (lambda (line) (concat "+" line))
                          (split-string new-content "\n")
                          "\n")
               "\n"))
      ;; Deleted file - show all as deletions
      ((null new-content)
       (concat (format "--- a/%s\n" file)
               "+++ /dev/null\n"
               "@@ -1,"
               (format "%d" (1+ (cl-count ?\n old-content)))
               " +0,0 @@\n"
               (mapconcat (lambda (line) (concat "-" line))
                          (split-string old-content "\n")
                          "\n")
               "\n"))
      ;; Modified - generate proper diff
      (t
       (efrit-diff-preview--generate-unified-diff old-content new-content file))))))

;;; UI Mode

(defvar efrit-diff-preview-mode-map
  (let ((map (make-sparse-keymap)))
    ;; a stray key must not type into the preview (tour 2026-09-29:
    ;; `dd' landed in the buffer)
    (suppress-keymap map)
    (define-key map (kbd "a") #'efrit-diff-preview-approve)
    (define-key map (kbd "y") #'efrit-diff-preview-approve)
    (define-key map (kbd "r") #'efrit-diff-preview-reject)
    (define-key map (kbd "n") #'efrit-diff-preview-reject)
    (define-key map (kbd "q") #'efrit-diff-preview-reject)
    (define-key map (kbd "SPC") #'efrit-diff-preview-toggle-change)
    (define-key map (kbd "RET") #'efrit-diff-preview-toggle-or-open)
    (define-key map (kbd "s") #'efrit-diff-preview-apply-selected)
    (define-key map (kbd "o") #'efrit-diff-preview-open-file)
    (define-key map (kbd "C-c C-o") #'efrit-diff-preview-open-file)
    (define-key map (kbd "E") #'efrit-diff-preview-ediff)
    (define-key map (kbd "e") #'efrit-diff-preview-ediff)
    map)
  "Keymap for `efrit-diff-preview-mode'.")

(define-derived-mode efrit-diff-preview-mode diff-mode "Efrit-Diff"
  "Major mode for reviewing proposed changes from Efrit.

\\{efrit-diff-preview-mode-map}"
  (setq buffer-read-only t)
  (setq-local header-line-format
              '(:eval (efrit-diff-preview--header-line))))

(defun efrit-diff-preview--header-line ()
  "Generate header line for diff preview buffer."
  (let ((mode-str (if (eq efrit-diff-preview--apply-mode 'selective)
                      (format "SELECTIVE [%d/%d selected]"
                              (length efrit-diff-preview--selected-indices)
                              (length efrit-diff-preview--changes))
                    "ALL-OR-NOTHING")))
    (concat " Efrit Diff Preview | " mode-str
            " | [a]pprove [r]eject [o]pen file at change"
            (when (eq efrit-diff-preview--apply-mode 'selective)
              " [SPC]toggle [s]apply-selected"))))

;;; Opening the file at the change
;;
;; The hunk's line numbers are relative to the old_content the model
;; sent, which is often a fragment of the file, so they are not
;; trusted.  The hunk's old-side text (context plus removed lines) is
;; searched for in the file instead, then its new-side text in case
;; the change was applied already.  Lifted from agent-shell's
;; agent-shell-diff.el (xenodium), issue 347 there.

(defun efrit-diff-preview--hunk-header-at-point ()
  "Position of the `@@' header of the hunk point is in, or nil."
  (save-excursion
    (beginning-of-line)
    (catch 'result
      (while t
        (cond ((looking-at "^@@") (throw 'result (point)))
              ((memq (char-after) '(?\s ?- ?+ ?\\))
               (unless (zerop (forward-line -1)) (throw 'result nil)))
              (t (throw 'result nil)))))))

(defun efrit-diff-preview--hunk-anchor (header-pos target-pos)
  "The hunk at HEADER-POS as (OLD-BLOCK NEW-BLOCK OFFSET).
OLD-BLOCK is the old-side text (context and removed lines), NEW-BLOCK
the new-side text.  OFFSET is the old-side line to land on: the line
at TARGET-POS when given, else the hunk's first change."
  (save-excursion
    (goto-char header-pos)
    (forward-line 1)
    (let ((old-lines nil) (new-lines nil) (seen 0) (offset nil) (first-change nil))
      (while (and (not (eobp)) (memq (char-after) '(?\s ?- ?+ ?\\)))
        (let ((char (char-after))
              (text (buffer-substring-no-properties (1+ (line-beginning-position)) (line-end-position)))
              (at-target (and target-pos (= (line-beginning-position) target-pos))))
          (pcase char
            (?\\ nil)
            (?\s (when at-target (setq offset seen))
                 (push text old-lines) (push text new-lines) (cl-incf seen))
            (?- (when at-target (setq offset seen))
                (unless first-change (setq first-change seen))
                (push text old-lines) (cl-incf seen))
            (?+ (when at-target (setq offset seen))
                (unless first-change (setq first-change seen))
                (push text new-lines))))
        (forward-line 1))
      (list (and old-lines (string-join (nreverse old-lines) "\n"))
            (and new-lines (string-join (nreverse new-lines) "\n"))
            (or offset first-change 0)))))

(defun efrit-diff-preview--file-at-point ()
  "The file of the change point is in, from its `=== Change N: FILE' line."
  (save-excursion
    (end-of-line)
    (when (re-search-backward "^=+ Change [0-9]+: \\(.*?\\) (\\(?:NEW FILE\\|DELETED\\|MODIFIED\\)) " nil t)
      (match-string-no-properties 1))))

(defun efrit-diff-preview--target-at-point ()
  "The file and anchors of the hunk at point, or the nearest one.
A list (FILE OLD-BLOCK NEW-BLOCK OFFSET); FILE alone when point is not
near a hunk."
  (save-excursion
    (let* ((header (efrit-diff-preview--hunk-header-at-point))
           (on-body (and header (/= header (line-beginning-position))))
           (near (or header
                     (save-excursion (and (re-search-forward "^@@" nil t) (line-beginning-position)))
                     (save-excursion (and (re-search-backward "^@@" nil t) (line-beginning-position))))))
      (if near
          (cons (save-excursion (goto-char near) (efrit-diff-preview--file-at-point))
                (efrit-diff-preview--hunk-anchor near (and on-body (line-beginning-position))))
        (list (efrit-diff-preview--file-at-point) nil nil 0)))))

(defun efrit-diff-preview--search-block (block)
  "Start of the first occurrence of BLOCK in the current buffer, or nil."
  (when (and block (not (string-empty-p block)))
    (save-excursion
      (goto-char (point-min))
      (and (search-forward block nil t) (match-beginning 0)))))

(defvar efrit-diff-preview--root nil
  "The directory relative file names in the preview resolve against.")

(defun efrit-diff-preview--root ()
  "Where relative file names in the preview resolve: the root the tool
ran with, else `default-directory'."
  (or efrit-diff-preview--root default-directory))

(defun efrit-diff-preview-open-file ()
  "Open the file of the change at point, at the changed lines.
The old-side text of the hunk is searched for first (the change is
usually not applied yet), then the new-side text.  A file that does
not exist yet (NEW FILE) opens empty."
  (interactive)
  (pcase-let ((`(,file ,old-block ,new-block ,offset) (efrit-diff-preview--target-at-point)))
    (unless file (user-error "No change at point"))
    ;; The user opens the file, not the model: no sandbox check
    (let ((path (expand-file-name file (efrit-diff-preview--root))))
      (find-file-other-window path)
      (if-let* ((pos (or (efrit-diff-preview--search-block old-block)
                         (efrit-diff-preview--search-block new-block))))
          (progn (goto-char pos) (forward-line offset))
        (goto-char (point-min))
        (when (or old-block new-block)
          (message "Changed text not found in %s; at the top" (file-name-nondirectory path))))
      (deactivate-mark t)
      (recenter))))

(defun efrit-diff-preview-toggle-or-open ()
  "RET: toggle the change's selection in selective mode, else open its file."
  (interactive)
  (if (eq efrit-diff-preview--apply-mode 'selective)
      (efrit-diff-preview-toggle-change)
    (efrit-diff-preview-open-file)))

;;; Interactive Commands

(defun efrit-diff-preview--user-edits ()
  "The user_edits value of the result: the edited changes, or :json-false.
Each is (index file new_content) so the model applies the user's text,
not its own proposal."
  (if (null efrit-diff-preview--edited)
      :json-false
    (vconcat
     (mapcar (lambda (i)
               (let ((c (nth i efrit-diff-preview--changes)))
                 `((index . ,i) (file . ,(alist-get 'file c))
                   (new_content . ,(alist-get 'new_content c)))))
             (sort (copy-sequence efrit-diff-preview--edited) #'<)))))

(defun efrit-diff-preview-approve ()
  "Approve the proposed changes."
  (interactive)
  (setq efrit-diff-preview--result
        `((approved . t)
          (selected_changes . ,(if (eq efrit-diff-preview--apply-mode 'selective)
                                   (vconcat efrit-diff-preview--selected-indices)
                                 (vconcat (number-sequence 0 (1- (length efrit-diff-preview--changes))))))
          (user_edits . ,(efrit-diff-preview--user-edits))))
  (setq efrit-diff-preview--waiting nil)
  (message "Changes approved."))

(defun efrit-diff-preview-reject ()
  "Reject the proposed changes."
  (interactive)
  (setq efrit-diff-preview--result
        `((approved . :json-false)
          (selected_changes . [])
          (reason . "user_rejected")))
  (setq efrit-diff-preview--waiting nil)
  (message "Changes rejected."))

(defun efrit-diff-preview-toggle-change ()
  "Toggle selection of the change at point (in selective mode)."
  (interactive)
  (unless (eq efrit-diff-preview--apply-mode 'selective)
    (user-error "Toggle only available in selective mode"))
  (let ((index (efrit-diff-preview--change-at-point)))
    (if index
        (progn
          (if (memq index efrit-diff-preview--selected-indices)
              (setq efrit-diff-preview--selected-indices
                    (delq index efrit-diff-preview--selected-indices))
            (push index efrit-diff-preview--selected-indices))
          (force-mode-line-update)
          (message "Change %d %s" (1+ index)
                   (if (memq index efrit-diff-preview--selected-indices)
                       "selected" "deselected")))
      (message "No change at point"))))

(defun efrit-diff-preview-apply-selected ()
  "Apply only selected changes (in selective mode)."
  (interactive)
  (unless (eq efrit-diff-preview--apply-mode 'selective)
    (user-error "Apply-selected only available in selective mode"))
  (if (null efrit-diff-preview--selected-indices)
      (message "No changes selected. Use SPC to select changes.")
    (setq efrit-diff-preview--result
          `((approved . t)
            (selected_changes . ,(vconcat (sort efrit-diff-preview--selected-indices #'<)))
            (user_edits . ,(efrit-diff-preview--user-edits))))
    (setq efrit-diff-preview--waiting nil)
    (message "Applied %d selected changes." (length efrit-diff-preview--selected-indices))))

(defun efrit-diff-preview--change-at-point ()
  "Return the index of the change at point, or nil if not on a change."
  (save-excursion
    (end-of-line)
    (when (re-search-backward "^=+ Change \\([0-9]+\\):" nil t)
      (1- (string-to-number (match-string 1))))))

;;; Editing a change in ediff before accepting
;;
;; `E' on a change opens ediff: A is the file as it is (the change's
;; old_content, or the file on disk for a new file an empty buffer),
;; B is the proposal.  The user merges hunks or types in B.  On quit,
;; "Accept?" makes B's final text the change's new_content: the
;; preview is redrawn and the tool result carries user_edits with the
;; edited content, so the model applies what the user settled on.
;; After claude-code-ide's openDiff handler (2026-09-28): our ediff is
;; told apart from a foreign one by `eq' on buffer B in the startup
;; hook, side windows are deleted first ("Cannot split side window"),
;; and the response is finished off a timer so ediff has torn down.

(defvar efrit-diff-preview--ediff-b nil
  "Buffer B of the ediff we opened, while it runs.")
(defvar efrit-diff-preview--ediff-index nil)

(declare-function ediff-buffers "ediff")
(defvar ediff-buffer-B)
(defvar ediff-window-setup-function)

(defun efrit-diff-preview--buffer-for-content (name content file)
  "A buffer NAME holding CONTENT (nil: empty), in FILE's major mode."
  (let ((buf (generate-new-buffer name)))
    (with-current-buffer buf
      (when content (insert content))
      (let ((buffer-file-name (expand-file-name file (efrit-diff-preview--root))))
        (condition-case nil (set-auto-mode) (error (fundamental-mode))))
      (set-buffer-modified-p nil)
      (goto-char (point-min)))
    buf))

(defun efrit-diff-preview-ediff ()
  "Edit the change at point in ediff; accepting on quit keeps your edits."
  (interactive)
  (require 'ediff)
  (let* ((index (or (efrit-diff-preview--change-at-point) (user-error "Not on a change")))
         (change (nth index efrit-diff-preview--changes))
         (file (alist-get 'file change))
         (a (efrit-diff-preview--buffer-for-content (format "*efrit A: %s*" file)
                                                    (alist-get 'old_content change) file))
         (b (efrit-diff-preview--buffer-for-content (format "*efrit B: %s*" file)
                                                    (alist-get 'new_content change) file))
         (config (current-window-configuration)))
    (setq efrit-diff-preview--ediff-b b
          efrit-diff-preview--ediff-index index)
    ;; ediff arranges windows itself and cannot split a side window or
    ;; a small pop-up: give it the frame.  The window configuration is
    ;; restored when ediff quits.
    (dolist (w (window-list))
      (when (window-parameter w 'window-side) (ignore-errors (delete-window w))))
    (let ((main (or (get-largest-window nil nil t) (selected-window))))
      (select-window main)
      (ignore-errors (delete-other-windows)))
    (add-hook 'ediff-quit-hook #'efrit-diff-preview--ediff-quit)
    (let ((ediff-window-setup-function 'ediff-setup-windows-plain))
      (ediff-buffers a b
                     (list (lambda ()
                             ;; our ediff only, not a foreign one
                             (when (eq ediff-buffer-B efrit-diff-preview--ediff-b)
                               (setq-local efrit-diff-preview--ediff-config config)
                               (ignore-errors (ediff-next-difference)))))))))

(defvar-local efrit-diff-preview--ediff-config nil)
(declare-function ediff-next-difference "ediff-util")
(defvar ediff-buffer-A)
(defvar ediff-control-buffer)

(defun efrit-diff-preview--ediff-quit ()
  "On our ediff's quit: ask, take B's text as the change, clean up."
  (when (and efrit-diff-preview--ediff-b
             (eq ediff-buffer-B efrit-diff-preview--ediff-b))
    (let* ((b efrit-diff-preview--ediff-b)
           (a ediff-buffer-A)
           (index efrit-diff-preview--ediff-index)
           (config efrit-diff-preview--ediff-config)
           (text (with-current-buffer b (buffer-substring-no-properties (point-min) (point-max)))))
      (setq efrit-diff-preview--ediff-b nil efrit-diff-preview--ediff-index nil)
      (remove-hook 'ediff-quit-hook #'efrit-diff-preview--ediff-quit)
      ;; off ediff's own teardown: it is still deleting its buffers
      (run-at-time 0 nil #'efrit-diff-preview--ediff-finish index text a b config))))

(defun efrit-diff-preview--ediff-finish (index text a b config)
  (unwind-protect
      (let ((change (nth index efrit-diff-preview--changes)))
        (when (and change
                   (not (equal text (alist-get 'new_content change)))
                   (y-or-n-p (format "Accept your edits to change %d? " (1+ index))))
          (setf (alist-get 'new_content change) text)
          (cl-pushnew index efrit-diff-preview--edited)
          (when-let* ((buf (get-buffer efrit-diff-preview-buffer-name)))
            (with-current-buffer buf
              (let ((inhibit-read-only t) (pos (point)))
                (efrit-diff-preview--redraw)
                (goto-char (min pos (point-max))))))
          (message "Change %d now carries your edits; a/y applies them" (1+ index))))
    (when (buffer-live-p a) (kill-buffer a))
    (when (buffer-live-p b) (kill-buffer b))
    (when (window-configuration-p config) (set-window-configuration config))
    (when-let* ((buf (get-buffer efrit-diff-preview-buffer-name)))
      (pop-to-buffer buf))))

;;; Display Functions

(defvar efrit-diff-preview--description nil)

(defun efrit-diff-preview--redraw ()
  "Draw the preview of the current changes into the current buffer."
  (let ((changes efrit-diff-preview--changes)
        (description efrit-diff-preview--description)
        (apply-mode efrit-diff-preview--apply-mode))
    (let ((inhibit-read-only t))
        (erase-buffer)
        ;; Header
        (insert "Efrit Proposed Changes\n")
        (insert (make-string 60 ?=) "\n\n")
        (insert (format "Description: %s\n" (or description "No description provided")))
        (insert (format "Mode: %s\n" (if (eq apply-mode 'selective)
                                         "Selective (choose which changes to apply)"
                                       "All-or-nothing")))
        (insert (format "Number of changes: %d\n" (length changes)))
        (insert "\nPress 'a' or 'y' to approve all, 'r' or 'n' to reject, 'E' to edit a change in ediff")
        (when (eq apply-mode 'selective)
          (insert "\nPress SPC to toggle selection, 's' to apply selected"))
        (insert "\n")
        ;; Each change
        (cl-loop for change in changes
                 for i from 0
                 do (insert (efrit-diff-preview--format-change change i))
                 when (memq i efrit-diff-preview--edited)
                 do (insert (propertize "  (edited by you in ediff)\n" 'face 'warning)))
        ;; Footer
        (insert "\n" (make-string 60 ?=) "\n")
        (insert "END OF PROPOSED CHANGES\n"))))

(defun efrit-diff-preview--display (changes description apply-mode)
  "Display CHANGES with DESCRIPTION in preview buffer using APPLY-MODE."
  (setq efrit-diff-preview--description description
        efrit-diff-preview--edited nil)
  (let ((buffer (get-buffer-create efrit-diff-preview-buffer-name)))
    (with-current-buffer buffer
      ;; `efrit-diff-preview--changes' and `--apply-mode' are set by the
      ;; tool before calling; the redraw reads them
      (setq efrit-diff-preview--changes changes
            efrit-diff-preview--apply-mode apply-mode)
      (efrit-diff-preview--redraw)
      (efrit-diff-preview-mode)
      (goto-char (point-min)))
    ;; Display buffer
    (pop-to-buffer buffer '((display-buffer-reuse-window
                             display-buffer-pop-up-window)
                            (window-height . 0.6)))
    buffer))

;;; Main Tool Function

(defun efrit-tool-show-diff-preview (args)
  "Show user proposed changes before applying them.

ARGS is an alist with:
  changes - list of proposed changes (required)
    Each change has: file, old_content, new_content
  description - what these changes accomplish
  apply_mode - 'all_or_nothing or 'selective (default: all_or_nothing)

Returns a standard tool response with:
  approved - boolean
  selected_changes - indices of changes to apply
  user_edits - any modifications made (currently not supported)"
  (efrit-tool-execute show_diff_preview args
    (let* ((changes (alist-get 'changes args))
           (description (alist-get 'description args))
           (apply-mode (or (alist-get 'apply_mode args) "all_or_nothing"))
           (apply-mode-sym (intern apply-mode)))

      ;; Validate
      (unless changes
        (signal 'user-error (list "changes list is required")))
      ;; Every target must be writable within the granted scope before
      ;; anything is shown or applied
      (mapc (lambda (change)
              (when-let* ((f (alist-get 'file change)))
                (efrit-resolve-path f 'write "show_diff_preview")))
            (append changes nil))

      (when (zerop (length changes))
        (signal 'user-error (list "changes list cannot be empty")))

      ;; Convert vector to list if needed
      (when (vectorp changes)
        (setq changes (append changes nil)))

      ;; Initialize state
      (setq efrit-diff-preview--root (efrit-resolve-path-simple nil))
      (setq efrit-diff-preview--changes changes)
      (setq efrit-diff-preview--result nil)
      (setq efrit-diff-preview--waiting t)
      (setq efrit-diff-preview--apply-mode apply-mode-sym)
      (setq efrit-diff-preview--selected-indices
            (when (eq apply-mode-sym 'selective)
              ;; In selective mode, start with all selected
              (number-sequence 0 (1- (length changes)))))

      ;; The preview is a modal prompt: one at a time, like the sandbox
      ;; menu.  A second session's preview while one is up would nest
      ;; two wait loops (2026-09-29: a hard lockup, C-g did nothing,
      ;; when a drive prompt and the preview waited on each other).
      (efrit-with-prompt-turn "the diff preview" nil
        ;; Display the diff
        (efrit-diff-preview--display changes description apply-mode-sym)

        ;; Wait for the user's decision; the time does not count
        ;; against the turn or the tool.  C-g while waiting is a
        ;; rejection, not a stuck loop: `sit-for' inside a tool call
        ;; can swallow the quit, so the flag is checked by hand.
        (let ((start-time (float-time))
              (timeout efrit-diff-preview-timeout-seconds))
          (efrit-with-user-waiting
            (while (and efrit-diff-preview--waiting
                        (not quit-flag)
                        (or (not timeout) (< (- (float-time) start-time) timeout)))
              (with-local-quit (sit-for 0.1))
              (redisplay)))
          (when (and efrit-diff-preview--waiting quit-flag)
            (setq quit-flag nil)
            (efrit-diff-preview-reject))
          ;; Handle timeout
          (when efrit-diff-preview--waiting
            (setq efrit-diff-preview--result
                  `((approved . :json-false)
                    (selected_changes . [])
                    (reason . "timeout")))
            (setq efrit-diff-preview--waiting nil))))
      ;; refused by the prompt gate (another prompt was up): a rejection
      (unless efrit-diff-preview--result
        (setq efrit-diff-preview--result
              `((approved . :json-false) (selected_changes . [])
                (reason . "another prompt was open; not shown"))))

      ;; Clean up and return result
      (when-let* ((buffer (get-buffer efrit-diff-preview-buffer-name)))
        (kill-buffer buffer))

      (efrit-tool-success efrit-diff-preview--result))))

(provide 'efrit-tool-show-diff-preview)

;;; efrit-tool-show-diff-preview.el ends here
