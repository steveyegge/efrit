;;; efrit-sandbox-ui.el --- Prompt, list and revoke sandbox grants -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; The interactive side of efrit-sandbox:
;;
;; - `efrit-sandbox-ui-prompt' is installed as
;;   `efrit-sandbox-request-function'.  When a tool needs more than the
;;   granted scope it opens a transient menu (`efrit-sandbox-ask')
;;   that names the tool, what it wants, and the exact scope each
;;   answer would grant:
;;
;;       o  once      this one operation
;;       s  session   until this Emacs exits
;;       p  project   saved in <project>/.efrit/sandbox.json
;;       n  no        deny (C-g and q do the same)
;;       ?  details   show/hide the full request in the menu
;;       y  yank      copy the full request to the kill ring
;;       b  buffer    open the full request in a popup (q closes)
;;
;;   The menu runs inside a `recursive-edit' so the calling tool gets
;;   an answer synchronously; `transient-post-exit-hook' exits the
;;   recursive edit once the menu closes for any reason.  Without a
;;   display (batch, no transient) it falls back to `read-char-choice'.
;;
;;   The grant is as narrow as the request: allowing a write to
;;   ~/notes/todo.org for the project grants write under ~/notes/, not
;;   under ~.  The menu says exactly what will be granted.
;;
;; - A denial is not the end of the turn.  The model gets a tool
;;   result saying the access was refused and carries on without it.
;;
;; - `M-x efrit-sandbox' lists every grant in force for the current
;;   project in a tabulated-list with `d' to revoke, `g' to refresh,
;;   `s' to add a grant by hand.
;;
;; - The agent buffer gets a one-line record of each grant and denial,
;;   so reading back a transcript shows where the fence moved.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'tabulated-list)
(require 'efrit-sandbox)
(require 'efrit-sandbox-store)

(defvar transient-post-exit-hook)
(declare-function efrit-agent--append-to-conversation "efrit-agent-core")
(declare-function efrit-show-preview "efrit-ui-helpers")
(defvar efrit-agent-buffer-name)

(defgroup efrit-sandbox-ui nil
  "Sandbox prompts and listing."
  :group 'efrit-sandbox)

(defface efrit-sandbox-prompt-face
  '((t :inherit warning :weight bold))
  "Face for the sandbox request line in the agent buffer.")

(defface efrit-sandbox-grant-face
  '((t :inherit success))
  "Face for a recorded grant in the agent buffer.")

(defface efrit-sandbox-deny-face
  '((t :inherit error))
  "Face for a recorded denial in the agent buffer.")

(defface efrit-sandbox-detail-face
  '((t :inherit fixed-pitch))
  "Face of the request detail (the form or command) in the sandbox menu.")

;;; Transcript notes

(defun efrit-sandbox-ui--note (text face)
  "Append a one-line TEXT in FACE to the agent conversation, if it exists."
  (when (and (boundp 'efrit-agent-buffer-name)
             (get-buffer efrit-agent-buffer-name)
             (fboundp 'efrit-agent--append-to-conversation))
    (with-current-buffer efrit-agent-buffer-name
      (efrit-agent--append-to-conversation
       (concat (propertize (concat "  ⛨ " text) 'face face) "\n")
       (list 'efrit-type 'sandbox-note)))))

;;; The prompt

(defun efrit-sandbox-ui--scope-word (req)
  "What the grant would cover, in the user's terms."
  (let ((cap (efrit-sandbox-request-cap req))
        (target (efrit-sandbox-request-target req)))
    (pcase cap
      ('read (format "read files under %s" (abbreviate-file-name target)))
      ('write (format "write files under %s" (abbreviate-file-name target)))
      ('elisp "evaluate Emacs Lisp (each file/process it touches is still checked)")
      ('shell "run shell commands (a single grant: any command)")
      ('net "make network requests")
      (_ (format "%s %s" cap target)))))

(defvar efrit-sandbox-ui--request nil
  "The request the open `efrit-sandbox-ask' menu is about.")

(defvar efrit-sandbox-ui--answer 'pending
  "Answer chosen in the menu: once/session/project/nil, or `pending'.")

(defvar efrit-sandbox-ui--depth nil
  "Recursion depth of the recursive edit waiting on the menu, or nil.")

(defvar efrit-sandbox-ui--details-shown nil
  "Non-nil while the menu shows the full request instead of the short block.
Toggled by ? in the menu; reset for each new request.")

(defun efrit-sandbox-ui--menu-description ()
  "Header of the menu: tool, request, and the detail (short, or full after ?)."
  (let* ((req efrit-sandbox-ui--request)
         (tool (or (efrit-sandbox-request-tool req) "a tool"))
         (detail (efrit-sandbox-request-detail req)))
    (concat
     (propertize (format "Efrit: %s wants to %s" tool (efrit-sandbox-ui--scope-word req))
                 'face 'efrit-sandbox-prompt-face)
     (cond
      (efrit-sandbox-ui--details-shown
       (concat "\n" (efrit-sandbox-ui--indent (efrit-sandbox-ui--details-text req t))))
      ((and detail (not (string-empty-p detail)))
       (concat "\n" (efrit-sandbox-ui--detail-block detail))))
     "\n")))

(defun efrit-sandbox-ui--indent (text)
  "TEXT with every line indented two spaces and chopped to the frame width."
  (let ((width (- (frame-width) 6)))
    (mapconcat (lambda (l) (concat "  " (truncate-string-to-width l width nil nil "…")))
               (split-string text "\n") "\n")))

(defcustom efrit-sandbox-ui-detail-lines 12
  "Most lines of the request detail (the form, the command) shown in the menu.
? shows the rest in the menu, b in a buffer."
  :type 'integer
  :group 'efrit-sandbox-ui)

(defun efrit-sandbox-ui--detail-block (detail)
  "DETAIL as an indented block for the menu heading: code as code.
Multi-line details (an eval_sexp form) keep their lines, in a
fixed-pitch face, cut to `efrit-sandbox-ui-detail-lines' with a note;
each line is chopped to the frame width."
  (let* ((lines (split-string detail "\n"))
         (shown (seq-take lines efrit-sandbox-ui-detail-lines))
         (width (- (frame-width) 6)))
    (concat
     (mapconcat (lambda (l)
                  (concat "  " (propertize (truncate-string-to-width l width nil nil "…")
                                           'face 'efrit-sandbox-detail-face)))
                shown "\n")
     (when (> (length lines) (length shown))
       (propertize (format "\n  … %d more lines (? shows them)" (- (length lines) (length shown)))
                   'face 'shadow)))))

(defun efrit-sandbox-ui--project-label ()
  (format "this project (%s, saved)"
          (abbreviate-file-name (directory-file-name (efrit-sandbox-project-root)))))

(defun efrit-sandbox-ui--choose (answer)
  (setq efrit-sandbox-ui--answer answer))

(defun efrit-sandbox-ui--exit-recursive-edit ()
  "Leave the recursive edit that waits on the menu, if we are in it."
  (when (and efrit-sandbox-ui--depth
             (= (recursion-depth) efrit-sandbox-ui--depth))
    (exit-recursive-edit)))

(defun efrit-sandbox-ui--define-menu ()
  "Define `efrit-sandbox-ask' if transient is available.  Return non-nil on success."
  (when (require 'transient nil t)
    (unless (fboundp 'efrit-sandbox-ask)
      (eval
       '(transient-define-prefix efrit-sandbox-ask ()
          "Allow the sandbox request?"
          [:description efrit-sandbox-ui--menu-description
           ["Allow"
            ("o" "once" (lambda () (interactive) (efrit-sandbox-ui--choose 'once)))
            ("s" "for this Emacs session" (lambda () (interactive) (efrit-sandbox-ui--choose 'session)))
            ("p" (lambda () (interactive) (efrit-sandbox-ui--choose 'project))
             :description efrit-sandbox-ui--project-label)]
           ["Refuse"
            ("n" "no, the model continues without it"
             (lambda () (interactive) (efrit-sandbox-ui--choose nil)))]
           ["Details"
            ("?" efrit-sandbox-ui-toggle-details
             :description efrit-sandbox-ui--toggle-label :transient t)
            ("y" "yank details to the kill ring" efrit-sandbox-ui-yank-details :transient t)
            ("b" "open details in a buffer" efrit-sandbox-ui-open-details :transient t)
            ("l" "grants in force" (lambda () (interactive) (efrit-sandbox (efrit-sandbox-project-root)))
             :transient t)]])
       t))
    (fboundp 'efrit-sandbox-ask)))

(defun efrit-sandbox-ui--ask-with-menu (req)
  "Open the transient menu for REQ and wait for an answer.
Returns once/session/project or nil.  Closing the menu any other way
\(C-g, q, another command) is a denial."
  (setq efrit-sandbox-ui--request req
        efrit-sandbox-ui--answer 'pending
        efrit-sandbox-ui--details-shown nil)
  (let ((efrit-sandbox-ui--depth (1+ (recursion-depth))))
    (unwind-protect
        (progn
          (add-hook 'transient-post-exit-hook #'efrit-sandbox-ui--exit-recursive-edit)
          ;; Open the menu from the command loop, not from inside the
          ;; tool's call stack: transient needs to be a command.
          (run-at-time 0 nil (lambda () (call-interactively #'efrit-sandbox-ask)))
          (condition-case nil
              (recursive-edit)
            (quit nil)))
      (remove-hook 'transient-post-exit-hook #'efrit-sandbox-ui--exit-recursive-edit)
      (efrit-sandbox-ui--hide-details)
      (setq efrit-sandbox-ui--request nil)))
  (if (eq efrit-sandbox-ui--answer 'pending) nil efrit-sandbox-ui--answer))

(defun efrit-sandbox-ui--ask-in-echo-area (req)
  "Fallback prompt in the echo area when no menu can be shown."
  (let* ((tool (or (efrit-sandbox-request-tool req) "a tool"))
         (header (format "Efrit (%s) wants to %s\n" tool (efrit-sandbox-ui--scope-word req)))
         (legend (format "[o]nce  [s]ession  [p]roject %s  [n]o  [?]details "
                         (abbreviate-file-name (efrit-sandbox-project-root)))))
    (unwind-protect
        (catch 'decided
          (while t
            (pcase (read-char-choice (concat header legend) '(?o ?s ?p ?n ??))
              (?o (throw 'decided 'once))
              (?s (throw 'decided 'session))
              (?p (throw 'decided 'project))
              (?n (throw 'decided nil))
              (?? (efrit-sandbox-ui--show-details req)))))
      (efrit-sandbox-ui--hide-details))))

(defun efrit-sandbox-ui-use-menu-p ()
  "Non-nil when the transient menu can be used for the prompt."
  (and (not noninteractive)
       (efrit-sandbox-ui--define-menu)))

(defun efrit-sandbox-ui-prompt (req)
  "Ask the user about REQ; return `once', `session', `project', or nil."
  (let* ((tool (or (efrit-sandbox-request-tool req) "a tool"))
         (what (efrit-sandbox-ui--scope-word req)))
    (efrit-sandbox-ui--note (format "%s asks to %s" tool what) 'efrit-sandbox-prompt-face)
    (let ((answer (if (efrit-sandbox-ui-use-menu-p)
                      (efrit-sandbox-ui--ask-with-menu req)
                    (efrit-sandbox-ui--ask-in-echo-area req))))
      (efrit-sandbox-ui--note
       (pcase answer
         ('once (format "granted once: %s" what))
         ('session (format "granted for this session: %s" what))
         ('project (format "granted for this project (saved): %s" what))
         (_ (format "denied: %s" what)))
       (if answer 'efrit-sandbox-grant-face 'efrit-sandbox-deny-face))
      answer)))

(defconst efrit-sandbox-ui--details-buffer "*efrit-sandbox-request*"
  "Popup showing the full sandbox request while the menu is up.")

(defun efrit-sandbox-ui--hide-details ()
  "Remove the details popup, if shown.
Called when the menu closes: the popup describes a request that has
just been answered.  The buffer stays for `q'-less inspection later."
  (when-let* ((buf (get-buffer efrit-sandbox-ui--details-buffer))
              (win (get-buffer-window buf t)))
    (ignore-errors (quit-window nil win))))

(defun efrit-sandbox-ui--details-text (req &optional fontify)
  "Everything known about REQ as plain text, for the menu, the kill ring, or a buffer.
With FONTIFY, an elisp form is fontified as Emacs Lisp (for display)."
  (let* ((tool (efrit-sandbox-request-tool req))
         (cap (efrit-sandbox-request-cap req))
         (target (efrit-sandbox-request-target req))
         (detail (efrit-sandbox-request-detail req))
         (what (pcase cap
                 ('elisp "Form to evaluate")
                 ('shell "Command")
                 ('net "Request")
                 ('buffer "Buffer")
                 (_ "Detail")))
         (body (cond
                ((null detail) "")
                ((and fontify (eq cap 'elisp)) (efrit-sandbox-ui--fontify-lisp detail))
                (t detail))))
    (concat
     (format "%s wants to %s\nProject: %s" (or tool "a tool") (efrit-sandbox-ui--scope-word req)
             (abbreviate-file-name (efrit-sandbox-project-root)))
     (if (and (stringp target) (not (eq cap 'elisp)))
         (format "\nGrant:   %s" (abbreviate-file-name target))
       "")
     (if detail (format "\n\n%s:\n%s" what body) "")
     "\n\nGrants in force:\n" (efrit-sandbox-ui--grants-text))))

(defun efrit-sandbox-ui--fontify-lisp (text)
  "TEXT with `emacs-lisp-mode' font-lock faces applied."
  (with-temp-buffer
    (insert text)
    (delay-mode-hooks (emacs-lisp-mode))
    ;; font-lock is off in batch and fresh temp buffers until enabled
    (font-lock-mode 1)
    (font-lock-ensure)
    (buffer-string)))

(defun efrit-sandbox-ui--toggle-label ()
  (if efrit-sandbox-ui--details-shown "hide details" "show details"))

(defun efrit-sandbox-ui-toggle-details ()
  "Show the full request in the menu, or the short block again."
  (interactive)
  ;; The heading is a function of this flag; transient re-renders the
  ;; menu after every :transient t suffix, so flipping it is enough
  (setq efrit-sandbox-ui--details-shown (not efrit-sandbox-ui--details-shown)))

(defun efrit-sandbox-ui-yank-details ()
  "Copy the full request text to the kill ring."
  (interactive)
  (when efrit-sandbox-ui--request
    (kill-new (efrit-sandbox-ui--details-text efrit-sandbox-ui--request))
    (message "Sandbox request copied to the kill ring")))

(defun efrit-sandbox-ui-open-details ()
  "Open the full request in a popup buffer (`q' closes it)."
  (interactive)
  (when efrit-sandbox-ui--request
    (efrit-sandbox-ui--show-details efrit-sandbox-ui--request)))

(defun efrit-sandbox-ui--show-details (req)
  "Pop up everything known about REQ.
Shown next to the transient menu, not selected: the menu is still
reading keys.  The popup is dedicated and `q' dismisses it once the
menu is gone; answering the menu removes it too."
  (require 'efrit-ui-helpers)
  (efrit-show-preview efrit-sandbox-ui--details-buffer
                      (efrit-sandbox-ui--details-text req t)
                      'efrit-preview-mode))

(defun efrit-sandbox-ui--grants-text ()
  (let ((gs (efrit-sandbox-grants)))
    (if (null gs) "  (only the default: read inside the project root)"
      (mapconcat (lambda (g) (format "  %-6s %-8s %s" (plist-get g :cap) (plist-get g :scope)
                                     (let ((tg (plist-get g :target)))
                                       (if (eq tg t) "" (abbreviate-file-name tg)))))
                 gs "\n"))))

;; Install as the default prompt
(unless efrit-sandbox-request-function
  (setq efrit-sandbox-request-function #'efrit-sandbox-ui-prompt))

;;; Listing

(defvar efrit-sandbox-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "d") #'efrit-sandbox-list-revoke)
    (define-key map (kbd "s") #'efrit-sandbox-list-add)
    (define-key map (kbd "g") #'efrit-sandbox-list-refresh)
    map))

(define-derived-mode efrit-sandbox-list-mode tabulated-list-mode "Efrit-Sandbox"
  "List of sandbox grants for the current efrit project."
  (setq tabulated-list-format [("Cap" 7 t) ("Scope" 9 t) ("Target" 60 t)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

(defvar-local efrit-sandbox-list--root nil)

(defun efrit-sandbox-list--entries (root)
  (let ((i 0))
    (append
     (list (list (cl-incf i)
                 (vector "read" "default" (abbreviate-file-name root))))
     (mapcar (lambda (g)
               (list (cons (cl-incf i) g)
                     (vector (symbol-name (plist-get g :cap))
                             (symbol-name (plist-get g :scope))
                             (let ((tg (plist-get g :target)))
                               (if (eq tg t) "—" (abbreviate-file-name tg))))))
             (efrit-sandbox-grants root)))))

(defun efrit-sandbox-list-refresh ()
  "Re-read grants for the buffer's project."
  (interactive)
  (efrit-sandbox-store-forget efrit-sandbox-list--root)
  (efrit-sandbox-store-ensure-loaded efrit-sandbox-list--root)
  (setq tabulated-list-entries (efrit-sandbox-list--entries efrit-sandbox-list--root))
  (tabulated-list-print t))

(defun efrit-sandbox-list-revoke ()
  "Revoke the grant at point."
  (interactive)
  (let ((id (tabulated-list-get-id)))
    (if (not (consp id))
        (user-error "The default project read cannot be revoked here; set efrit-sandbox-default-project-grants")
      (let ((g (cdr id)))
        (when (y-or-n-p (format "Revoke %s %s? " (plist-get g :cap)
                                (let ((tg (plist-get g :target))) (if (eq tg t) "" tg))))
          (efrit-sandbox-revoke (plist-get g :cap) (plist-get g :target) efrit-sandbox-list--root)
          (efrit-sandbox-list-refresh))))))

(defun efrit-sandbox-list-add ()
  "Add a grant by hand."
  (interactive)
  (let* ((cap (intern (completing-read "Capability: " '("read" "write" "elisp" "shell" "net") nil t)))
         (target (if (memq cap '(read write))
                     (efrit-sandbox-canonical (read-directory-name "Directory: " efrit-sandbox-list--root))
                   t))
         (scope (intern (completing-read "Scope: " '("session" "project") nil t "session"))))
    (efrit-sandbox-grant cap target scope efrit-sandbox-list--root)
    (efrit-sandbox-list-refresh)))

;;;###autoload
(defun efrit-sandbox (&optional root)
  "Show the sandbox grants for the current project (or ROOT).
d revokes the grant at point, s adds one, g refreshes."
  (interactive)
  (let ((root (or root (efrit-sandbox-project-root))))
    (with-current-buffer (get-buffer-create "*efrit-sandbox*")
      (efrit-sandbox-list-mode)
      (setq efrit-sandbox-list--root root)
      (setq header-line-format (format " Sandbox for %s   (d revoke, s add, g refresh, q close)"
                                       (abbreviate-file-name root)))
      (efrit-sandbox-list-refresh)
      ;; A popup at the bottom, not a takeover of the user's window:
      ;; `q' (quit-window, from tabulated-list-mode) then removes it.
      (pop-to-buffer (current-buffer)
                     '((display-buffer-reuse-window display-buffer-at-bottom)
                       (window-height . fit-window-to-buffer)
                       (dedicated . t))))))

(provide 'efrit-sandbox-ui)

;;; efrit-sandbox-ui.el ends here
