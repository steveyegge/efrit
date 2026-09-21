;;; efrit-permissions-ui.el --- Editor for grants and policy across projects -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; `M-x efrit-permissions' is one place to see and change everything
;; that decides what efrit may do:
;;
;;   - sandbox grants (session and project) for every project efrit
;;     has touched, not only the current one;
;;   - per-project policy: default grants, review on/off and classes,
;;     loop limits (all in <project>/.efrit/settings.json);
;;   - global policy: the customization variables behind those, and
;;     the legacy per-call layer (`efrit-permission-policy') that
;;     runs only when the sandbox is off.
;;
;; The buffer is a `tabulated-list': one row per grant or setting,
;; grouped by project.  RET (or e) on a row opens a transient menu
;; with the actions that make sense for it: change scope, widen or
;; narrow the target, revoke, toggle a class, set a number.  Marks
;; (m/u) with D revoke several grants at once; a adds a grant; p
;; switches the project in focus; g re-reads everything from disk.
;;
;; `M-x efrit-sandbox' still exists and opens the same buffer filtered
;; to the current project.
;;
;; Nothing here is model-callable: every efrit-permissions-* symbol is
;; refused by the eval_sexp inspection (prefix efrit-permission).

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'tabulated-list)
(require 'efrit-sandbox)
(require 'efrit-sandbox-store)
(require 'efrit-sandbox-ui)
(require 'efrit-settings)
(require 'efrit-review)
(require 'efrit-limits)
(require 'efrit-permissions)
;; the variables behind the limits (`efrit-limits--variable'), so the
;; value column shows the real default rather than nil
(require 'efrit-repl-loop)
(require 'efrit-do-circuit-breaker)

(declare-function transient-quit-one "transient")
(declare-function efrit-permissions-grant-menu "efrit-permissions-ui")
(declare-function efrit-permissions-default-menu "efrit-permissions-ui")
(declare-function efrit-permissions-review-menu "efrit-permissions-ui")
(declare-function efrit-permissions-limit-menu "efrit-permissions-ui")
(declare-function efrit-permissions-global-menu "efrit-permissions-ui")

(defgroup efrit-permissions-ui nil
  "The permissions editor."
  :group 'efrit-sandbox)

(defface efrit-permissions-project
  '((t :inherit font-lock-keyword-face :weight bold))
  "Face of a project heading row."
  :group 'efrit-permissions-ui)

(defface efrit-permissions-project-current
  '((t :inherit efrit-permissions-project :underline t))
  "Face of the heading row of the project in focus."
  :group 'efrit-permissions-ui)

(defface efrit-permissions-default
  '((t :inherit shadow))
  "Face of a value that comes from the customization default."
  :group 'efrit-permissions-ui)

(defface efrit-permissions-override
  '((t :inherit font-lock-constant-face))
  "Face of a value set for this project."
  :group 'efrit-permissions-ui)

(defface efrit-permissions-wide
  '((t :inherit warning))
  "Face of a grant that covers a lot: any shell command, or a path at / or ~."
  :group 'efrit-permissions-ui)

(defface efrit-permissions-mark
  '((t :inherit font-lock-warning-face :weight bold))
  "Face of the mark column."
  :group 'efrit-permissions-ui)

;;; Rows
;;
;; A row id is a plist: (:kind KIND :root ROOT ...).  KIND is one of
;;   project   the heading of ROOT
;;   grant     :grant PLIST (a sandbox grant of ROOT)
;;   default   the implicit default grants of ROOT
;;   review    review enabled/classes for ROOT
;;   limit     :name NAME, a loop limit for ROOT
;;   global    :var SYMBOL, a customization variable (ROOT nil)

(defvar-local efrit-permissions--focus nil
  "The project root in focus: listed first, target of `a'.")

(defvar-local efrit-permissions--only-focus nil
  "When non-nil, list only the project in focus (`M-x efrit-sandbox').")

(defvar-local efrit-permissions--marks nil
  "Grant ids marked for a bulk action.")

(defconst efrit-permissions--buffer "*efrit-permissions*")

(defconst efrit-permissions-markable-glyph "○"
  "Shown in the mark column of a row that `m' can mark (a grant).
Other rows leave the column blank, so what is markable is visible
before pressing anything.")

(defconst efrit-permissions-marked-glyph "●"
  "Shown in the mark column of a marked grant.")

(defun efrit-permissions--projects ()
  "Project roots to show: the focus first, then the registry, then any
root with session grants in memory."
  (let ((out nil))
    (dolist (r (append (list efrit-permissions--focus)
                       (efrit-settings-known-projects)
                       (hash-table-keys efrit-sandbox--session-grants)
                       (hash-table-keys efrit-sandbox--project-grants)))
      (when (and r (not (member r out))) (push r out)))
    (setq out (nreverse out))
    (if efrit-permissions--only-focus
        (list efrit-permissions--focus)
      out)))

(defun efrit-permissions--wide-p (grant)
  "Non-nil if GRANT covers a lot."
  (let ((cap (plist-get grant :cap)) (tg (plist-get grant :target)))
    (or (and (eq cap 'shell) (eq tg t))
        (and (memq cap '(read write)) (stringp tg)
             (member tg (list "/" (efrit-sandbox-canonical "~")))))))

(defun efrit-permissions--grant-target-string (grant)
  (let ((tg (plist-get grant :target)))
    (cond ((eq tg t) (if (eq (plist-get grant :cap) 'shell) "any command" "—"))
          (t (efrit-sandbox--target-label tg)))))

(defun efrit-permissions--project-rows (root)
  "The rows for ROOT: heading, defaults, grants, review, limits."
  (efrit-sandbox-store-ensure-loaded root)
  (let* ((current (equal root efrit-permissions--focus))
         (rows nil)
         (grants (efrit-sandbox-grants root))
         (defaults (efrit-sandbox-effective-default-grants root))
         (default-set (not (eq (efrit-sandbox-project-default-grants root) 'unset)))
         (review (efrit-review-project-override root))
         (review-on (efrit-review-enabled-p root))
         (review-set (or (not (eq (plist-get review :enabled) 'unset)) (plist-get review :classes))))
    ;; heading
    (push (list (list :kind 'project :root root)
                (vector "" (propertize (abbreviate-file-name (directory-file-name root))
                                       'face (if current 'efrit-permissions-project-current
                                               'efrit-permissions-project))
                        "" "" ""))
          rows)
    ;; implicit defaults
    (push (list (list :kind 'default :root root)
                (vector "" "  default grants"
                        (propertize (if defaults (mapconcat #'symbol-name defaults " ") "none")
                                    'face (if default-set 'efrit-permissions-override
                                            'efrit-permissions-default))
                        (if default-set "project" "global")
                        "on the project root, without asking"))
          rows)
    ;; grants
    (dolist (g grants)
      (let ((id (list :kind 'grant :root root :grant g)))
        (push (list id
                    (vector (if (member id efrit-permissions--marks)
                                (propertize efrit-permissions-marked-glyph 'face 'efrit-permissions-mark)
                              (propertize efrit-permissions-markable-glyph 'face 'efrit-permissions-default))
                            (format "  %s" (plist-get g :cap))
                            (propertize (efrit-permissions--grant-target-string g)
                                        'face (if (efrit-permissions--wide-p g)
                                                  'efrit-permissions-wide 'default))
                            (symbol-name (plist-get g :scope))
                            ""))
              rows)))
    ;; review
    (push (list (list :kind 'review :root root)
                (vector "" "  review"
                        (propertize (if review-on
                                        (format "on: %s" (mapconcat #'symbol-name
                                                                    (efrit-review-effective-classes root) " "))
                                      "off")
                                    'face (if review-set 'efrit-permissions-override
                                            'efrit-permissions-default))
                        (if review-set "project" "global")
                        "second-model review of tool calls"))
          rows)
    ;; limits
    (dolist (name efrit-limits-known)
      (let* ((var (efrit-limits--variable name))
             (default (and var (boundp var) (symbol-value var)))
             (project (alist-get name (efrit-limits-project-overrides root)))
             (session (alist-get name (gethash root efrit-limits--session)))
             (effective (efrit-limits-effective name default root)))
        (push (list (list :kind 'limit :root root :name name)
                    (vector "" (format "  %s" name)
                            (propertize (format "%s" effective)
                                        'face (if (or project session) 'efrit-permissions-override
                                                'efrit-permissions-default))
                            (cond (session "session") (project "project") (t "global"))
                            (format "%s per turn" (efrit-limits--unit name))))
              rows)))
    (nreverse rows)))

(defun efrit-permissions--global-rows ()
  "Rows for the global policy variables."
  (list
   (list (list :kind 'project :root nil)
         (vector "" (propertize "Global policy" 'face 'efrit-permissions-project) "" "" ""))
   (list (list :kind 'global :var 'efrit-sandbox-enabled)
         (vector "" "  sandbox" (if efrit-sandbox-enabled "on" (propertize "OFF" 'face 'efrit-permissions-wide))
                 "global" "scope checks on every tool"))
   (list (list :kind 'global :var 'efrit-sandbox-default-project-grants)
         (vector "" "  default grants"
                 (mapconcat #'symbol-name efrit-sandbox-default-project-grants " ")
                 "global" "for projects with no override"))
   (list (list :kind 'global :var 'efrit-review-enabled)
         (vector "" "  review" (if efrit-review-enabled "on" "off") "global" "for projects with no override"))
   (list (list :kind 'global :var 'efrit-review-classes)
         (vector "" "  review classes" (mapconcat #'symbol-name efrit-review-classes " ")
                 "global" "tool classes the reviewer judges"))
   (list (list :kind 'global :var 'efrit-review-on-failure)
         (vector "" "  review outage"
                 (format "exec %s, others %s"
                         (efrit-review-failure-policy '(exec))
                         (efrit-review-failure-policy '(write)))
                 "global" "verdict when the review call fails"))
   (list (list :kind 'global :var 'efrit-sandbox-shell-always-ask)
         (vector "" "  shell always-ask" (format "%d patterns" (length efrit-sandbox-shell-always-ask))
                 "global" "lines asked every time (rm -rf, sudo, ...)"))
   (list (list :kind 'global :var 'efrit-permission-policy)
         (vector "" "  per-call prompt"
                 (if efrit-sandbox-enabled
                     (propertize "inactive (sandbox on)" 'face 'efrit-permissions-default)
                   (mapconcat #'symbol-name efrit-permission-policy " "))
                 "global" "legacy consent layer, sandbox off only"))))

(defun efrit-permissions--entries ()
  (append (cl-mapcan #'efrit-permissions--project-rows (efrit-permissions--projects))
          (unless efrit-permissions--only-focus (efrit-permissions--global-rows))))

;;; Mode

(defvar efrit-permissions-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'efrit-permissions-edit)
    (define-key map (kbd "e") #'efrit-permissions-edit)
    (define-key map (kbd "m") #'efrit-permissions-mark)
    (define-key map (kbd "u") #'efrit-permissions-unmark)
    (define-key map (kbd "U") #'efrit-permissions-unmark-all)
    (define-key map (kbd "D") #'efrit-permissions-revoke-marked)
    (define-key map (kbd "d") #'efrit-permissions-revoke)
    (define-key map (kbd "a") #'efrit-permissions-add)
    (define-key map (kbd "p") #'efrit-permissions-focus-project)
    (define-key map (kbd "A") #'efrit-permissions-toggle-all-projects)
    (define-key map (kbd "g") #'efrit-permissions-refresh)
    (define-key map (kbd "?") #'efrit-permissions-help)
    map))

(define-derived-mode efrit-permissions-mode tabulated-list-mode "Efrit-Permissions"
  "Editor for efrit's grants and policy.  \\{efrit-permissions-mode-map}"
  (setq tabulated-list-format [("" 2 nil) ("What" 22 nil) ("Value" 44 nil) ("Scope" 8 nil) ("" 0 nil)])
  (setq tabulated-list-padding 1)
  (tabulated-list-init-header))

(defun efrit-permissions--id ()
  (or (tabulated-list-get-id) (user-error "No row here")))

(defun efrit-permissions-refresh ()
  "Re-read grants and settings from disk and redraw."
  (interactive)
  (efrit-sandbox-store-forget)
  (efrit-settings-forget)
  (let ((line (line-number-at-pos)))
    (setq tabulated-list-entries (efrit-permissions--entries))
    (tabulated-list-print t)
    (goto-char (point-min))
    (forward-line (1- line))))

(defun efrit-permissions--header ()
  (format " efrit permissions%s   RET edit · a add · m/D mark/revoke · p project · A all · g refresh · ? help · q close"
          (if efrit-permissions--only-focus
              (format " for %s" (abbreviate-file-name (directory-file-name efrit-permissions--focus)))
            "")))

;;;###autoload
(defun efrit-permissions (&optional root only)
  "Edit efrit's grants and policy for every known project.
ROOT (default the current project) is listed first and receives new
grants.  With ONLY, list ROOT alone."
  (interactive)
  (let ((root (file-name-as-directory (or root (efrit-sandbox-project-root)))))
    (with-current-buffer (get-buffer-create efrit-permissions--buffer)
      (efrit-permissions-mode)
      (setq efrit-permissions--focus root
            efrit-permissions--only-focus only
            efrit-permissions--marks nil)
      (setq header-line-format (efrit-permissions--header))
      (efrit-permissions-refresh)
      (pop-to-buffer (current-buffer)
                     '((display-buffer-reuse-window display-buffer-at-bottom)
                       (window-height . 0.5)
                       (dedicated . t))))))

;;;###autoload
(defun efrit-sandbox (&optional root)
  "Show and edit the sandbox grants for the current project (or ROOT).
The same editor as `efrit-permissions', limited to one project."
  (interactive)
  (efrit-permissions root t))

(defun efrit-permissions-focus-project ()
  "Choose the project in focus."
  (interactive)
  (let* ((known (efrit-permissions--projects))
         (choice (completing-read "Project: " (mapcar #'abbreviate-file-name known) nil nil)))
    (setq efrit-permissions--focus (efrit-sandbox-canonical (expand-file-name choice)))
    (setq header-line-format (efrit-permissions--header))
    (efrit-permissions-refresh)))

(defun efrit-permissions-toggle-all-projects ()
  "Switch between the project in focus alone and every project."
  (interactive)
  (setq efrit-permissions--only-focus (not efrit-permissions--only-focus))
  (setq header-line-format (efrit-permissions--header))
  (efrit-permissions-refresh))

(defun efrit-permissions-help ()
  "Describe the editor's keys."
  (interactive)
  (describe-keymap 'efrit-permissions-mode-map))

;;; Marks and bulk revoke

(defun efrit-permissions-mark ()
  "Mark the grant at point and move down."
  (interactive)
  (let ((id (efrit-permissions--id)))
    (unless (eq (plist-get id :kind) 'grant) (user-error "Only grants (rows with %s) can be marked" efrit-permissions-markable-glyph))
    (cl-pushnew id efrit-permissions--marks :test #'equal)
    (efrit-permissions-refresh)
    (forward-line 1)))

(defun efrit-permissions-unmark ()
  "Unmark the grant at point and move down."
  (interactive)
  (setq efrit-permissions--marks (delete (efrit-permissions--id) efrit-permissions--marks))
  (efrit-permissions-refresh)
  (forward-line 1))

(defun efrit-permissions-unmark-all ()
  "Remove every mark."
  (interactive)
  (setq efrit-permissions--marks nil)
  (efrit-permissions-refresh))

(defun efrit-permissions--revoke-id (id)
  (let ((g (plist-get id :grant)))
    (efrit-sandbox-revoke (plist-get g :cap) (plist-get g :target) (plist-get id :root))))

(defun efrit-permissions-revoke ()
  "Revoke the grant at point."
  (interactive)
  (let ((id (efrit-permissions--id)))
    (unless (eq (plist-get id :kind) 'grant)
      (user-error "Not a grant; RET edits this row"))
    (let ((g (plist-get id :grant)))
      (when (yes-or-no-p (format "Revoke %s %s? " (plist-get g :cap)
                                 (efrit-permissions--grant-target-string g)))
        (efrit-permissions--revoke-id id)
        (efrit-permissions-refresh)))))

(defun efrit-permissions-revoke-marked ()
  "Revoke every marked grant."
  (interactive)
  (unless efrit-permissions--marks (user-error "Nothing marked (m marks a grant; rows with %s can be marked)" efrit-permissions-markable-glyph))
  (when (yes-or-no-p (format "Revoke %d marked grant%s? " (length efrit-permissions--marks)
                             (if (cdr efrit-permissions--marks) "s" "")))
    (mapc #'efrit-permissions--revoke-id efrit-permissions--marks)
    (setq efrit-permissions--marks nil)
    (efrit-permissions-refresh)))

;;; Adding a grant

(defun efrit-permissions--read-target (cap root)
  "Ask for a grant target for CAP under ROOT."
  (pcase cap
    ((or 'read 'write)
     (efrit-sandbox-canonical (read-directory-name (format "%s under directory: " cap) root)))
    ('shell
     (let ((names (split-string (read-string "Commands (space-separated; empty = any command): ") " " t)))
       (if names (cons 'shell names) t)))
    ('buffer
     (let ((buf (read-buffer "Buffer: " nil t)))
       (efrit-sandbox-buffer-target (get-buffer buf))))
    (_ t)))

(defun efrit-permissions-add (&optional root)
  "Add a grant to ROOT (default the project in focus)."
  (interactive)
  (let* ((root (or root efrit-permissions--focus))
         (cap (intern (completing-read "Capability: " (mapcar #'symbol-name efrit-sandbox-capabilities) nil t)))
         (target (efrit-permissions--read-target cap root))
         (scope (intern (completing-read "Scope: " '("session" "project") nil t nil nil "session"))))
    (efrit-sandbox-grant cap target scope root)
    (efrit-permissions-refresh)))

;;; Editing a row: a transient per row kind

(defvar efrit-permissions--row nil
  "The row id the open edit menu is about.")

(defun efrit-permissions--row-root () (plist-get efrit-permissions--row :root))
(defun efrit-permissions--row-grant () (plist-get efrit-permissions--row :grant))

(defun efrit-permissions--after-edit ()
  "Redraw the editor after a change made from a menu."
  (when-let* ((buf (get-buffer efrit-permissions--buffer)))
    (with-current-buffer buf (efrit-permissions-refresh))))

(defun efrit-permissions--menu-heading ()
  (let* ((id efrit-permissions--row)
         (root (plist-get id :root))
         (where (if root (abbreviate-file-name (directory-file-name root)) "global")))
    (pcase (plist-get id :kind)
      ('grant (let ((g (plist-get id :grant)))
                (format "%s %s (%s) in %s\n" (plist-get g :cap)
                        (efrit-permissions--grant-target-string g)
                        (plist-get g :scope) where)))
      ('default (format "Default grants for %s: %s\n" where
                        (mapconcat #'symbol-name (efrit-sandbox-effective-default-grants root) " ")))
      ('review (format "Review for %s: %s, classes %s\n" where
                       (if (efrit-review-enabled-p root) "on" "off")
                       (mapconcat #'symbol-name (efrit-review-effective-classes root) " ")))
      ('limit (format "%s for %s\n" (plist-get id :name) where))
      (_ (format "%s\n" (plist-get id :var))))))

;; grant actions

(defun efrit-permissions--regrant (cap target scope)
  "Replace the row's grant with CAP TARGET SCOPE in the same project."
  (let* ((root (efrit-permissions--row-root)) (g (efrit-permissions--row-grant)))
    (efrit-sandbox-revoke (plist-get g :cap) (plist-get g :target) root)
    (efrit-sandbox-grant cap target scope root)
    (efrit-permissions--after-edit)))

(defun efrit-permissions-grant-to-project ()
  "Make the grant at point a saved project grant."
  (interactive)
  (let ((g (efrit-permissions--row-grant)))
    (efrit-permissions--regrant (plist-get g :cap) (plist-get g :target) 'project)))

(defun efrit-permissions-grant-to-session ()
  "Make the grant at point a session grant (removed from the project file)."
  (interactive)
  (let ((g (efrit-permissions--row-grant)))
    (efrit-permissions--regrant (plist-get g :cap) (plist-get g :target) 'session)))

(defun efrit-permissions-grant-revoke ()
  "Revoke the grant at point."
  (interactive)
  (efrit-permissions--revoke-id efrit-permissions--row)
  (efrit-permissions--after-edit))

(defun efrit-permissions-grant-widen ()
  "Widen the grant's target one level: the parent directory, or any command."
  (interactive)
  (let* ((g (efrit-permissions--row-grant)) (cap (plist-get g :cap)) (tg (plist-get g :target)))
    (cond
     ((and (stringp tg) (not (string= tg "/")))
      (efrit-permissions--regrant cap (efrit-sandbox-canonical
                                       (file-name-directory (directory-file-name tg)))
                                  (plist-get g :scope)))
     ((efrit-sandbox-shell-target-p tg)
      (when (yes-or-no-p "Widen to any shell command? ")
        (efrit-permissions--regrant cap t (plist-get g :scope))))
     (t (user-error "Cannot widen this grant")))))

(defun efrit-permissions-grant-narrow ()
  "Narrow the grant's target: a subdirectory, or a command list."
  (interactive)
  (let* ((g (efrit-permissions--row-grant)) (cap (plist-get g :cap)) (tg (plist-get g :target)))
    (cond
     ((stringp tg)
      (efrit-permissions--regrant cap (efrit-sandbox-canonical (read-directory-name "Narrow to: " tg))
                                  (plist-get g :scope)))
     ((eq cap 'shell)
      (let ((names (split-string (read-string "Commands (space-separated): "
                                              (and (efrit-sandbox-shell-target-p tg)
                                                   (mapconcat #'identity (cdr tg) " ")))
                                 " " t)))
        (unless names (user-error "Need at least one command"))
        (efrit-permissions--regrant cap (cons 'shell names) (plist-get g :scope))))
     (t (user-error "Cannot narrow this grant")))))

(defun efrit-permissions-grant-edit-commands ()
  "Edit the command list of a shell grant."
  (interactive)
  (efrit-permissions-grant-narrow))

;; default-grant actions

(defun efrit-permissions--toggle-in-list (cap list)
  (if (memq cap list) (remq cap list) (append list (list cap))))

(defun efrit-permissions-default-toggle (cap)
  "Toggle CAP in the project's default grants (creates the override)."
  (let ((root (efrit-permissions--row-root)))
    (efrit-sandbox-set-project-default-grants
     (efrit-permissions--toggle-in-list cap (efrit-sandbox-effective-default-grants root)) root)
    (efrit-permissions--after-edit)))

(defun efrit-permissions-default-reset ()
  "Drop the project's default-grants override."
  (interactive)
  (efrit-sandbox-set-project-default-grants 'unset (efrit-permissions--row-root))
  (efrit-permissions--after-edit))

(defun efrit-permissions--default-label (cap)
  "Checkbox label for CAP in the default-grants menu."
  (format "%s %s" (if (memq cap (efrit-sandbox-effective-default-grants (efrit-permissions--row-root)))
                      "[x]" "[ ]")
          cap))

;; review actions

(defun efrit-permissions-review-toggle ()
  "Toggle review for the project (creates the override)."
  (interactive)
  (let* ((root (efrit-permissions--row-root))
         (o (efrit-review-project-override root)))
    (efrit-review-set-project-override (not (efrit-review-enabled-p root)) (plist-get o :classes) root)
    (efrit-permissions--after-edit)))

(defun efrit-permissions-review-toggle-class (class)
  (let* ((root (efrit-permissions--row-root))
         (o (efrit-review-project-override root))
         (classes (efrit-permissions--toggle-in-list class (efrit-review-effective-classes root))))
    (efrit-review-set-project-override (plist-get o :enabled) classes root)
    (efrit-permissions--after-edit)))

(defun efrit-permissions-review-reset ()
  "Drop the project's review override."
  (interactive)
  (efrit-review-set-project-override 'unset nil (efrit-permissions--row-root))
  (efrit-permissions--after-edit))

(defun efrit-permissions--review-class-label (class)
  "Checkbox label for CLASS in the review menu."
  (format "%s %s" (if (memq class (efrit-review-effective-classes (efrit-permissions--row-root)))
                      "[x]" "[ ]")
          class))

(defun efrit-permissions--review-toggle-label ()
  (if (efrit-review-enabled-p (efrit-permissions--row-root)) "turn review off here" "turn review on here"))

;; limit actions

(defun efrit-permissions--limit-set (scope)
  (let* ((id efrit-permissions--row) (name (plist-get id :name)) (root (plist-get id :root))
         (var (efrit-limits--variable name))
         (current (efrit-limits-effective name (and var (boundp var) (symbol-value var)) root))
         (n (read-number (format "%s for this %s: " name scope) current)))
    (efrit-limits-set name n scope root)
    (efrit-permissions--after-edit)))

(defun efrit-permissions-limit-set-project ()
  "Set the limit for the project (saved)."
  (interactive) (efrit-permissions--limit-set 'project))

(defun efrit-permissions-limit-set-session ()
  "Set the limit for this Emacs session."
  (interactive) (efrit-permissions--limit-set 'session))

(defun efrit-permissions-limit-reset ()
  "Drop the project and session overrides for the limit."
  (interactive)
  (let* ((id efrit-permissions--row) (name (plist-get id :name)) (root (plist-get id :root)))
    (efrit-limits-set name nil 'project root)
    (efrit-limits-set name nil 'session root)
    (efrit-permissions--after-edit)))

;; global actions

(defun efrit-permissions-global-customize ()
  "Open the variable in Customize."
  (interactive)
  (customize-variable (plist-get efrit-permissions--row :var)))

(defun efrit-permissions-global-toggle ()
  "Toggle a boolean variable, or the sandbox."
  (interactive)
  (let ((var (plist-get efrit-permissions--row :var)))
    (unless (booleanp (symbol-value var)) (user-error "%s is not a boolean; c customizes it" var))
    (set var (not (symbol-value var)))
    (efrit-permissions--after-edit)))

(defun efrit-permissions-global-save ()
  "Save the variable's current value with Customize."
  (interactive)
  (let ((var (plist-get efrit-permissions--row :var)))
    (customize-save-variable var (symbol-value var))
    (message "Saved %s" var)))

;; the menus

(defun efrit-permissions-menu-done ()
  "Close the row menu.  The changes were applied as they were made."
  (interactive)
  (transient-quit-one))

(defconst efrit-permissions--menu-definitions
  '(progn
          (transient-define-prefix efrit-permissions-grant-menu ()
            "Edit a grant."
            [:description efrit-permissions--menu-heading
             ["Scope"
              ("p" "save for the project" efrit-permissions-grant-to-project)
              ("s" "this session only" efrit-permissions-grant-to-session)]
             ["Target"
              ("w" "widen (parent dir / any command)" efrit-permissions-grant-widen)
              ("n" "narrow (subdir / command list)" efrit-permissions-grant-narrow)]
             ["Remove"
              ("d" "revoke" efrit-permissions-grant-revoke)]
             [("RET" "done" efrit-permissions-menu-done)]])
          (transient-define-prefix efrit-permissions-default-menu ()
            "Edit the project's default grants."
            [:description efrit-permissions--menu-heading
             ["Granted on the project root without asking"
              ("r" (lambda () (interactive) (efrit-permissions-default-toggle 'read))
               :description (lambda () (efrit-permissions--default-label 'read)) :transient t)
              ("w" (lambda () (interactive) (efrit-permissions-default-toggle 'write))
               :description (lambda () (efrit-permissions--default-label 'write)) :transient t)
              ("e" (lambda () (interactive) (efrit-permissions-default-toggle 'elisp))
               :description (lambda () (efrit-permissions--default-label 'elisp)) :transient t)
              ("s" (lambda () (interactive) (efrit-permissions-default-toggle 'shell))
               :description (lambda () (efrit-permissions--default-label 'shell)) :transient t)
              ("n" (lambda () (interactive) (efrit-permissions-default-toggle 'net))
               :description (lambda () (efrit-permissions--default-label 'net)) :transient t)
              ("b" (lambda () (interactive) (efrit-permissions-default-toggle 'buffer))
               :description (lambda () (efrit-permissions--default-label 'buffer)) :transient t)]
             ["Override"
              ("x" "use the global default again" efrit-permissions-default-reset)]
             [("RET" "done" efrit-permissions-menu-done)]])
          (transient-define-prefix efrit-permissions-review-menu ()
            "Edit the project's review policy."
            [:description efrit-permissions--menu-heading
             ["Review"
              ("t" efrit-permissions-review-toggle :description efrit-permissions--review-toggle-label
               :transient t)]
             ["Classes reviewed"
              ("w" (lambda () (interactive) (efrit-permissions-review-toggle-class 'write))
               :description (lambda () (efrit-permissions--review-class-label 'write)) :transient t)
              ("e" (lambda () (interactive) (efrit-permissions-review-toggle-class 'exec))
               :description (lambda () (efrit-permissions--review-class-label 'exec)) :transient t)
              ("n" (lambda () (interactive) (efrit-permissions-review-toggle-class 'net))
               :description (lambda () (efrit-permissions--review-class-label 'net)) :transient t)
              ("r" (lambda () (interactive) (efrit-permissions-review-toggle-class 'read))
               :description (lambda () (efrit-permissions--review-class-label 'read)) :transient t)]
             ["Override"
              ("x" "use the global settings again" efrit-permissions-review-reset)]
             [("RET" "done" efrit-permissions-menu-done)]])
          (transient-define-prefix efrit-permissions-limit-menu ()
            "Edit a loop limit."
            [:description efrit-permissions--menu-heading
             ["Set"
              ("p" "for the project (saved)" efrit-permissions-limit-set-project)
              ("s" "for this session" efrit-permissions-limit-set-session)]
             ["Override"
              ("x" "use the global default again" efrit-permissions-limit-reset)]
             [("RET" "done" efrit-permissions-menu-done)]])
          (transient-define-prefix efrit-permissions-global-menu ()
            "Edit a global variable."
            [:description efrit-permissions--menu-heading
             ["Value"
              ("t" "toggle (booleans)" efrit-permissions-global-toggle)
              ("c" "customize" efrit-permissions-global-customize)
              ("S" "save current value" efrit-permissions-global-save)]
             [("RET" "done" efrit-permissions-menu-done)]]))
  "The per-row transient prefixes, kept as data so a reload redefines
them (a definition guarded by `fboundp' survived `efrit-reload' with
stale keys).  Evaluated at load when transient is available.")

(defun efrit-permissions--define-menus ()
  "Define the per-row transient prefixes.  Return non-nil when transient is available."
  (when (require 'transient nil t)
    (eval efrit-permissions--menu-definitions t)
    t))

(when (require 'transient nil t)
  (eval efrit-permissions--menu-definitions t))

(defun efrit-permissions-edit ()
  "Open the edit menu for the row at point."
  (interactive)
  (let* ((id (efrit-permissions--id))
         (kind (plist-get id :kind)))
    (cond
     ;; a project heading: bring that project into focus
     ((and (eq kind 'project) (plist-get id :root))
      (setq efrit-permissions--focus (plist-get id :root))
      (setq header-line-format (efrit-permissions--header))
      (efrit-permissions-refresh))
     ((eq kind 'project)
      (user-error "RET on a row below edits it"))
     (t
      (setq efrit-permissions--row id)
      (unless (efrit-permissions--define-menus)
        (user-error "Editing needs the `transient' package"))
      (call-interactively
       (pcase kind
         ('grant #'efrit-permissions-grant-menu)
         ('default #'efrit-permissions-default-menu)
         ('review #'efrit-permissions-review-menu)
         ('limit #'efrit-permissions-limit-menu)
         ('global #'efrit-permissions-global-menu)))))))

(provide 'efrit-permissions-ui)

;;; efrit-permissions-ui.el ends here
