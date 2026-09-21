;;; efrit-limits.el --- Per-project loop limits, raised on request -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; The iteration cap (`efrit-repl-loop-max-iterations',
;; `efrit-do-async-max-iterations') exists to stop a runaway loop.  A
;; long refactor is not a runaway loop, but it hits the same cap, and
;; until now the turn ended with "iteration limit" and the work was
;; lost mid-flight.
;;
;; Now, when a session reaches its cap, the loop asks:
;;
;;   continue this turn (N more calls, once)
;;   raise the limit for this Emacs session
;;   raise it for this project (saved)
;;   stop here
;;
;; Session and project overrides are kept here, per project root, and
;; the loop reads the *effective* limit through `efrit-limits-effective'
;; so both loops and any future cap share one mechanism.  Project
;; overrides persist as JSON in <project>/.efrit/settings.json,
;; beside the sandbox's grants and with the same reasoning: data, not
;; code, validated on load, written 0600.
;;
;; The prompt is a transient menu when one can be shown (same
;; recursive-edit pattern as efrit-sandbox-ui), an echo-area choice
;; otherwise; with no way to ask (batch, or `efrit-limits-ask' nil)
;; the answer is "stop", which is the old behaviour.

;;; Code:

(require 'cl-lib)
(require 'efrit-log)
(require 'efrit-settings)
(require 'efrit-events)

(defvar transient-post-exit-hook)
(declare-function efrit-limits-menu "efrit-limits")
(declare-function efrit-show-preview "efrit-ui-helpers")

(defgroup efrit-limits nil
  "Loop limits and how they are raised."
  :group 'efrit
  :prefix "efrit-limits-")

(defcustom efrit-limits-ask t
  "When non-nil, hitting an iteration cap asks whether to continue.
nil restores the old behaviour: the turn ends at the cap."
  :type 'boolean
  :group 'efrit-limits)

(defcustom efrit-limits-continue-step 50
  "How many more API calls \"continue once\" allows before asking again."
  :type 'integer
  :group 'efrit-limits)

(defconst efrit-limits-section "limits"
  "The section of the project settings file this module owns.")

(defconst efrit-limits-known
  '(max-iterations max-tool-calls)
  "Limit names this module manages.  A name maps to an integer.
`max-iterations' is API calls per turn (the loop engine);
`max-tool-calls' is tool calls per turn (the circuit breaker).")

;;; State
;;
;; Overrides are keyed by (root . name): root -> alist of (name . value).

(defvar efrit-limits--session (make-hash-table :test 'equal)
  "Project root -> alist of session overrides.")

(defvar efrit-limits--once (make-hash-table :test 'equal)
  "(root . name) -> one-shot raised value that applies until the next reset.
A \"continue once\" answer sets it; `efrit-limits-reset-once' clears it
at the end of the turn.")

(defun efrit-limits-project-root ()
  "The project root overrides are keyed on."
  (efrit-settings-project-root))

;;; Persistence (the \"limits\" section of the project settings file)

(defun efrit-limits-file (root)
  "Path of ROOT's settings file."
  (efrit-settings-file root))

(defun efrit-limits-project-overrides (root)
  "The valid project overrides for ROOT as an alist (NAME . INTEGER).
Unknown names and non-integer values in the file are ignored."
  (let ((limits (efrit-settings-get root efrit-limits-section)) (out nil))
    (when (hash-table-p limits)
      (maphash (lambda (k v)
                 (let ((name (and (stringp k) (intern k))))
                   (when (and (memq name efrit-limits-known) (integerp v) (>= v 0))
                     (push (cons name v) out))))
               limits))
    out))

(defun efrit-limits--save-project (root overrides)
  "Write OVERRIDES (alist) as ROOT's limits section; an empty alist removes it."
  (efrit-settings-put root efrit-limits-section
                      (when overrides
                        (let ((h (make-hash-table :test 'equal)))
                          (dolist (c overrides) (puthash (symbol-name (car c)) (cdr c) h))
                          h))))

;;; Lookup and set

(defun efrit-limits-effective (name default &optional root)
  "The limit NAME in force for ROOT: once > session > project > DEFAULT.
DEFAULT is the customization value the caller would otherwise use."
  (let ((root (or root (efrit-limits-project-root))))
    (or (gethash (cons root name) efrit-limits--once)
        (alist-get name (gethash root efrit-limits--session))
        (alist-get name (efrit-limits-project-overrides root))
        default)))

(defun efrit-limits-set (name value scope &optional root)
  "Set limit NAME to VALUE at SCOPE (`once', `session', `project') for ROOT.
A VALUE of nil at `session' or `project' scope removes that override."
  (let ((root (or root (efrit-limits-project-root))))
    (pcase scope
      ('once (puthash (cons root name) value efrit-limits--once))
      ('session
       (if value
           (setf (alist-get name (gethash root efrit-limits--session)) value)
         (setf (alist-get name (gethash root efrit-limits--session) nil t) nil)))
      ('project
       (let ((overrides (efrit-limits-project-overrides root)))
         (if value
             (setf (alist-get name overrides) value)
           (setf (alist-get name overrides nil t) nil))
         (efrit-limits--save-project root overrides)))
      (_ (error "Unknown limit scope %S" scope)))
    (efrit-log 'info "limits: %s = %s (%s) for %s" name value scope root)
    value))

(defun efrit-limits-reset-once (&optional root)
  "Drop one-shot raises for ROOT (default all).  Call at the end of a turn."
  (if root
      (maphash (lambda (k _) (when (equal (car k) root) (remhash k efrit-limits--once)))
               efrit-limits--once)
    (clrhash efrit-limits--once)))

(defun efrit-limits-reset-session ()
  "Forget session overrides and one-shot raises for every project."
  (interactive)
  (clrhash efrit-limits--session)
  (clrhash efrit-limits--once))

;;; Asking

(defface efrit-limits-heading
  '((t :inherit warning :weight bold))
  "Face of the limit prompt's first line."
  :group 'efrit-limits)

(defface efrit-limits-number
  '((t :inherit font-lock-constant-face :weight bold))
  "Face of the numbers in the limit prompt."
  :group 'efrit-limits)

(defface efrit-limits-dim
  '((t :inherit shadow))
  "Face of explanatory text in the limit prompt."
  :group 'efrit-limits)

(defvar efrit-limits--answer 'pending)
(defvar efrit-limits--depth nil)
(defvar efrit-limits--context nil
  "Plist (:name :current :step :root) for the open prompt.
:step is adjustable from the menu with + and -.")

(defun efrit-limits--choose (answer)
  (setq efrit-limits--answer answer))

(defun efrit-limits--exit-recursive-edit ()
  (when (and efrit-limits--depth (= (recursion-depth) efrit-limits--depth))
    (exit-recursive-edit)))

(defun efrit-limits--unit (name)
  "What limit NAME counts, for the prompt."
  (pcase name
    ('max-iterations "API calls")
    ('max-tool-calls "tool calls")
    (_ (symbol-name name))))

(defun efrit-limits--variable (name)
  "The customization variable behind limit NAME, for the prompt."
  (pcase name
    ('max-iterations 'efrit-repl-loop-max-iterations)
    ('max-tool-calls 'efrit-do-max-tool-calls-per-session)
    (_ nil)))

(defun efrit-limits--why (name)
  "One sentence on what limit NAME protects against."
  (pcase name
    ('max-iterations "It stops a turn that keeps calling the model without finishing.")
    ('max-tool-calls "It stops a turn that keeps running tools without finishing.")
    (_ "It stops a runaway turn.")))

(defun efrit-limits--num (n)
  (propertize (format "%d" n) 'face 'efrit-limits-number))

(defun efrit-limits--step () (plist-get efrit-limits--context :step))
(defun efrit-limits--current () (plist-get efrit-limits--context :current))
(defun efrit-limits--name () (plist-get efrit-limits--context :name))

(defun efrit-limits--raised ()
  "The new limit a session/project raise sets: current plus the step, rounded up to 50."
  (let ((n (+ (efrit-limits--current) (efrit-limits--step))))
    (* 50 (ceiling n 50.0))))

(defun efrit-limits--menu-description ()
  "Heading: what happened, why the limit exists, where it is set."
  (let* ((c efrit-limits--context)
         (name (plist-get c :name))
         (var (efrit-limits--variable name))
         (root (abbreviate-file-name (directory-file-name (plist-get c :root)))))
    (concat
     (propertize (format "Efrit reached %s %s this turn — the limit for %s"
                         (efrit-limits--num (plist-get c :current))
                         (efrit-limits--unit name) root)
                 'face 'efrit-limits-heading)
     "\n  "
     (propertize (efrit-limits--why name) 'face 'efrit-limits-dim)
     (when var
       (concat "  " (propertize (format "(%s)" var) 'face 'efrit-limits-dim)))
     (when efrit-limits--details-shown
       (concat "\n\n"
               (mapconcat (lambda (l) (concat "  " l))
                          (split-string (efrit-limits--details-text) "\n") "\n")))
     "\n")))

(defvar efrit-limits--details-shown nil
  "Non-nil while the menu shows the limits table; toggled by ?.")

(defun efrit-limits--label-continue ()
  (format "continue for %s more %s, then ask again"
          (efrit-limits--num (efrit-limits--step)) (efrit-limits--unit (efrit-limits--name))))
(defun efrit-limits--label-session ()
  (format "raise to %s until Emacs exits" (efrit-limits--num (efrit-limits--raised))))
(defun efrit-limits--label-project ()
  (format "raise to %s for this project %s"
          (efrit-limits--num (efrit-limits--raised))
          (propertize (format "(saved in %s)"
                              (abbreviate-file-name
                               (efrit-limits-file (plist-get efrit-limits--context :root))))
                      'face 'efrit-limits-dim)))
(defun efrit-limits--label-step ()
  (format "step: %s" (efrit-limits--num (efrit-limits--step))))

(defun efrit-limits--adjust-step (delta)
  "Change the step by DELTA (a count) inside the open menu."
  (let ((new (max 10 (+ (efrit-limits--step) delta))))
    (setq efrit-limits--context (plist-put efrit-limits--context :step new))))

(defun efrit-limits--details-text ()
  "The limits in force for this project and where they come from, as text."
  (let* ((root (plist-get efrit-limits--context :root))
         (file (efrit-limits-file root))
         (project (efrit-limits-project-overrides root)))
    (concat
     (format "Project:  %s\nSettings: %s%s\n\n" (abbreviate-file-name root)
             (abbreviate-file-name file)
             (if (file-exists-p file) "" "  (not written yet)"))
     (mapconcat
      (lambda (name)
        (let ((var (efrit-limits--variable name)))
          (format "%-16s default %-5s session %-5s project %-5s once %s"
                  name
                  (if (and var (boundp var)) (symbol-value var) "-")
                  (or (alist-get name (gethash root efrit-limits--session)) "-")
                  (or (alist-get name project) "-")
                  (or (gethash (cons root name) efrit-limits--once) "-"))))
      efrit-limits-known "\n")
     "\n\nA raise never lowers a limit; M-x efrit-limits-reset-session forgets session raises.")))

(defun efrit-limits--toggle-label ()
  (if efrit-limits--details-shown "hide limits in force" "show limits in force"))

(defun efrit-limits-toggle-details ()
  "Show the limits table in the menu, or hide it again."
  (interactive)
  (setq efrit-limits--details-shown (not efrit-limits--details-shown)))

(defun efrit-limits-yank-details ()
  "Copy the limits table to the kill ring."
  (interactive)
  (kill-new (efrit-limits--details-text))
  (message "Limits copied to the kill ring"))

(defun efrit-limits--show-details ()
  "Open the limits table in a popup buffer (`q' closes it)."
  (interactive)
  (require 'efrit-ui-helpers)
  (efrit-show-preview "*efrit-limits*" (efrit-limits--details-text)))

(defun efrit-limits--define-menu ()
  (when (require 'transient nil t)
    (unless (fboundp 'efrit-limits-menu)
      (eval
       '(transient-define-prefix efrit-limits-menu ()
          "Continue past a per-turn limit?"
          [:description efrit-limits--menu-description
           ["Continue"
            ("c" (lambda () (interactive) (efrit-limits--choose 'once))
             :description efrit-limits--label-continue)
            ("s" (lambda () (interactive) (efrit-limits--choose 'session))
             :description efrit-limits--label-session)
            ("p" (lambda () (interactive) (efrit-limits--choose 'project))
             :description efrit-limits--label-project)]
           ["Stop"
            ("n" "stop here; the conversation stays open, nothing is lost"
             (lambda () (interactive) (efrit-limits--choose nil)))]
           ["Adjust"
            ("+" (lambda () (interactive) (efrit-limits--adjust-step 50))
             :description efrit-limits--label-step :transient t)
            ("-" "smaller step" (lambda () (interactive) (efrit-limits--adjust-step -50))
             :transient t)
            ("?" efrit-limits-toggle-details :description efrit-limits--toggle-label :transient t)
            ("y" "yank limits to the kill ring" efrit-limits-yank-details :transient t)
            ("b" "open limits in a buffer" efrit-limits--show-details :transient t)]])
       t))
    (fboundp 'efrit-limits-menu)))

(defun efrit-limits--hide-details ()
  (when-let* ((buf (get-buffer "*efrit-limits*"))
              (win (get-buffer-window buf t)))
    (ignore-errors (quit-window nil win))))

(defun efrit-limits--ask-with-menu ()
  (setq efrit-limits--answer 'pending
        efrit-limits--details-shown nil)
  (let ((efrit-limits--depth (1+ (recursion-depth))))
    (unwind-protect
        (progn
          (add-hook 'transient-post-exit-hook #'efrit-limits--exit-recursive-edit)
          (run-at-time 0 nil (lambda () (call-interactively #'efrit-limits-menu)))
          (condition-case nil (recursive-edit) (quit nil)))
      (remove-hook 'transient-post-exit-hook #'efrit-limits--exit-recursive-edit)
      (efrit-limits--hide-details)))
  (if (eq efrit-limits--answer 'pending) nil efrit-limits--answer))

(defun efrit-limits--ask-in-echo-area ()
  "Fallback when no menu can be shown (terminal without transient)."
  (pcase (read-char-choice
          (format "%s reached %d %s.  [c]ontinue %d more  [s]ession %d  [p]roject %d  [n]o "
                  "Efrit" (efrit-limits--current) (efrit-limits--unit (efrit-limits--name))
                  (efrit-limits--step) (efrit-limits--raised) (efrit-limits--raised))
          '(?c ?s ?p ?n))
    (?c 'once) (?s 'session) (?p 'project) (_ nil)))

(defun efrit-limits--note (text face)
  "Publish TEXT in FACE as a transcript note (the agent buffer shows it)."
  (efrit-publish 'note `((:text . ,(concat "⏱ " text)) (:face . ,face) (:kind . limits))))

(defun efrit-limits-ask-to-raise (name current &optional root)
  "Ask whether to go past limit NAME, currently CURRENT, for ROOT.
Applies the chosen raise and returns the new effective limit, or nil
when the user stops (or nothing can ask).  Never signals."
  (let* ((root (or root (efrit-limits-project-root)))
         (efrit-limits--context (list :name name :current current
                                      :step efrit-limits-continue-step :root root))
         (answer (and efrit-limits-ask
                      (not noninteractive)
                      ;; The circuit-breaker check runs inside the
                      ;; tool's with-timeout; reading time is not tool time
                      (let ((suspended (with-timeout-suspend)))
                        (unwind-protect
                            (condition-case err
                                (if (efrit-limits--define-menu)
                                    (efrit-limits--ask-with-menu)
                                  (efrit-limits--ask-in-echo-area))
                              (quit nil)
                              (error (efrit-log 'warn "limits prompt: %s" (error-message-string err)) nil))
                          (with-timeout-unsuspend suspended)))))
         (unit (efrit-limits--unit name))
         (result
          (pcase answer
            ('once (efrit-limits-set name (+ current (efrit-limits--step)) 'once root))
            ((or 'session 'project) (efrit-limits-set name (efrit-limits--raised) answer root))
            (_ nil))))
    (when (and efrit-limits-ask (not noninteractive))
      (efrit-limits--note
       (pcase answer
         ('once (format "limit reached at %d %s; continuing for %d more" current unit (efrit-limits--step)))
         ('session (format "limit reached at %d %s; raised to %d for this session" current unit result))
         ('project (format "limit reached at %d %s; raised to %d for this project (saved)" current unit result))
         (_ (format "limit reached at %d %s; stopped" current unit)))
       (if answer 'efrit-limits-heading 'efrit-limits-dim)))
    result))

(provide 'efrit-limits)

;;; efrit-limits.el ends here
