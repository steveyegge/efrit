;;; efrit-presets.el --- Named bundles of efrit settings -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, ai, convenience

;;; Commentary:

;; A preset is a name and a plist of settings: the model, whether the
;; reviewer runs, the sandbox's default project grants, verbosity, the
;; tool display mode.  `efrit-preset-apply' sets only the keys the
;; preset carries, so a preset can change one thing.  Two ship:
;; `careful' (review on, reads only, normal verbosity) and `fast'
;; (review off, read and write granted, minimal rows).  Users add
;; their own to `efrit-presets'.  Menu: `P'; input: `/preset NAME'.
;; After copilot-chat-apply-preset (2026-09-28).

;;; Code:

(require 'cl-lib)
(require 'efrit-config)
(require 'efrit-review)
(require 'efrit-sandbox)

(defgroup efrit-presets nil
  "Named bundles of settings."
  :group 'efrit)

(defcustom efrit-presets
  '((careful :review t :grants (read) :verbosity normal :display-mode smart)
    (fast :review nil :grants (read write) :verbosity minimal :display-mode minimal))
  "Presets: (NAME . PLIST).  Keys: :model (string), :review (boolean),
:grants (list of capabilities for `efrit-sandbox-default-project-grants'),
:verbosity, :display-mode (symbols for the agent buffer).  Only the
keys present are applied."
  :type '(alist :key-type symbol
                :value-type (plist :key-type (choice (const :model) (const :review) (const :grants)
                                                     (const :verbosity) (const :display-mode))
                                   :value-type sexp))
  :group 'efrit-presets)

(defvar efrit-preset-current nil
  "The preset applied last, or nil.")

(defvar efrit-agent-verbosity)
(defvar efrit-agent-display-mode)

(defun efrit-preset-apply (name)
  "Apply preset NAME: set each setting it carries.  Returns what changed."
  (interactive (list (intern (completing-read "Preset: " (mapcar (lambda (p) (symbol-name (car p))) efrit-presets) nil t))))
  (let* ((plist (or (alist-get name efrit-presets) (user-error "No preset %s" name)))
         (changed nil))
    (cl-loop for (key value) on plist by #'cddr do
             (pcase key
               (:model (setq efrit-default-model value) (push (format "model %s" value) changed))
               (:review (setq efrit-review-enabled value) (push (format "review %s" (if value "on" "off")) changed))
               (:grants (setq efrit-sandbox-default-project-grants value)
                        (push (format "default grants %s" (mapconcat #'symbol-name value " ")) changed))
               (:verbosity (setq efrit-agent-verbosity value) (push (format "verbosity %s" value) changed))
               (:display-mode (setq efrit-agent-display-mode value) (push (format "rows %s" value) changed))
               (_ (push (format "unknown key %s ignored" key) changed))))
    (setq efrit-preset-current name)
    (when (fboundp 'efrit-agent--refresh-status-line)
      (dolist (b (buffer-list))
        (with-current-buffer b
          (when (derived-mode-p 'efrit-agent-mode) (efrit-agent--refresh-status-line)))))
    (message "efrit preset %s: %s" name (mapconcat #'identity (nreverse changed) ", "))
    changed))

(declare-function efrit-agent--refresh-status-line "efrit-agent")

(provide 'efrit-presets)

;;; efrit-presets.el ends here
