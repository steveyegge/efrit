;;; test-do-prompt.el --- Tests for efrit-do-prompt -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'efrit-do-prompt)
(require 'efrit-do)   ; provides the context/todo helpers the prompt calls

(ert-deftest test-do-prompt-hook-appends-contributions ()
  "Non-nil strings from `efrit-system-prompt-functions' land in the prompt, in order."
  (let* ((efrit-system-prompt-functions
          (list (lambda (id) (format "ZZ-FIRST-MARKER for %s" (or id "none")))
                (lambda (_id) nil)
                (lambda (_id) "")
                (lambda (_id) "ZZ-SECOND-MARKER")))
         (prompt (efrit-do--command-system-prompt nil nil nil "sess-42" nil)))
    (should (string-match-p "ADDITIONAL INSTRUCTIONS:" prompt))
    (should (string-match-p "ZZ-FIRST-MARKER for sess-42" prompt))
    (should (< (string-match "ZZ-FIRST-MARKER for sess-42" prompt)
               (string-match "ZZ-SECOND-MARKER" prompt)))
    ;; Sits after AGENTS.md text and before the closing reminder
    (should (< (string-match "ZZ-SECOND-MARKER" prompt)
               (string-match "Remember: Generate safe" prompt)))))

(ert-deftest test-do-prompt-hook-absent-when-empty ()
  (let ((efrit-system-prompt-functions nil))
    (should-not (string-match-p "ADDITIONAL INSTRUCTIONS:"
                                (efrit-do--command-system-prompt)))))

(ert-deftest test-do-prompt-hook-error-is-dropped ()
  "A signalling hook function is skipped; the rest still contribute."
  (let* ((efrit-system-prompt-functions
          (list (lambda (_id) (error "boom"))
                (lambda (_id) "SURVIVOR")))
         (prompt (efrit-do--command-system-prompt)))
    (should (string-match-p "SURVIVOR" prompt))
    (should-not (string-match-p "boom" prompt))))

(ert-deftest test-do-prompt-remote-root-guidance ()
  "The REMOTE PROJECT paragraph appears only for a Tramp project root."
  (let ((efrit-project-root nil))
    (cl-letf (((symbol-function 'efrit-tool--get-project-root)
               (lambda () "/ssh:host:/srv/proj/")))
      (let ((text (efrit-do--remote-root-guidance)))
        (should (string-match-p "REMOTE PROJECT" text))
        (should (string-match-p "/ssh:host:" text))
        (should (string-match-p "process-file" text))))
    (cl-letf (((symbol-function 'efrit-tool--get-project-root)
               (lambda () "/srv/proj/")))
      (should (string= (efrit-do--remote-root-guidance) "")))))

(ert-deftest test-do-prompt-emacs-first-guidance ()
  "The prompt teaches Emacs-first: discovery tool, the built-in map, shell last."
  (let ((text (efrit-do--command-common-tasks)))
    (should (string-match-p "EMACS FIRST" text))
    (should (string-match-p "emacs_apropos" text))
    (dolist (sym '("recentf-list" "dired" "vc-diff" "process-file" "find-file"
                   "directory-files-recursively" "Tramp"))
      (should (string-match-p (regexp-quote sym) text)))
    ;; shell is the last resort, stated as such
    (should (string-match-p "3\\. shell_exec" text)))
  (let ((examples (efrit-do--command-examples)))
    ;; the first example is recentf, not a shell command
    (should (< (string-match "recentf-list" examples)
               (or (string-match "shell_exec" examples) most-positive-fixnum)))
    (should (string-match-p "vcs_status" examples))
    (should (string-match-p "/ssh:build:" examples))
    ;; the embedded regexp survives the string escaping intact
    ;; in the prompt text (a JSON-ish string) the Lisp regexp appears as \\\\.jpeg\\\\'
    (should (string-search "\\\\\\\\.jpeg\\\\\\\\'" examples))))

(ert-deftest test-do-prompt-tool-selection-mentions-discovery ()
  (let ((efrit-project-root nil))
    (cl-letf (((symbol-function 'efrit-tool--get-project-root) (lambda () "/srv/p/")))
      (let ((text (efrit-do--command-system-prompt)))
        (should (string-match-p "- emacs_apropos:" text))
        (should (string-match-p "runs on the LOCAL machine" text))))))

(provide 'test-do-prompt)
;;; test-do-prompt.el ends here
