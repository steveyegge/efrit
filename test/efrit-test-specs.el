;;; efrit-test-specs.el --- Automated test specs for Efrit -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, testing

;;; Commentary:

;; Automated test specifications converted from the manual testing plan.
;; These tests exercise efrit-do-async through efrit-chat.
;;
;; Test tiers:
;; - Tier 1: Single-tool smoke tests
;; - Tier 2: Error recovery tests
;; - Tier 3: Multi-step workflow tests
;; - Tier 4: Emacs-specific capability tests
;; - Tier 5: Agent-level challenge tests (TODO)
;; - Tier 6: Stress tests and edge cases (TODO)
;;
;; Usage:
;;   (require 'efrit-test-specs)
;;   (efrit-test-register-tier1)  ; Register tier 1 tests
;;   (efrit-test-run-tier 1)      ; Run tier 1 tests
;;
;; Or register all:
;;   (efrit-test-register-all-tiers)
;;   (efrit-test-run-all)

;;; Code:

(require 'efrit-test-runner)

;;; ============================================================
;;; Tier 1: Single-Tool Smoke Tests
;;; ============================================================

(defun efrit-test-register-tier1 ()
  "Register Tier 1 smoke tests."
  (interactive)

  ;; 1.1 eval_sexp basics
  (efrit-test-register
   (efrit-test-spec
    :id "t1-1.1-arithmetic"
    :name "Simple arithmetic: 2 + 2"
    :tier 1
    :category 'eval-sexp
    :prompt "What is 2 + 2?"
    :validators '((response-contains "4")
                  (no-error))
    :timeout 60))

  (efrit-test-register
   (efrit-test-spec
    :id "t1-1.1-datetime"
    :name "Current date and time"
    :tier 1
    :category 'eval-sexp
    :prompt "Show me the current date and time"
    :validators '((response-matches "202[0-9]")
                  (no-error))
    :timeout 60))

  (efrit-test-register
   (efrit-test-spec
    :id "t1-1.1-buffers"
    :name "List recent buffers"
    :tier 1
    :category 'eval-sexp
    :prompt "List my recent buffers"
    :validators '((no-error))
    :timeout 60))

  (efrit-test-register
   (efrit-test-spec
    :id "t1-1.1-cwd"
    :name "Current working directory"
    :tier 1
    :category 'eval-sexp
    :prompt "What's my current working directory?"
    :validators '((response-matches "/")  ; Contains path
                  (no-error))
    :timeout 60))

  (efrit-test-register
   (efrit-test-spec
    :id "t1-1.1-memory"
    :name "Emacs memory info"
    :tier 1
    :category 'eval-sexp
    :prompt "How much free memory does Emacs have?"
    :validators '((no-error))
    :timeout 60))

  ;; 1.2 shell_exec basics
  (efrit-test-register
   (efrit-test-spec
    :id "t1-1.2-ls"
    :name "List home directory"
    :tier 1
    :category 'shell-exec
    :prompt "Run 'ls -la' in my home directory"
    :validators '((no-error))
    :timeout 60))

  (efrit-test-register
   (efrit-test-spec
    :id "t1-1.2-git-status"
    :name "Git status"
    :tier 1
    :category 'shell-exec
    :prompt "Show me my git status"
    :validators '((no-error))
    :timeout 60))

  (efrit-test-register
   (efrit-test-spec
    :id "t1-1.2-env"
    :name "Shell environment"
    :tier 1
    :category 'shell-exec
    :prompt "What's my current shell environment?"
    :validators '((response-matches "\\(SHELL\\|PATH\\|HOME\\)")
                  (no-error))
    :timeout 60))

  (efrit-test-register
   (efrit-test-spec
    :id "t1-1.2-disk"
    :name "Disk space"
    :tier 1
    :category 'shell-exec
    :prompt "How much disk space do I have?"
    :validators '((no-error))
    :timeout 60))

  ;; 1.3 Buffer operations
  (efrit-test-register
   (efrit-test-spec
    :id "t1-1.3-create-buffer"
    :name "Create scratch buffer"
    :tier 1
    :category 'buffer-ops
    :prompt "Create a scratch buffer called *my-notes*"
    :setup (lambda () (when (get-buffer "*my-notes*")
                       (kill-buffer "*my-notes*")))
    :teardown (lambda () (when (get-buffer "*my-notes*")
                          (kill-buffer "*my-notes*")))
    :validators '((buffer-contains "*my-notes*" "")  ; Buffer exists
                  (no-error))
    :timeout 60))

  (efrit-test-register
   (efrit-test-spec
    :id "t1-1.3-messages"
    :name "Show Messages buffer"
    :tier 1
    :category 'buffer-ops
    :prompt "Show me the contents of *Messages*"
    :validators '((no-error))
    :timeout 60))

  (efrit-test-register
   (efrit-test-spec
    :id "t1-1.3-init"
    :name "Switch to init.el"
    :tier 1
    :category 'buffer-ops
    :prompt "Switch to my init.el"
    :validators '((no-error))
    :timeout 90))

  ;; 1.4 Information gathering
  (efrit-test-register
   (efrit-test-spec
    :id "t1-1.4-glob-el"
    :name "Find .el files"
    :tier 1
    :category 'info-gather
    :prompt "Find all .el files in ~/.emacs.d"
    :validators '((no-error))
    :timeout 90))

  (efrit-test-register
   (efrit-test-spec
    :id "t1-1.4-features"
    :name "Loaded packages"
    :tier 1
    :category 'info-gather
    :prompt "What packages are currently loaded?"
    :validators '((no-error))
    :timeout 60))

  (efrit-test-register
   (efrit-test-spec
    :id "t1-1.4-recentf"
    :name "Recent files"
    :tier 1
    :category 'info-gather
    :prompt "List recently opened files"
    :validators '((no-error))
    :timeout 60))

  (message "Registered %d Tier 1 tests" 15))

;;; ============================================================
;;; Tier 2: Error Recovery Tests
;;; ============================================================

(defun efrit-test-register-tier2 ()
  "Register Tier 2 error recovery tests."
  (interactive)

  ;; 2.1 Recoverable errors
  (efrit-test-register
   (efrit-test-spec
    :id "t2-2.1-nonexistent-file"
    :name "Nonexistent file handling"
    :tier 2
    :category 'error-recovery
    :prompt "Open /nonexistent/path/file.txt and tell me if it exists"
    :validators '((response-matches "\\(not exist\\|doesn't exist\\|does not exist\\|no such\\)")
                  (no-error))  ; Should handle gracefully, not error out
    :timeout 60))

  (efrit-test-register
   (efrit-test-spec
    :id "t2-2.1-void-function"
    :name "Void function handling"
    :tier 2
    :category 'error-recovery
    :prompt "Run the function 'this-function-does-not-exist'"
    :validators '((response-matches "\\(void\\|not\\s+defined\\|doesn't exist\\|does not exist\\)"))
    :timeout 60))

  (efrit-test-register
   (efrit-test-spec
    :id "t2-2.1-missing-package"
    :name "Missing package handling"
    :tier 2
    :category 'error-recovery
    :prompt "Load the package 'this-package-does-not-exist'"
    :validators '((response-matches "\\(not found\\|cannot\\|unable\\|not available\\)"))
    :timeout 60))

  ;; 2.2 Ambiguous requests
  (efrit-test-register
   (efrit-test-spec
    :id "t2-2.2-config"
    :name "Ambiguous 'config' request"
    :tier 2
    :category 'ambiguous
    :prompt "Open my config"
    :validators '((no-error))  ; Should make reasonable choice
    :timeout 90))

  (efrit-test-register
   (efrit-test-spec
    :id "t2-2.2-fix-error"
    :name "Ambiguous 'fix error' request"
    :tier 2
    :category 'ambiguous
    :prompt "Fix the error"
    :validators '((no-error))  ; Should ask for context or check
    :timeout 90))

  (efrit-test-register
   (efrit-test-spec
    :id "t2-2.2-definition"
    :name "Ambiguous 'go to definition' request"
    :tier 2
    :category 'ambiguous
    :prompt "Go to the definition"
    :validators '((no-error))  ; Should ask what definition
    :timeout 90))

  ;; 2.3 Partial failures
  (efrit-test-register
   (efrit-test-spec
    :id "t2-2.3-open-txt"
    :name "Open txt files (partial success)"
    :tier 2
    :category 'partial-failure
    :prompt "Open all .txt files in ~/Documents"
    :validators '((no-error))
    :timeout 90))

  (efrit-test-register
   (efrit-test-spec
    :id "t2-2.3-kill-temp"
    :name "Kill temp buffers"
    :tier 2
    :category 'partial-failure
    :prompt "Kill all buffers named *temp*"
    :validators '((no-error))
    :timeout 60))

  (message "Registered %d Tier 2 tests" 8))

;;; ============================================================
;;; Tier 3: Multi-Step Workflow Tests
;;; ============================================================

(defun efrit-test-register-tier3 ()
  "Register Tier 3 multi-step workflow tests."
  (interactive)

  ;; 3.1 Simple multi-step
  (efrit-test-register
   (efrit-test-spec
    :id "t3-3.1-three-buffers"
    :name "Create three numbered buffers"
    :tier 3
    :category 'multi-step
    :prompt "Create three numbered buffers and put 'Hello' in each"
    :setup (lambda ()
            (dolist (n '("*1*" "*2*" "*3*"))
              (when (get-buffer n) (kill-buffer n))))
    :teardown (lambda ()
               (dolist (n '("*1*" "*2*" "*3*"))
                 (when (get-buffer n) (kill-buffer n))))
    :validators '((buffer-contains "*1*" "Hello")
                  (buffer-contains "*2*" "Hello")
                  (buffer-contains "*3*" "Hello"))
    :timeout 120))

  ;; 3.2 Conditional multi-step
  (efrit-test-register
   (efrit-test-spec
    :id "t3-3.2-tmp-files"
    :name "Check for .tmp files conditionally"
    :tier 3
    :category 'conditional
    :prompt "If there are .tmp files in /tmp, list them; otherwise say 'none'"
    :validators '((no-error))
    :timeout 90))

  (efrit-test-register
   (efrit-test-spec
    :id "t3-3.2-magit-check"
    :name "Check magit availability and adapt"
    :tier 3
    :category 'conditional
    :prompt "Check if magit is available; if so show git log; otherwise use shell"
    :validators '((no-error))
    :timeout 90))

  ;; 3.3 Data transformation
  (efrit-test-register
   (efrit-test-spec
    :id "t3-3.3-extract-urls"
    :name "Extract URLs from buffer"
    :tier 3
    :category 'data-transform
    :prompt "Extract all URLs from the *scratch* buffer"
    :setup (lambda ()
            (with-current-buffer (get-buffer-create "*scratch*")
              (erase-buffer)
              (insert "Visit https://example.com for more info.\n")
              (insert "Also check http://test.org/page\n")))
    :validators '((response-matches "\\(example.com\\|test.org\\)")
                  (no-error))
    :timeout 90))

  (message "Registered %d Tier 3 tests" 4))

;;; ============================================================
;;; Tier 4: Emacs-Specific Capability Tests
;;; ============================================================

(defun efrit-test-register-tier4 ()
  "Register Tier 4 Emacs-specific capability tests."
  (interactive)

  ;; 4.1 Org-mode integration
  (efrit-test-register
   (efrit-test-spec
    :id "t4-4.1-org-file"
    :name "Create org file with TODOs"
    :tier 4
    :category 'org-mode
    :prompt "Create an org file with today's date as heading and 3 TODOs"
    :setup (lambda ()
            (let ((f (expand-file-name "efrit-test.org" temporary-file-directory)))
              (when (file-exists-p f) (delete-file f))))
    :teardown (lambda ()
               (let ((f (expand-file-name "efrit-test.org" temporary-file-directory)))
                 (when (file-exists-p f) (delete-file f))
                 (when (get-buffer "efrit-test.org")
                   (kill-buffer "efrit-test.org"))))
    :validators '((no-error))
    :timeout 120))

  ;; 4.2 Buffer manipulation
  (efrit-test-register
   (efrit-test-spec
    :id "t4-4.2-split-window"
    :name "Split window and show scratch"
    :tier 4
    :category 'buffer-manip
    :prompt "Split window horizontally, show *scratch* on right"
    :validators '((no-error))
    :timeout 90))

  ;; 4.3 Version control
  (efrit-test-register
   (efrit-test-spec
    :id "t4-4.3-uncommitted"
    :name "Show uncommitted changes"
    :tier 4
    :category 'version-control
    :prompt "Show uncommitted changes"
    :validators '((no-error))
    :timeout 90))

  ;; 4.5 Dired operations
  (efrit-test-register
   (efrit-test-spec
    :id "t4-4.5-dired"
    :name "Open dired to downloads"
    :tier 4
    :category 'dired
    :prompt "Open dired to downloads folder"
    :validators '((no-error))
    :timeout 90))

  (message "Registered %d Tier 4 tests" 4))

;;; ============================================================
;;; Tier 5: Agent-Level Challenge Tests (Placeholder)
;;; ============================================================

(defun efrit-test-register-tier5 ()
  "Register Tier 5 agent-level challenge tests."
  (interactive)

  ;; 5.1 Code investigation
  (efrit-test-register
   (efrit-test-spec
    :id "t5-5.1-find-defun"
    :name "Find efrit-do-async definition"
    :tier 5
    :category 'code-investigation
    :prompt "Find where efrit-do-async is defined and explain it"
    :validators '((response-matches "\\(efrit-do\\|defun\\|async\\)")
                  (no-error))
    :timeout 180))

  ;; 5.2 Code generation
  ;; Note: Claude executes the code via tools and gives a summary, so we validate
  ;; that the function was actually defined rather than checking response text.
  (efrit-test-register
   (efrit-test-spec
    :id "t5-5.2-word-count"
    :name "Write word count function"
    :tier 5
    :category 'code-generation
    :prompt "Write an elisp function called my-count-words-in-region that counts words in region and define it now"
    :setup (lambda () (fmakunbound 'my-count-words-in-region))
    :teardown (lambda () (when (fboundp 'my-count-words-in-region)
                          (fmakunbound 'my-count-words-in-region)))
    :validators '((function-defined my-count-words-in-region)
                  (no-error))
    :timeout 180))

  (efrit-test-register
   (efrit-test-spec
    :id "t5-5.2-duplicate-line"
    :name "Write duplicate line command"
    :tier 5
    :category 'code-generation
    :prompt "Write an interactive command called my-duplicate-line that duplicates current line and define it now"
    :setup (lambda () (fmakunbound 'my-duplicate-line))
    :teardown (lambda () (when (fboundp 'my-duplicate-line)
                          (fmakunbound 'my-duplicate-line)))
    :validators '((function-defined my-duplicate-line)
                  (no-error))
    :timeout 180))

  (message "Registered %d Tier 5 tests" 3))

;;; ============================================================
;;; Tier 6: Stress Tests and Edge Cases (Placeholder)
;;; ============================================================

(defun efrit-test-register-tier6 ()
  "Register Tier 6 stress tests and edge cases."
  (interactive)

  ;; 6.2 Edge cases
  (efrit-test-register
   (efrit-test-spec
    :id "t6-6.2-unicode"
    :name "Unicode handling"
    :tier 6
    :category 'edge-cases
    :prompt "Handle a buffer with unicode: 你好世界"
    :validators '((no-error))
    :timeout 90))

  (efrit-test-register
   (efrit-test-spec
    :id "t6-6.2-spaces"
    :name "Filename with spaces"
    :tier 6
    :category 'edge-cases
    :prompt "Process filename with spaces: 'my file.txt'"
    :validators '((no-error))
    :timeout 90))

  (efrit-test-register
   (efrit-test-spec
    :id "t6-6.2-readonly"
    :name "Read-only buffer handling"
    :tier 6
    :category 'edge-cases
    :prompt "Work with a read-only buffer"
    :validators '((no-error))
    :timeout 90))

  (message "Registered %d Tier 6 tests" 3))

;;; ============================================================
;;; Registration Utilities
;;; ============================================================

(defun efrit-test-register-all-tiers ()
  "Register all test tiers."
  (interactive)
  (efrit-test-clear-registry)
  (efrit-test-register-tier1)
  (efrit-test-register-tier2)
  (efrit-test-register-tier3)
  (efrit-test-register-tier4)
  (efrit-test-register-tier5)
  (efrit-test-register-tier6)
  (message "Registered all %d tests across 6 tiers"
           (length efrit-test--registry)))

(defun efrit-test-register-passing-tiers ()
  "Register only Tier 1-4 tests (known to pass from manual testing)."
  (interactive)
  (efrit-test-clear-registry)
  (efrit-test-register-tier1)
  (efrit-test-register-tier2)
  (efrit-test-register-tier3)
  (efrit-test-register-tier4)
  (message "Registered %d passing tests (Tiers 1-4)"
           (length efrit-test--registry)))

(provide 'efrit-test-specs)

;;; efrit-test-specs.el ends here
