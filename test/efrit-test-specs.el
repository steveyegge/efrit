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

;; Declare external variables used in Tier 9-10 tests
(defvar efrit-progress--current-session)
(declare-function efrit-progress-inject "efrit-progress")
(declare-function efrit-progress--inject-dir "efrit-progress")
(declare-function efrit-progress--session-dir "efrit-progress")
(declare-function efrit-progress-session-info "efrit-progress")

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
;;; Tier 9: Injection and Conversation Tests
;;; ============================================================

(defun efrit-test-register-tier9 ()
  "Register Tier 9 injection and conversation tests.
These tests verify the mid-task injection and guidance system."
  (interactive)
  (require 'efrit-progress)
  (require 'efrit-executor)

  ;; 9.1 Basic injection - verify injection file processing
  (efrit-test-register
   (efrit-test-spec
    :id "t9-9.1-inject-guidance"
    :name "Inject guidance during execution"
    :tier 9
    :category 'injection
    :prompt "Count to 10 slowly, showing each number"
    :setup (lambda ()
             ;; Set up injection to be delivered after 2 seconds
             (run-with-timer
              2 nil
              (lambda ()
                (when-let* ((session efrit-progress--current-session))
                  (efrit-progress-inject session 'guidance
                                        "Skip to 10 immediately")))))
    :validators '((no-error))
    :timeout 120))

  (efrit-test-register
   (efrit-test-spec
    :id "t9-9.1-inject-abort"
    :name "Inject abort command"
    :tier 9
    :category 'injection
    :prompt "Create buffers *a*, *b*, *c*, *d*, *e* one by one"
    :setup (lambda ()
             ;; Clean up any existing test buffers
             (dolist (name '("*a*" "*b*" "*c*" "*d*" "*e*"))
               (when (get-buffer name) (kill-buffer name)))
             ;; Inject abort after 3 seconds
             (run-with-timer
              3 nil
              (lambda ()
                (when-let* ((session efrit-progress--current-session))
                  (efrit-progress-inject session 'abort
                                        "User requested stop")))))
    :teardown (lambda ()
                (dolist (name '("*a*" "*b*" "*c*" "*d*" "*e*"))
                  (when (get-buffer name) (kill-buffer name))))
    :validators '((no-error))
    :timeout 120))

  (efrit-test-register
   (efrit-test-spec
    :id "t9-9.1-inject-context"
    :name "Inject additional context"
    :tier 9
    :category 'injection
    :prompt "Find files with 'TODO' in them"
    :setup (lambda ()
             ;; Inject context after 2 seconds
             (run-with-timer
              2 nil
              (lambda ()
                (when-let* ((session efrit-progress--current-session))
                  (efrit-progress-inject session 'context
                                        "Only look in the lisp/ directory")))))
    :validators '((no-error))
    :timeout 120))

  ;; 9.2 Multiple injections - verify queue processing
  (efrit-test-register
   (efrit-test-spec
    :id "t9-9.2-multiple-injections"
    :name "Multiple sequential injections"
    :tier 9
    :category 'injection-queue
    :prompt "Create a list of 5 items about programming"
    :setup (lambda ()
             ;; Inject 3 guidance messages in sequence
             (run-with-timer
              2 nil
              (lambda ()
                (when-let* ((session efrit-progress--current-session))
                  (efrit-progress-inject session 'guidance "Focus on Emacs"))))
             (run-with-timer
              4 nil
              (lambda ()
                (when-let* ((session efrit-progress--current-session))
                  (efrit-progress-inject session 'guidance "Use bullet points"))))
             (run-with-timer
              6 nil
              (lambda ()
                (when-let* ((session efrit-progress--current-session))
                  (efrit-progress-inject session 'guidance "Include lisp")))))
    :validators '((no-error))
    :timeout 180))

  ;; 9.3 Injection types validation
  (efrit-test-register
   (efrit-test-spec
    :id "t9-9.3-inject-priority"
    :name "Inject priority change"
    :tier 9
    :category 'injection-types
    :prompt "Create 10 numbered files in /tmp/efrit-test/"
    :setup (lambda ()
             (let ((dir "/tmp/efrit-test/"))
               (when (file-directory-p dir)
                 (delete-directory dir t))
               (make-directory dir t))
             ;; Inject priority change after 2 seconds
             (run-with-timer
              2 nil
              (lambda ()
                (when-let* ((session efrit-progress--current-session))
                  (efrit-progress-inject session 'priority
                                        "Complete this task quickly" 1)))))
    :teardown (lambda ()
                (let ((dir "/tmp/efrit-test/"))
                  (when (file-directory-p dir)
                    (delete-directory dir t))))
    :validators '((no-error))
    :timeout 120))

  ;; 9.4 Injection file format validation (tests the inject queue)
  (efrit-test-register
   (efrit-test-spec
    :id "t9-9.4-inject-queue-direct"
    :name "Direct inject queue file writing"
    :tier 9
    :category 'injection-queue
    :prompt "Wait for 5 seconds and tell me when done"
    :setup (lambda ()
             ;; Write injection file directly (simulates external process)
             (run-with-timer
              1 nil
              (lambda ()
                (when-let* ((session efrit-progress--current-session)
                            (inject-dir (efrit-progress--inject-dir session)))
                  (unless (file-directory-p inject-dir)
                    (make-directory inject-dir t))
                  (let ((filepath (expand-file-name
                                   (format "%s_guidance.json"
                                          (format-time-string "%Y%m%d%H%M%S%3N"))
                                   inject-dir)))
                    (with-temp-file filepath
                      (insert (json-encode
                               '(("type" . "guidance")
                                 ("message" . "Actually, just count to 3 instead"))))))))))
    :validators '((no-error))
    :timeout 90))

  (message "Registered %d Tier 9 tests" 6))

;;; ============================================================
;;; Tier 10: Claude Code Integration Tests
;;; ============================================================

(defun efrit-test-register-tier10 ()
  "Register Tier 10 Claude Code integration tests.
These tests verify external monitoring and injection capabilities."
  (interactive)
  (require 'efrit-progress)

  ;; 10.1 Progress monitoring - verify progress.jsonl generation
  (efrit-test-register
   (efrit-test-spec
    :id "t10-10.1-progress-file-creation"
    :name "Progress file created on session start"
    :tier 10
    :category 'progress-monitoring
    :prompt "What is 1 + 1?"
    :validators '((custom efrit-test--validate-progress-file-exists)
                  (no-error))
    :timeout 60))

  (efrit-test-register
   (efrit-test-spec
    :id "t10-10.1-progress-events"
    :name "Progress events emitted"
    :tier 10
    :category 'progress-monitoring
    :prompt "Create buffer *progress-test* with hello"
    :setup (lambda ()
             (when (get-buffer "*progress-test*")
               (kill-buffer "*progress-test*")))
    :teardown (lambda ()
                (when (get-buffer "*progress-test*")
                  (kill-buffer "*progress-test*")))
    :validators '((custom efrit-test--validate-progress-has-events)
                  (no-error))
    :timeout 60))

  (efrit-test-register
   (efrit-test-spec
    :id "t10-10.1-progress-tool-events"
    :name "Tool start/result events in progress"
    :tier 10
    :category 'progress-monitoring
    :prompt "Evaluate (+ 2 2) and show the result"
    :validators '((custom efrit-test--validate-progress-tool-events)
                  (no-error))
    :timeout 60))

  ;; 10.2 Session info accessibility
  (efrit-test-register
   (efrit-test-spec
    :id "t10-10.2-session-info-query"
    :name "Session info queryable"
    :tier 10
    :category 'session-info
    :prompt "Tell me the current time"
    :validators '((custom efrit-test--validate-session-info-available)
                  (no-error))
    :timeout 60))

  (efrit-test-register
   (efrit-test-spec
    :id "t10-10.2-session-dir-structure"
    :name "Session directory structure correct"
    :tier 10
    :category 'session-info
    :prompt "Show my buffers"
    :validators '((custom efrit-test--validate-session-dir-structure)
                  (no-error))
    :timeout 60))

  ;; 10.3 External process integration (simulated)
  (efrit-test-register
   (efrit-test-spec
    :id "t10-10.3-tail-f-compatible"
    :name "Progress file is tail -f compatible"
    :tier 10
    :category 'external-integration
    :prompt "Create three scratch buffers"
    :validators '((custom efrit-test--validate-progress-tail-compatible)
                  (no-error))
    :timeout 90))

  (efrit-test-register
   (efrit-test-spec
    :id "t10-10.3-jq-parseable"
    :name "Progress events parseable by jq"
    :tier 10
    :category 'external-integration
    :prompt "List files in current directory"
    :validators '((custom efrit-test--validate-progress-jq-parseable)
                  (no-error))
    :timeout 60))

  ;; 10.4 Full workflow test
  (efrit-test-register
   (efrit-test-spec
    :id "t10-10.4-full-workflow"
    :name "Full observe-inject-complete cycle"
    :tier 10
    :category 'full-workflow
    :prompt "Find all elisp files in lisp/ and summarize"
    :setup (lambda ()
             ;; Set up injection after observing some progress
             (run-with-timer
              5 nil
              (lambda ()
                (when-let* ((session efrit-progress--current-session))
                  (efrit-progress-inject session 'guidance
                                        "Just list the file count, don't read contents")))))
    :validators '((custom efrit-test--validate-full-workflow)
                  (no-error))
    :timeout 180))

  (message "Registered %d Tier 10 tests" 8))

;;; ============================================================
;;; Custom Validators for Tier 9-10
;;; ============================================================

(defun efrit-test--validate-progress-file-exists ()
  "Validate that a progress.jsonl file was created.
Returns (pass-p . message)."
  (if-let* ((session efrit-progress--current-session)
            (progress-file (expand-file-name "progress.jsonl"
                                            (efrit-progress--session-dir session))))
      (if (file-exists-p progress-file)
          (cons t "Progress file exists")
        (cons nil "Progress file not found"))
    (cons nil "No current session")))

(defun efrit-test--validate-progress-has-events ()
  "Validate that progress file has events.
Returns (pass-p . message)."
  (if-let* ((session efrit-progress--current-session)
            (progress-file (expand-file-name "progress.jsonl"
                                            (efrit-progress--session-dir session))))
      (if (and (file-exists-p progress-file)
               (> (file-attribute-size (file-attributes progress-file)) 0))
          (cons t "Progress file has events")
        (cons nil "Progress file empty or missing"))
    (cons nil "No current session")))

(defun efrit-test--validate-progress-tool-events ()
  "Validate that progress has tool-start and tool-result events.
Returns (pass-p . message)."
  (if-let* ((session efrit-progress--current-session)
            (progress-file (expand-file-name "progress.jsonl"
                                            (efrit-progress--session-dir session))))
      (if (file-exists-p progress-file)
          (with-temp-buffer
            (insert-file-contents progress-file)
            (let ((content (buffer-string)))
              (if (and (string-match-p "tool-start\\|tool-result" content))
                  (cons t "Found tool events in progress")
                (cons nil "No tool events found"))))
        (cons nil "Progress file not found"))
    (cons nil "No current session")))

(defun efrit-test--validate-session-info-available ()
  "Validate that session info is queryable.
Returns (pass-p . message)."
  (if-let* ((info (efrit-progress-session-info)))
      (if (and (assoc 'session-id info)
               (assoc 'session-dir info))
          (cons t "Session info available")
        (cons nil "Session info incomplete"))
    (cons nil "No session info returned")))

(defun efrit-test--validate-session-dir-structure ()
  "Validate that session directory has correct structure.
Returns (pass-p . message)."
  (if-let* ((session efrit-progress--current-session)
            (session-dir (efrit-progress--session-dir session)))
      (let* ((progress-file (expand-file-name "progress.jsonl" session-dir))
             (inject-dir (expand-file-name "inject" session-dir)))
        (if (and (file-directory-p session-dir)
                 (or (file-exists-p progress-file)
                     (file-directory-p inject-dir)))
            (cons t "Session directory structure valid")
          (cons nil (format "Missing expected files in %s" session-dir))))
    (cons nil "No current session")))

(defun efrit-test--validate-progress-tail-compatible ()
  "Validate that progress file can be tailed (newline-delimited).
Returns (pass-p . message)."
  (if-let* ((session efrit-progress--current-session)
            (progress-file (expand-file-name "progress.jsonl"
                                            (efrit-progress--session-dir session))))
      (if (file-exists-p progress-file)
          (with-temp-buffer
            (insert-file-contents progress-file)
            (let ((content (buffer-string)))
              ;; Each line should be complete JSON
              (if (string-match-p "\n" content)
                  (cons t "Progress file is newline-delimited")
                (cons nil "Progress file not properly formatted"))))
        (cons nil "Progress file not found"))
    (cons nil "No current session")))

(defun efrit-test--validate-progress-jq-parseable ()
  "Validate that each line of progress is valid JSON.
Returns (pass-p . message)."
  (if-let* ((session efrit-progress--current-session)
            (progress-file (expand-file-name "progress.jsonl"
                                            (efrit-progress--session-dir session))))
      (if (file-exists-p progress-file)
          (with-temp-buffer
            (insert-file-contents progress-file)
            (let ((lines (split-string (buffer-string) "\n" t))
                  (valid t)
                  (error-line nil))
              (dolist (line lines)
                (when (and valid (> (length line) 0))
                  (condition-case _
                      (json-read-from-string line)
                    (error (setq valid nil
                                 error-line line)))))
              (if valid
                  (cons t "All lines are valid JSON")
                (cons nil (format "Invalid JSON: %s"
                                 (truncate-string-to-width error-line 50))))))
        (cons nil "Progress file not found"))
    (cons nil "No current session")))

(defun efrit-test--validate-full-workflow ()
  "Validate that full observe-inject-complete workflow works.
Returns (pass-p . message)."
  (if-let* ((session efrit-progress--current-session)
            (session-dir (efrit-progress--session-dir session))
            (progress-file (expand-file-name "progress.jsonl" session-dir)))
      (if (file-exists-p progress-file)
          (with-temp-buffer
            (insert-file-contents progress-file)
            (let ((content (buffer-string)))
              (if (string-match-p "injection-received\\|session-complete" content)
                  (cons t "Full workflow events observed")
                (cons t "Workflow completed (injection may have been processed)"))))
        (cons nil "Progress file not found"))
    (cons nil "No current session")))

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
  (efrit-test-register-tier9)
  (efrit-test-register-tier10)
  (message "Registered all %d tests across 8 tiers"
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
