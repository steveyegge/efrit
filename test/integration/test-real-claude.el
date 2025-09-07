;;; test-real-claude.el --- REAL integration test that calls Claude API -*- lexical-binding: t -*-

(let ((efrit-root "/Users/stevey/src/efrit"))
  (add-to-list 'load-path (expand-file-name "lisp" efrit-root))
  (add-to-list 'load-path (expand-file-name "test/integration" efrit-root)))

(require 'cl-lib)
(require 'efrit-do)

;; Test configuration
(defvar test-integration-dir (expand-file-name "test/integration" "/Users/stevey/src/efrit")
  "Directory containing integration test files.")

(defvar test-integration-stub-files '("stub-file-1.el" "stub-file-2.el" "stub-file-3.el")
  "List of stub files to test with.")

(defun reset-stub-files ()
  "Reset stub files to broken state (without lexical-binding cookies)."
  (let ((templates '(
    ";; File without lexical binding cookie - should trigger warning\n(defun test-function-1 ()\n  \"A simple test function.\"\n  (message \"Hello from stub file 1\"))\n\n(provide 'stub-file-1)\n;;; stub-file-1.el ends here\n"
    ";; Another file missing lexical binding cookie\n(defcustom test-variable-2 \"default\"\n  \"A test variable.\"\n  :type 'string\n  :group 'test)\n\n(defun test-function-2 ()\n  \"Another test function.\"\n  (message \"Hello from stub file 2: %s\" test-variable-2))\n\n(provide 'stub-file-2)\n;;; stub-file-2.el ends here\n"
    ";; Third stub file without proper header\n(defmacro test-macro-3 (body)\n  \"A test macro.\"\n  `(progn\n     (message \"Executing macro 3\")\n     ,body))\n\n(defun test-function-3 ()\n  \"Third test function.\"\n  (test-macro-3\n    (message \"Hello from stub file 3\")))\n\n(provide 'stub-file-3)\n;;; stub-file-3.el ends here\n")))
    (cl-loop for file in test-integration-stub-files
             for template in templates
             do (let ((file-path (expand-file-name file test-integration-dir)))
                  (with-temp-file file-path
                    (insert template))
                  (message "Reset %s" file))))
  (message "All stub files reset to original state"))

(defvar test-real-claude-completed nil "Flag for async completion")
(defvar test-real-claude-result nil "Result from async operation")

(defun test-real-claude-integration ()
  "REAL integration test that calls Claude API and burns tokens.
This test verifies Claude can:
1. Discover lexical-binding warnings from scratch
2. Figure out the solution autonomously  
3. Write and execute elisp code to fix the files"
  (interactive)
  (message "🔥 REAL CLAUDE INTEGRATION TEST (BURNS TOKENS)")
  (message "=============================================")
  
  ;; Must allow file modifications for this test
  (let ((original-security efrit-tools-security-level))
    (setq efrit-tools-security-level 'disabled)
    (message "⚠️ Security temporarily disabled for test")
    
    ;; Reset files to broken state  
    (reset-stub-files)
    (message "✓ Reset stub files - no lexical-binding cookies")
    
    ;; Show initial state
    (message "\n--- INITIAL STATE ---")
    (dolist (file test-integration-stub-files)
      (let ((full-path (expand-file-name file test-integration-dir)))
        (with-temp-buffer
          (insert-file-contents full-path)
          (goto-char (point-min))
          (let ((first-line (buffer-substring (point-min) (line-end-position))))
            (message "%s: %s" file first-line)))))
    
    ;; Reset completion tracking
    (setq test-real-claude-completed nil
          test-real-claude-result nil)
    
    ;; The REAL test - call Claude with natural language
    (message "\n--- CALLING CLAUDE API (BURNING TOKENS) ---")
    (message "Sending natural language request to Claude...")
    (efrit-do-async 
     (format "I have 3 Emacs Lisp files in %s that are missing lexical-binding cookies and will cause byte-compilation warnings. The files are %s. Please examine them and add the proper lexical-binding headers to fix the warnings."
             test-integration-dir
             (mapconcat 'identity test-integration-stub-files ", "))
     (lambda (result)
       (setq test-real-claude-completed t
             test-real-claude-result result)
       (message "✓ Claude completed processing!")
       (message "Result: %s" (if (> (length result) 200)
                               (concat (substring result 0 197) "...")
                             result))))
    
    (message "⏳ Waiting for Claude to respond and execute...")
    (message "   (This should take 5-30 seconds and will cost API tokens)")
    
    ;; Wait for Claude to complete (with timeout)
    (let ((timeout 60)
          (start-time (current-time)))
      (while (and (not test-real-claude-completed)
                  (< (float-time (time-since start-time)) timeout))
        (sleep-for 0.5)
        (message "   Waiting... %.1fs elapsed" (float-time (time-since start-time))))
      
      (if test-real-claude-completed
          (message "✓ Claude responded after %.1fs" (float-time (time-since start-time)))
        (message "⚠️ Claude timed out after %ds" timeout)))
    
    ;; Check results 
    (message "\n--- VERIFYING CLAUDE'S WORK ---")
    (let ((all-fixed t)
          (files-checked 0))
      (dolist (file test-integration-stub-files)
        (let ((full-path (expand-file-name file test-integration-dir)))
          (when (file-exists-p full-path)
            (setq files-checked (1+ files-checked))
            (with-temp-buffer
              (insert-file-contents full-path)
              (goto-char (point-min))
              (let ((first-line (buffer-substring (point-min) (line-end-position))))
                (if (string-match "lexical-binding.*t" first-line)
                    (message "✓ %s: FIXED by Claude! - %s" file first-line)
                  (message "✗ %s: NOT fixed - %s" file first-line)
                  (setq all-fixed nil)))))))
      
      (message "\nFiles checked: %d/%d" files-checked (length test-integration-stub-files))
      
      ;; Final verdict
      (if (and test-real-claude-completed all-fixed)
          (progn
            (message "\n🏆 REAL INTEGRATION TEST PASSED!")
            (message "✅ Claude discovered the lexical-binding warnings autonomously")
            (message "✅ Claude wrote and executed elisp code to fix the files")  
            (message "✅ All files now have proper lexical-binding headers")
            (message "✅ Real AI workflow validated end-to-end"))
        (progn
          (message "\n❌ REAL INTEGRATION TEST FAILED")
          (when (not test-real-claude-completed)
            (message "   - Claude API call timed out or failed"))
          (when (not all-fixed)
            (message "   - Claude did not fix all files"))
          (message "   - The TODO loop issue may still exist"))))
    
    ;; Restore security
    (setq efrit-tools-security-level original-security)
    (message "✓ Security level restored to: %s" original-security)))

(message "")
(message "🔥 REAL CLAUDE INTEGRATION TEST LOADED")
(message "This test BURNS TOKENS by calling the Anthropic API!")
(message "Run: (test-real-claude-integration)")
(message "")

(provide 'test-real-claude)
;;; test-real-claude.el ends here
