;;; test-real-simple.el --- Simple real Claude test -*- lexical-binding: t -*-

(let ((efrit-root "/Users/stevey/src/efrit"))
  (add-to-list 'load-path (expand-file-name "lisp" efrit-root))
  (add-to-list 'load-path (expand-file-name "test/integration" efrit-root)))

(require 'cl-lib)
(require 'efrit-do)

;; Test configuration
(defvar test-integration-dir (expand-file-name "test/integration" "/Users/stevey/src/efrit")
  "Directory containing integration test files.")

(defun reset-test-files ()
  "Create test files without lexical-binding cookies."
  (with-temp-file (expand-file-name "test-broken.el" test-integration-dir)
    (insert ";; This file is missing lexical-binding cookie - will cause warnings\n")
    (insert "(defun test-func () (message \"test\"))\n")
    (insert "(provide 'test-broken)\n"))
  (message "Created test-broken.el without lexical-binding cookie"))

(defun check-test-files ()
  "Check if test files were fixed."
  (let ((file-path (expand-file-name "test-broken.el" test-integration-dir)))
    (if (file-exists-p file-path)
        (with-temp-buffer
          (insert-file-contents file-path)
          (goto-char (point-min))
          (let ((first-line (buffer-substring (point-min) (line-end-position))))
            (if (string-match "lexical-binding.*t" first-line)
                (message "✅ test-broken.el FIXED! - %s" first-line)
              (message "❌ test-broken.el NOT fixed - %s" first-line))))
      (message "❌ test-broken.el not found"))))

(defun test-real-simple ()
  "Simple test that calls efrit-do-async with natural language."
  (interactive)
  (message "🔥 SIMPLE REAL CLAUDE TEST")
  (message "========================")
  
  ;; Allow file modifications
  (setq efrit-tools-security-level 'disabled)
  (message "⚠️ Security disabled for test")
  
  ;; Reset test file
  (reset-test-files)
  
  ;; Show before state
  (message "\n--- BEFORE ---")
  (check-test-files)
  
  ;; Call Claude with natural language - this should burn tokens
  (message "\n--- CALLING CLAUDE (BURNS TOKENS) ---")
  (efrit-do-async 
   (format "Fix the lexical-binding warning in %s/test-broken.el by adding the proper header cookie"
           test-integration-dir))
  
  (message "✓ Claude API call initiated")
  (message "⏳ Wait ~30 seconds then run (check-test-files) to see if Claude fixed it")
  (message "💰 This burns API tokens if it works!"))

(message "Simple real Claude test loaded.")
(message "Run: (test-real-simple)")
(message "Then wait 30s and run: (check-test-files)")

(provide 'test-real-simple)
