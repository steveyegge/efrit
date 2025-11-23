;;; test-real-integration.el --- Real Claude API integration test -*- lexical-binding: t; -*-
;;; Commentary:
;; This is the REAL integration test that:
;; 1. Creates elisp files missing lexical-binding cookies
;; 2. Loads them to generate warnings in *Warnings* buffer
;; 3. Uses efrit-do-async to fix the warnings (burns tokens!)
;; 4. Verifies files are actually modified

;;; Code:

(require 'efrit-do)

;; Test configuration
(defvar test-integration-dir (expand-file-name "test-integration-temp/")
  "Temporary directory for integration test files.")

(defvar test-integration-cleanup t
  "When non-nil, clean up test files before and after test.
Set to nil to leave files in place for inspection with git status.")

(defun test-create-stub-files ()
  "Create elisp files missing lexical-binding cookies to trigger warnings."
  (make-directory test-integration-dir t)
  
  ;; Create 3 stub files without lexical-binding cookies
  (dolist (i '(1 2 3))
    (let ((file (expand-file-name (format "test-stub-%d.el" i) test-integration-dir)))
      (with-temp-file file
        (insert (format ";;; test-stub-%d.el --- Test file missing lexical binding
;;; Commentary:
;; This file intentionally lacks the lexical-binding cookie to trigger warnings

;;; Code:

(defun test-function-%d ()
  \"Test function %d.\"
  (message \"Hello from test function %d\"))

(provide 'test-stub-%d)
;;; test-stub-%d.el ends here
" i i i i i i)))
      (message "Created %s" file))))

(defun test-load-files-and-generate-warnings ()
  "Load the stub files to generate warnings in *Warnings* buffer."
  (message "Loading files to generate warnings...")
  
  ;; Clear existing warnings
  (when (get-buffer "*Warnings*")
    (with-current-buffer "*Warnings*"
      (let ((inhibit-read-only t))
        (erase-buffer))))
  
  ;; Load each file to trigger warnings
  (dolist (i '(1 2 3))
    (let ((file (expand-file-name (format "test-stub-%d.el" i) test-integration-dir)))
      (load-file file)))
  
  ;; Give warnings time to appear
  (sleep-for 1)
  
  ;; Check if warnings were generated
  (if-let* ((warnings-buffer (get-buffer "*Warnings*")))
      (with-current-buffer warnings-buffer
        (let ((content (buffer-string)))
          (if (and (> (length content) 0)
                   (string-match-p "lexical-binding" content))
              (progn
                (message "✅ Warnings generated successfully")
                (message "Warnings content preview: %s" 
                         (substring content 0 (min 200 (length content))))
                t)
            (progn
              (message "❌ No lexical-binding warnings found")
              nil))))
    (progn
      (message "❌ No *Warnings* buffer found")
      nil)))

(defun test-check-file-modifications ()
  "Check if the stub files have been modified (lexical-binding added)."
  (let ((all-fixed t))
    (dolist (i '(1 2 3))
      (let ((file (expand-file-name (format "test-stub-%d.el" i) test-integration-dir)))
        (with-temp-buffer
          (insert-file-contents file)
          (let ((content (buffer-string)))
            (if (string-match-p "lexical-binding: t" content)
                (message "✅ File %d fixed: lexical-binding cookie added" i)
              (progn
                (message "❌ File %d not fixed: missing lexical-binding cookie" i)
                (setq all-fixed nil)))))))
    all-fixed))

(defun test-cleanup ()
  "Clean up test files."
  (when (file-exists-p test-integration-dir)
    (delete-directory test-integration-dir t)
    (message "Test files cleaned up")))

(defun run-real-integration-test ()
  "Run the complete real integration test."
  (interactive)
  (message "🚀 Starting REAL integration test - this will burn tokens!")
  
  ;; Clean up any previous test (configurable)
  (when test-integration-cleanup
    (test-cleanup))
  
  ;; Clear efrit state
  (efrit-do-clear-all)
  
  (unwind-protect
      (progn
        ;; Step 1: Create stub files
        (message "\n=== Step 1: Creating stub files ===")
        (test-create-stub-files)
        
        ;; Step 2: Generate warnings
        (message "\n=== Step 2: Generating warnings ===")
        (if (test-load-files-and-generate-warnings)
            (progn
              ;; Step 3: Use efrit-do-async to fix warnings (BURNS TOKENS!)
              (message "\n=== Step 3: Using efrit-do-async to fix warnings ===")
              (message "🔥 BURNING TOKENS: Calling Claude API...")
              
              (let ((test-complete nil)
                    (start-time (current-time)))
                
                (efrit-do-async "Fix all the lexical-binding warnings in the *Warnings* buffer by adding the missing cookies to the files")
                
                ;; Wait for async completion (max 90 seconds)
                (message "Waiting for Claude to complete the fix...")
                (while (and (not test-complete) 
                           (< (float-time (time-since start-time)) 90))
                  (sleep-for 1)
                  
                  ;; Check if we have results
                  (when (and efrit-do--last-result
                             (> (length efrit-do--last-result) 0))
                    (setq test-complete t)))
                
                (if test-complete
                    (progn
                      (message "✅ Claude API call completed!")
                      (message "Result: %s" 
                               (if (> (length efrit-do--last-result) 150)
                                   (concat (substring efrit-do--last-result 0 147) "...")
                                 efrit-do--last-result))
                      
                      ;; Step 4: Verify modifications
                      (message "\n=== Step 4: Verifying file modifications ===")
                      (sleep-for 2) ; Give file operations time to complete
                      
                      (if (test-check-file-modifications)
                          (message "🎉 SUCCESS: All files fixed by Claude!")
                        (message "⚠️  Files not yet modified - may need more time")))
                  (message "❌ TIMEOUT: Claude API call did not complete within 90 seconds"))))
          (message "❌ FAILED: Could not generate warnings - test aborted")))
    
    ;; Clean up (configurable - set test-integration-cleanup to nil to disable)
    (when test-integration-cleanup
      (test-cleanup))
    ))

;; Run the test
(run-real-integration-test)
