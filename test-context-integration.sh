#!/bin/bash

# test-context-integration.sh
# Integration test for context enhancement in retry logic

set -e

echo "=== Efrit Context Enhancement Integration Test ==="
echo ""

# Test 1: Basic context building function works
echo "🔧 Testing context building function..."
emacs --batch --load efrit.el --load efrit-tools.el --load efrit-chat.el --load efrit-do.el --eval "
(progn
  (unless (fboundp 'efrit-do--build-error-context)
    (error \"efrit-do--build-error-context function not available\"))
  (let ((context (efrit-do--build-error-context)))
    (unless (stringp context)
      (error \"Context should be a string\"))
    (unless (string-match \"CURRENT BUFFER:\" context)
      (error \"Context missing buffer information\"))
    (unless (string-match \"CURRENT DIRECTORY:\" context)
      (error \"Context missing directory information\"))
    (unless (string-match \"VISIBLE BUFFERS:\" context)
      (error \"Context missing visible buffers information\"))
    (unless (string-match \"WINDOW LAYOUT:\" context)
      (error \"Context missing window layout information\"))
    (message \"✅ Context building function works correctly\")))"

# Test 2: Context integration with retry prompts
echo "🔧 Testing context integration with retry prompts..."
emacs --batch --load efrit.el --load efrit-tools.el --load efrit-chat.el --load efrit-do.el --eval "
(progn
  ;; Create a test buffer with some content
  (with-current-buffer (get-buffer-create \"*test-context*\")
    (insert \"Test content for context\")
    (goto-char (point-min))
    
    ;; Test retry prompt with context
    (let ((prompt (efrit-do--command-system-prompt 2 \"Test error\" \"(test-code)\")))
      (unless (string-match \"RETRY ATTEMPT 2/\" prompt)
        (error \"Retry prompt missing retry attempt info\"))
      (unless (string-match \"Previous code that failed: (test-code)\" prompt)
        (error \"Retry prompt missing previous code\"))
      (unless (string-match \"Error encountered: Test error\" prompt)
        (error \"Retry prompt missing error info\"))
      (unless (string-match \"CURRENT EMACS STATE:\" prompt)
        (error \"Retry prompt missing context section\"))
      (unless (string-match \"CURRENT BUFFER: \\\\*test-context\\\\*\" prompt)
        (error \"Retry prompt missing current buffer info\"))
      (unless (string-match \"Test content for context\" prompt)
        (error \"Retry prompt missing buffer content\"))
      (message \"✅ Context integration with retry prompts works\"))))"

# Test 3: Context includes visible buffers and window info
echo "🔧 Testing window and buffer context..."
emacs --batch --load efrit.el --load efrit-tools.el --load efrit-chat.el --load efrit-do.el --eval "
(progn
  ;; Create multiple buffers and windows
  (let ((buf1 (get-buffer-create \"*context-test-1*\"))
        (buf2 (get-buffer-create \"*context-test-2*\")))
    (with-current-buffer buf1
      (insert \"Content in buffer 1\"))
    (with-current-buffer buf2
      (insert \"Content in buffer 2\"))
    
    ;; Switch to first buffer and split window
    (switch-to-buffer buf1)
    (when (> (length (window-list)) 1)
      (delete-other-windows)) ; Ensure we start with one window
    (split-window)
    (other-window 1)
    (switch-to-buffer buf2)
    (other-window 1) ; Back to first buffer
    
    (let ((context (efrit-do--build-error-context)))
      (unless (string-match \"VISIBLE BUFFERS:.*context-test\" context)
        (error \"Context missing visible buffer info\"))
      (unless (string-match \"WINDOW LAYOUT: 2 windows\" context)
        (error \"Context missing window layout info: %s\" context))
      (unless (string-match \"Content in buffer 1\" context)
        (error \"Context missing buffer content\"))
      (message \"✅ Window and buffer context works correctly\"))
    
    ;; Clean up
    (delete-other-windows)))"

# Test 4: Context with recent command history
echo "🔧 Testing recent command history in context..."
emacs --batch --load efrit.el --load efrit-tools.el --load efrit-chat.el --load efrit-do.el --eval "
(progn
  ;; Clear existing context and add test items
  (efrit-do--clear-context)
  (efrit-do--capture-context \"test command 1\" \"test result 1\")
  (efrit-do--capture-context \"test command 2\" \"test result 2\")
  
  (let ((context (efrit-do--build-error-context)))
    (unless (string-match \"RECENT COMMANDS:\" context)
      (error \"Context missing recent commands section\"))
    (unless (string-match \"test command\" context)
      (error \"Context missing command history\"))
    (unless (string-match \"test result\" context)
      (error \"Context missing result history\"))
    (message \"✅ Recent command history in context works\")))"

# Test 5: Error handling in context building
echo "🔧 Testing error handling in context building..."
emacs --batch --load efrit.el --load efrit-tools.el --load efrit-chat.el --load efrit-do.el --eval "
(progn
  ;; Test with corrupted or missing context ring
  (let ((efrit-do--context-ring nil)) ; Simulate missing ring
    (let ((context (efrit-do--build-error-context)))
      (unless (stringp context)
        (error \"Context should still be a string even with missing ring\"))
      (unless (string-match \"CURRENT BUFFER:\" context)
        (error \"Basic context info should still work\"))
      (message \"✅ Error handling in context building works\"))))"

# Test 6: Context in different buffer modes
echo "🔧 Testing context with different major modes..."
emacs --batch --load efrit.el --load efrit-tools.el --load efrit-chat.el --load efrit-do.el --eval "
(progn
  ;; Test with emacs-lisp-mode
  (with-current-buffer (get-buffer-create \"*lisp-test*\")
    (emacs-lisp-mode)
    (insert \"(defun test-function () 'test)\")
    (goto-char (point-min))
    
    (let ((context (efrit-do--build-error-context)))
      (unless (string-match \"mode: emacs-lisp-mode\" context)
        (error \"Context missing major mode info\"))
      (unless (string-match \"defun test-function\" context)
        (error \"Context missing elisp content\"))
      (message \"✅ Context with different major modes works\"))))"

echo ""
echo "🎉 ALL CONTEXT ENHANCEMENT INTEGRATION TESTS PASSED!"
echo ""
echo "✅ Context building function works correctly"
echo "✅ Context integrated with retry prompts"
echo "✅ Window and buffer context captured"
echo "✅ Recent command history included"
echo "✅ Error handling works properly"
echo "✅ Different major modes supported"
echo ""
echo "🚀 Context enhancement is ready for production!"
