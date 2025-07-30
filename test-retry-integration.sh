#!/bin/bash

# test-retry-integration.sh
# Integration test for retry functionality

set -e

echo "=== Efrit Retry Integration Test ==="
echo ""

# Test 1: Basic retry functions exist and work
echo "🔧 Testing retry function availability..."
emacs --batch --load efrit.el --load efrit-tools.el --load efrit-chat.el --load efrit-do.el --eval "
(progn
  (unless (fboundp 'efrit-do--extract-error-info)
    (error \"efrit-do--extract-error-info function not available\"))
  (unless (fboundp 'efrit-do--extract-executed-code)
    (error \"efrit-do--extract-executed-code function not available\"))
  (message \"✅ All retry functions available\"))"

# Test 2: Configuration variables exist
echo "🔧 Testing retry configuration variables..."
emacs --batch --load efrit.el --load efrit-tools.el --load efrit-chat.el --load efrit-do.el --eval "
(progn
  (unless (boundp 'efrit-do-max-retries)
    (error \"efrit-do-max-retries variable not available\"))
  (unless (boundp 'efrit-do-retry-on-errors)
    (error \"efrit-do-retry-on-errors variable not available\"))
  (message \"✅ All retry configuration variables available\")
  (message \"   Max retries: %d\" efrit-do-max-retries)
  (message \"   Retry enabled: %s\" efrit-do-retry-on-errors))"

# Test 3: Error extraction works correctly
echo "🔧 Testing error extraction logic..."
emacs --batch --load efrit.el --load efrit-tools.el --load efrit-chat.el --load efrit-do.el --eval "
(progn
  (let ((syntax-error \"[Syntax Error in (bad-code: Invalid read syntax]\")
        (runtime-error \"[Error executing (nonexistent-fn): Symbol's function definition is void]\")
        (success-result \"[Executed: (message \\\"test\\\")]\n[Result: test]\"))
    
    ;; Test syntax error extraction
    (let ((error-info (efrit-do--extract-error-info syntax-error)))
      (unless (car error-info)
        (error \"Failed to detect syntax error\"))
      (message \"✅ Syntax error detection works\"))
    
    ;; Test runtime error extraction  
    (let ((error-info (efrit-do--extract-error-info runtime-error)))
      (unless (car error-info)
        (error \"Failed to detect runtime error\"))
      (message \"✅ Runtime error detection works\"))
    
    ;; Test success case (no error)
    (let ((error-info (efrit-do--extract-error-info success-result)))
      (when (car error-info)
        (error \"False positive: detected error in success result\"))
      (message \"✅ Success case detection works\"))))"

# Test 4: Code extraction works correctly
echo "🔧 Testing code extraction logic..."
emacs --batch --load efrit.el --load efrit-tools.el --load efrit-chat.el --load efrit-do.el --eval "
(progn
  (let ((syntax-error \"[Syntax Error in (bad-code: Invalid read syntax]\")
        (runtime-error \"[Error executing (nonexistent-fn): Symbol's function definition is void]\")
        (success-result \"[Executed: (message \\\"hello\\\")]\n[Result: hello]\"))
    
    ;; Test code extraction from syntax error
    (let ((code (efrit-do--extract-executed-code syntax-error)))
      (unless (string= code \"(bad-code\")
        (error \"Failed to extract code from syntax error: got %s\" code))
      (message \"✅ Code extraction from syntax error works\"))
    
    ;; Test code extraction from runtime error
    (let ((code (efrit-do--extract-executed-code runtime-error)))
      (unless (string= code \"(nonexistent-fn)\")
        (error \"Failed to extract code from runtime error: got %s\" code))
      (message \"✅ Code extraction from runtime error works\"))
    
    ;; Test code extraction from success
    (let ((code (efrit-do--extract-executed-code success-result)))
      (unless (string= code \"(message \\\"hello\\\")\")
        (error \"Failed to extract code from success result: got %s\" code))
      (message \"✅ Code extraction from success result works\"))))"

# Test 5: Retry system prompt generation
echo "🔧 Testing retry system prompt generation..."
emacs --batch --load efrit.el --load efrit-tools.el --load efrit-chat.el --load efrit-do.el --eval "
(progn
  ;; Test normal prompt (no retry)
  (let ((prompt (efrit-do--command-system-prompt)))
    (when (string-match \"RETRY ATTEMPT\" prompt)
      (error \"Normal prompt contains retry information\"))
    (message \"✅ Normal system prompt generation works\"))
  
  ;; Test retry prompt
  (let ((prompt (efrit-do--command-system-prompt 2 \"Test error\" \"(test-code)\")))
    (unless (string-match \"RETRY ATTEMPT 2/\" prompt)
      (error \"Retry prompt missing retry attempt info\"))
    (unless (string-match \"Previous code that failed: (test-code)\" prompt)
      (error \"Retry prompt missing previous code\"))
    (unless (string-match \"Error encountered: Test error\" prompt)
      (error \"Retry prompt missing error info\"))
    (message \"✅ Retry system prompt generation works\")))"

echo ""
echo "🎉 ALL RETRY INTEGRATION TESTS PASSED!"
echo ""
echo "✅ Retry functions available and working"
echo "✅ Configuration variables properly set"
echo "✅ Error detection logic working correctly"
echo "✅ Code extraction logic working correctly"
echo "✅ Retry system prompt generation working"
echo ""
echo "🚀 Retry logic is ready for production!"
