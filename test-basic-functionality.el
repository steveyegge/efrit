;;; test-basic-functionality.el --- Basic functionality test for efrit -*- lexical-binding: t -*-

;; Simple test to verify efrit functionality after cleanup

(message "=== Starting Basic Efrit Functionality Test ===")

;; Add the current directory to load path (we're now in the efrit directory)
(add-to-list 'load-path ".")

;; Test loading core modules
(message "Testing module loading...")

(condition-case err
    (require 'efrit-tools)
  (error (message "ERROR loading efrit-tools: %s" err) (kill-emacs 1)))
(message "✅ efrit-tools loaded")

(condition-case err
    (require 'efrit-multi-turn)
  (error (message "ERROR loading efrit-multi-turn: %s" err) (kill-emacs 1)))
(message "✅ efrit-multi-turn loaded")

(condition-case err
    (require 'efrit-chat)
  (error (message "ERROR loading efrit-chat: %s" err) (kill-emacs 1)))
(message "✅ efrit-chat loaded")

;; Test core tools
(message "\nTesting core tools...")

;; Test eval_sexp
(let ((result (efrit-tools-eval-sexp "(+ 2 3)")))
  (if (string= result "5")
      (message "✅ eval_sexp working: %s" result)
    (message "❌ eval_sexp failed: %s" result)
    (kill-emacs 1)))

;; Test get_context
(let ((context (efrit-tools-get-context)))
  (if (and context (> (length context) 100))
      (message "✅ get_context working: %d characters" (length context))
    (message "❌ get_context failed: %s" context)
    (kill-emacs 1)))

;; Note: resolve_path removed - now handled by Claude via elisp
(message "✅ resolve_path removed (now handled by Claude)")

;; Test tool extraction
(message "\nTesting tool extraction...")
(let* ((test-text "Let me evaluate: <elisp>(* 6 7)</elisp>")
       (result (efrit-tools-extract-tools-from-response test-text)))
  (if (string-match-p "\\[Result: 42\\]" (car result))
      (message "✅ tool extraction working: %s" (car result))
    (message "❌ tool extraction failed: %s" (car result))
    (kill-emacs 1)))

;; Test multi-turn conversation
(message "\nTesting multi-turn functionality...")
(let ((conv (efrit--create-conversation "test request")))
  (if (efrit-conversation-p conv)
      (message "✅ multi-turn conversation creation working: %s" (efrit-conversation-id conv))
    (message "❌ multi-turn conversation creation failed")
    (kill-emacs 1)))

;; Test efrit-chat buffer setup
(message "\nTesting efrit-chat setup...")
(let ((buffer (efrit--setup-buffer)))
  (if (buffer-live-p buffer)
      (message "✅ efrit-chat buffer setup working: %s" (buffer-name buffer))
    (message "❌ efrit-chat buffer setup failed")
    (kill-emacs 1)))

;; Test system prompt
(let ((prompt (efrit-tools-system-prompt)))
  (if (and prompt (> (length prompt) 1000))
      (message "✅ system prompt working: %d characters" (length prompt))
    (message "❌ system prompt failed")
    (kill-emacs 1)))

(message "\n🎉 All basic functionality tests passed!")
(message "   ✅ All modules load correctly")
(message "   ✅ Core tools working (eval_sexp, get_context)")
(message "   ✅ Tool extraction working")
(message "   ✅ Multi-turn conversation setup working")
(message "   ✅ efrit-chat UI setup working")
(message "   ✅ System prompt generation working")
(message "\n🚀 efrit is ready for remote execution!")

;;; test-basic-functionality.el ends here
