#!/usr/bin/env emacs --script

;; Test unified command interface

(add-to-list 'load-path (expand-file-name "lisp" default-directory))

(require 'efrit-unified)

(message "=== Testing Unified Command Interface ===")
(message "")

;; Test 1: Command complexity estimation
(message "Test 1: Command complexity estimation")
(let ((sync-commands '("show time" 
                      "insert text" 
                      "goto line 10"
                      "display buffer"))
      (async-commands '("fetch weather from api"
                       "analyze all files"
                       "download and process data"
                       "compile project and run tests")))
  
  (message "  Sync commands:")
  (dolist (cmd sync-commands)
    (let ((score (efrit-unified--estimate-complexity cmd)))
      (message "    %s: score=%d, async=%s" 
               cmd score 
               (if (efrit-unified--should-use-async-p cmd) "yes" "no"))))
  
  (message "  Async commands:")
  (dolist (cmd async-commands)
    (let ((score (efrit-unified--estimate-complexity cmd)))
      (message "    %s: score=%d, async=%s" 
               cmd score 
               (if (efrit-unified--should-use-async-p cmd) "yes" "no")))))

(message "")
(message "Test 2: Mode suggestion")
(let ((test-commands '("show current directory"
                      "fetch data from https://example.com"
                      "find all TODO comments and summarize"
                      "insert date at point")))
  (dolist (cmd test-commands)
    (message "  %s" (efrit-unified-suggest-mode cmd))))

(message "")
(message "Test 3: Force mode settings")
(let ((efrit-unified-force-mode 'sync))
  (message "  Force sync: fetch weather -> %s" 
           (if (efrit-unified--should-use-async-p "fetch weather") "async" "sync")))

(let ((efrit-unified-force-mode 'async))
  (message "  Force async: show time -> %s" 
           (if (efrit-unified--should-use-async-p "show time") "async" "sync")))

(let ((efrit-unified-force-mode nil))
  (message "  Auto mode: complex task -> %s" 
           (if (efrit-unified--should-use-async-p "analyze all files and create report") "async" "sync")))

(message "")
(message "Test 4: Keyword detection")
(message "  Multi-step: 'do X and then Y' -> score=%d" 
         (efrit-unified--estimate-complexity "do X and then Y"))
(message "  External: 'run shell command' -> score=%d" 
         (efrit-unified--estimate-complexity "run shell command"))
(message "  Bulk ops: 'process all files' -> score=%d" 
         (efrit-unified--estimate-complexity "process all files"))

(message "")
(message "=== All Unified Interface Tests Complete ===")
(kill-emacs 0)