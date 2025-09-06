;;; debug-todo-responses.el --- Debug what Claude sees -*- lexical-binding: t -*-

(add-to-list 'load-path (expand-file-name "lisp" default-directory))
(require 'efrit)

;; Clear TODOs
(efrit-do-clear-todos)

(message "=== Testing TODO Tool Responses ===\n")

;; Test 1: Empty TODO status
(message "1. Empty todo_status:")
(message "%s" (efrit-do--handle-todo-status))

;; Test 2: TODO analyze for warnings
(message "\n2. todo_analyze for 'fix warnings in *Warnings* buffer':")
(let ((input (make-hash-table :test 'equal)))
  (puthash "command" "fix warnings in *Warnings* buffer" input)
  (message "%s" (efrit-do--handle-todo-analyze input)))

;; Test 3: Create a TODO and check status
(message "\n3. After adding a TODO:")
(efrit-do--add-todo "Test TODO item" 'high)
(message "%s" (efrit-do--handle-todo-status))

;; Test 4: Check session protocol
(message "\n4. Session protocol excerpt:")
(let ((protocol (efrit-do--session-protocol-instructions)))
  (message "%s" (substring protocol 0 500)))

;; Test 5: Check command examples
(message "\n5. Command examples (warnings part):")
(let ((examples (efrit-do--command-examples)))
  (when (string-match "fix warnings" examples)
    (message "%s" (substring examples (match-beginning 0) 
                            (min (length examples) (+ (match-beginning 0) 400))))))