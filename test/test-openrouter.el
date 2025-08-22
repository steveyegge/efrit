;;; test-openrouter.el ---  -*- lexical-binding: t; -*-
(message "=== Response Processing Debug Test ===")

;; Set up environment
(add-to-list 'load-path "../lisp")
(require 'efrit-tools)
(require 'efrit-do)
(setq efrit-do-debug t)

;; Sample API response from the actual call
(defvar test-api-response
  "{\"id\":\"gen-1755874447-77pPQOiXzJ9E4p2wUGQg\",\"provider\":\"Google\",\"model\":\"anthropic/claude-sonnet-4\",\"object\":\"chat.completion\",\"created\":1755874447,\"choices\":[{\"logprobs\":null,\"finish_reason\":\"tool_calls\",\"native_finish_reason\":\"tool_calls\",\"index\":0,\"message\":{\"role\":\"assistant\",\"content\":\"I'll calculate 15 * 23 and show the result.\",\"refusal\":null,\"reasoning\":null,\"tool_calls\":[{\"id\":\"toolu_vrtx_013wcQgbLbMbaKG1vXH4Hjq9\",\"index\":0,\"type\":\"function\",\"function\":{\"name\":\"eval_sexp\",\"arguments\":\"{\\\"expr\\\": \\\"(* 15 23)\\\"}\"}}]}}],\"usage\":{\"prompt_tokens\":2042,\"completion_tokens\":75,\"total_tokens\":2117}}")

(message "\n🔍 Testing response processing...")
(message "Raw response: %s" test-api-response)

;; Test the response processing function directly
(let ((result (efrit-do--process-api-response test-api-response)))
  (message "\nProcessed result: '%s'" result)
  (message "Result length: %d" (length result))
  (message "Contains 345? %s" (if (string-match-p "345" result) "YES" "NO")))

;; Let's also examine the parsed JSON structure
(message "\n🔍 Examining JSON structure...")
(let* ((json-object-type 'hash-table)
       (json-array-type 'vector)
       (json-key-type 'string)
       (response (json-read-from-string test-api-response)))

  (message "Response keys: %S" (hash-table-keys response))

  (let ((choices (gethash "choices" response)))
    (message "Choices length: %d" (length choices))

    (when (> (length choices) 0)
      (let* ((choice (aref choices 0))
             (message-obj (gethash "message" choice))
             (content (gethash "content" message-obj))
             (tool-calls (gethash "tool_calls" message-obj)))

        (message "Message content: '%s'" content)
        (message "Tool calls: %S" tool-calls)

        (when tool-calls
          (message "Tool calls length: %d" (length tool-calls))
          (when (> (length tool-calls) 0)
            (let* ((tool-call (aref tool-calls 0))
                   (function-obj (gethash "function" tool-call))
                   (name (gethash "name" function-obj))
                   (arguments (gethash "arguments" function-obj)))
              (message "Tool name: %s" name)
              (message "Tool arguments: %s" arguments))))))))

(message "\n=== Test Complete ===")


(message "=== Unit Test: efrit-tools-extract-tools-from-response ===")

;; Set up environment
(add-to-list 'load-path "../lisp")
(require 'efrit-tools)

;; Test function to run a single test case
(defun test-extract-tools (test-name input expected-blocks)
  "Test efrit-tools-extract-tools-from-response with given input."
  (message "\n--- Test: %s ---" test-name)
  (message "Input length: %d chars" (length input))
  (message "Expected blocks: %d" expected-blocks)

  (let ((result (efrit-tools-extract-tools-from-response input)))
    (let ((processed-text (car result))
          (results-list (cdr result))
          (actual-blocks (length (cdr result))))

      (message "Actual blocks processed: %d" actual-blocks)
      (message "Results list: %S" results-list)

      ;; Check if text was processed
      (let ((text-changed (not (string= processed-text input))))
        (message "Text changed: %s" (if text-changed "YES" "NO")))

      ;; Check if we got the expected number of blocks
      (let ((blocks-correct (= actual-blocks expected-blocks)))
        (message "Blocks correct: %s" (if blocks-correct "YES" "NO"))
        (if blocks-correct
            (message "✅ %s: PASSED" test-name)
          (message "❌ %s: FAILED (expected %d, got %d)" test-name expected-blocks actual-blocks))))))

;; Test Case 1: Original failing case (multi-line with newlines between blocks)
(let ((test1 "I'll help you with that. First, let me create a buffer:
<elisp>(with-current-buffer (get-buffer-create \"*test-output*\")
  (erase-buffer)
  (insert \"Hello World\\n\")
  (buffer-name))</elisp>

Now let me add more content:
<elisp>(with-current-buffer \"*test-output*\"
  (goto-char (point-max))
  (insert \"Additional line\\n\")
  (buffer-size))</elisp>

Done!"))
  (test-extract-tools "Multi-line with newlines between blocks" test1 2))

;; Test Case 2: Multi-line with no newlines between blocks
(let ((test2 "I'll help you with that. First, let me create a buffer:
<elisp>(with-current-buffer (get-buffer-create \"*test-output*\")
  (erase-buffer)
  (insert \"Hello World\\n\")
  (buffer-name))</elisp>
Now let me add more content:
<elisp>(with-current-buffer \"*test-output*\"
  (goto-char (point-max))
  (insert \"Additional line\\n\")
  (buffer-size))</elisp>
Done!"))
  (test-extract-tools "Multi-line with no newlines between blocks" test2 2))

;; Test Case 3: Single-line elisp blocks
(let ((test3 "Simple test: <elisp>(+ 2 3)</elisp> and <elisp>(* 4 5)</elisp> Done!"))
  (test-extract-tools "Single-line elisp blocks" test3 2))

;; Test Case 4: Mixed single and multi-line blocks
(let ((test4 "First: <elisp>(+ 1 2)</elisp>
Then complex:
<elisp>(with-current-buffer (get-buffer-create \"*test*\")
  (insert \"Hello\"))</elisp>
Finally: <elisp>(buffer-name)</elisp>"))
  (test-extract-tools "Mixed single and multi-line blocks" test4 3))

;; Test Case 5: Multi-line with extra whitespace and indentation
(let ((test5 "Let's test this:
<elisp>
  (with-current-buffer
    (get-buffer-create \"*test*\")
    (insert \"Hello World\"))
</elisp>

And another:
<elisp>
  (buffer-name)
</elisp>

Done!"))
  (test-extract-tools "Multi-line with extra whitespace and indentation" test5 2))

(message "\n=== Hypothesis Testing ===")

;; Test 1: Implementation Logic Bug in Capture Groups Approach
(message "\n--- Testing Hypothesis 1: Implementation Logic Bug ---")
(let ((test-input "Single: <elisp>(+ 1 2)</elisp>
Multi: <elisp>(with-current-buffer (get-buffer-create \"*test*\")
  (insert \"Hello\"))</elisp>
Single: <elisp>(buffer-name)</elisp>"))

  ;; Test if the manual tag finding logic is being executed at all
  (let ((pos 0)
        (found-tags 0))
    (while (string-match "<elisp>" test-input pos)
      (setq found-tags (1+ found-tags))
      (let* ((start-tag-pos (match-beginning 0))
             (start-tag-end (match-end 0))
             (end-tag-pos (string-match "</elisp>" test-input start-tag-end)))
        (if end-tag-pos
            (setq pos (+ end-tag-pos 8))
          (setq pos (1+ start-tag-pos)))))

    (message "Manual tag search found: %d tags" found-tags)
    (if (= found-tags 3)
        (message "✅ HYPOTHESIS 1 REJECTED: Manual tag finding works correctly")
      (message "❌ HYPOTHESIS 1 CONFIRMED: Manual tag finding has bugs"))))

;; Test 2: Variable Scope or State Management Issue
(message "\n--- Testing Hypothesis 2: Variable Scope/State Management ---")
(let ((test-input "Test: <elisp>(+ 1 2)</elisp>"))
  ;; Test if the function properly updates its internal state
  (let* ((result1 (efrit-tools-extract-tools-from-response test-input))
         (result2 (efrit-tools-extract-tools-from-response test-input)))

    (message "First call results: %S" (cdr result1))
    (message "Second call results: %S" (cdr result2))
    (message "Results identical: %s" (if (equal (cdr result1) (cdr result2)) "YES" "NO"))

    (if (and (cdr result1) (equal (cdr result1) (cdr result2)))
        (message "✅ HYPOTHESIS 2 REJECTED: Function state management is consistent")
      (message "❌ HYPOTHESIS 2 CONFIRMED: Function has state management issues"))))

;; Test 3: Conditional Logic or Feature Flag Issue
(message "\n--- Testing Hypothesis 3: Conditional Logic/Feature Flag ---")
(let ((single-line-input "Test: <elisp>(+ 1 2)</elisp>")
      (multi-line-input "Test: <elisp>(with-current-buffer (get-buffer-create \"*test*\")
  (insert \"Hello\"))</elisp>"))

  ;; Test if the function behaves differently based on input characteristics
  (let* ((single-result (efrit-tools-extract-tools-from-response single-line-input))
         (multi-result (efrit-tools-extract-tools-from-response multi-line-input))
         (single-blocks (length (cdr single-result)))
         (multi-blocks (length (cdr multi-result)))
         (single-changed (not (string= (car single-result) single-line-input)))
         (multi-changed (not (string= (car multi-result) multi-line-input))))

    (message "Single-line: %d blocks, text changed: %s" single-blocks single-changed)
    (message "Multi-line: %d blocks, text changed: %s" multi-blocks multi-changed)

    (if (and (= single-blocks 1) (= multi-blocks 0) single-changed (not multi-changed))
        (message "❌ HYPOTHESIS 3 CONFIRMED: Function has different code paths for different input types")
      (message "✅ HYPOTHESIS 3 REJECTED: Function behavior is consistent across input types"))))

(message "\n=== Code Path Investigation ===")

;; Add debug instrumentation to the function by temporarily redefining it
(message "\n--- Setting up debug instrumentation ---")

;; Save the original function
(fset 'efrit-tools-extract-tools-from-response-original
      (symbol-function 'efrit-tools-extract-tools-from-response))

;; Create a debug version with extensive logging
(defun efrit-tools-extract-tools-from-response (text)
  "Debug version with extensive logging."
  (message "🔍 DEBUG: Function called with text length: %d" (length text))
  (message "🔍 DEBUG: Text contains newlines: %s" (if (string-match-p "\n" text) "YES" "NO"))
  (message "🔍 DEBUG: First 50 chars: %S" (substring text 0 (min 50 (length text))))

  (unless (stringp text)
    (message "🔍 DEBUG: ERROR - Text is not a string!")
    (error "Response text must be a string"))

  (let ((results nil)
        (processed-text (or text ""))
        (elisp-regex "<elisp>\\(.*?\\)</elisp>"))

    (message "🔍 DEBUG: Initialized variables")
    (message "🔍 DEBUG: - results: %S" results)
    (message "🔍 DEBUG: - processed-text length: %d" (length processed-text))
    (message "🔍 DEBUG: - elisp-regex: %S" elisp-regex)

    (condition-case-unless-debug extraction-err
        (progn
          (message "🔍 DEBUG: Entering main processing block")

          ;; Test the regex first
          (message "🔍 DEBUG: Testing regex match...")
          (let ((regex-test-pos (string-match elisp-regex processed-text)))
            (message "🔍 DEBUG: Regex match result: %S" regex-test-pos))

          ;; Process Elisp evaluation requests using the while loop
          (message "🔍 DEBUG: Starting while loop for regex matches")
          (let ((loop-count 0))
            (while (string-match elisp-regex processed-text)
              (setq loop-count (1+ loop-count))
              (message "🔍 DEBUG: Loop iteration %d" loop-count)

              (let* ((elisp-code (match-string 1 processed-text))
                     (call-start (match-beginning 0))
                     (call-end (match-end 0)))

                (message "🔍 DEBUG: - elisp-code: %S" elisp-code)
                (message "🔍 DEBUG: - call-start: %d" call-start)
                (message "🔍 DEBUG: - call-end: %d" call-end)

                (let ((result (condition-case eval-err
                                  (efrit-tools-eval-sexp elisp-code)
                                (error
                                 (format "Error in Elisp evaluation: %s"
                                        (error-message-string eval-err))))))

                  (message "🔍 DEBUG: - evaluation result: %S" result)

                  ;; Add result to the list
                  (push result results)
                  (message "🔍 DEBUG: - results list now: %S" results)

                  ;; Replace the Elisp call with its result in the text
                  (setq processed-text
                        (concat (substring processed-text 0 call-start)
                                (format "[Result: %s]" result)
                                (substring processed-text call-end)))

                  (message "🔍 DEBUG: - processed-text length after replacement: %d" (length processed-text))
                  (message "🔍 DEBUG: - processed-text first 100 chars: %S"
                          (substring processed-text 0 (min 100 (length processed-text)))))))

            (message "🔍 DEBUG: While loop completed after %d iterations" loop-count)))

      ;; Handle extraction errors
      (error
       (message "🔍 DEBUG: ERROR in extraction: %s" (error-message-string extraction-err))
       (setq processed-text (concat processed-text
                                  "\n[Error processing tool calls: "
                                  (error-message-string extraction-err) "]"))))

    (message "🔍 DEBUG: Final results:")
    (message "🔍 DEBUG: - processed-text length: %d" (length processed-text))
    (message "🔍 DEBUG: - results list: %S" results)
    (message "🔍 DEBUG: - returning cons: %S" (cons processed-text (nreverse results)))

    ;; Return both the processed text and results
    (cons processed-text (nreverse results))))

;; Test both single-line and multi-line inputs with debug version
(message "\n--- Testing Single-line input with debug ---")
(let ((single-input "Test: <elisp>(+ 1 2)</elisp>"))
  (let ((result (efrit-tools-extract-tools-from-response single-input)))
    (message "Single-line result: %S" result)))

(message "\n--- Testing Multi-line input with debug ---")
(let ((multi-input "Test: <elisp>(with-current-buffer (get-buffer-create \"*test*\")
  (insert \"Hello\"))</elisp>"))
  (let ((result (efrit-tools-extract-tools-from-response multi-input)))
    (message "Multi-line result: %S" result)))

;; Restore the original function
(message "\n--- Restoring original function ---")
(fset 'efrit-tools-extract-tools-from-response
      (symbol-function 'efrit-tools-extract-tools-from-response-original))

(message "\n=== string-match Behavior Analysis ===")

;; Test the exact string-match behavior with our regex and inputs
(let ((elisp-regex "<elisp>\\(.*?\\)</elisp>")
      (single-input "Test: <elisp>(+ 1 2)</elisp>")
      (multi-input "Test: <elisp>(with-current-buffer (get-buffer-create \"*test*\")
  (insert \"Hello\"))</elisp>")
      (multi-simple "Test: <elisp>(message
\"hello\")</elisp>"))

  (message "\n--- Testing string-match with different inputs ---")
  (message "Regex pattern: %S" elisp-regex)

  ;; Test 1: Single-line input
  (message "\n🔍 Test 1: Single-line input")
  (message "Input: %S" single-input)
  (message "Input length: %d" (length single-input))
  (let ((match-pos (string-match elisp-regex single-input)))
    (message "string-match result: %S" match-pos)
    (when match-pos
      (message "match-beginning 0: %d" (match-beginning 0))
      (message "match-end 0: %d" (match-end 0))
      (message "match-string 0: %S" (match-string 0 single-input))
      (message "match-string 1: %S" (match-string 1 single-input))))

  ;; Test 2: Multi-line input (complex)
  (message "\n🔍 Test 2: Multi-line input (complex)")
  (message "Input: %S" multi-input)
  (message "Input length: %d" (length multi-input))
  (message "Input contains newlines: %s" (if (string-match-p "\n" multi-input) "YES" "NO"))
  (let ((match-pos (string-match elisp-regex multi-input)))
    (message "string-match result: %S" match-pos)
    (when match-pos
      (message "match-beginning 0: %d" (match-beginning 0))
      (message "match-end 0: %d" (match-end 0))
      (message "match-string 0: %S" (match-string 0 multi-input))
      (message "match-string 1: %S" (match-string 1 multi-input))))

  ;; Test 3: Multi-line input (simple)
  (message "\n🔍 Test 3: Multi-line input (simple)")
  (message "Input: %S" multi-simple)
  (message "Input length: %d" (length multi-simple))
  (message "Input contains newlines: %s" (if (string-match-p "\n" multi-simple) "YES" "NO"))
  (let ((match-pos (string-match elisp-regex multi-simple)))
    (message "string-match result: %S" match-pos)
    (when match-pos
      (message "match-beginning 0: %d" (match-beginning 0))
      (message "match-end 0: %d" (match-end 0))
      (message "match-string 0: %S" (match-string 0 multi-simple))
      (message "match-string 1: %S" (match-string 1 multi-simple))))

  ;; Test 4: Check if the issue is with the non-greedy matching
  (message "\n🔍 Test 4: Testing greedy vs non-greedy matching")
  (let ((greedy-regex "<elisp>\\(.*\\)</elisp>")
        (non-greedy-regex "<elisp>\\(.*?\\)</elisp>"))

    (message "Testing greedy regex: %S" greedy-regex)
    (let ((greedy-match (string-match greedy-regex multi-simple)))
      (message "Greedy match result: %S" greedy-match))

    (message "Testing non-greedy regex: %S" non-greedy-regex)
    (let ((non-greedy-match (string-match non-greedy-regex multi-simple)))
      (message "Non-greedy match result: %S" non-greedy-match)))

  ;; Test 5: Test with explicit newline in regex
  (message "\n🔍 Test 5: Testing regex with explicit newline handling")
  (let ((newline-regex "<elisp>\\([^<]*\\(?:\n[^<]*\\)*\\)</elisp>"))
    (message "Newline-aware regex: %S" newline-regex)
    (let ((newline-match (string-match newline-regex multi-simple)))
      (message "Newline-aware match result: %S" newline-match)
      (when newline-match
        (message "match-string 1: %S" (match-string 1 multi-simple)))))

  ;; Test 6: Test the exact character positions in multi-line input
  (message "\n🔍 Test 6: Character-by-character analysis of multi-line input")
  (message "Multi-simple input character analysis:")
  (dotimes (i (min 50 (length multi-simple)))
    (let ((char (aref multi-simple i)))
      (message "Position %d: %c (ASCII %d) %s"
               i char char
               (if (= char ?\n) "← NEWLINE" ""))))

  ;; Test 7: Manual search for opening and closing tags
  (message "\n🔍 Test 7: Manual tag search in multi-line input")
  (let ((open-pos (string-match "<elisp>" multi-simple))
        (close-pos (string-match "</elisp>" multi-simple)))
    (message "Manual <elisp> search: %S" open-pos)
    (message "Manual </elisp> search: %S" close-pos)
    (when (and open-pos close-pos)
      (let* ((start-content (+ open-pos 7))
             (content (substring multi-simple start-content close-pos)))
        (message "Content between tags: %S" content)
        (message "Content length: %d" (length content))
        (message "Content contains newlines: %s" (if (string-match-p "\n" content) "YES" "NO"))))))

(message "\n=== string-match Behavior Analysis Complete ===")
