;;; tool-protocol-fix-example.el --- Example of correct tool use protocol -*- lexical-binding: t; -*-

;; This file demonstrates the correct way to handle Claude's tool use protocol

;;; CURRENT (BROKEN) CODE:

(defun efrit--extract-content-and-tools-BROKEN (content)
  "Current broken implementation - executes tools but doesn't send results back."
  (let ((message-text ""))
    (when content
      (dotimes (i (length content))
        (let* ((item (aref content i))
               (type (gethash "type" item)))

          (when (string= type "tool_use")
            (let* ((tool-name (gethash "name" item))
                   (input (gethash "input" item))
                   ;; BUG: Not capturing tool-id!
                   )

              ;; Execute the tool
              (let ((result (efrit-tools-eval-sexp (gethash "expr" input))))
                ;; BUG: Result is ignored! Never sent back to Claude!
                (efrit-log-debug "Elisp result: %s" result)))))))
    message-text))

;;; FIXED CODE:

(defun efrit--extract-content-and-tools-FIXED (content)
  "Fixed implementation - properly handles tool use protocol."
  (let ((message-text "")
        (tool-results '()))  ;; Collect results to send back
    (when content
      (dotimes (i (length content))
        (let* ((item (aref content i))
               (type (gethash "type" item)))

          ;; Collect text content
          (when (string= type "text")
            (let ((text (gethash "text" item)))
              (when text
                (setq message-text (concat message-text text)))))

          ;; Handle tool uses
          (when (string= type "tool_use")
            (let* ((tool-name (gethash "name" item))
                   (input (gethash "input" item))
                   (tool-id (gethash "id" item)))  ;; FIX: Capture the ID!

              ;; Execute the tool
              (let ((result (condition-case err
                                (cond
                                 ((string= tool-name "eval_sexp")
                                  (efrit-tools-eval-sexp (gethash "expr" input)))
                                 ((string= tool-name "get_context")
                                  (efrit-tools-get-context))
                                 (t
                                  (format "Error: Unknown tool '%s'" tool-name)))
                              (error
                               (format "Error: %s" (error-message-string err))))))

                ;; FIX: Collect the result in proper format
                (push `((type . "tool_result")
                        (tool_use_id . ,tool-id)
                        (content . ,(format "%s" result)))
                      tool-results)))))))

    ;; Return both text and tool results
    (cons message-text (nreverse tool-results))))

(defun efrit--handle-api-response-FIXED (status)
  "Fixed API response handler that implements proper tool use protocol."
  (unless (efrit--process-http-status status)
    (condition-case _api-err
        (let ((content (efrit--parse-api-response)))
          (if content
              (with-current-buffer (efrit--setup-buffer)
                (let* ((result (efrit--extract-content-and-tools-FIXED content))
                       (message-text (car result))
                       (tool-results (cdr result)))

                  (if tool-results
                      ;; FIX: If tools were used, send results back to Claude first
                      (progn
                        (efrit-log-debug "Sending %d tool results back to Claude"
                                        (length tool-results))

                        ;; Build the tool_result message
                        (let ((tool-result-msg
                               `((role . "user")
                                 (content . ,(vconcat tool-results)))))

                          ;; Add assistant's text response to history
                          (when message-text
                            (push `((role . "assistant")
                                   (content . ,message-text))
                                  efrit--message-history))

                          ;; Add tool results to history
                          (push tool-result-msg efrit--message-history)

                          ;; Send back to Claude to continue
                          (efrit--send-api-request (reverse efrit--message-history))))

                    ;; No tools - just display the response normally
                    (efrit--update-ui-with-response message-text))))

            ;; Handle parse error
            (efrit--handle-parse-error)))
      (error (efrit--handle-parse-error)))))

;;; COMPLETE EXAMPLE: Building the API request

(defun efrit--build-api-request-with-tool-results (messages)
  "Build API request body with proper tool_result format in messages."
  (let ((formatted-messages
         (mapcar
          (lambda (msg)
            (let ((role (alist-get 'role msg))
                  (content (alist-get 'content msg)))

              ;; Check if content is an array of content blocks (for tool_result)
              (if (vectorp content)
                  ;; Multi-block content (e.g., with tool_result blocks)
                  `((role . ,role)
                    (content . ,content))

                ;; Simple text content
                `((role . ,role)
                  (content . ,content)))))
          messages)))

    `((model . ,efrit-model)
      (max_tokens . ,efrit-max-tokens)
      (temperature . ,efrit-temperature)
      (system . ,(efrit-tools-system-prompt))
      (messages . ,(vconcat formatted-messages))
      (tools . [
                (("name" . "eval_sexp")
                 ("description" . "Evaluate Elisp expression")
                 ("input_schema" . (("type" . "object")
                                    ("properties" . (("expr" . (("type" . "string")
                                                                ("description" . "Elisp code")))))
                                    ("required" . ["expr"]))))
                (("name" . "get_context")
                 ("description" . "Get Emacs environment context")
                 ("input_schema" . (("type" . "object")
                                    ("properties" . (("request" . (("type" . "string")))))
                                    ("required" . []))))
                ]))))

;;; EXAMPLE: What the actual API conversation looks like

;; Request 1: User asks to write fibonacci
'((role . "user")
  (content . "Write a function that computes fib(N) in the *scratch* buffer"))

;; Response 1: Claude uses tool
'((role . "assistant")
  (content . [((type . "text")
               (text . "I'll write a fibonacci function in the *scratch* buffer."))
              ((type . "tool_use")
               (id . "toolu_01234567890")
               (name . "eval_sexp")
               (input . ((expr . "(with-current-buffer \"*scratch*\" (insert \"(defun fibonacci (n)\\n  (if (< n 2) n (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))\"))"))))]))

;; Request 2: Send tool result back (THIS IS WHAT'S MISSING!)
'((role . "user")
  (content . [((type . "tool_result")
               (tool_use_id . "toolu_01234567890")
               (content . "#<buffer *scratch*>"))]))

;; Response 2: Claude confirms and continues
'((role . "assistant")
  (content . [((type . "text")
               (text . "I've written the fibonacci function to the *scratch* buffer."))]))

;;; This is how the conversation SHOULD flow, but currently doesn't!

(provide 'tool-protocol-fix-example)
;;; tool-protocol-fix-example.el ends here
