;;; fix-async-parens.el --- Fix parentheses in efrit-async.el -*- lexical-binding: t -*-

;; This script will analyze and fix the parentheses issue

(defun count-parens-in-defun (start-pos)
  "Count parentheses balance in defun starting at START-POS."
  (save-excursion
    (goto-char start-pos)
    (let ((balance 0)
          (end-pos (save-excursion 
                     (end-of-defun)
                     (point))))
      (while (< (point) end-pos)
        (let ((char (char-after)))
          (cond
           ((= char ?\() (cl-incf balance))
           ((= char ?\)) (cl-decf balance))))
        (forward-char))
      balance)))

(find-file "./lisp/efrit-async.el")
(goto-char (point-min))

;; Find efrit-async-execute-command
(search-forward "(defun efrit-async-execute-command")
(beginning-of-line)
(let ((start (point)))
  (message "Checking efrit-async-execute-command at line %d" (line-number-at-pos))
  (let ((balance (count-parens-in-defun start)))
    (message "Balance: %d (negative means too many closing parens)" balance)))

;; Count overall
(goto-char (point-min))
(let ((opens 0) (closes 0))
  (while (not (eobp))
    (let ((char (char-after)))
      (cond
       ((= char ?\() (cl-incf opens))
       ((= char ?\)) (cl-incf closes))))
    (forward-char))
  (message "\nTotal: %d open, %d close, diff=%d" opens closes (- opens closes)))

;; The fix: The issue is that efrit-async-execute-command has improper nesting
;; Let me trace through it:
;; 1. (defun efrit-async-execute-command (command callback)
;; 2.   (if efrit-async--active-session
;; 3.       (progn ...)  ;; true branch
;; 4.     ;; false branch starts here - this is where nesting gets complex
;; 5.     (let* ((context ...) (cache-key ...) (cached-response ...))
;; 6.       (if cached-response
;; 7.           (progn ...) ;; cache hit
;; 8.         ;; No cache hit - THE PROBLEM IS HERE
;; 9.         (let ((session-id ...))
;;            ;; This let needs to contain ALL the rest of the function
;;            ;; but currently it ends too early

(message "\nThe issue: The let binding for session-id ends but session-id is used later")
(message "This causes both a scope error AND parenthesis imbalance")