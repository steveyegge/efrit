;;; check-parens.el --- Check parentheses balance -*- lexical-binding: t; -*-

(find-file "./lisp/efrit-async.el")
(goto-char (point-min))

(let ((defuns '())
      (balance 0)
      (in-string nil)
      (in-comment nil))
  (while (not (eobp))
    (let ((char (char-after)))
      (cond
       ;; Skip strings
       ((and (= char ?\") (not in-comment))
        (setq in-string (not in-string)))
       ;; Skip comments  
       ((and (= char ?\;) (not in-string))
        (end-of-line))
       ;; Count parens when not in string/comment
       ((and (not in-string) (not in-comment))
        (cond
         ((= char ?\()
          (setq balance (1+ balance))
          (when (looking-at "(defun \\([^ ]+\\)")
            (push (list (match-string 1) (line-number-at-pos) balance) defuns)))
         ((= char ?\))
          (setq balance (1- balance))
          (when (and defuns (= balance (1- (nth 2 (car defuns)))))
            (message "Function %s ends at line %d" 
                     (nth 0 (car defuns))
                     (line-number-at-pos)))))))
    (forward-char))
  
  (message "\nFinal balance: %d" balance)
  (when (not (zerop balance))
    (message "UNBALANCED!")
    (dolist (fun (reverse defuns))
      (when (>= (nth 2 fun) balance)
        (message "Check function %s starting at line %d" (nth 0 fun) (nth 1 fun)))))))

(kill-emacs)