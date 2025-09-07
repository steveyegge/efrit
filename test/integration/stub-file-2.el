;; Another file missing lexical binding cookie
(defcustom test-variable-2 "default"
  "A test variable."
  :type 'string
  :group 'test)

(defun test-function-2 ()
  "Another test function."
  (message "Hello from stub file 2: %s" test-variable-2))

(provide 'stub-file-2)
;;; stub-file-2.el ends here
