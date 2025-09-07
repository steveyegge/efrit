;;; stub-file-3 --- Fixed -*- lexical-binding: t; -*-
;; Third stub file without proper header
(defmacro test-macro-3 (body)
  "A test macro."
  `(progn
     (message "Executing macro 3")
     ,body))

(defun test-function-3 ()
  "Third test function."
  (test-macro-3
    (message "Hello from stub file 3")))

(provide 'stub-file-3)
;;; stub-file-3.el ends here
