;;; test-tool-registry.el --- Tests for efrit-tool-registry -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'efrit-tool-registry)
(require 'efrit-do-schema)
(require 'efrit-do-dispatch)
(require 'efrit-permissions)

(defmacro test-registry--fresh (&rest body)
  "Run BODY with an empty registry, restoring it afterwards."
  (declare (indent 0))
  `(let ((efrit-tool-registry nil))
     ,@body))

(ert-deftest test-registry-register-and-schema ()
  "A registered tool appears after efrit's own tools in the schema,
with its name, description and input schema; unregistering removes it."
  (test-registry--fresh
    (let ((before (length (efrit-do--get-current-tools-schema))))
      (efrit-register-tool "gmail_search"
                           :description "Search Gmail"
                           :input-schema '(("type" . "object")
                                           ("properties" . (("query" . (("type" . "string")))))
                                           ("required" . ["query"]))
                           :function #'ignore :class 'read :package 'nngmail)
      (let* ((schema (efrit-do--get-current-tools-schema))
             (last (aref schema (1- (length schema)))))
        (should (= (1+ before) (length schema)))
        (should (equal "gmail_search" (alist-get "name" last nil nil #'equal)))
        (should (equal "Search Gmail" (alist-get "description" last nil nil #'equal)))
        (should (equal ["query"] (alist-get "required" (alist-get "input_schema" last nil nil #'equal)
                                            nil nil #'equal))))
      (should (efrit-unregister-tool "gmail_search"))
      (should (= before (length (efrit-do--get-current-tools-schema))))
      (should-not (efrit-unregister-tool "gmail_search")))))

(ert-deftest test-registry-dispatch-and-class ()
  "Dispatch reaches the registered function with an alist input; a tool
error becomes an error result; the permission class is the registered one."
  (test-registry--fresh
    (let (seen)
      (efrit-register-tool "gmail_messages"
                           :function (lambda (input) (setq seen input) "three messages")
                           :class 'read :package 'nngmail)
      (efrit-register-tool "gmail_label"
                           :function (lambda (_) (error "no such label"))
                           :class 'write :package 'nngmail)
      (let ((input (make-hash-table :test #'equal)))
        (puthash "ids" '("a" "b") input)
        (should (equal "three messages" (efrit-do--dispatch-tool "gmail_messages" input nil)))
        (should (equal '(("ids" "a" "b")) seen)))
      (should (string-match-p "no such label" (efrit-do--dispatch-tool "gmail_label" nil nil)))
      (should (string-match-p "Unknown tool" (efrit-do--dispatch-tool "gmail_nothing" nil nil)))
      (should (eq 'read (efrit-permission-tool-class "gmail_messages")))
      (should (eq 'write (efrit-permission-tool-class "gmail_label")))
      (should (eq 'read (efrit-permission-tool-class "never_heard_of"))))))

(ert-deftest test-registry-refuses-bad-registrations ()
  "Bad names, missing functions, unknown classes and collisions with
efrit's own tools are refused."
  (test-registry--fresh
    (should-error (efrit-register-tool "Gmail" :function #'ignore))
    (should-error (efrit-register-tool "gmail_x"))
    (should-error (efrit-register-tool "gmail_x" :function #'ignore :class 'root))
    (should-error (efrit-register-tool "eval_sexp" :function #'ignore))
    (should (null efrit-tool-registry))))

(ert-deftest test-registry-package-withdrawal ()
  "`efrit-unregister-package-tools' removes only that package's tools."
  (test-registry--fresh
    (efrit-register-tool "a_one" :function #'ignore :package 'a)
    (efrit-register-tool "a_two" :function #'ignore :package 'a)
    (efrit-register-tool "b_one" :function #'ignore :package 'b)
    (should (equal '("a_one" "a_two") (sort (efrit-unregister-package-tools 'a) #'string<)))
    (should (equal '("b_one") (mapcar #'car efrit-tool-registry)))))

(provide 'test-tool-registry)
;;; test-tool-registry.el ends here
