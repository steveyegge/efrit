;;; test-tool-elisp-docs.el --- Tests for elisp_docs tool -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;;; Commentary:
;; Tests for efrit-tool-elisp-docs structured documentation lookup.

;;; Code:

(require 'ert)
(require 'json)

;; Add load paths for test
(add-to-list 'load-path (expand-file-name "../lisp" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../lisp/core" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../lisp/tools" (file-name-directory load-file-name)))

(require 'efrit-tool-utils)
(require 'efrit-tool-elisp-docs)

;;; Helper functions

(defun test-elisp-docs--get-result (response)
  "Extract result from RESPONSE alist."
  (alist-get 'result response))

(defun test-elisp-docs--get-first-result (response)
  "Extract first result from RESPONSE."
  (aref (alist-get 'results (test-elisp-docs--get-result response)) 0))

(defun test-elisp-docs--success-p (response)
  "Check if RESPONSE indicates success."
  (eq (alist-get 'success response) t))

;;; Basic function lookup tests

(ert-deftest test-elisp-docs-function-basic ()
  "Test looking up a basic function."
  (let* ((response (efrit-tool-elisp-docs '((symbol . "car"))))
         (result (test-elisp-docs--get-first-result response)))
    (should (test-elisp-docs--success-p response))
    (should (eq (alist-get 'found result) t))
    (should (equal (alist-get 'type result) "function"))
    (should (equal (alist-get 'function_type result) "primitive"))
    (should (alist-get 'docstring result))
    (should (alist-get 'signature result))))

(ert-deftest test-elisp-docs-function-macro ()
  "Test looking up a macro."
  (let* ((response (efrit-tool-elisp-docs '((symbol . "when"))))
         (result (test-elisp-docs--get-first-result response)))
    (should (test-elisp-docs--success-p response))
    (should (eq (alist-get 'found result) t))
    (should (equal (alist-get 'type result) "function"))
    (should (equal (alist-get 'function_type result) "macro"))))

(ert-deftest test-elisp-docs-function-command ()
  "Test looking up an interactive command."
  (let* ((response (efrit-tool-elisp-docs '((symbol . "forward-char"))))
         (result (test-elisp-docs--get-first-result response)))
    (should (test-elisp-docs--success-p response))
    (should (eq (alist-get 'found result) t))
    (should (equal (alist-get 'type result) "function"))
    ;; forward-char is both primitive and command
    (should (member (alist-get 'function_type result) '("primitive" "command")))))

;;; Variable lookup tests

(ert-deftest test-elisp-docs-variable-basic ()
  "Test looking up a basic variable."
  (let* ((response (efrit-tool-elisp-docs '((symbol . "load-path"))))
         (result (test-elisp-docs--get-first-result response)))
    (should (test-elisp-docs--success-p response))
    (should (eq (alist-get 'found result) t))
    (should (equal (alist-get 'type result) "variable"))
    (should (alist-get 'docstring result))))

(ert-deftest test-elisp-docs-variable-user-option ()
  "Test looking up a customizable user option."
  (let* ((response (efrit-tool-elisp-docs '((symbol . "fill-column"))))
         (result (test-elisp-docs--get-first-result response)))
    (should (test-elisp-docs--success-p response))
    (should (eq (alist-get 'found result) t))
    (should (equal (alist-get 'type result) "variable"))
    (should (equal (alist-get 'variable_type result) "user-option"))))

;;; Face lookup tests

(ert-deftest test-elisp-docs-face-basic ()
  "Test looking up a face."
  (let* ((response (efrit-tool-elisp-docs '((symbol . "font-lock-keyword-face"))))
         (result (test-elisp-docs--get-first-result response)))
    (should (test-elisp-docs--success-p response))
    (should (eq (alist-get 'found result) t))
    (should (equal (alist-get 'type result) "face"))
    (should (alist-get 'docstring result))))

;;; Type filtering tests

(ert-deftest test-elisp-docs-type-filter-function ()
  "Test filtering by function type."
  (let* ((response (efrit-tool-elisp-docs '((symbol . "car")
                                            (type . "function"))))
         (result (test-elisp-docs--get-first-result response)))
    (should (test-elisp-docs--success-p response))
    (should (eq (alist-get 'found result) t))))

(ert-deftest test-elisp-docs-type-filter-mismatch ()
  "Test type filter mismatch returns error."
  (let* ((response (efrit-tool-elisp-docs '((symbol . "car")
                                            (type . "variable"))))
         (result (test-elisp-docs--get-first-result response)))
    (should (test-elisp-docs--success-p response))
    (should (eq (alist-get 'found result) :json-false))
    (should (alist-get 'error result))))

;;; Non-existent symbol tests

(ert-deftest test-elisp-docs-symbol-not-found ()
  "Test looking up non-existent symbol."
  (let* ((response (efrit-tool-elisp-docs '((symbol . "this-symbol-definitely-does-not-exist-xyz"))))
         (result (test-elisp-docs--get-first-result response)))
    (should (test-elisp-docs--success-p response))
    (should (eq (alist-get 'found result) :json-false))
    (should (equal (alist-get 'error result) "Symbol not found"))))

;;; Multiple symbols test

(ert-deftest test-elisp-docs-multiple-symbols ()
  "Test looking up multiple symbols at once."
  (let* ((response (efrit-tool-elisp-docs '((symbol . ("car" "cdr" "cons")))))
         (result (test-elisp-docs--get-result response))
         (results (alist-get 'results result)))
    (should (test-elisp-docs--success-p response))
    (should (= (length results) 3))
    (should (= (alist-get 'queried result) 3))
    ;; All should be found
    (should (seq-every-p (lambda (r)
                           (eq (alist-get 'found r) t))
                         results))))

;;; Include source location tests

(ert-deftest test-elisp-docs-include-source ()
  "Test include_source option returns source location."
  (let* ((response (efrit-tool-elisp-docs '((symbol . "json-encode")
                                            (include_source . t))))
         (result (test-elisp-docs--get-first-result response)))
    (should (test-elisp-docs--success-p response))
    (should (eq (alist-get 'found result) t))
    ;; Should have source_file (not all functions have line numbers)
    (should (alist-get 'source_file result))))

(ert-deftest test-elisp-docs-without-source ()
  "Test that source is not included by default."
  (let* ((response (efrit-tool-elisp-docs '((symbol . "json-encode"))))
         (result (test-elisp-docs--get-first-result response)))
    (should (test-elisp-docs--success-p response))
    (should-not (alist-get 'source_file result))))

;;; Related symbols tests

(ert-deftest test-elisp-docs-include-related ()
  "Test related option returns similar symbols."
  (let* ((response (efrit-tool-elisp-docs '((symbol . "car")
                                            (related . t))))
         (result (test-elisp-docs--get-first-result response)))
    (should (test-elisp-docs--success-p response))
    (should (eq (alist-get 'found result) t))
    ;; Should have related_symbols array
    (should (alist-get 'related_symbols result))
    (should (> (length (alist-get 'related_symbols result)) 0))))

(ert-deftest test-elisp-docs-without-related ()
  "Test that related symbols are not included by default."
  (let* ((response (efrit-tool-elisp-docs '((symbol . "car"))))
         (result (test-elisp-docs--get-first-result response)))
    (should (test-elisp-docs--success-p response))
    (should-not (alist-get 'related_symbols result))))

;;; Input validation tests

(ert-deftest test-elisp-docs-missing-symbol ()
  "Test that missing symbol looks up 'nil' which is special.
When no symbol is provided, it defaults to nil which is a special
case in Elisp - intern-soft returns nil for \"nil\" which looks
like 'not found' to the code."
  (let* ((response (efrit-tool-elisp-docs '()))
         (result (test-elisp-docs--get-first-result response)))
    ;; The tool succeeds but reports nil as "Symbol not found"
    ;; because of Elisp's nil ambiguity
    (should (test-elisp-docs--success-p response))
    (should (eq (alist-get 'found result) :json-false))))

(ert-deftest test-elisp-docs-invalid-type ()
  "Test that invalid type causes error."
  (let ((response (efrit-tool-elisp-docs '((symbol . "car")
                                           (type . "invalid-type")))))
    (should (eq (alist-get 'success response) :json-false))
    (should (alist-get 'error response))))

;;; Vector input test

(ert-deftest test-elisp-docs-vector-input ()
  "Test that vector input for symbols works."
  (let* ((response (efrit-tool-elisp-docs `((symbol . ,(vector "car" "cdr")))))
         (result (test-elisp-docs--get-result response)))
    (should (test-elisp-docs--success-p response))
    (should (= (alist-get 'queried result) 2))))

;;; JSON encoding test

(ert-deftest test-elisp-docs-json-encodable ()
  "Test that response is JSON encodable."
  (let ((response (efrit-tool-elisp-docs '((symbol . "car")
                                           (include_source . t)
                                           (related . t)))))
    (should (stringp (json-encode response)))))

(provide 'test-tool-elisp-docs)

;;; test-tool-elisp-docs.el ends here
