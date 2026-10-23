;;; test-models.el --- Tests for efrit-models -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'efrit-models)
(require 'efrit-api)

(ert-deftest test-models-error-recognition ()
  (dolist (m '("API Error (api_error): no keys found that support model: claude-sonnet-4-5"
               "API Error (not_found_error): model: foo"
               "HTTP 400: The model `x` does not exist"
               "unknown model bar" "model_not_found"))
    (should (efrit-models-model-error-p m)))
  (dolist (m '("HTTP error: (error http 401)" "rate_limit_error" nil ""))
    (should-not (efrit-models-model-error-p m))))

(ert-deftest test-models-parse-list-shapes ()
  (should (equal (efrit-models--parse-list
                  "{\"data\":[{\"id\":\"a\",\"type\":\"model\"},{\"id\":\"b\"}]}")
                 '("a" "b")))
  (should (equal (efrit-models--parse-list "{\"models\":[{\"name\":\"n1\"}]}") '("n1")))
  (should-not (efrit-models--parse-list "not json")))

(ert-deftest test-models-candidates-fallback-and-current-first ()
  (let ((efrit-default-model "mine") (efrit-models-fallback '("z" "a")))
    (cl-letf (((symbol-function 'efrit-models-list) (lambda (&rest _) nil)))
      (should (equal (efrit-models-candidates) '("mine" "z" "a"))))
    (cl-letf (((symbol-function 'efrit-models-list) (lambda (&rest _) '("q" "mine" "b"))))
      (should (equal (efrit-models-candidates) '("mine" "b" "q"))))))

(ert-deftest test-models-probe-ok-and-error ()
  (cl-letf (((symbol-function 'efrit-api-request-async)
             (lambda (req ok err)
               (if (equal (cdr (assoc "model" req)) "good")
                   (funcall ok (make-hash-table))
                 (funcall err "no keys found that support model: bad")))))
    (should (eq (efrit-models-probe "good") 'ok))
    (should (string-match-p "no keys" (efrit-models-probe "bad")))
    (should (equal (mapcar #'cdr (efrit-models-probe-all '("good" "bad")))
                   (list 'ok "no keys found that support model: bad")))))

(ert-deftest test-models-select-with-probe-filters-and-sets ()
  (let ((efrit-default-model "bad") (efrit-models-fallback '("good" "bad2")) (noninteractive t))
    (cl-letf (((symbol-function 'efrit-models-list) (lambda (&rest _) nil))
              ((symbol-function 'efrit-api-request-async)
               (lambda (req ok err)
                 (if (equal (cdr (assoc "model" req)) "good")
                     (funcall ok (make-hash-table))
                   (funcall err "unknown model"))))
              ((symbol-function 'completing-read)
               (lambda (_p choices &rest _) (should (equal choices '("good"))) "good")))
      (should (equal (efrit-select-model t) "good"))
      (should (equal efrit-default-model "good")))))

(ert-deftest test-models-select-rejects-freeform-that-fails ()
  (let ((efrit-default-model "m") (efrit-models-fallback '("m")) (noninteractive t))
    (cl-letf (((symbol-function 'efrit-models-list) (lambda (&rest _) nil))
              ((symbol-function 'efrit-api-request-async)
               (lambda (_req _ok err) (funcall err "unknown model")))
              ((symbol-function 'completing-read) (lambda (&rest _) "typed-in")))
      (should-error (efrit-select-model) :type 'user-error)
      (should (equal efrit-default-model "m")))))

(provide 'test-models)
;;; test-models.el ends here
