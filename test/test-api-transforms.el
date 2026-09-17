;;; test-api-transforms.el --- request transforms and extra body -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'efrit-api)

(ert-deftest test-api-extra-body-merge-override-delete ()
  (let ((efrit-api-extra-body '(("service_tier" . "auto")
                                ("max_tokens" . 99)
                                ("temperature" . :delete))))
    (let ((out (efrit-api-apply-extra-body '(("model" . "m") ("max_tokens" . 8192)
                                             ("temperature" . 0.2)))))
      (should (equal (cdr (assoc "model" out)) "m"))
      (should (equal (cdr (assoc "max_tokens" out)) 99))
      (should (equal (cdr (assoc "service_tier" out)) "auto"))
      (should-not (assoc "temperature" out))))
  (let ((efrit-api-extra-body nil))
    (should (equal (efrit-api-apply-extra-body '(("a" . 1))) '(("a" . 1))))))

(ert-deftest test-api-transforms-chain-partial-and-error ()
  (let ((efrit-api-request-transforms
         (list
          ;; retarget only
          (lambda (req) (list :url (concat (plist-get req :url) "?x=1")))
          ;; broken hook: skipped
          (lambda (_req) (error "boom"))
          ;; nil: no change
          (lambda (_req) nil)
          ;; body rewrite based on previous state
          (lambda (req)
            (list :body (cons '("renamed" . t)
                              (assoc-delete-all "model" (copy-alist (plist-get req :body)))))))))
    (let ((req (efrit-api-apply-transforms "https://h/v1/messages"
                                           '(("h" . "v"))
                                           '(("model" . "m") ("k" . 1)))))
      (should (equal (plist-get req :url) "https://h/v1/messages?x=1"))
      (should (equal (plist-get req :headers) '(("h" . "v"))))
      (should-not (assoc "model" (plist-get req :body)))
      (should (assoc "renamed" (plist-get req :body)))
      (should (equal (cdr (assoc "k" (plist-get req :body))) 1)))))

(ert-deftest test-api-request-uses-transforms ()
  "url-retrieve must see the transformed url, headers and body."
  (let ((efrit-api-key "sk-test-key-1234567890abcdefghij")
        (efrit-api-auth-scheme 'x-api-key)
        (efrit-api-extra-body '(("service_tier" . "auto")))
        (efrit-api-request-transforms
         (list (lambda (req)
                 (list :url "https://gw.example.com/anthropic/v1/messages"
                       :headers (cons '("x-extra" . "1") (plist-get req :headers))))))
        seen-url seen-headers seen-data)
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (url &rest _)
                 (setq seen-url url
                       seen-headers url-request-extra-headers
                       seen-data url-request-data)
                 nil)))
      (efrit-api-request-async '(("model" . "m")) #'ignore #'ignore)
      (should (equal seen-url "https://gw.example.com/anthropic/v1/messages"))
      (should (assoc "x-extra" seen-headers))
      (should (assoc "x-api-key" seen-headers))
      (should (string-match-p "service_tier" seen-data)))))

(provide 'test-api-transforms)
;;; test-api-transforms.el ends here
