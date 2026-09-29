;;; test-ask.el --- efrit-ask: one-shot side requests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'efrit-ask)

(defun test-ask--response (text &optional stop)
  "A fake API response hash with one text block TEXT."
  (let ((h (make-hash-table :test 'equal))
        (block (make-hash-table :test 'equal)))
    (puthash "type" "text" block)
    (puthash "text" text block)
    (puthash "content" (vector block) h)
    (puthash "stop_reason" (or stop "end_turn") h)
    h))

(defmacro test-ask--with-api (responder &rest body)
  "Run BODY with `efrit-api-request-async' replaced by RESPONDER.
RESPONDER is called with (REQUEST CALLBACK ERROR-CALLBACK) and decides."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'efrit-api-request-async) ,responder))
     (let ((efrit-ask--versions (make-hash-table :test 'equal)))
       ,@body)))

(ert-deftest test-ask-once-answers-once-and-off-buffer ()
  "The callback runs once with the text; the request goes from the hidden buffer."
  (let ((got nil) (from nil))
    (test-ask--with-api (lambda (_req cb _err)
                          (setq from (buffer-name))
                          (funcall cb (test-ask--response "forty-two"))
                          ;; a second delivery (retry path) must not reach the caller
                          (funcall cb (test-ask--response "again")))
      (efrit-ask-once "answer" (lambda (text msg) (push (cons text msg) got)) :purpose "test")
      (should (equal '(("forty-two" . nil)) got))
      (should (equal efrit-ask--buffer-name from)))))

(ert-deftest test-ask-once-failures-and-empty ()
  (let ((got nil))
    (test-ask--with-api (lambda (_req _cb err) (funcall err "boom"))
      (efrit-ask-once "x" (lambda (text msg) (setq got (cons text msg))))
      (should (equal '(nil . "boom") got)))
    (test-ask--with-api (lambda (_req cb _err) (funcall cb (test-ask--response "" "max_tokens")))
      (efrit-ask-once "x" (lambda (text msg) (setq got (cons text msg))))
      (should (null (car got)))
      (should (string-match-p "cut at max_tokens" (cdr got))))))

(ert-deftest test-ask-newer-ask-supersedes-older ()
  "Two asks with the same key: the first reply is dropped as superseded,
the second is delivered; a different key is independent."
  (let ((pending nil) (got nil))
    (test-ask--with-api (lambda (_req cb _err) (push cb pending))
      (efrit-ask-once "first" (lambda (text msg) (push (list 'a text msg) got)) :key "k")
      (efrit-ask-once "second" (lambda (text msg) (push (list 'b text msg) got)) :key "k")
      (efrit-ask-once "other" (lambda (text msg) (push (list 'c text msg) got)) :key "other")
      ;; replies arrive in order
      (let ((cbs (reverse pending)))
        (funcall (nth 0 cbs) (test-ask--response "one"))
        (funcall (nth 1 cbs) (test-ask--response "two"))
        (funcall (nth 2 cbs) (test-ask--response "three")))
      (should (equal '((a nil "superseded") (b "two" nil) (c "three" nil)) (reverse got))))))

(ert-deftest test-ask-cancel-drops-the-reply ()
  (let ((pending nil) (got nil))
    (test-ask--with-api (lambda (_req cb _err) (setq pending cb))
      (let ((ask (efrit-ask-once "x" (lambda (&rest r) (push r got)))))
        (should (efrit-ask-cancel ask))
        (funcall pending (test-ask--response "late"))
        (should-not got)))))

(ert-deftest test-ask-candidates-split-and-fences ()
  (should (equal '("a" "b c" "d")
                 (efrit-ask-split-candidates
                  (format "a\n%s\n```\nb c\n```\n%s\n\nd\n%s"
                          efrit-ask-candidate-separator efrit-ask-candidate-separator
                          efrit-ask-candidate-separator))))
  (should (equal "x" (efrit-ask-strip-fence "```elisp\nx\n```")))
  (should (equal "keep ``` inside" (efrit-ask-strip-fence "keep ``` inside")))
  (let ((got nil) (sent nil))
    (test-ask--with-api (lambda (req cb _err)
                          (setq sent (alist-get "content" (aref (alist-get "messages" req nil nil #'equal) 0) nil nil #'equal))
                          (funcall cb (test-ask--response (format "one\n%s\ntwo" efrit-ask-candidate-separator))))
      (efrit-ask-candidates "say a word" 2 (lambda (c msg) (setq got (cons c msg))))
      (should (equal '(("one" "two") . nil) got))
      (should (string-match-p "Give 2 different candidates" sent)))))

(provide 'test-ask)
;;; test-ask.el ends here
