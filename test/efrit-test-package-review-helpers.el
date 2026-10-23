;;; efrit-test-package-review-helpers.el --- fake packages and canned reviewer replies -*- lexical-binding: t; -*-

;;; Commentary:
;; Shared by test-package-review and test-package-review-ui: a fake
;; installed package on disk, canned API responses (a text answer, a
;; read_package_file call), and a stub of the synchronous request.
;; Not a test file: the runner loads every test-*.el with -l, so a
;; test file required by another would define its tests twice.

;;; Code:

(require 'cl-lib)
(require 'package)

(defun test-pr--fake-package (dir name version files)
  "Write FILES ((NAME . TEXT)...) under DIR/NAME-VERSION and return (PKG-DIR . DESC)."
  (let ((pkg-dir (expand-file-name (format "%s-%s" name version) dir)))
    (make-directory pkg-dir t)
    (dolist (f files)
      (with-temp-file (expand-file-name (car f) pkg-dir) (insert (cdr f))))
    (cons pkg-dir
          (package-desc-create :name (intern name)
                               :version (version-to-list version)
                               :summary "test" :kind 'tar :archive "test-archive"
                               :dir pkg-dir))))

(defun test-pr--text-response (text)
  (let ((r (make-hash-table :test 'equal)) (item (make-hash-table :test 'equal))
        (u (make-hash-table :test 'equal)))
    (puthash "type" "text" item) (puthash "text" text item)
    (puthash "content" (vector item) r) (puthash "stop_reason" "end_turn" r)
    (puthash "input_tokens" 1 u) (puthash "usage" u r)
    r))

(defun test-pr--tool-response (id path &optional start end)
  (let ((r (make-hash-table :test 'equal)) (item (make-hash-table :test 'equal))
        (input (make-hash-table :test 'equal)))
    (puthash "path" path input)
    (when start (puthash "start_line" start input))
    (when end (puthash "end_line" end input))
    (puthash "type" "tool_use" item) (puthash "id" id item)
    (puthash "name" "read_package_file" item) (puthash "input" input item)
    (puthash "content" (vector item) r) (puthash "stop_reason" "tool_use" r)
    r))

(defmacro test-pr--with-responses (responses &rest body)
  "Run BODY with `efrit-api-request-sync' answering RESPONSES in order and
recording each request's messages in `test-pr--requests'."
  (declare (indent 1))
  `(let ((test-pr--queue ,responses) (test-pr--requests nil))
     (cl-letf (((symbol-function 'efrit-api-request-sync)
                (lambda (req &rest _)
                  (push (append (alist-get "messages" req nil nil #'equal) nil) test-pr--requests)
                  (or (pop test-pr--queue) (error "no more canned responses")))))
       ,@body)))

(defvar test-pr--queue nil)
(defvar test-pr--requests nil)

(defconst test-pr--approve
  "{\"verdict\":\"approve\",\"summary\":\"ok\",\"findings\":[],\"files_read\":[\"foo.el\"],\"saw_everything\":true}")

(provide 'efrit-test-package-review-helpers)
;;; efrit-test-package-review-helpers.el ends here
