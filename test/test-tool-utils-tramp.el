;;; test-tool-utils-tramp.el --- Sandbox and process helpers under Tramp -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercises `efrit-resolve-path' and the remote-aware process helpers
;; with local paths and with Tramp's built-in "mock" method, which
;; runs a shell on localhost through the full Tramp file-name-handler
;; machinery without needing ssh.

;;; Code:

(require 'ert)
(require 'tramp)   ; must precede ert-x, which defines the "mock"
(require 'ert-x)   ; method only when tramp is already loaded (Emacs 29+)
(require 'efrit-tool-utils)

(defvar ert-remote-temporary-file-directory)

(defmacro test-tramp--with-root (root &rest body)
  "Run BODY with `efrit-project-root' bound to ROOT and sandbox on."
  (declare (indent 1))
  `(let ((efrit-project-root ,root)
         (efrit-project-sandbox t))
     ,@body))

(defun test-tramp--mock-available-p ()
  (and (boundp 'ert-remote-temporary-file-directory)
       (stringp ert-remote-temporary-file-directory)
       (file-remote-p ert-remote-temporary-file-directory)
       (ignore-errors (file-directory-p ert-remote-temporary-file-directory))))

(defmacro test-tramp--mock-dir ()
  "Create and return a fresh temp dir as a /mock: Tramp path.
Skips the enclosing test when the mock method is unavailable;
must be expanded inside an `ert-deftest' body."
  `(progn
     (skip-unless (test-tramp--mock-available-p))
     (let ((tmp (make-temp-file
                 (expand-file-name "efrit-tramp-"
                                   ert-remote-temporary-file-directory)
                 t)))
       (should (file-remote-p tmp))
       tmp)))

;;; Local behaviour is unchanged

(ert-deftest test-tramp-local-relative-path ()
  (let ((root (make-temp-file "efrit-local-" t)))
    (test-tramp--with-root root
      (let ((info (efrit-resolve-path "a/b.el")))
        (should (string= (plist-get info :path)
                         (expand-file-name "a/b.el" (file-truename root))))
        (should (string= (plist-get info :path-relative) "a/b.el"))
        (should-not (plist-get info :remote))))))

(ert-deftest test-tramp-local-outside-root-signals ()
  (let ((root (make-temp-file "efrit-local-" t)))
    (test-tramp--with-root root
      (should-error (efrit-resolve-path "/etc/passwd")
                    :type 'efrit-sandbox-violation))))

;;; Remote root

(ert-deftest test-tramp-remote-root-relative-path ()
  (let ((root (test-tramp--mock-dir)))
    (test-tramp--with-root root
      (let ((info (efrit-resolve-path "src/x.el")))
        (should (file-remote-p (plist-get info :path)))
        (should (string= (plist-get info :remote) (file-remote-p root)))
        (should (string= (plist-get info :path-relative) "src/x.el"))
        (should (string-suffix-p "/src/x.el" (plist-get info :path)))))))

(ert-deftest test-tramp-remote-root-absolute-local-name-goes-to-remote ()
  "A bare absolute path under a remote root means that path on the remote host."
  (let* ((root (test-tramp--mock-dir))
         (local (file-remote-p root 'localname)))
    (test-tramp--with-root root
      (let ((info (efrit-resolve-path (concat local "/inside.txt"))))
        (should (string= (plist-get info :remote) (file-remote-p root)))
        (should (string= (plist-get info :path-relative) "inside.txt"))))))

(ert-deftest test-tramp-remote-root-rejects-other-host ()
  "A path on a different remote is outside the sandbox even with a matching localname.
The mock method only accepts localhost names, so the other host is
expressed via a different method (/sudo:) on the same host; the
remote identity still differs, which is what the sandbox compares.
No connection is opened: the path does not exist, so
`efrit-resolve-path' never calls `file-truename' on it."
  (let* ((root (test-tramp--mock-dir))
         (local (file-remote-p root 'localname)))
    (test-tramp--with-root root
      (should-error (efrit-resolve-path
                     (concat "/sudo::" local "/does-not-exist"))
                    :type 'efrit-sandbox-violation))))

(ert-deftest test-tramp-remote-root-rejects-local-escape ()
  "Under a remote root, /etc/passwd resolves remotely, and is still outside."
  (let ((root (test-tramp--mock-dir)))
    (test-tramp--with-root root
      (should-error (efrit-resolve-path "/etc/passwd")
                    :type 'efrit-sandbox-violation))))

(ert-deftest test-tramp-local-root-rejects-remote-path ()
  "A local root never contains a remote path."
  (let ((root (make-temp-file "efrit-local-" t))
        (remote (test-tramp--mock-dir)))
    (test-tramp--with-root root
      (should-error (efrit-resolve-path
                     (concat (file-remote-p remote) root "/f"))
                    :type 'efrit-sandbox-violation))))

(ert-deftest test-tramp-path-in-directory-p-host-mismatch ()
  (should-not (efrit-tool--path-in-directory-p "/mock:a:/tmp/x" "/mock:b:/tmp/"))
  (should-not (efrit-tool--path-in-directory-p "/tmp/x" "/mock:b:/tmp/"))
  (should-not (efrit-tool--path-in-directory-p "/mock:b:/tmp/x" "/tmp/")))

;;; Process helpers

(ert-deftest test-tramp-executable-find-remote ()
  (let ((root (test-tramp--mock-dir)))
    (should (efrit-tool-executable-find "sh" root))
    (should-not (efrit-tool-executable-find "no-such-program-efrit" root))))

(ert-deftest test-tramp-call-process-runs-remotely ()
  "process-file under a /mock: default-directory sees the remote cwd."
  (let* ((root (test-tramp--mock-dir))
         (default-directory (file-name-as-directory root)))
    (with-temp-buffer
      (should (eq 0 (efrit-tool-call-process "sh" nil t nil "-c" "pwd")))
      (should (string= (string-trim (buffer-string))
                       (file-truename (file-remote-p root 'localname)))))))

(ert-deftest test-tramp-run-git-remote ()
  "efrit-tool-run-git runs on the host of the project root."
  (skip-unless (executable-find "git"))
  (let ((root (test-tramp--mock-dir)))
    (let ((default-directory root))
      (process-file "git" nil nil nil "init" "-q"))
    (test-tramp--with-root root
      (let ((r (efrit-tool-run-git '("rev-parse" "--is-inside-work-tree"))))
        (should (plist-get r :success))
        (should (string= (string-trim (plist-get r :output)) "true"))))))

(ert-deftest test-tramp-make-temp-file-remote ()
  (let* ((root (test-tramp--mock-dir))
         (default-directory (file-name-as-directory root))
         (tmp (efrit-tool-make-temp-file "efrit-t-")))
    (unwind-protect
        (progn (should (file-remote-p tmp))
               (should (file-exists-p tmp)))
      (delete-file tmp))))

(ert-deftest test-tramp-local-name ()
  (should (string= (efrit-tool-local-name "/mock:h:/a/b") "/a/b"))
  (should (string= (efrit-tool-local-name "/a/b") "/a/b")))

(provide 'test-tool-utils-tramp)
;;; test-tool-utils-tramp.el ends here
