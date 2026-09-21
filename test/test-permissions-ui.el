;;; test-permissions-ui.el --- the permissions editor -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'efrit-permissions-ui)

(defvar efrit-project-root)

(defmacro test-perm--in-project (&rest body)
  "Run BODY with a fresh temp project as the sandbox root and clean tables."
  (declare (indent 0))
  `(let* ((root (file-name-as-directory (make-temp-file "efrit-perm-" t)))
          (efrit-project-root root)
          (efrit-data-directory (expand-file-name "data" root))
          (efrit-sandbox-enabled t)
          (efrit-sandbox-default-project-grants '(read))
          (efrit-sandbox-request-function nil)
          (efrit-sandbox--session-grants (make-hash-table :test 'equal))
          (efrit-sandbox--project-grants (make-hash-table :test 'equal))
          (efrit-sandbox--once-grant nil)
          (efrit-sandbox-store--loaded (make-hash-table :test 'equal))
          (efrit-settings--cache (make-hash-table :test 'equal))
          (efrit-limits--session (make-hash-table :test 'equal))
          (efrit-limits--once (make-hash-table :test 'equal))
          (efrit-review-enabled t)
          (efrit-review-classes '(write exec net)))
     (unwind-protect
         (cl-letf (((symbol-function 'pop-to-buffer) #'ignore))
           ,@body)
       (when (get-buffer efrit-permissions--buffer) (kill-buffer efrit-permissions--buffer))
       (delete-directory root t))))

(defun test-perm--rows (kind)
  "The row ids of KIND in the editor buffer."
  (with-current-buffer efrit-permissions--buffer
    (cl-remove-if-not (lambda (id) (eq (plist-get id :kind) kind))
                      (mapcar #'car tabulated-list-entries))))

(defun test-perm--goto (pred)
  "Move point to the first row whose id satisfies PRED."
  (with-current-buffer efrit-permissions--buffer
    (goto-char (point-min))
    (while (and (not (eobp)) (not (funcall pred (tabulated-list-get-id))))
      (forward-line 1))
    (should (funcall pred (tabulated-list-get-id)))))

(ert-deftest test-perm-lists-grants-defaults-review-limits-per-project ()
  (test-perm--in-project
    (efrit-sandbox-grant 'write root 'project)
    (efrit-sandbox-grant 'shell '(shell "git" "make") 'session)
    (efrit-sandbox root)
    (with-current-buffer efrit-permissions--buffer
      (should (derived-mode-p 'efrit-permissions-mode))
      (should efrit-permissions--only-focus)
      (should (= 2 (length (test-perm--rows 'grant))))
      (should (= 1 (length (test-perm--rows 'default))))
      (should (= 1 (length (test-perm--rows 'review))))
      (should (= (length efrit-limits-known) (length (test-perm--rows 'limit))))
      ;; single-project view has no global rows
      (should-not (test-perm--rows 'global))
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "git, make" text))
        (should (string-match-p "review *on: write exec net *global" text))
        (should (string-match-p "default grants *read *global" text)))
      ;; A toggles to every project + the global section
      (efrit-permissions-toggle-all-projects)
      (should (test-perm--rows 'global))
      (should (cl-find 'efrit-permission-policy (test-perm--rows 'global)
                       :key (lambda (id) (plist-get id :var)))))))

(ert-deftest test-perm-marks-and-bulk-revoke ()
  (test-perm--in-project
    (efrit-sandbox-grant 'write root 'project)
    (efrit-sandbox-grant 'elisp t 'session)
    (efrit-sandbox-grant 'net t 'session)
    (efrit-sandbox root)
    (with-current-buffer efrit-permissions--buffer
      ;; d on a non-grant row is refused
      (test-perm--goto (lambda (id) (eq (plist-get id :kind) 'default)))
      (should-error (efrit-permissions-revoke) :type 'user-error)
      ;; mark elisp and net, D revokes both, write stays
      (test-perm--goto (lambda (id) (and (eq (plist-get id :kind) 'grant)
                                         (eq (plist-get (plist-get id :grant) :cap) 'elisp))))
      (efrit-permissions-mark)
      (test-perm--goto (lambda (id) (and (eq (plist-get id :kind) 'grant)
                                         (eq (plist-get (plist-get id :grant) :cap) 'net))))
      (efrit-permissions-mark)
      (should (= 2 (length efrit-permissions--marks)))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
        (efrit-permissions-revoke-marked))
      (should (= 1 (length (test-perm--rows 'grant))))
      (should-not (efrit-sandbox-allowed-p 'elisp t))
      (should (efrit-sandbox-allowed-p 'write (expand-file-name "f" root))))))

(ert-deftest test-perm-grant-menu-actions-change-scope-and-target ()
  "The per-row actions: session<->project, widen, narrow, revoke."
  (test-perm--in-project
    (let ((sub (file-name-as-directory (expand-file-name "sub" root))))
      (make-directory sub)
      (efrit-sandbox-grant 'write sub 'session)
      (efrit-sandbox root)
      (with-current-buffer efrit-permissions--buffer
        (test-perm--goto (lambda (id) (eq (plist-get id :kind) 'grant)))
        (setq efrit-permissions--row (tabulated-list-get-id))
        ;; to project: persisted
        (efrit-permissions-grant-to-project)
        (should (file-exists-p (efrit-sandbox-store-file root)))
        (should (eq 'project (plist-get (car (efrit-sandbox-grants root)) :scope)))
        ;; widen: parent directory
        (test-perm--goto (lambda (id) (eq (plist-get id :kind) 'grant)))
        (setq efrit-permissions--row (tabulated-list-get-id))
        (efrit-permissions-grant-widen)
        (should (equal (plist-get (car (efrit-sandbox-grants root)) :target)
                       (efrit-sandbox-canonical root)))
        ;; narrow back to sub
        (test-perm--goto (lambda (id) (eq (plist-get id :kind) 'grant)))
        (setq efrit-permissions--row (tabulated-list-get-id))
        (cl-letf (((symbol-function 'read-directory-name) (lambda (&rest _) sub)))
          (efrit-permissions-grant-narrow))
        (should (equal (plist-get (car (efrit-sandbox-grants root)) :target)
                       (efrit-sandbox-canonical sub)))
        ;; back to session: file no longer lists it
        (test-perm--goto (lambda (id) (eq (plist-get id :kind) 'grant)))
        (setq efrit-permissions--row (tabulated-list-get-id))
        (efrit-permissions-grant-to-session)
        (should (eq 'session (plist-get (car (efrit-sandbox-grants root)) :scope)))
        (clrhash efrit-sandbox--project-grants)
        (should-not (efrit-sandbox-store-load root))
        ;; revoke
        (test-perm--goto (lambda (id) (eq (plist-get id :kind) 'grant)))
        (setq efrit-permissions--row (tabulated-list-get-id))
        (efrit-permissions-grant-revoke)
        (should-not (efrit-sandbox-grants root))))))

(ert-deftest test-perm-shell-grant-widen-and-narrow ()
  (test-perm--in-project
    (efrit-sandbox-grant 'shell '(shell "git") 'session)
    (efrit-sandbox root)
    (with-current-buffer efrit-permissions--buffer
      (test-perm--goto (lambda (id) (eq (plist-get id :kind) 'grant)))
      (setq efrit-permissions--row (tabulated-list-get-id))
      (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "git make sed")))
        (efrit-permissions-grant-narrow))
      (should (equal (plist-get (car (efrit-sandbox-grants root)) :target) '(shell "git" "make" "sed")))
      (should (efrit-sandbox-allowed-p 'shell "make | sed s/a/b/"))
      (test-perm--goto (lambda (id) (eq (plist-get id :kind) 'grant)))
      (setq efrit-permissions--row (tabulated-list-get-id))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
        (efrit-permissions-grant-widen))
      (should (eq t (plist-get (car (efrit-sandbox-grants root)) :target)))
      ;; the wide grant is flagged in the listing
      (test-perm--goto (lambda (id) (eq (plist-get id :kind) 'grant)))
      (should (string-match-p "any command" (buffer-substring (line-beginning-position) (line-end-position)))))))

(ert-deftest test-perm-project-policy-rows-write-settings ()
  "Default grants, review and limits edits land in .efrit/settings.json and
read back; reset removes the override."
  (test-perm--in-project
    (efrit-sandbox root)
    (with-current-buffer efrit-permissions--buffer
      ;; default grants: add write
      (test-perm--goto (lambda (id) (eq (plist-get id :kind) 'default)))
      (setq efrit-permissions--row (tabulated-list-get-id))
      (efrit-permissions-default-toggle 'write)
      (should (equal (efrit-sandbox-effective-default-grants root) '(read write)))
      (should (efrit-sandbox-allowed-p 'write (expand-file-name "f" root)))
      ;; remove read: an empty override is still an override
      (efrit-permissions-default-toggle 'read)
      (efrit-permissions-default-toggle 'write)
      (should (equal (efrit-sandbox-effective-default-grants root) nil))
      (should-not (efrit-sandbox-allowed-p 'read (expand-file-name "f" root)))
      (efrit-permissions-default-reset)
      (should (equal (efrit-sandbox-effective-default-grants root) '(read)))
      ;; review: off for this project, then a narrower class list
      (test-perm--goto (lambda (id) (eq (plist-get id :kind) 'review)))
      (setq efrit-permissions--row (tabulated-list-get-id))
      (efrit-permissions-review-toggle)
      (should-not (efrit-review-enabled-p root))
      (efrit-permissions-review-toggle)
      (efrit-permissions-review-toggle-class 'net)
      (should (equal (efrit-review-effective-classes root) '(write exec)))
      (efrit-settings-forget)
      (should (equal (efrit-review-effective-classes root) '(write exec)))
      (efrit-permissions-review-reset)
      (should (equal (efrit-review-effective-classes root) '(write exec net)))
      ;; limits: project value, session value, reset
      (test-perm--goto (lambda (id) (and (eq (plist-get id :kind) 'limit)
                                         (eq (plist-get id :name) 'max-iterations))))
      (setq efrit-permissions--row (tabulated-list-get-id))
      (cl-letf (((symbol-function 'read-number) (lambda (&rest _) 250)))
        (efrit-permissions-limit-set-project))
      (should (= 250 (efrit-limits-effective 'max-iterations 100 root)))
      (cl-letf (((symbol-function 'read-number) (lambda (&rest _) 300)))
        (efrit-permissions-limit-set-session))
      (should (= 300 (efrit-limits-effective 'max-iterations 100 root)))
      (efrit-permissions-limit-reset)
      (should (= 100 (efrit-limits-effective 'max-iterations 100 root)))
      ;; the editor shows the scope column change
      (let ((text (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "max-iterations *[0-9]+ *global" text))))))

(ert-deftest test-perm-all-projects-view-and-registry ()
  "Projects that got a setting or a grant show up from the registry."
  (test-perm--in-project
    (let ((other (file-name-as-directory (expand-file-name "other" root))))
      (make-directory other)
      (efrit-limits-set 'max-iterations 5 'project other)
      (efrit-sandbox-grant 'write root 'project)
      (efrit-permissions root)
      (with-current-buffer efrit-permissions--buffer
        (should-not efrit-permissions--only-focus)
        (let ((heads (mapcar (lambda (id) (plist-get id :root)) (test-perm--rows 'project))))
          ;; focus first, other project present, global heading (nil) last
          (should (equal (car heads) root))
          (should (member other heads))
          (should (null (car (last heads)))))
        ;; RET on the other project's heading moves focus
        (test-perm--goto (lambda (id) (and (eq (plist-get id :kind) 'project)
                                           (equal (plist-get id :root) other))))
        (efrit-permissions-edit)
        (should (equal efrit-permissions--focus other))
        (should (equal (plist-get (car (test-perm--rows 'project)) :root) other))))))

(ert-deftest test-perm-global-rows-toggle-and-legacy-layer ()
  (test-perm--in-project
    (efrit-permissions root)
    (with-current-buffer efrit-permissions--buffer
      (test-perm--goto (lambda (id) (eq (plist-get id :var) 'efrit-review-enabled)))
      (setq efrit-permissions--row (tabulated-list-get-id))
      (let ((efrit-review-enabled t))
        (efrit-permissions-global-toggle)
        (should-not efrit-review-enabled)
        (efrit-permissions-global-toggle)
        (should efrit-review-enabled))
      ;; non-boolean refuses the toggle
      (test-perm--goto (lambda (id) (eq (plist-get id :var) 'efrit-review-classes)))
      (setq efrit-permissions--row (tabulated-list-get-id))
      (should-error (efrit-permissions-global-toggle) :type 'user-error)
      ;; the legacy layer row says it is inactive while the sandbox is on
      (test-perm--goto (lambda (id) (eq (plist-get id :var) 'efrit-permission-policy)))
      (should (string-match-p "inactive" (buffer-substring (line-beginning-position) (line-end-position))))
      (let ((efrit-sandbox-enabled nil) (efrit-permission-policy '(write exec)))
        (efrit-permissions-refresh)
        (test-perm--goto (lambda (id) (eq (plist-get id :var) 'efrit-permission-policy)))
        (should (string-match-p "write exec" (buffer-substring (line-beginning-position) (line-end-position))))))))

(ert-deftest test-perm-markable-rows-are-visibly-different-and-ret-closes-menus ()
  (test-perm--in-project
    (efrit-sandbox-grant 'write root 'project)
    (efrit-sandbox root)
    (with-current-buffer efrit-permissions--buffer
      (test-perm--goto (lambda (id) (eq (plist-get id :kind) 'grant)))
      (should (string-prefix-p (concat " " efrit-permissions-markable-glyph)
                               (buffer-substring (line-beginning-position) (line-end-position))))
      (efrit-permissions-mark)
      (test-perm--goto (lambda (id) (eq (plist-get id :kind) 'grant)))
      (should (string-prefix-p (concat " " efrit-permissions-marked-glyph)
                               (buffer-substring (line-beginning-position) (line-end-position))))
      (test-perm--goto (lambda (id) (eq (plist-get id :kind) 'review)))
      (should-not (string-match-p (regexp-quote efrit-permissions-markable-glyph)
                                  (buffer-substring (line-beginning-position) (line-end-position))))
      ;; every row menu binds RET to done
      (when (efrit-permissions--define-menus)
        (dolist (menu '(efrit-permissions-grant-menu efrit-permissions-default-menu
                        efrit-permissions-review-menu efrit-permissions-limit-menu
                        efrit-permissions-global-menu))
          (let ((suffix (transient-get-suffix menu "RET")))
            (should suffix)
            (should (eq (plist-get (cdr suffix) :command) 'efrit-permissions-menu-done))))))))

(ert-deftest test-perm-eval-cannot-reach-the-editor ()
  (require 'efrit-sandbox-eval)
  (should (efrit-sandbox-eval-inspect '(efrit-permissions-grant-to-project)))
  (should (efrit-sandbox-eval-inspect '(efrit-settings-put "/" "review" nil))))

(provide 'test-permissions-ui)
;;; test-permissions-ui.el ends here
