;;; test-reload.el --- efrit-reload -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'efrit-reload)
(require 'efrit-log)
(require 'efrit-sandbox)

(ert-deftest test-reload-features-are-efrit-oldest-first ()
  (let ((fs (efrit-reload-features)))
    (should (cl-every (lambda (f) (string-prefix-p "efrit" (symbol-name f))) fs))
    (should-not (memq 'efrit-reload fs))
    ;; efrit-log was required before efrit-sandbox above, and sandbox
    ;; requires log, so log must come first in reload order
    (should (< (cl-position 'efrit-log fs) (cl-position 'efrit-sandbox fs)))))

(ert-deftest test-reload-reloads-and-keeps-settings ()
  "A reload re-evaluates definitions but a customized value survives."
  (let ((efrit-sandbox-default-project-grants '(read write)))
    (defvar test-reload--marker nil)
    (setq test-reload--marker 'before)
    ;; a defvar in a reloaded file does not clobber a bound variable
    (should (> (efrit-reload) 0))
    (should (equal efrit-sandbox-default-project-grants '(read write)))
    (should (fboundp 'efrit-sandbox-check))
    (should (featurep 'efrit-sandbox))))

(ert-deftest test-reload-refreshes-keymaps ()
  "A binding added to the source reaches a live buffer after reload.
Keymap defvars are skipped on plain `load'; efrit-reload unbinds them
first and re-points the minor-mode alist and mode buffers."
  (require 'efrit-agent)
  (require 'efrit-agent-input)
  (efrit)
  (with-current-buffer (efrit-agent--get-buffer)
    ;; simulate a map defined before these keys existed
    (define-key efrit-agent-input-mode-map (kbd "<up>") nil)
    (define-key efrit-agent-mode-map (kbd "C-c C-m") nil)
    (goto-char (point-max)) (efrit-agent--maybe-enable-input-mode)
    (should-not (eq (key-binding (kbd "<up>")) 'efrit-agent-input-up))
    (efrit-reload)
    (efrit-agent--maybe-enable-input-mode)
    (should (eq (key-binding (kbd "<up>")) 'efrit-agent-input-up))
    (should (eq (key-binding (kbd "C-c C-m")) 'efrit-menu))
    (should (eq (cdr (assq 'efrit-agent-input-mode minor-mode-map-alist))
                efrit-agent-input-mode-map))))

(ert-deftest test-reload-updates-itself-first ()
  "An old efrit-reload (plain loads, no self-reload) still ends up running
the new code: the new definition reloads efrit-reload before the rest."
  (require 'efrit-agent) (require 'efrit-agent-input)
  (let ((real (symbol-function 'efrit-reload)))
    (unwind-protect
        (progn
          ;; the new definition's entry point re-dispatches after loading
          ;; itself; simulate a session where only the old body exists
          (fmakunbound 'efrit-reload--run)
          (efrit)
          (with-current-buffer (efrit-agent--get-buffer)
            (define-key efrit-agent-input-mode-map (kbd "<up>") nil)
            (goto-char (point-max)) (efrit-agent--maybe-enable-input-mode)
            (should-not (eq (key-binding [up]) 'efrit-agent-input-up))
            (funcall real)
            (efrit-agent--maybe-enable-input-mode)
            (should (fboundp 'efrit-reload--run))
            (should (eq (key-binding [up]) 'efrit-agent-input-up))))
      (fset 'efrit-reload real))))

(ert-deftest test-reload-reverts-unmodified-visiting-buffers ()
  "A buffer visiting a reloaded library is reverted if the file changed on disk
and the buffer has no unsaved edits; a modified buffer is left alone."
  (let* ((dir (file-name-as-directory (make-temp-file "efrit-reload-" t)))
         (file-a (expand-file-name "efrit-reload-probe-a.el" dir))
         (file-b (expand-file-name "efrit-reload-probe-b.el" dir))
         (clean nil) (dirty nil))
    (unwind-protect
        (let ((load-path (cons dir load-path)))
          (with-temp-file file-a (insert ";;; a -*- lexical-binding: t -*-\n(provide 'efrit-reload-probe-a)\n"))
          (with-temp-file file-b (insert ";;; b -*- lexical-binding: t -*-\n(provide 'efrit-reload-probe-b)\n"))
          (require 'efrit-reload-probe-a)
          (require 'efrit-reload-probe-b)
          (setq clean (find-file-noselect file-a)
                dirty (find-file-noselect file-b))
          (with-current-buffer dirty (goto-char (point-max)) (insert ";; local edit\n"))
          ;; both files change on disk (as an editor or git would)
          (sleep-for 1.1)
          (with-temp-file file-a (insert ";;; a v2 -*- lexical-binding: t -*-\n(provide 'efrit-reload-probe-a)\n"))
          (with-temp-file file-b (insert ";;; b v2 -*- lexical-binding: t -*-\n(provide 'efrit-reload-probe-b)\n"))
          (let ((efrit-reload-revert-buffers t))
            (should (= 1 (efrit-reload--revert-visiting-buffers))))
          (with-current-buffer clean
            (should (string-match-p "a v2" (buffer-string)))
            (should-not (buffer-modified-p)))
          (with-current-buffer dirty
            (should (buffer-modified-p))
            (should (string-match-p "local edit" (buffer-string)))
            (should-not (string-match-p "b v2" (buffer-string)))))
      (dolist (b (list dirty clean))
        (when (buffer-live-p b) (with-current-buffer b (set-buffer-modified-p nil)) (kill-buffer b)))
      (setq features (cl-set-difference features '(efrit-reload-probe-a efrit-reload-probe-b)))
      (delete-directory dir t))))

(ert-deftest test-reload-adopts-changed-defaults-unless-user-set ()
  "A defcustom whose default changed in the source takes the new default
after reload when the live value was still the old default; a value the
user set is kept.  This is why an edited display action reached a live
Emacs only after a restart."
  (let* ((dir (file-name-as-directory (make-temp-file "efrit-reload-" t)))
         (file (expand-file-name "efrit-reload-probe-opt.el" dir))
         (write (lambda (v)
                  (with-temp-file file
                    (insert ";;; opt -*- lexical-binding: t -*-\n"
                            (format "(defcustom efrit-reload-probe-untouched '%S \"\" :type 'sexp)\n" v)
                            (format "(defcustom efrit-reload-probe-user-set '%S \"\" :type 'sexp)\n" v)
                            "(provide 'efrit-reload-probe-opt)\n")))))
    (unwind-protect
        (let ((load-path (cons dir load-path)))
          (funcall write '(a . 1))
          (require 'efrit-reload-probe-opt)
          (set 'efrit-reload-probe-user-set '(mine . 9))
          (let ((before (efrit-reload-option-defaults)))
            (should (equal (cdr (assq 'efrit-reload-probe-untouched before)) '(a . 1)))
            (funcall write '(b . 2))
            (load file nil t)
            ;; plain load keeps both old values: the reason for this step
            (should (equal (symbol-value 'efrit-reload-probe-untouched) '(a . 1)))
            (should (equal (efrit-reload--refresh-changed-defaults before)
                           '(efrit-reload-probe-untouched)))
            (should (equal (symbol-value 'efrit-reload-probe-untouched) '(b . 2)))
            (should (equal (symbol-value 'efrit-reload-probe-user-set) '(mine . 9)))
            ;; nothing to do the second time round
            (should-not (efrit-reload--refresh-changed-defaults (efrit-reload-option-defaults)))))
      (makunbound 'efrit-reload-probe-untouched)
      (makunbound 'efrit-reload-probe-user-set)
      (setq features (delq 'efrit-reload-probe-opt features))
      (delete-directory dir t))))

(ert-deftest test-reload-redefines-lazy-transient-menus ()
  "A transient prefix defined behind an fboundp guard is void after the
unbind step, so the guard re-fires and the new keys appear."
  (skip-unless (require 'transient nil t))
  (require 'efrit-permissions-ui)
  (should (fboundp 'efrit-permissions-grant-menu))
  (should (memq 'efrit-permissions-grant-menu (efrit-reload-transient-prefixes)))
  (efrit-reload--unbind-transient-prefixes)
  (should-not (fboundp 'efrit-permissions-grant-menu))
  ;; the reload brings it back, with RET bound
  (efrit-reload)
  (should (fboundp 'efrit-permissions-grant-menu))
  (should (transient-get-suffix 'efrit-permissions-grant-menu "RET")))

(ert-deftest test-reload-keeps-global-minor-modes-on ()
  "A global minor mode that was on before the reload is on after it,
with its advice in place."
  (require 'efrit-package-review)
  (skip-unless (fboundp 'package-review))
  (unwind-protect
      (progn
        (efrit-package-review-mode 1)
        (should efrit-package-review-mode)
        (should (advice-member-p #'efrit-package-review--around 'package-review))
        (should (assq 'efrit-package-review-mode (efrit-reload-global-minor-modes)))
        (efrit-reload)
        (should efrit-package-review-mode)
        (should (advice-member-p #'efrit-package-review--around 'package-review)))
    (efrit-package-review-mode -1)))

(provide 'test-reload)
;;; test-reload.el ends here
