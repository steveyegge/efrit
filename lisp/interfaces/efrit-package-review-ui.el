;;; efrit-package-review-ui.el --- Review every installed package -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; `M-x efrit-review-all-packages' reviews the installed packages one
;; after another with `efrit-package-review-run-async' and collects
;; the verdicts in one `tabulated-list' buffer, worst first.  Each
;; row is one package: the verdict, the count of findings by
;; severity, the model that judged it.  RET opens the full report
;; (`efrit-package-review-render'), with its file buttons.
;;
;; A verdict is cached in `efrit-data-directory' under the package
;; name and version, so the second run only reviews what changed;
;; `r' re-reviews the row at point and `R' everything.  `k' stops the
;; queue after the request in flight.  The reviews go one at a time:
;; each is a conversation of several requests, and the API's rate
;; limits are per minute.
;;
;; The install-time review (`efrit-package-review-mode') judges a
;; package before it lands; this command judges what is already on
;; disk, for a periodic look at everything the Emacs runs.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'package)
(require 'tabulated-list)
(require 'efrit-log)
(require 'efrit-config)
(require 'efrit-settings)
(require 'efrit-package-review)
(require 'efrit-ui-helpers)

(defgroup efrit-package-review-ui nil
  "The batch review of installed packages."
  :group 'efrit-package-review)

(defcustom efrit-package-review-cache-file-name "package-reviews.json"
  "File under `efrit-data-directory' that keeps past verdicts by package version."
  :type 'string
  :group 'efrit-package-review-ui)

(defconst efrit-package-review-ui--buffer "*efrit-package-reviews*")

;;; Cache: name@version -> verdict plist (as JSON)

(defvar efrit-package-review-ui--cache nil
  "Hash table \"name@version\" -> verdict plist, or nil before the first read.")

(defun efrit-package-review-ui--cache-file ()
  (efrit-config-data-file efrit-package-review-cache-file-name))

(defun efrit-package-review-ui--key (name version)
  (format "%s@%s" name version))

(defun efrit-package-review-ui--verdict->json (verdict)
  "VERDICT as a hash table `json-serialize' accepts (symbols become strings)."
  (let ((h (make-hash-table :test 'equal)))
    (puthash "verdict" (symbol-name (plist-get verdict :verdict)) h)
    (puthash "summary" (or (plist-get verdict :summary) "") h)
    (puthash "saw_everything" (if (plist-get verdict :saw-everything) t :false) h)
    (puthash "model" (or (plist-get verdict :model) "") h)
    (puthash "reads" (vconcat (plist-get verdict :reads)) h)
    (when (plist-get verdict :raw) (puthash "raw" (plist-get verdict :raw) h))
    (puthash "findings"
             (vconcat (mapcar (lambda (f)
                                (let ((fh (make-hash-table :test 'equal)))
                                  (puthash "severity" (symbol-name (plist-get f :severity)) fh)
                                  (puthash "file" (or (plist-get f :file) :null) fh)
                                  (puthash "line" (or (plist-get f :line) :null) fh)
                                  (puthash "note" (or (plist-get f :note) "") fh)
                                  fh))
                              (plist-get verdict :findings)))
             h)
    h))

(defun efrit-package-review-ui--json->verdict (obj)
  "The verdict plist for the cached JSON object OBJ, or nil when malformed."
  (when (hash-table-p obj)
    (let ((verdict (intern (or (gethash "verdict" obj) "error"))))
      (list :verdict verdict
            :summary (or (gethash "summary" obj) "")
            :saw-everything (eq (gethash "saw_everything" obj) t)
            :model (gethash "model" obj)
            :raw (gethash "raw" obj)
            :reads (append (gethash "reads" obj) nil)
            :findings
            (delq nil
                  (mapcar (lambda (f)
                            (when (hash-table-p f)
                              (let ((sev (intern (or (gethash "severity" f) "info"))))
                                (when (memq sev efrit-package-review-severities)
                                  (list :severity sev
                                        :file (let ((v (gethash "file" f))) (and (stringp v) v))
                                        :line (let ((v (gethash "line" f))) (and (numberp v) v))
                                        :note (or (gethash "note" f) ""))))))
                          (append (gethash "findings" obj) nil)))))))

(defun efrit-package-review-ui--cache ()
  "The cache table, read from disk on first use.  Never signals."
  (or efrit-package-review-ui--cache
      (setq efrit-package-review-ui--cache
            (let ((file (efrit-package-review-ui--cache-file))
                  (table (make-hash-table :test 'equal)))
              (when (file-readable-p file)
                (condition-case err
                    (let ((obj (with-temp-buffer
                                 (insert-file-contents file)
                                 (json-parse-buffer :object-type 'hash-table :array-type 'list))))
                      (when (hash-table-p obj)
                        (maphash (lambda (k v)
                                   (when-let* ((verdict (efrit-package-review-ui--json->verdict v)))
                                     (puthash k verdict table)))
                                 obj)))
                  (error (efrit-log 'warn "package reviews cache %s: unreadable (%s)"
                                    file (error-message-string err)))))
              table))))

(defun efrit-package-review-ui--cache-put (name version verdict)
  "Remember VERDICT for NAME VERSION and write the cache (mode 0600).
A failed review (no verdict) is not kept: the next run tries again."
  (unless (eq (plist-get verdict :verdict) 'error)
    (puthash (efrit-package-review-ui--key name version) verdict (efrit-package-review-ui--cache))
    (let ((file (efrit-package-review-ui--cache-file))
          (obj (make-hash-table :test 'equal)))
      (maphash (lambda (k v) (puthash k (efrit-package-review-ui--verdict->json v) obj))
               (efrit-package-review-ui--cache))
      (efrit-settings-write-json file obj))))

(defun efrit-package-review-ui--cached (name version)
  (gethash (efrit-package-review-ui--key name version) (efrit-package-review-ui--cache)))

;;; The queue

(defvar efrit-package-review-ui--rows nil
  "Alist NAME -> plist (:desc :version :status :verdict :info).
Status is one of pending, running, done, cached, skipped.")

(defvar efrit-package-review-ui--queue nil
  "Package names still to review, in order.")

(defvar efrit-package-review-ui--current nil
  "The `efrit-package-review-state' in flight, or nil.")

(defvar efrit-package-review-ui--stopped nil
  "Non-nil after `efrit-package-review-ui-stop': the queue does not advance.")

(defun efrit-package-review-ui--row (name) (alist-get name efrit-package-review-ui--rows))

(defun efrit-package-review-ui--set (name &rest props)
  "Set PROPS on NAME's row."
  (let ((row (efrit-package-review-ui--row name)))
    (while props
      (setq row (plist-put row (car props) (cadr props)) props (cddr props)))
    (setf (alist-get name efrit-package-review-ui--rows) row)))

(defun efrit-package-review-ui--installed ()
  "The installed packages as (NAME . DESC), user packages first, by name."
  (sort (mapcar (lambda (p) (cons (car p) (cadr p))) package-alist)
        (lambda (a b) (string< (symbol-name (car a)) (symbol-name (car b))))))

(defun efrit-package-review-ui--start (names force)
  "Queue NAMES for review; FORCE ignores the cache."
  (setq efrit-package-review-ui--stopped nil)
  (dolist (name names)
    (let* ((row (efrit-package-review-ui--row name))
           (version (plist-get row :version))
           (cached (and (not force) (efrit-package-review-ui--cached name version))))
      (if cached
          (efrit-package-review-ui--set name :status 'cached :verdict cached)
        (efrit-package-review-ui--set name :status 'pending :verdict nil)
        (unless (memq name efrit-package-review-ui--queue)
          (setq efrit-package-review-ui--queue (append efrit-package-review-ui--queue (list name)))))))
  (efrit-package-review-ui--redraw)
  ;; a review is in flight only while its row says so; a `current'
  ;; left behind by a callback that failed (or a reload) must not
  ;; keep the queue from moving
  (unless (efrit-package-review-ui--running-p)
    (setq efrit-package-review-ui--current nil)
    (efrit-package-review-ui--next)))

(defun efrit-package-review-ui--running-p ()
  "Non-nil if a review is in flight: a state is held and a row is `running'."
  (and efrit-package-review-ui--current
       (cl-some (lambda (row) (eq (plist-get (cdr row) :status) 'running))
                efrit-package-review-ui--rows)))

(defun efrit-package-review-ui--next ()
  "Review the next queued package, or announce the end."
  (let ((name (and (not efrit-package-review-ui--stopped)
                   (pop efrit-package-review-ui--queue))))
    (cond
     ((null name)
      (setq efrit-package-review-ui--current nil)
      (efrit-package-review-ui--redraw)
      (message "efrit: package reviews %s (%s)"
               (if efrit-package-review-ui--stopped "stopped" "done")
               (efrit-package-review-ui--totals)))
     (t
      (let* ((row (efrit-package-review-ui--row name))
             (desc (plist-get row :desc))
             (dir (package-desc-dir desc)))
        (if (not (and dir (file-directory-p dir)))
            (progn
              (efrit-package-review-ui--set name :status 'skipped
                                            :verdict (list :verdict 'error :summary "package directory is missing"))
              (efrit-package-review-ui--next))
          (let ((info (efrit-package-review-gather desc dir nil)))
            (efrit-package-review-ui--set name :status 'running :info info)
            (efrit-package-review-ui--redraw)
            (setq efrit-package-review-ui--current
                  (efrit-package-review-run-async
                   info
                   (lambda (verdict)
                     ;; whatever happens to this verdict, the queue moves on
                     (unwind-protect
                         (let ((verdict (plist-put verdict :model (efrit-package-review-model))))
                           (efrit-log 'info "package review %s %s: %s" name (plist-get info :version)
                                      (efrit-package-review-verdict-line verdict))
                           (efrit-package-review-ui--set name :status 'done :verdict verdict)
                           (condition-case err
                               (efrit-package-review-ui--cache-put name (plist-get info :version) verdict)
                             (error (efrit-log 'warn "package reviews cache: %s" (error-message-string err)))))
                       (setq efrit-package-review-ui--current nil)
                       (efrit-package-review-ui--next))))))))))))

(defun efrit-package-review-ui--totals ()
  "\"3 rejected, 40 approved, 2 failed\" over the rows with a verdict."
  (let ((rejected 0) (approved 0) (failed 0))
    (dolist (row efrit-package-review-ui--rows)
      (pcase (plist-get (plist-get (cdr row) :verdict) :verdict)
        ('approve (cl-incf approved))
        ('error (cl-incf failed))
        ('nil nil)
        (_ (cl-incf rejected))))
    (format "%d rejected, %d approved, %d failed" rejected approved failed)))

;;; The list

(defface efrit-package-review-ui-pending
  '((t :inherit shadow))
  "Rows not yet reviewed."
  :group 'efrit-package-review-ui)

(defun efrit-package-review-ui--severity-rank (verdict)
  "A sort key: rejected with high findings first, approved clean last."
  (let ((v (plist-get verdict :verdict)))
    (cond
     ((null v) 90)
     ((eq v 'error) 80)
     (t
      (let ((worst (cl-position-if
                    (lambda (sev) (cl-some (lambda (f) (eq (plist-get f :severity) sev))
                                           (plist-get verdict :findings)))
                    efrit-package-review-severities)))
        (+ (if (eq v 'approve) 40 0)
           (or worst (length efrit-package-review-severities))))))))

(defun efrit-package-review-ui--entry (name row)
  "The `tabulated-list' entry for NAME's ROW."
  (let* ((verdict (plist-get row :verdict))
         (status (plist-get row :status))
         (badge (pcase status
                  ('running (propertize "reviewing" 'face 'efrit-package-review-ui-pending))
                  ('pending (propertize "queued" 'face 'efrit-package-review-ui-pending))
                  ('skipped (propertize "skipped" 'face 'efrit-package-review-ui-pending))
                  (_ (efrit-ui-badge (efrit-package-review--verdict-word verdict)
                                     (efrit-package-review--verdict-face verdict)))))
         (findings (if verdict (efrit-package-review--counts-text verdict) ""))
         (summary (if verdict
                      (truncate-string-to-width
                       (replace-regexp-in-string "\n+" " " (or (plist-get verdict :summary) ""))
                       80 nil nil "…")
                    "")))
    (list name
          (vector badge
                  (symbol-name name)
                  (or (plist-get row :version) "")
                  (if (eq status 'cached) (concat findings " (cached)") findings)
                  summary))))

(defun efrit-package-review-ui--entries ()
  (mapcar (lambda (row) (efrit-package-review-ui--entry (car row) (cdr row)))
          (sort (copy-sequence efrit-package-review-ui--rows)
                (lambda (a b)
                  (let ((ra (efrit-package-review-ui--severity-rank (plist-get (cdr a) :verdict)))
                        (rb (efrit-package-review-ui--severity-rank (plist-get (cdr b) :verdict))))
                    (if (= ra rb)
                        (string< (symbol-name (car a)) (symbol-name (car b)))
                      (< ra rb)))))))

(defun efrit-package-review-ui--redraw ()
  "Redraw the list buffer if it is live, keeping point on its row."
  (when-let* ((buf (get-buffer efrit-package-review-ui--buffer)))
    (with-current-buffer buf
      (let ((at (tabulated-list-get-id)))
        (setq tabulated-list-entries (efrit-package-review-ui--entries))
        (setq header-line-format (efrit-package-review-ui--header))
        (tabulated-list-print t)
        (when at
          (goto-char (point-min))
          (while (and (not (eobp)) (not (eq (tabulated-list-get-id) at)))
            (forward-line 1))
          (when (eobp) (goto-char (point-min))))))))

(defun efrit-package-review-ui--header ()
  (format " efrit package reviews · %s%s   RET report · r/R re-review one/all · k stop · g refresh · ? help · q close"
          (efrit-package-review-ui--totals)
          (if efrit-package-review-ui--current
              (format ", %d queued" (1+ (length efrit-package-review-ui--queue)))
            "")))

(defvar efrit-package-review-ui-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'efrit-package-review-ui-show)
    (define-key map (kbd "r") #'efrit-package-review-ui-rereview)
    (define-key map (kbd "R") #'efrit-package-review-ui-rereview-all)
    (define-key map (kbd "k") #'efrit-package-review-ui-stop)
    (define-key map (kbd "g") #'efrit-package-review-ui-refresh)
    (define-key map (kbd "?") #'efrit-package-review-ui-help)
    map)
  "Keys of `efrit-package-review-ui-mode'.")

(define-derived-mode efrit-package-review-ui-mode tabulated-list-mode "Efrit-Package-Reviews"
  "Verdicts of efrit's review of every installed package.  \\{efrit-package-review-ui-mode-map}"
  (setq tabulated-list-format [("Verdict" 11 nil) ("Package" 24 t) ("Version" 14 nil)
                               ("Findings" 24 nil) ("Summary" 0 nil)])
  (setq tabulated-list-padding 1)
  (tabulated-list-init-header))

(defun efrit-package-review-ui--name-at-point ()
  (or (tabulated-list-get-id) (user-error "No package on this line")))

(defun efrit-package-review-ui-show ()
  "Open the full report for the package at point."
  (interactive)
  (let* ((name (efrit-package-review-ui--name-at-point))
         (row (efrit-package-review-ui--row name))
         (verdict (plist-get row :verdict)))
    (unless verdict (user-error "%s has not been reviewed yet" name))
    (let ((info (or (plist-get row :info)
                    ;; a cached verdict: rebuild what the report needs
                    ;; without a new gather (no diff, no changelog)
                    (let ((desc (plist-get row :desc)))
                      (list :name (symbol-name name) :version (plist-get row :version)
                            :dir (package-desc-dir desc)
                            :files (mapcar (lambda (f) (cons f 0))
                                           (ignore-errors
                                             (efrit-package-review--source-files (package-desc-dir desc)))))))))
      (efrit-show-popup (format "*efrit-package-review: %s*" name)
                        (lambda () (efrit-package-review-render info verdict))))))

(defun efrit-package-review-ui-rereview ()
  "Review the package at point again, ignoring the cache."
  (interactive)
  (efrit-package-review-ui--start (list (efrit-package-review-ui--name-at-point)) t))

(defun efrit-package-review-ui-rereview-all ()
  "Review every package again, ignoring the cache."
  (interactive)
  (when (yes-or-no-p (format "Re-review all %d packages? " (length efrit-package-review-ui--rows)))
    (efrit-package-review-ui--start (mapcar #'car efrit-package-review-ui--rows) t)))

(defun efrit-package-review-ui-stop ()
  "Stop after the review in flight; queued packages stay queued."
  (interactive)
  (setq efrit-package-review-ui--stopped t)
  (when efrit-package-review-ui--current
    (efrit-package-review-cancel efrit-package-review-ui--current))
  (message "efrit: stopping after the current review; r or R starts again"))

(defun efrit-package-review-ui-refresh ()
  "Redraw; pick up packages installed since the buffer was made."
  (interactive)
  (efrit-package-review-ui--sync-rows)
  (efrit-package-review-ui--redraw))

(defun efrit-package-review-ui-help ()
  "Describe the keys."
  (interactive)
  (describe-keymap 'efrit-package-review-ui-mode-map))

(defun efrit-package-review-ui--sync-rows ()
  "Add a row for each installed package not yet listed; drop removed ones."
  (let ((installed (efrit-package-review-ui--installed)))
    (dolist (p installed)
      (unless (efrit-package-review-ui--row (car p))
        (setf (alist-get (car p) efrit-package-review-ui--rows)
              (list :desc (cdr p)
                    :version (package-version-join (package-desc-version (cdr p)))
                    :status 'pending))))
    (setq efrit-package-review-ui--rows
          (sort (cl-remove-if-not (lambda (row) (assq (car row) installed)) efrit-package-review-ui--rows)
                (lambda (a b) (string< (symbol-name (car a)) (symbol-name (car b))))))))

;;;###autoload
(defun efrit-review-all-packages (&optional force)
  "Review every installed package with efrit; collect the verdicts in one buffer.
Packages already reviewed at their installed version are shown from
the cache; with FORCE (the prefix argument) everything is reviewed
again.  Reviews run one at a time in the background; the buffer
updates as verdicts arrive."
  (interactive "P")
  (unless package-alist (user-error "No packages are installed"))
  (efrit-package-review-ui--sync-rows)
  (let ((buf (get-buffer-create efrit-package-review-ui--buffer)))
    (with-current-buffer buf
      (unless (derived-mode-p 'efrit-package-review-ui-mode)
        (efrit-package-review-ui-mode)))
    (pop-to-buffer buf)
    (efrit-package-review-ui--start (mapcar #'car efrit-package-review-ui--rows) force)))

(provide 'efrit-package-review-ui)

;;; efrit-package-review-ui.el ends here
