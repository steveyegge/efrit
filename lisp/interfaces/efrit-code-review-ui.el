;;; efrit-code-review-ui.el --- The findings of a code review, queue and apply -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.11.1
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, vc, ai

;;; Commentary:

;; `efrit-code-review' (M-x, the `C' entry of `efrit-menu', or `R' in
;; Magit's diff transient when Magit is loaded) reviews the staged
;; changes (with a prefix: unpushed commits, or the branch) and shows
;; the findings in `*efrit-code-review*': one block per finding with
;; its file and lines, title, description, and for a suggestion the
;; patch efrit made, fontified by `diff-mode'.  Comments are read;
;; suggestions are queued with `m' (`u' unqueues, `x' dismisses) and
;; applied together with `A'.  Applying refuses when the change set
;; moved since the review (the scope hash differs), because the
;; patches were made against what was reviewed.  `RET' opens the file
;; at the finding; `g' reviews again; `n'/`p' move; `q' closes.
;;
;; A review that already ran for this exact change set is shown from
;; its saved copy without a request; `g' asks anew.

;;; Code:

(require 'cl-lib)
(require 'diff-mode)
(require 'efrit-code-review)
(require 'efrit-ui-helpers)

(declare-function efrit-markdown--fontify-string "efrit-markdown")
(declare-function magit-toplevel "magit-git")
(declare-function transient-append-suffix "transient")

(defgroup efrit-code-review-ui nil
  "The code review buffer."
  :group 'efrit-code-review)

(defconst efrit-code-review-ui--buffer-name "*efrit-code-review*")

(defface efrit-code-review-title '((t :inherit bold :height 1.1))
  "The scope line at the top." :group 'efrit-code-review-ui)
(defface efrit-code-review-file '((t :inherit font-lock-function-name-face :weight bold))
  "A finding's file and lines." :group 'efrit-code-review-ui)
(defface efrit-code-review-suggestion
  '((((background dark)) :foreground "#1b1b1b" :background "#8ab4f8")
    (t :foreground "white" :background "#3b6ea5"))
  "Badge of a suggestion." :group 'efrit-code-review-ui)
(defface efrit-code-review-comment
  '((((background dark)) :foreground "#1b1b1b" :background "#e6b422")
    (t :foreground "#1b1b1b" :background "#f0c040"))
  "Badge of a comment." :group 'efrit-code-review-ui)
(defface efrit-code-review-lgtm
  '((((background dark)) :foreground "#1b1b1b" :background "#8fbc8f")
    (t :foreground "white" :background "#2e8b57"))
  "Badge of an all-clear." :group 'efrit-code-review-ui)
(defface efrit-code-review-queued '((t :inherit success :weight bold))
  "The state of a queued finding." :group 'efrit-code-review-ui)
(defface efrit-code-review-done '((t :inherit shadow :strike-through t))
  "A finding that was applied or dismissed." :group 'efrit-code-review-ui)
(defface efrit-code-review-invalid '((t :inherit error))
  "A finding whose patch did not apply." :group 'efrit-code-review-ui)
(defface efrit-code-review-meta '((t :inherit shadow))
  "Model, time, rounds; hints." :group 'efrit-code-review-ui)

(defvar-local efrit-code-review-ui--result nil "The result plist shown.")
(defvar-local efrit-code-review-ui--state nil "The review in flight, or nil.")
(defvar-local efrit-code-review-ui--progress nil "Last progress line.")

;;;; Mode

(defvar efrit-code-review-ui-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'efrit-code-review-ui-next)
    (define-key map (kbd "p") #'efrit-code-review-ui-previous)
    (define-key map (kbd "m") #'efrit-code-review-ui-queue)
    (define-key map (kbd "u") #'efrit-code-review-ui-unqueue)
    (define-key map (kbd "x") #'efrit-code-review-ui-dismiss)
    (define-key map (kbd "A") #'efrit-code-review-ui-apply)
    (define-key map (kbd "RET") #'efrit-code-review-ui-visit)
    (define-key map (kbd "g") #'efrit-code-review-ui-rerun)
    (define-key map (kbd "k") #'efrit-code-review-ui-cancel)
    (define-key map (kbd "S") #'efrit-code-review-ui-set-store)
    (define-key map (kbd "?") #'efrit-code-review-ui-help)
    map))

(define-derived-mode efrit-code-review-ui-mode special-mode "Efrit-Code-Review"
  "Findings of efrit's review of a change set.  \\{efrit-code-review-ui-mode-map}"
  (setq buffer-read-only t)
  (setq-local truncate-lines nil))

;;;; Rendering

(defun efrit-code-review-ui--badge (type)
  (pcase type
    ('suggestion (propertize " SUGGESTION " 'face 'efrit-code-review-suggestion))
    ('comment (propertize " COMMENT " 'face 'efrit-code-review-comment))
    (_ (propertize " LGTM " 'face 'efrit-code-review-lgtm))))

(defun efrit-code-review-ui--state-text (f)
  (pcase (plist-get f :state)
    ('queued (propertize "queued" 'face 'efrit-code-review-queued))
    ('applied (if (eq (plist-get f :type) 'suggestion) (propertize "applied" 'face 'efrit-code-review-done) ""))
    ('dismissed (propertize "dismissed" 'face 'efrit-code-review-done))
    ('invalid (propertize (format "did not apply: %s" (or (plist-get f :error) "?")) 'face 'efrit-code-review-invalid))
    (_ "")))

(defun efrit-code-review-ui--fontified-diff (text)
  (require 'efrit-markdown)
  (efrit-markdown--fontify-string text #'diff-mode))

(defun efrit-code-review-ui--insert-finding (f)
  (let ((start (point))
        (type (plist-get f :type))
        (done (memq (plist-get f :state) '(dismissed))))
    (insert (efrit-code-review-ui--badge type) " "
            (propertize (format "%s%s" (plist-get f :file)
                                (if (and (plist-get f :lines) (not (eq type 'lgtm)))
                                    (format ":%s" (plist-get f :lines)) ""))
                        'face 'efrit-code-review-file))
    (let ((st (efrit-code-review-ui--state-text f)))
      (unless (string-empty-p st) (insert "  " st)))
    (insert "\n")
    (unless (eq type 'lgtm)
      (insert (propertize (or (plist-get f :title) "") 'face (if done 'efrit-code-review-done 'bold)) "\n")
      (let ((desc (plist-get f :description)))
        (when (and desc (not (string-empty-p desc)))
          (let ((p (point)))
            (insert desc "\n")
            (let ((fill-column 78)) (fill-region p (point))))))
      (when-let* ((why (plist-get f :downgraded)))
        (insert (propertize (format "(the model's suggestion was not applicable: %s)\n" why)
                            'face 'efrit-code-review-meta)))
      (when-let* ((patch (plist-get f :patch)))
        (insert (efrit-code-review-ui--fontified-diff patch))
        (unless (bolp) (insert "\n"))))
    (insert "\n")
    (add-text-properties start (point) (list 'efrit-finding f))))

(defun efrit-code-review-ui--render ()
  (let* ((inhibit-read-only t)
         (result efrit-code-review-ui--result)
         (scope (plist-get result :scope))
         (findings (plist-get result :findings))
         (at-id (when-let* ((f (efrit-code-review-ui--finding-at))) (plist-get f :id))))
    (erase-buffer)
    (insert (propertize (if scope (efrit-code-review-scope-label scope) "efrit code review")
                        'face 'efrit-code-review-title)
            "\n")
    (cond
     (efrit-code-review-ui--state
      (insert (propertize (or efrit-code-review-ui--progress "reviewing…") 'face 'efrit-code-review-meta) "\n"
              (propertize "k cancels" 'face 'efrit-code-review-meta) "\n"))
     ((eq (plist-get result :status) 'error)
      (insert (propertize (format "review failed: %s" (plist-get result :message)) 'face 'error) "\n"
              (propertize "g tries again" 'face 'efrit-code-review-meta) "\n"))
     (t
      (insert (propertize
               (format "%s · %s · %s round%s%s · %d finding%s: %d suggestion%s, %d comment%s, %d clean file%s"
                       (or (plist-get result :model) "?") (or (plist-get result :at) "")
                       (or (plist-get result :rounds) "?") (if (eql (plist-get result :rounds) 1) "" "s")
                       (if (plist-get result :saved) " · from the saved review (g reviews again)" "")
                       (length findings) (if (= 1 (length findings)) "" "s")
                       (cl-count 'suggestion findings :key (lambda (f) (plist-get f :type)))
                       (if (= 1 (cl-count 'suggestion findings :key (lambda (f) (plist-get f :type)))) "" "s")
                       (cl-count 'comment findings :key (lambda (f) (plist-get f :type)))
                       (if (= 1 (cl-count 'comment findings :key (lambda (f) (plist-get f :type)))) "" "s")
                       (cl-count 'lgtm findings :key (lambda (f) (plist-get f :type)))
                       (if (= 1 (cl-count 'lgtm findings :key (lambda (f) (plist-get f :type)))) "" "s"))
               'face 'efrit-code-review-meta)
              "\n"
              (propertize (format "m queue  u unqueue  x dismiss  A apply queued  RET visit  n/p move  g again  S store [%s]  q close"
                                  (efrit-code-review-store-for (efrit-code-review-scope-root scope)))
                          'face 'efrit-code-review-meta)
              "\n\n")
      (if (null findings)
          (insert (propertize "No findings." 'face 'efrit-code-review-meta) "\n")
        ;; suggestions first, then comments, then the clean files
        (dolist (f (sort (copy-sequence findings)
                         (lambda (a b) (< (cl-position (plist-get a :type) '(suggestion comment lgtm))
                                          (cl-position (plist-get b :type) '(suggestion comment lgtm))))))
          (efrit-code-review-ui--insert-finding f)))))
    (goto-char (point-min))
    (if at-id
        (efrit-code-review-ui--goto-id at-id)
      (efrit-code-review-ui-next))))

(defun efrit-code-review-ui--goto-id (id)
  (when-let* ((pos (seq-find (lambda (p) (equal (plist-get (get-text-property p 'efrit-finding) :id) id))
                             (efrit-code-review-ui--starts))))
    (goto-char pos)))

;;;; Moving and acting

(defun efrit-code-review-ui--finding-at ()
  (get-text-property (point) 'efrit-finding))

(defun efrit-code-review-ui--require-finding ()
  (or (efrit-code-review-ui--finding-at) (user-error "No finding at point")))

(defun efrit-code-review-ui--starts ()
  "Where each finding block starts, in order."
  (let ((pos (point-min)) (out nil))
    (when (get-text-property pos 'efrit-finding) (push pos out))
    (while (setq pos (next-single-property-change pos 'efrit-finding))
      (when (and (get-text-property pos 'efrit-finding)
                 (not (eq (get-text-property pos 'efrit-finding)
                          (and (> pos (point-min)) (get-text-property (1- pos) 'efrit-finding)))))
        (push pos out)))
    (nreverse out)))

(defun efrit-code-review-ui-next ()
  "Move to the next finding."
  (interactive)
  (let ((next (seq-find (lambda (p) (> p (point))) (efrit-code-review-ui--starts))))
    (if next (goto-char next) (when (efrit-code-review-ui--finding-at) (message "Last finding")))))

(defun efrit-code-review-ui-previous ()
  "Move to the previous finding."
  (interactive)
  (let ((prev (car (last (seq-filter (lambda (p) (< p (point))) (efrit-code-review-ui--starts))))))
    (if prev (goto-char prev) (message "First finding"))))

(defun efrit-code-review-ui--transition (state)
  (let ((f (efrit-code-review-ui--require-finding)))
    (unless (eq (plist-get f :type) 'suggestion)
      (user-error "Only a suggestion can be %s" state))
    (let ((before (plist-get f :state)))
      (efrit-code-review-finding-transition f state)
      (when (eq before (plist-get f :state))
        (user-error "A %s suggestion cannot become %s" before state)))
    (efrit-code-review-ui--render)))

(defun efrit-code-review-ui-queue ()
  "Queue the suggestion at point for `A'."
  (interactive)
  (efrit-code-review-ui--transition 'queued)
  (efrit-code-review-ui-next))

(defun efrit-code-review-ui-unqueue ()
  "Take the suggestion at point out of the queue."
  (interactive)
  (efrit-code-review-ui--transition 'pending))

(defun efrit-code-review-ui-dismiss ()
  "Dismiss the suggestion at point."
  (interactive)
  (efrit-code-review-ui--transition 'dismissed)
  (efrit-code-review-ui-next))

(defun efrit-code-review-ui-apply ()
  "Apply every queued suggestion to the work tree.
Refused when the change set moved since the review."
  (interactive)
  (let* ((result efrit-code-review-ui--result)
         (scope (plist-get result :scope))
         (queued (seq-filter (lambda (f) (eq (plist-get f :state) 'queued)) (plist-get result :findings))))
    (unless queued (user-error "Nothing is queued (m queues a suggestion)"))
    (unless (efrit-code-review-scope-current-p scope)
      (user-error "The %s changed since this review; g reviews them again"
                  (pcase (efrit-code-review-scope-kind scope) ('staged "staged changes") (_ "commits"))))
    (let* ((done (efrit-code-review-apply-queued scope (plist-get result :findings)))
           (ok (cl-count-if (lambda (d) (eq (plist-get (car d) :state) 'applied)) done)))
      (when efrit-code-review-persist (efrit-code-review-save result))
      (efrit-code-review-ui--render)
      (message "efrit: applied %d of %d suggestion%s%s" ok (length done) (if (= 1 (length done)) "" "s")
               (if (< ok (length done)) " (the rest are marked)" "")))))

(defun efrit-code-review-ui-visit ()
  "Open the finding's file at its first line, in another window."
  (interactive)
  (let* ((f (efrit-code-review-ui--require-finding))
         (root (efrit-code-review-scope-root (plist-get efrit-code-review-ui--result :scope)))
         (file (expand-file-name (plist-get f :file) root))
         (line (efrit-code-review-parse-lines (plist-get f :lines))))
    (unless (file-exists-p file) (user-error "%s is not in the work tree" (plist-get f :file)))
    (let ((buf (find-file-noselect file)))
      (pop-to-buffer buf '((display-buffer-reuse-window display-buffer-pop-up-window)))
      (when (> line 0)
        (goto-char (point-min))
        (forward-line (1- line))))))

(defun efrit-code-review-ui-cancel ()
  "Stop the review in flight after the request in progress."
  (interactive)
  (if efrit-code-review-ui--state
      (progn (efrit-code-review-cancel efrit-code-review-ui--state)
             (message "efrit: cancelling after the request in flight"))
    (user-error "No review is running")))

(defun efrit-code-review-ui-set-store ()
  "Choose where this repository keeps its reviews: JSON files or git notes."
  (interactive)
  (let ((scope (plist-get efrit-code-review-ui--result :scope)))
    (unless scope (user-error "No repository here"))
    (let ((root (efrit-code-review-scope-root scope)))
      (efrit-code-review-set-project-store
       (intern (completing-read (format "Review store for %s: " (abbreviate-file-name root))
                                '("json" "notes" "unset") nil t))
       root)
      (message "efrit: reviews of %s go to %s" (abbreviate-file-name root) (efrit-code-review-store-for root)))
    (efrit-code-review-ui--render)))

(defun efrit-code-review-ui-help ()
  "Describe the keys."
  (interactive)
  (describe-keymap 'efrit-code-review-ui-mode-map))

;;;; Running

(defun efrit-code-review-ui--buffer ()
  (let ((buf (get-buffer-create efrit-code-review-ui--buffer-name)))
    (with-current-buffer buf
      (unless (derived-mode-p 'efrit-code-review-ui-mode) (efrit-code-review-ui-mode)))
    buf))

(defun efrit-code-review-ui--start (scope &optional fresh)
  "Show SCOPE's review: the saved one unless FRESH, else run it."
  (let ((buf (efrit-code-review-ui--buffer)))
    (with-current-buffer buf
      (when efrit-code-review-ui--state
        (user-error "A review is already running here (k cancels it)"))
      (let ((saved (and (not fresh) efrit-code-review-persist (efrit-code-review-load scope))))
        (if saved
            (progn (setq efrit-code-review-ui--result saved)
                   (efrit-code-review-ui--render))
          (setq efrit-code-review-ui--result (list :status 'running :scope scope :findings nil)
                efrit-code-review-ui--progress "sending the file list…")
          (setq efrit-code-review-ui--state
                (efrit-code-review-run
                 scope
                 (lambda (result)
                   (when (buffer-live-p buf)
                     (with-current-buffer buf
                       (setq efrit-code-review-ui--state nil
                             efrit-code-review-ui--result result)
                       (efrit-code-review-ui--render)
                       (message "efrit: code review %s"
                                (if (eq (plist-get result :status) 'ok)
                                    (format "done: %d finding(s)" (length (plist-get result :findings)))
                                  (format "failed: %s" (plist-get result :message)))))))
                 (lambda (round tools)
                   (when (buffer-live-p buf)
                     (with-current-buffer buf
                       (setq efrit-code-review-ui--progress
                             (format "round %d of at most %d · %s" round efrit-code-review-max-rounds
                                     (if tools (mapconcat #'identity tools ", ") "thinking")))
                       (efrit-code-review-ui--render))))))
          (efrit-code-review-ui--render))))
    (pop-to-buffer buf '((display-buffer-reuse-window display-buffer-pop-up-window)))
    buf))

(defun efrit-code-review-ui-rerun ()
  "Review this change set again, ignoring the saved review."
  (interactive)
  (let ((scope (plist-get efrit-code-review-ui--result :scope)))
    (unless scope (user-error "Nothing to review again"))
    (efrit-code-review-ui--start (efrit-code-review-scope (efrit-code-review-scope-kind scope)
                                                          (efrit-code-review-scope-root scope))
                                 t)))

;;;###autoload
(defun efrit-code-review (&optional kind dir)
  "Review the staged changes of DIR's repository and show the findings.
KIND is `staged' (default), `unpushed' or `branch'; interactively a
prefix argument asks which.  A saved review of this exact change set
is shown without a request; `g' in the buffer asks anew."
  (interactive
   (list (if current-prefix-arg
             (intern (completing-read "Review: " '("staged" "unpushed" "branch") nil t nil nil "staged"))
           'staged)
         nil))
  (let ((scope (condition-case err
                   (efrit-code-review-scope (or kind 'staged) (or dir default-directory))
                 (efrit-code-review-error (user-error "%s" (cadr err)))
                 (efrit-vcs-error (user-error "%s" (cadr err))))))
    (efrit-code-review-ui--start scope)))

;;;; Magit

(defun efrit-code-review-magit ()
  "Review the staged changes of the Magit repository at hand."
  (interactive)
  (efrit-code-review 'staged (and (fboundp 'magit-toplevel) (magit-toplevel))))

(with-eval-after-load 'magit
  (when (and (fboundp 'transient-append-suffix) (get 'magit-diff 'transient--prefix))
    (ignore-errors
      (transient-append-suffix 'magit-diff 'magit-diff-dwim
        '("R" "efrit code review (staged)" efrit-code-review-magit)))))

(provide 'efrit-code-review-ui)

;;; efrit-code-review-ui.el ends here
