;;; test-efrit-prompts.el --- Tests for efrit-prompts -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'efrit-prompts)

(defmacro test-prompts--with-library (&rest body)
  "Run BODY with a scratch prompt file and two built-ins."
  (declare (indent 0))
  `(let* ((dir (make-temp-file "efrit-prompts-test" t))
          (efrit-prompts-file (expand-file-name "prompts.json" dir))
          (efrit-prompts-builtin nil)
          (efrit-prompts--user nil)
          (efrit-prompts--loaded nil)
          (efrit-prompts-last nil)
          (efrit-prompts-change-hook nil))
     (efrit-prompts-define "one" "Item one." "Summary one." "first")
     (efrit-prompts-define "two" "Item two." "Summary two.")
     (unwind-protect (progn ,@body)
       (delete-directory dir t))))

(ert-deftest test-efrit-prompts-library-merges-user-and-builtin ()
  "User prompts come first, override built-ins by name, persist to disk,
and deleting the override restores the built-in."
  (test-prompts--with-library
    (should (equal '("one" "two") (mapcar (lambda (p) (plist-get p :name)) (efrit-prompts))))
    (should (plist-get (efrit-prompts-get "one") :builtin))
    ;; A prompt of the user's own.
    (efrit-prompts-put "mine" "Item mine." "Summary mine." "why")
    (should (equal '("mine" "one" "two") (mapcar (lambda (p) (plist-get p :name)) (efrit-prompts))))
    (should-not (plist-get (efrit-prompts-get "mine") :builtin))
    ;; Overriding a built-in keeps one entry, marked changed.
    (efrit-prompts-put "one" "Item one, better." "Summary one." "first")
    (should (= 3 (length (efrit-prompts))))
    (let ((p (efrit-prompts-get "one")))
      (should (equal "Item one, better." (plist-get p :item)))
      (should (plist-get p :builtin))
      (should (plist-get p :changed)))
    ;; Persisted: a fresh load reads the same.
    (should (file-exists-p efrit-prompts-file))
    (efrit-prompts-reload)
    (should (equal "Item one, better." (plist-get (efrit-prompts-get "one") :item)))
    (should (equal "why" (plist-get (efrit-prompts-get "mine") :description)))
    ;; Delete the override: the built-in is back.
    (efrit-prompts-delete "one")
    (should-not (plist-get (efrit-prompts-get "one") :changed))
    (should (equal "Item one." (plist-get (efrit-prompts-get "one") :item)))
    (should-error (efrit-prompts-delete "two") :type 'user-error)
    ;; Rename works on the user's prompts only.
    (efrit-prompts-rename "mine" "ours")
    (should (efrit-prompts-get "ours"))
    (should-not (efrit-prompts-get "mine"))
    (should-error (efrit-prompts-rename "two" "deux") :type 'user-error)
    ;; Validation.
    (should-error (efrit-prompts-put "" "x" "y") :type 'user-error)
    (should-error (efrit-prompts-put "blank" "  " "y") :type 'user-error)))

(ert-deftest test-efrit-prompts-kinds ()
  "A prompt is single or summarizing: derived from the summary when not
given, stored in the file, kept by copy and rename; a single prompt's
pair has no summary; a summarizing prompt needs one."
  (test-prompts--with-library
    (efrit-prompts-define "solo" "Draft a reply to each." nil "one task" 'single)
    (should (eq 'single (efrit-prompts-kind (efrit-prompts-get "solo"))))
    (should (eq 'summarizing (efrit-prompts-kind (efrit-prompts-get "one"))))
    (should (equal '("Draft a reply to each." . nil) (efrit-prompts-pair "solo")))
    ;; Explicit kind wins over the summary text; a single prompt drops it
    (efrit-prompts-put "quick" "Do this." "ignored" nil 'single)
    (should (equal "" (plist-get (efrit-prompts-get "quick") :summary)))
    (should-not (efrit-prompts-summarizing-p (efrit-prompts-get "quick")))
    ;; Derived: an empty summary means single
    (efrit-prompts-put "derived" "Do that." "")
    (should (eq 'single (efrit-prompts-kind (efrit-prompts-get "derived"))))
    (should-error (efrit-prompts-put "bad" "Item." "" nil 'summarizing) :type 'user-error)
    ;; Round trip through the file, and a pre-kind file entry
    (efrit-prompts-reload)
    (should (eq 'single (efrit-prompts-kind (efrit-prompts-get "quick"))))
    (should (eq 'summarizing
                (efrit-prompts-kind
                 (efrit-prompts--plist-from-json
                  (let ((h (make-hash-table :test 'equal)))
                    (puthash "name" "old" h) (puthash "item" "i" h) (puthash "summary" "s" h) h)))))
    (efrit-prompts-rename "quick" "quicker")
    (should (eq 'single (efrit-prompts-kind (efrit-prompts-get "quicker"))))
    ;; The manager shows the kind and no summary hint for a single prompt
    (with-temp-buffer
      (efrit-prompts-manage-mode)
      (efrit-prompts-manage-refresh)
      (let ((row (seq-find (lambda (e) (equal (car e) "quicker")) tabulated-list-entries)))
        (should (equal "single" (substring-no-properties (aref (cadr row) 2))))
        (should (equal "" (aref (cadr row) 4)))))
    ;; The editor round-trips the kind through its switch, and the
    ;; switch toggles it
    (with-temp-buffer
      (efrit-prompts-edit-mode)
      (efrit-prompts-edit--insert (efrit-prompts-get "quicker"))
      (should (eq 'single (efrit-prompts-edit--kind (efrit-prompts-edit--read))))
      (should (text-property-any (point-min) (point-max) 'efrit-prompts-kind 'summarizing))
      (efrit-prompts-edit-toggle-kind)
      (should (eq 'summarizing (efrit-prompts-edit--kind (efrit-prompts-edit--read))))
      ;; The lit pill follows the kind
      (let ((lit (text-property-any (point-min) (point-max) 'face 'efrit-prompts-kind-on)))
        (should (eq 'summarizing (get-text-property lit 'efrit-prompts-kind))))
      (efrit-prompts-edit-toggle-kind)
      (should (eq 'single (efrit-prompts-edit--kind (efrit-prompts-edit--read)))))
    ;; For a single prompt the summary section is hidden, not gone
    (with-temp-buffer
      (efrit-prompts-edit-mode)
      (efrit-prompts-edit--insert '(:name "t" :kind single :item "i" :summary "kept summary"))
      (let ((pos (text-property-any (point-min) (point-max) 'efrit-prompts-section :summary)))
        (should (eq 'efrit-prompts-summary (get-text-property pos 'invisible)))
        (should (invisible-p pos))
        (should (equal "kept summary" (plist-get (efrit-prompts-edit--read) :summary)))
        (efrit-prompts-edit-toggle-kind)
        (should-not (get-text-property pos 'invisible))
        (efrit-prompts-edit-toggle-kind)
        (should (invisible-p pos))))))

(ert-deftest test-efrit-prompts-editor-headings-are-not-editable ()
  "Typing anywhere on a heading, help line or the kind switch is refused;
typing in a field, at its start and at its end, works."
  (with-temp-buffer
    (efrit-prompts-edit-mode)
    (efrit-prompts-edit--insert '(:name "n" :description "d" :kind summarizing
                                  :item "item text" :summary "sum"))
    (let ((refused 0) (allowed 0))
      (cl-flet ((try (pos) (goto-char pos)
                     (condition-case nil
                         (progn (insert "X") (delete-char -1) (cl-incf allowed))
                       (text-read-only (cl-incf refused)))))
        ;; every position on protected text, including the boundary
        ;; between a heading and its help, and before the first heading
        (let ((pos (point-min)))
          (while (< pos (point-max))
            (when (and (get-text-property pos 'read-only)
                       (not (get-text-property (max 1 (1- pos)) 'rear-nonsticky)))
              (try pos))
            (setq pos (1+ pos))))
        (should (> refused 20))
        (should (= allowed 0))
        ;; the fields
        (dolist (key '(:name :description :item :summary))
          (let* ((heading (text-property-any (point-min) (point-max) 'efrit-prompts-section key))
                 (start (save-excursion (goto-char heading) (forward-line 1) (point))))
            (try start)
            (try (save-excursion (goto-char start) (line-end-position)))
            (try (save-excursion (goto-char start) (forward-line 1) (point)))))
        (should (= allowed 12))))
    ;; and the read-back is unchanged
    (should (equal "item text" (plist-get (efrit-prompts-edit--read) :item)))))

(ert-deftest test-efrit-prompts-review-accept-into-editor ()
  "`a' in the review puts the suggestion into the editor (it errored with
\"Buffer is read-only\" when the review was a diff-mode preview)."
  (let ((editor (generate-new-buffer "*efrit prompt: t*")))
    (unwind-protect
        (progn
          (with-current-buffer editor
            (efrit-prompts-edit-mode)
            (efrit-prompts-edit--insert '(:name "t" :description "" :kind summarizing
                                          :item "old item" :summary "old sum"))
            (setq efrit-prompts-edit--suggestion '("new item" . "new sum")))
          (cl-letf (((symbol-function 'quit-window) #'ignore)
                    ((symbol-function 'pop-to-buffer) #'ignore)
                    ((symbol-function 'fit-window-to-buffer) #'ignore))
            (let ((review (efrit-prompts--show-suggestion
                           editor '(:name "t" :item "old item" :summary "old sum")
                           '("new item" . "new sum"))))
              (with-current-buffer review
                (should (derived-mode-p 'efrit-prompts-review-mode))
                (should (eq (key-binding "a") #'efrit-prompts-review-accept))
                (should (string-match-p "^  yours     old item$" (buffer-string)))
                (should (string-match-p "^  suggested new item$" (buffer-string)))
                ;; continuation lines line up under the text, not the label
                (should (equal (make-string efrit-prompts--review-gutter ?\s)
                               (get-text-property
                                (string-match "old item" (buffer-string)) 'wrap-prefix
                                (current-buffer))))
                (efrit-prompts-review-accept))))
          (with-current-buffer editor
            (should (equal "new item" (plist-get (efrit-prompts-edit--read) :item)))
            (should (equal "new sum" (plist-get (efrit-prompts-edit--read) :summary)))
            (should (null efrit-prompts-edit--suggestion))))
      (kill-buffer editor)
      (when (get-buffer "*efrit prompt suggestion*") (kill-buffer "*efrit prompt suggestion*")))))

(ert-deftest test-efrit-prompts-pair-and-names ()
  "Pairs resolve from a name, a plist, a pair or a question; the last
used name is offered first."
  (test-prompts--with-library
    (should (equal '("Item one." . "Summary one.") (efrit-prompts-pair "one")))
    (should (equal '("Item one." . "Summary one.") (efrit-prompts-pair (efrit-prompts-get "one"))))
    (should (equal '("a" . "b") (efrit-prompts-pair '("a" . "b"))))
    (should (equal '("What now?" . nil) (efrit-prompts-pair "What now?")))
    (should (equal "two" (efrit-prompts-name-of "Item two.")))
    (should-not (efrit-prompts-name-of "nope"))
    (should (equal '("one" "two") (efrit-prompts-names)))
    (setq efrit-prompts-last "two")
    (should (equal '("two" "one") (efrit-prompts-names)))))

(ert-deftest test-efrit-prompts-read-falls-back-to-completion ()
  "In batch the chooser is `completing-read'; a name gives its pair and is
remembered, free text gives a question, \"edit\" opens the manager."
  (test-prompts--with-library
    (let ((managed nil))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "two")))
        (should (equal '("Item two." . "Summary two.") (efrit-prompts-read "Test")))
        (should (equal "two" efrit-prompts-last)))
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (&rest _) "Who is blocked?")))
        (should (equal '("Who is blocked?" . nil) (efrit-prompts-read))))
      (let ((answers '("edit" "one")))
        (cl-letf (((symbol-function 'completing-read)
                   (lambda (&rest _) (pop answers)))
                  ((symbol-function 'efrit-prompts-manage)
                   (lambda (&optional _wait) (setq managed t))))
          (should (equal '("Item one." . "Summary one.") (efrit-prompts-read)))
          (should managed))))))

(ert-deftest test-efrit-prompts-parse-suggestion ()
  "The model's two-block answer parses; anything else is nil."
  (should (equal '("Per item text." . "Summary text.")
                 (efrit-prompts-parse-suggestion
                  "=== ITEM ===\nPer item text.\n=== SUMMARY ===\nSummary text.\n")))
  (should (equal '("A\nB" . "C")
                 (efrit-prompts-parse-suggestion "=== ITEM ===\nA\nB\n\n=== SUMMARY ===\n\nC")))
  ;; A single prompt: empty or missing SUMMARY block
  (should (equal '("A" . "") (efrit-prompts-parse-suggestion "=== ITEM ===\nA\n=== SUMMARY ===\n")))
  (should (equal '("A" . "") (efrit-prompts-parse-suggestion "=== ITEM ===\nA\n=== SUMMARY ===")))
  (should (equal '("A\nB" . "") (efrit-prompts-parse-suggestion "=== ITEM ===\nA\nB\n")))
  (should-not (efrit-prompts-parse-suggestion "Here is a better prompt:\nA"))
  (should-not (efrit-prompts-parse-suggestion "Here is a better prompt: ..."))
  (should-not (efrit-prompts-parse-suggestion nil)))

(ert-deftest test-efrit-prompts-suggest-uses-api ()
  "A suggestion is one API call with the system instructions and both
parts; the callback gets the parsed pair, or the failure as a string."
  (test-prompts--with-library
    (let ((request nil) (got nil))
      (cl-letf (((symbol-function 'efrit-api-request-async)
                 (lambda (req callback &optional _err)
                   (setq request req)
                   (let ((resp (make-hash-table :test 'equal))
                         (block (make-hash-table :test 'equal)))
                     (puthash "type" "text" block)
                     (puthash "text" "=== ITEM ===\nBetter item.\n=== SUMMARY ===\nBetter summary." block)
                     (puthash "content" (vector block) resp)
                     (funcall callback resp)))))
        (let ((efrit-default-model "test-model"))
          (efrit-prompts-suggest "one" "Item one." "Summary one." "find blockers"
                                 (lambda (r) (setq got r))))
        (should (equal '("Better item." . "Better summary.") got))
        (should (equal "test-model" (alist-get "model" request nil nil #'equal)))
        (let ((user (alist-get "content" (aref (alist-get "messages" request nil nil #'equal) 0)
                               nil nil #'equal)))
          (should (string-match-p "Item one\\." user))
          (should (string-match-p "find blockers" user))))
      (cl-letf (((symbol-function 'efrit-api-request-async)
                 (lambda (_req _callback &optional err) (funcall err "boom"))))
        (efrit-prompts-suggest "one" "i" "s" nil (lambda (r) (setq got r)))
        (should (equal "boom" got))))))

(ert-deftest test-efrit-prompts-editor-round-trip ()
  "The editor buffer reads back what was inserted, saves through
`efrit-prompts-put', and a suggestion replaces the two parts."
  (test-prompts--with-library
    (let ((buf (get-buffer-create "*efrit prompt: test*")))
      (unwind-protect
          (with-current-buffer buf
            (efrit-prompts-edit-mode)
            (efrit-prompts-edit--insert '(:name "one" :description "first"
                                          :item "Item one." :summary "Summary one."))
            (let ((read (efrit-prompts-edit--read)))
              (dolist (kv '((:name . "one") (:description . "first") (:kind . summarizing)
                            (:item . "Item one.") (:summary . "Summary one.")))
                (should (equal (cdr kv) (plist-get read (car kv))))))
            (efrit-prompts-edit--replace-section :item "New item.")
            (efrit-prompts-edit--replace-section :summary "New summary.")
            (should (equal "New item." (plist-get (efrit-prompts-edit--read) :item)))
            (should (equal "New summary." (plist-get (efrit-prompts-edit--read) :summary)))
            (setq efrit-prompts-edit--original (efrit-prompts-get "one"))
            (cl-letf (((symbol-function 'quit-window) #'ignore))
              (efrit-prompts-edit-save))
            (should (equal "New item." (plist-get (efrit-prompts-get "one") :item)))
            (should (plist-get (efrit-prompts-get "one") :changed)))
        (kill-buffer buf)))))

(ert-deftest test-efrit-prompts-manager-lists-everything ()
  "The manager shows one row per prompt with its badge and hints."
  (test-prompts--with-library
    (efrit-prompts-put "mine" "Item mine." "Summary mine.")
    (with-temp-buffer
      (efrit-prompts-manage-mode)
      (efrit-prompts-manage-refresh)
      (should (= 3 (length tabulated-list-entries)))
      (goto-char (point-min))
      (should (equal "mine" (tabulated-list-get-id)))
      (should (string-match-p "yours" (buffer-string)))
      (should (string-match-p "built-in" (buffer-string)))
      (should (string-match-p "Item mine" (buffer-string))))))

(defvar test-prompts--depth 0 "The fake recursion depth; dynamic so the stubs see rebinding.")

(ert-deftest test-efrit-prompts-manager-wait-survives-view-and-edit ()
  "The chooser's wait on the manager ends on `q' or on killing the buffer,
not when another buffer (the view popup, the editor) takes its window."
  (test-prompts--with-library
    (let ((buf (get-buffer-create " *efrit prompts test*"))
          (events nil) (inside nil) (recursive nil))
      (unwind-protect
          (cl-letf* ((test-prompts--depth (recursion-depth))
                     ((symbol-function 'recursion-depth) (lambda () test-prompts--depth))
                     ;; A real recursive edit runs INSIDE one level deeper
                     ((symbol-function 'recursive-edit)
                      (lambda () (setq recursive t)
                        (let ((test-prompts--depth (1+ test-prompts--depth)))
                          (funcall inside))))
                    ((symbol-function 'exit-recursive-edit)
                     (lambda () (push 'exit events)))
                    ((symbol-function 'quit-window) (lambda (&rest _) (push 'quit events))))
            ;; Inside the "recursive edit": the window goes away (view,
            ;; edit) and nothing ends the wait; `q' does.
            (setq inside (lambda ()
                           (with-current-buffer buf
                             (run-hooks 'window-configuration-change-hook)
                             (run-hooks 'buffer-list-update-hook)
                             (should-not (memq 'exit events))
                             (efrit-prompts-manage-quit)
                             (should (equal '(exit quit) (seq-take events 2))))))
            (with-current-buffer buf (efrit-prompts-manage-mode))
            (efrit-prompts--wait-for-buffer buf)
            (should recursive)
            ;; Outside a wait, `q' only quits the window.
            (setq events nil)
            (with-current-buffer buf (efrit-prompts-manage-quit))
            (should (equal '(quit) events))
            ;; Killing the manager during a wait ends it too.
            (setq events nil)
            (setq inside (lambda () (kill-buffer buf)))
            (efrit-prompts--wait-for-buffer buf)
            (should (equal '(exit) events)))
        (when (buffer-live-p buf) (kill-buffer buf))))))

(provide 'test-efrit-prompts)
;;; test-efrit-prompts.el ends here
