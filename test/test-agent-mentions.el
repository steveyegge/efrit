;;; test-agent-mentions.el --- @file mentions, /commands, drag and drop -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'efrit-agent)
(require 'efrit-agent-mentions)

(defmacro test-mentions--with-project (&rest body)
  "Run BODY with a throwaway project root holding a few files."
  (declare (indent 0))
  `(let* ((root (file-name-as-directory (make-temp-file "efrit-mentions-" t)))
          (efrit-project-root root)
          (default-directory root)
          (efrit-agent-mention--files-cache nil))
     (unwind-protect
         (progn
           (make-directory (expand-file-name "src" root))
           (with-temp-file (expand-file-name "src/main.el" root) (insert "(defun main () 1)\n"))
           (with-temp-file (expand-file-name "notes with space.txt" root) (insert "remember\n"))
           (with-temp-file (expand-file-name "big.txt" root) (insert (make-string 100 ?x)))
           ,@body)
       (delete-directory root t))))

(ert-deftest test-mentions-parse-and-expand ()
  "Mentions are found at word boundaries (not inside emails), quoted paths
keep their spaces, punctuation after a bare path is dropped; expansion
appends readable files in fenced blocks, clipped, and leaves unknown
paths alone."
  (test-mentions--with-project
    (should (equal '("src/main.el" "notes with space.txt" "nope.txt")
                   (efrit-agent-mentions-in
                    "see @src/main.el, and @\"notes with space.txt\" then @nope.txt. mail me@example.com")))
    (should (equal "@a/b" (efrit-agent-mention-text "a/b")))
    (should (equal "@\"a b\"" (efrit-agent-mention-text "a b")))
    (let ((efrit-agent-mention-max-chars 50))
      (let ((out (efrit-agent-mentions-expand "look at @src/main.el and @big.txt and @nope.txt")))
        (should (string-prefix-p "look at @src/main.el" out))
        (should (string-match-p "@src/main.el\n```el\n(defun main () 1)\n```" out))
        (should (string-match-p "50 more characters not shown" out))
        (should-not (string-match-p "@nope.txt\n```" out))))
    ;; No mention, no change
    (should (equal "plain" (efrit-agent-mentions-expand "plain")))
    ;; Image blocks: a png is attached, not inlined
    (with-temp-file (expand-file-name "shot.png" root) (insert "\211PNG\r\n\032\n"))
    (cl-letf (((symbol-function 'image-supported-file-p)
               (lambda (f) (string-suffix-p ".png" f))))
      (let ((blocks (efrit-agent-mentions-content-blocks "here @shot.png")))
        (should (= 1 (length blocks)))
        (should (equal "image" (alist-get 'type (car blocks))))
        (should (equal "image/png" (alist-get 'media_type (alist-get 'source (car blocks)))))
        (should (string-prefix-p "iVBORw0K" (alist-get 'data (alist-get 'source (car blocks))))))
      (should (equal "here @shot.png" (efrit-agent-mentions-expand "here @shot.png"))))))

(ert-deftest test-mentions-completion-at-point ()
  "`@' at a word start completes project files; `/' only at the input start
completes commands; a completed path with spaces is rewritten quoted."
  (test-mentions--with-project
    (with-current-buffer (efrit-agent--get-buffer)
      (let ((inhibit-read-only t)) (erase-buffer))
      (efrit-agent-mode)
      (efrit-agent--init-regions)
      (efrit-agent--setup-regions)
      (goto-char (point-max))
      (insert "read @src/ma")
      (let ((capf (efrit-agent-mention-completion-at-point)))
        (should capf)
        (should (member "src/main.el" (all-completions "src/ma" (nth 2 capf))))
        (should (member "notes with space.txt" (all-completions "" (nth 2 capf)))))
      ;; not at a word start: an email
      (efrit-agent--clear-input) (goto-char (point-max)) (insert "me@exa")
      (should-not (efrit-agent-mention-completion-at-point))
      ;; slash at input start
      (efrit-agent--clear-input) (goto-char (point-max)) (insert "/mo")
      (let ((capf (efrit-agent-slash-completion-at-point)))
        (should capf)
        (should (member "model" (all-completions "mo" (nth 2 capf))))
        (should (member "mode" (all-completions "mo" (nth 2 capf)))))
      ;; slash later in the text: not a command
      (efrit-agent--clear-input) (goto-char (point-max)) (insert "a /mo")
      (should-not (efrit-agent-slash-completion-at-point))
      (should-not (efrit-agent-slash-parse "a /mo"))
      ;; the exit function quotes a path with spaces
      (efrit-agent--clear-input) (goto-char (point-max)) (insert "@notes with space.txt")
      (let* ((at (- (point) (length "@notes with space.txt"))))
        (goto-char (point-max))
        (let ((capf (save-excursion (goto-char (+ at 6)) (efrit-agent-mention-completion-at-point))))
          (should capf)
          (delete-region at (point-max))
          (insert "@notes with space.txt")
          (funcall (plist-get (nthcdr 3 capf) :exit-function) "notes with space.txt" 'finished)
          (should (string-suffix-p "@\"notes with space.txt\"" (efrit-agent--get-input)))))
      (kill-buffer))))

(ert-deftest test-mentions-slash-commands ()
  "A slash command at the input start runs and sends nothing; an unknown one
is reported, not sent; /model NAME sets the model."
  (should (equal '("model" . "claude-x") (efrit-agent-slash-parse "/model claude-x")))
  (should (equal '("help" . "") (efrit-agent-slash-parse "/help  ")))
  (should-not (efrit-agent-slash-parse "not /a command"))
  (should (assoc "help" efrit-agent-slash-commands))
  (with-current-buffer (efrit-agent--get-buffer)
    (let ((inhibit-read-only t)) (erase-buffer))
    (efrit-agent-mode)
    (efrit-agent--init-regions)
    (efrit-agent--setup-regions)
    (let ((efrit-default-model "before") (ran nil) (sent nil))
      (efrit-agent-define-slash-command "probe" "test" (lambda (args) (setq ran args)))
      (cl-letf (((symbol-function 'efrit-agent--repl-send)
                 (lambda (input &optional _api) (push input sent) t)))
        (goto-char (point-max)) (insert "/probe one two")
        (efrit-agent-input-send)
        (should (equal "one two" ran))
        (should-not sent)
        (should (equal "" (efrit-agent--get-input)))
        (goto-char (point-max)) (insert "/model claude-x")
        (efrit-agent-input-send)
        (should (equal "claude-x" efrit-default-model))
        (goto-char (point-max)) (insert "/nonesuch")
        (efrit-agent-input-send)
        (should-not sent)
        ;; The typo stays in the input for correction
        (should (equal "/nonesuch" (efrit-agent--get-input)))
        (efrit-agent--clear-input)
        ;; A real message with a mention is sent with the API text expanded
        (test-mentions--with-project
          (cl-letf (((symbol-function 'efrit-agent--repl-send)
                     (lambda (input &optional api) (push (cons input api) sent) t)))
            (goto-char (point-max)) (insert "explain @src/main.el")
            (efrit-agent-input-send)
            (should (equal "explain @src/main.el" (car (car sent))))
            (should (string-match-p "(defun main () 1)" (cdr (car sent))))))))
    (setq efrit-agent-slash-commands (cl-remove "probe" efrit-agent-slash-commands :key #'car :test #'equal))
    (kill-buffer)))

(ert-deftest test-mentions-drag-and-drop ()
  "A dropped file becomes a mention in the input, relative to the project;
one from a temporary directory is copied under .efrit/dropped first;
the handler is on `dnd-protocol-alist' of the agent buffer."
  (test-mentions--with-project
    (with-current-buffer (efrit-agent--get-buffer)
      (let ((inhibit-read-only t)) (erase-buffer))
      (efrit-agent-mode)
      (efrit-agent--init-regions)
      (efrit-agent--setup-regions)
      (should (rassq 'efrit-agent-dnd-handle dnd-protocol-alist))
      (goto-char (point-max)) (insert "look")
      (should (eq 'private
                  (efrit-agent-dnd-handle (list (concat "file://" (expand-file-name "src/main.el" root))) 'copy)))
      (should (equal "look @src/main.el" (efrit-agent--get-input)))
      ;; Temporary file: copied and mentioned by its new relative path
      (let* ((temporary-file-directory (file-name-as-directory (make-temp-file "efrit-tmp-" t)))
             (shot (expand-file-name "Screenshot 1.png" temporary-file-directory)))
        (with-temp-file shot (insert "png"))
        (unwind-protect
            (progn
              (efrit-agent-dnd-handle (concat "file://" shot) 'copy)
              (should (string-match-p "@\\.efrit/dropped/[0-9]+-[0-9]+-[0-9a-f]+\\.png" (efrit-agent--get-input)))
              (should (directory-files (expand-file-name ".efrit/dropped" root) nil "\\.png\\'")))
          (delete-directory temporary-file-directory t)))
      ;; A directory or a missing file is refused
      (should-error (efrit-agent-dnd-handle (list (concat "file://" root)) 'copy) :type 'user-error)
      (kill-buffer))))

(provide 'test-agent-mentions)
;;; test-agent-mentions.el ends here
