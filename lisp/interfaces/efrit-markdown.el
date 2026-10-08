;;; efrit-markdown.el --- In-place Markdown rendering for streamed text -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.10.3
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; The model answers in Markdown.  This renders it in place: the
;; markup characters are deleted and the text that remains gets
;; faces, links and code highlighting as text properties.  No
;; overlays -- they scale badly and fight `field' and read-only
;; text -- and no second buffer.
;;
;; Streaming.  The text arrives in chunks and the renderer runs after
;; each one.  A construct that may still grow (the last line, an open
;; fenced block) is left alone; everything before it is final.  That
;; frontier is the watermark: `efrit-markdown-render' stores it as an
;; offset from the region start in the `efrit-markdown-watermark'
;; property of the first character, and the next call starts there.
;; An offset, not a position, so the value survives text moving above
;; the region.  `efrit-markdown-render' with COMPLETE non-nil renders
;; to the end and releases what was held back.
;;
;; Rendered code is tagged `efrit-markdown-frozen' and skipped by the
;; inline passes, so a `*' inside a code span never becomes emphasis.
;;
;; What is rendered: ATX headers, bold, italic, strikethrough, inline
;; code, fenced code blocks (with the language mode's fontification
;; and a block background), [title](url) links, bare URLs, file
;; references like `lisp/efrit.el:120' (also inside code spans, where
;; the model puts them), bullets and horizontal rules.  Tables and
;; images are not rendered; they stay as text.
;;
;; Every face is also written as `font-lock-face', and the range is
;; marked `fontified', so a font-lock pass over the buffer neither
;; wipes nor redoes the work.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'browse-url)
(require 'url)

(defgroup efrit-markdown nil
  "Rendering of the model's Markdown in efrit buffers."
  :group 'efrit
  :prefix "efrit-markdown-")

(defcustom efrit-markdown-enabled t
  "Whether the model's answers are rendered as Markdown in the agent buffer.
nil shows the raw text."
  :type 'boolean)

(defcustom efrit-markdown-fontify-code t
  "Whether fenced code blocks are fontified with their language's major mode."
  :type 'boolean)

(defcustom efrit-markdown-language-modes
  '(("elisp" . emacs-lisp-mode) ("emacs-lisp" . emacs-lisp-mode) ("lisp" . lisp-mode)
    ("el" . emacs-lisp-mode) ("sh" . sh-mode) ("bash" . sh-mode) ("shell" . sh-mode)
    ("zsh" . sh-mode) ("js" . js-mode) ("javascript" . js-mode) ("ts" . typescript-ts-mode)
    ("py" . python-mode) ("python" . python-mode) ("json" . js-json-mode)
    ("yaml" . yaml-mode) ("yml" . yaml-mode) ("c" . c-mode) ("cpp" . c++-mode)
    ("c++" . c++-mode) ("go" . go-mode) ("rust" . rust-mode) ("rs" . rust-mode)
    ("html" . html-mode) ("css" . css-mode) ("sql" . sql-mode) ("diff" . diff-mode)
    ("org" . org-mode) ("makefile" . makefile-mode) ("make" . makefile-mode))
  "Fence language names to major modes.  A name not here tries NAME-mode."
  :type '(alist :key-type string :value-type symbol))

(defcustom efrit-markdown-open-file-function #'find-file-other-window
  "How a file reference is opened; called with the file name."
  :type 'function)

(defcustom efrit-markdown-images t
  "Whether `![alt](source)' shows the image (on a graphic display).
Local files and data: URLs show at once; http(s) images are fetched
in the background into `efrit-markdown-image-cache-directory'."
  :type 'boolean)

(defcustom efrit-markdown-image-max-width 600
  "Widest an image is drawn, in pixels, before scaling by the user."
  :type 'integer)

(defcustom efrit-markdown-image-scale-step 1.25
  "Factor one scale step multiplies an image's width by."
  :type 'number)

(defcustom efrit-markdown-image-cache-directory
  (expand-file-name "efrit-images" temporary-file-directory)
  "Where fetched remote images are kept."
  :type 'directory)

(defcustom efrit-markdown-tables t
  "Whether pipe tables are drawn as aligned columns."
  :type 'boolean)

;;;; Faces

(defface efrit-markdown-header
  '((t :inherit bold :height 1.1))
  "Headers.")

(defface efrit-markdown-bold '((t :inherit bold)) "Bold text.")
(defface efrit-markdown-italic '((t :inherit italic)) "Italic text.")
(defface efrit-markdown-strike '((t :strike-through t)) "Struck text.")

(defface efrit-markdown-inline-code
  '((((background dark)) :background "#2a2a2a" :inherit fixed-pitch)
    (t :background "#f0f0f0" :inherit fixed-pitch))
  "Inline code.")

(defface efrit-markdown-code-block
  '((((background dark)) :background "#1e1e1e" :extend t)
    (t :background "#f5f5f5" :extend t))
  "Background of a fenced code block.")

(defface efrit-markdown-code-label
  '((t :inherit shadow :height 0.85))
  "The language label above a code block.")

(defface efrit-markdown-link
  '((t :inherit link))
  "Links and file references.")

(defface efrit-markdown-bullet
  '((t :inherit font-lock-keyword-face))
  "List bullets.")

(defface efrit-markdown-rule
  '((t :inherit shadow :strike-through t))
  "Horizontal rules.")

(defface efrit-markdown-quote
  '((t :inherit italic))
  "Text of a block quote.")

(defface efrit-markdown-table-header
  '((t :inherit bold :underline t))
  "Header cells of a table.")

(defface efrit-markdown-table-border
  '((t :inherit shadow))
  "The bars between table cells.")

(defface efrit-markdown-image-alt
  '((t :inherit shadow :slant italic))
  "Alt text of an image that cannot be shown, or is loading.")

(defface efrit-markdown-quote-bar
  '((t :inherit shadow))
  "The bar drawn in place of a block quote's `> '.")

;;;; Properties and helpers

(defconst efrit-markdown--frozen 'efrit-markdown-frozen
  "Property on rendered ranges the inline passes must not touch.")

(defun efrit-markdown--frozen-p (pos)
  "Non-nil when POS is inside rendered code."
  (get-text-property pos efrit-markdown--frozen))

(defun efrit-markdown--span-frozen-p (start end)
  "Non-nil when any character of START..END is frozen."
  (text-property-not-all start end efrit-markdown--frozen nil))

(defun efrit-markdown--add-face (start end face)
  "Add FACE to START..END, keeping faces already there."
  (add-face-text-property start end face)
  ;; Mirror for font-lock: the whole composed face list
  (let ((pos start))
    (while (< pos end)
      (let ((next (or (next-single-property-change pos 'face nil end) end)))
        (put-text-property pos next 'font-lock-face (get-text-property pos 'face))
        (setq pos next)))))

(defun efrit-markdown--delete (start end)
  "Delete START..END, keeping markers and point sensible."
  (delete-region start end))

;;;; Links

(defvar efrit-markdown-link-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'efrit-markdown-follow-link)
    (define-key map [mouse-1] #'efrit-markdown-follow-link)
    (define-key map [mouse-2] #'efrit-markdown-follow-link)
    map)
  "Keys on a rendered link.")

(defun efrit-markdown-link-at-point (&optional pos)
  "The link target at POS (default point): a URL, or (FILE LINE COLUMN)."
  (get-text-property (or pos (point)) 'efrit-markdown-target))

(defun efrit-markdown-follow-link (&optional event)
  "Open the link at point (or at EVENT's position)."
  (interactive (list last-nonmenu-event))
  (let* ((pos (if (and event (listp event) (eventp event))
                  (posn-point (event-end event))
                (point)))
         (target (efrit-markdown-link-at-point pos)))
    (pcase target
      ((pred stringp) (browse-url target))
      (`(,file ,line ,column . ,rest)
       (funcall efrit-markdown-open-file-function file)
       (when line
         (goto-char (point-min))
         (forward-line (1- line))
         (when column (move-to-column column))
         ;; a range: select it, so the lines stand out
         (when-let* ((end (car rest)))
           (when (> end line)
             (push-mark (point) t t)
             (forward-line (- end line)) (end-of-line)
             (exchange-point-and-mark)))))
      (_ (user-error "No link here")))))

(defun efrit-markdown--linkify (start end target help)
  "Make START..END a link to TARGET with HELP as its help-echo."
  (efrit-markdown--add-face start end 'efrit-markdown-link)
  (add-text-properties start end
                       (list 'efrit-markdown-target target
                             'keymap efrit-markdown-link-map
                             'mouse-face 'highlight
                             'help-echo help
                             'follow-link t)))

;;;; Passes over a region
;;
;; Each pass walks START..END (markers: the passes delete markup, so
;; the caller keeps END as a marker) and rewrites in place.

(defun efrit-markdown--pass-fences (start end complete)
  "Render complete fenced code blocks in START..END.
Returns the position of an open fence (a block still streaming) or
nil.  With COMPLETE, an open fence at the end is rendered as is."
  (goto-char start)
  (let ((open nil))
    (while (and (not open)
                (re-search-forward "^\\(`\\{3,\\}\\|~\\{3,\\}\\)[ \t]*\\([^ \t\n`]*\\)[^\n]*\n" end t))
      (let* ((fence-start (match-beginning 0))
             (fence (match-string 1))
             (lang (match-string 2))
             (body-start (match-end 0))
             (close-re (format "^%s%s*[ \t]*$" (regexp-quote fence) (regexp-quote (substring fence 0 1))))
             (body-end (and (re-search-forward close-re end t) (match-beginning 0)))
             (close-end (and body-end (min end (1+ (match-end 0))))))
        (cond
         ((and (not body-end) (not complete))
          (setq open fence-start))
         (t
          (unless body-end (setq body-end end close-end end))
          (efrit-markdown--render-code-block fence-start body-start body-end close-end lang)
          (goto-char fence-start)))))
    open))

(defconst efrit-markdown--table-row-regexp "^[ \t]*|?[^\n|]*|[^\n]*$"
  "A pipe table row: at least one bar; the outer bars are optional
\(GitHub allows `a | b' rows, and models write them).")

(defconst efrit-markdown--table-separator-regexp
  "^[ \t]*|?\\(?:[ \t]*:?-+:?[ \t]*|\\)*[ \t]*:?-+:?[ \t]*|?[ \t]*$"
  "The row under the header: dashes with optional colons, bar-separated,
outer bars optional.  Must contain at least one bar overall to be a
separator (checked by the caller).")

(defun efrit-markdown--table-cells (line)
  "LINE's cells, trimmed; `\\|' stays a bar.
A leading or trailing bar is a border, not an empty cell: rows may
have them or not, and models mix the two forms in one table
\(`| Fruit | Count |' over `apple | 3 |', 2026-09-28)."
  (let* ((trimmed (string-trim line))
         (escaped (replace-regexp-in-string "\\\\|" "\x00" trimmed t t))
         (inner (string-trim (string-trim escaped "|" "|")))
         (parts (split-string inner "|")))
    (mapcar (lambda (c) (string-trim (replace-regexp-in-string "\x00" "|" c t t))) parts)))

(defun efrit-markdown--table-alignments (separator)
  "Each column's alignment from SEPARATOR: `left', `right' or `center'."
  (mapcar (lambda (cell)
            (let ((l (string-prefix-p ":" cell)) (r (string-suffix-p ":" cell)))
              (cond ((and l r) 'center) (r 'right) (t 'left))))
          (efrit-markdown--table-cells separator)))

(defun efrit-markdown--table-pad (text width align)
  "TEXT padded to WIDTH columns per ALIGN."
  (let* ((w (string-width text)) (pad (max 0 (- width w))))
    (pcase align
      ('right (concat (make-string pad ?\s) text))
      ('center (concat (make-string (/ pad 2) ?\s) text (make-string (- pad (/ pad 2)) ?\s)))
      (_ (concat text (make-string pad ?\s))))))

(defun efrit-markdown--render-cell (text)
  "TEXT with its inline Markdown rendered, as a propertized string."
  (with-temp-buffer
    (insert text)
    (let ((end (copy-marker (point-max) t)))
      (efrit-markdown--pass-inline-code (point-min) end)
      (efrit-markdown--pass-links (point-min) end)
      (efrit-markdown--pass-emphasis (point-min) end))
    (buffer-string)))

(defun efrit-markdown--render-table (start end)
  "Redraw the pipe table in START..END as aligned columns.
Each cell's inline Markdown is rendered first so the columns line up
on what is shown; bars are dimmed, the header row gets its face, the
separator row goes."
  (let* ((lines (split-string (buffer-substring-no-properties start end) "\n" t))
         (header (car lines))
         (separator (cadr lines))
         (body (cddr lines))
         (aligns (efrit-markdown--table-alignments separator))
         (rows (mapcar (lambda (line) (mapcar #'efrit-markdown--render-cell
                                              (efrit-markdown--table-cells line)))
                       (cons header body)))
         (ncol (apply #'max (length aligns) (mapcar #'length rows)))
         (widths (make-list ncol 1)))
    (dolist (row rows)
      (setq widths (cl-loop for i below ncol
                            collect (max (nth i widths) (string-width (or (nth i row) ""))))))
    (let ((bar (propertize " │ " 'face 'efrit-markdown-table-border))
          (props (text-properties-at start)))
      (delete-region start end)
      (goto-char start)
      (cl-loop for row in rows for r from 0 do
               (let ((line-start (point)))
                 (cl-loop for i below ncol do
                          (when (> i 0) (insert bar))
                          (let ((cell-start (point)))
                            (insert (if (and (= i (1- ncol)) (eq (or (nth i aligns) 'left) 'left))
                                        (or (nth i row) "")
                                      (efrit-markdown--table-pad (or (nth i row) "") (nth i widths)
                                                                 (or (nth i aligns) 'left))))
                            (when (= r 0)
                              (efrit-markdown--add-face cell-start (point) 'efrit-markdown-table-header))))
                 (insert "\n")
                 (ignore line-start)))
      ;; Under the header: a rule as wide as the table
      (save-excursion
        (goto-char start) (forward-line 1)
        (let ((rule (propertize (make-string (+ (apply #'+ widths) (* 3 (1- ncol))) ?─)
                                'face 'efrit-markdown-table-border)))
          (insert rule "\n")))
      (let ((keep (cl-loop for (k v) on props by #'cddr
                           unless (memq k '(face font-lock-face fontified efrit-markdown-frozen))
                           append (list k v))))
        (when keep (add-text-properties start (point) keep))))))

(defun efrit-markdown--pass-tables (start end complete)
  "Render pipe tables in START..END; return the start of a table that
may still grow (its last row touches END) unless COMPLETE."
  (goto-char start)
  (let ((open nil))
    (while (and (not open) efrit-markdown-tables
                (re-search-forward efrit-markdown--table-row-regexp end t))
      (let ((table-start (match-beginning 0)))
        (forward-line 1)
        (cond
         ;; A bar row that is the last (or next-to-last, unfinished)
         ;; line of a streaming region may be a header whose separator
         ;; has not arrived: hold the frontier there, or the watermark
         ;; moves past the header and the table is never seen as one
         ;; (2026-09-28: rendered as "│ Fruit │ Count" over plain rows)
         ((and (not complete)
               (or (>= (point) end)
                   (and (< (point) end)
                        (>= (save-excursion (goto-char (point)) (line-end-position)) end))))
          (setq open table-start))
         ((not (and (< (point) end)
                    (looking-at efrit-markdown--table-separator-regexp)
                    (string-match-p "|" (match-string 0))
                    (not (efrit-markdown--span-frozen-p table-start (point)))))
          (goto-char (max (1+ table-start) (point))))
         (t
          (forward-line 1)
          (while (and (< (point) end) (looking-at efrit-markdown--table-row-regexp))
            (forward-line 1))
          (let ((table-end (point)))
            (if (and (not complete) (>= table-end end))
                (setq open table-start)
              (efrit-markdown--render-table table-start table-end)
              (efrit-markdown--set-frozen table-start (point) t)))))))
    open))

(defun efrit-markdown--set-frozen (start end value)
  "Mark START..END frozen (VALUE non-nil) for the inline passes.
A rendered table's bars and padding must not be re-read as emphasis."
  (put-text-property start end efrit-markdown--frozen value))

(defun efrit-markdown--language-mode (lang)
  "The major mode for fence language LANG, or nil."
  (when (and lang (not (string-empty-p lang)))
    (let ((mode (or (cdr (assoc (downcase lang) efrit-markdown-language-modes))
                    (intern-soft (concat (downcase lang) "-mode")))))
      (and mode (fboundp mode) mode))))

(defun efrit-markdown--fontify-string (text mode)
  "TEXT with the faces MODE gives it, as `face' properties."
  (condition-case nil
      (with-temp-buffer
        (insert text)
        (delay-mode-hooks (funcall mode))
        (font-lock-ensure)
        ;; font-lock leaves the faces under `face'; keep them
        (buffer-string))
    (error text)))

(defun efrit-markdown--render-code-block (fence-start body-start body-end close-end lang)
  "Turn the fenced block at FENCE-START into a highlighted block.
The fence lines are deleted; BODY-START..BODY-END stays, fontified per
LANG, on a block background, with the language as a small label."
  (let* ((body (buffer-substring-no-properties body-start body-end))
         (mode (and efrit-markdown-fontify-code (efrit-markdown--language-mode lang)))
         (styled (if mode (efrit-markdown--fontify-string body mode) body))
         (props (text-properties-at fence-start)))
    ;; Replace fence + body + close with label + styled body
    (delete-region fence-start close-end)
    (goto-char fence-start)
    (let ((start (point)))
      (insert (propertize (concat (if (and lang (not (string-empty-p lang))) lang "code") "\n")
                          'face 'efrit-markdown-code-label))
      (insert styled)
      (unless (bolp) (insert "\n"))
      (let ((end (point)))
        ;; The fragment's own properties (type, id, read-only...) so the
        ;; block stays part of the message
        (let ((keep (cl-loop for (k v) on props by #'cddr
                             unless (memq k '(face font-lock-face fontified efrit-markdown-frozen))
                             append (list k v))))
          (when keep (add-text-properties start end keep)))
        (efrit-markdown--add-face start end 'efrit-markdown-code-block)
        ;; One id per block, and the body text kept raw, so commands
        ;; can act on "the block at point" without re-reading the
        ;; fontified display
        (add-text-properties start end (list efrit-markdown--frozen t 'fontified t
                                             'efrit-markdown-block
                                             (list (or lang "")
                                                   (concat (string-trim-right body "\n+") "\n"))))
        (goto-char end)))))

;;;; Code blocks as things

(defun efrit-markdown-block-at (&optional pos)
  "The code block at POS (default point) as (LANG . BODY), or nil."
  (when-let* ((b (get-text-property (or pos (point)) 'efrit-markdown-block)))
    (cons (nth 0 b) (nth 1 b))))

(defun efrit-markdown-blocks (&optional start end)
  "Every code block in START..END (default the buffer), in order, as (LANG . BODY)."
  (let ((pos (or start (point-min))) (end (or end (point-max))) (out nil) (last nil))
    (while (setq pos (text-property-not-all pos end 'efrit-markdown-block nil))
      (let ((b (get-text-property pos 'efrit-markdown-block)))
        (unless (eq b last)
          (push (cons (nth 0 b) (nth 1 b)) out)
          (setq last b)))
      (setq pos (or (next-single-property-change pos 'efrit-markdown-block nil end) end)))
    (nreverse out)))

(defun efrit-markdown--block-label (block n)
  "A completion label for BLOCK number N: its language and first line."
  (format "%d: [%s] %s" n (if (string-empty-p (car block)) "code" (car block))
          (truncate-string-to-width (car (split-string (cdr block) "\n" t)) 60 nil nil "…")))

(defun efrit-markdown-read-block (&optional prompt)
  "The block at point, else one chosen by completion over the buffer's blocks."
  (or (efrit-markdown-block-at)
      (let ((blocks (efrit-markdown-blocks)))
        (cond
         ((null blocks) (user-error "No code block here"))
         ((null (cdr blocks)) (car blocks))
         (t (let* ((labels (cl-loop for b in blocks for i from 1 collect (cons (efrit-markdown--block-label b i) b)))
                   (choice (completing-read (or prompt "Code block: ") labels nil t)))
              (cdr (assoc choice labels))))))))

(defun efrit-markdown-copy-block ()
  "Copy the code block at point (or a chosen one) to the kill ring."
  (interactive)
  (let ((block (efrit-markdown-read-block "Copy block: ")))
    (kill-new (cdr block))
    (message "Copied %d lines of %s" (length (split-string (cdr block) "\n" t))
             (if (string-empty-p (car block)) "code" (car block)))))

(defun efrit-markdown-insert-block-other-window (&optional then-return)
  "Insert the code block at point (or a chosen one) at point in the other window.
The other window's buffer gets the text at its point; focus stays here
unless THEN-RETURN is nil and a prefix argument was given."
  (interactive "P")
  (let* ((block (efrit-markdown-read-block "Insert block: "))
         (here (current-buffer))
         (win (seq-find (lambda (w) (not (eq (window-buffer w) here)))
                        (list (next-window nil 'no-minibuf) (get-mru-window nil nil t))))
         (target (and win (window-buffer win))))
    (unless target
      (user-error "No other window to insert into"))
    (with-current-buffer target
      (when buffer-read-only (user-error "%s is read-only" (buffer-name)))
      (save-excursion
        (goto-char (window-point win))
        (insert (cdr block))
        (unless (string-suffix-p "\n" (cdr block)) (insert "\n"))
        (set-window-point win (point))))
    (message "Inserted %d lines into %s" (length (split-string (cdr block) "\n" t)) (buffer-name target))
    (when then-return (select-window win))))

(defun efrit-markdown--pass-regexp (start end regexp function)
  "Call FUNCTION at every match of REGEXP in START..END outside frozen text.
FUNCTION runs with the match data set and may delete text; it must
leave point where scanning continues."
  (goto-char start)
  (while (re-search-forward regexp end t)
    (if (efrit-markdown--span-frozen-p (match-beginning 0) (match-end 0))
        (goto-char (match-end 0))
      (funcall function))))

(defun efrit-markdown--pass-quotes (start end)
  "Block quotes: the `> ' shows as a bar, the line in the quote face.
The `> ' stays in the buffer (copying gives Markdown back); only its
display changes."
  (efrit-markdown--pass-regexp
   start end "^\\(> ?\\)\\([^\n]*\\)$"
   (lambda ()
     (let ((mark-start (match-beginning 1)) (mark-end (match-end 1))
           (text-start (match-beginning 2)) (text-end (match-end 2)))
       (put-text-property mark-start mark-end 'display
                          (propertize "▌ " 'face 'efrit-markdown-quote-bar))
       (efrit-markdown--add-face text-start text-end 'efrit-markdown-quote)
       (goto-char text-end)))))

(defun efrit-markdown--pass-headers (start end)
  "ATX headers: the hashes go, the line gets the header face."
  (efrit-markdown--pass-regexp
   start end "^\\(#\\{1,6\\}\\)[ \t]+\\([^\n]*\\)$"
   (lambda ()
     (let ((text-start (match-beginning 2)) (text-end (match-end 2)))
       (efrit-markdown--add-face text-start text-end 'efrit-markdown-header)
       (delete-region (match-beginning 1) text-start)))))

(defun efrit-markdown--pass-rules (start end)
  "Horizontal rules become a thin line."
  (efrit-markdown--pass-regexp
   start end "^\\(?:-\\{3,\\}\\|\\*\\{3,\\}\\|_\\{3,\\}\\)[ \t]*$"
   (lambda ()
     (let ((s (match-beginning 0)) (e (match-end 0)))
       (delete-region s e)
       (efrit-markdown--insert-like s (make-string 40 ?\s))
       (efrit-markdown--add-face s (point) 'efrit-markdown-rule)))))

(defun efrit-markdown--insert-like (pos text)
  "Insert TEXT at POS carrying the non-face properties of the char at POS.
So a bullet or a label stays part of the message it is in (its
`efrit-id', read-only, field): a bare insert broke the message into
runs and readers of the buffer got a tail (2026-09-25)."
  (goto-char pos)
  (let ((props (cl-loop for (k v) on (text-properties-at pos) by #'cddr
                        unless (memq k '(face font-lock-face fontified efrit-markdown-frozen
                                             efrit-markdown-target keymap mouse-face help-echo))
                        append (list k v))))
    (insert (if props (apply #'propertize text props) text))))

(defun efrit-markdown--pass-bullets (start end)
  "List bullets get a face; `- ' becomes `• '."
  (efrit-markdown--pass-regexp
   start end "^\\([ \t]*\\)\\([-*+]\\|[0-9]+\\.\\)[ \t]+"
   (lambda ()
     (let ((s (match-beginning 2)) (e (match-end 2)))
       (when (memq (char-after s) '(?- ?* ?+))
         (goto-char s) (delete-char 1)
         (efrit-markdown--insert-like s "•") (setq e (point)))
       (efrit-markdown--add-face s e 'efrit-markdown-bullet)
       (goto-char (match-end 0))))))

(defun efrit-markdown--pass-inline-code (start end)
  "Code spans: backticks go, the span is frozen with the code face.
File references inside are linked first, since the model puts nearly
every citation in backticks."
  (efrit-markdown--pass-regexp
   start end "\\(`+\\)\\([^`\n]+?\\)\\1"
   (lambda ()
     (let* ((open-s (match-beginning 1)) (open-e (match-end 1))
            (text-e (match-end 2))
            (close-e (match-end 0)))
       (delete-region text-e close-e)
       (delete-region open-s open-e)
       (let ((s open-s) (e (- text-e (- open-e open-s))))
         (efrit-markdown--add-face s e 'efrit-markdown-inline-code)
         (efrit-markdown--link-file-refs s e)
         (put-text-property s e efrit-markdown--frozen t)
         (goto-char e))))))

(defun efrit-markdown--pass-emphasis (start end)
  "Bold, italic, strikethrough.  Delimiters go, the text gets the face.
Runs until nothing changes, so nested `***x***' resolves."
  (let ((changed t))
    (while changed
      (setq changed nil)
      (dolist (spec '(("\\*\\*\\([^*\n]+?\\)\\*\\*" . efrit-markdown-bold)
                      ("__\\([^_\n]+?\\)__" . efrit-markdown-bold)
                      ("~~\\([^~\n]+?\\)~~" . efrit-markdown-strike)
                      ("\\(?:^\\|[^*[:alnum:]]\\)\\(\\*\\([^*\n]+?\\)\\*\\)" . efrit-markdown-italic)
                      ("\\(?:^\\|[^_[:alnum:]]\\)\\(_\\([^_\n]+?\\)_\\)\\(?:[^_[:alnum:]]\\|$\\)" . efrit-markdown-italic)))
        (efrit-markdown--pass-regexp
         start end (car spec)
         (lambda ()
           (let* ((italic (eq (cdr spec) 'efrit-markdown-italic))
                  (whole-s (if italic (match-beginning 1) (match-beginning 0)))
                  (whole-e (if italic (match-end 1) (match-end 0)))
                  (text-s (if italic (match-beginning 2) (match-beginning 1)))
                  (text-e (if italic (match-end 2) (match-end 1)))
                  (before (- text-s whole-s))
                  (after (- whole-e text-e)))
             (delete-region text-e whole-e)
             (delete-region whole-s text-s)
             (efrit-markdown--add-face whole-s (- text-e before) (cdr spec))
             (goto-char (- whole-e before after))
             (setq changed t))))))))

(defun efrit-markdown--image-file (source)
  "A local file for SOURCE: a path, a file: URL, or a data: URL written
to the cache.  nil for http(s) (fetched separately) or nothing usable."
  (cond
   ((string-match "\\`data:image/\\([a-z+]+\\);base64,\\(.*\\)\\'" source)
    (let* ((ext (match-string 1 source))
           (data (ignore-errors (base64-decode-string (match-string 2 source))))
           (file (and data (expand-file-name (format "data-%s.%s" (md5 source) ext)
                                             efrit-markdown-image-cache-directory))))
      (when file
        (make-directory efrit-markdown-image-cache-directory t)
        (unless (file-exists-p file)
          (let ((coding-system-for-write 'binary))
            (with-temp-file file (set-buffer-multibyte nil) (insert data))))
        file)))
   ((string-match-p "\\`https?://" source) nil)
   (t (let ((path (if (string-prefix-p "file://" source) (substring source 7) source)))
        (efrit-markdown--file-ref-file path)))))

(defun efrit-markdown--image-cache-file (url)
  "Where URL's image is cached."
  (expand-file-name (format "url-%s%s" (md5 url)
                            (let ((ext (file-name-extension (car (split-string url "[?#]")))))
                              (if (and ext (< (length ext) 6)) (concat "." ext) "")))
                    efrit-markdown-image-cache-directory))

(defun efrit-markdown--make-image (file width &optional exact)
  "FILE as an image WIDTH pixels wide at most, or exactly WIDTH with EXACT, or nil.
The default draw caps a big picture; a user's + / - sets the width
outright, so a small picture grows too (tour 2026-09-30: the number
went up, the 8-pixel square did not)."
  (ignore-errors
    (if exact
        (create-image file nil nil :width width :ascent 'center)
      (create-image file nil nil :max-width width :ascent 'center))))

(defun efrit-markdown--show-image (start end file &optional exact)
  "Put FILE's image on START..END, keeping the alt text under it.
EXACT: the recorded width is the size, not a cap."
  (let ((width (or (get-text-property start 'efrit-markdown-image-width)
                   efrit-markdown-image-max-width)))
    (if-let* ((image (efrit-markdown--make-image file width exact)))
        (add-text-properties start end (list 'display image
                                             'efrit-markdown-image file
                                             'efrit-markdown-image-width width
                                             'help-echo (abbreviate-file-name file)))
      (efrit-markdown--add-face start end 'efrit-markdown-image-alt))))

(defun efrit-markdown--fetch-image (url buffer start end)
  "Fetch URL in the background; on success show it on START..END of BUFFER.
START and END are markers."
  (make-directory efrit-markdown-image-cache-directory t)
  (let ((file (efrit-markdown--image-cache-file url)))
    (if (file-exists-p file)
        (with-current-buffer buffer
          (let ((inhibit-read-only t)) (efrit-markdown--show-image start end file)))
      (url-retrieve
       url
       (lambda (status)
         (unwind-protect
             (unless (plist-get status :error)
               (goto-char (point-min))
               (when (re-search-forward "\n\n" nil t)
                 (let ((coding-system-for-write 'binary))
                   (write-region (point) (point-max) file nil 'quiet)))
               (when (and (buffer-live-p buffer) (marker-position start))
                 (with-current-buffer buffer
                   (let ((inhibit-read-only t))
                     (efrit-markdown--show-image start end file)))))
           (kill-buffer (current-buffer))))
       nil t t))))

(defun efrit-markdown--pass-images (start end)
  "![alt](source): the alt text stays; on a graphic display the image
is drawn over it (`display'), fetched first when remote."
  (efrit-markdown--pass-regexp
   start end "!\\[\\([^][\n]*\\)\\](\\(<[^>\n]+>\\|[^()[:space:]\n]+\\))"
   (lambda ()
     (let* ((s (match-beginning 0)) (e (match-end 0))
            (alt (match-string 1))
            (source (string-trim (match-string 2) "<" ">"))
            (label (if (string-empty-p alt) (file-name-nondirectory source) alt)))
       (delete-region s e)
       (efrit-markdown--insert-like s (concat "[" label "]"))
       (let ((ls s) (le (point)))
         (efrit-markdown--add-face ls le 'efrit-markdown-image-alt)
         (put-text-property ls le 'efrit-markdown-image-source source)
         (efrit-markdown--set-frozen ls le t)
         (when (and efrit-markdown-images (display-graphic-p))
           (if-let* ((file (efrit-markdown--image-file source)))
               (efrit-markdown--show-image ls le file)
             (when (string-match-p "\\`https?://" source)
               (efrit-markdown--fetch-image source (current-buffer)
                                            (copy-marker ls) (copy-marker le t))))))))))

(defun efrit-markdown--pass-links (start end)
  "[title](url): the title stays as a link to the url."
  (efrit-markdown--pass-regexp
   start end "\\[\\([^][\n]+\\)\\](\\(<[^>\n]+>\\|[^()[:space:]\n]*\\(?:([^()\n]*)[^()[:space:]\n]*\\)*\\))"
   (lambda ()
     (let* ((s (match-beginning 0)) (e (match-end 0))
            (title (match-string 1))
            (url (string-trim (match-string 2) "<" ">")))
       (delete-region s e)
       (efrit-markdown--insert-like s title)
       (efrit-markdown--linkify s (point) url url)))))

(defun efrit-markdown--pass-urls (start end)
  "Bare URLs become links."
  (efrit-markdown--pass-regexp
   start end browse-url-button-regexp
   (lambda ()
     (let ((s (match-beginning 0)) (e (match-end 0)))
       (unless (get-text-property s 'efrit-markdown-target)
         (let ((url (string-trim-right (buffer-substring-no-properties s e) "[.,;:!?)]+")))
           (efrit-markdown--linkify s (+ s (length url)) url url)))
       (goto-char e)))))

(defconst efrit-markdown--file-ref-body
  "\\(\\(?:~\\|\\.\\{1,2\\}\\)?/?\\(?:[[:alnum:]_.-]+/\\)*[[:alnum:]_-]+\\.[[:alnum:]]+\\)\\(?::L?\\([0-9]+\\)\\(?::\\([0-9]+\\)\\|-L?\\([0-9]+\\)\\)?\\|#L\\([0-9]+\\)\\(?:-L?\\([0-9]+\\)\\)?\\)?"
  "A path with an extension, optionally `:LINE', `:LINE:COL', `:L1-L2',
`#LNN' or `#L1-L2'.  Group 1 path, 2 line, 3 column, 4 end line
\\(the : form), 5 line and 6 end line (the #L form).")

(defun efrit-markdown--file-ref-target (file)
  "The link target for the file reference just matched: (FILE LINE COL END-LINE)."
  (let ((line (or (match-string 2) (match-string 5)))
        (col (match-string 3))
        (end (or (match-string 4) (match-string 6))))
    (append (list file (and line (string-to-number line))
                  (and col (string-to-number col)))
            ;; a fourth element only for a range: (FILE LINE COL) stays
            ;; the shape other code and tests know
            (and end (list (string-to-number end))))))

(defconst efrit-markdown--file-ref-regexp
  (concat "\\(?:^\\|[^[:alnum:]_/.-]\\)" efrit-markdown--file-ref-body)
  "A file reference in prose: at a line start or after a non-path character.")

(defconst efrit-markdown--file-ref-in-code-regexp
  (concat "\\(?:\\=\\|^\\|[^[:alnum:]_/.-]\\)" efrit-markdown--file-ref-body)
  "A file reference inside a code span: also at the span's very start.")

(defun efrit-markdown--file-ref-file (path)
  "PATH as an existing file, relative to the project when relative, or nil."
  (let ((root (or (and (fboundp 'efrit-tool--get-project-root)
                       (ignore-errors (funcall 'efrit-tool--get-project-root)))
                  default-directory)))
    (let ((file (expand-file-name path root)))
      (and (file-exists-p file) file))))

(defun efrit-markdown--link-file-refs (start end)
  "Link the file references in START..END that name existing files."
  (goto-char start)
  (while (re-search-forward efrit-markdown--file-ref-in-code-regexp end t)
    (let* ((s (match-beginning 1)) (e (match-end 0))
           (path (match-string 1))
           (file (efrit-markdown--file-ref-file path)))
      (when (and file (not (get-text-property s 'efrit-markdown-target)))
        (efrit-markdown--linkify s e (efrit-markdown--file-ref-target file)
                                 (concat "open " (abbreviate-file-name file))))
      (goto-char e))))

(defun efrit-markdown--pass-file-refs (start end)
  "File references in prose (outside code, which the code pass did)."
  (goto-char start)
  (while (re-search-forward efrit-markdown--file-ref-regexp end t)
    (let ((s (match-beginning 1)) (e (match-end 0)))
      (unless (or (efrit-markdown--span-frozen-p s e)
                  (get-text-property s 'efrit-markdown-target))
        (let* ((path (match-string 1))
               (file (efrit-markdown--file-ref-file path)))
          (when file
            (efrit-markdown--linkify s e (efrit-markdown--file-ref-target file)
                                     (concat "open " (abbreviate-file-name file))))))
      (goto-char e))))

;;;; Entry point

(defun efrit-markdown--watermark (start)
  "The rendered frontier of the region at START, as a position."
  (let ((offset (get-text-property start 'efrit-markdown-watermark)))
    (if offset (min (+ start offset) (point-max)) start)))

(defun efrit-markdown--set-watermark (start pos)
  "Record POS as the frontier of the region at START."
  (when (< start (point-max))
    (put-text-property start (1+ start) 'efrit-markdown-watermark (- pos start))))

(defun efrit-markdown--safe-frontier (end complete open-fence)
  "Where rendering may stop being final: the start of the last line,
or of an open fence, whichever is earlier.  END when COMPLETE."
  (if complete
      end
    (let ((last-line (save-excursion (goto-char end) (line-beginning-position))))
      (min last-line (or open-fence end)))))

(defun efrit-markdown-render (start end &optional complete)
  "Render the Markdown in START..END in place; return the new END.
Text before the stored watermark is left as it is.  Constructs that
may still grow are held back unless COMPLETE.  START and END may be
markers.  Safe to call on every streamed chunk."
  (when efrit-markdown-enabled
    (let* ((start (if (markerp start) (marker-position start) start))
           (end-marker (if (markerp end) end (copy-marker end t)))
           (from (max start (efrit-markdown--watermark start)))
           (inhibit-read-only t)
           (inhibit-modification-hooks t)
           (buffer-undo-list t))
      (save-excursion
        (save-restriction
          (widen)
          (let* ((open-fence (efrit-markdown--pass-fences from end-marker complete))
                 (open-table (efrit-markdown--pass-tables from end-marker complete))
                 (frontier (efrit-markdown--safe-frontier
                            end-marker complete
                            (if (and open-fence open-table) (min open-fence open-table)
                              (or open-fence open-table))))
                 (limit (copy-marker frontier t)))
            (when (< from limit)
              (efrit-markdown--pass-headers from limit)
              (efrit-markdown--pass-images from limit)
              (efrit-markdown--pass-quotes from limit)
              (efrit-markdown--pass-rules from limit)
              (efrit-markdown--pass-inline-code from limit)
              (efrit-markdown--pass-links from limit)
              (efrit-markdown--pass-emphasis from limit)
              (efrit-markdown--pass-bullets from limit)
              (efrit-markdown--pass-urls from limit)
              (efrit-markdown--pass-file-refs from limit)
              (put-text-property from limit 'fontified t))
            (efrit-markdown--set-watermark start (marker-position limit))
            (set-marker limit nil))))
      (prog1 (marker-position end-marker)
        (unless (markerp end) (set-marker end-marker nil))))))

;;;; Image scaling

(defun efrit-markdown--image-bounds-at (pos)
  "The (START . END) of the image at POS, or nil."
  (when (get-text-property pos 'efrit-markdown-image)
    (cons (or (previous-single-property-change (1+ pos) 'efrit-markdown-image) (point-min))
          (or (next-single-property-change pos 'efrit-markdown-image) (point-max)))))

(defun efrit-markdown--images-in-buffer ()
  "Bounds of every image in the buffer."
  (let ((out nil) (pos (point-min)))
    (while (setq pos (text-property-not-all pos (point-max) 'efrit-markdown-image nil))
      (let ((b (efrit-markdown--image-bounds-at pos)))
        (push b out)
        (setq pos (cdr b))))
    (nreverse out)))

(defun efrit-markdown--rescale (bounds factor)
  "Redraw the image in BOUNDS FACTOR times as wide; nil FACTOR resets."
  (let* ((start (car bounds))
         (file (get-text-property start 'efrit-markdown-image))
         (width (if factor
                    (max 32 (round (* factor (or (get-text-property start 'efrit-markdown-image-width)
                                                 efrit-markdown-image-max-width))))
                  efrit-markdown-image-max-width))
         (inhibit-read-only t))
    (put-text-property start (cdr bounds) 'efrit-markdown-image-width width)
    ;; a reset (nil factor) goes back to the cap; a scale is exact
    (efrit-markdown--show-image start (cdr bounds) file (and factor t))))

(defun efrit-markdown--scale-images (factor)
  "Scale the image at point, else every image, by FACTOR (nil: reset)."
  (let ((targets (or (and (efrit-markdown--image-bounds-at (point))
                          (list (efrit-markdown--image-bounds-at (point))))
                     (and (> (point) (point-min)) (efrit-markdown--image-bounds-at (1- (point)))
                          (list (efrit-markdown--image-bounds-at (1- (point)))))
                     (efrit-markdown--images-in-buffer))))
    (if (null targets)
        (message "No images here")
      (dolist (b targets) (efrit-markdown--rescale b factor))
      (message "%d image%s at %d px" (length targets) (if (= 1 (length targets)) "" "s")
               (get-text-property (car (car targets)) 'efrit-markdown-image-width)))))

(defun efrit-markdown-image-scale-increase ()
  "Widen the image at point (else all images) by one step."
  (interactive)
  (efrit-markdown--scale-images efrit-markdown-image-scale-step))

(defun efrit-markdown-image-scale-decrease ()
  "Narrow the image at point (else all images) by one step."
  (interactive)
  (efrit-markdown--scale-images (/ 1.0 efrit-markdown-image-scale-step)))

(defun efrit-markdown-image-scale-reset ()
  "Draw the image at point (else all images) at the default width."
  (interactive)
  (efrit-markdown--scale-images nil))

(defun efrit-markdown-render-string (text)
  "TEXT rendered as Markdown, as a propertized string."
  (with-temp-buffer
    (insert text)
    (efrit-markdown-render (point-min) (point-max) t)
    (buffer-string)))

(provide 'efrit-markdown)

;;; efrit-markdown.el ends here
