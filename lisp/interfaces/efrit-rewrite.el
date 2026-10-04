;;; efrit-rewrite.el --- Rewrite the region with the model, in place -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.9.2
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, ai, convenience

;;; Commentary:

;; `efrit-rewrite-region': select text in any buffer, say what you
;; want done to it, and the model's rewrite comes back as a diff you
;; accept or refuse.  A side request (`efrit-ask-once'); the agent
;; buffer is not involved.
;;
;; The request uses an editable-region protocol (after minuet's duet,
;; 2026-09-28): the region is sent between markers with some context
;; either side, and the model must return exactly one region between
;; the same markers.  That is verifiable: a reply without exactly one
;; region is rejected instead of pasted.  Text the model echoes from
;; the surrounding context is trimmed off.
;;
;; The target is tracked by markers and by `buffer-chars-modified-tick':
;; if the buffer changed while the model worked, or while you looked
;; at the diff (timers and process filters keep running under
;; `y-or-n-p'), nothing is replaced and you are told.  The replacement
;; itself is `replace-region-contents', so markers and point survive.
;; A read-only target gets the rewrite on the kill ring instead.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'efrit-ask)
(require 'efrit-text-window)
(require 'efrit-inline-diff)
(require 'efrit-log)
(require 'efrit-vcs)
(require 'efrit-ui-helpers)

(defgroup efrit-rewrite nil
  "Rewrite a region with the model."
  :group 'efrit)

(defcustom efrit-rewrite-context-chars 6000
  "Characters of context sent around the region (not editable).
Split by `efrit-text-window-ratio' before and after; whole lines."
  :type 'integer
  :group 'efrit-rewrite)

(defcustom efrit-rewrite-confirm t
  "Non-nil shows the diff and asks before replacing; nil replaces at once."
  :type 'boolean
  :group 'efrit-rewrite)

(defcustom efrit-rewrite-preview 'inline
  "How the diff is shown before you accept a rewrite.
`inline' draws removed and added lines over the region in the buffer
itself; `buffer' opens a `diff-mode' popup."
  :type '(choice (const inline) (const buffer))
  :group 'efrit-rewrite)

(defconst efrit-rewrite--start-marker "<editable_region_start>")
(defconst efrit-rewrite--end-marker "<editable_region_end>")

(defconst efrit-rewrite-system-prompt
  (format "You rewrite only the editable region of a document.

The user's text arrives with some context, and the part to change is wrapped in %s and %s.

Rules:
1. Return only the rewritten editable region, wrapped in %s and %s.
2. Text inside the region that need not change is copied verbatim: same indentation, blank lines and conventions.
3. Make the smallest change that does what the user asked.
4. Do not return explanations, markdown fences, or anything outside the region block.
5. Keep the rewrite coherent with the surrounding text, which you must not repeat."
          efrit-rewrite--start-marker efrit-rewrite--end-marker
          efrit-rewrite--start-marker efrit-rewrite--end-marker)
  "System prompt of the rewrite request.")

(defun efrit-rewrite--context (start end)
  "The text around START..END: (BEFORE . AFTER), whole lines, within the budget."
  (let ((w (efrit-text-window :start start :end end :chars efrit-rewrite-context-chars)))
    (cons (plist-get w :before) (plist-get w :after))))

(defun efrit-rewrite--prompt (instruction region before after)
  "The user message: INSTRUCTION, the header, AFTER, then BEFORE and the marked REGION.
The text after the region comes first so the editable region ends the
message: models continue best from the end of what they read (minuet
sends the suffix first to Claude)."
  (format "%s\n\n%s\n\nThe text after the region, for context only:\n<contextAfterCursor>\n%s</contextAfterCursor>\n\nThe text before the region, then the region to rewrite:\n<contextBeforeCursor>\n%s</contextBeforeCursor>\n%s%s%s"
          instruction
          (efrit-text-window-header)
          after before
          efrit-rewrite--start-marker region efrit-rewrite--end-marker))

(defun efrit-rewrite-parse (text &optional original)
  "The single editable region in the model's TEXT, or nil.
nil when the markers are missing, or appear more than once: such a
reply is not pasted.  Models put the markers on their own lines even
though the prompt does not: a newline right after the start marker
is dropped unless ORIGINAL began with one, and a newline right before
the end marker is dropped unless ORIGINAL ended with one."
  (let ((s-re (regexp-quote efrit-rewrite--start-marker))
        (e-re (regexp-quote efrit-rewrite--end-marker)))
    (when (and (= 1 (efrit-rewrite--count s-re text))
               (= 1 (efrit-rewrite--count e-re text))
               (string-match (concat s-re "\\(\\(?:.\\|\n\\)*?\\)" e-re) text))
      (let ((region (match-string 1 text)))
        (when (and (string-prefix-p "\n" region)
                   (not (and original (string-prefix-p "\n" original))))
          (setq region (substring region 1)))
        (when (and (string-suffix-p "\n" region)
                   (not (and original (string-suffix-p "\n" original))))
          (setq region (substring region 0 -1)))
        region))))

(defun efrit-rewrite--count (regexp text)
  (let ((n 0) (start 0))
    (while (string-match regexp text start)
      (cl-incf n) (setq start (match-end 0)))
    n))

(defun efrit-rewrite-trim-echo (reply before after)
  "REPLY without a prefix that repeats the end of BEFORE or a suffix
that repeats the start of AFTER.  Models often echo both; the longest
match of at least one full line is removed (after minuet 2)."
  (let ((out reply))
    ;; prefix echoing the tail of BEFORE
    (cl-loop for i from (min (length before) (length out)) downto 1
             for tail = (substring before (- (length before) i))
             when (and (string-prefix-p tail out) (string-match-p "\n" tail))
             do (setq out (substring out i)) and return nil)
    ;; suffix echoing the head of AFTER
    (cl-loop for i from (min (length after) (length out)) downto 1
             for head = (substring after 0 i)
             when (and (string-suffix-p head out) (string-match-p "\n" head))
             do (setq out (substring out 0 (- (length out) i))) and return nil)
    out))

;;;###autoload
(defun efrit-rewrite-region (start end instruction)
  "Ask the model to rewrite START..END per INSTRUCTION.
The diff is shown first, then the region is replaced.
Interactively, the region and a prompt for the instruction.  The
region must be unchanged when the answer arrives and when you accept
the diff, or nothing is replaced."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end)
             (read-string "Rewrite the region: "))
     (user-error "Select the text to rewrite first")))
  (let* ((buffer (current-buffer))
         (start-m (copy-marker start))
         (end-m (copy-marker end t))
         (original (buffer-substring-no-properties start end))
         (tick (buffer-chars-modified-tick))
         (context (efrit-rewrite--context start end))
         (prompt (efrit-rewrite--prompt instruction original (car context) (cdr context))))
    (deactivate-mark)
    (message "efrit: rewriting %d characters…" (length original))
    (efrit-ask-once
     prompt
     (lambda (text message)
       (cond
        ((null text) (message "efrit rewrite: %s" message))
        ((not (buffer-live-p buffer)) (message "efrit rewrite: the buffer is gone"))
        (t
         (let ((region (efrit-rewrite-parse text original)))
           (if (null region)
               (progn
                 (kill-new text)
                 (message "efrit rewrite: the reply was not one editable region; raw text on the kill ring"))
             (efrit-rewrite--apply buffer start-m end-m original tick
                                   (efrit-rewrite-trim-echo region (car context) (cdr context))))))))
     :system efrit-rewrite-system-prompt
     :purpose (format "rewriting a region of %s" (buffer-name buffer))
     :key (format "efrit-rewrite %s" (buffer-name buffer)))))

(defun efrit-rewrite--intact-p (buffer start-m end-m original tick)
  "Non-nil if BUFFER's START-M..END-M still holds ORIGINAL and the tick is TICK."
  (and (buffer-live-p buffer)
       (marker-position start-m) (marker-position end-m)
       (with-current-buffer buffer
         (and (= tick (buffer-chars-modified-tick))
              (equal original (buffer-substring-no-properties start-m end-m))))))

(defun efrit-rewrite--ask (buffer start-m original replacement)
  "Show the change from ORIGINAL to REPLACEMENT and ask; non-nil to apply.
Per `efrit-rewrite-preview': overlays in BUFFER at START-M, or a
diff popup.  The preview is taken down before returning."
  (if (eq efrit-rewrite-preview 'inline)
      (let ((win (get-buffer-window buffer)))
        (efrit-inline-diff-show buffer start-m (+ start-m (length original)) replacement)
        (when win
          (with-selected-window win (goto-char start-m) (recenter)))
        (unwind-protect
            (y-or-n-p "Apply this rewrite (shown in the buffer)? ")
          (efrit-inline-diff-clear buffer)))
    (let* ((label (buffer-name buffer))
           (diff (efrit-vcs-diff-strings original replacement
                                         (concat "a/" label) (concat "b/" label)))
           (win (efrit-show-preview "*efrit rewrite*" diff 'diff-mode)))
      (unwind-protect
          (y-or-n-p "Apply this rewrite? ")
        (when (window-live-p win) (quit-window t win))))))

(defun efrit-rewrite--apply (buffer start-m end-m original tick replacement)
  "Replace START-M..END-M of BUFFER with REPLACEMENT after the checks."
  (cond
   ((equal replacement original)
    (message "efrit rewrite: the model returned the text unchanged"))
   ((not (efrit-rewrite--intact-p buffer start-m end-m original tick))
    (kill-new replacement)
    (message "efrit rewrite: the region changed while the model worked; the rewrite is on the kill ring"))
   ((with-current-buffer buffer
      (or buffer-read-only (get-text-property start-m 'read-only)))
    (kill-new replacement)
    (message "efrit rewrite: %s is read-only; the rewrite is on the kill ring" (buffer-name buffer)))
   (t
    (let ((accept
           (or (not efrit-rewrite-confirm)
               (efrit-rewrite--ask buffer start-m original replacement))))
      (cond
       ((not accept) (message "efrit rewrite: not applied"))
       ;; the prompt ran the event loop: check again
       ((not (efrit-rewrite--intact-p buffer start-m end-m original tick))
        (kill-new replacement)
        (message "efrit rewrite: the region changed while you looked at the diff; the rewrite is on the kill ring"))
       (t
        (with-current-buffer buffer
          (save-excursion
            (let ((s (marker-position start-m)) (e (marker-position end-m)))
              ;; keeps markers and point: replace-region-contents
              ;; does a minimal edit, not delete+insert
              (replace-region-contents s e (lambda () replacement)))))
        (message "efrit rewrite: applied (%d → %d chars)" (length original) (length replacement))))))))


(provide 'efrit-rewrite)

;;; efrit-rewrite.el ends here
