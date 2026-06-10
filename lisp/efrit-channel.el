;;; efrit-channel.el --- Eval+snapshot channel for external agents -*- lexical-binding: t; -*-

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Keywords: tools, processes

;;; Commentary:

;; A playwright-style channel into a live Emacs for external agents
;; (e.g. Claude Code).  The agent calls bin/efrit, which calls
;; `emacsclient --eval' on the functions below.  Every call returns a
;; JSON envelope containing the eval result, any error, new *Messages*
;; output, and a snapshot of editor state (selected buffer, text around
;; point, windows, buffer list) -- so each action returns a perception.
;;
;; This file is deliberately standalone: it requires nothing from the
;; rest of efrit and can be loaded into any Emacs.  Code and responses
;; cross the emacsclient boundary base64-encoded in both directions, so
;; there are no shell/elisp escaping issues.

;;; Code:

(defconst efrit-channel-version "0.1.0")

(defvar efrit-channel-max-value-chars 20000
  "Truncate printed eval values beyond this many characters.")

(defvar efrit-channel-max-messages-chars 4000
  "Truncate captured *Messages* output beyond this many characters.")

(defvar efrit-channel-visible-lines 60
  "Total lines of buffer content around point included in snapshots.")

(defvar efrit-channel-max-visible-chars 8000
  "Truncate snapshot buffer content beyond this many characters.")

(defvar efrit-channel-buffer-list-limit 15
  "Maximum number of buffers listed in snapshots.")

;;; Encoding helpers

(defun efrit-channel--b64-decode (s)
  "Decode base64 string S as UTF-8 text."
  (decode-coding-string (base64-decode-string s) 'utf-8))

(defun efrit-channel--b64-encode (s)
  "Encode string S as single-line base64 of its UTF-8 bytes."
  (base64-encode-string (encode-coding-string s 'utf-8) t))

(defun efrit-channel--bool (x)
  "Convert X to a `json-serialize' boolean."
  (if x t :false))

(defun efrit-channel--truncate (s n)
  "Truncate string S to N characters, noting how much was dropped."
  (if (> (length s) n)
      (concat (substring s 0 n)
              (format "...[truncated %d chars]" (- (length s) n)))
    s))

(defun efrit-channel--print (value)
  "Print VALUE readably, bounded in depth and length."
  (let ((print-length 200)
        (print-level 10))
    (efrit-channel--truncate (prin1-to-string value)
                             efrit-channel-max-value-chars)))

(defun efrit-channel--respond (data)
  "Serialize alist DATA to JSON and return it base64-encoded."
  (efrit-channel--b64-encode (json-serialize data)))

;;; *Messages* capture

(defun efrit-channel--messages-tail-pos ()
  "Return current end position of the *Messages* buffer."
  (let ((buf (get-buffer "*Messages*")))
    (if buf (with-current-buffer buf (point-max)) 1)))

(defun efrit-channel--messages-since (pos)
  "Return *Messages* content added after position POS."
  (let ((buf (get-buffer "*Messages*")))
    (if (not buf)
        ""
      (with-current-buffer buf
        (efrit-channel--truncate
         (buffer-substring-no-properties (min pos (point-max)) (point-max))
         efrit-channel-max-messages-chars)))))

;;; Snapshot

(defun efrit-channel--buffer-point (buf win)
  "Return point in BUF, preferring WIN's window-point when WIN shows BUF."
  (if (and win (window-live-p win) (eq (window-buffer win) buf))
      (window-point win)
    (with-current-buffer buf (point))))

(defun efrit-channel--buffer-info (buf win)
  "Return detailed info alist for BUF as shown in WIN."
  (with-current-buffer buf
    (let ((pt (efrit-channel--buffer-point buf win)))
      (save-excursion
        (goto-char pt)
        `((name . ,(buffer-name))
          (file . ,(or buffer-file-name :null))
          (mode . ,(symbol-name major-mode))
          (line . ,(line-number-at-pos pt))
          (column . ,(current-column))
          (point . ,pt)
          (size . ,(buffer-size))
          (modified . ,(efrit-channel--bool (buffer-modified-p)))
          (read_only . ,(efrit-channel--bool buffer-read-only))
          (narrowed . ,(efrit-channel--bool (buffer-narrowed-p))))))))

(defun efrit-channel--context-text (buf win)
  "Return text around point in BUF, with point marked as <|point|>."
  (with-current-buffer buf
    (let ((pt (efrit-channel--buffer-point buf win))
          (half (max 1 (/ efrit-channel-visible-lines 2))))
      (save-excursion
        (goto-char pt)
        (let ((top (line-beginning-position (- 1 half)))
              (bot (line-end-position half)))
          (efrit-channel--truncate
           (concat (buffer-substring-no-properties top pt)
                   "<|point|>"
                   (buffer-substring-no-properties pt bot))
           efrit-channel-max-visible-chars))))))

(defun efrit-channel--window-info (w)
  "Return info alist for window W."
  `((buffer . ,(buffer-name (window-buffer w)))
    (selected . ,(efrit-channel--bool (eq w (selected-window))))
    (width . ,(window-width w))
    (height . ,(window-height w))))

(defun efrit-channel--buffer-brief (buf)
  "Return brief info alist for BUF for the snapshot buffer list."
  (with-current-buffer buf
    `((name . ,(buffer-name))
      (mode . ,(symbol-name major-mode))
      (file . ,(or buffer-file-name :null))
      (modified . ,(efrit-channel--bool (buffer-modified-p))))))

(defun efrit-channel--snapshot ()
  "Return an alist describing current editor state."
  (let* ((win (selected-window))
         (buf (window-buffer win))
         (visible-buffers
          (seq-take (seq-remove (lambda (b)
                                  (string-prefix-p " " (buffer-name b)))
                                (buffer-list))
                    efrit-channel-buffer-list-limit)))
    `((buffer . ,(efrit-channel--buffer-info buf win))
      (content . ,(efrit-channel--context-text buf win))
      (windows . ,(vconcat (mapcar #'efrit-channel--window-info
                                   (window-list nil 'no-mini))))
      (buffers . ,(vconcat (mapcar #'efrit-channel--buffer-brief
                                   visible-buffers)))
      (echo . ,(or (current-message) :null)))))

;;; Eval

(defun efrit-channel--read-forms (source)
  "Read all elisp forms from string SOURCE.
Signals an error on malformed input, including unbalanced forms."
  (let ((pos 0)
        (len (length source))
        forms)
    (while (progn
             (setq pos (or (string-match "[^ \t\n\r]" source pos) len))
             (< pos len))
      (let ((result (read-from-string source pos)))
        (push (car result) forms)
        (setq pos (cdr result))))
    (nreverse forms)))

;;; Public entry points (called via emacsclient by bin/efrit)

(defun efrit-channel-eval-b64 (b64 &optional no-snapshot)
  "Eval base64-encoded elisp source B64; return base64 JSON envelope.
Evaluates every form in the payload, in the buffer of the selected
window, and reports the value of the last one.  Captures errors and
new *Messages* output.  Includes a snapshot unless NO-SNAPSHOT."
  (let ((msg-start (efrit-channel--messages-tail-pos))
        (err nil)
        (value nil))
    (condition-case e
        (dolist (form (efrit-channel--read-forms
                       (efrit-channel--b64-decode b64)))
          (setq value (with-current-buffer (window-buffer (selected-window))
                        (eval form t))))
      (error (setq err e)))
    (efrit-channel--respond
     `((ok . ,(efrit-channel--bool (not err)))
       (value . ,(if err :null (efrit-channel--print value)))
       (error . ,(if err
                     `((type . ,(symbol-name (car err)))
                       (message . ,(error-message-string err)))
                   :null))
       (messages . ,(efrit-channel--messages-since msg-start))
       (snapshot . ,(if no-snapshot :null (efrit-channel--snapshot)))))))

(defun efrit-channel-snapshot-b64 ()
  "Return base64 JSON envelope containing only an editor snapshot."
  (efrit-channel--respond
   `((ok . t)
     (snapshot . ,(efrit-channel--snapshot)))))

(defun efrit-channel-ping-b64 ()
  "Return base64 JSON envelope identifying this Emacs."
  (efrit-channel--respond
   `((ok . t)
     (channel_version . ,efrit-channel-version)
     (emacs_version . ,emacs-version)
     (pid . ,(emacs-pid))
     (server . ,(or (bound-and-true-p server-name) :null))
     (uptime_seconds . ,(floor (float-time
                                (time-subtract (current-time)
                                               before-init-time)))))))

(provide 'efrit-channel)
;;; efrit-channel.el ends here
