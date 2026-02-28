;;; efrit-spinner.el --- Loading spinner for Efrit chat buffer -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai
;; URL: https://github.com/steveyegge/efrit

;;; Commentary:
;; Spinner animation for visual feedback during API calls.
;; Shows a rotating spinner in the chat buffer while waiting for responses.

;;; Code:

;; Forward declarations to avoid circular dependencies
(declare-function efrit--setup-buffer "efrit-chat-buffer")

;; Declare faces
(defvar efrit-system-face nil
  "Face for system messages in Efrit buffer.")

;;; Customization

(defcustom efrit-spinner-frames ["⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"]
  "Spinner animation frames to cycle through."
  :type '(vector string)
  :group 'efrit)

(defcustom efrit-spinner-interval 0.08
  "Interval in seconds between spinner frame updates."
  :type 'float
  :group 'efrit)

;;; Internal State

(defvar-local efrit-spinner--timer nil
  "Timer object for the currently running spinner animation.")

(defvar-local efrit-spinner--frame-index 0
  "Current frame index in the spinner animation.")

(defvar-local efrit-spinner--spinner-marker nil
  "Marker position where spinner was inserted.")

;;; Spinner Functions

(defun efrit-spinner--get-frame ()
  "Get the current spinner frame."
  (aref efrit-spinner-frames (mod efrit-spinner--frame-index (length efrit-spinner-frames))))

(defun efrit-spinner--update ()
  "Update spinner animation frame."
  (when (buffer-live-p (current-buffer))
    (with-current-buffer (efrit--setup-buffer)
      (when (and efrit-spinner--spinner-marker
                 (marker-position efrit-spinner--spinner-marker))
        (save-excursion
          (goto-char (marker-position efrit-spinner--spinner-marker))
          ;; Delete old frame
          (when (looking-at "\\(⠋\\|⠙\\|⠹\\|⠸\\|⠼\\|⠴\\|⠦\\|⠧\\|⠇\\|⠏\\)")
            (delete-char 1))
          ;; Insert new frame
          (setq efrit-spinner--frame-index (1+ efrit-spinner--frame-index))
          (let ((inhibit-read-only t))
            (insert (efrit-spinner--get-frame))))))))

(defun efrit-spinner-start ()
  "Start the loading spinner animation in the chat buffer."
  (with-current-buffer (efrit--setup-buffer)
    ;; Stop any existing spinner
    (efrit-spinner-stop)
    
    ;; Ensure buffer is editable
    (setq buffer-read-only nil)
    (let ((inhibit-read-only t))
      ;; Go to end of buffer
      (goto-char (point-max))
      
      ;; Add newline if not at beginning
      (unless (bobp) (insert "\n"))
      
      ;; Insert "System: Thinking..." with spinner
      (let ((prefix-start (point)))
        (insert "System: Thinking ")
        ;; Mark where the spinner starts
        (setq-local efrit-spinner--spinner-marker (point-marker))
        (insert (efrit-spinner--get-frame))
        ;; Add some styling
        (add-text-properties prefix-start (point) '(face efrit-system-face)))
      
      ;; Set up animation timer
      (setq-local efrit-spinner--timer
                  (run-at-time 0 efrit-spinner-interval #'efrit-spinner--update)))))

(defun efrit-spinner-stop ()
  "Stop the loading spinner animation and remove it from buffer."
  (when efrit-spinner--timer
    (cancel-timer efrit-spinner--timer)
    (setq-local efrit-spinner--timer nil))
  
  (when (buffer-live-p (current-buffer))
    (with-current-buffer (efrit--setup-buffer)
      (setq buffer-read-only nil)
      (let ((inhibit-read-only t))
        ;; Remove the "Thinking..." line if it exists
        (when (and efrit-spinner--spinner-marker
                   (marker-position efrit-spinner--spinner-marker))
          (save-excursion
            ;; Find the start of the thinking line
            (goto-char (marker-position efrit-spinner--spinner-marker))
            ;; Move back to the start of "System: "
            (beginning-of-line)
            ;; Delete the entire line safely
            (let ((end (line-end-position)))
              (delete-region (point) (if (< end (point-max)) (1+ end) end)))))
        (setq-local efrit-spinner--spinner-marker nil)
        (setq-local efrit-spinner--frame-index 0)))))

(provide 'efrit-spinner)

;;; efrit-spinner.el ends here
