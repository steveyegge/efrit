;;; efrit-agent-render.el --- Message rendering for efrit-agent -*- lexical-binding: t -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; Rendering module for efrit-agent providing:
;; - User and Claude message rendering
;; - Streaming display for Claude responses
;; - Thinking indicator display and management
;; - Header-line formatting

;;; Code:

(require 'cl-lib)
(require 'efrit-agent-core)
(require 'efrit-markdown)
(require 'efrit-usage)
(declare-function efrit-agent-svg-header "efrit-agent-svg-header")
(declare-function efrit-agent-svg--resize "efrit-agent-svg-header")

(defvar efrit-agent--repl-session)
(declare-function efrit-repl-session-id "efrit-repl-session")

;;; Message Rendering
;;
;; Functions to add messages to the conversation region without full re-render.
;; These use efrit-agent--append-to-conversation for incremental updates.

(defface efrit-agent-user-block
  '((((background dark)) :background "#2b2f3a" :extend t)
    (t :background "#eef1f6" :extend t))
  "Background for the user's turn, so questions stand apart from answers."
  :group 'efrit-agent)

(defconst efrit-agent--user-prefixes
  '((nil "❯ " efrit-agent-user-prefix)
    (steer "↳ " efrit-agent-steer-prefix)
    (queued "⋯ " efrit-agent-queued-prefix)
    (dropped "✗ " efrit-agent-queued-prefix))
  "Prefix and face of a user line by KIND: a turn, a steer, a queued input.")

(defun efrit-agent--user-line (line)
  "LINE of a user message with its face; a `> ' quote line gets a bar.
The `> ' stays in the text (the model and the kill ring see Markdown)."
  (if (string-match "\\`> ?" line)
      (concat (propertize (match-string 0 line)
                          'display (propertize "▌ " 'face 'efrit-markdown-quote-bar)
                          'face 'efrit-agent-user-message)
              (propertize (substring line (match-end 0))
                          'face '(efrit-markdown-quote efrit-agent-user-message)))
    (propertize line 'face 'efrit-agent-user-message)))

(defun efrit-agent--add-user-message (text &optional kind)
  "Add a user message with TEXT to the conversation region.
The turn is rendered as a shaded block with a `❯' prefix on the
first line and continuation lines indented, followed by one blank
line.  Text properties mark it `user-message' with an id.  KIND
`steer' marks text handed to a running turn, `queued' text that waits
for the turn to end (see `efrit-agent--unmark-queued-message')."
  ;; Any user line closes the message being streamed: drawn inside an
  ;; open one, the next chunk landed after the line and the message
  ;; read "I will now↳ steer text compute." (2026-09-25).  A steer or a
  ;; queued line leaves the thinking indicator alone: the turn goes on.
  (efrit-agent--stream-end-message)
  (unless kind
    (efrit-agent--hide-thinking))
  (let* ((msg-id (format "user-msg-%d" (cl-incf efrit-agent--message-counter)))
         (prefix (or (assq kind efrit-agent--user-prefixes) (assq nil efrit-agent--user-prefixes)))
         (lines (split-string (string-trim-right text) "\n"))
         (body (mapconcat
                (lambda (pair)
                  (let ((first (car pair)) (line (cdr pair)))
                    (concat (propertize (if first (nth 1 prefix) "  ")
                                        'face (nth 2 prefix) 'efrit-user-prefix t)
                            (efrit-agent--user-line line))))
                (cl-loop for l in lines for i from 0 collect (cons (zerop i) l))
                "\n"))
         (formatted-text
          ;; The block background is *added* under the prefix/message
          ;; faces; `propertize' would replace them and the line lost
          ;; its gold prompt and text colour.
          (let ((block (concat body "\n")))
            (add-face-text-property 0 (length block) 'efrit-agent-user-block t block)
            (concat block "\n"))))
    (efrit-agent--append-to-conversation
     formatted-text
     (list 'efrit-type 'user-message
           'efrit-id msg-id
           'efrit-user-kind kind
           'efrit-user-text text))))

(defun efrit-agent--find-user-message (text kind)
  "Start and end of the first user message of KIND with TEXT, or nil."
  (save-excursion
    (goto-char (point-min))
    (catch 'found
      (let ((pos (point-min)))
        (while (setq pos (text-property-not-all pos (point-max) 'efrit-user-text nil))
          (when (and (eq (get-text-property pos 'efrit-user-kind) kind)
                     (equal (get-text-property pos 'efrit-user-text) text))
            (throw 'found (cons pos (or (next-single-property-change pos 'efrit-id) (point-max)))))
          (setq pos (or (next-single-property-change pos 'efrit-user-text) (point-max))))
        nil))))

(defun efrit-agent--unmark-queued-message (text &optional new-kind)
  "Turn the queued line showing TEXT into a sent turn, or into NEW-KIND.
The waiting mark becomes the turn prefix (or the dropped one), so the
conversation reads in the order things happened."
  (when-let* ((region (efrit-agent--find-user-message text 'queued)))
    (let* ((prefix (or (assq new-kind efrit-agent--user-prefixes)
                       (assq nil efrit-agent--user-prefixes)))
           (start (car region)) (end (cdr region)))
      (efrit-agent--with-render
        (put-text-property start end 'efrit-user-kind new-kind)
        (when-let* ((p (text-property-any start end 'efrit-user-prefix t)))
          (goto-char p)
          (let ((props (text-properties-at p)))
            (delete-region p (+ p 2))
            (insert (apply #'propertize (nth 1 prefix) props))
            (put-text-property p (+ p 2) 'face (nth 2 prefix))
            (add-face-text-property p (+ p 2) 'efrit-agent-user-block t)))))))

(defun efrit-agent--add-claude-message (text)
  "Add a Claude message with TEXT to the conversation region.
If there's an active streaming message, append to it.
Otherwise start a new message.
Claude messages appear inline without a prefix."
  (if efrit-agent--streaming-message
      ;; Append to existing streaming message
      (efrit-agent--stream-append-text text)
    ;; Start a new message
    (efrit-agent--stream-start-message text)))

(defun efrit-agent--add-error-message (text)
  "Add an error message with TEXT to the conversation region.
Error messages are displayed in red to make them visible."
  ;; Ensure markers exist before proceeding
  (unless (and efrit-agent--conversation-end
               (marker-position efrit-agent--conversation-end))
    (efrit-agent--init-regions)
    (efrit-agent--setup-regions))
  (efrit-agent--stream-end-message)
  (efrit-agent--hide-thinking)
  (let* ((msg-id (format "error-msg-%d" (cl-incf efrit-agent--message-counter)))
         (formatted-text (concat "\n⚠ Error: " text "\n\n")))
    (efrit-agent--with-render
      (goto-char (marker-position efrit-agent--conversation-end))
      (let ((start (point)))
        (insert (propertize formatted-text 'face 'efrit-agent-error))
        (add-text-properties start (point)
                             (list 'efrit-type 'error-message
                                   'efrit-id msg-id))
        (efrit-agent--seal-rendered start (point))
        (set-marker efrit-agent--conversation-end (point))))))

(defun efrit-agent--stream-start-message (text)
  "Start a new streaming Claude message with TEXT.
Creates markers for tracking the message region for future appends."
  ;; Ensure markers exist before proceeding
  (unless (and efrit-agent--conversation-end
               (marker-position efrit-agent--conversation-end))
    (efrit-agent--init-regions)
    (efrit-agent--setup-regions))
  ;; Hide thinking indicator when Claude starts responding
  (efrit-agent--hide-thinking)
  (let* ((msg-id (format "claude-msg-%d" (cl-incf efrit-agent--message-counter)))
         start-marker end-marker)
    ;; Move to end of conversation region
    (efrit-agent--with-render
      (goto-char (marker-position efrit-agent--conversation-end))
      ;; Prose after tool rows gets a blank line, so the answer does
      ;; not read as the last row's continuation
      (when (and (> (point) (point-min))
                 (eq (get-text-property (1- (point)) 'efrit-type) 'tool-call))
        (insert (propertize "\n" 'read-only t)))
      (setq start-marker (point-marker))
      ;; Insert the text
      (insert (propertize text 'face 'efrit-agent-claude-message))
      (setq end-marker (point-marker))
      ;; Set the marker type (insert before for end-marker)
      (set-marker-insertion-type end-marker t)
      ;; Apply properties
      (add-text-properties start-marker end-marker
                           (list 'efrit-type 'claude-message
                                 'efrit-id msg-id))
      (efrit-agent--seal-rendered start-marker end-marker)
      ;; Update conversation end marker
      (set-marker efrit-agent--conversation-end (point)))
    ;; Track the streaming message
    (setq efrit-agent--streaming-message
          (list msg-id start-marker end-marker))
    ;; Scroll to show new content
    (efrit-agent--scroll-to-bottom)))

(defun efrit-agent--stream-append-text (text)
  "Append TEXT to the current streaming Claude message.
Updates the message region markers."
  (when (and efrit-agent--streaming-message
             (nth 2 efrit-agent--streaming-message)
             (markerp (nth 2 efrit-agent--streaming-message))
             (marker-position (nth 2 efrit-agent--streaming-message)))
    (let* ((msg-id (nth 0 efrit-agent--streaming-message))
           (end-marker (nth 2 efrit-agent--streaming-message)))
      (efrit-agent--with-render
        ;; end-marker has insertion-type t, so inserting AT it keeps
        ;; it after the new text.  (An earlier version inserted at
        ;; (1- marker), i.e. before the previous chunk's last char:
        ;; "Hello, " + "world." came out as "Hello,world. ".)
        (goto-char (marker-position end-marker))
        (insert (propertize text 'face 'efrit-agent-claude-message))
        (let ((start (- (point) (length text))))
          (add-text-properties start (point)
                               (list 'efrit-type 'claude-message
                                     'efrit-id msg-id))
          (efrit-agent--seal-rendered start (point)))
        ;; Markdown, incrementally: from the watermark to here
        (efrit-agent--render-markdown (nth 1 efrit-agent--streaming-message) end-marker nil)
        ;; Update conversation end if needed
        (when (and efrit-agent--conversation-end
                   (markerp efrit-agent--conversation-end)
                   (marker-position efrit-agent--conversation-end)
                   (> (point) (marker-position efrit-agent--conversation-end)))
          (set-marker efrit-agent--conversation-end (point))))
      ;; Scroll to show new content
      (efrit-agent--scroll-to-bottom))))

(defun efrit-agent--render-markdown (start end complete)
  "Render START..END of a streamed message as Markdown, when enabled.
The passes delete markup, so the message's end marker moves; the
conversation-end marker is corrected afterwards by the caller."
  (when (and (bound-and-true-p efrit-markdown-enabled)
             (fboundp 'efrit-markdown-render)
             start end (marker-position start) (marker-position end)
             (< start end))
    (condition-case err
        (efrit-markdown-render start end complete)
      (error
       (efrit-log 'warn "markdown render failed: %s" (error-message-string err))))))

(defun efrit-agent--stream-end-message ()
  "End the current streaming message, adding final newlines.
Call this when Claude's message is complete."
  (when efrit-agent--streaming-message
    (let ((inhibit-read-only t) (buffer-undo-list t)
          (end-marker (nth 2 efrit-agent--streaming-message)))
      ;; The final pass: what was held back (the last line, an open
      ;; fence) is rendered now
      (efrit-agent--render-markdown (nth 1 efrit-agent--streaming-message) end-marker t)
      ;; a closing "Next steps" list becomes buttons (efrit-next-steps)
      (when (fboundp 'efrit-next-steps-mark)
        (ignore-errors
          (efrit-next-steps-mark (marker-position (nth 1 efrit-agent--streaming-message))
                                 (marker-position end-marker))))
      (save-excursion
        (goto-char (marker-position end-marker))
        ;; Add trailing newlines for spacing
        (insert "\n\n")
        ;; Update conversation end
        (set-marker efrit-agent--conversation-end (point)))
      ;; Clear streaming state
      (setq efrit-agent--streaming-message nil))))

(defcustom efrit-agent-follow-threshold 2
  "Windows whose point is within this many lines of the end keep following output.
A window scrolled further up is left alone so the user can read while
the model streams (copilot-chat's following-windows rule)."
  :type 'integer
  :group 'efrit-agent)

(defvar-local efrit-agent--follow-windows nil
  "Windows that were following the output before the latest insert.")

(defun efrit-agent--following-windows ()
  "Return windows on this buffer, in any frame, whose point is near the end."
  (let ((end (or (and efrit-agent--conversation-end
                      (marker-position efrit-agent--conversation-end))
                 (point-max))))
    (cl-remove-if-not
     (lambda (w)
       (<= (count-lines (min (window-point w) end) end)
           efrit-agent-follow-threshold))
     (get-buffer-window-list (current-buffer) nil t))))

(defun efrit-agent--note-followers ()
  "Record which windows are following, before text is inserted.
Called from `before-change-functions' for this buffer."
  (setq efrit-agent--follow-windows (efrit-agent--following-windows)))

(defun efrit-agent--scroll-to-bottom ()
  "Scroll only the windows that were already following the output.
Never moves buffer point, and never touches a window the user has
scrolled up to read.  Windows that were at the end before the last
insert (see `efrit-agent--note-followers') are moved to the end;
when that list is empty (first output) every window is."
  (let* ((followers (or efrit-agent--follow-windows
                        (get-buffer-window-list (current-buffer) nil t)))
         (target (point-max)))
    (dolist (w followers)
      (when (and (window-live-p w) (eq (window-buffer w) (current-buffer)))
        (set-window-point w target)
        ;; Show the tail without recentering the user's own point
        (with-selected-window w
          (save-excursion
            (goto-char target)
            (recenter -3)))))
    (setq efrit-agent--follow-windows nil)))

;;; Thinking Indicator
;;
;; Shows when Claude is processing but no tool is running.
;; Disappears when content starts arriving.

(defun efrit-agent--thinking-line (text)
  "The in-buffer activity line: spinner, then TEXT (or a default)."
  (propertize (concat "  " (efrit-agent--spinner-frame) " "
                      (propertize (or text "thinking…") 'face 'efrit-agent-thinking)
                      "\n")
              'efrit-type 'thinking-indicator))

(defun efrit-agent--refresh-thinking-line ()
  "Redraw the thinking line's spinner glyph in place, if it is showing."
  (when-let* ((ind efrit-agent--thinking-indicator)
              (start (marker-position (car ind)))
              (end (marker-position (cdr ind))))
    (let ((inhibit-read-only t) (buffer-undo-list t)
          (label (save-excursion
                   (goto-char start)
                   (let ((s (buffer-substring-no-properties start (line-end-position))))
                     ;; "  <spinner> label"
                     (string-trim (if (> (length s) 4) (substring s 4) ""))))))
      (save-excursion
        (goto-char start)
        (delete-region start end)
        (insert (propertize (efrit-agent--thinking-line label) 'read-only t))
        (set-marker (cdr ind) (point))))))

(defun efrit-agent--show-thinking (&optional text)
  "Show the thinking indicator with optional TEXT description.
If TEXT is nil, shows just '[thinking...]'.
The indicator is removed when content arrives or explicitly hidden."
  (when (and (not efrit-agent--thinking-indicator)
             efrit-agent--conversation-end
             (marker-position efrit-agent--conversation-end))
    (let ((inhibit-read-only t) (buffer-undo-list t)
          start-marker end-marker)
      (save-excursion
        (goto-char (marker-position efrit-agent--conversation-end))
        (setq start-marker (point-marker))
        ;; Insert the thinking indicator
        (insert (efrit-agent--thinking-line text))
        (setq end-marker (point-marker))
        ;; Set marker insertion types
        (set-marker-insertion-type start-marker nil)
        (set-marker-insertion-type end-marker t)
        ;; Make it read-only
        (add-text-properties start-marker end-marker '(read-only t))
        ;; Update conversation end
        (set-marker efrit-agent--conversation-end (point)))
      ;; Track the indicator
      (setq efrit-agent--thinking-indicator (cons start-marker end-marker))
      ;; Scroll to show
      (efrit-agent--scroll-to-bottom))))

(defun efrit-agent--hide-thinking ()
  "Hide the thinking indicator if currently shown.
Also stops the header-line spinner."
  (efrit-agent--spinner-stop)
  (when efrit-agent--thinking-indicator
    (let ((inhibit-read-only t) (buffer-undo-list t)
          (start-marker (car efrit-agent--thinking-indicator))
          (end-marker (cdr efrit-agent--thinking-indicator)))
      (when (and (marker-position start-marker)
                 (marker-position end-marker))
        (delete-region start-marker end-marker)
        ;; Update conversation end marker if needed
        (when (> (marker-position efrit-agent--conversation-end)
                 (marker-position start-marker))
          (set-marker efrit-agent--conversation-end start-marker)))
      ;; Clean up markers
      (set-marker start-marker nil)
      (set-marker end-marker nil))
    (setq efrit-agent--thinking-indicator nil)))

(defun efrit-agent--update-thinking (text)
  "Update the thinking indicator TEXT without removing and re-adding.
This provides smooth updates for changing thinking status."
  (if efrit-agent--thinking-indicator
      (let ((inhibit-read-only t) (buffer-undo-list t)
            (start-marker (car efrit-agent--thinking-indicator))
            (end-marker (cdr efrit-agent--thinking-indicator)))
        (when (and (marker-position start-marker)
                   (marker-position end-marker))
          (save-excursion
            (goto-char start-marker)
            (delete-region start-marker end-marker)
            (insert (propertize (efrit-agent--thinking-line text) 'read-only t))
            (set-marker end-marker (point)))))
    ;; No indicator yet, show one
    (efrit-agent--show-thinking text)))

;;; Header Line (status bar at top of window)

(declare-function efrit-context-describe "efrit-context-sources")

(defun efrit-agent--format-header-line ()
  "Format the header-line for display.
Shows: status │ elapsed │ mode │ verbosity │ tool count │ hints"
  (let* ((status-str (efrit-agent--status-string))
         (elapsed (efrit-agent--format-elapsed))
         (tool-count (length (cl-remove-if-not
                              (lambda (a) (eq (plist-get a :type) 'tool))
                              efrit-agent--activities)))
         (mode-str (propertize (format "[%s]" efrit-agent-display-mode)
                               'face 'efrit-agent-session-id))
         (verbosity-str (propertize (format "v:%s" efrit-agent-verbosity)
                                    'face 'efrit-agent-session-id
                                    'help-echo "Press 'v' to cycle verbosity"))
         (sep (propertize " │ " 'face 'efrit-agent-session-id)))
    (concat
     " "
     status-str
     (when efrit-agent--thinking-label
       (concat sep (propertize (format "%s %s"
                                       (efrit-agent--spinner-frame)
                                       efrit-agent--thinking-label)
                               'face 'efrit-agent-thinking)))
     sep
     (propertize elapsed 'face 'efrit-agent-timestamp)
     sep
     mode-str
     sep
     verbosity-str
     (when (> tool-count 0)
       (concat sep (propertize (format "%d tools" tool-count)
                               'face 'efrit-agent-session-id)))
     ;; Live context-window usage from the API's own numbers
     (when-let* ((usage (efrit-agent--usage-segment)))
       (concat sep usage))
     ;; What the next turn will carry as editor context (the target
     ;; buffer, its point or region), and whether it was dismissed
     (when-let* ((ctx (and (fboundp 'efrit-context-describe)
                           (ignore-errors (efrit-context-describe)))))
       (concat sep (propertize ctx 'face 'efrit-agent-session-id
                               'help-echo "Editor context sent with the next turn; C-c C-; dismisses it")))
     ;; Show action hints based on status
     (pcase efrit-agent--status
       ((or 'idle 'waiting)
        (concat sep (propertize (efrit-agent-input-hint) 'face 'efrit-agent-timestamp)))
       ('working
        (concat sep (propertize "k:cancel  C-c ?:menu"
                                'face 'efrit-agent-timestamp)))
       (_ "")))))

(defun efrit-agent-input-hint ()
  "The one-line key hint for the input: how to send and how to break a line.
Chat clients (Slack, the Claude app) use RET to send and S-RET for a
newline; efrit does the same, and says so, because an Emacs user
expects RET to insert a newline.  Bindings are looked up, so a user
who rebinds them sees their own keys."
  (let* ((map (and (boundp 'efrit-agent-input-mode-map) efrit-agent-input-mode-map))
         (key (lambda (cmd fallback)
                ;; prefer the shifted/chorded key when several are bound:
                ;; "S-RET" is what a chat user looks for, "C-j" is not
                (let* ((keys (and map (where-is-internal cmd map)))
                       (descs (mapcar #'key-description keys))
                       ;; "S-RET" over "S-<return>", "RET" over "<return>":
                       ;; the plain names read better; the shifted one first
                       (k (or (cl-find-if (lambda (d) (and (string-prefix-p "S-" d) (not (string-match-p "<" d)))) descs)
                              (cl-find-if (lambda (d) (string-prefix-p "S-" d)) descs)
                              (cl-find-if (lambda (d) (not (string-match-p "<" d))) descs)
                              (car descs))))
                  (or k fallback)))))
    (format "%s sends · %s newline · %s steers · C-c ? menu"
            (funcall key 'efrit-agent-input-send "RET")
            (funcall key 'efrit-agent-input-newline "S-RET")
            (funcall key 'efrit-agent-input-send-override "M-RET"))))

(defun efrit-agent--usage-segment ()
  "Token usage indicator for this buffer's session, or nil."
  (when-let* ((id (cond ((and (boundp 'efrit-agent--repl-session)
                              efrit-agent--repl-session
                              (fboundp 'efrit-repl-session-id))
                         (efrit-repl-session-id efrit-agent--repl-session))
                        (efrit-agent--session-id))))
    (efrit-usage-indicator id)))

(defun efrit-agent--setup-header-line ()
  "Set up the header-line for the agent buffer.
`efrit-agent-svg-header' picks the graphical or text rendering per
`efrit-agent-header-style'."
  (require 'efrit-agent-svg-header)
  (setq header-line-format '(:eval (efrit-agent-svg-header)))
  (add-hook 'window-configuration-change-hook #'efrit-agent-svg--resize nil t))

;;; UI Helpers

(defun efrit-agent--make-button (label action &optional help-echo)
  "Create a clickable button with LABEL that runs ACTION when clicked.
HELP-ECHO is the tooltip text shown on hover."
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1] action)
    (define-key map (kbd "RET") action)
    (propertize (concat "[" label "]")
                'face 'efrit-agent-button
                'mouse-face 'efrit-agent-button-hover
                'keymap map
                'help-echo (or help-echo (format "Click to %s" (downcase label))))))

(defun efrit-agent--make-option-button (option index)
  "Create a clickable button for OPTION with INDEX (1-indexed)."
  (let* ((label (format "[%d] %s" index option))
         (map (make-sparse-keymap))
         (action (lambda ()
                   (interactive)
                   (efrit-agent--select-option index))))
    (define-key map [mouse-1] action)
    (define-key map (kbd "RET") action)
    (propertize label
                'face 'efrit-agent-option
                'mouse-face 'efrit-agent-option-hover
                'keymap map
                'help-echo (format "Click or press %d to select" index))))

;; Forward declaration for option selection
(declare-function efrit-agent--select-option "efrit-agent-input")

;;; Inline TODO Display
;;
;; TODOs are displayed inline in the conversation as a checklist block.
;; The block is tracked with markers and updated in-place when TODOs change.

(defvar-local efrit-agent--todos-region nil
  "Marker pair (start . end) for the inline TODO block, or nil if none.
Used to update TODOs in-place without full re-render.")

(defun efrit-agent--format-todo-item (todo)
  "Format a single TODO item for display.
TODO is a plist with :status, :content, and optionally :id."
  (let* ((status (plist-get todo :status))
         (content (or (plist-get todo :content) ""))
         (indicator (pcase status
                      ('completed (efrit-agent--char 'task-complete))
                      ('in_progress (efrit-agent--char 'task-in-progress))
                      (_ (efrit-agent--char 'task-pending))))
         (face (pcase status
                 ('completed 'efrit-agent-task-complete)
                 ('in_progress 'efrit-agent-task-current)
                 (_ 'efrit-agent-task-pending))))
    (concat
     "  "
     (propertize (format "%s " indicator) 'face face)
     (propertize content 'face face)
     (when (eq status 'in_progress)
       (propertize " ← current" 'face 'efrit-agent-timestamp))
     "\n")))

(defun efrit-agent--format-todos-block (todos)
  "Format the entire TODO block for TODOS list.
Returns a propertized string ready for insertion."
  (if (null todos)
      ""  ; Don't show anything when no TODOs
    (let* ((total (length todos))
           (complete (cl-count-if (lambda (item) (eq (plist-get item :status) 'completed))
                                  todos))
           (header (propertize (format "Tasks (%d/%d):\n" complete total)
                               'face 'efrit-agent-section-header)))
      (concat
       header
       (mapconcat #'efrit-agent--format-todo-item todos "")
       "\n"))))

(defun efrit-agent--add-todos-inline (todos)
  "Add or update the inline TODO block in the conversation.
TODOS is a list of plists with :status, :content, :id.
If a TODO block already exists, updates it in-place.
If no block exists, inserts one."
  ;; Ensure markers exist before proceeding
  (unless (and efrit-agent--conversation-end
               (marker-position efrit-agent--conversation-end))
    (efrit-agent--init-regions)
    (efrit-agent--setup-regions))
  ;; End any streaming message first (so Claude's text is finalized before TODOs)
  (efrit-agent--stream-end-message)
  (let ((inhibit-read-only t) (buffer-undo-list t)
        (formatted (efrit-agent--format-todos-block todos)))
    (if efrit-agent--todos-region
        ;; Update existing block in-place
        (let ((start-marker (car efrit-agent--todos-region))
              (end-marker (cdr efrit-agent--todos-region)))
          (when (and (marker-position start-marker)
                     (marker-position end-marker))
            (save-excursion
              (delete-region start-marker end-marker)
              (goto-char start-marker)
              (when (> (length formatted) 0)
                (insert (propertize formatted
                                    'efrit-type 'todos-block
                                    'read-only t))
                (set-marker end-marker (point))))))
      ;; Insert new block (if there are todos)
      (when (> (length formatted) 0)
        (let (start-marker end-marker)
          (save-excursion
            (goto-char (marker-position efrit-agent--conversation-end))
            (setq start-marker (point-marker))
            (insert (propertize formatted
                                'efrit-type 'todos-block
                                'read-only t))
            (setq end-marker (point-marker))
            ;; Set marker types
            ;; start-marker: nil = insertions go after (marker stays at start)
            ;; end-marker: nil = insertions go after (marker stays at end, doesn't capture new content)
            (set-marker-insertion-type start-marker nil)
            (set-marker-insertion-type end-marker nil)
            ;; Update conversation end
            (set-marker efrit-agent--conversation-end (point)))
          ;; Track the region
          (setq efrit-agent--todos-region (cons start-marker end-marker)))))))

(defun efrit-agent--clear-todos-inline ()
  "Remove the inline TODO block if present."
  (when efrit-agent--todos-region
    (let ((inhibit-read-only t) (buffer-undo-list t)
          (start-marker (car efrit-agent--todos-region))
          (end-marker (cdr efrit-agent--todos-region)))
      (when (and (marker-position start-marker)
                 (marker-position end-marker))
        (delete-region start-marker end-marker))
      ;; Clean up markers
      (set-marker start-marker nil)
      (set-marker end-marker nil))
    (setq efrit-agent--todos-region nil)))

(provide 'efrit-agent-render)

;;; efrit-agent-render.el ends here
