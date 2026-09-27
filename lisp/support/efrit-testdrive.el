;;; efrit-testdrive.el --- Live test drive of efrit -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; Two commands, for the question "does efrit work here" after an
;; upgrade, a model change or a proxy change, when the unit tests pass
;; but the live path has not been exercised.
;;
;; `M-x efrit-testdrive' is automatic.  It asks once for consent, then
;; runs about a dozen short model turns in a throwaway project and
;; checks every outcome itself: what the model answered, which tools
;; ran, what the sandbox granted or refused, what the agent buffer
;; shows (it reads the buffer's text and properties).  Nothing asks
;; you anything while it runs: a step that would need a sandbox
;; answer grants it beforehand and checks the grant.  Ten minutes
;; unattended, then a report.  `C-u' runs one chosen section.
;;
;; `M-x efrit-testdrive-tour' is for your eyes.  It shows one thing at
;; a time -- the header, a folded row to unfold, the menu, a sandbox
;; prompt to refuse -- with the buffer in front of you and nothing
;; running underneath, and asks after each whether it looked right.
;; Five minutes.
;;
;; Safety model
;; ------------
;; Everything the model touches lives in a throwaway project under
;; `temporary-file-directory', created at the start and deleted at the
;; end together with the session grants made on it.  `efrit-project-root'
;; is bound to it for the whole drive, so the model's tools resolve
;; there and not in whatever project you were in.  Nothing outside it
;; is granted; the one step that asks the model to reach outside
;; (in the tour) expects you to refuse.
;;
;; The report, *efrit-testdrive*, is Markdown rendered in place: the
;; summary first, one line per step, the tail of *efrit-log* under a
;; failure.  `M-x write-file' saves it as plain Markdown for a bug
;; report.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'efrit-log)
(require 'efrit-events)
(require 'efrit-sandbox)
(require 'efrit-review)
(require 'efrit-ui-helpers)

(defvar efrit-project-root)
(defvar efrit-agent-buffer-name)
(defvar efrit-default-model)
(defvar efrit-api-streaming)
(defvar efrit-agent-auto-show)
(defvar efrit-agent--conversation-end)
(defvar efrit-agent--repl-session)
(defvar efrit-agent-display-mode)
(defvar efrit-agent--input-start)
(defvar efrit-sandbox-request-function)
(defvar efrit-agent--thinking-indicator)
(declare-function efrit "efrit")
(declare-function efrit-doctor "efrit-doctor")
(declare-function efrit-menu "efrit-menu")
(declare-function efrit-agent-menu "efrit-agent-menu")
(declare-function efrit-sandbox "efrit-permissions-ui")
(declare-function efrit-common-get-api-key "efrit-common")
(declare-function efrit-common-get-api-url "efrit-common")
(declare-function efrit-markdown-render "efrit-markdown")
(declare-function efrit-submit "efrit-agent-input")
(declare-function efrit-agent-repl-session "efrit-agent-input")
(declare-function efrit-agent-cancel "efrit-agent")
(declare-function efrit-agent-display "efrit-agent-core")
(declare-function efrit-agent--clear-input "efrit-agent-core")
(declare-function efrit-agent--get-input "efrit-agent-core")
(declare-function efrit-agent-input-send "efrit-agent-input")
(declare-function efrit-agent-busy-submit-queue "efrit-agent-input")
(declare-function efrit-agent-busy-submit-steer "efrit-agent-input")
(declare-function efrit-agent--api-input-for "efrit-agent-input")
(declare-function efrit-agent-copy-last-output "efrit-agent-input")
(declare-function efrit-agent--last-claude-message-bounds "efrit-agent-input")
(declare-function efrit-agent-restart "efrit-agent-input")
(declare-function efrit-agent-session-id "efrit-agent-input")
(declare-function efrit-agent--find-user-message "efrit-agent-render")
(declare-function efrit-agent--unmark-queued-message "efrit-agent-render")
(declare-function efrit-agent--add-tool-call "efrit-agent-tools")
(declare-function efrit-agent--update-tool-result "efrit-agent-tools")
(declare-function efrit-agent--find-tool-region "efrit-agent-tools")
(declare-function efrit-agent--tool-body-bounds "efrit-agent-tools")
(declare-function efrit-agent--set-tool-expanded "efrit-agent-tools")
(declare-function efrit-agent--toggle-tool-expansion "efrit-agent-tools")
(declare-function efrit-agent-mention-completion-at-point "efrit-agent-mentions")
(declare-function efrit-agent-slash-completion-at-point "efrit-agent-mentions")
(declare-function efrit-agent-slash-run "efrit-agent-mentions")
(declare-function efrit-agent-dnd-handle "efrit-agent-mentions")
(declare-function efrit-agent-quote-region "efrit-agent-input")
(declare-function efrit-agent-input-newline "efrit-agent-input")
(declare-function efrit-agent-input-tab "efrit-agent-input")
(declare-function efrit-agent--session-busy-p "efrit-agent-input")
(declare-function efrit-agent--turn-starts "efrit-agent")
(declare-function efrit-agent-narrow-to-turns "efrit-agent")
(declare-function efrit-agent-widen "efrit-agent")
(declare-function efrit-transcript-file "efrit-transcript")
(declare-function efrit-repl-loop-hold "efrit-repl-loop")
(declare-function efrit-repl-loop-release "efrit-repl-loop")
(declare-function efrit-diff-preview--display "efrit-tool-show-diff-preview")
(declare-function efrit-diff-preview-open-file "efrit-tool-show-diff-preview")
(declare-function efrit-loop-adapter-elapsed-fn "efrit-loop")
(defvar efrit-repl-loop--adapter)
(defvar efrit-transcript-enabled)
(defvar efrit-diff-preview--root)
(defvar efrit-diff-preview--apply-mode)
(defvar efrit-diff-preview-buffer-name)
(defvar efrit-agent--input-start)
(declare-function efrit-repl-session-queue "efrit-repl-session")
(declare-function efrit-repl-session-status "efrit-repl-session")
(declare-function efrit-repl-session-dequeue "efrit-repl-session")
(declare-function efrit-repl-session-enqueue "efrit-repl-session")
(declare-function efrit-repl-session-set-status "efrit-repl-session")
(declare-function efrit-repl-session-pending-question "efrit-repl-session")
(declare-function efrit-agent--isearch-filter "efrit-agent-tools")
(declare-function efrit-agent--isearch-cleanup "efrit-agent-tools")
(declare-function efrit-usage-for "efrit-usage")
(declare-function efrit-usage-context "efrit-usage")

(defgroup efrit-testdrive nil
  "The live test drive."
  :group 'efrit
  :prefix "efrit-testdrive-")

(defcustom efrit-testdrive-turn-timeout 120
  "Seconds to wait for one model turn before the step fails."
  :type 'integer)

(defcustom efrit-testdrive-step-budget 60
  "Seconds a step may take before the report flags it as SLOW."
  :type 'integer)

;;;; State

(defvar efrit-testdrive--root nil "The throwaway project directory (canonical).")
(defvar efrit-testdrive--results nil "List of (SECTION NAME STATUS NOTE SECONDS), newest first.")
(defvar efrit-testdrive--buffer "*efrit-testdrive*")
(defvar efrit-testdrive--layout nil "Window configuration at the start, restored after each step.")
(defvar efrit-testdrive--events nil "Events since the last clear, newest first.")
(defvar efrit-testdrive--turns 0 "Model turns sent so far.")
(defvar efrit-testdrive--summary-marker nil "Where the summary goes in the report.")

(define-error 'efrit-testdrive-quit "test drive stopped")

;;;; The report

(defface efrit-testdrive-pass '((t :inherit success)) "A passed step.")
(defface efrit-testdrive-fail '((t :inherit error)) "A failed step.")
(defface efrit-testdrive-skip '((t :inherit shadow)) "A skipped step.")

(define-derived-mode efrit-testdrive-report-mode special-mode "Testdrive"
  "The test drive report: Markdown rendered in place; `q' buries it."
  (setq-local truncate-lines nil))

(defun efrit-testdrive--buf ()
  (let ((buf (get-buffer-create efrit-testdrive--buffer)))
    (with-current-buffer buf
      (unless (derived-mode-p 'efrit-testdrive-report-mode)
        (efrit-testdrive-report-mode)))
    buf))

(defun efrit-testdrive--render (start end)
  "Render START..END of the report as Markdown; colour the statuses."
  (require 'efrit-markdown)
  (let ((inhibit-read-only t))
    (efrit-markdown-render start end t)
    (save-excursion
      (goto-char start)
      (while (re-search-forward "^\\(PASS\\|FAIL\\|SKIP\\) " nil t)
        (put-text-property (match-beginning 1) (match-end 1) 'face
                           (pcase (match-string 1)
                             ("PASS" 'efrit-testdrive-pass)
                             ("FAIL" 'efrit-testdrive-fail)
                             (_ 'efrit-testdrive-skip)))))))

(defun efrit-testdrive--show-report ()
  "Keep the report visible in a side window without taking focus."
  (let ((buf (efrit-testdrive--buf)))
    (unless (get-buffer-window buf)
      (display-buffer buf '(display-buffer-in-side-window (side . right) (window-width . 0.4))))
    (when-let* ((w (get-buffer-window buf)))
      (with-current-buffer buf (set-window-point w (point-max))))))

(defun efrit-testdrive--show-agent ()
  "Keep the agent buffer visible beside the report, without taking focus."
  (when-let* ((buf (and (boundp 'efrit-agent-buffer-name) (get-buffer efrit-agent-buffer-name))))
    (unless (get-buffer-window buf)
      (let ((efrit-agent-auto-show t))
        (efrit-agent-display buf nil)))))

(defun efrit-testdrive--restore-layout ()
  (when (and efrit-testdrive--layout (window-configuration-p efrit-testdrive--layout))
    (set-window-configuration efrit-testdrive--layout))
  (efrit-testdrive--show-report)
  (efrit-testdrive--show-agent))

(defun efrit-testdrive--out (fmt &rest args)
  "Append FMT/ARGS (Markdown) to the report, rendered."
  (with-current-buffer (efrit-testdrive--buf)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (let ((start (point)))
        (insert (apply #'format fmt args) "\n")
        (efrit-testdrive--render start (point))))
    (when-let* ((w (get-buffer-window (current-buffer))))
      (set-window-point w (point-max)))))

(defun efrit-testdrive--log-tail (&optional n)
  (when-let* ((buf (get-buffer "*efrit-log*")))
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-max))
        (forward-line (- (or n 12)))
        (buffer-substring-no-properties (point) (point-max))))))

(defun efrit-testdrive--record (section name status &optional note secs)
  (push (list section name status note secs) efrit-testdrive--results)
  (efrit-testdrive--out "%s %s%s" status name (if secs (format "  (%.1fs)" secs) ""))
  (when (and note (not (string-empty-p note)))
    (efrit-testdrive--out "    %s" note))
  (when (eq status 'FAIL)
    (when-let* ((tail (efrit-testdrive--log-tail)))
      (efrit-testdrive--out "```log\n%s```" tail))))

;;;; Steps

(defvar efrit-testdrive--waited 0
  "Seconds the current step spent waiting for the user; not the step's time.")

(defmacro efrit-testdrive--step (section name &rest body)
  "Run BODY as step NAME in SECTION, recording outcome and timing.
BODY returns PASS/FAIL/SKIP or (STATUS . NOTE).  Errors become FAIL."
  (declare (indent 2))
  `(let ((t0 (float-time))
         (efrit-testdrive--waited 0))
     (message "efrit-testdrive: %s" ,name)
     (condition-case err
         (let* ((r (unwind-protect (progn ,@body)
                     (efrit-testdrive--restore-layout)))
                (secs (- (float-time) t0 efrit-testdrive--waited))
                (slow (and (> secs efrit-testdrive-step-budget)
                           (format "SLOW: %.0fs, budget %ds" secs efrit-testdrive-step-budget))))
           (pcase r
             (`(,st . ,note) (efrit-testdrive--record ,section ,name st
                                                      (if slow (concat note "  " slow) note) secs))
             (st (efrit-testdrive--record ,section ,name (or st 'PASS) slow secs))))
       (efrit-testdrive-quit (signal 'efrit-testdrive-quit nil))
       (quit (signal 'efrit-testdrive-quit nil))
       (error (efrit-testdrive--record ,section ,name 'FAIL
                                       (format "error: %s" (error-message-string err))
                                       (- (float-time) t0 efrit-testdrive--waited))))))

(defun efrit-testdrive--check (ok &optional note)
  (if ok 'PASS (cons 'FAIL note)))

;;;; Asking the user (the tour only)

(defun efrit-testdrive--read-char (prompt choices)
  (let ((t0 (float-time)))
    (unwind-protect (read-char-choice prompt choices)
      (cl-incf efrit-testdrive--waited (- (float-time) t0)))))

(defun efrit-testdrive--ask (prompt)
  "Ask PROMPT; return PASS, FAIL (with a note) or SKIP; q signals quit."
  (efrit-testdrive--show-agent)
  (redisplay)
  (let ((c (efrit-testdrive--read-char (concat prompt "  [y]es/[n]o/[s]kip/[q]uit ") '(?y ?n ?s ?q))))
    (pcase c
      (?y 'PASS)
      (?n (cons 'FAIL (let ((t0 (float-time)))
                        (unwind-protect (read-string "What did you see? ")
                          (cl-incf efrit-testdrive--waited (- (float-time) t0))))))
      (?s 'SKIP)
      (_ (signal 'efrit-testdrive-quit nil)))))

(defun efrit-testdrive--confirm (prompt)
  "For steps where you must do something first.  Non-nil to proceed."
  (efrit-testdrive--show-agent)
  (redisplay)
  (let ((c (efrit-testdrive--read-char (concat prompt "  [RET/y] done, [s]kip, [q]uit ") '(?y ?\r ?s ?q))))
    (pcase c
      ((or ?y ?\r) t)
      (?s nil)
      (_ (signal 'efrit-testdrive-quit nil)))))

(defmacro efrit-testdrive--after-confirm (prompt &rest body)
  "Ask PROMPT; run BODY if confirmed, else the step is SKIP."
  (declare (indent 1))
  `(if (efrit-testdrive--confirm ,prompt)
       (progn ,@body)
     (cons 'SKIP "skipped by user")))

;;;; The throwaway project

(defconst efrit-testdrive--files
  '(("README.md" . "# testdrive\n\nA throwaway project efrit's test drive created.  Safe to delete.\n")
    ("greet.el" . ";;; greet.el --- say hello -*- lexical-binding: t; -*-\n\n(defun greet (name)\n  \"Return a greeting for NAME.\"\n  (format \"Hello, %s!\" name))\n\n(provide 'greet)\n;;; greet.el ends here\n")
    ("notes.txt" . "The secret word is PELICAN.\n"))
  "Files of the throwaway project: (RELATIVE-NAME . CONTENT).")

(defconst efrit-testdrive--png-base64
  "iVBORw0KGgoAAAANSUhEUgAAAAgAAAAICAIAAABLbSncAAAAEklEQVR4nGP4z8CAFWEXHbQSACj/P8Fu7N9hAAAAAElFTkSuQmCC"
  "An 8x8 red PNG, for the image rendering steps.")

(defun efrit-testdrive--make-project ()
  "Create the throwaway project; return its canonical directory.
Canonical because the sandbox keys grants on `efrit-sandbox-canonical'
\(on macOS /var is /private/var)."
  (let ((dir (file-name-as-directory (make-temp-file "efrit-testdrive-" t))))
    (dolist (f efrit-testdrive--files)
      (with-temp-file (expand-file-name (car f) dir) (insert (cdr f))))
    (let ((coding-system-for-write 'binary))
      (with-temp-file (expand-file-name "red.png" dir)
        (set-buffer-multibyte nil)
        (insert (base64-decode-string efrit-testdrive--png-base64))))
    (file-name-as-directory (efrit-sandbox-canonical dir))))

(defun efrit-testdrive--file (rel)
  (expand-file-name rel efrit-testdrive--root))

(defun efrit-testdrive--file-text (rel)
  (let ((f (efrit-testdrive--file rel)))
    (and (file-exists-p f)
         (with-temp-buffer (insert-file-contents f) (buffer-string)))))

(defun efrit-testdrive--outside-file ()
  "A readable file outside the project: the init file, else /etc/hosts."
  (or (and user-init-file (file-readable-p user-init-file) user-init-file)
      (seq-find #'file-readable-p '("~/.emacs.d/init.el" "~/.emacs" "/etc/hosts" "/etc/passwd"))))

;;;; Driving the agent

(defun efrit-testdrive--on-event (event)
  (push event efrit-testdrive--events))

(defun efrit-testdrive--clear-events ()
  (setq efrit-testdrive--events nil))

(defun efrit-testdrive--events-of (type)
  "Events of TYPE since the last clear, oldest first."
  (cl-remove-if-not (lambda (e) (eq (alist-get :type e) type))
                    (reverse efrit-testdrive--events)))

(defun efrit-testdrive--agent-buffer ()
  (require 'efrit-agent)
  (get-buffer efrit-agent-buffer-name))

(defun efrit-testdrive--session ()
  (with-current-buffer (efrit-testdrive--agent-buffer) efrit-agent--repl-session))

(defun efrit-testdrive--wait-for (pred &optional timeout what)
  "Run the event loop until PRED is non-nil or TIMEOUT seconds pass."
  (let* ((timeout (or timeout efrit-testdrive-turn-timeout))
         (start (float-time))
         (deadline (+ start timeout))
         v)
    (while (and (not (setq v (funcall pred))) (< (float-time) deadline))
      (message "efrit-testdrive: waiting for %s (%ds of %ds)"
               (or what "the model") (round (- (float-time) start)) timeout)
      (accept-process-output nil 0.5)
      (sit-for 0.1))
    (message nil)
    v))

(defun efrit-testdrive--turn-ended-p ()
  "The turn-complete event that ended a turn, not a pause on a question."
  (cl-find-if (lambda (e) (not (equal (alist-get :stop-reason e) "waiting-for-user")))
              (efrit-testdrive--events-of 'turn-complete)))

(defun efrit-testdrive--submit (shown &optional api-input)
  "Start a turn; error when the agent is busy."
  (require 'efrit-agent-input)
  (efrit-testdrive--clear-events)
  (cl-incf efrit-testdrive--turns)
  (unless (efrit-submit shown api-input)
    (error "The agent buffer is busy; the turn was not sent")))

(defun efrit-testdrive--turn (shown &optional api-input)
  "Send SHOWN (and API-INPUT) as a turn; wait for it to end or pause.
Returns the `turn-complete' event, or nil on timeout (the turn is then
cancelled so the next step starts clean)."
  (efrit-testdrive--submit shown api-input)
  (let ((done (efrit-testdrive--wait-for
               (lambda () (car (efrit-testdrive--events-of 'turn-complete)))
               nil "the turn to complete")))
    (unless done
      (with-current-buffer (efrit-testdrive--agent-buffer)
        (ignore-errors (efrit-agent-cancel))))
    done))

(defun efrit-testdrive--reply-text ()
  "The assistant text of the last turn: streamed text plus the completion message."
  (concat (mapconcat (lambda (e) (or (alist-get :text e) ""))
                     (efrit-testdrive--events-of 'text-delta) "")
          (mapconcat (lambda (e) (or (alist-get :completion-message e) ""))
                     (efrit-testdrive--events-of 'turn-complete) "")))

(defun efrit-testdrive--tools-run ()
  "Names of the tools run in the last turn, in order."
  (mapcar (lambda (e) (alist-get :tool e)) (efrit-testdrive--events-of 'tool-result)))

(defun efrit-testdrive--stop-reason (event)
  (and event (alist-get :stop-reason event)))

(defun efrit-testdrive--turn-note (event)
  (format "stop %s; tools %s; %d turn(s) so far"
          (or (efrit-testdrive--stop-reason event) "timeout")
          (or (efrit-testdrive--tools-run) "none")
          efrit-testdrive--turns))

(defun efrit-testdrive--reply-check (ev regexp)
  "PASS when EV ended and the reply matches REGEXP; else FAIL with why."
  (let ((text (efrit-testdrive--reply-text)))
    (cond
     ((not ev) (cons 'FAIL "timed out"))
     ((string-match-p regexp text) (cons 'PASS (efrit-testdrive--turn-note ev)))
     (t (cons 'FAIL (format "%s; reply: %s" (efrit-testdrive--turn-note ev)
                            (truncate-string-to-width text 120 nil nil "…")))))))

(defun efrit-testdrive--agent-text ()
  "The conversation of the agent buffer, without properties."
  (with-current-buffer (efrit-testdrive--agent-buffer)
    (buffer-substring-no-properties (point-min) (marker-position efrit-agent--conversation-end))))

(defun efrit-testdrive--answer-text ()
  "The model's last rendered answer in the agent buffer, or \"\"."
  (with-current-buffer (efrit-testdrive--agent-buffer)
    (let ((b (efrit-agent--last-claude-message-bounds)))
      (if b (buffer-substring-no-properties (car b) (cdr b)) ""))))

(defun efrit-testdrive--user-lines ()
  "Every user line in the agent buffer as (POS TEXT KIND), for a failure note."
  (with-current-buffer (efrit-testdrive--agent-buffer)
    (save-restriction
      (widen)
      (let ((p (point-min)) out)
        (while (setq p (text-property-not-all p (point-max) 'efrit-user-text nil))
          (push (list p (truncate-string-to-width (get-text-property p 'efrit-user-text) 30 nil nil "…")
                      (get-text-property p 'efrit-user-kind))
                out)
          (setq p (or (next-single-property-change p 'efrit-user-text) (point-max))))
        (nreverse out)))))

(defun efrit-testdrive--type-input (text)
  "Put TEXT into the agent buffer's input region."
  (with-current-buffer (efrit-testdrive--agent-buffer)
    (efrit-agent--clear-input)
    (goto-char (point-max))
    (insert text)))

(defun efrit-testdrive--grant (cap &optional target)
  "Grant CAP on TARGET for this session, quietly.
TARGET defaults to the project for read/write and to t for `elisp'
\(the sandbox's name for eval_sexp; its target is always t).  The
automatic drive never lets a sandbox prompt reach the user: on
2026-09-25 a prompt raised while the drive owned the event loop could
not be answered and Emacs looked hung."
  (efrit-sandbox-grant cap (or target (if (eq cap 'elisp) t efrit-testdrive--root))
                       'session efrit-testdrive--root))

(defvar efrit-testdrive--unanswered nil
  "Sandbox requests the automatic drive refused, newest first.
Anything here is a step that did not grant what its turn needed.")

(defun efrit-testdrive--refuse (req)
  "The sandbox request function while the automatic drive runs: refuse and note."
  (push req efrit-testdrive--unanswered)
  (efrit-log 'warn "testdrive: sandbox request refused unattended: %S" req)
  nil)

(defun efrit-testdrive--start-slow-turn ()
  "Start a turn of six 2 s tool calls; return when the first tool has started."
  (efrit-testdrive--grant 'elisp)
  ;; A nonce in the request: the model answered a repeat of this
  ;; prompt from memory, without any tool call, and there was nothing
  ;; to cancel or steer
  (efrit-testdrive--submit
   (format "Task %s. You must call the eval_sexp tool six separate times, one call per number: evaluate (progn (sleep-for 2) (* N N)) for N = 1, 2, 3, 4, 5, 6. Do not compute them yourself and do not ask me anything. After the sixth call, list the six results."
           (format-time-string "%H%M%S")))
  (efrit-testdrive--wait-for (lambda () (or (efrit-testdrive--events-of 'tool-start)
                                            (efrit-testdrive--events-of 'turn-complete)))
                             30 "the first tool call")
  (and (efrit-testdrive--events-of 'tool-start)
       (not (efrit-testdrive--events-of 'turn-complete))))

;;;; Sections: automatic

(defun efrit-testdrive--section-0 ()
  "Setup: doctor, key, agent buffer."
  (efrit-testdrive--out "\n## 0. Setup")
  (efrit-testdrive--step 0 "efrit-doctor reports no failure"
    (require 'efrit-doctor)
    (efrit-testdrive--check (save-window-excursion (efrit-doctor))
                            "efrit-doctor found problems; see *efrit-doctor*"))
  (efrit-testdrive--step 0 "API key and endpoint resolve"
    (require 'efrit-common)
    (let ((key (ignore-errors (efrit-common-get-api-key)))
          (url (ignore-errors (efrit-common-get-api-url))))
      (if (and key url)
          (cons 'PASS (format "endpoint %s, model %s" url efrit-default-model))
        (cons 'FAIL "no key or no endpoint"))))
  (efrit-testdrive--step 0 "The agent buffer opens, with a header, in the project"
    (require 'efrit-agent)
    (let ((default-directory efrit-testdrive--root))
      (save-window-excursion (call-interactively #'efrit)))
    (with-current-buffer (efrit-testdrive--agent-buffer)
      ;; A buffer left from an earlier drive points at that drive's
      ;; deleted project; every subprocess then failed to start
      (setq default-directory efrit-testdrive--root)
      (efrit-testdrive--check (and header-line-format
                                   (efrit-agent-repl-session)
                                   (equal default-directory efrit-testdrive--root))
                              (format "header %S, dir %s" (and header-line-format t) default-directory)))))

(defun efrit-testdrive--section-1 ()
  "A round trip, and what the buffer shows of it."
  (efrit-testdrive--out "\n## 1. A round trip")
  (efrit-testdrive--step 1 "A plain question is answered"
    (efrit-testdrive--reply-check
     (efrit-testdrive--turn "Reply with exactly the word PONG and nothing else.") "PONG"))
  (efrit-testdrive--step 1 "The transcript shows the turn and the answer, and the prompt is back"
    (with-current-buffer (efrit-testdrive--agent-buffer)
      (progn
        (efrit-testdrive--check
         (and (efrit-agent--find-user-message "Reply with exactly the word PONG and nothing else." nil)
              (string-match-p "PONG" (efrit-testdrive--answer-text))
              (eq 'idle (efrit-repl-session-status efrit-agent--repl-session))
              (equal "" (efrit-agent--get-input)))
         (format "user line %s, answer %S, status %s"
                 (and (efrit-agent--find-user-message "Reply with exactly the word PONG and nothing else." nil) t)
                 (truncate-string-to-width (efrit-testdrive--answer-text) 40 nil nil "…")
                 (efrit-repl-session-status efrit-agent--repl-session))))))
  (efrit-testdrive--step 1 "Usage is recorded for the session"
    (require 'efrit-usage)
    (let ((u (efrit-usage-for (with-current-buffer (efrit-testdrive--agent-buffer)
                                (efrit-agent-session-id)))))
      (efrit-testdrive--check (and u (> (efrit-usage-context u) 0))
                              (format "usage record: %S" u)))))

(defun efrit-testdrive--section-2 ()
  "Tools and the sandbox: read, write, refuse, shell, eval."
  (efrit-testdrive--out "\n## 2. Tools and the sandbox")
  (efrit-testdrive--step 2 "A read inside the project runs without a prompt"
    (let ((ev (efrit-testdrive--turn "Read notes.txt in this project and tell me the secret word. Answer with the word only.")))
      (cond
       ((efrit-testdrive--events-of 'sandbox-denied) (cons 'FAIL "a read inside the project was denied"))
       (t (efrit-testdrive--reply-check ev "PELICAN")))))
  (efrit-testdrive--step 2 "The read shows as a folded tool row that unfolds"
    (with-current-buffer (efrit-testdrive--agent-buffer)
      (goto-char (point-min))
      ;; `text-property-any' compares with `eq': useless for a string
      (let ((pos (cl-loop for p = (point-min) then (next-single-property-change p 'efrit-tool-name)
                          while p
                          when (equal (get-text-property p 'efrit-tool-name) "read_file") return p)))
        (if (not pos)
            (cons 'FAIL "no read_file row in the buffer")
          (let* ((region (efrit-agent--find-tool-region (get-text-property pos 'efrit-id)))
                 (body (efrit-agent--tool-body-bounds (car region) (cdr region))))
            (goto-char (car region))
            (let ((folded (and body (get-text-property (car body) 'invisible))))
              (efrit-agent--toggle-tool-expansion)
              (let ((open (and body (not (get-text-property (car body) 'invisible)))))
                (efrit-agent--toggle-tool-expansion)
                (efrit-testdrive--check (and body folded open)
                                        (format "body %s, folded %s, unfolded on toggle %s"
                                                (and body t) folded open)))))))))
  (efrit-testdrive--step 2 "A write lands with a session grant, and the reviewer judged it"
    (efrit-testdrive--grant 'write)
    (efrit-testdrive--clear-events)
    (let ((ev (efrit-testdrive--turn "In greet.el, change the greeting from \"Hello, %s!\" to \"Howdy, %s!\". Edit the file, then stop."))
          (text (efrit-testdrive--file-text "greet.el")))
      (cond
       ((not ev) (cons 'FAIL "timed out"))
       ((not (and text (string-match-p "Howdy, %s!" text)))
        (cons 'FAIL (format "greet.el not changed; %s" (efrit-testdrive--turn-note ev))))
       ((and efrit-review-enabled (null (efrit-testdrive--events-of 'review-verdict)))
        (cons 'FAIL "edit landed but no review-verdict event"))
       (t (cons 'PASS (format "%s; review %s" (efrit-testdrive--turn-note ev)
                              (if efrit-review-enabled
                                  (alist-get :verdict (car (efrit-testdrive--events-of 'review-verdict)))
                                "off")))))))
  (efrit-testdrive--step 2 "A read outside the project is refused without a prompt when so configured"
    ;; No user at the keyboard: the sandbox must not block.  Bind the
    ;; prompt away so a request outside the project is a denial.
    (efrit-testdrive--clear-events)
    (let* ((file (efrit-testdrive--outside-file))
           (ev (efrit-testdrive--turn
                (format "Read the file %s and tell me its first line. If you cannot, say CANNOT and stop." file))))
      (cond
       ((not ev) (cons 'FAIL "timed out"))
       ((null (efrit-testdrive--events-of 'sandbox-denied))
        (cons 'FAIL (format "no sandbox denial recorded; %s" (efrit-testdrive--turn-note ev))))
       ((not (member (efrit-testdrive--stop-reason ev) '("end_turn" "session-complete")))
        (cons 'FAIL (format "the turn ended with %s, not a normal answer" (efrit-testdrive--stop-reason ev))))
       (t (cons 'PASS (efrit-testdrive--turn-note ev))))))
  (efrit-testdrive--step 2 "No grant leaked outside the project"
    (let ((outside (cl-remove-if
                    (lambda (g) (or (not (stringp (plist-get g :target)))
                                    (string-prefix-p efrit-testdrive--root (plist-get g :target))))
                    (efrit-sandbox-grants efrit-testdrive--root))))
      (efrit-testdrive--check (null outside) (format "grants outside: %S" outside))))
  (efrit-testdrive--step 2 "A shell command runs under a per-command grant"
    ;; The model runs it as "cd <project> && ls": both names need the grant
    (efrit-testdrive--grant 'shell '(shell "cd" "ls"))
    (let ((ev (efrit-testdrive--turn "Run the shell command `ls` in the project directory and list the file names it printed.")))
      (efrit-testdrive--reply-check ev "greet\\.el")))
  (efrit-testdrive--step 2 "eval_sexp cannot turn the sandbox off"
    (efrit-testdrive--grant 'elisp)
    (let ((was efrit-sandbox-enabled)
          (ev (efrit-testdrive--turn "Using eval_sexp, evaluate (setq efrit-sandbox-enabled nil) and report the result. If it is refused, say REFUSED.")))
      (cond
       ((not ev) (cons 'FAIL "timed out"))
       ((not (eq efrit-sandbox-enabled was)) (setq efrit-sandbox-enabled was)
        (cons 'FAIL "the model switched the sandbox off"))
       (t (cons 'PASS (efrit-testdrive--turn-note ev)))))))

(defun efrit-testdrive--section-3 ()
  "Interaction: a question, a cancel, the queue, steering."
  (efrit-testdrive--out "\n## 3. Interaction")
  (efrit-testdrive--step 3 "The model's question pauses the turn; the answer resumes it"
    (let ((ev (efrit-testdrive--turn "Use request_user_input to ask me which colour I prefer, with the options red and blue. Wait for my answer, then repeat it back.")))
      (cond
       ((not ev) (cons 'FAIL "timed out"))
       ((not (equal (efrit-testdrive--stop-reason ev) "waiting-for-user"))
        (cons 'FAIL (format "the turn did not pause: %s" (efrit-testdrive--turn-note ev))))
       (t
        ;; Answer as the user would, from the input
        (efrit-testdrive--type-input "blue")
        (with-current-buffer (efrit-testdrive--agent-buffer)
          (goto-char (point-max))
          (efrit-agent-input-send))
        (let ((done (efrit-testdrive--wait-for #'efrit-testdrive--turn-ended-p nil "the answer to be repeated")))
          (cond
           ((not done) (cons 'FAIL "no completion after the answer"))
           ((string-match-p "blue" (downcase (efrit-testdrive--reply-text))) 'PASS)
           (t (cons 'FAIL "the answer was not repeated back"))))))))
  (efrit-testdrive--step 3 "Cancel stops a running turn and the buffer is idle again"
    (if (not (efrit-testdrive--start-slow-turn))
        (cons 'FAIL "the slow turn did not start a tool within 30 s")
      (with-current-buffer (efrit-testdrive--agent-buffer) (efrit-agent-cancel))
      (let ((ev (efrit-testdrive--wait-for #'efrit-testdrive--turn-ended-p 30 "the cancel to land")))
        (cond
         ((not ev) (cons 'FAIL "the turn did not end within 30 s of the cancel"))
         ((not (eq 'idle (efrit-repl-session-status (efrit-testdrive--session))))
          (cons 'FAIL (format "turn ended (%s) but the session is %s" (efrit-testdrive--stop-reason ev)
                              (efrit-repl-session-status (efrit-testdrive--session)))))
         ((with-current-buffer (efrit-testdrive--agent-buffer)
            (bound-and-true-p efrit-agent--thinking-indicator))
          (cons 'FAIL "the thinking indicator is still shown"))
         (t (cons 'PASS (format "stop %s" (efrit-testdrive--stop-reason ev))))))))
  (efrit-testdrive--step 3 "An input submitted while busy is queued, marked, and sent after"
    (if (not (efrit-testdrive--start-slow-turn))
        (cons 'FAIL "the slow turn did not start")
      (with-current-buffer (efrit-testdrive--agent-buffer)
        (efrit-agent-busy-submit-queue "What is the secret word in notes.txt? Answer with the word only."))
      (let ((marked (with-current-buffer (efrit-testdrive--agent-buffer)
                      (and (efrit-agent--find-user-message
                            "What is the secret word in notes.txt? Answer with the word only." 'queued)
                           t)))
            (queued (car (efrit-testdrive--events-of 'queued))))
        (efrit-testdrive--wait-for
         (lambda () (= 2 (length (efrit-testdrive--events-of 'turn-complete))))
         (* 2 efrit-testdrive-turn-timeout) "the slow turn, then the queued one")
        (cl-incf efrit-testdrive--turns)
        (let ((sent-mark (with-current-buffer (efrit-testdrive--agent-buffer)
                           (and (efrit-agent--find-user-message
                                 "What is the secret word in notes.txt? Answer with the word only." nil)
                                t))))
          (cond
           ((not queued) (cons 'FAIL "no `queued' event"))
           ((not marked) (cons 'FAIL (format "the queued line was not drawn with the waiting mark; user lines: %S"
                                             (efrit-testdrive--user-lines))))
           ((< (length (efrit-testdrive--events-of 'turn-complete)) 2)
            (cons 'FAIL "the queued turn did not run"))
           ((not (string-match-p "PELICAN" (efrit-testdrive--agent-text)))
            (cons 'FAIL "the queued turn ran but did not answer PELICAN"))
           ((not sent-mark) (cons 'FAIL (format "the waiting mark was not turned into a sent one; user lines: %S"
                                                (efrit-testdrive--user-lines))))
           (t 'PASS))))))
  (efrit-testdrive--step 3 "A steer reaches the running turn with its next tool results"
    (if (not (efrit-testdrive--start-slow-turn))
        (cons 'FAIL "the slow turn did not start")
      (with-current-buffer (efrit-testdrive--agent-buffer)
        (efrit-agent-busy-submit-steer "Change of plan: end your final message with the single word BANANA in capitals."))
      (let ((delivered (efrit-testdrive--wait-for
                        (lambda () (car (efrit-testdrive--events-of 'steered)))
                        60 "the steering to reach the model")))
        (efrit-testdrive--wait-for #'efrit-testdrive--turn-ended-p nil "the steered turn to complete")
        (cond
         ((not delivered) (cons 'FAIL "no `steered' event: the text never went out with tool results"))
         ((not (with-current-buffer (efrit-testdrive--agent-buffer)
                 (efrit-agent--find-user-message
                  "Change of plan: end your final message with the single word BANANA in capitals." 'steer)))
          (cons 'FAIL (format "delivered, but the steer line is not in the buffer; user lines: %S"
                              (efrit-testdrive--user-lines))))
         ((not (string-match-p "BANANA" (efrit-testdrive--reply-text)))
          (cons 'PASS "delivered with the tool results; the model did not act on it (no BANANA) -- a model choice, not a plumbing fault"))
         (t 'PASS))))))

(defun efrit-testdrive--section-4 ()
  "Rendering: Markdown, folds, copy."
  (efrit-testdrive--out "\n## 4. Rendering")
  (efrit-testdrive--step 4 "Markdown renders in place: markup gone, faces on, file reference linked"
    (let ((ev (efrit-testdrive--turn "Reply with exactly this Markdown, nothing else: a level-2 header `Report`, one sentence with a **bold** word and an *italic* word, a bullet list of two items, a fenced ```elisp block containing (defun ok () t), and a final line citing greet.el:3.")))
      (if (not ev)
          (cons 'FAIL "timed out")
        (with-current-buffer (efrit-testdrive--agent-buffer)
          (let* ((b (efrit-agent--last-claude-message-bounds))
                 (text (if b (buffer-substring-no-properties (car b) (cdr b)) ""))
                 (has (lambda (face)
                        (and b (cl-loop for p from (car b) below (cdr b)
                                        thereis (memq face (ensure-list (get-text-property p 'face)))))))
                 (link (and b (text-property-not-all (car b) (cdr b) 'efrit-markdown-target nil))))
            (cond
             ((string-empty-p text) (cons 'FAIL "no rendered answer found"))
             ((string-match-p "\\*\\*\\|^## \\|```" text)
              (cons 'FAIL (format "markup still visible: %s" (truncate-string-to-width text 100 nil nil "…"))))
             ((not (funcall has 'efrit-markdown-header)) (cons 'FAIL "no header face"))
             ((not (funcall has 'efrit-markdown-bold)) (cons 'FAIL "no bold face"))
             ((not (funcall has 'efrit-markdown-code-block)) (cons 'FAIL "no code block face"))
             ((not link) (cons 'FAIL "greet.el:3 is not a link"))
             (t (cons 'PASS (format "link -> %S" (get-text-property link 'efrit-markdown-target))))))))))
  (efrit-testdrive--step 4 "A folded body is found by isearch and folds back after"
    (with-current-buffer (efrit-testdrive--agent-buffer)
      (let* ((efrit-agent-display-mode 'minimal)
             (id (efrit-agent--add-tool-call "eval_sexp" '(("expr" . "(concat \"needle-\" \"XYZZY\")")))))
        (efrit-agent--update-tool-result id "\"needle-XYZZY\"" t 0.1)
        (let* ((region (efrit-agent--find-tool-region id))
               (body (efrit-agent--tool-body-bounds (car region) (cdr region)))
               (m (and body (save-excursion (goto-char (car body)) (search-forward "XYZZY" (cdr body) t) (point))))
               (folded-before (and body (get-text-property (car body) 'invisible)))
               (opened (and m (let ((search-invisible 'open))
                                (efrit-agent--isearch-filter (- m 5) m))))
               (open-after (and body (not (get-text-property (car body) 'invisible)))))
          ;; end the "search" with point elsewhere: it must fold back
          (goto-char (point-max))
          (efrit-agent--isearch-cleanup)
          (efrit-testdrive--check
           (and folded-before opened open-after (get-text-property (car body) 'invisible))
           (format "folded %s, predicate opened %s, visible after %s, refolded %s"
                   folded-before opened open-after (and body (get-text-property (car body) 'invisible))))))))
  (efrit-testdrive--step 4 "Copy the last answer from anywhere"
    (with-current-buffer (efrit-testdrive--agent-buffer)
      (goto-char (point-min))
      (let ((kill-ring nil))
        (efrit-agent-copy-last-output)
        (efrit-testdrive--check (and (car kill-ring) (string-match-p "greet\\.el:3\\|Report" (car kill-ring)))
                                (format "kill ring got: %s"
                                        (truncate-string-to-width (or (car kill-ring) "nothing") 80 nil nil "…")))))))

(defun efrit-testdrive--section-5 ()
  "Input: @mentions, /commands, drop, restart."
  (efrit-testdrive--out "\n## 5. Input")
  (efrit-testdrive--step 5 "@ completes project files, and a mention inlines the file"
    (efrit-testdrive--type-input "@gr")
    (with-current-buffer (efrit-testdrive--agent-buffer)
      (goto-char (point-max))
      (let ((capf (efrit-agent-mention-completion-at-point)))
        (efrit-agent--clear-input)
        (unless (and capf (member "greet.el" (all-completions "gr" (nth 2 capf))))
          (error "@ completion did not offer greet.el"))))
    (let* ((q "What does the function in @greet.el return for the name Ada? Answer with the string only, no tools.")
           (ev (efrit-testdrive--turn q (efrit-agent--api-input-for q))))
      (cond
       ((not ev) (cons 'FAIL "timed out"))
       ((remove "session_complete" (efrit-testdrive--tools-run))
        (cons 'FAIL (format "the model used tools (%s): the mention was not inlined"
                            (remove "session_complete" (efrit-testdrive--tools-run)))))
       (t (efrit-testdrive--reply-check ev "\\(Hello\\|Howdy\\), Ada")))))
  (efrit-testdrive--step 5 "/commands complete at the input start and run in place"
    (with-current-buffer (efrit-testdrive--agent-buffer)
      (efrit-testdrive--type-input "/mo")
      (goto-char (point-max))
      (let* ((capf (efrit-agent-slash-completion-at-point))
             (offered (and capf (all-completions "mo" (nth 2 capf))))
             (ran (progn (efrit-testdrive--type-input "/help")
                         (goto-char (point-max))
                         (efrit-agent-input-send)
                         (get-buffer "*efrit slash commands*")))
             (kept (progn (efrit-testdrive--type-input "/nonesuch")
                          (goto-char (point-max))
                          (efrit-agent-input-send)
                          (efrit-agent--get-input))))
        (when ran (quit-windows-on ran))
        (efrit-testdrive--type-input "")
        (efrit-testdrive--check
         (and (member "model" offered) (member "mode" offered) ran (equal kept "/nonesuch"))
         (format "offered %S, /help ran %s, unknown kept %S" offered (and ran t) kept)))))
  (efrit-testdrive--step 5 "A dropped file becomes a mention"
    (with-current-buffer (efrit-testdrive--agent-buffer)
      (efrit-testdrive--type-input "")
      (efrit-agent-dnd-handle (list (concat "file://" (efrit-testdrive--file "notes.txt"))) 'copy)
      (let ((input (efrit-agent--get-input)))
        (efrit-testdrive--type-input "")
        (efrit-testdrive--check (equal input "@notes.txt") (format "input after the drop: %S" input)))))
  (efrit-testdrive--step 5 "Restart gives a fresh session in the same windows"
    (let ((before (with-current-buffer (efrit-testdrive--agent-buffer)
                    (list (efrit-agent-session-id)
                          (length (get-buffer-window-list (current-buffer) nil t))))))
      (with-current-buffer (efrit-testdrive--agent-buffer)
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t)))
          (efrit-agent-restart)))
      (let ((after (with-current-buffer (efrit-testdrive--agent-buffer)
                     (list (efrit-agent-session-id)
                           (length (get-buffer-window-list (current-buffer) nil t))))))
        (efrit-testdrive--check (and (not (equal (car before) (car after)))
                                     (= (cadr before) (cadr after)))
                                (format "session %s -> %s, windows %d -> %d"
                                        (car before) (car after) (cadr before) (cadr after)))))))

(defun efrit-testdrive--section-6 ()
  "Transcript tools: quote, narrow, transcript file, list edit, tables and images, stale busy, diff open."
  (efrit-testdrive--out "\n## 6. Transcript tools")
  (efrit-testdrive--step 6 "A region of the transcript is quoted into the input; while busy it is queued"
    (with-current-buffer (efrit-testdrive--agent-buffer)
      (efrit-testdrive--type-input "")
      (let* ((bounds (or (efrit-agent--last-claude-message-bounds) (cons (point-min) (point-min))))
             (start (car bounds))
             (end (min (cdr bounds) (save-excursion (goto-char start) (line-end-position))))
             (session (efrit-testdrive--session))
             (idle-input (progn (efrit-agent-quote-region start end) (efrit-agent--get-input)))
             (queued nil))
        (efrit-testdrive--type-input "")
        (efrit-repl-loop-hold session)
        (unwind-protect
            (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "and why?")))
              (efrit-agent-quote-region start end)
              (setq queued (copy-sequence (efrit-repl-session-queue session))))
          (while (efrit-repl-session-dequeue session))
          (efrit-repl-loop-release session)
          (when queued (efrit-agent--unmark-queued-message (car queued) 'dropped)))
        (efrit-testdrive--check
         (and (string-prefix-p "> " idle-input)
              (= 1 (length queued)) (string-suffix-p "\n\nand why?" (car queued)))
         (format "idle input %S; queued %S" (truncate-string-to-width idle-input 50 nil nil "…")
                 (and queued (truncate-string-to-width (car queued) 50 nil nil "…")))))))
  (efrit-testdrive--step 6 "Narrow to the last turn hides earlier ones; widen brings them back"
    (with-current-buffer (efrit-testdrive--agent-buffer)
      (let* ((turns (length (efrit-agent--turn-starts)))
             (_ (efrit-agent-narrow-to-turns 1))
             (narrowed (buffer-narrowed-p))
             (shown (length (efrit-agent--turn-starts)))
             (input-visible (>= (point-max) (marker-position efrit-agent--input-start))))
        (efrit-agent-widen)
        (efrit-testdrive--check
         (and (> turns 1) narrowed (= shown 1) input-visible (not (buffer-narrowed-p)))
         (format "%d turns, narrowed %s, %d shown, input visible %s, widened %s"
                 turns narrowed shown input-visible (not (buffer-narrowed-p)))))))
  (efrit-testdrive--step 6 "The transcript file has this session's turns and tool calls"
    (let* ((session (efrit-testdrive--session))
           (file (efrit-transcript-file session))
           (text (and file (file-exists-p file)
                      (with-temp-buffer (insert-file-contents file) (buffer-string)))))
      (cond
       ((not efrit-transcript-enabled) (cons 'SKIP "efrit-transcript-enabled is nil"))
       ((not text) (cons 'FAIL (format "no transcript at %s" file)))
       (t (efrit-testdrive--check
           (and (string-match-p "^## [0-9:]+ You$" text)
                (string-match-p "^### tool `" text)
                (string-match-p "^### efrit$" text))
           (format "%s: %d chars, %d turns, %d tool calls" (abbreviate-file-name file) (length text)
                   (cl-count-if (lambda (l) (string-match-p "^## [0-9:]+ You$" l)) (split-string text "\n"))
                   (cl-count-if (lambda (l) (string-prefix-p "### tool `" l)) (split-string text "\n"))))))))
  (efrit-testdrive--step 6 "S-RET continues a list item in the input; TAB indents it"
    (with-current-buffer (efrit-testdrive--agent-buffer)
      (efrit-testdrive--type-input "- one")
      (goto-char (point-max))
      (efrit-agent-input-newline)
      (insert "two")
      (efrit-agent-input-tab)
      (let ((a (buffer-substring-no-properties efrit-agent--input-start (point-max))))
        (efrit-agent-input-newline)
        (efrit-agent-input-newline)
        (let ((b (buffer-substring-no-properties efrit-agent--input-start (point-max))))
          (efrit-testdrive--type-input "")
          (efrit-testdrive--check (and (equal a "- one\n  - two") (equal b "- one\n  - two\n\n"))
                                  (format "after TAB %S, after two S-RET %S" a b))))))
  (efrit-testdrive--step 6 "A table and an image in the answer render as columns and a picture"
    (let ((ev (efrit-testdrive--turn
               "Reply with exactly this Markdown and nothing else: a pipe table with header `Fruit | Count`, a separator row, rows `apple | 3` and `kiwi | 12`; then a blank line; then the image `![red square](red.png)`.")))
      (if (not ev)
          (cons 'FAIL "timed out")
        (with-current-buffer (efrit-testdrive--agent-buffer)
          (let* ((b (efrit-agent--last-claude-message-bounds))
                 (text (if b (buffer-substring-no-properties (car b) (cdr b)) ""))
                 (border (and b (text-property-any (car b) (cdr b) 'face 'efrit-markdown-table-border)))
                 (header (and b (cl-loop for p from (car b) below (cdr b)
                                         thereis (memq 'efrit-markdown-table-header
                                                       (ensure-list (get-text-property p 'face))))))
                 (img-pos (and b (text-property-not-all (car b) (cdr b) 'efrit-markdown-image-source nil)))
                 (img (and img-pos (get-text-property img-pos 'display))))
            (cond
             ((string-empty-p text) (cons 'FAIL "no rendered answer"))
             ((string-match-p "^|" text) (cons 'FAIL "raw pipe rows still visible"))
             ((not (and border header)) (cons 'FAIL "no table faces"))
             ((not img-pos) (cons 'FAIL "no image reference found in the answer"))
             ((and (display-graphic-p) (not (eq (car-safe img) 'image)))
              (cons 'FAIL (format "image not drawn at %d (display %S)" img-pos img)))
             (t (cons 'PASS (format "table drawn; image %s" (if (display-graphic-p) "drawn" "alt only (text display)"))))))))))
  (efrit-testdrive--step 6 "A session left `working' with no loop is recovered instead of saying busy"
    (let ((session (efrit-testdrive--session)))
      (efrit-repl-session-set-status session 'working)
      (let ((busy (with-current-buffer (efrit-testdrive--agent-buffer) (efrit-agent--session-busy-p))))
        (efrit-testdrive--check (and (not busy) (eq 'idle (efrit-repl-session-status session)))
                                (format "busy-p %s, status %s" busy (efrit-repl-session-status session))))))
  (efrit-testdrive--step 6 "Time spent on a prompt does not count against the turn clock"
    (let* ((session (efrit-testdrive--session))
           (efrit-user-waiting-seconds efrit-user-waiting-seconds)
           (efrit-user-waiting-depth 0))
      ;; Pretend a turn began 1 s ago and the user then read a prompt for 3 s
      (setf (efrit-repl-session-current-turn-start session) (time-subtract (current-time) 1)
            (efrit-repl-session-turn-waiting-mark session) efrit-user-waiting-seconds)
      ;; The prompt itself is simulated: three seconds booked as waiting
      (cl-incf efrit-user-waiting-seconds 3)
      (let ((elapsed (funcall (efrit-loop-adapter-elapsed-fn efrit-repl-loop--adapter) session)))
        (efrit-testdrive--check (< elapsed 2)
                                (format "turn clock reads %.1fs after 1 s of work + 3 s of prompt" elapsed)))))
  (efrit-testdrive--step 6 "The diff preview opens the file at the changed line, found by text"
    (require 'efrit-tool-show-diff-preview)
    (let ((efrit-diff-preview--root (efrit-testdrive--file ""))
          (efrit-diff-preview--apply-mode 'all_or_nothing)
          (line nil))
      (cl-letf (((symbol-function 'pop-to-buffer) (lambda (b &rest _) (set-buffer b)))
                ((symbol-function 'find-file-other-window)
                 (lambda (f) (set-buffer (find-file-noselect f))))
                ((symbol-function 'recenter) #'ignore))
        (efrit-diff-preview--display
         '(((file . "greet.el") (old_content . "  (format \"Hello, %s!\" name))\n")
            (new_content . "  (format \"Howdy, %s!\" name))\n")))
         "greeting" 'all_or_nothing)
        (with-current-buffer efrit-diff-preview-buffer-name
          (goto-char (point-min))
          (re-search-forward "^-")
          (efrit-diff-preview-open-file))
        (setq line (line-number-at-pos))
        (when-let* ((b (get-file-buffer (efrit-testdrive--file "greet.el")))) (kill-buffer b))
        (kill-buffer efrit-diff-preview-buffer-name))
      (efrit-testdrive--check (= line 5) (format "landed on line %s (expected 5)" line)))))

(defconst efrit-testdrive--sections
  '((0 "Setup" efrit-testdrive--section-0)
    (1 "A round trip" efrit-testdrive--section-1)
    (2 "Tools and the sandbox" efrit-testdrive--section-2)
    (3 "Interaction: question, cancel, queue, steer" efrit-testdrive--section-3)
    (4 "Rendering" efrit-testdrive--section-4)
    (5 "Input: mentions, commands, drop, restart" efrit-testdrive--section-5)
    (6 "Transcript tools: quote, narrow, transcript, lists, tables, images" efrit-testdrive--section-6))
  "The automatic drive's sections.")

;;;; The tour: what needs eyes

(defun efrit-testdrive--tour-header ()
  (efrit-testdrive--out "\n## Header")
  (efrit-testdrive--step 'tour "The header shows the logo, model, usage and status"
    (efrit-testdrive--ask "At the top of the agent buffer: the ef tile, the model name, a usage bar, and a status word.  All there and readable?")))

(defun efrit-testdrive--tour-folding ()
  (efrit-testdrive--out "\n## Folding")
  (efrit-testdrive--step 'tour "A folded row unfolds on RET, on click, and on C-s"
    (with-current-buffer (efrit-testdrive--agent-buffer)
      (let ((efrit-agent-display-mode 'minimal)
            (id (efrit-agent--add-tool-call "eval_sexp" '(("expr" . "(list 'needle 'XYZZY)")))))
        (efrit-agent--update-tool-result id "(needle XYZZY)" t 0.1)
        (goto-char (car (efrit-agent--find-tool-region id)))))
    (efrit-testdrive--ask "A folded row `▶ ✓ eval_sexp` was added and point is on it.  Press RET: does it unfold (▼) and show the result?  Press RET again to fold.  Click the ▶ with the mouse: does it unfold too?  Then C-s XYZZY RET: does the search open it?")))

(defun efrit-testdrive--tour-menu ()
  (efrit-testdrive--out "\n## Menus")
  (efrit-testdrive--step 'tour "C-c ? opens the buffer menu; q closes it"
    (efrit-testdrive--ask "In the agent buffer press C-c ?.  A menu with Turn / Tool rows / View / Session columns, each entry showing its key?  Press q: does it close?"))
  (efrit-testdrive--step 'tour "C-c C-m opens the efrit menu; q closes it"
    (efrit-testdrive--ask "Press C-c C-m.  The efrit menu with model, sandbox, diagnostics?  q closes it?")))

(defun efrit-testdrive--tour-queue ()
  (efrit-testdrive--out "\n## Queue view")
  (efrit-testdrive--step 'tour "C-c C-q lists queued inputs and drops one"
    ;; A pretend busy state: nothing runs, so there is no race
    (let ((session (efrit-testdrive--session)))
      (efrit-repl-loop-hold session)
      (with-current-buffer (efrit-testdrive--agent-buffer)
        (efrit-agent-busy-submit-queue "first queued")
        (efrit-agent-busy-submit-queue "second queued"))
      (unwind-protect
          (progn
            (efrit-testdrive--confirm "Two lines marked ⋯ are queued (the session is held busy for this step).  In the agent buffer's input press C-c C-q and drop `first queued'.  Then RET here.")
            (let ((q (copy-sequence (efrit-repl-session-queue session))))
              (efrit-testdrive--check (equal q '("second queued"))
                                      (format "queue after your drop: %S (expected (\"second queued\"))" q))))
        (while (efrit-repl-session-dequeue session))
        (with-current-buffer (efrit-testdrive--agent-buffer)
          (efrit-agent--unmark-queued-message "second queued" 'dropped))
        (efrit-repl-loop-release session)))))

(defun efrit-testdrive--tour-sandbox ()
  (efrit-testdrive--out "\n## Sandbox prompt")
  (efrit-testdrive--step 'tour "A read outside the project asks, and NO is respected"
    (efrit-testdrive--after-confirm
        (format "Next turn asks the model to read %s, outside the project.  A sandbox prompt will appear: answer n (no).  Ready?"
                (efrit-testdrive--outside-file))
      (efrit-testdrive--clear-events)
      (let ((ev (efrit-testdrive--turn
                 (format "Read the file %s and tell me its first line. If you cannot, say CANNOT and stop."
                         (efrit-testdrive--outside-file)))))
        (cond
         ((not ev) (cons 'FAIL "timed out"))
         ((null (efrit-testdrive--events-of 'sandbox-denied)) (cons 'FAIL "no denial recorded: did you answer no?"))
         (t (efrit-testdrive--ask "Did the prompt name the file and the tool, and did the model say CANNOT (or similar) afterwards?")))))))

(defun efrit-testdrive--tour-drop ()
  (efrit-testdrive--out "\n## Drag and drop")
  (efrit-testdrive--step 'tour "A file dragged onto the buffer becomes a mention"
    (efrit-testdrive--after-confirm
        "Drag any file from your file manager (a screenshot works) onto the agent buffer, then RET here."
      (let ((input (with-current-buffer (efrit-testdrive--agent-buffer) (efrit-agent--get-input))))
        (efrit-testdrive--type-input "")
        (efrit-testdrive--check (string-match-p "@" input)
                                (format "input after the drop: %S" (truncate-string-to-width input 80 nil nil "…")))))))

(defun efrit-testdrive--tour-images ()
  (efrit-testdrive--out "\n## Images and the transcript file")
  (efrit-testdrive--step 'tour "+ and - resize the picture in the last answer; = resets"
    (if (not (display-graphic-p))
        (cons 'SKIP "text display")
      (efrit-testdrive--ask "Put point on the red square in the last answer.  Press + twice: does it grow?  - once: smaller?  = back to normal?  (C-c + / C-c - / C-c = work from the input too.)")))
  (efrit-testdrive--step 'tour "C-c C-f opens the session transcript as readable Markdown"
    (efrit-testdrive--ask "Press C-c C-f in the agent buffer.  A Markdown file with `## HH:MM:SS You` headings, tool calls in fences and the answers?  q closes it.")))

(defconst efrit-testdrive--tour-stops
  '(("Header" efrit-testdrive--tour-header)
    ("Folding" efrit-testdrive--tour-folding)
    ("Menus" efrit-testdrive--tour-menu)
    ("Queue view" efrit-testdrive--tour-queue)
    ("Sandbox prompt" efrit-testdrive--tour-sandbox)
    ("Drag and drop" efrit-testdrive--tour-drop)
    ("Images and the transcript file" efrit-testdrive--tour-images))
  "The tour's stops: (TITLE FUNCTION).")

;;;; Driver

(defun efrit-testdrive--summary ()
  "Write the summary at the top of the report."
  (let* ((rs (reverse efrit-testdrive--results))
         (count (lambda (st) (cl-count st rs :key #'caddr)))
         (lines
          (append
           (list (format "**%d PASS, %d FAIL, %d SKIP** in %.0fs, %d model turn(s)"
                         (funcall count 'PASS) (funcall count 'FAIL) (funcall count 'SKIP)
                         (apply #'+ (mapcar (lambda (r) (or (nth 4 r) 0)) rs))
                         efrit-testdrive--turns))
           (cl-loop for r in rs when (eq (caddr r) 'FAIL)
                    collect (format "- FAIL [%s] %s%s" (car r) (cadr r)
                                    (if (nth 3 r) (concat ": " (nth 3 r)) "")))
           (cl-loop for r in rs when (and (nth 4 r) (> (nth 4 r) efrit-testdrive-step-budget))
                    collect (format "- SLOW [%s] %s: %.0fs" (car r) (cadr r) (nth 4 r))))))
    (with-current-buffer (efrit-testdrive--buf)
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (or (and efrit-testdrive--summary-marker
                              (marker-position efrit-testdrive--summary-marker))
                         (point-max)))
          (let ((start (point)))
            (insert "\n## Summary\n\n" (string-join lines "\n") "\n")
            (efrit-testdrive--render start (point))))))))

(defun efrit-testdrive--cleanup ()
  "Remove the throwaway project and the session grants made on it."
  (when efrit-testdrive--root
    (efrit-sandbox-reset-session efrit-testdrive--root)
    (when (file-directory-p efrit-testdrive--root)
      (delete-directory efrit-testdrive--root t))
    (efrit-testdrive--out "\nCleaned up: %s removed, its session grants forgotten." efrit-testdrive--root)))

(defun efrit-testdrive--begin (title turns)
  "Start a report for TITLE, ask consent for TURNS model turns, make the project.
Signals `user-error' when declined."
  (setq efrit-testdrive--results nil
        efrit-testdrive--turns 0
        efrit-testdrive--events nil)
  (with-current-buffer (efrit-testdrive--buf)
    (let ((inhibit-read-only t)) (erase-buffer)))
  (efrit-testdrive--show-report)
  (setq efrit-testdrive--layout (current-window-configuration))
  (efrit-testdrive--out "# %s\n\n%s. Emacs %s, model %s, streaming %S, review %S, sandbox %S"
                        title (format-time-string "%F %T") emacs-version efrit-default-model
                        (bound-and-true-p efrit-api-streaming) efrit-review-enabled efrit-sandbox-enabled)
  (with-current-buffer (efrit-testdrive--buf)
    (setq efrit-testdrive--summary-marker (copy-marker (point-max))))
  (unless (yes-or-no-p
           (format "Run %s?  It creates a throwaway project under %s, opens the agent buffer, and sends about %d short turns to %s (costs tokens).  Nothing outside that project is changed. "
                   title (abbreviate-file-name temporary-file-directory) turns efrit-default-model))
    (efrit-testdrive--out "\n(declined; nothing was sent)")
    (user-error "Test drive declined"))
  (setq efrit-testdrive--root (efrit-testdrive--make-project))
  (efrit-testdrive--out "Throwaway project: %s" efrit-testdrive--root))

(defun efrit-testdrive--run (parts &optional unattended)
  "Run PARTS, each (TITLE FUNCTION), with the project bound; then wrap up.
With UNATTENDED, every sandbox request the steps did not grant ahead
is refused instead of prompting (see `efrit-testdrive--refuse')."
  (efrit-subscribe t #'efrit-testdrive--on-event)
  (setq efrit-testdrive--unanswered nil)
  (let ((efrit-project-root efrit-testdrive--root)
        (default-directory efrit-testdrive--root)
        (efrit-sandbox-request-function (if unattended #'efrit-testdrive--refuse
                                          efrit-sandbox-request-function)))
    (unwind-protect
        (condition-case nil
            (dolist (p parts)
              (efrit-testdrive--out "\n---")
              (funcall (cadr p)))
          (efrit-testdrive-quit
           (efrit-testdrive--out "\n(stopped by user)")))
      (efrit-unsubscribe t #'efrit-testdrive--on-event)
      (when efrit-testdrive--unanswered
        ;; Inside the project: a step forgot a grant (drive bug).
        ;; Outside: the model reached beyond the project and the
        ;; sandbox stopped it -- the sandbox working, worth knowing.
        (let* ((inside (lambda (r)
                         (let ((tgt (efrit-sandbox-request-target r)))
                           (and (stringp tgt)
                                (string-prefix-p efrit-testdrive--root (efrit-sandbox-canonical tgt))))))
               (drive-bugs (cl-remove-if-not inside efrit-testdrive--unanswered))
               (reaches (cl-remove-if inside efrit-testdrive--unanswered))
               (line (lambda (r) (format "- %s %s by %s: %s"
                                         (efrit-sandbox-request-cap r)
                                         (efrit-sandbox-request-target r)
                                         (efrit-sandbox-request-tool r)
                                         (efrit-sandbox-request-detail r)))))
          (when drive-bugs
            (efrit-testdrive--out "\n%d request(s) inside the project were refused unattended -- a step did not pre-grant what its turn needed (drive bug):\n%s"
                                  (length drive-bugs) (mapconcat line (reverse drive-bugs) "\n")))
          (when reaches
            (efrit-testdrive--out "\nThe model reached outside the project %d time(s) and the sandbox refused (as it should):\n%s"
                                  (length reaches) (mapconcat line (reverse reaches) "\n")))))
      (ignore-errors (efrit-testdrive--cleanup))
      (efrit-testdrive--summary)
      (pop-to-buffer (efrit-testdrive--buf))
      (goto-char (point-min))
      (let* ((rs efrit-testdrive--results)
             (count (lambda (st) (cl-count st rs :key #'caddr))))
        (message "efrit test drive finished: %d PASS, %d FAIL, %d SKIP.  The report is in %s."
                 (funcall count 'PASS) (funcall count 'FAIL) (funcall count 'SKIP)
                 (buffer-name (efrit-testdrive--buf)))))))

(defun efrit-testdrive--pick (parts prompt)
  "Let the user pick one of PARTS ((TITLE FUNCTION) ...)."
  (let* ((names (mapcar #'car parts))
         (pick (completing-read prompt names nil t)))
    (list (assoc pick parts))))

;;;###autoload
(defun efrit-testdrive (&optional one-section)
  "Run efrit's automatic live test drive; with ONE-SECTION, one chosen section.
About a dozen short model turns in a throwaway project, every outcome
checked by the drive itself; nothing asks you anything after the
consent.  See the Commentary for the safety model.  For the checks
that need eyes, see `efrit-testdrive-tour'."
  (interactive "P")
  (efrit-testdrive--begin "the efrit test drive" 12)
  (efrit-testdrive--run
   (let ((parts (mapcar (lambda (s) (list (format "%s. %s" (car s) (cadr s)) (caddr s)))
                        efrit-testdrive--sections)))
     (if one-section (efrit-testdrive--pick parts "Section: ") parts))
   'unattended))

;;;###autoload
(defun efrit-testdrive-tour (&optional one-stop)
  "Walk through what only eyes can check in the agent buffer; with ONE-STOP, one stop.
Each stop sets one thing up with nothing running underneath, shows the
agent buffer, and asks whether it looked right.  One model turn (the
sandbox prompt); the rest is local."
  (interactive "P")
  (efrit-testdrive--begin "the efrit tour" 1)
  (require 'efrit-agent)
  (let ((default-directory efrit-testdrive--root))
    (save-window-excursion (call-interactively #'efrit)))
  (with-current-buffer (efrit-testdrive--agent-buffer)
    (setq default-directory efrit-testdrive--root))
  (efrit-testdrive--run
   (if one-stop (efrit-testdrive--pick efrit-testdrive--tour-stops "Stop: ")
     efrit-testdrive--tour-stops)))

(provide 'efrit-testdrive)

;;; efrit-testdrive.el ends here
