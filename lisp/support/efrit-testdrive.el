;;; efrit-testdrive.el --- Guided live test drive of efrit -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; `M-x efrit-testdrive' walks through efrit's live test plan against
;; the real API and the real agent buffer: it does the mechanical
;; parts itself and asks you to confirm what needs eyes (did the
;; header render?  did the sandbox prompt look right?).  It is the
;; check to run after an upgrade, a model change, or a proxy change,
;; when the unit tests pass but the question is "does it work here".
;;
;; Safety model
;; ------------
;; Everything the model is asked to touch lives in a throwaway project
;; the drive creates under `temporary-file-directory' and deletes at
;; the end.  The drive never grants the model anything outside that
;; directory; the tests that ask the model to reach outside expect a
;; sandbox prompt, and you answer it (the expected answer is "no").
;; Session grants and the turn limits are restored when the drive
;; ends.  Your own projects, files and settings are not touched.
;;
;; Consent is asked once up front: the drive costs tokens (a few
;; short turns; the count is shown) and opens the agent buffer.
;; Without consent nothing is sent.
;;
;; Results accumulate in *efrit-testdrive* as an Org document you can
;; paste into a bug report: one heading per step with PASS / FAIL /
;; SKIP, notes, timings, and the tail of *efrit-log* for failures.
;;
;; Answers at each step: y = pass, n = fail (with a note), s = skip,
;; q = quit (results so far are kept).  `C-u M-x efrit-testdrive'
;; runs one chosen section.

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
(declare-function efrit-submit "efrit-agent-input")
(declare-function efrit-agent-cancel "efrit-agent")
(declare-function efrit-doctor "efrit-doctor")
(declare-function efrit "efrit")
(declare-function efrit-menu "efrit-menu")
(declare-function efrit-sandbox "efrit-permissions-ui")
(declare-function efrit-common-get-api-key "efrit-common")
(declare-function efrit-common-get-api-url "efrit-common")

(defgroup efrit-testdrive nil
  "The guided live test drive."
  :group 'efrit
  :prefix "efrit-testdrive-")

(defcustom efrit-testdrive-turn-timeout 120
  "Seconds to wait for one model turn before the step fails."
  :type 'integer
  :group 'efrit-testdrive)

(defcustom efrit-testdrive-step-budget 60
  "Seconds a step may take before the report flags it as SLOW.
The whole drive is meant to finish in about ten minutes; a slow step
is a finding in itself."
  :type 'integer
  :group 'efrit-testdrive)

;;;; State

(defvar efrit-testdrive--root nil "The throwaway project directory.")
(defvar efrit-testdrive--results nil "List of (SECTION NAME STATUS NOTE SECONDS).")
(defvar efrit-testdrive--buffer "*efrit-testdrive*")
(defvar efrit-testdrive--layout nil
  "Window configuration at the start of the drive, restored after every step.")
(defvar efrit-testdrive--events nil
  "Events published since the last `efrit-testdrive--clear-events', newest first.")
(defvar efrit-testdrive--turns 0 "Model turns sent so far.")

(define-error 'efrit-testdrive-quit "test drive stopped")

;;;; Reporting

(defun efrit-testdrive--buf ()
  (let ((buf (get-buffer-create efrit-testdrive--buffer)))
    (with-current-buffer buf
      (unless (derived-mode-p 'org-mode)
        (ignore-errors (org-mode))))
    buf))

(defun efrit-testdrive--show-report ()
  "Keep the report visible in a side window without taking focus."
  (let ((buf (efrit-testdrive--buf)))
    (unless (get-buffer-window buf)
      (display-buffer buf '(display-buffer-in-side-window (side . right) (window-width . 0.4))))
    (when-let* ((w (get-buffer-window buf)))
      (with-current-buffer buf (set-window-point w (point-max))))))

(defun efrit-testdrive--restore-layout ()
  (when (and efrit-testdrive--layout (window-configuration-p efrit-testdrive--layout))
    (set-window-configuration efrit-testdrive--layout))
  (efrit-testdrive--show-report))

(defun efrit-testdrive--out (fmt &rest args)
  "Append FMT/ARGS to the report and keep its end visible."
  (with-current-buffer (efrit-testdrive--buf)
    (goto-char (point-max))
    (insert (apply #'format fmt args) "\n")
    (when-let* ((w (get-buffer-window (current-buffer))))
      (set-window-point w (point-max)))))

(defun efrit-testdrive--log-tail (&optional n)
  (when-let* ((buf (get-buffer "*efrit-log*")))
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-max))
        (forward-line (- (or n 15)))
        (buffer-substring-no-properties (point) (point-max))))))

(defun efrit-testdrive--record (section name status &optional note secs)
  (push (list section name status note secs) efrit-testdrive--results)
  (efrit-testdrive--out "** %s %s%s" status name (if secs (format "  (%.1fs)" secs) ""))
  (when (and note (not (string-empty-p note)))
    (efrit-testdrive--out "   %s" note))
  (when (eq status 'FAIL)
    (when-let* ((tail (efrit-testdrive--log-tail)))
      (efrit-testdrive--out "#+begin_example\n%s#+end_example" tail))))

;;;; Interaction

(defvar efrit-testdrive--waited 0
  "Seconds the current step spent waiting for the user; not the step's time.")

(defun efrit-testdrive--read-char (prompt choices)
  (let ((t0 (float-time)))
    (unwind-protect (read-char-choice prompt choices)
      (cl-incf efrit-testdrive--waited (- (float-time) t0)))))

(defun efrit-testdrive--ask (prompt)
  "Ask PROMPT; return PASS, FAIL (with a note) or SKIP; q signals quit."
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
  (let ((c (efrit-testdrive--read-char (concat prompt "  [RET/y] done, [s]kip, [q]uit ") '(?y ?\r ?s ?q))))
    (pcase c
      ((or ?y ?\r) t)
      (?s nil)
      (_ (signal 'efrit-testdrive-quit nil)))))

(defmacro efrit-testdrive--step (section name &rest body)
  "Run BODY as step NAME in SECTION, recording outcome and timing.
BODY returns PASS/FAIL/SKIP or (STATUS . NOTE).  Errors become FAIL."
  (declare (indent 2))
  `(let ((t0 (float-time))
         (efrit-testdrive--waited 0))
     (efrit-testdrive--out "\n*** %s" ,name)
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

(defun efrit-testdrive--make-project ()
  "Create the throwaway project; return its directory."
  (let ((dir (file-name-as-directory (make-temp-file "efrit-testdrive-" t))))
    (dolist (f efrit-testdrive--files)
      (with-temp-file (expand-file-name (car f) dir) (insert (cdr f))))
    dir))

(defun efrit-testdrive--file (rel)
  (expand-file-name rel efrit-testdrive--root))

(defun efrit-testdrive--file-text (rel)
  (let ((f (efrit-testdrive--file rel)))
    (and (file-exists-p f)
         (with-temp-buffer (insert-file-contents f) (buffer-string)))))

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

(defun efrit-testdrive--wait-for (pred &optional timeout what)
  "Run the event loop until PRED is non-nil or TIMEOUT seconds pass.
Returns PRED's value.  WHAT names the wait in the echo area, so a slow
model is visible."
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

(defun efrit-testdrive--turn (shown &optional api-input)
  "Send SHOWN (and API-INPUT) as a turn; wait for it to complete.
Returns the `turn-complete' event, or nil on timeout (the turn is
then cancelled so the next step starts clean)."
  (require 'efrit-agent-input)
  (efrit-testdrive--clear-events)
  (cl-incf efrit-testdrive--turns)
  (let ((efrit-project-root efrit-testdrive--root)
        (default-directory efrit-testdrive--root))
    (unless (efrit-submit shown api-input)
      (error "The agent buffer is busy; the turn was not sent"))
    (let ((done (efrit-testdrive--wait-for
                 (lambda () (car (efrit-testdrive--events-of 'turn-complete)))
                 nil "the turn to complete")))
      (unless done
        (with-current-buffer (efrit-testdrive--agent-buffer)
          (ignore-errors (efrit-agent-cancel))))
      done)))

(defun efrit-testdrive--reply-text ()
  "The assistant text of the last turn, from the text-delta events."
  (mapconcat (lambda (e) (or (alist-get :text e) ""))
             (efrit-testdrive--events-of 'text-delta) ""))

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

;;;; Sections

(defun efrit-testdrive--section-0 ()
  "Setup sanity: doctor, key, model, agent buffer, header."
  (efrit-testdrive--out "\n* 0. Setup sanity")
  (efrit-testdrive--step 0 "efrit-doctor reports no failure"
    (require 'efrit-doctor)
    (let ((ok (save-window-excursion (efrit-doctor))))
      (efrit-testdrive--check ok "efrit-doctor found problems; see *efrit-doctor*")))
  (efrit-testdrive--step 0 "API key and endpoint resolve"
    (require 'efrit-common)
    (let ((key (ignore-errors (efrit-common-get-api-key)))
          (url (ignore-errors (efrit-common-get-api-url))))
      (if (and key url)
          (cons 'PASS (format "endpoint %s, model %s" url efrit-default-model))
        (cons 'FAIL "no key or no endpoint"))))
  (efrit-testdrive--step 0 "Agent buffer opens with its header"
    (require 'efrit-agent)
    (let ((default-directory efrit-testdrive--root))
      (call-interactively #'efrit))
    (efrit-testdrive--ask "Is the efrit header (logo, model, status) visible at the top of the agent buffer?")))

(defun efrit-testdrive--section-1 ()
  "One round trip and the transcript."
  (efrit-testdrive--out "\n* 1. A round trip")
  (efrit-testdrive--step 1 "A plain question is answered"
    (let ((ev (efrit-testdrive--turn "Reply with exactly the word PONG and nothing else.")))
      (let ((text (efrit-testdrive--reply-text)))
        (if (and ev (string-match-p "PONG" text))
            (cons 'PASS (efrit-testdrive--turn-note ev))
          (cons 'FAIL (format "%s; reply: %s" (efrit-testdrive--turn-note ev)
                              (truncate-string-to-width text 120 nil nil "…")))))))
  (efrit-testdrive--step 1 "The transcript shows the turn"
    (efrit-testdrive--ask "In the agent buffer: is your PONG request shown as your turn, the answer below it, and the input prompt ready again?"))
  (efrit-testdrive--step 1 "Usage is shown after the turn"
    (efrit-testdrive--ask "Does the header (or mode line) show token usage for the session now?")))

(defun efrit-testdrive--section-2 ()
  "Reading the project: tools inside the sandbox run without asking."
  (efrit-testdrive--out "\n* 2. Reading the project")
  (efrit-testdrive--step 2 "The model reads a project file without a prompt"
    (efrit-testdrive--clear-events)
    (let ((ev (efrit-testdrive--turn "Read notes.txt in this project and tell me the secret word. Answer with the word only.")))
      (let ((text (efrit-testdrive--reply-text))
            (denied (efrit-testdrive--events-of 'sandbox-denied)))
        (cond
         ((not ev) (cons 'FAIL "timed out"))
         (denied (cons 'FAIL "a read inside the project was denied or prompted"))
         ((string-match-p "PELICAN" text) (cons 'PASS (efrit-testdrive--turn-note ev)))
         (t (cons 'FAIL (format "%s; reply: %s" (efrit-testdrive--turn-note ev)
                                (truncate-string-to-width text 120 nil nil "…"))))))))
  (efrit-testdrive--step 2 "Tool calls are shown as collapsed rows"
    (efrit-testdrive--ask "Is the read shown as a tool row in the transcript, and does RET on it expand the output?")))

(defun efrit-testdrive--section-3 ()
  "Writing: the sandbox asks for write, the review judges the edit."
  (efrit-testdrive--out "\n* 3. Writing inside the project")
  (efrit-testdrive--step 3 "A write asks for consent and, granted, lands"
    (efrit-testdrive--after-confirm
        "Next turn asks the model to edit greet.el.  Expect a sandbox prompt for WRITE on the project: answer for this SESSION.  Ready?"
      (let ((ev (efrit-testdrive--turn "In greet.el, change the greeting from \"Hello, %s!\" to \"Howdy, %s!\". Edit the file, then stop.")))
      (let ((text (efrit-testdrive--file-text "greet.el")))
        (cond
         ((not ev) (cons 'FAIL "timed out"))
         ((and text (string-match-p "Howdy, %s!" text))
          (cons 'PASS (efrit-testdrive--turn-note ev)))
         (t (cons 'FAIL (format "greet.el not changed; %s" (efrit-testdrive--turn-note ev)))))))))
  (efrit-testdrive--step 3 "The write grant is listed"
    (let ((grants (efrit-sandbox-grants efrit-testdrive--root)))
      (efrit-testdrive--check
       (cl-some (lambda (g) (eq (plist-get g :cap) 'write)) grants)
       (format "no write grant for the project; grants: %S" grants))))
  (efrit-testdrive--step 3 "The review verdict is visible"
    (if (not efrit-review-enabled)
        (cons 'SKIP "efrit-review-enabled is nil")
      (let ((verdicts (efrit-testdrive--events-of 'review-verdict)))
        (if (null verdicts)
            (cons 'FAIL "no review-verdict event for the write turn")
          (efrit-testdrive--ask
           (format "The reviewer said %s.  Is the review row shown in the transcript before the edit?"
                   (alist-get :verdict (car verdicts)))))))))

(defun efrit-testdrive--section-4 ()
  "Escaping the project: the sandbox refuses, the turn continues."
  (efrit-testdrive--out "\n* 4. The sandbox boundary")
  (efrit-testdrive--step 4 "A read outside the project is refused and the turn goes on"
    (efrit-testdrive--after-confirm
        "Next turn asks the model to read your ~/.emacs.d/init.el (or ~/.emacs).  Expect a sandbox prompt: answer NO (n or q).  Ready?"
      (let ((ev (efrit-testdrive--turn "Read the file ~/.emacs.d/init.el and tell me its first line. If you cannot, say CANNOT and stop.")))
      (let ((denied (efrit-testdrive--events-of 'sandbox-denied)))
        (cond
         ((not ev) (cons 'FAIL "timed out"))
         ((null denied) (cons 'FAIL (format "no sandbox denial recorded; %s" (efrit-testdrive--turn-note ev))))
         ((member (efrit-testdrive--stop-reason ev) '("end_turn" "session-complete"))
          (cons 'PASS (efrit-testdrive--turn-note ev)))
         (t (cons 'FAIL (format "the turn ended with %s, not a normal answer" (efrit-testdrive--stop-reason ev)))))))))
  (efrit-testdrive--step 4 "No grant leaked outside the project"
    (let ((outside (cl-remove-if
                    (lambda (g) (or (not (stringp (plist-get g :target)))
                                    (string-prefix-p efrit-testdrive--root (plist-get g :target))))
                    (efrit-sandbox-grants efrit-testdrive--root))))
      (efrit-testdrive--check (null outside) (format "grants outside the project: %S" outside))))
  (efrit-testdrive--step 4 "eval_sexp cannot touch the sandbox"
    (let ((ev (efrit-testdrive--turn "Use eval_sexp to evaluate (setq efrit-sandbox-enabled nil) and report the result verbatim.")))
      (cond
       ((not ev) (cons 'FAIL "timed out"))
       ((not efrit-sandbox-enabled) (cons 'FAIL "efrit-sandbox-enabled is now nil"))
       (t (cons 'PASS (efrit-testdrive--turn-note ev)))))))

(defun efrit-testdrive--section-5 ()
  "Shell: per-command grants."
  (efrit-testdrive--out "\n* 5. Shell commands")
  (efrit-testdrive--step 5 "A shell command asks per command"
    (efrit-testdrive--after-confirm
        "Next turn asks the model to run `ls` in the project.  Expect a sandbox prompt naming the command: grant it for this SESSION.  Ready?"
      (let ((ev (efrit-testdrive--turn "Run the shell command `ls` in the project directory and list the file names it printed.")))
      (let ((text (efrit-testdrive--reply-text)))
        (cond
         ((not ev) (cons 'FAIL "timed out"))
         ((and (string-match-p "greet.el" text) (string-match-p "notes.txt" text))
          (cons 'PASS (efrit-testdrive--turn-note ev)))
         (t (cons 'FAIL (format "%s; reply: %s" (efrit-testdrive--turn-note ev)
                                (truncate-string-to-width text 120 nil nil "…")))))))))
  (efrit-testdrive--step 5 "The shell grant names the command"
    (let ((shell (cl-find 'shell (efrit-sandbox-grants efrit-testdrive--root)
                          :key (lambda (g) (plist-get g :cap)))))
      (cond
       ((null shell) (cons 'SKIP "no shell grant recorded"))
       ((eq (plist-get shell :target) t) (cons 'FAIL "the grant is a blanket shell grant, not per command"))
       (t (cons 'PASS (format "target %S" (plist-get shell :target))))))))

(defun efrit-testdrive--section-6 ()
  "Interaction: a question from the model, cancel, multiline input."
  (efrit-testdrive--out "\n* 6. Interaction")
  (efrit-testdrive--step 6 "The model's question pauses the turn"
    (let ((ev (efrit-testdrive--turn "Use request_user_input to ask me which colour I prefer, with the options red and blue. Wait for my answer, then repeat it back.")))
      (cond
       ((not ev) (cons 'FAIL "timed out"))
       ((equal (efrit-testdrive--stop-reason ev) "waiting-for-user")
        (unless (efrit-testdrive--confirm "The model asked a question.  Answer it in the agent buffer (pick blue), then press RET here.")
          (cons 'SKIP "not answered"))
        (let ((done (efrit-testdrive--wait-for
                     (lambda () (cl-find "end_turn" (efrit-testdrive--events-of 'turn-complete)
                                         :key (lambda (e) (alist-get :stop-reason e)) :test #'equal))
                     nil "the answer to be repeated")))
          (if (and done (string-match-p "blue" (downcase (efrit-testdrive--reply-text))))
              'PASS
            (cons 'FAIL "the answer was not repeated back"))))
       (t (cons 'FAIL (format "the turn did not pause: %s" (efrit-testdrive--turn-note ev)))))))
  (efrit-testdrive--step 6 "Multiline input: S-RET newline, RET sends"
    (efrit-testdrive--confirm "In the agent buffer type two lines with S-RET between them, then RET.  Did RET send both lines as one turn?  (Wait for the reply.)")
    (efrit-testdrive--ask "Were both lines shown as one turn, and was RET the only key that sent?"))
  (efrit-testdrive--step 6 "Cancel stops a turn"
    (efrit-testdrive--clear-events)
    (let ((efrit-project-root efrit-testdrive--root)
          (default-directory efrit-testdrive--root))
      (efrit-submit "Count slowly from 1 to 200, one number per line, using eval_sexp with (sleep-for 1) between each.")
      (cl-incf efrit-testdrive--turns))
    (efrit-testdrive--wait-for (lambda () (efrit-testdrive--events-of 'tool-start)) 30 "the first tool call")
    (with-current-buffer (efrit-testdrive--agent-buffer) (efrit-agent-cancel))
    (let ((ev (efrit-testdrive--wait-for
               (lambda () (car (efrit-testdrive--events-of 'turn-complete))) 30 "the cancel to land")))
      (efrit-testdrive--ask
       (format "The turn was cancelled (stop %s).  Is the agent buffer idle again, with the input prompt usable?"
               (or (efrit-testdrive--stop-reason ev) "not reported"))))))

(defun efrit-testdrive--section-7 ()
  "Configuration surfaces."
  (efrit-testdrive--out "\n* 7. Configuration surfaces")
  (efrit-testdrive--step 7 "The menu opens"
    (require 'efrit-menu)
    (call-interactively #'efrit-menu)
    (efrit-testdrive--ask "Did the efrit menu (transient) open with model, sandbox, review and diagnostics entries?  Dismiss it with q."))
  (efrit-testdrive--step 7 "The permissions editor lists the drive's grants"
    (require 'efrit-permissions-ui)
    (let ((efrit-project-root efrit-testdrive--root)
          (default-directory efrit-testdrive--root))
      (call-interactively #'efrit-sandbox))
    (efrit-testdrive--ask "Does M-x efrit-sandbox list the session grants made during the drive (write, shell) for the throwaway project?  Close it with q."))
  (efrit-testdrive--step 7 "The log has the API lines"
    (require 'efrit-log)
    (efrit-testdrive--ask "Open *efrit-log* (efrit-menu l).  Are there `api →' and `api ←' lines for the turns just made?")))

(defconst efrit-testdrive--sections
  '((0 "Setup sanity" efrit-testdrive--section-0)
    (1 "A round trip" efrit-testdrive--section-1)
    (2 "Reading the project" efrit-testdrive--section-2)
    (3 "Writing inside the project" efrit-testdrive--section-3)
    (4 "The sandbox boundary" efrit-testdrive--section-4)
    (5 "Shell commands" efrit-testdrive--section-5)
    (6 "Interaction" efrit-testdrive--section-6)
    (7 "Configuration surfaces" efrit-testdrive--section-7)))

;;;; Driver

(defun efrit-testdrive--summary ()
  (let* ((rs (reverse efrit-testdrive--results))
         (count (lambda (st) (cl-count st rs :key #'caddr))))
    (efrit-testdrive--out "\n* Summary: %d PASS, %d FAIL, %d SKIP in %.0fs, %d model turn(s)"
                          (funcall count 'PASS) (funcall count 'FAIL) (funcall count 'SKIP)
                          (apply #'+ (mapcar (lambda (r) (or (nth 4 r) 0)) rs))
                          efrit-testdrive--turns)
    (dolist (r rs)
      (when (and (nth 4 r) (> (nth 4 r) efrit-testdrive-step-budget))
        (efrit-testdrive--out "- SLOW [%s] %s: %.0fs" (car r) (cadr r) (nth 4 r))))
    (dolist (r rs)
      (when (eq (caddr r) 'FAIL)
        (efrit-testdrive--out "- FAIL [%s] %s%s" (car r) (cadr r)
                              (if (nth 3 r) (concat ": " (nth 3 r)) ""))))
    (efrit-testdrive--out "\nThis buffer holds no file contents beyond the throwaway project's and can be pasted into a bug report.")))

(defun efrit-testdrive--cleanup ()
  "Remove the throwaway project and the session grants made on it."
  (when efrit-testdrive--root
    (efrit-sandbox-reset-session efrit-testdrive--root)
    (when (file-directory-p efrit-testdrive--root)
      (delete-directory efrit-testdrive--root t))
    (efrit-testdrive--out "\nCleaned up: %s removed, its session grants forgotten." efrit-testdrive--root)))

;;;###autoload
(defun efrit-testdrive (&optional one-section)
  "Walk through efrit's live test plan interactively.
With prefix ONE-SECTION, run just one chosen section.

The drive creates a throwaway project, opens the agent buffer, and
sends a handful of short turns to the configured model (it costs
tokens).  Everything the model is asked to change lives in that
project, which is deleted at the end together with the session
grants made on it.  You are asked once before anything is sent."
  (interactive "P")
  (setq efrit-testdrive--results nil
        efrit-testdrive--turns 0
        efrit-testdrive--events nil)
  (with-current-buffer (efrit-testdrive--buf) (erase-buffer))
  (efrit-testdrive--show-report)
  (setq efrit-testdrive--layout (current-window-configuration))
  (efrit-testdrive--out "#+TITLE: efrit test drive\n#+DATE: %s\nEmacs %s, model %s, streaming %S, review %S, sandbox %S"
                        (format-time-string "%F %T") emacs-version efrit-default-model
                        (bound-and-true-p efrit-api-streaming) efrit-review-enabled efrit-sandbox-enabled)
  (unless (yes-or-no-p
           (format "Run the efrit test drive?  It creates a throwaway project under %s, opens the agent buffer, and sends about %d short turns to %s (costs tokens).  Nothing outside that project is changed. "
                   (abbreviate-file-name temporary-file-directory) 10 efrit-default-model))
    (efrit-testdrive--out "\n(declined; nothing was sent)")
    (user-error "Test drive declined"))
  (setq efrit-testdrive--root (efrit-testdrive--make-project))
  (efrit-testdrive--out "Throwaway project: %s" efrit-testdrive--root)
  (efrit-subscribe t #'efrit-testdrive--on-event)
  (let ((sections (if one-section
                      (let* ((names (mapcar (lambda (s) (format "%s. %s" (car s) (cadr s))) efrit-testdrive--sections))
                             (pick (completing-read "Section: " names nil t)))
                        (list (nth (cl-position pick names :test #'equal) efrit-testdrive--sections)))
                    efrit-testdrive--sections)))
    (unwind-protect
        (condition-case nil
            (dolist (s sections)
              (pcase-let ((`(,n ,title ,fn) s))
                (when (or one-section
                          (efrit-testdrive--confirm (format "Run section %s: %s?" n title)))
                  (funcall fn))))
          (efrit-testdrive-quit
           (efrit-testdrive--out "\n(stopped by user)")))
      (efrit-unsubscribe t #'efrit-testdrive--on-event)
      (ignore-errors (efrit-testdrive--cleanup))
      (efrit-testdrive--summary)
      (pop-to-buffer (efrit-testdrive--buf))
      (goto-char (point-min))
      (let* ((rs efrit-testdrive--results)
             (count (lambda (st) (cl-count st rs :key #'caddr))))
        (message "efrit test drive finished: %d PASS, %d FAIL, %d SKIP.  The report is in %s."
                 (funcall count 'PASS) (funcall count 'FAIL) (funcall count 'SKIP)
                 (buffer-name (efrit-testdrive--buf)))))))

(provide 'efrit-testdrive)

;;; efrit-testdrive.el ends here
