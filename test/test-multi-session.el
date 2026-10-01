;;; test-multi-session.el --- two agent buffers, two sessions -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'efrit-agent)
(require 'efrit-agent-input)
(require 'efrit-repl-session)
(require 'efrit-events)
(require 'efrit-api-stream)

(defun test-ms--agent-buffer (name root)
  "A fresh agent buffer NAME whose REPL session has ROOT."
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t)) (erase-buffer))
      (efrit-agent-mode)
      (efrit-agent--init-regions)
      (efrit-agent--setup-regions)
      (setq default-directory root)
      (setq efrit-agent--repl-session (efrit-repl-session-create root))
      (setf (efrit-repl-session-buffer efrit-agent--repl-session) buf))
    buf))

(defmacro test-ms--with-two (a b &rest body)
  "BODY with A and B bound to two agent buffers with their own sessions."
  (declare (indent 2))
  `(let* ((efrit-agent-auto-show nil)
          (root-b (file-name-as-directory (make-temp-file "efrit-ms-b-" t)))
          (,a (test-ms--agent-buffer " *ms-a*" temporary-file-directory))
          (,b (test-ms--agent-buffer " *ms-b*" root-b)))
     (unwind-protect (progn ,@body)
       (kill-buffer ,a) (kill-buffer ,b))))

(defun test-ms--session-id (buf)
  (efrit-repl-session-id (buffer-local-value 'efrit-agent--repl-session buf)))

(defun test-ms--text (buf)
  (with-current-buffer buf (buffer-substring-no-properties (point-min) (point-max))))

(ert-deftest test-ms-events-render-in-their-sessions-buffer ()
  "text-delta, tool rows, notes, status and questions land in the
buffer of the event's session, not in the default agent buffer."
  (test-ms--with-two a b
    (let ((ida (test-ms--session-id a)) (idb (test-ms--session-id b)))
      (should (eq a (efrit-agent-buffer-for ida)))
      (should (eq b (efrit-agent-buffer-for idb)))
      (efrit-publish 'text-delta `((:session-id . ,ida) (:text . "alpha says hi")))
      (efrit-publish 'text-end `((:session-id . ,ida)))
      (efrit-publish 'text-delta `((:session-id . ,idb) (:text . "bravo says yo")))
      (efrit-publish 'text-end `((:session-id . ,idb)))
      (should (string-match-p "alpha says hi" (test-ms--text a)))
      (should-not (string-match-p "bravo" (test-ms--text a)))
      (should (string-match-p "bravo says yo" (test-ms--text b)))
      (should-not (string-match-p "alpha" (test-ms--text b)))
      ;; tool rows pair within a session
      (efrit-publish 'tool-start `((:session-id . ,idb) (:tool . "read_file") (:tool-id . "t1") (:input . nil)))
      (efrit-publish 'tool-result `((:session-id . ,idb) (:tool . "read_file") (:tool-id . "t1")
                                    (:result . "contents") (:success . t)))
      (should (string-match-p "read_file" (test-ms--text b)))
      (should-not (string-match-p "read_file" (test-ms--text a)))
      ;; a note published as session A's code (no :session-id in the data)
      (efrit-with-session ida
        (efrit-publish 'note '((:text . "⛨ granted") (:face . shadow) (:kind . sandbox))))
      (should (string-match-p "granted" (test-ms--text a)))
      (should-not (string-match-p "granted" (test-ms--text b)))
      ;; status per buffer
      (efrit-publish 'status `((:session-id . ,idb) (:status . working)))
      (should (eq 'working (buffer-local-value 'efrit-agent--status b)))
      (should (eq 'idle (buffer-local-value 'efrit-agent--status a)))
      ;; a question waits only in its buffer
      (let ((efrit-agent-question-menu nil))
        (efrit-publish 'question `((:session-id . ,ida) (:question . "Which?") (:options . ("x" "y")))))
      (should (buffer-local-value 'efrit-agent--pending-question a))
      (should-not (buffer-local-value 'efrit-agent--pending-question b)))))

(ert-deftest test-ms-cancel-stops-only-this-sessions-stream ()
  "efrit-agent-cancel in buffer A cancels A's stream and leaves B's."
  (test-ms--with-two a b
    (let* ((ida (test-ms--session-id a)) (idb (test-ms--session-id b))
           (sa (efrit-api-stream--make :session-id ida))
           (sb (efrit-api-stream--make :session-id idb))
           (efrit-api-stream--active (list sa sb)))
      (with-current-buffer a
        (setq efrit-agent--status 'working)
        (efrit-repl-session-set-status efrit-agent--repl-session 'working)
        (cl-letf (((symbol-function 'efrit-session-active) (lambda () nil))
                  ((symbol-function 'efrit-agent--refresh-status-line) #'ignore))
          (efrit-agent-cancel)))
      (should (efrit-api-stream-cancelled sa))
      (should-not (efrit-api-stream-cancelled sb))
      (should (eq 'failed (buffer-local-value 'efrit-agent--status a)))
      (should (eq 'idle (buffer-local-value 'efrit-agent--status b))))))

(ert-deftest test-ms-tools-run-with-the-sessions-root ()
  "Around dispatch and API callbacks the session's project root is the
current root and its id is the current session, whatever buffer was
current when the sentinel fired."
  (test-ms--with-two a b
    (let* ((sb (buffer-local-value 'efrit-agent--repl-session b))
           (seen nil))
      (with-temp-buffer
        (setq default-directory "/")
        (efrit-repl-loop--with-session sb (lambda ()
                                            (setq seen (list default-directory efrit-project-root
                                                             efrit-current-session-id (current-buffer))))))
      (should (equal root-b (nth 0 seen)))
      (should (equal root-b (nth 1 seen)))
      (should (equal (efrit-repl-session-id sb) (nth 2 seen)))
      ;; the current buffer is not switched to the agent buffer
      (should-not (eq b (nth 3 seen))))))

(ert-deftest test-ms-question-menu-timer-is-per-buffer ()
  "Closing the menu in one buffer leaves the other buffer's opener alone."
  (test-ms--with-two a b
    (let ((fired nil))
      (with-current-buffer b
        (setq efrit-agent--question-menu-timer
              (run-at-time 10 nil (lambda () (setq fired t)))))
      (with-current-buffer a
        (efrit-agent--close-question-menu))
      (should (timerp (buffer-local-value 'efrit-agent--question-menu-timer b)))
      (should (memq (buffer-local-value 'efrit-agent--question-menu-timer b) timer-list))
      (with-current-buffer b (efrit-agent--cancel-question-menu-timer))
      (should-not fired))))

(ert-deftest test-ms-sandbox-turn-state-is-per-session ()
  "A's deny-all and once grant do not reach B; B's begin-turn does not clear A's."
  (require 'efrit-sandbox)
  (let ((efrit-sandbox--turn-state (make-hash-table :test 'equal)))
    (efrit-with-session "A"
      (efrit-sandbox-begin-turn)
      (efrit-sandbox-deny-rest-of-turn)
      (efrit-sandbox--turn-set :once '(:cap read :target "/x" :scope once)))
    (efrit-with-session "B"
      (should-not (efrit-sandbox-turn-answer))
      (should-not (efrit-sandbox--turn-get :once))
      (efrit-sandbox-begin-turn))
    (efrit-with-session "A"
      (should (eq 'deny-all (efrit-sandbox-turn-answer)))
      (should (efrit-sandbox--turn-get :once)))
    ;; outside any session: its own slot
    (should-not (efrit-sandbox-turn-answer))))

(ert-deftest test-ms-second-sessions-prompt-is-refused-not-nested ()
  "While A's prompt is up, B's prompt is not shown: it gets the default
and B gets a note.  A's own nested prompt still runs."
  (let* ((efrit-prompt--owner nil) (efrit-current-session-id nil) (notes nil) (ran nil)
         (sub (lambda (e) (push (cons (alist-get :session-id e) (alist-get :text e)) notes))))
    (efrit-subscribe 'note sub)
    (unwind-protect
        (efrit-with-session "A"
          (efrit-with-prompt-turn "a" 'denied
            (should (eq 'inner (efrit-with-session "A"
                                 (efrit-with-prompt-turn "a2" 'denied 'inner))))
            (should (eq 'denied (efrit-with-session "B"
                                  (efrit-with-prompt-turn "b" 'denied (setq ran t) 'asked))))
            (should-not ran)))
      (efrit-unsubscribe 'note sub))
    (should (equal "B" (caar notes)))
    (should (string-match-p "not asked" (cdar notes)))
    (should-not efrit-prompt--owner)))

(require 'efrit-agent-instances)

(ert-deftest test-ms-instances-are-named-per-project-and-numbered ()
  "First instance *efrit[proj]*, next *efrit[proj:2]*, renamed *efrit[proj:NAME]*;
each has its own session rooted in the project; rename keeps the slot."
  (let* ((root (file-name-as-directory (make-temp-file "efrit-proj-" t)))
         (efrit-agent--projects nil)
         (efrit-agent-auto-show nil)
         a b)
    (make-directory (expand-file-name ".git" root))   ; a project for project.el
    (unwind-protect
        (progn
          (setq a (efrit-agent-instance-create root))
          (setq b (efrit-agent-instance-create root))
          (should (equal (format "*efrit[%s]*" (file-name-nondirectory (directory-file-name root)))
                         (buffer-name a)))
          (should (string-suffix-p ":2]*" (buffer-name b)))
          (should (equal (list a b) (efrit-agent-instances root)))
          (should (equal root (efrit-repl-session-project-root
                               (buffer-local-value 'efrit-agent--repl-session b))))
          (should-not (eq (buffer-local-value 'efrit-agent--repl-session a)
                          (buffer-local-value 'efrit-agent--repl-session b)))
          (with-current-buffer b (efrit-agent-rename-instance "review"))
          (should (string-suffix-p ":review]*" (buffer-name b)))
          (should (= 1 (efrit-agent--slot b)))
          ;; the most recently used instance is what the project opens
          (with-current-buffer a (should (eq a (efrit-agent-instance-for-project root))))
          ;; kill b: number 2 is free again
          (kill-buffer b)
          (should (= 2 (efrit-agent--free-number root))))
      (when (buffer-live-p a) (kill-buffer a))
      (when (buffer-live-p b) (kill-buffer b))
      (delete-directory root t))))

(ert-deftest test-ms-toggle-hides-and-restores-per-tab ()
  "efrit-agent-toggle hides the project's shown agent windows and
remembers them in a frame parameter; a second toggle restores them."
  (let* ((root (file-name-as-directory (make-temp-file "efrit-proj-" t)))
         (efrit-agent--projects nil)
         (a (efrit-agent-instance-create root)))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (efrit-agent-display-in-side-window a)
          (should (get-buffer-window a))
          (efrit-agent-toggle root)
          (should-not (get-buffer-window a))
          (should (member a (efrit-agent--hidden-set root)))
          (efrit-agent-toggle root)
          (should (get-buffer-window a))
          (should-not (efrit-agent--hidden-set root)))
      (kill-buffer a)
      (delete-directory root t))))

(provide 'test-multi-session)
;;; test-multi-session.el ends here

(ert-deftest test-multi-session-do-path-gets-the-project-instance ()
  "An efrit-do session renders into the project's instance under
`efrit-agent-instances-mode' (not into a `*efrit-agent*' nobody has
open), into the default buffer otherwise, and into the buffer already
attached to it in either case."
  (let* ((root (file-name-as-directory (make-temp-file "efrit-do-route-" t)))
         (was-on (bound-and-true-p efrit-agent-instances-mode))
         (made nil))
    (unwind-protect
        (progn
          (make-directory (expand-file-name ".git" root))
          ;; instances off: the default buffer
          (efrit-agent-instances-mode -1)
          (let ((default-directory root))
            (should (equal efrit-agent-buffer-name
                           (buffer-name (efrit-agent-buffer-for-do-session "do-1")))))
          ;; instances on: the project's instance, created on demand
          (efrit-agent-instances-mode 1)
          (let* ((default-directory root)
                 (buf (efrit-agent-buffer-for-do-session "do-2")))
            (push buf made)
            (should (string-prefix-p "*efrit[efrit-do-route-" (buffer-name buf)))
            (should (equal root (plist-get (buffer-local-value 'efrit-agent--instance buf) :project)))
            ;; once attached, the same buffer answers by id from anywhere
            (with-current-buffer buf (efrit-agent--attach-session "do-2" "list files"))
            (with-temp-buffer
              (should (eq buf (efrit-agent-buffer-for-do-session "do-2")))
              (should (eq buf (efrit-agent-buffer-for "do-2"))))))
      (dolist (b made) (when (buffer-live-p b) (let ((kill-buffer-query-functions nil)) (kill-buffer b))))
      (unless was-on (efrit-agent-instances-mode -1))
      (delete-directory root t))))
