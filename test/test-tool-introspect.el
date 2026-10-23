;;; test-tool-introspect.el --- symbol_info, live_state, mode_line_report, buffer_scan -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'efrit-test-sandbox-helpers)
(require 'efrit-tool-introspect)
(require 'efrit-do-handlers)

(defvar test-introspect--option 42 "A variable for the tests.")
(defun test-introspect--fn (a &optional b) "Add A and B." (+ a (or b 0)))
(defvar-local test-introspect--local 'default)

(ert-deftest test-introspect-symbol-info-function-and-variable ()
  "symbol_info names the kinds, gives signature, docstring, file and
line and the definition text of a function; the value, buffer-locality
and docstring of a variable; refuses an uninterned name."
  (let ((r (efrit-tool-symbol-info '((symbol . "test-introspect--fn")))))
    (should (eq t (alist-get 'success r)))
    (let* ((res (alist-get 'result r))
           (fn (alist-get 'function res)))
      (should (member "function" (append (alist-get 'kinds res) nil)))
      (should (string-match-p "&optional" (alist-get 'signature fn)))
      (should (equal "Add A and B." (alist-get 'docstring fn)))
      (should (string-suffix-p "test-tool-introspect.el" (or (alist-get 'file fn) "")))
      (should (integerp (alist-get 'line fn)))
      (should (string-prefix-p "(defun test-introspect--fn" (or (alist-get 'source fn) "")))
      (should (eq :json-false (alist-get 'interactive fn)))))
  ;; a variable, read in a buffer where it is local
  (with-temp-buffer
    (setq test-introspect--local 'here)
    (let* ((r (efrit-tool-symbol-info `((symbol . "test-introspect--local") (buffer . ,(buffer-name)))))
           (var (alist-get 'variable (alist-get 'result r))))
      (should (eq t (alist-get 'success r)))
      (should (equal "here" (alist-get 'value var)))
      (should (eq t (alist-get 'buffer_local var)))
      (should (equal "default" (alist-get 'default_value var)))))
  ;; a built-in command: compiled, interactive, no source text needed
  (let* ((r (efrit-tool-symbol-info '((symbol . "forward-char") (include_source . :json-false))))
         (fn (alist-get 'function (alist-get 'result r))))
    (should (eq t (alist-get 'interactive fn)))
    (should (eq t (alist-get 'compiled fn)))
    (should-not (alist-get 'source fn)))
  ;; not interned: an error that says so
  (should-not (eq t (alist-get 'success (efrit-tool-symbol-info '((symbol . "no-such-symbol-anywhere-xyz")))))))

(ert-deftest test-introspect-live-state-names-only ()
  "live_state returns bound/fbound and values per name without evaluating
anything; a name that is not interned says so; vectors from JSON work;
the handler is wired."
  (with-temp-buffer
    (setq test-introspect--local 'mine)
    (let* ((r (efrit-tool-live-state `((names . ["test-introspect--option" "test-introspect--fn"
                                                 "test-introspect--local" "not-interned-qq"])
                                       (buffer . ,(buffer-name)))))
           (names (append (alist-get 'names (alist-get 'result r)) nil)))
      (should (eq t (alist-get 'success r)))
      (should (equal "42" (alist-get 'value (nth 0 names))))
      (should (eq t (alist-get 'fbound (nth 1 names))))
      (should (eq :json-false (alist-get 'bound (nth 1 names))))
      (should (equal "mine" (alist-get 'value (nth 2 names))))
      (should (eq t (alist-get 'buffer_local (nth 2 names))))
      (should (eq :json-false (alist-get 'interned (nth 3 names))))))
  (let ((input (make-hash-table :test 'equal)))
    (puthash "names" (vector "test-introspect--option") input)
    (should (string-match-p "\"value\":\"42\"" (efrit-do--handle-live-state input)))))

(ert-deftest test-introspect-mode-line-report-names-pieces ()
  "mode_line_report renders each window's lines and names the construct
each piece came from, including an :eval function and a global-mode-string entry."
  (with-temp-buffer
    (rename-buffer "*introspect-ml*" t)
    (setq-local mode-line-format '("lit-" mode-line-buffer-identification " " (:eval (test-introspect--fn 1 2)) " " global-mode-string))
    (setq-local header-line-format '("hdr " mode-name))
    (let ((global-mode-string '("" (:eval (format "%s" 'gms-here)))))
      (save-window-excursion
        (set-window-buffer (selected-window) (current-buffer))
        (let* ((r (efrit-tool-mode-line-report '((buffer . "*introspect-ml*"))))
               (res (alist-get 'result r))
               (win (aref (alist-get 'windows res) 0))
               (ml (alist-get 'mode_line win))
               (sources (mapcar (lambda (p) (alist-get 'source p)) (append (alist-get 'pieces ml) nil))))
          (should (eq t (alist-get 'success r)))
          (should (equal "*introspect-ml*" (alist-get 'buffer win)))
          ;; `format-mode-line' renders nothing in batch; the text
          ;; checks need a display
          (unless noninteractive
            (should (string-prefix-p "lit-" (alist-get 'text ml))))
          (should (member "mode-line-buffer-identification" sources))
          (should (member "(:eval (test-introspect--fn …))" sources))
          (should (member "global-mode-string" sources))
          (should (listp (alist-get 'header_line win)))
          (unless noninteractive
            (should (string-prefix-p "hdr" (alist-get 'text (alist-get 'header_line win))))
            (should (cl-some (lambda (p) (equal "gms-here" (alist-get 'text p)))
                             (append (alist-get 'global_mode_string res) nil)))))))))

(ert-deftest test-introspect-buffer-scan-one-and-many ()
  "buffer_scan finds matches with context in one buffer, newest first
on request; scans several buffers by name pattern, skipping hidden
ones and those the sandbox refuses; caps the total; rejects a bad regexp."
  (let* ((root (file-name-as-directory (make-temp-file "efrit-scan-" t)))
         (efrit-project-root root)
         (efrit-sandbox-enabled t)
         (efrit-sandbox-request-function nil)
         (efrit-sandbox--session-grants (make-hash-table :test 'equal))
         (efrit-sandbox--project-grants (make-hash-table :test 'equal))
         (efrit-sandbox--turn-state (make-hash-table :test 'equal))
         (efrit-sandbox-store--loaded (make-hash-table :test 'equal)))
    (let ((a (generate-new-buffer "*scan-a*"))
          (b (generate-new-buffer "*scan-b*"))
          (hidden (generate-new-buffer " *scan-hidden*"))
          (efrit-sandbox-target-buffer-function nil)
          (efrit-sandbox-agent-buffer-p-function nil))
      (unwind-protect
          (progn
            (with-current-buffer a (insert "one\nkill-ring first\nthree\nfour\nkill-ring second\nsix\n"))
            (with-current-buffer b (insert "kill-ring in b\n"))
            (with-current-buffer hidden (insert "kill-ring hidden\n"))
            ;; fileless non-target buffers need a grant: give it for these
            (efrit-sandbox-grant 'buffer '(buffer . "*scan-a*") 'session root)
            (efrit-sandbox-grant 'buffer '(buffer . "*scan-b*") 'session root)
            (let* ((r (efrit-tool-buffer-scan '((pattern . "kill-ring") (buffer . "*scan-a*") (context . 1))))
                   (res (alist-get 'result r))
                   (m (append (alist-get 'matches (aref (alist-get 'results res) 0)) nil)))
              (should (eq t (alist-get 'success r)))
              (should (= 2 (alist-get 'total_matches res)))
              (should (= 2 (alist-get 'line (nth 0 m))))
              (should (equal "one" (alist-get 'context_before (nth 0 m))))
              (should (equal "three" (alist-get 'context_after (nth 0 m)))))
            ;; newest first, capped
            (let* ((r (efrit-tool-buffer-scan '((pattern . "kill-ring") (buffer . "*scan-a*") (from_end . t) (max . 1))))
                   (res (alist-get 'result r))
                   (m (append (alist-get 'matches (aref (alist-get 'results res) 0)) nil)))
              (should (= 5 (alist-get 'line (nth 0 m))))
              (should (eq t (alist-get 'truncated res))))
            ;; by name pattern: a and b, not the hidden one
            (let* ((r (efrit-tool-buffer-scan '((pattern . "kill-ring") (buffer_regexp . "scan-"))))
                   (res (alist-get 'result r)))
              (should (= 2 (alist-get 'buffers_scanned res)))
              (should (= 3 (alist-get 'total_matches res))))
            ;; a buffer without a grant is skipped and named in a warning
            (let ((c (generate-new-buffer "*scan-c*")))
              (unwind-protect
                  (let ((r (efrit-tool-buffer-scan '((pattern . "kill-ring") (buffer_regexp . "scan-c")))))
                    (should (= 0 (alist-get 'buffers_scanned (alist-get 'result r))))
                    (should (cl-some (lambda (w) (string-match-p "scan-c" w)) (alist-get 'warnings r))))
                (kill-buffer c)))
            (should-not (eq t (alist-get 'success (efrit-tool-buffer-scan '((pattern . "[") (buffer . "*scan-a*")))))))
        (kill-buffer a) (kill-buffer b) (kill-buffer hidden)
        (delete-directory root t)))))

(ert-deftest test-introspect-buffer-list-local-first-remote-flagged ()
  "buffer_list describes buffers without entering them, local ones
first, remote ones flagged with their host; buffer_scan under a name
pattern skips remote buffers and names them unless include_remote."
  (let ((local (generate-new-buffer "*bl-local*"))
        (remote (generate-new-buffer "bl-remote.py"))
        (efrit-sandbox-enabled nil))
    (unwind-protect
        (progn
          (with-current-buffer local (insert "needle here\n"))
          (with-current-buffer remote
            (insert "needle too\n")
            (setq buffer-file-name "/ssh:host.invalid:/home/u/bl-remote.py")
            (set-buffer-modified-p nil))
          (let* ((r (efrit-tool-buffer-list '((pattern . "bl-"))))
                 (res (alist-get 'result r))
                 (bufs (append (alist-get 'buffers res) nil)))
            (should (eq t (alist-get 'success r)))
            (should (= 1 (alist-get 'local res)))
            (should (= 1 (alist-get 'remote res)))
            (should (equal "*bl-local*" (alist-get 'name (nth 0 bufs))))
            (should (eq :json-false (alist-get 'remote (nth 0 bufs))))
            (should (equal "/ssh:host.invalid:" (alist-get 'remote (nth 1 bufs)))))
          (let* ((r (efrit-tool-buffer-scan '((pattern . "needle") (buffer_regexp . "bl-"))))
                 (res (alist-get 'result r)))
            (should (= 1 (alist-get 'buffers_scanned res)))
            (should (cl-some (lambda (w) (string-match-p "remote buffers not scanned.*bl-remote" w))
                             (alist-get 'warnings r))))
          ;; opting in scans it (no sandbox here; live it would ask)
          (let* ((r (efrit-tool-buffer-scan '((pattern . "needle") (buffer_regexp . "bl-") (include_remote . t)))))
            (should (= 2 (alist-get 'buffers_scanned (alist-get 'result r))))))
      (with-current-buffer remote (setq buffer-file-name nil) (set-buffer-modified-p nil))
      (kill-buffer local) (kill-buffer remote))))

(provide 'test-tool-introspect)

;;; test-tool-introspect.el ends here
