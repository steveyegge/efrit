;;; test-edit-history.el --- efrit-throttle, efrit-edit-history -*- lexical-binding: t; -*-
;;; Code:
(require 'ert)
(require 'efrit-throttle)
(require 'efrit-edit-history)
(require 'efrit-context-sources)

(ert-deftest test-throttle-debounces-and-skips-own-commands ()
  "Many requests in a burst run the function once; a request during
one of our own commands is dropped; a block function drops it too."
  (let* ((runs nil)
         (blocked nil)
         (th (efrit-throttle-create "t" (lambda (&rest a) (push a runs))
                                    :debounce 0.05 :interval 0.05
                                    :own-command-prefix "efrit-test-own-"
                                    :block-functions (list (lambda () blocked)))))
    (dotimes (i 5) (efrit-throttle-request th i))
    (should-not runs)
    ;; the pending timer is one; let it fire (a fixed sit-for flaked
    ;; under sweep load 2026-09-30, so wait for the run itself)
    (let ((deadline (+ (float-time) 3)))
      (while (and (not runs) (< (float-time) deadline)) (sit-for 0.05)))
    (should (equal '((4)) runs))
    (should (= 1 (efrit-throttle-runs th)))
    (let ((this-command 'efrit-test-own-accept))
      (should (eq 'own-command (efrit-throttle-request th 'x))))
    (setq blocked t)
    (should (eq 'blocked (efrit-throttle-request th 'y)))
    (setq blocked nil)
    (should (= 2 (efrit-throttle-skipped th)))
    (efrit-throttle-request th 'z)
    (efrit-throttle-cancel th)
    (sit-for 0.2)
    (should (= 1 (efrit-throttle-runs th)))))

(ert-deftest test-edit-history-records-bursts-as-diffs ()
  "A burst of edits becomes one entry with the hunk; an unchanged
buffer records nothing; the context source renders newest first and
only while the mode is on."
  (skip-unless (executable-find diff-command))
  (with-temp-buffer
    (rename-buffer "eh-test" t)
    (insert "one\ntwo\nthree\n")
    (efrit-edit-history-mode 1)
    (should efrit-edit-history-mode)
    (should-not (efrit-edit-history-record))
    (goto-char (point-max)) (insert "four\n") (insert "five\n")
    (let ((e (efrit-edit-history-record)))
      (should e)
      (should (string-match-p "^\\+four\n\\+five" (plist-get e :diff)))
      (should-not (string-match-p "^--- \\|^\\+\\+\\+ " (plist-get e :diff))))
    (should-not (efrit-edit-history-record))
    (goto-char (point-min)) (delete-region (point) (line-beginning-position 2))
    (efrit-edit-history-record)
    (should (= 2 (length efrit-edit-history--entries)))
    (let ((text (efrit-edit-history-text)))
      (should (string-match-p "Recent edits" text))
      ;; newest first: the deletion of "one" comes before the additions
      (should (< (string-search "-one" text) (string-search "+four" text))))
    ;; the budget cuts whole entries
    (should-not (string-search "+four" (efrit-edit-history-text nil 60)))
    ;; through the context source
    (let ((efrit-context-sources '(edit-history)))
      (should (string-match-p "-one" (efrit-context-snapshot (current-buffer)))))
    (efrit-edit-history-mode -1)
    (let ((efrit-context-sources '(edit-history)))
      (should-not (efrit-context-snapshot (current-buffer))))))

(ert-deftest test-edit-history-change-hook-arms-one-throttle ()
  "Typing arms the throttle; after the idle time one entry exists."
  (skip-unless (executable-find diff-command))
  (with-temp-buffer
    (rename-buffer "eh-hook" t)
    (insert "a\n")
    (let ((efrit-edit-history-idle-seconds 0.05))
      (efrit-edit-history-mode 1)
      (insert "b\n") (insert "c\n")
      (should-not efrit-edit-history--entries)
      (sit-for 0.3)
      (should (= 1 (length efrit-edit-history--entries)))
      (efrit-edit-history-mode -1))))

(provide 'test-edit-history)
;;; test-edit-history.el ends here
