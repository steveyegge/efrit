;;; test-queue.el --- Tests for efrit-do-queue -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'efrit-do-queue)
(require 'efrit-session)
(require 'efrit-log)

(defgroup test-queue nil
  "Tests for command queue infrastructure."
  :group 'efrit)

;;; Test Fixtures

(defun test-queue-cleanup ()
  "Clean up test queue state."
  (setq efrit-do-queue--global-queue nil)
  (setq efrit-do-queue--processing nil))

;;; Tests

(ert-deftest test-queue-add-command ()
  "Test adding a command to the queue."
  (unwind-protect
      (let ((added (efrit-do-queue-add-command "test command")))
        (should (eq added t))
        (should (= (efrit-do-queue-size) 1)))
    (test-queue-cleanup)))

(ert-deftest test-queue-add-multiple-commands ()
  "Test adding multiple commands to the queue."
  (unwind-protect
      (progn
        (efrit-do-queue-add-command "command 1")
        (efrit-do-queue-add-command "command 2")
        (efrit-do-queue-add-command "command 3")
        (should (= (efrit-do-queue-size) 3)))
    (test-queue-cleanup)))

(ert-deftest test-queue-pop-command ()
  "Test popping a command from the queue."
  (unwind-protect
      (progn
        (efrit-do-queue-add-command "first")
        (efrit-do-queue-add-command "second")
        
        (let ((cmd (efrit-do-queue-pop-command)))
          (should (string= cmd "first"))
          (should (= (efrit-do-queue-size) 1))))
    (test-queue-cleanup)))

(ert-deftest test-queue-pop-fifo-order ()
  "Test that queue pops in FIFO order."
  (unwind-protect
      (progn
        (efrit-do-queue-add-command "first")
        (efrit-do-queue-add-command "second")
        (efrit-do-queue-add-command "third")
        
        (should (string= (efrit-do-queue-pop-command) "first"))
        (should (string= (efrit-do-queue-pop-command) "second"))
        (should (string= (efrit-do-queue-pop-command) "third"))
        (should (null (efrit-do-queue-pop-command)))
        (should (= (efrit-do-queue-size) 0)))
    (test-queue-cleanup)))

(ert-deftest test-queue-clear ()
  "Test clearing the queue."
  (unwind-protect
      (progn
        (efrit-do-queue-add-command "cmd1")
        (efrit-do-queue-add-command "cmd2")
        (should (= (efrit-do-queue-size) 2))
        
        (let ((cleared (efrit-do-queue-clear)))
          (should (= cleared 2))
          (should (= (efrit-do-queue-size) 0))))
    (test-queue-cleanup)))

(ert-deftest test-queue-clear-empty ()
  "Test clearing an empty queue."
  (unwind-protect
      (let ((cleared (efrit-do-queue-clear)))
        (should (= cleared 0)))
    (test-queue-cleanup)))

(ert-deftest test-queue-size ()
  "Test queue size tracking."
  (unwind-protect
      (progn
        (should (= (efrit-do-queue-size) 0))
        (efrit-do-queue-add-command "cmd1")
        (should (= (efrit-do-queue-size) 1))
        (efrit-do-queue-add-command "cmd2")
        (should (= (efrit-do-queue-size) 2))
        (efrit-do-queue-pop-command)
        (should (= (efrit-do-queue-size) 1)))
    (test-queue-cleanup)))

(ert-deftest test-queue-max-size-limit ()
  "Test that queue respects maximum size limit."
  (unwind-protect
      (let ((original-max efrit-do-queue-max-size))
        (setq efrit-do-queue-max-size 3)
        (unwind-protect
            (progn
              (efrit-do-queue-add-command "cmd1")
              (efrit-do-queue-add-command "cmd2")
              (efrit-do-queue-add-command "cmd3")
              
              ;; Queue is full, adding 4th should fail
              (let ((added (efrit-do-queue-add-command "cmd4")))
                (should (null added))
                (should (= (efrit-do-queue-size) 3))))
          (setq efrit-do-queue-max-size original-max)))
    (test-queue-cleanup)))

(ert-deftest test-queue-pop-empty ()
  "Test popping from empty queue."
  (unwind-protect
      (let ((cmd (efrit-do-queue-pop-command)))
        (should (null cmd)))
    (test-queue-cleanup)))

(ert-deftest test-queue-add-empty-command ()
  "Test adding an empty command string."
  (unwind-protect
      (progn
        (efrit-do-queue-add-command "")
        (should (= (efrit-do-queue-size) 1))
        (let ((cmd (efrit-do-queue-pop-command)))
          (should (string= cmd ""))))
    (test-queue-cleanup)))

(ert-deftest test-queue-add-long-command ()
  "Test adding a very long command string."
  (unwind-protect
      (let ((long-cmd (make-string 1000 ?a)))
        (efrit-do-queue-add-command long-cmd)
        (should (= (efrit-do-queue-size) 1))
        (let ((cmd (efrit-do-queue-pop-command)))
          (should (string= cmd long-cmd))))
    (test-queue-cleanup)))

(ert-deftest test-queue-add-special-characters ()
  "Test adding commands with special characters."
  (unwind-protect
      (let ((special-cmd "find files matching \"*.el\" in ~/projects"))
        (efrit-do-queue-add-command special-cmd)
        (let ((cmd (efrit-do-queue-pop-command)))
          (should (string= cmd special-cmd))))
    (test-queue-cleanup)))

(ert-deftest test-queue-show-size-command ()
  "Test the show-size interactive command."
  (unwind-protect
      (progn
        (efrit-do-queue-add-command "cmd1")
        (efrit-do-queue-add-command "cmd2")
        ;; Should not error
        (should (progn
                  (efrit-do-queue-show-size)
                  t)))
    (test-queue-cleanup)))

(ert-deftest test-queue-process-next ()
  "Test queue processing function."
  (unwind-protect
      (progn
        (efrit-do-queue-add-command "cmd1")
        
        ;; Process next should return t if a command exists
        (let ((has-cmd (efrit-do-queue-process-next)))
          (should (eq has-cmd t))))
    (test-queue-cleanup)))

(ert-deftest test-queue-process-next-empty ()
  "Test queue processing on empty queue."
  (unwind-protect
      (progn
        (let ((has-cmd (efrit-do-queue-process-next)))
          (should (null has-cmd))))
    (test-queue-cleanup)))

(ert-deftest test-queue-autoproc-disabled ()
  "Test that queue doesn't process when auto-process is disabled."
  (unwind-protect
      (let ((original-auto efrit-do-queue-auto-process))
        (setq efrit-do-queue-auto-process nil)
        (unwind-protect
            (progn
              (efrit-do-queue-add-command "cmd1")
              (let ((has-cmd (efrit-do-queue-process-next)))
                (should (null has-cmd))
                ;; Command should still be in queue since auto-process is off
                (should (= (efrit-do-queue-size) 1))))
          (setq efrit-do-queue-auto-process original-auto)))
    (test-queue-cleanup)))

(ert-deftest test-queue-show-queue-command ()
  "Test the show-queue interactive command."
  (unwind-protect
      (progn
        (efrit-do-queue-add-command "cmd1")
        (efrit-do-queue-add-command "cmd2")
        ;; Should not error
        (should (progn
                  (efrit-do-show-queue)
                  t)))
    (test-queue-cleanup)))

(ert-deftest test-queue-clear-queue-command ()
  "Test the clear-queue interactive command."
  (unwind-protect
      (progn
        (efrit-do-queue-add-command "cmd1")
        (should (> (efrit-do-queue-size) 0))
        ;; Should not error
        (should (progn
                  (efrit-do-clear-queue)
                  t))
        (should (= (efrit-do-queue-size) 0)))
    (test-queue-cleanup)))

(ert-deftest test-queue-next-command ()
  "Test the next interactive command."
  (unwind-protect
      (progn
        (efrit-do-queue-add-command "cmd1")
        ;; Should not error
        (should (progn
                  (efrit-do-next)
                  t)))
    (test-queue-cleanup)))

(ert-deftest test-queue-command-command ()
  "Test the queue-command interactive command."
  (unwind-protect
      (let ((original-add (symbol-function 'efrit-do-queue-add-command)))
        ;; Just verify it doesn't error
        (should (progn
                  (efrit-do-queue-command "test command")
                  t)))
    (test-queue-cleanup)))

(provide 'test-queue)

;;; test-queue.el ends here
