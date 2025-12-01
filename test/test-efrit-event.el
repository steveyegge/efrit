;;; test-efrit-event.el --- Tests for efrit-event module -*- lexical-binding: t -*-

(require 'ert)

;; Add load paths
(add-to-list 'load-path (expand-file-name "../lisp" (file-name-directory load-file-name)))
(add-to-list 'load-path (expand-file-name "../lisp/core" (file-name-directory load-file-name)))

(require 'efrit-event)

;;; Session Events

(ert-deftest efrit-event-session-start-creation ()
  "Test creating a session-start event from alist."
  (let* ((alist '(("type" . "session-start")
                  ("timestamp" . "2025-11-30T12:34:56-0800")
                  ("seq" . 1)
                  ("command" . "test command")))
         (event (efrit-event-from-alist alist)))
    (should (efrit-session-start-event-p event))
    (should (equal (slot-value event 'type) "session-start"))
    (should (equal (slot-value event 'command) "test command"))
    (should (equal (slot-value event 'seq) 1))))

(ert-deftest efrit-event-session-end-creation ()
  "Test creating a session-end event from alist."
  (let* ((alist '(("type" . "session-end")
                  ("timestamp" . "2025-11-30T12:35:00-0800")
                  ("seq" . 42)
                  ("success" . t)))
         (event (efrit-event-from-alist alist)))
    (should (efrit-session-end-event-p event))
    (should (equal (slot-value event 'success) t))))

(ert-deftest efrit-event-session-end-failure ()
  "Test creating a failed session-end event."
  (let* ((alist '(("type" . "session-end")
                  ("timestamp" . "2025-11-30T12:35:00-0800")
                  ("seq" . 43)
                  ("success" . :false)))
         (event (efrit-event-from-alist alist)))
    (should (efrit-session-end-event-p event))
    (should (not (slot-value event 'success)))))

;;; Tool Events

(ert-deftest efrit-event-tool-start-creation ()
  "Test creating a tool-start event from alist."
  (let* ((alist '(("type" . "tool-start")
                  ("timestamp" . "2025-11-30T12:35:01-0800")
                  ("seq" . 2)
                  ("tool" . "read-file")
                  ("repeat_count" . 1)))
         (event (efrit-event-from-alist alist)))
    (should (efrit-tool-start-event-p event))
    (should (equal (slot-value event 'tool) "read-file"))
    (should (equal (slot-value event 'repeat-count) 1))))

(ert-deftest efrit-event-tool-start-with-repeat ()
  "Test tool-start event with repeat count."
  (let* ((alist '(("type" . "tool-start")
                  ("timestamp" . "2025-11-30T12:35:01-0800")
                  ("seq" . 3)
                  ("tool" . "read-file")
                  ("repeat_count" . 3)))
         (event (efrit-event-from-alist alist)))
    (should (equal (slot-value event 'repeat-count) 3))))

(ert-deftest efrit-event-tool-result-creation ()
  "Test creating a tool-result event from alist."
  (let* ((alist '(("type" . "tool-result")
                  ("timestamp" . "2025-11-30T12:35:02-0800")
                  ("seq" . 4)
                  ("tool" . "read-file")
                  ("success" . t)
                  ("elapsed_ms" . 250)))
         (event (efrit-event-from-alist alist)))
    (should (efrit-tool-result-event-p event))
    (should (equal (slot-value event 'success) t))
    (should (equal (slot-value event 'elapsed-ms) 250))))

;;; Text Events

(ert-deftest efrit-event-text-message-creation ()
  "Test creating a text event from alist."
  (let* ((alist '(("type" . "text")
                  ("timestamp" . "2025-11-30T12:35:03-0800")
                  ("seq" . 5)
                  ("message" . "Test message")
                  ("message_type" . "info")))
         (event (efrit-event-from-alist alist)))
    (should (efrit-text-event-p event))
    (should (equal (slot-value event 'message) "Test message"))
    (should (equal (slot-value event 'message-type) "info"))))

(ert-deftest efrit-event-text-message-default-type ()
  "Test text event with default message type."
  (let* ((alist '(("type" . "text")
                  ("timestamp" . "2025-11-30T12:35:03-0800")
                  ("seq" . 6)
                  ("message" . "Test message")))
         (event (efrit-event-from-alist alist)))
    (should (equal (slot-value event 'message-type) "info"))))

;;; Injection Events

(ert-deftest efrit-event-injection-creation ()
  "Test creating an injection-received event from alist."
  (let* ((content '(("type" . "guidance")
                    ("message" . "Do something different")))
         (alist `(("type" . "injection-received")
                  ("timestamp" . "2025-11-30T12:35:04-0800")
                  ("seq" . 7)
                  ("content" . ,content)))
         (event (efrit-event-from-alist alist)))
    (should (efrit-injection-event-p event))
    (should (equal (slot-value event 'injection-type) "guidance"))
    (should (equal (slot-value event 'injection-message) "Do something different"))))

(ert-deftest efrit-event-injection-missing-content ()
  "Test injection event with missing content field."
  (let* ((alist '(("type" . "injection-received")
                  ("timestamp" . "2025-11-30T12:35:04-0800")
                  ("seq" . 8)))
         (event (efrit-event-from-alist alist)))
    (should (efrit-injection-event-p event))
    (should (equal (slot-value event 'injection-type) "?"))
    (should (equal (slot-value event 'injection-message) "?"))))

;;; Accessors

(ert-deftest efrit-event-get-type ()
  "Test accessing event type."
  (let* ((alist '(("type" . "session-start")
                  ("timestamp" . "2025-11-30T12:34:56-0800")
                  ("seq" . 1)
                  ("command" . "test")))
         (event (efrit-event-from-alist alist)))
    (should (equal (efrit-event-get-type event) "session-start"))))

(ert-deftest efrit-event-get-timestamp ()
  "Test accessing event timestamp."
  (let* ((alist '(("type" . "session-start")
                  ("timestamp" . "2025-11-30T12:34:56-0800")
                  ("seq" . 1)
                  ("command" . "test")))
         (event (efrit-event-from-alist alist)))
    (should (equal (efrit-event-get-timestamp event) "2025-11-30T12:34:56-0800"))))

(ert-deftest efrit-event-format-time ()
  "Test time formatting from full timestamp."
  (let* ((alist '(("type" . "session-start")
                  ("timestamp" . "2025-11-30T12:34:56-0800")
                  ("seq" . 1)
                  ("command" . "test")))
         (event (efrit-event-from-alist alist)))
    (should (equal (efrit-event-format-time event) "12:34:56"))))

(ert-deftest efrit-event-format-time-missing ()
  "Test time formatting with missing timestamp."
  (let* ((alist '(("type" . "session-start")
                  ("seq" . 1)
                  ("command" . "test")))
         (event (efrit-event-from-alist alist)))
    (should (equal (efrit-event-format-time event) "??:??:??"))))

;;; Formatting

(ert-deftest efrit-event-session-start-format ()
  "Test formatting a session-start event."
  (let* ((alist '(("type" . "session-start")
                  ("timestamp" . "2025-11-30T12:34:56-0800")
                  ("seq" . 1)
                  ("command" . "test command")))
         (event (efrit-event-from-alist alist))
         (formatted (efrit-event-format event)))
    (should (stringp formatted))
    (should (string-match "SESSION START" formatted))
    (should (string-match "test command" formatted))))

(ert-deftest efrit-event-tool-start-format ()
  "Test formatting a tool-start event."
  (let* ((alist '(("type" . "tool-start")
                  ("timestamp" . "2025-11-30T12:35:01-0800")
                  ("seq" . 2)
                  ("tool" . "read-file")
                  ("repeat_count" . 1)))
         (event (efrit-event-from-alist alist))
         (formatted (efrit-event-format event)))
    (should (stringp formatted))
    (should (string-match "read-file" formatted))))

(ert-deftest efrit-event-tool-start-format-with-repeat ()
  "Test formatting tool-start with repeat count."
  (let* ((alist '(("type" . "tool-start")
                  ("timestamp" . "2025-11-30T12:35:01-0800")
                  ("seq" . 3)
                  ("tool" . "read-file")
                  ("repeat_count" . 3)))
         (event (efrit-event-from-alist alist))
         (formatted (efrit-event-format event)))
    (should (string-match "repeat #3" formatted))))

(ert-deftest efrit-event-session-end-success-format ()
  "Test formatting a successful session-end event."
  (let* ((alist '(("type" . "session-end")
                  ("timestamp" . "2025-11-30T12:35:00-0800")
                  ("seq" . 42)
                  ("success" . t)))
         (event (efrit-event-from-alist alist))
         (formatted (efrit-event-format event)))
    (should (string-match "SESSION COMPLETE" formatted))))

(ert-deftest efrit-event-session-end-failure-format ()
  "Test formatting a failed session-end event."
  (let* ((alist '(("type" . "session-end")
                  ("timestamp" . "2025-11-30T12:35:00-0800")
                  ("seq" . 43)
                  ("success" . :false)))
         (event (efrit-event-from-alist alist))
         (formatted (efrit-event-format event)))
    (should (string-match "SESSION FAILED" formatted))))

;;; String Truncation

(ert-deftest efrit-event--truncate-string ()
  "Test string truncation."
  (should (equal (efrit-event--truncate-string "hello" 10) "hello"))
  (should (equal (efrit-event--truncate-string "hello world" 8) "hello...")))

(ert-deftest efrit-event--truncate-string-nil ()
  "Test truncation with nil input."
  (should (equal (efrit-event--truncate-string nil 10) nil)))

(ert-deftest efrit-event--truncate-string-exact ()
  "Test truncation with exact length."
  (should (equal (efrit-event--truncate-string "hello" 5) "hello")))

(provide 'test-efrit-event)

;;; test-efrit-event.el ends here
