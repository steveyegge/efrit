;;; efrit-event.el --- Event abstraction for Efrit operations -*- lexical-binding: t -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (eieio "1.4"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; This module provides EIEIO classes for representing different event types
;; in Efrit's progress tracking system. It replaces ad-hoc alist extraction
;; patterns with type-safe, documented classes.
;;
;; The class hierarchy:
;;   efrit-event (base class)
;;   ├── efrit-session-start-event
;;   ├── efrit-session-end-event
;;   ├── efrit-tool-start-event
;;   ├── efrit-tool-result-event
;;   ├── efrit-tool-error-event
;;   ├── efrit-text-event
;;   └── efrit-injection-event
;;
;; Event creation is handled by factory functions that accept alists
;; (from JSON parsing) and validate/convert them to proper objects.

;;; Code:

(require 'eieio)

;;; Helpers

(defun efrit-event--truncate-string (str max-len)
  "Truncate STR to MAX-LEN characters with ellipsis."
  (when str
    (if (and max-len (> (length str) max-len))
        (concat (substring str 0 (max 0 (- max-len 3))) "...")
      str)))

;;; Base Event Class

(defclass efrit-event ()
  ((type :initarg :type :type string :documentation "Event type identifier")
   (timestamp :initarg :timestamp :type t :documentation "ISO8601 timestamp or nil" :initform nil)
   (seq :initarg :seq :type t :initform 0 :documentation "Sequence number within session"))
  :abstract t
  :documentation "Base class for all Efrit events.")

(cl-defmethod efrit-event-get-type ((event efrit-event))
  "Get the type identifier for EVENT."
  (slot-value event 'type))

(cl-defmethod efrit-event-get-timestamp ((event efrit-event))
  "Get the timestamp for EVENT."
  (slot-value event 'timestamp))

(cl-defmethod efrit-event-format-time ((event efrit-event))
  "Format the timestamp for EVENT into HH:MM:SS format."
  (let ((timestamp (efrit-event-get-timestamp event)))
    (if timestamp
        (replace-regexp-in-string ".*T\\([0-9:]+\\).*" "\\1" timestamp)
      "??:??:??")))

;;; Session Events

(defclass efrit-session-start-event (efrit-event)
  ((command :initarg :command :type string :documentation "The command being executed"))
  :documentation "Event emitted when a session starts.")

(cl-defmethod efrit-event-format ((event efrit-session-start-event))
  "Format EVENT for display."
  (let ((time-str (efrit-event-format-time event))
        (command (slot-value event 'command)))
    (concat (propertize (format "[%s] " time-str) 'face 'efrit-progress-timestamp)
            (propertize "═══ SESSION START ═══\n" 'face 'efrit-progress-section-header)
            (propertize (format "[%s] " time-str) 'face 'efrit-progress-timestamp)
            (format "Command: %s\n" command))))

(defclass efrit-session-end-event (efrit-event)
  ((success :initarg :success :type boolean :documentation "Whether the session succeeded"))
  :documentation "Event emitted when a session ends.")

(cl-defmethod efrit-event-format ((event efrit-session-end-event))
  "Format EVENT for display."
  (let ((time-str (efrit-event-format-time event))
        (success (slot-value event 'success)))
    (concat (propertize (format "[%s] " time-str) 'face 'efrit-progress-timestamp)
            (propertize (format "═══ SESSION %s ═══\n"
                              (if success "COMPLETE" "FAILED"))
                       'face (if success 'efrit-progress-success 'efrit-progress-error)))))

;;; Tool Events

(defclass efrit-tool-start-event (efrit-event)
  ((tool :initarg :tool :type string :documentation "Tool name")
   (repeat-count :initarg :repeat-count :type integer :documentation "Number of consecutive calls"))
  :documentation "Event emitted when a tool starts executing.")

(cl-defmethod efrit-event-format ((event efrit-tool-start-event))
  "Format EVENT for display."
  (let ((time-str (efrit-event-format-time event))
        (tool (slot-value event 'tool))
        (repeat (slot-value event 'repeat-count)))
    (concat (propertize (format "[%s] " time-str) 'face 'efrit-progress-timestamp)
            (propertize (format "▶ %s" tool) 'face 'efrit-progress-tool-name)
            (if (and repeat (> repeat 1))
                (format " (repeat #%d)" repeat)
              "")
            "\n")))

(defclass efrit-tool-result-event (efrit-event)
  ((tool :initarg :tool :type string :documentation "Tool name")
   (success :initarg :success :type boolean :documentation "Whether tool succeeded")
   (elapsed-ms :initarg :elapsed-ms :type integer :documentation "Elapsed time in milliseconds"))
  :documentation "Event emitted when a tool finishes executing.")

(cl-defmethod efrit-event-format ((event efrit-tool-result-event))
  "Format EVENT for display."
  (let ((time-str (efrit-event-format-time event))
        (tool (slot-value event 'tool))
        (success (slot-value event 'success)))
    (concat (propertize (format "[%s] " time-str) 'face 'efrit-progress-timestamp)
            (propertize (format "◀ %s %s\n" tool (if success "✓" "✗"))
                       'face (if success 'efrit-progress-success 'efrit-progress-error)))))

;;; Text Events

(defclass efrit-text-event (efrit-event)
  ((message :initarg :message :type string :documentation "Message text")
   (message-type :initarg :message-type :type string :initform "info" :documentation "Type of message (claude, error, success, info)"))
  :documentation "Event for general text messages.")

(cl-defmethod efrit-event-format ((event efrit-text-event))
  "Format EVENT for display."
  (let ((time-str (efrit-event-format-time event))
        (message (slot-value event 'message))
        (msg-type (slot-value event 'message-type)))
    (concat (propertize (format "[%s] " time-str) 'face 'efrit-progress-timestamp)
            (propertize (efrit-event--truncate-string message 200)
                       'face (pcase msg-type
                               ("claude" 'efrit-progress-claude-message)
                               ("error" 'efrit-progress-error)
                               (_ nil)))
            "\n")))

;;; Injection Events

(defclass efrit-injection-event (efrit-event)
  ((injection-type :initarg :injection-type :type string :documentation "Type of injection")
   (injection-message :initarg :injection-message :type string :documentation "Injection message"))
  :documentation "Event emitted when an injection is received.")

(cl-defmethod efrit-event-format ((event efrit-injection-event))
  "Format EVENT for display."
  (let ((time-str (efrit-event-format-time event))
        (inject-type (slot-value event 'injection-type))
        (inject-msg (slot-value event 'injection-message)))
    (concat (propertize (format "[%s] " time-str) 'face 'efrit-progress-timestamp)
            (propertize (format "📥 INJECTION [%s]: %s\n"
                              inject-type
                              (efrit-event--truncate-string inject-msg 100))
                       'face 'efrit-progress-section-header))))

;;; Factory Functions

(defun efrit-event-from-alist (alist)
  "Convert an alist (from JSON) to an appropriate efrit-event subclass.
Returns an event object, or nil if the alist is invalid."
  (let* ((event-type (alist-get "type" alist nil nil 'string-equal))
         (timestamp (alist-get "timestamp" alist nil nil 'string-equal))
         (seq (alist-get "seq" alist 0 nil 'string-equal)))
    (unless event-type
      (error "Event alist missing 'type' field"))
    (pcase event-type
      ("session-start"
       (let ((command (alist-get "command" alist "" nil 'string-equal)))
         (make-instance 'efrit-session-start-event
           :type event-type
           :timestamp timestamp
           :seq seq
           :command command)))
      ("session-end"
       (let ((success (alist-get "success" alist nil nil 'string-equal)))
         (make-instance 'efrit-session-end-event
           :type event-type
           :timestamp timestamp
           :seq seq
           :success (eq success t))))
      ("tool-start"
       (let ((tool (alist-get "tool" alist "" nil 'string-equal))
             (repeat-count (alist-get "repeat_count" alist 1 nil 'string-equal)))
         (make-instance 'efrit-tool-start-event
           :type event-type
           :timestamp timestamp
           :seq seq
           :tool tool
           :repeat-count repeat-count)))
      ("tool-result"
       (let ((tool (alist-get "tool" alist "" nil 'string-equal))
             (success (alist-get "success" alist nil nil 'string-equal))
             (elapsed-ms (alist-get "elapsed_ms" alist 0 nil 'string-equal)))
         (make-instance 'efrit-tool-result-event
           :type event-type
           :timestamp timestamp
           :seq seq
           :tool tool
           :success (eq success t)
           :elapsed-ms elapsed-ms)))
      ("text"
       (let ((message (alist-get "message" alist "" nil 'string-equal))
             (msg-type (alist-get "message_type" alist "info" nil 'string-equal)))
         (make-instance 'efrit-text-event
           :type event-type
           :timestamp timestamp
           :seq seq
           :message message
           :message-type msg-type)))
      ("injection-received"
       (let* ((content (alist-get "content" alist nil nil 'string-equal))
              (inject-type (if content (alist-get "type" content "" nil 'string-equal) "?"))
              (inject-msg (if content (alist-get "message" content "" nil 'string-equal) "?")))
         (make-instance 'efrit-injection-event
           :type event-type
           :timestamp timestamp
           :seq seq
           :injection-type inject-type
           :injection-message inject-msg)))
      (_
       ;; Unknown event type - create a generic event with base class info
       nil))))

(provide 'efrit-event)

;;; efrit-event.el ends here
