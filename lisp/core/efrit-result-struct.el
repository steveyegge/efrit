;;; efrit-result-struct.el --- The result of one tool call -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.

;; Author: Ted Zlatanov <tzz@lifelogs.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; A tool call used to return a bare string, and every layer above
;; sniffed it: "^Error " for failure, "[SESSION-COMPLETE: ...]" for
;; the end of a task, "[WAITING-FOR-USER]" for a pause, one exact
;; string for a C-g.  Fourteen sites depended on those spellings, a
;; tool whose legitimate output began with "Error " was recorded as a
;; failure, and the completion regexp was greedy enough to swallow a
;; trailing "]" of the user's own text.
;;
;; `efrit-do--execute-tool' now returns an `efrit-tool-result'.  The
;; handlers still return strings (there are forty of them and their
;; text is what the model reads); the dispatcher is the one place that
;; classifies a handler's string into a status and a control signal.
;; The loop engine reads slots.  The string form is produced only at
;; the API boundary (`efrit-api-build-tool-result') and for display.
;;
;;   status   ok | error | denied | interrupted
;;   signal   nil | complete | waiting
;;   text     what the model is told (the handler's string)
;;   message  the completion message (signal complete), or the
;;            question (signal waiting)

;;; Code:

(require 'cl-lib)

(cl-defstruct (efrit-tool-result (:constructor efrit-tool-result-create)
                                 (:copier nil))
  (status 'ok)      ; ok | error | denied | interrupted
  (signal nil)      ; nil | complete | waiting
  (text "")         ; the tool_result text the model sees
  (message nil))    ; completion message or question, per signal

(defun efrit-tool-result-error-p (result)
  "Non-nil if RESULT is a failure of any kind (error, denied, interrupted)."
  (not (eq (efrit-tool-result-status result) 'ok)))

(defun efrit-tool-result-complete-p (result)
  "Non-nil if RESULT asks to end the task (session_complete)."
  (eq (efrit-tool-result-signal result) 'complete))

(defun efrit-tool-result-waiting-p (result)
  "Non-nil if RESULT pauses the turn for the user (request_user_input)."
  (eq (efrit-tool-result-signal result) 'waiting))

(defun efrit-tool-result-interrupted-p (result)
  "Non-nil if the user ended the tool with C-g."
  (eq (efrit-tool-result-status result) 'interrupted))

(defun efrit-tool-result-ok (text)
  "A successful RESULT carrying TEXT."
  (efrit-tool-result-create :status 'ok :text text))

(defun efrit-tool-result-fail (text &optional status)
  "A failed result carrying TEXT; STATUS defaults to `error'."
  (efrit-tool-result-create :status (or status 'error) :text text))

(provide 'efrit-result-struct)

;;; efrit-result-struct.el ends here
