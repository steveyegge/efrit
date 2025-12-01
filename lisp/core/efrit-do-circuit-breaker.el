;;; efrit-do-circuit-breaker.el --- Circuit breaker for efrit-do -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai
;; URL: https://github.com/stevey/efrit

;;; Commentary:
;; Circuit breaker implementation for efrit-do to prevent infinite loops
;; and runaway tool calls.  Includes error loop detection to catch repeated
;; identical errors.

;;; Code:

(require 'efrit-common)
(require 'efrit-log)
(require 'cl-lib)
(require 'seq)

;; Forward declaration for function defined in efrit-do.el
(declare-function efrit-do--extract-error-info "efrit-do")

;;; Customization

(defgroup efrit-do-circuit-breaker nil
  "Circuit breaker settings for efrit-do."
  :group 'efrit-do
  :prefix "efrit-do-")

(defcustom efrit-do-max-tool-calls-per-session 30
  "Maximum number of tool calls allowed in a single session.
When this limit is reached, the circuit breaker trips and the session is
terminated.  This prevents infinite loops from burning through API tokens."
  :type 'integer
  :group 'efrit-do-circuit-breaker)

(defcustom efrit-do-max-same-tool-calls 15
  "Maximum consecutive calls to the same tool before circuit breaker trips.
Set higher (15) to allow legitimate multi-step coding tasks that require
many eval_sexp calls in sequence (e.g., writing multiple functions)."
  :type 'integer
  :group 'efrit-do-circuit-breaker)

(defcustom efrit-do-max-identical-tool-calls 3
  "Maximum number of consecutive identical tool calls (same tool + same input).
This catches true infinite loops where the same operation is repeated.
Lower than `efrit-do-max-same-tool-calls' since identical calls are more
likely to indicate a loop than varied calls to the same tool."
  :type 'integer
  :group 'efrit-do-circuit-breaker)

(defcustom efrit-do-same-tool-warning-threshold 10
  "Warn after this many consecutive calls to the same tool.
Warning is sent before the hard limit at `efrit-do-max-same-tool-calls'."
  :type 'integer
  :group 'efrit-do-circuit-breaker)

(defcustom efrit-do-circuit-breaker-enabled t
  "Whether circuit breaker protection is enabled.
When non-nil, enforces hard limits on tool calls to prevent infinite loops.
Recommended to keep enabled for safety."
  :type 'boolean
  :group 'efrit-do-circuit-breaker)

(defcustom efrit-do-max-same-error-occurrences 3
  "Maximum times the same error can occur before triggering loop detection.
When this limit is reached, a warning message is injected telling Claude
to try a different approach.  Error signatures are tracked by hashing
the error message."
  :type 'integer
  :group 'efrit-do-circuit-breaker)

(defcustom efrit-do-error-loop-auto-complete 5
  "Auto-complete session after this many same-error occurrences.
When the same error occurs this many times, the session is automatically
terminated with an error summary.  Set to nil to disable auto-completion."
  :type '(choice integer (const nil))
  :group 'efrit-do-circuit-breaker)

;;; Circuit Breaker State

(defvar efrit-do--session-tool-count 0
  "Total number of tool calls in the current session.
Reset when a new session starts.  Used by circuit breaker to prevent
runaway loops.")

(defvar efrit-do--circuit-breaker-tripped nil
  "When non-nil, circuit breaker has tripped and session is terminated.
Stores reason for tripping as a string.")

(defvar efrit-do--last-tool-called nil
  "Track the last tool that was called.")

(defvar efrit-do--tool-call-count 0
  "Count consecutive calls to the same tool.")

(defvar efrit-do--last-tool-input nil
  "Track the last tool input hash for detecting identical calls.")

(defvar efrit-do--identical-call-count 0
  "Count consecutive identical tool calls (same tool + same input).")

;;; Error Loop Detection State

(defvar efrit-do--last-error-hash nil
  "Hash of the most recent error message for loop detection.")

(defvar efrit-do--same-error-count 0
  "Count of consecutive occurrences of the same error.")

(defvar efrit-do--error-history nil
  "List of recent error signatures for pattern analysis.
Each entry is (error-hash . error-message).")

;;; Error Loop Detection Functions

(defun efrit-do--hash-error-message (error-msg)
  "Create a hash string for ERROR-MSG to detect repeated errors.
Normalizes the error message to ignore minor variations like timestamps
or memory addresses that might differ between identical errors."
  (when error-msg
    (let ((normalized (replace-regexp-in-string
                       ;; Remove hex addresses like #<buffer 0x12345>
                       "#<[^>]+>"
                       "#<normalized>"
                       (replace-regexp-in-string
                        ;; Remove line numbers that might vary
                        "line [0-9]+"
                        "line N"
                        error-msg))))
      (secure-hash 'md5 normalized))))

(defun efrit-do--error-loop-reset ()
  "Reset error loop detection state for a new session."
  (setq efrit-do--last-error-hash nil)
  (setq efrit-do--same-error-count 0)
  (setq efrit-do--error-history nil))

(defun efrit-do--error-loop-record (error-msg)
  "Record ERROR-MSG for error loop detection.
Returns (WARNING-P . MESSAGE) where WARNING-P indicates an error loop detected."
  (when (and error-msg (stringp error-msg))
    (let* ((error-hash (efrit-do--hash-error-message error-msg))
           (is-same-error (and error-hash
                               efrit-do--last-error-hash
                               (equal error-hash efrit-do--last-error-hash))))
      ;; Update tracking state
      (if is-same-error
          (setq efrit-do--same-error-count (1+ efrit-do--same-error-count))
        (setq efrit-do--same-error-count 1))
      (setq efrit-do--last-error-hash error-hash)

      ;; Add to history (keep last 10)
      (push (cons error-hash (substring error-msg 0 (min 100 (length error-msg))))
            efrit-do--error-history)
      (when (> (length efrit-do--error-history) 10)
        (setq efrit-do--error-history (seq-take efrit-do--error-history 10)))

      ;; Check for error loop conditions
      (cond
       ;; Auto-complete threshold reached - trip the circuit breaker
       ((and efrit-do-error-loop-auto-complete
             (>= efrit-do--same-error-count efrit-do-error-loop-auto-complete))
        (setq efrit-do--circuit-breaker-tripped
              (format "Error loop detected: Same error occurred %d times. Error: %s"
                      efrit-do--same-error-count
                      (substring error-msg 0 (min 150 (length error-msg)))))
        (efrit-log 'error "Error loop auto-complete: %s" efrit-do--circuit-breaker-tripped)
        (cons 'auto-complete efrit-do--circuit-breaker-tripped))

       ;; Warning threshold reached - inject guidance
       ((>= efrit-do--same-error-count efrit-do-max-same-error-occurrences)
        (let ((warning (format "[ERROR LOOP DETECTED: This error has occurred %d times. The same approach keeps failing. You MUST try a COMPLETELY DIFFERENT approach. Consider:\n- Different function or API\n- Alternative algorithm\n- Simplified approach\n- Ask user for clarification\nDO NOT retry the same code pattern.]"
                               efrit-do--same-error-count)))
          (efrit-log 'warn "Error loop warning: error occurred %d times" efrit-do--same-error-count)
          (cons 'warning warning)))

       ;; No loop detected yet
       (t (cons nil nil))))))

(defun efrit-do--error-loop-check-result (result)
  "Check RESULT for errors and track for loop detection.
Returns (MODIFIED-P . NEW-RESULT) where MODIFIED-P indicates if result
was modified with an error loop warning."
  (let* ((error-info (efrit-do--extract-error-info result))
         (is-error (car error-info))
         (error-msg (cdr error-info)))
    (if (not is-error)
        ;; No error - reset the same-error counter (but keep last hash)
        (progn
          (setq efrit-do--same-error-count 0)
          (cons nil result))
      ;; Error detected - record and check for loop
      (let ((loop-result (efrit-do--error-loop-record error-msg)))
        (pcase (car loop-result)
          ('auto-complete
           ;; Session should be terminated
           (cons t (concat result "\n\n" (cdr loop-result))))
          ('warning
           ;; Inject warning into result
           (cons t (concat result "\n\n" (cdr loop-result))))
          (_
           ;; No loop detected
           (cons nil result)))))))

;;; Circuit Breaker Implementation

(defun efrit-do--circuit-breaker-reset ()
  "Reset circuit breaker state for a new session."
  (setq efrit-do--session-tool-count 0)
  (setq efrit-do--circuit-breaker-tripped nil)
  (setq efrit-do--last-tool-called nil)
  (setq efrit-do--tool-call-count 0)
  (setq efrit-do--last-tool-input nil)
  (setq efrit-do--identical-call-count 0)
  ;; Reset error loop detection state
  (efrit-do--error-loop-reset))

(defun efrit-do--hash-tool-input (tool-input)
  "Create a hash string for TOOL-INPUT to detect identical calls.
For hash tables, sorts keys to ensure consistent ordering."
  (let ((normalized (efrit-do--normalize-for-hash tool-input)))
    (secure-hash 'md5 (format "%S" normalized))))

(defun efrit-do--normalize-for-hash (obj)
  "Normalize OBJ for consistent hashing, handling hash table key order.
Converts hash tables to sorted alists for order-independent comparison."
  (cond
   ((hash-table-p obj)
    ;; Convert to alist with sorted keys for consistent ordering
    (let ((pairs '()))
      (maphash (lambda (k v)
                 (push (cons k (efrit-do--normalize-for-hash v)) pairs))
               obj)
      (sort pairs (lambda (a b)
                    (string< (format "%S" (car a))
                            (format "%S" (car b)))))))
   ((vectorp obj)
    (vconcat (mapcar #'efrit-do--normalize-for-hash obj)))
   ((listp obj)
    (mapcar #'efrit-do--normalize-for-hash obj))
   (t obj)))

(defun efrit-do--circuit-breaker-check-limits (tool-name &optional tool-input)
  "Check circuit breaker limits before executing TOOL-NAME with TOOL-INPUT.
Returns (ALLOWED-P . MESSAGE) where ALLOWED-P is t if execution should proceed.
MESSAGE may contain a warning even when ALLOWED-P is t.
Uses efrit--safe-execute for error handling."
  (if (not efrit-do-circuit-breaker-enabled)
      (cons t nil) ; Circuit breaker disabled
    (let* ((input-hash (when tool-input (efrit-do--hash-tool-input tool-input)))
           (is-same-tool (string= tool-name efrit-do--last-tool-called))
           (is-identical-call (and is-same-tool
                                   input-hash
                                   (equal input-hash efrit-do--last-tool-input)))
           (result
            (efrit--safe-execute
             (lambda ()
               (cond
                ;; Already tripped - block all further calls
                (efrit-do--circuit-breaker-tripped
                 (cons nil (format "Circuit breaker active: %s"
                                   efrit-do--circuit-breaker-tripped)))

                ;; Check session-wide limit
                ((>= efrit-do--session-tool-count efrit-do-max-tool-calls-per-session)
                 (setq efrit-do--circuit-breaker-tripped
                       (format "Session limit reached: %d/%d tool calls. Last tool: %s (consecutive: %d)"
                               efrit-do--session-tool-count
                               efrit-do-max-tool-calls-per-session
                               (or efrit-do--last-tool-called "none")
                               efrit-do--tool-call-count))
                 (cons nil efrit-do--circuit-breaker-tripped))

                ;; Check identical call limit (same tool + same input) - this catches true loops
                ((and is-identical-call
                      (>= efrit-do--identical-call-count efrit-do-max-identical-tool-calls))
                 (setq efrit-do--circuit-breaker-tripped
                       (format "Identical tool call detected: '%s' called %d times with same input (limit: %d). This appears to be an infinite loop."
                               tool-name
                               (1+ efrit-do--identical-call-count)
                               efrit-do-max-identical-tool-calls))
                 (efrit-log 'warn "Circuit breaker tripped: %s" efrit-do--circuit-breaker-tripped)
                 (cons nil efrit-do--circuit-breaker-tripped))

                ;; Check same-tool hard limit
                ((and is-same-tool
                      (>= efrit-do--tool-call-count efrit-do-max-same-tool-calls))
                 (setq efrit-do--circuit-breaker-tripped
                       (format "Same tool '%s' called %d times consecutively (limit: %d)"
                               tool-name
                               (1+ efrit-do--tool-call-count)
                               efrit-do-max-same-tool-calls))
                 (efrit-log 'warn "Circuit breaker tripped: %s" efrit-do--circuit-breaker-tripped)
                 (cons nil efrit-do--circuit-breaker-tripped))

                ;; Check same-tool warning threshold - allow but warn
                ((and is-same-tool
                      (>= efrit-do--tool-call-count efrit-do-same-tool-warning-threshold))
                 (cons t (format "[WARNING: '%s' called %d times consecutively. Consider varying your approach. Hard limit at %d calls.]"
                                 tool-name
                                 (1+ efrit-do--tool-call-count)
                                 efrit-do-max-same-tool-calls)))

                ;; All checks passed
                (t (cons t nil))))
             "circuit breaker check"
             "Check efrit-do-circuit-breaker-enabled and tool call limits")))
      (if (car result)
          (cdr result) ; Return the check result
        ;; Safe-execute caught an error - treat as breaker trip
        (progn
          (setq efrit-do--circuit-breaker-tripped (cdr result))
          (cons nil (format "Circuit breaker error: %s" (cdr result))))))))

(defun efrit-do--circuit-breaker-record-call (tool-name &optional tool-input)
  "Record a tool call for TOOL-NAME with TOOL-INPUT in circuit breaker tracking.
Updates counters and session tracking. Uses efrit--safe-execute for safety."
  (when efrit-do-circuit-breaker-enabled
    (efrit--safe-execute
     (lambda ()
       (let ((input-hash (when tool-input (efrit-do--hash-tool-input tool-input))))
         ;; Update session-wide counter
         (cl-incf efrit-do--session-tool-count)

         ;; Update same-tool counter
         (if (string= tool-name efrit-do--last-tool-called)
             (cl-incf efrit-do--tool-call-count)
           (setq efrit-do--tool-call-count 1))

         ;; Update identical-call counter (same tool + same input)
         (if (and (string= tool-name efrit-do--last-tool-called)
                  input-hash
                  (equal input-hash efrit-do--last-tool-input))
             (cl-incf efrit-do--identical-call-count)
           (setq efrit-do--identical-call-count 1))

         ;; Update last tool and input
         (setq efrit-do--last-tool-called tool-name)
         (setq efrit-do--last-tool-input input-hash)

         ;; Track in session
         (when (fboundp 'efrit-session-track-tool-used)
           (efrit-session-track-tool-used tool-name))

         ;; Log the call
         (efrit-log 'debug "Circuit breaker: tool=%s count=%d/%d same=%d/%d identical=%d/%d"
                    tool-name
                    efrit-do--session-tool-count
                    efrit-do-max-tool-calls-per-session
                    efrit-do--tool-call-count
                    efrit-do-max-same-tool-calls
                    efrit-do--identical-call-count
                    efrit-do-max-identical-tool-calls)))
     "recording tool call"
     "Check circuit breaker state and session tracking")))

(provide 'efrit-do-circuit-breaker)
;;; efrit-do-circuit-breaker.el ends here
