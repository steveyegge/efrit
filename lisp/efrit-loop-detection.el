;;; efrit-loop-detection-fixed.el --- Progress-based loop detection for Efrit -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Efrit Contributors

;; This file is part of Efrit.

;;; Commentary:

;; This module implements progress-based loop detection as recommended by the Oracle.
;; Instead of counting tool names, it tracks actual state changes and work completion.

;;; Code:

(require 'cl-lib)

(defun efrit-loop--get-recent-progress (session steps)
  "Get progress ticks for the last STEPS tool calls in SESSION."
  (when session
    (let* ((history (efrit-session-tool-history session))
           (recent (cl-subseq history 0 (min steps (length history)))))
      (mapcar (lambda (entry) 
                (or (nth 2 entry) 0))  ; Third element is progress-tick
              recent))))

(defun efrit-loop--pending-work-count (session)
  "Count pending work items in SESSION (TODOs, warnings, etc)."
  (when session
    ;; Better heuristic: check if session has been doing TODO-related work
    ;; If so, assume work is pending unless explicitly marked complete
    (let* ((history (efrit-session-tool-history session))
           (recent-tools (mapcar #'car (cl-subseq history 0 (min 10 (length history)))))
           (has-todo-activity (cl-some (lambda (tool) 
                                        (member tool '("todo_analyze" "todo_execute_next" 
                                                      "todo_update" "todo_status")))
                                      recent-tools)))
      (cond
       ;; If session status is not active, no pending work
       ((not (eq (efrit-session-status session) 'active)) 0)
       ;; If no history, no pending work
       ((not history) 0)
       ;; If recent TODO activity, assume pending work
       (has-todo-activity 1)
       ;; Otherwise, only assume pending work if session is very new
       ((< (length history) 5) 1)
       ;; Default: no pending work
       (t 0)))))

(defun efrit-loop--check-progress-stall (session)
  "Check if SESSION is stalled (no progress despite having pending work).
Returns error message if stalled, nil otherwise."
  (when session
    (let* ((recent-progress (efrit-loop--get-recent-progress session 8))
           (pending-work (efrit-loop--pending-work-count session))
           (history-length (length (or (efrit-session-tool-history session) '()))))
      
      ;; Only check for stalls if we have enough history and pending work
      (when (and (>= (length recent-progress) 8)
                 (> pending-work 0)
                 (> history-length 10))  ; Don't trigger too early
        ;; Check if progress has stalled (no meaningful increase over window)
        (let ((min-progress (apply #'min recent-progress))
              (max-progress (apply #'max recent-progress)))
          (when (<= (- max-progress min-progress) 0)
            (format "🚨 NO PROGRESS DETECTED 🚨
Progress stalled (min=%d max=%d) for %d steps with %d pending tasks!
This indicates the system is stuck in a loop without making real progress.
REQUIRED: Call session_complete with current status!" 
                    min-progress max-progress (length recent-progress) pending-work)))))))

(defun efrit-loop--check-session-safety (session)
  "Check basic safety limits for SESSION.
Returns error message if limits exceeded, nil otherwise."
  (when session
    (let* ((continuation-count (or (efrit-session-continuation-count session) 0))
           (history-length (length (or (efrit-session-tool-history session) '()))))
      
      ;; Absolute safety parachute - use a much higher limit for legitimate workloads
      (when (> continuation-count 500)
        (format "🚨 SESSION SAFETY LIMIT EXCEEDED 🚨
%d total API calls > 500 absolute limit! EXECUTION BLOCKED!
This session has run too long and must be terminated."
                continuation-count)))))

(defun efrit-loop-check (session _tool-name)
  "Main loop detection function using progress-based approach.
SESSION is the current efrit session, _TOOL-NAME is the tool about to be called.
Returns error message if a loop is detected, nil if safe to continue."
  (or (efrit-loop--check-session-safety session)
      (efrit-loop--check-progress-stall session)))

(provide 'efrit-loop-detection)
;;; efrit-loop-detection-fixed.el ends here
