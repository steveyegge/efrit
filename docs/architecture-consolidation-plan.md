# Efrit Architecture Consolidation Plan

## Overview

This document outlines the plan to consolidate Efrit's multiple command interfaces into a unified, session-based execution model where Claude controls the flow of multi-step operations.

## Current State Analysis

### Existing Systems
1. **efrit-chat** - Interactive conversation interface (like ChatGPT)
   - Working well, no changes needed
   - Good for exploratory discussions and learning

2. **efrit-do** - Direct command execution
   - Most mature system with retry logic, context tracking, TODO management
   - Will become the primary command interface with session support

3. **efrit** - Agent-based command interface
   - Uses popup buffer UI and complex agent system
   - Redundant with efrit-do functionality
   - To be removed (no deprecation needed - single user)

### Key Design Principles
- **No client-side cognition** - Claude decides everything about flow and context
- **Single protocol** - Everything is elisp execution
- **Session continuity** - Multi-step operations maintain context automatically
- **Async by default** - Non-blocking UI for better user experience

## Architecture: Session-Based Execution

### Core Concept

Every interaction with Claude can be part of a multi-step session. Claude controls:
- Whether a command starts a new session or continues an existing one
- What context is needed at each step
- When to continue, complete, or request user input
- How to handle the work log and context accumulation

### Unified Response Protocol

Every response from Claude has the same structure:

```json
{
  "elisp": "(some-expression ...)",      // ALWAYS present - what to execute
  "session": {
    "id": "unique-session-id",
    "status": "continue"                 // or "complete", "need-input"  
  }
}
```

### Session Flow Example

```elisp
;; User command
(efrit-do "refactor this function to use async/await")

;; Claude response 1: Gather context
{
  "elisp": "(list :function (thing-at-point 'defun) :mode major-mode)",
  "session": {"id": "ref-123", "status": "continue"}
}

;; Efrit executes elisp, sends result back automatically

;; Claude response 2: More context needed  
{
  "elisp": "(list :imports (efrit-context-find-imports) :test-file (efrit-find-test-file))",
  "session": {"id": "ref-123", "status": "continue"}
}

;; Efrit executes, sends result back

;; Claude response 3: Perform refactoring
{
  "elisp": "(progn (goto-char ...) (delete-region ...) (insert ...) \"Refactored to async/await\")",
  "session": {"id": "ref-123", "status": "complete"}
}

;; Session complete
```

## Implementation Plan

### Session 1: Core Infrastructure

#### 1.1 Remove Redundant Systems
- Delete `efrit-command.el` and `efrit-agent.el`
- Remove `efrit` command entirely
- Clean up any references

#### 1.2 Session Data Structure
```elisp
(defstruct efrit-session
  id              ; Unique identifier from Claude
  work-log        ; List of (elisp . result) pairs
  start-time      ; When session started
  status)         ; 'active, 'waiting, 'complete

(defvar efrit-async--active-session nil
  "Currently active session.")

(defvar efrit-async--session-queue nil  
  "Queue of pending commands.")
```

#### 1.3 Async Request Infrastructure
```elisp
(defun efrit-async--api-request (data callback)
  "Send DATA to Claude API asynchronously, calling CALLBACK with response."
  (let ((url-request-method "POST")
        (url-request-extra-headers (efrit--api-headers))
        (url-request-data (json-encode data)))
    (url-retrieve efrit-api-url
                  (lambda (status)
                    (if-let ((error (plist-get status :error)))
                        (efrit-async--handle-error error)
                      (funcall callback (efrit--parse-response))))
                  nil t t)))
```

### Session 2: Session Protocol Implementation

#### 2.1 Main Entry Point
```elisp
(defun efrit-do (command)
  "Execute COMMAND with session-based async execution."
  (interactive "sCommand: ")
  
  ;; Build request including session context if active
  (let ((request-data
         `((command . ,command)
           ,@(when efrit-async--active-session
               `((session-id . ,(efrit-session-id efrit-async--active-session))
                 (work-log . ,(efrit-session--compress-log 
                               efrit-async--active-session)))))))
    
    (if efrit-async--active-session
        ;; Queue if session active (let Claude decide if it's related)
        (push command efrit-async--session-queue)
      ;; Start immediately if no active session
      (efrit-async--start-request request-data))))
```

#### 2.2 Response Handler
```elisp
(defun efrit-async--handle-response (response)
  "Handle RESPONSE from Claude - always execute elisp and check status."
  (let* ((elisp (alist-get 'elisp response))
         (session-info (alist-get 'session response))
         (session-id (alist-get 'id session-info))
         (status (alist-get 'status session-info))
         ;; Always execute the elisp
         (result (efrit-tools-eval-sexp elisp)))
    
    ;; Start session if needed
    (unless efrit-async--active-session
      (when session-id
        (setq efrit-async--active-session
              (make-efrit-session :id session-id 
                                  :start-time (current-time)
                                  :status 'active))))
    
    ;; Log the work
    (when efrit-async--active-session
      (push (cons elisp result) 
            (efrit-session-work-log efrit-async--active-session)))
    
    ;; Handle status
    (pcase status
      ("continue"
       (efrit-async--continue-session session-id result))
      
      ("complete"
       (efrit-async--complete-session session-id result))
      
      ("need-input"
       (setf (efrit-session-status efrit-async--active-session) 'waiting)))))
```

#### 2.3 Session Continuation
```elisp
(defun efrit-async--continue-session (session-id result)
  "Continue session with RESULT of last execution."
  (efrit-async--api-request
   `((session-id . ,session-id)
     (last-result . ,result)
     (work-summary . ,(efrit-session--compress-log efrit-async--active-session)))
   #'efrit-async--handle-response))

(defun efrit-session--compress-log (session)
  "Create compressed summary of work for Claude."
  (let ((log (efrit-session-work-log session)))
    `((step-count . ,(length log))
      (last-result . ,(cdr (car log)))
      (elapsed-time . ,(float-time (time-since (efrit-session-start-time session)))))))
```

### Session 3: User Experience

#### 3.1 Progress Feedback
```elisp
(defvar efrit-async-mode-line-string nil)

(defun efrit-async--show-progress (message)
  "Show progress MESSAGE in minibuffer and mode line."
  (setq efrit-async-mode-line-string 
        (when efrit-async--active-session
          (format "[Efrit: %s]" message)))
  (force-mode-line-update t)
  (message "Efrit: %s" message))

;; Add to mode-line
(add-to-list 'mode-line-misc-info 
             '(efrit-async-mode-line-string (" " efrit-async-mode-line-string " ")))
```

#### 3.2 Quick Commands
```elisp
(defun efrit-continue ()
  "Send simple continuation to active session."
  (interactive)
  (if efrit-async--active-session
      (efrit-do "continue")
    (message "No active Efrit session")))

(defun efrit-cancel ()
  "Cancel active session."
  (interactive)
  (when (and efrit-async--active-session
             (y-or-n-p "Cancel active Efrit session? "))
    (setq efrit-async--active-session nil)
    (efrit-async--process-queue)))

(global-set-key (kbd "C-c e c") 'efrit-continue)
(global-set-key (kbd "C-c e k") 'efrit-cancel)
```

#### 3.3 Session Status
```elisp
(defun efrit-status ()
  "Show current session status."
  (interactive)
  (if efrit-async--active-session
      (message "Efrit: %s (%.1fs, %d steps)"
               (efrit-session-id efrit-async--active-session)
               (float-time (time-since (efrit-session-start-time 
                                        efrit-async--active-session)))
               (length (efrit-session-work-log efrit-async--active-session)))
    (message "No active Efrit session")))
```

### Session 4: Context Utilities and Testing

#### 4.1 Context Gathering Utilities
```elisp
;; New file: efrit-context.el
(defun efrit-context-function-at-point ()
  "Get complete function at point with metadata."
  (save-excursion
    (let ((bounds (bounds-of-thing-at-point 'defun)))
      (when bounds
        `(:text ,(buffer-substring-no-properties (car bounds) (cdr bounds))
          :name ,(which-function)
          :line ,(line-number-at-pos (car bounds)))))))

(defun efrit-context-project-files (pattern)
  "Get project files matching PATTERN."
  (when-let ((root (locate-dominating-file default-directory ".git")))
    (directory-files-recursively root pattern)))

(defun efrit-context-recent-changes ()
  "Get recent buffer changes if available."
  (when (bound-and-true-p undo-tree-mode)
    (efrit-context--summarize-undo-tree)))
```

#### 4.2 System Prompt Updates
```elisp
(defun efrit--system-prompt ()
  "System prompt explaining session protocol."
  "You are Efrit, an Emacs assistant that executes elisp in sessions.

CRITICAL: Every response must be valid JSON with:
- elisp: Elisp expression to execute (REQUIRED)
- session: {id: 'unique-id', status: 'continue'|'complete'|'need-input'}

Session Protocol:
- You control whether operations need multiple steps
- 'continue' means you need more steps (Efrit will call back with result)
- 'complete' means the task is done
- 'need-input' means you need user clarification

The elisp you return is ALWAYS executed. Use it to:
- Gather context: (list :buffer (buffer-name) :mode major-mode)
- Perform actions: (progn (goto-char ...) (insert ...) ...)
- Get user input: (read-string \"Prompt: \")
- Show progress: (message \"Working on refactoring...\")

Example responses:
{\"elisp\": \"(list :function (thing-at-point 'defun))\", 
 \"session\": {\"id\": \"ref-123\", \"status\": \"continue\"}}

{\"elisp\": \"(progn (delete-region ...) (insert ...) 'done)\",
 \"session\": {\"id\": \"ref-123\", \"status\": \"complete\"}}")
```

## Benefits of This Design

1. **Claude Controls Everything** - No client-side language processing or flow decisions
2. **Unified Protocol** - Every response is elisp execution + session status
3. **Natural Multi-Step** - Complex operations work seamlessly
4. **Async Throughout** - UI never blocks
5. **Simple Mental Model** - User types command, Claude handles the rest

## Success Metrics

1. **No Client Logic** - Client only executes elisp and manages sessions
2. **Seamless Multi-Step** - Refactoring, debugging, etc. work naturally
3. **Responsive UI** - Async execution keeps Emacs usable
4. **Clear Progress** - User always knows what's happening
5. **Robust Queueing** - Multiple commands handled gracefully

## Migration Path

1. Remove `efrit` command and related files
2. Implement basic session structure
3. Add async API infrastructure  
4. Update prompts for session protocol
5. Add progress feedback and quick commands
6. Test with complex multi-step operations

## Timeline

- Session 1: 2-3 hours (cleanup and core infrastructure)
- Session 2: 3-4 hours (session protocol implementation)
- Session 3: 2-3 hours (user experience and feedback)
- Session 4: 2-3 hours (context utilities and testing)

Total: 10-13 hours across 4 focused sessions