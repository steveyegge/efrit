;;; test-interruption-manual.el --- Manual testing guide for C-g interruption -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Steve Yegge

;; Author: Steve Yegge <steve.yegge@gmail.com>
;; Version: 0.4.1
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, convenience, ai

;;; Commentary:

;; Manual testing procedures for C-g (keyboard-quit) interruption handling.
;; These are NOT automated tests but step-by-step verification procedures.

;;; Code:

;;; Interruption Verification Checklist
;;
;; BEFORE STARTING:
;; - Have Emacs running with Efrit loaded
;; - Have a Claude API key configured
;; - Open a file or buffer to work with
;;
;; TEST 1: Basic Interruption (Session Still Running)
;; ====================================================
;; 1. Start a long-running command: M-x efrit-do RET
;; 2. Type a long command, e.g.:
;;    "Create 20 TODO items with numbers 1-20"
;; 3. Wait for execution to start (progress buffer should appear)
;; 4. While API call is in progress, press C-g
;; 5. VERIFY:
;;    - Progress buffer shows "interrupt requested"
;;    - Execution stops cleanly (no error messages)
;;    - The "*efrit-progress-*" buffer is archived
;;    - No "ghost" session lingers
;;    - Next command can be started normally
;;
;; TEST 2: Interruption During Tool Execution
;; =============================================
;; 1. Start another long command
;; 2. Wait for a tool to start executing
;; 3. Press C-g DURING the tool execution
;; 4. VERIFY:
;;    - Tool execution stops (or completes but is ignored)
;;    - Session marks as interrupted
;;    - Progress buffer archives cleanly
;;    - Next command works normally
;;
;; TEST 3: Multiple Interruptions
;; ===============================
;; 1. Start command 1
;; 2. While running, press C-g (interrupt)
;; 3. Immediately start command 2
;; 4. While command 2 is running, press C-g again
;; 5. VERIFY:
;;    - Each command interrupts cleanly
;;    - No state leaks between sessions
;;    - Each has its own progress buffer (archived when done)
;;    - Third command can start normally
;;
;; TEST 4: No Error on Clean Interruption
;; =======================================
;; 1. Start a command
;; 2. Press C-g
;; 3. VERIFY:
;;    - No error message in *Messages* buffer (except "Interrupted" message)
;;    - No stack trace in log
;;    - Progress buffer shows graceful completion
;;    - No "Quit" errors in the message buffer
;;
;; TEST 5: State After Interruption
;; =================================
;; Use M-x efrit-dashboard to check session state
;; After interrupting a session, verify:
;; - Session status shows "interrupted" or "complete"
;; - Tool history is preserved (what was executed before interrupt)
;; - Work log shows all completed tools
;; - Buffers modified count reflects actual changes
;; - No orphaned async processes
;;
;; TEST 6: API Call Cancellation
;; =============================
;; Monitor your Claude API usage dashboard while testing.
;; After interrupting:
;; - Completed API calls should be visible in usage
;; - No pending requests should remain
;; - Token count should be reasonable (not excessive)

;;; Implementation Details (for developers)
;;
;; How Interruption Works:
;;
;; 1. User presses C-g in progress buffer
;;    → efrit-progress--interrupt() called
;;    → Sets interrupt-requested flag on session
;;
;; 2. Next iteration check in async loop
;;    → efrit-do-async--continue-iteration() checks flag
;;    → If set: clears flag, sends completion message to Claude
;;    → Calls efrit-do-async--stop-loop() with "interrupted" reason
;;
;; 3. Stop loop function:
;;    → Calls efrit-session-complete() (marks session complete)
;;    → Fires "interrupted" progress event
;;    → Archives progress buffer
;;    → Removes from active loops hash
;;    → Calls completion callback
;;
;; 4. Session is marked as complete and cleaned up
;;    → Next command can use a fresh session
;;    → No state leaks between sessions

(provide 'test-interruption-manual)

;;; test-interruption-manual.el ends here
