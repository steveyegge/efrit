# Investigation: efrit-chat Tool Protocol Bug ("Pushing a Rope")

**Date**: 2024-11-24
**Investigator**: Claude (Sonnet 4.5)
**Related Issues**: ef-rk0 (epic), ef-ajc, ef-7w9, ef-af8

## Problem Statement

User reported that efrit-chat exhibits "pushing a rope" behavior:

1. **First request**: "Write a function that computes fib(N) in the *scratch* buffer"
   - Pops to *scratch* buffer ✅
   - Does NOT insert the function ❌

2. **Second request** (retry): "Try again"
   - NOW it inserts the function ✅

3. **Third request**: "Evaluate the function with n=10"
   - Inserts `(fibonacci 10)` ❌
   - Does NOT evaluate the defun first ❌
   - Gets "undefined symbol" error ❌

## Root Cause Analysis

### The Anthropic Tool Use Protocol

Per [Anthropic's API documentation](https://docs.anthropic.com/en/docs/build-with-claude/tool-use), the correct tool use flow is:

1. **Send initial request** with tools defined in API call
2. **Claude responds** with `stop_reason: "tool_use"` and content blocks like:
   ```json
   {
     "type": "tool_use",
     "id": "toolu_01234567890",
     "name": "eval_sexp",
     "input": {"expr": "(+ 1 2)"}
   }
   ```
3. **Client executes tools** and sends results back:
   ```json
   {
     "role": "user",
     "content": [{
       "type": "tool_result",
       "tool_use_id": "toolu_01234567890",
       "content": "3"
     }]
   }
   ```
4. **Claude processes results** and continues or responds

### What efrit-chat Actually Does

#### Classic Chat Mode (`efrit--extract-content-and-tools`)

**Location**: `lisp/core/efrit-chat.el:493-552`

**What happens**:
```elisp
;; Line 525: Execute the tool
(let ((result (efrit-tools-eval-sexp expr)))
  (efrit-log-debug "Elisp result: %s" result)
  result)

;; Line 541-544: Only display errors to user
;; In chat mode, don't display tool results inline
;; Only show errors (buffer objects and nil results are suppressed)
(when (string-match-p "^Error:" result)
  (setq message-text (concat message-text "\n" result)))
```

**Problems**:
1. ❌ Never sends tool results back to Claude
2. ❌ Doesn't capture `tool_use_id` from response (line 514-515)
3. ❌ Claude has no idea if the tool succeeded

**Result**: From Claude's perspective, the tool call vanished into the void. Claude doesn't know if the code was inserted, so it acts confused on subsequent turns.

#### Streamlined Mode (`efrit-streamlined--continue-with-results`)

**Location**: `lisp/core/efrit-chat.el:854-882`

**What happens**:
```elisp
;; Line 822: Execute tools and collect results
(setq tool-results (efrit-streamlined--execute-tools tool-uses))

;; Line 863: Send back as plain text
(push `((role . "user") (content . ,clean-result)) result-messages)
```

Where `clean-result` is formatted as (line 927):
```elisp
(format "Tool result for %s: %s" tool-id result-str)
```

**Problems**:
1. ✅ DOES send results back to Claude
2. ❌ Uses WRONG format (plain text instead of tool_result blocks)
3. ⚠️ Claude receives garbled results that don't match the protocol

**Result**: Claude gets confused because the tool result format is non-standard. Works better than classic mode but still unreliable.

## The Three Bugs

### Bug #1: Classic Mode - No Tool Results Sent Back
**Issue**: ef-ajc
**Priority**: P0 (Critical)
**Location**: `efrit-chat.el:493-552`

Tool execution happens but results never return to Claude. This breaks the entire tool use protocol.

### Bug #2: Streamlined Mode - Wrong Format
**Issue**: ef-7w9
**Priority**: P0 (Critical)
**Location**: `efrit-chat.el:854-882`

Tool results sent as:
```elisp
{role: "user", content: "Tool result for toolu_xxx: 3"}
```

Should be:
```elisp
{
  role: "user",
  content: [{
    type: "tool_result",
    tool_use_id: "toolu_xxx",
    content: "3"
  }]
}
```

### Bug #3: Classic Mode - Missing tool_use_id
**Issue**: ef-af8
**Priority**: P0 (Critical)
**Location**: `efrit-chat.el:514-515`

Code extracts:
```elisp
(let* ((tool-name (gethash "name" item))
       (input (gethash "input" item)))
```

Should extract:
```elisp
(let* ((tool-name (gethash "name" item))
       (input (gethash "input" item))
       (tool-id (gethash "id" item)))  ;; MISSING!
```

## Why This Causes "Pushing a Rope"

Let's trace the fibonacci scenario:

### Turn 1: "Write fibonacci in *scratch*"

1. User sends message
2. Claude analyzes and decides to use tool:
   ```json
   {
     "type": "tool_use",
     "id": "toolu_abc123",
     "name": "eval_sexp",
     "input": {
       "expr": "(with-current-buffer \"*scratch*\" (insert \"(defun fibonacci (n) ...)\"))"
     }
   }
   ```
3. efrit-chat executes the elisp ✅
4. The code runs, *scratch* buffer gets the text ✅
5. **efrit-chat does NOT send tool_result back to Claude** ❌
6. Claude displays its text response to user
7. **Claude's internal state**: "I used a tool but never got confirmation. Did it work? 🤷"

### Turn 2: "Try again"

1. User sends "try again"
2. Claude's context:
   - Previous turn: I sent a tool_use request
   - Previous turn: I got no tool_result back
   - Conclusion: Maybe the tool didn't work?
3. Claude tries a different approach:
   - Maybe just explain the code in text?
   - Or try the tool again (sometimes works due to temperature randomness)
4. If lucky, this time it works (but only because the first one already worked!)

### Turn 3: "Evaluate with n=10"

1. User asks to evaluate
2. Claude's context:
   - I think I maybe inserted the code (not sure)
   - Now they want me to evaluate fibonacci(10)
3. Claude sends:
   ```elisp
   (insert "(fibonacci 10)\n")  ;; Inserts the CALL
   ```
4. But Claude DOESN'T send:
   ```elisp
   (eval '(fibonacci 10))  ;; Actually EVALUATE it
   ```
5. Why? Because Claude doesn't have proper feedback about what's been executed!

## The Fix

### Step 1: Capture tool_use_id (Bug #3)

```elisp
(when (string= type "tool_use")
  (let* ((tool-name (gethash "name" item))
         (input (gethash "input" item))
         (tool-id (gethash "id" item)))  ;; ADD THIS
    ...))
```

### Step 2: Build Proper tool_result Messages (Bugs #1 & #2)

After executing tools, construct:
```elisp
(defun efrit--build-tool-result-content (tool-id result)
  "Build a proper tool_result content block."
  `((type . "tool_result")
    (tool_use_id . ,tool-id)
    (content . ,(format "%s" result))))
```

### Step 3: Send Results Back Before Displaying to User

Instead of immediately displaying Claude's text response, first:

1. Execute all tools
2. Build tool_result message
3. **Send back to Claude** with role="user", content=[tool_result, ...]
4. Wait for Claude's next response
5. THEN display to user

This requires restructuring the response handling to support multi-turn tool use.

### Step 4: Handle Multiple Tools in One Response

Claude can send multiple tool_use blocks in a single response. We need to:
1. Collect ALL tool_use blocks
2. Execute ALL of them
3. Build array of tool_result blocks
4. Send ALL results back in one message

## Testing Strategy

Create test cases:

1. **Single tool use**:
   - Send: "Insert 'hello' into *scratch*"
   - Verify: (a) tool executes, (b) result sent back, (c) Claude confirms

2. **Multi-step with dependencies**:
   - Send: "Write fibonacci in *scratch*, then evaluate fib(10)"
   - Verify: Claude executes in correct order with proper tool result feedback

3. **Tool use with continuation**:
   - Send: "Count all TODO items in current buffer"
   - Verify: Claude uses tools, processes results, uses more tools if needed

## Verification Checklist

Before marking this fixed:

- [ ] Classic mode captures tool_use_id
- [ ] Classic mode sends tool_result back in correct format
- [ ] Streamlined mode uses correct tool_result format (not plain text)
- [ ] Both modes handle multiple tools in one response
- [ ] Both modes wait for Claude's next response before showing to user
- [ ] Fibonacci test case works end-to-end
- [ ] No regressions in simple queries (no tools)

## Related Files

- `lisp/core/efrit-chat.el` - Main chat interface (classic and streamlined)
- `lisp/core/efrit-tools.el` - Tool execution (eval_sexp, get_context)
- `lisp/interfaces/efrit-do.el` - Command interface (may have similar bugs?)

## References

- [Anthropic Tool Use Documentation](https://docs.anthropic.com/en/docs/build-with-claude/tool-use)
- [Claude API Messages Format](https://docs.anthropic.com/en/api/messages)
- GitHub Issue: TBD (once we file externally if needed)
