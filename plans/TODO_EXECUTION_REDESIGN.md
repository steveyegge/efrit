# TODO Execution Workflow Redesign - ORACLE-VALIDATED APPROACH

## Problem Analysis - CONFIRMED BY ORACLE

### Root Cause (Oracle Insight)
The fundamental issue is **API contract ambiguity**: `todo_execute_next` sounds like an action tool but behaves like an information tool. This creates expectation mismatches that lead to loops.

### Current Flawed Workflow
```
User: "fix warnings"
→ todo_analyze: Creates TODOs
→ todo_execute_next: Returns instructions (misleading name!)
→ Claude calls todo_execute_next again expecting execution
→ Loop detection triggers: "todo_execute_next called 3rd time!"
→ Session fails
```

### Oracle-Confirmed Root Causes
1. **Misleading Function Name**: `todo_execute_next` implies execution but only returns instructions
2. **Pattern-Based Loop Detection**: Complex heuristics that are hard to maintain and reason about
3. **No Progress Tracking**: Loop detection infers progress from tool names rather than actual progress
4. **Documentation Gap**: Function behavior doesn't match name expectations

## ORACLE-RECOMMENDED SOLUTION: Instruction-Based + Better Naming

### Why NOT Auto-Execution (Oracle Analysis)
❌ **Auto-execution would VIOLATE Zero Client-Side Intelligence**
- Requires parsing natural language TODO content in Elisp
- Duplicates Claude-side reasoning in client
- Creates "two potentially divergent brains"
- Reintroduces forbidden intelligence into client

✅ **Instruction-based approach is architecturally sound**
- Respects ZCSI principle
- Keeps all cognition in Claude
- Maintains minimal blast radius

## NEW IMPLEMENTATION PLAN - ORACLE ENDORSED

### Phase 1: Function Rename (Non-Breaking)
1. **Add Alias**: `todo_get_instructions` that does what `todo_execute_next` currently does
2. **Deprecate**: `todo_execute_next` in docs but keep for backward compatibility  
3. **Update System Prompt**: Guide Claude to use the new, clearer name
4. **Clear Contract**: Document that `todo_get_instructions` returns strings, never executes

### Phase 2: Progress-Based Loop Detection (Oracle's Key Recommendation)
Replace complex pattern matching with simple progress tracking:

```elisp
(defun efrit-progress-made-p (session)
  "Return t if something material changed since last tool call."
  (or (efrit-session-last-buffer-modification-tick-changed-p session)
      (efrit-session-todo-status-changed-p session)
      (efrit-session-new-buffer-created-p session)))

(when (and (equal tool-name last-tool-called)
           (not (efrit-progress-made-p session)))
  (signal 'efrit-loop "Tool called twice without progress"))
```

**Benefits (Oracle)**:
- No need to hard-code tool-name patterns
- Future tools automatically participate in loop detection  
- Much easier to reason about behavior
- Eliminates "regular-expression firewall" complexity

### Phase 3: Workflow State Management
1. **Reset Logic**: Ensure workflow state resets when all TODOs complete
2. **Progress Flags**: Write flags when TODOs change status, buffers created, etc.
3. **Clear Signaling**: Make progress visible to loop detection system

### Phase 4: Documentation & Testing
1. **Update Design Docs**: Reflect instruction-based model retention
2. **AGENTS.md Update**: "todo_get_instructions returns guidance; never executes itself"
3. **Unit Tests**: Happy-path session + deliberate infinite-loop scenarios
4. **Integration Test**: Verify byte-compile warnings scenario completes

## ORACLE-RECOMMENDED SUCCESS METRICS
- [ ] Function renamed to `todo_get_instructions` with backward compatibility
- [ ] Progress-based loop detection replaces pattern-based heuristics
- [ ] Byte-compile warnings scenario completes without loops
- [ ] Loop detection only blocks when no actual progress is made
- [ ] Clear documentation distinguishes information tools from action tools
- [ ] Unit tests verify both happy path and deliberate loop scenarios

## IMPLEMENTATION PRIORITY (Oracle Order)
1. **IMMEDIATE**: Add `todo_get_instructions` alias and update system prompts
2. **HIGH**: Implement progress-based loop detection 
3. **MEDIUM**: Add progress tracking flags to session state
4. **LOW**: Update documentation and comprehensive testing

## Current Status vs Oracle Recommendations
✅ **Already Done**: Loop detection fix works but uses pattern-based approach
🔄 **Next**: Implement Oracle's cleaner progress-based approach  
📋 **Pending**: Function rename and documentation updates
