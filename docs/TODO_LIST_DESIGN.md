# Efrit TODO List System Design

## Problem Statement

Currently, efrit-do-async can get stuck in loops when performing complex multi-step tasks. For example, when fixing all warnings in a buffer, Claude doesn't know when the task is complete and keeps checking the same warnings repeatedly. 

## Solution Overview

Implement a TODO list system that:
1. Automatically creates TODO items for multi-step tasks
2. Tracks progress as each item is completed
3. Provides clear termination conditions for sessions
4. Prevents infinite loops by giving Claude visibility into remaining work

## Key Design Principles

### 1. Automatic TODO Generation
- When Claude receives a complex task, it should analyze it and create TODO items
- Each TODO represents a discrete, actionable step
- TODOs can be hierarchical (main task with subtasks)

### 2. Progress Tracking
- Each TODO has states: pending, in-progress, completed
- Only one TODO should be in-progress at a time
- Claude marks TODOs complete as work is finished

### 3. Session Termination
- Session completes when all TODOs are marked complete
- Claude checks TODO status before continuing
- Explicit completion criteria prevents loops

### 4. Work Log Integration
- TODO state is included in the work log
- Claude can see what's been done and what remains
- Provides context across session continuations

## Implementation Architecture

### Data Structure
```elisp
(cl-defstruct efrit-do-todo-item
  id          ; Unique identifier
  content     ; Task description
  status      ; 'pending, 'in-progress, 'completed
  priority    ; 'low, 'medium, 'high
  created-at  ; Timestamp
  completed-at ; Timestamp when completed
  parent-id   ; For hierarchical TODOs
  metadata)   ; Additional context
```

### Tool Extensions
1. **todo_analyze** - Analyze a command and generate TODO items
2. **todo_status** - Get current TODO list state
3. **todo_next** - Get next pending TODO item
4. **todo_complete_all** - Mark all related TODOs complete

### Session Protocol Updates

#### Initial Command Processing
```
1. Receive command
2. Use todo_analyze to break down into steps
3. Create TODO items for each step
4. Mark first TODO as in-progress
5. Execute first TODO
```

#### Session Continuation
```
1. Check TODO list status
2. If all complete -> session_complete
3. Else:
   - Complete current in-progress TODO
   - Mark next TODO as in-progress
   - Execute next TODO
```

### Example: Fix Warnings Flow

Command: "Fix all warnings in the buffer"

1. **Initial Analysis**
   ```
   TODO-1: Scan buffer for warnings
   TODO-2: Fix each warning found
   TODO-3: Verify all warnings resolved
   ```

2. **After Scanning** (finds 5 warnings)
   ```
   TODO-1: ✓ Scan buffer for warnings
   TODO-2: Fix lexical-binding warning in file1.el
   TODO-3: Fix lexical-binding warning in file2.el  
   TODO-4: Fix lexical-binding warning in file3.el
   TODO-5: Fix lexical-binding warning in file4.el
   TODO-6: Fix lexical-binding warning in file5.el
   TODO-7: Verify all warnings resolved
   ```

3. **Progress Tracking**
   - Claude works through each TODO
   - Marks complete as finished
   - When TODO-7 completes, session ends

## Benefits

1. **No More Loops** - Clear termination conditions
2. **Better Progress Visibility** - Users see what's happening
3. **Resumable Sessions** - Can pause/resume with TODO state
4. **Improved Planning** - Claude thinks through steps upfront
5. **Debugging** - Easy to see where things went wrong

## Integration Points

1. **efrit-do.el** - Core TODO management functions
2. **efrit-async.el** - TODO state in work log
3. **efrit-progress.el** - Display TODO progress
4. **efrit-protocol.el** - Include TODO state in API calls

## Migration Strategy

1. Phase 1: Implement TODO tools but make them optional
2. Phase 2: Update system prompts to encourage TODO usage
3. Phase 3: Make TODO creation mandatory for multi-step tasks
4. Phase 4: Add TODO-based session termination logic

## Future Enhancements

1. **Persistent TODOs** - Save/restore across Emacs sessions
2. **TODO Templates** - Common task patterns
3. **Parallel TODOs** - Mark which can be done concurrently
4. **TODO Dependencies** - Some TODOs require others first
5. **Time Estimates** - Predict completion time