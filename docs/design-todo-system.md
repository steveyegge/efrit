# Efrit TODO System Design Proposal

## Dogfood Run #6 Summary

### Issues Discovered

1. **ef-t7f** [P1 BUG]: Response detail is lost - only shows "Command executed successfully"
   - Root cause: `efrit-remote-queue--execute-command` returns generic success when `efrit-execute` returns nil/empty
   - Impact: User has no visibility into what Efrit actually did

2. **ef-9jp** [P1 BUG]: Circuit breaker trips but session keeps calling tools
   - Root cause: Circuit breaker sets flag but doesn't force session termination
   - Impact: Can burn significant tokens on failed loops (seen 15+ calls after circuit breaker tripped)

3. **ef-utf** [P2 BUG]: TODO system not used unless explicitly requested
   - Root cause: System prompt doesn't encourage TODO usage for complex tasks
   - Impact: Multi-step tasks get compressed into single operations, losing visibility

4. **ef-a46** [P2 BUG]: session_complete message overwrites tool execution results
   - Root cause: `(or completion-message result-text)` prefers completion message
   - Impact: Detailed results are lost in favor of generic completion message

### Positive Findings

- When explicitly asked to use TODOs, Efrit does use them correctly
- The circuit breaker does detect loops (just doesn't stop them effectively)
- Simple commands work reliably (buffer_create, eval_sexp)
- Session logging captures useful debug information

---

## Research: How TodoWrite Works in Coding Agents

### Claude Code's TodoWrite Tool

I have direct experience with this tool - it's what I (as Claude Code) use to track tasks. Here's how it works:

#### Schema
```json
{
  "name": "TodoWrite",
  "parameters": {
    "todos": [
      {
        "content": "Task description (imperative form, e.g., 'Run tests')",
        "status": "pending|in_progress|completed",
        "activeForm": "Present continuous (e.g., 'Running tests')"
      }
    ]
  }
}
```

#### Key Design Principles

1. **Single Source of Truth**: The entire todo list is passed each time - NOT incremental updates
   - No `add_todo` or `update_todo` - just one `TodoWrite` that sets the whole list
   - This prevents sync issues and makes the state always explicit

2. **Status-Driven Display**: The UI shows todos with visual indicators
   - `pending`: Not started yet
   - `in_progress`: Currently working on (should be exactly ONE at a time)
   - `completed`: Done

3. **Dual Form Content**: Both imperative and active forms
   - `content`: "Run the build" (what needs to be done)
   - `activeForm`: "Running the build" (shown while in progress)
   - This enables better UX without client-side string manipulation

4. **User Visibility**: The todo list is displayed to the user in the UI
   - Provides transparency about what the agent is doing
   - Helps user understand progress on complex tasks

5. **When to Use**: The system prompt guides usage:
   - Complex multi-step tasks (3+ steps)
   - Tasks with multiple sub-items
   - When user provides a list of things to do
   - NOT for simple single-action tasks

### Why This Design Works

1. **No State Sync Bugs**: Because the whole list is passed each time, there's no drift between what the agent thinks the list is and what it actually is.

2. **Atomic Updates**: Each call fully defines the todo state - no partial updates.

3. **Self-Documenting**: The agent's todo list serves as a plan that the user can see.

4. **Progress Tracking**: User knows what's done and what's remaining.

5. **Prevents Wandering**: When the agent has an explicit list, it's less likely to go off on tangents.

---

## Proposed Design for Efrit

### Option A: Migrate to TodoWrite Pattern (Recommended)

Replace the current 8 TODO-related tools with a single `todo_write` tool:

```elisp
(defconst efrit-do--todo-write-schema
  '(("name" . "todo_write")
    ("description" . "Update the task list. ALWAYS pass the COMPLETE todo list.

Use this when:
- Breaking down a complex task into steps
- Starting work on a task (mark as in_progress)
- Completing a task (mark as completed)
- Adding new discovered work

DO NOT use for simple single-step tasks like opening files.")
    ("input_schema" .
     (("type" . "object")
      ("properties" .
       (("todos" .
         (("type" . "array")
          ("items" .
           (("type" . "object")
            ("properties" .
             (("content" .
               (("type" . "string")
                ("description" . "Task in imperative form (e.g., 'Run tests')")))
              ("status" .
               (("type" . "string")
                ("enum" . ["pending" "in_progress" "completed"])
                ("description" . "Current status")))
              ("activeForm" .
               (("type" . "string")
                ("description" . "Task in present continuous (e.g., 'Running tests')")))))
            ("required" . ["content" "status" "activeForm"])))))))
      ("required" . ["todos"])))))
```

#### Benefits:
- Matches proven pattern from Claude Code
- Simpler API surface (1 tool vs 8)
- No sync bugs between add/update operations
- User always sees complete state

#### Implementation:
1. Remove: `todo_add`, `todo_update`, `todo_show`, `todo_analyze`, `todo_status`, `todo_next`, `todo_execute_next`, `todo_complete_check`
2. Add: `todo_write`
3. Update system prompt to guide proper usage
4. Update UI to display todos with status indicators

### Option B: Fix Current System

Keep existing tools but fix the issues:

1. **Improve system prompt** to encourage TODO usage for complex tasks
2. **Fix circuit breaker** to force session termination when tripped
3. **Fix response handling** to include tool results in response
4. **Add todo validation** to prevent inconsistent states

This is less work but doesn't address the fundamental complexity of 8 tools vs 1.

---

## Recommendation

**Go with Option A (TodoWrite pattern)** because:

1. It's a proven pattern - Claude Code uses this successfully
2. Simpler API = fewer bugs
3. Full state replacement = no sync issues
4. Better aligns with "Pure Executor" philosophy (Claude decides state, Efrit executes)

---

## Implementation Plan

### Phase 1: Core TodoWrite Tool (ef-dc8)
1. Create new `todo_write` handler in efrit-do.el
2. Define schema with content/status/activeForm fields
3. Implement display function for TODO list
4. Update system prompt with usage guidance

### Phase 2: UI Integration
1. Create dedicated *efrit-todos* buffer with auto-update
2. Show current task in mode line
3. Visual indicators for status (pending/in_progress/completed)

### Phase 3: Deprecate Old Tools
1. Remove 8 old TODO tools from schema
2. Update documentation
3. Clean up efrit-do.el

### Phase 4: Fix Related Bugs
1. Fix response detail issue (ef-t7f)
2. Fix circuit breaker termination (ef-9jp)
3. Fix session_complete overwrite (ef-a46)

---

## Example Usage

User: "Refactor the login function to use async/await and add proper error handling"

Efrit calls `todo_write`:
```json
{
  "todos": [
    {"content": "Read current login function implementation",
     "status": "in_progress",
     "activeForm": "Reading current login function"},
    {"content": "Convert callbacks to async/await",
     "status": "pending",
     "activeForm": "Converting callbacks to async/await"},
    {"content": "Add try/catch error handling",
     "status": "pending",
     "activeForm": "Adding error handling"},
    {"content": "Test refactored function",
     "status": "pending",
     "activeForm": "Testing refactored function"}
  ]
}
```

After reading the function, Efrit calls `todo_write` again:
```json
{
  "todos": [
    {"content": "Read current login function implementation",
     "status": "completed",
     "activeForm": "Reading current login function"},
    {"content": "Convert callbacks to async/await",
     "status": "in_progress",
     "activeForm": "Converting callbacks to async/await"},
    {"content": "Add try/catch error handling",
     "status": "pending",
     "activeForm": "Adding error handling"},
    {"content": "Test refactored function",
     "status": "pending",
     "activeForm": "Testing refactored function"}
  ]
}
```

And so on until all tasks are completed.

---

## Open Questions

1. Should we persist TODO state across sessions? (Currently ephemeral)
2. Should we allow nested/hierarchical TODOs?
3. Should TODOs have IDs for referencing in responses?
4. How to handle user interruption mid-TODO?
