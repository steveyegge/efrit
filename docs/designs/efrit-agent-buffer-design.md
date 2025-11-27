# `*efrit-agent*` Buffer Design

## Overview

The `*efrit-agent*` buffer provides a structured, real-time view of agentic Efrit sessions. Unlike the raw `*Efrit Progress*` buffer which shows all events linearly, this buffer organizes information for interactive agentic workflows.

## Design Principles

1. **Show what matters** - Task progress, current action, errors
2. **Hide complexity** - Collapse tool details by default
3. **Enable interaction** - User can interrupt, redirect, provide input
4. **Stay synchronized** - Real-time updates from progress events

## Buffer Layout

```
╭─────────────────────────────────────────────────────────────╮
│ 🤖 Efrit Agent Session: async-20241127001234               │
│ Command: Build focus-mode.el package                        │
│ Status: ● Working (3.2s)                     [Cancel] [Pause]│
╰─────────────────────────────────────────────────────────────╯

━━━ Tasks (3/5 complete) ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
  ✓ Create basic package structure
  ✓ Add margin centering functionality
  ▶ Add word count to mode line           ← current
  ○ Test the complete package
  ○ Save to /tmp/focus-mode.el

━━━ Activity ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
[00:01] 🔧 eval_sexp → Created buffer *focus-mode-dev*
[00:03] 🔧 eval_sexp → Evaluated package structure
[00:05] 🔧 eval_sexp → Added margin centering
[00:07] 💬 Claude: "Now adding word count functionality..."
[00:08] 🔧 eval_sexp → ...                          [▼ expand]

━━━ Input ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
Type message to Claude (C-c C-s to send, C-c C-c to cancel):
> _
```

## Sections

### 1. Header Section
- Session ID and original command
- Status indicator with elapsed time:
  - `● Working` (green, animated)
  - `⏸ Paused` (yellow)
  - `⏳ Waiting for input` (blue, pulsing)
  - `✓ Complete` (green)
  - `✗ Failed` (red)
- Action buttons: [Cancel] [Pause] [Show Progress]

### 2. Tasks Section
Shows the current TODO list from `efrit-do--current-todos`:
- `✓` completed (dimmed)
- `▶` in-progress (highlighted, current marker)
- `○` pending

Updates in real-time as Claude progresses through tasks.

### 3. Activity Section
Summarized activity log:
- Tool calls shown as single lines with expandable details
- Claude's text messages shown with 💬 prefix
- Errors highlighted in red with ❌
- Timestamps relative to session start

**Expansion:** Press RET or click `[▼ expand]` to show:
- Full tool input
- Complete result
- Execution time

### 4. Input Section
When session is waiting for user input:
- Shows the question being asked
- Provides completion options if applicable
- Standard editing with `C-c C-s` to send

During active execution:
- Allows typing messages that get injected as guidance
- `C-c C-c` to cancel current operation

## Key Bindings

| Key | Action |
|-----|--------|
| `q` | Quit buffer (doesn't cancel session) |
| `k` | Kill/cancel session |
| `p` | Pause session |
| `r` | Resume paused session |
| `g` | Refresh display |
| `RET` | Expand/collapse tool details at point |
| `TAB` | Next section |
| `C-c C-s` | Send input to Claude |
| `C-c C-c` | Cancel current operation |
| `v` | Toggle verbosity (minimal/normal/verbose) |
| `?` | Show help |

## Data Sources

The buffer pulls from:

1. **efrit-progress events** (via `efrit-progress--emit-event`):
   - `session-start`, `session-end`
   - `tool-start`, `tool-result`
   - `text` (Claude messages)

2. **efrit-do TODO tracking** (via `efrit-do--current-todos`):
   - `efrit-do-todo-item` structs with status, content, activeForm

3. **efrit-session state** (via `efrit-session-active`):
   - Waiting for user flag
   - Pending question
   - Continuation count

## Implementation Approach

### Phase 1: Basic Structure
1. Create `efrit-agent.el` in `lisp/interfaces/`
2. Define `efrit-agent-mode` derived from `special-mode`
3. Implement header rendering with status
4. Hook into `efrit-progress-start-session` to auto-open

### Phase 2: Live Updates
1. Subscribe to progress events (hook or advice on emit functions)
2. Implement activity section with live appending
3. Add tool call expansion/collapse

### Phase 3: Task Integration
1. Hook into `efrit-do--update-todo-status`
2. Render tasks section with progress indicator
3. Highlight current task

### Phase 4: Input Handling
1. Detect `waiting-for-user` state
2. Render input prompt with question
3. Wire up `efrit-executor-respond` on submit
4. Support injection for guidance during execution

## Integration Points

```elisp
;; In efrit-do.el or efrit-executor.el:
(defun efrit-agent-start-session (session-id command)
  "Called when agentic session starts."
  (efrit-agent--create-buffer session-id command)
  (efrit-agent--show-buffer))

;; Hook into progress events:
(add-hook 'efrit-progress-event-hook #'efrit-agent--handle-event)

;; Hook into TODO updates:
(advice-add 'efrit-do--update-todo-status :after #'efrit-agent--refresh-tasks)
```

## Open Questions

1. **Auto-show vs opt-in**: Should the agent buffer appear automatically for `efrit-do`, or require explicit invocation like `M-x efrit-agent`?
   - **Recommendation**: Auto-show for multi-step tasks (when TODO list is created), opt-in for simple commands.

2. **Relationship to `*efrit-do*`**: Should `*efrit-agent*` replace `*efrit-do*` buffer, or coexist?
   - **Recommendation**: Coexist. `*efrit-do*` remains the detailed log, `*efrit-agent*` is the structured view.

3. **Buffer persistence**: Should the buffer persist after session ends?
   - **Recommendation**: Yes, with final status. User can dismiss with `q`.

4. **Multiple sessions**: What if user runs another `efrit-do` while one is active?
   - **Recommendation**: Queue (already implemented), show queue position in header.

## Success Criteria

A successful implementation means during dogfood testing:
1. User can see exactly what Efrit is doing at any moment
2. Task progress is visible and accurate
3. Errors are immediately visible with context
4. User can interrupt and redirect without losing progress
5. The experience feels like working *with* an agent, not waiting for it

## Files to Create/Modify

- **Create**: `lisp/interfaces/efrit-agent.el` (new module)
- **Modify**: `lisp/core/efrit-progress.el` (add hook mechanism)
- **Modify**: `lisp/interfaces/efrit-do.el` (hook TODO updates)
- **Modify**: `lisp/core/efrit-executor.el` (trigger buffer on start)
