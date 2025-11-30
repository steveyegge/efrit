# Efrit Agent Buffer Design: Conversation-First Architecture

> **Status:** Draft v2.0
> **Last Updated:** 2025-11-29
> **Epic:** ef-e8n

## Executive Summary

The `*efrit-agent*` buffer should feel like **chatting with a colleague who shows their work**, not like watching a dashboard. We embrace Claude Code's conversational UX while adding Emacs-native superpowers.

**Core Principle:** Conversation first, structure as enhancement.

## Vision

```
┌─────────────────────────────────────────────────────────────────┐
│ ◉ Working │ 1:23 │ auth.el │ 3 tools │ [Pause] [Cancel]        │ <- header-line
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│ > Please refactor the login function to use async/await         │
│                                                                 │
│ I'll refactor the login function. Let me first read the         │
│ current implementation.                                         │
│                                                                 │
│   ▶ Read lisp/auth.el:45-80 ✓ (0.3s)                           │
│                                                                 │
│ I see the current code uses callbacks. I'll convert it to       │
│ async/await style.                                              │
│                                                                 │
│   ▼ Edit lisp/auth.el ✓ (0.5s)                                 │
│   │ @@ -45,5 +45,8 @@                                           │
│   │ -(defun login (user pass callback)                          │
│   │ -  (auth-request user pass callback))                       │
│   │ +(defun login (user pass)                                   │
│   │ +  "Login USER with PASS asynchronously."                   │
│   │ +  (async-let* ((result (auth-call user pass)))             │
│   │ +    result))                                               │
│   └────────────────────────────────                             │
│                                                                 │
│ Done! The function now returns a promise instead of using       │
│ callbacks. Want me to update the callers?                       │
│                                                                 │
├─────────────────────────────────────────────────────────────────┤
│ > _                                                             │
└─────────────────────────────────────────────────────────────────┘
```

## Design Principles

### 1. Conversation is King
- Messages flow naturally down the buffer
- User input prefixed with `> `
- Claude's responses appear inline
- Tool calls are inline elements, not separate sections

### 2. Progressive Disclosure
- Tool calls start collapsed: `▶ Read auth.el ✓ (0.3s)`
- Expand inline with RET to see details
- Verbosity levels control default expansion
- Errors always expanded

### 3. Always Visible Context
- Header-line shows status, elapsed time, current file, tool count
- Input area at bottom, always accessible
- No need to scroll to see where you are

### 4. Incremental Updates (No Flicker)
- Append-only for conversation flow
- Update in-place for tool results
- Never full-buffer re-render during active session
- Use text properties and overlays, not re-generation

### 5. Streaming Native
- Characters appear as Claude generates them
- Thinking indicators during pauses
- Smooth typing feel like terminal

## Buffer Architecture

### Layout Regions

```
┌─────────────────────────────────────┐
│         [header-line-format]        │  <- Emacs header-line, not buffer text
├─────────────────────────────────────┤
│                                     │
│        Conversation Region          │  <- Main scrolling content
│        (read-only, append-only)     │
│                                     │
├─────────────────────────────────────┤
│         Input Region                │  <- Editable, at point-max
│         (editable)                  │
└─────────────────────────────────────┘
```

### Region Implementation

**Conversation Region:**
- Read-only via text property `(read-only t)`
- Append new content at end (before input region)
- Tool calls use overlays for expand/collapse
- Marker `efrit-agent--conversation-end` tracks boundary

**Input Region:**
- Starts after `efrit-agent--input-start` marker
- Uses `efrit-agent-input-mode` minor mode
- Different keymap for editing
- Submit with `C-c C-c` or `RET` (configurable)

### Text Properties Schema

```elisp
;; User message
'(efrit-type user-message
  efrit-id "msg-001"
  read-only t
  face efrit-agent-user-message)

;; Claude message
'(efrit-type claude-message
  efrit-id "msg-002"
  efrit-streaming t  ; still receiving
  read-only t
  face efrit-agent-claude-message)

;; Tool call (collapsed)
'(efrit-type tool-call
  efrit-id "tool-003"
  efrit-tool-name "read_file"
  efrit-tool-input ((:path . "auth.el"))
  efrit-tool-result "..."
  efrit-tool-success t
  efrit-tool-elapsed 0.3
  efrit-expanded nil
  read-only t)

;; Tool call (expanded) - uses overlay for expansion content
```

## Component Design

### 1. Header Line

```elisp
(setq header-line-format
      '(:eval (efrit-agent--format-header-line)))

(defun efrit-agent--format-header-line ()
  "Format: ◉ Working │ 1:23 │ auth.el │ 3 tools │ [Pause] [Cancel]"
  ...)
```

**Elements:**
- Status indicator (◉/◯/⏸/✓/✗)
- Elapsed time (updates every second via timer)
- Current context (file being worked on)
- Tool count
- Action buttons (clickable)

### 2. Conversation Flow

**User Message:**
```
> This is what the user typed
  Can be multi-line
```

**Claude Response:**
```
Here's Claude's response. It can include:
- Formatted text
- Code blocks (with syntax highlighting)
- Questions with options
```

**Tool Call (Collapsed):**
```
  ▶ read_file lisp/auth.el ✓ (0.3s)
```

**Tool Call (Expanded):**
```
  ▼ read_file lisp/auth.el ✓ (0.3s)
  │ Input:  {:path "lisp/auth.el" :lines [45 80]}
  │ Result: (defun login (user pass callback) ...
  │         ... 35 lines ...
  └────────────────────────────────────
```

**Tool Call (Running):**
```
  ⟳ edit_file lisp/auth.el... (2.1s)
```

**Tool Call (Failed):**
```
  ✗ write_file /etc/passwd failed (0.1s)
  │ Error: Permission denied
  │ [Retry] [Skip] [Abort]
  └────────────────────────────────────
```

### 3. Diff Display for Edits

When a tool modifies a file, show inline diff:

```
  ▼ Edit lisp/auth.el ✓ (0.5s)
  │ @@ -45,5 +45,8 @@
  │ -(defun login (user pass callback)
  │ -  (auth-request user pass callback))
  │ +(defun login (user pass)
  │ +  "Login USER with PASS asynchronously."
  │ +  (async-let* ((result (auth-call user pass)))
  │ +    result))
  └────────────────────────────────────
```

Use `diff-mode` faces for coloring.

### 4. Input Area

**Normal state:**
```
> _
```

**When Claude asks a question:**
```
┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄
Which authentication method should we use?
  [1] OAuth    [2] JWT    [3] Session cookies

> _
```

**Multi-line input:**
```
> Here's a longer message
  that spans multiple lines.

  With a blank line even._
```

### 5. Thinking Indicator

When Claude is processing but no tool is running:

```
[thinking...] analyzing code structure...
```

Or with progress bar for long operations:

```
[thinking] ████████░░░░░░░░ processing large file...
```

### 6. Streaming Display

Characters appear as they arrive:

```
I'll help you refactor th|  <- cursor shows typing position
```

Then completes:

```
I'll help you refactor the login function.
```

## Keymap

### Conversation Region (read-only)
| Key | Action |
|-----|--------|
| `RET` | Expand/collapse tool at point |
| `TAB` | Next tool call |
| `S-TAB` | Previous tool call |
| `v` | Cycle verbosity |
| `g` | Refresh display |
| `q` | Quit buffer |

### Input Region
| Key | Action |
|-----|--------|
| `RET` | Send message (or newline if shift) |
| `S-RET` | Insert newline |
| `C-c C-c` | Send message |
| `C-c C-k` | Cancel/clear input |
| `M-p` | Previous input history |
| `M-n` | Next input history |
| `C-c C-x` | Cancel session |
| `1-9` | Select option (when options shown) |

### Global (anywhere in buffer)
| Key | Action |
|-----|--------|
| `C-c C-p` | Pause session |
| `C-c C-r` | Resume session |
| `C-c C-x` | Cancel session |
| `?` | Show help |

## State Machine

```
                    ┌─────────────┐
                    │    IDLE     │
                    └──────┬──────┘
                           │ start-session
                           ▼
                    ┌─────────────┐
            ┌───────│   WORKING   │◄──────┐
            │       └──────┬──────┘       │
            │              │              │
     pause  │              │ ask-user     │ respond
            │              ▼              │
            │       ┌─────────────┐       │
            └──────►│   WAITING   │───────┘
                    └──────┬──────┘
                           │
            ┌──────────────┼──────────────┐
            │ success      │ fail         │ cancel
            ▼              ▼              ▼
     ┌─────────────┐ ┌─────────────┐ ┌─────────────┐
     │  COMPLETE   │ │   FAILED    │ │  CANCELLED  │
     └─────────────┘ └─────────────┘ └─────────────┘
```

## Integration Points

### With efrit-executor

```elisp
;; Session lifecycle
(add-hook 'efrit-executor-session-start-hook #'efrit-agent-on-session-start)
(add-hook 'efrit-executor-session-end-hook #'efrit-agent-on-session-end)

;; Streaming content
(add-hook 'efrit-executor-content-hook #'efrit-agent-on-content)

;; Tool calls
(add-hook 'efrit-executor-tool-start-hook #'efrit-agent-on-tool-start)
(add-hook 'efrit-executor-tool-result-hook #'efrit-agent-on-tool-result)

;; User input needed
(add-hook 'efrit-executor-input-needed-hook #'efrit-agent-on-input-needed)
```

### With efrit-progress

The agent buffer should *replace* the progress buffer for interactive use:
- Progress buffer remains for headless/batch modes
- Agent buffer subscribes to same events
- Agent buffer provides richer display

### With efrit-do TODOs

When Claude creates TODOs, they appear in the conversation:

```
I'll break this into steps:

  ☐ Read current implementation
  ☐ Refactor to async/await
  ☐ Update callers
  ☐ Run tests
```

And update as completed:

```
  ✓ Read current implementation
  ▶ Refactor to async/await  <- current
  ☐ Update callers
  ☐ Run tests
```

## Performance Considerations

### Avoid Full Re-renders

Current problem: `efrit-agent--render` erases and redraws everything.

Solution:
1. **Append-only for messages** - Just insert at `efrit-agent--conversation-end`
2. **Update in-place for tool results** - Find by text property, update region
3. **Overlays for expansion** - No re-render, just show/hide overlay
4. **Timer only updates header-line** - Not buffer content

### Memory Management

- Limit conversation history (configurable, default 1000 entries)
- Archive old tool details to file
- Weak references for large results

### Large Results

For tool results >1000 chars:
- Show truncated in buffer
- Full result in overlay (lazy-loaded)
- Option to open in separate buffer

## Faces

```elisp
;; Messages
(defface efrit-agent-user-message ...)      ; User input
(defface efrit-agent-claude-message ...)    ; Claude response
(defface efrit-agent-claude-thinking ...)   ; Thinking indicator

;; Tools
(defface efrit-agent-tool-name ...)         ; Tool name
(defface efrit-agent-tool-running ...)      ; Currently executing
(defface efrit-agent-tool-success ...)      ; Completed successfully
(defface efrit-agent-tool-failure ...)      ; Failed

;; Diff (reuse diff-mode faces)
(defface efrit-agent-diff-added ...)
(defface efrit-agent-diff-removed ...)
(defface efrit-agent-diff-context ...)

;; Status
(defface efrit-agent-status-working ...)
(defface efrit-agent-status-waiting ...)
(defface efrit-agent-status-complete ...)
(defface efrit-agent-status-failed ...)

;; Input
(defface efrit-agent-input-prompt ...)
(defface efrit-agent-option-button ...)
(defface efrit-agent-option-selected ...)
```

## Configuration

```elisp
(defcustom efrit-agent-auto-show t
  "Automatically show agent buffer when session starts.")

(defcustom efrit-agent-send-on-ret t
  "If non-nil, RET sends message. If nil, use C-c C-c.")

(defcustom efrit-agent-default-verbosity 'normal
  "Default verbosity: minimal, normal, or verbose.")

(defcustom efrit-agent-max-history 1000
  "Maximum conversation entries to keep in buffer.")

(defcustom efrit-agent-show-thinking t
  "Show thinking indicator when Claude is processing.")

(defcustom efrit-agent-expand-errors t
  "Automatically expand failed tool calls.")

(defcustom efrit-agent-show-diff t
  "Show inline diffs for file edits.")

(defcustom efrit-agent-diff-context-lines 3
  "Number of context lines in diffs.")
```

## Migration from Current Implementation

The current `efrit-agent.el` has good bones but needs restructuring:

### Keep
- Face definitions (well designed)
- Unicode/ASCII character tables
- Integration hook pattern
- Keymap structure
- Verbosity concept

### Remove
- Section-based layout (Tasks, Activity, Input sections)
- Full-buffer re-render on every update
- Separate render functions per section

### Add
- Streaming content support
- Incremental updates with markers
- Overlay-based expansion
- Header-line (not header in buffer)
- Proper input minor mode
- Diff display for edits

### Refactor
- `efrit-agent--render` → multiple focused update functions
- Activity list → inline in conversation
- TODO display → inline in conversation
- Input section → permanent input region at bottom

## Success Criteria

1. **Feels like chatting** - Natural conversation flow
2. **See the work** - Tool calls visible but not overwhelming
3. **Never confused** - Always know what's happening (status, progress)
4. **Can intervene** - Pause, redirect, provide input easily
5. **Responsive** - No flicker, smooth updates, streaming feel
6. **Accessible** - Works in terminal, with screen readers
7. **Comparable to Claude Code** - Same quality UX in Emacs

## Implementation Phases

### Phase 1: Core Conversation Architecture
- New buffer layout with conversation region + input region
- Header-line status display
- Basic message rendering (user + Claude)
- Incremental update system (no full re-render)

### Phase 2: Tool Call Display
- Inline collapsed tool calls
- Expand/collapse with overlays
- Running tool indicators
- Result display (success/failure)

### Phase 3: Streaming & Thinking
- Character-by-character streaming
- Thinking indicators
- Smooth typing animation

### Phase 4: Enhanced Interactions
- Diff display for edits
- Option buttons for questions
- Error recovery actions
- Input history

### Phase 5: Polish
- Inline TODO display
- Context-aware completion
- Session persistence/restore
- Multi-session support

---

*This design prioritizes the conversational experience while leveraging Emacs's unique capabilities for a truly world-class coding assistant interface.*
