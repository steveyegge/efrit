# Agentic Buffer Interface Implementation Plan (ef-9sy)

> **Status**: Planning document - not yet filed as Beads
> **Created**: 2025-12-03
> **Issue**: ef-9sy - Agentic buffer interface for Efrit

## Executive Summary

Efrit is approximately **80% complete** on the agentic buffer interface. The `efrit-agent.el` module and supporting files already implement a remarkably complete Claude Code-like interface. The core infrastructure exists:

- **Two-region buffer architecture** (read-only conversation + editable input)
- **Incremental rendering** (streaming, in-place tool updates, no full re-render)
- **Integration hooks** connecting to `efrit-do`/`efrit-progress`
- **Tool display** with expand/collapse, diff highlighting
- **User input handling** with history, completion, option selection

What's missing are **integration gaps** and **UX polish** to make it the primary interface.

---

## Current State Analysis

### What Works Well

| Component | Status | Notes |
|-----------|--------|-------|
| Buffer architecture | ✅ Complete | Conversation/input regions with markers |
| Streaming display | ✅ Complete | `efrit-agent--add-claude-message` chunks properly |
| Tool call display | ✅ Complete | Start→result in-place updates |
| Thinking indicator | ✅ Complete | Shows/hides automatically |
| TODO display | ✅ Complete | Inline updates |
| Question/options | ✅ Complete | Clickable buttons, keyboard shortcuts |
| Input handling | ✅ Complete | History, completion, mode switching |
| Header-line status | ✅ Complete | Live elapsed time, tool count |
| Integration hooks | ✅ Complete | Advice-based connections to efrit-do |

### Existing Module Structure

```
lisp/interfaces/
├── efrit-agent.el              # Public API, faces, mode, keymaps
├── efrit-agent-core.el         # State variables, region management, buffer lifecycle
├── efrit-agent-render.el       # Message rendering, streaming, thinking indicator
├── efrit-agent-tools.el        # Tool call display, expansion, diff formatting
├── efrit-agent-input.el        # Input minor mode, question handling
└── efrit-agent-integration.el  # Hook integrations with efrit-do/progress
```

### Integration Gaps

1. **Agent buffer not always triggered**: The integration hooks in `efrit-agent-integration.el` are set up but may not be called consistently. The `efrit-agent-setup-integration` function needs to be called at init time, and the hooks rely on `efrit-progress-*` functions being called.

2. **efrit-do-async-loop bypasses agent integration**: The async loop (`efrit-do-async-loop.el:163-180`) calls `efrit-progress-insert-event` for progress buffer updates, but the agent buffer integration expects `efrit-progress-show-tool-start` / `efrit-progress-show-tool-result` which are different functions.

3. **Two progress systems**: There are two parallel display systems:
   - `efrit-progress-buffer.el` - Raw event stream (`*Efrit Progress: session-id*`)
   - `efrit-agent.el` - Conversation-first view (`*efrit-agent*`)

   The async loop feeds the progress buffer, but the agent buffer integration hooks on different functions.

4. **Session lifecycle disconnect**: `efrit-agent--on-session-start` hooks on `efrit-progress-start-session`, but `efrit-do-async-loop` calls `efrit-progress-create-buffer` directly without triggering the hook.

---

## Architecture Recommendation

The simplest path is to **wire the agent buffer directly into the async loop**, making it the primary display surface rather than relying on progress buffer integration.

### Option A: Direct Integration (Recommended)

Modify `efrit-do-async-loop.el` to call agent buffer functions directly:

```elisp
;; In efrit-do-async-loop
(efrit-agent-start-session session-id command)  ; Create/show buffer

;; In efrit-do-async--on-api-response
(efrit-agent-stream-content text)  ; Stream Claude's text

;; In efrit-do-async--execute-tools
(efrit-agent-show-tool-start tool-name input)  ; Returns tool-id
(efrit-agent-show-tool-result tool-id result success elapsed)

;; In efrit-do-async--stop-loop
(efrit-agent-end-session success-p)
```

### Option B: Unified Event System

Create a single event dispatch system that both progress buffer and agent buffer consume:

```elisp
(efrit-emit-event 'tool-started :name "read_file" :input input)
;; → Progress buffer and agent buffer both update
```

**Recommendation**: Option A is simpler and lower-risk.

---

## Implementation Plan

### Phase 1: Wire Agent Buffer to Async Loop

**Goal**: Make agent buffer appear and update during `efrit-do` execution

#### Task 1.1: Call agent buffer on session start
**File**: `efrit-do-async-loop.el:66-88`

Add `(efrit-agent-start-session session-id command)` after progress buffer creation. The agent buffer will show alongside or instead of progress buffer.

#### Task 1.2: Stream Claude's text to agent buffer
**File**: `efrit-do-async-loop.el:154-181`

Extract text content from response and call `efrit-agent-stream-content`. Call `efrit-agent-stream-end` when done with text.

#### Task 1.3: Show tool start/result
**File**: `efrit-do-async-loop.el:183-255`

- Before each tool execution: `(efrit-agent-show-tool-start tool-name input)`
- After execution: `(efrit-agent-show-tool-result tool-id result success elapsed)`

#### Task 1.4: End session properly
**File**: `efrit-do-async-loop.el:319-356`

Add: `(efrit-agent-end-session (string= stop-reason "end_turn"))`

### Phase 2: Input Integration

**Goal**: Make typed input in agent buffer actually work

Currently, `efrit-agent-input-send` displays the message but doesn't execute. The comment says:
> "Actual processing happens in efrit-do-async-loop."

#### Task 2.1: Connect input to executor
When user sends input, it should be injected into the session:
- Use `efrit-session-respond-to-question` for question responses
- Use injection system for ad-hoc guidance

#### Task 2.2: Question handling
- Hook `request_user_input` tool to show question in agent buffer
- Response routes back through session

### Phase 3: Polish

**Goal**: Claude Code-level UX

#### Task 3.1: Syntax highlighting in tool results
- Code blocks with language detection
- Diff display already works (`efrit-agent-tools.el`)

#### Task 3.2: Better collapse/expand
- Default collapsed for long results
- Keyboard shortcuts (RET already works)

#### Task 3.3: Session persistence
- Save/restore conversation on buffer kill
- Already have `efrit-session-transcript.el`

#### Task 3.4: Multi-session support
- Queue display in agent buffer
- Session switching

---

## Key Files to Modify

| File | Changes |
|------|---------|
| `efrit-do-async-loop.el` | Call agent buffer functions in loop |
| `efrit-agent-integration.el` | May need cleanup after direct integration |
| `efrit-agent-input.el` | Wire input send to session |
| `efrit-do.el` | Possibly toggle progress buffer off when agent buffer active |

## Estimated Scope

- **Phase 1**: ~4-6 focused edits to `efrit-do-async-loop.el`
- **Phase 2**: ~2-3 edits to input handling
- **Phase 3**: Polish items are nice-to-have

The core integration (Phase 1) is likely **30-50 lines of changes** to the async loop, plus ensuring the agent buffer functions handle edge cases.

---

## Critical Questions Before Implementation

1. **Replace or complement progress buffer?**
   - Should agent buffer replace progress buffer entirely?
   - Or run both (progress for debugging, agent for interaction)?

2. **Buffer naming/lifecycle**
   - One `*efrit-agent*` buffer reused across sessions?
   - Or per-session buffers like progress (`*efrit-agent: session-id*`)?

3. **Default display**
   - Auto-show agent buffer on `efrit-do`?
   - Or require explicit `M-x efrit-agent`?

4. **Input routing**
   - Should input always go through agent buffer when it exists?
   - Or allow `efrit-do` in minibuffer while agent buffer shows output?

---

## What Makes This "Claude Code"-like

The existing implementation already has the essential Claude Code patterns:

| Feature | Claude Code | Efrit Status |
|---------|-------------|--------------|
| Streaming text display | ✅ | ✅ Already implemented |
| Tool calls with input/output | ✅ | ✅ Already implemented |
| Inline diffs | ✅ | ✅ Already implemented |
| TODO tracking | ✅ | ✅ Already implemented |
| User input with history | ✅ | ✅ Already implemented |
| Keyboard shortcuts | ✅ | ✅ Already implemented |
| Approval prompts | ✅ | ✅ `confirm_action` tool |

What's different from Claude Code:
- Claude Code uses terminal (tmux/PTY), Efrit uses Emacs buffers (arguably better in Emacs!)
- Claude Code has fancy spinners, Efrit has text indicators (fine for Emacs)
- Claude Code has file watching, Efrit has buffer integration (native to Emacs)

The main gap is simply **wiring the pieces together**.

---

## Recommendation

Start with **Phase 1 only** - wire the agent buffer into the async loop. This is:

- **Low risk**: Agent buffer code is solid and well-tested
- **High visibility**: Users see the new interface immediately
- **Testable quickly**: Run `efrit-do`, see if agent buffer works

After Phase 1 is working, assess whether Phase 2 (input integration) is needed or if the existing minibuffer-based input is sufficient.

---

## Appendix: Agent Buffer Public API

These are the functions to call from the async loop:

```elisp
;; Session lifecycle
(efrit-agent-start-session SESSION-ID COMMAND)  ; Create buffer, set working
(efrit-agent-end-session SUCCESS-P)             ; Mark complete/failed

;; Messages
(efrit-agent-add-message TEXT &optional TYPE)   ; TYPE: user, claude, error
(efrit-agent-stream-content TEXT)               ; Append to Claude message
(efrit-agent-stream-end)                        ; Finalize Claude message

;; Tools
(efrit-agent-show-tool-start TOOL-NAME INPUT)   ; Returns TOOL-ID
(efrit-agent-show-tool-result TOOL-ID RESULT SUCCESS-P ELAPSED)

;; User interaction
(efrit-agent-show-question QUESTION OPTIONS)    ; Display question
(efrit-agent-show-thinking &optional TEXT)      ; Show thinking indicator
(efrit-agent-hide-thinking)                     ; Hide thinking indicator

;; TODOs
(efrit-agent-show-todos TODOS)                  ; Update TODO display

;; Status
(efrit-agent-set-status STATUS)                 ; working, paused, waiting, complete, failed
```

All these functions handle the buffer lookup internally and are safe to call even if the buffer doesn't exist yet.
