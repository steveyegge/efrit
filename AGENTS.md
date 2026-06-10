# Instructions for AI Agents Working on Efrit

> **For humans**: This file contains instructions for AI coding assistants. See [README.md](README.md) for user documentation.

## Project Overview

**Efrit** is an AI-powered Emacs coding assistant built on the **Zero Client-Side Intelligence** principle: Efrit is a pure executor that delegates ALL cognitive computation to Claude.

## Core Architectural Principle

**ZERO CLIENT-SIDE INTELLIGENCE**: Efrit must NEVER contain:
- Pattern recognition or parsing logic
- Decision-making heuristics
- Pre-written solutions or templates
- Task-specific logic
- Flow control decisions

Efrit's ONLY job: execute what Claude tells it to do. See [ARCHITECTURE.md](ARCHITECTURE.md) for details.

## 🚨 COMMIT AND PUSH AFTER EVERY FIX 🚨

**This is the most important rule.** After completing ANY task:

```bash
git add -A
git commit -m "Descriptive message"
git push
```

Do NOT batch multiple fixes. Do NOT wait until "end of session". Push immediately after each completed task.

## Issue Tracking with Beads

This project uses **bd (beads)** for ALL issue tracking. Do NOT use markdown TODOs or other tracking methods.

```bash
bd ready                              # Find unblocked work
bd create "Title" -t bug|feature|task -p 0-4
bd update <id> --status in_progress
bd close <id> --reason "Done"
bd export -o .beads/issues.jsonl      # Export to JSONL, then git commit it
```

The JSONL file is the source of truth; the database file is gitignored.
After changing issues, run `bd export -o .beads/issues.jsonl` and commit
the JSONL. On a fresh clone, the db auto-imports from issues.jsonl on the
first bd command. (bd 1.0.4 removed `bd sync` — do not use it.)

**Issue Types**: bug, feature, task, epic, chore  
**Priorities**: 0=Critical, 1=High, 2=Medium, 3=Low, 4=Backlog

## Development Workflow

### Before Making Changes
1. Check ready work: `bd ready`
2. Claim task: `bd update <id> --status in_progress`

### After Each Fix
1. Compile: `make compile`
2. Commit and push immediately
3. Close issue: `bd close <id> --reason "Done"`

### Verification Requirements

**YOU MUST VERIFY ALL CHANGES BEFORE REPORTING COMPLETION.**

For all changes (necessary, not sufficient):
- ✅ `make compile` passes (includes the top-level-form lint)
- ✅ `emacs --batch` execution without errors

For changes to live behavior (UI, sessions, async loops, buffers, timers):
- ✅ Verify through `bin/efrit` against a live daemon (see docs/CHANNEL.md).
  Every eval returns the value, any error, new `*Messages*` output, and an
  editor snapshot — observe the actual behavior, don't infer it.
- ✅ Use a dedicated test daemon, not the user's running Emacs:
  `emacs -Q --daemon=NAME`, then add the lisp/ load-paths, set
  `(setq load-prefer-newer t)` (so a stale .elc can never shadow your
  edits), and `(require 'efrit)` via
  `EFRIT_SOCKET=NAME bin/efrit eval - <<'EOF' ... EOF`
- ✅ Run `make compile` before live testing so .elc files are fresh
  (it also deletes orphaned .elc whose source was renamed or removed)
- ✅ Actual API calls for efrit-chat/efrit-do changes

Why batch mode is not enough: both June 2026 root-cause bugs (098182c —
parens nested a defun inside its caller; 7916d58 — url-retrieve cleanup
killing user buffers) passed `make compile` and `emacs --batch`, and were
only findable by observing a live session through the channel.

Unacceptable:
- ❌ "The code looks correct"
- ❌ "This should work"
- ❌ Reading code and assuming it works
- ❌ Verifying live-behavior changes with batch mode alone

## Code Standards

- Use `lexical-binding: t` in all elisp files
- Use `efrit-` prefix for public functions
- Use `efrit--` (double dash) for private functions
- Docstrings required for all functions
- NO client-side intelligence

## Project Structure

```
efrit/
├── lisp/
│   ├── efrit.el              # Main entry point
│   ├── core/                 # Core functionality
│   │   ├── efrit-chat.el     # Chat interface
│   │   ├── efrit-tools.el    # Tool implementations
│   │   ├── efrit-session.el  # Session management
│   │   └── efrit-do-schema.el # Tool schemas
│   ├── interfaces/           # User-facing interfaces
│   │   ├── efrit-do.el       # Command execution
│   │   ├── efrit-agent.el    # Agent mode
│   │   └── efrit-remote-queue.el # AI-to-AI queue
│   ├── tools/                # Individual tool implementations
│   └── support/              # UI helpers (dashboard, todos)
├── test/                     # Test files
├── docs/                     # Documentation
├── mcp/                      # MCP server (TypeScript/Node)
├── .beads/                   # Issue tracker database
└── ARCHITECTURE.md           # Core design principles
```

## Key Files

- **ARCHITECTURE.md** - Pure Executor principle (READ FIRST)
- **lisp/interfaces/efrit-do.el** - Main command execution
- **lisp/core/efrit-do-schema.el** - Tool definitions
- **lisp/core/efrit-tools.el** - Tool system prompt and utilities

## Common Tasks

**Adding a new tool**:
1. Add handler to `lisp/tools/efrit-tool-*.el`
2. Add schema to `lisp/core/efrit-do-schema.el`
3. Add dispatch entry in `lisp/interfaces/efrit-do.el`

**Fixing a bug**:
1. `bd create "Bug: description" -t bug -p 1`
2. Fix the bug
3. `make compile`
4. Commit and push
5. `bd close <id> --reason "Fixed"`

### Planning Work with Dependencies

When breaking down large features into tasks, use **beads dependencies** to sequence work - NOT phases or numbered steps.

**⚠️ COGNITIVE TRAP: Temporal Language Inverts Dependencies**

Words like "Phase 1", "Step 1", "first", "before" trigger temporal reasoning that **flips dependency direction**. Your brain thinks:
- "Phase 1 comes before Phase 2" → "Phase 1 blocks Phase 2" → `bd dep add phase1 phase2`

But that's **backwards**! The correct mental model:
- "Phase 2 **depends on** Phase 1" → `bd dep add phase2 phase1`

**Solution: Use requirement language, not temporal language**

Instead of phases, name tasks by what they ARE, and think about what they NEED:

```bash
# ❌ WRONG - temporal thinking leads to inverted deps
bd create "Phase 1: Create buffer layout" ...
bd create "Phase 2: Add message rendering" ...
bd dep add phase1 phase2  # WRONG! Says phase1 depends on phase2

# ✅ RIGHT - requirement thinking
bd create "Create buffer layout" ...
bd create "Add message rendering" ...
bd dep add msg-rendering buffer-layout  # msg-rendering NEEDS buffer-layout
```

**Verification**: After adding deps, run `bd blocked` - tasks should be blocked by their prerequisites, not their dependents.

**Example breakdown** (for a multi-part feature):
```bash
# Create tasks named by what they do, not what order they're in
bd create "Implement conversation region" -t task -p 1
bd create "Add header-line status display" -t task -p 1
bd create "Render tool calls inline" -t task -p 2
bd create "Add streaming content support" -t task -p 2

# Set up dependencies: X depends on Y means "X needs Y first"
bd dep add header-line conversation-region    # header needs region
bd dep add tool-calls conversation-region     # tools need region
bd dep add streaming tool-calls               # streaming needs tools

# Verify with bd blocked - should show sensible blocking
bd blocked
```

## Testing

```bash
# Compile all elisp
make compile

# Run tests (if available)
make test

# Quick elisp check
emacs --batch --eval "(progn (require 'efrit) (message \"OK\"))"

# Live verification channel (see docs/CHANNEL.md and Verification Requirements)
bin/efrit ping
bin/efrit eval '(efrit-do "...")'
```

## Rules Summary

### ✅ DO:
- Commit and push after EVERY fix
- Use bd for all task tracking
- Verify changes compile before reporting done
- Keep changes minimal and focused

### ❌ DO NOT:
- Batch commits until end of session
- Add client-side intelligence
- Create files without reading existing code first
- Skip compilation verification

---

**Remember**: Efrit is a Pure Executor. Claude thinks, Efrit executes.
