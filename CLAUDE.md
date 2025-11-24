# Instructions for AI Agents Working on Efrit

> **For humans**: This file contains instructions for AI coding assistants. See [README.md](README.md) for user documentation.

## Project Overview

**Efrit** is an AI-powered Emacs coding assistant built on the **Zero Client-Side Intelligence** principle: Efrit is a pure executor that delegates ALL cognitive computation to Claude.

## 🎯 Core Architectural Principle

**ZERO CLIENT-SIDE INTELLIGENCE**: Efrit must NEVER contain:
- Pattern recognition or parsing logic
- Decision-making heuristics
- Pre-written solutions or templates
- Task-specific logic
- Flow control decisions

Efrit's ONLY job: execute what Claude tells it to do. See [ARCHITECTURE.md](ARCHITECTURE.md) for details.

## Issue Tracking with Beads

**CRITICAL**: This project uses **bd (beads)** for ALL issue tracking. Do NOT use markdown TODOs, task lists, or other tracking methods.

### Quick Start

```bash
# The .beads/ directory is already initialized
bd ready --json                                    # Find unblocked work
bd create "Issue title" -t bug|feature|task -p 0-4 --json
bd update <id> --status in_progress --json
bd close <id> --reason "Done" --json
bd sync                                            # CRITICAL at end of session
```

### Issue Types

- `bug` - Something broken that needs fixing
- `feature` - New functionality
- `task` - Work item (tests, docs, refactoring)
- `epic` - Large feature composed of multiple issues
- `chore` - Maintenance work (dependencies, tooling)

### Priorities

- `0` - Critical (security, data loss, broken builds)
- `1` - High (major features, important bugs)
- `2` - Medium (nice-to-have features, minor bugs)
- `3` - Low (polish, optimization)
- `4` - Backlog (future ideas)

### Workflow

1. **Check for ready work**: `bd ready --json`
2. **Claim your task**: `bd update <id> --status in_progress`
3. **Work on it**: Implement, test, document
4. **Discover new work?** File issues with dependencies:
   ```bash
   bd create "Found bug in async" -p 1 --deps discovered-from:<parent-id> --json
   ```
5. **Complete**: `bd close <id> --reason "Implemented" --json`
6. **CRITICAL - End of session**: `bd sync` (force immediate export/commit/push)

## Current Project State

### What Works
- ✅ Core architecture and module loading
- ✅ API communication with Claude
- ✅ Basic chat interface (efrit-chat)
- ✅ Remote queue system for AI-to-AI communication
- ✅ Session tracking and context management

### What's Broken (Known Issues)
- ❌ Test infrastructure incomplete (see mcp-fmw)
- ❌ MCP server only 50% complete (see mcp-b0f)
- ❌ Async workflow may have issues (see mcp-6gr)
- ❌ Tool selection loops (todo_get_instructions) - see mcp-bt0
- ❌ Documentation scattered across multiple files

### Active Work
Check current ready work with:
```bash
bd ready --json | jq -r '.[] | "[\(.priority)] \(.id): \(.title)"'
```

## Development Guidelines

### MANDATORY VERIFICATION RULE

**🚨 CRITICAL - READ THIS FIRST 🚨**

**YOU MUST VERIFY ALL CHANGES BEFORE REPORTING COMPLETION TO THE USER.**

This is non-negotiable. The user has spent significant time in the past finding bugs that you claimed were fixed but weren't actually tested. This wastes the user's time and is unacceptable.

**What "verification" means:**
1. **For code changes**: Run the actual code in Emacs (batch mode or daemon) and verify it executes without errors
2. **For API-related changes**: Make actual API calls or simulate the full request/response cycle
3. **For bug fixes**: Reproduce the original error, apply your fix, verify the error is gone
4. **For new features**: Execute the feature end-to-end and confirm it works

**Acceptable verification methods:**
- ✅ `emacs --batch` with actual execution of the changed code
- ✅ Starting Emacs daemon and testing via remote queue
- ✅ Running `make compile` and checking for errors (for compilation issues only)
- ✅ Executing the actual command/function and showing output

**UNACCEPTABLE - these do NOT count as verification:**
- ❌ "The code looks correct"
- ❌ "This should work"
- ❌ "The function is now available" (without actually calling it)
- ❌ Reading the code and assuming it works
- ❌ Compilation success alone (unless the issue was a compilation error)

**When reporting to user:**
- Include actual command output showing the verification
- Show before/after behavior if fixing a bug
- Demonstrate the feature working, don't just claim it does

**If you cannot verify:**
- State clearly: "I cannot verify this works because [reason]"
- Explain what manual verification steps the user should take
- DO NOT claim something works if you haven't verified it

### Before Making Changes

1. **Read the architecture**: [ARCHITECTURE.md](ARCHITECTURE.md) - understand Pure Executor principle
2. **Check for existing issues**: `bd list --json | grep -i "your topic"`
3. **Create an issue if needed**: `bd create "Your task" -t task -p 2 --json`
4. **Claim the issue**: `bd update <id> --status in_progress`

### The Toolchest: Choosing the Right Approach

As an AI agent working on Efrit, you have three primary ways to interact with the system. **Choose the right tool for the task:**

#### 1. Direct File Access (Read/Edit/Write tools)
**Use for:** Modifying source code, documentation, configuration files

**When:**
- Editing `.el` files in `lisp/`
- Updating documentation (`.md` files)
- Reading source code to understand implementation
- Making any changes to the codebase itself

**Example:**
```
Read lisp/efrit-do.el
Edit lisp/efrit-do.el (modify specific function)
Write test/new-test.el (create new file)
```

**Why:** Most direct, efficient for code changes, no indirection

#### 2. Bash + Batch Emacs
**Use for:** Compilation, unit tests, one-off elisp evaluation

**When:**
- Compiling elisp files (`make compile`)
- Running ERT unit tests
- Quick elisp evaluation to check behavior
- Verifying code compiles without errors

**Example:**
```bash
# Compile all elisp files
make compile

# Run specific test file
emacs --batch -L lisp -l test/test-remote-queue-validation.el -f ert-run-tests-batch-and-exit

# Quick elisp evaluation
emacs --batch --eval "(progn (require 'json) (print (json-encode '((a . 1)))))"
```

**Why:** Fast, synchronous, familiar Unix pipeline model, good for CI/CD

#### 3. Remote Queue System (AI-to-AI Communication)
**Use for:** Integration testing, protocol validation, end-to-end testing

**When:**
- Testing changes to `efrit-remote-queue.el`
- Validating request/response protocol (version, status, etc.)
- Testing multi-turn interactions
- Simulating real AI-to-AI communication scenarios
- Verifying MCP integration works correctly

**Example:**
```bash
# 1. Start Efrit daemon (if not already running)
emacs --daemon=efrit-test --load lisp/dev/efrit-autonomous-startup.el

# 2. Send a test request
cat > ~/.emacs.d/.efrit/queues/requests/test_$(date +%s).json <<EOF
{
  "id": "test-validation-$(date +%s)",
  "version": "1.0.0",
  "type": "eval",
  "content": "(+ 1 2)",
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)"
}
EOF

# 3. Wait briefly for processing
sleep 0.5

# 4. Check the response
ls -ltr ~/.emacs.d/.efrit/queues/responses/ | tail -1
cat ~/.emacs.d/.efrit/queues/responses/resp_test-*.json

# 5. Verify response has correct structure (version, status, result, timestamp)
```

**Why:** Tests the complete request/response cycle, validates protocol compatibility, catches integration issues that unit tests miss

**Queue directories:**
- `~/.emacs.d/.efrit/queues/requests/` - Your JSON requests go here
- `~/.emacs.d/.efrit/queues/processing/` - Currently being processed
- `~/.emacs.d/.efrit/queues/responses/` - Completed responses appear here
- `~/.emacs.d/.efrit/queues/archive/` - Historical data

#### Decision Matrix

| Task | Tool | Example |
|------|------|---------|
| Edit source code | Direct file access | `Edit lisp/efrit-do.el` |
| Compile code | Bash | `make compile` |
| Run unit tests | Bash + batch | `emacs --batch -l test/foo.el -f ert...` |
| Test protocol changes | Remote queue | Send JSON to `queues/requests/`, read `queues/responses/` |
| Validate request/response format | Remote queue | Check version/status fields in actual responses |
| Test multi-turn interaction | Remote queue | Send multiple sequential requests |
| Quick elisp check | Bash + batch | `emacs --batch --eval "(+ 1 2)"` |

#### Recommended Workflow

**When making changes to Efrit:**
1. **Edit code** using direct file access (Read/Edit/Write)
2. **Compile** using `make compile` (Bash)
3. **Unit test** using `emacs --batch` (Bash)
4. **Integration test** using remote queue (if protocol-related changes)
5. **Commit** using git (Bash)

**When you modify `efrit-remote-queue.el` or protocol-related code:**
- ALWAYS validate with the remote queue system
- This ensures request/response format is correct
- Catches version compatibility issues, status enum problems, etc.
- Unit tests alone won't catch protocol integration bugs

### Code Standards

- **Emacs Lisp conventions**: Use `lexical-binding: t` in all files
- **Naming**: Use `efrit-` prefix for all public functions
- **Documentation**: Docstrings required for all functions
- **NO client-side intelligence**: If you add pattern matching or decision logic, you've violated the architecture
- **File locations**: All source code in `lisp/`, tests in `test/`, docs in `docs/`

### Common Tasks

**Adding a new tool**:
1. Add function to `lisp/efrit-tools.el`
2. Update tool schema in `efrit-do--get-tools-schema`
3. Add tests
4. Document in tool list

**Fixing a bug**:
1. File issue: `bd create "Bug: description" -t bug -p 1 --json`
2. Add test that reproduces the bug
3. Fix the bug
4. Verify test passes
5. Close issue: `bd close <id> --reason "Fixed" --json`

**Adding documentation**:
1. Prefer editing existing docs over creating new files
2. Keep README.md user-focused
3. Keep ARCHITECTURE.md for design principles
4. Use `docs/` for detailed guides

## Landing the Plane (End of Session)

When ending a session, you MUST complete ALL these steps:

### 1. File Issues for Remaining Work
```bash
bd create "Follow-up task" -t task -p 2 --json
bd create "Bug discovered" -t bug -p 1 --deps discovered-from:<parent-id> --json
```

### 2. Run Quality Gates (only if code changes were made)
```bash
make compile  # Byte-compile elisp
make test     # Run tests (when working)
# File P0 issues if builds are broken
```

**Optional: Queue-based validation** (if you modified protocol-related code):
```bash
# If you changed efrit-remote-queue.el, MCP, or protocol code:
# 1. Start daemon if needed
emacs --daemon=efrit-test --load lisp/dev/efrit-autonomous-startup.el

# 2. Send validation request
cat > ~/.emacs.d/.efrit/queues/requests/final_check_$(date +%s).json <<EOF
{
  "id": "final-validation-$(date +%s)",
  "version": "1.0.0",
  "type": "eval",
  "content": "(message \"Protocol validation OK\")",
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)"
}
EOF

# 3. Verify response structure is correct
sleep 0.5
cat ~/.emacs.d/.efrit/queues/responses/resp_final-*.json | jq '.'
# Check: version, status, result, timestamp fields present and valid
```

### 3. Update Beads Issues
```bash
bd close mcp-42 mcp-43 --reason "Completed" --json
bd update mcp-44 --status in_progress --json  # If partially done
```

### 4. PUSH TO REMOTE - MANDATORY
```bash
# Pull first to catch remote changes
git pull --rebase

# If conflicts in .beads/beads.jsonl:
#   Option A: git checkout --theirs .beads/beads.jsonl && bd import
#   Option B: Manually merge, then bd import

# Sync the database (exports to JSONL, commits)
bd sync

# MANDATORY: Push everything to remote
# THE SESSION IS NOT COMPLETE UNTIL THIS SUCCEEDS
git push

# MANDATORY: Verify push succeeded
git status  # MUST show "up to date with origin/main"
```

**CRITICAL RULES:**
- ❌ NEVER stop before `git push` completes successfully
- ❌ NEVER say "ready to push when you are!" - YOU must push
- ✅ If `git push` fails, resolve and retry until it succeeds
- ✅ The session has NOT ended until `git push` shows success

### 5. Clean Up Git State
```bash
git stash clear
git remote prune origin
```

### 6. Verify Clean State
```bash
git status  # Should show "nothing to commit, working tree clean"
bd stats    # Verify issue tracker state
```

### 7. Choose Follow-Up Issue
```bash
bd ready --json
bd show <next-id> --json

# Provide user with:
# "Continue work on <id>: [issue title]. [Context: what's done, what's next]"
```

## Important Rules

### ✅ DO:
- Use bd for ALL task tracking
- Always use `--json` flag for programmatic use
- Link discovered work with `discovered-from` dependencies
- Check `bd ready` before asking "what next?"
- Run `bd sync` at end of session
- Respect the Pure Executor principle
- Read existing code before proposing changes
- Keep changes minimal and focused

### ❌ DO NOT:
- Create markdown TODO lists
- Use external issue trackers
- Add client-side intelligence (pattern matching, decision logic)
- Create files without reading existing code first
- Make changes beyond what was requested
- Stop session before pushing to remote
- Clutter repo root with planning documents

## Project Structure

```
efrit/
├── lisp/                  # All Emacs Lisp source code
│   ├── efrit.el          # Main entry point
│   ├── efrit-chat.el     # Chat interface
│   ├── efrit-do.el       # Command execution
│   ├── efrit-tools.el    # Tool implementations
│   └── ...               # Other modules
├── test/                  # Test files
├── docs/                  # Documentation
├── mcp/                   # MCP server (TypeScript/Node)
├── plans/                 # Planning documents (archive old ones)
├── .beads/                # Issue tracker database
├── ARCHITECTURE.md        # Core design principles
├── README.md              # User documentation
└── CLAUDE.md              # This file (agent instructions)
```

## Key Files to Know

- **ARCHITECTURE.md** - Pure Executor principle (READ THIS FIRST)
- **README.md** - User-facing documentation
- **CONTRIBUTING.md** - Contribution guidelines
- **lisp/efrit-do.el** - Main command execution (~1783 lines)
- **lisp/efrit-tools.el** - Tool implementations
- **.beads/beads.jsonl** - Issue tracker source of truth (versioned in git)

## Getting Help

- **Check existing issues**: `bd list --json`
- **Check documentation**: README.md, ARCHITECTURE.md, docs/
- **File unclear issues**: `bd create "Need clarification on X" -t task -p 2 --json`

## Success Metrics

You're doing well when:
- All work tracked in Beads (no markdown TODOs)
- No client-side intelligence added
- Changes are minimal and focused
- Tests pass (once test infrastructure is fixed)
- All work pushed to remote at end of session
- Issues linked with proper dependencies

---

**Remember**: Efrit is a Pure Executor. Claude does the thinking, Efrit does the executing. If you find yourself adding "smart" logic to Efrit, stop and rethink.

Happy coding! 🔗
