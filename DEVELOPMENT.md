# Efrit Development Guide

Quick guide to hacking on Efrit.

## 5-Minute Quick Start

```bash
# Clone and set up
git clone https://github.com/steveyegge/efrit.git
cd efrit

# Configure API key
echo "machine api.anthropic.com login personal password YOUR_KEY" >> ~/.authinfo

# Compile and test
make compile
make test-simple

# Try it
emacs -L lisp -l efrit --eval "(efrit-chat)"
```

## Core Principle: Pure Executor

**Efrit NEVER contains intelligence. Claude does ALL the thinking.**

```
PROHIBITED in Efrit:
  - Pattern recognition, parsing, decision logic
  - Code generation, pre-written solutions
  - Task-specific heuristics, flow control

ALLOWED in Efrit:
  - Context gathering (buffer contents, file listings)
  - Tool execution (eval_sexp, shell_exec)
  - Result relay (returning output, errors)
```

If you're adding "smart" logic to Efrit, stop and rethink. Read [ARCHITECTURE.md](ARCHITECTURE.md).

## Project Structure

```
efrit/
├── lisp/                  # Emacs Lisp source
│   ├── efrit.el          # Entry point
│   ├── efrit-chat.el     # Chat interface
│   ├── efrit-do.el       # Command execution
│   ├── efrit-tools.el    # Tool implementations
│   └── ...
├── test/                  # Test files
├── mcp/                   # MCP server (TypeScript)
├── .beads/                # Issue tracker
└── *.md                   # Documentation
```

## Development Workflow

### Issue Tracking (Beads)

All work is tracked in Beads, not markdown TODOs.

```bash
bd ready --json                    # Find work
bd update ef-abc --status in_progress --json  # Claim it
# ... do work ...
bd close ef-abc --reason "Done" --json  # Complete it
bd sync && git push                # End of session (MANDATORY)
```

### Building and Testing

```bash
make compile       # Byte-compile
make test-simple   # Basic tests (no API calls)
make test          # Full test suite
make test-integration  # Real API calls (burns tokens)
```

## Code Standards

- **Lexical binding**: `;;; -*- lexical-binding: t; -*-` in all files
- **Naming**: `efrit-` prefix for public, `efrit--` for internal
- **Docstrings**: Required for all functions
- **Error handling**: Use `condition-case`

```elisp
(defun efrit-example (arg)
  "Do something with ARG."
  (condition-case err
      (do-something arg)
    (error
     (message "Failed: %s" err)
     nil)))
```

## Common Tasks

### Adding a New Tool

1. Add function to `lisp/efrit-tools.el`
2. Update schema in `efrit-do--get-tools-schema`
3. Add to tool handler
4. Write tests

### Fixing a Bug

```bash
bd create "Bug: description" -t bug -p 1 --json
# Write failing test, fix bug, verify
bd close ef-xyz --reason "Fixed" --json
```

## Submitting Changes

1. Fork and create feature branch
2. Make changes following code standards
3. Test: `make compile && make test-simple`
4. Commit with clear message (e.g., "Fix: handle empty API responses")
5. Submit PR with description and test results

## End of Session Checklist

**NEVER end without completing these steps:**

```bash
# 1. Update issues
bd close ef-abc --reason "Completed" --json
bd create "Follow-up" -t task -p 2 --json  # File remaining work

# 2. Quality gates
make compile && make test-simple

# 3. Sync and push (MANDATORY)
git pull --rebase && bd sync && git push

# 4. Verify clean state
git status  # Must show "up to date with origin/main"
```

**The session is NOT complete until `git push` succeeds.**

## Getting Help

```bash
# Diagnostics
emacs -L lisp --eval "(require 'efrit)" --eval "(efrit-doctor)"

# Check issues
bd list --status=open --json
```

- [ARCHITECTURE.md](ARCHITECTURE.md) - Design principles (required reading)
- [README.md](README.md) - User documentation
- [CLAUDE.md](CLAUDE.md) - AI agent instructions
