# Efrit Development Guide

> **Quick Start**: Get hacking on Efrit in 5 minutes.

## Prerequisites

- **Emacs 28.1+** (check with `emacs --version`)
- **Claude API key** from [Anthropic Console](https://console.anthropic.com/)
- **Make** (for build automation)
- **Git** (for version control)

## 5-Minute Quick Start

```bash
# 1. Clone and navigate
git clone https://github.com/steveyegge/efrit.git
cd efrit

# 2. Set up API key in ~/.authinfo
echo "machine api.anthropic.com login personal password YOUR_API_KEY_HERE" >> ~/.authinfo

# 3. Compile the code
make compile

# 4. Run tests (safe - no API calls)
make test-simple

# 5. Try it out
emacs -L lisp -l efrit --eval "(efrit-chat)"
```

You're ready to develop!

## Architecture in 2 Minutes

**ZERO CLIENT-SIDE INTELLIGENCE** is Efrit's core principle:

- **Efrit's job**: Execute what Claude tells it to do
- **Claude's job**: All thinking, planning, and decision-making
- **Never add to Efrit**: Pattern matching, heuristics, task-specific logic, pre-written solutions

### What Goes Where?

```
✅ ALLOWED in Efrit:
  - Context gathering (buffer contents, file listings)
  - Tool execution (eval_sexp, shell_exec)
  - Result relay (returning output, errors)
  - State persistence (sessions, logs)

❌ PROHIBITED in Efrit:
  - Pattern recognition (parsing errors)
  - Decision logic (which tool to use)
  - Code generation (pre-written solutions)
  - Flow control (when to stop/continue)
```

**Remember**: If you're adding "smart" logic to Efrit, you're violating the architecture. Stop and rethink.

See [ARCHITECTURE.md](ARCHITECTURE.md) for the complete design philosophy.

## Project Structure

```
efrit/
├── lisp/                    # All Emacs Lisp source
│   ├── efrit.el            # Entry point & autoloads
│   ├── efrit-config.el     # Configuration & paths
│   ├── efrit-chat.el       # Interactive chat interface
│   ├── efrit-do.el         # Command execution (sync)
│   ├── efrit-async.el      # Async command execution
│   ├── efrit-unified.el    # Unified sync/async interface
│   ├── efrit-tools.el      # Tool implementations
│   ├── efrit-remote-queue.el # AI-to-AI communication
│   └── ...                 # Supporting modules
├── test/                    # Test files
├── mcp/                     # MCP server (TypeScript/Node)
├── docs/                    # Additional documentation
├── .beads/                  # Issue tracker (beads database)
├── Makefile                # Build automation
└── *.md                    # Documentation
```

## Development Workflow

### Issue Tracking with Beads

**CRITICAL**: All work is tracked in Beads, not markdown TODOs.

```bash
# Find work
bd ready --json                    # Show ready-to-work issues
bd list --status=open --json       # All open issues

# Create issues
bd create "Fix bug X" -t bug -p 1 --json
bd create "Add feature Y" -t feature -p 2 --json

# Work on issues
bd update ef-abc --status in_progress --json
# ... do the work ...
bd close ef-abc --reason "Implemented" --json

# End of session (MANDATORY)
bd sync        # Export to JSONL, commit
git push       # Push to remote
```

**Issue types**: `bug`, `feature`, `task`, `epic`, `chore`
**Priorities**: `0` (critical) to `4` (backlog)

### Building and Testing

```bash
# Compilation
make compile       # Byte-compile all .el files
make clean         # Remove compiled files

# Testing
make test-simple   # Basic tests (no API calls)
make test-loop     # Loop detection tests (safe)
make test          # Full test suite

# Integration testing (⚠️ BURNS TOKENS!)
make test-integration  # Real API calls

# Code quality
make check         # Syntax validation
make lint          # Style checks
make ci            # Full CI pipeline (check + lint + compile + test)

# Development utilities
emacs -L lisp --eval "(require 'efrit)" --eval "(efrit-version)"
emacs -L lisp --eval "(require 'efrit)" --eval "(efrit-doctor)"
```

### The Toolchest: Choosing the Right Approach

#### 1. Direct File Access (Read/Edit/Write)
**Use for**: Modifying source code, documentation

```
Edit lisp/efrit-do.el
Write test/new-test.el
Read lisp/efrit-tools.el
```

#### 2. Bash + Batch Emacs
**Use for**: Compilation, unit tests, quick evaluation

```bash
make compile
emacs --batch -L lisp -l test/test-foo.el -f ert-run-tests-batch-and-exit
emacs --batch --eval "(+ 1 2)"
```

#### 3. Remote Queue System
**Use for**: Protocol validation, integration testing

```bash
# Start daemon
emacs --daemon=efrit-test --load lisp/dev/efrit-autonomous-startup.el

# Send request
cat > ~/.emacs.d/.efrit/queues/requests/test_$(date +%s).json <<EOF
{
  "id": "test-$(date +%s)",
  "version": "1.0.0",
  "type": "eval",
  "content": "(+ 1 2)",
  "timestamp": "$(date -u +%Y-%m-%dT%H:%M:%SZ)"
}
EOF

# Check response
sleep 0.5
cat ~/.emacs.d/.efrit/queues/responses/resp_test-*.json
```

## Common Development Tasks

### Adding a New Tool

1. **Add function to `lisp/efrit-tools.el`**:
```elisp
(defun efrit-tools-my-new-tool (arg)
  "Do something with ARG."
  ;; Implementation
  )
```

2. **Update tool schema in `efrit-do--get-tools-schema`**:
```elisp
((name . "my_new_tool")
 (description . "Does something useful")
 (input_schema
  (type . "object")
  (properties
   (arg (type . "string") (description . "The argument")))
  (required . ["arg"])))
```

3. **Add to tool handler**:
```elisp
((string= tool-name "my_new_tool")
 (efrit-tools-my-new-tool (alist-get 'arg input)))
```

4. **Write tests**:
```elisp
(ert-deftest test-my-new-tool ()
  (should (equal (efrit-tools-my-new-tool "foo") expected-result)))
```

5. **Document**: Update tool list in documentation

### Fixing a Bug

```bash
# 1. File issue
bd create "Bug: description" -t bug -p 1 --json

# 2. Reproduce with a test
emacs -L lisp -l test/test-foo.el  # Add failing test

# 3. Fix the bug
Edit lisp/efrit-module.el

# 4. Verify fix
make compile
make test-simple

# 5. Close issue
bd close ef-xyz --reason "Fixed in lisp/efrit-module.el:123" --json
```

### Debugging Loop Detection Issues

Efrit has protection against infinite loops. To debug:

```bash
# Run loop detection test (safe, no API)
make test-loop

# Check loop detection logic in:
# - lisp/efrit-do.el (efrit-do--detect-loop function)
# - lisp/efrit-session-tracker.el (session state tracking)
```

### Testing with Real API

**⚠️ WARNING: This burns API tokens!**

```bash
# Interactive testing
emacs -L lisp -l efrit --eval "(efrit-chat)"

# Programmatic testing
make test-integration

# Test specific functionality
emacs --batch -L lisp -l test/test-real-integration.el
```

## Code Standards

### Emacs Lisp Conventions

- **Lexical binding**: Always use `;;; -*- lexical-binding: t; -*-`
- **Naming**: Use `efrit-` prefix for public functions, `efrit--` for internal
- **Docstrings**: Required for all functions
- **Dependencies**: Declare all `require` statements at file top
- **Error handling**: Use `condition-case` for robust error recovery

### Example

```elisp
;;; efrit-example.el --- Example module -*- lexical-binding: t; -*-

(require 'efrit-config)
(require 'efrit-log)

(defun efrit-example-public-function (arg)
  "Do something with ARG.
This is a public function (no double-dash in name)."
  (efrit--example-helper arg))

(defun efrit--example-helper (arg)
  "Internal helper for ARG.
This is internal (double-dash in name)."
  (condition-case err
      (do-something-risky arg)
    (error
     (efrit-log 'error "Failed to process %s: %s" arg err)
     nil)))

(provide 'efrit-example)
;;; efrit-example.el ends here
```

## MCP Server Development

The MCP (Model Context Protocol) server enables Efrit to work with Claude Desktop and other MCP clients.

```bash
# Install dependencies
cd mcp && npm install

# Build
npm run build

# Test
npm test

# Start server
npm start

# Development mode (auto-rebuild)
npm run dev

# Clean build artifacts
npm run clean
```

See [mcp/README.md](mcp/README.md) for details.

## End of Session Checklist

Before ending your development session:

### 1. File Issues for Remaining Work
```bash
bd create "Follow-up: X" -t task -p 2 --json
```

### 2. Run Quality Gates
```bash
make compile  # Must succeed
make test     # Should pass (when working)
```

### 3. Update Beads
```bash
bd close ef-abc ef-xyz --reason "Completed" --json
```

### 4. Sync and Push (MANDATORY)
```bash
git pull --rebase
bd sync
git push  # MUST succeed before session ends
git status  # Verify: "up to date with origin/main"
```

**CRITICAL**: The session is NOT complete until `git push` succeeds.

### 5. Verify Clean State
```bash
git status  # Should be clean
bd stats    # Check issue tracker state
```

## Useful Resources

- **Architecture**: [ARCHITECTURE.md](ARCHITECTURE.md) - Pure Executor principle
- **Contributing**: [CONTRIBUTING.md](CONTRIBUTING.md) - Contribution guidelines
- **User Guide**: [README.md](README.md) - User-facing documentation
- **AI Instructions**: [CLAUDE.md](CLAUDE.md) - Instructions for AI agents
- **Troubleshooting**: [TROUBLESHOOTING.md](TROUBLESHOOTING.md) - Common issues

## Getting Help

```bash
# Check installation health
emacs -L lisp --eval "(require 'efrit)" --eval "(efrit-doctor)"

# Check version
emacs -L lisp --eval "(require 'efrit)" --eval "(efrit-version)"

# Run all tests
emacs -L lisp --eval "(require 'efrit)" --eval "(efrit-run-tests)"

# Check for open issues
bd list --status=open --json | jq -r '.[] | "\(.id): \(.title)"'
```

## Development Best Practices

### DO:
- ✅ Use `bd` for all task tracking
- ✅ Run `make compile` before committing
- ✅ Add tests for new features
- ✅ Keep changes focused and minimal
- ✅ Respect the Pure Executor principle
- ✅ Push to remote at end of session

### DON'T:
- ❌ Create markdown TODO lists
- ❌ Add client-side intelligence
- ❌ Make changes beyond what's needed
- ❌ Skip compilation checks
- ❌ End session without pushing
- ❌ Clutter repo root with planning docs

---

**Happy Hacking!** Remember: Efrit is a Pure Executor. Claude does the thinking, Efrit does the executing.
