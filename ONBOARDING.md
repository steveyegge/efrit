# Efrit Onboarding Guide for New Maintainers

Welcome! This guide will help you start contributing to Efrit in 5 minutes.

## Efrit in 5 Minutes

**What is Efrit?** An AI-powered Emacs assistant built on the **Zero Client-Side Intelligence** principle: Efrit executes what Claude tells it to do. Nothing more, nothing less.

**See it in action:**

![Efrit creating four different poems in separate buffers](docs/images/efrit-poems.jpg)

*Efrit handling a complex multi-part request through natural language*

### Quick Start

```bash
# 1. Clone and set up
git clone https://github.com/steveyegge/efrit.git
cd efrit

# 2. Configure API key
echo "machine api.anthropic.com login personal password YOUR_KEY" >> ~/.authinfo

# 3. Compile and test
make compile
make test-simple

# 4. Try it
emacs -L lisp -l efrit --eval "(efrit-chat)"
```

You're ready to contribute!

## Core Philosophy: Pure Executor Principle

**CRITICAL**: Understand this before making any code changes.

### The Golden Rule

**Efrit NEVER contains intelligence. Claude does ALL the thinking.**

```
❌ PROHIBITED in Efrit:
  - Pattern recognition (parsing errors/warnings)
  - Decision logic (which tool to use next)
  - Code generation (pre-written solutions)
  - Task-specific heuristics
  - Flow control decisions

✅ ALLOWED in Efrit:
  - Context gathering (buffer contents, file listings)
  - Tool execution (eval_sexp, shell_exec)
  - Result relay (returning output, errors)
  - State persistence (sessions, logs)
  - Basic validation (syntax, security)
```

### Why This Matters

**WRONG approach** (violates Pure Executor):
```elisp
;; ❌ Client-side intelligence - NEVER do this
(when (string-match "Warning.*lexical-binding" warning)
  (efrit-fix-lexical-binding file))
```

**RIGHT approach** (Pure Executor):
```elisp
;; ✅ Just give Claude the raw data
(with-current-buffer "*Warnings*" (buffer-string))
;; Let Claude analyze and decide what to do
```

**Read the full architecture**: [ARCHITECTURE.md](ARCHITECTURE.md) - This is mandatory reading!

## What's Working / What's Broken

### ✅ What Works

- **Core execution**: `efrit-chat`, `efrit-do` work reliably
- **API communication**: Claude API integration stable
- **Session management**: Multi-turn conversations work
- **Remote queue**: AI-to-AI communication functional
- **Tool system**: Core tools (eval_sexp, shell_exec, buffer ops) solid
- **Build system**: `make compile`, `make test-simple` work
- **Issue tracking**: Using `bd` (beads) for all work tracking

### ⚠️ Known Issues

- **Test infrastructure**: Integration tests incomplete (ef-4nc, ef-zlw)
- **MCP server**: Only ~50% complete (ef-2z5 epic)
- **Async workflow**: May have edge cases (ef-6gr)
- **Tool selection loops**: `todo_get_instructions` loops detected (ef-bt0)
- **Documentation**: Scattered across multiple files, needs consolidation
- **Performance**: Some opportunities for optimization (ef-2z5)

### 🎯 Current Focus

Working toward **Efrit Modernization for Community Readiness** (ef-hsl):
- ✅ Development experience modernized (ef-9qu) - **COMPLETE**
- 🔄 Community readiness (ef-2kq) - **IN PROGRESS**
- 📋 MCP server completion (ef-2z5) - **NEXT**
- 📋 Documentation consolidation - **NEXT**

Check latest status: `bd stats && bd ready --json`

## Quick Wins: Good First Issues

### Documentation Tasks (Easy, High Impact)

These are perfect for first contributions:

1. **Update inline docstrings** (P2-P3)
   - Many functions lack detailed documentation
   - Look for functions without docstrings in `lisp/*.el`
   - Example: `grep -r "^(defun efrit-" lisp/ | grep -v '"\w'`

2. **Improve error messages** (P2-P3)
   - Make error messages more user-friendly
   - Add context about what went wrong and how to fix it
   - Check `*Messages*` buffer for cryptic errors

3. **Add code examples** (P3)
   - Document common use cases in docstrings
   - Add examples to README.md
   - Show real workflows

### Code Cleanup Tasks (Easy, Safe)

4. **Remove unused dependencies** (ef-svx, P4)
   - Remove unused `uuid` dependency (using `crypto.randomUUID` now)
   - Check for other unused imports
   - Run: `grep -r "require '" lisp/ | sort | uniq`

5. **Align constant naming** (ef-q58, P3)
   - MCP tool names vs constants need alignment
   - Simple find-and-replace work
   - Located in `mcp/src/`

6. **Import schema version from package.json** (ef-uyu, P3)
   - Remove hardcoded version strings
   - Use single source of truth
   - File: `mcp/src/schema.ts`

### Testing Tasks (Medium, Important)

7. **Add MCP integration tests** (ef-m63, P2)
   - Write tests for MCP protocol communication
   - Use existing test patterns from `test/`
   - See [DEVELOPMENT.md](DEVELOPMENT.md) section "Remote Queue System"

8. **Fix queue stats implementation** (ef-53o, P2)
   - Complete unimplemented queue statistics
   - Or return null for unimplemented fields
   - File: `lisp/efrit-remote-queue.el`

### How to Claim an Issue

```bash
# 1. Find work
bd ready --json

# 2. Claim it
bd update ef-xyz --status in_progress --json

# 3. Work on it (read files, make changes, test)
make compile && make test-simple

# 4. Complete it
bd close ef-xyz --reason "Fixed in lisp/efrit-foo.el:123" --json
bd sync && git push
```

## How to Get Help

### Before Asking

1. **Read the docs**:
   - [README.md](README.md) - User guide
   - [ARCHITECTURE.md](ARCHITECTURE.md) - Design principles **[REQUIRED]**
   - [DEVELOPMENT.md](DEVELOPMENT.md) - Developer guide
   - [CONTRIBUTING.md](CONTRIBUTING.md) - Contribution process

2. **Check existing issues**:
   ```bash
   bd list --status=open --json | grep -i "your topic"
   ```

3. **Run diagnostics**:
   ```bash
   emacs -L lisp --eval "(require 'efrit)" --eval "(efrit-doctor)"
   ```

### Getting Help

- **GitHub Issues**: [github.com/steveyegge/efrit/issues](https://github.com/steveyegge/efrit/issues)
  - Use issue templates (bug, feature, docs)
  - Include version info, error messages, minimal repro

- **GitHub Discussions**: [github.com/steveyegge/efrit/discussions](https://github.com/steveyegge/efrit/discussions)
  - General questions
  - Architecture discussions
  - Feature brainstorming

- **Code Questions**: Tag `@steveyegge` in issues/PRs for architectural guidance

### Common Issues

**"My API calls fail"**
- Check `~/.authinfo` has correct format
- Run `M-x efrit-doctor` to diagnose
- Verify key works: `curl https://api.anthropic.com/v1/messages -H "x-api-key: YOUR_KEY"`

**"Compilation fails"**
- Clean build: `make clean && make compile`
- Check for syntax errors: `emacs --batch -L lisp -l efrit.el`
- File an issue if it's not your changes

**"Tests don't pass"**
- Start simple: `make test-simple` (no API calls)
- Then try: `make test-loop` (loop detection, safe)
- Integration tests burn tokens: `make test-integration` (use sparingly)

## Development Workflow

### Standard Workflow

1. **Check for ready work**: `bd ready --json`
2. **Claim a task**: `bd update ef-abc --status in_progress --json`
3. **Make changes**: Edit files, keeping Pure Executor principle
4. **Test**: `make compile && make test-simple`
5. **Close task**: `bd close ef-abc --reason "Done" --json`
6. **Sync**: `bd sync && git push` (**MANDATORY** at end of session)

### Key Commands

```bash
# Issue tracking
bd ready --json                    # Find ready work
bd show ef-abc --json              # View issue details
bd list --status=open --json       # All open issues
bd create "Task" -t task -p 2 --json  # Create issue
bd update ef-abc --status in_progress --json
bd close ef-abc --reason "Fixed" --json

# Building
make compile         # Byte-compile elisp
make clean          # Remove compiled files
make test-simple    # Basic tests (no API)
make test          # Full test suite

# Development utilities
emacs -L lisp --eval "(require 'efrit)" --eval "(efrit-version)"
emacs -L lisp --eval "(require 'efrit)" --eval "(efrit-doctor)"
emacs -L lisp --eval "(require 'efrit)" --eval "(efrit-run-tests)"
```

### End of Session (CRITICAL)

**NEVER end a session without completing these steps:**

```bash
# 1. Update beads
bd close ef-abc ef-xyz --reason "Completed" --json
bd create "Follow-up" -t task -p 2 --json  # File remaining work

# 2. Quality gates (if you changed code)
make compile  # Must succeed
make test-simple  # Should pass

# 3. Sync and push (MANDATORY)
git pull --rebase
bd sync
git push  # MUST succeed before stopping

# 4. Verify
git status  # Should show "up to date with origin/main"
```

**The session is NOT complete until `git push` succeeds!**

## Important Rules

### ✅ DO:
- Read [ARCHITECTURE.md](ARCHITECTURE.md) before making changes
- Use `bd` for ALL task tracking
- Keep changes minimal and focused
- Run `make compile` before committing
- Push to remote at end of session
- Respect the Pure Executor principle

### ❌ DON'T:
- Add client-side intelligence (pattern matching, heuristics)
- Create markdown TODO lists (use `bd` instead)
- Make changes beyond what's requested
- Skip compilation checks
- End session without pushing to remote
- Violate the Zero Client-Side Intelligence principle

## Architecture at a Glance

```
efrit/
├── lisp/                  # All Emacs Lisp source
│   ├── efrit.el          # Entry point
│   ├── efrit-chat.el     # Chat interface
│   ├── efrit-do.el       # Sync command execution (~1783 lines)
│   ├── efrit-async.el    # Async execution
│   ├── efrit-tools.el    # Tool implementations
│   ├── efrit-remote-queue.el  # AI-to-AI communication
│   └── ...               # Supporting modules
├── test/                  # Test files
├── mcp/                   # MCP server (TypeScript/Node)
├── .beads/                # Issue tracker database
├── ARCHITECTURE.md        # Core design principles [READ THIS]
├── DEVELOPMENT.md         # Developer guide
└── ONBOARDING.md          # This file
```

## Your First Contribution

**Recommended path for your first PR:**

1. **Start with documentation** (lowest risk, high value)
   - Fix typos in README.md
   - Add missing docstrings
   - Improve error messages

2. **Move to code cleanup** (low risk, learning)
   - Remove unused dependencies
   - Align constant naming
   - Simplify configurations

3. **Then try features/fixes** (medium risk, high value)
   - Add tests
   - Implement small tools
   - Fix bugs

**Before your first PR:**
- ✅ Read [ARCHITECTURE.md](ARCHITECTURE.md) (required!)
- ✅ Read [CONTRIBUTING.md](CONTRIBUTING.md)
- ✅ Run `make compile && make test-simple`
- ✅ Claim your issue: `bd update ef-xyz --status in_progress --json`

## Success Metrics

You're doing well when:
- ✅ All work tracked in beads (no markdown TODOs)
- ✅ No client-side intelligence added
- ✅ Changes are minimal and focused
- ✅ Tests pass: `make test-simple`
- ✅ All work pushed to remote at end of session
- ✅ Issues properly linked and closed

## Additional Resources

- **User Guide**: [README.md](README.md)
- **Architecture**: [ARCHITECTURE.md](ARCHITECTURE.md) - **MANDATORY READING**
- **Developer Guide**: [DEVELOPMENT.md](DEVELOPMENT.md)
- **Contributing**: [CONTRIBUTING.md](CONTRIBUTING.md)
- **Troubleshooting**: [TROUBLESHOOTING.md](TROUBLESHOOTING.md)
- **AI Instructions**: [CLAUDE.md](CLAUDE.md) - For AI agents

---

**Remember**: Efrit is a Pure Executor. Claude does the thinking, Efrit does the executing. If you find yourself adding "smart" logic to Efrit, stop and rethink.

**Welcome to the team! Let's build something amazing together.** 🚀
