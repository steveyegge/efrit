# Efrit Troubleshooting Guide

This guide covers common issues, known bugs, and solutions for Efrit development and usage.

## Table of Contents

- [Quick Diagnosis](#quick-diagnosis)
- [Installation & Setup Issues](#installation--setup-issues)
- [Runtime Errors](#runtime-errors)
- [Known Issues & Workarounds](#known-issues--workarounds)
- [Development Issues](#development-issues)
- [MCP Server Issues](#mcp-server-issues)
- [Performance Problems](#performance-problems)
- [Getting More Help](#getting-more-help)

## Quick Diagnosis

### Is Efrit Loading?

```elisp
;; Test basic loading
(require 'efrit)
(message "Efrit version: %s" efrit-version)
```

If this fails, you have an installation or compilation issue. See [Installation & Setup Issues](#installation--setup-issues).

### Is the API Working?

```elisp
;; Check API key configuration
M-x efrit-chat
;; Try: "Say hello"
```

If you get API errors, see [API Configuration Issues](#api-configuration-issues).

### Check System Status

```bash
# Verify compilation
make compile

# Run basic tests
make test-simple

# Check for syntax errors
make check
```

## Installation & Setup Issues

### Problem: "Cannot open load file: efrit"

**Cause**: Emacs can't find efrit in its `load-path`.

**Solution**:
```elisp
;; Add to ~/.emacs.d/init.el
(add-to-list 'load-path "/absolute/path/to/efrit/lisp")
(require 'efrit)
```

**Verify**:
```elisp
M-: load-path  ; Check if efrit/lisp is listed
```

### Problem: "End of file during parsing"

**Cause**: Syntax error in Elisp file (unbalanced parentheses).

**Solution**:
```bash
# Check all files for syntax errors
make check

# Or check specific file
emacs --batch --eval "(check-parens)" lisp/efrit-file.el
```

**Common causes**:
- Missing closing parenthesis
- Extra opening parenthesis
- Unmatched quotes or strings

### Problem: Byte-compilation warnings

**Cause**: Code style issues or undefined functions.

**Solution**:
```bash
# Clean and recompile
make clean
make compile
```

**Note**: Some warnings are safe to ignore (e.g., "function might not be defined at runtime" for lazy-loaded functions).

### Problem: Missing lexical-binding header

**Cause**: Elisp files must have `;;; -*- lexical-binding: t -*-` header.

**Solution**:
```bash
# Check all files
make lint

# Fix manually - add to top of file:
;;; filename.el --- Description  -*- lexical-binding: t -*-
```

## API Configuration Issues

### Problem: "Invalid API key"

**Cause**: API key not configured or incorrect format.

**Solution**:

1. **Check ~/.authinfo** (recommended):
```bash
# ~/.authinfo should contain:
machine api.anthropic.com login personal password sk-ant-XXXXXXXX
```

2. **Or use environment variable**:
```bash
export ANTHROPIC_API_KEY="sk-ant-XXXXXXXX"
```

3. **Or set in Emacs**:
```elisp
(setq efrit-api-key "sk-ant-XXXXXXXX")  ; Not recommended - insecure
```

**Verify**:
```elisp
M-: (efrit-common-get-api-key)  ; Should return your key (will show in minibuffer)
```

### Problem: Using wrong API key for channel

**Cause**: Multi-channel setup (e.g., `ai-efrit` channel) using wrong key.

**Solution**:
```bash
# In ~/.authinfo, add channel-specific keys:
machine api.anthropic.com login personal password sk-ant-PERSONAL-KEY
machine api.anthropic.com login ai-efrit password sk-ant-AI-KEY

# Or set in Emacs:
(setq efrit-api-channel "ai-efrit")  ; Use ai-efrit channel
```

**See**: [docs/BYOK_CONFIGURATION.md](docs/BYOK_CONFIGURATION.md) for multi-channel setup.

### Problem: "Invalid model name" error

**Cause**: Using incorrect or outdated model identifier.

**Current valid models**:
```elisp
(setq efrit-model "claude-3-5-sonnet-20241022")  ; Default (recommended)
;; Other options:
;; "claude-3-opus-20240229"
;; "claude-3-sonnet-20240229"
;; "claude-3-haiku-20240307"
```

**Note**: Do NOT use `"claude-sonnet-4-20250514"` - this is invalid.

## Runtime Errors

### Problem: "Args out of range" or buffer errors

**Cause**: Efrit trying to access invalid buffer positions.

**Solution**:
```elisp
;; Check buffer state
M-x efrit-debug-mode  ; Enable debug logging

;; View debug log
(switch-to-buffer "*efrit-debug*")
```

**Workaround**: Restart Emacs and try again with a fresh buffer.

### Problem: Commands hang indefinitely

**Cause**: API call timeout or infinite loop.

**Solution**:
```elisp
;; Interrupt execution
C-g  ; Keyboard quit

;; Check async queue status
M-x efrit-async-status

;; Kill hanging background process
M-x list-processes
;; Select and kill the hanging process
```

**Known issue**: No timeout on API calls (see mcp-* for tracking).

### Problem: "Session not found" errors

**Cause**: Session cleanup or invalid session ID.

**Solution**:
```elisp
;; Clear all sessions
(efrit-performance-cleanup-old-sessions)

;; Start fresh
M-x efrit-do  ; Creates new session
```

### Problem: Unicode/encoding errors

**Cause**: Non-ASCII characters in buffers or responses.

**Solution**:
```elisp
;; Ensure UTF-8 encoding
(setq efrit-encoding 'utf-8)

;; Or set buffer-local encoding
(set-buffer-file-coding-system 'utf-8)
```

**Note**: Efrit should handle Unicode correctly. If this persists, file a bug.

## Known Issues & Workarounds

### Issue: Test Infrastructure Incomplete (mcp-fmw)

**Status**: Test runner partially working, some tests fail in batch mode.

**Workarround**:
```bash
# Use specific test scripts
make test-simple      # Works
make test-loop        # Works
make test-integration # Works but burns tokens

# Avoid:
make test  # May fail due to missing dependencies
```

**Tracking**: See issue `mcp-fmw` for status.

### Issue: MCP Server 50% Complete (mcp-b0f)

**Status**: MCP server skeleton exists but protocol implementation incomplete.

**What works**:
- Directory structure
- Basic TypeScript setup
- Test framework

**What's broken**:
- Protocol handler not implemented
- Tool registration incomplete
- No request routing

**Workaround**: Use efrit-remote-queue instead of MCP for agent communication.

**Tracking**: See issues `mcp-b0f`, `mcp-dnu`, `mcp-dsb` for progress.

### Issue: Tool Selection Loops (mcp-bt0)

**Status**: Known infinite loop with `todo_get_instructions` tool.

**Symptoms**:
- Same tool called repeatedly
- Session never completes
- Token usage spirals

**Workaround**:
```elisp
;; Not yet implemented - circuit breaker planned
;; Current mitigation: Use efrit-do (sync) instead of efrit-do-async
```

**Planned fixes**:
1. Schema-based tool prevention (mcp-it8)
2. Circuit breaker with hard limits (mcp-nyo)
3. Better loop detection (mcp-bt0)

**See**: [ARCHITECTURE.md](ARCHITECTURE.md) anti-patterns section.

### Issue: Async Workflow May Be Broken (mcp-6gr)

**Status**: Async execution needs validation.

**Symptoms**:
- Async commands don't complete
- Queue processing stalls
- Callbacks never fire

**Workaround**: Use synchronous execution until validated:
```elisp
M-x efrit-do        ; Instead of efrit-do-async
M-x efrit-chat      ; Instead of efrit-unified-do
```

**Tracking**: See issue `mcp-6gr` - "Validate Core Workflows End-to-End".

### Issue: efrit-do-async Interface Mismatch

**Status**: Known API inconsistency.

**Problem**: Tests expect callback parameter, but function doesn't accept it.

**Workaround**:
```elisp
;; Use internal function directly for testing
(efrit-async-execute-command "your command here")

;; Or use wrapper without callback
(efrit-do-async "your command here")
```

**See**: [docs/BUG_REPORT.md](docs/BUG_REPORT.md) for details.

## Development Issues

### Problem: "Function not defined" in batch mode

**Cause**: Module loading order issues.

**Solution**:
```elisp
;; In test files, explicitly require all dependencies
(require 'efrit-config)
(require 'efrit-log)
(require 'efrit-common)
(require 'efrit-tools)
;; ... etc
```

**See**: Makefile compilation dependency hierarchy (lines 61-83).

### Problem: Compilation fails with circular dependency

**Cause**: Incorrect `require` statements creating dependency loop.

**Solution**:
```bash
# Check dependency order in Makefile
cat Makefile | grep "lisp.*\.elc:"

# Build in dependency order
make clean
make compile
```

**Dependency hierarchy**:
1. efrit-config (foundation)
2. efrit-log (logging)
3. efrit-common (utilities)
4. efrit-tools (core tools)
5. Everything else

### Problem: Changes not taking effect

**Cause**: Old byte-compiled files cached.

**Solution**:
```bash
# Clean and rebuild
make clean
make compile

# Or in Emacs
M-x byte-recompile-directory RET /path/to/efrit/lisp RET
```

### Problem: Git conflicts in .beads/beads.jsonl

**Cause**: Concurrent updates to issue tracker.

**Solution**:
```bash
# Option A: Accept remote version
git checkout --theirs .beads/beads.jsonl
bd import

# Option B: Manually merge
# 1. Open .beads/beads.jsonl
# 2. Remove conflict markers
# 3. Keep all non-duplicate entries
bd import

# Verify
bd stats
```

**Prevention**: Always `git pull --rebase` before starting work.

## MCP Server Issues

### Problem: MCP server won't start

**Cause**: Dependencies not installed or build failed.

**Solution**:
```bash
cd mcp
npm install
npm run build
npm start
```

### Problem: TypeScript compilation errors

**Cause**: Incomplete server implementation.

**Solution**:
```bash
# Check what's implemented
ls mcp/src/

# Expected files:
# ✅ package.json, tsconfig.json
# ❌ server.ts (not implemented yet)
# ❌ tools.ts (not implemented yet)
```

**Note**: MCP server is 50% complete. See [docs/mcp-implementation-plan.md](docs/mcp-implementation-plan.md).

### Problem: MCP tests fail

**Cause**: Server implementation incomplete.

**Workaround**: Skip MCP tests for now:
```bash
make test-simple  # Skip MCP tests
```

**Tracking**: See issue `mcp-4nc` - "Add MCP server integration tests".

## Performance Problems

### Problem: Slow API responses

**Cause**: Network latency or Claude API load.

**Solution**:
```elisp
;; Enable response caching
(setq efrit-performance-cache-ttl 300)  ; 5 minutes

;; Check cache stats
M-x efrit-performance-show-stats
```

### Problem: Memory growth over time

**Cause**: Session accumulation.

**Solution**:
```elisp
;; Configure session cleanup
(setq efrit-performance-max-sessions 10)

;; Manual cleanup
(efrit-performance-cleanup-old-sessions)
```

### Problem: Work log too large

**Cause**: Long async sessions accumulate entries.

**Solution**:
```elisp
;; Limit work log size
(setq efrit-async-max-work-log-entries 50)

;; Work log auto-compresses when limit hit
```

**See**: [docs/PERFORMANCE_ANALYSIS.md](docs/PERFORMANCE_ANALYSIS.md) for details.

## Common Emacs/Elisp Issues

### Problem: "Symbol's value as variable is void: xyz"

**Cause**: Variable used before definition or module not loaded.

**Solution**:
```elisp
;; Check if variable is defined
M-: (boundp 'variable-name)

;; Ensure module is loaded
(require 'efrit-module)
```

### Problem: "Wrong type argument: stringp, nil"

**Cause**: Function expects string but got nil.

**Debug**:
```elisp
;; Enable debug mode
(setq debug-on-error t)

;; Or use edebug
M-x edebug-defun  ; On the function
;; Then call it and step through
```

### Problem: "Buffer is read-only"

**Cause**: Trying to modify protected buffer.

**Solution**:
```elisp
;; Make buffer writable temporarily
(let ((inhibit-read-only t))
  ;; Your modifications here
  )
```

## Debug Mode & Logging

### Enable Debug Logging

```elisp
;; Global debug mode
(setq efrit-debug-mode t)

;; API request/response logging
(setq efrit-log-api-calls t)

;; View logs
(switch-to-buffer "*efrit-debug*")
(switch-to-buffer "*Messages*")
```

### Trace Function Calls

```elisp
;; Trace specific function
(trace-function 'efrit-do)

;; Call the function
M-x efrit-do

;; View trace buffer
(switch-to-buffer "*trace-output*")

;; Stop tracing
(untrace-function 'efrit-do)
```

### Check Data Directories

```bash
# Inspect efrit data
ls -la ~/.emacs.d/.efrit/
ls -la ~/.emacs.d/.efrit/queues/
ls -la ~/.emacs.d/.efrit/sessions/
ls -la ~/.emacs.d/.efrit/logs/

# View recent activity
tail -f ~/.emacs.d/.efrit/logs/efrit.log
```

## Getting More Help

### Before Filing a Bug

1. **Check this troubleshooting guide** (you're here!)
2. **Search existing issues**: `bd list --json | grep -i "your topic"`
3. **Enable debug logging** and capture output
4. **Try minimal reproduction** (fresh Emacs, minimal config)

### File a Bug Report

```bash
# Create new issue
bd create "Bug: description of problem" -t bug -p 1 --json

# Include in description:
# - Emacs version: M-x emacs-version
# - Efrit version: (message efrit-version)
# - Steps to reproduce
# - Expected vs actual behavior
# - Debug logs if available
```

**See**: [docs/BUG_REPORT.md](docs/BUG_REPORT.md) for bug report template.

### Check Project Status

```bash
# See what's broken vs working
bd list --priority 0 --json  # Critical issues
bd ready --json              # What's ready to work on

# View specific issue
bd show mcp-xyz --json
```

### Documentation Resources

- [README.md](README.md) - User documentation
- [ARCHITECTURE.md](ARCHITECTURE.md) - Core design principles
- [CLAUDE.md](CLAUDE.md) - Instructions for AI agents
- [CONTRIBUTING.md](CONTRIBUTING.md) - Contribution guidelines
- [docs/](docs/) - Detailed guides

### Community & Support

This is an open-source project. For help:
1. Check documentation first
2. Search existing issues (`bd list --json`)
3. File detailed bug reports with reproduction steps
4. Consider contributing fixes (see [CONTRIBUTING.md](CONTRIBUTING.md))

---

**Last Updated**: 2025-11-21

**Note**: This is a living document. If you encounter issues not covered here, please file them so we can update this guide.
