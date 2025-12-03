# Efrit - AI-Powered Emacs Coding Assistant

**Version 0.4.0**

Efrit is an AI coding agent that brings Claude's intelligence directly into Emacs. It can execute natural language commands, explore codebases, run shell commands, and have multi-turn conversations - all while you stay in your editor.

**Core principle**: Zero client-side intelligence. Claude makes all decisions, Efrit executes.

## Features

### Conversational Chat (`efrit-chat`)
Multi-turn conversations with Claude that understand your Emacs context:
- Ask questions about code, get explanations
- Refactor with guidance
- Debug issues interactively

### Agentic Task Execution (`efrit-do`)
Execute natural language commands that Claude translates to actions:
- "Create a buffer with today's date"
- "List all elisp files in this directory and count them"
- "Search for the function definition of `efrit-execute`"
- "Run git status and show me the output"

### Agent Session Buffer (`efrit-agent`)
A structured, real-time view of agentic sessions with:
- Session status and elapsed time
- Task progress tracking (TODOs)
- Activity log with expandable tool calls
- Interactive input for follow-up commands

### Rich Tool Suite
Efrit provides Claude with 15+ tools:

| Category | Tools |
|----------|-------|
| **Code Execution** | `eval_sexp` (elisp), `shell_exec` (shell commands) |
| **Codebase Exploration** | `search_content`, `read_file`, `project_files`, `glob_files` |
| **Version Control** | `vcs_status`, `vcs_diff`, `vcs_log`, `vcs_blame` |
| **Buffer Management** | `buffer_create`, `display_in_buffer` |
| **Safety** | `confirm_action`, `checkpoint`, `restore_checkpoint` |
| **External** | `web_search`, `fetch_url`, `read_image` |

### Robust Error Handling
- Circuit breaker prevents infinite loops
- Automatic error recovery and retry
- Security controls on shell commands

### AI-to-AI Communication
Other AI agents (Claude Code, Cursor, etc.) can interact with Efrit via file-based JSON queue for autonomous development.

## Installation

### Prerequisites
- Emacs 28.1+
- Anthropic API key from [console.anthropic.com](https://console.anthropic.com/)

### Quick Start

```bash
git clone https://github.com/steveyegge/efrit.git
cd efrit
```

Add to `~/.emacs.d/init.el`:

```elisp
(add-to-list 'load-path "/path/to/efrit/lisp")
(require 'efrit)
```

### API Key Configuration

**Option A: Encrypted authinfo (recommended)**

Create `~/.authinfo.gpg`:
```
machine api.anthropic.com login personal password YOUR_API_KEY_HERE
```

**Option B: Environment variable**
```bash
export ANTHROPIC_API_KEY="sk-your-key-here"
```

**Option C: Plain authinfo**

Create `~/.authinfo`:
```
machine api.anthropic.com login personal password YOUR_API_KEY_HERE
```

Verify with `M-x efrit-doctor`.

## Usage

### When to Use Each Mode

| Mode | Use When |
|------|----------|
| **`efrit-chat`** | Multi-turn conversations, asking questions, getting explanations. Tools: buffer-centric only (read current buffer, search). |
| **`efrit-do`** | Single agentic commands, project-wide refactoring, file creation/editing, running tests. Tools: full suite (30+ tools including write_file, shell_exec, vcs). |
| **`efrit-agent`** | Same as `efrit-do` but with a structured session view showing TODOs, tool calls, and progress. Best for complex multi-step tasks. |

**Quick guide:**
- Need to ask about code? → `efrit-chat`
- Need to change code or run commands? → `efrit-do` or `efrit-agent`
- Long-running task with multiple steps? → `efrit-agent` (better visibility)

### Commands

| Command | Description |
|---------|-------------|
| `M-x efrit-chat` | Multi-turn conversational interface |
| `M-x efrit-do` | Execute natural language command asynchronously with progress buffer |
| `M-x efrit-agent` | Open the structured agent session buffer |
| `M-x efrit-do-sync` | Execute natural language command synchronously (blocking) |
| `M-x efrit-do-silently` | Execute command asynchronously without showing progress buffer |
| `M-x efrit-do-show-progress` | Show the progress buffer for the active command |
| `M-x efrit-do-show-queue` | Show commands queued for execution |
| `M-x efrit-doctor` | Run diagnostics |
| `M-x efrit-help` | Show help |

### Examples

**Simple queries:**
```
M-x efrit-do RET
> what buffer am I in?
```

**Code exploration:**
```
M-x efrit-do RET
> search for all uses of 'defcustom' in this project and list them
```

**Multi-step tasks:**
```
M-x efrit-do RET
> create a new elisp file with a function that reverses a string, include proper headers
```

**Shell integration:**
```
M-x efrit-do RET
> run git log --oneline -10 and summarize the recent changes
```

**Conversational:**
```
M-x efrit-chat RET
You: Help me understand how the error handling works in this file
Assistant: [analyzes current buffer and explains]
You: Can you refactor it to use condition-case-unless-debug instead?
```

### Async Execution (Default Behavior)

By default, `efrit-do` executes asynchronously:

```
M-x efrit-do RET
> create a buffer with a function to reverse strings
```

Efrit automatically:
1. Displays a **progress buffer** with real-time updates on Claude's thinking and tool execution
2. Supports **interruption** with `C-g` (keyboard-quit)
3. **Queues commands** if another command is already running (use `efrit-do-show-queue` to view)
4. Shows **session status** in the minibuffer and modeline

The progress buffer shows:
- Claude's reasoning and tool calls
- Results from each step
- Time elapsed

### Synchronous Execution (Legacy)

For simpler tasks or scripting, use `efrit-do-sync` for blocking execution:

```elisp
(efrit-do-sync "list all python files in current directory")
```

This is the older API that waits for completion before returning.

### Background Execution

To run a command without showing the progress buffer:

```
M-x efrit-do-silently RET
> long running task...
```

Progress is visible in the minibuffer. Show the buffer later with `M-x efrit-do-show-progress`.

### Key Bindings

Enable the global keymap:
```elisp
(efrit-setup-keybindings)  ; Binds C-c C-e prefix
```

Then use:
- `C-c C-e c` - Chat interface
- `C-c C-e d` - Async command with progress buffer
- `C-c C-e D` - Async command in background
- `C-c C-e q` - Start remote queue
- `C-c C-e Q` - Queue status

## Agent Communication (MCP)

AI agents can interact with Efrit via the Model Context Protocol:

```elisp
(efrit-remote-queue-start)  ; Start the queue
```

**Request format:**
```json
{
  "id": "req_001",
  "version": "1.0.0",
  "type": "eval",
  "content": "(buffer-name)",
  "timestamp": "2025-11-28T20:00:00Z"
}
```

Types: `eval` (elisp), `command` (natural language), `chat` (conversation), `status` (health check)

**Response format:**
```json
{
  "id": "req_001",
  "version": "1.0.0",
  "status": "success",
  "result": "*scratch*",
  "timestamp": "2025-11-28T20:00:01Z"
}
```

See [mcp/README.md](mcp/README.md) for MCP server setup.

## Migration Guide

If you have existing keybindings or configurations using older Efrit APIs:

**Old name** → **New name** | **Status**
--- | --- | ---
`efrit-do` (sync) | `efrit-do-sync` | Deprecated
`efrit-do-async` | `efrit-do` | Primary interface
`efrit-do-async-legacy` | — | Do not use

If you have in your `init.el`:
```elisp
;; OLD: sync command (blocking)
(global-set-key (kbd "C-c C-e d") 'efrit-do)

;; NEW: use async command with progress buffer
(global-set-key (kbd "C-c C-e d") 'efrit-do)
```

Or if you have code calling `efrit-do-async`:
```elisp
;; OLD: explicit async
(efrit-do-async "your command")

;; NEW: efrit-do is async by default
(efrit-do "your command")
```

The async API is now recommended for all use cases. Use `efrit-do-sync` only if you need blocking behavior (e.g., in scripts where waiting for completion is required).

## Configuration

```elisp
;; Model selection (default: claude-sonnet-4-5-20250929)
(setq efrit-default-model "claude-sonnet-4-5-20250929")

;; Max tokens per response
(setq efrit-max-tokens 8192)

;; Data directory
(setq efrit-data-directory "~/.emacs.d/.efrit/")

;; Enable debug logging
(setq efrit-log-level 'debug)

;; Progress buffer configuration
(setq efrit-do-show-progress-buffer t)  ; Show progress buffer automatically
(setq efrit-do-queue-max-size 10)       ; Max commands to queue
```

### Shell Command Security

Efrit restricts which shell commands Claude can execute. By default, a wide range of development tools are allowed (git, npm, cargo, go, python, etc.).

```elisp
;; Disable all shell security checks (trust Claude completely)
(setq efrit-do-shell-security-enabled nil)

;; OR allow all commands but keep forbidden pattern checks
(setq efrit-do-allowed-shell-commands '("*"))

;; Add additional allowed commands
(add-to-list 'efrit-do-allowed-shell-commands "mycustomtool")

;; Disable forbidden pattern checking
(setq efrit-do-forbidden-shell-patterns nil)
```

The default forbidden patterns block truly dangerous commands like `sudo`, `shutdown`, and shell command injection via `$(...)` or backticks.

## Data Directory

All data lives under `~/.emacs.d/.efrit/`:

```
.efrit/
├── cache/       # Temporary cache
├── context/     # Context persistence
├── queues/      # AI communication (requests/responses/processing/archive)
├── logs/        # Debug logs
└── sessions/    # Session data
```

## Troubleshooting

**Check installation:**
```elisp
M-x efrit-doctor
```

**API issues:**
```elisp
M-: (efrit-common-get-api-key)  ; Should return your key
```

**Debug logging:**
```elisp
(setq efrit-log-level 'debug)
M-x efrit-log-show
```

**View errors:**
```elisp
M-x efrit-show-errors
```

## Development

```bash
make compile  # Byte-compile elisp
make test     # Run test suite
```

See [DEVELOPMENT.md](DEVELOPMENT.md) for contributor guidelines.

## Architecture

Efrit follows the **Pure Executor** principle:
- Claude decides what to do
- Efrit executes those decisions
- No pattern matching or heuristics in the client

See [ARCHITECTURE.md](ARCHITECTURE.md) for design details.

## License

Apache License 2.0. See [LICENSE](LICENSE).
