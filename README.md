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

### Commands

| Command | Description |
|---------|-------------|
| `M-x efrit-chat` | Multi-turn conversational interface |
| `M-x efrit-do` | Execute natural language command (sync) |
| `M-x efrit-do-async` | Execute command asynchronously |
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

### Key Bindings

Enable the global keymap:
```elisp
(efrit-setup-keybindings)  ; Binds C-c C-e prefix
```

Then use:
- `C-c C-e c` - Chat interface
- `C-c C-e d` - Sync command
- `C-c C-e D` - Async command
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
```

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

See [CONTRIBUTING.md](CONTRIBUTING.md) for contributor guidelines.

## Architecture

Efrit follows the **Pure Executor** principle:
- Claude decides what to do
- Efrit executes those decisions
- No pattern matching or heuristics in the client

See [ARCHITECTURE.md](ARCHITECTURE.md) for design details.

## License

Apache License 2.0. See [LICENSE](LICENSE).
