# Efrit - AI-Powered Emacs Assistant

An AI coding agent that enables autonomous development through direct Elisp evaluation.

**Core principle**: Zero client-side intelligence. Claude makes all decisions, Efrit executes.

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

Test with `M-x efrit-chat`.

## Usage

### Commands

| Command | Description |
|---------|-------------|
| `M-x efrit-chat` | Multi-turn conversational interface |
| `M-x efrit-do` | Synchronous command execution |
| `M-x efrit-do-async` | Asynchronous execution |
| `M-x efrit-unified-do` | Let Claude decide sync/async |

### Key Bindings

After loading, use `C-c C-e` followed by:
- `c` - Chat interface
- `d` - Sync command
- `D` - Async command
- `u` - Unified (Claude decides)
- `q` - Start remote queue
- `Q` - Queue status

### Examples

```
M-x efrit-do
> explain what buffer I'm in

M-x efrit-do-async
> analyze all elisp files and summarize their purpose

M-x efrit-chat
> Let's refactor the error handling in this file
```

## Agent Communication

AI agents can interact with Efrit via file-based JSON queue:

```elisp
(efrit-remote-queue-start)  ; Start the queue
```

**Request format:**
```json
{
  "id": "req_001",
  "version": "1.0.0",
  "type": "eval",
  "content": "(+ 40 2)",
  "timestamp": "2025-11-23T20:00:00Z"
}
```

Types: `eval` (elisp), `command` (natural language), `chat` (conversation)

**Response format:**
```json
{
  "id": "req_001",
  "version": "1.0.0",
  "status": "success",
  "result": "42",
  "timestamp": "2025-11-23T20:00:01Z"
}
```

## Configuration

```elisp
(setq efrit-model "claude-sonnet-4-20250514")
(setq efrit-max-tokens 8192)
(setq efrit-data-directory "~/.emacs.d/.efrit/")
```

## Data Directory

All data lives under `~/.emacs.d/.efrit/`:

```
.efrit/
├── cache/       # Temporary cache
├── context/     # Context persistence
├── queues/      # AI communication
├── logs/        # Debug logs
└── sessions/    # Session data
```

## Troubleshooting

**API connection issues:**
```elisp
M-x efrit-doctor  ; Run diagnostics
M-: (efrit-common-get-api-key)  ; Check key is found
```

**Debug logging:**
```elisp
(setq efrit-log-level 'debug)
```

## Development

See [DEVELOPMENT.md](DEVELOPMENT.md) for contributor guide.

```bash
make compile  # Build
make test     # Run tests
```

## Architecture

See [ARCHITECTURE.md](ARCHITECTURE.md) for design principles.

## License

Apache License 2.0. See [LICENSE](LICENSE).
