# Efrit - AI-Powered Autonomous Emacs Assistant

*A sophisticated AI coding agent that enables autonomous development through direct Elisp evaluation and agent-to-agent communication.*

## 🚀 What's New: Session-Based Architecture & Performance

**Efrit v0.3.0 features advanced session management and performance optimizations**:

- **🔄 Session-Based Architecture**: Multi-step operations with Claude-controlled flow and context preservation
- **⚡ Async Execution**: Non-blocking commands with queue management for responsive interaction
- **📊 Performance Optimizations**: Response caching, memory management, and API call tracking
- **🤖 Multi-Channel Support**: Support for different API channels (default, ai-efrit) with channel-specific keys
- **🔧 Self-Enhancing Capabilities**: AI agents can debug, test, and enhance Efrit's functionality
- **💬 Unified Interface**: Claude decides whether commands run synchronously or asynchronously
- **🐛 Bug Fixes**: Fixed critical syntax errors, API key selection, and model configuration

*Zero client-side intelligence - Claude makes all decisions about execution mode and flow.*

---

## Overview

Efrit provides multiple interfaces for AI-powered Emacs development:

- **efrit-chat** - Multi-turn conversational interface for complex discussions  
- **efrit-do** - Synchronous natural language command execution
- **efrit-do-async** - 🆕 Asynchronous command execution with session management
- **efrit-unified-do** - 🆕 Claude decides sync/async execution mode
- **efrit-remote-queue** - File-based channel for AI agent communication
- **efrit-streamlined-send** - Streamlined chat interface

## Key Features

### Core Capabilities
- **Direct Elisp Evaluation**: Full access to Emacs' native programmability
- **Session Management**: Multi-step operations with context preservation
- **Async Execution**: Non-blocking commands with automatic queue processing
- **Performance Optimized**: Response caching, memory management, API tracking
- **Tool Integration**: Manipulate buffers, execute functions, interact with environment
- **Safety-First Design**: Comprehensive error handling and confirmation systems

### 🆕 Advanced Features
- **Session Protocol**: Claude controls multi-step execution flow
- **Work Log Compression**: Efficient context management for long sessions
- **Performance Monitoring**: Track API call times and cache hit rates
- **Remote Queue System**: AI agents communicate via JSON file exchange
- **Autonomous Operation**: AI can spawn and control its own Emacs instances  
- **Self-Enhancement**: Agents can modify and improve Efrit's source code

## Installation

### Prerequisites

- **Emacs**: Version 28.1 or later
- **Anthropic API Key**: Get yours from [Anthropic Console](https://console.anthropic.com/)
- **Internet Connection**: Required for Claude API access

### Quick Installation

1. **Clone the repository**:
   ```bash
   git clone https://github.com/steveyegge/efrit.git
   cd efrit
   ```

2. **Add to your Emacs configuration** (`~/.emacs.d/init.el`):
   ```elisp
   (add-to-list 'load-path "/path/to/efrit/lisp")
   (require 'efrit)
   ```

3. **Configure your API key** in `~/.authinfo`:
   ```
   machine api.anthropic.com login personal password YOUR_API_KEY_HERE
   ```

4. **Restart Emacs** and test with `M-x efrit-chat`

### Data Directory

Efrit organizes all user data under a single configurable directory (default: `~/.emacs.d/.efrit/`):

```
~/.emacs.d/.efrit/
├── cache/              # Temporary cache files
├── context/            # Context persistence (efrit-do)
│   └── efrit-do-context.el
├── queues/             # AI-to-efrit communication
│   ├── requests/       # Incoming AI requests
│   ├── processing/     # Currently processing  
│   ├── responses/      # Completed responses
│   └── archive/        # Archived communications
├── queue-ai/           # Autonomous AI communication
├── logs/               # Debug and activity logs
├── sessions/           # Session persistence data
└── workspace/          # Autonomous workspace
    ├── auto-saves/     # Emacs auto-save files
    ├── backups/        # Backup files  
    └── startup.log     # Startup logging
```

**Key Benefits**:
- 🗂️ **Organized**: All efrit data in one predictable location
- ⚙️ **Configurable**: Easily change location via `efrit-data-directory`
- 🚫 **Version Control Safe**: Excluded from git via `.efrit/` in `.gitignore`
- 📊 **Dashboard Ready**: Structured for easy monitoring and management

**Customization**:
```elisp
;; Custom location (set before loading efrit)
(setq efrit-data-directory "~/my-custom-efrit-data")
(require 'efrit)
```

**Migration**: Existing scattered efrit files (`~/.emacs.d/efrit-*`) are automatically migrated to the organized structure on first load.

**Maintenance**: 
- Safe to delete individual subdirectories to reset specific components
- Queue directories auto-recreate as needed
- Context files can be manually backed up/restored

### 🆕 Enable Agent Communication

Start the remote queue to allow AI agents to interact with Efrit:

```elisp
;; In Emacs
(efrit-remote-queue-start)  ; or C-c C-e q
```

This creates `~/.emacs.d/.efrit/queues/` for AI agent communication.

## Usage

### Interactive Usage (Human → Efrit)

```elisp
M-x efrit-chat              ; Conversational interface
M-x efrit-do               ; Synchronous command execution
M-x efrit-do-async         ; Asynchronous command execution
M-x efrit-unified-do       ; Let Claude decide sync/async
M-x efrit-streamlined-send ; Streamlined chat mode
M-x efrit-performance-show-stats ; View performance statistics
```

**Key Bindings**: 
- `C-c C-e c` - Chat interface
- `C-c C-e d` - Sync command execution  
- `C-c C-e D` - Async command execution
- `C-c C-e u` - Unified (Claude decides)
- `C-c C-e s` - Streamlined chat
- `C-c C-e q` - Start queue
- `C-c C-e Q` - Queue status
- `C-c C-e A` - Async status

### Example: Creative Content Generation

Here's efrit creating four different poems about IDEs in separate buffers with a single request:

![Efrit creating four different poems in separate buffers](docs/images/efrit-poems.jpg)

This demonstrates efrit's ability to handle complex, multi-part requests through natural language commands.

### 🆕 Agent Communication (AI → Efrit)

AI agents write JSON requests to the queue directory:

```json
{
  "id": "req_001",
  "type": "eval", 
  "content": "(+ 40 2)"
}
```

Efrit processes and responds with JSON results:

```json
{
  "id": "req_001",
  "status": "success",
  "result": "42"
}
```

See [`QUEUE_USAGE_EXAMPLE.md`](QUEUE_USAGE_EXAMPLE.md) for detailed integration examples.

### Examples

**Human Interaction**:
```
M-x efrit-do
> write an ode in one buffer, and a sonnet in another, both about Vim
```

**AI Agent Interaction**:
```python
# Any AI agent can now interact with Efrit
efrit = EfritQueue()
result = efrit.send_request("command", "split window and open dired")
print(f"✓ {result['status']}")
```

## Architecture

### Core Philosophy

**Efrit is becoming a platform for autonomous AI development**:

1. **Human-Friendly**: Rich interactive interfaces for users
2. **Agent-Friendly**: File-based communication channel for AI systems
3. **Self-Enhancing**: AI agents can improve Efrit's own capabilities
4. **Zero Client Intelligence**: All AI processing happens in Claude, Efrit is pure executor

### Core Components

- **efrit.el** - Main entry point and coordination
- **efrit-chat.el** - Conversational interfaces  
- **efrit-do.el** - Synchronous natural language command execution
- **efrit-async.el** - 🆕 Asynchronous execution with session management
- **efrit-unified.el** - 🆕 Claude-controlled execution mode selection
- **efrit-context.el** - 🆕 Context capture and work log compression
- **efrit-performance.el** - 🆕 Caching, memory management, and stats
- **efrit-protocol.el** - 🆕 Shared protocols and tool dispatch
- **efrit-remote-queue.el** - Agent communication system
- **efrit-tools.el** - Core Elisp evaluation engine

## 🛣️ Roadmap

### Current Development
- ✅ **File-based Agent Communication** - Complete
- ✅ **Autonomous AI Development Environment** - Complete
- 🔄 **Self-Enhancement Capabilities** - Next Phase  

### Vision
Transform Efrit from a user assistant into an **autonomous AI development platform** where:
- AI agents enhance Efrit's functionality independently
- Multiple AI systems can collaborate through Efrit
- Users benefit from continuously improving capabilities

## Configuration

```elisp
;; Standard Efrit settings
(setq efrit-model "claude-3-5-sonnet-20241022")
(setq efrit-max-tokens 8192)

;; 🆕 Performance settings
(setq efrit-performance-cache-ttl 300) ; Cache for 5 minutes
(setq efrit-performance-max-sessions 10) ; Memory management
(setq efrit-async-max-work-log-entries 50) ; Prevent memory growth

;; 🆕 Agent communication settings  
(setq efrit-remote-queue-directory "~/.emacs.d/efrit-queue")
(setq efrit-remote-queue-max-concurrent 10)

;; Start agent communication on load (optional)
(add-hook 'after-init-hook #'efrit-remote-queue-start)
```

## Development

### For AI Agents
See [`AUTONOMOUS_AI_PLAN.md`](AUTONOMOUS_AI_PLAN.md) for autonomous development architecture.

### For Contributors
```bash
make compile  # Build
make test     # Run tests
```

See [CONTRIBUTING.md](CONTRIBUTING.md) for detailed guidelines.

## License

Licensed under the Apache License, Version 2.0. See [LICENSE](LICENSE) for details.

---

*Efrit: Autonomous AI development meets the power of Emacs.*
