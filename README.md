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
   (add-to-list 'load-path "/path/to/efrit/lisp/core")
   (add-to-list 'load-path "/path/to/efrit/lisp/support")
   (add-to-list 'load-path "/path/to/efrit/lisp/interfaces")
   (require 'efrit)
   ```

3. **Configure your API key** (choose one method):

   **Option A: Encrypted authinfo (most secure, recommended)**:
   ```
   # Create ~/.authinfo.gpg (GPG-encrypted)
   machine api.anthropic.com login personal password YOUR_API_KEY_HERE
   ```
   Save with GPG encryption. Emacs will prompt for passphrase on first use.

   **Option B: Environment variable**:
   ```bash
   export ANTHROPIC_API_KEY="sk-your-key-here"
   ```

   **Option C: Plain authinfo (less secure)**:
   ```
   # Create ~/.authinfo (unencrypted)
   machine api.anthropic.com login personal password YOUR_API_KEY_HERE
   ```

4. **Restart Emacs** and test with `M-x efrit-chat`

### Use with use-package + straight.el

Efrit is structured for lazy loading with `use-package`. Example setup using a local checkout and a custom data directory:

```elisp
(use-package efrit
  :straight (:type git :host github :repo "steveyegge/efrit"
             :files ("lisp/*.el" "lisp/core/*.el" "lisp/support/*.el" "lisp/interfaces/*.el"))
  :init
  ;; Set data directory before loading to avoid side effects
  (setq efrit-data-directory (expand-file-name "~/efrit-data"))
  ;; Optional: disable auto-initialize to control when directories are created
  ;; (setq efrit-auto-initialize nil)
  :commands (efrit-chat efrit efrit-do efrit-agent-run
             efrit-remote-queue-start efrit-remote-queue-status)
  :config
  ;; Bind a convenient prefix (optional)
  (setq efrit-enable-global-keymap t)
  (efrit-setup-keybindings))
```

Or, if you vendored Efrit locally:

```elisp
(add-to-list 'load-path (expand-file-name "~/repos/3p/efrit/lisp"))
(setq efrit-data-directory (expand-file-name "~/efrit-data"))
(require 'efrit)
;; Optional keybindings
;; (setq efrit-enable-global-keymap t)
;; (efrit-setup-keybindings)
```

### Advanced Configuration

**Enterprise/Proxy Setup** - Configure custom API endpoints:

```elisp
;; Static base URL
(setq efrit-api-base-url "https://proxy.company.com/anthropic")

;; Dynamic base URL (function)
(setq efrit-api-base-url
  (lambda ()
    (if (company-vpn-connected-p)
        "https://internal-proxy.company.com/anthropic"
      "https://api.anthropic.com")))

;; Custom authentication
(setq efrit-api-key 'COMPANY_ANTHROPIC_KEY)  ; Use env variable
```

**Alternative Authentication Methods**:

```elisp
;; Environment variable (recommended)
(setq efrit-api-key 'ANTHROPIC_API_KEY)

;; Custom function (e.g., for platform keychain integration)
(setq efrit-api-key (lambda () (get-secret-from-vault "anthropic-key")))

;; Direct string (NOT recommended - security risk)
(setq efrit-api-key "sk-your-key-here")
```

**Platform-Specific Secure Storage**:

Efrit's `auth-source` integration automatically supports platform-specific secure storage:

- **macOS**: Use `~/.authinfo.gpg` (GPG-encrypted) or macOS Keychain via `auth-source-macos-keychain`
- **Linux**: Use `~/.authinfo.gpg` (GPG-encrypted) or Secret Service API via `auth-source-secrets`
- **Windows**: Use `~/.authinfo.gpg` (GPG-encrypted) with GPG for Windows

**GPG Setup Example**:
```bash
# Create encrypted authinfo
gpg --gen-key  # Generate GPG key if needed
echo "machine api.anthropic.com login personal password sk-your-key" > ~/.authinfo
gpg -c ~/.authinfo  # Encrypts to ~/.authinfo.gpg
rm ~/.authinfo  # Remove plaintext version
```

Emacs will automatically detect and use `~/.authinfo.gpg` when available.

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

AI agents write JSON requests to the queue directory. Three request types are supported:

#### Request Types

**1. `eval` - Evaluate Elisp expressions**
```json
{
  "id": "req_001",
  "version": "1.0.0",
  "type": "eval",
  "content": "(+ 40 2)",
  "timestamp": "2025-11-23T20:00:00Z"
}
```

**2. `command` - Execute natural language commands via efrit-do**
```json
{
  "id": "req_002",
  "version": "1.0.0",
  "type": "command",
  "content": "create a new buffer called test.txt",
  "timestamp": "2025-11-23T20:00:00Z"
}
```

**3. `chat` - Send messages to Claude via streamlined chat**
```json
{
  "id": "req_003",
  "version": "1.0.0",
  "type": "chat",
  "content": "What is the capital of France?",
  "timestamp": "2025-11-23T20:00:00Z"
}
```

#### Response Format

Efrit processes requests and responds with JSON results:

```json
{
  "id": "req_001",
  "version": "1.0.0",
  "status": "success",
  "result": "42",
  "timestamp": "2025-11-23T20:00:01Z",
  "execution_time": 0.002
}
```

Response status values: `success`, `error`, `timeout`

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
