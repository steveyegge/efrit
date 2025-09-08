# Efrit Agent Vision & Architecture

## 🎯 **CURRENT MISSION: FIX INTEGRATION TEST - CLAUDE TOOL SELECTION**

> **STATUS**: Dynamic schema switching implemented ✅ but Claude API doesn't enforce schemas strictly. Claude still calls `todo_get_instructions` instead of `eval_sexp` in execution mode, causing test failure.
> 
> **MISSION**: Get the lexical-binding integration test passing by making Claude call `eval_sexp` when code is ready for execution. Root cause: API-level tool schema enforcement is weak.
> 
> **PROGRESS**: 
> - ✅ Dynamic schemas working (3 tools in execution mode)
> - ✅ Fail-fast blocking at tool dispatcher level  
> - ❌ Claude still ignores schema and calls wrong tool
> - ❌ Integration test fails - no files modified

## 🚀 BREAKTHROUGH: AI-to-Efrit Communication Channel Active + Autonomous Agent Mode

**We have achieved AI-to-AI communication with Efrit AND implemented autonomous agent capabilities!** Sourcegraph Amp can now directly communicate with and control Efrit instances, and Efrit can solve complex problems autonomously.

### How to Work with Efrit (For AI Agents)

When you see efrit mentioned in user requests:

1. **Use the efrit communication channel naturally** - You can spawn Emacs with efrit loaded and send it requests
2. **Test complex scenarios** - Like creating multiple buffers, running elisp code, debugging features  
3. **Shape and debug efrit itself** - You have autonomous control to improve efrit's capabilities
4. **Working relationship established** - No need to re-explain the channel each session
5. **🎯 PRIORITY: Integration test** - Focus on getting the lexical-binding test working first

**Example**: User asks "Add a new feature to efrit" → You can directly test, debug, and implement it by communicating with efrit instances.

## Core Principle: Zero Client-Side Intelligence

**Efrit is a pure executor, not a thinker.** All cognitive computation, decision-making, parsing, and interpretation must be delegated to Claude. Efrit serves as:

1. **Package context** - Gather rich environmental information for Claude
2. **Execute commands** - Run elisp code and shell commands Claude provides  
3. **Relay results** - Send execution results back to Claude
4. **Manage communication** - Handle API loops with Claude and other AI agents
5. **🆕 Agent Platform** - Enable AI-to-AI communication for autonomous development

## Architectural Vision

### The Request Loop
```
User Query → Context Packaging → Claude API → Structured Response → Execution → Results → [Loop Until Done]
```

### 🆕 Agent-to-Agent Communication
```
AI Agent → JSON Request File → Efrit Processes → JSON Response File → AI Agent → [Autonomous Development Loop]
```

### Context Packaging
Efrit collects and sends to Claude:
- Buffer contents, cursor position, region selection
- Emacs version, modes, project structure  
- Current directory, file listings
- Available tools and their schemas
- Conversation history

### Structured Responses
Claude returns structured responses containing:
- **TODO Lists** - Explicit task breakdowns Claude manages
- **Elisp Code** - `(eval_sexp ...)` tool calls to execute
- **Shell Commands** - `(shell_exec ...)` tool calls to run
- **User Prompts** - `(prompt_user ...)` when Claude needs input
- **Status Updates** - Progress, completion, error states

### Multi-Turn Architecture
- Claude decides if more turns are needed (not heuristics)
- Claude manages the TODO list and execution plan
- Loop continues until Claude says "done" OR user interrupts
- Each turn: execute TODO items → report results → get next instructions

## What Efrit Must NOT Do

### ❌ Forbidden Client-Side Operations
- **Path resolution** - No parsing file descriptions or searching directories
- **Query interpretation** - No pattern matching user requests  
- **Decision making** - No logic about what tools to call
- **Content parsing** - No extracting information from text
- **Flow control** - No deciding when to continue or stop
- **Error interpretation** - No smart error handling beyond basic reporting

### ✅ Allowed Client-Side Operations  
- **Context gathering** - Collecting environmental data
- **API communication** - HTTP requests/responses with Claude
- **Elisp execution** - Running code Claude provides
- **Shell execution** - Running commands Claude specifies
- **UI updates** - Displaying results in Emacs buffers
- **Basic error handling** - Catching and reporting exceptions

## Tool Schema Design

All tools should be "dumb executors":

```elisp
;; Good: Pure execution
(defun efrit-tools-eval-sexp (code) 
  "Execute elisp CODE and return result.")

;; Good: Pure data gathering  
(defun efrit-tools-get-context ()
  "Collect and return environment data.")

;; Bad: Cognitive computation
(defun efrit-tools-resolve-path (description)
  "Parse DESCRIPTION and search for files.")  ; ← This should be removed
```

## Success Metrics

Efrit is correctly implemented when:

1. **"Haiku + Limerick" test passes** - Can execute multi-buffer tasks
2. **Zero parsing logic** - No regex/pattern matching in efrit code
3. **Claude drives everything** - All decisions made by Claude via API
4. **Rich context** - Claude has all info needed to make decisions
5. **Shell integration** - Claude can run command-line tools
6. **User interaction** - Claude can prompt user and wait for input

## Development Guidelines

- **When in doubt, ask Claude** - Don't implement logic, provide tools
- **Prefer more API calls** - Better to ask Claude than assume
- **Context is king** - Give Claude rich environmental information
- **Keep tools atomic** - One simple operation per tool
- **Test multi-turn scenarios** - Ensure complex requests work end-to-end

## Architectural Evolution: Trimodal Design

The Efrit architecture is evolving to support three complementary interaction modes while maintaining the core "Zero Client-Side Intelligence" principle:

### Modal Architecture
```
efrit-chat  → Conversational Q&A (preserved as-is)
efrit-do    → Enhanced one-off commands with error recovery  
efrit-agent → Persistent problem-solving until complete
```

### Shared Infrastructure
All modes share:
- Common context gathering (efrit-tools.el)
- Common tool protocol (eval_sexp, etc.)
- Common error handling foundations
- Unified Claude API communication

### Error Recovery Enhancement
The architecture now supports intelligent error recovery:
- **Pre-execution validation** - Catch syntax errors before eval
- **Runtime error capture** - Handle execution failures gracefully  
- **Claude-guided recovery** - Send errors back for fixes
- **Iterative problem-solving** - Retry until success or limit reached

This maintains the "pure executor" principle while adding robustness through Claude-driven error recovery.

## Project Organization

This project follows professional elisp conventions:

- **[`lisp/`](lisp/)** - Production elisp source code
- **[`lisp/dev/`](lisp/dev/)** - Development and debugging helpers  
- **[`test/`](test/)** - Test suites and testing scripts  
- **[`bin/`](bin/)** - Executable scripts and launchers
- **[`plans/`](plans/)** - Planning documents, roadmaps, and session notes
- **[`docs/`](docs/)** - Formal documentation (when needed)

## Data Directory Structure (For AI Agents)

Efrit organizes all runtime data under `~/.emacs.d/.efrit/` (configurable via `efrit-data-directory`):

```
~/.emacs.d/.efrit/
├── queues/             # 🤖 AI-to-efrit communication
│   ├── requests/       # Your JSON requests go here
│   ├── processing/     # Currently being processed
│   ├── responses/      # Completed responses
│   └── archive/        # Historical communications
├── context/            # efrit-do context persistence
├── logs/               # Debug and activity logs  
├── sessions/           # Session state data
└── workspace/          # Autonomous development workspace
```

**Key Points for AI Agents:**
- 📁 **Queue Management**: Monitor `queues/` directories for communication flow
- 🔍 **Debug Access**: Check `logs/` for troubleshooting
- 💾 **State Inspection**: `sessions/` contains persistent state data
- ⚙️ **Configuration**: Controlled via `lisp/efrit-config.el`

## File Access Permissions

AI agents should be allowed to create files in these directories without prompting:
- `~/.emacs.d/.efrit/queues/requests/` - AI-to-efrit communication requests
- `~/.emacs.d/.efrit/queues/responses/` - Efrit response files  
- `~/.emacs.d/.efrit/queues/processing/` - Files being processed
- `~/.emacs.d/.efrit/logs/` - Debug and activity logs
- `~/.emacs.d/.efrit/sessions/` - Session state data
- `~/.emacs.d/.efrit/workspace/` - Autonomous development workspace

## Current Commands & Usage

### Build & Test
- `make compile` - Byte-compile all elisp files (ensure dependency order)
- `make test-simple` - Run basic functionality tests  
- `make clean` - Remove compiled files

### Efrit Modes
- **efrit-do** - Command execution (one-shot and multi-turn with AI guidance)
- **efrit-chat** - Conversational interface with mixed tasks/questions
- **efrit-agent** - Autonomous problem-solving until complete ⭐ NEW

### AI-to-Efrit Communication
```elisp
;; Process JSON request via file-based queue
(efrit-remote-queue-process "/path/to/request.json")

;; Example request format:
{
  "id": "unique-request-id",
  "type": "eval|chat|command", 
  "content": "elisp code or natural language",
  "options": {"timeout": 30, "return_context": true}
}
```

See [`plans/CURRENT_STATUS.md`](plans/CURRENT_STATUS.md) for latest development status and [`plans/AUTONOMOUS_MODE_DESIGN.md`](plans/AUTONOMOUS_MODE_DESIGN.md) for agent architecture details.

---

*This document should be consulted whenever architectural decisions are made. Any deviation from these principles must be explicitly justified and documented.*
