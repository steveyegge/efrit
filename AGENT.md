# Efrit Agent Vision & Architecture

## Core Principle: Zero Client-Side Intelligence

**Efrit is a pure executor, not a thinker.** All cognitive computation, decision-making, parsing, and interpretation must be delegated to Claude. Efrit's only job is to:

1. **Package context** - Gather rich environmental information for Claude
2. **Execute commands** - Run elisp code and shell commands Claude provides  
3. **Relay results** - Send execution results back to Claude
4. **Manage communication** - Handle the API loop with Claude

## Architectural Vision

### The Request Loop
```
User Query → Context Packaging → Claude API → Structured Response → Execution → Results → [Loop Until Done]
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

---

*This document should be consulted whenever architectural decisions are made. Any deviation from these principles must be explicitly justified and documented.*
