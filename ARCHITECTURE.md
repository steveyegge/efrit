# Efrit Architecture: Pure Executor Principle

## 🎯 **CORE ARCHITECTURAL PRINCIPLE**

**ZERO CLIENT-SIDE INTELLIGENCE**: Efrit is a pure executor that delegates ALL cognitive computation to Claude.

## 🚨 **ABSOLUTE PROHIBITIONS**

### ❌ NEVER IMPLEMENT IN EFRIT:
- **Pattern recognition** (parsing warnings, error messages, file formats)
- **Task-specific logic** (lexical-binding fixes, syntax corrections, etc.)
- **Decision-making heuristics** (what tool to call next, workflow guidance)
- **Code generation** (pre-written elisp solutions, template code)
- **Content analysis** (understanding user intent, command classification)
- **Flow control** (deciding when to continue or stop operations)
- **Implementation hints** (task-specific guidance or instructions)

### ✅ ALLOWED IN EFRIT:
- **Context gathering** (collecting buffer contents, file listings, environment data)
- **Tool execution** (eval_sexp, shell_exec with provided code/commands)
- **Result relay** (returning execution results, error messages)
- **Basic validation** (syntax checking, security filtering)
- **State persistence** (session tracking, logging, history)
- **API communication** (HTTP requests/responses with Claude)

## 🏗️ **ARCHITECTURAL COMPONENTS**

### Pure Executor Tools
```elisp
eval_sexp    - Execute elisp provided by Claude
shell_exec   - Execute shell commands provided by Claude  
todo_add     - Create TODO with Claude-provided content
todo_update  - Mark TODOs complete when Claude decides
session_complete - End session when Claude signals done
```

### Context Providers
```elisp
buffer_contents     - Raw buffer text
directory_files     - File listings
warnings_buffer     - Raw warning messages
current_context     - Point, mark, mode info
```

### State Management
```elisp
workflow_state   - Track planning vs execution phase
session_history  - Log all tool calls and results  
dynamic_schemas  - Provide different tool sets per phase
```

## 🔄 **REQUEST-RESPONSE CYCLE**

```
1. User Query → efrit packages context
2. Context + Tools Schema → Claude API
3. Claude analyzes, plans, decides
4. Claude returns structured tool calls
5. efrit executes tools as pure functions
6. Results → back to Claude
7. Repeat until Claude calls session_complete
```

## 🧠 **CLAUDE'S RESPONSIBILITIES**

Claude must handle ALL cognitive tasks:
- Parse and understand user requests
- Analyze warnings, errors, file contents
- Generate task-specific elisp code
- Decide tool execution sequence
- Create appropriate TODO items
- Determine when work is complete

## 🤖 **EFRIT'S RESPONSIBILITIES** 

Efrit provides pure execution environment:
- Gather and package environmental context
- Expose safe, schema-driven tool interface
- Execute Claude's instructions without modification
- Return raw results without interpretation
- Maintain session state and history

## 🚫 **ANTI-PATTERNS TO AVOID**

### Pattern Recognition Anti-Pattern
```elisp
;; ❌ WRONG - efrit doing cognitive work
(when (string-match "Warning.*lexical-binding" line)
  (create-todo "Fix lexical binding"))

;; ✅ RIGHT - Claude gets raw data
(with-current-buffer "*Warnings*" (buffer-string))
```

### Code Generation Anti-Pattern
```elisp
;; ❌ WRONG - efrit pre-generating solutions
(defun fix-lexical-binding (filename)
  "(find-file-noselect filename) (insert cookie) (save-buffer)")

;; ✅ RIGHT - Claude provides all code
(eval_sexp claude-provided-elisp-string)
```

### Decision-Making Anti-Pattern
```elisp
;; ❌ WRONG - efrit deciding next steps
(if (string-match "TODO completed" result)
    "Call todo_update next"
  "Call eval_sexp to continue")

;; ✅ RIGHT - Claude decides everything
"Raw result: %s. Available tools: %s" result tool-list
```

## 🧪 **TESTING PHILOSOPHY**

Integration tests must verify **Claude's abilities**, not efrit's shortcuts:
- No hard-coded solutions in efrit
- No pattern matching or parsing assistance
- Claude must genuinely solve problems using only basic tools
- Tests measure end-to-end cognitive problem-solving

## 📜 **HISTORICAL NOTE**

Previous versions of efrit contained hard-coded lexical-binding logic, warning parsers, and pre-generated elisp solutions. **All such code has been purged** to restore architectural purity.

## ⚖️ **ENFORCEMENT**

Any PR introducing client-side intelligence must be rejected. Code reviews should specifically check for:
- String pattern matching with semantic meaning
- Task-specific conditional logic  
- Pre-written solution templates
- Workflow decision heuristics

**Remember: If efrit "knows" how to solve a problem, the architecture is broken.**
