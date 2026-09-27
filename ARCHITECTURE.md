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

### Tools from other packages (efrit-tool-registry.el)

A package outside efrit can offer the model tools of its own without
patching efrit's tables:

```elisp
(efrit-register-tool "gmail_search"
  :description "Search the user's Gmail..."   ; what the model reads
  :input-schema '(("type" . "object") ...)    ; same shape as efrit-do--tools-schema
  :function #'my-package--search              ; (lambda (input-alist)) -> string
  :class 'read                                ; read / write / exec / net / control
  :package 'my-package)
```

The schema getter appends registered tools after efrit's own, the
dispatcher falls back to the registry for unknown names, and
`efrit-permission-tool-class` returns the registered class (so review
sees writes and execs).  A registered tool is still a Pure Executor
tool: the model decides when to call it, the function only does what it
is asked, and consent stays with the sandbox (`efrit-sandbox-check`
inside the function for anything beyond the package's own data).
Names must not collide with efrit's own tools.  `M-x
efrit-list-registered-tools` shows what is registered.

A package can also start a REPL turn with a prompt it prepared:
`(efrit-submit SHOWN API-INPUT)` shows SHOWN as the user's line in the
agent buffer and sends API-INPUT (with the editor-context block
prepended, as for typed input) to the model.  First user: `efrit-gnus.el` (lisp/interfaces), which sends selected
Gnus articles with an analysis prompt and registers `gnus_groups`,
`gnus_search` and `gnus_articles`; nngmail adds Gmail-specific tools on
top of it from its own repository.  A package that runs several turns
over separate data takes a history mark first
(`efrit-repl-session-history-mark` on `(efrit-agent-repl-session)`),
rewinds to it before each turn (`efrit-repl-session-rewind`) and reads
each answer with `efrit-repl-session-last-answer`; efrit-gnus does this
per batch, so the model never rereads earlier batches, and sends the
stitched answers to the closing turn.

While a turn runs the prompt stays writable.  RET routes through
`efrit-agent-busy-submit-default-function` (queue: the text is kept on
the session, `efrit-repl-session-queue`, and sent when the turn ends
well) and M-RET through the override function (steer: the agent buffer
publishes a `steer` event; `efrit-repl-loop--on-steer` keeps the text
on the session and the adapter's `before-request-fn`,
`efrit-repl-loop--deliver-steering`, appends it as text blocks to the
user message that carries the next tool results, publishing
`steered`).  Steering that finds no tool round is queued at turn end.
Both are drawn in the conversation at once, with their own prefix.

Rendering discipline: every programmatic edit of the conversation goes
through `efrit-agent--with-render` (no undo entries, read-only lifted;
afterwards the undo list is reset to one entry for the input region)
and `efrit-agent--seal-rendered` (read-only, `field', `font-lock-face'
mirrored, `fontified').  Tool bodies fold with the `invisible' property
(`efrit-tool-body'), and `efrit-agent--isearch-filter' honours
`search-invisible'.  The model's text is rendered as Markdown in place
by `efrit-markdown.el` (lisp/interfaces): markup deleted, faces and
links as text properties, a watermark offset on the first character so
each streamed chunk renders only from the last safe frontier; fenced
blocks are fontified with the language's mode and frozen.  Pipe
tables are a block pass like fences (held back while the last row may
still grow), each cell rendered on its own before the columns are
measured; images (`![alt](src)`) keep the alt text and put the picture
on it as a `display' property, local files and `data:` URLs at once,
http(s) fetched with `url-retrieve` into a cache, width per image in
`efrit-markdown-image-width` so `+`/`-`/`=` rescale in place.
`efrit-agent-mentions.el` adds `@path` completion and expansion,
`/commands` (`efrit-agent-define-slash-command`), and drag and drop.

`efrit-transcript.el` (lisp/interfaces) subscribes to the same events
(`turn-start`, `text-delta`/`text-end`, `tool-start`/`tool-result`,
`steer`, `question`, `error`, `turn-complete`) and appends a Markdown
file per session under `efrit-data-directory/transcripts/`, so turns
started from Lisp are recorded too and nothing is held in memory.

A REPL session's `working' status is only meaningful while
`efrit-repl-loop--active` has its loop.  A reload replaces that table,
so `efrit-repl-loop-recover-stale` (called from
`efrit-agent--session-busy-p`) ends such a turn as interrupted instead
of refusing every later submit as busy; the struct upgrade on reload
does the same.  Tests and the test drive that need a pretend-busy
session use `efrit-repl-loop-hold` / `-release`, which register a
placeholder loop.

The REPL history is also bounded by size: before every request
`efrit-repl-session-fit-context` estimates the history against the
model's window (`efrit-usage-window`, per-model via
`efrit-usage-context-windows`, less `efrit-repl-context-headroom`) and
elides the oldest tool results, then the oldest user messages, in
place; a `note` event says what it dropped.  Without this a long
tool-heavy turn grew past the window and every request after it failed
with "prompt is too long".

Prompts for such analyses come from `efrit-prompts.el` (lisp/interfaces):
a library of two-part prompts (one part per batch of items, one over
everything), with built-ins from `efrit-prompts-define`, the user's own
in `prompts.json` under `efrit-data-directory`, a transient chooser
(`efrit-prompts-read`), and a manager/editor (`M-x efrit-prompts-manage`)
that can ask the model for an improved version.

Documents outside Emacs come through `efrit-documents.el` (lisp/core): a
source protocol (`efrit-document-source` with match/fetch/metadata/search
generics), a session cache keyed by the source's modified time, a
related-documents lookup (title words near a date), and the
`doc_fetch`/`doc_search`/`doc_sources` tools.  `efrit-documents-gdrive.el`
is the Google Drive source and `efrit-documents-confluence.el` the
Confluence one (Atlassian Cloud and self-hosted, one source per site in
`efrit-documents-confluence-sites`).  `efrit-documents-jira.el` wraps
the jira.el package as a source and a
key-mention provider. `efrit-documents-gcalendar.el` is
not a source but a related-documents provider
(`efrit-documents-related-functions`): it finds the calendar event an
item is about by date, title words and people, and returns the event's
attachments as Drive documents, so a renamed meeting still yields its
notes.  Sources
talk to their APIs through `efrit-auth.el`: auth-source lookup by host
(OAuth2 client fields → oauth2.el consent/refresh/plstore, otherwise a
bearer or basic secret), and `efrit-auth-request` with retries and typed
errors.  efrit-gnus expands article links and adds related documents
through this layer (and, with `gnus-treat-related-documents`, writes
them as a footnote into the article buffer that a later analysis reads
back); backends only add expanders for links no source handles.

## 📦 **MODULE ORGANIZATION & LOAD ORDER**

### Directory Structure
```
lisp/
├── core/           - Low-level dependencies, non-UI modules
├── interfaces/     - High-level tools, execution, commands
├── support/        - UI, progress, helper utilities
└── tools/          - Individual tool implementations
```

### Load Order Dependencies
**Intentional Circular Dependencies** (use lazy requires):
- `efrit-do` ↔ `efrit-do-async-loop` - Sync/async execution
- `efrit-executor` → `efrit-do` (requires `efrit-do`)
- `efrit-session` ↔ user input handlers (e.g., in `efrit-do-handlers`)

**Non-circular Dependencies** (use top-level requires):
- `efrit-do-handlers` requires `efrit-session`, `efrit-progress` (moved from lazy)
- `efrit-agent-tools` requires `efrit-do` (moved from lazy)  
- `efrit-ui-progress` requires `efrit-do` (moved from lazy)

### Lazy vs Top-Level Requires
- **Top-level**: Standard pattern, declare functions that are called in function bodies
- **Lazy (only when circular)**: Use `require` inside function body when module A needs module B, and module B needs module A
- **Forward declarations**: Use `declare-function` for functions, `defvar` for variables to avoid circular deps

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

## 🛡️ **LOOP PREVENTION: SCHEMA-BASED TOOL FILTERING**

While efrit remains a pure executor, it must prevent infinite loops that waste tokens and hang sessions. The solution: **dynamically restrict available tools based on workflow state**.

### Implementation

`efrit-do--get-tools-for-state()` filters the tool schema before each API call:

**Workflow States**:
- `initial` - Planning phase: allow `todo_analyze`, `todo_add`, query tools
- `todos-created` - Execution phase: prioritize `eval_sexp`, `shell_exec`, `todo_update`
- After 1 `todo_get_instructions` call: **Block it from schema**, force execution tools

**Tool Categories**:
```elisp
Planning tools:     todo_analyze, todo_add
Execution tools:    eval_sexp, shell_exec, todo_update, todo_complete_check
Query tools:        todo_status, todo_next (limited use)
Dangerous tools:    todo_get_instructions, todo_execute_next (strict limits)
Always available:   glob_files, buffer_create, session_complete, etc.
```

### Why This Preserves Pure Executor Principle

This is **NOT** client-side intelligence because:
- ✅ No semantic analysis of content
- ✅ No pre-generated solutions
- ✅ No task-specific logic
- ✅ Only workflow state tracking (which TODO phase we're in)
- ✅ Tool availability based on phase, not content understanding

**Analogy**: Like a toolbox that only shows screwdrivers during the "screwing" phase and only shows hammers during the "hammering" phase. The human (Claude) still decides what to do; efrit just manages which tools are on the table.

### Circuit Breaker (Complementary Defense)

The circuit breaker (see `efrit-do-max-tool-calls-per-session`) provides hard limits:
- Total tool calls per session (default: 30)
- Same tool call repetitions (default: 3)

When limits are exceeded, efrit forcibly terminates the session with an error.

**Key Distinction**: Schema filtering is **gentle guidance** (removing problematic tools). Circuit breaker is **emergency shutdown** (hard limits).

## 🧪 **TESTING PHILOSOPHY**

Integration tests must verify **Claude's abilities**, not efrit's shortcuts:
- No hard-coded solutions in efrit
- No pattern matching or parsing assistance
- Claude must genuinely solve problems using only basic tools
- Tests measure end-to-end cognitive problem-solving

## 📜 **HISTORICAL NOTE**

Previous versions of efrit contained hard-coded lexical-binding logic, warning parsers, and pre-generated elisp solutions. **All such code has been purged** to restore architectural purity.

### Multi-Turn Conversation System (Removed)

**Archived**: 2025-11-23

The `efrit-multi-turn.el` module (~320 lines) provided automatic conversation continuation by asking Claude whether tasks were complete. This violated the Pure Executor principle in several ways:

1. **Client-side decision-making**: Efrit decided when to continue conversations
2. **Pattern-based heuristics**: Simple logic for determining task completion
3. **Workflow control**: Managing turn limits and termination conditions

**Why it was removed**:
- Conversation control belongs to Claude, not the client
- In the REPL (`M-x efrit`), users control multi-turn interactions
- In `efrit-do`, Claude can request continuation via tool calls if needed
- The module was unused (disabled in chat mode, never initialized elsewhere)

**Modern approach**: Claude manages its own workflow via the executor's tool schema. If Claude needs multiple turns, it explicitly requests them via tool calls rather than client-side heuristics deciding for it.

## ⚖️ **ENFORCEMENT**

Any PR introducing client-side intelligence must be rejected. Code reviews should specifically check for:
- String pattern matching with semantic meaning
- Task-specific conditional logic  
- Pre-written solution templates
- Workflow decision heuristics

**Remember: If efrit "knows" how to solve a problem, the architecture is broken.**
