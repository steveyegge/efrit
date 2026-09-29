# Efrit - AI-Powered Emacs Coding Assistant

**Version 0.4.1**

Efrit is an AI coding agent that brings Claude's intelligence directly into Emacs. It can execute natural language commands, explore codebases, run shell commands, and have multi-turn conversations - all while you stay in your editor.

**Core principle**: Zero client-side intelligence. Claude makes all decisions, Efrit executes.

> ⚠️ **Security Note**: Efrit executes AI-generated code in your Emacs. See [SECURITY.md](SECURITY.md) for trust model and recommended practices.

## Features

### Agentic Task Execution (`efrit-do`)
Execute natural language commands that Claude translates to actions:
- "Create a buffer with today's date"
- "List all elisp files in this directory and count them"
- "Search for the function definition of `efrit-execute`"
- "Run git status and show me the output"

### Agent Session Buffer (`efrit-agent`)
A structured, real-time view of agentic sessions with:
- Session status and elapsed time in header-line
- Task progress tracking (TODOs) updated by Claude
- Conversation view with expandable tool calls (folded bodies are
  searchable: `isearch` honours `search-invisible`)
- The model's answers rendered as Markdown in place: headers, emphasis,
  code blocks with the language's fontification, links, block quotes,
  pipe tables as aligned columns, images (`![alt](file-or-url)`, drawn
  in the buffer; `+` / `-` / `=` resize the one at point), and file
  references like `lisp/efrit.el:120` that open the file
- Select part of an earlier answer and `C-c C-y` quotes it into the
  input as a `>` block (queued for the next turn when one is running);
  `C-c C-z` narrows to the last turn (`C-u 3 C-c C-z` the last three),
  `C-c C-a` widens
- A Markdown transcript of every session is written as it goes under
  `efrit-data-directory/transcripts/` (`efrit-transcript-enabled`);
  `C-c C-f` opens this session's
- A prompt that is writable at all times.  `RET` while a turn runs
  queues the input for the next turn; `M-RET` (or `C-u RET`) steers
  the running turn: the model reads the text with its next tool
  results.  Both are `efrit-agent-busy-submit-*-function`.  `C-c C-q`
  lists and drops queued inputs.
- `@path` mentions complete over the project's files and inline the
  file for the model; images are attached.  Drag a file onto the buffer
  to mention it (a screenshot dragged from the OS is kept under
  `.efrit/dropped/`).
- `/commands` at the start of the input run efrit's own commands:
  `/help` lists them (`/new`, `/model`, `/mode`, `/queue`, ...).
- Lists in the input: `S-RET` on a `- ` or `1. ` line continues the
  list (on an empty item it ends it), `TAB` / `S-TAB` indent and dedent
  the item.
- The diff preview (`show_diff_preview`) opens the file at the change
  on `o` or `RET`; it finds the hunk by its text, not its line numbers.
- `C-c C-.` (menu `g`) regenerates: the last question is sent again
  and the old exchange is replaced when the new turn succeeds.  `C-u`
  edits the question first.
- Code blocks in answers are things: `C-c C-b` copies the block at
  point raw (off a block, pick one by completion), `C-c C-j` inserts
  it at point in the other window without leaving the agent buffer.
- Presets bundle settings (model, reviewer, default grants, verbosity,
  rows): `/preset fast`, `/preset careful`, menu `P`; add your own to
  `efrit-presets`.
- Optional desktop notification when a slow turn ends while you are in
  another buffer (`efrit-notify-enabled`; through `alert` when
  installed, else `notifications-notify`, else the echo area).

**Several agent buffers** (`efrit-agent-instances-mode`): one buffer
per project, `*efrit[proj]*`; `C-u M-x efrit` opens another instance,
`*efrit[proj:2]*`, and menu `j` names it.  They live in side windows
grouped by project (`efrit-agent-side`).  `C-c t` hides and restores a
project's agent windows per tab, `C-c l` switches between all of them.
Each has its own session: its own turns, project root for the sandbox
and tools, cancel (`C-c C-k` stops only this one), queue and question
menu.  Gnus gets `*efrit[gnus]*` (`efrit-gnus-own-buffer`).  A sandbox
prompt is one at a time: a request from another session that arrives
while a prompt is open is denied with a note, and the model can retry.

**Navigation and context** (after claude-code-ide): the model has
`xref_references`, `xref_apropos`, `imenu_symbols` and `treesit_info`,
answered by whatever backend you run (eglot, lsp-mode, etags, the
elisp backend), and `show_location` to open a file for you at a text
anchor.  `get_diagnostics path="project"` covers every open file of
the project.  The header shows what editor context the next turn
carries (`⧉ foo.el:120, 3 lines`); `C-c C-;` dismisses the current
file's until you move on, `C-c ;` restores it.
`M-x efrit-agent-mention-range` on a region puts `@file#L10-L20` into
the input: the model gets those lines, not the file.  In a diff
preview `E` opens the change in ediff; edit buffer B, quit, accept:
the model applies your text (`user_edits`).  `C-c C-m` `S` saves the
menu's toggles with `customize-save-variable`.

**Briefs, pins, next steps** (after ai-code-interface):

- Commands from any buffer build a structured brief (Goal / Scope /
  Context / Boundaries / Agent responsibilities / Verification
  evidence): `efrit-fix-errors-in-scope` (the Flymake/Flycheck errors
  of the region, line, defun or file), `efrit-investigate-exception`
  (a visible `*compilation*`, `*Backtrace*` or `*Warnings*`, the
  region, C-u the clipboard; a question), `efrit-send-dwim` (Dired
  marks, region, diagnostics at point as `@file#L42`, else the line),
  `efrit-refactor` (Fowler's catalog; placeholders ask you with a
  default from point), `blame-analysis` / `log-analysis` prompts for
  `efrit-scope-run`, `efrit-shell-command` (`:words` has the model
  write the one-liner; you edit, it runs in `*compilation*`).
- A question is a read-only turn: `explain`, `review`,
  `efrit-investigate-exception` and Magit `explain`/`question` make the
  sandbox refuse writes, shell and eval for that turn without asking.
- `C-u RET` grills you first: the model asks its clarifying questions
  and does not act until you answer (`C-c g` arms it too).  `C-u` on
  `efrit-scope-run` adds the files of your other windows, `C-u C-u`
  the clipboard as well.
- `efrit-prompt-suffix-functions`: an abnormal hook over every
  outgoing input (a context with the text, the command, a memo).  Add
  your project rule or test-after-change instruction there.
- Text marked with `efrit-mark-verbatim` (a pasted diff) is data: no
  mention expansion inside it.
- `efrit-context-pin` on a region pins `file#L10-L20` for this project:
  sent with every turn (header shows `+N pins`) until
  `efrit-context-unpin` / `efrit-context-clear-pins`.  After `@file`,
  typing `#` completes the file's definitions and writes the range.
- Answers that end with a `Next steps` list get buttons: `RET` on an
  item or `M-1`..`M-4` sends that step.  `efrit-next-steps-ask` asks
  the model for such a list on non-change prompts.
- After each `edit_file`/`edit_buffer`, the new Flymake/Flycheck
  diagnostics of the file (against the turn's baseline) are appended
  to the tool result, so the model fixes what it broke before saying
  done (`efrit-diagnostics-baseline-enabled`).
- `get_last_error`: the model can read your recent command errors
  with backtrace frames (`efrit-last-error-mode`, on with efrit);
  `eval_sexp` results also list messages written and buffers changed.
- `C-c C-,` / menu `,`: checkpoint, the running turn stops and reports
  goal, files, hypothesis, tests, blockers, failed approaches, next.
- Magit: `C-c e` on a hunk (or selected hunks) explains, asks, changes
  or writes tests, with a provenance line; changes are refused on a
  historical diff.  `efrit-agent-dashboard` (menu `D`) lists every
  agent buffer with project, branch, dirty count, status and queue.
- The `position` context source now carries the enclosing function
  and type headers with line ranges (tree-sitter, else which-function).
  `path#L3-L5` and `path:L3-L5` in answers open the file with the range
  selected.

**Outside the agent buffer:**

- `M-x efrit-rewrite-region`: the model rewrites the selected text per
  your instruction; the change is drawn over the text itself (removed
  lines in red, new lines in green; `efrit-rewrite-preview` `buffer`
  for a diff popup instead), then it replaces the region (only if the
  region is unchanged since).
- `M-x efrit-edit-history-mode` (or the global variant) records what
  you change in a buffer as diffs, one entry per burst of typing, and
  the `edit-history` context source shows the model the latest ones:
  "continue what I was doing" works.  Off by default.
- A turn you cancel mid-answer keeps the text that arrived, marked
  `[answer cut short here]` in the history, so the next turn knows.
- `M-x efrit-scope-run`: run a library prompt (`explain`, `fix`,
  `document`, `tests`, `review`, `simplify`, or your own) over the
  region, the defun at point, or the buffer, as a turn in the agent
  buffer.  Prompts may use `{{{:text}}}`, `{{{:file}}}`, `{{{:mode}}}`,
  `{{{:scope}}}`, `{{{:lines}}}`.
- `M-x efrit-commit-message` in a commit buffer: a conventional commit
  message from the staged diff (read through VC); `C-u` asks for three
  and opens a pick panel.  A `.github/git-commit-instructions.md` in
  the repository is obeyed.
- A sandbox prompt for a shell line or an elisp form has `e`: edit the
  text, then allow; the edited text runs once, no standing grant.
- `eval_sexp` balances an unbalanced form before evaluation and tells
  the model; `edit_buffer` refuses positional edits to a buffer that
  changed since the model read it.

**Agent Buffer Keybindings:**

Actions use the `C-c` prefix so standard editing keys (`C-k`, `C-g`,
`C-h`, `C-M-*`, ...) keep their normal meaning in the input region.

| Key | Action |
|-----|--------|
| `TAB` / `S-TAB` | Move between sections |
| `M-n` / `M-p` | Next / previous tool call |
| `RET` | Expand/collapse tool at point |
| `RET` (input) | Send; queue while a turn runs |
| `M-RET` / `C-u RET` (input) | Steer the running turn |
| `S-RET` / `C-j` (input) | Newline |
| `C-c C-c` | Send input / continue session |
| `C-c C-q` (input) | Show / drop queued inputs |
| `C-c C-w` | Copy the last answer |
| `C-c C-i` | Copy the session id |
| `C-c C-x` | Restart: fresh session, same windows |
| `C-c C-y` | Quote the region into the input (queued when busy) |
| `C-c C-z` / `C-c C-a` | Narrow to the last N turns / widen |
| `C-c C-f` | Open this session's transcript file |
| `C-c C-.` | Regenerate the last answer (`C-u`: edit the question) |
| `C-c C-;` / `C-c ;` | Dismiss / restore the current file's editor context |
| `C-c l` / `C-c t` / `C-c I` | Switch agent buffer / toggle this project's / new instance |
| `C-c C-b` / `C-c C-j` | Copy the code block at point / insert it in the other window |
| `+` / `-` / `=` (transcript), `C-c +` etc. | Resize the image at point, else all |
| `S-RET` on a list item | Continue the list; `TAB` / `S-TAB` indent it |
| `C-c C-k` | Cancel session |
| `C-c C-n` | New session |
| `C-c C-r` | Resume paused session |
| `C-c C-p` | Pause session |
| `C-c C-h` | Browse session history |
| `C-c C-t` | Toggle expand tool at point |
| `C-c C-e` | Expand all tool calls |
| `C-c C-d` | Collapse all tool calls |
| `C-c C-v` | Cycle verbosity (minimal/normal/verbose) |
| `C-c C-o` | Cycle display mode (minimal/smart/verbose) |
| `C-c C-g` | Refresh display |
| `C-c C-q` (conversation) | Quit buffer (session continues) |
| `1`-`4` | Select option when Claude asks (in input region) |
| `C-c ?` / `?` (transcript) | The buffer's command menu (transient) |

### Rich Tool Suite
Efrit provides Claude with 35+ tools:

| Category | Tools |
|----------|-------|
| **Code Execution** | `eval_sexp` (elisp), `shell_exec` (shell commands) |
| **File Editing** | `edit_file`, `create_file`, `undo_edit`, `format_file` |
| **Codebase Exploration** | `search_content`, `read_file`, `project_files`, `glob_files`, `file_info` |
| **Version Control** | `vcs_status`, `vcs_diff`, `vcs_log`, `vcs_blame` |
| **Task Management** | `todo_write`, `session_complete`, `request_user_input` |
| **Safety** | `confirm_action`, `checkpoint`, `restore_checkpoint`, `show_diff_preview` |
| **Diagnostics** | `get_diagnostics`, `elisp_docs` |
| **Issue Tracking** | `beads_ready`, `beads_create`, `beads_update`, `beads_close`, `beads_list` |
| **External** | `web_search`, `fetch_url`, `read_image` |
| **Buffer Management** | `buffer_create`, `display_in_buffer` |

### Robust Error Handling
- Circuit breaker prevents infinite loops
- Automatic error recovery and retry
- Security controls on shell commands

### AI-to-AI Communication
Other AI agents (Claude Code, Cursor, etc.) can interact with Efrit via file-based JSON queue for autonomous development.

### Mail and documents (Gnus, Google Drive, Google Calendar, Confluence)
`efrit-gnus` analyzes selected Gnus articles with a library of two-part
prompts (`M-x efrit-prompts-manage`), and the model gets `gnus_search`
and `gnus_articles`. Documents the articles link to, and documents
related to them (the notes Calendar attached to the meeting, the
Confluence page with the same title), are fetched through
`efrit-documents` and its sources; `doc_search` and `doc_fetch` let the
model look for more. Setup, including which Google APIs to enable in
the Cloud console, is in [docs/DOCUMENTS.md](docs/DOCUMENTS.md).

## Installation

### Prerequisites
- Emacs 28.1+
- Anthropic API key from [console.anthropic.com](https://console.anthropic.com/)

### Quick Start

```bash
git clone https://github.com/steveyegge/efrit.git
cd efrit
```

Generate the autoloads once (`make autoloads`), then add to
`~/.emacs.d/init.el`:

```elisp
(add-to-list 'load-path "/path/to/efrit/lisp")
(load "efrit-autoloads")   ; every command available lazily, incl. efrit-doctor
```

or, to load everything eagerly, `(require 'efrit)` instead of the
`load`.  With `use-package :load-path`, put the `load` in `:init`
(see `docs/examples/proxy-config.el`).

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

| Mode | Use When | Tools Available |
|------|----------|-----------------|
| **`efrit-do`** | Quick agentic commands, shows progress in minibuffer | Full suite (35+ tools) |
| **`efrit-agent`** | Complex tasks needing visibility and interaction | Full suite + structured UI |

**Decision guide:**
- **Just asking?** → `M-x efrit` (the same buffer; a question is a turn with read-only tools)
- **Quick command?** → `efrit-do` (runs in background, shows progress buffer)
- **Complex task?** → `efrit-agent` (real-time visibility, can guide mid-session)

**Key differences:**
- `efrit-do`: Fire-and-forget, progress buffer with raw output
- `efrit-agent`: Interactive session buffer with expandable tool calls, TODO tracking, and mid-session guidance (`i` key)

### Commands

| Command | Description |
|---------|-------------|
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
M-x efrit RET
> Help me understand how the error handling works in this file
[analyzes current buffer and explains]
> Can you refactor it to use condition-case-unless-debug instead?
```

### Agentic Workflows

**Bug fix workflow:**
```
M-x efrit-agent RET
> The function `my-parse-data` throws an error on empty input. Find and fix it.

Claude will:
1. Search for the function definition
2. Analyze the code
3. Identify the bug
4. Create the fix with edit_file
5. Show you the diff for review
```

**Refactoring workflow:**
```
M-x efrit-agent RET
> Refactor all uses of deprecated `my-old-api` to use `my-new-api` in this project

Claude will:
1. Find all occurrences with search_content
2. Create a TODO list for each file
3. Edit each file, showing progress
4. Verify changes compile
```

**Mid-session guidance:**
While Claude is working, you can:
- Press `i` to inject guidance: "Focus on the src/ directory only"
- Press `k` to cancel if going wrong direction
- Wait for questions - Claude will ask via `request_user_input` and you can respond directly in the buffer

**Providing corrections:**
When Claude asks a question, you can:
- Type your response after the `>` prompt and press `C-c C-s` to send
- Press `1`, `2`, `3`, or `4` to select from offered options
- Type "actually, I meant..." to correct course

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
(setq efrit-data-directory (expand-file-name ".efrit" user-emacs-directory))

;; Enable debug logging
(setq efrit-log-level 'debug)

;; Progress buffer configuration
(setq efrit-do-show-progress-buffer t)  ; Show progress buffer automatically
(setq efrit-do-queue-max-size 10)       ; Max commands to queue
```

### Version control

The `vcs_status`, `vcs_diff`, `vcs_log` and `vcs_blame` tools, the
project file list and checkpoints all go through Emacs's VC layer, not
a `git` subprocess, so they work wherever VC does (local, TRAMP) with
your VC settings.  A `checkpoint` is a Git stash named
`efrit-checkpoint <id>: <description>`; you will see it in `git stash
list` and Magit.  In a tree without Git, checkpoints are file snapshots
under `.efrit/checkpoints/`.

### Remote hosts (TRAMP)

A path with a TRAMP prefix is on another machine, and the local
project's default grants never apply to it, even when the project root
itself is remote.  What the model may read or write there is decided
per host:

```elisp
(setq efrit-sandbox-remote-hosts
      '(("build-box"          . (:read allow :write ask))
        ("prod-"              . (:read once  :write deny))
        ("/sudo:"             . (:read deny  :write deny))))
(setq efrit-sandbox-remote-default '(:read ask :write once))
```

A host entry matches the TRAMP identity (`/ssh:me@box:`), the host
name, or a host suffix.  `allow` needs no prompt, `ask` prompts with
the usual once / session / project scopes, `once` prompts and only a
one-time grant is offered, `deny` refuses without a prompt.  The
policy covers every path the sandbox sees: the file tools, elisp
evaluated by `eval_sexp` (through the file-name handler), buffers
visiting remote files, and shell commands run in a remote project root
(governed by the host's write policy).  The sandbox decides lexically
for remote paths and never opens a connection to ask.  `M-x
efrit-permissions` shows and edits both variables.

### Elisp Evaluation Safety

Efrit includes safety controls for `eval_sexp`:

```elisp
;; Timeout for elisp evaluation (default: 30 seconds)
(setq efrit-tools-eval-timeout 30)

;; Disable elisp evaluation entirely (Claude uses predefined tools only)
(setq efrit-tools-sexp-evaluation-enabled nil)
```

See [SECURITY.md](SECURITY.md) for full security configuration.

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

By default, all data lives under `.efrit/` inside `user-emacs-directory`:

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
