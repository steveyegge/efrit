# Changelog

All notable changes to Efrit will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Changed
- **API Simplification**: `efrit-do` is now the primary async interface (non-blocking execution with progress buffer)
  - Old `efrit-do` (sync) renamed to `efrit-do-sync`
  - Old `efrit-do-async` renamed to `efrit-do`
  - Old `efrit-do-async-legacy` deprecated (use `efrit-do-sync` instead)
- **Documentation**: Added migration guide in README for users upgrading from previous versions
- **Key bindings**: Updated default keybinding documentation to reflect new async-first approach

### Added
- **Progress buffer integration**: Async commands now show real-time progress with thinking and tool execution
- **Command queuing**: Support for queuing commands while another is running
- **Background execution**: `efrit-do-silently` for non-blocking execution without visible progress buffer
- **Progress visualization**: Interactive progress buffer with session status updates

### Deprecated
- `efrit-do-sync`: Use `efrit-do` for async or if blocking is needed use `efrit-do-sync` explicitly
- `efrit-do-async-legacy`: Use `efrit-do-sync` if blocking behavior is required
- `efrit-do-async`: Use `efrit-do` (now the primary interface)
- `efrit-api-url`: Use `efrit-api-base-url` in efrit-common.el instead. The legacy variable still works as an override but is deprecated.

## [0.13.14] - 2026-10-21

Agent buffer interaction.

### Added

- RET steers a running turn, M-RET queues
- Introspection tools: symbol_info, live_state, mode_line_report, buffer_scan, buffer_list (local buffers first, remote by name only)
- efrit-agent-screenshot: /shot and show_ui_element attach a frame image with a layout block of windows and line constructs
- efrit-agent-fold: a finished turn's tool and review rows fold under one line above the answer; isearch opens folds
- efrit-choice: one transient chooser for questions, multi-select sets and confirmations; question options as wid-edit widgets with All / None / Submit; confirm_action never times out
- efrit-questions: a bare re-ask after the user asked something is refused once with the question quoted; prose choice lists become a real question
- reproduce_command: run the user's failing command and return the backtrace from the signal point
- efrit-default-project-root for turns outside a VC repo
- Reviewer sees the turn so far; a removal after an addition is a move
- Stale loop entries recovered; cancel follows the session status
- Package installs and synchronous downloads refused in eval_sexp
- File changed on disk reconciled without asking; chopped rows re-cut on resize

## [0.12.36] - 2026-10-20

Agents, pills, worktrees, symbolic paths.

### Added

- efrit-agent-ring: move between agents, attention flags (waiting, unread with dwell), mark unread; worktree instances; diagnose rows; dashboard folders; focus and restore
- efrit-pill: SVG pill primitive; agents in the tab line as pills; pills in dashboard, diagnose and mode line; one spinner per mode line
- efrit-agent-consult: consult another agent
- run_program tool (argv, no shell); shell grants tolerate read-only extras; no shelling out for what Emacs wraps
- efrit-sandbox-symbolic: instruction templates become expected paths and reviewer facts; placeholders rendered in prompts
- read_file over several paths with a budget; project_outline tool; load_project_file and run_ert run in this Emacs
- Pasted and dropped images kept outside the project, pruned by age
- Editing without junk backups; buffer grant covered by read and write grants on the file's directory
- Final message shown after narration; full-width header

## [0.11.1] - 2026-10-11

Local code review.

### Added

- efrit-code-review: scope by diff hash, findings model, gates from old_lines to a patch, applied bottom-up; submit_review, verify_block and read_diff tools
- efrit-code-review-ui: review buffer with queue, apply and dismiss; Magit dispatch entry and agent menu entry
- surrounding_context tool; scoped read_file

## [0.10.3] - 2026-10-08

Reconnect.

### Added

- efrit-reconnect: transport failures retried with backoff and a probe of the endpoint; the turn's work is kept
- After repeated failures the user is asked: keep waiting, retry or abort; nothing answers for them

## [0.9.2] - 2026-10-04

Reviewer confidence.

### Added

- efrit-grant-history records what the user allowed
- efrit-review-confidence: history and facts give a score; reviewer confidence at or above the threshold satisfies the sandbox without a prompt, once per session, with hard blockers

## [0.8.5] - 2026-10-02

Walk-away.

### Added

- Timeout via the limits menu (30 min default); a notification on every prompt; efrit-unattended mode; progress-based stall detection
- Prompts never time out and are answered only by the user; a menu closed by anything else is a denial
- efrit-review-flags: static analysis of proposed Lisp changes (keymaps, hooks, advice, load order, eval forms) feeds the reviewer as FLAG lines
- Circuit breaker per target (URL, path, command) and per tool; a trip pauses one tool, not the turn

## [0.6.2] - 2026-10-01

Sandbox and review fixes.

### Fixed

- Grants are scoped to the VC root; an expected/unusual classifier decides what is asked about; transport paths are exempt
- A review rejection advises the model and never ends the turn; standing instructions are shown to the reviewer
- Session history fixes

## [0.5.3] - 2026-09-30

Test drive fixes.

### Fixed

- Tour stops no longer depend on each other; the drive dumps state on failure
- Dedicated window layout for the drive
- The sandbox prompt works under inhibit-quit
- efrit-run-tests: one Emacs per ERT file, explicit file selection
- efrit-do legacy path renders into the session's buffer

## [0.5.0] - 2026-09-29

Editor-integrated agent.

### Added

- efrit-api-stream: streaming over curl; token-by-token rendering, real cancel, partial answer kept on timeout
- efrit-api-auth-scheme for bearer-token LLM proxies; efrit-api-request-transforms and efrit-api-extra-body
- efrit-permissions: consent before mutating tool calls, with a diff preview and edit-before-approve in the prompt
- efrit-events: an event bus the agent buffer subscribes to; idle timer; live token usage in the header line
- efrit-context-sources: editor context (buffer, point, region, project) prepended to REPL turns; editor_state tool
- efrit-file-io: buffer-aware, verified file edits
- efrit-models: model discovery and selection; gateway model errors recognised
- efrit-doctor: checks every configuration layer and offers fixes
- efrit-agent-svg-header: graphical header for the agent buffer
- efrit-system-prompt-functions; dead task classifier dropped
- Tramp: sandbox and tool processes are remote-aware
- Generated autoloads cover lisp/ subdirectories; make test runs ERT
- Agent buffer scrolls only windows that were following the output
- Smaller ports: transient menu, template expander, Lisp balance check, tail truncation, warn-once, hardened persistence
- efrit-sandbox: scope grants per project (read, write, shell, net, elisp, buffer), JSON store, every tool checked, eval_sexp wrapped with static inspection and dynamic enforcement; live buffers visiting out-of-project files gated
- efrit-sandbox-ui: transient prompt with the form shown, grant list, ? toggles inline details, y yanks, b opens a buffer; a denial continues the turn as a failed tool result
- efrit-review: a second model reviews mutating tool calls before they run; every turn shows its review status; control-only turns emit no review note; network calls reviewed
- efrit-instructions: AGENTS.md / CLAUDE.md loaded in layers
- efrit-limits: circuit breaker per turn with a raise prompt; limits menu
- efrit-reload: self-updating reload that refreshes keymaps, reverts unmodified buffers visiting reloaded files, adopts changed defcustom defaults the user never set
- Agent buffer: comint-style input, user-turn block, answers as prose, one-line tool rows chopped at the window width, menus for choices and limits, review rows, report buffers on demand, SVG activity spinner; the window is dedicated and closes on quit
- emacs_apropos tool and a prompt that teaches the model Emacs first
- Permissions: editor across projects, per-command shell grants, project policy file; read requests ask for the enclosing git tree once and absorb narrower grants
- Zero byte-compile warnings, gated in make
- efrit-package-review: wraps package-review (Emacs 31); the model reads sources, diff and changelog before "Install?", with a rendered report; batch review of installed packages
- efrit-testdrive: guided live test drive of the whole system
- efrit-tool-registry: packages register tools and submit prompts
- efrit-settings: per-project settings file
- efrit-log-mode with a debug toggle; API requests and responses, bus events, sandbox decisions and raw reviewer answers logged
- efrit-permissions-ui split out; sandbox prompt N/q
- API: astral characters escaped as UTF-16 surrogate pairs; JSON escapes never case-converted; synchronous requests keep Emacs alive; a turn whose request cannot be built fails instead of hanging
- Reload drops features whose file was removed
- Agent buffer subscribes to the bus; efrit-loop knows no interface
- efrit-gnus: analyse Gnus articles in isolated batch turns with a stitched closing summary; generic expander for articles with links
- efrit-documents: a source protocol with doc_* tools; Google Drive, Google Calendar, Confluence and Jira sources; efrit-auth over auth-source and OAuth2
- efrit-prompts and a prompt editor: scoped task prompts, review before send
- efrit-markdown: renderer for the agent buffer (images, tables, code blocks)
- efrit-agent-mentions: @file completion with #symbol and #L ranges, slash commands, drag and drop of files and images
- efrit-agent-menu: the transient menu for the agent buffer
- efrit-transcript: the turn is appended to a transcript file as it goes
- API watchdog for asynchronous requests; timeouts tuned
- Agent buffer: queue and steer while a turn runs, invisible folds with isearch support, undo hygiene, quote region into prompt, narrow to the last N turns, list editing in the input
- efrit-vcs: all version-control work through VC and Magit, never a shell git; efrit-magit hunk actions with provenance; efrit-commit builds commit messages from the staged diff
- Multi-session: rendering routed by session id, per-session project root, per-turn state keyed by session; efrit-agent-instances with *efrit[project]* buffers, side-window slots, rename, dashboard
- Navigation tools: xref_references, xref_apropos, imenu_symbols, treesit_info, show_location, get_last_error; get_diagnostics over the project
- efrit-brief: question-only briefs drive a read-only turn; efrit-ask side requests; efrit-rewrite with an editable-region protocol; efrit-scope task commands; regenerate the last turn
- efrit-commands: send-DWIM, investigate-exception, checkpoint steer, :shell commands, blame and log prompts, refactor catalog
- efrit-diagnostics-baseline: new diagnostics appended to edit results; fix-errors-in-scope
- efrit-edit-history context source; efrit-inline-diff hunk overlays; efrit-text-window; efrit-throttle; efrit-notify; efrit-presets; efrit-next-steps buttons; efrit-elisp-fix balancer
- ediff review from the diff preview; context indicator in the input header; pins

## [0.4.1] - 2025-11-30

### Added
- **efrit-chat improvements**: Batch mode support and error classification
- **Transparency features**: Tool visibility indicators and incremental response display for streaming content
- **Input handling**: Display of thinking/reasoning to users when available
- **Buffer tools**: New buffer creation and editing capabilities in efrit-chat
- **Session features**: History navigation (M-p/M-n) and context-aware input completion in efrit-agent
- **Error recovery**: Action buttons in efrit-agent for failed tool execution
- **Display enhancements**: Inline diff display for tool results and thinking indicator display

### Changed
- **Chat refactoring**: Reorganized efrit-chat.el into three focused modules
  - `efrit-chat-api.el`: API communication
  - `efrit-chat-buffer.el`: Buffer management
  - `efrit-chat-persistence.el`: Session persistence and history
- **UI refactoring**: Split efrit-agent into 5 logical modules for better maintainability
  - `efrit-agent-core.el`: Core session management
  - `efrit-agent-render.el`: Rendering logic
  - `efrit-agent-integration.el`: Executor integration
  - `efrit-agent-tools.el`: Tool-specific display
  - `efrit-agent-input.el`: Input handling
- **Tool handlers**: Extracted tool-specific logic from efrit-do.el into `efrit-do-handlers.el`
- **Model version**: Fixed model version mismatch issues in efrit-chat

### Fixed
- Fixed efrit-chat model version configuration
- Fixed efrit-agent mode initialization logic
- Corrected Makefile dependency paths for reorganized modules
- Corrected link to contributor guidelines in README
- Improved error handling and classification in chat mode
- Fixed batch mode support for concurrent requests

### Removed
- Removed debug test files and stale documentation references

## [0.4.0] - 2025-11-28

### Added
- **Public release of 0.4.0**: First stable version with modernized architecture
- **Comprehensive documentation**: Consolidated from 27 files to 10
- **Test coverage reporting**: Added coverage metrics to test runner
- **Development tooling**: Improved with CLAUDE.md agent instructions
- **Session persistence**: Full session transcript persistence and resume capability
- **Autonomous startup**: Development mode for daemon startup and testing

### Changed
- **Async architecture refactoring**: Simplified and reorganized core modules
  - Moved executor and tools to core/ directory
  - Consolidated session-related modules (6 modules → 1 focused efrit-session.el)
  - Reorganized interfaces into interfaces/ directory
  - Merged UI modules into support/efrit-ui.el
- **Major version bump**: Incremented to 0.4.0 to reflect significant modernization
- **Testing**: Expanded automated test specs for Tiers 1-6
- **MCP integration**: Enabled ESM tests and improved security

### Fixed
- Fixed unicode escape bug causing API failures
- Fixed session message syncing for transcript persistence
- Fixed circuit breaker being too restrictive for multi-step coding tasks
- Added error loop detection to circuit breaker
- Fixed multibyte text in HTTP request errors

## [0.3.1] - 2025-11-26

### Added
- **Multimodal support**: Added read_image tool for image analysis
- **Image handling**: Proper base64 encoding and Anthropic Claude API integration
- **Resource limits**: Budget management system (efrit-budget.el)

### Fixed
- Fixed void function error requiring efrit-tool-utils at load time
- Improved read_image tool prompting to prevent find-file misuse
- Fixed sandbox bypass for read-only operations

## [0.3.0] - 2025-11-24

### Added
- **First working e2e multi-turn chat**: Complete request/response cycle with proper tool handling
- **Tool protocol fixes**: Proper tool_use_id capture and tool result submission
- **MCP server foundation**: Started TypeScript/Node MCP server implementation
- **Todo write system**: Implemented todo_write tool for session note-taking

### Changed
- **Tool protocol**: Completely rewrote tool result submission format
- **Circuit breaker**: Added better error recovery and action buttons

### Fixed
- Fixed tool use protocol to properly submit tool results back to Claude
- Fixed multibyte text in HTTP request errors
- Fixed streamlined mode tool_result format

## [0.2.0] and Earlier

Earlier versions focused on architecture modernization, refactoring, and laying groundwork for the public release.

---

## Legend

- **Added**: New features
- **Changed**: Changes in existing functionality
- **Fixed**: Bug fixes
- **Removed**: Removed functionality
- **Deprecated**: Deprecated functionality (will be removed in a future version)
