# Changelog

All notable changes to Efrit will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

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
