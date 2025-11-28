# Efrit Changelog

All notable changes to Efrit will be documented in this file.

## [0.3.1] - 2025-11-28

### Added
- **efrit-agent.el**: World-class agentic session buffer with real-time tool progress display
  - Interactive input section for user interaction
  - Terminal-compatible display for tool execution
  - Memory-efficient parallel tool call handling
- **Session Transcript Persistence**: Sessions can now be saved and resumed
  - `efrit-session--save-transcript` and `efrit-session--load-transcript`
  - Messages synced to session for full transcript persistence
- **Naming Conventions Guidance**: Added to system prompts for consistent Elisp style
- **Improved TodoWrite Documentation**: Enhanced guidance for tool usage

### Changed
- Increased executor turn limit from 25 to 50 with encouragement for batching
- Enhanced tool progress display for better terminal compatibility

### Fixed
- **search_content Tool**: Fixed path sandbox bugs that blocked legitimate searches
- **UTF-8 Encoding**: Resolved encoding bug in watch buffer refresh (efrit-progress)
- **Memory Leak**: Fixed timer and buffer leaks in efrit-agent parallel tool handling
- **Agentic Loop**: Fixed efrit-do agentic loop and string/symbol key bug
- **Executor Messages**: Fixed message sync to session for transcript persistence

### Infrastructure
- **MCP Tests**: All 79 tests now passing (was 17/27)
  - Added `isolatedModules: true` to tsconfig.json
  - Added `NODE_OPTIONS='--experimental-vm-modules'` to npm test script
  - Fixed ESM `require()` calls to use proper imports
- **Security**: Fixed npm vulnerabilities (body-parser DoS, js-yaml prototype pollution)
- **STATUS.md**: Updated to reflect current project state

## [0.3.0] - 2025-11-24

### Milestone: First Working End-to-End Since Modernization

This release marks a major milestone: the first fully working end-to-end chat experience
since the modernization effort began. Multi-turn tool use conversations now work correctly.

### Added
- **Session-Based Architecture**: Complete implementation of multi-step operations with Claude-controlled flow
- **Async Execution**: Non-blocking command execution with automatic queue processing
- **Performance Module**: Response caching, memory management, and API call tracking
- **Unified Interface**: Claude decides whether commands run synchronously or asynchronously
- **Context Compression**: Smart work log compression for efficient token usage
- **Performance Statistics**: Track API call times and view performance metrics
- **Integration Tests**: Comprehensive test suite that hits production API
- **Multi-Channel Support**: Support for different API channels with channel-specific keys

### Changed
- Restructured modules to eliminate circular dependencies via `efrit-protocol.el`
- Unified context management under `efrit-context.el`
- Improved memory management with automatic session cleanup
- Enhanced error handling and logging throughout
- Updated model from claude-sonnet-4 to claude-3-5-sonnet-20241022

### Fixed
- **Critical**: Multi-turn tool use conversations now properly send tool_result blocks
  - Fixed `efrit-streamlined--send-request` to handle non-string content (tool_result vectors)
  - Fixed `efrit-streamlined--continue-with-results` message ordering (was using push, now uses append)
  - Assistant messages with tool_use blocks now properly precede user messages with tool_result
- **Critical**: Syntax error in efrit-async.el preventing module from loading
- **Critical**: Wrong API key selection for ai-efrit channel
- Buffer memory leaks in chat interface
- Circular dependency issues between modules
- Byte compilation warnings
- Unicode handling in API requests
- Session-id variable scope issue in efrit-async-execute-command
- Missing final newline in efrit-async.el

### Removed
- Client-side heuristics for execution mode decisions (now 100% Claude-controlled)
- References to non-existent `efrit-command` and `efrit-agent` modules
- Temporary debug and test files

## [0.2.0] - 2024-12-20

### Added
- File-based remote queue system for AI agent communication
- Autonomous development environment support
- Self-enhancement capabilities for AI agents
- Streamlined chat interface

### Changed
- Reorganized data directory structure under single configurable location
- Enhanced logging and debugging capabilities

## [0.1.0] - 2024-12-01

### Initial Release
- Core chat interface with multi-turn conversations
- Natural language command execution (`efrit-do`)
- Direct Elisp evaluation with safety checks
- Tool system for buffer manipulation and file operations
- Basic error handling and confirmation systems