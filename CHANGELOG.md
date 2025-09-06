# Efrit Changelog

All notable changes to Efrit will be documented in this file.

## [0.3.0] - 2025-01-06

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