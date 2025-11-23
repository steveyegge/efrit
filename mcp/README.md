# Efrit MCP Server

An MCP (Model Context Protocol) server that enables Claude and other AI models to interact with Efrit instances through the file-based remote queue system.

## Project Status

**Phase 1 Complete** ✅
- TypeScript project setup with strict configuration
- Complete type definitions with Oracle security recommendations
- EfritClient with file-based queue communication
- Security features: path validation, atomic operations, request sanitization
- Comprehensive error handling with structured error types
- Full test suite passing (27/27 tests)

**Phase 2 Complete** ✅
- MCP server implementation with protocol handler
- Configuration system with multi-instance support
- Logging to file (avoids stdio pollution)
- Graceful shutdown handling
- Production-ready error handling

## Quick Start (Development)

```bash
# Install dependencies
make mcp-install

# Build the server
make mcp-build

# Run tests
make mcp-test
```

## Architecture

The MCP server acts as a bridge between MCP clients and Efrit's file-based queue system:

- **Request Flow**: MCP tool call → JSON file → Efrit queue → Emacs execution
- **Response Flow**: Execution result → JSON file → Response polling → MCP response
- **Multi-instance**: Support for multiple Efrit instances with load balancing

## Security Features

- **Path Validation**: Prevents directory traversal attacks
- **Request Sanitization**: Validates JSON structure and content size
- **Atomic Operations**: Uses temp files and renames for consistency
- **Proper Permissions**: Files created with secure permissions (0o600/0o700)
- **Whitelisted Roots**: Queue directories must be in allowed paths

## Core Components

### EfritClient
- Handles file-based queue communication
- Atomic file operations with proper synchronization
- Security validation and error handling
- Queue statistics and health monitoring

### Type System
- Complete interfaces for requests/responses
- Oracle-recommended versioning and namespacing
- Structured error types with context
- Configuration schemas

## Testing

```bash
# Run all tests (27 tests)
npm test

# Run specific test suite
npm test -- efrit-client-simple
```

The test suite includes:
- Path validation and security checks
- Request/response serialization
- File system operations
- Error handling scenarios
- Queue statistics
- Response polling with proper timeout handling

All tests pass cleanly with Jest/ESM compatibility fixes.

## Production Ready

The MCP server is production-ready with:
- ✅ All tests passing (27/27)
- ✅ Security hardened (path validation, atomic operations)
- ✅ Proper error handling and logging
- ✅ Graceful shutdown support
- ✅ Multi-instance management
- ✅ TypeScript strict mode compliance

## Next Steps (Phase 3)

- Integration tests with real MCP clients
- Performance optimization and load testing
- Enhanced monitoring and observability
- Production deployment documentation

This enables seamless AI-to-Efrit communication where Claude can orchestrate complex development workflows across distributed Emacs instances.
