# Efrit MCP Server Design

## Overview

This document outlines the design for implementing a Model Context Protocol (MCP) server for Efrit, inspired by PR #14 but built to be compatible with the current main branch and our recent Oracle fixes for tool selection and loop prevention.

## Architecture

### Core Principle: Zero Client-Side Intelligence in MCP Layer

Following Efrit's architectural principle, the MCP server will be a "dumb bridge" that:
- Accepts MCP tool calls from AI models
- Translates them to Efrit's file-based queue system
- Polls for responses and returns them to the MCP client
- Does NOT interpret, parse, or make decisions about requests

### System Components

```
AI Model (Claude) → MCP Client → MCP Server → File Queue → Efrit Instance → Results
```

1. **MCP Server** (`mcp/src/server.ts`)
   - Implements MCP protocol specification
   - Provides efrit-specific tools to AI models
   - Handles request/response translation

2. **Efrit Client** (`mcp/src/efrit-client.ts`)  
   - Manages file-based queue communication
   - Handles atomic file operations
   - Supports multiple instance configurations

3. **Type Definitions** (`mcp/src/types.ts`)
   - Request/response interfaces
   - Instance configuration schemas
   - MCP tool parameter types

## MCP Tools

### Primary Tool: `efrit_execute`

The main interface for AI models to interact with Efrit:

```typescript
interface EfritExecuteParams {
  type: "command" | "eval" | "chat";
  content: string;
  instance_id?: string;
  return_context?: boolean;
  timeout?: number;
}
```

**Usage Examples:**
- `type: "command"` - Natural language commands (uses efrit-do)
- `type: "eval"` - Direct Elisp evaluation
- `type: "chat"` - Conversational queries (uses efrit-chat)

### Supporting Tools

1. **`efrit_list_instances`** - List available Efrit instances
2. **`efrit_get_queue_stats`** - Monitor queue processing statistics
3. **`efrit_start_instance`** / **`efrit_stop_instance`** - Instance lifecycle management

## File-Based Queue Integration

### Request Format
```json
{
  "id": "efrit_<uuid>",
  "type": "command|eval|chat",
  "content": "request content",
  "instance_id": "development",
  "options": {
    "timeout": 30,
    "return_context": true
  },
  "timestamp": "2025-09-08T12:00:00Z"
}
```

### Response Format
```json
{
  "id": "efrit_<uuid>", 
  "status": "success|error|timeout",
  "result": "execution result",
  "context": { ... },
  "timestamp": "2025-09-08T12:00:30Z",
  "execution_time": 1.2
}
```

## Instance Configuration

Support for multiple Efrit instances with different configurations:

```json
{
  "instances": {
    "default": {
      "queue_dir": "~/.emacs.d/.efrit/queues",
      "workspace_dir": "~/.emacs.d/.efrit/workspace", 
      "timeout": 30,
      "daemon_name": "efrit-default"
    },
    "development": {
      "queue_dir": "/shared/efrit-dev/queues",
      "workspace_dir": "/shared/efrit-dev/workspace",
      "timeout": 45,
      "daemon_name": "efrit-dev"
    }
  },
  "default_instance": "default"
}
```

## Integration with Current Architecture

### Compatibility with Oracle Fixes

The MCP server will be compatible with our recent Oracle fixes:
- **Tool Selection**: Requests will leverage fixed `eval_sexp` vs `todo_analyze` logic
- **Loop Prevention**: File-based communication naturally avoids API loops
- **ZCSI Compliance**: MCP server remains a pure bridge with no decision-making

### Queue Directory Structure

Uses existing Efrit queue structure:
```
~/.emacs.d/.efrit/queues/
├── requests/     # MCP → Efrit requests
├── processing/   # Currently being processed  
├── responses/    # Efrit → MCP responses
└── archive/      # Historical communications
```

## Security Considerations

1. **Path Validation**: Prevent directory traversal attacks
2. **Request Sanitization**: Validate JSON structure and content
3. **Timeout Management**: Prevent resource exhaustion
4. **File Permissions**: Secure queue directory access
5. **Rate Limiting**: Prevent MCP client abuse

## Error Handling

1. **Request Errors**: Invalid JSON, missing parameters
2. **Queue Errors**: File system issues, permission problems  
3. **Timeout Errors**: No response within configured time
4. **Instance Errors**: Efrit daemon not running, communication failures

## Implementation Phases

### Phase 1: Core MCP Server
- Basic MCP protocol implementation
- Single `efrit_execute` tool
- Default instance support
- Basic error handling

### Phase 2: Multi-Instance Support
- Instance configuration system
- Load balancing across instances
- Instance lifecycle management tools

### Phase 3: Advanced Features
- Queue statistics and monitoring
- Request/response caching
- Performance optimization
- Comprehensive logging

## Testing Strategy

1. **Unit Tests**: Core client functionality, request/response handling
2. **Integration Tests**: End-to-end MCP → Efrit → Response flow
3. **Performance Tests**: High-volume request handling, timeout scenarios
4. **Security Tests**: Path traversal, malformed requests, resource exhaustion

## Deployment Options

1. **Local Development**: Single instance, default configuration
2. **Docker Container**: Isolated MCP server with mounted queue directories
3. **Kubernetes**: Multiple instances with shared storage (NFS/S3)
4. **Standalone Service**: System-wide MCP server for multiple users

## Benefits

1. **AI Integration**: Seamless Claude → Efrit communication
2. **Zero Client Intelligence**: Maintains architectural purity
3. **Multi-Instance**: Scalable across development environments
4. **Standard Protocol**: Compatible with any MCP client
5. **Queue-Based**: Leverages existing, proven communication mechanism

This design provides a robust, scalable MCP server that integrates naturally with Efrit's architecture while enabling powerful AI-to-Efrit workflows.
