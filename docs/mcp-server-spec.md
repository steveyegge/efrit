# Efrit MCP Server Specification

## Overview

An MCP (Model Context Protocol) server that enables Claude and other AI models to interact with Efrit instances through the file-based remote queue system. This server provides structured tools for submitting commands, evaluating Elisp code, and managing conversational sessions with Efrit.

## Server Metadata

```json
{
  "name": "efrit",
  "version": "1.0.0", 
  "description": "MCP server for AI-powered Emacs instances via Efrit",
  "protocol_version": "2024-11-05"
}
```

## Tools

### 1. efrit_execute

Execute commands, code, or chat with an Efrit instance.

**Parameters:**
- `instance_id` (string, required): Target Efrit instance identifier
- `type` (enum, required): Request type - "command", "eval", or "chat"
- `content` (string, required): The command, code, or message content
- `timeout` (number, optional): Execution timeout in seconds (default: 30)
- `return_context` (boolean, optional): Include Emacs context in response

**Returns:**
```json
{
  "request_id": "efrit_1735939200_1234",
  "status": "success|error|timeout",
  "result": "execution output or response",
  "error": "error message if failed", 
  "execution_time": 1.25,
  "context": {
    "buffers": [...],
    "current_buffer": "...",
    "cursor_position": 123
  }
}
```

### 2. efrit_list_instances

List available Efrit instances and their status.

**Parameters:** None

**Returns:**
```json
{
  "instances": [
    {
      "id": "default",
      "status": "active|inactive|error",
      "queue_dir": "~/.emacs.d/.efrit/queues", 
      "uptime_seconds": 3600,
      "last_activity": "2025-01-03T18:30:00Z"
    }
  ]
}
```

### 3. efrit_get_queue_stats

Get queue processing statistics for an instance.

**Parameters:**
- `instance_id` (string, required): Target Efrit instance

**Returns:**
```json
{
  "requests_processed": 42,
  "requests_succeeded": 38,
  "requests_failed": 4,
  "currently_processing": 2,
  "average_processing_time": 1.8,
  "uptime_seconds": 7200
}
```

### 4. efrit_start_instance

Start an Efrit instance with specified configuration.

**Parameters:**
- `instance_id` (string, required): Instance identifier
- `queue_dir` (string, optional): Custom queue directory path
- `daemon_name` (string, optional): Emacs daemon name
- `workspace_dir` (string, optional): Workspace directory path

**Returns:**
```json
{
  "status": "started|already_running|error",
  "message": "Instance started successfully",
  "queue_dir": "/path/to/queue",
  "daemon_name": "efrit-ai"
}
```

### 5. efrit_stop_instance

Stop a running Efrit instance.

**Parameters:**
- `instance_id` (string, required): Target instance to stop

**Returns:**
```json
{
  "status": "stopped|not_running|error",
  "message": "Instance stopped successfully"
}
```

## Implementation Architecture

The MCP server acts as a bridge between MCP clients (like Claude) and Efrit's file-based queue system:

```
Claude/MCP Client → MCP Server → JSON Files → Efrit Queue System → Emacs/Elisp
                                     ↑
                              File Watcher Monitoring
```

### Core Workflow

1. **Request Submission**: MCP client calls `efrit_execute` with command
2. **JSON Generation**: Server creates JSON request file in `queues/requests/`  
3. **File Monitoring**: Efrit's file watcher processes the request
4. **Response Polling**: Server monitors `queues/responses/` for completion
5. **Result Return**: Server returns formatted response to MCP client

### Multi-Instance Support

The server manages multiple Efrit instances by:
- Maintaining separate queue directories per instance
- Tracking instance status and configuration
- Load balancing requests across available instances
- Supporting both local and shared storage (NFS/S3) for Kubernetes deployments

### Configuration Format

```json
{
  "instances": {
    "default": {
      "queue_dir": "~/.emacs.d/.efrit/queues",
      "daemon_name": "efrit-default",
      "timeout": 30,
      "auto_start": true
    },
    "pod-1": {
      "queue_dir": "/shared/efrit-instance-1/queues",
      "daemon_name": "efrit-ai-1", 
      "timeout": 60,
      "auto_start": false
    }
  },
  "polling_interval_ms": 500,
  "max_concurrent_requests": 20,
  "cleanup_interval_minutes": 10
}
```

## Request/Response Protocol

### Request Format (JSON file written to `requests/`)
```json
{
  "id": "efrit_1735939200_1234",
  "type": "command|eval|chat",
  "content": "create a buffer called test.txt with hello world",
  "options": {
    "timeout": 30,
    "return_context": true
  }
}
```

### Response Format (JSON file read from `responses/`)
```json
{
  "id": "efrit_1735939200_1234", 
  "timestamp": "2025-01-03T18:30:00Z",
  "status": "success|error",
  "result": "Buffer 'test.txt' created with content",
  "execution_time": 1.25,
  "context": {
    "current_buffer": "test.txt",
    "buffer_count": 3,
    "cursor_position": 12
  }
}
```

## Usage Example

```python
# MCP client interaction
response = mcp_client.call_tool("efrit_execute", {
    "instance_id": "default",
    "type": "command", 
    "content": "create a new Python file with a hello world function",
    "return_context": True
})

print(f"Efrit says: {response['result']}")
# Output: "Created hello.py with hello_world() function"
```

## Benefits

1. **Seamless Integration**: Leverages existing proven remote queue architecture
2. **Multi-Instance Support**: Can orchestrate work across multiple Efrit instances  
3. **Cloud Native**: Works with shared storage for Kubernetes deployments
4. **Asynchronous**: Non-blocking request processing with proper error handling
5. **Context Aware**: Optional rich context return for advanced AI workflows

This MCP server transforms Efrit from a standalone tool into a distributed AI development platform that Claude can orchestrate naturally.
