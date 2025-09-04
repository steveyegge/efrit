# Using the Efrit MCP Server

## Overview

The Efrit MCP Server enables Claude and other AI models to interact with Efrit instances through the Model Context Protocol (MCP). This allows for seamless AI-to-AI communication where Claude can orchestrate complex development workflows across distributed Emacs instances.

## Quick Start

### 1. Build and Install

```bash
cd efrit/mcp
npm install
npm run build
```

### 2. Start the Server

```bash
npm start
```

The server runs as a stdio-based MCP server, communicating via JSON-RPC over stdin/stdout.

### 3. Configure Claude

Add to your Claude Desktop or MCP client configuration:

```json
{
  "mcpServers": {
    "efrit": {
      "command": "node",
      "args": ["/absolute/path/to/efrit/mcp/dist/server.js"]
    }
  }
}
```

## Available Tools

### efrit_execute - Core Execution Tool

Execute commands, evaluate Elisp code, or chat with Efrit instances.

**Parameters:**
- `instance_id` (string, optional): Target instance (default: "default")  
- `type` (required): "command", "eval", or "chat"
- `content` (required): The command, code, or message
- `timeout` (optional): Execution timeout in seconds
- `return_context` (optional): Include Emacs context in response

**Examples:**

```typescript
// Execute natural language commands
efrit_execute({
  type: "command",
  content: "create a Python file with a factorial function"
})

// Evaluate Elisp code directly
efrit_execute({
  type: "eval", 
  content: "(buffer-list)"
})

// Chat with Efrit for complex tasks
efrit_execute({
  type: "chat",
  content: "Help me refactor this code to use modern Python patterns",
  return_context: true
})
```

### efrit_list_instances - Instance Management

List all configured Efrit instances and their current status.

```typescript
efrit_list_instances()
// Returns: { instances: [{ id, status, queue_dir, uptime_seconds }] }
```

### efrit_get_queue_stats - Monitoring

Get processing statistics for monitoring and debugging.

```typescript
efrit_get_queue_stats({ instance_id: "production" })
// Returns: { requests_processed, requests_succeeded, pending_requests, ... }
```

### efrit_start_instance - Lifecycle Management

Start new Efrit instances dynamically.

```typescript
efrit_start_instance({
  instance_id: "development",
  queue_dir: "/shared/efrit-dev/queues",
  daemon_name: "efrit-dev"
})
```

### efrit_stop_instance - Cleanup

Stop running instances cleanly.

```typescript
efrit_stop_instance({ instance_id: "development" })
```

## Configuration

### Instance Configuration

Create custom instance configurations for different environments:

```json
{
  "instances": {
    "local": {
      "queue_dir": "~/.emacs.d/.efrit/queues",
      "daemon_name": "efrit-local",
      "timeout": 30,
      "auto_start": true
    },
    "development": {
      "queue_dir": "/shared/efrit-dev/queues", 
      "daemon_name": "efrit-dev",
      "workspace_dir": "/shared/efrit-dev/workspace",
      "timeout": 45
    },
    "production": {
      "queue_dir": "/shared/efrit-prod/queues",
      "daemon_name": "efrit-prod", 
      "timeout": 60,
      "auto_start": false
    }
  }
}
```

Save as `mcp/config/instances.json` and the server will load it automatically.

## Usage Patterns

### Single Instance Development

For local development, use the default instance:

```typescript
// Claude can directly command your local Efrit
efrit_execute({
  type: "command",
  content: "open the main.py file and add error handling to the parse function"
})
```

### Multi-Instance Workflows  

Distribute work across multiple instances:

```typescript
// Start development work on one instance
efrit_execute({
  instance_id: "development",
  type: "command", 
  content: "create unit tests for the user authentication module"
})

// Meanwhile, run integration tests on another
efrit_execute({
  instance_id: "testing",
  type: "command",
  content: "run the full test suite and report any failures"
})
```

### Code Review and Analysis

Use the chat mode for complex analysis:

```typescript
efrit_execute({
  type: "chat",
  content: "Review the authentication.py file and suggest security improvements",
  return_context: true
})
```

### Batch Operations

Process multiple related tasks:

```typescript
// Get current project state
efrit_execute({ type: "eval", content: "(projectile-project-root)" })

// Create multiple related files
efrit_execute({
  type: "command",
  content: "create a complete REST API with models, views, and tests for user management"
})

// Verify the results
efrit_execute({ type: "command", content: "run syntax check on all Python files" })
```

## Deployment Scenarios

### Local Development Setup

1. Start Efrit normally in Emacs
2. Launch the MCP server
3. Configure Claude to use the local server
4. Begin AI-assisted development

### Docker Container Deployment

```dockerfile
FROM node:18-alpine

# Install Emacs
RUN apk add --no-cache emacs

# Copy Efrit
COPY . /app/efrit
WORKDIR /app/efrit

# Build everything
RUN make compile

# Start MCP server
CMD ["npm", "start"]
```

### Kubernetes with Shared Storage

```yaml
apiVersion: apps/v1
kind: Deployment
metadata:
  name: efrit-mcp-server
spec:
  replicas: 3
  selector:
    matchLabels:
      app: efrit-mcp
  template:
    spec:
      containers:
      - name: efrit-mcp
        image: efrit:latest
        volumeMounts:
        - name: shared-queue
          mountPath: /shared/efrit/queues
        env:
        - name: EFRIT_QUEUE_DIR
          value: "/shared/efrit/queues"
      volumes:
      - name: shared-queue
        persistentVolumeClaim:
          claimName: efrit-shared-storage
```

## Monitoring and Troubleshooting

### Health Checks

Monitor instance health:

```typescript
// Check if instances are responsive
const instances = await efrit_list_instances();
for (const instance of instances.instances) {
  if (instance.status !== 'active') {
    console.log(`Instance ${instance.id} needs attention`);
  }
}
```

### Queue Monitoring

Track processing metrics:

```typescript
const stats = await efrit_get_queue_stats({ instance_id: "production" });
if (stats.requests_failed > stats.requests_succeeded * 0.1) {
  console.log("High error rate detected!");
}
```

### Debug Mode

Enable debug logging by setting environment variables:

```bash
EFRIT_MCP_DEBUG=true npm start
```

### Common Issues

**Instance Not Responding**
- Check if Emacs daemon is running: `emacsclient --socket-name=efrit-default --eval t`
- Verify queue directory permissions and accessibility
- Check logs in `~/.emacs.d/.efrit/logs/`

**Request Timeouts**  
- Increase timeout values for complex operations
- Check system resources (CPU, memory, disk I/O)
- Monitor queue directory for stuck files in `processing/`

**Permission Errors**
- Ensure MCP server process has read/write access to queue directories
- For shared storage, verify NFS/S3 permissions are correct

## Advanced Usage

### Custom Tool Integration

Extend Efrit with custom tools that Claude can invoke:

```typescript
// Have Claude add new functionality to Efrit itself
efrit_execute({
  type: "command",
  content: "implement a new tool for code formatting that integrates with prettier"
})
```

### Multi-Project Workflows

Use different instances for different projects:

```typescript
// Web frontend work
efrit_execute({
  instance_id: "frontend",
  type: "command", 
  content: "optimize the React component rendering performance"
})

// Backend API development  
efrit_execute({
  instance_id: "backend",
  type: "command",
  content: "add rate limiting to the API endpoints"
})
```

### Autonomous Development

Let Claude drive entire development sessions:

```typescript
// Claude plans and executes a complete feature
efrit_execute({
  type: "chat",
  content: "I need to add user authentication to this app. Plan the implementation, create all necessary files, write tests, and integrate with the existing codebase.",
  return_context: true
})
```

## Security Considerations

- **Input Sanitization**: All user inputs are automatically sanitized for path traversal
- **Resource Limits**: Concurrent request limits prevent resource exhaustion  
- **Atomic Operations**: File operations are atomic to prevent race conditions
- **Graceful Shutdown**: Proper cleanup prevents orphaned processes or files

## Performance Tips

- Use `return_context: false` for simple operations to reduce response size
- Batch related operations in single requests when possible
- Monitor queue stats to optimize instance allocation
- Use dedicated instances for long-running operations

The Efrit MCP Server transforms static Emacs configurations into dynamic, AI-orchestrated development environments where Claude can autonomously manage complex software engineering workflows.
