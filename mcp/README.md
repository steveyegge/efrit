# Efrit MCP Server

An MCP (Model Context Protocol) server that enables Claude and other AI models to interact with Efrit instances through the file-based remote queue system.

## Quick Start

```bash
cd mcp
npm install
npm run build
npm start
```

## Usage with Claude

Add to your Claude MCP configuration:

```json
{
  "mcpServers": {
    "efrit": {
      "command": "node",
      "args": ["/path/to/efrit/mcp/dist/server.js"]
    }
  }
}
```

## Available Tools

### efrit_execute
Execute commands, code, or chat with an Efrit instance.

```javascript
// Execute a natural language command
efrit_execute({
  type: "command",
  content: "create a Python file with a hello world function",
  return_context: true
})

// Evaluate Elisp code
efrit_execute({
  type: "eval", 
  content: "(buffer-list)",
  instance_id: "development"
})

// Chat with Efrit
efrit_execute({
  type: "chat",
  content: "How can I optimize this Python code for performance?"
})
```

### efrit_list_instances
List all configured Efrit instances and their status.

### efrit_get_queue_stats
Get queue processing statistics for monitoring.

### efrit_start_instance / efrit_stop_instance  
Manage Efrit instance lifecycle.

## Configuration

Create `config/instances.json` to customize instance configuration:

```json
{
  "instances": {
    "development": {
      "queue_dir": "/shared/efrit-dev/queues",
      "daemon_name": "efrit-dev", 
      "workspace_dir": "/shared/efrit-dev/workspace",
      "timeout": 45
    },
    "production": {
      "queue_dir": "/shared/efrit-prod/queues", 
      "daemon_name": "efrit-prod",
      "timeout": 60
    }
  }
}
```

## Architecture

The MCP server acts as a bridge between MCP clients and Efrit's file-based queue system:

- **Request Flow**: MCP tool call → JSON file → Efrit queue → Emacs execution
- **Response Flow**: Execution result → JSON file → Response polling → MCP response
- **Multi-instance**: Support for multiple Efrit instances with load balancing

## Deployment

### Local Development
Use the default instance pointing to `~/.emacs.d/.efrit/queues`.

### Kubernetes
Mount shared storage (NFS/S3) and configure multiple instances with different queue directories.

### Docker
```dockerfile
FROM node:18-alpine
COPY mcp/ /app
WORKDIR /app
RUN npm install && npm run build
CMD ["npm", "start"]
```

This enables seamless AI-to-Efrit communication where Claude can orchestrate complex development workflows across distributed Emacs instances.
