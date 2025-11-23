# Efrit MCP Server Implementation Plan

## Project Structure

```
mcp/
├── package.json                 # Node.js project configuration
├── tsconfig.json               # TypeScript configuration
├── jest.config.js              # Jest testing configuration
├── README.md                   # Usage documentation
├── config/
│   └── instances.json          # Instance configurations
├── src/
│   ├── server.ts              # Main MCP server implementation
│   ├── efrit-client.ts        # File queue communication
│   ├── types.ts               # TypeScript interfaces
│   └── utils.ts               # Shared utilities
├── test/
│   ├── efrit-client.test.ts   # Unit tests for client
│   ├── server.test.ts         # Integration tests
│   └── fixtures/              # Test data
└── dist/                      # Compiled JavaScript output
```

## Implementation Tasks

### Phase 1: Foundation (Week 1)

#### Task 1.1: Project Setup
- [ ] Create `mcp/` directory structure
- [ ] Initialize `package.json` with dependencies:
  - `@modelcontextprotocol/sdk` (MCP protocol)
  - `typescript`, `@types/node` (TypeScript support)
  - `jest`, `@types/jest` (testing)
  - `uuid`, `@types/uuid` (request ID generation)
- [ ] Configure `tsconfig.json` for Node.js environment
- [ ] Configure `jest.config.js` for TypeScript testing
- [ ] Add build scripts to root `Makefile`

#### Task 1.2: Type Definitions (`src/types.ts`)
```typescript
// Core request/response interfaces
interface EfritRequest {
  id: string;
  type: 'command' | 'eval' | 'chat';
  content: string;
  instance_id?: string;
  options?: {
    timeout?: number;
    return_context?: boolean;
  };
  timestamp: string;
}

interface EfritResponse {
  id: string;
  status: 'success' | 'error' | 'timeout';
  result?: any;
  error?: string;
  context?: any;
  timestamp: string;
  execution_time?: number;
}

interface InstanceConfig {
  queue_dir: string;
  workspace_dir?: string;
  timeout: number;
  daemon_name?: string;
}

interface McpServerConfig {
  instances: Record<string, InstanceConfig>;
  default_instance: string;
  max_concurrent_requests?: number;
  log_level?: 'debug' | 'info' | 'warn' | 'error';
}
```

#### Task 1.3: Efrit Client (`src/efrit-client.ts`)
- [ ] Implement constructor with instance configuration
- [ ] Add `generateRequestId()` using crypto.randomUUID()
- [ ] Add `expandUser()` for tilde expansion with security validation
- [ ] Add `ensureDirectories()` to create queue structure
- [ ] Add `writeRequest()` with atomic file operations (temp + rename)
- [ ] Add `pollForResponse()` with configurable timeout and intervals
- [ ] Add `execute()` main method combining write + poll
- [ ] Add comprehensive error handling and logging
- [ ] Add request cleanup on timeout/error

#### Task 1.4: Unit Tests (`test/efrit-client.test.ts`)
- [ ] Test request ID generation uniqueness
- [ ] Test path expansion and security validation
- [ ] Test directory creation
- [ ] Test atomic file writing
- [ ] Test response polling with mock files
- [ ] Test timeout handling
- [ ] Test error scenarios (permissions, malformed JSON)

### Phase 2: MCP Server Core (Week 2)

#### Task 2.1: MCP Server (`src/server.ts`)
- [ ] Initialize MCP server with protocol compliance
- [ ] Load configuration from `config/instances.json`
- [ ] Implement `efrit_execute` tool with parameter validation
- [ ] Add request routing to appropriate instance
- [ ] Add response formatting for MCP protocol
- [ ] Add basic logging infrastructure
- [ ] Add graceful shutdown handling

#### Task 2.2: Configuration System
- [ ] Create default `config/instances.json`
- [ ] Add configuration validation
- [ ] Support environment variable overrides
- [ ] Add configuration reload capability

#### Task 2.3: Integration Tests (`test/server.test.ts`)
- [ ] Test MCP protocol compliance
- [ ] Test `efrit_execute` with different types (command/eval/chat)
- [ ] Test multi-instance routing
- [ ] Test error response formatting
- [ ] Test timeout scenarios
- [ ] Mock file system operations for CI/CD

### Phase 3: Advanced Features (Week 3)

#### Task 3.1: Additional MCP Tools
- [ ] Implement `efrit_list_instances` tool
- [ ] Implement `efrit_get_queue_stats` tool
- [ ] Implement `efrit_start_instance` tool (if daemon management available)
- [ ] Implement `efrit_stop_instance` tool (if daemon management available)
- [ ] Add tool parameter validation and documentation

#### Task 3.2: Queue Management
- [ ] Add queue statistics collection (pending, processing, completed)
- [ ] Add request archiving after completion
- [ ] Add queue cleanup for old files
- [ ] Add queue health monitoring

#### Task 3.3: Performance & Reliability
- [ ] Add request/response caching
- [ ] Add concurrent request limiting
- [ ] Add exponential backoff for polling
- [ ] Add request deduplication
- [ ] Add performance metrics collection

### Phase 4: Production Ready (Week 4)

#### Task 4.1: Security Hardening
- [ ] Add comprehensive input validation
- [ ] Add rate limiting per client
- [ ] Add request size limits
- [ ] Add file system permission checks
- [ ] Add security audit logging

#### Task 4.2: Documentation
- [ ] Complete `mcp/README.md` with examples
- [ ] Add configuration reference documentation
- [ ] Add troubleshooting guide
- [ ] Add deployment examples (Docker, K8s)

#### Task 4.3: Deployment Support
- [ ] Add Dockerfile
- [ ] Add docker-compose.yml for development
- [ ] Add Kubernetes manifests
- [ ] Add systemd service files
- [ ] Add environment-specific configurations

#### Task 4.4: Testing & Quality
- [ ] Add end-to-end tests with real Efrit instances
- [ ] Add performance benchmarks
- [ ] Add security penetration tests
- [ ] Add CI/CD pipeline configuration
- [ ] Achieve >95% test coverage

## Integration with Existing Codebase

### Makefile Updates
```make
# Add to existing Makefile
.PHONY: mcp-install mcp-build mcp-test mcp-start mcp-clean

mcp-install:
	cd mcp && npm install

mcp-build: mcp-install
	cd mcp && npm run build

mcp-test: mcp-build  
	cd mcp && npm test

mcp-start: mcp-build
	cd mcp && npm start

mcp-clean:
	rm -rf mcp/node_modules mcp/dist

# Update existing targets
build: compile mcp-build
test: test-simple mcp-test
clean: mcp-clean
```

### Documentation Updates
- [ ] Update main `README.md` to mention MCP server
- [ ] Add MCP section to `AGENTS.md`
- [ ] Update `ARCHITECTURE.md` with MCP integration

### Queue System Compatibility
- [ ] Verify compatibility with existing `~/.emacs.d/.efrit/queues` structure
- [ ] Test integration with current `efrit-do`, `efrit-chat`, `efrit-agent` modes
- [ ] Ensure no conflicts with existing file-based communication

## Testing Strategy

### Development Testing
1. **Unit Tests**: Individual component testing with Jest
2. **Integration Tests**: MCP server + mock file system
3. **Manual Testing**: Real Efrit instance communication
4. **Performance Testing**: High volume request simulation

### CI/CD Testing
1. **Automated Test Suite**: Run on every commit
2. **Cross-Platform Testing**: Linux, macOS, Windows
3. **Node.js Version Matrix**: Test multiple Node versions
4. **Security Scanning**: Static analysis and dependency audit

## Delivery Schedule

- **Week 1**: Foundation complete, basic client working
- **Week 2**: MCP server functional, core tool implemented
- **Week 3**: All tools implemented, performance optimized
- **Week 4**: Production ready, fully documented

## Success Criteria

1. **Functional**: Claude can execute commands via MCP → Efrit
2. **Compatible**: Works with current main branch Oracle fixes
3. **Reliable**: Handles errors gracefully, no data loss
4. **Performant**: Sub-500ms response for simple commands
5. **Secure**: Validates inputs, prevents path traversal
6. **Maintainable**: Well-tested, documented, follows Efrit patterns

## Risk Mitigation

1. **API Changes**: Follow MCP protocol specification strictly
2. **File System Issues**: Robust error handling, atomic operations
3. **Performance**: Implement timeouts, rate limiting, concurrent limits
4. **Security**: Input validation, sandboxing, audit logging
5. **Compatibility**: Extensive testing with current Efrit functionality

This plan delivers a production-ready MCP server that integrates seamlessly with Efrit while maintaining architectural purity and leveraging our recent improvements.
