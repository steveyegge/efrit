#!/usr/bin/env node

import { Server } from '@modelcontextprotocol/sdk/server/index.js';
import { StdioServerTransport } from '@modelcontextprotocol/sdk/server/stdio.js';
import {
  CallToolRequestSchema,
  ListToolsRequestSchema,
  ListResourcesRequestSchema,
  ListPromptsRequestSchema,
  GetPromptRequestSchema,
  ReadResourceRequestSchema,
  ErrorCode,
  McpError,
} from '@modelcontextprotocol/sdk/types.js';

import { EfritClient } from './efrit-client.js';
import { McpServerConfig, EfritResponse, EfritExecuteParams, InstanceConfig } from './types.js';
import { validateEfritExecuteParams } from './validation.js';
import * as fs from 'fs';
import * as path from 'path';
import { fileURLToPath } from 'url';
import pLimit from 'p-limit';
import pino from 'pino';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

class EfritMcpServer {
  private server: Server;
  private config: McpServerConfig;
  private clients: Map<string, EfritClient> = new Map();
  private concurrencyLimit: ReturnType<typeof pLimit>;
  private logger: pino.Logger;

  constructor(configPath?: string) {
    this.server = new Server(
      {
        name: 'efrit-mcp-server',
        version: '1.0.0',
        description: 'MCP Server for Efrit Emacs agent communication'
      },
      {
        capabilities: {
          resources: {},
          tools: {
            'mcp/efrit_execute': {
              description: 'Execute commands, evaluate Elisp code, or chat with an Efrit instance',
              inputSchema: {
                type: 'object',
                properties: {
                  type: {
                    type: 'string',
                    enum: ['command', 'eval', 'chat'],
                    description: 'Type of request'
                  },
                  content: {
                    type: 'string',
                    description: 'The command, elisp code, or chat message'
                  },
                  instance_id: {
                    type: 'string',
                    description: 'Efrit instance ID (optional)'
                  },
                  timeout: {
                    type: 'number',
                    description: 'Request timeout in milliseconds (optional)'
                  },
                  return_context: {
                    type: 'boolean',
                    description: 'Whether to return additional context (optional)'
                  }
                },
                required: ['type', 'content'],
                additionalProperties: false
              }
            },
            'mcp/efrit_list_instances': {
              description: 'List available Efrit instances and their status',
              inputSchema: {
                type: 'object',
                properties: {},
                additionalProperties: false
              }
            }
          },
          prompts: {},
          logging: {}
        }
      }
    );

    this.config = this.loadConfig(configPath);
    this.logger = this.createLogger();
    this.concurrencyLimit = pLimit(this.config.max_concurrent_requests || 10);
    this.initializeClients();
    this.setupHandlers();
  }

  private createLogger(): pino.Logger {
    return pino({
      level: this.config?.log_level || 'info',
      transport: {
        target: 'pino-pretty',
        options: {
          colorize: true,
          translateTime: 'HH:MM:ss Z',
          ignore: 'pid,hostname'
        }
      }
    });
  }

  private loadConfig(configPath?: string): McpServerConfig {
    const resolvedPath = configPath || path.resolve(__dirname, '../config/instances.json');
    
    if (!fs.existsSync(resolvedPath)) {
      // Create default config if it doesn't exist
      const defaultConfig: McpServerConfig = {
        instances: {
          default: {
            queue_dir: '~/.emacs.d/.efrit/queues',
            timeout: 30000,
            workspace_dir: '~/.emacs.d/.efrit/workspace'
          }
        },
        default_instance: 'default',
        max_concurrent_requests: 10,
        log_level: 'info'
      };

      // Ensure config directory exists
      fs.mkdirSync(path.dirname(resolvedPath), { recursive: true });
      fs.writeFileSync(resolvedPath, JSON.stringify(defaultConfig, null, 2));
      return defaultConfig;
    }

    try {
      const configContent = fs.readFileSync(resolvedPath, 'utf-8');
      const config = JSON.parse(configContent) as McpServerConfig;
      this.validateConfig(config);
      return config;
    } catch (error) {
      console.error('Failed to load configuration:', error);
      throw new Error(`Invalid configuration file: ${resolvedPath}`);
    }
  }

  private validateConfig(config: McpServerConfig): void {
    if (!config.instances || Object.keys(config.instances).length === 0) {
      throw new Error('Configuration must contain at least one instance');
    }

    if (!config.default_instance || !config.instances[config.default_instance]) {
      throw new Error('Default instance must exist in instances configuration');
    }

    // Validate each instance configuration
    for (const [instanceId, instanceConfig] of Object.entries(config.instances)) {
      if (!instanceConfig.queue_dir) {
        throw new Error(`Instance ${instanceId} must have a queue_dir`);
      }
      if (typeof instanceConfig.timeout !== 'number' || instanceConfig.timeout <= 0) {
        throw new Error(`Instance ${instanceId} must have a positive timeout value`);
      }
      
      // Oracle recommendation: Path security validation after expansion
      this.validateInstancePaths(instanceId, instanceConfig);
    }
  }

  private validateInstancePaths(instanceId: string, config: InstanceConfig): void {
    const queueDir = this.expandPath(config.queue_dir);
    
    // Oracle recommendation: Validate paths are absolute after expansion
    if (!path.isAbsolute(queueDir)) {
      throw new Error(`Instance ${instanceId} queue_dir must resolve to absolute path: ${queueDir}`);
    }
    
    // Oracle recommendation: Check for path traversal attempts  
    if (queueDir.includes('..') || queueDir.includes('\0')) {
      throw new Error(`Instance ${instanceId} queue_dir contains invalid characters: ${config.queue_dir}`);
    }
    
    // Validate workspace_dir if provided
    if (config.workspace_dir) {
      const workspaceDir = this.expandPath(config.workspace_dir);
      if (!path.isAbsolute(workspaceDir)) {
        throw new Error(`Instance ${instanceId} workspace_dir must resolve to absolute path: ${workspaceDir}`);
      }
      if (workspaceDir.includes('..') || workspaceDir.includes('\0')) {
        throw new Error(`Instance ${instanceId} workspace_dir contains invalid characters: ${config.workspace_dir}`);
      }
    }
  }

  private expandPath(filepath: string): string {
    // Expand tilde to home directory
    if (filepath.startsWith('~/')) {
      return path.join(process.env['HOME'] || process.env['USERPROFILE'] || '/', filepath.slice(2));
    }
    return filepath;
  }

  private initializeClients(): void {
    for (const [instanceId, instanceConfig] of Object.entries(this.config.instances)) {
      try {
        const client = new EfritClient(instanceId, instanceConfig);
        this.clients.set(instanceId, client);
        this.logger.info(`Initialized Efrit client for instance: ${instanceId}`);
      } catch (error) {
        this.logger.error(`Failed to initialize client for instance ${instanceId}:`, error);
        throw error;
      }
    }
  }

  private setupHandlers(): void {
    this.server.setRequestHandler(ListToolsRequestSchema, async () => ({
      tools: [
        {
          name: 'mcp/efrit_execute',
          description: 'Execute commands, evaluate Elisp code, or chat with an Efrit instance',
          inputSchema: {
            type: 'object',
            properties: {
              type: {
                type: 'string',
                enum: ['command', 'eval', 'chat'],
                description: 'Type of request: command (efrit-do), eval (elisp code), or chat (efrit-chat)'
              },
              content: {
                type: 'string',
                description: 'The command, elisp code, or chat message to execute'
              },
              instance_id: {
                type: 'string',
                description: 'Efrit instance ID (optional, uses default if not specified)'
              },
              timeout: {
                type: 'number',
                description: 'Request timeout in milliseconds (optional)'
              },
              return_context: {
                type: 'boolean',
                description: 'Whether to return additional context (optional)'
              }
            },
            required: ['type', 'content']
          }
        },
        {
          name: 'mcp/efrit_list_instances',
          description: 'List available Efrit instances and their status',
          inputSchema: {
            type: 'object',
            properties: {},
            additionalProperties: false
          }
        }
      ]
    }));

    this.server.setRequestHandler(CallToolRequestSchema, async (request) => {
      const { name, arguments: args } = request.params;

      try {
        switch (name) {
          case 'mcp/efrit_execute':
            return await this.handleEfritExecute(args);
          case 'mcp/efrit_list_instances':
            return await this.handleListInstances();
          default:
            throw new McpError(
              ErrorCode.MethodNotFound,
              `Unknown tool: ${name}`
            );
        }
      } catch (error) {
        if (error instanceof McpError) {
          throw error;
        }
        throw new McpError(
          ErrorCode.InternalError,
          error instanceof Error ? error.message : 'Unknown error occurred'
        );
      }
    });

    // Handle resources (empty for now)
    this.server.setRequestHandler(ListResourcesRequestSchema, async () => ({
      resources: []
    }));

    this.server.setRequestHandler(ReadResourceRequestSchema, async (request) => {
      throw new McpError(
        ErrorCode.InvalidRequest,
        `Resource not found: ${request.params.uri}`
      );
    });

    // Handle prompts (empty for now)
    this.server.setRequestHandler(ListPromptsRequestSchema, async () => ({
      prompts: []
    }));

    this.server.setRequestHandler(GetPromptRequestSchema, async (request) => {
      throw new McpError(
        ErrorCode.InvalidRequest,
        `Prompt not found: ${request.params.name}`
      );
    });
  }

  private async handleEfritExecute(args: unknown): Promise<any> {
    // Oracle recommendation: Type-safe validation
    let params: EfritExecuteParams;
    try {
      params = validateEfritExecuteParams(args);
    } catch (error) {
      throw new McpError(
        ErrorCode.InvalidParams,
        `Invalid parameters: ${error instanceof Error ? error.message : 'Unknown validation error'}`
      );
    }

    // Determine which instance to use
    const instanceId = params.instance_id || this.config.default_instance;
    const client = this.clients.get(instanceId);
    
    if (!client) {
      throw new McpError(
        ErrorCode.InvalidParams,
        `Unknown instance: ${instanceId}`
      );
    }

    // Oracle recommendation: Apply concurrency control 
    return await this.concurrencyLimit(async () => {
      this.logger.info(`Executing ${params.type} request on instance ${instanceId}: ${params.content.substring(0, 100)}...`);
      
      const options: {
        timeout?: number;
        return_context?: boolean;
      } = {
        return_context: params.return_context || false
      };
      
      // Oracle recommendation: Fix timeout precision (keep in seconds for EfritClient)
      if (params.timeout) {
        options.timeout = Math.floor(params.timeout / 1000);
      }

      const response = await client.execute(
        params.type,
        params.content,
        options
      );
      
      this.logger.info(`Request ${response.id} completed with status: ${response.status}`);
      
      // Oracle recommendation: Add missing role: "assistant"
      return {
        role: 'assistant',
        content: [
          {
            type: 'text',
            text: this.formatResponse(response)
          }
        ]
      };
    }).catch((error) => {
      this.logger.error(`Request failed:`, error);
      throw new McpError(
        ErrorCode.InternalError,
        error instanceof Error ? error.message : 'Request execution failed'
      );
    });
  }

  private async handleListInstances(): Promise<any> {
    const instances = Object.entries(this.config.instances).map(([id, config]) => ({
      id,
      queue_dir: config.queue_dir,
      workspace_dir: config.workspace_dir,
      timeout: config.timeout,
      daemon_name: config.daemon_name,
      is_default: id === this.config.default_instance,
      status: this.clients.has(id) ? 'connected' : 'disconnected'
    }));

    return {
      role: 'assistant',
      content: [
        {
          type: 'text',
          text: JSON.stringify({
            instances,
            total: instances.length,
            default_instance: this.config.default_instance
          }, null, 2)
        }
      ]
    };
  }

  private formatResponse(response: EfritResponse): string {
    const parts: string[] = [];
    
    // Add status line
    parts.push(`Status: ${response.status.toUpperCase()}`);
    
    if (response.execution_time) {
      parts.push(`Execution time: ${response.execution_time}ms`);
    }
    
    // Add result or error
    if (response.status === 'success' && response.result !== undefined) {
      if (typeof response.result === 'string') {
        parts.push('', 'Result:', response.result);
      } else {
        parts.push('', 'Result:', JSON.stringify(response.result, null, 2));
      }
    } else if (response.status === 'error' && response.error) {
      parts.push('', 'Error:', response.error);
    }
    
    // Add context if present
    if (response.context) {
      parts.push('', 'Context:', JSON.stringify(response.context, null, 2));
    }
    
    return parts.join('\n');
  }

  async start(): Promise<void> {
    const transport = new StdioServerTransport();
    
    this.logger.info('Starting Efrit MCP Server...');
    this.logger.info(`Configured instances: ${Object.keys(this.config.instances).join(', ')}`);
    this.logger.info(`Default instance: ${this.config.default_instance}`);
    this.logger.info(`Concurrency limit: ${this.config.max_concurrent_requests || 10}`);
    
    await this.server.connect(transport);
    this.logger.info('Efrit MCP Server is ready!');
  }

  async stop(): Promise<void> {
    this.logger.info('Stopping Efrit MCP Server...');
    // Clean up any resources
    this.clients.clear();
  }
}

// Start the server if this file is run directly
if (import.meta.url === `file://${process.argv[1]}`) {
  const server = new EfritMcpServer();
  
  // Handle graceful shutdown
  process.on('SIGINT', async () => {
    console.error('Received SIGINT, shutting down gracefully...');
    await server.stop();
    process.exit(0);
  });
  
  process.on('SIGTERM', async () => {
    console.error('Received SIGTERM, shutting down gracefully...');
    await server.stop();
    process.exit(0);
  });
  
  server.start().catch((error) => {
    console.error('Failed to start server:', error);
    process.exit(1);
  });
}

export { EfritMcpServer };
