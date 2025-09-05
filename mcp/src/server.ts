#!/usr/bin/env node

/**
 * Efrit MCP Server
 * Enables Claude and other AI models to interact with Efrit instances
 */

import { Server } from '@modelcontextprotocol/sdk/server/index.js';
import { StdioServerTransport } from '@modelcontextprotocol/sdk/server/stdio.js';
import { 
  CallToolRequestSchema,
  ListToolsRequestSchema,
} from '@modelcontextprotocol/sdk/types.js';
import { EfritClient } from './efrit-client.js';
import { ServerConfig, InstanceConfig, EfritInstance } from './types.js';
import * as fs from 'fs/promises';
import * as path from 'path';

// Simple semaphore for concurrency control
class Semaphore {
  private permits: number;
  private queue: (() => void)[] = [];
  
  constructor(permits: number) {
    this.permits = permits;
  }
  
  async acquire(): Promise<void> {
    if (this.permits > 0) {
      this.permits--;
      return;
    }
    
    return new Promise<void>((resolve) => {
      this.queue.push(resolve);
    });
  }
  
  release(): void {
    if (this.queue.length > 0) {
      const resolve = this.queue.shift()!;
      resolve();
    } else {
      this.permits++;
    }
  }
}

class EfritMCPServer {
  private server: Server;
  private clients: Map<string, EfritClient> = new Map();
  private config: ServerConfig;
  private concurrencySemaphore: Semaphore;
  private isShuttingDown: boolean = false;

  constructor() {
    this.server = new Server(
      {
        name: 'efrit',
        version: '0.3.0',
      },
      {
        capabilities: {
          tools: {},
        },
      }
    );

    // Load default configuration
    this.config = {
      instances: {
        default: {
          queue_dir: '~/.emacs.d/.efrit/queues',
          daemon_name: 'efrit-default',
          auto_start: true,
          timeout: 30
        }
      },
      polling_interval_ms: 500,
      max_concurrent_requests: 20,
      request_timeout_seconds: 60,
      cleanup_interval_minutes: 10
    };

    // Initialize concurrency semaphore
    this.concurrencySemaphore = new Semaphore(this.config.max_concurrent_requests);

    this.setupHandlers();
  }

  private setupHandlers() {
    // List available tools
    this.server.setRequestHandler(ListToolsRequestSchema, async () => {
      return {
        tools: [
          {
            name: 'efrit_execute',
            description: 'Execute commands, code, or chat with an Efrit instance',
            inputSchema: {
              type: 'object',
              properties: {
                instance_id: {
                  type: 'string',
                  description: 'Target Efrit instance identifier',
                  default: 'default'
                },
                type: {
                  type: 'string',
                  enum: ['command', 'eval', 'chat'],
                  description: 'Type of request to execute'
                },
                content: {
                  type: 'string',
                  description: 'The command, code, or message content'
                },
                timeout: {
                  type: 'number',
                  description: 'Execution timeout in seconds',
                  default: 30
                },
                return_context: {
                  type: 'boolean',
                  description: 'Include Emacs context in response',
                  default: false
                }
              },
              required: ['type', 'content']
            }
          },
          {
            name: 'efrit_list_instances',
            description: 'List available Efrit instances and their status',
            inputSchema: {
              type: 'object',
              properties: {},
              additionalProperties: false
            }
          },
          {
            name: 'efrit_get_queue_stats',
            description: 'Get queue processing statistics for an instance',
            inputSchema: {
              type: 'object',
              properties: {
                instance_id: {
                  type: 'string',
                  description: 'Target Efrit instance identifier',
                  default: 'default'
                }
              },
              required: ['instance_id']
            }
          },
          {
            name: 'efrit_start_instance',
            description: 'Start an Efrit instance with specified configuration',
            inputSchema: {
              type: 'object',
              properties: {
                instance_id: {
                  type: 'string',
                  description: 'Instance identifier'
                },
                queue_dir: {
                  type: 'string',
                  description: 'Custom queue directory path'
                },
                daemon_name: {
                  type: 'string',
                  description: 'Emacs daemon name'
                },
                workspace_dir: {
                  type: 'string',
                  description: 'Workspace directory path'
                }
              },
              required: ['instance_id']
            }
          },
          {
            name: 'efrit_stop_instance',
            description: 'Stop a running Efrit instance',
            inputSchema: {
              type: 'object',
              properties: {
                instance_id: {
                  type: 'string',
                  description: 'Target instance to stop'
                }
              },
              required: ['instance_id']
            }
          }
        ]
      };
    });

    // Handle tool calls with proper error handling
    this.server.setRequestHandler(CallToolRequestSchema, async (request) => {
      const { name, arguments: args } = request.params;

      try {
        switch (name) {
          case 'efrit_execute':
            return await this.handleExecute(args);
          case 'efrit_list_instances':
            return await this.handleListInstances();
          case 'efrit_get_queue_stats':
            return await this.handleGetQueueStats(args);
          case 'efrit_start_instance':
            return await this.handleStartInstance(args);
          case 'efrit_stop_instance':
            return await this.handleStopInstance(args);
          default:
            throw new Error(`Unknown tool: ${name}`);
        }
      } catch (error) {
        // Return error as content instead of bubbling to MCP SDK
        return {
          content: [{
            type: 'text',
            text: `Error: ${error instanceof Error ? error.message : String(error)}`
          }],
          isError: true
        };
      }
    });
  }

  /**
   * Sanitize instance_id and daemon_name to prevent path traversal
   */
  private sanitizeId(id: string): void {
    if (!id || typeof id !== 'string' || id.includes('..') || id.includes('/') || id.includes('\\') || id.includes('\0')) {
      throw new Error('Invalid identifier - contains illegal characters');
    }
  }

  private async getClient(instanceId: string = 'default'): Promise<EfritClient> {
    this.sanitizeId(instanceId);
    
    if (!this.clients.has(instanceId)) {
      const config = this.config.instances[instanceId];
      if (!config) {
        throw new Error(`Unknown instance: ${instanceId}`);
      }
      this.clients.set(instanceId, new EfritClient(instanceId, config));
    }
    return this.clients.get(instanceId)!;
  }

  private async handleExecute(args: any) {
    if (this.isShuttingDown) {
      throw new Error('Server is shutting down');
    }

    const { instance_id = 'default', type, content, timeout, return_context } = args;
    
    if (!['command', 'eval', 'chat'].includes(type)) {
      throw new Error('Type must be one of: command, eval, chat');
    }

    // Acquire semaphore for concurrency control
    await this.concurrencySemaphore.acquire();
    
    try {
      const client = await this.getClient(instance_id);
      const response = await client.execute(type, content, { timeout, return_context });

      // Return structured data, not double-encoded JSON
      return {
        content: [{
          type: 'text',
          text: response.status === 'success' ? (response.result || 'Success') : (response.error || 'Unknown error')
        }],
        isError: response.status !== 'success',
        _metadata: {
          status: response.status,
          execution_time: response.execution_time,
          context: response.context
        }
      };
    } finally {
      this.concurrencySemaphore.release();
    }
  }

  private async handleListInstances() {
    const instances: EfritInstance[] = [];
    
    // Use Promise.allSettled for parallel ping operations
    const instanceEntries = Object.entries(this.config.instances);
    const results = await Promise.allSettled(instanceEntries.map(async ([id, config]) => {
      const client = await this.getClient(id);
      const isActive = await client.ping();
      const stats = await client.getQueueStats();
      
      return {
        id,
        status: isActive ? 'active' : 'inactive',
        queue_dir: config.queue_dir,
        daemon_name: config.daemon_name,
        workspace_dir: config.workspace_dir,
        last_activity: new Date().toISOString()
      } as EfritInstance;
    }));
    
    // Process results
    for (let i = 0; i < results.length; i++) {
      const [id, config] = instanceEntries[i];
      const result = results[i];
      
      if (result.status === 'fulfilled') {
        instances.push(result.value);
      } else {
        instances.push({
          id,
          status: 'error',
          queue_dir: config.queue_dir,
          daemon_name: config.daemon_name,
          workspace_dir: config.workspace_dir
        });
      }
    }

    return {
      content: [{
        type: 'text', 
        text: instances.map(i => `${i.id}: ${i.status} (${i.daemon_name || 'no daemon'})`).join('\n')
      }],
      _metadata: { instances }
    };
  }

  private async handleGetQueueStats(args: any) {
    const { instance_id } = args;
    const client = await this.getClient(instance_id);
    const stats = await client.getQueueStats();

    return {
      content: [{
        type: 'text',
        text: `Queue Stats for ${instance_id}:\nPending: ${stats.pending_requests}\nProcessing: ${stats.currently_processing}\nResponses: ${stats.pending_responses}`
      }],
      _metadata: { stats }
    };
  }

  private async handleStartInstance(args: any) {
    const { instance_id, queue_dir, daemon_name, workspace_dir } = args;
    
    // Sanitize all user inputs
    this.sanitizeId(instance_id);
    if (daemon_name) this.sanitizeId(daemon_name);
    
    // Create new instance configuration
    const config: InstanceConfig = {
      queue_dir: queue_dir || `~/.emacs.d/.efrit-${instance_id}/queues`,
      daemon_name: daemon_name || `efrit-${instance_id}`,
      workspace_dir: workspace_dir || `~/.emacs.d/.efrit-${instance_id}/workspace`,
      auto_start: false,
      timeout: 30
    };

    // Add to configuration
    this.config.instances[instance_id] = config;
    
    // Start the Emacs daemon
    const { spawn } = require('child_process');
    const startupFile = path.join(__dirname, '../../lisp/efrit-autonomous-startup.el');
    
    try {
      const daemon = spawn('emacs', [
        '--no-init-file',
        `--daemon=${config.daemon_name}`,
        '--load', startupFile,
        '--eval', `(setq efrit-data-directory "${path.dirname(config.queue_dir)}")`
      ], { 
        detached: true,
        stdio: 'ignore'
      });

      daemon.unref();
      
      // Wait a moment for startup
      await new Promise(resolve => setTimeout(resolve, 2000));
      
      // Verify instance is running
      const client = new EfritClient(instance_id, config);
      const isActive = await client.ping();
      
      if (isActive) {
        this.clients.set(instance_id, client);
        return {
          content: [{
            type: 'text',
            text: `Instance ${instance_id} started successfully`
          }],
          _metadata: {
            status: 'started',
            instance_id,
            queue_dir: config.queue_dir,
            daemon_name: config.daemon_name
          }
        };
      } else {
        throw new Error('Instance failed to start or respond');
      }
    } catch (error) {
      return {
        content: [{
          type: 'text',
          text: `Failed to start instance ${instance_id}: ${error instanceof Error ? error.message : String(error)}`
        }],
        isError: true
      };
    }
  }

  private async handleStopInstance(args: any) {
    const { instance_id } = args;
    
    // Sanitize input
    this.sanitizeId(instance_id);
    
    if (instance_id === 'default') {
      throw new Error('Cannot stop default instance');
    }

    const config = this.config.instances[instance_id];
    if (!config) {
      throw new Error(`Unknown instance: ${instance_id}`);
    }

    try {
      // Send shutdown command to Emacs daemon
      const { spawn } = require('child_process');
      await new Promise((resolve, reject) => {
        const shutdown = spawn('emacsclient', [
          `--socket-name=${config.daemon_name}`,
          '--eval', '(kill-emacs)'
        ]);
        
        shutdown.on('close', (code: number | null) => {
          if (code === 0) resolve(null);
          else reject(new Error(`Shutdown failed with code ${code}`));
        });
      });

      // Remove from active clients
      this.clients.delete(instance_id);
      delete this.config.instances[instance_id];

      return {
        content: [{
          type: 'text',
          text: `Instance ${instance_id} stopped successfully`
        }],
        _metadata: { status: 'stopped', instance_id }
      };
    } catch (error) {
      return {
        content: [{
          type: 'text',
          text: `Failed to stop instance ${instance_id}: ${error instanceof Error ? error.message : String(error)}`
        }],
        isError: true
      };
    }
  }

  private setupGracefulShutdown() {
    const shutdown = async (signal: string) => {
      console.error(`Received ${signal}, shutting down gracefully...`);
      this.isShuttingDown = true;
      
      // Close all clients
      for (const client of this.clients.values()) {
        // Could add client.close() if implemented
      }
      
      console.error('Shutdown complete');
      process.exit(0);
    };

    process.on('SIGINT', () => shutdown('SIGINT'));
    process.on('SIGTERM', () => shutdown('SIGTERM'));
  }

  async start() {
    this.setupGracefulShutdown();
    
    const transport = new StdioServerTransport();
    await this.server.connect(transport);
    console.error('Efrit MCP Server running');
  }
}

// Start server if called directly
if (require.main === module) {
  const server = new EfritMCPServer();
  server.start().catch((error) => {
    console.error('Failed to start server:', error);
    process.exit(1);
  });
}

export { EfritMCPServer };
