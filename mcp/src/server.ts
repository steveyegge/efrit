#!/usr/bin/env node
/**
 * Efrit MCP Server
 * Implements Model Context Protocol server for Efrit AI coding assistant
 *
 * Architecture: Pure Executor - delegates all cognitive computation to Claude
 * This server provides tools for Claude to execute commands in Emacs instances
 */

import { McpServer } from "@modelcontextprotocol/sdk/server/mcp.js";
import { StdioServerTransport } from "@modelcontextprotocol/sdk/server/stdio.js";
import { z } from "zod";
import * as fs from 'fs/promises';
import { mkdirSync } from 'fs';
import * as path from 'path';
import { fileURLToPath } from 'url';
import pino from 'pino';
import pLimit from 'p-limit';
import { EfritClient } from './efrit-client.js';
import {
  McpServerConfig,
  EfritExecuteParams,
  EfritListInstancesParams,
  EfritGetQueueStatsParams,
  InstanceStatus,
  QueueStats,
  EfritError
} from './types.js';

const __filename = fileURLToPath(import.meta.url);
const __dirname = path.dirname(__filename);

/**
 * Create pino logger with appropriate configuration
 * Logs to file to avoid polluting stdio transport used by MCP protocol
 */
function createLogger(level: 'debug' | 'info' | 'warn' | 'error' = 'info', logFile?: string): pino.Logger {
  // Default log file location if not specified
  const defaultLogFile = path.join(process.env['HOME'] || '~', '.efrit', 'logs', 'mcp-server.log');
  const targetLogFile = logFile || defaultLogFile;

  // Ensure log directory exists
  const logDir = path.dirname(targetLogFile);
  try {
    mkdirSync(logDir, { recursive: true });
  } catch (error) {
    // Fallback to stderr if we can't create log directory
    // (This shouldn't happen in normal usage, but better than crashing)
    console.error(`Warning: Could not create log directory ${logDir}, logging to stderr`);
    return pino({ level });
  }

  return pino({
    level,
    transport: {
      target: 'pino-pretty',
      options: {
        destination: targetLogFile,
        colorize: false,  // No colors in log files
        translateTime: 'yyyy-mm-dd HH:MM:ss',
        ignore: 'pid,hostname',
        mkdir: true  // Create directory if needed
      }
    }
  });
}

/**
 * Main Efrit MCP Server class
 */
export class EfritMcpServer {
  private config!: McpServerConfig;
  private clients: Map<string, EfritClient> = new Map();
  private logger: pino.Logger;
  private server: McpServer;
  private concurrencyLimit!: ReturnType<typeof pLimit>;

  constructor() {
    this.logger = createLogger('info');
    this.server = new McpServer({
      name: "efrit-mcp-server",
      version: "1.0.0"
    });
  }

  /**
   * Load configuration from file with environment variable overrides
   */
  private async loadConfig(configPath?: string): Promise<McpServerConfig> {
    const defaultPath = path.join(__dirname, '../config/instances.json');
    const finalPath = configPath || process.env['EFRIT_MCP_CONFIG'] || defaultPath;

    try {
      const configContent = await fs.readFile(finalPath, 'utf-8');
      const config = JSON.parse(configContent) as McpServerConfig;

      // Environment variable overrides
      if (process.env['EFRIT_LOG_LEVEL']) {
        config.log_level = process.env['EFRIT_LOG_LEVEL'] as 'debug' | 'info' | 'warn' | 'error';
      }
      if (process.env['EFRIT_DEFAULT_INSTANCE']) {
        config.default_instance = process.env['EFRIT_DEFAULT_INSTANCE'];
      }

      // Validate configuration
      this.validateConfig(config);

      return config;
    } catch (error) {
      throw new Error(`Failed to load configuration from ${finalPath}: ${error instanceof Error ? error.message : String(error)}`);
    }
  }

  /**
   * Validate configuration structure
   */
  private validateConfig(config: McpServerConfig): void {
    if (!config.instances || Object.keys(config.instances).length === 0) {
      throw new Error('Configuration must define at least one instance');
    }

    if (!config.default_instance || !config.instances[config.default_instance]) {
      throw new Error(`Default instance "${config.default_instance}" not found in instances configuration`);
    }

    for (const [id, instanceConfig] of Object.entries(config.instances)) {
      if (!instanceConfig.queue_dir) {
        throw new Error(`Instance "${id}" missing required queue_dir`);
      }
      if (!instanceConfig.timeout || instanceConfig.timeout <= 0) {
        throw new Error(`Instance "${id}" missing valid timeout`);
      }
    }
  }

  /**
   * Initialize Efrit clients for all configured instances
   */
  private async initializeClients(): Promise<void> {
    const allowedRoots = this.config.security?.allowed_queue_roots || ['~/.emacs.d', '~/'];

    for (const [instanceId, instanceConfig] of Object.entries(this.config.instances)) {
      const client = new EfritClient(instanceId, instanceConfig, allowedRoots);

      // Health check
      const healthy = await client.healthCheck();
      if (!healthy) {
        this.logger.warn(`Instance "${instanceId}" queue directory not accessible, but will be created on first request`);
      }

      this.clients.set(instanceId, client);
      this.logger.info(`Initialized client for instance: ${instanceId}`);
    }
  }

  /**
   * Get Efrit client for specified instance
   */
  private getClient(instanceId?: string): EfritClient {
    const targetId = instanceId || this.config.default_instance;
    const client = this.clients.get(targetId);

    if (!client) {
      throw new Error(`Unknown instance: ${targetId}. Available instances: ${Array.from(this.clients.keys()).join(', ')}`);
    }

    return client;
  }

  /**
   * Register MCP tools
   */
  private registerTools(): void {
    // Tool 1: efrit_execute - Main execution tool
    this.server.registerTool(
      "efrit_execute",
      {
        title: "Execute Efrit Command",
        description: "Execute a command, elisp code, or chat message in an Efrit (Emacs) instance. Returns execution results. Types: 'command' (efrit-do), 'eval' (elisp evaluation), 'chat' (efrit-chat). AI-friendly: accepts both string and number formats for timeout.",
        inputSchema: {
          type: z.enum(['command', 'eval', 'chat']).describe('Type of execution: command, eval, or chat'),
          content: z.string().describe('Command, elisp code, or chat message to execute'),
          instance_id: z.string().optional().describe('Target Efrit instance ID (uses default if not specified)'),
          return_context: z.coerce.boolean().optional().describe('Whether to return Emacs context (buffers, modes, etc.)'),
          timeout: z.coerce.number().positive().finite().optional().describe('Timeout override in seconds (accepts number or string like "30")')
        }
      },
      async (params: EfritExecuteParams) => {
        try {
          this.logger.info(`Executing ${params.type} on instance ${params.instance_id || 'default'}`);

          // Use concurrency limiter to prevent overload
          const response = await this.concurrencyLimit(async () => {
            const client = this.getClient(params.instance_id);
            const options: { timeout?: number; return_context?: boolean } = {};
            if (params.timeout !== undefined) options.timeout = params.timeout;
            if (params.return_context !== undefined) options.return_context = params.return_context;
            return await client.execute(params.type, params.content, options);
          });

          // Format response for MCP
          if (response.status === 'success') {
            return {
              content: [{
                type: "text" as const,
                text: JSON.stringify({
                  status: 'success',
                  result: response.result,
                  context: response.context,
                  execution_time: response.execution_time,
                  instance_id: response.instance_id
                }, null, 2)
              }]
            };
          } else {
            return {
              content: [{
                type: "text" as const,
                text: JSON.stringify({
                  status: response.status,
                  error: response.error,
                  error_code: response.error_code,
                  instance_id: response.instance_id
                }, null, 2)
              }],
              isError: true
            };
          }
        } catch (error) {
          this.logger.error(`Execution failed: ${error instanceof Error ? error.message : String(error)}`);
          return {
            content: [{
              type: "text" as const,
              text: JSON.stringify({
                status: 'error',
                error: error instanceof Error ? error.message : String(error),
                error_code: (error as EfritError).code || 'UNKNOWN_ERROR'
              }, null, 2)
            }],
            isError: true
          };
        }
      }
    );

    // Tool 2: efrit_list_instances - List available instances
    this.server.registerTool(
      "efrit_list_instances",
      {
        title: "List Efrit Instances",
        description: "List all configured Efrit instances with their status and optional queue statistics.",
        inputSchema: {
          include_stats: z.coerce.boolean().optional().describe('Include queue statistics for each instance')
        }
      },
      async (params: EfritListInstancesParams) => {
        try {
          this.logger.info('Listing instances');

          const instances: InstanceStatus[] = [];

          for (const [instanceId, instanceConfig] of this.clients.entries()) {
            let queueStats: QueueStats | undefined;

            if (params.include_stats) {
              const stats = await instanceConfig.getQueueStats();
              queueStats = {
                instance_id: instanceId,
                pending_requests: stats.pending,
                processing_requests: stats.processing,
                completed_last_hour: null, // Not implemented - requires historical tracking
                failed_last_hour: null, // Not implemented - requires historical tracking
                avg_processing_time: null, // Not implemented - requires historical tracking
                health: stats.pending + stats.processing > 100 ? 'degraded' : 'healthy',
                last_activity: null // Not implemented - requires tracking last queue activity
              };
            } else {
              queueStats = {
                instance_id: instanceId,
                pending_requests: 0,
                processing_requests: 0,
                completed_last_hour: null,
                failed_last_hour: null,
                avg_processing_time: null,
                health: 'healthy',
                last_activity: null
              };
            }

            const healthy = await instanceConfig.healthCheck();

            const instanceCfg = this.config.instances[instanceId];
            if (instanceCfg) {
              instances.push({
                instance_id: instanceId,
                running: healthy,
                queue_stats: queueStats,
                config: instanceCfg
              });
            }
          }

          return {
            content: [{
              type: "text" as const,
              text: JSON.stringify({
                default_instance: this.config.default_instance,
                instances
              }, null, 2)
            }]
          };
        } catch (error) {
          this.logger.error(`Failed to list instances: ${error instanceof Error ? error.message : String(error)}`);
          return {
            content: [{
              type: "text" as const,
              text: JSON.stringify({
                error: error instanceof Error ? error.message : String(error)
              }, null, 2)
            }],
            isError: true
          };
        }
      }
    );

    // Tool 3: efrit_get_queue_stats - Get queue statistics
    this.server.registerTool(
      "efrit_get_queue_stats",
      {
        title: "Get Queue Statistics",
        description: "Get detailed queue statistics for a specific Efrit instance.",
        inputSchema: {
          instance_id: z.string().optional().describe('Target instance ID (uses default if not specified)')
        }
      },
      async (params: EfritGetQueueStatsParams) => {
        try {
          const instanceId = params.instance_id || this.config.default_instance;
          this.logger.info(`Getting queue stats for instance: ${instanceId}`);

          const client = this.getClient(params.instance_id);
          const stats = await client.getQueueStats();

          return {
            content: [{
              type: "text" as const,
              text: JSON.stringify({
                instance_id: instanceId,
                stats,
                timestamp: new Date().toISOString()
              }, null, 2)
            }]
          };
        } catch (error) {
          this.logger.error(`Failed to get queue stats: ${error instanceof Error ? error.message : String(error)}`);
          return {
            content: [{
              type: "text" as const,
              text: JSON.stringify({
                error: error instanceof Error ? error.message : String(error)
              }, null, 2)
            }],
            isError: true
          };
        }
      }
    );

    this.logger.info('Registered 3 MCP tools: efrit_execute, efrit_list_instances, efrit_get_queue_stats');
  }

  /**
   * Initialize and start the server
   */
  async start(configPath?: string): Promise<void> {
    try {
      // Load configuration
      this.logger.info('Loading configuration...');
      this.config = await this.loadConfig(configPath);

      // Update logger with config values (level and file path) and initialize concurrency limiter
      this.logger = createLogger(this.config.log_level || 'info', this.config.log_file);
      this.concurrencyLimit = pLimit(this.config.max_concurrent_requests || 10);

      this.logger.info(`Configuration loaded: ${Object.keys(this.config.instances).length} instance(s) configured`);

      // Initialize clients
      this.logger.info('Initializing Efrit clients...');
      await this.initializeClients();

      // Register tools
      this.logger.info('Registering MCP tools...');
      this.registerTools();

      // Connect to stdio transport
      this.logger.info('Starting MCP server on stdio transport...');
      const transport = new StdioServerTransport();
      await this.server.connect(transport);

      this.logger.info('Efrit MCP Server is running');
    } catch (error) {
      this.logger.error(`Failed to start server: ${error instanceof Error ? error.message : String(error)}`);
      process.exit(1);
    }
  }

  /**
   * Graceful shutdown
   */
  async shutdown(): Promise<void> {
    this.logger.info('Shutting down Efrit MCP Server...');

    // Clean up clients
    for (const [instanceId, client] of this.clients.entries()) {
      try {
        await client.cleanup();
        this.logger.debug(`Cleaned up client for instance: ${instanceId}`);
      } catch (error) {
        this.logger.warn(`Failed to cleanup instance ${instanceId}: ${error instanceof Error ? error.message : String(error)}`);
      }
    }

    this.logger.info('Shutdown complete');
    process.exit(0);
  }
}

/**
 * Main entry point
 */
async function main() {
  const server = new EfritMcpServer();

  // Handle graceful shutdown
  process.on('SIGINT', () => server.shutdown());
  process.on('SIGTERM', () => server.shutdown());

  // Start the server
  await server.start();
}

// Run if this is the main module
if (import.meta.url === `file://${process.argv[1]}`) {
  main().catch(error => {
    console.error('Fatal error:', error);
    process.exit(1);
  });
}

export default EfritMcpServer;
