/**
 * Efrit Queue System Client
 * Handles JSON file-based communication with Efrit instances
 * Implements Oracle security recommendations: path validation, atomic operations, proper permissions
 */

import * as fs from 'fs/promises';
import * as path from 'path';
import * as os from 'os';
import { randomUUID } from 'crypto';
import {
  EfritRequest,
  EfritResponse,
  InstanceConfig,
  EfritError,
  EFRIT_SCHEMA_VERSION,
  DEFAULT_TIMEOUT,
  DEFAULT_POLL_INTERVAL,
  MAX_REQUEST_SIZE,
  QUEUE_SUBDIRS
} from './types.js';

/**
 * Client for communicating with Efrit instances via file-based queues
 */
export class EfritClient {
  private instanceConfig: InstanceConfig;
  private instanceId: string;
  private allowedRoots: string[];

  constructor(instanceId: string, config: InstanceConfig, allowedRoots: string[] = []) {
    this.instanceId = instanceId;
    this.instanceConfig = config;
    this.allowedRoots = allowedRoots.length > 0 ? allowedRoots : [os.homedir()];
  }

  /**
   * Generate unique request ID using crypto.randomUUID() to prevent collisions
   */
  private generateRequestId(): string {
    return `efrit_${randomUUID()}`;
  }

  /**
   * Sanitize and expand tilde in file paths with security validation
   * Oracle recommendation: prevent path traversal, validate against whitelist
   */
  private expandUser(filepath: string): string {
    // Security validation: prevent path traversal
    if (filepath.includes('..') || filepath.includes('\0')) {
      throw this.createError('INVALID_PATH', `Invalid file path detected: ${filepath}`);
    }
    
    // Expand tilde using regex
    const expanded = filepath.replace(/^~(?=$|[/\\])/, os.homedir());
    const resolved = path.resolve(expanded);
    
    // Validate against allowed roots (Oracle security recommendation)
    const isAllowed = this.allowedRoots.some(root => {
      const expandedRoot = root.replace(/^~(?=$|[/\\])/, os.homedir());
      const resolvedRoot = path.resolve(expandedRoot);
      return resolved.startsWith(resolvedRoot);
    });
    
    if (!isAllowed) {
      throw this.createError('PATH_NOT_ALLOWED', 
        `Path ${resolved} is not within allowed roots: ${this.allowedRoots.join(', ')}`);
    }
    
    return resolved;
  }

  /**
   * Create structured error with context
   */
  private createError(code: string, message: string, details?: Record<string, unknown>): EfritError {
    const error = new Error(message) as EfritError;
    error.code = code;
    error.instance_id = this.instanceId;
    if (details) {
      error.details = details;
    }
    return error;
  }

  /**
   * Ensure queue directories exist with proper permissions (Oracle recommendation: 0o700)
   */
  private async ensureDirectories(): Promise<void> {
    const queueDir = this.expandUser(this.instanceConfig.queue_dir);
    
    // Create main queue directory
    await fs.mkdir(queueDir, { recursive: true, mode: 0o700 });
    
    // Create subdirectories
    for (const subdir of QUEUE_SUBDIRS) {
      const dir = path.join(queueDir, subdir);
      await fs.mkdir(dir, { recursive: true, mode: 0o700 });
    }
  }

  /**
   * Validate request size and content (Oracle security recommendation)
   */
  private validateRequest(request: EfritRequest): void {
    const requestJson = JSON.stringify(request);
    const requestSize = Buffer.byteLength(requestJson, 'utf8');
    
    if (requestSize > MAX_REQUEST_SIZE) {
      throw this.createError('REQUEST_TOO_LARGE', 
        `Request size ${requestSize} bytes exceeds limit ${MAX_REQUEST_SIZE} bytes`);
    }
    
    // Validate content doesn't contain null bytes (can cause issues in elisp)
    if (request.content.includes('\0')) {
      throw this.createError('INVALID_CONTENT', 'Request content cannot contain null bytes');
    }
  }

  /**
   * Write request file to queue using atomic write (tmp + rename) with fsync
   * Oracle recommendation: atomic operations with proper synchronization
   */
  private async writeRequest(request: EfritRequest): Promise<string> {
    await this.ensureDirectories();
    this.validateRequest(request);
    
    const queueDir = this.expandUser(this.instanceConfig.queue_dir);
    const requestsDir = path.join(queueDir, 'requests');
    const filename = `req_${request.id}.json`;
    const filepath = path.join(requestsDir, filename);
    const tempPath = `${filepath}.tmp`;
    
    try {
      // Atomic write: write to temp file then rename
      const requestJson = JSON.stringify(request, null, 2);
      const fileHandle = await fs.open(tempPath, 'w', 0o600); // Oracle recommendation: 0o600 permissions
      
      try {
        await fileHandle.writeFile(requestJson);
        await fileHandle.sync(); // Oracle recommendation: fsync for NFS
        await fileHandle.close();
      } catch (error) {
        await fileHandle.close();
        throw error;
      }
      
      await fs.rename(tempPath, filepath);
      return filepath;
    } catch (error) {
      // Clean up temp file on error
      await fs.unlink(tempPath).catch(() => {});
      throw this.createError('WRITE_FAILED', 
        `Failed to write request file: ${error instanceof Error ? error.message : String(error)}`,
        { filepath, tempPath });
    }
  }

  /**
   * Poll for response file with exponential backoff
   * Oracle recommendation: configurable polling with performance optimization
   */
  private async pollForResponse(requestId: string, timeoutMs: number): Promise<EfritResponse> {
    const queueDir = this.expandUser(this.instanceConfig.queue_dir);
    const responsesDir = path.join(queueDir, 'responses');
    const responseFile = path.join(responsesDir, `resp_${requestId}.json`);
    
    const startTime = Date.now();
    let pollInterval = DEFAULT_POLL_INTERVAL;
    const maxInterval = 2000; // Max 2 second intervals
    
    while (Date.now() - startTime < timeoutMs) {
      try {
        const content = await fs.readFile(responseFile, 'utf-8');
        let response: EfritResponse;
        
        try {
          response = JSON.parse(content);
        } catch (parseError) {
          throw this.createError('INVALID_RESPONSE', 
            `Invalid JSON in response file: ${parseError instanceof Error ? parseError.message : String(parseError)}`,
            { responseFile, content: content.substring(0, 200) });
        }
        
        // Validate response structure
        if (!response.id || !response.version || !response.status) {
          throw this.createError('MALFORMED_RESPONSE', 
            'Response missing required fields (id, version, status)',
            { response });
        }
        
        // Clean up response file after successful read
        await fs.unlink(responseFile).catch(() => {});
        
        return response;
      } catch (error) {
        // If file doesn't exist yet, continue polling
        if (error instanceof Error && 'code' in error && error.code === 'ENOENT') {
          await new Promise(resolve => setTimeout(resolve, pollInterval));

          // Exponential backoff (Oracle recommendation)
          pollInterval = Math.min(pollInterval * 2.0, maxInterval);
          continue;
        }
        
        // Other errors are thrown immediately
        throw error;
      }
    }
    
    throw this.createError('TIMEOUT', 
      `Request timed out after ${timeoutMs}ms`,
      { requestId, timeoutMs });
  }

  /**
   * Execute a request against the Efrit instance
   * Main public interface for the client
   */
  async execute(
    type: EfritRequest['type'],
    content: string,
    options: {
      timeout?: number;
      return_context?: boolean;
      max_response_size?: number;
    } = {}
  ): Promise<EfritResponse> {
    const requestId = this.generateRequestId();
    const timeout = (options.timeout ?? this.instanceConfig.timeout ?? DEFAULT_TIMEOUT) * 1000;
    
    const request: EfritRequest = {
      id: requestId,
      version: EFRIT_SCHEMA_VERSION,
      type,
      content,
      instance_id: this.instanceId,
      options: {
        timeout: Math.floor(timeout / 1000),
        return_context: options.return_context ?? false,
        ...(options.max_response_size && { max_response_size: options.max_response_size })
      },
      timestamp: new Date().toISOString(),
      metadata: {
        client_id: 'efrit-mcp-server',
        source: 'mcp'
      }
    };
    
    let requestFile: string | undefined;
    try {
      requestFile = await this.writeRequest(request);
      const response = await this.pollForResponse(requestId, timeout);

      // Validate response ID matches request
      if (response.id !== requestId) {
        throw this.createError('ID_MISMATCH',
          `Response ID ${response.id} doesn't match request ID ${requestId}`);
      }

      // Clean up request file after successful response
      await fs.unlink(requestFile).catch(() => {});

      return response;
    } catch (error) {
      // Clean up request file on error
      if (requestFile) {
        await fs.unlink(requestFile).catch(() => {});
      }

      // Add request ID to error context
      if (error instanceof Error && 'code' in error) {
        (error as EfritError).request_id = requestId;
      }
      throw error;
    }
  }

  /**
   * Get queue statistics for this instance
   */
  async getQueueStats(): Promise<{
    pending: number;
    processing: number;
    responses: number;
  }> {
    const queueDir = this.expandUser(this.instanceConfig.queue_dir);
    
    try {
      const [requests, processing, responses] = await Promise.all([
        fs.readdir(path.join(queueDir, 'requests')),
        fs.readdir(path.join(queueDir, 'processing')),
        fs.readdir(path.join(queueDir, 'responses'))
      ]);
      
      return {
        pending: requests.filter(f => f.endsWith('.json')).length,
        processing: processing.filter(f => f.endsWith('.json')).length,
        responses: responses.filter(f => f.endsWith('.json')).length
      };
    } catch (error) {
      throw this.createError('STATS_FAILED',
        `Failed to get queue statistics: ${error instanceof Error ? error.message : String(error)}`);
    }
  }

  /**
   * Check if instance queue directory is accessible
   */
  async healthCheck(): Promise<boolean> {
    try {
      const queueDir = this.expandUser(this.instanceConfig.queue_dir);
      await fs.access(queueDir, fs.constants.R_OK | fs.constants.W_OK);
      return true;
    } catch {
      return false;
    }
  }

  /**
   * Clean up old files from queue directories
   * Oracle recommendation: prevent directory explosion
   */
  async cleanup(maxAgeHours: number = 24): Promise<void> {
    const queueDir = this.expandUser(this.instanceConfig.queue_dir);
    const maxAge = Date.now() - (maxAgeHours * 60 * 60 * 1000);
    
    const archiveDir = path.join(queueDir, 'archive');
    await fs.mkdir(archiveDir, { recursive: true, mode: 0o700 });
    
    // Clean up old response files
    const responsesDir = path.join(queueDir, 'responses');
    try {
      const responseFiles = await fs.readdir(responsesDir);
      for (const file of responseFiles) {
        if (!file.endsWith('.json')) continue;
        
        const filepath = path.join(responsesDir, file);
        const stats = await fs.stat(filepath);
        
        if (stats.mtimeMs < maxAge) {
          const archivePath = path.join(archiveDir, file);
          await fs.rename(filepath, archivePath);
        }
      }
    } catch (error) {
      // Log error but don't throw - cleanup is best effort
      console.warn(`Cleanup failed: ${error instanceof Error ? error.message : String(error)}`);
    }
  }
}
