/**
 * Efrit Queue System Client
 * Handles JSON file-based communication with Efrit instances
 */

import * as fs from 'fs/promises';
import * as path from 'path';
import * as os from 'os';
import * as crypto from 'crypto';
import { EfritRequest, EfritResponse, InstanceConfig } from './types';

export class EfritClient {
  private instanceConfig: InstanceConfig;
  private instanceId: string;

  constructor(instanceId: string, config: InstanceConfig) {
    this.instanceId = instanceId;
    this.instanceConfig = config;
  }

  /**
   * Generate unique request ID using crypto.randomUUID() to prevent collisions
   */
  private generateRequestId(): string {
    return `efrit_${crypto.randomUUID()}`;
  }

  /**
   * Sanitize and expand tilde in file paths with proper regex
   */
  private expandUser(filepath: string): string {
    // Sanitize path to prevent path traversal
    if (filepath.includes('..') || filepath.includes('\0')) {
      throw new Error('Invalid file path detected');
    }
    
    // Use regex for proper tilde expansion
    return filepath.replace(/^~(?=$|[/\\])/, os.homedir());
  }

  /**
   * Ensure queue directories exist
   */
  private async ensureDirectories(): Promise<void> {
    const queueDir = path.resolve(this.expandUser(this.instanceConfig.queue_dir));
    const subdirs = ['requests', 'responses', 'processing', 'archive'];
    
    for (const subdir of subdirs) {
      const dir = path.join(queueDir, subdir);
      await fs.mkdir(dir, { recursive: true });
    }
  }

  /**
   * Write request file to queue using atomic write (tmp + rename)
   */
  private async writeRequest(request: EfritRequest): Promise<string> {
    await this.ensureDirectories();
    
    const queueDir = path.resolve(this.expandUser(this.instanceConfig.queue_dir));
    const requestsDir = path.join(queueDir, 'requests');
    const filename = `req_${request.id}.json`;
    const filepath = path.join(requestsDir, filename);
    const tempPath = `${filepath}.tmp`;
    
    try {
      // Atomic write: write to temp file then rename
      await fs.writeFile(tempPath, JSON.stringify(request, null, 2));
      await fs.rename(tempPath, filepath);
      return filepath;
    } catch (error) {
      // Clean up temp file on error
      await fs.unlink(tempPath).catch(() => {});
      throw error;
    }
  }

  /**
   * Poll for response file
   */
  private async pollForResponse(requestId: string, timeoutMs: number): Promise<EfritResponse> {
    const queueDir = path.resolve(this.expandUser(this.instanceConfig.queue_dir));
    const responsesDir = path.join(queueDir, 'responses');
    const responseFile = path.join(responsesDir, `resp_${requestId}.json`);
    
    const startTime = Date.now();
    const pollInterval = 500; // 500ms polling
    
    while (Date.now() - startTime < timeoutMs) {
      try {
        const content = await fs.readFile(responseFile, 'utf-8');
        const response: EfritResponse = JSON.parse(content);
        
        // Clean up response file
        await fs.unlink(responseFile).catch(() => {});
        
        return response;
      } catch (error) {
        // File doesn't exist yet, continue polling
        await new Promise(resolve => setTimeout(resolve, pollInterval));
      }
    }
    
    throw new Error(`Request ${requestId} timed out after ${timeoutMs}ms`);
  }

  /**
   * Execute a request and wait for response
   */
  async execute(
    type: 'command' | 'eval' | 'chat',
    content: string,
    options?: { timeout?: number; return_context?: boolean }
  ): Promise<EfritResponse> {
    const requestId = this.generateRequestId();
    const timeout = (options?.timeout || this.instanceConfig.timeout || 30) * 1000;
    
    const request: EfritRequest = {
      id: requestId,
      type,
      content,
      options
    };

    await this.writeRequest(request);
    return await this.pollForResponse(requestId, timeout);
  }

  /**
   * Check if instance is responsive
   */
  async ping(): Promise<boolean> {
    try {
      const response = await this.execute('eval', '(+ 1 1)', { timeout: 5 });
      return response.status === 'success' && response.result === '2';
    } catch {
      return false;
    }
  }

  /**
   * Get queue statistics by checking directory contents
   */
  async getQueueStats() {
    try {
      const queueDir = path.resolve(this.expandUser(this.instanceConfig.queue_dir));
      
      const [requests, processing, responses] = await Promise.all([
        fs.readdir(path.join(queueDir, 'requests')).catch(() => []),
        fs.readdir(path.join(queueDir, 'processing')).catch(() => []),
        fs.readdir(path.join(queueDir, 'responses')).catch(() => [])
      ]);

      return {
        pending_requests: requests.filter(f => f.endsWith('.json')).length,
        currently_processing: processing.filter(f => f.endsWith('.json')).length,
        pending_responses: responses.filter(f => f.endsWith('.json')).length
      };
    } catch (error) {
      throw new Error(`Failed to get queue stats: ${error}`);
    }
  }
}


