/**
 * Simple tests to verify EfritClient core functionality
 */

import * as path from 'path';
import { EfritClient } from '../src/efrit-client';
import { InstanceConfig, EFRIT_SCHEMA_VERSION } from '../src/types';
import {
  createTempDir,
  cleanupTempDir,
  createQueueStructure,
  writeMockResponse
} from './setup';

describe('EfritClient - Simple Tests', () => {
  let tempDir: string;
  let queueStructure: Awaited<ReturnType<typeof createQueueStructure>>;
  let client: EfritClient;
  let config: InstanceConfig;

  beforeEach(async () => {
    tempDir = await createTempDir();
    queueStructure = await createQueueStructure(tempDir);
    
    config = {
      queue_dir: queueStructure.queueDir,
      workspace_dir: path.join(tempDir, 'workspace'),
      timeout: 1, // Short timeout for tests
      daemon_name: 'efrit-test'
    };
    
    client = new EfritClient('test-instance', config, [tempDir]);
  });

  afterEach(async () => {
    await cleanupTempDir(tempDir);
  });

  test('should create client successfully', () => {
    expect(client).toBeInstanceOf(EfritClient);
  });

  test('should reject invalid paths', async () => {
    const maliciousConfig = {
      ...config,
      queue_dir: '../../etc/passwd'
    };
    
    const maliciousClient = new EfritClient('malicious', maliciousConfig, [tempDir]);
    
    await expect(maliciousClient.execute('command', 'test'))
      .rejects.toThrow('Invalid file path detected');
  });

  test('should write request file correctly', async () => {
    // Start execution (will timeout but that's okay)
    const executePromise = client.execute('command', 'test');
    
    // Wait a bit for file to be written
    await new Promise(resolve => setTimeout(resolve, 100));
    
    // Check request file exists
    const files = await require('fs/promises').readdir(queueStructure.requestsDir);
    const requestFiles = files.filter((f: string) => f.startsWith('req_') && f.endsWith('.json'));
    expect(requestFiles.length).toBeGreaterThan(0);
    
    // Read and validate request
    const requestId = requestFiles[0].replace('req_', '').replace('.json', '');
    const content = await require('fs/promises').readFile(
      path.join(queueStructure.requestsDir, requestFiles[0]), 
      'utf-8'
    );
    const request = JSON.parse(content);
    
    expect(request).toMatchObject({
      id: requestId,
      version: EFRIT_SCHEMA_VERSION,
      type: 'command',
      content: 'test',
      instance_id: 'test-instance'
    });
    
    // Clean up the promise (it will timeout)
    try {
      await executePromise;
    } catch (error) {
      // Expected timeout
    }
  });

  test('should handle response correctly when provided', async () => {
    const executePromise = client.execute('eval', '(+ 1 1)');
    
    // Wait for request file
    await new Promise(resolve => setTimeout(resolve, 100));
    
    // Get request ID
    const files = await require('fs/promises').readdir(queueStructure.requestsDir);
    const requestId = files[0].replace('req_', '').replace('.json', '');
    
    // Create response immediately
    await writeMockResponse(queueStructure.responsesDir, requestId, {
      id: requestId,
      version: EFRIT_SCHEMA_VERSION,
      status: 'success',
      result: 2,
      timestamp: new Date().toISOString()
    });
    
    const response = await executePromise;
    expect(response.status).toBe('success');
    expect(response.result).toBe(2);
  });

  test('should get queue statistics', async () => {
    // Create some mock files
    await require('fs/promises').writeFile(
      path.join(queueStructure.requestsDir, 'req_1.json'), 
      '{}'
    );
    await require('fs/promises').writeFile(
      path.join(queueStructure.processingDir, 'req_2.json'),
      '{}'
    );
    
    const stats = await client.getQueueStats();
    expect(stats).toEqual({
      pending: 1,
      processing: 1,
      responses: 0
    });
  });

  test('should validate request content', async () => {
    await expect(client.execute('command', 'test\0malicious'))
      .rejects.toThrow('Request content cannot contain null bytes');
  });

  test('should validate request size', async () => {
    const largeContent = 'x'.repeat(2 * 1024 * 1024); // 2MB
    await expect(client.execute('command', largeContent))
      .rejects.toThrow('Request size');
  });
});
