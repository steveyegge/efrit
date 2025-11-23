/**
 * Simple tests to verify EfritClient core functionality
 */

import * as path from 'path';
import { EfritClient } from '../src/efrit-client';
import { InstanceConfig, EFRIT_SCHEMA_VERSION } from '../src/types';
import * as setup from './setup';
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

    // Wait for request file to be written
    const requestId = await setup.waitForRequestFile(queueStructure.requestsDir);
    const content = await require('fs/promises').readFile(
      path.join(queueStructure.requestsDir, `req_${requestId}.json`),
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

    // Wait for request file and get request ID
    const requestId = await setup.waitForRequestFile(queueStructure.requestsDir);
    
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
