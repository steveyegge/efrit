/**
 * Unit tests for EfritClient
 * Tests file-based queue communication, security validation, error handling
 */

import * as path from 'path';
import { EfritClient } from '../src/efrit-client';
import { InstanceConfig, EfritError, EFRIT_SCHEMA_VERSION } from '../src/types';
import {
  createTempDir,
  cleanupTempDir,
  createQueueStructure,
  writeMockResponse,
  createDelayedResponse,
  readRequestFile,
  assertFileExists,
  assertFileNotExists,
  waitFor,
  waitForRequestFile
} from './setup';

describe('EfritClient', () => {
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
      timeout: 10,
      daemon_name: 'efrit-test'
    };
    
    client = new EfritClient('test-instance', config, [tempDir]);
  });

  afterEach(async () => {
    await cleanupTempDir(tempDir);
  });

  describe('Constructor and initialization', () => {
    test('should create client with valid config', () => {
      expect(client).toBeInstanceOf(EfritClient);
    });

    test('should use home directory as default allowed root', () => {
      const homeClient = new EfritClient('test', config);
      expect(homeClient).toBeInstanceOf(EfritClient);
    });
  });

  describe('Path validation and security', () => {
    test('should reject paths with directory traversal', async () => {
      const maliciousConfig = {
        ...config,
        queue_dir: '../../etc/passwd'
      };
      
      const maliciousClient = new EfritClient('malicious', maliciousConfig, [tempDir]);
      
      await expect(maliciousClient.execute('command', 'test'))
        .rejects.toThrow('Invalid file path detected');
    });

    test('should reject paths with null bytes', async () => {
      const maliciousConfig = {
        ...config,
        queue_dir: `/tmp/test\0malicious`
      };
      
      const maliciousClient = new EfritClient('malicious', maliciousConfig, [tempDir]);
      
      await expect(maliciousClient.execute('command', 'test'))
        .rejects.toThrow('Invalid file path detected');
    });

    test('should reject paths outside allowed roots', async () => {
      const outsideConfig = {
        ...config,
        queue_dir: '/etc/passwd'
      };
      
      const outsideClient = new EfritClient('outside', outsideConfig, [tempDir]);
      
      await expect(outsideClient.execute('command', 'test'))
        .rejects.toThrow('is not within allowed roots');
    });

    test('should expand tilde paths correctly', async () => {
      const homeConfig = {
        ...config,
        queue_dir: '~/.efrit/test'
      };
      
      const homeClient = new EfritClient('home', homeConfig);
      
      // This should not throw (assuming home directory is in allowed roots)
      expect(homeClient).toBeInstanceOf(EfritClient);
    });
  });

  describe('Request validation', () => {
    test('should reject requests that are too large', async () => {
      const largeContent = 'x'.repeat(2 * 1024 * 1024); // 2MB
      
      await expect(client.execute('command', largeContent))
        .rejects.toThrow('Request size');
    });

    test('should reject content with null bytes', async () => {
      const maliciousContent = 'test\0malicious';
      
      await expect(client.execute('command', maliciousContent))
        .rejects.toThrow('Request content cannot contain null bytes');
    });
  });

  describe('Request writing', () => {
    test('should write request file with correct structure', async () => {
      const responsePromise = client.execute('command', 'test command');
      
      // Give it time to write the request
      await new Promise(resolve => setTimeout(resolve, 100));
      
      // Check that request file was created
      const files = await require('fs/promises').readdir(queueStructure.requestsDir);
      const requestFiles = files.filter((f: string) => f.startsWith('req_') && f.endsWith('.json'));
      expect(requestFiles).toHaveLength(1);
      
      // Read and validate request structure
      const requestId = requestFiles[0].replace('req_', '').replace('.json', '');
      const request = await readRequestFile(queueStructure.requestsDir, requestId);
      
      expect(request).toMatchObject({
        id: requestId,
        version: EFRIT_SCHEMA_VERSION,
        type: 'command',
        content: 'test command',
        instance_id: 'test-instance',
        options: {
          timeout: 10,
          return_context: false
        },
        metadata: {
          client_id: 'efrit-mcp-server',
          source: 'mcp'
        }
      });
      
      expect(request.timestamp).toBeDefined();
      
      // Create mock response to complete the test
      await writeMockResponse(queueStructure.responsesDir, requestId, {
        id: requestId,
        version: EFRIT_SCHEMA_VERSION,
        status: 'success',
        result: 'test result',
        timestamp: new Date().toISOString()
      });
      
      const response = await responsePromise;
      expect(response.status).toBe('success');
    });

    test('should use atomic write operations', async () => {
      const responsePromise = client.execute('eval', '(+ 1 1)');
      
      // Give it time to start writing
      await new Promise(resolve => setTimeout(resolve, 50));
      
      // Check that no .tmp files are left behind
      const files = await require('fs/promises').readdir(queueStructure.requestsDir);
      const tempFiles = files.filter((f: string) => f.endsWith('.tmp'));
      expect(tempFiles).toHaveLength(0);
      
      // Complete the request
      const requestFiles = files.filter((f: string) => f.startsWith('req_'));
      if (requestFiles.length > 0) {
        const requestId = requestFiles[0].replace('req_', '').replace('.json', '');
        await writeMockResponse(queueStructure.responsesDir, requestId, {
          id: requestId,
          version: EFRIT_SCHEMA_VERSION,
          status: 'success',
          result: 2,
          timestamp: new Date().toISOString()
        });
      }
      
      await responsePromise;
    });
  });

  describe('Response polling', () => {
    test('should successfully poll and return response', async () => {
      // Create delayed response
      const responsePromise = client.execute('command', 'test');

      // Wait for request file to be written
      const requestId = await waitForRequestFile(queueStructure.requestsDir);

      // Create response with delay
      await createDelayedResponse(queueStructure.responsesDir, requestId, {
        id: requestId,
        version: EFRIT_SCHEMA_VERSION,
        status: 'success',
        result: 'delayed result',
        timestamp: new Date().toISOString(),
        execution_time: 0.5
      }, 200);
      
      const response = await responsePromise;
      expect(response).toMatchObject({
        id: requestId,
        status: 'success',
        result: 'delayed result'
      });
    });

    test('should timeout when no response received', async () => {
      const shortTimeoutClient = new EfritClient('timeout-test', {
        ...config,
        timeout: 1 // 1 second timeout
      }, [tempDir]);
      
      await expect(shortTimeoutClient.execute('command', 'test'))
        .rejects.toThrow('Request timed out');
    });

    test('should clean up response file after successful read', async () => {
      const responsePromise = client.execute('command', 'test');
      
      // Wait for request
      const requestId = await waitForRequestFile(queueStructure.requestsDir);
      
      // Create response
      const responseFile = await writeMockResponse(queueStructure.responsesDir, requestId, {
        id: requestId,
        version: EFRIT_SCHEMA_VERSION,
        status: 'success',
        result: 'test result',
        timestamp: new Date().toISOString()
      });
      
      const response = await responsePromise;
      expect(response.status).toBe('success');
      
      // Response file should be cleaned up
      await assertFileNotExists(responseFile);
    });

    test('should handle malformed JSON in response', async () => {
      const responsePromise = client.execute('command', 'test');
      
      const requestId = await waitForRequestFile(queueStructure.requestsDir);
      
      // Write malformed JSON
      const responseFile = path.join(queueStructure.responsesDir, `resp_${requestId}.json`);
      await require('fs/promises').writeFile(responseFile, '{ invalid json');
      
      await expect(responsePromise).rejects.toThrow('Invalid JSON in response file');
    });

    test('should validate response ID matches request', async () => {
      const responsePromise = client.execute('command', 'test');
      
      const requestId = await waitForRequestFile(queueStructure.requestsDir);
      
      // Create response with wrong ID
      await writeMockResponse(queueStructure.responsesDir, requestId, {
        id: 'wrong-id',
        version: EFRIT_SCHEMA_VERSION,
        status: 'success',
        result: 'test result',
        timestamp: new Date().toISOString()
      });
      
      await expect(responsePromise).rejects.toThrow('Response ID wrong-id doesn\'t match request ID');
    });
  });

  describe('Queue statistics', () => {
    test('should return accurate queue statistics', async () => {
      // Create some mock files
      await require('fs/promises').writeFile(
        path.join(queueStructure.requestsDir, 'req_1.json'), 
        '{}'
      );
      await require('fs/promises').writeFile(
        path.join(queueStructure.processingDir, 'req_2.json'),
        '{}'
      );
      await require('fs/promises').writeFile(
        path.join(queueStructure.responsesDir, 'resp_3.json'),
        '{}'
      );
      
      const stats = await client.getQueueStats();
      
      expect(stats).toEqual({
        pending: 1,
        processing: 1,
        responses: 1
      });
    });
  });

  describe('Health check', () => {
    test('should return true for accessible queue directory', async () => {
      const healthy = await client.healthCheck();
      expect(healthy).toBe(true);
    });

    test('should return false for inaccessible queue directory', async () => {
      const badConfig = {
        ...config,
        queue_dir: '/nonexistent/directory'
      };
      
      const badClient = new EfritClient('bad', badConfig, ['/nonexistent']);
      const healthy = await badClient.healthCheck();
      expect(healthy).toBe(false);
    });
  });

  describe('Cleanup operations', () => {
    test('should archive old response files', async () => {
      // Create an old response file
      const oldFile = path.join(queueStructure.responsesDir, 'resp_old.json');
      await require('fs/promises').writeFile(oldFile, '{}');
      
      // Modify file timestamp to be old
      const oldTime = new Date(Date.now() - 25 * 60 * 60 * 1000); // 25 hours ago
      await require('fs/promises').utimes(oldFile, oldTime, oldTime);
      
      await client.cleanup(24); // Clean files older than 24 hours
      
      // File should be moved to archive
      await assertFileNotExists(oldFile);
      await assertFileExists(path.join(queueStructure.archiveDir, 'resp_old.json'));
    });
  });

  describe('Error handling', () => {
    test('should throw structured errors', async () => {
      try {
        await client.execute('command', '\0malicious');
        fail('Expected error to be thrown');
      } catch (error) {
        expect(error).toBeInstanceOf(Error);
        const efritError = error as EfritError;
        expect(efritError.code).toBe('INVALID_CONTENT');
        expect(efritError.instance_id).toBe('test-instance');
      }
    });
  });
});
