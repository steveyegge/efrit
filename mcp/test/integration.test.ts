/**
 * Integration tests for Efrit MCP Server components
 *
 * Tests end-to-end request/response flow through the queue system,
 * validating protocol compliance and multi-instance coordination.
 *
 * Note: These tests validate the complete integration of EfritClient
 * with the queue system, which is the core of the MCP server's functionality.
 * Full MCP protocol testing (with StdioServerTransport) would require a separate
 * MCP client process, which is beyond the scope of unit/integration tests.
 */

import * as path from 'path';
import * as fs from 'fs/promises';
import { EfritClient } from '../src/efrit-client.js';
import { InstanceConfig, EFRIT_SCHEMA_VERSION } from '../src/types.js';
import {
  createTempDir,
  cleanupTempDir,
  createQueueStructure,
  writeMockResponse,
  waitForRequestFile,
  readRequestFile,
  createDelayedResponse
} from './setup.js';

describe('MCP Server Integration Tests', () => {
  describe('Multi-Instance Queue Coordination', () => {
    let tempDir: string;
    let queueStructure1: Awaited<ReturnType<typeof createQueueStructure>>;
    let queueStructure2: Awaited<ReturnType<typeof createQueueStructure>>;
    let client1: EfritClient;
    let client2: EfritClient;

    beforeEach(async () => {
      tempDir = await createTempDir('mcp-integration-');

      const instance1Dir = path.join(tempDir, 'instance1');
      const instance2Dir = path.join(tempDir, 'instance2');

      queueStructure1 = await createQueueStructure(instance1Dir);
      queueStructure2 = await createQueueStructure(instance2Dir);

      const config1: InstanceConfig = {
        queue_dir: queueStructure1.queueDir,
        workspace_dir: path.join(instance1Dir, 'workspace'),
        timeout: 5,
        daemon_name: 'efrit-primary'
      };

      const config2: InstanceConfig = {
        queue_dir: queueStructure2.queueDir,
        workspace_dir: path.join(instance2Dir, 'workspace'),
        timeout: 3,
        daemon_name: 'efrit-secondary'
      };

      client1 = new EfritClient('primary', config1, [tempDir]);
      client2 = new EfritClient('secondary', config2, [tempDir]);
    });

    afterEach(async () => {
      await cleanupTempDir(tempDir);
    });

    test('should maintain separate queues per instance', async () => {
      // Execute on both instances concurrently
      const promise1 = client1.execute('command', 'cmd1');
      const promise2 = client2.execute('command', 'cmd2');

      // Wait for requests to be written
      const requestId1 = await waitForRequestFile(queueStructure1.requestsDir);
      const requestId2 = await waitForRequestFile(queueStructure2.requestsDir);

      // Verify requests went to correct queues
      const req1 = await readRequestFile(queueStructure1.requestsDir, requestId1);
      const req2 = await readRequestFile(queueStructure2.requestsDir, requestId2);

      expect(req1.content).toBe('cmd1');
      expect(req1.instance_id).toBe('primary');
      expect(req2.content).toBe('cmd2');
      expect(req2.instance_id).toBe('secondary');

      // Complete both requests
      await writeMockResponse(queueStructure1.responsesDir, requestId1, {
        id: requestId1,
        version: EFRIT_SCHEMA_VERSION,
        status: 'success',
        result: 'result1',
        timestamp: new Date().toISOString(),
        instance_id: 'primary'
      });

      await writeMockResponse(queueStructure2.responsesDir, requestId2, {
        id: requestId2,
        version: EFRIT_SCHEMA_VERSION,
        status: 'success',
        result: 'result2',
        timestamp: new Date().toISOString(),
        instance_id: 'secondary'
      });

      const [response1, response2] = await Promise.all([promise1, promise2]);
      expect(response1.result).toBe('result1');
      expect(response2.result).toBe('result2');
    });

    test('should handle concurrent requests to different instances', async () => {
      // Send multiple requests to each instance
      const promises = [
        client1.execute('command', 'cmd1-a'),
        client1.execute('command', 'cmd1-b'),
        client2.execute('command', 'cmd2-a'),
        client2.execute('command', 'cmd2-b')
      ];

      // Wait a bit for all requests to be written
      await new Promise(resolve => setTimeout(resolve, 200));

      // Check that requests appeared in correct queues
      const files1 = await fs.readdir(queueStructure1.requestsDir);
      const files2 = await fs.readdir(queueStructure2.requestsDir);

      const reqFiles1 = files1.filter(f => f.startsWith('req_') && f.endsWith('.json'));
      const reqFiles2 = files2.filter(f => f.startsWith('req_') && f.endsWith('.json'));

      expect(reqFiles1.length).toBeGreaterThanOrEqual(2);
      expect(reqFiles2.length).toBeGreaterThanOrEqual(2);

      // Complete all requests
      for (const file of reqFiles1) {
        const requestId = file.replace('req_', '').replace('.json', '');
        await writeMockResponse(queueStructure1.responsesDir, requestId, {
          id: requestId,
          version: EFRIT_SCHEMA_VERSION,
          status: 'success',
          result: 'ok',
          timestamp: new Date().toISOString()
        });
      }

      for (const file of reqFiles2) {
        const requestId = file.replace('req_', '').replace('.json', '');
        await writeMockResponse(queueStructure2.responsesDir, requestId, {
          id: requestId,
          version: EFRIT_SCHEMA_VERSION,
          status: 'success',
          result: 'ok',
          timestamp: new Date().toISOString()
        });
      }

      const results = await Promise.all(promises);
      expect(results.every(r => r.status === 'success')).toBe(true);
    });

    test('should respect per-instance timeout settings', async () => {
      // Client1 has 5 second timeout, client2 has 3 second timeout
      // Both will timeout since we won't provide responses

      const start = Date.now();
      await expect(client2.execute('command', 'test'))
        .rejects.toThrow('timed out');
      const duration = Date.now() - start;

      // Should timeout around 3 seconds (allow 1000ms tolerance for CI)
      expect(duration).toBeGreaterThan(2500);
      expect(duration).toBeLessThan(4000);
    });
  });

  describe('Protocol Compliance', () => {
    let tempDir: string;
    let queueStructure: Awaited<ReturnType<typeof createQueueStructure>>;
    let client: EfritClient;

    beforeEach(async () => {
      tempDir = await createTempDir('mcp-protocol-');
      queueStructure = await createQueueStructure(tempDir);

      const config: InstanceConfig = {
        queue_dir: queueStructure.queueDir,
        workspace_dir: path.join(tempDir, 'workspace'),
        timeout: 5,
        daemon_name: 'efrit-test'
      };

      client = new EfritClient('test', config, [tempDir]);
    });

    afterEach(async () => {
      await cleanupTempDir(tempDir);
    });

    test('should include all required request fields', async () => {
      const responsePromise = client.execute('command', 'test');

      const requestId = await waitForRequestFile(queueStructure.requestsDir);
      const request = await readRequestFile(queueStructure.requestsDir, requestId);

      // Verify protocol-required fields
      expect(request).toHaveProperty('id');
      expect(request).toHaveProperty('version');
      expect(request).toHaveProperty('type');
      expect(request).toHaveProperty('content');
      expect(request).toHaveProperty('timestamp');
      expect(request).toHaveProperty('instance_id');
      expect(request).toHaveProperty('options');
      expect(request).toHaveProperty('metadata');

      // Verify field types and values
      expect(typeof request.id).toBe('string');
      expect(request.version).toBe(EFRIT_SCHEMA_VERSION);
      expect(['command', 'eval', 'chat']).toContain(request.type);
      expect(typeof request.timestamp).toBe('string');

      // Complete request
      await writeMockResponse(queueStructure.responsesDir, requestId, {
        id: requestId,
        version: EFRIT_SCHEMA_VERSION,
        status: 'success',
        result: 'ok',
        timestamp: new Date().toISOString()
      });

      await responsePromise;
    });

    test('should validate response protocol compliance', async () => {
      const responsePromise = client.execute('eval', '(+ 1 2)');

      const requestId = await waitForRequestFile(queueStructure.requestsDir);

      // Create response missing required field
      const invalidResponse = {
        id: requestId,
        version: EFRIT_SCHEMA_VERSION,
        // Missing 'status' field
        result: 3,
        timestamp: new Date().toISOString()
      };

      await writeMockResponse(queueStructure.responsesDir, requestId, invalidResponse);

      // Client DOES validate required fields, so this should reject
      await expect(responsePromise).rejects.toThrow('Response missing required fields');
    });

    test('should handle different execution types', async () => {
      const types = ['command', 'eval', 'chat'] as const;

      for (const type of types) {
        const responsePromise = client.execute(type, `test ${type}`);

        const requestId = await waitForRequestFile(queueStructure.requestsDir);
        const request = await readRequestFile(queueStructure.requestsDir, requestId);

        expect(request.type).toBe(type);
        expect(request.content).toBe(`test ${type}`);

        await writeMockResponse(queueStructure.responsesDir, requestId, {
          id: requestId,
          version: EFRIT_SCHEMA_VERSION,
          status: 'success',
          result: `${type} result`,
          timestamp: new Date().toISOString()
        });

        const response = await responsePromise;
        expect(response.result).toBe(`${type} result`);
      }
    });

    test('should handle context return option', async () => {
      const responsePromise = client.execute('command', 'test', {
        return_context: true
      });

      const requestId = await waitForRequestFile(queueStructure.requestsDir);
      const request = await readRequestFile(queueStructure.requestsDir, requestId);

      expect(request.options.return_context).toBe(true);

      // Provide response with context
      await writeMockResponse(queueStructure.responsesDir, requestId, {
        id: requestId,
        version: EFRIT_SCHEMA_VERSION,
        status: 'success',
        result: 'ok',
        context: {
          current_buffer: 'test.el',
          major_mode: 'emacs-lisp-mode',
          point: 42
        },
        timestamp: new Date().toISOString()
      });

      const response = await responsePromise;
      expect(response.context).toBeDefined();
      expect(response.context?.['current_buffer']).toBe('test.el');
    });

    test('should handle timeout override', async () => {
      const responsePromise = client.execute('command', 'test', {
        timeout: 10
      });

      const requestId = await waitForRequestFile(queueStructure.requestsDir);
      const request = await readRequestFile(queueStructure.requestsDir, requestId);

      expect(request.options.timeout).toBe(10);

      await writeMockResponse(queueStructure.responsesDir, requestId, {
        id: requestId,
        version: EFRIT_SCHEMA_VERSION,
        status: 'success',
        result: 'ok',
        timestamp: new Date().toISOString()
      });

      await responsePromise;
    });
  });

  describe('Error Handling Integration', () => {
    let tempDir: string;
    let queueStructure: Awaited<ReturnType<typeof createQueueStructure>>;
    let client: EfritClient;

    beforeEach(async () => {
      tempDir = await createTempDir('mcp-errors-');
      queueStructure = await createQueueStructure(tempDir);

      const config: InstanceConfig = {
        queue_dir: queueStructure.queueDir,
        workspace_dir: path.join(tempDir, 'workspace'),
        timeout: 5,
        daemon_name: 'efrit-test'
      };

      client = new EfritClient('test', config, [tempDir]);
    });

    afterEach(async () => {
      await cleanupTempDir(tempDir);
    });

    test('should handle malformed JSON in response gracefully', async () => {
      const responsePromise = client.execute('command', 'test');

      const requestId = await waitForRequestFile(queueStructure.requestsDir);

      // Write malformed JSON
      const responseFile = path.join(queueStructure.responsesDir, `resp_${requestId}.json`);
      await fs.writeFile(responseFile, '{ invalid json');

      await expect(responsePromise).rejects.toThrow('Invalid JSON in response file');
    });

    test('should handle response ID mismatch', async () => {
      const responsePromise = client.execute('command', 'test');

      const requestId = await waitForRequestFile(queueStructure.requestsDir);

      // Create response with wrong ID
      await writeMockResponse(queueStructure.responsesDir, requestId, {
        id: 'wrong-id-12345',
        version: EFRIT_SCHEMA_VERSION,
        status: 'success',
        result: 'test result',
        timestamp: new Date().toISOString()
      });

      await expect(responsePromise).rejects.toThrow('doesn\'t match request ID');
    });

    test('should handle error responses from Efrit', async () => {
      const responsePromise = client.execute('eval', '(error "test error")');

      const requestId = await waitForRequestFile(queueStructure.requestsDir);

      await writeMockResponse(queueStructure.responsesDir, requestId, {
        id: requestId,
        version: EFRIT_SCHEMA_VERSION,
        status: 'error',
        error: 'Elisp error: test error',
        error_code: 'EVAL_ERROR',
        timestamp: new Date().toISOString()
      });

      const response = await responsePromise;
      expect(response.status).toBe('error');
      expect(response.error).toContain('test error');
      expect(response.error_code).toBe('EVAL_ERROR');
    });

    test('should handle delayed responses correctly', async () => {
      const responsePromise = client.execute('command', 'slow-command');

      const requestId = await waitForRequestFile(queueStructure.requestsDir);

      // Create response with 500ms delay (simulating Efrit processing time)
      await createDelayedResponse(queueStructure.responsesDir, requestId, {
        id: requestId,
        version: EFRIT_SCHEMA_VERSION,
        status: 'success',
        result: 'delayed result',
        execution_time: 0.5,
        timestamp: new Date().toISOString()
      }, 500);

      const response = await responsePromise;
      expect(response.result).toBe('delayed result');
      expect(response.execution_time).toBe(0.5);
    });
  });

  describe('Queue Statistics and Health', () => {
    let tempDir: string;
    let queueStructure: Awaited<ReturnType<typeof createQueueStructure>>;
    let client: EfritClient;

    beforeEach(async () => {
      tempDir = await createTempDir('mcp-stats-');
      queueStructure = await createQueueStructure(tempDir);

      const config: InstanceConfig = {
        queue_dir: queueStructure.queueDir,
        workspace_dir: path.join(tempDir, 'workspace'),
        timeout: 5,
        daemon_name: 'efrit-test'
      };

      client = new EfritClient('test', config, [tempDir]);
    });

    afterEach(async () => {
      await cleanupTempDir(tempDir);
    });

    test('should accurately track pending requests', async () => {
      // Create multiple pending requests
      const promises = [
        client.execute('command', 'cmd1'),
        client.execute('command', 'cmd2'),
        client.execute('command', 'cmd3')
      ];

      // Wait for all to be written
      await new Promise(resolve => setTimeout(resolve, 200));

      const stats = await client.getQueueStats();
      expect(stats.pending).toBeGreaterThanOrEqual(3);

      // Complete all requests
      const files = await fs.readdir(queueStructure.requestsDir);
      for (const file of files.filter(f => f.startsWith('req_'))) {
        const requestId = file.replace('req_', '').replace('.json', '');
        await writeMockResponse(queueStructure.responsesDir, requestId, {
          id: requestId,
          version: EFRIT_SCHEMA_VERSION,
          status: 'success',
          result: 'ok',
          timestamp: new Date().toISOString()
        });
      }

      await Promise.all(promises);

      // Stats should show no pending requests after completion
      const finalStats = await client.getQueueStats();
      expect(finalStats.pending).toBe(0);
    });

    test('should report healthy status for accessible queue', async () => {
      const healthy = await client.healthCheck();
      expect(healthy).toBe(true);
    });

    test('should report unhealthy status for inaccessible queue', async () => {
      const badConfig: InstanceConfig = {
        queue_dir: '/nonexistent/totally/fake/path',
        timeout: 5,
        daemon_name: 'bad'
      };

      const badClient = new EfritClient('bad', badConfig, ['/nonexistent']);
      const healthy = await badClient.healthCheck();
      expect(healthy).toBe(false);
    });
  });

  describe('Cleanup and Archival', () => {
    let tempDir: string;
    let queueStructure: Awaited<ReturnType<typeof createQueueStructure>>;
    let client: EfritClient;

    beforeEach(async () => {
      tempDir = await createTempDir('mcp-cleanup-');
      queueStructure = await createQueueStructure(tempDir);

      const config: InstanceConfig = {
        queue_dir: queueStructure.queueDir,
        workspace_dir: path.join(tempDir, 'workspace'),
        timeout: 5,
        daemon_name: 'efrit-test'
      };

      client = new EfritClient('test', config, [tempDir]);
    });

    afterEach(async () => {
      await cleanupTempDir(tempDir);
    });

    test('should archive old response files during cleanup', async () => {
      // Create an old response file
      const oldFile = path.join(queueStructure.responsesDir, 'resp_old.json');
      await fs.writeFile(oldFile, JSON.stringify({
        id: 'old',
        version: EFRIT_SCHEMA_VERSION,
        status: 'success',
        result: 'old result',
        timestamp: new Date(Date.now() - 25 * 60 * 60 * 1000).toISOString() // 25 hours ago
      }));

      // Modify file timestamp to be old
      const oldTime = new Date(Date.now() - 25 * 60 * 60 * 1000);
      await fs.utimes(oldFile, oldTime, oldTime);

      await client.cleanup(24); // Clean files older than 24 hours

      // File should be moved to archive
      try {
        await fs.access(oldFile);
        fail('Expected old file to be archived');
      } catch (error: any) {
        expect(error.code).toBe('ENOENT');
      }

      // Check it's in archive
      const archivedFile = path.join(queueStructure.archiveDir, 'resp_old.json');
      await expect(fs.access(archivedFile)).resolves.not.toThrow();
    });

    test('should not archive recent response files', async () => {
      const recentFile = path.join(queueStructure.responsesDir, 'resp_recent.json');
      await fs.writeFile(recentFile, JSON.stringify({
        id: 'recent',
        version: EFRIT_SCHEMA_VERSION,
        status: 'success',
        result: 'recent result',
        timestamp: new Date().toISOString()
      }));

      await client.cleanup(24);

      // File should still be in responses directory
      await expect(fs.access(recentFile)).resolves.not.toThrow();
    });
  });
});
