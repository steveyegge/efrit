/**
 * Tests for EfritClient
 */

import { EfritClient } from '../efrit-client';
import { InstanceConfig } from '../types';
import * as fs from 'fs/promises';
import * as path from 'path';
import * as os from 'os';

describe('EfritClient', () => {
  let testDir: string;
  let client: EfritClient;
  let config: InstanceConfig;

  beforeEach(async () => {
    // Create temporary test directory
    testDir = path.join(os.tmpdir(), 'efrit-test-' + Date.now());
    await fs.mkdir(testDir, { recursive: true });
    
    config = {
      queue_dir: testDir,
      daemon_name: 'efrit-test',
      timeout: 5
    };
    
    client = new EfritClient('test', config);
  });

  afterEach(async () => {
    // Clean up test directory
    try {
      await fs.rm(testDir, { recursive: true, force: true });
    } catch (error) {
      // Ignore cleanup errors
    }
  });

  describe('directory management', () => {
    it('should create required queue directories', async () => {
      await client['ensureDirectories']();
      
      const subdirs = ['requests', 'responses', 'processing', 'archive'];
      for (const subdir of subdirs) {
        const dir = path.join(testDir, subdir);
        const stats = await fs.stat(dir);
        expect(stats.isDirectory()).toBe(true);
      }
    });

    it('should expand tilde paths', () => {
      const expanded = client['expandUser']('~/test/path');
      expect(expanded).toBe(path.join(os.homedir(), 'test/path'));
      
      const notExpanded = client['expandUser']('/absolute/path');
      expect(notExpanded).toBe('/absolute/path');
    });
  });

  describe('request generation', () => {
    it('should generate unique request IDs', () => {
      const id1 = client['generateRequestId']();
      const id2 = client['generateRequestId']();
      
      // Updated pattern for UUID format
      expect(id1).toMatch(/^efrit_[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/);
      expect(id2).toMatch(/^efrit_[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/);
      expect(id1).not.toBe(id2);
    });
  });

  describe('queue stats', () => {
    it('should return queue statistics', async () => {
      await client['ensureDirectories']();
      
      // Create some test files
      await fs.writeFile(path.join(testDir, 'requests/test1.json'), '{}');
      await fs.writeFile(path.join(testDir, 'processing/test2.json'), '{}');
      
      const stats = await client.getQueueStats();
      
      expect(stats.pending_requests).toBe(1);
      expect(stats.currently_processing).toBe(1);
      expect(stats.pending_responses).toBe(0);
    });
  });

  describe('ping functionality', () => {
    it('should handle ping timeout gracefully', async () => {
      // Mock short timeout
      config.timeout = 1;
      
      const result = await client.ping();
      expect(result).toBe(false);
    }, 10000); // 10 second timeout
  });
});
