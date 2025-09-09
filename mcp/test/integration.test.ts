/**
 * Integration tests for MCP server with real file system operations
 * Oracle recommendation: Add real file system integration tests
 */

import { describe, it, expect, beforeEach, afterEach } from '@jest/globals';
import * as fs from 'fs';
import * as path from 'path';
import * as os from 'os';
import { EfritMcpServer } from '../dist/server.js';

describe('MCP Server Integration Tests', () => {
  let tempDir: string;
  let configPath: string;
  let server: EfritMcpServer;

  beforeEach(() => {
    // Create temporary directory for testing
    tempDir = fs.mkdtempSync(path.join(os.tmpdir(), 'efrit-mcp-integration-'));
    configPath = path.join(tempDir, 'instances.json');
    
    // Create test configuration
    const testConfig = {
      instances: {
        test: {
          queue_dir: path.join(tempDir, 'queues'),
          workspace_dir: path.join(tempDir, 'workspace'), 
          timeout: 5000
        }
      },
      default_instance: 'test',
      max_concurrent_requests: 2,
      log_level: 'error' as const // Reduce log noise during tests
    };

    fs.writeFileSync(configPath, JSON.stringify(testConfig, null, 2));
  });

  afterEach(() => {
    // Clean up temporary directory
    if (fs.existsSync(tempDir)) {
      fs.rmSync(tempDir, { recursive: true, force: true });
    }
  });

  it('should initialize server with real configuration', () => {
    expect(() => {
      server = new EfritMcpServer(configPath);
    }).not.toThrow();
    
    expect(server).toBeDefined();
  });

  it('should validate tilde expansion in config', () => {
    const configWithTilde = {
      instances: {
        home_test: {
          queue_dir: '~/test-queues',
          timeout: 5000
        }
      },
      default_instance: 'home_test'
    };

    const tildeConfigPath = path.join(tempDir, 'tilde-config.json');
    fs.writeFileSync(tildeConfigPath, JSON.stringify(configWithTilde, null, 2));

    expect(() => {
      server = new EfritMcpServer(tildeConfigPath);
    }).not.toThrow();
  });

  it('should reject config with path traversal attempts', () => {
    const maliciousConfig = {
      instances: {
        malicious: {
          queue_dir: '../../../etc/passwd',
          timeout: 5000
        }
      },
      default_instance: 'malicious'
    };

    const maliciousConfigPath = path.join(tempDir, 'malicious-config.json');
    fs.writeFileSync(maliciousConfigPath, JSON.stringify(maliciousConfig, null, 2));

    expect(() => {
      server = new EfritMcpServer(maliciousConfigPath);
    }).toThrow('queue_dir contains invalid characters');
  });

  it('should create default config when none exists', () => {
    const nonExistentConfigPath = path.join(tempDir, 'nonexistent', 'config.json');
    
    expect(() => {
      server = new EfritMcpServer(nonExistentConfigPath);
    }).not.toThrow();
    
    expect(fs.existsSync(nonExistentConfigPath)).toBe(true);
    
    const createdConfig = JSON.parse(fs.readFileSync(nonExistentConfigPath, 'utf-8'));
    expect(createdConfig).toHaveProperty('instances.default');
    expect(createdConfig.default_instance).toBe('default');
  });

  it('should handle concurrent configuration validation', async () => {
    const promises = [];
    
    // Try to create multiple servers concurrently with different configs
    for (let i = 0; i < 3; i++) {
      const testConfigPath = path.join(tempDir, `config-${i}.json`);
      const testConfig = {
        instances: {
          [`test-${i}`]: {
            queue_dir: path.join(tempDir, `queues-${i}`),
            timeout: 5000
          }
        },
        default_instance: `test-${i}`
      };
      
      fs.writeFileSync(testConfigPath, JSON.stringify(testConfig, null, 2));
      
      promises.push(
        Promise.resolve().then(() => new EfritMcpServer(testConfigPath))
      );
    }
    
    const servers = await Promise.all(promises);
    expect(servers).toHaveLength(3);
    servers.forEach(s => expect(s).toBeDefined());
  });
});
