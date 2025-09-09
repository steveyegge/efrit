import { describe, it, expect, beforeEach, afterEach, jest } from '@jest/globals';
import * as fs from 'fs';
import * as path from 'path';
import * as os from 'os';
import { EfritMcpServer } from '../src/server.js';
import { EfritClient } from '../src/efrit-client.js';
import { EfritResponse } from '../src/types.js';

// Mock the MCP SDK
jest.mock('@modelcontextprotocol/sdk/server/index.js', () => ({
  Server: jest.fn().mockImplementation(() => ({
    setRequestHandler: jest.fn(),
    connect: jest.fn(),
  })),
}));

jest.mock('@modelcontextprotocol/sdk/server/stdio.js', () => ({
  StdioServerTransport: jest.fn(),
}));

// Mock EfritClient
jest.mock('../src/efrit-client.js');

describe('EfritMcpServer', () => {
  let tempDir: string;
  let configPath: string;
  let mockEfritClient: jest.Mocked<EfritClient>;
  
  beforeEach(() => {
    // Create temporary directory for testing
    tempDir = fs.mkdtempSync(path.join(os.tmpdir(), 'efrit-mcp-test-'));
    configPath = path.join(tempDir, 'instances.json');
    
    // Create mock EfritClient
    mockEfritClient = {
      execute: jest.fn(),
    } as any;
    
    (EfritClient as jest.MockedClass<typeof EfritClient>).mockImplementation(() => mockEfritClient);
    
    // Clear all mocks
    jest.clearAllMocks();
  });
  
  afterEach(() => {
    // Clean up temporary directory
    if (fs.existsSync(tempDir)) {
      fs.rmSync(tempDir, { recursive: true, force: true });
    }
  });

  describe('Configuration Loading', () => {
    it('should create default config when none exists', () => {
      // Temporarily change the config path by mocking path.resolve
      // Create the server (which should create default config)
      const server = new EfritMcpServer(configPath);
      
      expect(fs.existsSync(configPath)).toBe(true);
      
      const config = JSON.parse(fs.readFileSync(configPath, 'utf-8'));
      expect(config).toHaveProperty('instances.default');
      expect(config).toHaveProperty('default_instance', 'default');
    });

    it('should load existing valid configuration', () => {
      const testConfig = {
        instances: {
          test: {
            queue_dir: '/tmp/test-queue',
            timeout: 15000
          }
        },
        default_instance: 'test',
        max_concurrent_requests: 5,
        log_level: 'debug' as const
      };

      fs.writeFileSync(configPath, JSON.stringify(testConfig));

      const server = new EfritMcpServer(configPath);
    });

    it('should reject invalid configuration', () => {
      const invalidConfig = {
        instances: {},
        default_instance: 'nonexistent'
      };

      fs.writeFileSync(configPath, JSON.stringify(invalidConfig));

      expect(() => new EfritMcpServer(configPath)).toThrow('Configuration must contain at least one instance');
    });
  });

  describe('Tool Handling', () => {
    let server: EfritMcpServer;
    let listToolsHandler: any;
    let callToolHandler: any;

    beforeEach(() => {
      const testConfig = {
        instances: {
          test: {
            queue_dir: '/tmp/test-queue',
            timeout: 15000
          }
        },
        default_instance: 'test'
      };

      fs.writeFileSync(configPath, JSON.stringify(testConfig));

      const { Server } = require('@modelcontextprotocol/sdk/server/index.js');
      const mockServer = {
        setRequestHandler: jest.fn((schema, handler) => {
          if ((schema as any).method === 'tools/list') {
            listToolsHandler = handler;
          } else if ((schema as any).method === 'tools/call') {
            callToolHandler = handler;
          }
        }),
        connect: jest.fn(),
      };
      
      Server.mockImplementation(() => mockServer);
      
      server = new EfritMcpServer(configPath);
    });

    it('should list available tools', async () => {
      const result = await listToolsHandler();
      
      expect(result.tools).toHaveLength(2);
      expect(result.tools[0]).toMatchObject({
        name: 'mcp/efrit_execute',
        description: expect.stringContaining('Execute commands')
      });
      expect(result.tools[1]).toMatchObject({
        name: 'mcp/efrit_list_instances',
        description: expect.stringContaining('List available')
      });
    });

    it('should handle efrit_execute tool calls', async () => {
      const mockResponse: EfritResponse = {
        id: 'test-id',
        status: 'success',
        result: 'Hello from Efrit!',
        timestamp: new Date().toISOString(),
        execution_time: 100,
        version: '1.0.0'
      };

      mockEfritClient.execute.mockResolvedValue(mockResponse);

      const request = {
        params: {
          name: 'mcp/efrit_execute',
          arguments: {
            type: 'command',
            content: 'echo "Hello"'
          }
        }
      };

      const result = await callToolHandler(request);
      
      expect(mockEfritClient.execute).toHaveBeenCalledWith(
        'command',
        'echo "Hello"',
        expect.objectContaining({
          return_context: false
        })
      );
      
      expect(result.content[0].text).toContain('Status: SUCCESS');
      expect(result.content[0].text).toContain('Hello from Efrit!');
    });

    it('should handle efrit_list_instances tool calls', async () => {
      const request = {
        params: {
          name: 'mcp/efrit_list_instances',
          arguments: {}
        }
      };

      const result = await callToolHandler(request);
      
      expect(result.content[0].text).toContain('test');
      expect(result.content[0].text).toContain('default_instance');
    });

    it('should validate required parameters', async () => {
      const request = {
        params: {
          name: 'mcp/efrit_execute',
          arguments: {
            type: 'command'
            // missing content
          }
        }
      };

      await expect(callToolHandler(request)).rejects.toThrow('Missing required parameters');
    });

    it('should validate request type', async () => {
      const request = {
        params: {
          name: 'mcp/efrit_execute',
          arguments: {
            type: 'invalid_type',
            content: 'test'
          }
        }
      };

      await expect(callToolHandler(request)).rejects.toThrow('Invalid type');
    });

    it('should handle unknown instance ID', async () => {
      const request = {
        params: {
          name: 'mcp/efrit_execute',
          arguments: {
            type: 'command',
            content: 'test',
            instance_id: 'nonexistent'
          }
        }
      };

      await expect(callToolHandler(request)).rejects.toThrow('Unknown instance');
    });

    it('should handle execution errors', async () => {
      mockEfritClient.execute.mockRejectedValue(new Error('Execution failed'));

      const request = {
        params: {
          name: 'mcp/efrit_execute',
          arguments: {
            type: 'command',
            content: 'failing command'
          }
        }
      };

      await expect(callToolHandler(request)).rejects.toThrow('Execution failed');
    });

    it('should handle unknown tools', async () => {
      const request = {
        params: {
          name: 'unknown_tool',
          arguments: {}
        }
      };

      await expect(callToolHandler(request)).rejects.toThrow('Unknown tool');
    });
  });

  describe('Response Formatting', () => {
    let server: EfritMcpServer;

    beforeEach(() => {
      const testConfig = {
        instances: {
          test: {
            queue_dir: '/tmp/test-queue',
            timeout: 15000
          }
        },
        default_instance: 'test'
      };

      fs.writeFileSync(configPath, JSON.stringify(testConfig));

      const originalResolve = path.resolve;
      jest.spyOn(path, 'resolve').mockImplementation((dir, file) => {
        if (file === '../config/instances.json') {
          return configPath;
        }
        return originalResolve(dir, file);
      });

      server = new EfritMcpServer();
      
      path.resolve = originalResolve;
    });

    it('should format successful responses', () => {
      const response: EfritResponse = {
        id: 'test-id',
        status: 'success',
        result: 'Test result',
        timestamp: new Date().toISOString(),
        execution_time: 150
      };

      const formatted = (server as any).formatResponse(response);
      
      expect(formatted).toContain('Status: SUCCESS');
      expect(formatted).toContain('Execution time: 150ms');
      expect(formatted).toContain('Result:');
      expect(formatted).toContain('Test result');
    });

    it('should format error responses', () => {
      const response: EfritResponse = {
        id: 'test-id',
        status: 'error',
        error: 'Something went wrong',
        timestamp: new Date().toISOString()
      };

      const formatted = (server as any).formatResponse(response);
      
      expect(formatted).toContain('Status: ERROR');
      expect(formatted).toContain('Error:');
      expect(formatted).toContain('Something went wrong');
    });

    it('should format responses with context', () => {
      const response: EfritResponse = {
        id: 'test-id',
        status: 'success',
        result: 'Test result',
        context: { buffer: 'test.txt', line: 42 },
        timestamp: new Date().toISOString()
      };

      const formatted = (server as any).formatResponse(response);
      
      expect(formatted).toContain('Status: SUCCESS');
      expect(formatted).toContain('Context:');
      expect(formatted).toContain('test.txt');
      expect(formatted).toContain('42');
    });
  });
});
