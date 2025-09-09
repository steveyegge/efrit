/**
 * Jest test setup and utilities for Efrit MCP Server tests
 */

import * as fs from 'fs/promises';
import * as path from 'path';
import * as os from 'os';

/**
 * Create a temporary directory for test files
 */
export async function createTempDir(prefix: string = 'efrit-test-'): Promise<string> {
  const tempDir = await fs.mkdtemp(path.join(os.tmpdir(), prefix));
  return tempDir;
}

/**
 * Clean up temporary directory and all its contents
 */
export async function cleanupTempDir(tempDir: string): Promise<void> {
  try {
    await fs.rm(tempDir, { recursive: true, force: true });
  } catch (error) {
    // Ignore cleanup errors in tests
    console.warn(`Failed to cleanup temp dir ${tempDir}: ${error}`);
  }
}

/**
 * Create queue directory structure for testing
 */
export async function createQueueStructure(baseDir: string): Promise<{
  queueDir: string;
  requestsDir: string;
  processingDir: string;
  responsesDir: string;
  archiveDir: string;
}> {
  const queueDir = path.join(baseDir, 'queues');
  const requestsDir = path.join(queueDir, 'requests');
  const processingDir = path.join(queueDir, 'processing');
  const responsesDir = path.join(queueDir, 'responses');
  const archiveDir = path.join(queueDir, 'archive');

  await fs.mkdir(requestsDir, { recursive: true });
  await fs.mkdir(processingDir, { recursive: true });
  await fs.mkdir(responsesDir, { recursive: true });
  await fs.mkdir(archiveDir, { recursive: true });

  return {
    queueDir,
    requestsDir,
    processingDir,
    responsesDir,
    archiveDir
  };
}

/**
 * Write a mock response file for testing
 */
export async function writeMockResponse(
  responsesDir: string,
  requestId: string,
  response: any
): Promise<string> {
  const responseFile = path.join(responsesDir, `resp_${requestId}.json`);
  await fs.writeFile(responseFile, JSON.stringify(response, null, 2));
  return responseFile;
}

/**
 * Create a delayed mock response (simulates Efrit processing time)
 */
export async function createDelayedResponse(
  responsesDir: string,
  requestId: string,
  response: any,
  delayMs: number
): Promise<void> {
  setTimeout(async () => {
    await writeMockResponse(responsesDir, requestId, response);
  }, delayMs);
}

/**
 * Read and parse a request file
 */
export async function readRequestFile(requestsDir: string, requestId: string): Promise<any> {
  const requestFile = path.join(requestsDir, `req_${requestId}.json`);
  const content = await fs.readFile(requestFile, 'utf-8');
  return JSON.parse(content);
}

/**
 * Assert that a file exists
 */
export async function assertFileExists(filepath: string): Promise<void> {
  try {
    await fs.access(filepath);
  } catch (error) {
    throw new Error(`Expected file to exist: ${filepath}`);
  }
}

/**
 * Assert that a file does not exist
 */
export async function assertFileNotExists(filepath: string): Promise<void> {
  try {
    await fs.access(filepath);
    throw new Error(`Expected file to not exist: ${filepath}`);
  } catch (error) {
    if (error instanceof Error && 'code' in error && error.code === 'ENOENT') {
      return; // File doesn't exist as expected
    }
    throw error;
  }
}

/**
 * Mock console methods to capture output in tests
 */
export function mockConsole() {
  const originalLog = console.log;
  const originalWarn = console.warn;
  const originalError = console.error;

  const logs: string[] = [];
  const warnings: string[] = [];
  const errors: string[] = [];

  console.log = (...args: any[]) => {
    logs.push(args.join(' '));
  };

  console.warn = (...args: any[]) => {
    warnings.push(args.join(' '));
  };

  console.error = (...args: any[]) => {
    errors.push(args.join(' '));
  };

  return {
    logs,
    warnings,
    errors,
    restore: () => {
      console.log = originalLog;
      console.warn = originalWarn;
      console.error = originalError;
    }
  };
}

/**
 * Wait for a condition to be true with timeout
 */
export async function waitFor(
  condition: () => Promise<boolean>,
  timeoutMs: number = 5000,
  intervalMs: number = 100
): Promise<void> {
  const startTime = Date.now();
  
  while (Date.now() - startTime < timeoutMs) {
    if (await condition()) {
      return;
    }
    await new Promise(resolve => setTimeout(resolve, intervalMs));
  }
  
  throw new Error(`Condition not met within ${timeoutMs}ms`);
}
