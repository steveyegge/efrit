/**
 * TypeScript type definitions for Efrit MCP Server
 * Incorporates Oracle recommendations: versioning, security, namespacing
 */

import packageJson from '../package.json' with { type: 'json' };

/**
 * Request types supported by Efrit
 */
export type EfritRequestType = 'command' | 'eval' | 'chat';

/**
 * Response status types
 */
export type EfritResponseStatus = 'success' | 'error' | 'timeout';

/**
 * Log levels for server configuration
 */
export type LogLevel = 'debug' | 'info' | 'warn' | 'error';

/**
 * Core request interface sent to Efrit instances
 * Includes version field for schema evolution (Oracle recommendation)
 */
export interface EfritRequest {
  /** Unique request identifier */
  id: string;
  
  /** Schema version for future compatibility */
  version: string;
  
  /** Type of request: command (efrit-do), eval (elisp), chat (efrit-chat) */
  type: EfritRequestType;
  
  /** Request content/payload */
  content: string;
  
  /** Target instance ID (must match config keys) */
  instance_id?: string;
  
  /** Request options */
  options?: {
    /** Timeout in seconds (default from instance config) */
    timeout?: number;
    
    /** Whether to return context information */
    return_context?: boolean;
    
    /** Maximum response size in bytes */
    max_response_size?: number;
  };
  
  /** ISO timestamp when request was created */
  timestamp: string;
  
  /** Optional metadata for auditing */
  metadata?: {
    /** MCP client identifier */
    client_id?: string;
    
    /** Request source/origin */
    source?: string;
    
    /** Additional audit fields */
    [key: string]: unknown;
  };
}

/**
 * Response interface from Efrit instances
 */
export interface EfritResponse {
  /** Matching request ID */
  id: string;
  
  /** Schema version */
  version: string;
  
  /** Response status */
  status: EfritResponseStatus;
  
  /** Execution result (on success) */
  result?: unknown;
  
  /** Error message (on error) */
  error?: string;
  
  /** Error code for programmatic handling */
  error_code?: string;
  
  /** Context information (if requested) */
  context?: {
    /** Working directory */
    cwd?: string;
    
    /** Emacs version */
    emacs_version?: string;
    
    /** Active modes */
    modes?: string[];
    
    /** Buffer information */
    buffers?: Array<{
      name: string;
      file?: string;
      modified?: boolean;
    }>;
    
    /** Additional context */
    [key: string]: unknown;
  };
  
  /** ISO timestamp when response was created */
  timestamp: string;
  
  /** Execution time in seconds */
  execution_time?: number;
  
  /** Instance that processed the request */
  instance_id?: string;
}

/**
 * Instance configuration
 */
export interface InstanceConfig {
  /** Path to queue directory (will be expanded) */
  queue_dir: string;
  
  /** Workspace directory for file operations */
  workspace_dir?: string;
  
  /** Default timeout in seconds */
  timeout: number;
  
  /** Daemon process name (for lifecycle management) */
  daemon_name?: string;
  
  /** Maximum concurrent requests for this instance */
  max_concurrent?: number;
  
  /** Instance-specific environment variables */
  environment?: Record<string, string>;
}

/**
 * MCP Server configuration
 */
export interface McpServerConfig {
  /** Available instances */
  instances: Record<string, InstanceConfig>;
  
  /** Default instance to use when not specified */
  default_instance: string;
  
  /** Global maximum concurrent requests */
  max_concurrent_requests?: number;
  
  /** Log level */
  log_level?: LogLevel;

  /** Log file path (defaults to ~/.efrit/logs/mcp-server.log if not specified) */
  log_file?: string;

  /** Server port (if running HTTP endpoint for health checks) */
  port?: number;

  /** Security configuration */
  security?: {
    /** Maximum request size in bytes */
    max_request_size: number;
    
    /** Allowed queue directory roots (security whitelist) */
    allowed_queue_roots: string[];
    
    /** Whether to enable request signing (HMAC) */
    enable_request_signing?: boolean;
    
    /** Signing secret for HMAC (if enabled) */
    signing_secret?: string;
  };
  
  /** Audit configuration */
  audit?: {
    /** Enable audit logging */
    enabled: boolean;
    
    /** Audit log directory */
    log_dir: string;
    
    /** Log retention in days */
    retention_days: number;
  };
}

/**
 * MCP Tool parameter interfaces (following Oracle's namespacing recommendation)
 */

/**
 * Parameters for mcp/efrit_execute tool
 */
export interface EfritExecuteParams {
  /** Request type */
  type: EfritRequestType;

  /** Request content */
  content: string;

  /** Target instance ID */
  instance_id?: string | undefined;

  /** Return context information */
  return_context?: boolean | undefined;

  /** Timeout override in seconds */
  timeout?: number | undefined;
}

/**
 * Parameters for mcp/efrit_list_instances tool
 */
export interface EfritListInstancesParams {
  /** Include queue statistics */
  include_stats?: boolean | undefined;
}

/**
 * Parameters for mcp/efrit_get_queue_stats tool
 */
export interface EfritGetQueueStatsParams {
  /** Target instance ID */
  instance_id?: string | undefined;
}

/**
 * Parameters for mcp/efrit_start_instance tool
 */
export interface EfritStartInstanceParams {
  /** Instance ID to start */
  instance_id: string;
}

/**
 * Parameters for mcp/efrit_stop_instance tool
 */
export interface EfritStopInstanceParams {
  /** Instance ID to stop */
  instance_id: string;
  
  /** Force stop (SIGKILL vs SIGTERM) */
  force?: boolean;
}

/**
 * Queue statistics interface
 */
export interface QueueStats {
  /** Instance ID */
  instance_id: string;

  /** Number of pending requests */
  pending_requests: number;

  /** Number of requests being processed */
  processing_requests: number;

  /** Number of completed requests in last hour (null if not implemented) */
  completed_last_hour: number | null;

  /** Number of failed requests in last hour (null if not implemented) */
  failed_last_hour: number | null;

  /** Average processing time in seconds (null if not implemented) */
  avg_processing_time: number | null;

  /** Queue health status */
  health: 'healthy' | 'degraded' | 'failed';

  /** Last activity timestamp (null if not implemented) */
  last_activity: string | null;
}

/**
 * Instance status information
 */
export interface InstanceStatus {
  /** Instance ID */
  instance_id: string;
  
  /** Whether instance is running */
  running: boolean;
  
  /** Process ID (if running) */
  pid?: number;
  
  /** Queue statistics */
  queue_stats: QueueStats;
  
  /** Instance configuration */
  config: InstanceConfig;
}

/**
 * Error types for structured error handling
 */
export interface EfritError extends Error {
  /** Error code for programmatic handling */
  code: string;
  
  /** Instance ID where error occurred */
  instance_id?: string;
  
  /** Request ID that caused the error */
  request_id?: string;
  
  /** Additional error details */
  details?: Record<string, unknown>;
}

/**
 * Constants
 */
export const EFRIT_SCHEMA_VERSION = packageJson.version;
export const DEFAULT_TIMEOUT = 30;
export const DEFAULT_POLL_INTERVAL = 250;
export const MAX_REQUEST_SIZE = 1024 * 1024; // 1MB
export const MAX_RESPONSE_SIZE = 10 * 1024 * 1024; // 10MB
export const QUEUE_SUBDIRS = ['requests', 'processing', 'responses', 'archive'] as const;
