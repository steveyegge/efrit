/**
 * Type definitions for Efrit MCP Server
 */

export interface EfritRequest {
  id: string;
  type: 'command' | 'eval' | 'chat';
  content: string;
  options?: {
    timeout?: number;
    return_context?: boolean;
  };
}

export interface EfritResponse {
  id: string;
  timestamp: string;
  status: 'success' | 'error' | 'timeout';
  result?: string;
  error?: string;
  execution_time?: number;
  context?: EfritContext;
}

export interface EfritContext {
  buffers?: string[];
  current_buffer?: string;
  cursor_position?: number;
  emacs_version?: string;
  working_directory?: string;
}

export interface EfritInstance {
  id: string;
  status: 'active' | 'inactive' | 'error';
  queue_dir: string;
  daemon_name?: string;
  workspace_dir?: string;
  last_activity?: string;
  uptime_seconds?: number;
}

export interface QueueStats {
  requests_processed: number;
  requests_succeeded: number;
  requests_failed: number;
  currently_processing: number;
  average_processing_time: number;
  uptime_seconds: number;
}

export interface ServerConfig {
  instances: Record<string, InstanceConfig>;
  polling_interval_ms: number;
  max_concurrent_requests: number;
  request_timeout_seconds: number;
  cleanup_interval_minutes: number;
}

export interface InstanceConfig {
  queue_dir: string;
  daemon_name?: string;
  workspace_dir?: string;
  auto_start?: boolean;
  priority?: number;
  timeout?: number;
}
