/**
 * Zod validation schemas for MCP tool parameters
 * Oracle recommendation: Type-safe validation for MCP tool calls
 */

import { z } from 'zod';
import { EfritExecuteParams, EfritListInstancesParams } from './types.js';

/**
 * Schema for efrit_execute parameters
 */
export const EfritExecuteParamsSchema = z.object({
  type: z.enum(['command', 'eval', 'chat']),
  content: z.string().min(1, 'Content cannot be empty'),
  instance_id: z.string().optional(),
  timeout: z.number().positive('Timeout must be positive').optional(),
  return_context: z.boolean().optional()
}).strict(); // Oracle recommendation: prevent additional properties

/**
 * Schema for efrit_list_instances parameters  
 */
export const EfritListInstancesParamsSchema = z.object({
  include_stats: z.boolean().optional()
}).strict();

/**
 * Type-safe validation helper
 */
export function validateEfritExecuteParams(params: unknown): EfritExecuteParams {
  const validated = EfritExecuteParamsSchema.parse(params);
  return {
    type: validated.type,
    content: validated.content,
    ...(validated.instance_id !== undefined && { instance_id: validated.instance_id }),
    ...(validated.timeout !== undefined && { timeout: validated.timeout }),
    ...(validated.return_context !== undefined && { return_context: validated.return_context })
  };
}

export function validateEfritListInstancesParams(params: unknown): EfritListInstancesParams {
  const validated = EfritListInstancesParamsSchema.parse(params);
  return {
    ...(validated.include_stats !== undefined && { include_stats: validated.include_stats })
  };
}
