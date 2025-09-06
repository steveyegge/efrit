# Efrit Quick Start Guide

## Basic Commands

### Synchronous Execution (Blocking)
Use when you want immediate results:
```
M-x efrit-do
Command: explain what buffer I'm in
```

### Asynchronous Execution (Non-blocking) 
Use for long-running or multi-step operations:
```
M-x efrit-do-async
Command: analyze all elisp files in this project and summarize their purpose
```

### Let Claude Decide
When unsure, let Claude choose the best execution mode:
```
M-x efrit-unified-do
Command: fetch weather for Seattle and show it in a new buffer
```

## Key Bindings

After loading Efrit, use `C-c C-e` followed by:
- `c` - Chat interface (multi-turn conversation)
- `d` - Do command (synchronous)
- `D` - Do command (asynchronous) 
- `u` - Unified (Claude decides)
- `s` - Streamlined chat
- `A` - Show async status
- `S` - Show unified status

## Performance Features

### View Statistics
```
M-x efrit-performance-show-stats
```
Shows API call times and performance metrics.

### Clear Cache
```
M-x efrit-performance-clear-cache
```
Clears cached responses to force fresh API calls.

## Session Management

### Check Active Sessions
```
M-x efrit-async-status
```
Shows current session and queue status.

### Cancel Active Session
```
M-x efrit-async-cancel
```
Cancels the current async operation.

## Tips

1. **Use async for multi-step tasks**: Operations that require multiple tool calls work better async
2. **Use sync for quick queries**: Simple questions are faster with sync execution
3. **Let Claude decide when unsure**: The unified interface picks the optimal mode
4. **Cached responses are fast**: Identical commands within 5 minutes use cached results
5. **Queue commands**: Multiple async commands automatically queue for processing

## Examples

### Quick Information (Sync)
```elisp
(efrit-do "what's the current buffer name?")
```

### File Operations (Async)
```elisp
(efrit-do-async "create test files for all elisp files in lisp/")
```

### Let Claude Choose (Unified)
```elisp
(efrit-unified-do "refactor this function to be more efficient")
```

## Troubleshooting

### Command Not Available
If commands aren't available after loading:
```elisp
(require 'efrit)
```

### Check API Connection
```elisp
(efrit-do "hello")
```

### View Debug Logs
```elisp
(setq efrit-debug-mode t)
```

### Performance Issues
Check statistics and clear cache if needed:
```elisp
(efrit-performance-show-stats)
(efrit-performance-clear-cache)
```