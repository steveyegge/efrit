# TODO System Tests

This directory contains tests for the Efrit TODO list system that prevents infinite loops during async command execution.

## Test Files

### Core Tests
- `test-todo-system.el` - Basic TODO system functionality test
- `verify-todo-workflow.el` - Verifies the TODO workflow logic and loop prevention
- `test-todo-simulation.el` - Simulates Claude's TODO-based workflow

### Interactive Tests  
- `test-efrit-interactive.el` - Interactive test framework for manual testing
- `interactive-todo-test.el` - Interactive TODO system test with monitoring
- `test-efrit-todos.el` - Basic TODO functionality tests

### AI-Efrit Channel Tests
- `test-ai-efrit-batch.el` - Batch test for ai-efrit channel configuration
- `test-ai-efrit-todo.el` - TODO system test using ai-efrit channel

### Support Files
- `test-warnings-setup.el` - Creates test *Warnings* buffer
- `debug-todo-responses.el` - Debug tool to see what Claude sees
- `comprehensive-todo-test.el` - Comprehensive workflow simulation
- `simple-todo-test.el` - Simple TODO creation test
- `test-results-summary.el` - Test report generator

## Running Tests

### Basic Test
```elisp
(load-file "test/todo-system/verify-todo-workflow.el")
```

### Interactive Test
```elisp
(load-file "test/todo-system/test-efrit-interactive.el")
;; Then: M-x test-efrit-todo-workflow
```

### Debug What Claude Sees
```elisp
(load-file "test/todo-system/debug-todo-responses.el")
```

## What These Tests Verify

1. **Loop Prevention** - Claude doesn't get stuck calling `todo_analyze` repeatedly
2. **TODO Creation** - TODOs are created for multi-step tasks
3. **Progress Tracking** - TODOs move through states properly
4. **Session Completion** - Sessions complete when all TODOs are done
5. **Monitoring** - Progress and TODO displays work correctly