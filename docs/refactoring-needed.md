# Functions Needing Refactoring

Based on review of the efrit codebase, the following functions exceed 80 lines and should be refactored:

## efrit-do.el

### `efrit-do--command-system-prompt` (88 lines)
- Lines: 671-759
- Issue: Long string concatenation for system prompt
- Suggested refactoring:
  - Extract example sections into separate functions
  - Create `efrit-do--command-examples` function
  - Create `efrit-do--formatting-tools-doc` function
  - Create `efrit-do--common-tasks-doc` function

### `efrit-do` (79 lines - borderline)
- Lines: 870-949
- Issue: Complex retry logic mixed with result handling
- Suggested refactoring:
  - Extract retry loop into `efrit-do--execute-with-retry`
  - Extract result processing into separate function
  - Keep main function as orchestrator

## efrit-tools.el

### `efrit-tools-system-prompt` (81 lines)
- Lines: 467-548
- Issue: Long documentation string
- Suggested refactoring:
  - Extract sections into constants or separate functions
  - Create `efrit-tools--elisp-examples` function
  - Create `efrit-tools--safety-guidelines` function

## Other Files

Most other functions are within acceptable limits. The new efrit-async.el has all functions under 40 lines, which is good.

## Priority

1. **High**: `efrit-do--command-system-prompt` - Most over limit and contains important prompt engineering
2. **Medium**: `efrit-tools-system-prompt` - Just over limit, but well-structured
3. **Low**: `efrit-do` - At the boundary, but complex logic would benefit from refactoring