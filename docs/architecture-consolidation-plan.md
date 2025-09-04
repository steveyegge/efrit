# Efrit Architecture Consolidation Plan

## Overview

This document outlines the plan to consolidate Efrit's multiple command interfaces into a unified, two-phase execution model that lets Claude decide what context it needs.

## Current State Analysis

### Existing Systems
1. **efrit-chat** - Interactive conversation interface (like ChatGPT)
   - Working well, no changes needed
   - Good for exploratory discussions and learning

2. **efrit-do** - Direct command execution
   - Most mature system with retry logic, context tracking, TODO management
   - Will become the primary command interface

3. **efrit** - Agent-based command interface
   - Uses popup buffer UI and complex agent system
   - Redundant with efrit-do functionality
   - To be deprecated

### Key Design Principles
- **No client-side cognition** - Claude makes all decisions about context needs
- **Single protocol** - Everything is elisp expressions
- **Progressive refinement** - Can request additional context as needed
- **User control** - Universal arguments for explicit context control

## Proposed Architecture: Two-Phase Elisp Protocol

### Conceptual Flow

```elisp
;; Phase 1: Context Discovery
;; User: "refactor this function to use async/await"
;; Claude returns elisp to gather needed context:
(list :current-function (efrit-context-function-at-point)
      :buffer-mode major-mode
      :imports (efrit-context-find-imports))

;; Phase 2: Execution  
;; Claude receives context and returns elisp to execute:
(progn 
  (efrit-refactor-function-to-async 
    (alist-get :current-function context))
  ...)
```

### Benefits
1. **Symmetric protocol** - Both phases use elisp expressions
2. **Optimal context** - Claude gets exactly what it needs, no more
3. **Flexibility** - Can gather from buffers, files, shell commands, etc.
4. **Composability** - Context functions can build on each other

## Implementation Plan

### Session 1: Architecture Cleanup

#### 1.1 Deprecate Redundant Systems
- Mark `efrit` command as deprecated alias to `efrit-do`
- Archive efrit-agent.el (move to archived/ directory)
- Simplify efrit-command.el to redirect to efrit-do
- Add deprecation warnings with migration guidance

#### 1.2 Refactor efrit-do Structure
```elisp
;; New structure for efrit-do.el
(defun efrit-do (command &optional skip-context-phase)
  "Execute COMMAND with intelligent context gathering."
  ...)

(defun efrit-do--context-phase (command)
  "Ask Claude what context is needed for COMMAND."
  ...)

(defun efrit-do--execution-phase (command context)
  "Execute COMMAND with gathered CONTEXT."
  ...)
```

#### 1.3 Create Context Utilities
New file: `efrit-context.el`
```elisp
;; Buffer context
(defun efrit-context-function-at-point () ...)
(defun efrit-context-buffer-contents () ...)
(defun efrit-context-visible-region () ...)

;; Project context
(defun efrit-context-project-structure () ...)
(defun efrit-context-find-imports () ...)
(defun efrit-context-related-files () ...)

;; System context
(defun efrit-context-git-status () ...)
(defun efrit-context-recent-errors () ...)
(defun efrit-context-installed-packages () ...)
```

### Session 2: Protocol Implementation

#### 2.1 Two-Phase Protocol Core
```elisp
(defun efrit-do--context-phase (command)
  "First phase: Ask Claude what context is needed."
  (let ((response (efrit-do--api-request
                   (format "What context do you need to: %s

Return an elisp expression that gathers the needed context.
Available functions:
- (efrit-context-function-at-point)
- (efrit-context-buffer-contents)
- (efrit-context-project-structure)
... etc

Example response:
(list :buffer (buffer-name)
      :function (efrit-context-function-at-point)
      :mode major-mode)" 
                          command))))
    (efrit-do--extract-elisp response)))

(defun efrit-do--execution-phase (command context)
  "Second phase: Execute with context."
  (let ((response (efrit-do--api-request
                   (format "Execute: %s

Context:
%S

Return elisp to execute the command."
                          command context))))
    (efrit-tools-eval-sexp (efrit-do--extract-elisp response))))
```

#### 2.2 Update System Prompts
- Add two-phase protocol documentation
- Include context function reference
- Provide examples of both phases

#### 2.3 Progressive Context Refinement
- Allow execution phase to return special marker for "need more context"
- Implement context caching between phases
- Support multi-round context gathering if needed

### Session 3: User Interface Polish

#### 3.1 Universal Argument Behavior
```elisp
;; M-x efrit-do          → Let Claude decide context
;; C-u M-x efrit-do      → Force include current buffer context  
;; C-u C-u M-x efrit-do  → Skip context phase entirely (backward compat)
```

#### 3.2 User Feedback
- "Gathering context..." message during phase 1
- Show what context is being collected
- Progress indication for long operations
- Clear indication of phase transitions

#### 3.3 Optimization
- Cache frequently used context patterns
- Fast path for obvious buffer operations
- Batch multiple context requests
- Lazy context evaluation

### Session 4: Testing and Migration

#### 4.1 Test Suite
Test categories:
- Buffer-specific operations ("fix this function")
- General operations ("create a new project")
- Context edge cases (large files, missing context)
- Backward compatibility
- Error handling in both phases

#### 4.2 Migration Guide
- Update all uses of deprecated `efrit` command
- Convert existing prompts to two-phase aware
- Document common patterns and examples
- Provide troubleshooting guide

#### 4.3 Performance Optimization
- Benchmark context gathering overhead
- Optimize elisp evaluation performance
- Consider async context gathering
- Implement context size budgets

## Success Metrics

1. **Unified Interface** - Single efrit-do command for all operations
2. **Intelligent Context** - Claude determines optimal context
3. **Performance** - Minimal overhead from two-phase approach
4. **Compatibility** - Existing workflows continue to function
5. **User Satisfaction** - Fewer errors, better results

## Open Questions

1. **Deprecation strategy**: Preserve `efrit` as alias or remove completely?
2. **Context limits**: How to handle very large context requests?
3. **Async operations**: Should context gathering be asynchronous?
4. **Fallback behavior**: What if context phase fails?
5. **Caching strategy**: How long to cache context between phases?

## Timeline Estimate

- Session 1: 2-3 hours (cleanup and refactoring)
- Session 2: 3-4 hours (protocol implementation)
- Session 3: 2-3 hours (UI and optimization)
- Session 4: 2-3 hours (testing and documentation)

Total: 9-13 hours across 4 focused sessions

## Next Steps

1. Review and approve this plan
2. Create feature branch for consolidation work
3. Begin with Session 1 deprecation and cleanup
4. Implement minimal two-phase prototype
5. Iterate based on testing results