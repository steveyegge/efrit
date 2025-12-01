# Efrit Chat Transparency Features

This document describes the transparency features added to efrit-chat to improve visibility into Claude's reasoning and tool execution process.

## Overview

Efrit now provides three key transparency features:

1. **Tool Call Visibility** - See what tools Claude is calling and their parameters
2. **Incremental Response Display** - Responses appear gradually like a streaming interface
3. **Tool Result Display** - See the results of tool execution immediately

These features make conversations feel more natural and help users understand what Claude is doing behind the scenes.

## Features

### 1. Visible Thinking/Reasoning (Extended Thinking Models)

When using extended thinking models (Claude Opus, Claude Sonnet with extended thinking enabled), Claude's thinking process is displayed to you in the chat. This provides transparency into Claude's reasoning before tool execution and response formulation.

Example display:
```
💭 Thinking: Let me analyze this request carefully. The user is asking for
a solution that involves multiple steps. First, I should check the current
state of the system...
```

**Configuration:**
- `efrit-show-thinking` (default: `t`) - Enable/disable thinking display

### 2. Tool Call Visibility

When enabled, efrit displays each tool call as Claude makes it, showing:
- The tool name (e.g., `eval_sexp`, `read_buffer`)
- The input parameters (truncated if too long)
- Visual indication with arrow and bold formatting

Example display:
```
  → Calling tool: eval_sexp
    Input: ((expr . "(+ 1 2)"))
    Result: 3
```

**Configuration:**
- `efrit-show-tool-calls` (default: `t`) - Enable/disable tool call display
- `efrit-show-tool-results` (default: `t`) - Enable/disable tool result display

### 3. Incremental Response Display

Responses are displayed incrementally, character by character, similar to streaming chat interfaces. This makes the response feel more alive and allows you to start reading before the entire response is ready.

The incremental display works by:
- Breaking text into chunks of `efrit-response-chunk-size` characters
- Displaying each chunk with a small delay between them
- Creating a streaming effect without actual server-side streaming

**Configuration:**
- `efrit-incremental-responses` (default: `t`) - Enable/disable incremental display
- `efrit-response-chunk-size` (default: `50`) - Characters per chunk (smaller = more streaming-like, larger = faster)
- `efrit-incremental-delay` (default: `0.01`) - Delay in seconds between chunks

Example:
```elisp
;; Fast display (instant with chunking)
(setq efrit-response-chunk-size 200)
(setq efrit-incremental-delay 0)

;; Streaming-like display (slower, more like GPT)
(setq efrit-response-chunk-size 20)
(setq efrit-incremental-delay 0.05)
```

### 4. Visual Hierarchy

Different types of content are displayed with different faces (colors/styles):

- **Assistant responses** - `efrit-assistant-face` (inherited from `font-lock-doc-face`)
- **Tool calls** - `efrit-tool-call-face` (inherited from `font-lock-function-name-face`, bold)
- **Tool results** - `efrit-tool-result-face` (inherited from `font-lock-doc-face`)
- **Thinking/reasoning** - `efrit-thinking-face` (inherited from `font-lock-comment-face`, italic)

You can customize these faces by setting them in your `init.el`:
```elisp
(set-face-foreground 'efrit-tool-call-face "#ff6b6b")
(set-face-foreground 'efrit-tool-result-face "#51cf66")
```

## Implementation Details

### Files Modified

- **lisp/core/efrit-chat-transparency.el** (new) - Core transparency functions
- **lisp/core/efrit-chat-api.el** - Updated to call transparency functions

### Key Functions

```elisp
(efrit-transparency--display-tool-call tool-name tool-input)
  ; Display a tool call before execution

(efrit-transparency--display-tool-result tool-name result)
  ; Display the result of tool execution

(efrit-transparency--display-incremental text)
  ; Display response text incrementally
```

## Examples

### Disable tool visibility (legacy behavior)
```elisp
(setq efrit-show-tool-calls nil)
(setq efrit-show-tool-results nil)
```

### Disable incremental responses
```elisp
(setq efrit-incremental-responses nil)
```

### Maximum streaming feel
```elisp
(setq efrit-response-chunk-size 10)
(setq efrit-incremental-delay 0.02)
```

## Future Enhancements

Potential improvements to transparency features:

1. **Thinking/reasoning display** - Show Claude's multi-turn reasoning process
2. **Conversation branching** - Show when Claude asks clarifying questions
3. **Tool execution times** - Display how long each tool takes
4. **Error recovery** - Show when Claude retries failed tool calls
5. **Token usage** - Display token count for each message
6. **Conversation history** - Visual timeline of multi-turn conversations

## Related Issues

- [ef-4iy](https://beads.local/ef-4iy) - Missing conversation flow and transparency features
- [ef-gcq](https://beads.local/ef-gcq) - Async response handling in batch mode
- [ef-z8d](https://beads.local/ef-z8d) - Error recovery and rate limiting display

## See Also

- [README.md](../README.md) - User documentation
- [ARCHITECTURE.md](../ARCHITECTURE.md) - Design principles
