# Claude Code to Efrit Migration Guide

This guide helps users familiar with Claude Code (Anthropic's VS Code extension) find equivalent functionality in Efrit.

## Quick Reference

| Claude Code | Efrit Equivalent | Notes |
|------------|------------------|-------|
| Chat sidebar | `M-x efrit-chat` | Multi-turn conversations |
| Agentic mode | `M-x efrit-do` | Full tool suite (35+ tools) |
| Diff preview | `show_diff_preview` tool | Claude calls this automatically |
| Agent view | `M-x efrit-agent` | Structured session buffer |
| Progress/activity | `*Efrit Progress*` buffer | Real-time tool execution |
| Keyboard interrupt | `C-g` | Works during execution |
| Project context | Automatic | Reads project files via tools |

## Detailed Mapping

### Chat Interface

**Claude Code**: Chat sidebar with conversation history, context from open files.

**Efrit**: 
- `M-x efrit-chat` - Multi-turn conversation in a dedicated buffer
- Context comes from current buffer automatically
- Chat has limited tools (read-only, buffer-centric)

For tasks requiring file editing or shell commands, use `efrit-do` instead.

### Agentic Task Execution

**Claude Code**: "@agent" or agentic mode that can edit files, run commands.

**Efrit**:
- `M-x efrit-do` - Single command with full tool access
- `M-x efrit-agent` - Same, but with structured session view
- 35+ tools including: `edit_file`, `create_file`, `shell_exec`, `search_content`

Example:
```
M-x efrit-do RET
> refactor this function to use async/await
```

### Diff Preview

**Claude Code**: Shows diffs before applying changes.

**Efrit**: Claude uses the `show_diff_preview` tool when appropriate. Diffs appear in a preview buffer before edits are applied.

### Progress Tracking

**Claude Code**: Shows agent thinking, tool calls, results.

**Efrit**: 
- Progress buffer (`*Efrit Progress*`) shows real-time updates
- `efrit-agent` buffer shows structured view with:
  - Session status and elapsed time
  - TODO list for multi-step tasks
  - Expandable tool call log

### File Operations

| Operation | Claude Code | Efrit Tool |
|-----------|-------------|------------|
| Read file | Opens file | `read_file` |
| Edit file | Inline diff | `edit_file` |
| Create file | Creates with preview | `create_file` |
| Delete file | — | `shell_exec "rm ..."` |
| Search files | Search panel | `search_content`, `glob_files` |
| Project files | File tree | `project_files` |

### Version Control

| Operation | Claude Code | Efrit Tool |
|-----------|-------------|------------|
| Git status | Built-in | `vcs_status` |
| Git diff | Built-in | `vcs_diff` |
| Git log | Built-in | `vcs_log` |
| Git blame | Built-in | `vcs_blame` |

### Shell Commands

**Claude Code**: Can run terminal commands.

**Efrit**: `shell_exec` tool runs commands. Security controls restrict dangerous operations by default.

```
M-x efrit-do RET
> run the test suite and summarize failures
```

### Interruption

**Claude Code**: Stop button.

**Efrit**: `C-g` (keyboard-quit) interrupts the current operation.

## Features Not Yet Available

Some Claude Code features are not yet in Efrit:

| Claude Code Feature | Status |
|---------------------|--------|
| Image generation | Not available |
| Claude-to-Claude delegation | Partial (remote queue) |
| Inline completions | Not available |
| Multi-file inline diff view | Single-file diffs only |

## AI-to-AI Communication

**Claude Code**: Can spawn sub-agents.

**Efrit**: Uses file-based JSON queue for AI-to-AI communication:
- `M-x efrit-remote-queue-start` to enable
- Other AI agents (Claude Code, Cursor, etc.) can send commands
- See [mcp/README.md](../mcp/README.md) for MCP server setup

## Key Bindings

Enable Efrit's keybindings:
```elisp
(efrit-setup-keybindings)  ; Binds C-c C-e prefix
```

| Key | Command | Like Claude Code... |
|-----|---------|---------------------|
| `C-c C-e c` | `efrit-chat` | Opening chat |
| `C-c C-e d` | `efrit-do` | Running agentic command |
| `C-c C-e a` | `efrit-agent` | Agent view |
| `C-g` | Interrupt | Stop button |

## Tips for Claude Code Users

1. **Use `efrit-do` more than `efrit-chat`**: Unlike Claude Code where chat has full tools, Efrit chat is intentionally limited. Use `efrit-do` for anything beyond questions.

2. **The agent buffer is your friend**: `efrit-agent` provides visibility similar to Claude Code's agent view.

3. **Context is automatic**: Efrit reads project files as needed via tools. No need to explicitly add context.

4. **Emacs keybindings work**: You can use standard Emacs navigation in all Efrit buffers.

5. **Check diagnostics**: `M-x efrit-doctor` helps troubleshoot setup issues.

## Example Session

Claude Code workflow → Efrit equivalent:

```
# In Claude Code:
# 1. Open chat sidebar
# 2. Type: "@agent add error handling to the auth module"
# 3. Watch agent work, approve diffs

# In Efrit:
M-x efrit-agent RET
> add error handling to the auth module
# Watch progress in agent buffer
# Diffs shown via show_diff_preview tool
```
