# Smart Output Management Mode - Architecture Design

**Epic**: ef-3at
**Design Task**: ef-hy7
**Status**: Draft v2 (ZCI-Pure)

## Executive Summary

Transform the agent buffer from linear log display into a sophisticated output management interface with:
- Collapsed summaries by default (scannable)
- Type-aware rendering (diffs, JSON, grep results, etc.)
- Click-to-expand access to full content
- Error emphasis (auto-expand failures)
- Three display modes: minimal / smart / verbose

**Key Principle**: Zero Client-Side Intelligence. Claude provides ALL rendering decisions via the `DisplayHint` tool. Efrit is a pure executor that renders what Claude tells it to render.

## ZCI Architecture

### The Problem with Client-Side Classification

The naive approach has Efrit inspect tool results and decide how to render them:

```elisp
;; ❌ WRONG - This is client-side intelligence
(defun efrit-agent--classify-output (tool-name result)
  (cond
    ((string-match-p "^@@" result) 'diff)      ; Pattern matching = intelligence
    ((string-match-p "^{" result) 'json)       ; Decision-making = intelligence
    (t 'text)))
```

This violates ZCI because Efrit is making decisions about content.

### The Solution: Claude-Provided Display Hints

Claude sees the tool results and tells Efrit exactly how to display them:

```
┌─────────────────────────────────────────────────────────────────┐
│                        ZCI Flow                                  │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  1. Claude calls tool (e.g., Read)                              │
│     ┌──────────────────────────────────────────┐                │
│     │ {"tool": "Read",                         │                │
│     │  "input": {"file_path": "/src/foo.el"}}  │                │
│     └──────────────────────────────────────────┘                │
│                         │                                        │
│                         ▼                                        │
│  2. Efrit executes, shows placeholder:                          │
│     "⟳ Read /src/foo.el..."                                     │
│                         │                                        │
│                         ▼                                        │
│  3. Efrit returns result to Claude                              │
│                         │                                        │
│                         ▼                                        │
│  4. Claude calls DisplayHint with rendering instructions:       │
│     ┌──────────────────────────────────────────┐                │
│     │ {"tool": "DisplayHint",                  │                │
│     │  "input": {                              │                │
│     │    "tool_use_id": "toolu_abc123",        │                │
│     │    "summary": "📄 Main entry point",     │                │
│     │    "render_type": "elisp",               │                │
│     │    "auto_expand": false,                 │                │
│     │    "annotations": [                      │                │
│     │      {"line": 42, "note": "key func"}    │                │
│     │    ]                                     │                │
│     │  }}                                      │                │
│     └──────────────────────────────────────────┘                │
│                         │                                        │
│                         ▼                                        │
│  5. Efrit updates display based on hint:                        │
│     "▶ ✓ Read (0.1s) → 📄 Main entry point"                     │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

## DisplayHint Tool Schema

```json
{
  "name": "DisplayHint",
  "description": "Provide rendering instructions for a tool result. Call this after any tool to control how its output is displayed to the user.",
  "input_schema": {
    "type": "object",
    "properties": {
      "tool_use_id": {
        "type": "string",
        "description": "ID of the tool call to annotate"
      },
      "summary": {
        "type": "string",
        "description": "One-line summary for collapsed view. Be contextual, e.g., 'Found auth module' not just 'Read file'"
      },
      "render_type": {
        "type": "string",
        "enum": ["text", "diff", "elisp", "json", "shell", "grep", "markdown", "error"],
        "description": "How to syntax-highlight the expanded content"
      },
      "auto_expand": {
        "type": "boolean",
        "description": "Whether to expand by default. Use true for errors or important results"
      },
      "importance": {
        "type": "string",
        "enum": ["normal", "success", "warning", "error"],
        "description": "Visual emphasis level"
      },
      "annotations": {
        "type": "array",
        "items": {
          "type": "object",
          "properties": {
            "line": {"type": "integer"},
            "note": {"type": "string"}
          }
        },
        "description": "Optional line-level annotations for the expanded view"
      }
    },
    "required": ["tool_use_id", "summary"]
  }
}
```

## Why This Is Better

### 1. Contextual Summaries

Client-side can only produce generic summaries:
```
📄 src/auth.el (150 lines)
```

Claude can produce contextual summaries:
```
📄 Found the authentication module - uses JWT tokens
```

### 2. Semantic Understanding

Client-side doesn't know if a diff is important:
```
✏️ src/config.el (+2 -1)  ← same treatment for all diffs
```

Claude knows what matters:
```
✏️ Fixed the memory leak in session cleanup  ← auto_expand: true, importance: success
```

### 3. Cross-Tool Context

Claude can reference earlier context:
```
🔍 Found 3 uses of deprecated API (as discussed above)
```

### 4. Error Explanation

Client-side just shows the error:
```
❌ Permission denied: /etc/passwd
```

Claude explains and suggests:
```
❌ Can't read system file - try using sudo or check file permissions
```

## Efrit's Role: Pure Renderer

Efrit only does mechanical rendering based on Claude's instructions:

```elisp
(defun efrit-agent--apply-display-hint (hint)
  "Apply HINT to update tool display. No decision-making, just execution."
  (let* ((tool-id (plist-get hint :tool_use_id))
         (summary (plist-get hint :summary))
         (render-type (plist-get hint :render_type))
         (auto-expand (plist-get hint :auto_expand))
         (importance (plist-get hint :importance)))
    ;; Find the tool region
    (when-let ((region (efrit-agent--find-tool-region tool-id)))
      ;; Update summary text
      (efrit-agent--set-tool-summary tool-id summary)
      ;; Set render type for expansion
      (efrit-agent--set-tool-render-type tool-id render-type)
      ;; Apply importance styling
      (efrit-agent--set-tool-importance tool-id importance)
      ;; Auto-expand if instructed
      (when auto-expand
        (efrit-agent--expand-tool tool-id)))))
```

### Render Type → Syntax Highlighting

The `render_type` maps to Emacs major modes for syntax highlighting:

| render_type | Major Mode | Notes |
|-------------|-----------|-------|
| `text` | `fundamental-mode` | Plain text, no highlighting |
| `diff` | `diff-mode` | +/- line coloring |
| `elisp` | `emacs-lisp-mode` | Lisp syntax |
| `json` | `json-mode` | JSON structure |
| `shell` | `shell-mode` | Command output with ANSI |
| `grep` | `grep-mode` | Clickable file:line refs |
| `markdown` | `markdown-mode` | Headers, code blocks |
| `error` | N/A | Red emphasis, recovery buttons |

### Importance → Visual Styling

| importance | Face | Icon |
|------------|------|------|
| `normal` | default | ✓ |
| `success` | green | ✓ |
| `warning` | yellow | ⚠ |
| `error` | red | ✗ |

## Display Modes (User Preference, Not Intelligence)

Display modes are user preferences that affect the *default* behavior. They don't involve Efrit making decisions about content:

| Mode | Behavior |
|------|----------|
| **minimal** | All collapsed, ignore `auto_expand` hints |
| **smart** | Respect Claude's `auto_expand` hints |
| **verbose** | All expanded, ignore `auto_expand` hints |

```elisp
(defcustom efrit-agent-display-mode 'smart
  "How to handle Claude's auto_expand hints.
- minimal: Always collapsed (user must expand manually)
- smart: Respect Claude's auto_expand recommendations
- verbose: Always expanded"
  :type '(choice (const minimal) (const smart) (const verbose))
  :group 'efrit-agent)
```

This is configuration, not intelligence. Efrit isn't deciding what to expand—it's applying a user preference to Claude's recommendations.

## Handling Missing Hints

If Claude doesn't call DisplayHint for a tool, Efrit uses minimal defaults:

```elisp
(defun efrit-agent--default-display (tool-name result success-p elapsed)
  "Default display when no DisplayHint received. Minimal, no intelligence."
  (let ((status-icon (if success-p "✓" "✗"))
        (truncated (truncate-string-to-width
                    (replace-regexp-in-string "\n" " " result)
                    50 nil nil "…")))
    (format "%s %s (%.1fs) → %s" status-icon tool-name elapsed truncated)))
```

This is just string truncation, not content classification.

## State Management

Expansion state tracking remains—it's user interaction state, not intelligence:

```elisp
(defvar-local efrit-agent--expansion-state (make-hash-table :test 'equal)
  "Hash table: tool-id → expansion state.
Values: nil (use mode default), t (user expanded), 'collapsed (user collapsed)")
```

When user toggles with RET, their preference overrides Claude's hint and the display mode.

## System Prompt Addition

Add to Claude's system prompt:

```
## Tool Result Display

After executing tools, use DisplayHint to control how results are shown to the user:

- Provide contextual summaries (e.g., "Found the config file" not just "Read config.yaml")
- Set render_type for syntax highlighting (diff, elisp, json, shell, grep, markdown)
- Use auto_expand: true for errors or important results the user should see immediately
- Set importance to highlight successes, warnings, or errors

Example:
After reading a file:
  DisplayHint(tool_use_id="...", summary="📄 Main entry point for the app", render_type="elisp")

After a failed edit:
  DisplayHint(tool_use_id="...", summary="❌ File is read-only", render_type="error", auto_expand=true, importance="error")
```

## Implementation Tasks (Revised)

### Phase 1: DisplayHint Infrastructure (P1)

1. **ef-XXX: Add DisplayHint tool**
   - Add tool schema to efrit-do-schema.el
   - Implement handler in efrit-tools
   - Wire up to agent buffer display update

2. **ef-XXX: Tool result placeholder display**
   - Show "⟳ ToolName..." while executing
   - Update when DisplayHint arrives
   - Fall back to default if no hint after timeout

3. **ef-XXX: System prompt for DisplayHint**
   - Add DisplayHint usage instructions to tools prompt
   - Include examples of good summaries
   - Document render_type options

### Phase 2: Renderers (P2)

4. **ef-XXX: Render type dispatch**
   - Map render_type to major-mode for highlighting
   - Apply syntax highlighting in expanded view
   - Handle unknown types gracefully (fall back to text)

5. **ef-XXX: Importance styling**
   - Define faces for normal/success/warning/error
   - Apply to tool summary line
   - Update icon based on importance

6. **ef-XXX: Annotation support**
   - Render line-level annotations in expanded view
   - Overlay notes at specified line numbers
   - Clickable annotation markers

### Phase 3: Polish (P2)

7. **ef-XXX: Display mode toggle**
   - Add `M` keybinding to cycle modes
   - Show mode in header-line
   - Apply mode to auto_expand behavior

8. **ef-XXX: Persistent expansion state**
   - Track user-toggled expansion/collapse
   - Override Claude hints and display mode
   - Preserve across buffer updates

9. **ef-XXX: Expand/collapse all commands**
   - `E` for expand all
   - `C` for collapse all
   - Update expansion state hash

## Open Questions

1. **Hint timing**: What if DisplayHint arrives before tool result display is ready?
   - Solution: Queue hints, apply when tool region exists

2. **Hint for streaming tools**: How to handle Bash with long-running output?
   - Solution: Allow DisplayHint updates (summary can change)

3. **Multiple hints per tool**: Allow or reject?
   - Proposal: Allow, last hint wins (enables progressive updates)

4. **Missing hints**: How long to wait before applying defaults?
   - Proposal: Apply defaults immediately, update if hint arrives

## Success Criteria

- [ ] DisplayHint tool implemented and documented
- [ ] Claude provides contextual summaries (verified in testing)
- [ ] Render types produce correct syntax highlighting
- [ ] Display modes work as specified
- [ ] User toggle overrides all automatic behavior
- [ ] No client-side content classification or decision-making
- [ ] Buffer remains scannable with 10+ tool calls
