# Smart Output Management Mode - Architecture Design

**Epic**: ef-3at
**Design Task**: ef-hy7
**Status**: Draft v1

## Executive Summary

Transform the agent buffer from linear log display into a sophisticated output management interface with:
- Collapsed summaries by default (scannable)
- Type-aware rendering (diffs, JSON, grep results, etc.)
- Click-to-expand access to full content
- Error emphasis (auto-expand failures)
- Three display modes: minimal / smart / verbose

## Current State Analysis

The agent buffer already has foundational mechanisms:

| Feature | Current State | Gap |
|---------|--------------|-----|
| Expansion/collapse | ✅ RET toggles `efrit-tool-expanded` | Works but limited |
| Diff highlighting | ✅ Basic diff face detection | Good foundation |
| Code blocks | ✅ Markdown fence detection | Needs syntax highlighting |
| Verbosity levels | ✅ minimal/normal/verbose | Per-item, not global mode |
| Error recovery | ✅ Retry/Skip/MakeWritable buttons | Good |
| Streaming | ✅ Character-by-character | Good |

**Key gaps to address:**
1. No output type classification system
2. No type-specific renderers (grep, JSON, file listings)
3. No smart summarization ("3 files modified")
4. No auto-expand for errors / auto-collapse for success
5. No global display mode toggle
6. No persistent expansion state

## Architecture

### 1. Output Type Registry

```elisp
(defvar efrit-agent-output-types
  '((Read       . file-content)
    (Edit       . diff)
    (Write      . file-content)
    (Grep       . grep-results)
    (Glob       . file-list)
    (Bash       . shell-output)
    (WebFetch   . web-content)
    (WebSearch  . search-results)
    (Task       . subagent-result)
    (TodoWrite  . todo-list)
    (AskUserQuestion . question))
  "Map tool names to output types for rendering.")
```

### 2. Summarizer Functions

Each output type has a summarizer that produces the collapsed one-liner:

| Type | Collapsed Summary Example |
|------|--------------------------|
| `file-content` | `📄 src/foo.el (120 lines)` |
| `diff` | `✏️ src/foo.el (+15 -7)` |
| `grep-results` | `🔍 23 matches in 5 files` |
| `file-list` | `📁 Found 12 files` |
| `shell-output` | `$ make test (exit 0, 2.3s)` |
| `error` | `❌ Permission denied: /etc/passwd` |

```elisp
(defun efrit-agent--summarize-output (type tool-name result success-p)
  "Generate one-line summary for TYPE from RESULT."
  (pcase type
    ('file-content (efrit-agent--summarize-file-content result))
    ('diff         (efrit-agent--summarize-diff result))
    ('grep-results (efrit-agent--summarize-grep result))
    ('file-list    (efrit-agent--summarize-file-list result))
    ('shell-output (efrit-agent--summarize-shell result success-p))
    ('error        (efrit-agent--summarize-error result))
    (_             (truncate-string-to-width result 60 nil nil "…"))))
```

### 3. Renderer Plugin System

Each type has a dedicated renderer for expanded view:

```elisp
(defvar efrit-agent-renderers
  '((file-content  . efrit-agent--render-file-content)
    (diff          . efrit-agent--render-diff)
    (grep-results  . efrit-agent--render-grep)
    (file-list     . efrit-agent--render-file-list)
    (shell-output  . efrit-agent--render-shell)
    (web-content   . efrit-agent--render-web)
    (subagent-result . efrit-agent--render-subagent)
    (error         . efrit-agent--render-error))
  "Map output types to renderer functions.")
```

### 4. Display Modes

Three global display modes that control default behavior:

| Mode | Default Collapse | Auto-Expand Errors | Show Summaries |
|------|------------------|-------------------|----------------|
| **minimal** | All collapsed | No | Shortest possible |
| **smart** | Success collapsed, errors expanded | Yes | Context-aware |
| **verbose** | All expanded | Yes | Full detail |

```elisp
(defcustom efrit-agent-display-mode 'smart
  "Output display mode: minimal, smart, or verbose."
  :type '(choice (const minimal) (const smart) (const verbose))
  :group 'efrit-agent)
```

### 5. State Management

```elisp
;; Persistent expansion state (survives re-render)
(defvar-local efrit-agent--expansion-state (make-hash-table :test 'equal)
  "Hash table: tool-id → expansion state (t, nil, or 'user-set).")

;; Cache rendered content for performance
(defvar-local efrit-agent--render-cache (make-hash-table :test 'equal)
  "Hash table: (tool-id . type) → rendered string.")
```

**State transitions:**
- Tool starts: not in hash (default to mode-based behavior)
- Tool completes: auto-expand if error + smart/verbose mode
- User toggles: set to 'user-set, never auto-change again
- Re-render: preserve 'user-set, recalculate others

### 6. Integration Flow

```
Tool Result Arrives
        │
        ▼
┌───────────────────┐
│ Classify Output   │  efrit-agent--classify-output(tool-name, result)
│ → output-type     │
└───────────────────┘
        │
        ▼
┌───────────────────┐
│ Determine State   │  Check expansion-state hash
│ → expanded?       │  If not set: use display-mode + success-p
└───────────────────┘
        │
        ├─── collapsed ───┐
        │                 │
        ▼                 ▼
┌───────────────────┐ ┌───────────────────┐
│ Summarize         │ │ Render Full       │
│ (one-liner)       │ │ (type-specific)   │
└───────────────────┘ └───────────────────┘
        │                 │
        └────────┬────────┘
                 ▼
        ┌───────────────────┐
        │ Insert in Buffer  │
        │ with properties   │
        └───────────────────┘
```

## Type-Specific Rendering Details

### file-content (Read/Write results)

**Collapsed:**
```
  ▶ ✓ Read (0.1s) → 📄 lisp/efrit.el (245 lines)
```

**Expanded:**
```elisp
  ▼ ✓ Read (0.1s) → 📄 lisp/efrit.el (245 lines)
     ┌────────────────────────────────────────────
     │  1 ;;; efrit.el --- AI coding assistant
     │  2
     │  3 (require 'efrit-core)
     │ ...
     │245 ;;; efrit.el ends here
     └────────────────────────────────────────────
```

- Syntax highlighting via `font-lock` for file extension
- Line numbers
- Clickable path opens file

### diff (Edit results)

**Collapsed:**
```
  ▶ ✓ Edit (0.3s) → ✏️ lisp/efrit.el (+12 -5)
```

**Expanded:**
```diff
  ▼ ✓ Edit (0.3s) → ✏️ lisp/efrit.el (+12 -5)
     ┌────────────────────────────────────────────
     │ @@ -10,5 +10,12 @@
     │  (defun efrit-foo ()
     │-  (message "old"))
     │+  (message "new")
     │+  (efrit-bar))
     └────────────────────────────────────────────
```

- Green/red highlighting for +/-
- Hunk headers in distinct color
- Click on file path to jump to location

### grep-results (Grep results)

**Collapsed:**
```
  ▶ ✓ Grep (1.2s) → 🔍 23 matches in 5 files
```

**Expanded:**
```
  ▼ ✓ Grep (1.2s) → 🔍 23 matches in 5 files
     ┌────────────────────────────────────────────
     │ lisp/efrit.el:
     │   42: (defun efrit-do ...)
     │   87: (efrit-do-internal ...)
     │
     │ lisp/efrit-tools.el:
     │   15: (require 'efrit-do)
     │ ...
     └────────────────────────────────────────────
```

- Grouped by file
- Line numbers clickable (jump to location)
- Match text highlighted

### shell-output (Bash results)

**Collapsed:**
```
  ▶ ✓ Bash (2.1s) → $ make compile (exit 0)
  ▶ ✗ Bash (0.5s) → $ npm test (exit 1)  ← auto-expanded if smart mode
```

**Expanded:**
```
  ▼ ✓ Bash (2.1s) → $ make compile (exit 0)
     ┌────────────────────────────────────────────
     │ $ make compile
     │ Compiling lisp/efrit.el...
     │ Compiling lisp/efrit-tools.el...
     │ Done.
     └────────────────────────────────────────────
```

- Command echoed at top
- ANSI color codes converted to faces
- stderr in warning face if present
- Exit code prominent

### error (Failed tools)

**Always expanded in smart/verbose mode:**
```
  ▼ ✗ Edit (0.1s) → ❌ File not found: /tmp/missing.el
     ┌────────────────────────────────────────────
     │ Error: File not found
     │ Path: /tmp/missing.el
     │
     │ [Retry] [Skip] [Abort]
     └────────────────────────────────────────────
```

## UI Controls

### Mode Toggle

Keybinding `M` cycles through modes:
```
minimal → smart → verbose → minimal
```

Header-line shows current mode:
```
[efrit] working 2:34 | smart mode | 5 tools
```

### Expand/Collapse All

- `E` - Expand all tool results
- `C` - Collapse all tool results
- Both set `user-set` flag to prevent auto-changes

### Individual Toggle

- `RET` on tool line - toggle that tool
- Sets `user-set` flag for that tool

## Implementation Tasks

### Phase 1: Foundation (P1)

1. **ef-XXX: Output type classification system**
   - Add `efrit-agent-output-types` registry
   - Implement `efrit-agent--classify-output`
   - Add type detection from tool result content

2. **ef-XXX: Summarizer framework**
   - Implement summarizer dispatch
   - Create summarizers for: file-content, diff, shell-output, error
   - Parse diff to extract +/- counts
   - Parse grep to count matches/files

3. **ef-XXX: Display mode infrastructure**
   - Add `efrit-agent-display-mode` custom variable
   - Add mode toggle keybinding `M`
   - Show mode in header-line
   - Implement mode-based default expansion

### Phase 2: Renderers (P2)

4. **ef-XXX: File content renderer**
   - Syntax highlighting via major-mode
   - Line numbers
   - Clickable file path

5. **ef-XXX: Diff renderer enhancement**
   - Improve current diff faces
   - Add unified diff parsing
   - Show file stats (+N -M) in header

6. **ef-XXX: Grep results renderer**
   - Parse ripgrep output format
   - Group by file
   - Clickable line numbers
   - Match highlighting

7. **ef-XXX: Shell output renderer**
   - ANSI to Emacs face conversion
   - Command echo at top
   - Exit code styling

### Phase 3: Polish (P2)

8. **ef-XXX: Persistent expansion state**
   - `efrit-agent--expansion-state` hash table
   - Track user-set vs auto-set
   - Preserve across re-renders

9. **ef-XXX: Expand/collapse all commands**
   - `E` for expand all
   - `C` for collapse all
   - Respect user-set on individual toggle

10. **ef-XXX: Error auto-expansion**
    - Auto-expand failed tools in smart/verbose mode
    - Visual emphasis for errors
    - Never auto-collapse user-expanded items

## Open Questions

1. **Side panel vs inline?** Current design is inline expansion. Should we consider a side panel for very long outputs?

2. **Render cache invalidation?** When should cached rendered content be cleared?

3. **Maximum expanded height?** Should we cap expanded content and provide scroll-within-expansion?

4. **Copy support?** How to copy just the tool output without the chrome?

## Success Criteria

- [ ] Buffer remains scannable with 10+ tool calls (collapsed by default)
- [ ] RET expands to full type-specific rendering
- [ ] Errors are visually prominent and auto-expanded
- [ ] Mode toggle cycles minimal → smart → verbose
- [ ] Expansion state persists across re-renders
- [ ] No performance regression on buffer updates
