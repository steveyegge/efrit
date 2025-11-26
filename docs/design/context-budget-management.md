# Context Budget Management Design

**Issue**: ef-w3s
**Author**: Claude
**Date**: 2025-11-25
**Status**: Draft

## Problem Statement

With the expanded tool set (Phase 1-4 tools), Efrit now has tools that can return large amounts of data:

- `project_files`: Up to 500 files with metadata per call
- `search_content`: Up to 50 matches with context lines
- `read_file`: Entire file contents (potentially megabytes)
- `vcs_log`, `vcs_diff`: Unbounded git output

Without budget management, a session can easily exceed context limits by:
1. Accumulating large tool results in work log
2. Requesting broad searches/listings repeatedly
3. Reading multiple large files

## Design Goals

1. **Preserve Pure Executor Principle**: No client-side intelligence about *what* to keep
2. **Token-Aware**: Provide Claude with budget information to make informed decisions
3. **Graceful Degradation**: Large results are usable immediately, then compressed
4. **Minimal Latency**: Budget tracking shouldn't slow down tool execution
5. **Transparency**: Claude sees current budget state to plan accordingly

## Architecture Overview

```
┌─────────────────────────────────────────────────────────────────┐
│                      Context Budget Manager                      │
├─────────────────────────────────────────────────────────────────┤
│  Budget Tracker         │  Result Handler        │  Compressor  │
│  - Token estimation     │  - Immediate delivery  │  - Tiered    │
│  - Budget allocation    │  - Store for history   │    summary   │
│  - Usage monitoring     │  - Pagination support  │  - LRU evict │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                         Tool Results                             │
├──────────────────┬──────────────────┬───────────────────────────┤
│  Immediate Use   │  Work Log Entry  │  Session Context Summary  │
│  (full data)     │  (compressed)    │  (highly compressed)      │
└──────────────────┴──────────────────┴───────────────────────────┘
```

## Key Design Decisions

### 1. Token Estimation (Not Counting)

Accurate token counting requires the tokenizer. Instead, use character-based estimation:

```elisp
;; Rough estimate: 1 token ≈ 4 characters for English text
;; JSON/code tends to be ~3 chars per token
(defun efrit-budget-estimate-tokens (text)
  "Estimate token count for TEXT."
  (/ (length text) 3.5))
```

This is good enough for budget management (within 20% accuracy).

### 2. Budget Tiers

Define budget tiers based on Claude's context window:

| Tier | Tokens | Purpose |
|------|--------|---------|
| System Prompt | 8,000 | Tool schemas, instructions |
| Work History | 12,000 | Compressed work log for continuation |
| Tool Results | 40,000 | Room for current tool results |
| User Message | 10,000 | User's request + context |
| Response Buffer | 30,000 | Claude's response generation |
| **Total Target** | 100,000 | Safe buffer below 200k limit |

### 3. Tool Result Budget Allocation

Each tool call gets a budget allocation from the Tool Results tier:

```elisp
(defcustom efrit-budget-per-tool-call 8000
  "Target token budget per tool result."
  :type 'integer)
```

Tools should respect this by:
1. Pagination: Return first N items within budget
2. Truncation: Shorten long content
3. Summarization: Provide summary + detail on request

### 4. Two-Phase Result Delivery

**Phase 1: Immediate (Full)**
Tool returns complete result for Claude's immediate use. No compression.

**Phase 2: Historical (Compressed)**
When storing in work log for future continuations:

```elisp
(defun efrit-budget-compress-for-history (tool-name result)
  "Compress RESULT from TOOL-NAME for work log storage."
  (let ((budget 500))  ; Max chars for history entry
    (pcase tool-name
      ("project_files"
       ;; Store: file count, important paths, truncated flag
       (efrit-budget--compress-file-list result budget))
      ("search_content"
       ;; Store: match count, file names, first few matches
       (efrit-budget--compress-search result budget))
      ("read_file"
       ;; Store: file path, line count, first/last few lines
       (efrit-budget--compress-file-content result budget))
      (_
       ;; Generic: truncate with "..."
       (efrit-common-truncate-string result budget)))))
```

**Important**: This requires the work log to track which tool generated each result.
Currently the work log format is `(result elisp todo-snapshot)`. We need to extend this
to `(result elisp todo-snapshot tool-name)` or use a plist/alist format.

### 5. Session Budget Tracking

Track cumulative token usage across the session:

```elisp
(cl-defstruct efrit-budget
  "Token budget tracking for a session."
  (total-limit 100000)           ; Total target budget
  (system-used 0)                ; System prompt tokens
  (history-used 0)               ; Work log tokens
  (tool-results-used 0)          ; Current tool results
  (warnings '()))                ; Budget warnings

(defun efrit-budget-remaining (budget)
  "Calculate remaining token budget."
  (- (efrit-budget-total-limit budget)
     (efrit-budget-system-used budget)
     (efrit-budget-history-used budget)
     (efrit-budget-tool-results-used budget)))
```

### 6. Budget-Aware Tool Schema

Inform Claude of budget state in tool schemas:

```json
{
  "name": "project_files",
  "description": "List project files. Current budget allows ~200 files with metadata.",
  "parameters": {
    "max_files": {
      "type": "integer",
      "description": "Limit files returned (budget suggests: 200)",
      "default": 200
    }
  }
}
```

Dynamic descriptions update based on remaining budget.

### 7. Result Pagination with Continuation Tokens

Large results return pagination info:

```json
{
  "status": "success",
  "data": {
    "files": ["...first 100..."],
    "total": 2847,
    "returned": 100,
    "continuation_token": "offset:100",
    "budget_note": "Use offset=100 to continue. Budget allows ~100 more."
  }
}
```

### 8. LRU Context Eviction

When budget is tight, evict old context:

```elisp
(defun efrit-budget-evict-if-needed (budget needed-tokens)
  "Evict old work log entries if NEEDED-TOKENS exceeds budget."
  (while (and (> needed-tokens (efrit-budget-remaining budget))
              (efrit-session-work-log (efrit-session-active)))
    ;; Remove oldest entry from work log
    (efrit-session-evict-oldest-work)
    ;; Update budget tracking
    (efrit-budget-recalculate-history budget)))
```

## Implementation Plan

### Prerequisites: Work Log Format Update

**Current format** in `efrit-session-add-work`:
```elisp
(list result elisp todo-snapshot)
```

**Required format** for tool-specific compression:
```elisp
(list result elisp todo-snapshot tool-name)
;; or use alist:
`((result . ,result) (elisp . ,elisp) (todos . ,todo-snapshot) (tool . ,tool-name))
```

This change must be made to:
1. `efrit-session-add-work` - accept and store tool-name
2. `efrit-session-track-tool` - already has tool-name, could be unified
3. `efrit-session--compress-smart` and related - use tool-name for compression

### Phase 1: Budget Tracking Infrastructure

New file: `lisp/core/efrit-budget.el`

```elisp
;; Core data structures
(cl-defstruct efrit-budget ...)

;; Token estimation
(defun efrit-budget-estimate-tokens (text) ...)

;; Budget allocation
(defun efrit-budget-allocate-tool (budget tool-name) ...)

;; Usage tracking
(defun efrit-budget-record-usage (budget category tokens) ...)
```

### Phase 2: Tool Result Compression

Update each Phase 1-4 tool to:
1. Accept optional `budget` parameter
2. Respect budget in result size
3. Provide compression function for history

### Phase 3: Session Integration

Integrate budget tracking with efrit-session.el:
1. Create budget struct on session start
2. Update budget after each tool call
3. Use budget in work log compression
4. Report budget in session summary

### Phase 4: Dynamic Schema Updates

Update efrit-do.el to:
1. Calculate remaining budget before each API call
2. Update tool descriptions with budget hints
3. Inject budget warnings into system prompt when low

## Tool-Specific Compression Strategies

### project_files
```elisp
;; Full result: 500 files × ~100 chars = 50k chars
;; Compressed: "Listed 500 files in /project. Types: .el (234), .md (45)..."
;; ~200 chars for history
```

### search_content
```elisp
;; Full result: 50 matches × ~500 chars = 25k chars
;; Compressed: "Found 50 matches for 'pattern' in 12 files: file1.el (5), ..."
;; ~300 chars for history
```

### read_file
```elisp
;; Full result: entire file (potentially huge)
;; Compressed: "Read file.el (1234 lines, 45KB). Key sections: ..."
;; ~200 chars for history
```

### vcs_diff
```elisp
;; Full result: complete diff output
;; Compressed: "Diff: 5 files, +234/-89 lines. Modified: file1.el, file2.el, ..."
;; ~200 chars for history
```

## Budget Warning System

When budget runs low, include warnings:

```elisp
(defun efrit-budget-format-warning (budget)
  "Generate budget warning if needed."
  (let ((remaining (efrit-budget-remaining budget))
        (threshold 20000))
    (when (< remaining threshold)
      (format "[Budget Alert: ~%dk tokens remaining. Consider summarizing or completing soon.]"
              (/ remaining 1000)))))
```

This warning goes into the system prompt for Claude to consider.

## Configuration

```elisp
(defcustom efrit-budget-total-target 100000
  "Target total token budget for sessions."
  :type 'integer
  :group 'efrit-session)

(defcustom efrit-budget-per-tool-default 8000
  "Default token budget per tool result."
  :type 'integer
  :group 'efrit-session)

(defcustom efrit-budget-history-max 12000
  "Maximum tokens for compressed work history."
  :type 'integer
  :group 'efrit-session)

(defcustom efrit-budget-warning-threshold 20000
  "Remaining tokens that trigger budget warning."
  :type 'integer
  :group 'efrit-session)
```

## Pure Executor Compliance

This design maintains Pure Executor principle:

**What Efrit does (allowed):**
- Count/estimate token usage (mechanical)
- Truncate results to budget limits (mechanical)
- Report budget state to Claude (context gathering)
- Evict old entries when full (mechanical, LRU)

**What Efrit does NOT do (prohibited):**
- Decide what content is "important" (Claude decides)
- Analyze results for relevance (Claude does this)
- Choose which files/results to summarize (Claude requests)
- Determine when to stop (Claude calls session_complete)

Claude sees budget information and makes all decisions about:
- Whether to request more results
- When to summarize vs request details
- Which areas to explore vs skip
- When session goals are achieved

## Testing Strategy

1. **Unit tests**: Token estimation accuracy, compression functions
2. **Integration tests**: Budget tracking across multi-tool sessions
3. **Load tests**: Large repo (10k+ files), deep searches
4. **Real API tests**: Full session with budget monitoring

## Open Questions

1. **Token estimation accuracy**: Should we use a real tokenizer? (tiktoken-like library)
   - **Decision**: No. Character-based estimation is good enough for budget management.
   - Rationale: Adding a tokenizer dependency adds complexity. 20% estimation error is acceptable
     for budget warnings - we're not doing exact accounting.

2. **Per-tool budgets**: Should different tools get different allocations?
   - **Decision**: Start with uniform budgets, add per-tool config later if needed.
   - Rationale: Keep initial implementation simple. Monitor real usage to inform future changes.

3. **User override**: Allow users to increase budget at the cost of potential errors?
   - **Decision**: Yes, make `efrit-budget-total-target` a defcustom.
   - Users who want to push limits can increase it. Default is conservative.

4. **Model differences**: Claude 3 vs 4 have different context limits - make configurable?
   - **Decision**: Yes, `efrit-budget-total-target` handles this. Document recommended values.
   - Claude 3.5 Sonnet: 200k context → recommend 100k target
   - Future models with larger context can use higher values.

## Related Issues

- ef-4wr: Integration tests for new tools (can run in parallel)
- ef-3ta: Core budget module (first implementation step)
- ef-85i: Tool-specific compression (requires ef-3ta)
- ef-4in: Session integration (requires ef-3ta and ef-85i)
- ef-8nm: Dynamic schema hints (requires ef-4in, lower priority)

## Implementation Order

```
ef-3ta (core module + session format update)
    ↓
ef-85i (compression functions)
    ↓
ef-4in (session integration)
    ↓
ef-8nm (dynamic hints) [optional, P3]
```

## Revision History

- 2025-11-25: Initial design (ef-w3s)
- 2025-11-25: Added prerequisites section, resolved open questions, fixed dependency order
