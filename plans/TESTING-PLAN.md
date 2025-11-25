# Efrit Testing Plan: Road to Conversational Agents

**Epic**: ef-6sv - Manual Testing Plan: Efrit-do-async Agent Capability
**Related Epic**: ef-ehx - Conversational Efrit: Progress Visibility and User Interaction
**Created**: 2025-11-24
**Updated**: 2025-11-24
**Status**: Active

## Overview

Progressive testing plan to exercise `efrit-do-async` capabilities, building from simple smoke tests to Claude Code-level agent tasks and ultimately **conversational interaction** where users can observe and guide Efrit as it works.

### North Star Vision

The ultimate goal is to interact with Efrit like you interact with Claude Code:
- **See progress** as it happens (tool calls, Claude's thinking)
- **Inject guidance** mid-task ("Actually, skip the test files")
- **Pause and review** at checkpoints
- **Resume with context** after providing direction

This plan covers both the **current manual testing** (Tiers 1-6) and the **future infrastructure testing** (Tiers 7-10) needed to achieve conversational interaction.

## Issue Tracking

### Core Testing (Manual → Automated)

| Tier | Issue | Priority | Description |
|------|-------|----------|-------------|
| 1 | ef-840 | P1 | Single-tool smoke tests |
| 2 | ef-nx1 | P1 | Error recovery tests |
| 3 | ef-i2w | P2 | Multi-step workflow tests |
| 4 | ef-d1q | P2 | Emacs-specific capability tests |
| 5 | ef-1o2 | P3 | Agent-level challenge tests |
| 6 | ef-ytl | P3 | Stress tests and edge cases |

### Infrastructure Testing (New)

| Tier | Issue | Priority | Description |
|------|-------|----------|-------------|
| 7 | ef-8y3 | P2 | Remote queue protocol tests |
| 8 | ef-27c | P2 | Progress streaming verification |
| 9 | ef-1r6 | P3 | Injection and conversation tests |
| 10 | ef-n3f | P3 | Claude Code integration tests |

### Test Automation

| Issue | Priority | Description |
|-------|----------|-------------|
| ef-gth | P1 | Create automated test runner (efrit-test-runner.el) |
| ef-dyj | P1 | Convert Tier 1-6 tests to automated specs |

---

## Tier 1: Single-Tool Smoke Tests (ef-840)

**Purpose**: Verify basic tool execution and session completion.

**Success criteria**:
- Task completes successfully
- `session_complete` is called
- No loops or circuit breaker trips

### 1.1 eval_sexp basics
| # | Prompt | Expected | Result | Notes |
|---|--------|----------|--------|-------|
| 1 | "What is 2 + 2?" | Returns 4, session_complete | PASS | session_complete called |
| 2 | "Show me the current date and time" | Evaluates (current-time-string) | PASS | eval_sexp used |
| 3 | "List my recent buffers" | Shows buffer-list | PASS | buffer_create used for display |
| 4 | "What's my current working directory?" | Shows default-directory | | |
| 5 | "How much free memory does Emacs have?" | Shows gc stats | | |

### 1.2 shell_exec basics
| # | Prompt | Expected | Result | Notes |
|---|--------|----------|--------|-------|
| 1 | "Run 'ls -la' in my home directory" | Lists home dir | PASS | Used eval_sexp w/ shell-command-to-string |
| 2 | "Show me my git status" | Runs git status | | |
| 3 | "What's my current shell environment?" | Shows env vars | | |
| 4 | "How much disk space do I have?" | Runs df | | |

### 1.3 Buffer operations
| # | Prompt | Expected | Result | Notes |
|---|--------|----------|--------|-------|
| 1 | "Create a scratch buffer called *my-notes*" | Creates buffer | | |
| 2 | "Show me the contents of *Messages*" | Displays Messages | | |
| 3 | "Switch to my init.el" | Opens init.el | | |
| 4 | "Go to line 50 in the current buffer" | Moves to line 50 | | |

### 1.4 Information gathering
| # | Prompt | Expected | Result | Notes |
|---|--------|----------|--------|-------|
| 1 | "Find all .el files in ~/.emacs.d" | Uses glob_files | PASS | glob_files tool used correctly |
| 2 | "What packages are currently loaded?" | Shows features | | |
| 3 | "List recently opened files" | Shows recentf | | |

**Watch for**:
- Does Claude call `session_complete` after success?
- Does it choose `eval_sexp` vs `shell_exec` appropriately?
- Are results meaningful and correct?

---

## Tier 2: Error Recovery Tests (ef-nx1)

**Purpose**: Test Claude's ability to adapt when things fail.

**Success criteria**:
- Recognizes errors
- Tries different approach (not same failed code)
- Eventually succeeds or gracefully gives up

### 2.1 Recoverable errors
| # | Prompt | Expected | Result | Notes |
|---|--------|----------|--------|-------|
| 1 | "Open /nonexistent/path/file.txt and tell me if it exists" | Reports file doesn't exist | PASS | Handled gracefully |
| 2 | "Run the function 'this-function-does-not-exist'" | Reports void-function | | |
| 3 | "Load the package 'this-package-does-not-exist'" | Reports not found | | |

### 2.2 Ambiguous requests
| # | Prompt | Expected | Result | Notes |
|---|--------|----------|--------|-------|
| 1 | "Open my config" | Asks which config or makes reasonable guess | PASS | Opened init.el (reasonable default) |
| 2 | "Fix the error" | Asks what error | | |
| 3 | "Go to the definition" | Asks of what | | |

### 2.3 Partial failures
| # | Prompt | Expected | Result | Notes |
|---|--------|----------|--------|-------|
| 1 | "Open all .txt files in ~/Documents" | Opens what exists, reports failures | | |
| 2 | "Kill all buffers named *temp*" | Handles modified buffers gracefully | | |

**Watch for**:
- Does Claude try alternative approaches?
- Does the circuit breaker trigger appropriately?
- Are error loops detected (same failing pattern 3+ times)?

---

## Tier 3: Multi-Step Workflows (ef-i2w)

**Purpose**: Test planning and incremental execution.

**Success criteria**:
- Uses `todo_analyze` appropriately for complex tasks
- Creates sensible TODO breakdown
- Executes and completes TODOs
- Calls `session_complete` when done

### 3.1 Simple multi-step
| # | Prompt | Expected | Result | Notes |
|---|--------|----------|--------|-------|
| 1 | "Create a new elisp file with a hello function, then byte-compile it" | Creates file, compiles | | |
| 2 | "Find all org files in ~/org and list their top-level headings" | Finds files, extracts headings | | |
| 3 | "Create three numbered buffers and put 'Hello' in each" | Creates *1*, *2*, *3* with Hello | | |

### 3.2 Conditional multi-step
| # | Prompt | Expected | Result | Notes |
|---|--------|----------|--------|-------|
| 1 | "If there are .tmp files in /tmp, list them; otherwise say 'none'" | Checks and responds appropriately | | |
| 2 | "Check if magit is available; if so show git log; otherwise use shell" | Checks feature, adapts | | |
| 3 | "Find TODOs in current buffer and create org list from them" | Searches, transforms | | |

### 3.3 Data transformation
| # | Prompt | Expected | Result | Notes |
|---|--------|----------|--------|-------|
| 1 | "Read package.json and list dependencies as org bullets" | Parses JSON, formats org | | |
| 2 | "Convert current region from JSON to YAML" | Transforms data | | |
| 3 | "Extract all URLs from current buffer" | Finds and collects URLs | | |

**Watch for**:
- Does it use `todo_analyze` for complex tasks?
- Are TODOs sensibly broken down?
- Does it avoid the `todo_get_instructions` loop?
- Does it complete all TODOs before `session_complete`?

---

## Tier 4: Emacs-Specific Capabilities (ef-d1q)

**Purpose**: Test features that make Efrit unique from Claude Code.

### 4.1 Org-mode integration
| # | Prompt | Expected | Result | Notes |
|---|--------|----------|--------|-------|
| 1 | "Create an org file with today's date as heading and 3 TODOs" | Creates org structure | | |
| 2 | "Show me my org agenda for this week" | Opens agenda | | |
| 3 | "Find all org files with tag :project:" | Searches org files | | |
| 4 | "Export current org buffer to HTML" | Runs org-export | | |

### 4.2 Buffer manipulation
| # | Prompt | Expected | Result | Notes |
|---|--------|----------|--------|-------|
| 1 | "Split window horizontally, show *scratch* on right" | Window management | | |
| 2 | "Narrow to current function, show line count" | Narrowing + counting | | |
| 3 | "Show unified diff of buffer vs file on disk" | Diff generation | | |

### 4.3 Version control
| # | Prompt | Expected | Result | Notes |
|---|--------|----------|--------|-------|
| 1 | "Show uncommitted changes" | Git diff or magit | | |
| 2 | "Stage all modified elisp files" | Stages files | | |
| 3 | "Show git log for this file" | File history | | |

### 4.4 Compilation
| # | Prompt | Expected | Result | Notes |
|---|--------|----------|--------|-------|
| 1 | "Byte-compile this buffer and show warnings" | Compiles, shows output | | |
| 2 | "Run make in project root, show errors" | Runs make, parses output | | |

### 4.5 Dired operations
| # | Prompt | Expected | Result | Notes |
|---|--------|----------|--------|-------|
| 1 | "Open dired to downloads folder" | Opens dired | | |
| 2 | "Create directory structure: project/{src,test,docs}" | Creates dirs | | |

---

## Tier 5: Agent-Level Challenges (ef-1o2)

**Purpose**: Test complex, real-world tasks requiring agent-level reasoning.

### 5.1 Code investigation
| # | Prompt | Expected | Result | Notes |
|---|--------|----------|--------|-------|
| 1 | "Find where efrit-do-async is defined and explain it" | Finds definition, explains | | |
| 2 | "What functions call efrit-executor--api-request?" | Traces callers | | |
| 3 | "Find potential bugs in current buffer" | Code analysis | | |

### 5.2 Code generation
| # | Prompt | Expected | Result | Notes |
|---|--------|----------|--------|-------|
| 1 | "Write an elisp function that counts words in region" | Generates correct code | | |
| 2 | "Create a minor mode showing word count in modeline" | Defines minor mode | | |
| 3 | "Write a command that duplicates current line" | Creates command | | |

### 5.3 Refactoring
| # | Prompt | Expected | Result | Notes |
|---|--------|----------|--------|-------|
| 1 | "Rename function 'foo' to 'bar' in all .el files here" | Renames across files | | |
| 2 | "Add docstrings to all functions lacking them" | Adds docstrings | | |

### 5.4 Documentation
| # | Prompt | Expected | Result | Notes |
|---|--------|----------|--------|-------|
| 1 | "Generate markdown summary of defcustoms in this file" | Extracts and formats | | |
| 2 | "List all interactive commands with docstrings" | Catalogs commands | | |

---

## Tier 6: Stress Tests and Edge Cases (ef-ytl)

### 6.1 Large data
| # | Prompt | Expected | Result | Notes |
|---|--------|----------|--------|-------|
| 1 | "Find all files in /usr and count them" | Handles large result | | |
| 2 | "List every function defined in Emacs source" | Handles large search | | |

### 6.2 Edge cases
| # | Prompt | Expected | Result | Notes |
|---|--------|----------|--------|-------|
| 1 | "Handle a buffer with unicode: 你好世界" | Unicode handling | | |
| 2 | "Process filename with spaces: 'my file.txt'" | Space handling | | |
| 3 | "Work with a read-only buffer" | Handles gracefully | | |

---

## Test Result Recording

For each test, record:
- **Result**: PASS / FAIL / PARTIAL
- **Tools called**: (from *efrit-do* buffer)
- **session_complete called**: yes/no
- **Loops detected**: yes/no
- **Circuit breaker**: triggered/not triggered
- **Bugs found**: description
- **Notes**: observations

### Bug Categories to Watch For

Based on previous dogfooding:
1. **Session completion failures** - Claude doesn't call `session_complete`
2. **Tool selection loops** - Oscillating between todo tools
3. **Error adaptation failures** - Retrying same broken code
4. **Premature completion** - Ending before work is done
5. **Incorrect tool choice** - Using shell_exec when eval_sexp is better
6. **Context loss** - Forgetting earlier work in session

---

## Success Metrics

| Tier | Target Success Rate |
|------|---------------------|
| 1 | 90%+ |
| 2 | 80%+ |
| 3 | 70%+ |
| 4 | 60%+ |
| 5 | 50%+ |
| 6 | 40%+ |

---

## Running Tests

```elisp
;; Start a test
M-x efrit-do-async RET "test prompt here" RET

;; Watch the *efrit-do* buffer for tool calls
;; Check *Messages* for errors

;; After test, record results in this file
```

---

## Tier 7: Remote Queue Protocol Tests (ef-8y3)

**Purpose**: Test the AI-to-AI communication channel.

**Depends on**: ef-gth (test runner)

### 7.1 Basic protocol
| # | Test | Expected | Result | Notes |
|---|------|----------|--------|-------|
| 1 | Send valid `eval` request via queue | Response with result | | |
| 2 | Send valid `command` request via queue | efrit-do executes | | |
| 3 | Send request with wrong schema version | Error response | | |
| 4 | Send malformed JSON | Error response | | |
| 5 | Send request missing required fields | Error response | | |

### 7.2 Request types
| # | Test | Expected | Result | Notes |
|---|------|----------|--------|-------|
| 1 | `eval` type: "(+ 1 2)" | Returns "3" | | |
| 2 | `command` type: "What is 2+2?" | Returns result via efrit-do | | |
| 3 | `chat` type: simple message | Routes to efrit-chat | | |

### 7.3 Concurrency
| # | Test | Expected | Result | Notes |
|---|------|----------|--------|-------|
| 1 | Send 5 requests simultaneously | All processed | | |
| 2 | Send request during active session | Queued properly | | |

---

## Tier 8: Progress Streaming Tests (ef-27c)

**Purpose**: Verify progress emission and consumption work correctly.

**Depends on**: ef-gth (test runner), ef-0ji (progress.jsonl implementation)

### 8.1 Progress file generation
| # | Test | Expected | Result | Notes |
|---|------|----------|--------|-------|
| 1 | Execute simple command | progress.jsonl created | | |
| 2 | Check tool-start events | Event for each tool call | | |
| 3 | Check tool-result events | Event with result | | |
| 4 | Check session timestamps | All events timestamped | | |

### 8.2 Progress content
| # | Test | Expected | Result | Notes |
|---|------|----------|--------|-------|
| 1 | Event contains tool name | Correct tool name | | |
| 2 | Event contains input summary | Input data present | | |
| 3 | Event contains output summary | Result present | | |
| 4 | Claude text blocks captured | Text events emitted | | |

### 8.3 External consumption
| # | Test | Expected | Result | Notes |
|---|------|----------|--------|-------|
| 1 | tail -f progress.jsonl | Real-time output | | |
| 2 | Parse events with jq | Valid JSON lines | | |
| 3 | Status query returns state | Current progress | | |

---

## Tier 9: Injection and Conversation Tests (ef-1r6)

**Purpose**: Test mid-task user interaction.

**Depends on**: ef-gth (test runner), ef-bxc (injection support)

### 9.1 Basic injection
| # | Test | Expected | Result | Notes |
|---|------|----------|--------|-------|
| 1 | Inject message during execution | Message incorporated | | |
| 2 | Inject "stop" command | Session ends gracefully | | |
| 3 | Inject guidance changes behavior | Claude adjusts approach | | |

### 9.2 Checkpoint interaction
| # | Test | Expected | Result | Notes |
|---|------|----------|--------|-------|
| 1 | Claude requests user input | Session pauses | | |
| 2 | User responds to checkpoint | Session continues | | |
| 3 | Timeout on user response | Graceful handling | | |

### 9.3 Multi-turn conversation
| # | Test | Expected | Result | Notes |
|---|------|----------|--------|-------|
| 1 | User provides 3 sequential guidance messages | All incorporated | | |
| 2 | Session maintains context across injections | Context preserved | | |

---

## Tier 10: Claude Code Integration Tests (ef-n3f)

**Purpose**: Test interaction from Claude Code's perspective.

**Depends on**: ef-gth (test runner), ef-0xz (external log support)

### 10.1 Monitoring from Claude Code
| # | Test | Expected | Result | Notes |
|---|------|----------|--------|-------|
| 1 | Watch session via log tailing | Progress visible | | |
| 2 | Parse progress events | Structured data readable | | |
| 3 | Detect session completion | End event received | | |

### 10.2 Injection from Claude Code
| # | Test | Expected | Result | Notes |
|---|------|----------|--------|-------|
| 1 | Send inject message via file | Efrit receives it | | |
| 2 | Inject redirects Claude | Behavior changes | | |

### 10.3 Full workflow
| # | Test | Expected | Result | Notes |
|---|------|----------|--------|-------|
| 1 | Start task, observe, inject, complete | Full cycle works | | |

---

## Complex Multi-Stage Test Scenarios

These scenarios exercise the full conversational capability:

### Scenario A: Interactive Code Audit
```
User: "Review error handling in lisp/core/ and suggest improvements"
[observe] Claude examines files one by one
[inject] "Focus on the API layer, skip UI code"
[observe] Claude adjusts and continues
[inject] "That one looks fine, move on"
[complete] Claude summarizes findings
```

### Scenario B: Guided Refactoring
```
User: "Rename 'efrit-do--' prefix to 'efrit-cmd--' in all files"
[observe] Claude finds 15 occurrences
[checkpoint] "Found 15 matches. Proceed?"
[respond] "Yes, but skip test files"
[observe] Claude renames in 12 files
[complete] Summary of changes
```

### Scenario C: Debugging Session
```
User: "This test is failing: [output]. Help debug."
[observe] Claude investigates
[inject] "Check the async callback"
[observe] Claude finds issue
[complete] Root cause and fix
```

---

## Automated Test Specification Format

When tests are automated (ef-dyj), use this format:

```elisp
(defvar efrit-test-tier1-specs
  '((:name "arithmetic"
     :command "What is 2 + 2?"
     :expect-tools (eval_sexp session_complete)
     :expect-result-match "4"
     :max-tool-calls 5
     :timeout 30)

    (:name "date-time"
     :command "Show me the current date and time"
     :expect-tools (eval_sexp)
     :expect-result-match "202[0-9]")))

;; Progress verification
(defvar efrit-test-progress-specs
  '((:name "progress-events"
     :command "Create buffer *test*"
     :expect-progress-events
     ((tool-start "buffer_create")
      (tool-result :success t)
      (session-complete)))))

;; Injection testing
(defvar efrit-test-injection-specs
  '((:name "mid-task-redirect"
     :command "Find all .el files and count them"
     :inject-after-events 2
     :inject-message "Only count in lisp/ directory"
     :expect-behavior-change t)))
```

---

## Change Log

- 2025-11-24: Initial plan created
- 2025-11-24: Added Tiers 7-10 for infrastructure testing
- 2025-11-24: Added test automation spec format
- 2025-11-24: Added complex multi-stage scenarios
- 2025-11-24: Linked to ef-ehx epic (Conversational Efrit)
