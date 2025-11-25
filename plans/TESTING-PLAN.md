# Efrit Manual Testing Plan: Road to Agent Parity

**Epic**: ef-6sv - Manual Testing Plan: Efrit-do-async Agent Capability
**Created**: 2025-11-24
**Status**: Active

## Overview

Progressive testing plan to exercise `efrit-do-async` capabilities, building from simple smoke tests to Claude Code-level agent tasks.

## Issue Tracking

| Tier | Issue | Priority | Description |
|------|-------|----------|-------------|
| 1 | ef-840 | P1 | Single-tool smoke tests |
| 2 | ef-nx1 | P1 | Error recovery tests |
| 3 | ef-i2w | P2 | Multi-step workflow tests |
| 4 | ef-d1q | P2 | Emacs-specific capability tests |
| 5 | ef-1o2 | P3 | Agent-level challenge tests |
| 6 | ef-ytl | P3 | Stress tests and edge cases |

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

## Change Log

- 2025-11-24: Initial plan created
