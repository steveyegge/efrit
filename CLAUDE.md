# Village Workspace Instructions

## Your Current Context

**Workspace**: Run `git branch --show-current` to see your workspace name
**Project**: efrit
**Model**: Multi-agent village with worktree isolation

## Critical: This is a Village Workspace

You are working in an **isolated workspace** on a **long-lived branch**. This is NOT the single-repo model.

### What This Means

- **Your workspace**: Isolated copy of the codebase on your own branch
- **Other agents**: Working in parallel in their own isolated workspaces
- **No file conflicts**: You cannot have filesystem conflicts with other agents
- **Integration**: Happens through git merge/PR, not real-time file sharing

**Read**: `~/ai/village/WORKFLOW_MODEL.md` for detailed explanation and examples

## Beads Workflow (Issue Tracking)

This project uses **Beads** for issue tracking. Special village configuration:

### Required: No-Daemon Mode

```bash
# CRITICAL: Always set this in your environment
export BEADS_NO_DAEMON=1
```

**Why**: Git worktrees + beads daemon = incompatible. Daemon commits to wrong branch.

### Daily Workflow

**Start of session**:
```bash
# Get latest shared beads state from main
git pull origin main

# Check available work
bd ready

# View all issues
bd list
```

**During work**:
```bash
# Create issues
bd create "Implement feature X" -t feature -p 1

# Update status
bd update bd-123 --status in_progress

# View issue details
bd show bd-123
```

**End of session**:
```bash
# Sync beads to your branch
bd sync

# Push your branch
git push origin $(git branch --show-current)
```

**Integration**:
- Create PR from your branch to main
- After merge, other workspaces run `git pull origin main` to get your beads updates
- Merge conflicts are rare (intelligent field-level merge driver handles them)

### Shared Visibility

All workspaces share the same beads database (`.beads/beads.jsonl` in git):
- You see issues created by other workspaces (after they merge to main)
- Your issues are visible to others (after you merge to main)
- Coordination happens through shared issue visibility + agent mail

## Agent Mail (Inter-Agent Communication)

Use agent mail for message-based delegation and coordination.

### Check Your Identity

```bash
# See your agent name and project
cat .agent-identity 2>/dev/null || echo "Not configured"
```

### Common Patterns

**Delegate work**:
```
"Hey BlueDog, can you add tests for the auth module? Thread: bd-123"
```

**Coordinate integration**:
```
"I'm merging a breaking API change to main - you'll need to update your branch"
```

**Signal major refactoring** (optional file reservation):
```
bd reserve "src/auth/**" --reason "Major refactoring in progress in my branch"
```

**Read**: `~/ai/village/QUICKSTART.md` for agent mail examples

## Git Workflow

### Your Branch Model

- **Your workspace**: Always on your dedicated branch (run `git branch --show-current`)
- **Your commits**: Go to your branch
- **Integration**: Via PR/merge to main
- **Sync from others**: `git pull origin main`

### Typical Workflow

```bash
# 1. Start with latest from main
git pull origin main

# 2. Make changes
# ... edit files ...
git add .
git commit -m "Implement feature X"

# 3. Sync beads
bd sync

# 4. Push your branch
git push origin $(git branch --show-current)

# 5. Create PR: your-branch → main
# 6. After merge, other workspaces: git pull origin main
```

### Viewing Other Workspaces' Work

```bash
# See what's in another workspace's branch
git fetch origin
git show origin/BlueDog:path/to/file.go

# Or check out their branch temporarily
git checkout BlueDog
# ... review ...
git checkout $(git rev-parse --abbrev-ref @{-1})  # Back to your branch
```

## Coordination Strategy

### Primary: Message-Based Delegation

Use agent mail to delegate work, not file reservations:

✅ **DO**: "BlueDog, can you handle X in your branch?"
❌ **DON'T**: "Let me reserve all files before working"

### Secondary: Shared Beads Visibility

Check beads to see what's being worked on:

```bash
bd list --status in_progress    # What's actively being worked on
bd ready                        # What's available to work on
bd show bd-123                  # Details about specific issue
```

### Optional: File Reservations

File reservations are **advisory signals only** in the village model:

```bash
# Signal major work in progress (optional)
bd reserve "src/core/**" --reason "Refactoring type system in my branch"

# Check reservations (to avoid duplicate effort)
bd reservations
```

**Remember**: Reservations don't prevent conflicts (impossible in separate workspaces). They're just broadcasts to avoid duplicate work.

## Quick Reference

### Essential Commands

```bash
# Beads (issue tracking)
bd ready                        # Show available work
bd create "Title" -t type -p n  # Create issue
bd update <id> --status <s>     # Update status
bd show <id>                    # View details
bd sync                         # Sync to git

# Git (version control)
git pull origin main            # Get latest shared state
git push origin $(git branch)   # Push your work
git status                      # Check working tree

# Agent mail (check mail interface for details)
# Send messages via mail interface
# Check inbox via mail interface
```

### Environment Variables

```bash
export BEADS_NO_DAEMON=1        # Required for worktrees
export BD_ACTOR="YourName"      # Optional: set your name in beads audit trail
```

## Help & Documentation

- **Village model**: `~/ai/village/WORKFLOW_MODEL.md`
- **Village quickstart**: `~/ai/village/QUICKSTART.md`
- **Beads architecture**: `~/ai/village/BEADS_VILLAGE_ARCHITECTURE.md`
- **Agent mail**: `~/ai/village/QUICKSTART.md` (agent mail section)

## Common Mistakes to Avoid

❌ Running beads without `BEADS_NO_DAEMON=1` (commits to wrong branch)
❌ Thinking you need to reserve files before editing (you're in isolated workspace!)
❌ Forgetting to pull from main before starting (miss other agents' work)
❌ Not running `bd sync` before pushing (beads changes not committed)
❌ Trying to use beads-mcp server (requires daemon, incompatible with worktrees)

## Quick Checklist

**At start of session**:
- [ ] `export BEADS_NO_DAEMON=1` (or add to shell config permanently)
- [ ] `git pull origin main`
- [ ] `bd ready` (see available work)

**During work**:
- [ ] Use beads to track issues (`bd create`, `bd update`)
- [ ] Commit code changes to git regularly
- [ ] Use agent mail to coordinate with other workspaces

**At end of session**:
- [ ] `bd sync` (commit beads changes)
- [ ] `git push origin $(git branch --show-current)` (push your branch)
- [ ] Consider creating PR if work is ready to integrate

**Remember**: You're in an isolated workspace. Work independently, coordinate through messages and shared beads visibility, integrate through git PR workflow.
