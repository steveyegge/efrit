# Efrit Migration Guide

**For users upgrading from older versions of Efrit**

This guide helps you migrate from older Efrit versions to v0.4.0+. If you're a new user, you can skip this and follow the [README.md](README.md) installation guide.

---

## Overview

**Current version**: v0.3.0
**Target version**: v0.4.0+
**Migration difficulty**: Low to Medium

### What Changed

- **Architectural purge**: Removed client-side intelligence (v0.3.0)
- **Data directory reorganization**: Centralized data files (v0.3.0)
- **Configuration updates**: New customization variables
- **Command deprecations**: Some old commands removed
- **MCP server updates**: Protocol changes for stability

---

## Breaking Changes

### 1. Architectural Purge (v0.3.0) - Zero Client-Side Intelligence

**What changed**: Removed all hard-coded task-specific logic from Efrit.

**Impact**: Claude must now genuinely solve problems without built-in shortcuts.

**Removed functionality**:
- ❌ Hard-coded lexical-binding warning parsing
- ❌ Pre-generated elisp code solutions
- ❌ Task-specific cognitive hints and guidance
- ❌ Workflow decision-making heuristics
- ❌ Continuation examples with smart routing

**Why**: These violated the Pure Executor principle. See [ARCHITECTURE.md](ARCHITECTURE.md).

**Action required**: None. Efrit now works correctly by delegating all intelligence to Claude.

**Related commit**: `76f1d4f` (Sept 2025)

---

### 2. Data Directory Reorganization (v0.3.0)

**What changed**: All Efrit data files moved to centralized `~/.emacs.d/.efrit/` directory.

**Old locations** → **New locations**:

```
~/.emacs.d/efrit-do-context.el          → ~/.emacs.d/.efrit/context/efrit-do-context.el
~/.emacs.d/efrit-queue/                 → ~/.emacs.d/.efrit/queues/
~/.emacs.d/efrit-queue-ai/              → ~/.emacs.d/.efrit/queue-ai/
~/.emacs.d/efrit-ai-workspace/          → ~/.emacs.d/.efrit/workspace/
~/.emacs.d/efrit-*                      → ~/.emacs.d/.efrit/[appropriate-subdir]/
```

**New structure**:
```
~/.emacs.d/.efrit/
├── cache/              # Temporary cache files
├── context/            # Context persistence (efrit-do)
├── queues/             # AI-to-AI communication
│   ├── requests/       # Incoming requests
│   ├── processing/     # Currently processing
│   ├── responses/      # Completed responses
│   └── archive/        # Historical data
├── queue-ai/           # Autonomous AI communication
├── logs/               # Debug and activity logs
├── sessions/           # Session persistence data
└── workspace/          # Autonomous workspace
    ├── auto-saves/     # Emacs auto-save files
    └── backups/        # Backup files
```

**Action required**:

**Automatic migration** (recommended):
```elisp
;; Efrit automatically migrates on first load
(require 'efrit)
;; Check migration: old files should be moved
```

**Manual migration** (if automatic fails):
```bash
# Backup old files first
mkdir -p ~/.efrit-backup
cp -r ~/.emacs.d/efrit-* ~/.efrit-backup/

# Let Efrit auto-migrate
emacs -L /path/to/efrit/lisp -l efrit
# Or manually move files following structure above
```

**Custom data directory**:
```elisp
;; Set BEFORE loading efrit
(setq efrit-data-directory "~/my-custom-efrit-data")
(require 'efrit)
```

---

### 3. Configuration Variables

**New variables** (v0.3.0+):

```elisp
;; Data directory (default: ~/.emacs.d/.efrit)
(setq efrit-data-directory "~/.emacs.d/.efrit")

;; Auto-initialize directories on load (default: t)
(setq efrit-auto-initialize t)

;; For use-package users: disable side effects during load
(use-package efrit
  :init
  (setq efrit-data-directory "~/efrit-data")
  (setq efrit-auto-initialize nil)  ; Control initialization
  :commands (efrit-chat efrit-do))
```

**Deprecated variables**: None currently, but check release notes.

---

### 4. API Endpoint Configuration

**What changed**: Better support for enterprise proxies and custom endpoints (v0.3.0).

**Old approach**:
```elisp
;; Hard-coded endpoint
(setq efrit-api-base-url "https://api.anthropic.com")
```

**New approach** (dynamic, function-based):
```elisp
;; Static URL (still works)
(setq efrit-api-base-url "https://proxy.company.com/anthropic")

;; Dynamic URL (recommended for VPN/proxy scenarios)
(setq efrit-api-base-url
  (lambda ()
    (if (company-vpn-connected-p)
        "https://internal-proxy.company.com/anthropic"
      "https://api.anthropic.com")))
```

**Action required**: Update if using custom endpoints. See [README.md](README.md#advanced-configuration).

---

### 5. Deprecated/Removed Commands

**None officially deprecated yet**, but watch for future changes in v0.4.0:

**Commands to watch**:
- `efrit-command` - May be deprecated (not found in current codebase)
- `efrit-agent` - May be deprecated (not found in current codebase)

**If using these**: Check v0.4.0 release notes for alternatives.

---

## Migration Steps (v0.2.x → v0.3.0+)

### Step 1: Backup Your Configuration

```bash
# Backup your entire .emacs.d directory
cp -r ~/.emacs.d ~/.emacs.d.backup.$(date +%Y%m%d)

# Or just backup Efrit files
mkdir -p ~/.efrit-backup
cp -r ~/.emacs.d/.efrit* ~/.efrit-backup/
cp ~/.emacs.d/efrit-* ~/.efrit-backup/ 2>/dev/null
```

### Step 2: Update Efrit Code

```bash
cd /path/to/efrit
git fetch origin
git checkout main
git pull origin main
```

### Step 3: Update Configuration (Optional)

If you want custom data directory:

```elisp
;; In ~/.emacs.d/init.el, BEFORE (require 'efrit)
(setq efrit-data-directory "~/my-efrit-data")
```

### Step 4: Restart Emacs and Test

```bash
# Clean restart
emacs -q -L /path/to/efrit/lisp -l efrit
```

Then test:
```elisp
M-x efrit-version  ; Check version
M-x efrit-doctor   ; Run diagnostics
M-x efrit-chat     ; Try interactive chat
```

### Step 5: Verify Migration

Check that old files were moved:

```bash
# Old locations should be empty or gone
ls -la ~/.emacs.d/efrit-*

# New location should have migrated files
ls -la ~/.emacs.d/.efrit/
```

### Step 6: Clean Up (After Verification)

Once everything works:

```bash
# Remove backup (optional, after confirming everything works)
rm -rf ~/.emacs.d.backup.*

# Old Efrit files should already be migrated
# If any remain and you've verified migration, you can remove them:
# rm -rf ~/.emacs.d/efrit-*  # BE CAREFUL - only after verifying migration!
```

---

## Version-Specific Migrations

### Migrating from v0.1.x → v0.2.x

**Major changes**:
- Session-based architecture introduced
- Async execution added
- Performance optimizations

**Action required**: None, backward compatible.

### Migrating from v0.2.x → v0.3.0

**Major changes**:
- Architectural purge (removed client-side intelligence)
- Data directory reorganization
- Better configuration system

**Action required**: Follow steps above.

### Migrating to v0.4.0+ (Future)

**Planned changes** (check release notes):
- MCP server completion
- Possible command deprecations
- Enhanced error handling

**Action required**: TBD in v0.4.0 release notes.

---

## Rollback Instructions

If you need to roll back to an older version:

### Step 1: Restore Backup

```bash
# Restore entire .emacs.d
rm -rf ~/.emacs.d
cp -r ~/.emacs.d.backup.YYYYMMDD ~/.emacs.d

# Or restore just Efrit files
rm -rf ~/.emacs.d/.efrit
cp -r ~/.efrit-backup/* ~/.emacs.d/
```

### Step 2: Checkout Older Version

```bash
cd /path/to/efrit
git fetch --tags
git checkout v0.2.0  # or whichever version you want
```

### Step 3: Clean Compiled Files

```bash
make clean
make compile
```

### Step 4: Restart Emacs

```bash
emacs -q -L /path/to/efrit/lisp -l efrit
```

---

## Common Migration Issues

### Issue: "Cannot find efrit-do-context.el"

**Cause**: Context file not migrated.

**Solution**:
```elisp
;; Create missing directories
(require 'efrit-config)
(efrit-config--ensure-directories)

;; Manually migrate if needed
(efrit-config-migrate-old-files)
```

### Issue: "Queue directory not found"

**Cause**: Queue directories not created.

**Solution**:
```elisp
M-x efrit-remote-queue-start
;; This auto-creates queue directories
```

### Issue: "Old behavior (smart parsing) missing"

**Cause**: Architectural purge removed client-side intelligence.

**This is intentional**: Efrit is now a Pure Executor. Claude handles all intelligence.

**Solution**: None needed. Claude now does genuine problem-solving. If you see failures, it's Claude needing better prompts, not Efrit needing logic.

### Issue: "Performance seems slower"

**Cause**: No more shortcuts; Claude does real work now.

**Solution**: This is correct behavior. If too slow, consider:
- Using faster model: `(setq efrit-model "claude-3-haiku-20240307")`
- Caching: v0.3.0 has response caching
- Check `M-x efrit-performance-show-stats`

### Issue: "API key not found"

**Cause**: Migration didn't copy API configuration.

**Solution**:
```bash
# Check ~/.authinfo has your key
cat ~/.authinfo | grep anthropic

# If missing, add it:
echo "machine api.anthropic.com login personal password YOUR_KEY" >> ~/.authinfo
```

---

## Getting Help

If you encounter migration issues:

1. **Check diagnostics**: `M-x efrit-doctor`
2. **Check logs**: `~/.emacs.d/.efrit/logs/`
3. **Check version**: `M-x efrit-version`
4. **File an issue**: [GitHub Issues](https://github.com/steveyegge/efrit/issues) with:
   - Old version number
   - New version number
   - Error messages
   - Steps you took
   - Output of `M-x efrit-doctor`

---

## Compatibility Matrix

| Version | Emacs 28.1 | Emacs 29.x | Emacs 30.x | API v1 | Notes |
|---------|-----------|-----------|-----------|--------|-------|
| v0.1.x  | ✅        | ✅        | ✅        | ✅     | Legacy |
| v0.2.x  | ✅        | ✅        | ✅        | ✅     | Stable |
| v0.3.0  | ✅        | ✅        | ✅        | ✅     | Current |
| v0.4.0+ | ✅        | ✅        | ✅        | ✅     | Planned |

**Minimum Emacs version**: 28.1
**Recommended Emacs version**: 29.1+

---

## Resources

- **Installation Guide**: [README.md](README.md)
- **Architecture**: [ARCHITECTURE.md](ARCHITECTURE.md) - Understand Pure Executor principle
- **Development**: [DEVELOPMENT.md](DEVELOPMENT.md)
- **Onboarding**: [ONBOARDING.md](ONBOARDING.md)
- **Roadmap**: [plans/ROADMAP.md](plans/ROADMAP.md)
- **Troubleshooting**: [TROUBLESHOOTING.md](TROUBLESHOOTING.md)

---

**Last Updated**: November 2025
**Applies to**: v0.3.0 → v0.4.0+ migrations
**Next Review**: Before v0.4.0 release (Q1 2026)
