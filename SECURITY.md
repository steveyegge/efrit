# Security Model

Efrit executes AI-generated code in your Emacs environment. This document describes the security implications and recommended practices.

## Trust Model

**Efrit trusts Claude (via Anthropic's API) to execute arbitrary Elisp code in your Emacs.**

This means:
- Claude decides what code to run
- Efrit executes it without semantic validation
- The code runs with your user privileges

This is fundamentally "remote code execution" - you're trusting Anthropic's AI to act responsibly within your environment.

## What Gets Sent to the API

Each Efrit session sends context to Claude including:

| Data Type | Example | Purpose |
|-----------|---------|---------|
| Username | `user-login-name` | Context for file paths |
| Home directory | `~` expansion | File operations |
| Current buffer | File name, mode, region | Understanding what you're working on |
| Project files | Git-tracked files | Codebase exploration |
| Recent files | `recentf-list` | Quick navigation |
| Shell output | Command results | Task completion |

**Sensitive data handling:**
- API keys are NOT sent (use `.authinfo.gpg` for secure storage)
- File contents are only sent when explicitly read by a tool
- Shell command output is returned to Claude

## Shell Command Security

Efrit restricts which shell commands Claude can execute.

### Default Allowed Commands

Development tools: `git`, `npm`, `cargo`, `go`, `python`, `make`, `grep`, `find`, etc.

**Remote access enabled by default:** `ssh`, `scp`, `rsync`

This means Claude can:
- Connect to any host in your SSH config
- Copy files to/from remote systems
- Execute commands on remote systems

### Configuring Shell Security

```elisp
;; Disable all shell security (trust Claude completely)
(setq efrit-do-shell-security-enabled nil)

;; OR allow all commands but keep forbidden pattern checks
(setq efrit-do-allowed-shell-commands '("*"))

;; Remove remote access from defaults
(setq efrit-do-allowed-shell-commands
      (delete "ssh" (delete "scp" (delete "rsync" 
        efrit-do-allowed-shell-commands))))

;; View current whitelist
M-: efrit-do-allowed-shell-commands
```

### Forbidden Patterns

Regardless of whitelist, these patterns are always blocked:
- `sudo` - Privilege escalation
- `shutdown`, `reboot`, `halt` - System control
- `rm -rf /` - Catastrophic deletion
- `$(...)`, `` `...` `` - Command injection in arguments

Configure with `efrit-do-forbidden-shell-patterns`.

## Elisp Execution

The `eval_sexp` tool executes arbitrary Elisp with no sandbox.

### Risks
- Infinite loops or slow code can freeze Emacs
- Destructive operations (deleting buffers, files)
- Network access via `url-retrieve`

### Mitigations

**Timeout protection (default: 30 seconds)**

Elisp evaluations automatically timeout after 30 seconds to prevent runaway code:

```elisp
;; Adjust timeout (seconds)
(setq efrit-tools-eval-timeout 60)  ; Increase to 60s for slow operations

;; Disable timeout (not recommended)
(setq efrit-tools-eval-timeout nil)
```

If an eval hangs despite the timeout, use `C-g` to manually interrupt.

**Disable Elisp evaluation entirely**

```elisp
(setq efrit-tools-sexp-evaluation-enabled nil)
```

This completely prevents Claude from running arbitrary Elisp. Claude can still use the predefined tools (file operations, shell commands, etc.), but cannot evaluate custom expressions.

## Recommended Practices

1. **Never run Emacs as root** - Claude's actions inherit your privileges

2. **Use encrypted authinfo** - Store API keys in `~/.authinfo.gpg`, not plain text

3. **Audit the shell whitelist** - Remove commands you don't need:
   ```elisp
   (setq efrit-do-allowed-shell-commands
         '("git" "grep" "find" "ls" "cat"))
   ```

4. **Review before confirming** - Efrit asks for confirmation on dangerous operations; read them carefully

5. **Use checkpoints** - Before risky operations, Efrit can create restore points:
   ```
   M-x efrit-do RET
   > create a checkpoint before refactoring
   ```

6. **Monitor the progress buffer** - Watch what commands are being executed

7. **Use `efrit-do-silently` cautiously** - Background execution hides what's happening

## Incident Response

If something goes wrong:

1. **Interrupt:** `C-g` stops the current operation
2. **Restore:** Use `restore_checkpoint` to undo file changes
3. **Review logs:** `M-x efrit-log-show`
4. **Git recovery:** `git stash list` and `git reflog` for recent changes

## Reporting Security Issues

Report security vulnerabilities privately via GitHub Security Advisories:
https://github.com/steveyegge/efrit/security/advisories

Do not open public issues for security vulnerabilities.
