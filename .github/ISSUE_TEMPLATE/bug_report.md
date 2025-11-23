---
name: Bug Report
about: Report a bug or unexpected behavior in Efrit
title: "[BUG] "
labels: ["bug"]
assignees: []
---

## Bug Description

**Clear, concise description of the bug**

<!-- Example: "efrit-chat hangs when executing shell commands with large output" -->

## Environment

- **Emacs version**: <!-- Run `emacs --version` -->
- **Efrit version**: <!-- Run `M-x efrit-version` or check git commit -->
- **Operating System**: <!-- macOS, Linux, Windows -->
- **Claude API key configured**: <!-- Yes/No - check ~/.authinfo -->

## Steps to Reproduce

1.
2.
3.

## Expected Behavior

<!-- What should happen? -->

## Actual Behavior

<!-- What actually happens? -->

## Error Messages

```
<!-- Paste any error messages from *Messages* buffer or terminal -->
<!-- Run M-x efrit-doctor to check for common issues -->
```

## Minimal Reproduction

**Can you reproduce this with a minimal example?**

```elisp
;; Paste minimal elisp code that reproduces the issue
;; Or describe the minimal steps in efrit-chat
```

## Additional Context

<!-- Add any other context, screenshots, or logs -->

## Checklist

- [ ] I've run `M-x efrit-doctor` to check for common configuration issues
- [ ] I've verified my Claude API key works (check ~/.authinfo)
- [ ] I've checked existing issues to avoid duplicates
- [ ] I've included error messages from *Messages* buffer
- [ ] I've provided a minimal reproduction case
