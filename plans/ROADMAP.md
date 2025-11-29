# Efrit Roadmap

## Philosophy: Dogfooding

Efrit's development follows a simple approach:

1. **Use Efrit to build Efrit** - Push its capabilities constantly
2. **File issues as you find them** - Every limitation becomes a tracked issue
3. **Work through the backlog** - Prioritize what blocks real work
4. **Repeat** - Each session improves the tool

The goal: Make Efrit as capable as a human-assisted Claude Code session.

## Current Focus

Check what's ready to work on:

```bash
bd ready --json
```

Check overall project health:

```bash
bd stats
```

## Issue Types

- **bug** - Something broken
- **feature** - New capability needed
- **task** - Work item (tests, docs, refactoring)
- **epic** - Large feature spanning multiple issues

## Priorities

- **P0** - Critical (security, data loss, broken builds)
- **P1** - High (blocks real work)
- **P2** - Medium (improves workflow)
- **P3** - Low (nice to have)
- **P4** - Backlog (someday/maybe)

## Success Metrics

Efrit is ready when:
- It can handle multi-file refactoring sessions
- It can run tests and fix failures iteratively
- It can explore codebases and answer questions
- It doesn't require frequent manual intervention
- Sessions feel as productive as Claude Code

## Contributing

See [DEVELOPMENT.md](../DEVELOPMENT.md) for how to contribute.

The only rule: Respect the Pure Executor principle. No client-side intelligence.
