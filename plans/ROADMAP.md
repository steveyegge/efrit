# Efrit Roadmap

**Current Version**: v0.3.0

**Philosophy**: Work incrementally, ship frequently, maintain stability.

---

## v0.4.0 - Foundation & Community Readiness (Q1 2026)

**Focus**: Fix foundation issues, complete community-facing work, finish MCP server.

### Core Goals

- ✅ **Development Experience** (ef-9qu) - COMPLETED
  - ✅ CI/CD pipeline for multi-version Emacs testing
  - ✅ DEVELOPMENT.md onboarding guide
  - ✅ Developer utilities (efrit-version, efrit-doctor, efrit-run-tests)

- 🔄 **Community Readiness** (ef-2kq) - IN PROGRESS
  - ✅ GitHub issue templates (bug, feature, docs)
  - ✅ ONBOARDING.md for new maintainers
  - 📋 ROADMAP.md (this file)
  - 📋 MIGRATION.md for early adopters

- 📋 **MCP Server Completion** (ef-2z5)
  - Complete MCP server to 100% functionality
  - Add integration tests for real protocol communication (ef-m63)
  - Performance and reliability improvements
  - Rate limiting implementation (ef-dsu)
  - Queue stats completion (ef-53o)

### Secondary Goals

- **Documentation Consolidation**
  - Ensure all docs are current and consistent
  - Cross-reference properly (ARCHITECTURE.md ↔ DEVELOPMENT.md ↔ ONBOARDING.md)
  - Migration guide for breaking changes

- **Code Quality**
  - Remove unused dependencies (ef-svx)
  - Align naming conventions (ef-q58, ef-uyu)
  - Module structure cleanup (ef-tok)

### Success Criteria

- [ ] MCP server 100% complete with integration tests
- [ ] All community-readiness tasks closed
- [ ] CI/CD pipeline stable across Emacs 28.1, 29.x, 30.x
- [ ] ONBOARDING.md + MIGRATION.md complete
- [ ] Zero P0/P1 bugs

**Target Date**: March 2026

---

## v0.5.0 - Stability & Performance (Q2 2026)

**Focus**: Stabilize async execution, improve performance, comprehensive testing.

### Core Goals

- **Async Execution Stability** (ef-cf1)
  - Simplify async execution architecture
  - Fix edge cases in workflow state management
  - Eliminate tool selection loops (ef-bt0 family)
  - Robust error handling and recovery

- **Performance Optimization**
  - Response caching improvements
  - Memory management tuning
  - API call optimization
  - Queue processing efficiency

- **Testing Infrastructure**
  - Complete test infrastructure (ef-4nc, ef-zlw)
  - Integration tests for all core flows
  - Protocol validation tests
  - Performance regression tests

### Secondary Goals

- **API Stability**
  - Lock down public API signatures
  - Version all tools schemas
  - Deprecation warnings for breaking changes

- **Error Handling**
  - Better error messages with actionable guidance
  - Graceful degradation for API failures
  - Circuit breaker improvements

- **Monitoring & Observability**
  - Enhanced efrit-doctor diagnostics
  - Performance statistics dashboard
  - Session history analysis tools

### Success Criteria

- [ ] Zero async execution bugs in production use
- [ ] Test suite covers >80% of core functionality
- [ ] Performance benchmarks established and stable
- [ ] API versioning and deprecation policy documented
- [ ] No tool selection loops in any workflow

**Target Date**: June 2026

---

## v1.0.0 - Production Ready (Q3 2026)

**Focus**: Production readiness, comprehensive documentation, stable API, long-term maintenance plan.

### Core Goals

- **Production Readiness**
  - Comprehensive end-to-end testing
  - Load testing and stress testing
  - Security audit of all eval_sexp/shell_exec usage
  - Rollback and recovery procedures

- **Documentation Excellence**
  - Complete user guide with examples
  - Architecture deep-dive documentation
  - API reference documentation
  - Video tutorials and demos
  - Troubleshooting guide expansion

- **API Stability Guarantee**
  - Semantic versioning commitment
  - Deprecation policy (6-month notice minimum)
  - Backwards compatibility testing
  - Migration tooling for major version bumps

### Secondary Goals

- **Enterprise Features**
  - Proxy/custom endpoint support (already exists, document well)
  - Custom authentication methods (already exists, document well)
  - Audit logging capabilities
  - Rate limiting and quota management

- **Community Growth**
  - Contributor guidelines refinement
  - Good first issue curation
  - Regular release cadence (monthly)
  - Active issue triage

- **Self-Enhancement Framework**
  - Clear guidelines for AI-driven improvements
  - Safety boundaries for autonomous changes
  - Test-before-commit enforcement
  - Rollback mechanisms

### Success Criteria

- [ ] All documentation complete and current
- [ ] 90%+ test coverage
- [ ] Zero P0/P1 bugs for 3 consecutive months
- [ ] API stability guarantee in place
- [ ] 10+ external contributors
- [ ] Production deployments with no critical issues

**Target Date**: September 2026

---

## Post-1.0 Vision (Future)

### Potential Features (Not Committed)

- **Multi-AI Support**: Support for GPT-4, Claude variants, local models
- **Voice Integration**: Voice command support for Efrit
- **Visual Understanding**: Image/screenshot analysis capabilities
- **Advanced Workflows**: DAG-based multi-step workflows
- **Collaborative AI**: Multiple AI agents working together
- **Plugin System**: Third-party tool integrations
- **Cloud Sync**: Session sync across machines
- **Mobile Companion**: Mobile app for on-the-go Efrit access

**Note**: These are ideas, not commitments. Will be prioritized based on community needs.

---

## Issue Tracking

We use **bd (beads)** for all issue tracking. See `.beads/` directory.

### Check Current Status

```bash
# View roadmap progress
bd stats --json

# Find ready work
bd ready --json

# View open issues by epic
bd list --status=open --json | jq -r 'group_by(.issue_type) | map({type: .[0].issue_type, count: length})'

# View blocked issues
bd blocked --json
```

### Priority Levels

- **P0** (Critical): Security, data loss, broken builds
- **P1** (High): Major features, important bugs (v0.4.0 blockers)
- **P2** (Medium): Nice-to-have features, minor bugs (v0.5.0 targets)
- **P3** (Low): Polish, optimization (v1.0.0 targets)
- **P4** (Backlog): Future ideas, maybe never

### Milestone Tracking

Issues are tagged with target milestones:
- `ef-hsl`: Efrit Modernization (spans v0.4.0)
- `ef-2kq`: Community Readiness (v0.4.0)
- `ef-2z5`: MCP Server (v0.4.0)
- `ef-cf1`: Async Stability (v0.5.0)
- etc.

---

## Release Process

### Version Numbering

Following [Semantic Versioning](https://semver.org/):
- **Major** (1.0.0): Breaking API changes
- **Minor** (0.4.0): New features, backwards compatible
- **Patch** (0.3.1): Bug fixes, no API changes

### Release Checklist

Before each release:

1. **Quality Gates**
   - [ ] All tests pass: `make test`
   - [ ] Compilation clean: `make compile`
   - [ ] Integration tests pass: `make test-integration`
   - [ ] No P0/P1 bugs open for milestone

2. **Documentation**
   - [ ] CHANGELOG.md updated
   - [ ] README.md current
   - [ ] Version bumped in all modules
   - [ ] Migration guide updated (if breaking changes)

3. **Testing**
   - [ ] Test across Emacs 28.1, 29.x, 30.x
   - [ ] Manual smoke tests on macOS, Linux
   - [ ] Remote queue integration tests pass

4. **Release**
   - [ ] Tag release: `git tag -a v0.X.0 -m "Release v0.X.0"`
   - [ ] Push tags: `git push origin v0.X.0`
   - [ ] Create GitHub release with notes
   - [ ] Announce in Discussions

---

## Contributing to Roadmap

Have ideas for the roadmap? Here's how to influence it:

1. **File an issue**: Use GitHub issue templates
2. **Discuss in Discussions**: Gauge community interest
3. **Submit a PR**: Show working prototype
4. **Align with Pure Executor**: Must respect Zero Client-Side Intelligence

See [CONTRIBUTING.md](../CONTRIBUTING.md) for details.

---

## Roadmap Principles

### ✅ DO:
- Work incrementally (small PRs, frequent releases)
- Ship features when stable (don't wait for perfect)
- Maintain backwards compatibility (until 2.0)
- Test thoroughly (especially integration tests)
- Document as you build (not afterwards)
- Respect Pure Executor principle (always)

### ❌ DON'T:
- Add client-side intelligence (violates architecture)
- Build speculative features (YAGNI)
- Break APIs without deprecation notice (pre-1.0 is more flexible)
- Skip testing to ship faster
- Sacrifice stability for features

---

## Questions?

- **Roadmap priorities**: File issue or discuss in GitHub Discussions
- **Feature requests**: Use feature request template (must align with Pure Executor)
- **Timeline concerns**: We prioritize quality over speed
- **Want to help**: Check [ONBOARDING.md](../ONBOARDING.md) for good first issues

---

**Last Updated**: November 2025

**Status**: v0.3.0 shipped, v0.4.0 in progress (50% complete)

**Next Release**: v0.4.0 (estimated March 2026)
