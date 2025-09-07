# Efrit Development Roadmap

## 🎯 **PHASE 1: INTEGRATION TEST SUCCESS** (Current Priority)

> **Mission-Critical**: The lexical-binding integration test is our top priority. This validates the entire efrit system end-to-end and represents our core use case.

### Critical Path 🚨
- [ ] **Lexical-Binding Integration Test** - **TOP PRIORITY**
  - Get Claude to autonomously fix lexical-binding warnings in elisp files
  - This validates the entire efrit system end-to-end
  - See `INTEGRATION_TEST_HANDOFF.md` for current status and instructions

### Known Issues to Fix (Blocking Integration Test)
- [ ] **TODO System Loop Detection** - Fix `todo_get_instructions` infinite loops
- [ ] **Security vs Functionality** - Better balance for file modifications  
- [ ] **Instruction Interpretation** - Improve buffer code execution understanding

---

## PHASE 2: Core Stabilization (After Integration Test Success)

### Integration Test Suite Expansion
- [ ] **Real-world scenarios** with chatgpt-shell upgrade
- [ ] **Multi-buffer operations** testing
- [ ] **Error recovery** and resume testing

### Architecture Improvements
- [ ] **TODO System Robustness** - Add loop detection to all TODO tools
- [ ] **Security System Refinement** - Configurable security levels for different use cases
- [ ] **Performance Optimization** - API call batching, caching, memory monitoring

---

## PHASE 3: Trimodal Problem-Solving Architecture (Future)

### Vision: Three Complementary Modes

```
┌─────────────┐    ┌─────────────┐    ┌─────────────┐
│ CHAT MODE   │    │ ONE-OFF     │    │ AGENT MODE  │
│ efrit-chat  │    │ efrit-do    │    │ efrit-agent │
├─────────────┤    ├─────────────┤    ├─────────────┤
│ Preserved   │    │ Enhanced    │    │ New Build   │
│ Current     │    │ Error       │    │ Aggressive  │
│ Behavior    │    │ Recovery    │    │ Takeover    │
│             │    │ One Session │    │ Until Done  │
│ Q&A Focus   │    │ Quick Tasks │    │ Complex     │
│ No Agendas  │    │ Smart Retry │    │ Problems    │
└─────────────┘    └─────────────┘    └─────────────┘
```

### Current State (v0.2.1)
- ✅ Working chat interface (`efrit-chat`)  
- ✅ Working one-off commands (`efrit-do`)
- ✅ Basic multi-turn conversations
- ✅ Comprehensive test suite
- ✅ Elisp syntax validation in `efrit-do`
- ✅ Intelligent retry logic with Claude error feedback
- ✅ Dashboard and session tracking system

## Implementation Priorities

**Priority 1**: Integration test success (validates everything works)
**Priority 2**: Stabilize core systems (TODO loops, security, performance)
**Priority 3**: Expand capabilities (agent mode, advanced features)

---

*The lexical-binding integration test is our north star - everything else is secondary until this works reliably.*
