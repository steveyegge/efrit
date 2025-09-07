# Efrit Development Roadmap

## 🚨 **CRITICAL ISSUE: CORE WORKFLOW BROKEN**

> **URGENT DISCOVERY**: efrit-do-async doesn't work - gets stuck in TODO loops and never calls Claude API. Previous integration test was fake and tested offline elisp execution only.

## Phase 1: Fix Core Workflow 🚨 CRITICAL

### Immediate Priorities (Blocking Everything)
- [ ] **Debug TODO Loop Issue** - efrit-do-async gets stuck in `todo_get_instructions` infinite loops
- [ ] **Fix API Calling** - Sessions show 0 API calls despite running for minutes  
- [ ] **Create REAL Integration Test** - One that actually burns tokens and calls Claude API
- [ ] **Verify Autonomy** - Test Claude can discover warnings and fix files independently

### Root Cause Analysis Needed
- Why does `todo_get_instructions` loop infinitely?
- Why does efrit-do-async never reach Claude API?
- What's blocking the async workflow from completing?

---

## Phase 2: Real Integration Testing (After Core Fix)

### Token-Burning Integration Test
- [ ] **Automatic Warning Generation** - Load 3 elisp files missing lexical-binding cookies
- [ ] **Natural Language Direction** - `(efrit-do-async "Fix lexical-binding warnings")`  
- [ ] **Claude Discovery** - Let Claude find warnings in *Warnings* buffer autonomously
- [ ] **Autonomous Fix** - Claude writes elisp code and fixes files without pre-written solutions
- [ ] **Token Verification** - Confirm API calls occurred and money was spent

### Validation Criteria
- [ ] Session logs show `"api-calls": > 0`
- [ ] All 3 files get lexical-binding headers added  
- [ ] *Warnings* buffer clears after fixes
- [ ] Real network traffic to Claude API
- [ ] Actual cost in API tokens

---

## Phase 3: Architecture Improvements (After Real Test Works)

### Workflow Robustness  
- [ ] Loop detection in TODO system
- [ ] Better error handling and recovery
- [ ] Security system refinement
- [ ] Performance optimization

### Test Suite Expansion
- [ ] Multi-file modification scenarios
- [ ] Complex warning types beyond lexical-binding
- [ ] Integration with real projects
- [ ] Autonomous development workflows

## Success Metrics

**Phase 1 Success**: efrit-do-async successfully calls Claude API and burns tokens
**Phase 2 Success**: Real integration test fixes lexical-binding warnings autonomously  
**Phase 3 Success**: Robust workflow handles complex real-world scenarios

## Current Blocker

The fundamental efrit-do-async workflow is broken. Until we fix the TODO loops preventing API calls, no real testing or development can proceed.
